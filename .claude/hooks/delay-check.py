#!/usr/bin/env python3
"""PreToolUse hook: deny added time constants used to wait or give up.

Claude Code runs this before Bash, Write, Edit and MultiEdit calls with the
call as JSON on stdin. It looks only at what the call adds: the Bash command
(including heredoc bodies, `python3 -c` code and task text passed to
`herdr worker start`, `herdr worker prompt` or `herdr agent prompt`), Write's
content and Edit's new_string, minus lines that were already there. When it
finds a sleep, a timeout, a deadline, a debounce, a poll or a resend interval
without a `delay: <reason>` marker next to it, it denies the call and tells
the agent to wait for the positive event instead.

It never blocks read-only commands and fails open: any error of its own
allows the call and says why on stderr.

Rule: "Added delay is a bug signal" in ~/.claude/CLAUDE.md.
"""

import json
import os
import re
import shlex
import sys

MARKER = re.compile(r"\bdelay:\s*\S", re.IGNORECASE)

# The marker may sit on the flagged line, up to this many lines above it, or
# on the line below it.
MARKER_LINES_ABOVE = 2
MARKER_LINES_BELOW = 1

MAX_FILE_BYTES = 2 * 1024 * 1024
MAX_REPORTED = 3

# This hook's own files describe every pattern it flags.
SELF_FILES = {"delay-check.py", "test_delay_check.py"}

# JSON cannot carry a marker comment; lock files and images are not written
# by hand.
SKIPPED_SUFFIXES = {
    ".json", ".lock", ".svg", ".png", ".jpg", ".jpeg", ".gif", ".ico",
    ".pdf", ".zip", ".gz",
}

DOC_SUFFIXES = {".md", ".markdown", ".txt", ".rst"}
# Prose files are checked only when they instruct agents (skills, rules,
# commands); notes such as TODO.md describe past delays without adding them.
AGENT_DOC_NAMES = {"AGENTS.md", "CLAUDE.md", "SKILL.md", "AGENTS.policy.md"}
AGENT_DOC_DIRS = {"skills", "commands", "agents", "rules", "output-styles"}

QTY = (
    r"\d+(?:[.,]\d+)?\s*-?\s*"
    r"(?:ms|msec|milliseconds?|s|secs?|seconds?|m|mins?|minutes?|h|hrs?|hours?)\b"
)
KEY = (
    r"(?:debounc\w*|deadlines?|time[- ]?outs?|timed[- ]out|give up|giving up"
    r"|wait\w*|poll\w*|retr(?:y|ies|ied|ying)|resen[dt]\w*|re-?send\w*"
    r"|sleep\w*|idle|grace|settl\w*|back-?off|heartbeat\w*)"
)
SHELL_START = r"(?:^|[;&|(`{]|\$\(|\b(?:do|then|else|time|nohup|exec|command)\b)\s*"
# A zero length (`0`, `0.0`, `0s`) does not wait: `sleep 0` is a no-op,
# `timeout 0` disables the limit, `setTimeout(fn, 0)` only yields.
ZERO = r"0+(?:\.0*)?"
ZERO_VALUE = re.compile(r"^" + ZERO + r"[smhd]?$")
NOT_ZERO = r"(?!" + ZERO + r"[smhd]?(?![^\s;&|)]))"
NOT_ZERO_CALL = r"(?!\s*" + ZERO + r"\s*\))"
TIME_NAME = (
    r"(?:timeout|deadline|debounce|interval|grace|retry_after|backoff|delay"
    r"|wait_(?:ms|millis|secs?|seconds)|poll_(?:ms|millis|secs?|seconds|every))"
)

# (label, regex, unless): a match is ignored when `unless` matches its line.
CODE_PATTERNS = [
    ("sleep command", re.compile(SHELL_START + r"(sleep\s+" + NOT_ZERO + r"[^\s;&|)]+)"), None),
    (
        "timeout command",
        re.compile(
            SHELL_START + r"(g?timeout\s+(?:-\S+\s+)*" + NOT_ZERO + r"\d[\d.]*[smhd]?)(?!\S)"
        ),
        None,
    ),
    (
        "timeout option",
        re.compile(
            r"(--[\w-]*timeout[\w-]*(?:=|\s+|[\"'],\s*[\"'])[\"']?(?!" + ZERO + r"(?![\d_.]))"
            r"\d[\d_.]*)"
        ),
        None,
    ),
    (
        "sleep call",
        re.compile(r"((?:\btime|\basyncio|\btrio|\bgevent)\.sleep\s*\(" + NOT_ZERO_CALL + r")"),
        None,
    ),
    (
        "sleep call",
        re.compile(
            r"(\b(?:thread|time)::sleep\s*\((?!\s*(?:[\w:]*::)?Duration::(?:ZERO\b|from_\w+\("
            + ZERO
            + r"\s*\))))"
        ),
        None,
    ),
    (
        "duration literal",
        re.compile(
            r"(Duration::from_(?:secs|millis|micros|nanos|secs_f32|secs_f64|mins|hours)"
            r"\(" + NOT_ZERO_CALL + r"\s*\d[\d_.]*\s*\))"
        ),
        None,
    ),
    ("deadline from now", re.compile(r"(Instant::now\(\)\s*\+)"), None),
    (
        "deadline from now",
        re.compile(r"(\b(?:time\.(?:monotonic|time|perf_counter)\(\)|Date\.now\(\))\s*\+)"),
        None,
    ),
    (
        "timer",
        re.compile(r"(\bset(?:Timeout|Interval)\s*\()"),
        re.compile(r",\s*" + ZERO + r"\s*\)"),
    ),
    ("timeout call", re.compile(r"(\b\w*_timeout(?:_ms)?\s*\([^)\n]*[1-9])"), None),
    (
        "time constant",
        re.compile(
            r"(\b\w*" + TIME_NAME + r"\w*[\"']?\s*[:=]\s*[\"']?(?!" + ZERO + r"(?![\d_.]))"
            r"\d[\d_.]*)",
            re.IGNORECASE,
        ),
        None,
    ),
]

PROSE_PATTERNS = [
    (
        "timed wait",
        re.compile(r"(" + QTY + r"(?:[\s-]+[\w`]+){0,2}?[\s-]+" + KEY + r")", re.IGNORECASE),
        None,
    ),
    (
        "timed wait",
        re.compile(r"(\b" + KEY + r"\b[^.\n;:]{0,30}?\b" + QTY + r")", re.IGNORECASE),
        None,
    ),
    ("interval", re.compile(r"(\bevery\s+" + QTY + r")", re.IGNORECASE), None),
]

ALL_PATTERNS = CODE_PATTERNS + PROSE_PATTERNS

READ_ONLY_COMMANDS = {
    "cat", "head", "tail", "less", "more", "grep", "egrep", "fgrep", "rg",
    "ag", "ls", "tree", "wc", "sort", "uniq", "cut", "tr", "jq", "yq",
    "file", "stat", "du", "df", "pwd", "which", "type", "realpath",
    "dirname", "basename", "diff", "cmp", "awk", "echo", "printf", "true",
    "false", "test", "[", "date", "whoami", "id", "uname", "ps", "pgrep",
    "lsof", "column", "nl", "od", "xxd", "hexdump", "shasum", "md5",
}
READ_ONLY_GIT = {
    "log", "show", "diff", "status", "grep", "blame", "rev-parse",
    "ls-files", "ls-tree", "cat-file", "describe", "shortlog", "reflog",
    "rev-list", "merge-base", "name-rev", "for-each-ref",
}
READ_ONLY_HERDR = {
    ("agent", "read"), ("agent", "list"), ("agent", "explain"),
    ("agent", "status"), ("worker", "log"), ("worker", "status"),
    ("worker", "list"), ("worker", "obligations"), ("workspace", "list"),
    ("tab", "list"), ("pane", "list"),
}
TASK_COMMANDS = {("worker", "start"), ("worker", "prompt"), ("agent", "prompt")}
WRAPPERS = {"time", "nohup", "exec", "command", "nice", "caffeinate", "sudo"}
SHELL_KEYWORDS = {"do", "then", "else", "elif", "if", "while", "until", "!", "{", "}"}
SHELLS = {"bash", "sh", "zsh", "dash"}
INTERPRETERS = {"python", "python3", "node", "ruby", "perl", "deno", "bun"}
SEPARATORS = {";", "&&", "||", "|", "&", "|&", ";;", "(", ")", "\n"}

HEREDOC = re.compile(
    r"<<-?[ \t]*(['\"]?)([A-Za-z_][\w]*)\1[^\n]*\n(.*?)\n[ \t]*\2[ \t]*(?=\n|\)|$)",
    re.DOTALL,
)
CAT_FILE = re.compile(r"\$\(\s*cat\s+['\"]?([^'\")\s]+)")


class Finding:
    def __init__(self, label, text, where):
        self.label = label
        self.text = text
        self.where = where


def scan_lines(text, patterns, where, added=None, marker_scope=None):
    """Return findings in `text` without a nearby marker.

    `added` is the set of stripped lines the call adds (None: all of them).
    `marker_scope` is a text in which any marker counts (Bash commands and
    task texts are one added text); otherwise the marker must be near the line.
    """
    if marker_scope is not None and MARKER.search(marker_scope):
        return []
    lines = text.split("\n")
    findings = []
    for index, line in enumerate(lines):
        if added is not None and line.strip() not in added:
            continue
        for label, regex, unless in patterns:
            match = regex.search(line)
            if not match or (unless is not None and unless.search(line)):
                continue
            if marker_scope is None:
                low = max(0, index - MARKER_LINES_ABOVE)
                high = min(len(lines), index + MARKER_LINES_BELOW + 1)
                if any(MARKER.search(near) for near in lines[low:high]):
                    break
            findings.append(Finding(label, match.group(1).strip(), where))
            break
    return findings


def added_lines(old, new):
    old_lines = {line.strip() for line in old.split("\n")}
    return {line.strip() for line in new.split("\n") if line.strip() not in old_lines}


def file_patterns(path):
    """Patterns for a file, or None when the file is not checked."""
    name = os.path.basename(path or "")
    if name in SELF_FILES:
        return None
    suffix = os.path.splitext(name)[1].lower()
    if suffix in SKIPPED_SUFFIXES:
        return None
    if suffix in DOC_SUFFIXES:
        parts = set(os.path.normpath(path).split(os.sep))
        if name in AGENT_DOC_NAMES or parts & AGENT_DOC_DIRS:
            return ALL_PATTERNS
        return None
    return ALL_PATTERNS


def read_file(path):
    try:
        if os.path.getsize(path) > MAX_FILE_BYTES:
            return None
        with open(path, encoding="utf-8", errors="replace") as handle:
            return handle.read()
    except OSError:
        return None


def check_write(tool_input):
    path = tool_input.get("file_path") or ""
    patterns = file_patterns(path)
    if patterns is None:
        return []
    content = tool_input.get("content") or ""
    old = read_file(path) if os.path.exists(path) else None
    added = added_lines(old, content) if old is not None else None
    return scan_lines(content, patterns, path, added)


def check_edits(path, edits):
    patterns = file_patterns(path)
    if patterns is None:
        return []
    findings = []
    for edit in edits:
        old = edit.get("old_string") or ""
        new = edit.get("new_string") or ""
        findings += scan_lines(new, patterns, path, added_lines(old, new))
    return findings


def tokenize(command):
    command = command.replace("\\\n", " ")
    lexer = shlex.shlex(command, posix=True, punctuation_chars=";&|()<>\n")
    lexer.whitespace = " \t\r"
    lexer.whitespace_split = True
    lexer.commenters = ""
    tokens = []
    for token in lexer:
        tokens.append(token)
    return tokens


def segments(tokens):
    current = []
    for token in tokens:
        if token in SEPARATORS or (token and set(token) <= set(";&|()\n")):
            if current:
                yield current
            current = []
        else:
            current.append(token)
    if current:
        yield current


def strip_prefix(words):
    """Drop shell keywords, assignments and wrappers before the command word."""
    words = list(words)
    while words:
        head = words[0]
        if head in SHELL_KEYWORDS or head in WRAPPERS:
            words.pop(0)
        elif re.match(r"^[A-Za-z_]\w*=", head):
            words.pop(0)
        elif head == "env":
            words.pop(0)
            while words and (words[0].startswith("-") or "=" in words[0]):
                words.pop(0)
        else:
            break
    return words


def has_write_redirect(words):
    for index, word in enumerate(words):
        if word in (">", ">>"):
            target = words[index + 1] if index + 1 < len(words) else ""
            if target != "/dev/null" and not target.startswith("&"):
                return True
    return False


def is_read_only(words):
    words = strip_prefix(words)
    if not words or has_write_redirect(words):
        return not words
    command = os.path.basename(words[0])
    args = words[1:]
    if command in READ_ONLY_COMMANDS:
        return True
    if command == "find":
        return not {"-exec", "-execdir", "-delete", "-ok", "-okdir"} & set(args)
    if command == "sed":
        return not any(arg.startswith("-i") or arg == "--in-place" for arg in args)
    if command == "git":
        rest = [arg for arg in args if not arg.startswith("-")]
        return bool(rest) and rest[0] in READ_ONLY_GIT
    if command in ("herdr", "herdr-job"):
        rest = [arg for arg in args if not arg.startswith("-")]
        if command == "herdr-job":
            return bool(rest) and rest[0] in ("log", "list", "status")
        return tuple(rest[:2]) in READ_ONLY_HERDR
    return False


def task_text(words, command):
    """The task text of a herdr worker/agent prompt segment, or None."""
    if not words or os.path.basename(words[0]) != "herdr":
        return None
    rest = [word for word in words[1:] if not word.startswith("-")]
    if tuple(rest[:2]) not in TASK_COMMANDS:
        return None
    parts = [" ".join(words[3:])]
    for match in CAT_FILE.finditer(command):
        content = read_file(os.path.expanduser(os.path.expandvars(match.group(1))))
        if content:
            parts.append(content)
    return "\n".join(parts)


def check_segment(words, command, findings, depth=0):
    words = strip_prefix(words)
    if not words or depth > 3:
        return
    command_word = os.path.basename(words[0])
    args = words[1:]
    if command_word == "sleep" and args and not all(ZERO_VALUE.match(arg) for arg in args):
        findings.append(Finding("sleep command", "sleep " + args[0], "Bash command"))
    if command_word in ("timeout", "gtimeout"):
        duration = next((arg for arg in args if not arg.startswith("-")), "")
        if not ZERO_VALUE.match(duration):
            findings.append(Finding("timeout command", command_word + " " + duration, "Bash command"))
        tail = args[args.index(duration) + 1:] if duration in args else []
        check_segment(tail, command, findings, depth + 1)
        return
    if command_word == "watch" and "-n" in args:
        findings.append(Finding("polling command", "watch -n", "Bash command"))
    for index, word in enumerate(args):
        option = re.match(r"^(--[\w-]*timeout[\w-]*)(?:=(.*))?$", word)
        if not option:
            continue
        value = option.group(2)
        if value is None and index + 1 < len(args):
            value = args[index + 1]
        if value and re.match(r"^\d", value) and not ZERO_VALUE.match(value):
            findings.append(Finding("timeout option", option.group(1) + " " + value, "Bash command"))
    if command_word == "herdr-job" and "--" in args:
        check_segment(args[args.index("--") + 1:], command, findings, depth + 1)
    code_flag = {"-c"} if command_word in SHELLS else {"-c", "-e"}
    if command_word in SHELLS or re.match(r"^(python[\d.]*|node|ruby|perl|deno|bun)$", command_word):
        for index, word in enumerate(args[:-1]):
            if word in code_flag:
                code = args[index + 1]
                if command_word in SHELLS:
                    check_bash_text(code, findings, depth + 1)
                else:
                    findings += scan_lines(code, CODE_PATTERNS, "inline code", marker_scope=code)
                break
    task = task_text(words, command)
    if task is not None:
        findings += scan_lines(task, ALL_PATTERNS, "agent task text", marker_scope=task)


def heredoc_patterns(intro, command):
    """Patterns for a heredoc body from the line that introduces it."""
    if re.search(r"\bherdr\s+(?:worker\s+(?:start|prompt)|agent\s+prompt)\b", intro):
        return ALL_PATTERNS, "agent task text"
    if re.search(r"\bgit\s+(?:commit|tag|notes)\b", intro):
        return None, None
    target = re.search(r"(?:>>?|\btee(?:\s+-a)?)\s*['\"]?([^\s'\"<>|;&]+)", intro)
    if target and not target.group(1).startswith("&"):
        path = os.path.expanduser(target.group(1))
        return file_patterns(path), path
    return CODE_PATTERNS, "heredoc"


def check_bash_text(command, findings, depth=0):
    bodies = []

    def cut(match):
        start = command.rfind("\n", 0, match.start()) + 1
        intro = command[start:match.start()] + match.group(0).split("\n", 1)[0]
        bodies.append((intro, match.group(3)))
        return "\n"

    outer = HEREDOC.sub(cut, command)
    try:
        tokens = tokenize(outer)
    except ValueError:
        findings += scan_lines(outer, CODE_PATTERNS, "Bash command", marker_scope=command)
        tokens = None
    if tokens is not None:
        parts = list(segments(tokens))
        if parts and all(is_read_only(words) for words in parts) and not bodies:
            return
        for words in parts:
            check_segment(words, outer, findings, depth)
    for intro, body in bodies:
        patterns, where = heredoc_patterns(intro, command)
        if patterns is not None:
            findings += scan_lines(body, patterns, where, marker_scope=body + "\n" + intro)


def check_bash(tool_input):
    command = tool_input.get("command") or ""
    if MARKER.search(command):
        return []
    findings = []
    check_bash_text(command, findings)
    return findings


def check(payload):
    tool = payload.get("tool_name")
    tool_input = payload.get("tool_input") or {}
    if tool == "Bash":
        return check_bash(tool_input)
    if tool == "Write":
        return check_write(tool_input)
    if tool == "Edit":
        return check_edits(tool_input.get("file_path") or "", [tool_input])
    if tool == "MultiEdit":
        return check_edits(tool_input.get("file_path") or "", tool_input.get("edits") or [])
    return []


def reason(findings):
    shown = "; ".join(
        "`{}` ({}, {})".format(f.text, f.label, f.where) for f in findings[:MAX_REPORTED]
    )
    more = len(findings) - MAX_REPORTED
    if more > 0:
        shown += "; and {} more".format(more)
    return (
        "delay-check: this call adds a time constant used to wait or give up: "
        + shown
        + ". Added delay is a bug signal (~/.claude/CLAUDE.md): find the race, "
        "lost event or non-atomic write instead, wait for the positive event "
        "and name what produces it (a job's exit, a file published atomically, "
        "an acknowledgement, a state change). If it is a real external "
        "deadline, rate limit, external polling, designed backoff or measured "
        "slow hardware, add `delay: <which>` next to it (a code comment, "
        "`# delay: ...` in a shell command, a line in a task text). In a fix "
        "(not a test harness or tooling), get the user's or the coordinating "
        "agent's agreement, with the evidence, before adding the label; never "
        "add one just to get past this check (Rule 10). A constant "
        "that does not wait at all takes `delay: not a wait, <what it is>`."
    )


def main():
    try:
        payload = json.load(sys.stdin)
        findings = check(payload)
    except Exception as error:  # fail open on any error of the hook itself
        print("delay-check: internal error, allowing the call: {!r}".format(error), file=sys.stderr)
        return 0
    if findings:
        json.dump(
            {
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": reason(findings),
                }
            },
            sys.stdout,
        )
    return 0


if __name__ == "__main__":
    sys.exit(main())
