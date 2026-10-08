"""Tests for delay-check.py. Run: python3 -m unittest test_delay_check.py"""

import importlib.util
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
HOOK = os.path.join(HERE, "delay-check.py")

spec = importlib.util.spec_from_file_location("delay_check", HOOK)
delay_check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(delay_check)


def bash(command):
    return delay_check.check({"tool_name": "Bash", "tool_input": {"command": command}})


def write(path, content):
    return delay_check.check(
        {"tool_name": "Write", "tool_input": {"file_path": path, "content": content}}
    )


def edit(path, old, new):
    return delay_check.check(
        {
            "tool_name": "Edit",
            "tool_input": {"file_path": path, "old_string": old, "new_string": new},
        }
    )


class SessionHistory(unittest.TestCase):
    """The delays the coordinator added on 2026-10-07."""

    def test_debounce_120_s_in_a_script(self):
        self.assertTrue(write("/x/watch.py", "DEBOUNCE_SECS = 120\n"))
        self.assertTrue(write("/x/watch.sh", "# a 120 s debounce, not a delay\n"))

    def test_debounce_in_a_worker_task(self):
        self.assertTrue(
            bash('herdr worker start --name w --prompt "Add a 2-minute idle debounce."')
        )

    def test_30_min_deadline_in_a_worker_task(self):
        self.assertTrue(
            bash("herdr worker start --name w --prompt 'Give up after a 30 min deadline.'")
        )

    def test_30_min_deadline_in_code(self):
        self.assertTrue(write("/x/wait.py", "deadline = time.monotonic() + 1800\n"))

    def test_5_s_transcript_poll(self):
        self.assertTrue(write("/x/poll.py", "while True:\n    check()\n    time.sleep(5)\n"))
        self.assertTrue(
            bash("herdr agent prompt w1:p1 'poll the transcript every 5 s until it ends'")
        )

    def test_15_s_resend(self):
        self.assertTrue(write("/x/send.sh", "send\nsleep 15\nsend  # resend after 15 s\n"))
        self.assertTrue(
            bash("herdr worker prompt w3 'If it does not start, resend the prompt after 15 s.'")
        )

    def test_sleep_5(self):
        self.assertTrue(bash("sleep 5"))
        self.assertTrue(bash("cargo build && sleep 5 && herdr agent read p1"))
        self.assertTrue(bash("while ! test -f done; do sleep 5; done"))
        self.assertTrue(bash("cd x\nsleep 5\nls"))


class Flagged(unittest.TestCase):
    def test_shell_forms(self):
        self.assertTrue(bash("timeout 30 cargo test"))
        self.assertTrue(bash("herdr agent wait p1 --timeout 600000"))
        self.assertTrue(bash("herdr agent wait p1 --timeout=600000"))
        self.assertTrue(bash("herdr-job run --name t -- bash -c 'sleep 3; make'"))
        self.assertTrue(bash("python3 -c 'import time; time.sleep(2)'"))
        self.assertTrue(bash("x=$(sleep 1; date)"))
        self.assertTrue(bash("watch -n 2 ls"))

    def test_heredoc_script(self):
        self.assertTrue(bash("cat > /x/run.py <<'EOF'\nimport time\ntime.sleep(10)\nEOF"))
        self.assertTrue(bash("python3 - <<EOF\nimport time\ntime.sleep(10)\nEOF"))

    def test_heredoc_task(self):
        command = (
            "herdr worker start --name w --prompt \"$(cat <<'EOF'\n"
            "Fix it.\nWait 30 seconds for the server, then retry.\nEOF\n)\""
        )
        self.assertTrue(bash(command))

    def test_task_from_file(self):
        with tempfile.TemporaryDirectory(dir=HERE) as tmp:
            path = os.path.join(tmp, "task.md")
            with open(path, "w") as handle:
                handle.write("Poll every 10 s until the build finishes.\n")
            self.assertTrue(bash('herdr worker start --name w --prompt "$(cat ' + path + ')"'))

    def test_rust(self):
        self.assertTrue(write("/x/a.rs", "std::thread::sleep(Duration::from_millis(200));\n"))
        self.assertTrue(write("/x/a.rs", "let deadline = Instant::now() + timeout;\n"))
        self.assertTrue(write("/x/a.rs", "rx.recv_timeout(Duration::from_secs(5))?;\n"))
        self.assertTrue(write("/x/a.rs", "tokio::time::sleep(d).await;\n"))
        self.assertTrue(write("/x/a.rs", "child.wait_timeout(30)?;\n"))

    def test_js(self):
        self.assertTrue(write("/x/a.js", "setTimeout(check, 500);\n"))
        self.assertTrue(write("/x/a.ts", "const POLL_INTERVAL_MS = 250;\n"))

    def test_python_kwargs(self):
        self.assertTrue(write("/x/a.py", "subprocess.run(cmd, timeout=30)\n"))
        self.assertTrue(write("/x/a.py", 'args = ["--timeout", "60"]\n'))

    def test_agent_instructions(self):
        self.assertTrue(write("/x/skills/foo/SKILL.md", "Then wait 10 seconds and check.\n"))
        self.assertTrue(write("/x/AGENTS.md", "Run `sleep 2` before reading the pane.\n"))

    def test_multiedit(self):
        findings = delay_check.check(
            {
                "tool_name": "MultiEdit",
                "tool_input": {
                    "file_path": "/x/a.py",
                    "edits": [
                        {"old_string": "a = 1", "new_string": "a = 2"},
                        {"old_string": "go()", "new_string": "time.sleep(1)\ngo()"},
                    ],
                },
            }
        )
        self.assertEqual(len(findings), 1)


class Justified(unittest.TestCase):
    def test_marker_in_command(self):
        self.assertFalse(bash("sleep 60  # delay: rate limit, the API allows 1 call/min"))
        self.assertFalse(
            bash("herdr worker start --prompt 'Retry after 30 s (delay: designed backoff).'")
        )

    def test_marker_next_to_line(self):
        self.assertFalse(
            write("/x/a.py", "# delay: external polling, the API has no push\ntime.sleep(30)\n")
        )
        self.assertFalse(write("/x/a.rs", "let t = Duration::from_secs(5); // delay: measured slow CI\n"))

    def test_marker_far_away_does_not_count(self):
        content = "# delay: rate limit\n" + "x = 1\n" * 5 + "time.sleep(30)\n"
        self.assertTrue(write("/x/a.py", content))


class Unrelated(unittest.TestCase):
    def test_read_only_commands(self):
        self.assertFalse(bash("grep -rn 'sleep 5' src"))
        self.assertFalse(bash("rg -e '--timeout=5' src | head -20"))
        self.assertFalse(bash("git log --oneline -5 && git diff -- src"))
        self.assertFalse(bash("cat notes.md | grep 'every 5 s'"))
        self.assertFalse(bash("herdr agent read p1 --source detection"))
        self.assertFalse(bash("find . -name '*.rs' 2>/dev/null"))

    def test_plain_commands(self):
        self.assertFalse(bash("cargo nextest run --stress-count 40 -E 'test(x)'"))
        self.assertFalse(bash("herdr-job wait 12"))
        self.assertFalse(bash("git commit -m 'drop the 120 s debounce' -- a.py"))
        self.assertFalse(bash("git commit -F - <<'EOF'\nfix: no more sleep 5\nEOF"))
        self.assertFalse(bash("just test"))
        self.assertFalse(bash("herdr worker start --name w --prompt 'Run the tests and commit.'"))
        self.assertFalse(bash("echo it's broken"))

    def test_code_without_waits(self):
        self.assertFalse(write("/x/a.rs", "fn wait_until(deadline: Instant) -> bool {\n"))
        self.assertFalse(write("/x/a.rs", "let d = Duration::from_secs(0);\n"))
        self.assertFalse(write("/x/a.js", "setTimeout(render, 0);\n"))
        self.assertFalse(write("/x/a.py", "if now > deadline:\n    return\n"))
        self.assertFalse(write("/x/a.py", "print('took 134 s, 5 sessions')\n"))
        self.assertFalse(write("/x/a.rs", "if timeout == 0 {\n"))

    def test_notes_and_json_are_skipped(self):
        self.assertFalse(write("/x/TODO.md", "The coordinator polled a transcript every 5 s.\n"))
        self.assertFalse(write("/x/settings.json", '{"timeout": 10}\n'))
        self.assertFalse(write("/x/delay-check.py", "time.sleep(5)\n"))

    def test_existing_lines_are_not_added(self):
        old = "time.sleep(5)\nx = 1"
        self.assertFalse(edit("/x/a.py", old, "time.sleep(5)\nx = 2"))
        with tempfile.TemporaryDirectory(dir=HERE) as tmp:
            path = os.path.join(tmp, "a.py")
            with open(path, "w") as handle:
                handle.write("time.sleep(5)\n")
            self.assertFalse(write(path, "time.sleep(5)\nprint('hi')\n"))

    def test_other_tools(self):
        self.assertFalse(delay_check.check({"tool_name": "Read", "tool_input": {"file_path": "x"}}))


class Process(unittest.TestCase):
    def run_hook(self, stdin):
        return subprocess.run(
            [sys.executable, HOOK], input=stdin, capture_output=True, text=True, check=True
        )

    def test_deny_output(self):
        result = self.run_hook(json.dumps({"tool_name": "Bash", "tool_input": {"command": "sleep 5"}}))
        output = json.loads(result.stdout)["hookSpecificOutput"]
        self.assertEqual(output["hookEventName"], "PreToolUse")
        self.assertEqual(output["permissionDecision"], "deny")
        self.assertIn("`sleep 5`", output["permissionDecisionReason"])
        self.assertIn("delay: <which>", output["permissionDecisionReason"])

    def test_allow_prints_nothing(self):
        result = self.run_hook(json.dumps({"tool_name": "Bash", "tool_input": {"command": "ls"}}))
        self.assertEqual(result.stdout, "")

    def test_fails_open(self):
        result = self.run_hook("not json")
        self.assertEqual(result.stdout, "")
        self.assertIn("delay-check: internal error", result.stderr)


if __name__ == "__main__":
    unittest.main()
