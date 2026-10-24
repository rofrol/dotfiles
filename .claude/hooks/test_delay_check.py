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
        self.assertFalse(write("/x/a.rs", "let t = Duration::from_secs(5); // delay: measured slow hardware, CI measurements show this duration\n"))

    def test_marker_far_away_does_not_count(self):
        content = "# delay: rate limit\n" + "x = 1\n" * 5 + "time.sleep(30)\n"
        self.assertTrue(write("/x/a.py", content))

    def test_interval_marker_names_an_external_producer(self):
        self.assertFalse(write("/x/a.rs", "const SAMPLE: Duration = Duration::from_secs(5); // interval: kernel process table and free disk (read-only observer)\n"))
        self.assertFalse(write("/x/a.rs", "// interval: the OS process table (read-only observer)\nthread::sleep(SAMPLE);\n"))

    def test_interval_marker_without_external_producer_does_not_count(self):
        self.assertTrue(write("/x/a.rs", "thread::sleep(Duration::from_secs(5)); // interval: our queue table (read-only observer)\n"))
        self.assertTrue(write("/x/a.rs", "thread::sleep(Duration::from_secs(5)); // interval: our scheduler, kernel tick (read-only observer)\n"))
        self.assertTrue(write("/x/a.rs", "thread::sleep(Duration::from_secs(5)); // interval: local service API (read-only observer)\n"))

    def test_interval_marker_must_declare_a_read_only_observer(self):
        self.assertTrue(write("/x/a.rs", "thread::sleep(Duration::from_secs(5)); // interval: kernel process table\n"))

    def test_interval_marker_that_acts_does_not_count(self):
        self.assertTrue(write("/x/a.rs", "thread::sleep(Duration::from_secs(5)); // interval: kernel (read-only observer), rescan lost events\n"))
        self.assertTrue(write("/x/a.rs", "thread::sleep(Duration::from_secs(5)); // interval: kernel (read-only observer), wakes the agent\n"))

    def test_interval_marker_outside_a_comment_does_not_count(self):
        self.assertTrue(write("/x/a.js", "setTimeout(f, 5000); const o = { interval: kernel (read-only observer) };\n"))


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


class ZeroLength(unittest.TestCase):
    """A zero length does not wait (the coordinator's `sleep 0`, 2026-10-08)."""

    def test_sleep_0(self):
        for value in ("0", "0.0", "0s", "00", "0."):
            self.assertFalse(bash("sleep " + value), value)
        self.assertFalse(bash("cargo build && sleep 0 && herdr agent read p1"))
        self.assertFalse(bash("bash -c 'sleep 0; make'"))
        self.assertFalse(write("/x/run.sh", "make\nsleep 0\n"))
        self.assertFalse(write("/x/run.sh", "make; sleep 0.0; ls\n"))

    def test_timeout_0(self):
        self.assertFalse(bash("timeout 0 cargo test"))
        self.assertFalse(bash("gtimeout 0s cargo test"))
        self.assertFalse(bash("herdr agent wait p1 --timeout 0.0"))
        self.assertFalse(bash("herdr agent wait p1 --timeout=0"))
        self.assertFalse(write("/x/run.sh", "timeout 0 cargo test\n"))
        self.assertFalse(write("/x/a.py", 'args = ["--timeout", "0"]\n'))
        self.assertFalse(write("/x/a.py", "TIMEOUT = 0.0\n"))

    def test_command_after_timeout_0_is_still_checked(self):
        self.assertTrue(bash("timeout 0 sleep 5"))

    def test_rust_zero_durations(self):
        self.assertFalse(write("/x/a.rs", "let d = Duration::from_millis(0);\n"))
        self.assertFalse(write("/x/a.rs", "let d = Duration::from_secs_f64(0.0);\n"))
        self.assertFalse(write("/x/a.rs", "std::thread::sleep(Duration::from_secs(0));\n"))
        self.assertFalse(write("/x/a.rs", "thread::sleep(std::time::Duration::ZERO);\n"))
        self.assertFalse(write("/x/a.rs", "rx.recv_timeout(Duration::from_secs(0))?;\n"))

    def test_js_and_python_yields(self):
        self.assertFalse(write("/x/a.js", "setTimeout(() => render(), 0);\n"))
        self.assertFalse(write("/x/a.js", "setTimeout(render, 0.0);\n"))
        self.assertFalse(write("/x/a.py", "await asyncio.sleep(0)\n"))
        self.assertFalse(write("/x/a.py", "time.sleep(0.0)\n"))
        self.assertFalse(bash("python3 -c 'import time; time.sleep(0)'"))

    def test_non_zero_still_flagged(self):
        for value in ("0.5", "0.01", "1", "10", "0.5s", "infinity"):
            self.assertTrue(bash("sleep " + value), value)
        self.assertTrue(bash("sleep 0 5"))
        self.assertTrue(bash("timeout 0.5 cargo test"))
        self.assertTrue(bash("herdr agent wait p1 --timeout=0.5"))
        self.assertTrue(write("/x/run.sh", "sleep 0.5\n"))
        self.assertTrue(write("/x/run.sh", "timeout 05 make\n"))
        self.assertTrue(write("/x/a.py", 'args = ["--timeout", "0.5"]\n'))
        self.assertTrue(write("/x/a.py", "TIMEOUT = 0.5\n"))
        self.assertTrue(write("/x/a.py", "time.sleep(0.5)\n"))
        self.assertTrue(write("/x/a.py", "await asyncio.sleep(0.01)\n"))
        self.assertTrue(write("/x/a.rs", "let d = Duration::from_millis(10);\n"))
        self.assertTrue(write("/x/a.rs", "let d = Duration::from_secs_f64(0.5);\n"))
        self.assertTrue(write("/x/a.rs", "std::thread::sleep(Duration::from_millis(1));\n"))
        self.assertTrue(write("/x/a.js", "setTimeout(render, 0.5);\n"))
        self.assertTrue(write("/x/a.js", "setTimeout(render, 10);\n"))


class Rule10Safeguards(unittest.TestCase):
    def test_periodic_internal_recovery(self):
        prescriptions = [
            "Add a 5-minute sweep to recover herdr's missed own events.",
            "Use a safety net to recover dropped internal notifications.",
            "Implement a watchdog to resend lost internal prompts.",
            "Add periodic catch-up for missed events.",
            "Periodically reconcile internal events that were lost.",
            "Run a regular repair of dropped signals.",
            "Use a periodic sweep to recover events missed by herdr.",
            "Add a sweep for lost events. delay: external polling, the API",
        ]
        for text in prescriptions:
            for path in ("/x/TODO.md", "/x/DECISIONS.md", "/x/AGENTS.md"):
                with self.subTest(text=text, path=path):
                    self.assertTrue(write(path, text))
            self.assertTrue(bash("herdr worker start --prompt " + json.dumps(text)))

    def test_sweep_at_lifecycle_events_is_allowed(self):
        text = "Sweep missed internal events at startup and handoff, triggered by those lifecycle events."
        self.assertFalse(write("/x/TODO.md", text))
        self.assertFalse(bash("herdr worker start --prompt " + json.dumps(text)))

    def test_watchdog_at_producer_commit_is_allowed(self):
        text = "Use an event-driven watchdog to repair lost internal events when the producer commits progress."
        self.assertFalse(write("/x/TODO.md", text))
        self.assertFalse(bash("herdr worker start --prompt " + json.dumps(text)))

    def test_mechanism_names_do_not_make_event_contracts_periodic(self):
        for mechanism in ("sweep", "watchdog", "catch-up", "safety net"):
            for trigger in ("at startup", "at handoff", "at producing events",
                            "when the producer commits durable progress"):
                text = "Use a " + mechanism + " to recover lost internal events " + trigger + "."
                with self.subTest(text=text):
                    self.assertFalse(write("/x/TODO.md", text))

    def test_event_contract_does_not_excuse_recurring_recovery(self):
        for text in (
            "Sweep missed internal events at startup and handoff and every 5 minutes.",
            "Use an event-driven watchdog to repair lost internal events when the producer commits progress and every 5 minutes.",
            "Use a periodic watchdog rescan for lost internal events when the producer commits progress.",
            "Run a timer-driven sweep of missed internal events at startup and handoff.",
            "Use recurring catch-up for lost internal events at producing events.",
            "Use a safety net to recover lost internal events when the producer commits progress and every 5 minutes. delay: external polling, the API",
            "Use an event-driven safety net to recover lost internal events.",
        ):
            with self.subTest(text=text):
                self.assertTrue(write("/x/TODO.md", text))
                self.assertTrue(bash("herdr worker start --prompt " + json.dumps(text)))

    def test_positive_event_and_external_cases(self):
        for text in (
            "Use durable state as the source of truth for missed internal events.",
            "Persist the cursor before acknowledging internal events.",
            "Reconcile missed internal events at producing events, startup or handoff.",
            "Reconcile internal events when the producer commits durable progress.",
            "The external watchdog reports the provider's health.",
            "The external watchdog checks the provider every 5 minutes.",
            "Run cargo sweep to reclaim build artifacts.",
            "Poll every 5 s. delay: external polling, GitHub produces the remote status",
        ):
            with self.subTest(text=text):
                self.assertFalse(write("/x/TODO.md", text))
        self.assertFalse(bash("cargo sweep"))

    def test_historical_removed_and_rejected_prose(self):
        for text in (
            "Removed the 5-minute sweep for missed own events.",
            "Rejected a safety net to recover dropped notifications.",
            "Previously we used periodic catch-up for lost internal events.",
            "Historically the supervisor ran a 5-minute sweep for lost events.",
            "Remove the 30 s retry and use the acknowledgement instead.",
            "Removed `sleep 5` from the plan.",
            "Reject `time.sleep(5)` as a repair.",
            "Do not poll every 5 s for internal events.",
            "The coordinator polled a transcript every 5 s.",
        ):
            for path in ("/x/TODO.md", "/x/DECISIONS.md"):
                with self.subTest(text=text, path=path):
                    self.assertFalse(write(path, text))
        self.assertTrue(write("/x/TODO.md", "Removed a sweep, but add a 30 s retry."))
        self.assertTrue(write("/x/DECISIONS.md", "Use a 30 s retry."))
        self.assertTrue(write("/x/TODO.md", "Rejected a sweep. Add periodic repair for lost events."))
        self.assertTrue(write("/x/a.py", "# Removed old timing\ntime.sleep(30)"))

    def test_plan_added_only_and_heredoc(self):
        old = "Add a sweep for missed internal events.\nOther work."
        self.assertFalse(edit("/x/TODO.md", old, old + "\nEvent-driven handoff."))
        self.assertFalse(edit("/x/DECISIONS.md", old, "Other work."))
        self.assertTrue(edit("/x/TODO.md", "Other work.", old))
        with tempfile.TemporaryDirectory(dir=HERE) as tmp:
            path = os.path.join(tmp, "TODO.md")
            with open(path, "w") as handle:
                handle.write(old)
            self.assertFalse(write(path, old + "\nPersist the cursor."))
        self.assertTrue(bash("cat > /x/TODO.md <<'EOF'\nAdd periodic repair of lost events.\nEOF"))
        self.assertFalse(bash("cat > /x/DECISIONS.md <<'EOF'\nRejected a 5 s retry.\nEOF"))
        self.assertFalse(bash("rg 'periodic repair of lost events' TODO.md"))

    def test_invalid_markers_in_all_paths(self):
        for marker in ("sweep", "tooling", "safety net", "generic reason",
                       "self-event polling", "not a wait, display setting",
                       "external polling", "external polling, generic reason",
                       "external polling, missed own events"):
            with self.subTest(marker=marker):
                self.assertTrue(bash("sleep 5 # delay: " + marker))
                self.assertTrue(write("/x/a.py", "time.sleep(5) # delay: " + marker))
                self.assertTrue(edit("/x/a.py", "go()", "time.sleep(5) # delay: " + marker))
                self.assertTrue(bash("python3 -c " + json.dumps("time.sleep(5) # delay: " + marker)))
                self.assertTrue(bash("herdr worker prompt w " + json.dumps("Wait 5 s. delay: " + marker)))
                self.assertTrue(bash("cat > /x/a.py <<'EOF'\ntime.sleep(5) # delay: " + marker + "\nEOF"))

    def test_multiedit_marker_scope(self):
        for marker, blocked in (("tooling", True), ("rate limit, API policy", False)):
            result = delay_check.check({
                "tool_name": "MultiEdit",
                "tool_input": {"file_path": "/x/a.py", "edits": [
                    {"old_string": "go()", "new_string": "time.sleep(5) # delay: " + marker},
                ]},
            })
            self.assertEqual(bool(result), blocked)
        result = delay_check.check({
            "tool_name": "MultiEdit",
            "tool_input": {"file_path": "/x/a.py", "edits": [
                {"old_string": "go()", "new_string": "time.sleep(5) # delay: rate limit"},
                {"old_string": "next()", "new_string": "time.sleep(8)"},
            ]},
        })
        self.assertEqual(len(result), 1)

    def test_allowed_categories(self):
        for marker in ("real external deadline, provider expires the request",
                       "rate limit, API permits one call per minute",
                       "external polling, the remote builder publishes status",
                       "designed backoff, documented connection policy",
                       "measured slow hardware, measured disk latency"):
            with self.subTest(marker=marker):
                code = "time.sleep(5) # delay: " + marker
                self.assertFalse(write("/x/a.py", code))
                self.assertFalse(edit("/x/a.py", "go()", code))
                self.assertFalse(bash("sleep 5 # delay: " + marker))
                self.assertFalse(bash("python3 -c " + json.dumps(code)))
                self.assertFalse(bash("cat > /x/a.py <<'EOF'\n" + code + "\nEOF"))
                self.assertFalse(bash("herdr agent prompt w " + json.dumps("Wait 5 s. delay: " + marker)))

    def test_markers_do_not_excuse_unrelated_commands_or_tasks(self):
        for command in (
            "echo 'delay: rate limit'; sleep 5",
            "sleep 5 # delay: rate limit\nsleep 8",
            "sleep 8; sleep 5 # delay: rate limit",
            "python3 -c 'time.sleep(5) # delay: rate limit'; sleep 8",
            "herdr worker prompt w 'Wait 5 s. delay: rate limit'; sleep 8",
            "herdr worker prompt w 'delay: rate limit\nDone.\nDone.\nDone.\nWait 8 s.'",
            "herdr worker prompt w 'Wait 5 s. delay: rate limit; Wait 8 s.'",
            "herdr worker prompt w 'Wait 5 s. delay: rate limit\nWait 8 s.'",
            "herdr worker prompt w 'delay: rate limit\nWait 5 s.\nWait 8 s.'",
            "cat > /x/a.py <<'EOF'\n# delay: rate limit\ntime.sleep(5)\nEOF\nsleep 8",
        ):
            with self.subTest(command=command):
                self.assertTrue(bash(command))

    def test_code_is_not_exempted_by_prose_removal_wording(self):
        self.assertTrue(write("/x/a.py", "remove(timeout=30)"))
        self.assertTrue(bash("python3 -c 'remove(timeout=30)'"))

    def test_numeric_non_wait_settings(self):
        for content in ("max_fps = 60", "retry_count = 5", "batch_size = 30",
                        "window_width = 120", "stress_iterations = 40"):
            self.assertFalse(write("/x/config.py", content))


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
