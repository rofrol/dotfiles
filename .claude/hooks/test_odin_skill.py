#!/usr/bin/env python3
"""Tests of odin-skill.py: run `python3 -I ~/.claude/hooks/test_odin_skill.py`."""

import json
import os
import subprocess
import sys
import tempfile
import unittest

HOOK = os.path.join(os.path.dirname(os.path.abspath(__file__)), "odin-skill.py")


class OdinSkill(unittest.TestCase):
    def setUp(self):
        self.home = tempfile.mkdtemp()
        skill = os.path.join(self.home, ".claude/skills/odin")
        os.makedirs(skill)
        with open(os.path.join(skill, "SKILL.md"), "w") as f:
            f.write("PITFALLS")

    def run_hook(self, tool, inp, session="s1"):
        event = {"session_id": session, "tool_name": tool, "tool_input": inp}
        out = subprocess.run([sys.executable, "-I", HOOK], input=json.dumps(event),
                             capture_output=True, text=True, env={"HOME": self.home})
        self.assertEqual(out.returncode, 0, out.stderr)
        if not out.stdout:
            return None
        return json.loads(out.stdout)["hookSpecificOutput"]["additionalContext"]

    def test_first_odin_touch_gives_the_skill_once_per_session(self):
        self.assertIn("PITFALLS", self.run_hook("Read", {"file_path": "/r/a.odin"}))
        self.assertIsNone(self.run_hook("Edit", {"file_path": "/r/b.odin"}))
        self.assertIn("PITFALLS", self.run_hook("Read", {"file_path": "/r/a.odin"}, session="s2"))

    def test_odin_commands_count(self):
        for i, cmd in enumerate(["./odin test server", "odin build .", "cd x && ./odin run e2e/report",
                                 "ad job run --name check -- ./odin test proto",
                                 "/opt/homebrew/bin/odin version"]):
            self.assertIn("PITFALLS", self.run_hook("Bash", {"command": cmd}, session=f"c{i}"), cmd)

    def test_other_calls_do_not_count(self):
        cases = [("Read", {"file_path": "/r/a.py"}), ("Bash", {"command": "ls odin-tui"}),
                 ("Bash", {"command": "cat docs/odin.md"}), ("Bash", {"command": "git log -- odin"}),
                 ("Write", {"file_path": "/r/a.odin.txt"})]
        for tool, inp in cases:
            self.assertIsNone(self.run_hook(tool, inp), inp)
        self.assertIn("PITFALLS", self.run_hook("Read", {"file_path": "/r/a.odin"}))

    def test_a_loaded_skill_is_not_given_again(self):
        self.assertIsNone(self.run_hook("Skill", {"skill": "odin"}))
        self.assertIsNone(self.run_hook("Read", {"file_path": "/r/a.odin"}))

    def test_a_missing_skill_never_blocks(self):
        os.remove(os.path.join(self.home, ".claude/skills/odin/SKILL.md"))
        self.assertIsNone(self.run_hook("Read", {"file_path": "/r/a.odin"}))


if __name__ == "__main__":
    unittest.main()
