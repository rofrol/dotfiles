#!/usr/bin/env python3
"""Test atomic video publication with event-gated mocks and real FFmpeg."""
import os
from pathlib import Path
import signal
import struct
import subprocess
import tempfile

SCRIPT = Path(__file__).with_name("video_for_whatsapp.sh")


def run(args, **kwargs):
    result = subprocess.run(args, capture_output=True, text=True, **kwargs)
    assert result.returncode == 0, (args, result.stdout, result.stderr)
    return result


def main():
    with tempfile.TemporaryDirectory(prefix="video-publication-") as directory:
        root = Path(directory)
        mocks = root / "bin"
        mocks.mkdir()
        probe = mocks / "ffprobe"
        probe.write_text('''#!/usr/bin/env python3
import os, sys
args = " ".join(sys.argv)
if "codec_name" in args:
    print("aac" if "a:0" in args else os.environ.get("TEST_CODEC", "h264"))
elif "pix_fmt" in args:
    print("yuv420p")
else:
    print("192000")
''')
        encoder = mocks / "ffmpeg"
        encoder.write_text('''#!/usr/bin/env python3
from pathlib import Path
import os, sys
assert sys.argv[-3:-1] == ["-f", "mp4"], sys.argv
assert sys.argv[-1].endswith(".temp"), sys.argv
output = Path(sys.argv[-1])
output.write_text("partial")
print("ENCODER_READY " + str(output), flush=True)
sys.stdin.readline()
if os.environ.get("TEST_FAIL") == "1":
    sys.exit(23)
output.write_text("complete")
''')
        # Pin the second so concurrent tests prove PID-based separation.
        clock = mocks / "date"
        clock.write_text('#!/bin/sh\nprintf "%s\\n" "20261013-120000"\n')
        clock.chmod(0o755)
        probe.chmod(0o755)
        encoder.chmod(0o755)
        env = dict(os.environ, PATH=f"{mocks}:{os.environ['PATH']}")
        source = root / "input with spaces.mkv"
        source.write_text("source")
        output = root / "output with spaces.mp4"

        def launch(options=(), **changes):
            process = subprocess.Popen(
                ["/bin/bash", str(SCRIPT), *options, "-o", str(output), str(source)],
                env=dict(env, **changes), stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
                start_new_session=True,
            )
            for _ in range(100):
                line = process.stdout.readline()
                if line.startswith("ENCODER_READY "):
                    process.stage = Path(line.strip().split(" ", 1)[1])
                    assert process.stage.parent == output.parent
                    assert process.stage.name.startswith(output.name + ".")
                    assert process.stage.name.endswith(f".{process.pid}.temp")
                    return process
                assert line, process.communicate()
            raise AssertionError("Encoder handshake was not produced")

        def finish(process, expected=0):
            stdout, stderr = process.communicate("publish\n")
            assert process.returncode == expected, (stdout, stderr)
            assert not process.stage.exists()
            assert not list(root.glob("*.lock"))

        # All modes keep the final path absent until FFmpeg reports completion.
        for options, codec in [((), "h264"), ((), "vp9"), (("--30fps",), "h264")]:
            process = launch(options, TEST_CODEC=codec)
            assert process.stage.read_text() == "partial" and not output.exists()
            finish(process)
            assert output.read_text() == "complete"
            output.unlink()

        # An old output remains intact while working and after an encoder error.
        output.write_text("old")
        process = launch(TEST_FAIL="1")
        assert output.read_text() == "old"
        finish(process, 23)
        assert output.read_text() == "old"

        # Concurrent runs have private files; publishing one leaves the other
        # untouched, and cleanup of a failed run preserves the published output.
        process = launch()
        competitor = launch(TEST_FAIL="1")
        assert process.stage != competitor.stage
        assert process.stage.read_text() == competitor.stage.read_text() == "partial"
        assert output.read_text() == "old"
        finish(process)
        assert competitor.stage.read_text() == "partial"
        assert output.read_text() == "complete"
        finish(competitor, 23)
        assert output.read_text() == "complete"

        # Either successful run may publish last, without sharing its stage.
        process = launch()
        competitor = launch()
        finish(competitor)
        assert process.stage.read_text() == "partial"
        process.stage.write_text("still private")
        assert output.read_text() == "complete"
        finish(process)
        assert output.read_text() == "complete"

        # Terminating the process group models cancellation, including FFmpeg.
        output.write_text("old")
        process = launch()
        os.killpg(process.pid, signal.SIGTERM)
        process.communicate()
        assert process.returncode == 143
        assert output.read_text() == "old"
        assert not process.stage.exists()

        # Other runs' leftovers are not deleted or mistaken for our own stage.
        leftover = Path(f"{output}.20000101-000000.1.temp")
        leftover.write_text("leftover")
        dangling = Path(f"{output}.20000101-000000.2.temp")
        missing = root / "missing-symlink-target"
        dangling.symlink_to(missing)
        process = launch()
        finish(process)
        assert leftover.read_text() == "leftover"
        assert dangling.is_symlink() and not missing.exists()
        leftover.unlink()
        dangling.unlink()

        # Even an exact timestamp/PID collision must not overwrite a leftover
        # or follow a dangling symlink. exec preserves the wrapper shell's PID.
        for creation in ['printf leftover > "$stage"', 'ln -s "$4" "$stage"']:
            rejected = subprocess.run(
                ["/bin/bash", "-c",
                 'stage="$1.20261013-120000.$$.temp"; ' + creation
                 + '; printf "%s\\n" "$stage"; exec /bin/bash "$2" -o "$1" "$3"',
                 "bash", str(output), str(SCRIPT), str(source), str(missing)],
                env=env, capture_output=True, text=True,
            )
            assert rejected.returncode != 0
            collision = Path(rejected.stdout.splitlines()[0])
            if collision.is_symlink():
                assert not missing.exists()
            else:
                assert collision.read_text() == "leftover"
            collision.unlink()

        # Source aliases are still rejected without modifying the input.
        alias = root / "source-alias.mp4"
        alias.symlink_to(source)
        rejected = subprocess.run(
            ["/bin/bash", str(SCRIPT), "-o", str(alias), str(source)],
            env=env, capture_output=True, text=True,
        )
        assert rejected.returncode != 0 and source.read_text() == "source"

        # Real encodes verify explicit MP4 muxing, faststart and decodability.
        h264 = root / "h264.mp4"
        vp9 = root / "vp9.mkv"
        for path, codec in [(h264, "libx264"), (vp9, "libvpx-vp9")]:
            run(["ffmpeg", "-v", "error", "-f", "lavfi", "-i",
                 "testsrc2=size=64x64:rate=30", "-t", "0.2", "-c:v", codec,
                 "-pix_fmt", "yuv420p", str(path)])
        for index, (path, options) in enumerate([(h264, []), (vp9, []), (h264, ["--30fps"])]):
            target = root / f"real-{index}.mp4"
            run(["/bin/bash", str(SCRIPT), *options, "-o", str(target), str(path)])
            run(["ffmpeg", "-v", "error", "-xerror", "-i", str(target), "-f", "null", "-"])
            atoms = []
            with target.open("rb") as file:
                while header := file.read(8):
                    size, kind = struct.unpack(">I4s", header)
                    assert size >= 8
                    atoms.append(kind)
                    file.seek(size - 8, 1)
            assert atoms.index(b"moov") < atoms.index(b"mdat")
            assert not list(root.glob(f"{target.name}.*.temp"))
            assert not list(root.glob("*.lock"))
        run(["/bin/bash", str(SCRIPT), str(h264), str(vp9)])
        assert (root / "h264_wa.mp4").exists() and (root / "vp9_wa.mp4").exists()
    print("PASS: publication, failure, cancellation, concurrency, aliases, all modes and batch")


if __name__ == "__main__":
    main()
