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
print("ENCODER_READY", flush=True)
sys.stdin.readline()
if os.environ.get("TEST_FAIL") == "1":
    sys.exit(23)
output.write_text("complete")
''')
        probe.chmod(0o755)
        encoder.chmod(0o755)
        env = dict(os.environ, PATH=f"{mocks}:{os.environ['PATH']}")
        source = root / "input with spaces.mkv"
        source.write_text("source")
        output = root / "output with spaces.mp4"
        stage = Path(f"{output}.temp")
        lock = Path(f"{stage}.lock")

        def launch(options=(), **changes):
            process = subprocess.Popen(
                ["/bin/bash", str(SCRIPT), *options, "-o", str(output), str(source)],
                env=dict(env, **changes), stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
                start_new_session=True,
            )
            for _ in range(100):
                line = process.stdout.readline()
                if line.strip() == "ENCODER_READY":
                    return process
                assert line, process.communicate()
            raise AssertionError("Encoder handshake was not produced")

        def finish(process, expected=0):
            stdout, stderr = process.communicate("publish\n")
            assert process.returncode == expected, (stdout, stderr)
            assert not stage.exists() and not lock.exists()

        # All modes keep the final path absent until FFmpeg reports completion.
        for options, codec in [((), "h264"), ((), "vp9"), (("--30fps",), "h264")]:
            process = launch(options, TEST_CODEC=codec)
            assert stage.read_text() == "partial" and not output.exists()
            finish(process)
            assert output.read_text() == "complete"
            output.unlink()

        # An old output remains intact while working and after an encoder error.
        output.write_text("old")
        process = launch(TEST_FAIL="1")
        assert output.read_text() == "old"
        finish(process, 23)
        assert output.read_text() == "old"

        # A competing run must not touch the first run's temporary file or lock.
        process = launch()
        competitor = subprocess.run(
            ["/bin/bash", str(SCRIPT), "-o", str(output), str(source)],
            env=env, capture_output=True, text=True,
        )
        assert competitor.returncode != 0
        assert stage.read_text() == "partial" and lock.is_dir()
        assert output.read_text() == "old"
        finish(process)
        assert output.read_text() == "complete"

        # Terminating the process group models cancellation, including FFmpeg.
        output.write_text("old")
        process = launch()
        os.killpg(process.pid, signal.SIGTERM)
        process.communicate()
        assert process.returncode == 143
        assert output.read_text() == "old"
        assert not stage.exists() and not lock.exists()

        # Refuse stale temporary files and source aliases without deleting them.
        stage.write_text("leftover")
        rejected = subprocess.run(
            ["/bin/bash", str(SCRIPT), "-o", str(output), str(source)],
            env=env, capture_output=True, text=True,
        )
        assert rejected.returncode != 0 and stage.read_text() == "leftover"
        assert not lock.exists()
        stage.unlink()
        missing = root / "missing-symlink-target"
        stage.symlink_to(missing)
        rejected = subprocess.run(
            ["/bin/bash", str(SCRIPT), "-o", str(output), str(source)],
            env=env, capture_output=True, text=True,
        )
        assert rejected.returncode != 0 and stage.is_symlink()
        assert not missing.exists() and not lock.exists()
        stage.unlink()
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
            assert not Path(f"{target}.temp").exists()
            assert not Path(f"{target}.temp.lock").exists()
        run(["/bin/bash", str(SCRIPT), str(h264), str(vp9)])
        assert (root / "h264_wa.mp4").exists() and (root / "vp9_wa.mp4").exists()
    print("PASS: publication, failure, cancellation, concurrency, aliases, all modes and batch")


if __name__ == "__main__":
    main()
