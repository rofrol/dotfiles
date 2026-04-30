#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["lxml"]
# ///
"""
Extracts chapter timestamps from a Matroska chapters.xml and prints
them in a format ready for mkvmerge --split timestamps:...

Usage:
    uv run chapters-to-split.py chapters.xml
"""

import sys
from lxml import etree


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} chapters.xml", file=sys.stderr)
        sys.exit(1)

    tree = etree.parse(sys.argv[1])
    timestamps = [
        el.text.strip()
        for el in tree.findall(".//ChapterTimeStart")
        if not el.text.strip().startswith("00:00:00")
    ]

    # mkvmerge wants HH:MM:SS.nnn (milliseconds), not nanoseconds
    def trim_ts(ts: str) -> str:
        # 00:26:17.472000000 -> 00:26:17.472
        dot = ts.index(".")
        return ts[: dot + 4]

    print(",".join(trim_ts(ts) for ts in timestamps))


if __name__ == "__main__":
    main()
