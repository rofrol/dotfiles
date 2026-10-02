# ~/scripts

Small personal tools on `PATH`. Usage of each tool is in its `--help` / docstring.

## Music (MPD) tools

How they fit together: https://github.com/kisswiki/kisswiki/blob/master/src/os/macos/mpd.md

| Tool | What it does |
|---|---|
| `yt-mp3-mb` | YouTube → mp3, identified on MusicBrainz, tagged, cover embedded |
| `mbtag.py` | shared library for the tools below (MusicBrainz/AcoustID/ListenBrainz lookups, tagging, covers) |
| `musicdb` | play history → MPD stickers; run hourly by `~/Library/LaunchAgents/com.rofrol.musicdb.plist` |
| `hits` | decade hits (Billboard year-end) with a genre filter → MPD playlists |
| `yt-playlist` | my YouTube playlists via the official Data API (OAuth); used by `musicdb deletions --confirm` |

Invariants to keep when changing them:

- The `plays` sticker is space-padded on purpose: rmpc sorts sticker values as text. Do not "clean up" the padding.
- `mbtag.http()` returns `None` on 404 and, by default, when retries run out; with `strict=True` it raises instead.
  `musicdb import-lb` uses `strict=True` so a failed page is never read as "end of history" (a silent partial import).
- Deleting is two-phase: `musicdb delete` (rmpc Ctrl-x) only trashes and queues; ListenBrainz listens are deleted
  by `musicdb deletions --confirm`, never by a key. Deleted events become tombstones (kept, and re-imports skip them).
  Listens of a recording that another library file still has are not deleted.
- Likes: rmpc's `like` sticker is the source of truth; only changes go to ListenBrainz (ledger `lb_feedback`), and a
  song without a like sticker never clears LB feedback.
- Private data (play history, exports, backups) lives in a private repo (`$MUSICDB_DATA`, default
  `~/personal_projects/music-data`), never here: this repo is public. The ListenBrainz token stays in the
  listenbrainz-mpd config, read at runtime.
