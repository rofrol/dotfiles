# ~/scripts

Small personal tools on `PATH`. Usage of each tool is in its `--help` / docstring.

## Music (MPD) tools

They moved to [rormpc-tools](https://github.com/rofrol/rormpc-tools) (`hits`, `musicdb`, `mpd-gap`, `yt-mp3-mb`,
`yt-playlist`), installed with rormpc's `scripts/rormpc_install.sh companions` (`--local` from
`~/personal_projects/rormpc-tools`, editable). My settings: `~/.config/rormpc-tools/config.toml` (here). Play
history stays in the private repo `~/personal_projects/music-data`, never here: this repo is public.

## yt-dlp

`yt-dlp` itself comes from Homebrew (`brew upgrade yt-dlp`). My extractors are a yt-dlp plugin, cloned from the private
`rofrol/yt-dlp-plugins` into `~/.config/yt-dlp/plugins/rofrol` and updated with
`git -C ~/.config/yt-dlp/plugins/rofrol pull --ff-only`; brew does not touch it.

`yt-dlp-master` runs upstream master in an isolated `uvx` environment, for the days when a YouTube countermeasure is
fixed in master before brew's release catches up. It resolves master on each run (~1 s once cached), uses the same
config and plugin, and leaves the brew installation alone. Its `--version` still prints the last released version:
upstream bumps that only at release time, so it does not say which commit is running.
