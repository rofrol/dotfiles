---
name: macbook-2010
description: Use the owner's 2010 MacBookPro7,1 (x86_64 Guix System on the LAN) from the M1 Mac — for x86_64 work the M1 cannot do (building or running x86_64 Guix packages, images or VMs), and as an archive to free the M1's disk (find what is big with gdu-go, move it there with rsync and verify before deleting, copy it back). Use when a task needs x86_64, when the M1's disk is nearly full, or when the user asks to move or fetch files to or from the MacBook.
---

# The 2010 MacBook: x86_64 work and an archive for the M1

The machine's rules live in two documents; read them before acting, and do
not copy them here:

- `~/personal_projects/guix/guix-x86_64-iso-omguix/docs/macbook.md`: how to
  reach it (`mb-ssh`, `mb-run`, `MB_HOST`, its known_hosts file), long
  commands through `./mb-run NAME -- COMMAND` inside one `herdr-job` (the
  WiFi ssh drops), one `guix time-machine` at a time, no `sudo` in a
  herdr-job, no reboot or bootloader change without the user.
- `~/personal_projects/guix/macbook-guix-config/README.md`: its system
  configuration and the Roguix pin.

Facts: user `roman`, sshd on port 42022 (key only), mDNS name
`macbook.local`, which agent shells resolve only sometimes; then use its LAN
IP (192.168.1.69 on 2026-10-16; check the router or ask the user if it
changed). Core 2 Duo without AES-NI, 15 GB RAM, about 400 GB free on its
ext4 SSD. Its rsync is 3.5.0, the M1's (Homebrew) 3.5.1.

Before using it: `ssh -n -p 42022 roman@HOST 'pgrep -af guix'`, and tell the
guix-x86_64-iso-omguix coordinator (it owns the machine's work queue).

## x86_64 work

The M1's aarch64 builders cannot build or run x86_64 (no emulation there),
so x86_64 Guix builds, images and VMs go to the MacBook, through `mb-run`
as the document above says. Results come back with rsync (below).

## Freeing the M1's disk: find, move, verify, delete

Decided by the user 2026-10-16 (consult round 20261016-182347-eaad, five
models): gdu-go non-interactively to find, rsync to copy, an rsync checksum
pass to verify, and deletion only after that pass is clean.

1. Find what is big, without the interactive UI:

       gdu-go -n -p -x --top 30 ~

   `-n` non-interactive, `-p` no progress, `-x` stays on one filesystem,
   `-i DIR,DIR` / `-I PATTERN` ignore paths. The default size is disk usage,
   which predicts what deleting frees better than `-a` (apparent size); on
   APFS neither is exact (below). gdu-go's list is for reading; build the
   file list with `find -print0`.
2. Agree with the user what to move. Their own data (photos, films,
   documents) moves only when they name it; caches other projects share
   (`~/.cache/*`) and other repositories' build trees only after telling
   their coordinators. Keep folders macOS or apps rely on: move the
   contents of `~/Screenshots` (the screenshot location), never `~/Library`,
   `~/Desktop`, `~/Documents` or an app's live data. Quit the app or VM that
   writes a file first; `lsof +D DIR` shows open files.
3. Write the list, NUL-separated, paths relative to `~`, kept outside the
   moved tree (here `LIST`):

       cd ~ && find Movies Downloads -type f \( -iname '*.mov' -o -iname '*.mp4' \) -print0 > LIST

4. Copy (the archive is `~/m1-archive/` there, same relative paths):

       ssh -n -p 42022 roman@HOST 'umask 077; mkdir -p m1-archive'
       rsync -aHS --whole-file --partial-dir=.rsync-partial --protect-args \
         --from0 --files-from=LIST --info=progress2 \
         -e 'ssh -p 42022 -c chacha20-poly1305@openssh.com' \
         ./ roman@HOST:m1-archive/

   - `--whole-file`: new files gain nothing from the delta algorithm, whose
     rolling checksum is the Core 2 Duo's bottleneck.
   - No `--checksum` here: on a first copy it only makes both sides read
     everything twice; it belongs in the verification pass.
   - No `-z`: films, photos and images do not compress and the old CPU
     would be the bottleneck.
   - ChaCha20 because the MacBook has no AES-NI.
   - `-S` keeps VM disk images sparse; `-H` keeps hard links.
   - `--partial-dir` keeps a file cut off by a WiFi drop for the rerun.
     Not `--append-verify`: it assumes files only grow, and conflicts with
     `-S`.
   - `-X`, `-A` and `-N` are left out: macOS extended attributes, ACLs,
     resource forks and creation times do not round-trip through ext4. Pack
     files whose macOS metadata matters into an archive first
     (`ditto -c -k --sequesterRsrc --keepParent DIR DIR.zip`) and move that.
   - `-a` keeps modes (a private key stays `600`); the archive is created
     with umask 077.
   - `--files-from` turns off the recursion `-a` implies: list files, not
     directories.
5. Verify: the same command with `--checksum --dry-run --itemize-changes`
   in place of `--info=progress2`. Both sides re-read every byte; it must
   exit 0 and print nothing. Anything it prints is a file that differs or
   is missing: rerun the copy and verify again.
   For irreplaceable data, also keep an independent SHA-256 manifest: one
   ssh, not one per file, and `ssh -n` (or `< /dev/null`) whenever ssh runs
   inside a loop that reads stdin, or it eats the loop's input (on
   2026-10-16 a per-file loop without `-n` checked one file of 738):

       ssh -p 42022 roman@HOST 'cd m1-archive && xargs -0 sha256sum' < LIST > remote.sums
       (cd ~ && xargs -0 shasum -a 256 < LIST) > local.sums
       diff local.sums remote.sums

6. Delete the local copies only after a clean verification, NUL-safely:

       cd ~ && xargs -0 rm -- < LIST

   Leave the directories; remove one only when it is empty and nothing
   (an app setting, a TCC grant) points at it.
7. Check what was freed with `df -h ~`, not `du`: APFS clones (`cp -c`)
   share blocks, so deleting one frees little, and local Time Machine
   snapshots hold deleted data (`tmutil listlocalsnapshots /`). Say what
   moved, where, and the free space before and after.

On an ssh drop (`Broken pipe`), rerun the same rsync once ssh answers
again (`ssh -n -p 42022 roman@HOST true`); rsync is idempotent and skips
what arrived. Bound the reruns (three); then report instead of retrying.
Run long transfers inside `herdr-job run`, never detached.

## Copying back

The same command with source and destination swapped, without deleting the
archive copy:

    rsync -aHS --whole-file --partial-dir=.rsync-partial --protect-args \
      --info=progress2 -e 'ssh -p 42022 -c chacha20-poly1305@openssh.com' \
      roman@HOST:m1-archive/PATH ./PATH

Verify it with the `--checksum --dry-run --itemize-changes` pass before
the user decides whether the archive copy may go.
