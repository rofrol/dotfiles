#!/bin/sh
# Machine-local git setup for the dotfiles repo (not stored in the repo itself).
# Idempotent: safe to rerun.
#
# ~/.claude/settings.json is tracked, but Claude Code writes an "autoMode"
# block (machine/trust description) into it that must never reach the public
# repo. A clean filter strips it on add/status; a pre-commit hook rejects a
# staged blob that still has it (e.g. filter not configured).
set -eu

gd=${DOTFILES_HOME:-$HOME/personal_projects/dotfiles}
g() { git --git-dir="$gd" "$@"; }

command -v jq >/dev/null || { echo "jq is required" >&2; exit 1; }

g config filter.strip-automode.clean "jq 'del(.autoMode)'"
g config filter.strip-automode.smudge cat
# fail closed: if jq fails, git add fails instead of staging the raw file
g config filter.strip-automode.required true

attr='.claude/settings.json filter=strip-automode'
mkdir -p "$gd/info"
grep -qxF "$attr" "$gd/info/attributes" 2>/dev/null || echo "$attr" >> "$gd/info/attributes"

cat > "$gd/hooks/pre-commit" <<'EOF'
#!/bin/sh
# Never commit Claude Code's auto-generated autoMode block (repo is public).
# Installed by ~/scripts/dotfiles-setup-filters.sh
if git cat-file -e :.claude/settings.json 2>/dev/null &&
   git show :.claude/settings.json | jq -e 'has("autoMode")' >/dev/null; then
  echo "pre-commit: autoMode in staged .claude/settings.json, aborting" >&2
  exit 1
fi
EOF
chmod +x "$gd/hooks/pre-commit"

echo "dotfiles filters and hooks installed in $gd"
