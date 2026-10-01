#!/bin/bash
# Backup MacBook 2010 → Samsung SSD T5
# Usage: ./macbook-2010-to-ssd-t5-backup.sh [--dry-run]
#
# Prerequisites:
# - MacBook 2010 disk mounted at /Volumes/MacBook2010 (or specify SOURCE)
# - Samsung SSD T5 mounted at /Volumes/SamsungT5 (or specify DEST)
# - rsync installed

set -euo pipefail

SOURCE="${1:--/Volumes/MacBook2010}"
DEST="${2:-/Volumes/SamsungT5/backup/MacBook2010}"
DRY_RUN=""

# Check for --dry-run flag
[[ "${3:-}" == "--dry-run" ]] && DRY_RUN="-n"

# Validate source exists
if [[ ! -d "$SOURCE" ]]; then
    echo "Error: Source directory not found: $SOURCE"
    echo "Usage: $0 [SOURCE] [DEST] [--dry-run]"
    exit 1
fi

# Create destination if doesn't exist
mkdir -p "$DEST"

# Log file
LOG_FILE="${DEST}/backup-log-$(date +%Y%m%d-%H%M%S).txt"

echo "=== MacBook 2010 → Samsung SSD T5 Backup ==="
echo "Source: $SOURCE"
echo "Destination: $DEST"
[[ -n "$DRY_RUN" ]] && echo "Mode: DRY RUN (no actual copying)"
echo "Log file: $LOG_FILE"
echo "---"

# rsync flags:
# -a          archive mode (permissions, times, ownership)
# -x          skip files on different filesystems
# -v          verbose
# -z          compress during transfer (saves I/O)
# -W          copy whole files (faster for local, avoid delta)
# -c          skip based on checksum, not modification time
# --delete    delete files on dest if missing on source
# --info=progress2  progress information
# --exclude   skip specified patterns

time rsync $DRY_RUN \
  -axvzW --delete --info=progress2 \
  -c \
  --exclude='.DS_Store' \
  --exclude='*.tmp' \
  --exclude='.Trash' \
  --exclude='Library/Caches' \
  --exclude='Library/Logs' \
  "$SOURCE/" "$DEST/" \
  2>&1 | tee "$LOG_FILE"

echo ""
echo "=== Backup complete ==="
echo "Log saved to: $LOG_FILE"

# Summary
if [[ -z "$DRY_RUN" ]]; then
    SOURCE_SIZE=$(du -sh "$SOURCE" | cut -f1)
    DEST_SIZE=$(du -sh "$DEST" | cut -f1)
    echo "Source size: $SOURCE_SIZE"
    echo "Backup size: $DEST_SIZE"
fi
