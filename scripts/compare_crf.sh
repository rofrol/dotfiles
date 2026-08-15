#!/usr/bin/env bash
#
# compare_crf.sh
#
# Purpose:
#   Encodes a SHORT test segment of a video at several CRF values so you
#   can compare quality and resulting file size before committing to a
#   full-length encode with fix_video_for_whatsapp.sh.
#
# For each CRF value it reports:
#   - the test segment's file size
#   - an ESTIMATED full-video file size (scaled from the segment)
#   - a VMAF score (0-100, higher = closer to the reference; ~95+ is
#     considered visually indistinguishable for most viewers)
#   - one extracted PNG frame per CRF, so you can eyeball the difference
#
# How it works:
#   1. Cuts an exact, frame-accurate reference segment from the source
#      and re-encodes it losslessly (FFV1) so every CRF test below
#      starts from IDENTICAL frames — this keeps the VMAF comparison
#      valid (misaligned frames give meaningless VMAF scores).
#   2. Encodes that lossless reference segment to H.264 at each
#      requested CRF (same settings as fix_video_for_whatsapp.sh).
#   3. Runs ffmpeg's libvmaf filter to score each CRF encode against
#      the lossless reference.
#   4. Extracts one comparison frame per encode.
#
# Requirements:
#   - ffmpeg built with --enable-libvmaf (already the case per your
#     `ffmpeg -version` output)
#
# Usage:
#   ./compare_crf.sh input.mkv [options]
#
# Options:
#   --start TIME       Timestamp to start the test segment (default 00:10:00)
#   --duration SEC      Length of the test segment in seconds (default 30)
#   --crfs "18 20 22"   Space-separated CRF values to test (default "18 20 22 24")
#   --preset NAME        x264 preset (default slow, same as the main script)
#   --frame-offset SEC  Second within the segment to grab the comparison
#                        frame from (default 5)
#   --outdir DIR         Where to write test files (default: ./crf_test
#                        next to the input file)

set -euo pipefail

START="00:10:00"
DURATION="30"
CRFS="18 20 22 24"
PRESET="slow"
FRAME_OFFSET="5"
OUTDIR=""
INPUT=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --start) START="$2"; shift 2 ;;
        --duration) DURATION="$2"; shift 2 ;;
        --crfs) CRFS="$2"; shift 2 ;;
        --preset) PRESET="$2"; shift 2 ;;
        --frame-offset) FRAME_OFFSET="$2"; shift 2 ;;
        --outdir) OUTDIR="$2"; shift 2 ;;
        *) INPUT="$1"; shift ;;
    esac
done

if [[ -z "$INPUT" ]]; then
    echo "Usage: $0 <input_video> [--start TIME] [--duration SEC] [--crfs \"18 20 22\"] [--preset NAME] [--frame-offset SEC] [--outdir DIR]" >&2
    exit 1
fi

if [[ ! -f "$INPUT" ]]; then
    echo "Error: input file not found: $INPUT" >&2
    exit 1
fi

if ! command -v ffmpeg >/dev/null 2>&1 || ! command -v ffprobe >/dev/null 2>&1; then
    echo "Error: ffmpeg/ffprobe not found. Install with: brew install ffmpeg" >&2
    exit 1
fi

if [[ -z "$OUTDIR" ]]; then
    DIR="$(dirname "$INPUT")"
    OUTDIR="${DIR}/crf_test"
fi
mkdir -p "$OUTDIR"

# Total source duration, used later to scale the test segment's file
# size up to an estimate for the full video.
TOTAL_DURATION=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$INPUT")

echo "Source: $INPUT"
echo "Test segment: ${DURATION}s starting at ${START}"
echo "CRF values to test: $CRFS"
echo "Preset: $PRESET"
echo "Output folder: $OUTDIR"
echo ""

# --- Step 1: frame-accurate lossless reference segment ---
# Coarse seek before -i (fast), fine seek after -i (frame-accurate),
# then re-encode losslessly with FFV1 so all CRF tests below share
# EXACTLY the same source frames.
REF="${OUTDIR}/reference_lossless.mkv"
echo "Extracting frame-accurate lossless reference segment..."
ffmpeg -y -ss "$START" -i "$INPUT" -t "$DURATION" \
    -c:v ffv1 -pix_fmt yuv420p \
    -c:a pcm_s16le \
    "$REF" -loglevel error

REF_FRAME_PNG="${OUTDIR}/frame_reference.png"
ffmpeg -y -ss "$FRAME_OFFSET" -i "$REF" -frames:v 1 "$REF_FRAME_PNG" -loglevel error

# --- Step 2: encode the reference segment at each CRF, score it ---
printf "%-6s %-14s %-20s %-10s\n" "CRF" "Segment size" "Est. full-file size" "VMAF"
printf "%-6s %-14s %-20s %-10s\n" "---" "------------" "--------------------" "----"

for CRF in $CRFS; do
    OUT_MP4="${OUTDIR}/test_crf${CRF}.mp4"
    OUT_FRAME="${OUTDIR}/frame_crf${CRF}.png"

    ffmpeg -y -i "$REF" \
        -c:v libx264 -profile:v high -level 4.1 \
        -preset "$PRESET" -crf "$CRF" \
        -pix_fmt yuv420p \
        -c:a aac -b:a 192k \
        -movflags +faststart \
        "$OUT_MP4" -loglevel error

    ffmpeg -y -ss "$FRAME_OFFSET" -i "$OUT_MP4" -frames:v 1 "$OUT_FRAME" -loglevel error

    SEG_SIZE_BYTES=$(stat -f%z "$OUT_MP4" 2>/dev/null || stat -c%s "$OUT_MP4")
    SEG_SIZE_MB=$(echo "scale=1; $SEG_SIZE_BYTES / 1024 / 1024" | bc)
    EST_FULL_MB=$(echo "scale=1; $SEG_SIZE_BYTES * $TOTAL_DURATION / $DURATION / 1024 / 1024" | bc)

    # VMAF: first input is the distorted (encoded) stream, second is
    # the reference. Score is logged to a small JSON file we then read.
    VMAF_LOG="${OUTDIR}/vmaf_crf${CRF}.json"
    ffmpeg -y -i "$OUT_MP4" -i "$REF" \
        -lavfi "[0:v]setpts=PTS-STARTPTS[dist];[1:v]setpts=PTS-STARTPTS[ref];[dist][ref]libvmaf=log_fmt=json:log_path=${VMAF_LOG}" \
        -f null - -loglevel error

    VMAF_SCORE=$(python3 -c "import json; d=json.load(open('${VMAF_LOG}')); print(round(d['pooled_metrics']['vmaf']['mean'], 2))" 2>/dev/null || echo "n/a")

    printf "%-6s %-14s %-20s %-10s\n" "$CRF" "${SEG_SIZE_MB} MB" "${EST_FULL_MB} MB" "$VMAF_SCORE"
done

echo ""
echo "Comparison frames written to: $OUTDIR"
echo "  frame_reference.png   <- original quality (ground truth)"
for CRF in $CRFS; do
    echo "  frame_crf${CRF}.png       <- H.264 CRF ${CRF}"
done
echo ""
echo "Open these side by side (e.g. in Preview/QuickLook) to eyeball the difference,"
echo "and use the VMAF/size table above to pick a CRF. VMAF ~95+ is generally"
echo "considered visually transparent; below ~90 differences become noticeable."
echo ""
echo "Once you've picked a CRF, run the full encode with:"
echo "  ./fix_video_for_whatsapp.sh \"$INPUT\" --crf <chosen_value> --preset $PRESET"
