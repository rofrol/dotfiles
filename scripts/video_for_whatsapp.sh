#!/usr/bin/env bash
#
# video_for_whatsapp.sh
#
# Purpose:
#   Takes a video file and re-packages/re-encodes it so that:
#     - WhatsApp Desktop (macOS) recognizes and previews it as a video
#       (not as a generic file attachment)
#     - macOS Finder generates a proper thumbnail
#   ...while preserving audio quality and, above all, preserving VISUAL
#   quality (not just matching the source's numeric bitrate).
#
# Why this is needed:
#   WhatsApp Desktop and macOS Finder's QuickLook thumbnailer both expect
#   a fairly "boring" MP4:
#     - Video codec: H.264 (avc1), yuv420p pixel format, no exotic profiles
#     - Audio codec: AAC
#     - moov atom at the START of the file ("faststart"), so players and
#       thumbnailers can read metadata without scanning the whole file
#   Files that fail on any of these points (HEVC/H.265, 10-bit color,
#   VP9/AV1, odd containers, moov atom at the end, etc.) often get shown
#   by WhatsApp as a plain file icon, and Finder shows a blank/gray
#   thumbnail instead of a real preview frame.
#
# Why CRF instead of bitrate-matching:
#   Modern codecs (AV1, HEVC, VP9) are significantly more bit-efficient
#   than H.264 — AV1 roughly 2x, HEVC roughly 1.4x, content-dependent.
#   Forcing H.264 to the SAME numeric bitrate as an AV1/HEVC source
#   therefore produces a visibly worse result (blockiness, banding),
#   even though the file size matches. There is no fixed multiplier that
#   is exactly right for every scene (text overlays and sharp graphics
#   widen the gap further). The only way to actually preserve perceptual
#   quality is to let x264 target a constant quality level (CRF) and
#   allocate however many bits each scene needs, instead of a fixed
#   average. This is the default strategy below.
#
# Strategy:
#   1. Inspect the source file with ffprobe.
#   2. If video is already H.264 + yuv420p and audio is already AAC,
#      just REMUX (copy streams, no re-encode) and move the moov atom
#      to the front. This is fast and 100% lossless.
#   3. Otherwise, RE-ENCODE video to H.264 using CRF (constant quality),
#      with a generous VBV ceiling (-maxrate/-bufsize) only as a safety
#      net against runaway bitrate on pathological content, force
#      yuv420p, and re-encode audio to AAC at a quality that matches
#      (or exceeds) the source audio bitrate.
#
# Requirements:
#   - ffmpeg and ffprobe installed (e.g. `brew install ffmpeg` on macOS)
#
# Usage:
#   video_for_whatsapp.sh [--crf N] [--preset NAME] [--30fps] [-o OUT] input...
#
#   --crf N        Constant Rate Factor, lower = higher quality/bigger file.
#                   Default: 18 (visually near-lossless for x264).
#                   Typical range: 16 (very high quality) .. 23 (smaller file).
#   --preset NAME  x264 preset (compression effort vs. encode speed).
#                   Default: slow. Options (fastest→slowest, worst→best
#                   compression): ultrafast, superfast, veryfast, faster,
#                   fast, medium, slow, slower, veryslow.
#   --30fps        Small and fast instead of faithful: always re-encode to
#                   30 fps, 1280 px wide, AAC 128k. Defaults become
#                   --crf 23 --preset veryfast. Output "<name>.whatsapp.30fps.mp4".
#   -o OUT         Output path; only with a single input.
#
# Without -o the script writes "<input_basename>_wa.mp4" next to each input.
# Several inputs (e.g. a Finder selection passed by Automator) are processed
# one by one; a failure is reported and the rest still run.
#
# Automator: Quick Action, "Workflow receives current files or folders in
# Finder", Run Shell Script with "Pass input: as arguments":
#   video_for_whatsapp.sh "$@"            (or with --30fps)
# Automator runs scripts non-login and non-interactive, so PATH (homebrew,
# ~/scripts) has to be set in ~/.zshenv.

set -euo pipefail

CRF=""
PRESET=""
FPS30=false
OUTPUT=""
FORWARD=()
INPUTS=()

usage() {
    echo "Usage: $0 [--crf N] [--preset NAME] [--30fps] [-o OUT] <input_video>..." >&2
    exit 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --crf)
            CRF="${2:?--crf needs a value}"
            FORWARD+=("$1" "$2")
            shift 2
            ;;
        --preset)
            PRESET="${2:?--preset needs a value}"
            FORWARD+=("$1" "$2")
            shift 2
            ;;
        --30fps)
            FPS30=true
            FORWARD+=("$1")
            shift
            ;;
        -o|--output)
            OUTPUT="${2:?-o needs a path}"
            shift 2
            ;;
        --)
            shift
            INPUTS+=("$@")
            break
            ;;
        -*)
            echo "Unknown option: $1" >&2
            usage
            ;;
        *)
            INPUTS+=("$1")
            shift
            ;;
    esac
done

if [[ ${#INPUTS[@]} -eq 0 ]]; then
    usage
fi

# Several inputs: run this script once per file, so `set -e` still aborts
# each file on its first error without stopping the others.
if [[ ${#INPUTS[@]} -gt 1 ]]; then
    if [[ -n "$OUTPUT" ]]; then
        echo "Error: -o works only with a single input" >&2
        exit 1
    fi
    STATUS=0
    for f in "${INPUTS[@]}"; do
        "$0" ${FORWARD[@]+"${FORWARD[@]}"} -- "$f" || { echo "FAILED: $f" >&2; STATUS=1; }
        echo ""
    done
    exit "$STATUS"
fi

INPUT="${INPUTS[0]}"

if [[ ! -f "$INPUT" ]]; then
    echo "Error: input file not found: $INPUT" >&2
    exit 1
fi

if ! command -v ffmpeg >/dev/null 2>&1 || ! command -v ffprobe >/dev/null 2>&1; then
    echo "Error: ffmpeg/ffprobe not found. Install with: brew install ffmpeg" >&2
    exit 1
fi

DIR="$(dirname "$INPUT")"
BASE="$(basename "${INPUT%.*}")"

if [[ "$FPS30" == true ]]; then
    CRF="${CRF:-23}"
    PRESET="${PRESET:-veryfast}"
    OUTPUT="${OUTPUT:-${DIR}/${BASE}.whatsapp.30fps.mp4}"
    echo "Encoding $INPUT -> $OUTPUT (30 fps, 1280 px, CRF ${CRF}, preset ${PRESET})"

    # -loglevel error -stats: in Automator's output show only errors and progress.
    ffmpeg -y -loglevel error -stats \
        -i "$INPUT" \
        -vf "fps=30,scale=1280:-2" \
        -c:v libx264 -profile:v high -preset "${PRESET}" -crf "${CRF}" \
        -pix_fmt yuv420p \
        -c:a aac -b:a 128k \
        -movflags +faststart \
        "$OUTPUT"

    echo "Done. Output written to: $OUTPUT"
    exit 0
fi

CRF="${CRF:-18}"
PRESET="${PRESET:-slow}"
OUTPUT="${OUTPUT:-${DIR}/${BASE}_wa.mp4}"

echo "Analyzing source file: $INPUT"

# --- Probe video stream ---
VCODEC=$(ffprobe -v error -select_streams v:0 \
    -show_entries stream=codec_name -of csv=p=0 "$INPUT")
PIXFMT=$(ffprobe -v error -select_streams v:0 \
    -show_entries stream=pix_fmt -of csv=p=0 "$INPUT")
V_BITRATE=$(ffprobe -v error -select_streams v:0 \
    -show_entries stream=bit_rate -of csv=p=0 "$INPUT")

# --- Probe audio stream (may be absent) ---
ACODEC=$(ffprobe -v error -select_streams a:0 \
    -show_entries stream=codec_name -of csv=p=0 "$INPUT" || true)
A_BITRATE=$(ffprobe -v error -select_streams a:0 \
    -show_entries stream=bit_rate -of csv=p=0 "$INPUT" || true)

# Many files (especially iPhone .mov/HEVC) don't store a per-stream
# bitrate at all. In that case DO NOT fall back to format=bit_rate,
# since that is the TOTAL container bitrate (video + audio + overhead)
# and would inflate the video-only target. Instead, derive the video
# bitrate from (file size / duration), minus the audio bitrate.
if [[ -z "$V_BITRATE" || "$V_BITRATE" == "N/A" ]]; then
    echo "  Per-stream video bitrate not stored in file, estimating from size/duration..."

    DURATION=$(ffprobe -v error -show_entries format=duration \
        -of csv=p=0 "$INPUT")
    FILE_SIZE_BYTES=$(stat -f%z "$INPUT" 2>/dev/null || stat -c%s "$INPUT")
    TOTAL_BITRATE=$(echo "$FILE_SIZE_BYTES * 8 / $DURATION" | bc)

    EST_AUDIO_BITRATE="${A_BITRATE:-192000}"
    if [[ "$EST_AUDIO_BITRATE" == "N/A" ]]; then
        EST_AUDIO_BITRATE="192000"
    fi
    if [[ -z "$ACODEC" ]]; then
        EST_AUDIO_BITRATE="0"
    fi

    # Subtract audio and ~2% for container/muxing overhead.
    V_BITRATE=$(echo "($TOTAL_BITRATE - $EST_AUDIO_BITRATE) * 0.98" | bc | cut -d. -f1)
fi

echo "  Video codec : ${VCODEC:-unknown}"
echo "  Pixel format: ${PIXFMT:-unknown}"
echo "  Video bitrate (target): ${V_BITRATE:-unknown} bps"
echo "  Audio codec : ${ACODEC:-none}"
echo "  Audio bitrate: ${A_BITRATE:-unknown} bps"

COMPATIBLE_VIDEO=false
if [[ "$VCODEC" == "h264" && "$PIXFMT" == "yuv420p" ]]; then
    COMPATIBLE_VIDEO=true
fi

COMPATIBLE_AUDIO=false
if [[ "$ACODEC" == "aac" || -z "$ACODEC" ]]; then
    COMPATIBLE_AUDIO=true
fi

if [[ "$COMPATIBLE_VIDEO" == true && "$COMPATIBLE_AUDIO" == true ]]; then
    echo ""
    echo "Source is already H.264/yuv420p (+ AAC or no audio)."
    echo "Remuxing only (no quality loss, no re-encode)..."
    ffmpeg -y -i "$INPUT" \
        -c copy \
        -movflags +faststart \
        "$OUTPUT"
else
    echo ""
    echo "Source needs re-encoding for compatibility."
    echo "Re-encoding to H.264/yuv420p using CRF ${CRF} (preset: ${PRESET}) to preserve visual quality..."

    # Fallback bitrate if ffprobe couldn't determine one (rare) — only
    # used below to compute a safety ceiling, not as the encoding target.
    if [[ -z "$V_BITRATE" || "$V_BITRATE" == "N/A" ]]; then
        echo "  Could not detect source video bitrate, using 8M as a safe default."
        V_BITRATE="8000000"
    fi

    # CRF lets x264 allocate bits per-scene to hit a constant quality
    # level, which is what actually preserves visual quality when
    # converting from a more bit-efficient source codec (AV1/HEVC/VP9).
    # We still set a generous VBV ceiling (-maxrate/-bufsize) purely as
    # a safety net against pathological content (e.g. heavy grain/noise)
    # ballooning the file size unexpectedly — sized well above the
    # source's own bitrate so it practically never kicks in for normal
    # content, it just caps worst-case blowup.
    case "$VCODEC" in
        av1)      CEILING_MULTIPLIER="4" ;;   # AV1 source: generous headroom
        hevc|h265) CEILING_MULTIPLIER="3" ;;
        vp9)      CEILING_MULTIPLIER="3" ;;
        *)        CEILING_MULTIPLIER="2" ;;
    esac
    MAXRATE=$(( V_BITRATE * CEILING_MULTIPLIER ))
    BUFSIZE=$(( MAXRATE * 2 ))

    AUDIO_ARGS=(-an)
    if [[ -n "$ACODEC" ]]; then
        if [[ -z "$A_BITRATE" || "$A_BITRATE" == "N/A" ]]; then
            A_BITRATE="192000"
        fi
        AUDIO_ARGS=(-c:a aac -b:a "${A_BITRATE}")
    fi

    ffmpeg -y -i "$INPUT" \
        -c:v libx264 \
        -profile:v high -level 4.1 \
        -preset "${PRESET}" \
        -crf "${CRF}" \
        -maxrate "${MAXRATE}" -bufsize "${BUFSIZE}" \
        -pix_fmt yuv420p \
        "${AUDIO_ARGS[@]}" \
        -movflags +faststart \
        "$OUTPUT"
fi

echo ""
echo "Done. Output written to: $OUTPUT"
echo "Tip: if Finder still shows a stale/gray thumbnail, run:"
echo "  qlmanage -r cache && qlmanage -r"
echo "or simply move the file to a new location once — Finder will regenerate it."
