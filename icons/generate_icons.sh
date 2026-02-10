#!/bin/bash
# Generate app icons from SVG source
# Requires: rsvg-convert (brew install librsvg) or Inkscape
# Usage: ./generate_icons.sh

set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SVG="$SCRIPT_DIR/markcraft-icon.svg"
OUT_DIR="$SCRIPT_DIR/../MarkCraft/Assets.xcassets/AppIcon.appiconset"

mkdir -p "$OUT_DIR"

# macOS App Store requires 1024x1024
# Xcode 15+ single-size icon

if command -v rsvg-convert &>/dev/null; then
    rsvg-convert -w 1024 -h 1024 "$SVG" -o "$OUT_DIR/AppIcon.png"
    echo "✅ AppIcon.png (1024x1024) generated via rsvg-convert"
elif command -v inkscape &>/dev/null; then
    inkscape "$SVG" -w 1024 -h 1024 -o "$OUT_DIR/AppIcon.png"
    echo "✅ AppIcon.png (1024x1024) generated via Inkscape"
elif command -v sips &>/dev/null && command -v qlmanage &>/dev/null; then
    # macOS native: render SVG via Quick Look
    qlmanage -t -s 1024 -o /tmp "$SVG" 2>/dev/null
    SVG_BASE=$(basename "$SVG")
    mv "/tmp/${SVG_BASE}.png" "$OUT_DIR/AppIcon.png" 2>/dev/null || {
        echo "⚠️ qlmanage failed. Install librsvg: brew install librsvg"
        exit 1
    }
    echo "✅ AppIcon.png (1024x1024) generated via qlmanage"
else
    echo "❌ No SVG converter found. Install one of:"
    echo "   brew install librsvg     # rsvg-convert"
    echo "   brew install --cask inkscape"
    exit 1
fi

# Generate additional sizes for GitHub, website, etc.
SIZES="16 32 64 128 256 512"
for S in $SIZES; do
    if command -v sips &>/dev/null; then
        sips -z $S $S "$OUT_DIR/AppIcon.png" --out "$SCRIPT_DIR/icon-${S}.png" >/dev/null 2>&1
    fi
done

echo "✅ All icons generated in $OUT_DIR"
echo "   AppIcon.png (1024x1024) — Xcode 15+ single-size"
