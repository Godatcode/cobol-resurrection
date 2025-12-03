#!/bin/bash

# PLACEHOLDER SOUND GENERATOR
# Creates minimal silent MP3 files for development
# Replace these with authentic vintage recordings for production

echo "GENERATING PLACEHOLDER SOUND FILES..."
echo "These are SILENT placeholders - replace with authentic recordings!"

# Check if ffmpeg is installed
if ! command -v ffmpeg &> /dev/null; then
    echo "ERROR: ffmpeg not installed"
    echo "Install with: brew install ffmpeg (macOS)"
    echo ""
    echo "Alternatively, download authentic sounds from:"
    echo "- archive.org (search: IBM mainframe sounds)"
    echo "- freesound.org (search: teletype, tape reel)"
    echo ""
    exit 1
fi

# Generate silent MP3 files with ffmpeg
# These are minimal placeholders to prevent console errors

# Teletype (0.5 seconds)
ffmpeg -f lavfi -i anullsrc=r=44100:cl=mono -t 0.5 -q:a 9 -acodec libmp3lame teletype.mp3 -y 2>/dev/null

# Tape Drive (2 seconds)
ffmpeg -f lavfi -i anullsrc=r=44100:cl=mono -t 2 -q:a 9 -acodec libmp3lame tapedrive.mp3 -y 2>/dev/null

# Card Reader (1 second)
ffmpeg -f lavfi -i anullsrc=r=44100:cl=mono -t 1 -q:a 9 -acodec libmp3lame cardreader.mp3 -y 2>/dev/null

# Bell (0.5 seconds)
ffmpeg -f lavfi -i anullsrc=r=44100:cl=mono -t 0.5 -q:a 9 -acodec libmp3lame bell.mp3 -y 2>/dev/null

# Buzzer (0.5 seconds)
ffmpeg -f lavfi -i anullsrc=r=44100:cl=mono -t 0.5 -q:a 9 -acodec libmp3lame buzzer.mp3 -y 2>/dev/null

echo "✓ Generated 5 placeholder sound files"
echo ""
echo "IMPORTANT: These are SILENT placeholders!"
echo "Replace with authentic vintage recordings for production."
echo "See README.md for sourcing instructions."
echo ""
echo "[END OF TAPE]"
