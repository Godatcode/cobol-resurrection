# SOUND EFFECTS INSTALLATION GUIDE

## Quick Start

### Option 1: Generate Silent Placeholders (Development)

If you have ffmpeg installed:

```bash
cd client/public/sounds
./generate-placeholders.sh
```

This creates SILENT MP3 files that prevent console errors during development.

### Option 2: Download Authentic Sounds (Production)

For the FULL IMMERSIVE EXPERIENCE, download authentic vintage computing sounds.

## Sourcing Authentic Recordings

### Free Resources

#### 1. Archive.org
Visit: https://archive.org

Search terms:
- "IBM mainframe sounds"
- "teletype audio"
- "computer room 1960s"
- "tape drive sounds"

#### 2. Freesound.org
Visit: https://freesound.org

Search terms:
- "typewriter mechanical"
- "tape reel motor"
- "card punch"
- "computer bell"
- "alarm buzzer"

#### 3. YouTube Audio Library
Visit: https://www.youtube.com/audiolibrary

Search terms:
- "vintage computer sounds"
- "mechanical typewriter"
- "retro computing"

Download videos and extract audio using:
```bash
youtube-dl -x --audio-format mp3 [URL]
```

### Commercial Resources

- **BBC Sound Effects Library**: Professional quality
- **Pro Sound Effects**: Vintage computing collection
- **Sound Ideas**: Historical technology sounds

## File Requirements

Place the following files in `client/public/sounds/`:

1. **teletype.mp3** - Mechanical typing sound (~0.5-1 sec)
2. **tapedrive.mp3** - Tape motor sound (~2-3 sec)
3. **cardreader.mp3** - Card feeding sound (~1-2 sec)
4. **bell.mp3** - Console bell (~0.5 sec)
5. **buzzer.mp3** - Error alarm (~0.5-1 sec)

### Technical Specifications

- **Format**: MP3
- **Bitrate**: 128-192 kbps
- **Sample Rate**: 44.1 kHz
- **Channels**: Mono or Stereo
- **File Size**: < 100KB per file (recommended)

## Converting Audio Files

If you have audio in other formats (WAV, OGG, etc.):

```bash
# Convert to MP3 with ffmpeg
ffmpeg -i input.wav -b:a 128k -ar 44100 output.mp3

# Trim to specific duration
ffmpeg -i input.mp3 -t 2 -c copy output.mp3

# Convert to mono
ffmpeg -i input.mp3 -ac 1 output.mp3
```

## Verification

After installing sound files:

1. Start the development server:
   ```bash
   cd client
   npm run dev
   ```

2. Open the application in browser

3. Click the **TEST** button in the volume control (bottom-right)

4. You should hear the bell sound

5. Submit a calculation to test the full sound sequence

## Troubleshooting

### "Sound file not found" warnings

This means the MP3 files are not in the correct location.

**Solution**: Verify files are in `client/public/sounds/` directory

### No sound playing

1. Check browser console for errors
2. Verify volume control is not muted
3. Check browser autoplay policy
4. Try clicking TEST button first (user interaction required)

### Poor audio quality

1. Use higher bitrate (192 kbps)
2. Ensure source audio is high quality
3. Avoid over-compression

## Recommended Sound Characteristics

### Teletype
- Short, crisp mechanical click
- Similar to typewriter key press
- Authentic: ASR-33 Teletype

### Tape Drive
- Motor startup sound
- Continuous running hum
- Motor stop sound
- Authentic: IBM 729 Tape Drive

### Card Reader
- Mechanical feeding sound
- Card sliding through reader
- Brief duration
- Authentic: IBM 2540 Card Reader

### Bell
- Clear, single tone
- Console notification sound
- Pleasant, not harsh
- Authentic: Teletype Bell

### Buzzer
- Harsh, attention-grabbing
- Error alarm tone
- Brief but noticeable
- Authentic: IBM Console Buzzer

## Example Sources

### Teletype Sound
- Search: "ASR-33 teletype sound"
- Alternative: Mechanical typewriter key press

### Tape Drive Sound
- Search: "IBM 729 tape drive"
- Alternative: Reel-to-reel tape recorder motor

### Card Reader Sound
- Search: "IBM card punch sound"
- Alternative: Paper shuffling through machine

### Bell Sound
- Search: "teletype bell"
- Alternative: Desk bell or chime

### Buzzer Sound
- Search: "computer error alarm"
- Alternative: Electronic buzzer or alarm

## Legal Considerations

- Ensure you have rights to use the audio files
- Check licenses on free sound libraries
- Attribute sources if required
- For commercial use, verify licensing terms

## Testing Checklist

- [ ] All 5 MP3 files present in `/public/sounds/`
- [ ] Files are under 100KB each
- [ ] TEST button plays bell sound
- [ ] Form submission plays card reader sound
- [ ] Calculation plays tape drive sound
- [ ] Terminal messages play teletype sound
- [ ] Success plays bell sound
- [ ] Error plays buzzer sound
- [ ] Volume control adjusts sound level
- [ ] Mute button silences all sounds

[END OF TAPE]
