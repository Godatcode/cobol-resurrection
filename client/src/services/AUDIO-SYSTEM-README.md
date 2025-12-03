# AUDIO SYSTEM DOCUMENTATION

## Overview

The NECRO-BANK Audio System provides AUTHENTIC VINTAGE COMPUTING SOUND EFFECTS to enhance the immersive mainframe museum experience.

## Architecture

### AudioManager Service (`AudioManager.ts`)

Singleton service that manages all sound effects in the application.

**Key Features:**
- Preloads all sound files on initialization
- Supports volume control (0.0 to 1.0)
- Mute/unmute functionality
- Persistent settings via localStorage
- Graceful fallback for missing audio files

### VolumeControl Component (`VolumeControl.tsx`)

UI component for controlling audio settings.

**Features:**
- Mute/unmute toggle button
- Volume slider (0-10 scale)
- Test sound button
- Visual status indicator
- Vintage mainframe styling

## Sound Effects

### 1. Teletype (`teletype.mp3`)
- **Trigger**: Terminal text output
- **Usage**: `audioManager.playTeletype()`
- **Description**: Mechanical teletype clacking sound
- **Duration**: ~0.5-1 second

### 2. Tape Drive (`tapedrive.mp3`)
- **Trigger**: During calculation execution
- **Usage**: `audioManager.playTapeDrive()`
- **Description**: Magnetic tape drive motor sound
- **Duration**: ~2-3 seconds

### 3. Card Reader (`cardreader.mp3`)
- **Trigger**: Form submission
- **Usage**: `audioManager.playCardReader()`
- **Description**: Punch card reader mechanical sound
- **Duration**: ~1-2 seconds

### 4. Bell (`bell.mp3`)
- **Trigger**: Successful calculation completion
- **Usage**: `audioManager.playBell()`
- **Description**: Console bell or chime
- **Duration**: ~0.5 seconds

### 5. Buzzer (`buzzer.mp3`)
- **Trigger**: Error conditions
- **Usage**: `audioManager.playBuzzer()`
- **Description**: Error buzzer or alarm
- **Duration**: ~0.5-1 second

## Integration Points

### App.jsx

Sound effects are triggered at key points in the application flow:

```javascript
// On calculation start
audioManager.playCardReader();  // Form submission
audioManager.playTapeDrive();   // Calculation begins

// During boot sequence
audioManager.playTeletype();    // Each log message

// On completion
audioManager.playBell();        // Success

// On error
audioManager.playBuzzer();      // Error condition
```

### Component Integration

```javascript
import { audioManager } from '../services/AudioManager';

// Initialize in component
useEffect(() => {
  audioManager.initialize();
}, []);

// Play sounds
audioManager.playTeletype();
```

## User Controls

### Volume Control UI

Located in bottom-right corner of screen:
- **Mute Button**: Toggle audio on/off
- **VOL Button**: Show/hide volume slider
- **TEST Button**: Play test sound (bell)
- **Volume Slider**: Adjust volume 0-10

### Persistent Settings

Settings are saved to localStorage:
```javascript
{
  "volume": 0.5,    // 0.0 to 1.0
  "muted": false    // boolean
}
```

## Installation

### 1. Install Audio Files

Place authentic vintage recordings in `client/public/sounds/`:
- `teletype.mp3`
- `tapedrive.mp3`
- `cardreader.mp3`
- `bell.mp3`
- `buzzer.mp3`

### 2. Generate Placeholders (Development)

If ffmpeg is installed:
```bash
cd client/public/sounds
./generate-placeholders.sh
```

This creates SILENT placeholder files to prevent console errors during development.

### 3. Source Authentic Recordings

See `client/public/sounds/README.md` for sourcing instructions.

**Recommended Sources:**
- Archive.org - Computer History Collection
- Freesound.org - Vintage computing sounds
- YouTube Audio Library

## API Reference

### AudioManager Methods

```typescript
// Initialize audio system
await audioManager.initialize(): Promise<void>

// Play specific sounds
audioManager.playTeletype(): void
audioManager.playTapeDrive(): void
audioManager.playCardReader(): void
audioManager.playBell(): void
audioManager.playBuzzer(): void

// Generic play method
audioManager.play(effect: SoundEffect): void

// Volume control
audioManager.setVolume(volume: number): void
audioManager.getVolume(): number

// Mute control
audioManager.toggleMute(): void
audioManager.setMuted(muted: boolean): void
audioManager.isMuted(): boolean
```

### Sound Effect Types

```typescript
type SoundEffect = 
  | 'teletype'
  | 'tapedrive'
  | 'cardreader'
  | 'bell'
  | 'buzzer';
```

## Testing

### Manual Testing

1. Open application
2. Click TEST button in volume control
3. Verify bell sound plays
4. Adjust volume slider
5. Test mute button
6. Submit calculation form
7. Verify sound sequence:
   - Card reader (submission)
   - Tape drive (calculation)
   - Teletype (each log message)
   - Bell (success) or Buzzer (error)

### Browser Compatibility

Tested on:
- Chrome/Edge (Chromium)
- Firefox
- Safari

**Note**: Some browsers require user interaction before playing audio (autoplay policy).

## Troubleshooting

### No Sound Playing

1. Check browser console for errors
2. Verify audio files exist in `/public/sounds/`
3. Check volume control is not muted
4. Verify browser autoplay policy allows audio
5. Test with TEST button

### Console Warnings

```
Sound file not found: teletype - Install authentic recordings for full experience
```

This is NORMAL if audio files are not installed. The system will continue to function without sound.

### Performance Issues

If audio causes lag:
1. Reduce file sizes (< 100KB per file)
2. Use lower bitrate MP3 (128 kbps)
3. Convert to mono channel
4. Mute audio if not needed

## Requirements Validation

This implementation satisfies:

**Requirement 11.5**: "WHEN user interactions occur THEN the system SHALL play authentic teletype and mainframe sound effects"

**Features Implemented:**
- ✓ Teletype sound on terminal output
- ✓ Tape drive sound during calculations
- ✓ Card reader sound on form submission
- ✓ Bell sound on successful completion
- ✓ Buzzer sound on errors
- ✓ Volume controls
- ✓ Mute option
- ✓ Persistent settings
- ✓ Graceful fallback for missing files

## Future Enhancements

- Add more sound variations
- Implement sound themes (different mainframe models)
- Add spatial audio effects
- Support for custom sound packs
- Audio visualization
- Accessibility options (visual indicators for deaf users)

[END OF TAPE]
