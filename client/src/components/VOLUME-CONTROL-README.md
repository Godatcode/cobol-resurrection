# VOLUME CONTROL COMPONENT

## Visual Reference

```
┌─────────────────────────────────┐
│  🔊  VOL  TEST                  │  ← Control Buttons
├─────────────────────────────────┤
│  0 ═══════●═══ 10               │  ← Volume Slider (when expanded)
│     VOLUME: 7                   │
├─────────────────────────────────┤
│  AUDIO: 70%                     │  ← Status Indicator
└─────────────────────────────────┘
```

## Location

Fixed position: **Bottom-right corner** of screen

## Controls

### 1. Mute Button (🔊/🔇)
- **Click**: Toggle audio on/off
- **Icon**: 🔊 (unmuted) or 🔇 (muted)
- **Tooltip**: "MUTE AUDIO" / "UNMUTE AUDIO"

### 2. VOL Button
- **Click**: Show/hide volume slider
- **Text**: "VOL"
- **Tooltip**: "VOLUME CONTROL"

### 3. TEST Button
- **Click**: Play test sound (bell)
- **Text**: "TEST"
- **Tooltip**: "TEST AUDIO"
- **Purpose**: Verify audio system is working

### 4. Volume Slider
- **Range**: 0 to 10 (maps to 0.0 to 1.0 internally)
- **Step**: 0.1 (10 steps)
- **Disabled**: When muted
- **Color**: Mainframe green (#00ff00)

### 5. Status Indicator
- **Muted**: "AUDIO: MUTED"
- **Unmuted**: "AUDIO: XX%" (percentage)

## Styling

### Colors
- **Background**: Black (#000000)
- **Border**: Mainframe green (#00ff00)
- **Text**: Mainframe green (#00ff00)
- **Hover**: Inverted (green background, black text)

### Typography
- **Font**: Monospace (Courier New)
- **Size**: Small (text-xs for labels)

### Layout
- **Border**: 2px solid green
- **Padding**: 3 units (p-3)
- **Gap**: 3 units between buttons
- **Z-index**: 50 (appears above other content)

## Behavior

### Initialization
- Loads saved settings from localStorage on mount
- Initializes AudioManager service
- Displays current volume and mute state

### Volume Changes
- Updates AudioManager volume
- Saves to localStorage
- Auto-unmutes if volume increased from 0

### Mute Toggle
- Updates AudioManager mute state
- Saves to localStorage
- Disables volume slider when muted

### Test Sound
- Plays bell sound effect
- Verifies audio system is working
- Respects mute state

## Integration

### Import
```typescript
import { VolumeControl } from './components/VolumeControl';
```

### Usage
```jsx
<VolumeControl />
```

### Placement
Add to App.jsx before closing `</div>`:
```jsx
<VolumeControl />
```

## State Management

### Local State
- `volume`: Current volume level (0.0 to 1.0)
- `muted`: Mute state (boolean)
- `showSlider`: Slider visibility (boolean)

### Persistent State
Saved to localStorage as:
```json
{
  "volume": 0.5,
  "muted": false
}
```

## Accessibility

### Keyboard Navigation
- All buttons are keyboard accessible
- Tab order: Mute → VOL → TEST
- Enter/Space to activate buttons

### Screen Readers
- Buttons have descriptive titles
- Status indicator provides audio state

### Visual Indicators
- Clear icons and labels
- High contrast (green on black)
- Status text shows current state

## Responsive Design

### Desktop
- Fixed position bottom-right
- Full controls visible

### Mobile
- Same position and size
- Touch-friendly button sizes
- Slider may be harder to use (consider tap-to-increment)

## Browser Compatibility

### Tested On
- ✓ Chrome/Edge (Chromium)
- ✓ Firefox
- ✓ Safari

### Known Issues
- Safari may require user interaction before playing audio
- Some browsers block autoplay (use TEST button first)

## Troubleshooting

### Volume control not appearing
- Check VolumeControl is imported in App.jsx
- Verify component is rendered in JSX
- Check z-index conflicts

### Test button not playing sound
- Verify audio files are installed
- Check browser console for errors
- Try unmuting if muted
- Check browser autoplay policy

### Settings not persisting
- Check localStorage is enabled
- Verify no browser privacy mode
- Check console for localStorage errors

## Customization

### Change Position
Modify className in VolumeControl.tsx:
```tsx
className="fixed bottom-4 right-4 z-50"
```

### Change Colors
Update Tailwind classes:
- `border-mainframe-green` → custom color
- `text-mainframe-green` → custom color
- `bg-black` → custom background

### Change Volume Range
Modify slider attributes:
```tsx
min="0"
max="1"
step="0.1"
```

### Add More Controls
Extend component with additional buttons:
- Sound theme selector
- Individual sound toggles
- Preset volume levels

## Performance

### Optimization
- Singleton AudioManager (no re-initialization)
- Lazy loading of audio files
- Minimal re-renders (local state only)

### Memory Usage
- ~5KB for component code
- ~500KB for audio files (when loaded)
- Minimal localStorage usage

## Future Enhancements

- [ ] Keyboard shortcuts (M for mute, +/- for volume)
- [ ] Visual audio level indicator
- [ ] Sound theme selector
- [ ] Individual sound effect toggles
- [ ] Preset volume levels (Low/Medium/High)
- [ ] Fade in/out effects
- [ ] Spatial audio controls

[END OF TAPE]
