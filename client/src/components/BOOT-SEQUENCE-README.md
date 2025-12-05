# Boot Sequence Component

## Overview

The BootSequence component provides an authentic vintage IBM mainframe boot experience when the NECRO-BRIDGE application first loads.

## Features

### ✅ Vintage IBM Boot Screen
- Displays authentic 1959-style mainframe initialization messages
- ASCII art border styling with IBM aesthetic
- Sequential boot messages showing system initialization

### ✅ "INITIALIZING NECRO-BRIDGE v1.0" Message
- Prominent title display: "NECRO-BRIDGE v1.0"
- Subtitle: "LEGACY SYSTEM RESURRECTION PROTOCOL"
- Authentic mainframe typography

### ✅ Sequential Startup Messages
The boot sequence displays 11 messages in order:
1. NECRO-BRIDGE SYSTEM INITIALIZATION...
2. CHECKING CORE MEMORY... OK
3. LOADING CHANNEL CONTROLLERS... OK
4. INITIALIZING TAPE DRIVES... OK
5. MOUNTING SYSTEM VOLUMES... OK
6. LOADING COBOL RUNTIME... OK
7. LOADING FORTRAN RUNTIME... OK
8. LOADING PASCAL RUNTIME... OK
9. LOADING BASIC RUNTIME... OK
10. INITIALIZING NECRO-BRIDGE v1.0... OK
11. SYSTEM READY

Each message appears with a 600ms delay between them.

### ✅ Progress Bar with Authentic Styling
- Visual progress indicator showing initialization progress
- Percentage display (0% to 100%)
- Green-on-black mainframe color scheme
- Animated pulse effect on progress bar

### ✅ Skippable After First View
- Uses localStorage to track if user has seen boot sequence
- Key: `necro-bridge-boot-seen`
- Skip button: "[PRESS ANY KEY TO SKIP]"
- Automatically skips on subsequent visits

## Implementation Details

### Component Location
`client/src/components/BootSequence.jsx`

### Integration
The component is integrated into `App.jsx`:

```jsx
import BootSequence from './components/BootSequence'

function App() {
  const [showBootSequence, setShowBootSequence] = useState(true);
  
  const handleBootComplete = () => {
    setShowBootSequence(false);
    setSystemReady(true);
  };
  
  return (
    <div>
      {showBootSequence && <BootSequence onComplete={handleBootComplete} />}
      {/* Rest of app */}
    </div>
  );
}
```

### Props
- `onComplete`: Callback function called when boot sequence finishes or is skipped

### Styling
- Full-screen overlay (z-index: 50)
- Mainframe black background (#000000)
- Mainframe green text (#00ff00)
- Monospace Courier New font
- Authentic IBM 7090 aesthetic

### LocalStorage Management
- Automatically sets `necro-bridge-boot-seen` to `'true'` after first view
- Checks this value on mount to determine if boot should be skipped
- Can be reset by clearing localStorage in browser DevTools

## Testing

### Manual Testing
1. Clear localStorage: `localStorage.clear()`
2. Refresh the page
3. Boot sequence should display
4. Refresh again - boot sequence should be skipped

### Build Verification
```bash
cd client
npm run build
```
Build succeeds without errors, confirming component compiles correctly.

## Requirements Satisfied

✅ **Implement vintage IBM boot screen** - Complete with authentic styling and ASCII art borders

✅ **Add "INITIALIZING NECRO-BRIDGE v1.0" message** - Prominently displayed as main title

✅ **Create sequential startup messages** - 11 messages displayed with 600ms delays

✅ **Add progress bar with authentic styling** - Green-on-black with percentage display

✅ **Make skippable after first view** - Uses localStorage to track and skip on subsequent visits

**Validates: Requirements 13.1**

## Future Enhancements

- Add sound effects during boot sequence
- Add keyboard shortcut to skip (ESC key)
- Add animation effects for message appearance
- Add "BIOS-style" memory check animation

[END OF TAPE]
