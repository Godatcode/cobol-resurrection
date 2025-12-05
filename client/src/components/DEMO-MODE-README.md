# DEMO MODE COMPONENT

## OVERVIEW

THE DEMO MODE COMPONENT PROVIDES A GUIDED TOUR EXPERIENCE FOR THE NECRO-BANK RESURRECTION BRIDGE SYSTEM. IT INCLUDES TOOLTIPS, AUTO-PLAY FUNCTIONALITY, AND SHAREABLE DEMO LINKS.

## FEATURES

### 1. GUIDED TOUR
- **9 INTERACTIVE STEPS** COVERING ALL MAJOR COMPONENTS
- **TOOLTIPS** WITH DETAILED DESCRIPTIONS
- **VISUAL HIGHLIGHTS** ON TARGET ELEMENTS
- **SMOOTH SCROLLING** TO EACH COMPONENT
- **PROGRESS BAR** SHOWING TOUR COMPLETION

### 2. AUTO-PLAY MODE
- **AUTOMATIC PROGRESSION** THROUGH ALL STEPS
- **4-SECOND INTERVALS** BETWEEN STEPS
- **PAUSE/RESUME** FUNCTIONALITY
- **36-SECOND TOTAL DURATION**

### 3. SHAREABLE DEMO LINK
- **COPY-TO-CLIPBOARD** FUNCTIONALITY
- **URL PARAMETER** (?demo=true) FOR AUTO-START
- **GITHUB REPOSITORY LINK**
- **SOCIAL SHARING** CAPABILITIES

### 4. TRY IT YOURSELF PROMPTS
- **PAUSE TOUR** TO INTERACT WITH COMPONENTS
- **RESUME WHEN READY**
- **HANDS-ON EXPLORATION** ENCOURAGED

## TOUR STEPS

1. **NECRO-BANK MAINFRAME SYSTEM** - Introduction to the resurrection bridge
2. **AI CODE GENERATOR** - Summon ancient spirits to generate legacy code
3. **MULTI-LANGUAGE SUPPORT** - Select from 4 legacy languages
4. **LEGACY CALCULATOR INTERFACE** - Enter parameters for calculations
5. **CALCULATION HISTORY** - View previous calculations
6. **IBM 729 TAPE DRIVE** - Authentic tape reel animation
7. **IBM 7090 CONSOLE PANEL** - Blinking panel lights display
8. **SYSTEM CONSOLE** - Vintage terminal with CRT effects
9. **AUDIO SYSTEM** - Authentic mainframe sound effects

## USAGE

### STARTING THE TOUR

**MANUAL START:**
```javascript
// CLICK THE "START GUIDED TOUR" BUTTON IN THE TOP-LEFT CORNER
```

**AUTO-START VIA URL:**
```
https://your-domain.com/?demo=true
```

### NAVIGATION

- **NEXT →** - ADVANCE TO NEXT STEP
- **← PREVIOUS** - RETURN TO PREVIOUS STEP
- **✕** - EXIT TOUR
- **▶ AUTO-PLAY** - START AUTOMATIC PROGRESSION
- **⏸ PAUSE** - PAUSE AUTO-PLAY
- **🔗 SHARE** - OPEN SHARE MODAL

### SHARING

1. CLICK **🔗 SHARE** BUTTON
2. COPY THE DEMO LINK
3. SHARE WITH OTHERS
4. RECIPIENTS WILL AUTO-START THE TOUR

## TECHNICAL IMPLEMENTATION

### COMPONENT STRUCTURE

```
DemoMode
├── Demo Control Panel (Fixed Top-Right)
│   ├── Progress Indicator
│   ├── Progress Bar
│   ├── Auto-Play Controls
│   ├── Navigation Buttons
│   └── Try It Yourself Prompt
├── Tooltip (Positioned Near Target)
│   ├── Title
│   ├── Description
│   └── Arrow Indicator
└── Share Modal (Centered Overlay)
    ├── Demo URL Input
    ├── Copy Button
    └── GitHub Link
```

### HIGHLIGHTING SYSTEM

**CSS CLASS:** `.demo-highlight`
- **BOX SHADOW:** GLOWING GREEN BORDER
- **ANIMATION:** PULSING EFFECT
- **Z-INDEX:** 45 (ABOVE OVERLAY)

### AUTO-PLAY SEQUENCE

```javascript
const AUTO_PLAY_SEQUENCE = [
  { step: 0, delay: 0, action: 'show' },
  { step: 1, delay: 4000, action: 'show' },
  { step: 2, delay: 8000, action: 'show' },
  // ... 9 STEPS TOTAL
  { step: null, delay: 36000, action: 'complete' }
];
```

### TOOLTIP POSITIONING

**POSITIONS:** bottom, top, left, right
**CALCULATION:** BASED ON TARGET ELEMENT BOUNDING RECT
**SCROLLING:** SMOOTH SCROLL TO CENTER

## INTEGRATION

### APP.JSX INTEGRATION

```javascript
import DemoMode from './components/DemoMode';

// STATE
const [isDemoMode, setIsDemoMode] = useState(false);

// CHECK URL PARAMETER
useEffect(() => {
  const urlParams = new URLSearchParams(window.location.search);
  if (urlParams.get('demo') === 'true') {
    setIsDemoMode(true);
  }
}, []);

// RENDER
<DemoMode 
  isActive={isDemoMode}
  onClose={() => setIsDemoMode(false)}
  onAutoPlayComplete={() => {
    addLog('GUIDED TOUR COMPLETE. TRY IT YOURSELF!', 'info');
  }}
/>
```

### COMPONENT IDS REQUIRED

ALL TARGET COMPONENTS MUST HAVE IDS:
- `header` - HEADER COMPONENT
- `code-generator-button` - AI CODE GENERATOR BUTTON
- `language-selector` - LANGUAGE DROPDOWN
- `calculator-form` - MAIN CALCULATOR FORM
- `history-panel` - CALCULATION HISTORY PANEL
- `tape-reel` - TAPE REEL ANIMATION
- `panel-lights` - PANEL LIGHTS DISPLAY
- `terminal` - TERMINAL WINDOW
- `volume-control` - VOLUME CONTROL

## STYLING

### MAINFRAME AESTHETIC

- **BACKGROUND:** BLACK (#000000)
- **TEXT:** MAINFRAME GREEN (#00ff00)
- **FONT:** MONOSPACE (COURIER NEW)
- **BORDERS:** 2PX SOLID GREEN
- **HOVER:** INVERTED COLORS

### RESPONSIVE DESIGN

- **CONTROL PANEL:** FIXED TOP-RIGHT
- **TOOLTIP:** DYNAMIC POSITIONING
- **SHARE MODAL:** CENTERED WITH OVERLAY
- **MOBILE:** RESPONSIVE LAYOUT

## REQUIREMENTS VALIDATION

**REQUIREMENT 13.2:** GUIDED TOUR FEATURE ✓
- TOOLTIPS EXPLAINING EACH COMPONENT ✓
- SEQUENTIAL NAVIGATION ✓
- VISUAL HIGHLIGHTS ✓

**REQUIREMENT 13.3:** AUTO-PLAY DEMO SEQUENCE ✓
- AUTOMATIC PROGRESSION ✓
- TRY IT YOURSELF PROMPTS ✓
- SHAREABLE DEMO LINK ✓

## FUTURE ENHANCEMENTS

- **KEYBOARD SHORTCUTS** (ARROW KEYS FOR NAVIGATION)
- **VOICE-OVER NARRATION**
- **INTERACTIVE CHALLENGES**
- **COMPLETION CERTIFICATE**
- **ANALYTICS TRACKING**

[END OF TAPE]
