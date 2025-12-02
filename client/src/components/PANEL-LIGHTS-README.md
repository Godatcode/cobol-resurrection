# PanelLights Component

## Overview

The PanelLights component provides an authentic IBM 7090 Console Panel visualization with blinking LED patterns that represent memory addresses during calculation execution.

## Features

### Authentic IBM 7090 Layout
- **8 Registers**: AC (Accumulator), MQ (Multiplier-Quotient), SI (Storage Indicator), IC (Instruction Counter), XR1, XR2, XR4, XR7
- **18-bit Display**: Each register shows 18 bits (half of the IBM 7090's 36-bit word architecture)
- **144 Total LEDs**: 8 registers × 18 bits = 144 individual LED indicators

### Dynamic Blinking Patterns
- **Idle State**: Minimal activity with slow blink rate (1000ms)
- **Active State**: Dynamic memory address patterns with fast blink rate (150ms)
- **Pattern Updates**: LED patterns refresh every 300ms during calculations
- **Pseudo-Random Patterns**: Deterministic but realistic-looking memory access patterns

### Visual Effects
- **LED Glow**: Authentic phosphor-like glow effect using CSS blur
- **Smooth Animations**: RequestAnimationFrame-based animation loop
- **Opacity Modulation**: Sinusoidal blinking creates realistic LED pulsing
- **Status Indicators**: PWR (power) and RUN (execution) indicators

### Synchronization
- **Calculation Status**: Automatically syncs with `isCalculating` prop
- **Memory Address Display**: Shows simulated hexadecimal memory addresses
- **Performance Optimized**: Efficient animation loop with cleanup

## Usage

```tsx
import PanelLights from './components/PanelLights';

function App() {
  const [isCalculating, setIsCalculating] = useState(false);
  
  return (
    <PanelLights 
      isCalculating={isCalculating}
      calculationProgress={50} // Optional: 0-100
    />
  );
}
```

## Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `isCalculating` | `boolean` | required | Controls animation speed and LED patterns |
| `calculationProgress` | `number` | `0` | Optional progress indicator (0-100) |

## Technical Details

### Blink Rate Configuration
- **IDLE_BLINK_RATE**: 1000ms (slow, minimal activity)
- **ACTIVE_BLINK_RATE**: 150ms (fast, intense activity)
- **PATTERN_UPDATE_RATE**: 300ms (how often patterns change)

### LED Pattern Algorithm
During active calculations:
- High-order bits (0-5) have 70% activation probability
- Low-order bits (6-17) have 50% activation probability
- Pseudo-random noise adds realistic variation
- Certain registers (SI, IC) show more activity

During idle state:
- Only 5-15% of LEDs are active
- SI and IC registers show occasional status updates
- Most registers remain dark

### Animation Loop
1. Calculate delta time since last frame
2. Update blink phase based on current blink rate
3. Periodically update LED patterns (every 300ms)
4. Calculate LED opacity using sinusoidal function
5. Request next animation frame

## Styling

The component uses:
- **Background**: Dark gray (#2a2a2a) panel housing
- **LEDs**: Bright green (#00ff00) mainframe color
- **Border**: Green border matching vintage theme
- **Font**: Monospace font for authentic look

## Requirements Validation

This component satisfies **Requirement 11.3**:
- ✅ Blinking panel lights with random binary patterns
- ✅ Authentic IBM 7090 console panel layout
- ✅ Configurable blink rates and patterns
- ✅ Synchronized with calculation execution

## Historical Accuracy

The IBM 7090 (1959) featured:
- 36-bit word architecture
- Multiple index registers (XR1, XR2, XR4, XR7)
- Accumulator (AC) and Multiplier-Quotient (MQ) registers
- Console panel with indicator lights showing register contents

This component faithfully recreates that experience for the modern web.

[END OF TAPE]
