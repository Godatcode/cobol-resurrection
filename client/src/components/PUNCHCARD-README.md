# Punch Card Visualizer Component

## Overview

The PunchCard component is an authentic IBM 029 Keypunch card visualizer that converts text into visual punch card representations. This component brings the vintage computing experience to life by displaying code as it would have appeared on physical punch cards in the 1960s mainframe era.

## Features

### ✅ Authentic IBM 029 Encoding
- Complete implementation of IBM 029 Keypunch character encoding
- 12-row punch card format (rows 12, 11, 0, 1-9)
- Zone punches (rows 12, 11, 0) for letters and special characters
- Digit punches (rows 1-9) for numbers

### ✅ 80-Column Standard
- Default 80-column format matching physical punch cards
- Configurable column count via `maxColumns` prop
- Automatic text padding and truncation

### ✅ Visual Differentiation
- **Control Holes (Zone)**: Red color for rows 12, 11, 0
- **Data Holes**: Black/gray color for rows 1-9
- Unpunched positions shown as small dots

### ✅ Interactive Tooltips
- Hover over any column to see:
  - Column number
  - Character being represented
  - Row numbers that are punched

### ✅ Authentic Styling
- Cream/beige card background (#f5e6d3)
- Green border matching mainframe theme
- Vintage typography and layout

## Usage

### Basic Example

```tsx
import PunchCard from './components/PunchCard';

function MyComponent() {
  return (
    <PunchCard text="HELLO WORLD" />
  );
}
```

### Custom Column Count

```tsx
<PunchCard text="SHORT" maxColumns={40} />
```

### Multiple Cards (for multi-line code)

```tsx
const cobolCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORTGAGE.`;

{cobolCode.split('\n').map((line, idx) => (
  <PunchCard key={idx} text={line} maxColumns={80} />
))}
```

## Character Encoding Reference

### Digits (Single Punch)
- `0` → Row 0
- `1` → Row 1
- `2` → Row 2
- ... through `9` → Row 9

### Letters A-I (12 Punch + Digit)
- `A` → Rows 12, 1
- `B` → Rows 12, 2
- ... through `I` → Rows 12, 9

### Letters J-R (11 Punch + Digit)
- `J` → Rows 11, 1
- `K` → Rows 11, 2
- ... through `R` → Rows 11, 9

### Letters S-Z (0 Punch + Digit)
- `S` → Rows 0, 2
- `T` → Rows 0, 3
- ... through `Z` → Rows 0, 9

### Special Characters
- Space → No punches
- `.` → Rows 12, 3, 8
- `,` → Rows 0, 3, 8
- `(` → Rows 12, 5, 8
- `)` → Rows 11, 5, 8
- `+` → Rows 12, 6, 8
- `-` → Row 11
- `*` → Rows 11, 4, 8
- `/` → Rows 0, 1
- `=` → Rows 3, 8
- `$` → Rows 11, 3, 8

## Integration

The PunchCard component is integrated into the CodeGeneratorModal to display generated COBOL code as authentic punch cards. When COBOL code is generated, the first 10 lines are automatically rendered as punch cards above the text display.

## Technical Details

### Props Interface

```typescript
interface PunchCardProps {
  text: string;           // Text to display on the card
  maxColumns?: number;    // Number of columns (default: 80)
}
```

### State Management

- `hoveredColumn`: Tracks which column is currently hovered for tooltip display
- Automatic uppercase conversion for all input text
- Padding/truncation to match specified column count

### Styling

- Uses Tailwind CSS for responsive design
- Vintage color scheme matching mainframe aesthetic
- Responsive hover effects and transitions

## Historical Accuracy

This implementation follows the authentic IBM 029 Keypunch encoding standard used from 1964-1978. The 029 was the most common keypunch machine during the mainframe era and was used to create punch cards for COBOL, FORTRAN, and other languages.

### Physical Card Specifications
- 80 columns × 12 rows
- 7⅜ inches × 3¼ inches (187.3mm × 82.6mm)
- Rectangular holes (not round)
- Manila card stock

## Testing

The component includes comprehensive unit tests covering:
- IBM 029 encoding table completeness
- 12-row structure
- Character mapping for digits, letters, and special characters
- Hover tooltip functionality
- Color coding for control vs data holes
- Uppercase conversion
- Legend display

Run tests with:
```bash
npm test -- PunchCard.test.tsx
```

## Future Enhancements

Potential improvements for future versions:
- Animation of "punching" holes when text changes
- Sound effects (keypunch machine sounds)
- Export as printable PDF
- Support for other encoding standards (IBM 026, EBCDIC)
- Card reader animation (cards feeding through)

## Requirements Fulfilled

This component fulfills **Requirement 11.1**:
> WHEN the Necro-Bank UI displays COBOL code THEN the system SHALL render it as ASCII punch card visualization with authentic hole patterns

---

**[END OF TAPE]**
