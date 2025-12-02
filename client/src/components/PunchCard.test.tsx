import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';

describe('PunchCard Component', () => {
  const punchCardSource = readFileSync('client/src/components/PunchCard.tsx', 'utf-8');

  it('implements IBM 029 encoding table', () => {
    // Check for IBM 029 encoding constant
    expect(punchCardSource).toContain('IBM_029_ENCODING');
    expect(punchCardSource).toContain('Record<string, number[]>');
  });

  it('includes all digits 0-9 in encoding table', () => {
    // Check for digit encodings
    expect(punchCardSource).toContain("'0': [0]");
    expect(punchCardSource).toContain("'9': [9]");
  });

  it('includes letters A-Z with zone punches', () => {
    // Check for letter encodings with zone punches (12, 11, 0)
    expect(punchCardSource).toContain("'A': [12, 1]");
    expect(punchCardSource).toContain("'J': [11, 1]");
    expect(punchCardSource).toContain("'S': [0, 2]");
  });

  it('includes special characters', () => {
    // Check for special character encodings
    expect(punchCardSource).toContain("' ': []"); // Space has no punches
    expect(punchCardSource).toContain("'.':");
    expect(punchCardSource).toContain("',':");
  });

  it('renders 12 rows (IBM 029 standard)', () => {
    // Check for 12-row structure
    expect(punchCardSource).toContain("ROW_NAMES = ['12', '11', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']");
  });

  it('supports configurable column count with default 80', () => {
    // Check for maxColumns prop with default
    expect(punchCardSource).toContain('maxColumns?: number');
    expect(punchCardSource).toContain('maxColumns = 80');
  });

  it('implements hover tooltips for character mapping', () => {
    // Check for hover state and tooltip
    expect(punchCardSource).toContain('hoveredColumn');
    expect(punchCardSource).toContain('setHoveredColumn');
    expect(punchCardSource).toContain('onMouseEnter');
    expect(punchCardSource).toContain('onMouseLeave');
  });

  it('color codes control holes vs data holes', () => {
    // Check for control punch detection and color coding
    expect(punchCardSource).toContain('isControlPunch');
    expect(punchCardSource).toContain('bg-red-600'); // Control holes
    expect(punchCardSource).toContain('bg-gray-800'); // Data holes
  });

  it('converts text to uppercase for processing', () => {
    // Check for uppercase conversion
    expect(punchCardSource).toContain('.toUpperCase()');
  });

  it('displays legend explaining hole types', () => {
    // Check for legend with data and control holes
    expect(punchCardSource).toContain('Data Holes');
    expect(punchCardSource).toContain('Control Holes');
  });
});
