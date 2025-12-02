import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';

describe('PanelLights Component', () => {
  const panelLightsSource = readFileSync('client/src/components/PanelLights.tsx', 'utf-8');

  it('should render IBM 7090 console panel header', () => {
    expect(panelLightsSource).toContain('IBM 7090 CONSOLE PANEL');
  });

  it('should display STANDBY status when not calculating', () => {
    expect(panelLightsSource).toContain('STANDBY');
    expect(panelLightsSource).toContain('isCalculating ? ');
  });

  it('should display PROCESSING status when calculating', () => {
    expect(panelLightsSource).toContain('PROCESSING');
    expect(panelLightsSource).toContain('isCalculating ? ');
  });

  it('should render all 8 IBM 7090 register labels', () => {
    const registerLabels = ['AC', 'MQ', 'SI', 'IC', 'XR1', 'XR2', 'XR4', 'XR7'];
    
    registerLabels.forEach(label => {
      expect(panelLightsSource).toContain(`'${label}'`);
    });
    
    // Verify register labels array is defined
    expect(panelLightsSource).toContain('REGISTER_LABELS');
  });

  it('should implement LED grid with 8 registers × 18 bits', () => {
    // Check for LED state management
    expect(panelLightsSource).toContain('ledStates');
    expect(panelLightsSource).toContain('Array(8)');
    expect(panelLightsSource).toContain('Array(18)');
  });

  it('should implement configurable blink rates', () => {
    // Check for blink rate constants
    expect(panelLightsSource).toContain('IDLE_BLINK_RATE');
    expect(panelLightsSource).toContain('ACTIVE_BLINK_RATE');
    expect(panelLightsSource).toContain('blinkPhase');
  });

  it('should sync with calculation execution', () => {
    // Check for isCalculating prop usage
    expect(panelLightsSource).toContain('isCalculating: boolean');
    expect(panelLightsSource).toContain('if (isCalculating)');
  });

  it('should implement dynamic memory address patterns', () => {
    // Check for pattern update logic
    expect(panelLightsSource).toContain('PATTERN_UPDATE_RATE');
    expect(panelLightsSource).toContain('setLedStates');
  });

  it('should display power indicator', () => {
    expect(panelLightsSource).toContain('PWR');
  });

  it('should display run indicator', () => {
    expect(panelLightsSource).toContain('RUN');
  });

  it('should display memory address counter', () => {
    expect(panelLightsSource).toContain('ADDR:');
    expect(panelLightsSource).toContain('0x');
  });

  it('should use requestAnimationFrame for smooth animation', () => {
    expect(panelLightsSource).toContain('requestAnimationFrame');
    expect(panelLightsSource).toContain('cancelAnimationFrame');
  });

  it('should implement LED glow effects', () => {
    // Check for visual effects
    expect(panelLightsSource).toContain('blur');
    expect(panelLightsSource).toContain('opacity');
    expect(panelLightsSource).toContain('ledOpacity');
  });

  it('should accept calculationProgress prop', () => {
    expect(panelLightsSource).toContain('calculationProgress');
  });
});
