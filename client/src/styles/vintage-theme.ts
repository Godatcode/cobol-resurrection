/**
 * VINTAGE MAINFRAME THEME CONFIGURATION
 * IBM 7090 Era Color Palette and Typography
 * 
 * This module provides centralized theme constants for the
 * NECRO-BANK immersive mainframe museum experience.
 */

export const vintageColors = {
  // Core mainframe terminal colors
  mainframe: {
    black: '#000000',
    green: '#00ff00',
    amber: '#ffb000',
    red: '#ff0000',
  },
  
  // IBM corporate palette (1960s era)
  ibm: {
    blue: '#0f62fe',
    cyan: '#00d4ff',
    teal: '#009d9a',
    green: '#24a148',
    yellow: '#f1c21b',
    orange: '#ff832b',
    red: '#da1e28',
    magenta: '#ee5396',
    purple: '#8a3ffc',
  },
  
  // CRT phosphor glow colors
  phosphor: {
    green: '#33ff33',
    amber: '#ffaa00',
    white: '#f0f0f0',
  },
  
  // Hardware panel colors
  panel: {
    gray: '#2a2a2a',
    beige: '#d4c5a9',
    tapeBrown: '#8b4513',
  },
} as const;

export const vintageFonts = {
  mono: "'Courier New', monospace",
  ibm: "'IBM Plex Mono', 'Courier New', monospace",
} as const;

export const vintageAnimations = {
  blink: 'blink 1s step-end infinite',
  spinSlow: 'spin 3s linear infinite',
  pulseSlow: 'pulse 3s cubic-bezier(0.4, 0, 0.6, 1) infinite',
  flicker: 'flicker 0.15s infinite',
} as const;

export const vintageShadows = {
  crt: '0 0 10px rgba(0, 255, 0, 0.5)',
  panel: 'inset 0 2px 4px rgba(0, 0, 0, 0.6)',
} as const;

/**
 * Type-safe theme access
 */
export type VintageTheme = {
  colors: typeof vintageColors;
  fonts: typeof vintageFonts;
  animations: typeof vintageAnimations;
  shadows: typeof vintageShadows;
};

export const vintageTheme: VintageTheme = {
  colors: vintageColors,
  fonts: vintageFonts,
  animations: vintageAnimations,
  shadows: vintageShadows,
};
