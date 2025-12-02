/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        // Core mainframe colors
        'mainframe-black': '#000000',
        'mainframe-green': '#00ff00',
        'mainframe-amber': '#ffb000',
        'mainframe-red': '#ff0000',
        
        // Vintage IBM color palette
        'ibm-blue': '#0f62fe',
        'ibm-cyan': '#00d4ff',
        'ibm-teal': '#009d9a',
        'ibm-green': '#24a148',
        'ibm-yellow': '#f1c21b',
        'ibm-orange': '#ff832b',
        'ibm-red': '#da1e28',
        'ibm-magenta': '#ee5396',
        'ibm-purple': '#8a3ffc',
        
        // CRT phosphor colors
        'phosphor-green': '#33ff33',
        'phosphor-amber': '#ffaa00',
        'phosphor-white': '#f0f0f0',
        
        // Panel and console colors
        'panel-gray': '#2a2a2a',
        'console-beige': '#d4c5a9',
        'tape-brown': '#8b4513',
      },
      fontFamily: {
        'mono': ['Courier New', 'monospace'],
        'ibm': ['IBM Plex Mono', 'Courier New', 'monospace'],
      },
      animation: {
        'blink': 'blink 1s step-end infinite',
        'spin-slow': 'spin 3s linear infinite',
        'pulse-slow': 'pulse 3s cubic-bezier(0.4, 0, 0.6, 1) infinite',
        'flicker': 'flicker 0.15s infinite',
      },
      keyframes: {
        blink: {
          '0%, 50%': { opacity: '1' },
          '51%, 100%': { opacity: '0' },
        },
        flicker: {
          '0%, 100%': { opacity: '1' },
          '50%': { opacity: '0.8' },
        },
      },
      boxShadow: {
        'crt': '0 0 10px rgba(0, 255, 0, 0.5)',
        'panel': 'inset 0 2px 4px rgba(0, 0, 0, 0.6)',
      },
    },
  },
  plugins: [],
}

