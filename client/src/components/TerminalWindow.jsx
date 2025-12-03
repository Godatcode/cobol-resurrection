import React, { useState, useEffect, useRef } from 'react';

function TerminalWindow({ logs, isCalculating }) {
  const terminalRef = useRef(null);
  const [displayedLogs, setDisplayedLogs] = useState([]);
  const [konamiSequence, setKonamiSequence] = useState([]);
  const [showManual, setShowManual] = useState(false);
  
  // Konami code sequence: up, up, down, down, left, right, left, right, b, a
  const KONAMI_CODE = ['ArrowUp', 'ArrowUp', 'ArrowDown', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'ArrowLeft', 'ArrowRight', 'b', 'a'];
  
  // Operator manual easter egg content
  const OPERATOR_MANUAL = [
    "═══════════════════════════════════════════════════════════",
    "  IBM 7090 OPERATOR MANUAL - CLASSIFIED DOCUMENT",
    "  NECRO-BANK SYSTEM v1.0 - RESURRECTION PROTOCOLS",
    "═══════════════════════════════════════════════════════════",
    "",
    "SECTION 1: EMERGENCY PROCEDURES",
    "  - IN CASE OF MOTH INFESTATION: APPLY DEBUGGING PROTOCOL",
    "  - CORE DUMP DETECTED: CONSULT HEXADECIMAL MEMORY DISPLAY",
    "  - TAPE DRIVE MALFUNCTION: REWIND AND REMOUNT VOLUME",
    "",
    "SECTION 2: ANCIENT SPIRIT SUMMONING",
    "  - USE AI CODE GENERATOR FOR LEGACY LANGUAGE RESURRECTION",
    "  - SUPPORTED LANGUAGES: COBOL (1959), FORTRAN (1957),",
    "    PASCAL (1970), BASIC (1983)",
    "",
    "SECTION 3: MAINTENANCE SCHEDULE",
    "  - VACUUM TUBES: INSPECT DAILY FOR BURNOUT",
    "  - PUNCH CARDS: KEEP DRY, AVOID FOLDING OR MUTILATING",
    "  - MAGNETIC TAPE: STORE IN CLIMATE-CONTROLLED VAULT",
    "",
    "SECTION 4: KNOWN ISSUES",
    "  - SYSTEM MAY SPONTANEOUSLY RESURRECT DEAD CODE",
    "  - PHANTOM CALCULATIONS REPORTED AFTER MIDNIGHT",
    "  - MAINFRAME OCCASIONALLY SPEAKS IN BINARY",
    "",
    "═══════════════════════════════════════════════════════════",
    "  END OF MANUAL - PRESS ANY KEY TO CLOSE",
    "═══════════════════════════════════════════════════════════",
  ];

  // Auto-scroll to bottom when new logs are added
  useEffect(() => {
    if (terminalRef.current) {
      terminalRef.current.scrollTop = terminalRef.current.scrollHeight;
    }
  }, [displayedLogs]);

  // Typewriter effect for new logs
  useEffect(() => {
    if (logs.length > displayedLogs.length) {
      const newLog = logs[logs.length - 1];
      const chars = newLog.message.split('');
      let currentIndex = 0;
      
      // Add log with empty message first
      setDisplayedLogs(prev => [...prev, { ...newLog, message: '' }]);
      
      // Typewriter effect - reveal characters one by one
      const typewriterInterval = setInterval(() => {
        if (currentIndex < chars.length) {
          setDisplayedLogs(prev => {
            const updated = [...prev];
            const lastIndex = updated.length - 1;
            updated[lastIndex] = {
              ...updated[lastIndex],
              message: chars.slice(0, currentIndex + 1).join('')
            };
            return updated;
          });
          currentIndex++;
        } else {
          clearInterval(typewriterInterval);
        }
      }, 20); // 20ms per character for authentic teletype speed
      
      return () => clearInterval(typewriterInterval);
    }
  }, [logs]);

  // Konami code detection
  useEffect(() => {
    const handleKeyDown = (e) => {
      // Close manual if open
      if (showManual) {
        setShowManual(false);
        return;
      }
      
      const newSequence = [...konamiSequence, e.key].slice(-KONAMI_CODE.length);
      setKonamiSequence(newSequence);
      
      // Check if Konami code is complete
      if (newSequence.length === KONAMI_CODE.length && 
          newSequence.every((key, index) => key === KONAMI_CODE[index])) {
        setShowManual(true);
        setKonamiSequence([]); // Reset sequence
      }
    };
    
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [konamiSequence, showManual]);

  return (
    <div className="max-w-2xl mx-auto p-6">
      <div className="border-2 border-mainframe-green bg-mainframe-black relative overflow-hidden">
        {/* CRT Scanline Overlay */}
        <div 
          className="absolute inset-0 pointer-events-none z-10"
          style={{
            background: 'repeating-linear-gradient(0deg, rgba(0, 0, 0, 0.15) 0px, rgba(0, 0, 0, 0.15) 1px, transparent 1px, transparent 2px)',
            animation: 'scanline 8s linear infinite'
          }}
        />
        
        {/* CRT Phosphor Glow Effect */}
        <div 
          className="absolute inset-0 pointer-events-none z-10"
          style={{
            boxShadow: 'inset 0 0 100px rgba(0, 255, 0, 0.1), inset 0 0 50px rgba(0, 255, 0, 0.05)',
            background: 'radial-gradient(ellipse at center, rgba(0, 255, 0, 0.02) 0%, transparent 70%)'
          }}
        />
        
        {/* Terminal Header */}
        <div className="border-b-2 border-mainframe-green px-4 py-2 relative z-20">
          <span 
            className="font-mono text-mainframe-green text-sm"
            style={{ textShadow: '0 0 5px rgba(0, 255, 0, 0.7)' }}
          >
            SYSTEM CONSOLE - TAPE DRIVE #001
          </span>
        </div>

        {/* Terminal Content */}
        <div 
          ref={terminalRef}
          className="h-96 overflow-y-auto p-4 font-mono text-mainframe-green text-sm relative z-20"
          style={{ 
            fontFamily: "'Courier New', monospace",
            textShadow: '0 0 5px rgba(0, 255, 0, 0.5)'
          }}
        >
          {showManual ? (
            // Operator Manual Easter Egg
            <div className="whitespace-pre font-mono">
              {OPERATOR_MANUAL.map((line, index) => (
                <div key={index} className="mb-1">
                  {line}
                </div>
              ))}
            </div>
          ) : (
            <>
              {displayedLogs.length === 0 ? (
                <div className="opacity-50 animate-pulse">
                  AWAITING INPUT...
                </div>
              ) : (
                displayedLogs.map((log, index) => (
                  <div key={index} className="mb-1">
                    <span className="opacity-70">[{log.timestamp}]</span>{' '}
                    <span className={log.type === 'error' ? 'text-red-500' : ''}>
                      {log.message}
                    </span>
                  </div>
                ))
              )}
              {isCalculating && (
                <div className="animate-pulse">▮</div>
              )}
            </>
          )}
        </div>
      </div>
      
      {/* Hint for Konami code (subtle) */}
      <div className="text-center mt-2 text-mainframe-green opacity-20 text-xs font-mono">
        TIP: TRY THE KONAMI CODE FOR OPERATOR MANUAL
      </div>
    </div>
  );
}

export default TerminalWindow;
