import React, { useState, useEffect, useRef } from 'react';

function TerminalWindow({ logs, isCalculating }) {
  const terminalRef = useRef(null);

  // Auto-scroll to bottom when new logs are added
  useEffect(() => {
    if (terminalRef.current) {
      terminalRef.current.scrollTop = terminalRef.current.scrollHeight;
    }
  }, [logs]);

  return (
    <div className="max-w-2xl mx-auto p-6">
      <div className="border-2 border-mainframe-green bg-mainframe-black">
        {/* Terminal Header */}
        <div className="border-b-2 border-mainframe-green px-4 py-2">
          <span className="font-mono text-mainframe-green text-sm">
            SYSTEM CONSOLE - TAPE DRIVE #001
          </span>
        </div>

        {/* Terminal Content */}
        <div 
          ref={terminalRef}
          className="h-96 overflow-y-auto p-4 font-mono text-mainframe-green text-sm"
          style={{ fontFamily: "'Courier New', monospace" }}
        >
          {logs.length === 0 ? (
            <div className="opacity-50">
              AWAITING INPUT...
            </div>
          ) : (
            logs.map((log, index) => (
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
        </div>
      </div>
    </div>
  );
}

export default TerminalWindow;
