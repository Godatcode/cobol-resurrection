import React, { useState, useEffect } from 'react';

/**
 * VINTAGE IBM BOOT SEQUENCE COMPONENT
 * Displays authentic 1959 mainframe initialization sequence
 * Skippable after first view (stored in localStorage)
 */

const SYSTEM_BOOT_MESSAGES = [
  { delay: 0, message: "NECRO-BRIDGE SYSTEM INITIALIZATION..." },
  { delay: 600, message: "CHECKING CORE MEMORY... OK" },
  { delay: 1200, message: "LOADING CHANNEL CONTROLLERS... OK" },
  { delay: 1800, message: "INITIALIZING TAPE DRIVES... OK" },
  { delay: 2400, message: "MOUNTING SYSTEM VOLUMES... OK" },
  { delay: 3000, message: "LOADING COBOL RUNTIME... OK" },
  { delay: 3600, message: "LOADING FORTRAN RUNTIME... OK" },
  { delay: 4200, message: "LOADING PASCAL RUNTIME... OK" },
  { delay: 4800, message: "LOADING BASIC RUNTIME... OK" },
  { delay: 5400, message: "INITIALIZING NECRO-BRIDGE v1.0... OK" },
  { delay: 6000, message: "SYSTEM READY" },
];

const PROGRESS_STEPS = 11; // Number of boot messages

function BootSequence({ onComplete }) {
  const [currentStep, setCurrentStep] = useState(0);
  const [messages, setMessages] = useState([]);
  const [isSkipped, setIsSkipped] = useState(false);

  useEffect(() => {
    // Check if user has seen boot sequence before
    const hasSeenBoot = localStorage.getItem('necro-bridge-boot-seen');
    
    if (hasSeenBoot === 'true') {
      // Skip boot sequence if seen before
      onComplete();
      return;
    }

    // Execute boot sequence
    const timeouts = SYSTEM_BOOT_MESSAGES.map(({ delay, message }, index) => {
      return setTimeout(() => {
        if (!isSkipped) {
          setMessages(prev => [...prev, message]);
          setCurrentStep(index + 1);
          
          // Complete when all messages shown
          if (index === SYSTEM_BOOT_MESSAGES.length - 1) {
            setTimeout(() => {
              localStorage.setItem('necro-bridge-boot-seen', 'true');
              onComplete();
            }, 1000);
          }
        }
      }, delay);
    });

    // Cleanup timeouts on unmount or skip
    return () => {
      timeouts.forEach(timeout => clearTimeout(timeout));
    };
  }, [onComplete, isSkipped]);

  const handleSkip = () => {
    setIsSkipped(true);
    localStorage.setItem('necro-bridge-boot-seen', 'true');
    onComplete();
  };

  const progressPercentage = (currentStep / PROGRESS_STEPS) * 100;

  return (
    <div className="fixed inset-0 bg-mainframe-black z-50 flex items-center justify-center">
      <div className="w-full max-w-3xl px-8">
        {/* IBM LOGO / TITLE */}
        <div className="text-center mb-12">
          <div className="text-6xl font-mono text-mainframe-green mb-4 tracking-widest">
            ╔═══════════════════════════════╗
          </div>
          <div className="text-5xl font-mono text-mainframe-green mb-2 tracking-wider">
            NECRO-BRIDGE v1.0
          </div>
          <div className="text-xl font-mono text-mainframe-green opacity-75">
            LEGACY SYSTEM RESURRECTION PROTOCOL
          </div>
          <div className="text-6xl font-mono text-mainframe-green mt-4 tracking-widest">
            ╚═══════════════════════════════╝
          </div>
        </div>

        {/* BOOT MESSAGES */}
        <div className="bg-black border-2 border-mainframe-green p-6 mb-6 h-64 overflow-y-auto">
          {messages.map((message, index) => (
            <div 
              key={index}
              className="font-mono text-mainframe-green text-sm mb-2 animate-pulse"
            >
              {`> ${message}`}
            </div>
          ))}
          {currentStep < PROGRESS_STEPS && (
            <div className="font-mono text-mainframe-green text-sm animate-pulse">
              █
            </div>
          )}
        </div>

        {/* PROGRESS BAR */}
        <div className="mb-6">
          <div className="flex justify-between font-mono text-mainframe-green text-xs mb-2">
            <span>INITIALIZATION PROGRESS</span>
            <span>{Math.round(progressPercentage)}%</span>
          </div>
          <div className="w-full h-6 border-2 border-mainframe-green bg-black">
            <div 
              className="h-full bg-mainframe-green transition-all duration-300"
              style={{ width: `${progressPercentage}%` }}
            >
              <div className="h-full w-full animate-pulse opacity-75"></div>
            </div>
          </div>
        </div>

        {/* SKIP BUTTON */}
        <div className="text-center">
          <button
            onClick={handleSkip}
            className="font-mono text-mainframe-green text-sm border border-mainframe-green px-6 py-2 hover:bg-mainframe-green hover:text-black transition-colors"
          >
            [PRESS ANY KEY TO SKIP]
          </button>
        </div>

        {/* VINTAGE FOOTER */}
        <div className="text-center mt-8 font-mono text-mainframe-green text-xs opacity-50">
          IBM 7090 MAINFRAME EMULATION SYSTEM © 1959-2024
        </div>
      </div>
    </div>
  );
}

export default BootSequence;
