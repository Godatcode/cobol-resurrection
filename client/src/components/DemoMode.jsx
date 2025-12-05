import React, { useState, useEffect } from 'react';

/**
 * DEMO MODE COMPONENT
 * Provides guided tour with tooltips, auto-play demo sequence, and shareable demo link
 * Requirements: 13.2, 13.3
 */

const DEMO_STEPS = [
  {
    id: 'header',
    title: 'NECRO-BANK MAINFRAME SYSTEM',
    description: 'WELCOME TO THE CYBER-NECROMANCY RESURRECTION BRIDGE. THIS SYSTEM RESURRECTS ANCIENT PROGRAMMING LANGUAGES FROM THE 1950s-1980s ERA.',
    target: 'header',
    position: 'bottom'
  },
  {
    id: 'code-generator',
    title: 'AI CODE GENERATOR',
    description: 'SUMMON ANCIENT SPIRITS TO GENERATE LEGACY CODE ON DEMAND. THE AI CREATES SYNTACTICALLY CORRECT VINTAGE CODE IN COBOL, FORTRAN, PASCAL, OR BASIC.',
    target: 'code-generator-button',
    position: 'bottom'
  },
  {
    id: 'language-selector',
    title: 'MULTI-LANGUAGE SUPPORT',
    description: 'SELECT FROM 4 LEGACY LANGUAGES: COBOL (1959), FORTRAN (1957), PASCAL (1970), BASIC (1983). EACH LANGUAGE PERFORMS DIFFERENT CALCULATIONS.',
    target: 'language-selector',
    position: 'bottom'
  },
  {
    id: 'calculator-form',
    title: 'LEGACY CALCULATOR INTERFACE',
    description: 'ENTER PARAMETERS FOR YOUR SELECTED LEGACY LANGUAGE. THE SYSTEM SPAWNS THE COMPILED BINARY AND CAPTURES OUTPUT VIA BRIDGE SERVER.',
    target: 'calculator-form',
    position: 'right'
  },
  {
    id: 'history-panel',
    title: 'CALCULATION HISTORY',
    description: 'VIEW PREVIOUS CALCULATIONS WITH TIMESTAMPS AND SOURCE LANGUAGE. TRACK WHICH ANCIENT ENGINE PROCESSED EACH REQUEST.',
    target: 'history-panel',
    position: 'left'
  },
  {
    id: 'tape-reel',
    title: 'IBM 729 TAPE DRIVE',
    description: 'AUTHENTIC TAPE REEL ANIMATION WITH REALISTIC PHYSICS. SPINS DURING CALCULATIONS TO SIMULATE VINTAGE MAINFRAME OPERATIONS.',
    target: 'tape-reel',
    position: 'top'
  },
  {
    id: 'panel-lights',
    title: 'IBM 7090 CONSOLE PANEL',
    description: 'BLINKING PANEL LIGHTS DISPLAY BINARY PATTERNS REPRESENTING MEMORY ADDRESSES DURING EXECUTION. AUTHENTIC MAINFRAME VISUALIZATION.',
    target: 'panel-lights',
    position: 'top'
  },
  {
    id: 'terminal',
    title: 'SYSTEM CONSOLE',
    description: 'VINTAGE TERMINAL WITH CRT PHOSPHOR GLOW AND SCANLINE EFFECTS. DISPLAYS BOOT SEQUENCE AND CALCULATION RESULTS. TRY THE KONAMI CODE!',
    target: 'terminal',
    position: 'top'
  },
  {
    id: 'volume-control',
    title: 'AUDIO SYSTEM',
    description: 'AUTHENTIC MAINFRAME SOUND EFFECTS: TELETYPE CLACKING, TAPE DRIVE MOTORS, CARD READERS, BELLS, AND BUZZERS. ADJUST VOLUME HERE.',
    target: 'volume-control',
    position: 'left'
  }
];

const AUTO_PLAY_SEQUENCE = [
  { step: 0, delay: 0, action: 'show' },
  { step: 1, delay: 4000, action: 'show' },
  { step: 2, delay: 8000, action: 'show' },
  { step: 3, delay: 12000, action: 'show' },
  { step: 4, delay: 16000, action: 'show' },
  { step: 5, delay: 20000, action: 'show' },
  { step: 6, delay: 24000, action: 'show' },
  { step: 7, delay: 28000, action: 'show' },
  { step: 8, delay: 32000, action: 'show' },
  { step: null, delay: 36000, action: 'complete' }
];

function DemoMode({ isActive, onClose, onAutoPlayComplete }) {
  const [currentStep, setCurrentStep] = useState(0);
  const [isAutoPlay, setIsAutoPlay] = useState(false);
  const [tooltipPosition, setTooltipPosition] = useState({ top: 0, left: 0 });
  const [showShareModal, setShowShareModal] = useState(false);
  const [copySuccess, setCopySuccess] = useState(false);

  // AUTO-PLAY SEQUENCE
  useEffect(() => {
    if (!isAutoPlay || !isActive) return;

    const timeouts = AUTO_PLAY_SEQUENCE.map(({ step, delay, action }) => {
      return setTimeout(() => {
        if (action === 'show' && step !== null) {
          setCurrentStep(step);
          highlightElement(DEMO_STEPS[step].target);
        } else if (action === 'complete') {
          setIsAutoPlay(false);
          if (onAutoPlayComplete) {
            onAutoPlayComplete();
          }
        }
      }, delay);
    });

    return () => {
      timeouts.forEach(timeout => clearTimeout(timeout));
    };
  }, [isAutoPlay, isActive, onAutoPlayComplete]);

  // CALCULATE TOOLTIP POSITION
  useEffect(() => {
    if (!isActive) return;

    const step = DEMO_STEPS[currentStep];
    const targetElement = document.getElementById(step.target);
    
    if (targetElement) {
      const rect = targetElement.getBoundingClientRect();
      let top = 0;
      let left = 0;

      switch (step.position) {
        case 'bottom':
          top = rect.bottom + window.scrollY + 20;
          left = rect.left + window.scrollX + (rect.width / 2);
          break;
        case 'top':
          top = rect.top + window.scrollY - 20;
          left = rect.left + window.scrollX + (rect.width / 2);
          break;
        case 'left':
          top = rect.top + window.scrollY + (rect.height / 2);
          left = rect.left + window.scrollX - 20;
          break;
        case 'right':
          top = rect.top + window.scrollY + (rect.height / 2);
          left = rect.right + window.scrollX + 20;
          break;
        default:
          top = rect.bottom + window.scrollY + 20;
          left = rect.left + window.scrollX + (rect.width / 2);
      }

      setTooltipPosition({ top, left });
      
      // SCROLL TO ELEMENT
      targetElement.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }, [currentStep, isActive]);

  const highlightElement = (targetId) => {
    // REMOVE PREVIOUS HIGHLIGHTS
    document.querySelectorAll('.demo-highlight').forEach(el => {
      el.classList.remove('demo-highlight');
    });

    // ADD HIGHLIGHT TO TARGET
    const element = document.getElementById(targetId);
    if (element) {
      element.classList.add('demo-highlight');
    }
  };

  const handleNext = () => {
    if (currentStep < DEMO_STEPS.length - 1) {
      setCurrentStep(currentStep + 1);
      highlightElement(DEMO_STEPS[currentStep + 1].target);
    } else {
      handleClose();
    }
  };

  const handlePrevious = () => {
    if (currentStep > 0) {
      setCurrentStep(currentStep - 1);
      highlightElement(DEMO_STEPS[currentStep - 1].target);
    }
  };

  const handleClose = () => {
    // REMOVE ALL HIGHLIGHTS
    document.querySelectorAll('.demo-highlight').forEach(el => {
      el.classList.remove('demo-highlight');
    });
    setIsAutoPlay(false);
    if (onClose) {
      onClose();
    }
  };

  const handleStartAutoPlay = () => {
    setIsAutoPlay(true);
    setCurrentStep(0);
  };

  const handleStopAutoPlay = () => {
    setIsAutoPlay(false);
  };

  const handleShare = () => {
    setShowShareModal(true);
  };

  const handleCopyLink = () => {
    const demoUrl = `${window.location.origin}?demo=true`;
    navigator.clipboard.writeText(demoUrl).then(() => {
      setCopySuccess(true);
      setTimeout(() => setCopySuccess(false), 2000);
    });
  };

  if (!isActive) return null;

  const step = DEMO_STEPS[currentStep];

  return (
    <>
      {/* OVERLAY */}
      <div 
        className="fixed inset-0 bg-black bg-opacity-80 z-40"
        onClick={handleClose}
      />

      {/* DEMO CONTROL PANEL */}
      <div className="fixed top-4 right-4 z-50 bg-black border-2 border-mainframe-green p-4 max-w-sm">
        <div className="flex justify-between items-center mb-4">
          <h3 className="text-mainframe-green font-mono text-lg">
            GUIDED TOUR
          </h3>
          <button
            onClick={handleClose}
            className="text-mainframe-green font-mono hover:bg-mainframe-green hover:text-black px-2 py-1"
          >
            ✕
          </button>
        </div>

        <div className="space-y-3">
          {/* PROGRESS */}
          <div className="text-mainframe-green font-mono text-sm">
            STEP {currentStep + 1} OF {DEMO_STEPS.length}
          </div>

          {/* PROGRESS BAR */}
          <div className="w-full bg-black border border-mainframe-green h-2">
            <div 
              className="bg-mainframe-green h-full transition-all duration-300"
              style={{ width: `${((currentStep + 1) / DEMO_STEPS.length) * 100}%` }}
            />
          </div>

          {/* AUTO-PLAY CONTROLS */}
          <div className="flex gap-2">
            {!isAutoPlay ? (
              <button
                onClick={handleStartAutoPlay}
                className="flex-1 bg-black border border-mainframe-green text-mainframe-green font-mono py-2 px-3 hover:bg-mainframe-green hover:text-black transition-colors text-sm"
              >
                ▶ AUTO-PLAY
              </button>
            ) : (
              <button
                onClick={handleStopAutoPlay}
                className="flex-1 bg-mainframe-green text-black font-mono py-2 px-3 hover:bg-black hover:text-mainframe-green border border-mainframe-green transition-colors text-sm"
              >
                ⏸ PAUSE
              </button>
            )}
            <button
              onClick={handleShare}
              className="flex-1 bg-black border border-mainframe-green text-mainframe-green font-mono py-2 px-3 hover:bg-mainframe-green hover:text-black transition-colors text-sm"
            >
              🔗 SHARE
            </button>
          </div>

          {/* NAVIGATION */}
          <div className="flex gap-2">
            <button
              onClick={handlePrevious}
              disabled={currentStep === 0}
              className="flex-1 bg-black border border-mainframe-green text-mainframe-green font-mono py-2 px-3 hover:bg-mainframe-green hover:text-black transition-colors disabled:opacity-30 disabled:cursor-not-allowed text-sm"
            >
              ← PREVIOUS
            </button>
            <button
              onClick={handleNext}
              className="flex-1 bg-black border border-mainframe-green text-mainframe-green font-mono py-2 px-3 hover:bg-mainframe-green hover:text-black transition-colors text-sm"
            >
              {currentStep === DEMO_STEPS.length - 1 ? 'FINISH' : 'NEXT →'}
            </button>
          </div>

          {/* TRY IT YOURSELF PROMPT */}
          <div className="border-t border-mainframe-green pt-3 mt-3">
            <p className="text-mainframe-green font-mono text-xs mb-2">
              💡 TRY IT YOURSELF:
            </p>
            <p className="text-mainframe-green font-mono text-xs opacity-70">
              PAUSE THE TOUR AND INTERACT WITH THE HIGHLIGHTED COMPONENT. RESUME WHEN READY.
            </p>
          </div>
        </div>
      </div>

      {/* TOOLTIP */}
      <div
        className="fixed z-50 bg-black border-2 border-mainframe-green p-4 max-w-md transform -translate-x-1/2"
        style={{
          top: `${tooltipPosition.top}px`,
          left: `${tooltipPosition.left}px`,
        }}
      >
        <div className="flex items-start gap-3">
          <div className="text-mainframe-green text-2xl">
            {currentStep === 0 ? '👻' : '📍'}
          </div>
          <div className="flex-1">
            <h4 className="text-mainframe-green font-mono font-bold mb-2">
              {step.title}
            </h4>
            <p className="text-mainframe-green font-mono text-sm leading-relaxed">
              {step.description}
            </p>
          </div>
        </div>

        {/* TOOLTIP ARROW */}
        <div
          className="absolute w-0 h-0 border-8"
          style={{
            ...(step.position === 'bottom' && {
              top: '-16px',
              left: '50%',
              transform: 'translateX(-50%)',
              borderLeft: '8px solid transparent',
              borderRight: '8px solid transparent',
              borderBottom: '8px solid #00ff00',
              borderTop: 'none'
            }),
            ...(step.position === 'top' && {
              bottom: '-16px',
              left: '50%',
              transform: 'translateX(-50%)',
              borderLeft: '8px solid transparent',
              borderRight: '8px solid transparent',
              borderTop: '8px solid #00ff00',
              borderBottom: 'none'
            }),
            ...(step.position === 'left' && {
              right: '-16px',
              top: '50%',
              transform: 'translateY(-50%)',
              borderTop: '8px solid transparent',
              borderBottom: '8px solid transparent',
              borderLeft: '8px solid #00ff00',
              borderRight: 'none'
            }),
            ...(step.position === 'right' && {
              left: '-16px',
              top: '50%',
              transform: 'translateY(-50%)',
              borderTop: '8px solid transparent',
              borderBottom: '8px solid transparent',
              borderRight: '8px solid #00ff00',
              borderLeft: 'none'
            })
          }}
        />
      </div>

      {/* SHARE MODAL */}
      {showShareModal && (
        <div className="fixed inset-0 flex items-center justify-center z-60">
          <div 
            className="absolute inset-0 bg-black bg-opacity-90"
            onClick={() => setShowShareModal(false)}
          />
          <div className="relative bg-black border-2 border-mainframe-green p-6 max-w-lg w-full mx-4">
            <h3 className="text-mainframe-green font-mono text-xl mb-4">
              SHARE DEMO LINK
            </h3>
            
            <p className="text-mainframe-green font-mono text-sm mb-4">
              COPY THIS LINK TO SHARE THE NECRO-BANK DEMO WITH OTHERS:
            </p>

            <div className="flex gap-2 mb-4">
              <input
                type="text"
                value={`${window.location.origin}?demo=true`}
                readOnly
                className="flex-1 bg-black border border-mainframe-green text-mainframe-green font-mono px-3 py-2 text-sm"
              />
              <button
                onClick={handleCopyLink}
                className="bg-black border border-mainframe-green text-mainframe-green font-mono px-4 py-2 hover:bg-mainframe-green hover:text-black transition-colors"
              >
                {copySuccess ? '✓ COPIED' : 'COPY'}
              </button>
            </div>

            <div className="border-t border-mainframe-green pt-4 mt-4">
              <p className="text-mainframe-green font-mono text-xs mb-3">
                GITHUB REPOSITORY:
              </p>
              <a
                href="https://github.com/yourusername/cobol-resurrection-bridge"
                target="_blank"
                rel="noopener noreferrer"
                className="block bg-black border border-mainframe-green text-mainframe-green font-mono px-3 py-2 hover:bg-mainframe-green hover:text-black transition-colors text-center text-sm"
              >
                ⭐ STAR ON GITHUB
              </a>
            </div>

            <button
              onClick={() => setShowShareModal(false)}
              className="mt-4 w-full bg-black border border-mainframe-green text-mainframe-green font-mono py-2 hover:bg-mainframe-green hover:text-black transition-colors"
            >
              CLOSE
            </button>
          </div>
        </div>
      )}
    </>
  );
}

export default DemoMode;
