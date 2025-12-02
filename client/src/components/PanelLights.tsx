import React, { useEffect, useState, useRef } from 'react';

interface PanelLightsProps {
  isCalculating: boolean;
  calculationProgress?: number; // 0-100, optional progress indicator
}

/**
 * PanelLights Component - Authentic IBM 7090 Console Panel Visualization
 * 
 * Features:
 * - LED grid displaying blinking patterns representing memory addresses
 * - Authentic IBM 7090 console panel layout
 * - Configurable blink rates and patterns
 * - Synchronized with calculation execution
 * 
 * Requirements: 11.3
 */
function PanelLights({ isCalculating, calculationProgress = 0 }: PanelLightsProps) {
  // State for LED patterns - 8 registers × 18 bits each (IBM 7090 had 36-bit words, showing half)
  const [ledStates, setLedStates] = useState<boolean[][]>(
    Array(8).fill(null).map(() => Array(18).fill(false))
  );
  
  const [blinkPhase, setBlinkPhase] = useState(0);
  const animationFrameRef = useRef<number>();
  const lastUpdateRef = useRef<number>(Date.now());

  // Blink rate configuration (in milliseconds)
  const IDLE_BLINK_RATE = 1000; // Slow blink when idle
  const ACTIVE_BLINK_RATE = 150; // Fast blink when calculating
  const PATTERN_UPDATE_RATE = 300; // How often to change the pattern

  // IBM 7090 register names (authentic labels)
  const REGISTER_LABELS = [
    'AC',      // Accumulator
    'MQ',      // Multiplier-Quotient
    'SI',      // Storage Indicator
    'IC',      // Instruction Counter
    'XR1',     // Index Register 1
    'XR2',     // Index Register 2
    'XR4',     // Index Register 4
    'XR7',     // Index Register 7
  ];

  useEffect(() => {
    let lastPatternUpdate = Date.now();

    const animate = () => {
      const now = Date.now();
      const deltaTime = now - lastUpdateRef.current;
      lastUpdateRef.current = now;

      // Update blink phase
      const blinkRate = isCalculating ? ACTIVE_BLINK_RATE : IDLE_BLINK_RATE;
      setBlinkPhase(prev => (prev + deltaTime) % blinkRate);

      // Update LED patterns periodically
      if (now - lastPatternUpdate > PATTERN_UPDATE_RATE) {
        lastPatternUpdate = now;
        
        if (isCalculating) {
          // Active calculation: show dynamic memory address patterns
          setLedStates(prev => {
            return prev.map((register, regIdx) => {
              return register.map((_, bitIdx) => {
                // Create pseudo-random but deterministic patterns
                // Simulate memory addresses being accessed during calculation
                const seed = now + regIdx * 1000 + bitIdx * 100;
                const noise = Math.sin(seed * 0.001) * Math.cos(seed * 0.0007);
                
                // Higher probability of lights being on during calculation
                // Certain bits (like high-order bits) are more active
                const baseProbability = 0.4;
                const bitWeight = bitIdx < 6 ? 0.3 : 0.1; // High-order bits more active
                const threshold = baseProbability + bitWeight + (noise * 0.2);
                
                return Math.random() < threshold;
              });
            });
          });
        } else {
          // Idle state: show minimal activity (only a few status lights)
          setLedStates(prev => {
            return prev.map((register, regIdx) => {
              return register.map((_, bitIdx) => {
                // Only show occasional status lights when idle
                // Instruction Counter (IC) and Storage Indicator (SI) show some activity
                if (regIdx === 2 || regIdx === 3) { // SI or IC
                  return Math.random() < 0.15;
                }
                return Math.random() < 0.05;
              });
            });
          });
        }
      }

      animationFrameRef.current = requestAnimationFrame(animate);
    };

    animationFrameRef.current = requestAnimationFrame(animate);

    return () => {
      if (animationFrameRef.current) {
        cancelAnimationFrame(animationFrameRef.current);
      }
    };
  }, [isCalculating]);

  // Calculate LED opacity based on blink phase
  const blinkRate = isCalculating ? ACTIVE_BLINK_RATE : IDLE_BLINK_RATE;
  const blinkProgress = blinkPhase / blinkRate;
  const ledOpacity = 0.3 + (Math.sin(blinkProgress * Math.PI * 2) * 0.7 + 0.7) / 2;

  return (
    <div className="panel-lights-container bg-[#2a2a2a] p-6 rounded-lg border-2 border-mainframe-green shadow-lg">
      {/* Panel Header */}
      <div className="text-center mb-4">
        <div className="font-mono text-mainframe-green text-sm font-bold">
          IBM 7090 CONSOLE PANEL
        </div>
        <div className="font-mono text-mainframe-green text-xs opacity-70 mt-1">
          {isCalculating ? '⚡ PROCESSING' : '⏸ STANDBY'}
        </div>
      </div>

      {/* LED Grid */}
      <div className="space-y-3">
        {ledStates.map((register, regIdx) => (
          <div key={`register-${regIdx}`} className="flex items-center gap-2">
            {/* Register Label */}
            <div className="w-12 text-right font-mono text-mainframe-green text-xs font-bold opacity-80">
              {REGISTER_LABELS[regIdx]}
            </div>

            {/* LED Row */}
            <div className="flex gap-1 flex-1">
              {register.map((isOn, bitIdx) => (
                <div
                  key={`led-${regIdx}-${bitIdx}`}
                  className="relative"
                  style={{ width: '12px', height: '12px' }}
                >
                  {/* LED housing (always visible) */}
                  <div className="absolute inset-0 rounded-full bg-black border border-gray-600" />
                  
                  {/* LED light (visible when on) */}
                  {isOn && (
                    <>
                      {/* Glow effect */}
                      <div
                        className="absolute inset-0 rounded-full bg-mainframe-green blur-sm"
                        style={{ opacity: ledOpacity * 0.6 }}
                      />
                      {/* Core light */}
                      <div
                        className="absolute inset-0.5 rounded-full bg-mainframe-green"
                        style={{ opacity: ledOpacity }}
                      />
                    </>
                  )}
                </div>
              ))}
            </div>

            {/* Bit position markers (every 6 bits) */}
            <div className="flex gap-1 ml-1">
              {[0, 6, 12].map(pos => (
                <div
                  key={`marker-${regIdx}-${pos}`}
                  className="font-mono text-gray-500 text-[8px] w-12 text-center"
                >
                  {pos}
                </div>
              ))}
            </div>
          </div>
        ))}
      </div>

      {/* Panel Footer with Status Indicators */}
      <div className="mt-4 pt-3 border-t border-gray-600 flex justify-between items-center">
        <div className="flex gap-4">
          {/* Power indicator */}
          <div className="flex items-center gap-2">
            <div className="w-3 h-3 rounded-full bg-mainframe-green animate-pulse" />
            <span className="font-mono text-mainframe-green text-xs">PWR</span>
          </div>
          
          {/* Ready indicator */}
          <div className="flex items-center gap-2">
            <div className={`w-3 h-3 rounded-full ${isCalculating ? 'bg-red-500 animate-pulse' : 'bg-gray-600'}`} />
            <span className="font-mono text-mainframe-green text-xs">RUN</span>
          </div>
        </div>

        {/* Memory address counter (simulated) */}
        <div className="font-mono text-mainframe-green text-xs">
          ADDR: {isCalculating 
            ? `0x${Math.floor(Math.random() * 0xFFFF).toString(16).toUpperCase().padStart(4, '0')}`
            : '0x0000'
          }
        </div>
      </div>

      {/* Legend */}
      <div className="mt-3 text-center">
        <div className="font-mono text-gray-500 text-[10px]">
          BINARY REPRESENTATION OF ACTIVE MEMORY REGISTERS
        </div>
      </div>
    </div>
  );
}

export default PanelLights;
