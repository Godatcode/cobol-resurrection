import React, { useEffect, useState, useRef } from 'react';

interface TapeReelProps {
  isCalculating: boolean;
  calculationProgress?: number; // 0-100, optional progress indicator
}

/**
 * TapeReel Component - Authentic IBM 729 Magnetic Tape Drive Visualization
 * 
 * Features:
 * - Realistic physics with acceleration/deceleration
 * - Rotation speed synchronized with calculation status
 * - Authentic IBM 729 tape drive styling
 * - SVG-based animations for smooth performance
 * 
 * Requirements: 11.2
 */
function TapeReel({ isCalculating, calculationProgress = 0 }: TapeReelProps) {
  const [rotation, setRotation] = useState(0);
  const [velocity, setVelocity] = useState(0);
  const animationFrameRef = useRef<number>();
  const lastTimeRef = useRef<number>(Date.now());

  // Physics constants for realistic tape reel behavior
  const MAX_VELOCITY = 360; // degrees per second when calculating
  const ACCELERATION = 180; // degrees per second squared
  const DECELERATION = 240; // degrees per second squared (faster stop)
  const IDLE_VELOCITY = 30; // slow rotation when idle

  useEffect(() => {
    const animate = () => {
      const now = Date.now();
      const deltaTime = (now - lastTimeRef.current) / 1000; // Convert to seconds
      lastTimeRef.current = now;

      setVelocity(prevVelocity => {
        let newVelocity = prevVelocity;
        
        if (isCalculating) {
          // Accelerate to max velocity when calculating
          if (newVelocity < MAX_VELOCITY) {
            newVelocity = Math.min(MAX_VELOCITY, newVelocity + ACCELERATION * deltaTime);
          }
        } else {
          // Decelerate to idle velocity when not calculating
          if (newVelocity > IDLE_VELOCITY) {
            newVelocity = Math.max(IDLE_VELOCITY, newVelocity - DECELERATION * deltaTime);
          } else if (newVelocity < IDLE_VELOCITY) {
            newVelocity = Math.min(IDLE_VELOCITY, newVelocity + ACCELERATION * deltaTime);
          }
        }
        
        return newVelocity;
      });

      setRotation(prevRotation => {
        const newRotation = (prevRotation + velocity * deltaTime) % 360;
        return newRotation;
      });

      animationFrameRef.current = requestAnimationFrame(animate);
    };

    animationFrameRef.current = requestAnimationFrame(animate);

    return () => {
      if (animationFrameRef.current) {
        cancelAnimationFrame(animationFrameRef.current);
      }
    };
  }, [isCalculating, velocity]);

  // Calculate opacity based on calculation status for visual feedback
  const reelOpacity = isCalculating ? 1.0 : 0.6;
  const glowIntensity = isCalculating ? 0.3 : 0;

  return (
    <div className="flex justify-center items-center gap-8 py-6">
      {/* Left Tape Reel */}
      <div className="relative">
        <svg
          width="120"
          height="120"
          viewBox="0 0 120 120"
          className="transition-opacity duration-300"
          style={{ opacity: reelOpacity }}
        >
          {/* Glow effect when calculating */}
          {isCalculating && (
            <defs>
              <filter id="glow">
                <feGaussianBlur stdDeviation="3" result="coloredBlur"/>
                <feMerge>
                  <feMergeNode in="coloredBlur"/>
                  <feMergeNode in="SourceGraphic"/>
                </feMerge>
              </filter>
            </defs>
          )}
          
          {/* Outer reel housing */}
          <circle
            cx="60"
            cy="60"
            r="55"
            fill="#000000"
            stroke="#00ff00"
            strokeWidth="2"
            filter={isCalculating ? "url(#glow)" : undefined}
          />
          
          {/* Tape reel hub (rotating) */}
          <g transform={`rotate(${rotation} 60 60)`}>
            {/* Hub center */}
            <circle
              cx="60"
              cy="60"
              r="15"
              fill="#00ff00"
              opacity="0.8"
            />
            
            {/* Tape spokes (6 spokes for authentic look) */}
            {[0, 60, 120, 180, 240, 300].map((angle, index) => (
              <line
                key={index}
                x1="60"
                y1="60"
                x2={60 + 40 * Math.cos((angle * Math.PI) / 180)}
                y2={60 + 40 * Math.sin((angle * Math.PI) / 180)}
                stroke="#00ff00"
                strokeWidth="3"
                opacity="0.7"
              />
            ))}
            
            {/* Tape wound on reel */}
            <circle
              cx="60"
              cy="60"
              r="35"
              fill="none"
              stroke="#00ff00"
              strokeWidth="8"
              opacity="0.4"
            />
            <circle
              cx="60"
              cy="60"
              r="28"
              fill="none"
              stroke="#00ff00"
              strokeWidth="6"
              opacity="0.3"
            />
            <circle
              cx="60"
              cy="60"
              r="22"
              fill="none"
              stroke="#00ff00"
              strokeWidth="4"
              opacity="0.2"
            />
          </g>
          
          {/* Center spindle */}
          <circle
            cx="60"
            cy="60"
            r="8"
            fill="#00ff00"
          />
        </svg>
        
        {/* Reel label */}
        <div className="text-center mt-2 font-mono text-mainframe-green text-xs opacity-70">
          REEL A
        </div>
      </div>

      {/* Tape path indicator */}
      <div className="flex flex-col items-center">
        <div className="font-mono text-mainframe-green text-xs mb-2">
          {isCalculating ? '⚡ READING' : '⏸ IDLE'}
        </div>
        <svg width="60" height="40" viewBox="0 0 60 40">
          {/* Tape path line */}
          <path
            d="M 5 20 Q 30 10, 55 20"
            fill="none"
            stroke="#00ff00"
            strokeWidth="2"
            opacity="0.5"
          />
          {/* Animated tape movement when calculating */}
          {isCalculating && (
            <>
              <circle cx="15" cy="18" r="2" fill="#00ff00" opacity="0.8">
                <animate
                  attributeName="cx"
                  from="15"
                  to="45"
                  dur="1s"
                  repeatCount="indefinite"
                />
              </circle>
              <circle cx="30" cy="15" r="2" fill="#00ff00" opacity="0.6">
                <animate
                  attributeName="cx"
                  from="30"
                  to="60"
                  dur="1s"
                  repeatCount="indefinite"
                />
              </circle>
            </>
          )}
        </svg>
        <div className="font-mono text-mainframe-green text-xs mt-1">
          {Math.round(velocity)} RPM
        </div>
      </div>

      {/* Right Tape Reel (counter-rotating) */}
      <div className="relative">
        <svg
          width="120"
          height="120"
          viewBox="0 0 120 120"
          className="transition-opacity duration-300"
          style={{ opacity: reelOpacity }}
        >
          {/* Outer reel housing */}
          <circle
            cx="60"
            cy="60"
            r="55"
            fill="#000000"
            stroke="#00ff00"
            strokeWidth="2"
            filter={isCalculating ? "url(#glow)" : undefined}
          />
          
          {/* Tape reel hub (rotating opposite direction) */}
          <g transform={`rotate(${-rotation} 60 60)`}>
            {/* Hub center */}
            <circle
              cx="60"
              cy="60"
              r="15"
              fill="#00ff00"
              opacity="0.8"
            />
            
            {/* Tape spokes */}
            {[0, 60, 120, 180, 240, 300].map((angle, index) => (
              <line
                key={index}
                x1="60"
                y1="60"
                x2={60 + 40 * Math.cos((angle * Math.PI) / 180)}
                y2={60 + 40 * Math.sin((angle * Math.PI) / 180)}
                stroke="#00ff00"
                strokeWidth="3"
                opacity="0.7"
              />
            ))}
            
            {/* Tape wound on reel */}
            <circle
              cx="60"
              cy="60"
              r="35"
              fill="none"
              stroke="#00ff00"
              strokeWidth="8"
              opacity="0.4"
            />
            <circle
              cx="60"
              cy="60"
              r="28"
              fill="none"
              stroke="#00ff00"
              strokeWidth="6"
              opacity="0.3"
            />
            <circle
              cx="60"
              cy="60"
              r="22"
              fill="none"
              stroke="#00ff00"
              strokeWidth="4"
              opacity="0.2"
            />
          </g>
          
          {/* Center spindle */}
          <circle
            cx="60"
            cy="60"
            r="8"
            fill="#00ff00"
          />
        </svg>
        
        {/* Reel label */}
        <div className="text-center mt-2 font-mono text-mainframe-green text-xs opacity-70">
          REEL B
        </div>
      </div>
    </div>
  );
}

export default TapeReel;
