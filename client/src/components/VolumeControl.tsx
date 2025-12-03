/**
 * VOLUME CONTROL COMPONENT
 * Vintage-styled audio controls for the NECRO-BANK system
 */

import { useState, useEffect } from 'react';
import { audioManager } from '../services/AudioManager';

export const VolumeControl: React.FC = () => {
  const [volume, setVolume] = useState(audioManager.getVolume());
  const [muted, setMuted] = useState(audioManager.isMuted());
  const [showSlider, setShowSlider] = useState(false);

  useEffect(() => {
    // Initialize audio manager on mount
    audioManager.initialize().catch(console.error);
  }, []);

  const handleVolumeChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const newVolume = parseFloat(e.target.value);
    setVolume(newVolume);
    audioManager.setVolume(newVolume);
    
    // Unmute if volume is increased
    if (newVolume > 0 && muted) {
      setMuted(false);
      audioManager.setMuted(false);
    }
  };

  const handleMuteToggle = () => {
    const newMuted = !muted;
    setMuted(newMuted);
    audioManager.setMuted(newMuted);
  };

  const handleTestSound = () => {
    audioManager.playBell();
  };

  return (
    <div className="fixed bottom-4 right-4 z-50">
      <div className="bg-black border-2 border-mainframe-green p-3 font-mono text-mainframe-green">
        <div className="flex items-center gap-3">
          {/* Mute Toggle Button */}
          <button
            onClick={handleMuteToggle}
            className="hover:bg-mainframe-green hover:text-black transition-colors px-2 py-1 border border-mainframe-green"
            title={muted ? "UNMUTE AUDIO" : "MUTE AUDIO"}
          >
            {muted ? '🔇' : '🔊'}
          </button>

          {/* Volume Slider Toggle */}
          <button
            onClick={() => setShowSlider(!showSlider)}
            className="hover:bg-mainframe-green hover:text-black transition-colors px-2 py-1 border border-mainframe-green text-xs"
            title="VOLUME CONTROL"
          >
            VOL
          </button>

          {/* Test Sound Button */}
          <button
            onClick={handleTestSound}
            className="hover:bg-mainframe-green hover:text-black transition-colors px-2 py-1 border border-mainframe-green text-xs"
            title="TEST AUDIO"
          >
            TEST
          </button>
        </div>

        {/* Volume Slider (Expandable) */}
        {showSlider && (
          <div className="mt-3 pt-3 border-t border-mainframe-green">
            <div className="flex items-center gap-2">
              <span className="text-xs">0</span>
              <input
                type="range"
                min="0"
                max="1"
                step="0.1"
                value={volume}
                onChange={handleVolumeChange}
                disabled={muted}
                className="flex-1 h-1 bg-mainframe-green appearance-none cursor-pointer"
                style={{
                  accentColor: '#00ff00'
                }}
              />
              <span className="text-xs">10</span>
            </div>
            <div className="text-xs text-center mt-1">
              VOLUME: {Math.round(volume * 10)}
            </div>
          </div>
        )}

        {/* Status Indicator */}
        <div className="text-xs text-center mt-2 opacity-70">
          {muted ? 'AUDIO: MUTED' : `AUDIO: ${Math.round(volume * 100)}%`}
        </div>
      </div>
    </div>
  );
};
