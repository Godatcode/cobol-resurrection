/**
 * AUDIO MANAGER SERVICE
 * Manages authentic vintage computing sound effects
 * IBM 7090 Mainframe Era (1959)
 */

export type SoundEffect = 
  | 'teletype'
  | 'tapedrive'
  | 'cardreader'
  | 'bell'
  | 'buzzer';

interface AudioSettings {
  volume: number;
  muted: boolean;
}

class AudioManager {
  private sounds: Map<SoundEffect, HTMLAudioElement>;
  private settings: AudioSettings;
  private initialized: boolean = false;

  constructor() {
    this.sounds = new Map();
    this.settings = {
      volume: 0.5,
      muted: false
    };
    
    // Load settings from localStorage
    this.loadSettings();
  }

  /**
   * Initialize audio system and preload sound files
   */
  async initialize(): Promise<void> {
    if (this.initialized) return;

    const soundFiles: Record<SoundEffect, string> = {
      teletype: '/sounds/teletype.mp3',
      tapedrive: '/sounds/tapedrive.mp3',
      cardreader: '/sounds/cardreader.mp3',
      bell: '/sounds/bell.mp3',
      buzzer: '/sounds/buzzer.mp3'
    };

    // Preload all sound files
    for (const [effect, path] of Object.entries(soundFiles)) {
      try {
        const audio = new Audio(path);
        audio.volume = this.settings.volume;
        audio.preload = 'auto';
        
        // Handle load errors gracefully
        audio.addEventListener('error', () => {
          console.warn(`Sound file not found: ${effect} - Install authentic recordings for full experience`);
        });
        
        this.sounds.set(effect as SoundEffect, audio);
      } catch (error) {
        console.warn(`Failed to load sound: ${effect}`, error);
      }
    }

    this.initialized = true;
  }

  /**
   * Play a sound effect
   */
  play(effect: SoundEffect): void {
    if (this.settings.muted) return;

    const audio = this.sounds.get(effect);
    if (!audio) {
      console.warn(`Sound effect not found: ${effect}`);
      return;
    }

    // Clone the audio to allow overlapping plays
    const clone = audio.cloneNode() as HTMLAudioElement;
    clone.volume = this.settings.volume;
    
    clone.play().catch(error => {
      console.warn(`Failed to play sound: ${effect}`, error);
    });
  }

  /**
   * Play teletype sound (for terminal text output)
   */
  playTeletype(): void {
    this.play('teletype');
  }

  /**
   * Play tape drive sound (during calculations)
   */
  playTapeDrive(): void {
    this.play('tapedrive');
  }

  /**
   * Play card reader sound (on form submission)
   */
  playCardReader(): void {
    this.play('cardreader');
  }

  /**
   * Play bell sound (on successful completion)
   */
  playBell(): void {
    this.play('bell');
  }

  /**
   * Play buzzer sound (on error)
   */
  playBuzzer(): void {
    this.play('buzzer');
  }

  /**
   * Set volume (0.0 to 1.0)
   */
  setVolume(volume: number): void {
    this.settings.volume = Math.max(0, Math.min(1, volume));
    
    // Update all loaded sounds
    this.sounds.forEach(audio => {
      audio.volume = this.settings.volume;
    });

    this.saveSettings();
  }

  /**
   * Get current volume
   */
  getVolume(): number {
    return this.settings.volume;
  }

  /**
   * Toggle mute
   */
  toggleMute(): void {
    this.settings.muted = !this.settings.muted;
    this.saveSettings();
  }

  /**
   * Set mute state
   */
  setMuted(muted: boolean): void {
    this.settings.muted = muted;
    this.saveSettings();
  }

  /**
   * Check if muted
   */
  isMuted(): boolean {
    return this.settings.muted;
  }

  /**
   * Save settings to localStorage
   */
  private saveSettings(): void {
    try {
      localStorage.setItem('necro-bank-audio', JSON.stringify(this.settings));
    } catch (error) {
      console.warn('Failed to save audio settings', error);
    }
  }

  /**
   * Load settings from localStorage
   */
  private loadSettings(): void {
    try {
      const saved = localStorage.getItem('necro-bank-audio');
      if (saved) {
        const parsed = JSON.parse(saved);
        this.settings = {
          volume: parsed.volume ?? 0.5,
          muted: parsed.muted ?? false
        };
      }
    } catch (error) {
      console.warn('Failed to load audio settings', error);
    }
  }
}

// Export singleton instance
export const audioManager = new AudioManager();
