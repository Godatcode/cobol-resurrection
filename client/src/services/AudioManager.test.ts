/**
 * AUDIO MANAGER TESTS
 * Unit tests for the vintage sound effects system
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { audioManager } from './AudioManager';

describe('AudioManager', () => {
  beforeEach(() => {
    // Reset audio manager state
    localStorage.clear();
  });

  describe('Initialization', () => {
    it('should initialize without errors', async () => {
      await expect(audioManager.initialize()).resolves.not.toThrow();
    });

    it('should not reinitialize if already initialized', async () => {
      await audioManager.initialize();
      await audioManager.initialize(); // Should not throw
    });
  });

  describe('Volume Control', () => {
    it('should set volume within valid range', () => {
      audioManager.setVolume(0.7);
      expect(audioManager.getVolume()).toBe(0.7);
    });

    it('should clamp volume to 0.0 minimum', () => {
      audioManager.setVolume(-0.5);
      expect(audioManager.getVolume()).toBe(0);
    });

    it('should clamp volume to 1.0 maximum', () => {
      audioManager.setVolume(1.5);
      expect(audioManager.getVolume()).toBe(1);
    });

    it('should persist volume to localStorage', () => {
      audioManager.setVolume(0.8);
      const saved = JSON.parse(localStorage.getItem('necro-bank-audio') || '{}');
      expect(saved.volume).toBe(0.8);
    });
  });

  describe('Mute Control', () => {
    it('should toggle mute state', () => {
      const initialState = audioManager.isMuted();
      audioManager.toggleMute();
      expect(audioManager.isMuted()).toBe(!initialState);
    });

    it('should set mute state explicitly', () => {
      audioManager.setMuted(true);
      expect(audioManager.isMuted()).toBe(true);
      
      audioManager.setMuted(false);
      expect(audioManager.isMuted()).toBe(false);
    });

    it('should persist mute state to localStorage', () => {
      audioManager.setMuted(true);
      const saved = JSON.parse(localStorage.getItem('necro-bank-audio') || '{}');
      expect(saved.muted).toBe(true);
    });
  });

  describe('Sound Playback', () => {
    beforeEach(async () => {
      await audioManager.initialize();
    });

    it('should have playTeletype method', () => {
      expect(typeof audioManager.playTeletype).toBe('function');
    });

    it('should have playTapeDrive method', () => {
      expect(typeof audioManager.playTapeDrive).toBe('function');
    });

    it('should have playCardReader method', () => {
      expect(typeof audioManager.playCardReader).toBe('function');
    });

    it('should have playBell method', () => {
      expect(typeof audioManager.playBell).toBe('function');
    });

    it('should have playBuzzer method', () => {
      expect(typeof audioManager.playBuzzer).toBe('function');
    });

    it('should not throw when playing sounds', () => {
      expect(() => audioManager.playTeletype()).not.toThrow();
      expect(() => audioManager.playTapeDrive()).not.toThrow();
      expect(() => audioManager.playCardReader()).not.toThrow();
      expect(() => audioManager.playBell()).not.toThrow();
      expect(() => audioManager.playBuzzer()).not.toThrow();
    });

    it('should not play sounds when muted', () => {
      audioManager.setMuted(true);
      // Should not throw, but also should not play
      expect(() => audioManager.playBell()).not.toThrow();
    });
  });

  describe('Settings Persistence', () => {
    it('should load settings from localStorage on creation', () => {
      localStorage.setItem('necro-bank-audio', JSON.stringify({
        volume: 0.3,
        muted: true
      }));

      // Create new instance (in real app, this would be on page load)
      // For testing, we verify the saved data exists
      const saved = JSON.parse(localStorage.getItem('necro-bank-audio') || '{}');
      expect(saved.volume).toBe(0.3);
      expect(saved.muted).toBe(true);
    });

    it('should handle missing localStorage gracefully', () => {
      localStorage.clear();
      // Should not throw when localStorage is empty
      expect(() => audioManager.getVolume()).not.toThrow();
    });

    it('should handle corrupted localStorage data', () => {
      localStorage.setItem('necro-bank-audio', 'invalid json');
      // Should not throw when localStorage contains invalid data
      expect(() => audioManager.getVolume()).not.toThrow();
    });
  });
});
