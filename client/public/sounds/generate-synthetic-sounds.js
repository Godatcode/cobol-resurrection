#!/usr/bin/env node

/**
 * SYNTHETIC VINTAGE SOUND GENERATOR
 * Creates approximations of 1960s mainframe sounds using Web Audio API
 * IBM 7090 Mainframe Era (1959)
 */

import { writeFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

console.log('═══════════════════════════════════════════════════════════');
console.log('  SYNTHETIC VINTAGE SOUND GENERATOR');
console.log('  IBM 7090 MAINFRAME ERA (1959)');
console.log('═══════════════════════════════════════════════════════════');
console.log('');

// Audio parameters
const SAMPLE_RATE = 44100;
const CHANNELS = 1; // Mono

/**
 * Generate WAV file header
 */
function generateWAVHeader(dataLength) {
  const buffer = Buffer.alloc(44);
  
  // RIFF header
  buffer.write('RIFF', 0);
  buffer.writeUInt32LE(36 + dataLength, 4);
  buffer.write('WAVE', 8);
  
  // fmt chunk
  buffer.write('fmt ', 12);
  buffer.writeUInt32LE(16, 16); // fmt chunk size
  buffer.writeUInt16LE(1, 20); // PCM format
  buffer.writeUInt16LE(CHANNELS, 22);
  buffer.writeUInt32LE(SAMPLE_RATE, 24);
  buffer.writeUInt32LE(SAMPLE_RATE * CHANNELS * 2, 28); // byte rate
  buffer.writeUInt16LE(CHANNELS * 2, 32); // block align
  buffer.writeUInt16LE(16, 34); // bits per sample
  
  // data chunk
  buffer.write('data', 36);
  buffer.writeUInt32LE(dataLength, 40);
  
  return buffer;
}

/**
 * Convert float samples to 16-bit PCM
 */
function floatTo16BitPCM(samples) {
  const buffer = Buffer.alloc(samples.length * 2);
  for (let i = 0; i < samples.length; i++) {
    const s = Math.max(-1, Math.min(1, samples[i]));
    buffer.writeInt16LE(s < 0 ? s * 0x8000 : s * 0x7FFF, i * 2);
  }
  return buffer;
}

/**
 * Generate TELETYPE sound (mechanical click)
 * Simulates ASR-33 Teletype key press
 */
function generateTeletype() {
  const duration = 0.08; // 80ms
  const samples = Math.floor(SAMPLE_RATE * duration);
  const audio = new Float32Array(samples);
  
  for (let i = 0; i < samples; i++) {
    const t = i / SAMPLE_RATE;
    
    // Sharp attack with noise burst
    const envelope = Math.exp(-t * 50);
    const noise = (Math.random() * 2 - 1) * 0.3;
    const click = Math.sin(2 * Math.PI * 2000 * t) * 0.2;
    
    audio[i] = (noise + click) * envelope;
  }
  
  return audio;
}

/**
 * Generate TAPE DRIVE sound (motor hum)
 * Simulates IBM 729 Tape Drive motor
 */
function generateTapeDrive() {
  const duration = 2.0; // 2 seconds
  const samples = Math.floor(SAMPLE_RATE * duration);
  const audio = new Float32Array(samples);
  
  for (let i = 0; i < samples; i++) {
    const t = i / SAMPLE_RATE;
    
    // Motor startup and running
    const envelope = Math.min(1, t * 5) * Math.min(1, (duration - t) * 5);
    const motor = Math.sin(2 * Math.PI * 120 * t) * 0.15; // 120 Hz hum
    const harmonics = Math.sin(2 * Math.PI * 240 * t) * 0.05; // 2nd harmonic
    const noise = (Math.random() * 2 - 1) * 0.02; // Background noise
    
    audio[i] = (motor + harmonics + noise) * envelope;
  }
  
  return audio;
}

/**
 * Generate CARD READER sound (mechanical feeding)
 * Simulates IBM 2540 Card Reader
 */
function generateCardReader() {
  const duration = 1.2; // 1.2 seconds
  const samples = Math.floor(SAMPLE_RATE * duration);
  const audio = new Float32Array(samples);
  
  for (let i = 0; i < samples; i++) {
    const t = i / SAMPLE_RATE;
    
    // Card feeding mechanism - rhythmic clicks
    const clickRate = 15; // 15 Hz
    const clickPhase = (t * clickRate) % 1;
    const click = clickPhase < 0.1 ? Math.exp(-clickPhase * 50) : 0;
    
    const envelope = Math.min(1, t * 10) * Math.min(1, (duration - t) * 3);
    const noise = (Math.random() * 2 - 1) * 0.2;
    const mechanical = Math.sin(2 * Math.PI * 800 * t) * 0.1;
    
    audio[i] = (click * 0.4 + noise * 0.3 + mechanical * 0.3) * envelope;
  }
  
  return audio;
}

/**
 * Generate BELL sound (console notification)
 * Simulates Teletype Bell
 */
function generateBell() {
  const duration = 0.5; // 500ms
  const samples = Math.floor(SAMPLE_RATE * duration);
  const audio = new Float32Array(samples);
  
  for (let i = 0; i < samples; i++) {
    const t = i / SAMPLE_RATE;
    
    // Bell tone with harmonics
    const envelope = Math.exp(-t * 5);
    const fundamental = Math.sin(2 * Math.PI * 800 * t) * 0.5;
    const harmonic2 = Math.sin(2 * Math.PI * 1600 * t) * 0.2;
    const harmonic3 = Math.sin(2 * Math.PI * 2400 * t) * 0.1;
    
    audio[i] = (fundamental + harmonic2 + harmonic3) * envelope;
  }
  
  return audio;
}

/**
 * Generate BUZZER sound (error alarm)
 * Simulates IBM Console Buzzer
 */
function generateBuzzer() {
  const duration = 0.6; // 600ms
  const samples = Math.floor(SAMPLE_RATE * duration);
  const audio = new Float32Array(samples);
  
  for (let i = 0; i < samples; i++) {
    const t = i / SAMPLE_RATE;
    
    // Harsh buzzer with square wave
    const envelope = Math.min(1, t * 20) * Math.min(1, (duration - t) * 5);
    const freq = 400; // 400 Hz buzzer
    const square = Math.sin(2 * Math.PI * freq * t) > 0 ? 1 : -1;
    const noise = (Math.random() * 2 - 1) * 0.1;
    
    audio[i] = (square * 0.3 + noise) * envelope;
  }
  
  return audio;
}

/**
 * Save audio as WAV file
 */
function saveWAV(filename, samples) {
  const pcmData = floatTo16BitPCM(samples);
  const header = generateWAVHeader(pcmData.length);
  const wavFile = Buffer.concat([header, pcmData]);
  
  const filepath = join(__dirname, filename);
  writeFileSync(filepath, wavFile);
  
  return filepath;
}

// Generate all sounds
console.log('GENERATING SYNTHETIC VINTAGE SOUNDS...');
console.log('');

try {
  console.log('⚙️  Synthesizing TELETYPE.WAV (mechanical click)...');
  saveWAV('teletype.wav', generateTeletype());
  console.log('   ✓ ASR-33 Teletype simulation complete');
  
  console.log('');
  console.log('⚙️  Synthesizing TAPEDRIVE.WAV (motor hum)...');
  saveWAV('tapedrive.wav', generateTapeDrive());
  console.log('   ✓ IBM 729 Tape Drive simulation complete');
  
  console.log('');
  console.log('⚙️  Synthesizing CARDREADER.WAV (mechanical feeding)...');
  saveWAV('cardreader.wav', generateCardReader());
  console.log('   ✓ IBM 2540 Card Reader simulation complete');
  
  console.log('');
  console.log('⚙️  Synthesizing BELL.WAV (console notification)...');
  saveWAV('bell.wav', generateBell());
  console.log('   ✓ Teletype Bell simulation complete');
  
  console.log('');
  console.log('⚙️  Synthesizing BUZZER.WAV (error alarm)...');
  saveWAV('buzzer.wav', generateBuzzer());
  console.log('   ✓ IBM Console Buzzer simulation complete');
  
  console.log('');
  console.log('═══════════════════════════════════════════════════════════');
  console.log('  ✓ ALL SOUNDS GENERATED SUCCESSFULLY');
  console.log('═══════════════════════════════════════════════════════════');
  console.log('');
  console.log('NEXT STEPS:');
  console.log('1. Convert WAV to MP3 (requires ffmpeg):');
  console.log('   brew install ffmpeg');
  console.log('   ./convert-to-mp3.sh');
  console.log('');
  console.log('2. OR rename WAV files to MP3 (browsers support WAV):');
  console.log('   mv teletype.wav teletype.mp3');
  console.log('   mv tapedrive.wav tapedrive.mp3');
  console.log('   mv cardreader.wav cardreader.mp3');
  console.log('   mv bell.wav bell.mp3');
  console.log('   mv buzzer.wav buzzer.mp3');
  console.log('');
  console.log('[END OF TAPE]');
  
} catch (error) {
  console.error('ERROR GENERATING SOUNDS:', error);
  process.exit(1);
}
