// EMERGENCY PLACEHOLDER GENERATOR
// Creates minimal valid MP3 files without ffmpeg
// These are SILENT placeholders - replace with authentic recordings!

const fs = require('fs');

// Minimal valid MP3 file header (silent, 0.1 seconds)
// This is a valid MP3 frame with no audio data
const minimalMP3 = Buffer.from([
  0xFF, 0xFB, 0x90, 0x00, // MP3 sync word and header
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  // Repeat for minimal duration
]);

const files = [
  'teletype.mp3',
  'tapedrive.mp3', 
  'cardreader.mp3',
  'bell.mp3',
  'buzzer.mp3'
];

console.log('GENERATING PLACEHOLDER SOUND FILES...');
console.log('These are SILENT placeholders - replace with authentic recordings!');
console.log('');

files.forEach(file => {
  fs.writeFileSync(file, minimalMP3);
  console.log(`✓ Created ${file}`);
});

console.log('');
console.log('IMPORTANT: These are SILENT placeholders!');
console.log('Replace with authentic vintage recordings for production.');
console.log('See README.md for sourcing instructions.');
console.log('');
console.log('[END OF TAPE]');
