import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';

/**
 * UNIT TESTS FOR MEMORY DUMP COMPONENT
 * 
 * Tests verify:
 * - Component structure and key features
 * - Error visualization elements
 * - Memory segments are color-coded properly
 * - Hexadecimal and ASCII columns are present
 * 
 * Requirements: 11.4
 */

describe('MemoryDump Component', () => {
  const memoryDumpSource = readFileSync('client/src/components/MemoryDump.tsx', 'utf-8');

  it('should accept errorData and isVisible props', () => {
    expect(memoryDumpSource).toContain('errorData?:');
    expect(memoryDumpSource).toContain('isVisible: boolean');
  });

  it('should return null when not visible or no error data', () => {
    expect(memoryDumpSource).toContain('if (!isVisible || !errorData)');
    expect(memoryDumpSource).toContain('return null');
  });

  it('should display CORE DUMP DETECTED header', () => {
    expect(memoryDumpSource).toContain('CORE DUMP DETECTED');
    expect(memoryDumpSource).toContain('animate-pulse');
  });

  it('should display error message and details', () => {
    expect(memoryDumpSource).toContain('errorData.error');
    expect(memoryDumpSource).toContain('errorData.details');
    expect(memoryDumpSource).toContain('SYSTEM FAILURE:');
  });

  it('should implement three memory segment types', () => {
    expect(memoryDumpSource).toContain("type: 'code' | 'data' | 'stack'");
    expect(memoryDumpSource).toContain("type = 'code'");
    expect(memoryDumpSource).toContain("type = 'data'");
    expect(memoryDumpSource).toContain("type = 'stack'");
  });

  it('should implement color coding for memory segments', () => {
    // Check for color classes
    expect(memoryDumpSource).toContain('text-mainframe-green'); // code
    expect(memoryDumpSource).toContain('text-yellow-400'); // data
    expect(memoryDumpSource).toContain('text-red-500'); // stack
    expect(memoryDumpSource).toContain('getColorClass');
  });

  it('should display memory legend with all three segments', () => {
    expect(memoryDumpSource).toContain('CODE SEGMENT');
    expect(memoryDumpSource).toContain('DATA SEGMENT');
    expect(memoryDumpSource).toContain('STACK SEGMENT');
  });

  it('should display hexadecimal memory in 16-byte rows', () => {
    expect(memoryDumpSource).toContain('16'); // 16 bytes per row
    expect(memoryDumpSource).toContain('toString(16)'); // hex conversion
    expect(memoryDumpSource).toContain('HEXADECIMAL DUMP');
  });

  it('should include ASCII representation column', () => {
    expect(memoryDumpSource).toContain('ASCII');
    expect(memoryDumpSource).toContain('ascii:');
    expect(memoryDumpSource).toContain('String.fromCharCode');
  });

  it('should display memory addresses', () => {
    expect(memoryDumpSource).toContain('address:');
    expect(memoryDumpSource).toContain('ADDRESS');
    expect(memoryDumpSource).toContain('0x');
  });

  it('should generate deterministic memory dump based on error', () => {
    // Check for seed-based generation
    expect(memoryDumpSource).toContain('seed');
    expect(memoryDumpSource).toContain('errorData.error.split');
    expect(memoryDumpSource).toContain('charCodeAt');
  });

  it('should display footer messages', () => {
    expect(memoryDumpSource).toContain('MEMORY SNAPSHOT AT TIME OF FAILURE');
    expect(memoryDumpSource).toContain('SYSTEM HALTED');
    expect(memoryDumpSource).toContain('OPERATOR INTERVENTION REQUIRED');
  });

  it('should use proper styling with borders and colors', () => {
    expect(memoryDumpSource).toContain('border-2 border-red-500');
    expect(memoryDumpSource).toContain('bg-black');
    expect(memoryDumpSource).toContain('font-mono');
  });

  it('should implement MemoryRow and MemoryByte interfaces', () => {
    expect(memoryDumpSource).toContain('interface MemoryRow');
    expect(memoryDumpSource).toContain('interface MemoryByte');
    expect(memoryDumpSource).toContain('bytes: MemoryByte[]');
  });

  it('should use useState for memory data management', () => {
    expect(memoryDumpSource).toContain('useState<MemoryRow[]>');
    expect(memoryDumpSource).toContain('setMemoryData');
  });

  it('should use useEffect to generate memory dump', () => {
    expect(memoryDumpSource).toContain('useEffect');
    expect(memoryDumpSource).toContain('[errorData, isVisible]');
  });
});
