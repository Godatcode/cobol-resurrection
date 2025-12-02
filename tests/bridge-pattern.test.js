/**
 * ═══════════════════════════════════════════════════════════
 * BRIDGE PATTERN UNIT TESTS
 * ═══════════════════════════════════════════════════════════
 * 
 * TESTS FOR UNIVERSAL LEGACY BRIDGE PATTERN IMPLEMENTATION
 * VALIDATES ABSTRACT CLASS, SUBCLASSES, AND FACTORY
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createRequire } from 'module';

const require = createRequire(import.meta.url);
const LegacyBridge = require('../server/bridges/LegacyBridge');
const CobolBridge = require('../server/bridges/CobolBridge');
const FortranBridge = require('../server/bridges/FortranBridge');
const PascalBridge = require('../server/bridges/PascalBridge');
const BasicBridge = require('../server/bridges/BasicBridge');
const bridgeFactory = require('../server/bridges/BridgeFactory');

describe('LegacyBridge Abstract Class', () => {
  it('should throw error when instantiated directly', () => {
    expect(() => {
      new LegacyBridge({
        name: 'TEST',
        year: 2024,
        binary: 'test',
        description: 'TEST',
        params: ['param1']
      });
    }).toThrow('CANNOT INSTANTIATE ABSTRACT CLASS LegacyBridge DIRECTLY');
  });
  
  it('should require subclasses to implement parseOutput()', async () => {
    // CREATE A MINIMAL SUBCLASS WITHOUT parseOutput()
    class TestBridge extends LegacyBridge {
      constructor() {
        super({
          name: 'TEST',
          year: 2024,
          binary: 'test',
          description: 'TEST',
          params: ['param1']
        });
      }
    }
    
    const bridge = new TestBridge();
    expect(() => bridge.parseOutput('test')).toThrow('SUBCLASSES MUST IMPLEMENT parseOutput() METHOD');
  });
});

describe('CobolBridge', () => {
  let bridge;
  
  beforeEach(() => {
    bridge = new CobolBridge();
  });
  
  it('should have correct configuration', () => {
    expect(bridge.name).toBe('COBOL');
    expect(bridge.year).toBe(1959);
    expect(bridge.binary).toBe('mortgage');
    expect(bridge.params).toEqual(['principal', 'rate', 'term']);
  });
  
  it('should validate parameters correctly', () => {
    const validParams = { principal: 200000, rate: 5.5, term: 30 };
    const result = bridge.validateParams(validParams);
    expect(result.valid).toBe(true);
    expect(result.error).toBe(null);
  });
  
  it('should reject missing parameters', () => {
    const invalidParams = { principal: 200000, rate: 5.5 };
    const result = bridge.validateParams(invalidParams);
    expect(result.valid).toBe(false);
    expect(result.error).toContain('MISSING PARAMETERS');
  });
  
  it('should reject non-numeric parameters', () => {
    const invalidParams = { principal: '200000', rate: 5.5, term: 30 };
    const result = bridge.validateParams(invalidParams);
    expect(result.valid).toBe(false);
    expect(result.error).toContain('MUST BE NUMERIC');
  });
  
  it('should reject negative parameters', () => {
    const invalidParams = { principal: -200000, rate: 5.5, term: 30 };
    const result = bridge.validateParams(invalidParams);
    expect(result.valid).toBe(false);
    expect(result.error).toContain('MUST BE POSITIVE');
  });
  
  it('should parse COBOL output correctly', () => {
    const stdout = 'RESULT: 1135.58\n';
    const result = bridge.parseOutput(stdout);
    expect(result.success).toBe(true);
    expect(result.result).toBe(1135.58);
    expect(result.error).toBe(null);
  });
  
  it('should handle invalid COBOL output format', () => {
    const stdout = 'INVALID OUTPUT\n';
    const result = bridge.parseOutput(stdout);
    expect(result.success).toBe(false);
    expect(result.error).toContain('DOES NOT MATCH EXPECTED FORMAT');
  });
  
  it('should build command correctly', () => {
    const params = { principal: 200000, rate: 5.5, term: 30 };
    const command = bridge.buildCommand(params);
    expect(command).toContain('mortgage');
    expect(command).toContain('200000 5.5 30');
  });
});

describe('FortranBridge', () => {
  let bridge;
  
  beforeEach(() => {
    bridge = new FortranBridge();
  });
  
  it('should have correct configuration', () => {
    expect(bridge.name).toBe('FORTRAN');
    expect(bridge.year).toBe(1957);
    expect(bridge.binary).toBe('trajectory');
    expect(bridge.params).toEqual(['velocity', 'angle']);
  });
  
  it('should parse FORTRAN output with variable decimal places', () => {
    const stdout1 = 'RESULT: 1234.56\n';
    const result1 = bridge.parseOutput(stdout1);
    expect(result1.success).toBe(true);
    expect(result1.result).toBe(1234.56);
    
    const stdout2 = 'RESULT: 1234.5678\n';
    const result2 = bridge.parseOutput(stdout2);
    expect(result2.success).toBe(true);
    expect(result2.result).toBe(1234.5678);
  });
  
  it('should reject invalid output format', () => {
    const stdout = 'INVALID OUTPUT\n';
    const result = bridge.parseOutput(stdout);
    expect(result.success).toBe(false);
    expect(result.error).toContain('DOES NOT MATCH EXPECTED FORMAT');
  });
});

describe('PascalBridge', () => {
  let bridge;
  
  beforeEach(() => {
    bridge = new PascalBridge();
  });
  
  it('should have correct configuration', () => {
    expect(bridge.name).toBe('PASCAL');
    expect(bridge.year).toBe(1970);
    expect(bridge.binary).toBe('tax');
    expect(bridge.params).toEqual(['income']);
  });
  
  it('should parse PASCAL output correctly', () => {
    const stdout = 'RESULT: 5432.10\n';
    const result = bridge.parseOutput(stdout);
    expect(result.success).toBe(true);
    expect(result.result).toBe(5432.10);
  });
});

describe('BasicBridge', () => {
  let bridge;
  
  beforeEach(() => {
    bridge = new BasicBridge();
  });
  
  it('should have correct configuration', () => {
    expect(bridge.name).toBe('BASIC');
    expect(bridge.year).toBe(1983);
    expect(bridge.binary).toBe('interest');
    expect(bridge.params).toEqual(['principal', 'rate', 'time', 'compounds']);
  });
  
  it('should parse BASIC output correctly', () => {
    const stdout = 'RESULT: 10500.00\n';
    const result = bridge.parseOutput(stdout);
    expect(result.success).toBe(true);
    expect(result.result).toBe(10500.00);
  });
});

describe('BridgeFactory', () => {
  it('should return list of supported languages', () => {
    const languages = bridgeFactory.getSupportedLanguages();
    expect(languages).toContain('cobol');
    expect(languages).toContain('fortran');
    expect(languages).toContain('pascal');
    expect(languages).toContain('basic');
    expect(languages.length).toBe(4);
  });
  
  it('should check if language is supported', () => {
    expect(bridgeFactory.isSupported('cobol')).toBe(true);
    expect(bridgeFactory.isSupported('COBOL')).toBe(true);
    expect(bridgeFactory.isSupported('fortran')).toBe(true);
    expect(bridgeFactory.isSupported('rust')).toBe(false);
  });
  
  it('should return correct bridge instance for each language', () => {
    const cobolBridge = bridgeFactory.getBridge('cobol');
    expect(cobolBridge).toBeInstanceOf(CobolBridge);
    
    const fortranBridge = bridgeFactory.getBridge('fortran');
    expect(fortranBridge).toBeInstanceOf(FortranBridge);
    
    const pascalBridge = bridgeFactory.getBridge('pascal');
    expect(pascalBridge).toBeInstanceOf(PascalBridge);
    
    const basicBridge = bridgeFactory.getBridge('basic');
    expect(basicBridge).toBeInstanceOf(BasicBridge);
  });
  
  it('should throw error for unsupported language', () => {
    expect(() => bridgeFactory.getBridge('rust')).toThrow('UNSUPPORTED LANGUAGE');
  });
  
  it('should return same instance on multiple calls (singleton)', () => {
    const bridge1 = bridgeFactory.getBridge('cobol');
    const bridge2 = bridgeFactory.getBridge('cobol');
    expect(bridge1).toBe(bridge2);
  });
  
  it('should return language metadata', () => {
    const metadata = bridgeFactory.getLanguageMetadata();
    expect(metadata.length).toBe(4);
    
    const cobolMeta = metadata.find(m => m.id === 'cobol');
    expect(cobolMeta.name).toBe('COBOL');
    expect(cobolMeta.year).toBe(1959);
    expect(cobolMeta.description).toBe('MORTGAGE PAYMENT CALCULATOR');
    expect(cobolMeta.parameters).toEqual(['principal', 'rate', 'term']);
  });
});
