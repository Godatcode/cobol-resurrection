/**
 * SIMPLIFIED AGENT HOOK INTEGRATION TEST
 * Tests core workflow: generate → save → compile → execute
 * Validates Requirements 10.4 and 10.5
 * 
 * @vitest-environment node
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import fs from 'fs';
import path from 'path';
import { exec } from 'child_process';
import { promisify } from 'util';
import { createRequire } from 'module';

const require = createRequire(import.meta.url);
const {
  saveGeneratedCode,
  compileCode,
  summonAncientSpirit,
  LANGUAGE_CONFIG
} = require('../server/services/code-generator.js');

const execAsync = promisify(exec);

describe('Agent Hook Integration - Core Workflow', () => {
  
  // Simple test programs that don't require command-line arguments
  const simplePrograms = {
    COBOL: {
      filename: 'hook-test-cobol',
      code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOOK-TEST.
       PROCEDURE DIVISION.
           DISPLAY "RESULT: 42.00".
           STOP RUN.
`,
      expectedOutput: 'RESULT: 42.00'
    },
    
    FORTRAN: {
      filename: 'hook-test-fortran',
      code: `      PROGRAM HOOKTEST
      WRITE(*, '(A)') 'RESULT: 99.00'
      END PROGRAM HOOKTEST
`,
      expectedOutput: 'RESULT: 99.00'
    }
  };
  
  // Cleanup function
  const cleanupTestFiles = () => {
    Object.keys(simplePrograms).forEach(lang => {
      const config = LANGUAGE_CONFIG[lang];
      const program = simplePrograms[lang];
      
      if (config && program) {
        const sourceFile = path.join(config.directory, `${program.filename}${config.extension}`);
        const binaryFile = path.join(config.directory, program.filename);
        
        if (fs.existsSync(sourceFile)) {
          fs.unlinkSync(sourceFile);
        }
        
        if (fs.existsSync(binaryFile)) {
          fs.unlinkSync(binaryFile);
        }
      }
    });
  };
  
  beforeAll(() => {
    cleanupTestFiles();
  });
  
  afterAll(() => {
    cleanupTestFiles();
  });
  
  describe('Task 13.1: Hook configuration exists', () => {
    it('should have agent hook configuration file', () => {
      const hookPath = '.kiro/hooks/compile.yaml';
      expect(fs.existsSync(hookPath)).toBe(true);
    });
    
    it('should watch all legacy file extensions', () => {
      const hookContent = fs.readFileSync('.kiro/hooks/compile.yaml', 'utf8');
      expect(hookContent).toContain('.cbl');
      expect(hookContent).toContain('.f');
      expect(hookContent).toContain('.pas');
      expect(hookContent).toContain('.bas');
    });
  });
  
  describe('Task 13.2: Language-specific compilation commands', () => {
    it('should have compilation commands for all languages', () => {
      const hookContent = fs.readFileSync('.kiro/hooks/compile.yaml', 'utf8');
      expect(hookContent).toContain('cobc');
      expect(hookContent).toContain('gfortran');
      expect(hookContent).toContain('fpc');
      expect(hookContent).toContain('node');
    });
  });
  
  describe('Task 13.3: Success/failure notifications', () => {
    it('should have success notification messages', () => {
      const hookContent = fs.readFileSync('.kiro/hooks/compile.yaml', 'utf8');
      expect(hookContent).toContain('COMPILATION SUCCESSFUL');
      expect(hookContent).toContain('Ancient Spirit');
    });
    
    it('should have failure notification messages', () => {
      const hookContent = fs.readFileSync('.kiro/hooks/compile.yaml', 'utf8');
      expect(hookContent).toContain('COMPILATION FAILED');
      expect(hookContent).toContain('MOTH DETECTED');
    });
  });
  
  describe('Task 13.4: End-to-end workflow - COBOL', () => {
    it('should complete full workflow: generate → save → compile → execute', async () => {
      const { code, filename, expectedOutput } = simplePrograms.COBOL;
      
      // STEP 1: SAVE (simulates code generation)
      const saveResult = await saveGeneratedCode(code, 'COBOL', filename);
      expect(saveResult.success).toBe(true);
      expect(fs.existsSync(saveResult.filePath)).toBe(true);
      
      // STEP 2: COMPILE (simulates agent hook trigger)
      const compileResult = await compileCode('COBOL', filename);
      expect(compileResult.success).toBe(true);
      expect(compileResult.message).toContain('Ancient Spirit');
      
      // STEP 3: VERIFY BINARY EXISTS
      const binaryPath = path.join('legacy/cobol', filename);
      expect(fs.existsSync(binaryPath)).toBe(true);
      
      // STEP 4: EXECUTE
      const { stdout } = await execAsync(`./${binaryPath}`);
      expect(stdout.trim()).toContain(expectedOutput);
    }, 30000);
  });
  
  describe('Task 13.4: End-to-end workflow - FORTRAN', () => {
    it('should complete full workflow: generate → save → compile → execute', async () => {
      const { code, filename, expectedOutput } = simplePrograms.FORTRAN;
      
      // STEP 1: SAVE
      const saveResult = await saveGeneratedCode(code, 'FORTRAN', filename);
      expect(saveResult.success).toBe(true);
      expect(fs.existsSync(saveResult.filePath)).toBe(true);
      
      // STEP 2: COMPILE
      const compileResult = await compileCode('FORTRAN', filename);
      expect(compileResult.success).toBe(true);
      
      // STEP 3: VERIFY BINARY EXISTS
      const binaryPath = path.join('legacy/fortran', filename);
      expect(fs.existsSync(binaryPath)).toBe(true);
      
      // STEP 4: EXECUTE
      const { stdout } = await execAsync(`./${binaryPath}`);
      expect(stdout.trim()).toContain(expectedOutput);
    }, 30000);
  });
  
  describe('Task 13.4: Complete workflow with summonAncientSpirit', () => {
    it('should execute complete workflow using summonAncientSpirit function', async () => {
      const testCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUMMON-TEST.
       PROCEDURE DIVISION.
           DISPLAY "RESULT: 777.77".
           STOP RUN.
`;
      
      const result = await summonAncientSpirit(testCode, 'COBOL', 'summon-test');
      
      expect(result.success).toBe(true);
      expect(result.saved.success).toBe(true);
      expect(result.compiled.success).toBe(true);
      expect(result.message).toContain('Ancient Spirit Summoned');
      
      // Verify binary exists and executes
      const binaryPath = path.join('legacy/cobol', 'summon-test');
      expect(fs.existsSync(binaryPath)).toBe(true);
      
      const { stdout } = await execAsync(`./${binaryPath}`);
      expect(stdout.trim()).toContain('RESULT: 777.77');
      
      // Cleanup
      fs.unlinkSync(path.join('legacy/cobol', 'summon-test.cbl'));
      fs.unlinkSync(binaryPath);
    }, 30000);
  });
  
  describe('Requirements 10.4 and 10.5 validation', () => {
    it('should satisfy Requirement 10.4: Agent Hook compiles saved files', async () => {
      const testCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. REQ-10-4.
       PROCEDURE DIVISION.
           DISPLAY "RESULT: 104.00".
           STOP RUN.
`;
      
      // Save file (simulates code generation)
      const saveResult = await saveGeneratedCode(testCode, 'COBOL', 'req-10-4-test');
      expect(saveResult.success).toBe(true);
      
      // Compile (simulates agent hook automatic compilation)
      const compileResult = await compileCode('COBOL', 'req-10-4-test');
      expect(compileResult.success).toBe(true);
      expect(compileResult.message).toContain('Ancient Spirit Rebound');
      
      // Cleanup
      fs.unlinkSync(path.join('legacy/cobol', 'req-10-4-test.cbl'));
      fs.unlinkSync(path.join('legacy/cobol', 'req-10-4-test'));
    }, 30000);
    
    it('should satisfy Requirement 10.5: Compiled binary is immediately executable', async () => {
      const testCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. REQ-10-5.
       PROCEDURE DIVISION.
           DISPLAY "RESULT: 105.00".
           STOP RUN.
`;
      
      // Complete workflow
      const result = await summonAncientSpirit(testCode, 'COBOL', 'req-10-5-test');
      expect(result.success).toBe(true);
      
      // Immediately execute (validates binary is available)
      const binaryPath = path.join('legacy/cobol', 'req-10-5-test');
      const { stdout } = await execAsync(`./${binaryPath}`);
      expect(stdout.trim()).toContain('RESULT: 105.00');
      
      // Cleanup
      fs.unlinkSync(path.join('legacy/cobol', 'req-10-5-test.cbl'));
      fs.unlinkSync(binaryPath);
    }, 30000);
  });
});
