#!/usr/bin/env node
/**
 * CODE GENERATOR DEMONSTRATION
 * Shows all features of the code generation service
 */

const codeGenerator = require('./services/code-generator');
const fs = require('fs');
const path = require('path');

console.log('═══════════════════════════════════════════════════════');
console.log('👻 NECRO-BRIDGE CODE GENERATOR DEMONSTRATION');
console.log('═══════════════════════════════════════════════════════\n');

// DEMONSTRATION 1: Few-Shot Templates
console.log('📜 DEMONSTRATION 1: Few-Shot Prompting Templates');
console.log('─────────────────────────────────────────────────────');
console.log('Loading templates with vintage code examples...\n');

const languages = ['COBOL', 'FORTRAN', 'PASCAL', 'BASIC'];
languages.forEach(lang => {
  const template = codeGenerator.loadTemplate(lang);
  const lines = template.split('\n').length;
  const hasExample = template.includes('Example:');
  
  console.log(`${lang}:`);
  console.log(`  ✓ Template loaded: ${lines} lines`);
  console.log(`  ✓ Contains examples: ${hasExample ? 'YES' : 'NO'}`);
  console.log(`  ✓ First 100 chars: ${template.substring(0, 100)}...`);
  console.log();
});

// DEMONSTRATION 2: Syntax Validation
console.log('\n🔍 DEMONSTRATION 2: Syntax Validation');
console.log('─────────────────────────────────────────────────────');

const validCobol = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "RESULT: 100.00"
           STOP RUN.`;

const invalidCobol = `PROGRAM WITHOUT PROPER STRUCTURE`;

console.log('Testing valid COBOL code...');
const valid = codeGenerator.validateSyntax(validCobol, 'COBOL');
console.log(`  Result: ${valid.valid ? '✓ VALID' : '✗ INVALID'}`);

console.log('\nTesting invalid COBOL code...');
const invalid = codeGenerator.validateSyntax(invalidCobol, 'COBOL');
console.log(`  Result: ${invalid.valid ? '✗ VALID' : '✓ INVALID (as expected)'}`);
console.log(`  Errors: ${invalid.errors.join(', ')}`);

// DEMONSTRATION 3: Complete Workflow
console.log('\n\n⚡ DEMONSTRATION 3: Complete Workflow');
console.log('─────────────────────────────────────────────────────');
console.log('Generating, saving, and compiling COBOL code...\n');

const demoCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO-CALC.
       AUTHOR. NECRO-BRIDGE GENERATOR.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER1 PIC 9(4) VALUE 1234.
       01 WS-NUMBER2 PIC 9(4) VALUE 5678.
       01 WS-RESULT PIC 9(8).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           COMPUTE WS-RESULT = WS-NUMBER1 + WS-NUMBER2
           DISPLAY "RESULT: " WS-RESULT
           STOP RUN.`;

(async () => {
  try {
    console.log('Step 1: Validating syntax...');
    const validation = codeGenerator.validateSyntax(demoCode, 'COBOL');
    console.log(`  ${validation.valid ? '✓' : '✗'} Validation: ${validation.valid ? 'PASSED' : 'FAILED'}`);
    
    if (!validation.valid) {
      console.log('  Errors:', validation.errors);
      return;
    }
    
    console.log('\nStep 2: Saving to legacy directory...');
    const result = await codeGenerator.summonAncientSpirit(
      demoCode,
      'COBOL',
      'demo-calc'
    );
    
    console.log(`  ${result.success ? '✓' : '✗'} Save: ${result.success ? 'SUCCESS' : 'FAILED'}`);
    if (result.saved) {
      console.log(`  File: ${result.saved.filePath}`);
    }
    
    console.log('\nStep 3: Compiling code...');
    console.log(`  ${result.compiled.success ? '✓' : '✗'} Compilation: ${result.compiled.success ? 'SUCCESS' : 'FAILED'}`);
    console.log(`  Compiler: ${result.compiled.compiler}`);
    console.log(`  Message: ${result.compiled.message}`);
    
    if (result.success) {
      console.log('\n✨ ANCIENT SPIRIT SUCCESSFULLY SUMMONED!');
      
      // Test execution
      console.log('\nStep 4: Testing execution...');
      const { exec } = require('child_process');
      const binaryPath = path.join(process.cwd(), 'legacy/cobol/demo-calc');
      
      exec(binaryPath, (error, stdout, stderr) => {
        if (error) {
          console.log(`  ✗ Execution failed: ${error.message}`);
        } else {
          console.log(`  ✓ Execution successful!`);
          console.log(`  Output: ${stdout.trim()}`);
        }
        
        // Cleanup
        console.log('\nCleaning up demo files...');
        try {
          if (fs.existsSync(result.saved.filePath)) {
            fs.unlinkSync(result.saved.filePath);
            console.log('  ✓ Source file removed');
          }
          if (fs.existsSync(binaryPath)) {
            fs.unlinkSync(binaryPath);
            console.log('  ✓ Binary removed');
          }
        } catch (cleanupError) {
          console.log('  ⚠ Cleanup warning:', cleanupError.message);
        }
        
        console.log('\n═══════════════════════════════════════════════════════');
        console.log('✅ DEMONSTRATION COMPLETE');
        console.log('═══════════════════════════════════════════════════════');
        console.log('\nThe Code Generator Service is fully operational with:');
        console.log('  ✓ Few-shot prompting templates');
        console.log('  ✓ Syntax validation');
        console.log('  ✓ Automatic file saving');
        console.log('  ✓ Automatic compilation');
        console.log('  ✓ Multi-language support (COBOL, FORTRAN, PASCAL, BASIC)');
        console.log('\n[END OF TAPE]');
      });
    }
  } catch (error) {
    console.log(`\n✗ ERROR: ${error.message}`);
    console.log('\n[END OF TAPE]');
  }
})();
