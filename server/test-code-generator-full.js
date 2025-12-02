/**
 * COMPREHENSIVE CODE GENERATOR TEST
 * Tests all functionality: templates, validation, saving, compilation
 */

const codeGenerator = require('./services/code-generator');
const fs = require('fs');
const path = require('path');

console.log('═══════════════════════════════════════════════════════');
console.log('🧪 CODE GENERATOR COMPREHENSIVE TEST');
console.log('═══════════════════════════════════════════════════════\n');

// TEST 1: Load Templates (Few-Shot Prompting)
console.log('TEST 1: Loading Templates with Few-Shot Examples');
console.log('─────────────────────────────────────────────────────');
const languages = ['COBOL', 'FORTRAN', 'PASCAL', 'BASIC'];
let templatesLoaded = 0;

languages.forEach(lang => {
  try {
    const template = codeGenerator.loadTemplate(lang);
    const hasExample = template.includes('Example:') || template.includes('EXAMPLE');
    const hasSyntax = template.includes('Syntax') || template.includes('SYNTAX');
    
    console.log(`✓ ${lang}: Template loaded (${template.length} chars)`);
    console.log(`  - Has syntax rules: ${hasSyntax ? '✓' : '✗'}`);
    console.log(`  - Has examples: ${hasExample ? '✓' : '✗'}`);
    templatesLoaded++;
  } catch (error) {
    console.log(`✗ ${lang}: Failed - ${error.message}`);
  }
});

console.log(`\nResult: ${templatesLoaded}/${languages.length} templates loaded\n`);

// TEST 2: Syntax Validation
console.log('TEST 2: Syntax Validation');
console.log('─────────────────────────────────────────────────────');

// Valid COBOL code
const validCobol = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "RESULT: 100.00"
           STOP RUN.`;

const invalidCobol = `SOME RANDOM TEXT WITHOUT PROPER STRUCTURE`;

const cobolValid = codeGenerator.validateSyntax(validCobol, 'COBOL');
const cobolInvalid = codeGenerator.validateSyntax(invalidCobol, 'COBOL');

console.log('Valid COBOL:', cobolValid.valid ? '✓ PASS' : '✗ FAIL');
console.log('Invalid COBOL:', !cobolInvalid.valid ? '✓ PASS (correctly rejected)' : '✗ FAIL');
if (!cobolInvalid.valid) {
  console.log('  Errors detected:', cobolInvalid.errors.join(', '));
}

// Valid FORTRAN code
const validFortran = `      PROGRAM TEST
      WRITE(*,*) 'RESULT: 100.00'
      STOP
      END`;

const fortranValid = codeGenerator.validateSyntax(validFortran, 'FORTRAN');
console.log('Valid FORTRAN:', fortranValid.valid ? '✓ PASS' : '✗ FAIL');

// Valid PASCAL code
const validPascal = `PROGRAM Test;
BEGIN
    WriteLn('RESULT: 100.00');
END.`;

const pascalValid = codeGenerator.validateSyntax(validPascal, 'PASCAL');
console.log('Valid PASCAL:', pascalValid.valid ? '✓ PASS' : '✗ FAIL');

// Valid BASIC code
const validBasic = `10 REM TEST PROGRAM
20 PRINT "RESULT: 100.00"
30 END`;

const basicValid = codeGenerator.validateSyntax(validBasic, 'BASIC');
console.log('Valid BASIC:', basicValid.valid ? '✓ PASS' : '✗ FAIL');

console.log();

// TEST 3: File Saving
console.log('TEST 3: Automatic File Saving to Legacy Directory');
console.log('─────────────────────────────────────────────────────');

async function testFileSaving() {
  const testCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTGEN.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "RESULT: 999.99"
           STOP RUN.`;
  
  try {
    const result = await codeGenerator.saveGeneratedCode(testCode, 'COBOL', 'testgen');
    
    console.log('✓ File saved successfully');
    console.log(`  Path: ${result.filePath}`);
    console.log(`  Language: ${result.language}`);
    console.log(`  Filename: ${result.filename}`);
    
    // Verify file exists
    if (fs.existsSync(result.filePath)) {
      console.log('✓ File verified on disk');
      
      // Clean up test file
      fs.unlinkSync(result.filePath);
      console.log('✓ Test file cleaned up');
    } else {
      console.log('✗ File not found on disk');
    }
  } catch (error) {
    console.log(`✗ File saving failed: ${error.message}`);
  }
}

// TEST 4: Complete Workflow
console.log('\nTEST 4: Complete Workflow (summonAncientSpirit)');
console.log('─────────────────────────────────────────────────────');

async function testCompleteWorkflow() {
  const simpleCobol = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORKFLOW.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 9(4)V99 VALUE 123.45.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "RESULT: " WS-RESULT
           STOP RUN.`;
  
  try {
    const result = await codeGenerator.summonAncientSpirit(simpleCobol, 'COBOL', 'workflow-test');
    
    console.log(`Success: ${result.success ? '✓' : '✗'}`);
    console.log(`Message: ${result.message}`);
    
    if (result.saved) {
      console.log(`✓ Saved to: ${result.saved.filePath}`);
    }
    
    if (result.compiled) {
      console.log(`Compilation: ${result.compiled.success ? '✓ SUCCESS' : '✗ FAILED'}`);
      console.log(`Compiler: ${result.compiled.compiler}`);
      if (result.compiled.message) {
        console.log(`Message: ${result.compiled.message}`);
      }
    }
    
    // Clean up
    if (result.saved && fs.existsSync(result.saved.filePath)) {
      fs.unlinkSync(result.saved.filePath);
      console.log('✓ Test files cleaned up');
    }
    
    // Clean up binary if created
    const binaryPath = path.join(process.cwd(), 'legacy/cobol/workflow-test');
    if (fs.existsSync(binaryPath)) {
      fs.unlinkSync(binaryPath);
    }
  } catch (error) {
    console.log(`✗ Workflow failed: ${error.message}`);
  }
}

// Run async tests
(async () => {
  await testFileSaving();
  await testCompleteWorkflow();
  
  console.log('\n═══════════════════════════════════════════════════════');
  console.log('✨ CODE GENERATOR TEST COMPLETE');
  console.log('═══════════════════════════════════════════════════════');
})();
