/**
 * TEST SCRIPT FOR CODE GENERATOR SERVICE
 * Validates that the code generation workflow functions correctly
 */

const codeGenerator = require('./services/code-generator');
const fs = require('fs');
const path = require('path');

console.log('═══════════════════════════════════════════════════════');
console.log('👻 TESTING CODE GENERATOR SERVICE');
console.log('═══════════════════════════════════════════════════════');
console.log('');

// TEST 1: LOAD TEMPLATES
console.log('TEST 1: LOADING LANGUAGE TEMPLATES');
console.log('─────────────────────────────────────────────────────');

const languages = ['COBOL', 'FORTRAN', 'PASCAL', 'BASIC'];
let templatesLoaded = 0;

languages.forEach(lang => {
  try {
    const template = codeGenerator.loadTemplate(lang);
    if (template && template.length > 0) {
      console.log(`✅ ${lang}: Template loaded (${template.length} bytes)`);
      templatesLoaded++;
    } else {
      console.log(`❌ ${lang}: Template empty`);
    }
  } catch (error) {
    console.log(`❌ ${lang}: ${error.message}`);
  }
});

console.log(`\nRESULT: ${templatesLoaded}/${languages.length} templates loaded`);
console.log('');

// TEST 2: VALIDATE SYNTAX
console.log('TEST 2: SYNTAX VALIDATION');
console.log('─────────────────────────────────────────────────────');

// VALID COBOL CODE
const validCobol = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM PIC 9(5).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "RESULT: 12345"
           STOP RUN.
`;

const cobolValidation = codeGenerator.validateSyntax(validCobol, 'COBOL');
console.log(`COBOL Validation: ${cobolValidation.valid ? '✅ PASSED' : '❌ FAILED'}`);
if (!cobolValidation.valid) {
  console.log('  Errors:', cobolValidation.errors);
}

// INVALID COBOL CODE (MISSING PROCEDURE DIVISION)
const invalidCobol = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
`;

const invalidValidation = codeGenerator.validateSyntax(invalidCobol, 'COBOL');
console.log(`Invalid COBOL Detection: ${!invalidValidation.valid ? '✅ PASSED' : '❌ FAILED'}`);
if (!invalidValidation.valid) {
  console.log('  Expected errors:', invalidValidation.errors);
}

console.log('');

// TEST 3: SAVE CODE
console.log('TEST 3: SAVING GENERATED CODE');
console.log('─────────────────────────────────────────────────────');

const testCode = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTGEN.
       AUTHOR. CODE-GENERATOR-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 9(5) VALUE 42.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "RESULT: " WS-RESULT
           STOP RUN.
`;

async function testSaveCode() {
  try {
    const result = await codeGenerator.saveGeneratedCode(testCode, 'COBOL', 'testgen');
    
    if (result.success) {
      console.log('✅ Code saved successfully');
      console.log(`   File: ${result.filePath}`);
      
      // VERIFY FILE EXISTS
      if (fs.existsSync(result.filePath)) {
        console.log('✅ File verified on disk');
        
        // CLEAN UP TEST FILE
        fs.unlinkSync(result.filePath);
        console.log('✅ Test file cleaned up');
      } else {
        console.log('❌ File not found on disk');
      }
    } else {
      console.log('❌ Save failed');
    }
  } catch (error) {
    console.log(`❌ Error: ${error.message}`);
  }
}

// TEST 4: LANGUAGE CONFIG
console.log('');
console.log('TEST 4: LANGUAGE CONFIGURATION');
console.log('─────────────────────────────────────────────────────');

const config = codeGenerator.LANGUAGE_CONFIG;
Object.keys(config).forEach(lang => {
  const langConfig = config[lang];
  console.log(`${lang}:`);
  console.log(`  Extension: ${langConfig.extension}`);
  console.log(`  Directory: ${langConfig.directory}`);
  console.log(`  Compiler: ${langConfig.compiler}`);
});

console.log('');

// RUN ASYNC TESTS
(async () => {
  await testSaveCode();
  
  console.log('');
  console.log('═══════════════════════════════════════════════════════');
  console.log('✅ CODE GENERATOR SERVICE TESTS COMPLETE');
  console.log('═══════════════════════════════════════════════════════');
  console.log('[END OF TAPE]');
})();
