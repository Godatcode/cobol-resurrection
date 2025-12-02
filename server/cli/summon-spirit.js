#!/usr/bin/env node

/**
 * SUMMON ANCIENT SPIRIT - CLI TOOL
 * Command-line interface for AI-powered legacy code generation
 * Can be invoked by Kiro MCP tools
 */

const codeGenerator = require('../services/code-generator');
const fs = require('fs');
const path = require('path');

// PARSE COMMAND LINE ARGUMENTS
const args = process.argv.slice(2);

// DISPLAY USAGE IF NO ARGUMENTS
if (args.length === 0 || args.includes('--help') || args.includes('-h')) {
  console.log(`
╔═══════════════════════════════════════════════════════════╗
║         👻 SUMMON ANCIENT SPIRIT - CLI TOOL              ║
║         AI-Powered Legacy Code Generator                  ║
╚═══════════════════════════════════════════════════════════╝

USAGE:
  summon-spirit <command> [options]

COMMANDS:
  generate    Generate and compile legacy code
  template    Display language template
  validate    Validate code syntax
  compile     Compile existing code

GENERATE OPTIONS:
  --language <lang>     Target language (COBOL, FORTRAN, PASCAL, BASIC)
  --filename <name>     Output filename (without extension)
  --code <code>         Source code (or use --file)
  --file <path>         Read code from file
  --description <desc>  Natural language description (for AI)

TEMPLATE OPTIONS:
  --language <lang>     Language template to display

VALIDATE OPTIONS:
  --language <lang>     Target language
  --file <path>         Code file to validate

COMPILE OPTIONS:
  --language <lang>     Target language
  --filename <name>     Filename to compile

EXAMPLES:
  # Generate COBOL code from file
  summon-spirit generate --language COBOL --filename loan --file code.cbl

  # Display FORTRAN template
  summon-spirit template --language FORTRAN

  # Validate PASCAL code
  summon-spirit validate --language PASCAL --file tax.pas

  # Compile BASIC program
  summon-spirit compile --language BASIC --filename interest

[END OF TAPE]
  `);
  process.exit(0);
}

// PARSE ARGUMENTS INTO OBJECT
function parseArgs(args) {
  const parsed = {
    command: args[0]
  };
  
  for (let i = 1; i < args.length; i++) {
    if (args[i].startsWith('--')) {
      const key = args[i].substring(2);
      const value = args[i + 1];
      parsed[key] = value;
      i++; // SKIP NEXT ARGUMENT
    }
  }
  
  return parsed;
}

const options = parseArgs(args);

// EXECUTE COMMAND
async function executeCommand() {
  try {
    switch (options.command) {
      case 'generate':
        await generateCode();
        break;
        
      case 'template':
        displayTemplate();
        break;
        
      case 'validate':
        validateCode();
        break;
        
      case 'compile':
        await compileCode();
        break;
        
      default:
        console.error(`❌ UNKNOWN COMMAND: ${options.command}`);
        console.error('USE --help FOR USAGE INFORMATION');
        process.exit(1);
    }
  } catch (error) {
    console.error(`❌ ERROR: ${error.message}`);
    process.exit(1);
  }
}

// GENERATE CODE COMMAND
async function generateCode() {
  const { language, filename, code, file } = options;
  
  if (!language || !filename) {
    console.error('❌ MISSING REQUIRED OPTIONS: --language and --filename');
    process.exit(1);
  }
  
  let sourceCode = code;
  
  // READ FROM FILE IF SPECIFIED
  if (file) {
    const filePath = path.resolve(file);
    if (!fs.existsSync(filePath)) {
      console.error(`❌ FILE NOT FOUND: ${filePath}`);
      process.exit(1);
    }
    sourceCode = fs.readFileSync(filePath, 'utf8');
  }
  
  if (!sourceCode) {
    console.error('❌ NO CODE PROVIDED: USE --code OR --file');
    process.exit(1);
  }
  
  console.log('🔮 SUMMONING ANCIENT SPIRIT...');
  console.log(`   LANGUAGE: ${language.toUpperCase()}`);
  console.log(`   FILENAME: ${filename}`);
  console.log(`   CODE LENGTH: ${sourceCode.length} BYTES`);
  console.log('');
  
  // VALIDATE SYNTAX
  const validation = codeGenerator.validateSyntax(sourceCode, language);
  if (!validation.valid) {
    console.error('⚠️  SYNTAX VALIDATION WARNINGS:');
    validation.errors.forEach(err => console.error(`   • ${err}`));
    console.log('');
  }
  
  // SUMMON THE SPIRIT
  const result = await codeGenerator.summonAncientSpirit(sourceCode, language, filename);
  
  if (result.success) {
    console.log('✨ ANCIENT SPIRIT SUMMONED SUCCESSFULLY!');
    console.log(`   FILE: ${result.saved.filePath}`);
    console.log(`   COMPILER: ${result.compiled.compiler}`);
    console.log(`   STATUS: ${result.compiled.message}`);
    console.log('');
    console.log('[END OF TAPE]');
    process.exit(0);
  } else {
    console.error('❌ SUMMONING FAILED:');
    console.error(`   ${result.message}`);
    if (result.compiled && result.compiled.stderr) {
      console.error('');
      console.error('COMPILATION ERRORS:');
      console.error(result.compiled.stderr);
    }
    process.exit(1);
  }
}

// DISPLAY TEMPLATE COMMAND
function displayTemplate() {
  const { language } = options;
  
  if (!language) {
    console.error('❌ MISSING REQUIRED OPTION: --language');
    process.exit(1);
  }
  
  try {
    const template = codeGenerator.loadTemplate(language);
    console.log(`📜 ${language.toUpperCase()} TEMPLATE:`);
    console.log('═══════════════════════════════════════════════════════');
    console.log(template);
    console.log('═══════════════════════════════════════════════════════');
    console.log('[END OF TAPE]');
  } catch (error) {
    console.error(`❌ TEMPLATE NOT FOUND: ${error.message}`);
    process.exit(1);
  }
}

// VALIDATE CODE COMMAND
function validateCode() {
  const { language, file } = options;
  
  if (!language || !file) {
    console.error('❌ MISSING REQUIRED OPTIONS: --language and --file');
    process.exit(1);
  }
  
  const filePath = path.resolve(file);
  if (!fs.existsSync(filePath)) {
    console.error(`❌ FILE NOT FOUND: ${filePath}`);
    process.exit(1);
  }
  
  const code = fs.readFileSync(filePath, 'utf8');
  const validation = codeGenerator.validateSyntax(code, language);
  
  console.log(`🔍 VALIDATING ${language.toUpperCase()} CODE...`);
  console.log(`   FILE: ${filePath}`);
  console.log('');
  
  if (validation.valid) {
    console.log('✅ SYNTAX VALIDATION PASSED');
    console.log('[END OF TAPE]');
    process.exit(0);
  } else {
    console.error('❌ SYNTAX VALIDATION FAILED:');
    validation.errors.forEach(err => console.error(`   • ${err}`));
    console.log('');
    console.log('[END OF TAPE]');
    process.exit(1);
  }
}

// COMPILE CODE COMMAND
async function compileCode() {
  const { language, filename } = options;
  
  if (!language || !filename) {
    console.error('❌ MISSING REQUIRED OPTIONS: --language and --filename');
    process.exit(1);
  }
  
  console.log(`🔨 COMPILING ${language.toUpperCase()} CODE...`);
  console.log(`   FILENAME: ${filename}`);
  console.log('');
  
  const result = await codeGenerator.compileCode(language, filename);
  
  if (result.success) {
    console.log('✅ COMPILATION SUCCESSFUL');
    console.log(`   COMPILER: ${result.compiler}`);
    console.log(`   MESSAGE: ${result.message}`);
    console.log('');
    console.log('[END OF TAPE]');
    process.exit(0);
  } else {
    console.error('❌ COMPILATION FAILED:');
    console.error(`   ${result.message}`);
    if (result.stderr) {
      console.error('');
      console.error('COMPILER OUTPUT:');
      console.error(result.stderr);
    }
    console.log('');
    console.log('[END OF TAPE]');
    process.exit(1);
  }
}

// RUN THE CLI
executeCommand();
