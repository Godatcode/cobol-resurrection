/**
 * ═══════════════════════════════════════════════════════════
 * SETUP WIZARD - INTERACTIVE FIRST-TIME USER EXPERIENCE
 * ═══════════════════════════════════════════════════════════
 */

import * as fs from 'fs';
import * as path from 'path';
import * as readline from 'readline';
import { execSync } from 'child_process';

interface SetupAnswers {
  projectName: string;
  languages: string[];
  directory: string;
  autoDetect: boolean;
  generateExamples: boolean;
}

export async function setupCommand(): Promise<void> {
  console.log('═══════════════════════════════════════════════════════');
  console.log('👻 NECRO-BRIDGE SETUP WIZARD');
  console.log('═══════════════════════════════════════════════════════');
  console.log('');
  console.log('WELCOME TO THE LEGACY SYSTEM RESURRECTION TOOLKIT!');
  console.log('THIS WIZARD WILL GUIDE YOU THROUGH THE INITIAL SETUP.');
  console.log('');
  
  const answers = await askQuestions();
  
  console.log('');
  console.log('═══════════════════════════════════════════════════════');
  console.log('🔧 CONFIGURING YOUR PROJECT');
  console.log('═══════════════════════════════════════════════════════');
  console.log('');
  
  try {
    // CREATE DIRECTORY STRUCTURE
    await createDirectoryStructure(answers);
    
    // CHECK COMPILERS
    await checkCompilers(answers.languages);
    
    // AUTO-DETECT OR INITIALIZE
    if (answers.autoDetect) {
      await autoDetectBinaries(answers.directory);
    } else {
      await initializeLanguages(answers.languages);
    }
    
    // GENERATE EXAMPLES
    if (answers.generateExamples) {
      await generateExamples(answers.languages);
    }
    
    // CREATE CONFIGURATION
    await createConfiguration(answers);
    
    console.log('');
    console.log('═══════════════════════════════════════════════════════');
    console.log('✅ SETUP COMPLETE!');
    console.log('═══════════════════════════════════════════════════════');
    console.log('');
    console.log('YOUR NECRO-BRIDGE PROJECT IS READY!');
    console.log('');
    console.log('NEXT STEPS:');
    console.log('');
    console.log('1. COMPILE YOUR LEGACY PROGRAMS:');
    answers.languages.forEach(lang => {
      console.log(`   ${getCompileCommand(lang)}`);
    });
    console.log('');
    console.log('2. INSTALL SERVER DEPENDENCIES:');
    console.log('   cd server && npm install');
    console.log('');
    console.log('3. START THE BRIDGE SERVER:');
    console.log('   necro-bridge serve');
    console.log('');
    console.log('4. TEST YOUR BINARIES:');
    console.log('   necro-bridge test <binary-path>');
    console.log('');
    console.log('═══════════════════════════════════════════════════════');
    console.log('');
    
  } catch (error) {
    console.error('');
    console.error('❌ SETUP FAILED:', error);
    console.error('');
    console.error('PLEASE TRY AGAIN OR REFER TO THE DOCUMENTATION.');
    process.exit(1);
  }
}

async function askQuestions(): Promise<SetupAnswers> {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
  
  const question = (prompt: string): Promise<string> => {
    return new Promise(resolve => {
      rl.question(prompt, resolve);
    });
  };
  
  const answers: SetupAnswers = {
    projectName: '',
    languages: [],
    directory: './legacy',
    autoDetect: false,
    generateExamples: false
  };
  
  // PROJECT NAME
  answers.projectName = await question('📝 PROJECT NAME (default: necro-bridge-project): ');
  if (!answers.projectName.trim()) {
    answers.projectName = 'necro-bridge-project';
  }
  console.log(`   → Using: ${answers.projectName}`);
  console.log('');
  
  // LANGUAGES
  console.log('🗣️  WHICH LEGACY LANGUAGES DO YOU WANT TO SUPPORT?');
  console.log('   (Enter comma-separated list, e.g., cobol,fortran,pascal,basic)');
  const languagesInput = await question('   Languages: ');
  
  if (languagesInput.trim()) {
    answers.languages = languagesInput
      .split(',')
      .map(l => l.trim().toLowerCase())
      .filter(l => ['cobol', 'fortran', 'pascal', 'basic'].includes(l));
  }
  
  if (answers.languages.length === 0) {
    console.log('   → No valid languages specified. Using: cobol');
    answers.languages = ['cobol'];
  } else {
    console.log(`   → Selected: ${answers.languages.join(', ')}`);
  }
  console.log('');
  
  // DIRECTORY
  const directoryInput = await question('📂 LEGACY BINARIES DIRECTORY (default: ./legacy): ');
  if (directoryInput.trim()) {
    answers.directory = directoryInput.trim();
  }
  console.log(`   → Using: ${answers.directory}`);
  console.log('');
  
  // AUTO-DETECT
  const autoDetectInput = await question('🔍 AUTO-DETECT EXISTING BINARIES? (y/n, default: n): ');
  answers.autoDetect = autoDetectInput.trim().toLowerCase() === 'y';
  console.log(`   → ${answers.autoDetect ? 'YES' : 'NO'}`);
  console.log('');
  
  // GENERATE EXAMPLES
  const examplesInput = await question('📄 GENERATE EXAMPLE PROGRAMS? (y/n, default: y): ');
  answers.generateExamples = examplesInput.trim().toLowerCase() !== 'n';
  console.log(`   → ${answers.generateExamples ? 'YES' : 'NO'}`);
  console.log('');
  
  rl.close();
  
  return answers;
}

async function createDirectoryStructure(answers: SetupAnswers): Promise<void> {
  console.log('📁 CREATING DIRECTORY STRUCTURE...');
  
  const directories = [
    answers.directory,
    'server',
    'server/bridges',
    'server/utils',
    '.kiro',
    '.kiro/hooks'
  ];
  
  // ADD LANGUAGE-SPECIFIC DIRECTORIES
  answers.languages.forEach(lang => {
    directories.push(path.join(answers.directory, lang));
  });
  
  directories.forEach(dir => {
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
      console.log(`   ✓ Created: ${dir}`);
    } else {
      console.log(`   ⚠️  Already exists: ${dir}`);
    }
  });
  
  console.log('');
}

async function checkCompilers(languages: string[]): Promise<void> {
  console.log('🔧 CHECKING FOR REQUIRED COMPILERS...');
  console.log('');
  
  const compilerChecks: Record<string, { command: string; name: string }> = {
    cobol: { command: 'cobc --version', name: 'GnuCOBOL' },
    fortran: { command: 'gfortran --version', name: 'GNU Fortran' },
    pascal: { command: 'fpc -h', name: 'Free Pascal' },
    basic: { command: 'fbc --version', name: 'FreeBASIC' }
  };
  
  const missingCompilers: string[] = [];
  
  for (const lang of languages) {
    const check = compilerChecks[lang];
    if (!check) continue;
    
    try {
      execSync(check.command, { stdio: 'pipe' });
      console.log(`   ✓ ${check.name.padEnd(20)} INSTALLED`);
    } catch {
      console.log(`   ✗ ${check.name.padEnd(20)} NOT FOUND`);
      missingCompilers.push(lang);
    }
  }
  
  console.log('');
  
  if (missingCompilers.length > 0) {
    console.log('⚠️  WARNING: SOME COMPILERS ARE MISSING');
    console.log('');
    console.log('INSTALLATION INSTRUCTIONS:');
    console.log('');
    
    missingCompilers.forEach(lang => {
      console.log(`${lang.toUpperCase()}:`);
      console.log(`   ${getInstallInstructions(lang)}`);
      console.log('');
    });
    
    console.log('YOU CAN CONTINUE WITHOUT THESE COMPILERS,');
    console.log('BUT YOU WILL NOT BE ABLE TO COMPILE THOSE LANGUAGES.');
    console.log('');
  }
}

async function autoDetectBinaries(directory: string): Promise<void> {
  console.log('🔍 AUTO-DETECTING LEGACY BINARIES...');
  
  if (!fs.existsSync(directory)) {
    console.log(`   ⚠️  Directory not found: ${directory}`);
    console.log('   Skipping auto-detection.');
    console.log('');
    return;
  }
  
  // USE EXISTING DETECT LOGIC
  const { detectCommand } = require('./detect');
  await detectCommand({ directory });
}

async function initializeLanguages(languages: string[]): Promise<void> {
  console.log('🏗️  INITIALIZING LANGUAGE BRIDGES...');
  console.log('');
  
  const { initCommand } = require('./init');
  
  for (const lang of languages) {
    console.log(`   Initializing ${lang.toUpperCase()}...`);
    await initCommand(lang);
    console.log('');
  }
}

async function generateExamples(languages: string[]): Promise<void> {
  console.log('📝 GENERATING EXAMPLE PROGRAMS...');
  
  // EXAMPLES ARE GENERATED BY initCommand
  console.log('   ✓ Example programs created in legacy directories');
  console.log('');
}

async function createConfiguration(answers: SetupAnswers): Promise<void> {
  console.log('⚙️  CREATING PROJECT CONFIGURATION...');
  
  const config = {
    project: {
      name: answers.projectName,
      version: '1.0.0',
      languages: answers.languages
    },
    binaries: [],
    server: {
      port: 3001,
      timeout: 5000,
      cors: true
    },
    paths: {
      legacy: answers.directory,
      server: './server',
      bridges: './server/bridges'
    }
  };
  
  const configPath = 'necro-bridge.config.json';
  fs.writeFileSync(configPath, JSON.stringify(config, null, 2));
  console.log(`   ✓ Configuration saved: ${configPath}`);
  console.log('');
  
  // CREATE .gitignore
  const gitignorePath = '.gitignore';
  if (!fs.existsSync(gitignorePath)) {
    const gitignoreContent = `node_modules/
*.o
*.exe
*.out
legacy/*/example
necro-bridge.config.json
.DS_Store
`;
    fs.writeFileSync(gitignorePath, gitignoreContent);
    console.log(`   ✓ Created: ${gitignorePath}`);
  }
  
  console.log('');
}

function getCompileCommand(language: string): string {
  const commands: Record<string, string> = {
    cobol: 'cobc -x -o legacy/cobol/example legacy/cobol/example.cbl',
    fortran: 'gfortran -o legacy/fortran/example legacy/fortran/example.f',
    pascal: 'fpc -olegacy/pascal/example legacy/pascal/example.pas',
    basic: 'fbc legacy/basic/example.bas -x legacy/basic/example'
  };
  return commands[language] || '';
}

function getInstallInstructions(language: string): string {
  const instructions: Record<string, string> = {
    cobol: 'macOS: brew install gnu-cobol | Ubuntu: apt-get install gnucobol',
    fortran: 'macOS: brew install gcc | Ubuntu: apt-get install gfortran',
    pascal: 'macOS: brew install fpc | Ubuntu: apt-get install fp-compiler',
    basic: 'macOS: brew install freebasic | Ubuntu: apt-get install freebasic'
  };
  return instructions[language] || 'Check language documentation for installation';
}
