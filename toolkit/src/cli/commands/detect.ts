/**
 * ═══════════════════════════════════════════════════════════
 * DETECT COMMAND - AUTO-DETECT LEGACY BINARIES
 * ═══════════════════════════════════════════════════════════
 */

import * as fs from 'fs';
import * as path from 'path';
import { execSync } from 'child_process';

interface DetectOptions {
  directory?: string;
}

interface DetectedBinary {
  name: string;
  path: string;
  language: string;
  sourceFile?: string;
}

export async function detectCommand(options: DetectOptions): Promise<void> {
  const directory = options.directory || './legacy';
  
  console.log('═══════════════════════════════════════════════════════');
  console.log('🔍 SCANNING FOR LEGACY BINARIES');
  console.log('═══════════════════════════════════════════════════════');
  console.log(`📂 Directory: ${directory}`);
  console.log('');
  
  if (!fs.existsSync(directory)) {
    console.error(`❌ DIRECTORY NOT FOUND: ${directory}`);
    console.error('   Create the directory or specify a different path with --directory');
    process.exit(1);
  }
  
  try {
    // DETECT COMPILERS
    console.log('🔧 CHECKING FOR INSTALLED COMPILERS...');
    const compilers = detectCompilers();
    console.log('');
    
    // SCAN FOR BINARIES
    console.log('📡 SCANNING FOR LEGACY BINARIES...');
    const binaries = scanDirectory(directory);
    console.log('');
    
    if (binaries.length === 0) {
      console.log('⚠️  NO LEGACY BINARIES DETECTED');
      console.log('');
      console.log('SUGGESTIONS:');
      console.log('1. Compile your legacy programs first');
      console.log('2. Ensure binaries are executable (chmod +x)');
      console.log('3. Check the directory path');
      process.exit(0);
    }
    
    // DISPLAY DETECTED BINARIES
    console.log(`✅ FOUND ${binaries.length} LEGACY BINARY(IES):`);
    console.log('');
    binaries.forEach((binary, index) => {
      console.log(`${index + 1}. ${binary.name}`);
      console.log(`   Path: ${binary.path}`);
      console.log(`   Language: ${binary.language.toUpperCase()}`);
      if (binary.sourceFile) {
        console.log(`   Source: ${binary.sourceFile}`);
      }
      console.log('');
    });
    
    // GENERATE CONFIGURATION
    const config = generateConfiguration(binaries);
    const configPath = 'necro-bridge.config.json';
    
    fs.writeFileSync(configPath, JSON.stringify(config, null, 2));
    console.log(`✓ Configuration saved to: ${configPath}`);
    console.log('');
    
    console.log('═══════════════════════════════════════════════════════');
    console.log('✅ DETECTION COMPLETE');
    console.log('═══════════════════════════════════════════════════════');
    console.log('');
    console.log('NEXT STEPS:');
    console.log('1. Review the generated configuration file');
    console.log('2. Start the bridge server: necro-bridge serve');
    console.log('3. Test your binaries: necro-bridge test <binary>');
    console.log('');
    
  } catch (error) {
    console.error('❌ DETECTION FAILED:', error);
    process.exit(1);
  }
}

function detectCompilers(): Record<string, boolean> {
  const compilers = {
    cobol: checkCompiler('cobc --version', 'GnuCOBOL'),
    fortran: checkCompiler('gfortran --version', 'GNU Fortran'),
    pascal: checkCompiler('fpc -h', 'Free Pascal'),
    basic: checkCompiler('fbc --version', 'FreeBASIC')
  };
  
  Object.entries(compilers).forEach(([lang, installed]) => {
    const status = installed ? '✓' : '✗';
    const langUpper = lang.toUpperCase().padEnd(8);
    const pathInfo = installed ? getCompilerPath(getCompilerCommand(lang)) : '';
    console.log(`   ${status} ${langUpper} ${installed ? 'INSTALLED' : 'NOT FOUND'}${pathInfo ? ' (' + pathInfo + ')' : ''}`);
  });
  
  return compilers;
}

function checkCompiler(command: string, expectedOutput: string): boolean {
  try {
    const output = execSync(command, { encoding: 'utf-8', stdio: 'pipe' });
    return output.includes(expectedOutput) || output.length > 0;
  } catch {
    return false;
  }
}

function getCompilerCommand(language: string): string {
  const commands: Record<string, string> = {
    cobol: 'cobc',
    fortran: 'gfortran',
    pascal: 'fpc',
    basic: 'fbc'
  };
  return commands[language] || '';
}

function getCompilerPath(command: string): string {
  try {
    // USE 'which' ON UNIX-LIKE SYSTEMS TO FIND COMPILER PATH
    const path = execSync(`which ${command}`, { encoding: 'utf-8', stdio: 'pipe' }).trim();
    return path;
  } catch {
    return '';
  }
}

function scanDirectory(directory: string): DetectedBinary[] {
  const binaries: DetectedBinary[] = [];
  
  function scanRecursive(dir: string): void {
    const entries = fs.readdirSync(dir, { withFileTypes: true });
    
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      
      if (entry.isDirectory()) {
        scanRecursive(fullPath);
      } else if (entry.isFile()) {
        // CHECK IF FILE IS EXECUTABLE
        try {
          fs.accessSync(fullPath, fs.constants.X_OK);
          
          // DETECT LANGUAGE BASED ON DIRECTORY OR SOURCE FILE
          const language = detectLanguage(dir, entry.name);
          
          if (language) {
            const sourceFile = findSourceFile(dir, entry.name);
            
            binaries.push({
              name: entry.name,
              path: fullPath,
              language: language,
              sourceFile: sourceFile
            });
            
            console.log(`   ✓ Found: ${fullPath} (${language.toUpperCase()})`);
          }
        } catch {
          // NOT EXECUTABLE, SKIP
        }
      }
    }
  }
  
  scanRecursive(directory);
  return binaries;
}

function detectLanguage(directory: string, filename: string): string | null {
  // CHECK DIRECTORY NAME
  const dirName = path.basename(directory).toLowerCase();
  if (['cobol', 'fortran', 'pascal', 'basic'].includes(dirName)) {
    return dirName;
  }
  
  // CHECK FOR SOURCE FILES IN SAME DIRECTORY
  const files = fs.readdirSync(directory);
  
  if (files.some(f => f.endsWith('.cbl') || f.endsWith('.cob'))) return 'cobol';
  if (files.some(f => f.endsWith('.f') || f.endsWith('.f90') || f.endsWith('.for'))) return 'fortran';
  if (files.some(f => f.endsWith('.pas') || f.endsWith('.pp'))) return 'pascal';
  if (files.some(f => f.endsWith('.bas') || f.endsWith('.bi'))) return 'basic';
  
  // CHECK IF BINARY NAME MATCHES KNOWN SOURCE FILE
  const baseName = filename.replace(/\.(exe|out)$/, '');
  if (files.some(f => f === baseName + '.cbl' || f === baseName + '.cob')) return 'cobol';
  if (files.some(f => f === baseName + '.f' || f === baseName + '.f90' || f === baseName + '.for')) return 'fortran';
  if (files.some(f => f === baseName + '.pas' || f === baseName + '.pp')) return 'pascal';
  if (files.some(f => f === baseName + '.bas' || f === baseName + '.bi')) return 'basic';
  
  return null;
}

function findSourceFile(directory: string, binaryName: string): string | undefined {
  const extensions = ['.cbl', '.f', '.f90', '.pas', '.bas'];
  
  for (const ext of extensions) {
    const sourcePath = path.join(directory, binaryName + ext);
    if (fs.existsSync(sourcePath)) {
      return sourcePath;
    }
  }
  
  return undefined;
}

function generateConfiguration(binaries: DetectedBinary[]): any {
  return {
    project: {
      name: 'necro-bridge-project',
      version: '1.0.0',
      description: 'Legacy system integration via Necro-Bridge',
      languages: [...new Set(binaries.map(b => b.language))]
    },
    binaries: binaries.map(binary => ({
      name: binary.name,
      path: binary.path,
      language: binary.language,
      endpoint: `/api/calculate/${binary.language}/${binary.name}`,
      parameters: getDefaultParameters(binary.language),
      outputPattern: getDefaultOutputPattern(binary.language),
      timeout: 5000
    })),
    server: {
      port: 3001,
      timeout: 5000,
      cors: true,
      logging: true
    },
    paths: {
      legacy: './legacy',
      server: './server',
      bridges: './server/bridges'
    }
  };
}

function getDefaultParameters(language: string): string[] {
  const defaults: Record<string, string[]> = {
    cobol: ['principal', 'rate', 'term'],
    fortran: ['velocity', 'angle', 'gravity'],
    pascal: ['income', 'deductions', 'rate'],
    basic: ['principal', 'rate', 'time']
  };
  return defaults[language] || ['param1', 'param2', 'param3'];
}

function getDefaultOutputPattern(language: string): string {
  // ALL LANGUAGES USE STANDARDIZED OUTPUT FORMAT
  return 'RESULT:\\s*([\\d.]+)';
}
