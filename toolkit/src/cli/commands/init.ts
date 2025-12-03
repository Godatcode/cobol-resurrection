/**
 * ═══════════════════════════════════════════════════════════
 * INIT COMMAND - INITIALIZE BRIDGE FOR LEGACY LANGUAGE
 * ═══════════════════════════════════════════════════════════
 */

import * as fs from 'fs';
import * as path from 'path';

const SUPPORTED_LANGUAGES = ['cobol', 'fortran', 'pascal', 'basic'];

export async function initCommand(language: string): Promise<void> {
  const lang = language.toLowerCase();
  
  // VALIDATE LANGUAGE
  if (!SUPPORTED_LANGUAGES.includes(lang)) {
    console.error(`❌ UNSUPPORTED LANGUAGE: ${language}`);
    console.error(`   SUPPORTED LANGUAGES: ${SUPPORTED_LANGUAGES.join(', ')}`);
    process.exit(1);
  }
  
  console.log('═══════════════════════════════════════════════════════');
  console.log(`👻 INITIALIZING ${language.toUpperCase()} BRIDGE`);
  console.log('═══════════════════════════════════════════════════════');
  
  try {
    // CREATE DIRECTORY STRUCTURE
    const dirs = ['legacy', `legacy/${lang}`, 'server', 'server/bridges'];
    for (const dir of dirs) {
      if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir, { recursive: true });
        console.log(`✓ Created directory: ${dir}`);
      }
    }
    
    // GENERATE BRIDGE SERVER FILE
    const bridgeTemplate = generateBridgeTemplate(lang);
    const bridgePath = `server/bridges/${capitalize(lang)}Bridge.js`;
    
    if (fs.existsSync(bridgePath)) {
      console.log(`⚠️  Bridge file already exists: ${bridgePath}`);
      console.log(`   Skipping bridge generation`);
    } else {
      fs.writeFileSync(bridgePath, bridgeTemplate);
      console.log(`✓ Generated bridge: ${bridgePath}`);
    }
    
    // GENERATE SERVER FILE
    const serverTemplate = generateServerTemplate(lang);
    const serverPath = 'server/server.js';
    
    if (fs.existsSync(serverPath)) {
      console.log(`⚠️  Server file already exists: ${serverPath}`);
      console.log(`   Skipping server generation`);
    } else {
      fs.writeFileSync(serverPath, serverTemplate);
      console.log(`✓ Generated server: ${serverPath}`);
    }
    
    // GENERATE CONFIG FILE
    const configTemplate = generateConfigTemplate(lang);
    const configPath = 'necro-bridge.config.json';
    
    if (fs.existsSync(configPath)) {
      console.log(`⚠️  Config file already exists: ${configPath}`);
      console.log(`   Skipping config generation`);
    } else {
      fs.writeFileSync(configPath, configTemplate);
      console.log(`✓ Generated config: ${configPath}`);
    }
    
    // GENERATE EXAMPLE LEGACY CODE
    const exampleCode = generateExampleCode(lang);
    const examplePath = `legacy/${lang}/example${getFileExtension(lang)}`;
    
    if (fs.existsSync(examplePath)) {
      console.log(`⚠️  Example file already exists: ${examplePath}`);
    } else {
      fs.writeFileSync(examplePath, exampleCode);
      console.log(`✓ Generated example: ${examplePath}`);
    }
    
    // GENERATE PACKAGE.JSON IF NOT EXISTS
    const packagePath = 'server/package.json';
    if (!fs.existsSync(packagePath)) {
      const packageTemplate = generatePackageJson();
      fs.writeFileSync(packagePath, packageTemplate);
      console.log(`✓ Generated package.json: ${packagePath}`);
    }
    
    console.log('═══════════════════════════════════════════════════════');
    console.log('✅ INITIALIZATION COMPLETE');
    console.log('═══════════════════════════════════════════════════════');
    console.log('');
    console.log('NEXT STEPS:');
    console.log(`1. Compile your ${lang.toUpperCase()} program:`);
    console.log(`   ${getCompileCommand(lang)}`);
    console.log('');
    console.log('2. Install server dependencies:');
    console.log('   cd server && npm install');
    console.log('');
    console.log('3. Start the bridge server:');
    console.log('   necro-bridge serve');
    console.log('');
    console.log('4. Test your legacy binary:');
    console.log(`   necro-bridge test ./legacy/${lang}/example`);
    console.log('');
    
  } catch (error) {
    console.error('❌ INITIALIZATION FAILED:', error);
    process.exit(1);
  }
}

function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

function getFileExtension(language: string): string {
  const extensions: Record<string, string> = {
    cobol: '.cbl',
    fortran: '.f',
    pascal: '.pas',
    basic: '.bas'
  };
  return extensions[language] || '.txt';
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

function generateBridgeTemplate(language: string): string {
  const langUpper = language.toUpperCase();
  const langCapital = capitalize(language);
  
  return `/**
 * ═══════════════════════════════════════════════════════════
 * ${langUpper} BRIDGE - GENERATED BY NECRO-BRIDGE CLI
 * ═══════════════════════════════════════════════════════════
 */

const LegacyBridge = require('./LegacyBridge');

class ${langCapital}Bridge extends LegacyBridge {
  constructor() {
    super({
      name: '${langUpper}',
      year: ${getLanguageYear(language)},
      binary: 'example',
      description: '${langUpper} CALCULATION',
      params: ['param1', 'param2', 'param3']
    });
  }
  
  /**
   * PARSE ${langUpper} OUTPUT FORMAT
   * CUSTOMIZE THIS METHOD BASED ON YOUR PROGRAM'S OUTPUT
   */
  parseOutput(stdout) {
    // DEFAULT PATTERN: "RESULT: XXXX.XX"
    const resultPattern = /RESULT:\\s*(\\d+\\.\\d{2})/;
    const match = stdout.match(resultPattern);
    
    if (!match) {
      return {
        success: false,
        result: null,
        error: '${langUpper} OUTPUT DOES NOT MATCH EXPECTED FORMAT'
      };
    }
    
    const result = parseFloat(match[1]);
    
    return {
      success: true,
      result: result,
      error: null
    };
  }
}

module.exports = ${langCapital}Bridge;
`;
}

function generateServerTemplate(language: string): string {
  const langCapital = capitalize(language);
  
  return `const express = require('express');
const cors = require('cors');
const ${langCapital}Bridge = require('./bridges/${langCapital}Bridge');

const app = express();
const PORT = 3001;

app.use(cors());
app.use(express.json());

// HEALTH CHECK
app.get('/api/health', (req, res) => {
  res.json({ 
    status: 'OPERATIONAL',
    message: 'NECRO-BRIDGE SERVER ONLINE'
  });
});

// CALCULATION ENDPOINT
app.post('/api/calculate', async (req, res) => {
  const { param1, param2, param3 } = req.body;
  
  try {
    const bridge = new ${langCapital}Bridge();
    const result = await bridge.execute({ param1, param2, param3 });
    res.json(result);
  } catch (error) {
    res.status(500).json(error);
  }
});

app.listen(PORT, () => {
  console.log(\`👻 NECRO-BRIDGE SERVER LISTENING ON PORT \${PORT}\`);
});
`;
}

function generateConfigTemplate(language: string): string {
  const config = {
    binaries: [
      {
        name: 'example',
        path: `./legacy/${language}/example`,
        language: language,
        endpoint: '/api/calculate',
        parameters: ['param1', 'param2', 'param3'],
        outputPattern: 'RESULT:\\s*(\\d+\\.\\d{2})'
      }
    ],
    server: {
      port: 3001,
      timeout: 5000
    }
  };
  
  return JSON.stringify(config, null, 2);
}

function generateExampleCode(language: string): string {
  const examples: Record<string, string> = {
    cobol: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXAMPLE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PARAM1        PIC 9(8)V99.
       01 WS-PARAM2        PIC 9(8)V99.
       01 WS-PARAM3        PIC 9(8)V99.
       01 WS-RESULT        PIC 9(8)V99.
       
       PROCEDURE DIVISION.
           ACCEPT WS-PARAM1.
           ACCEPT WS-PARAM2.
           ACCEPT WS-PARAM3.
           
           COMPUTE WS-RESULT = WS-PARAM1 + WS-PARAM2 + WS-PARAM3.
           
           DISPLAY "RESULT: " WS-RESULT.
           STOP RUN.`,
    
    fortran: `      PROGRAM EXAMPLE
      REAL PARAM1, PARAM2, PARAM3, RESULT
      
      READ(*,*) PARAM1
      READ(*,*) PARAM2
      READ(*,*) PARAM3
      
      RESULT = PARAM1 + PARAM2 + PARAM3
      
      WRITE(*,*) 'RESULT: ', RESULT
      
      END`,
    
    pascal: `program Example;
var
  param1, param2, param3, result: real;
begin
  readln(param1);
  readln(param2);
  readln(param3);
  
  result := param1 + param2 + param3;
  
  writeln('RESULT: ', result:0:2);
end.`,
    
    basic: `10 INPUT "PARAM1: ", P1
20 INPUT "PARAM2: ", P2
30 INPUT "PARAM3: ", P3
40 LET R = P1 + P2 + P3
50 PRINT "RESULT: "; R
60 END`
  };
  
  return examples[language] || '';
}

function generatePackageJson(): string {
  const pkg = {
    name: 'necro-bridge-server',
    version: '1.0.0',
    description: 'Bridge server for legacy system integration',
    main: 'server.js',
    scripts: {
      start: 'node server.js'
    },
    dependencies: {
      express: '^4.18.2',
      cors: '^2.8.5'
    }
  };
  
  return JSON.stringify(pkg, null, 2);
}

function getLanguageYear(language: string): number {
  const years: Record<string, number> = {
    cobol: 1959,
    fortran: 1957,
    pascal: 1970,
    basic: 1983
  };
  return years[language] || 1970;
}
