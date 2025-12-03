/**
 * ═══════════════════════════════════════════════════════════
 * SERVE COMMAND - START BRIDGE SERVER
 * ═══════════════════════════════════════════════════════════
 */

import * as fs from 'fs';
import * as path from 'path';
import { spawn } from 'child_process';

interface ServeOptions {
  port?: string;
  config?: string;
}

export async function serveCommand(options: ServeOptions): Promise<void> {
  const port = options.port || '3001';
  const configPath = options.config || './necro-bridge.config.json';
  
  console.log('═══════════════════════════════════════════════════════');
  console.log('👻 STARTING NECRO-BRIDGE SERVER');
  console.log('═══════════════════════════════════════════════════════');
  console.log(`🔌 Port: ${port}`);
  console.log(`📋 Config: ${configPath}`);
  console.log('');
  
  // CHECK IF CONFIG EXISTS
  if (!fs.existsSync(configPath)) {
    console.error(`❌ CONFIGURATION FILE NOT FOUND: ${configPath}`);
    console.error('');
    console.error('SUGGESTIONS:');
    console.error('1. Run: necro-bridge detect');
    console.error('2. Or: necro-bridge init <language>');
    console.error('3. Or specify config path: --config <path>');
    process.exit(1);
  }
  
  // LOAD CONFIGURATION
  let config: any;
  try {
    const configContent = fs.readFileSync(configPath, 'utf-8');
    config = JSON.parse(configContent);
  } catch (error) {
    console.error(`❌ FAILED TO PARSE CONFIGURATION: ${error}`);
    process.exit(1);
  }
  
  // VALIDATE BINARIES EXIST
  console.log('🔍 VALIDATING BINARIES...');
  let validationFailed = false;
  
  for (const binary of config.binaries || []) {
    if (!fs.existsSync(binary.path)) {
      console.error(`   ✗ Binary not found: ${binary.path}`);
      validationFailed = true;
    } else {
      console.log(`   ✓ ${binary.name} (${binary.language.toUpperCase()})`);
    }
  }
  
  if (validationFailed) {
    console.error('');
    console.error('❌ VALIDATION FAILED');
    console.error('   Compile your legacy programs before starting the server');
    process.exit(1);
  }
  
  console.log('');
  
  // CHECK IF SERVER FILE EXISTS
  const serverPath = path.join(process.cwd(), 'server', 'server.js');
  
  if (!fs.existsSync(serverPath)) {
    console.log('⚠️  SERVER FILE NOT FOUND');
    console.log('   Generating minimal server...');
    console.log('');
    
    // GENERATE MINIMAL SERVER
    generateMinimalServer(config, port);
  }
  
  // START SERVER
  console.log('🚀 LAUNCHING SERVER...');
  console.log('');
  
  const serverProcess = spawn('node', [serverPath], {
    env: {
      ...process.env,
      PORT: port,
      CONFIG_PATH: configPath
    },
    stdio: 'inherit'
  });
  
  serverProcess.on('error', (error) => {
    console.error('❌ FAILED TO START SERVER:', error);
    process.exit(1);
  });
  
  serverProcess.on('exit', (code) => {
    if (code !== 0) {
      console.error(`❌ SERVER EXITED WITH CODE: ${code}`);
      process.exit(code || 1);
    }
  });
  
  // HANDLE GRACEFUL SHUTDOWN
  process.on('SIGINT', () => {
    console.log('');
    console.log('🛑 SHUTTING DOWN SERVER...');
    serverProcess.kill('SIGINT');
    process.exit(0);
  });
}

function generateMinimalServer(config: any, port: string): void {
  const serverDir = path.join(process.cwd(), 'server');
  const bridgesDir = path.join(serverDir, 'bridges');
  
  // CREATE DIRECTORIES
  if (!fs.existsSync(serverDir)) {
    fs.mkdirSync(serverDir, { recursive: true });
  }
  if (!fs.existsSync(bridgesDir)) {
    fs.mkdirSync(bridgesDir, { recursive: true });
  }
  
  // GENERATE LEGACY BRIDGE BASE CLASS
  const legacyBridgeCode = `const { exec } = require('child_process');

class LegacyBridge {
  constructor(config) {
    this.config = config;
    this.name = config.name;
    this.binary = config.binary;
    this.params = config.params;
    this.timeout = 5000;
  }
  
  validateParams(params) {
    const missingParams = this.params.filter(param => params[param] === undefined);
    if (missingParams.length > 0) {
      return { valid: false, error: \`MISSING PARAMETERS: \${missingParams.join(', ')}\` };
    }
    return { valid: true, error: null };
  }
  
  buildCommand(params) {
    const paramValues = this.params.map(param => params[param]).join(' ');
    return \`\${this.binary} \${paramValues}\`;
  }
  
  parseOutput(stdout) {
    throw new Error('SUBCLASSES MUST IMPLEMENT parseOutput()');
  }
  
  async execute(params) {
    return new Promise((resolve, reject) => {
      const validation = this.validateParams(params);
      if (!validation.valid) {
        return reject({ error: 'INVALID INPUT', details: validation.error });
      }
      
      const command = this.buildCommand(params);
      
      exec(command, { timeout: this.timeout }, (error, stdout, stderr) => {
        if (error) {
          return reject({ error: 'CORE DUMP DETECTED', details: stderr || error.message });
        }
        
        const parseResult = this.parseOutput(stdout);
        
        if (!parseResult.success) {
          return reject({ error: 'PARSING FAILED', details: parseResult.error });
        }
        
        resolve({
          result: parseResult.result,
          source: \`\${this.name}_LEGACY_ENGINE\`,
          timestamp: new Date().toISOString()
        });
      });
    });
  }
}

module.exports = LegacyBridge;
`;
  
  fs.writeFileSync(path.join(bridgesDir, 'LegacyBridge.js'), legacyBridgeCode);
  
  // GENERATE BRIDGE FOR EACH LANGUAGE
  for (const binary of config.binaries || []) {
    const langCapital = capitalize(binary.language);
    const bridgeCode = `const LegacyBridge = require('./LegacyBridge');

class ${langCapital}Bridge extends LegacyBridge {
  constructor() {
    super({
      name: '${binary.language.toUpperCase()}',
      binary: '${binary.path}',
      params: ${JSON.stringify(binary.parameters)}
    });
  }
  
  parseOutput(stdout) {
    const pattern = /${binary.outputPattern}/;
    const match = stdout.match(pattern);
    
    if (!match) {
      return { success: false, result: null, error: 'OUTPUT FORMAT MISMATCH' };
    }
    
    return { success: true, result: parseFloat(match[1]), error: null };
  }
}

module.exports = ${langCapital}Bridge;
`;
    
    fs.writeFileSync(path.join(bridgesDir, `${langCapital}Bridge.js`), bridgeCode);
  }
  
  // GENERATE SERVER FILE
  const serverCode = `const express = require('express');
const cors = require('cors');
${config.binaries.map((b: any) => `const ${capitalize(b.language)}Bridge = require('./bridges/${capitalize(b.language)}Bridge');`).join('\n')}

const app = express();
const PORT = process.env.PORT || ${port};

app.use(cors());
app.use(express.json());

app.get('/api/health', (req, res) => {
  res.json({ status: 'OPERATIONAL', message: 'NECRO-BRIDGE SERVER ONLINE' });
});

${config.binaries.map((b: any) => `
app.post('${b.endpoint}', async (req, res) => {
  try {
    const bridge = new ${capitalize(b.language)}Bridge();
    const result = await bridge.execute(req.body);
    res.json(result);
  } catch (error) {
    res.status(500).json(error);
  }
});`).join('\n')}

app.listen(PORT, () => {
  console.log('═══════════════════════════════════════════════════════');
  console.log('👻 NECRO-BRIDGE SERVER ONLINE');
  console.log('═══════════════════════════════════════════════════════');
  console.log(\`🔌 Listening on port \${PORT}\`);
  console.log('📡 Available endpoints:');
${config.binaries.map((b: any) => `  console.log('   POST ${b.endpoint} (${b.language.toUpperCase()})');`).join('\n')}
  console.log('═══════════════════════════════════════════════════════');
});
`;
  
  fs.writeFileSync(path.join(serverDir, 'server.js'), serverCode);
  
  // GENERATE PACKAGE.JSON
  const packageJson = {
    name: 'necro-bridge-server',
    version: '1.0.0',
    main: 'server.js',
    dependencies: {
      express: '^4.18.2',
      cors: '^2.8.5'
    }
  };
  
  fs.writeFileSync(
    path.join(serverDir, 'package.json'),
    JSON.stringify(packageJson, null, 2)
  );
  
  console.log('   ✓ Generated server files');
  console.log('   ⚠️  Run: cd server && npm install');
  console.log('');
}

function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}
