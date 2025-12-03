# Bridge Templates

This directory contains pre-built bridge server templates for various legacy languages. These templates provide a standardized way to wrap legacy binaries with modern REST APIs.

## 📦 Available Templates

### Language-Specific Bridges
- **`cobol-bridge.js`** - Bridge template for COBOL binaries (1959)
- **`fortran-bridge.js`** - Bridge template for FORTRAN binaries (1957)
- **`pascal-bridge.js`** - Bridge template for PASCAL binaries (1970)
- **`basic-bridge.js`** - Bridge template for BASIC binaries (1983)

### Core Infrastructure
- **`LegacyBridge.js`** - Abstract base class for all bridges
- **`universal-server.js`** - Express.js server supporting multiple languages
- **`package.json.template`** - NPM package configuration

## 🚀 Quick Start

### Option 1: Single Language Bridge

1. **Copy the files you need:**
   ```bash
   cp LegacyBridge.js /your-project/
   cp cobol-bridge.js /your-project/
   ```

2. **Customize the bridge:**
   ```javascript
   // In cobol-bridge.js
   constructor() {
     super({
       name: 'COBOL',
       year: 1959,
       binary: 'mortgage',           // ⚠️ YOUR BINARY NAME
       description: 'MORTGAGE CALC',
       params: ['principal', 'rate', 'term']  // ⚠️ YOUR PARAMETERS
     });
   }
   ```

3. **Customize the output parser:**
   ```javascript
   parseOutput(stdout) {
     // ⚠️ MODIFY REGEX TO MATCH YOUR OUTPUT
     const resultPattern = /RESULT:\s*(\d+\.\d{2})/;
     const match = stdout.match(resultPattern);
     // ... rest of parsing logic
   }
   ```

4. **Run as standalone server:**
   ```javascript
   // Uncomment the server code at the bottom of cobol-bridge.js
   // Then run:
   node cobol-bridge.js
   ```

### Option 2: Multi-Language Universal Server

1. **Copy all files:**
   ```bash
   cp LegacyBridge.js /your-project/
   cp *-bridge.js /your-project/
   cp universal-server.js /your-project/
   cp package.json.template /your-project/package.json
   ```

2. **Install dependencies:**
   ```bash
   npm install
   ```

3. **Customize each bridge** (see Option 1, step 2-3)

4. **Register bridges in universal-server.js:**
   ```javascript
   const CobolBridge = require('./cobol-bridge');
   const FortranBridge = require('./fortran-bridge');
   
   const bridges = {
     cobol: new CobolBridge(),
     fortran: new FortranBridge()
   };
   ```

5. **Start the server:**
   ```bash
   npm start
   ```

## 📡 API Endpoints

Once your server is running, you'll have these endpoints:

### Health Check
```bash
GET /api/health
```

**Response:**
```json
{
  "status": "OPERATIONAL",
  "supported_languages": ["cobol", "fortran", "pascal", "basic"]
}
```

### List Languages
```bash
GET /api/languages
```

**Response:**
```json
{
  "count": 4,
  "languages": [
    {
      "id": "cobol",
      "name": "COBOL",
      "year": 1959,
      "description": "MORTGAGE CALCULATOR",
      "parameters": ["principal", "rate", "term"]
    }
  ]
}
```

### Execute Calculation
```bash
POST /api/calculate/:language
Content-Type: application/json

{
  "param1": 200000,
  "param2": 5.5,
  "param3": 30
}
```

**Response (Success):**
```json
{
  "result": 1135.58,
  "source": "COBOL_LEGACY_ENGINE",
  "language": "cobol",
  "year": 1959,
  "calculation": "MORTGAGE CALCULATOR",
  "timestamp": "2024-01-15T10:30:00.000Z"
}
```

**Response (Error):**
```json
{
  "error": "CORE DUMP DETECTED",
  "details": "Binary execution failed",
  "language": "cobol"
}
```

## 🔧 Customization Guide

### Customizing Binary Location

Override `getBinaryPath()` in your bridge:

```javascript
getBinaryPath() {
  return path.join(__dirname, 'custom', 'path', this.binary);
}
```

### Customizing Parameter Format

Override `buildCommand()` in your bridge:

```javascript
buildCommand(params) {
  const binaryPath = this.getBinaryPath();
  // Custom format with flags
  return `${binaryPath} --principal=${params.principal} --rate=${params.rate}`;
}
```

### Customizing Validation

Override `validateParams()` in your bridge:

```javascript
validateParams(params) {
  // Call parent validation first
  const baseValidation = super.validateParams(params);
  if (!baseValidation.valid) return baseValidation;
  
  // Add custom validation
  if (params.principal > 10000000) {
    return {
      valid: false,
      error: 'PRINCIPAL EXCEEDS MAXIMUM ALLOWED VALUE'
    };
  }
  
  return { valid: true, error: null };
}
```

### Customizing Timeout

Set timeout in constructor:

```javascript
constructor() {
  super({
    name: 'COBOL',
    // ... other config
    timeout: 10000  // 10 seconds
  });
}
```

## 📝 Output Format Requirements

Your legacy binary MUST output results to STDOUT in a parseable format. The default templates expect:

```
RESULT: <number>
```

Examples:
- `RESULT: 1234.56`
- `RESULT: 5678.90`

You can customize the regex pattern in `parseOutput()` to match your format:

```javascript
parseOutput(stdout) {
  // Match "TOTAL: 1234.56"
  const resultPattern = /TOTAL:\s*([\d.]+)/;
  
  // Match "OUTPUT = 1234.56"
  const resultPattern = /OUTPUT\s*=\s*([\d.]+)/;
  
  // Match just a number on its own line
  const resultPattern = /^([\d.]+)$/m;
}
```

## 🧪 Testing Your Bridge

Create a simple test file:

```javascript
// test-bridge.js
const CobolBridge = require('./cobol-bridge');

async function test() {
  const bridge = new CobolBridge();
  
  try {
    const result = await bridge.execute({
      principal: 200000,
      rate: 5.5,
      term: 30
    });
    
    console.log('✅ SUCCESS:', result);
  } catch (error) {
    console.error('❌ FAILED:', error);
  }
}

test();
```

Run: `node test-bridge.js`

## 🐛 Troubleshooting

### "Binary not found" error
- Check that `getBinaryPath()` returns the correct path
- Verify the binary has execute permissions: `chmod +x your-binary`
- Test the binary directly: `./your-binary arg1 arg2`

### "Parsing failed" error
- Check your binary's actual output: `./your-binary arg1 arg2`
- Update the regex in `parseOutput()` to match the actual format
- Add console.log in `parseOutput()` to debug: `console.log('RAW OUTPUT:', stdout)`

### "Timeout" error
- Increase timeout in constructor: `timeout: 10000`
- Check if binary is hanging or waiting for input
- Verify binary completes quickly when run directly

### "Invalid input" error
- Check parameter names match between API request and bridge config
- Verify all parameters are numbers
- Verify all parameters are positive (> 0)

## 📚 Examples

See the main project for complete working examples:
- COBOL mortgage calculator
- FORTRAN trajectory calculator
- PASCAL tax calculator
- BASIC interest calculator

## 🤝 Contributing

These templates are part of the @necro-bridge/core toolkit. To contribute:
1. Test your changes with multiple legacy languages
2. Maintain backward compatibility
3. Update this README with new features
4. Follow the vintage computing aesthetic 👻

## 📄 License

MIT License - Resurrect freely!
