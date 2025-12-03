# 🚀 Quick Start Guide

Get your legacy binary wrapped with a REST API in 5 minutes!

## Prerequisites

- Node.js 14+ installed
- A compiled legacy binary (COBOL, FORTRAN, PASCAL, or BASIC)
- Basic knowledge of your binary's input/output format

## Step-by-Step Setup

### 1️⃣ Create Project Directory

```bash
mkdir my-legacy-bridge
cd my-legacy-bridge
```

### 2️⃣ Copy Template Files

Copy these files from the templates directory:
- `LegacyBridge.js` (required)
- `cobol-bridge.js` (or your language)
- `universal-server.js` (optional, for multi-language)
- `package.json.template` → rename to `package.json`

### 3️⃣ Install Dependencies

```bash
npm install
```

### 4️⃣ Place Your Binary

Create a directory structure:
```
my-legacy-bridge/
├── legacy/
│   └── cobol/
│       └── your-binary
├── LegacyBridge.js
├── cobol-bridge.js
└── package.json
```

### 5️⃣ Configure Your Bridge

Edit `cobol-bridge.js`:

```javascript
constructor() {
  super({
    name: 'COBOL',
    year: 1959,
    binary: 'your-binary',  // ⚠️ CHANGE THIS
    description: 'YOUR CALCULATION',
    params: ['param1', 'param2']  // ⚠️ CHANGE THIS
  });
}
```

### 6️⃣ Test Your Binary

First, test your binary directly:

```bash
./legacy/cobol/your-binary 100 5
```

Note the output format. Example: `RESULT: 123.45`

### 7️⃣ Configure Output Parser

Update `parseOutput()` in your bridge to match your binary's output:

```javascript
parseOutput(stdout) {
  // If your output is "RESULT: 123.45"
  const resultPattern = /RESULT:\s*([\d.]+)/;
  
  // If your output is "TOTAL: 123.45"
  // const resultPattern = /TOTAL:\s*([\d.]+)/;
  
  // If your output is just "123.45"
  // const resultPattern = /^([\d.]+)$/m;
  
  const match = stdout.match(resultPattern);
  
  if (!match) {
    return {
      success: false,
      result: null,
      error: 'OUTPUT DOES NOT MATCH EXPECTED FORMAT'
    };
  }
  
  return {
    success: true,
    result: parseFloat(match[1]),
    error: null
  };
}
```

### 8️⃣ Start the Server

**Option A: Standalone Bridge Server**

Uncomment the server code at the bottom of your bridge file, then:

```bash
node cobol-bridge.js
```

**Option B: Universal Multi-Language Server**

Edit `universal-server.js` to register your bridge:

```javascript
const CobolBridge = require('./cobol-bridge');

const bridges = {
  cobol: new CobolBridge()
};
```

Then start:

```bash
npm start
```

### 9️⃣ Test the API

```bash
# Health check
curl http://localhost:3001/api/health

# Execute calculation
curl -X POST http://localhost:3001/api/calculate/cobol \
  -H "Content-Type: application/json" \
  -d '{"param1": 100, "param2": 5}'
```

### 🔟 Success! 🎉

You should see a response like:

```json
{
  "result": 123.45,
  "source": "COBOL_LEGACY_ENGINE",
  "language": "cobol",
  "year": 1959,
  "calculation": "YOUR CALCULATION",
  "timestamp": "2024-01-15T10:30:00.000Z"
}
```

## Common Issues

### ❌ "Binary not found"

**Solution:** Check your binary path in `getBinaryPath()`:

```javascript
getBinaryPath() {
  return path.join(__dirname, 'legacy', 'cobol', this.binary);
}
```

### ❌ "Parsing failed"

**Solution:** Your regex doesn't match the output. Debug it:

```javascript
parseOutput(stdout) {
  console.log('RAW OUTPUT:', stdout);  // Add this line
  const resultPattern = /RESULT:\s*([\d.]+)/;
  // ... rest of code
}
```

### ❌ "Permission denied"

**Solution:** Make your binary executable:

```bash
chmod +x legacy/cobol/your-binary
```

### ❌ "Timeout"

**Solution:** Increase timeout in constructor:

```javascript
constructor() {
  super({
    // ... other config
    timeout: 10000  // 10 seconds instead of 5
  });
}
```

## Next Steps

- ✅ Add more languages (copy and customize other bridge templates)
- ✅ Add input validation for your specific use case
- ✅ Deploy to production (Heroku, Railway, Fly.io)
- ✅ Add authentication/authorization
- ✅ Add rate limiting
- ✅ Add logging and monitoring
- ✅ Create a frontend UI

## Need Help?

- Check the full README.md for detailed documentation
- Review the example implementations in the main project
- Test your binary directly first before wrapping it

## 🎯 Pro Tips

1. **Always test your binary directly first** - Make sure it works standalone
2. **Start simple** - Get one language working before adding more
3. **Log everything** - Add console.log statements while debugging
4. **Use descriptive parameter names** - Makes the API self-documenting
5. **Handle errors gracefully** - Your binary might fail, plan for it

Happy resurrecting! 👻
