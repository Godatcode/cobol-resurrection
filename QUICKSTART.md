# ⚡ QUICK START GUIDE

GET THE COBOL RESURRECTION BRIDGE RUNNING IN 5 MINUTES!

---

## 🎯 GOAL

BY THE END OF THIS GUIDE, YOU WILL HAVE:
- ✅ All legacy compilers installed
- ✅ All legacy binaries compiled
- ✅ Bridge server running
- ✅ UI running
- ✅ Successfully executed a calculation

---

## 📋 PREREQUISITES CHECK

BEFORE STARTING, VERIFY YOU HAVE:

```bash
# Check Node.js (need 18+)
node --version

# Check npm
npm --version

# Check Git
git --version
```

IF ANY ARE MISSING, INSTALL THEM FIRST:
- **Node.js**: https://nodejs.org/
- **Git**: https://git-scm.com/

---

## 🚀 5-MINUTE SETUP

### STEP 1: INSTALL LEGACY COMPILERS (2 minutes)

**MACOS**:
```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install all compilers at once
brew install gnu-cobol gcc fpc freebasic

# Verify installations
cobc --version
gfortran --version
fpc -version
fbc --version
```

**UBUNTU/DEBIAN**:
```bash
# Update package list
sudo apt-get update

# Install all compilers
sudo apt-get install -y gnucobol gfortran fp-compiler freebasic

# Verify installations
cobc --version
gfortran --version
fpc -version
fbc --version
```

**WINDOWS** (WSL recommended):
```bash
# Install WSL first: https://docs.microsoft.com/en-us/windows/wsl/install
# Then follow Ubuntu/Debian instructions above
```

### STEP 2: CLONE AND SETUP (1 minute)

```bash
# Clone repository
git clone https://github.com/YOUR-USERNAME/cobol-resurrection-bridge.git
cd cobol-resurrection-bridge

# Install root dependencies
npm install

# Install server dependencies
cd server && npm install && cd ..

# Install client dependencies
cd client && npm install && cd ..
```

### STEP 3: COMPILE LEGACY BINARIES (1 minute)

```bash
# Make compilation script executable
chmod +x compile-all.sh

# Compile all legacy languages
./compile-all.sh

# You should see:
# ✓ COBOL compiled successfully
# ✓ FORTRAN compiled successfully
# ✓ PASCAL compiled successfully
# ✓ BASIC compiled successfully
```

**TROUBLESHOOTING**:
```bash
# If compile-all.sh fails, compile individually:
cd legacy/cobol && ./compile.sh && cd ../..
cd legacy/fortran && ./compile.sh && cd ../..
cd legacy/pascal && ./compile.sh && cd ../..
cd legacy/basic && ./compile.sh && cd ../..
```

### STEP 4: START SERVERS (30 seconds)

**TERMINAL 1 - BRIDGE SERVER**:
```bash
cd server
npm start

# Wait for: "🏦 NECRO-BRIDGE SERVER OPERATIONAL ON PORT 3001"
```

**TERMINAL 2 - UI DEV SERVER**:
```bash
cd client
npm run dev

# Wait for: "Local: http://localhost:5173"
```

### STEP 5: TEST IT! (30 seconds)

**OPTION A: USE THE UI**

1. Open browser to http://localhost:5173
2. You should see: "🏦 NECRO-BANK SYSTEM v1.0"
3. Select language: COBOL
4. Enter values:
   - Principal: 200000
   - Annual Rate: 5.5
   - Term: 30
5. Click "CALCULATE"
6. Watch the boot sequence animation
7. See result: "RESULT: Monthly Payment = $1135.58"

**OPTION B: USE THE API**

```bash
# Test COBOL
curl -X POST http://localhost:3001/api/calculate/cobol \
  -H "Content-Type: application/json" \
  -d '{"principal": 200000, "rate": 5.5, "term": 30}'

# Expected: {"result":1135.58,"source":"COBOL_LEGACY_ENGINE",...}

# Test FORTRAN
curl -X POST http://localhost:3001/api/calculate/fortran \
  -H "Content-Type: application/json" \
  -d '{"velocity": 100, "angle": 45, "gravity": 9.8}'

# Expected: {"result":1020.41,"source":"FORTRAN_LEGACY_ENGINE",...}

# Test PASCAL
curl -X POST http://localhost:3001/api/calculate/pascal \
  -H "Content-Type: application/json" \
  -d '{"income": 75000, "bracket_rate": 25, "bracket_threshold": 50000}'

# Expected: {"result":6250.00,"source":"PASCAL_LEGACY_ENGINE",...}

# Test BASIC
curl -X POST http://localhost:3001/api/calculate/basic \
  -H "Content-Type: application/json" \
  -d '{"principal": 10000, "rate": 5, "time": 10, "compounds": 12}'

# Expected: {"result":16470.09,"source":"BASIC_LEGACY_ENGINE",...}
```

**OPTION C: TEST BINARIES DIRECTLY**

```bash
# COBOL
./legacy/cobol/mortgage 200000 5.5 30
# Output: RESULT:    1135.58

# FORTRAN
./legacy/fortran/trajectory 100 45 9.8
# Output: RESULT:    1020.41

# PASCAL
./legacy/pascal/tax 75000 25 50000
# Output: RESULT: 6250.00

# BASIC (requires piped input)
echo -e "10000\n5\n12\n10" | ./legacy/basic/interest
# Output: RESULT: 16470.09
```

---

## ✅ SUCCESS CHECKLIST

YOU'RE READY TO GO IF:
- [ ] All four legacy binaries compile without errors
- [ ] Bridge server starts on port 3001
- [ ] UI loads at http://localhost:5173
- [ ] At least one calculation completes successfully
- [ ] No error messages in terminal

---

## 🐛 COMMON ISSUES

### ISSUE: "cobc: command not found"

**SOLUTION**: GnuCOBOL not installed
```bash
# macOS
brew install gnu-cobol

# Ubuntu/Debian
sudo apt-get install gnucobol
```

### ISSUE: "Port 3001 already in use"

**SOLUTION**: Kill process using port 3001
```bash
# Find process
lsof -ti:3001

# Kill it
lsof -ti:3001 | xargs kill -9

# Or use different port
cd server
PORT=3002 npm start
```

### ISSUE: "CORE DUMP DETECTED" when calling API

**SOLUTION**: Binary not compiled or not executable
```bash
# Recompile
cd legacy/cobol && ./compile.sh && cd ../..

# Make executable
chmod +x legacy/cobol/mortgage

# Test directly
./legacy/cobol/mortgage 200000 5.5 30
```

### ISSUE: CORS errors in browser console

**SOLUTION**: Ensure server has CORS enabled
```javascript
// server/server.js should have:
const cors = require('cors');
app.use(cors());
```

### ISSUE: "Cannot find module" errors

**SOLUTION**: Dependencies not installed
```bash
# Root
npm install

# Server
cd server && npm install && cd ..

# Client
cd client && npm install && cd ..
```

### ISSUE: Compilation fails on Windows

**SOLUTION**: Use WSL (Windows Subsystem for Linux)
```bash
# Install WSL
wsl --install

# Open WSL terminal and follow Ubuntu instructions
```

---

## 🎓 NEXT STEPS

NOW THAT YOU'RE RUNNING, TRY:

### 1. EXPLORE THE UI
- Try all four languages
- Watch the boot sequence animation
- Check out the terminal window styling
- Look for the punch card visualizer
- Listen for sound effects (if implemented)

### 2. TEST THE API
- Use Postman or curl to test all endpoints
- Try invalid inputs to see error handling
- Check the `/api/languages` endpoint
- Test the `/api/health` endpoint

### 3. RUN THE TESTS
```bash
# Run all tests
npm test

# Run specific test file
npm test -- tests/cobol-mortgage.test.js

# Run with coverage
npm test -- --coverage
```

### 4. EXPLORE THE CODE
- Read `legacy/cobol/mortgage.cbl` - authentic 1959 COBOL!
- Check `server/bridges/CobolBridge.js` - bridge pattern
- Look at `client/src/components/TerminalWindow.jsx` - UI magic
- Review `tests/cobol-mortgage.test.js` - property-based testing

### 5. TRY KIRO FEATURES (if you have Kiro IDE)
- Open project in Kiro
- Edit `legacy/cobol/mortgage.cbl` and save
- Watch agent hook auto-compile
- Use MCP tool: "Use run_legacy_calc to calculate mortgage for $300,000 at 4.5% for 20 years"
- Notice the IBM 7090 persona in AI responses

### 6. READ THE DOCS
- [ARCHITECTURE.md](ARCHITECTURE.md) - System design
- [EXAMPLES.md](EXAMPLES.md) - Code examples for all languages
- [CONTRIBUTING.md](CONTRIBUTING.md) - How to contribute
- [API-DOCUMENTATION.md](server/API-DOCUMENTATION.md) - API reference

---

## 🎯 QUICK REFERENCE

### USEFUL COMMANDS

```bash
# Start everything
cd server && npm start &
cd client && npm run dev &

# Stop everything
pkill -f "node.*server"
pkill -f "vite"

# Recompile all
./compile-all.sh

# Run tests
npm test

# Check API health
curl http://localhost:3001/api/health

# List supported languages
curl http://localhost:3001/api/languages
```

### DIRECTORY STRUCTURE

```
cobol-resurrection-bridge/
├── legacy/           # Vintage language source code
│   ├── cobol/       # COBOL (1959)
│   ├── fortran/     # FORTRAN (1957)
│   ├── pascal/      # PASCAL (1970)
│   └── basic/       # BASIC (1983)
├── server/          # Node.js Bridge Server
│   ├── bridges/     # Language-specific bridges
│   └── server.js    # Express API
├── client/          # React UI
│   └── src/
│       └── components/  # UI components
├── tests/           # Test suite
└── toolkit/         # NPM package
```

### PORT REFERENCE

- **3001**: Bridge Server API
- **5173**: Vite Dev Server (UI)

### API ENDPOINTS

- `POST /api/calculate/cobol` - COBOL mortgage
- `POST /api/calculate/fortran` - FORTRAN trajectory
- `POST /api/calculate/pascal` - PASCAL tax
- `POST /api/calculate/basic` - BASIC interest
- `GET /api/languages` - List languages
- `GET /api/health` - Health check

---

## 🎉 YOU'RE READY!

CONGRATULATIONS! YOU'VE SUCCESSFULLY RESURRECTED FOUR VINTAGE PROGRAMMING LANGUAGES AND BRIDGED THEM TO MODERN WEB TECHNOLOGIES.

**WHAT YOU'VE ACCOMPLISHED**:
- ✅ Compiled authentic 1950s-1980s code
- ✅ Created a universal bridge pattern
- ✅ Built a modern web interface
- ✅ Integrated AI-powered development tools

**NOW GO FORTH AND RESURRECT MORE LEGACY SYSTEMS!**

---

**SYSTEM STATUS**: OPERATIONAL  
**TAPE DRIVES**: SPINNING  
**LEGACY ENGINES**: RESURRECTED  
**CYBER-NECROMANCY**: SUCCESSFUL  

`[END OF TAPE]`
