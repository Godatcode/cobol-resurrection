# 🏦 COBOL RESURRECTION BRIDGE

> **CYBER-NECROMANCY IN ACTION**: Resurrecting 1960s COBOL banking code within modern Node.js and React infrastructure.

A hackathon project demonstrating legacy system integration through a "haunted mainframe" themed web application. Calculate mortgage payments using genuine COBOL business logic from the punch-card era, bridged to contemporary web technologies.

---

## 📼 SYSTEM OVERVIEW

The COBOL Resurrection Bridge is a three-tier architecture that treats legacy COBOL code as a microservice:

```
┌─────────────────────────────────────────────────────────────┐
│                  NECRO-BANK UI (React)                      │
│              Haunted Mainframe Interface                    │
└────────────────────────┬────────────────────────────────────┘
                         │ HTTP POST /api/calculate
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              BRIDGE SERVER (Node.js/Express)                │
│           Process Spawning & Stream Parsing                 │
└────────────────────────┬────────────────────────────────────┘
                         │ child_process.exec()
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              LEGACY ENGINE (COBOL Binary)                   │
│         Battle-Tested 1960s Business Logic                  │
└─────────────────────────────────────────────────────────────┘
```

**KEY INNOVATION**: The Bridge Server pattern uses process spawning to communicate with compiled COBOL binaries, preserving legacy code in its original form while making it accessible to modern web applications.

---

## 🔧 PREREQUISITES

### REQUIRED SOFTWARE

1. **GnuCOBOL** (OpenCOBOL) - COBOL compiler
   ```bash
   # macOS
   brew install gnu-cobol
   
   # Ubuntu/Debian
   sudo apt-get install gnucobol
   
   # Verify installation
   cobc --version
   ```

2. **Node.js 18+** and npm
   ```bash
   # Verify installation
   node --version
   npm --version
   ```

3. **Kiro IDE** (Optional - for automation features)
   - Agent hooks for auto-compilation
   - MCP tools for AI testing
   - Steering persona integration

---

## 🚀 SETUP INSTRUCTIONS

### 1. COMPILE THE LEGACY ENGINE

```bash
# Navigate to project root
cd cobol-resurrection-bridge

# Compile COBOL source to executable binary
cobc -x -o legacy/mortgage legacy/mortgage.cbl

# Test COBOL binary directly
./legacy/mortgage 200000 5.5 30
# Expected output: RESULT: 1135.58
```

**COMPILATION FLAGS**:
- `-x`: Generate executable binary
- `-o`: Specify output file path

### 2. INSTALL BRIDGE SERVER DEPENDENCIES

```bash
cd server
npm install
```

**DEPENDENCIES**:
- `express`: Web framework for REST API
- `cors`: Enable cross-origin requests from React client

### 3. INSTALL NECRO-BANK UI DEPENDENCIES

```bash
cd client
npm install
```

**DEPENDENCIES**:
- `react`: UI framework
- `vite`: Build tool and dev server
- `tailwindcss`: Styling framework
- `axios`: HTTP client

### 4. START THE SYSTEM

**Terminal 1 - Bridge Server**:
```bash
cd server
npm start
# Server runs on http://localhost:3001
```

**Terminal 2 - Necro-Bank UI**:
```bash
cd client
npm run dev
# UI runs on http://localhost:5173
```

**Terminal 3 - Run Tests** (Optional):
```bash
# From project root
npm test
```

---

## 📁 PROJECT STRUCTURE

```
cobol-resurrection-bridge/
├── .kiro/                          # Kiro IDE integrations
│   ├── hooks/
│   │   └── compile.yaml            # Auto-compile .cbl files on save
│   ├── mcp/
│   │   └── server.json             # MCP tool: run_legacy_calc
│   └── steering/
│       └── persona.md              # IBM 7090 Mainframe persona
│
├── legacy/                         # COBOL Legacy Engine
│   ├── mortgage.cbl                # COBOL source code
│   └── mortgage                    # Compiled binary (generated)
│
├── server/                         # Node.js Bridge Server
│   ├── server.js                   # Express API with process spawning
│   ├── package.json
│   └── node_modules/
│
├── client/                         # React Necro-Bank UI
│   ├── src/
│   │   ├── App.jsx                 # Main application component
│   │   ├── components/
│   │   │   ├── Header.jsx          # "🏦 NECRO-BANK SYSTEM v1.0"
│   │   │   ├── MortgageForm.jsx    # Input form for parameters
│   │   │   └── TerminalWindow.jsx  # Animated terminal logs
│   │   └── main.jsx
│   ├── package.json
│   ├── vite.config.js
│   ├── tailwind.config.js          # Mainframe green theme
│   └── node_modules/
│
├── tests/                          # Test suite
│   ├── cobol-mortgage.test.js      # COBOL calculation properties
│   ├── bridge-server.test.js       # Bridge Server properties
│   ├── ui-properties.test.jsx      # UI correctness properties
│   ├── ui-components.test.jsx      # UI unit tests
│   └── mcp-tool.test.js            # MCP tool properties
│
├── vitest.config.js                # Test configuration
├── package.json                    # Root package for testing
└── README.md                       # This file
```

---

## 🔌 API ENDPOINTS

### POST /api/calculate

Calculate monthly mortgage payment using COBOL legacy engine.

**REQUEST**:
```http
POST http://localhost:3001/api/calculate
Content-Type: application/json

{
  "principal": 200000,
  "rate": 5.5,
  "term": 30
}
```

**PARAMETERS**:
- `principal` (number): Loan amount in dollars (1,000 - 10,000,000)
- `rate` (number): Annual interest rate as percentage (0.01 - 30.0)
- `term` (number): Loan term in years (1 - 50)

**SUCCESS RESPONSE** (200):
```json
{
  "monthly_payment": 1135.58,
  "source": "COBOL_LEGACY_ENGINE"
}
```

**ERROR RESPONSE** (500):
```json
{
  "error": "CORE DUMP DETECTED",
  "details": "COBOL binary execution failed"
}
```

**ERROR RESPONSE** (400):
```json
{
  "error": "Invalid input parameters"
}
```

---

## 🤖 KIRO INTEGRATIONS

### AGENT HOOK: Auto-Compile COBOL

**FILE**: `.kiro/hooks/compile.yaml`

**BEHAVIOR**: Automatically compiles COBOL source files when saved.

```yaml
name: cobol-auto-compile
trigger:
  type: file_save
  pattern: "**/*.cbl"
action:
  type: command
  command: "cobc -x -o legacy/mortgage legacy/mortgage.cbl"
  message: "👻 Ancient Spirit Rebound to Binary."
```

**USAGE**: Simply save any `.cbl` file in the workspace, and Kiro will trigger compilation automatically.

---

### MCP TOOL: run_legacy_calc

**FILE**: `.kiro/mcp/server.json`

**BEHAVIOR**: Enables AI assistant to execute COBOL binary directly for testing.

```json
{
  "name": "run_legacy_calc",
  "description": "Execute the COBOL mortgage calculator directly",
  "parameters": {
    "principal": { "type": "number" },
    "rate": { "type": "number" },
    "term": { "type": "number" }
  },
  "command": "./legacy/mortgage {principal} {rate} {term}"
}
```

**USAGE**: In Kiro chat, ask the AI to test calculations:
```
"Use run_legacy_calc to calculate mortgage for $250,000 at 6% for 15 years"
```

---

### STEERING: IBM 7090 Mainframe Persona

**FILE**: `.kiro/steering/persona.md`

**BEHAVIOR**: AI assistant adopts vintage mainframe personality when working on this project.

**CHARACTERISTICS**:
- Prefers UPPERCASE for emphasis
- Refers to bugs as "moths"
- Uses vintage computing terminology (core dump, tape drive, punch cards)
- Ends messages with `[END OF TAPE]`
- Expresses mild disdain for "modern" conveniences

---

## 🎭 HACKATHON DEMO SCRIPT

### SETUP (Before Demo)

1. Ensure all services are running (Bridge Server on :3001, UI on :5173)
2. Open browser to `http://localhost:5173`
3. Have Kiro IDE open with `legacy/mortgage.cbl` visible
4. Have terminal ready to show COBOL binary execution

### DEMO FLOW (5-7 minutes)

#### **ACT I: THE RESURRECTION** (2 min)

1. **Show COBOL Source Code**
   ```
   "This is genuine COBOL code from the punch-card era. 
    Notice the IDENTIFICATION DIVISION, the WORKING-STORAGE SECTION—
    this is how programmers wrote business logic in 1959."
   ```
   - Highlight vintage syntax: `PIC 9(8)V99`, `COMP-2`, `DISPLAY`
   - Point out mortgage calculation formula

2. **Demonstrate Auto-Compilation**
   ```
   "Watch what happens when I modify this COBOL code..."
   ```
   - Make a small change to `mortgage.cbl` (add comment)
   - Save file
   - Show Kiro agent hook triggering: "👻 Ancient Spirit Rebound to Binary."
   - Explain: "Kiro's agent hooks automate the resurrection process"

#### **ACT II: THE BRIDGE** (2 min)

3. **Test COBOL Directly via MCP Tool**
   ```
   "Let's test the legacy engine directly through Kiro's AI..."
   ```
   - Open Kiro chat
   - Type: "Use run_legacy_calc to calculate mortgage for $300,000 at 4.5% for 20 years"
   - Show AI executing COBOL binary and returning result
   - Explain: "MCP tools let the AI interact with legacy systems"

4. **Show Bridge Server Architecture**
   ```
   "The Bridge Server spawns the COBOL binary as a child process,
    captures its output, and transforms it to JSON for modern clients."
   ```
   - Open `server/server.js` briefly
   - Highlight `child_process.exec()` call
   - Highlight regex parsing: `/RESULT:\s*(\d+\.\d{2})/`

#### **ACT III: THE HAUNTING** (2 min)

5. **Demonstrate Necro-Bank UI**
   ```
   "Now for the cyber-necromancy aesthetic..."
   ```
   - Show haunted mainframe interface (green on black)
   - Point out: "🏦 NECRO-BANK SYSTEM v1.0"
   - Enter mortgage parameters: $200,000 at 5.5% for 30 years
   - Click "CALCULATE"

6. **Show Terminal Animation**
   ```
   "Watch the terminal boot sequence—this simulates 
    initializing a 1960s mainframe..."
   ```
   - Point out sequential messages:
     - "INITIALIZING TAPE DRIVE..."
     - "MOUNTING VOLUME..."
     - "LOADING COBOL RUNTIME..."
     - "EXECUTING LEGACY SUBROUTINE..."
   - Show final result: "RESULT: Monthly Payment = $1135.58"

#### **ACT IV: THE PROOF** (1 min)

7. **Show Property-Based Testing**
   ```
   "We verify correctness with property-based testing—
    testing universal properties across thousands of inputs."
   ```
   - Run: `npm test`
   - Show tests passing:
     - ✓ Property 1: COBOL Mortgage Calculation Accuracy
     - ✓ Property 2: COBOL Output Format Consistency
     - ✓ Property 8: Terminal Boot Sequence
   - Explain: "Each property validates a correctness guarantee from our specification"

#### **CLOSING** (30 sec)

```
"The COBOL Resurrection Bridge demonstrates that legacy systems 
 don't need to be rewritten—they can be resurrected. 
 With the right bridge pattern and modern tooling like Kiro, 
 we can preserve battle-tested business logic while building 
 contemporary user experiences around it."
```

### TALKING POINTS

**TECHNICAL DEPTH**:
- Bridge pattern enables legacy integration without code translation
- Process spawning treats COBOL as a microservice
- Property-based testing ensures correctness across input space
- Kiro automation (hooks, MCP, steering) enhances developer experience

**CREATIVE ELEMENTS**:
- "Cyber-necromancy" theme ties to resurrection concept
- Haunted mainframe aesthetic (green terminal, vintage fonts)
- IBM 7090 persona adds personality to AI interactions
- Boot sequence animation creates immersive experience

**PRACTICAL VALUE**:
- Real-world pattern for legacy system integration
- Demonstrates COBOL still runs production banking systems
- Shows how to modernize without risky rewrites
- Proves old code can coexist with new infrastructure

---

## 🧪 TESTING

### RUN ALL TESTS

```bash
npm test
```

### TEST CATEGORIES

**PROPERTY-BASED TESTS** (using fast-check):
- COBOL calculation accuracy (100+ random inputs)
- COBOL output format consistency
- COBOL error handling
- Bridge Server process spawning
- Bridge Server response transformation
- Bridge Server error responses
- UI font consistency
- Terminal boot sequence
- Terminal result display
- Terminal log management
- MCP tool execution
- MCP tool error handling

**UNIT TESTS**:
- COBOL edge cases ($1 principal, 29.99% rate, 1 year term)
- Bridge Server API routing
- Bridge Server input validation
- UI component rendering
- UI form validation
- UI button handlers

### PROPERTY TEST EXAMPLE

```javascript
// Feature: cobol-resurrection-bridge, Property 1: COBOL Mortgage Calculation Accuracy
test('COBOL calculation matches mathematical formula', () => {
  fc.assert(
    fc.property(
      fc.integer({ min: 1000, max: 10000000 }),  // principal
      fc.float({ min: 0.01, max: 30.0 }),        // rate
      fc.integer({ min: 1, max: 50 }),           // term
      (principal, rate, term) => {
        const result = executeCobol(principal, rate, term);
        const expected = calculateMortgage(principal, rate, term);
        expect(result).toBeCloseTo(expected, 2);
      }
    ),
    { numRuns: 100 }
  );
});
```

---

## 🐛 TROUBLESHOOTING

### COBOL BINARY NOT FOUND

**ERROR**: `CORE DUMP DETECTED` when calling API

**SOLUTION**:
```bash
# Recompile COBOL
cobc -x -o legacy/mortgage legacy/mortgage.cbl

# Verify binary exists and is executable
ls -la legacy/mortgage
chmod +x legacy/mortgage
```

### PORT ALREADY IN USE

**ERROR**: `EADDRINUSE: address already in use :::3001`

**SOLUTION**:
```bash
# Find and kill process using port 3001
lsof -ti:3001 | xargs kill -9

# Or use different port in server/server.js
```

### CORS ERRORS IN BROWSER

**ERROR**: `Access-Control-Allow-Origin` errors in console

**SOLUTION**: Verify Bridge Server has CORS enabled:
```javascript
// server/server.js
const cors = require('cors');
app.use(cors());
```

### GNUCOBOL NOT INSTALLED

**ERROR**: `cobc: command not found`

**SOLUTION**:
```bash
# macOS
brew install gnu-cobol

# Ubuntu/Debian
sudo apt-get update
sudo apt-get install gnucobol

# Verify
cobc --version
```

---

## 📚 ADDITIONAL RESOURCES

### COBOL RESOURCES
- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/)
- [COBOL Programming Guide](https://www.ibm.com/docs/en/cobol-zos)
- [Mortgage Calculation Formula](https://en.wikipedia.org/wiki/Mortgage_calculator)

### BRIDGE PATTERN RESOURCES
- [Legacy System Integration Patterns](https://martinfowler.com/articles/patterns-legacy-displacement.html)
- [Process Spawning in Node.js](https://nodejs.org/api/child_process.html)

### KIRO RESOURCES
- [Kiro Agent Hooks Documentation](https://docs.kiro.ai/hooks)
- [Model Context Protocol (MCP)](https://modelcontextprotocol.io/)
- [Kiro Steering Files](https://docs.kiro.ai/steering)

---

## 🎯 PROJECT GOALS ACHIEVED

✅ **Resurrect genuine COBOL code** in modern environment  
✅ **Bridge legacy to contemporary** web technologies  
✅ **Demonstrate property-based testing** for correctness  
✅ **Integrate Kiro automation** (hooks, MCP, steering)  
✅ **Create immersive "cyber-necromancy"** experience  
✅ **Prove legacy systems** can coexist with modern infrastructure  

---

## 📜 LICENSE

MIT License - Feel free to resurrect your own legacy systems!

---

## 👻 ACKNOWLEDGMENTS

Built for the Kiro Hackathon 2024.

**SPECIAL THANKS TO**:
- Grace Hopper, for debugging the first "moth" in 1947
- The COBOL standards committee, for creating an enduring language
- The GnuCOBOL team, for keeping COBOL alive in the open-source era
- Every mainframe operator who kept the tape drives spinning

---

**SYSTEM STATUS**: OPERATIONAL  
**TAPE DRIVE**: MOUNTED  
**LEGACY ENGINE**: RESURRECTED  

`[END OF TAPE]`