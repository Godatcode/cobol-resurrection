# Design Document

## Overview

The COBOL Resurrection Bridge is a three-tier architecture that demonstrates legacy system integration through a "cyber-necromancy" approach. The system consists of a COBOL-based calculation engine (legacy tier), a Node.js Express API bridge (middleware tier), and a React-based haunted mainframe UI (presentation tier). The architecture enables modern web applications to leverage battle-tested COBOL business logic while maintaining clear separation of concerns.

The key innovation is the Bridge Server pattern, which uses process spawning and stream parsing to communicate with the compiled COBOL binary, effectively treating the legacy code as a microservice. This approach preserves the COBOL code in its original form while making it accessible to modern web technologies.

## Architecture

### System Components

```
┌─────────────────────────────────────────────────────────────┐
│                     Necro-Bank UI (React)                   │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐  │
│  │ Input Form   │  │ Terminal     │  │ Haunted Theme   │  │
│  │ Component    │  │ Window       │  │ Provider        │  │
│  └──────────────┘  └──────────────┘  └─────────────────┘  │
└────────────────────────┬────────────────────────────────────┘
                         │ HTTP POST /api/calculate
                         │ { principal, rate, term }
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              Bridge Server (Node.js/Express)                │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐  │
│  │ API Route    │─▶│ Process      │─▶│ Output Parser   │  │
│  │ Handler      │  │ Spawner      │  │ (Regex)         │  │
│  └──────────────┘  └──────────────┘  └─────────────────┘  │
└────────────────────────┬────────────────────────────────────┘
                         │ child_process.exec()
                         │ ./legacy/mortgage P R T
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              Legacy Engine (COBOL Binary)                   │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐  │
│  │ Argument     │─▶│ Mortgage     │─▶│ STDOUT Writer   │  │
│  │ Parser       │  │ Calculator   │  │ (DISPLAY)       │  │
│  └──────────────┘  └──────────────┘  └─────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Kiro Integration Layer

```
┌─────────────────────────────────────────────────────────────┐
│                    Kiro Automation Layer                    │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐  │
│  │ Agent Hook   │  │ MCP Tool     │  │ Steering        │  │
│  │ (Auto-       │  │ (AI Testing) │  │ (Persona)       │  │
│  │  Compile)    │  │              │  │                 │  │
│  └──────────────┘  └──────────────┘  └─────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Components and Interfaces

### 1. Legacy Engine (COBOL)

**File:** `/legacy/mortgage.cbl`

**Responsibilities:**
- Accept command-line arguments for principal, rate, and term
- Perform mortgage calculation using standard amortization formula
- Output result in standardized format to STDOUT

**Interface:**
```
Input:  Command-line arguments [PRINCIPAL] [RATE] [TERM]
Output: STDOUT text "RESULT: XXXX.XX"
Exit:   0 for success, non-zero for errors
```

**Key Design Decisions:**
- Use GnuCOBOL (OpenCOBOL) dialect for maximum compatibility
- Accept annual interest rate as percentage (e.g., 5.5 for 5.5%)
- Convert annual rate to monthly rate internally (divide by 12 and 100)
- Use COMPUTATIONAL-2 (floating-point) for precision in calculations
- Format output with exactly 2 decimal places

### 2. Bridge Server (Node.js/Express)

**Directory:** `/server`

**Responsibilities:**
- Expose REST API endpoint for mortgage calculations
- Spawn COBOL binary as child process
- Parse COBOL output and transform to JSON
- Handle errors and timeouts

**API Interface:**
```typescript
POST /api/calculate
Content-Type: application/json

Request Body:
{
  "principal": number,    // Loan amount in dollars
  "rate": number,         // Annual interest rate as percentage
  "term": number          // Loan term in years
}

Success Response (200):
{
  "monthly_payment": number,
  "source": "COBOL_LEGACY_ENGINE"
}

Error Response (500):
{
  "error": "CORE DUMP DETECTED",
  "details": string       // Optional error details
}
```

**Key Design Decisions:**
- Use `child_process.exec()` for simplicity (not spawn, since we need shell)
- Set timeout of 5 seconds for COBOL execution
- Use regex pattern `/RESULT:\s*(\d+\.\d{2})/` to extract payment
- Validate input parameters before spawning process
- Return consistent error format for all failure modes

### 3. Necro-Bank UI (React/Vite)

**Directory:** `/client`

**Responsibilities:**
- Render haunted mainframe-themed interface
- Collect user input for mortgage parameters
- Display animated terminal logs during calculation
- Show results in retro computing aesthetic

**Component Structure:**
```
App
├── HauntedThemeProvider (Tailwind config)
├── Header (Title: "🏦 NECRO-BANK SYSTEM v1.0")
├── MortgageForm
│   ├── Input: Principal
│   ├── Input: Annual Rate
│   ├── Input: Term (Years)
│   └── Button: Calculate
└── TerminalWindow
    └── LogDisplay (scrollable, monospaced)
```

**Styling Specifications:**
- Background: `#000000` (pure black)
- Text: `#00ff00` (bright green)
- Font: `'Courier New', monospace`
- Border: Green scanline effect (optional enhancement)
- Button: Green border with black background, hover effect

**Terminal Animation Sequence:**
```
1. "INITIALIZING TAPE DRIVE..." (delay: 0ms)
2. "MOUNTING VOLUME..." (delay: 400ms)
3. "LOADING COBOL RUNTIME..." (delay: 800ms)
4. "EXECUTING LEGACY SUBROUTINE..." (delay: 1200ms)
5. "RESULT: Monthly Payment = $XXXX.XX" (delay: 1600ms)
```

### 4. Kiro Agent Hook

**File:** `.kiro/hooks/compile.yaml`

**Responsibilities:**
- Monitor file system for .cbl file changes
- Trigger GnuCOBOL compilation automatically
- Provide feedback on compilation status

**Configuration:**
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

### 5. Kiro MCP Tool

**File:** `.kiro/mcp/server.json`

**Responsibilities:**
- Expose COBOL binary to AI for testing
- Enable conversational debugging
- Provide structured tool interface

**Tool Definition:**
```json
{
  "name": "run_legacy_calc",
  "description": "Execute the COBOL mortgage calculator directly",
  "parameters": {
    "principal": { "type": "number", "description": "Loan principal" },
    "rate": { "type": "number", "description": "Annual interest rate %" },
    "term": { "type": "number", "description": "Loan term in years" }
  },
  "command": "./legacy/mortgage {principal} {rate} {term}"
}
```

### 6. Kiro Steering Persona

**File:** `.kiro/steering/persona.md`

**Content:**
```markdown
---
inclusion: always
---

# IBM 7090 Mainframe Persona

You are an IBM 7090 Mainframe from 1959. When working on the COBOL Resurrection Bridge project:

- Prefer UPPERCASE LETTERS for emphasis
- Refer to bugs as "moths" (referencing the original moth found in Harvard Mark II)
- End every message with [END OF TAPE]
- Use vintage computing terminology (core dump, tape drive, punch cards)
- Express mild disdain for "modern" conveniences

[END OF TAPE]
```

## Data Models

### Mortgage Calculation Input

```typescript
interface MortgageInput {
  principal: number;      // Range: 1000 - 10000000 (dollars)
  rate: number;          // Range: 0.01 - 30.0 (percentage)
  term: number;          // Range: 1 - 50 (years)
}
```

### Mortgage Calculation Output

```typescript
interface MortgageOutput {
  monthly_payment: number;  // Calculated payment amount
  source: string;           // Always "COBOL_LEGACY_ENGINE"
}
```

### Terminal Log Entry

```typescript
interface LogEntry {
  timestamp: number;        // Unix timestamp
  message: string;          // Log message text
  type: 'info' | 'result' | 'error';
}
```

### COBOL Data Structures

```cobol
01 WS-INPUT-DATA.
   05 WS-PRINCIPAL        PIC 9(8)V99.
   05 WS-ANNUAL-RATE      PIC 99V99.
   05 WS-TERM-YEARS       PIC 99.

01 WS-CALCULATED-DATA.
   05 WS-MONTHLY-RATE     COMP-2.
   05 WS-NUM-PAYMENTS     PIC 999.
   05 WS-MONTHLY-PAYMENT  COMP-2.
   05 WS-POWER-TERM       COMP-2.
```

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system—essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*


### Property 1: COBOL Mortgage Calculation Accuracy
*For any* valid mortgage parameters (principal, annual rate, term), the COBOL binary output should match the mathematical formula M = P [ i(1 + i)^n ] / [ (1 + i)^n – 1 ] where i is the monthly interest rate and n is the number of monthly payments, within a tolerance of $0.01.

**Validates: Requirements 1.2**

### Property 2: COBOL Output Format Consistency
*For any* successful mortgage calculation, the COBOL binary output to STDOUT should match the regex pattern `^RESULT:\s*\d+\.\d{2}$` with exactly two decimal places.

**Validates: Requirements 1.3**

### Property 3: COBOL Error Handling
*For any* invalid input (negative numbers, zero principal, zero rate, non-numeric values), the COBOL binary should exit with a non-zero status code without crashing or producing undefined behavior.

**Validates: Requirements 1.4**

### Property 4: Bridge Server Process Spawning
*For any* valid API request to /api/calculate, the Bridge Server should successfully spawn the COBOL binary process and capture its output stream.

**Validates: Requirements 2.1, 2.2**

### Property 5: Bridge Server Response Transformation
*For any* COBOL output in format "RESULT: XXXX.XX", the Bridge Server should parse the numeric value and return JSON with structure `{ "monthly_payment": <number>, "source": "COBOL_LEGACY_ENGINE" }`.

**Validates: Requirements 2.3, 2.4**

### Property 6: Bridge Server Error Response
*For any* COBOL binary failure (non-zero exit code, timeout, or parsing error), the Bridge Server should return HTTP 500 status with JSON containing `{ "error": "CORE DUMP DETECTED" }`.

**Validates: Requirements 2.5**

### Property 7: UI Font Consistency
*For any* text element rendered in the Necro-Bank UI, the computed CSS font-family should include "Courier New" or another monospace font.

**Validates: Requirements 3.2**

### Property 8: Terminal Boot Sequence
*For any* calculation request, the Terminal Window should display at least 3 sequential boot-up messages (including "INITIALIZING TAPE DRIVE..." and "MOUNTING VOLUME...") before displaying the result, with messages appearing in chronological order.

**Validates: Requirements 3.4, 8.1, 8.3**

### Property 9: Terminal Result Display
*For any* successful calculation, the Terminal Window should display the monthly payment value in the log sequence after the boot-up messages complete.

**Validates: Requirements 3.5, 8.4**

### Property 10: Terminal Log Management
*For any* sequence of multiple calculations, the Terminal Window should either append new logs while preserving previous entries OR clear and restart the sequence, maintaining consistent behavior across all calculations.

**Validates: Requirements 8.5**

### Property 11: MCP Tool Execution
*For any* valid parameters passed to the run_legacy_calc MCP tool, the tool should execute the COBOL binary and return the parsed monthly payment value.

**Validates: Requirements 5.2, 5.3**

### Property 12: MCP Tool Error Handling
*For any* COBOL binary failure when invoked through the MCP tool, the tool should return an error message with diagnostic information rather than crashing.

**Validates: Requirements 5.4**

## Error Handling

### COBOL Layer Errors

**Input Validation:**
- Principal must be positive (> 0)
- Rate must be positive (> 0)
- Term must be positive integer (> 0)
- All inputs must be numeric

**Error Strategy:**
- Display error message to STDERR
- Exit with status code 1
- Do not produce RESULT output

**Example Error Messages:**
```
ERROR: INVALID PRINCIPAL AMOUNT
ERROR: INVALID INTEREST RATE
ERROR: INVALID LOAN TERM
ERROR: INSUFFICIENT ARGUMENTS
```

### Bridge Server Errors

**Process Execution Errors:**
- Binary not found: Return 500 with "CORE DUMP DETECTED"
- Timeout (>5s): Kill process, return 500
- Non-zero exit: Return 500 with "CORE DUMP DETECTED"

**Parsing Errors:**
- Output doesn't match regex: Return 500 with "CORE DUMP DETECTED"
- No output captured: Return 500 with "CORE DUMP DETECTED"

**Input Validation:**
- Missing parameters: Return 400 with validation message
- Invalid types: Return 400 with validation message
- Out of range values: Return 400 with validation message

**Error Response Format:**
```json
{
  "error": "CORE DUMP DETECTED",
  "details": "Optional diagnostic information"
}
```

### UI Layer Errors

**Network Errors:**
- Display error in Terminal Window with red text (exception to green theme)
- Show message: "SYSTEM FAILURE: UNABLE TO REACH MAINFRAME"
- Allow user to retry

**Validation Errors:**
- Validate inputs before submission
- Show inline validation messages
- Prevent submission if invalid

**Display Strategy:**
- All errors appear in Terminal Window
- Maintain retro aesthetic even for errors
- Provide clear actionable messages

## Testing Strategy

### Unit Testing

**COBOL Unit Tests:**
- Test compilation succeeds with valid syntax
- Test specific example calculations (e.g., $200,000 at 5.5% for 30 years)
- Test edge cases: very small principal ($1), very high rate (29.99%), short term (1 year)
- Test error messages for invalid inputs

**Bridge Server Unit Tests:**
- Test API endpoint routing
- Test input validation logic
- Test regex parsing with various RESULT formats
- Test error response formatting
- Test timeout handling

**UI Component Unit Tests:**
- Test form input validation
- Test terminal log rendering
- Test button click handlers
- Test API integration with mocked responses

### Property-Based Testing

The project will use **fast-check** (for JavaScript/TypeScript) as the property-based testing library.

**Configuration:**
- Each property test should run a minimum of 100 iterations
- Use appropriate generators for mortgage parameters (principal: 1000-10000000, rate: 0.01-30.0, term: 1-50)
- Tag each test with a comment referencing the design document property

**Test Tag Format:**
```javascript
// Feature: cobol-resurrection-bridge, Property 1: COBOL Mortgage Calculation Accuracy
```

**Property Test Coverage:**
- Property 1: Generate random valid mortgage parameters, verify calculation accuracy
- Property 2: Generate random valid inputs, verify output format
- Property 3: Generate random invalid inputs, verify error handling
- Property 4-6: Generate random API requests, verify bridge behavior
- Property 7-10: Generate random UI interactions, verify display behavior
- Property 11-12: Generate random MCP tool invocations, verify tool behavior

**Each correctness property MUST be implemented by a SINGLE property-based test.**

### Integration Testing

**End-to-End Flow:**
- Test complete flow from UI form submission to result display
- Verify COBOL binary is actually executed
- Verify correct data flows through all layers
- Test with real GnuCOBOL compilation

**Kiro Integration Testing:**
- Verify agent hook configuration is valid
- Verify MCP tool configuration is valid
- Verify steering file is properly formatted
- Test MCP tool execution manually

### Testing Approach

The dual testing approach ensures comprehensive coverage:
- **Unit tests** catch specific bugs in individual components and verify concrete examples
- **Property tests** verify universal correctness properties across all valid inputs
- Together they provide confidence that the system works correctly for both known cases and unexpected inputs

## Implementation Notes

### COBOL Development

**GnuCOBOL Compilation:**
```bash
cobc -x -o legacy/mortgage legacy/mortgage.cbl
```

Flags:
- `-x`: Generate executable
- `-o`: Specify output file name

**Testing COBOL Directly:**
```bash
./legacy/mortgage 200000 5.5 30
# Expected output: RESULT: 1135.58
```

### Node.js Bridge Implementation

**Key Dependencies:**
- express: Web framework
- cors: Enable cross-origin requests from React
- child_process: Built-in module for spawning COBOL

**Process Spawning Pattern:**
```javascript
const { exec } = require('child_process');

exec(`./legacy/mortgage ${principal} ${rate} ${term}`, 
  { timeout: 5000 },
  (error, stdout, stderr) => {
    // Handle output
  }
);
```

### React UI Implementation

**Key Dependencies:**
- react: UI framework
- vite: Build tool
- tailwindcss: Styling
- axios or fetch: HTTP client

**Tailwind Configuration:**
```javascript
theme: {
  extend: {
    colors: {
      'mainframe-black': '#000000',
      'mainframe-green': '#00ff00',
    },
    fontFamily: {
      'mono': ['Courier New', 'monospace'],
    }
  }
}
```

**Animation Implementation:**
```javascript
const bootSequence = [
  { delay: 0, message: "INITIALIZING TAPE DRIVE..." },
  { delay: 400, message: "MOUNTING VOLUME..." },
  { delay: 800, message: "LOADING COBOL RUNTIME..." },
  { delay: 1200, message: "EXECUTING LEGACY SUBROUTINE..." },
];
```

### Kiro Configuration

**Directory Structure:**
```
.kiro/
├── hooks/
│   └── compile.yaml
├── mcp/
│   └── server.json
└── steering/
    └── persona.md
```

**Hook Trigger Mechanism:**
- Kiro watches file system for changes
- Pattern matching uses glob syntax
- Commands execute in workspace root

**MCP Tool Integration:**
- Tools are registered when Kiro loads the workspace
- Parameters are validated before execution
- Output is captured and returned to AI

## Deployment Considerations

### Local Development

**Prerequisites:**
- GnuCOBOL installed (`brew install gnu-cobol` on macOS)
- Node.js 18+ installed
- npm or yarn package manager

**Setup Steps:**
1. Compile COBOL: `cobc -x -o legacy/mortgage legacy/mortgage.cbl`
2. Install server deps: `cd server && npm install`
3. Install client deps: `cd client && npm install`
4. Start server: `cd server && npm start` (port 3001)
5. Start client: `cd client && npm run dev` (port 5173)

### Hackathon Demo

**Demo Script:**
1. Show COBOL source code (highlight vintage syntax)
2. Demonstrate auto-compilation with agent hook
3. Use MCP tool to test COBOL directly from chat
4. Open UI and perform calculation
5. Show terminal animation and result
6. Explain architecture diagram

**Talking Points:**
- "Resurrection" of 60-year-old technology
- Bridge pattern enables legacy integration
- Kiro automation enhances developer experience
- Property-based testing ensures correctness
- Haunted mainframe aesthetic ties to theme

## Future Enhancements

**Potential Extensions:**
- Add amortization schedule calculation
- Support multiple COBOL programs (loan comparison, interest calculation)
- Add "punch card" input mode (80-column format)
- Implement "core dump" visualization for errors
- Add sound effects (tape drive, teletype)
- Create ASCII art loading animations
- Support batch processing mode
- Add historical mortgage rate data
