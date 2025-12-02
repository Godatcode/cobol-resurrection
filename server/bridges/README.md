# Universal Legacy Bridge Pattern

## Overview

THE UNIVERSAL LEGACY BRIDGE PATTERN IS A DESIGN PATTERN THAT ENABLES MODERN WEB APPLICATIONS TO COMMUNICATE WITH COMPILED LEGACY BINARIES FROM MULTIPLE PROGRAMMING ERAS (1950s-1980s). IT TREATS VINTAGE CODE AS MICROSERVICES, SPAWNING PROCESSES AND PARSING OUTPUT STREAMS TO PROVIDE A STANDARDIZED JSON API.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Express API Layer                    │
│                  (Modern JavaScript)                    │
└────────────────────────┬────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────┐
│                   Bridge Factory                        │
│              (Singleton Pattern)                        │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌────────┐ │
│  │  COBOL   │  │ FORTRAN  │  │  PASCAL  │  │  BASIC │ │
│  │  Bridge  │  │  Bridge  │  │  Bridge  │  │ Bridge │ │
│  └──────────┘  └──────────┘  └──────────┘  └────────┘ │
└────────────────────────┬────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────┐
│              Abstract LegacyBridge Class                │
│  • Parameter Validation                                 │
│  • Process Spawning (child_process.exec)               │
│  • Timeout Handling (5 seconds)                        │
│  • Error Management                                     │
└────────────────────────┬────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────┐
│              Legacy Binary Executables                  │
│  • COBOL (1959) - mortgage                             │
│  • FORTRAN (1957) - trajectory                         │
│  • PASCAL (1970) - tax                                 │
│  • BASIC (1983) - interest                             │
└─────────────────────────────────────────────────────────┘
```

## Components

### 1. LegacyBridge (Abstract Base Class)

**File:** `LegacyBridge.js`

**Responsibilities:**
- Define universal interface for all legacy bridges
- Implement common functionality (validation, spawning, error handling)
- Enforce subclass implementation of language-specific parsing

**Key Methods:**
- `validateParams(params)` - Validates input parameters
- `getBinaryPath()` - Returns path to legacy binary (overridable)
- `buildCommand(params)` - Constructs command string (overridable)
- `parseOutput(stdout)` - **ABSTRACT** - Must be implemented by subclasses
- `execute(params)` - Universal execution method (spawns process, handles errors)

**Example Usage:**
```javascript
// CANNOT INSTANTIATE DIRECTLY - ABSTRACT CLASS
const bridge = new LegacyBridge(config); // THROWS ERROR
```

### 2. Language-Specific Bridges

#### CobolBridge
**File:** `CobolBridge.js`

**Configuration:**
- Name: COBOL
- Year: 1959
- Binary: mortgage
- Parameters: [principal, rate, term]

**Output Format:** `RESULT: XXXX.XX` (exactly 2 decimal places)

**Example:**
```javascript
const bridge = new CobolBridge();
const result = await bridge.execute({ principal: 200000, rate: 5.5, term: 30 });
// { result: 1135.57, source: "COBOL_LEGACY_ENGINE", language: "cobol", year: 1959, ... }
```

#### FortranBridge
**File:** `FortranBridge.js`

**Configuration:**
- Name: FORTRAN
- Year: 1957
- Binary: trajectory
- Parameters: [velocity, angle]

**Output Format:** `RESULT: <number>` (variable decimal places)

#### PascalBridge
**File:** `PascalBridge.js`

**Configuration:**
- Name: PASCAL
- Year: 1970
- Binary: tax
- Parameters: [income]

**Output Format:** `RESULT: <number>`

#### BasicBridge
**File:** `BasicBridge.js`

**Configuration:**
- Name: BASIC
- Year: 1983
- Binary: interest
- Parameters: [principal, rate, time, compounds]

**Output Format:** `RESULT: <number>`

### 3. BridgeFactory (Singleton)

**File:** `BridgeFactory.js`

**Responsibilities:**
- Centralize bridge instantiation
- Implement singleton pattern (one instance per language)
- Provide language discovery and metadata

**Key Methods:**
- `getSupportedLanguages()` - Returns array of language IDs
- `isSupported(language)` - Checks if language is supported
- `getBridge(language)` - Returns bridge instance (creates if needed)
- `getLanguageMetadata()` - Returns metadata for all languages

**Example Usage:**
```javascript
const bridgeFactory = require('./bridges/BridgeFactory');

// GET SUPPORTED LANGUAGES
const languages = bridgeFactory.getSupportedLanguages();
// ['cobol', 'fortran', 'pascal', 'basic']

// GET BRIDGE INSTANCE
const cobolBridge = bridgeFactory.getBridge('cobol');

// EXECUTE CALCULATION
const result = await cobolBridge.execute({ principal: 200000, rate: 5.5, term: 30 });
```

## Universal Interface

### Input Format

All bridges accept parameters as JavaScript objects:

```javascript
{
  param1: number,
  param2: number,
  ...
}
```

### Output Format (Success)

```javascript
{
  result: number,           // Calculated value
  source: string,           // e.g., "COBOL_LEGACY_ENGINE"
  language: string,         // e.g., "cobol"
  year: number,            // e.g., 1959
  calculation: string      // e.g., "MORTGAGE PAYMENT CALCULATOR"
}
```

### Output Format (Error)

```javascript
{
  error: string,           // e.g., "INVALID INPUT" or "CORE DUMP DETECTED"
  details: string,         // Error description
  language: string         // Language identifier
}
```

## Error Handling

### Validation Errors
- Missing parameters → `{ error: "INVALID INPUT", details: "MISSING PARAMETERS: ..." }`
- Non-numeric parameters → `{ error: "INVALID INPUT", details: "PARAMETER 'x' MUST BE NUMERIC" }`
- Negative/zero parameters → `{ error: "INVALID INPUT", details: "PARAMETER 'x' MUST BE POSITIVE" }`

### Execution Errors
- Timeout (>5s) → `{ error: "CORE DUMP DETECTED", details: "PROCESS EXCEEDED 5 SECOND TIMEOUT" }`
- Binary not found → `{ error: "CORE DUMP DETECTED", details: <stderr> }`
- Non-zero exit code → `{ error: "CORE DUMP DETECTED", details: <stderr> }`
- Parse failure → `{ error: "CORE DUMP DETECTED", details: "FAILED TO PARSE OUTPUT" }`

## Adding New Languages

TO ADD A NEW LEGACY LANGUAGE:

1. **Create Bridge Subclass**

```javascript
// server/bridges/AdaBridge.js
const LegacyBridge = require('./LegacyBridge');

class AdaBridge extends LegacyBridge {
  constructor() {
    super({
      name: 'ADA',
      year: 1980,
      binary: 'calculator',
      description: 'MILITARY CALCULATOR',
      params: ['value1', 'value2']
    });
  }
  
  parseOutput(stdout) {
    const resultPattern = /RESULT:\s*([\d.]+)/;
    const match = stdout.match(resultPattern);
    
    if (!match) {
      return {
        success: false,
        result: null,
        error: 'ADA OUTPUT DOES NOT MATCH EXPECTED FORMAT'
      };
    }
    
    return {
      success: true,
      result: parseFloat(match[1]),
      error: null
    };
  }
}

module.exports = AdaBridge;
```

2. **Register in BridgeFactory**

```javascript
// server/bridges/BridgeFactory.js
const AdaBridge = require('./AdaBridge');

class BridgeFactory {
  constructor() {
    this.bridges = {
      cobol: CobolBridge,
      fortran: FortranBridge,
      pascal: PascalBridge,
      basic: BasicBridge,
      ada: AdaBridge  // ADD NEW BRIDGE
    };
    // ...
  }
}
```

3. **Create Legacy Binary**

Compile your Ada program and place it in `/legacy/ada/calculator`

4. **Test**

```bash
curl -X POST http://localhost:3001/api/calculate/ada \
  -H "Content-Type: application/json" \
  -d '{"value1": 10, "value2": 20}'
```

## Testing

### Unit Tests

```bash
npm test tests/bridge-pattern.test.js
```

Tests cover:
- Abstract class enforcement
- Parameter validation
- Output parsing
- Bridge factory functionality
- Singleton pattern

### Integration Tests

```bash
npm test tests/bridge-server.test.js
```

Tests cover:
- End-to-end API calls
- Error handling
- Timeout behavior
- Backward compatibility

## Benefits

1. **SEPARATION OF CONCERNS** - Legacy code remains unchanged, bridge handles integration
2. **EXTENSIBILITY** - Easy to add new languages without modifying core logic
3. **TESTABILITY** - Each component can be tested independently
4. **MAINTAINABILITY** - Clear structure makes debugging and updates straightforward
5. **REUSABILITY** - Bridge pattern can be applied to any legacy system
6. **TYPE SAFETY** - Consistent interfaces reduce integration errors

## Performance

- **Process Spawning:** ~10-50ms overhead per request
- **Timeout:** 5 seconds maximum per calculation
- **Concurrency:** Node.js handles multiple concurrent requests
- **Caching:** Bridge instances are cached (singleton pattern)

## Security Considerations

- **Input Validation:** All parameters validated before execution
- **Command Injection:** Parameters are passed as arguments, not shell commands
- **Timeout Protection:** Prevents runaway processes
- **Error Sanitization:** Sensitive paths not exposed in error messages

## Future Enhancements

- **Connection Pooling:** Reuse processes instead of spawning new ones
- **Caching:** Cache results for identical inputs
- **Metrics:** Track execution time, success rate, error frequency
- **Docker Support:** Containerize legacy compilers for portability
- **WebSocket Streaming:** Real-time progress updates for long calculations

[END OF TAPE]
