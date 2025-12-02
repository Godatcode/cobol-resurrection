# CODE GENERATOR SERVICE

## Overview

The Code Generator Service is the AI-POWERED CORE of the NECRO-BRIDGE system. It enables AUTOMATIC GENERATION of syntactically correct VINTAGE CODE (COBOL, FORTRAN, PASCAL, BASIC) from natural language descriptions or AI-generated code.

## Features

### ✨ Few-Shot Prompting with Vintage Examples
- Comprehensive templates for each legacy language
- Authentic syntax examples from the 1950s-1980s
- Era-appropriate coding conventions
- Real-world calculation examples (mortgage, trajectory, tax, interest)

### 🔍 Syntax Validation
- Language-specific validation rules
- Checks for required structural elements
- Prevents invalid code from being saved
- Provides detailed error messages

### 💾 Automatic File Saving
- Saves generated code to appropriate legacy directory
- Creates directories if they don't exist
- Uses correct file extensions (.cbl, .f, .pas, .bas)
- Maintains proper project structure

### ⚙️ Automatic Compilation
- Compiles code immediately after saving
- Uses appropriate compiler for each language
- Returns compilation status and messages
- Integrates with agent hooks for auto-compilation

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                  CODE GENERATOR SERVICE                 │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │   Template   │  │   Syntax     │  │    File      │ │
│  │   Loader     │  │  Validator   │  │    Saver     │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
│         │                  │                  │        │
│         └──────────────────┴──────────────────┘        │
│                          │                             │
│                          ▼                             │
│              ┌──────────────────────┐                  │
│              │  summonAncientSpirit │                  │
│              │   (Main Workflow)    │                  │
│              └──────────────────────┘                  │
│                          │                             │
│                          ▼                             │
│              ┌──────────────────────┐                  │
│              │   Code Compiler      │                  │
│              └──────────────────────┘                  │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

## API Reference

### `loadTemplate(language)`
Loads the few-shot prompting template for the specified language.

**Parameters:**
- `language` (string): Target language (COBOL, FORTRAN, PASCAL, BASIC)

**Returns:**
- (string): Template content with syntax rules and examples

**Example:**
```javascript
const template = codeGenerator.loadTemplate('COBOL');
console.log(template); // Full COBOL template with examples
```

### `validateSyntax(code, language)`
Validates generated code against language-specific syntax rules.

**Parameters:**
- `code` (string): Source code to validate
- `language` (string): Target language

**Returns:**
- (object): `{ valid: boolean, errors: string[] }`

**Example:**
```javascript
const validation = codeGenerator.validateSyntax(cobolCode, 'COBOL');
if (!validation.valid) {
  console.log('Errors:', validation.errors);
}
```

### `saveGeneratedCode(code, language, filename)`
Saves generated code to the appropriate legacy directory.

**Parameters:**
- `code` (string): Source code to save
- `language` (string): Target language
- `filename` (string): Filename without extension

**Returns:**
- (Promise<object>): `{ success, filePath, language, filename }`

**Example:**
```javascript
const result = await codeGenerator.saveGeneratedCode(
  cobolCode, 
  'COBOL', 
  'mortgage-calc'
);
console.log('Saved to:', result.filePath);
```

### `compileCode(language, filename)`
Compiles the saved code using the appropriate compiler.

**Parameters:**
- `language` (string): Target language
- `filename` (string): Filename without extension

**Returns:**
- (Promise<object>): `{ success, compiler, stdout, stderr, message }`

**Example:**
```javascript
const result = await codeGenerator.compileCode('COBOL', 'mortgage-calc');
if (result.success) {
  console.log('Compilation successful!');
}
```

### `summonAncientSpirit(code, language, filename)`
Complete workflow: validate, save, and compile generated code.

**Parameters:**
- `code` (string): Generated source code
- `language` (string): Target language
- `filename` (string): Filename without extension

**Returns:**
- (Promise<object>): Complete result with saved and compiled status

**Example:**
```javascript
const result = await codeGenerator.summonAncientSpirit(
  generatedCode,
  'COBOL',
  'new-calculator'
);

if (result.success) {
  console.log('✨ Ancient Spirit Summoned!');
  console.log('File:', result.saved.filePath);
  console.log('Compiled:', result.compiled.success);
}
```

## Language Configuration

### COBOL (1959)
- **Extension:** `.cbl`
- **Directory:** `legacy/cobol/`
- **Compiler:** `cobc` (GnuCOBOL)
- **Compile Command:** `cobc -x -o legacy/cobol/{filename} legacy/cobol/{filename}.cbl`

### FORTRAN (1957)
- **Extension:** `.f`
- **Directory:** `legacy/fortran/`
- **Compiler:** `gfortran` (GNU Fortran)
- **Compile Command:** `gfortran -o legacy/fortran/{filename} legacy/fortran/{filename}.f`

### PASCAL (1970)
- **Extension:** `.pas`
- **Directory:** `legacy/pascal/`
- **Compiler:** `fpc` (Free Pascal Compiler)
- **Compile Command:** `fpc -olegacy/pascal/{filename} legacy/pascal/{filename}.pas`

### BASIC (1983)
- **Extension:** `.bas`
- **Directory:** `legacy/basic/`
- **Compiler:** `fbc` (FreeBASIC)
- **Compile Command:** `fbc -o legacy/basic/{filename} legacy/basic/{filename}.bas`

## REST API Integration

### POST /api/generate
Generates, saves, and compiles legacy code.

**Request Body:**
```json
{
  "code": "       IDENTIFICATION DIVISION...",
  "language": "COBOL",
  "filename": "my-calculator"
}
```

**Success Response (200):**
```json
{
  "success": true,
  "message": "✨ Ancient Spirit Summoned: my-calculator.cbl",
  "file_path": "/path/to/legacy/cobol/my-calculator.cbl",
  "language": "COBOL",
  "filename": "my-calculator",
  "compilation": {
    "success": true,
    "compiler": "cobc",
    "message": "👻 Ancient Spirit Rebound to Binary: my-calculator"
  }
}
```

**Error Response (400/500):**
```json
{
  "error": "SYNTAX VALIDATION FAILED",
  "details": ["Missing IDENTIFICATION DIVISION"],
  "message": "⚠️ GENERATED CODE CONTAINS SYNTAX ERRORS"
}
```

### GET /api/template/:language
Retrieves the template for a specific language.

**Example:**
```bash
curl http://localhost:3001/api/template/COBOL
```

**Response:**
```json
{
  "language": "COBOL",
  "template": "# COBOL Code Generation Template...",
  "message": "📜 TEMPLATE LOADED FOR COBOL"
}
```

## Usage Examples

### Example 1: Generate COBOL Mortgage Calculator
```javascript
const codeGenerator = require('./services/code-generator');

const cobolCode = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORTGAGE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL PIC 9(8)V99.
       01 WS-RATE PIC 99V99.
       01 WS-PAYMENT COMP-2.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           ACCEPT WS-PRINCIPAL FROM COMMAND-LINE
           ACCEPT WS-RATE FROM COMMAND-LINE
           COMPUTE WS-PAYMENT = WS-PRINCIPAL * WS-RATE / 1200
           DISPLAY "RESULT: " WS-PAYMENT
           STOP RUN.`;

const result = await codeGenerator.summonAncientSpirit(
  cobolCode,
  'COBOL',
  'mortgage'
);

console.log(result.message);
```

### Example 2: Validate Before Saving
```javascript
// Validate first
const validation = codeGenerator.validateSyntax(generatedCode, 'FORTRAN');

if (validation.valid) {
  // Save and compile
  const result = await codeGenerator.summonAncientSpirit(
    generatedCode,
    'FORTRAN',
    'trajectory'
  );
} else {
  console.log('Validation errors:', validation.errors);
}
```

### Example 3: Using Templates for AI Generation
```javascript
// Load template for AI prompting
const template = codeGenerator.loadTemplate('PASCAL');

// Use template in AI prompt
const prompt = `${template}

Generate a Pascal program that calculates compound interest.
Input: principal, rate, years
Output: RESULT: XXXX.XX`;

// AI generates code...
// Then save and compile
const result = await codeGenerator.summonAncientSpirit(
  aiGeneratedCode,
  'PASCAL',
  'interest'
);
```

## Testing

### Run Comprehensive Tests
```bash
node server/test-code-generator-full.js
```

Tests include:
- ✓ Template loading with few-shot examples
- ✓ Syntax validation for all languages
- ✓ File saving to legacy directories
- ✓ Complete workflow (save + compile)

### Run API Integration Test
```bash
# Start server first
node server/server.js

# In another terminal
node server/test-api-generation.js
```

## Integration with Kiro

### Agent Hooks
When code is saved to legacy directories, Kiro agent hooks automatically:
1. Detect file changes (*.cbl, *.f, *.pas, *.bas)
2. Trigger compilation
3. Report success/failure

### MCP Tools
The code generator integrates with Kiro MCP tools:
- `summon_ancient_spirit`: Generate code from natural language
- Templates provide few-shot examples for AI

## Requirements Validation

This implementation satisfies:

✅ **Requirement 10.2:** AI generates syntactically correct COBOL code
- Comprehensive syntax validation
- Language-specific structural checks
- Error reporting with details

✅ **Requirement 10.3:** Automatically saves generated code to legacy directory
- Creates directories if needed
- Uses correct file extensions
- Maintains project structure

✅ **Few-Shot Prompting:** Templates with vintage code examples
- 4 comprehensive templates (COBOL, FORTRAN, PASCAL, BASIC)
- Real-world calculation examples
- Era-appropriate syntax conventions

✅ **Syntax Validation:** Language-specific validation rules
- Checks for required divisions/sections
- Validates structure and format
- Provides actionable error messages

✅ **Automatic Compilation:** Compiles after saving
- Uses appropriate compiler for each language
- Returns compilation status
- Integrates with agent hooks

## Troubleshooting

### Compilation Fails
- Ensure compilers are installed (cobc, gfortran, fpc, fbc)
- Check compiler paths in system PATH
- Verify code syntax is correct

### File Not Saved
- Check directory permissions
- Ensure legacy directories exist
- Verify filename is valid

### Validation Errors
- Review language-specific requirements
- Check template for syntax examples
- Ensure all required sections are present

## [END OF TAPE]
