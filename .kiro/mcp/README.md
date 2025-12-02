# NECRO-BRIDGE MCP TOOLS

## Overview

THIS DIRECTORY CONTAINS MODEL CONTEXT PROTOCOL (MCP) TOOL CONFIGURATIONS FOR THE COBOL RESURRECTION BRIDGE PROJECT. THESE TOOLS ENABLE KIRO AI TO INTERACT WITH LEGACY SYSTEMS AND GENERATE VINTAGE CODE ON DEMAND.

## Available Tools

### 1. run_legacy_calc
**EXECUTE LEGACY CALCULATIONS DIRECTLY**

INVOKES THE COBOL MORTGAGE CALCULATOR BINARY WITH SPECIFIED PARAMETERS.

**Parameters:**
- `principal` (number): LOAN PRINCIPAL AMOUNT IN DOLLARS
- `rate` (number): ANNUAL INTEREST RATE AS PERCENTAGE
- `term` (number): LOAN TERM IN YEARS

**Example Usage:**
```javascript
{
  "tool": "run_legacy_calc",
  "parameters": {
    "principal": 200000,
    "rate": 5.5,
    "term": 30
  }
}
```

**Returns:**
```json
{
  "result": 1135.58,
  "output": "RESULT: 1135.58"
}
```

---

### 2. execute_legacy_binary
**UNIVERSAL LEGACY BINARY EXECUTOR**

EXECUTES ANY LEGACY BINARY (COBOL, FORTRAN, PASCAL, BASIC) WITH CUSTOM ARGUMENTS.

**Parameters:**
- `binary` (string): PATH TO LEGACY BINARY (e.g., ./legacy/trajectory)
- `args` (string): SPACE-SEPARATED ARGUMENTS

**Example Usage:**
```javascript
{
  "tool": "execute_legacy_binary",
  "parameters": {
    "binary": "./legacy/fortran/trajectory",
    "args": "100 45"
  }
}
```

---

### 3. summon_ancient_spirit ⭐
**AI-POWERED LEGACY CODE GENERATOR**

GENERATES SYNTACTICALLY CORRECT VINTAGE CODE FROM NATURAL LANGUAGE DESCRIPTIONS. THIS IS THE KILLER FEATURE THAT DEMONSTRATES ADVANCED KIRO INTEGRATION.

**Parameters:**
- `description` (string): NATURAL LANGUAGE DESCRIPTION OF DESIRED CALCULATION
- `language` (string): TARGET LANGUAGE (COBOL, FORTRAN, PASCAL, BASIC)
- `filename` (string): OUTPUT FILENAME WITHOUT EXTENSION

**Example Usage:**
```javascript
{
  "tool": "summon_ancient_spirit",
  "parameters": {
    "description": "Calculate compound interest with principal, rate, years, and compounding frequency",
    "language": "COBOL",
    "filename": "compound"
  }
}
```

**Workflow:**
1. KIRO AI READS THE LANGUAGE-SPECIFIC TEMPLATE FROM `.kiro/mcp/prompts/`
2. AI GENERATES CODE FOLLOWING VINTAGE SYNTAX RULES
3. CODE IS SAVED TO `legacy/{language}/` DIRECTORY
4. AGENT HOOK AUTOMATICALLY TRIGGERS COMPILATION
5. COMPILED BINARY IS IMMEDIATELY AVAILABLE VIA BRIDGE SERVER

**Returns:**
```json
{
  "success": true,
  "message": "✨ Ancient Spirit Summoned: compound.cbl",
  "file_path": "legacy/cobol/compound.cbl",
  "compilation": {
    "success": true,
    "compiler": "cobc",
    "message": "👻 Ancient Spirit Rebound to Binary: compound"
  }
}
```

---

## Language Templates

EACH LEGACY LANGUAGE HAS A DETAILED TEMPLATE IN `.kiro/mcp/prompts/`:

- **cobol-template.md**: 1959 COBOL SYNTAX WITH GNUCOBOL DIALECT
- **fortran-template.md**: 1957 FORTRAN IV FIXED-FORMAT
- **pascal-template.md**: 1970 STANDARD PASCAL
- **basic-template.md**: 1983 LINE-NUMBERED BASIC

THESE TEMPLATES PROVIDE:
- SYNTAX RULES AND CONVENTIONS
- STANDARD PROGRAM STRUCTURE
- WORKING EXAMPLES
- ERROR HANDLING PATTERNS
- COMPILATION COMMANDS

---

## Code Generation Service

**Location:** `server/services/code-generator.js`

**Key Functions:**
- `saveGeneratedCode(code, language, filename)`: SAVE CODE TO FILE
- `compileCode(language, filename)`: COMPILE SAVED CODE
- `loadTemplate(language)`: LOAD LANGUAGE TEMPLATE
- `summonAncientSpirit(code, language, filename)`: COMPLETE WORKFLOW
- `validateSyntax(code, language)`: BASIC SYNTAX VALIDATION

---

## CLI Tool

**Location:** `server/cli/summon-spirit.js`

COMMAND-LINE INTERFACE FOR CODE GENERATION THAT CAN BE INVOKED BY MCP TOOLS OR MANUALLY.

**Commands:**
```bash
# GENERATE AND COMPILE CODE
node server/cli/summon-spirit.js generate --language COBOL --filename loan --file code.cbl

# DISPLAY LANGUAGE TEMPLATE
node server/cli/summon-spirit.js template --language FORTRAN

# VALIDATE CODE SYNTAX
node server/cli/summon-spirit.js validate --language PASCAL --file tax.pas

# COMPILE EXISTING CODE
node server/cli/summon-spirit.js compile --language BASIC --filename interest
```

**NPM Script:**
```bash
npm run summon -- generate --language COBOL --filename test --file test.cbl
```

---

## API Endpoints

### POST /api/generate
**GENERATE AND COMPILE LEGACY CODE**

**Request Body:**
```json
{
  "code": "IDENTIFICATION DIVISION...",
  "language": "COBOL",
  "filename": "mortgage"
}
```

**Response:**
```json
{
  "success": true,
  "message": "✨ Ancient Spirit Summoned: mortgage.cbl",
  "file_path": "legacy/cobol/mortgage.cbl",
  "compilation": {
    "success": true,
    "compiler": "cobc",
    "message": "👻 Ancient Spirit Rebound to Binary: mortgage"
  }
}
```

### GET /api/template/:language
**RETRIEVE LANGUAGE TEMPLATE**

**Response:**
```json
{
  "language": "COBOL",
  "template": "# COBOL Code Generation Template...",
  "message": "📜 TEMPLATE LOADED FOR COBOL"
}
```

---

## Integration with Kiro Hooks

THE CODE GENERATOR INTEGRATES WITH KIRO AGENT HOOKS FOR AUTOMATIC COMPILATION:

**Hook Configuration:** `.kiro/hooks/compile.yaml`

```yaml
name: legacy-auto-compile
trigger:
  type: file_save
  pattern: "**/*.{cbl,f,pas,bas}"
action:
  type: command
  command: "compile-all.sh"
  message: "👻 Ancient Spirit Rebound to Binary."
```

WHEN AI GENERATES CODE AND SAVES IT, THE HOOK AUTOMATICALLY:
1. DETECTS THE FILE SAVE EVENT
2. TRIGGERS APPROPRIATE COMPILER
3. PRODUCES EXECUTABLE BINARY
4. MAKES IT AVAILABLE VIA BRIDGE SERVER

---

## Usage Examples

### Example 1: Generate COBOL Loan Calculator
```javascript
// KIRO AI INVOKES:
summon_ancient_spirit({
  description: "Calculate monthly loan payment with principal, annual rate, and term in years",
  language: "COBOL",
  filename: "loan"
})

// RESULT: legacy/cobol/loan.cbl created and compiled
// AVAILABLE AT: POST /api/calculate/cobol with loan parameters
```

### Example 2: Generate FORTRAN Physics Calculator
```javascript
summon_ancient_spirit({
  description: "Calculate projectile range given initial velocity and launch angle",
  language: "FORTRAN",
  filename: "projectile"
})

// RESULT: legacy/fortran/projectile.f created and compiled
```

### Example 3: Generate PASCAL Tax Calculator
```javascript
summon_ancient_spirit({
  description: "Calculate progressive tax with multiple brackets",
  language: "PASCAL",
  filename: "tax"
})

// RESULT: legacy/pascal/tax.pas created and compiled
```

---

## Requirements Validation

THIS IMPLEMENTATION SATISFIES:

**Requirement 10.1:** ✅ ACCEPTS NATURAL LANGUAGE DESCRIPTIONS
**Requirement 10.2:** ✅ GENERATES SYNTACTICALLY CORRECT CODE
**Requirement 10.3:** ✅ AUTOMATICALLY SAVES TO LEGACY DIRECTORY
**Requirement 10.4:** ✅ AGENT HOOK TRIGGERS COMPILATION
**Requirement 10.5:** ✅ BRIDGE SERVER MAKES CODE AVAILABLE VIA API

---

## Technical Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    KIRO AI AGENT                        │
│  (Receives natural language description)               │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              MCP TOOL: summon_ancient_spirit            │
│  1. Load language template from prompts/               │
│  2. Generate code following syntax rules                │
│  3. Validate syntax                                     │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│           CODE GENERATOR SERVICE                        │
│  server/services/code-generator.js                      │
│  • saveGeneratedCode()                                  │
│  • compileCode()                                        │
│  • summonAncientSpirit()                                │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              FILE SYSTEM                                │
│  legacy/{language}/{filename}.{ext}                     │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              KIRO AGENT HOOK                            │
│  .kiro/hooks/compile.yaml                               │
│  Triggers on file save → Compiles code                  │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              COMPILED BINARY                            │
│  legacy/{language}/{filename}                           │
│  Available via Bridge Server API                        │
└─────────────────────────────────────────────────────────┘
```

---

## Future Enhancements

- **SYNTAX HIGHLIGHTING**: ADD SYNTAX HIGHLIGHTING FOR GENERATED CODE IN UI
- **CODE DIFF**: SHOW BEFORE/AFTER WHEN REGENERATING CODE
- **VERSION CONTROL**: TRACK GENERATED CODE VERSIONS
- **BATCH GENERATION**: GENERATE MULTIPLE PROGRAMS AT ONCE
- **INTERACTIVE REFINEMENT**: ALLOW USER TO REFINE GENERATED CODE
- **UNIT TEST GENERATION**: AUTO-GENERATE TESTS FOR GENERATED CODE

---

[END OF TAPE]
