# AGENT HOOKS - AUTO-COMPILATION SYSTEM

## OVERVIEW

THE NECRO-BRIDGE AUTO-COMPILATION SYSTEM USES KIRO AGENT HOOKS TO AUTOMATICALLY COMPILE LEGACY CODE WHEN FILES ARE SAVED. THIS PROVIDES SEAMLESS INTEGRATION BETWEEN CODE GENERATION AND COMPILATION.

## HOOK CONFIGURATION

**FILE:** `.kiro/hooks/compile.yaml`

**TRIGGER:** File save events matching pattern `legacy/**/*.{cbl,f,pas,bas}`

**SUPPORTED LANGUAGES:**
- COBOL (`.cbl`) - GnuCOBOL Compiler
- FORTRAN (`.f`) - GNU Fortran Compiler
- PASCAL (`.pas`) - Free Pascal Compiler
- BASIC (`.bas`) - Node.js Wrapper (No compilation needed)

## COMPILATION BEHAVIOR

### COBOL COMPILATION
```bash
cobc -x -o <basename> <file.cbl>
```
- Generates executable binary in same directory as source
- Success: "👻 Ancient Spirit Rebound to Binary"
- Failure: "🐛 MOTH DETECTED IN COBOL PUNCH CARDS"

### FORTRAN COMPILATION
```bash
gfortran -o <basename> <file.f>
```
- Generates executable binary in same directory as source
- Success: "👻 Ancient Spirit Rebound to Binary"
- Failure: "🐛 MOTH DETECTED IN FORTRAN PUNCH CARDS"

### PASCAL COMPILATION
```bash
fpc -o<basename> <file.pas>
```
- Generates executable binary in same directory as source
- Success: "👻 Ancient Spirit Rebound to Binary"
- Failure: "🐛 MOTH DETECTED IN PASCAL SOURCE"

### BASIC VALIDATION
```bash
command -v node
```
- BASIC uses Node.js wrapper, no compilation needed
- Validates Node.js interpreter is available
- Success: "👻 Ancient Spirit Bound to Node.js Wrapper"
- Failure: "🐛 NODE.JS INTERPRETER NOT FOUND"

## END-TO-END WORKFLOW

### 1. CODE GENERATION
```bash
node server/cli/summon-spirit.js generate \
  --language COBOL \
  --filename mortgage \
  --file mortgage.cbl
```

### 2. AUTOMATIC SAVE
Code generator saves file to `legacy/cobol/mortgage.cbl`

### 3. HOOK TRIGGER
Kiro detects file save event and triggers compilation hook

### 4. COMPILATION
Hook executes appropriate compiler for the language

### 5. EXECUTION
Binary is ready to execute: `./legacy/cobol/mortgage 200000 5.5 30`

## TESTING THE HOOK

### TEST COBOL COMPILATION
```bash
export KIRO_FILE_PATH="legacy/cobol/mortgage.cbl"
bash .kiro/hooks/compile.yaml
```

### TEST FORTRAN COMPILATION
```bash
export KIRO_FILE_PATH="legacy/fortran/trajectory.f"
bash .kiro/hooks/compile.yaml
```

### TEST PASCAL COMPILATION
```bash
export KIRO_FILE_PATH="legacy/pascal/tax.pas"
bash .kiro/hooks/compile.yaml
```

### TEST BASIC VALIDATION
```bash
export KIRO_FILE_PATH="legacy/basic/interest.bas"
bash .kiro/hooks/compile.yaml
```

## ERROR HANDLING

### COMPILATION ERRORS
- Compiler output is displayed to user
- Hook exits with code 1
- Error message includes "MOTH DETECTED" terminology
- User can fix code and save again to retry

### MISSING COMPILERS
- Hook will fail if required compiler is not installed
- Install instructions:
  - COBOL: `brew install gnu-cobol`
  - FORTRAN: `brew install gcc` (includes gfortran)
  - PASCAL: `brew install fpc`
  - BASIC: `brew install node` (for wrapper)

## INTEGRATION WITH CODE GENERATOR

The code generator service (`server/services/code-generator.js`) automatically:
1. Saves generated code to appropriate `legacy/<language>/` directory
2. Triggers Kiro file save event
3. Hook detects save and compiles code
4. Returns compilation status to user

This creates a seamless "SUMMON → COMPILE → EXECUTE" workflow.

## REQUIREMENTS VALIDATION

✅ **Requirement 10.4:** Agent hooks automatically compile generated code
✅ **Requirement 10.5:** Compilation success/failure notifications provided
✅ **Requirement 4.2:** Hook executes language-specific compilation commands
✅ **Requirement 4.3:** Success message displays "Ancient Spirit Rebound"
✅ **Requirement 4.4:** Compilation errors reported to user

## VINTAGE COMPUTING TERMINOLOGY

- **MOTH:** Bug or error (referencing the original moth found in Harvard Mark II)
- **ANCIENT SPIRIT:** Legacy code or binary
- **REBOUND TO BINARY:** Successfully compiled
- **PUNCH CARDS:** Source code files
- **TAPE DRIVE:** Compilation process

[END OF TAPE]
