# QUICK START GUIDE: AI CODE GENERATOR

## 🚀 GETTING STARTED IN 60 SECONDS

### STEP 1: VERIFY SETUP
```bash
# CHECK THAT TEMPLATES EXIST
ls .kiro/mcp/prompts/

# SHOULD SEE:
# cobol-template.md
# fortran-template.md
# pascal-template.md
# basic-template.md
```

### STEP 2: TEST THE SERVICE
```bash
# RUN TEST SCRIPT
node server/test-code-generator.js

# EXPECTED OUTPUT:
# ✅ 4/4 templates loaded
# ✅ All tests passed
```

### STEP 3: GENERATE YOUR FIRST CODE

**OPTION A: USING KIRO AI (RECOMMENDED)**

JUST ASK KIRO:
```
"Generate a COBOL program that calculates the area of a circle given the radius"
```

KIRO WILL:
1. LOAD THE COBOL TEMPLATE
2. GENERATE THE CODE
3. SAVE IT TO `legacy/cobol/circle.cbl`
4. AUTOMATICALLY COMPILE IT
5. MAKE IT AVAILABLE VIA API

**OPTION B: USING CLI TOOL**

```bash
# CREATE A CODE FILE
cat > mycode.cbl << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIRCLE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RADIUS           PIC 9(5)V99.
       01 WS-AREA             COMP-2.
       01 WS-PI               COMP-2 VALUE 3.14159.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           ACCEPT WS-RADIUS FROM COMMAND-LINE
           COMPUTE WS-AREA = WS-PI * WS-RADIUS * WS-RADIUS
           DISPLAY "RESULT: " WS-AREA
           STOP RUN.
EOF

# GENERATE AND COMPILE
node server/cli/summon-spirit.js generate \
  --language COBOL \
  --filename circle \
  --file mycode.cbl

# EXPECTED OUTPUT:
# ✨ Ancient Spirit Summoned Successfully!
```

**OPTION C: USING API**

```bash
# START SERVER
node server/server.js

# IN ANOTHER TERMINAL, SEND REQUEST
curl -X POST http://localhost:3001/api/generate \
  -H "Content-Type: application/json" \
  -d '{
    "code": "IDENTIFICATION DIVISION...",
    "language": "COBOL",
    "filename": "test"
  }'
```

---

## 📚 LANGUAGE TEMPLATES

### VIEW A TEMPLATE
```bash
# DISPLAY COBOL TEMPLATE
node server/cli/summon-spirit.js template --language COBOL

# OR USE API
curl http://localhost:3001/api/template/COBOL
```

### TEMPLATE CONTENTS
EACH TEMPLATE INCLUDES:
- ✅ SYNTAX RULES
- ✅ STANDARD STRUCTURE
- ✅ WORKING EXAMPLES
- ✅ ERROR HANDLING
- ✅ COMPILATION COMMANDS

---

## 🎯 COMMON USE CASES

### USE CASE 1: GENERATE FINANCIAL CALCULATOR
```
DESCRIPTION: "Calculate loan amortization schedule with principal, rate, and term"
LANGUAGE: COBOL
FILENAME: amortization
```

### USE CASE 2: GENERATE PHYSICS SIMULATOR
```
DESCRIPTION: "Calculate projectile motion with initial velocity, angle, and gravity"
LANGUAGE: FORTRAN
FILENAME: projectile
```

### USE CASE 3: GENERATE TAX CALCULATOR
```
DESCRIPTION: "Calculate progressive tax with multiple brackets and deductions"
LANGUAGE: PASCAL
FILENAME: tax
```

### USE CASE 4: GENERATE INTEREST CALCULATOR
```
DESCRIPTION: "Calculate simple and compound interest with various compounding periods"
LANGUAGE: BASIC
FILENAME: interest
```

---

## 🔧 TROUBLESHOOTING

### PROBLEM: TEMPLATE NOT FOUND
```bash
# CHECK TEMPLATE EXISTS
ls .kiro/mcp/prompts/cobol-template.md

# IF MISSING, REGENERATE FROM BACKUP
# (Templates are in version control)
```

### PROBLEM: COMPILATION FAILS
```bash
# CHECK COMPILER IS INSTALLED
which cobc      # COBOL
which gfortran  # FORTRAN
which fpc       # PASCAL
which fbc       # BASIC

# INSTALL IF MISSING (macOS)
brew install gnu-cobol
brew install gcc  # includes gfortran
brew install fpc
brew install freebasic
```

### PROBLEM: SYNTAX VALIDATION FAILS
```bash
# VALIDATE CODE MANUALLY
node server/cli/summon-spirit.js validate \
  --language COBOL \
  --file mycode.cbl

# FIX REPORTED ERRORS
```

---

## 📊 MONITORING

### CHECK GENERATED FILES
```bash
# LIST ALL GENERATED CODE
ls -la legacy/cobol/
ls -la legacy/fortran/
ls -la legacy/pascal/
ls -la legacy/basic/

# VIEW GENERATED CODE
cat legacy/cobol/circle.cbl
```

### CHECK COMPILED BINARIES
```bash
# LIST COMPILED BINARIES
ls -la legacy/cobol/circle
ls -la legacy/fortran/trajectory
ls -la legacy/pascal/tax
ls -la legacy/basic/interest

# TEST BINARY DIRECTLY
./legacy/cobol/circle 5.0
# EXPECTED: RESULT: 78.54
```

---

## 🎓 ADVANCED USAGE

### BATCH GENERATION
```bash
# GENERATE MULTIPLE PROGRAMS
for calc in loan mortgage interest; do
  node server/cli/summon-spirit.js generate \
    --language COBOL \
    --filename $calc \
    --file ${calc}.cbl
done
```

### CUSTOM VALIDATION
```javascript
// IN YOUR CODE
const codeGenerator = require('./server/services/code-generator');

const validation = codeGenerator.validateSyntax(code, 'COBOL');
if (!validation.valid) {
  console.error('Errors:', validation.errors);
}
```

### PROGRAMMATIC GENERATION
```javascript
const codeGenerator = require('./server/services/code-generator');

async function generateAndCompile() {
  const code = `...`; // YOUR GENERATED CODE
  
  const result = await codeGenerator.summonAncientSpirit(
    code,
    'COBOL',
    'myprogram'
  );
  
  if (result.success) {
    console.log('Success!', result.saved.filePath);
  }
}
```

---

## 🎪 DEMO SCRIPT

FOR HACKATHON PRESENTATION:

```bash
# 1. SHOW TEMPLATE
node server/cli/summon-spirit.js template --language COBOL

# 2. GENERATE CODE (HAVE PRE-WRITTEN CODE READY)
node server/cli/summon-spirit.js generate \
  --language COBOL \
  --filename demo \
  --file demo.cbl

# 3. SHOW GENERATED FILE
cat legacy/cobol/demo.cbl

# 4. TEST BINARY
./legacy/cobol/demo 100 5.5 30

# 5. SHOW API INTEGRATION
curl http://localhost:3001/api/calculate/cobol \
  -H "Content-Type: application/json" \
  -d '{"principal": 100000, "rate": 5.5, "term": 30}'
```

---

## 📖 FURTHER READING

- **FULL DOCUMENTATION**: `.kiro/mcp/README.md`
- **IMPLEMENTATION DETAILS**: `.kiro/specs/cobol-resurrection-bridge/CODE-GENERATOR-IMPLEMENTATION.md`
- **DESIGN DOCUMENT**: `.kiro/specs/cobol-resurrection-bridge/design.md`
- **REQUIREMENTS**: `.kiro/specs/cobol-resurrection-bridge/requirements.md`

---

## 🆘 SUPPORT

IF YOU ENCOUNTER ISSUES:

1. CHECK LOGS: `server/server.js` OUTPUTS DETAILED LOGS
2. RUN TESTS: `node server/test-code-generator.js`
3. VERIFY COMPILERS: `which cobc gfortran fpc fbc`
4. CHECK FILE PERMISSIONS: `ls -la legacy/`

---

[END OF TAPE]
