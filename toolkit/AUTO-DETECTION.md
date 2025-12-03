# AUTO-DETECTION SYSTEM

## OVERVIEW

THE NECRO-BRIDGE AUTO-DETECTION SYSTEM AUTOMATICALLY DISCOVERS LEGACY BINARIES AND COMPILERS IN YOUR PROJECT, ELIMINATING MANUAL CONFIGURATION.

## FEATURES

### 1. COMPILER DETECTION (PATH CHECKING)

THE SYSTEM CHECKS YOUR SYSTEM PATH FOR INSTALLED LEGACY COMPILERS:

- **COBOL**: GnuCOBOL (cobc)
- **FORTRAN**: GNU Fortran (gfortran)
- **PASCAL**: Free Pascal Compiler (fpc)
- **BASIC**: FreeBASIC (fbc)

FOR EACH COMPILER, THE SYSTEM:
- VERIFIES IT EXISTS IN PATH
- DISPLAYS INSTALLATION STATUS
- SHOWS FULL PATH TO COMPILER BINARY
- PROVIDES INSTALLATION INSTRUCTIONS IF MISSING

### 2. BINARY DISCOVERY

THE SYSTEM RECURSIVELY SCANS PROJECT DIRECTORIES TO FIND:

- **EXECUTABLE FILES**: Files with execute permissions
- **LANGUAGE DETECTION**: Identifies language based on:
  - Directory name (e.g., `/legacy/cobol/`)
  - Source file extensions (`.cbl`, `.f`, `.pas`, `.bas`)
  - Binary-to-source file matching
- **SOURCE FILE LINKING**: Associates binaries with their source code

SUPPORTED FILE EXTENSIONS:
- COBOL: `.cbl`, `.cob`
- FORTRAN: `.f`, `.f90`, `.for`
- PASCAL: `.pas`, `.pp`
- BASIC: `.bas`, `.bi`

### 3. CONFIGURATION FILE GENERATION

AUTOMATICALLY GENERATES `necro-bridge.config.json` WITH:

```json
{
  "project": {
    "name": "necro-bridge-project",
    "version": "1.0.0",
    "description": "Legacy system integration via Necro-Bridge",
    "languages": ["cobol", "fortran", "pascal", "basic"]
  },
  "binaries": [
    {
      "name": "mortgage",
      "path": "./legacy/cobol/mortgage",
      "language": "cobol",
      "endpoint": "/api/calculate/cobol/mortgage",
      "parameters": ["principal", "rate", "term"],
      "outputPattern": "RESULT:\\s*([\\d.]+)",
      "timeout": 5000
    }
  ],
  "server": {
    "port": 3001,
    "timeout": 5000,
    "cors": true,
    "logging": true
  },
  "paths": {
    "legacy": "./legacy",
    "server": "./server",
    "bridges": "./server/bridges"
  }
}
```

### 4. SETUP WIZARD (FIRST-TIME USERS)

INTERACTIVE COMMAND-LINE WIZARD THAT:

1. **ASKS QUESTIONS**:
   - Project name
   - Languages to support
   - Legacy binaries directory
   - Auto-detect existing binaries?
   - Generate example programs?

2. **CREATES STRUCTURE**:
   - Directory hierarchy
   - Language-specific subdirectories
   - Server and bridge directories
   - Kiro integration directories

3. **CHECKS COMPILERS**:
   - Verifies installed compilers
   - Provides installation instructions
   - Warns about missing compilers

4. **INITIALIZES PROJECT**:
   - Generates bridge templates
   - Creates example programs
   - Sets up configuration files
   - Creates .gitignore

5. **PROVIDES GUIDANCE**:
   - Next steps instructions
   - Compilation commands
   - Server startup commands
   - Testing commands

## USAGE

### DETECT COMMAND

AUTO-DETECT LEGACY BINARIES IN PROJECT:

```bash
necro-bridge detect
```

WITH CUSTOM DIRECTORY:

```bash
necro-bridge detect --directory ./my-legacy-code
```

### SETUP WIZARD

INTERACTIVE FIRST-TIME SETUP:

```bash
necro-bridge setup
```

FOLLOW THE PROMPTS TO CONFIGURE YOUR PROJECT.

## DETECTION ALGORITHM

### COMPILER DETECTION

```
FOR EACH COMPILER (cobc, gfortran, fpc, fbc):
  1. EXECUTE: <compiler> --version
  2. CHECK: Output contains expected string
  3. IF SUCCESS:
     - EXECUTE: which <compiler>
     - DISPLAY: Full path to compiler
  4. IF FAILURE:
     - DISPLAY: Installation instructions
```

### BINARY DISCOVERY

```
FUNCTION scanDirectory(directory):
  FOR EACH file IN directory (recursive):
    IF file.isExecutable():
      language = detectLanguage(file.directory, file.name)
      IF language IS NOT NULL:
        sourceFile = findSourceFile(file.directory, file.name)
        ADD TO binaries:
          - name: file.name
          - path: file.fullPath
          - language: language
          - sourceFile: sourceFile (if found)
```

### LANGUAGE DETECTION

```
FUNCTION detectLanguage(directory, filename):
  # METHOD 1: CHECK DIRECTORY NAME
  IF directory.basename IN ['cobol', 'fortran', 'pascal', 'basic']:
    RETURN directory.basename
  
  # METHOD 2: CHECK FOR SOURCE FILES
  files = listFiles(directory)
  IF files CONTAINS '*.cbl' OR '*.cob':
    RETURN 'cobol'
  IF files CONTAINS '*.f' OR '*.f90' OR '*.for':
    RETURN 'fortran'
  IF files CONTAINS '*.pas' OR '*.pp':
    RETURN 'pascal'
  IF files CONTAINS '*.bas' OR '*.bi':
    RETURN 'basic'
  
  # METHOD 3: MATCH BINARY TO SOURCE FILE
  baseName = filename.removeExtension()
  IF files CONTAINS baseName + '.cbl':
    RETURN 'cobol'
  # ... (similar for other languages)
  
  RETURN NULL
```

## CONFIGURATION GENERATION

THE SYSTEM GENERATES INTELLIGENT DEFAULTS:

### LANGUAGE-SPECIFIC PARAMETERS

- **COBOL**: `['principal', 'rate', 'term']` (mortgage calculation)
- **FORTRAN**: `['velocity', 'angle', 'gravity']` (trajectory calculation)
- **PASCAL**: `['income', 'deductions', 'rate']` (tax calculation)
- **BASIC**: `['principal', 'rate', 'time']` (interest calculation)

### OUTPUT PATTERNS

ALL LANGUAGES USE STANDARDIZED FORMAT:
```
RESULT: <numeric-value>
```

REGEX PATTERN: `RESULT:\s*([\d.]+)`

### API ENDPOINTS

GENERATED AS: `/api/calculate/<language>/<binary-name>`

EXAMPLES:
- `/api/calculate/cobol/mortgage`
- `/api/calculate/fortran/trajectory`
- `/api/calculate/pascal/tax`
- `/api/calculate/basic/interest`

## SETUP WIZARD WORKFLOW

```
1. WELCOME MESSAGE
   ↓
2. ASK QUESTIONS
   - Project name
   - Languages
   - Directory
   - Auto-detect?
   - Examples?
   ↓
3. CREATE DIRECTORIES
   - /legacy/<language>
   - /server/bridges
   - /.kiro/hooks
   ↓
4. CHECK COMPILERS
   - Verify installations
   - Show paths
   - Warn if missing
   ↓
5. INITIALIZE OR DETECT
   IF auto-detect:
     - Run detect command
   ELSE:
     - Run init for each language
   ↓
6. GENERATE EXAMPLES
   IF requested:
     - Create example programs
   ↓
7. CREATE CONFIGURATION
   - necro-bridge.config.json
   - .gitignore
   ↓
8. DISPLAY NEXT STEPS
   - Compilation commands
   - Installation commands
   - Server startup
   - Testing commands
```

## ERROR HANDLING

### COMPILER NOT FOUND

```
⚠️  WARNING: SOME COMPILERS ARE MISSING

INSTALLATION INSTRUCTIONS:

COBOL:
   macOS: brew install gnu-cobol | Ubuntu: apt-get install gnucobol

YOU CAN CONTINUE WITHOUT THESE COMPILERS,
BUT YOU WILL NOT BE ABLE TO COMPILE THOSE LANGUAGES.
```

### NO BINARIES DETECTED

```
⚠️  NO LEGACY BINARIES DETECTED

SUGGESTIONS:
1. Compile your legacy programs first
2. Ensure binaries are executable (chmod +x)
3. Check the directory path
```

### DIRECTORY NOT FOUND

```
❌ DIRECTORY NOT FOUND: ./legacy
   Create the directory or specify a different path with --directory
```

## INTEGRATION WITH OTHER COMMANDS

### AFTER DETECTION

```bash
# 1. REVIEW CONFIGURATION
cat necro-bridge.config.json

# 2. START SERVER
necro-bridge serve

# 3. TEST BINARIES
necro-bridge test ./legacy/cobol/mortgage
```

### AFTER SETUP

```bash
# 1. COMPILE PROGRAMS
cobc -x -o legacy/cobol/example legacy/cobol/example.cbl

# 2. INSTALL DEPENDENCIES
cd server && npm install

# 3. START SERVER
necro-bridge serve

# 4. TEST
necro-bridge test ./legacy/cobol/example
```

## REQUIREMENTS VALIDATION

THIS IMPLEMENTATION SATISFIES **REQUIREMENT 12.3**:

> "WHEN the toolkit is used THEN the system SHALL auto-detect legacy binaries in the project directory"

### ACCEPTANCE CRITERIA MET:

✅ **COMPILER DETECTION**: Checks PATH for all supported compilers
✅ **BINARY DISCOVERY**: Recursively scans project directories
✅ **CONFIGURATION GENERATION**: Creates necro-bridge.config.json
✅ **SETUP WIZARD**: Interactive first-time user experience

## EXAMPLES

### EXAMPLE 1: DETECT EXISTING PROJECT

```bash
$ necro-bridge detect

═══════════════════════════════════════════════════════
🔍 SCANNING FOR LEGACY BINARIES
═══════════════════════════════════════════════════════
📂 Directory: ./legacy

🔧 CHECKING FOR INSTALLED COMPILERS...
   ✓ COBOL    INSTALLED (/usr/local/bin/cobc)
   ✓ FORTRAN  INSTALLED (/usr/local/bin/gfortran)
   ✓ PASCAL   INSTALLED (/usr/local/bin/fpc)
   ✗ BASIC    NOT FOUND

📡 SCANNING FOR LEGACY BINARIES...
   ✓ Found: ./legacy/cobol/mortgage (COBOL)
   ✓ Found: ./legacy/fortran/trajectory (FORTRAN)
   ✓ Found: ./legacy/pascal/tax (PASCAL)

✅ FOUND 3 LEGACY BINARY(IES):

1. mortgage
   Path: ./legacy/cobol/mortgage
   Language: COBOL
   Source: ./legacy/cobol/mortgage.cbl

2. trajectory
   Path: ./legacy/fortran/trajectory
   Language: FORTRAN
   Source: ./legacy/fortran/trajectory.f

3. tax
   Path: ./legacy/pascal/tax
   Language: PASCAL
   Source: ./legacy/pascal/tax.pas

✓ Configuration saved to: necro-bridge.config.json

═══════════════════════════════════════════════════════
✅ DETECTION COMPLETE
═══════════════════════════════════════════════════════
```

### EXAMPLE 2: SETUP NEW PROJECT

```bash
$ necro-bridge setup

═══════════════════════════════════════════════════════
👻 NECRO-BRIDGE SETUP WIZARD
═══════════════════════════════════════════════════════

WELCOME TO THE LEGACY SYSTEM RESURRECTION TOOLKIT!
THIS WIZARD WILL GUIDE YOU THROUGH THE INITIAL SETUP.

📝 PROJECT NAME (default: necro-bridge-project): my-legacy-app
   → Using: my-legacy-app

🗣️  WHICH LEGACY LANGUAGES DO YOU WANT TO SUPPORT?
   (Enter comma-separated list, e.g., cobol,fortran,pascal,basic)
   Languages: cobol,fortran
   → Selected: cobol, fortran

📂 LEGACY BINARIES DIRECTORY (default: ./legacy): 
   → Using: ./legacy

🔍 AUTO-DETECT EXISTING BINARIES? (y/n, default: n): n
   → NO

📄 GENERATE EXAMPLE PROGRAMS? (y/n, default: y): y
   → YES

═══════════════════════════════════════════════════════
🔧 CONFIGURING YOUR PROJECT
═══════════════════════════════════════════════════════

📁 CREATING DIRECTORY STRUCTURE...
   ✓ Created: ./legacy
   ✓ Created: server
   ✓ Created: server/bridges
   ✓ Created: ./legacy/cobol
   ✓ Created: ./legacy/fortran

🔧 CHECKING FOR REQUIRED COMPILERS...
   ✓ GnuCOBOL            INSTALLED
   ✓ GNU Fortran         INSTALLED

🏗️  INITIALIZING LANGUAGE BRIDGES...
   [... initialization output ...]

═══════════════════════════════════════════════════════
✅ SETUP COMPLETE!
═══════════════════════════════════════════════════════
```

## TECHNICAL NOTES

### EXECUTABLE DETECTION

USES `fs.accessSync(path, fs.constants.X_OK)` TO CHECK EXECUTE PERMISSIONS.

### PATH DETECTION

USES `which <compiler>` COMMAND TO FIND COMPILER LOCATIONS.

### RECURSIVE SCANNING

TRAVERSES DIRECTORY TREE DEPTH-FIRST, CHECKING EACH FILE.

### CONFIGURATION MERGING

IF CONFIG FILE EXISTS, WIZARD WARNS BUT DOESN'T OVERWRITE.

## FUTURE ENHANCEMENTS

- DOCKER CONTAINER DETECTION
- REMOTE BINARY SUPPORT
- CUSTOM OUTPUT PATTERN DETECTION
- AUTOMATIC BRIDGE GENERATION
- COMPILER VERSION CHECKING
- DEPENDENCY RESOLUTION

[END OF TAPE]
