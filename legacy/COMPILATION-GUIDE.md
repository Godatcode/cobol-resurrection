# Legacy Language Compilation Guide

## Overview

This directory contains authentic vintage programs from four different eras of computing history:

- **COBOL (1959)** - Mortgage Calculator
- **FORTRAN (1957)** - Ballistic Trajectory Calculator  
- **PASCAL (1970)** - Progressive Tax Calculator
- **BASIC (1983)** - Compound Interest Calculator

## Quick Start

### Compile All Languages

From the project root directory:

```bash
./compile-all.sh
```

This master script will:
1. Detect which compilers are installed on your system
2. Compile all available legacy programs
3. Test each compiled binary
4. Provide a summary report

### Compile Individual Languages

Each language has its own compilation script:

```bash
# COBOL
./legacy/cobol/compile.sh

# FORTRAN
./legacy/fortran/compile.sh

# PASCAL
./legacy/pascal/compile.sh

# BASIC
./legacy/basic/compile.sh
```

## Compiler Installation

### macOS (using Homebrew)

```bash
# COBOL
brew install gnu-cobol

# FORTRAN
brew install gcc

# PASCAL
brew install fpc

# BASIC
brew install freebasic
```

### Ubuntu/Debian

```bash
# COBOL
sudo apt-get install gnucobol

# FORTRAN
sudo apt-get install gfortran

# PASCAL
sudo apt-get install fp-compiler

# BASIC
sudo apt-get install freebasic
```

### RHEL/CentOS

```bash
# COBOL
sudo yum install gnucobol

# FORTRAN
sudo yum install gcc-gfortran

# PASCAL
sudo yum install fpc

# BASIC
sudo yum install freebasic
```

## Testing Compiled Binaries

### COBOL - Mortgage Calculator

```bash
./legacy/cobol/mortgage 200000 5.5 30
# Expected output: RESULT:    1135.57
```

**Parameters:**
- Principal: $200,000
- Annual Interest Rate: 5.5%
- Loan Term: 30 years

### FORTRAN - Trajectory Calculator

```bash
./legacy/fortran/trajectory 100 45 9.8
# Expected output: RESULT:    1020.41
```

**Parameters:**
- Initial Velocity: 100 m/s
- Launch Angle: 45 degrees
- Gravity: 9.8 m/s²

### PASCAL - Tax Calculator

```bash
./legacy/pascal/tax 50000 25 10000
# Expected output: RESULT: 10000.00
```

**Parameters:**
- Income: $50,000
- Tax Rate: 25%
- Bracket Threshold: $10,000

### BASIC - Interest Calculator

```bash
echo -e "1000\n5\n12\n10" | ./legacy/basic/interest
# Expected output: RESULT:    1647.01
```

**Parameters (interactive input):**
- Principal: $1,000
- Annual Rate: 5%
- Compounds Per Year: 12
- Years: 10

## Compilation Details

### COBOL

**Compiler:** GnuCOBOL (cobc)  
**Command:** `cobc -x -o legacy/cobol/mortgage legacy/cobol/mortgage.cbl`  
**Flags:**
- `-x` - Generate executable binary
- `-o` - Specify output file name

**Features:**
- Authentic 1959 COBOL syntax
- IDENTIFICATION, ENVIRONMENT, DATA, and PROCEDURE divisions
- COMPUTATIONAL-2 floating-point precision
- Command-line argument parsing
- Comprehensive input validation

### FORTRAN

**Compiler:** GNU Fortran (gfortran)  
**Command:** `gfortran -std=legacy -o legacy/fortran/trajectory legacy/fortran/trajectory.f`  
**Flags:**
- `-std=legacy` - Support vintage FORTRAN-IV syntax
- `-o` - Specify output file name

**Features:**
- Fixed-format source (columns 1-72)
- Authentic 1957 FORTRAN-IV syntax
- IMPLICIT NONE for type safety
- Classical ballistic trajectory formula
- Error handling with IOSTAT

### PASCAL

**Compiler:** Free Pascal Compiler (fpc)  
**Command:** `fpc -FElegacy/pascal legacy/pascal/tax.pas`  
**Flags:**
- `-FE` - Set executable output directory

**Features:**
- Authentic 1970 Pascal syntax
- Structured programming with BEGIN/END blocks
- Progressive tax bracket calculation
- Procedures and functions
- Comprehensive error handling

### BASIC

**Compiler:** FreeBASIC (fbc)  
**Command:** `fbc -lang deprecated legacy/basic/interest.bas -x legacy/basic/interest`  
**Flags:**
- `-lang deprecated` - Support vintage BASIC syntax
- `-x` - Specify output executable name

**Features:**
- Authentic 1983 line-numbered BASIC
- GOTO statements for vintage authenticity
- Subroutine structure with GOSUB/RETURN
- Interactive INPUT statements
- Compound interest formula

## Troubleshooting

### Compiler Not Found

If you see "compiler not found" errors, install the required compiler using the installation commands above.

### Permission Denied

Make scripts executable:

```bash
chmod +x compile-all.sh
chmod +x legacy/*/compile.sh
```

### Compilation Errors

1. Verify source files are present in their respective directories
2. Check compiler version compatibility
3. Review error messages for syntax issues
4. Ensure all dependencies are installed

### Binary Execution Errors

1. Verify binary was compiled successfully
2. Check file permissions (should be executable)
3. Validate input parameters match expected format
4. Review error messages on STDERR

## Architecture

```
legacy/
├── cobol/
│   ├── mortgage.cbl      # Source code
│   ├── mortgage          # Compiled binary
│   └── compile.sh        # Compilation script
├── fortran/
│   ├── trajectory.f      # Source code
│   ├── trajectory        # Compiled binary
│   └── compile.sh        # Compilation script
├── pascal/
│   ├── tax.pas           # Source code
│   ├── tax               # Compiled binary
│   └── compile.sh        # Compilation script
└── basic/
    ├── interest.bas      # Source code
    ├── interest          # Compiled binary
    └── compile.sh        # Compilation script
```

## Universal Output Format

All legacy programs follow a standardized output format for bridge server integration:

```
RESULT: <numeric_value>
```

This universal format enables the Node.js Bridge Server to parse outputs consistently across all legacy languages.

## Integration with Bridge Server

The compiled binaries are designed to be spawned as child processes by the Node.js Bridge Server. The server:

1. Spawns the appropriate binary based on calculation type
2. Passes parameters as command-line arguments
3. Captures STDOUT for result parsing
4. Captures STDERR for error handling
5. Returns JSON responses to the React UI

## Historical Authenticity

Each program maintains era-appropriate syntax and conventions:

- **COBOL**: Column-based formatting, verbose division structure
- **FORTRAN**: Fixed-format columns, uppercase keywords
- **PASCAL**: Structured programming, explicit BEGIN/END
- **BASIC**: Line numbers, GOTO statements, interactive INPUT

This authenticity demonstrates the "cyber-necromancy" theme of resurrecting genuine vintage computing practices.

---

**[END OF TAPE]**
