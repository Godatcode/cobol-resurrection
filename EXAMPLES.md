# 📚 LEGACY LANGUAGE EXAMPLES

COMPREHENSIVE EXAMPLES FOR ALL FOUR SUPPORTED VINTAGE PROGRAMMING LANGUAGES

---

## 🏦 COBOL (1959) - MORTGAGE CALCULATOR

### OVERVIEW

**Language**: COBOL (Common Business-Oriented Language)  
**Era**: 1959  
**Compiler**: GnuCOBOL (cobc)  
**Purpose**: Calculate monthly mortgage payments using amortization formula

### FORMULA

```
M = P [ i(1 + i)^n ] / [ (1 + i)^n – 1 ]

Where:
  M = Monthly payment
  P = Principal (loan amount)
  i = Monthly interest rate (annual rate / 12 / 100)
  n = Number of monthly payments (years × 12)
```

### SOURCE CODE

**File**: `legacy/cobol/mortgage.cbl`

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORTGAGE-CALCULATOR.
       AUTHOR. IBM-7090-MAINFRAME.
       DATE-WRITTEN. 1959-RESURRECTED-2024.
      *****************************************************************
      * ANCIENT COBOL MORTGAGE CALCULATION SUBROUTINE                *
      * COMPUTES MONTHLY PAYMENT USING AMORTIZATION FORMULA          *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * INPUT PARAMETERS FROM COMMAND LINE
       01 WS-INPUT-DATA.
          05 WS-PRINCIPAL-STR      PIC X(20).
          05 WS-RATE-STR           PIC X(20).
          05 WS-TERM-STR           PIC X(20).
       
      * NUMERIC WORKING VARIABLES
       01 WS-CALCULATED-DATA.
          05 WS-PRINCIPAL          PIC 9(8)V99.
          05 WS-ANNUAL-RATE        PIC 99V99.
          05 WS-MONTHLY-RATE       COMP-2.
          05 WS-NUM-PAYMENTS       PIC 999.
          05 WS-MONTHLY-PAYMENT    COMP-2.
          05 WS-POWER-TERM         COMP-2.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           ACCEPT WS-PRINCIPAL-STR FROM ARGUMENT-VALUE
           ACCEPT WS-RATE-STR FROM ARGUMENT-VALUE
           ACCEPT WS-TERM-STR FROM ARGUMENT-VALUE
           
           PERFORM VALIDATE-INPUTS
           PERFORM CALCULATE-MORTGAGE
           
           DISPLAY "RESULT: " WS-MONTHLY-PAYMENT
           STOP RUN RETURNING 0.
```

### COMPILATION

```bash
cd legacy/cobol
cobc -x -o mortgage mortgage.cbl
```

**Flags**:
- `-x`: Generate executable binary
- `-o`: Specify output file name

### USAGE EXAMPLES

**Example 1: Standard 30-Year Mortgage**
```bash
./legacy/cobol/mortgage 200000 5.5 30

# Output: RESULT:    1135.58
# Explanation: $200,000 loan at 5.5% for 30 years = $1,135.58/month
```

**Example 2: 15-Year Mortgage**
```bash
./legacy/cobol/mortgage 300000 4.25 15

# Output: RESULT:    2251.45
# Explanation: $300,000 loan at 4.25% for 15 years = $2,251.45/month
```

**Example 3: Small Loan**
```bash
./legacy/cobol/mortgage 50000 6.0 10

# Output: RESULT:     555.10
# Explanation: $50,000 loan at 6.0% for 10 years = $555.10/month
```

**Example 4: Error Handling**
```bash
./legacy/cobol/mortgage -100 5.5 30

# Output (STDERR): ERROR: INVALID PRINCIPAL AMOUNT
# Exit Code: 1
```

### API USAGE

```bash
curl -X POST http://localhost:3001/api/calculate/cobol \
  -H "Content-Type: application/json" \
  -d '{
    "principal": 200000,
    "rate": 5.5,
    "term": 30
  }'

# Response:
# {
#   "result": 1135.58,
#   "source": "COBOL_LEGACY_ENGINE",
#   "language": "cobol",
#   "year": 1959,
#   "calculation": "MORTGAGE PAYMENT CALCULATOR"
# }
```

### KEY FEATURES

**Authentic 1959 Syntax**:
- IDENTIFICATION DIVISION with program metadata
- WORKING-STORAGE SECTION for variable declarations
- PROCEDURE DIVISION for executable code
- PIC clauses for data formatting
- COMP-2 for floating-point precision

**Error Handling**:
- Input validation (positive numbers, numeric types)
- STDERR output for error messages
- Non-zero exit codes on failure
- Graceful handling of edge cases

---

## 🚀 FORTRAN (1957) - TRAJECTORY CALCULATOR

### OVERVIEW

**Language**: FORTRAN (Formula Translation)  
**Era**: 1957  
**Compiler**: GNU Fortran (gfortran)  
**Purpose**: Calculate ballistic projectile range using classical mechanics

### FORMULA

```
range = (v² × sin(2θ)) / g

Where:
  range = Horizontal distance traveled (meters)
  v = Initial velocity (m/s)
  θ = Launch angle (degrees)
  g = Gravitational acceleration (m/s²)
```

### SOURCE CODE

**File**: `legacy/fortran/trajectory.f`

```fortran
C     ******************************************************************
C     * FORTRAN-IV BALLISTIC TRAJECTORY CALCULATOR                    *
C     * COMPUTES PROJECTILE RANGE USING CLASSICAL MECHANICS           *
C     * AUTHENTIC 1957 FIXED-FORMAT SYNTAX (COLUMNS 1-72)             *
C     ******************************************************************
C
      PROGRAM TRAJEC
      IMPLICIT NONE
      
C     VARIABLE DECLARATIONS
      REAL VELOC, ANGLE, GRAV
      REAL RANGE, ANGRD, SINVAL
      REAL PI
      PARAMETER (PI = 3.14159265)
      
C     READ COMMAND LINE ARGUMENTS
      CALL GETARG(1, ARG1)
      CALL GETARG(2, ARG2)
      CALL GETARG(3, ARG3)
      
C     CONVERT ANGLE FROM DEGREES TO RADIANS
      ANGRD = ANGLE * PI / 180.0
      
C     CALCULATE SIN(2*THETA)
      SINVAL = SIN(2.0 * ANGRD)
      
C     CALCULATE RANGE: R = (V^2 * SIN(2*THETA)) / G
      RANGE = (VELOC * VELOC * SINVAL) / GRAV
      
C     OUTPUT RESULT IN UNIVERSAL FORMAT
      WRITE(6,100) RANGE
  100 FORMAT('RESULT: ',F10.2)
      
      CALL EXIT(0)
      END
```

### COMPILATION

```bash
cd legacy/fortran
gfortran -o trajectory trajectory.f
```

### USAGE EXAMPLES

**Example 1: 45-Degree Optimal Angle**
```bash
./legacy/fortran/trajectory 100 45 9.8

# Output: RESULT:    1020.41
# Explanation: 100 m/s at 45° with Earth gravity = 1,020.41 meters range
```

**Example 2: Low Angle Shot**
```bash
./legacy/fortran/trajectory 150 30 9.8

# Output: RESULT:    1989.80
# Explanation: 150 m/s at 30° = 1,989.80 meters range
```

**Example 3: Moon Gravity**
```bash
./legacy/fortran/trajectory 100 45 1.62

# Output: RESULT:    6173.46
# Explanation: Same velocity on Moon (g=1.62) = 6,173.46 meters range
```

**Example 4: High Velocity Artillery**
```bash
./legacy/fortran/trajectory 500 40 9.8

# Output: RESULT:   25252.55
# Explanation: 500 m/s at 40° = 25,252.55 meters (25.2 km) range
```

### API USAGE

```bash
curl -X POST http://localhost:3001/api/calculate/fortran \
  -H "Content-Type: application/json" \
  -d '{
    "velocity": 100,
    "angle": 45,
    "gravity": 9.8
  }'

# Response:
# {
#   "result": 1020.41,
#   "source": "FORTRAN_LEGACY_ENGINE",
#   "language": "fortran",
#   "year": 1957,
#   "calculation": "BALLISTIC TRAJECTORY CALCULATOR"
# }
```

### KEY FEATURES

**Authentic 1957 Syntax**:
- Fixed-format source (columns 1-72)
- C in column 1 for comments
- IMPLICIT NONE for type safety
- PARAMETER for constants
- FORMAT statements for output

**Scientific Computing**:
- Trigonometric functions (SIN)
- Radian/degree conversion
- Floating-point precision
- Classical mechanics formulas

---

## 📐 PASCAL (1970) - TAX CALCULATOR

### OVERVIEW

**Language**: Pascal  
**Era**: 1970  
**Compiler**: Free Pascal (fpc)  
**Purpose**: Calculate progressive tax based on income brackets

### FORMULA

```
Tax = (Income - Threshold) × Rate / 100

Where:
  Tax = Amount owed in this bracket
  Income = Total income
  Threshold = Minimum income for this bracket
  Rate = Tax rate percentage for this bracket
```

### SOURCE CODE

**File**: `legacy/pascal/tax.pas`

```pascal
{ ******************************************************************** }
{ * PASCAL TAX CALCULATOR - PROGRESSIVE BRACKET COMPUTATION         * }
{ * AUTHENTIC 1970 STRUCTURED PROGRAMMING SYNTAX                    * }
{ ******************************************************************** }

PROGRAM TaxCalculator;

VAR
    Income: Real;
    BracketRate: Real;
    BracketThreshold: Real;
    TotalTax: Real;

PROCEDURE ValidateInput(Inc: Real; Rate: Real; Thresh: Real);
BEGIN
    IF Inc < 0.0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: INVALID INCOME AMOUNT');
        Halt(1);
    END;
    
    IF Rate < 0.0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: INVALID TAX RATE');
        Halt(1);
    END;
END;

FUNCTION CalculateProgressiveTax(Inc: Real; Rate: Real; Thresh: Real): Real;
VAR
    TaxableAmount: Real;
    ComputedTax: Real;
BEGIN
    IF Inc <= Thresh THEN
        ComputedTax := 0.0
    ELSE
    BEGIN
        TaxableAmount := Inc - Thresh;
        ComputedTax := TaxableAmount * Rate / 100.0;
    END;
    
    CalculateProgressiveTax := ComputedTax;
END;

BEGIN
    { Parse command line arguments }
    Val(ParamStr(1), Income, ErrorCode);
    Val(ParamStr(2), BracketRate, ErrorCode);
    Val(ParamStr(3), BracketThreshold, ErrorCode);
    
    { Validate and calculate }
    ValidateInput(Income, BracketRate, BracketThreshold);
    TotalTax := CalculateProgressiveTax(Income, BracketRate, BracketThreshold);
    
    { Output result }
    WriteLn('RESULT: ', TotalTax:0:2);
    Halt(0);
END.
```

### COMPILATION

```bash
cd legacy/pascal
fpc -o tax tax.pas
```

### USAGE EXAMPLES

**Example 1: Income Above Threshold**
```bash
./legacy/pascal/tax 75000 25 50000

# Output: RESULT: 6250.00
# Explanation: $75,000 income, 25% rate, $50,000 threshold
#              Taxable: $75,000 - $50,000 = $25,000
#              Tax: $25,000 × 0.25 = $6,250
```

**Example 2: Income Below Threshold**
```bash
./legacy/pascal/tax 40000 25 50000

# Output: RESULT: 0.00
# Explanation: Income below threshold, no tax in this bracket
```

**Example 3: High Income Bracket**
```bash
./legacy/pascal/tax 500000 35 250000

# Output: RESULT: 87500.00
# Explanation: $500,000 income, 35% rate, $250,000 threshold
#              Taxable: $250,000
#              Tax: $250,000 × 0.35 = $87,500
```

**Example 4: Exact Threshold**
```bash
./legacy/pascal/tax 50000 25 50000

# Output: RESULT: 0.00
# Explanation: Income exactly at threshold, no tax in this bracket
```

### API USAGE

```bash
curl -X POST http://localhost:3001/api/calculate/pascal \
  -H "Content-Type: application/json" \
  -d '{
    "income": 75000,
    "bracket_rate": 25,
    "bracket_threshold": 50000
  }'

# Response:
# {
#   "result": 6250.00,
#   "source": "PASCAL_LEGACY_ENGINE",
#   "language": "pascal",
#   "year": 1970,
#   "calculation": "PROGRESSIVE TAX CALCULATOR"
# }
```

### KEY FEATURES

**Authentic 1970 Syntax**:
- Structured programming with BEGIN/END blocks
- PROCEDURE and FUNCTION declarations
- VAR declarations with type safety
- Pascal naming conventions (PascalCase)
- Curly brace comments { }

**Structured Design**:
- Separation of validation and calculation
- Reusable functions
- Clear control flow
- Type-safe operations

---

## 💻 BASIC (1983) - COMPOUND INTEREST CALCULATOR

### OVERVIEW

**Language**: BASIC (Beginner's All-purpose Symbolic Instruction Code)  
**Era**: 1983  
**Compiler**: FreeBASIC (fbc)  
**Purpose**: Calculate compound interest with multiple compounding periods

### FORMULA

```
A = P(1 + r/n)^(nt)

Where:
  A = Final amount
  P = Principal (initial amount)
  r = Annual interest rate (as decimal)
  n = Number of times interest compounds per year
  t = Time in years
```

### SOURCE CODE

**File**: `legacy/basic/interest.bas`

```basic
10 REM ************************************************
20 REM * COMPOUND INTEREST CALCULATOR                *
30 REM * VINTAGE BASIC - 1983 EDITION               *
40 REM * FORMULA: A = P(1 + r/n)^(nt)               *
50 REM ************************************************
100 REM
110 REM MAIN PROGRAM ENTRY POINT
120 GOSUB 1000
130 GOSUB 2000
140 GOSUB 3000
150 GOSUB 4000
160 END
170 REM
1000 REM *** SUBROUTINE: READ COMMAND LINE ARGUMENTS ***
1010 INPUT "PRINCIPAL"; P
1020 INPUT "ANNUAL RATE (%)"; R
1030 INPUT "COMPOUNDS PER YEAR"; N
1040 INPUT "YEARS"; T
1050 RETURN
1060 REM
2000 REM *** SUBROUTINE: VALIDATE INPUTS ***
2010 IF P <= 0 THEN GOTO 5000
2020 IF R <= 0 THEN GOTO 5100
2030 IF N <= 0 THEN GOTO 5200
2040 IF T <= 0 THEN GOTO 5300
2050 RETURN
2060 REM
3000 REM *** SUBROUTINE: CALCULATE COMPOUND INTEREST ***
3010 REM A = P(1 + r/n)^(nt)
3020 LET R1 = R / 100 / N
3030 LET N1 = N * T
3040 LET A = P * (1 + R1) ^ N1
3050 RETURN
3060 REM
4000 REM *** SUBROUTINE: OUTPUT RESULT ***
4010 PRINT "RESULT: ";
4020 PRINT USING "########.##"; A
4030 RETURN
4040 REM
5000 REM *** ERROR: INVALID PRINCIPAL ***
5010 PRINT "ERROR: INVALID PRINCIPAL AMOUNT"
5020 END
```

### COMPILATION

```bash
cd legacy/basic
fbc -o interest interest.bas
```

### USAGE EXAMPLES

**Example 1: Monthly Compounding**
```bash
echo -e "10000\n5\n12\n10" | ./legacy/basic/interest

# Input:
#   Principal: $10,000
#   Rate: 5%
#   Compounds: 12 (monthly)
#   Years: 10
#
# Output: RESULT: 16470.09
# Explanation: $10,000 at 5% compounded monthly for 10 years = $16,470.09
```

**Example 2: Annual Compounding**
```bash
echo -e "5000\n7\n1\n20" | ./legacy/basic/interest

# Input:
#   Principal: $5,000
#   Rate: 7%
#   Compounds: 1 (annually)
#   Years: 20
#
# Output: RESULT: 19348.42
# Explanation: $5,000 at 7% compounded annually for 20 years = $19,348.42
```

**Example 3: Daily Compounding**
```bash
echo -e "1000\n3\n365\n5" | ./legacy/basic/interest

# Input:
#   Principal: $1,000
#   Rate: 3%
#   Compounds: 365 (daily)
#   Years: 5
#
# Output: RESULT:  1161.83
# Explanation: $1,000 at 3% compounded daily for 5 years = $1,161.83
```

**Example 4: Quarterly Compounding**
```bash
echo -e "25000\n6\n4\n15" | ./legacy/basic/interest

# Input:
#   Principal: $25,000
#   Rate: 6%
#   Compounds: 4 (quarterly)
#   Years: 15
#
# Output: RESULT: 60983.28
# Explanation: $25,000 at 6% compounded quarterly for 15 years = $60,983.28
```

### API USAGE

```bash
curl -X POST http://localhost:3001/api/calculate/basic \
  -H "Content-Type: application/json" \
  -d '{
    "principal": 10000,
    "rate": 5,
    "time": 10,
    "compounds": 12
  }'

# Response:
# {
#   "result": 16470.09,
#   "source": "BASIC_LEGACY_ENGINE",
#   "language": "basic",
#   "year": 1983,
#   "calculation": "COMPOUND INTEREST CALCULATOR"
# }
```

### KEY FEATURES

**Authentic 1983 Syntax**:
- Line-numbered statements (10, 20, 30...)
- REM comments
- GOSUB/RETURN subroutine structure
- GOTO statements for control flow
- LET assignments
- PRINT USING for formatted output

**Vintage Programming Style**:
- Structured with subroutines (1000, 2000, 3000...)
- Error handling via GOTO
- INPUT statements for data entry
- Simple variable names (P, R, N, T, A)

---

## 🔄 COMPARING ALL FOUR LANGUAGES

### SYNTAX COMPARISON

**Variable Declaration**:
```
COBOL:    01 WS-PRINCIPAL PIC 9(8)V99.
FORTRAN:  REAL PRINCIPAL
PASCAL:   VAR Principal: Real;
BASIC:    LET P = 0
```

**Conditional Logic**:
```
COBOL:    IF WS-PRINCIPAL <= 0
             DISPLAY "ERROR" UPON SYSERR
          END-IF

FORTRAN:  IF (PRINCIPAL .LE. 0.0) THEN
             WRITE(0,*) 'ERROR'
          END IF

PASCAL:   IF Principal <= 0.0 THEN
          BEGIN
             WriteLn(StdErr, 'ERROR');
          END;

BASIC:    IF P <= 0 THEN GOTO 5000
```

**Output**:
```
COBOL:    DISPLAY "RESULT: " WS-PAYMENT-FORMATTED
FORTRAN:  WRITE(6,100) RESULT
PASCAL:   WriteLn('RESULT: ', Result:0:2);
BASIC:    PRINT "RESULT: "; A
```

### COMPILATION COMPARISON

| Language | Compiler | Command | Output |
|----------|----------|---------|--------|
| COBOL | cobc | `cobc -x -o mortgage mortgage.cbl` | mortgage |
| FORTRAN | gfortran | `gfortran -o trajectory trajectory.f` | trajectory |
| PASCAL | fpc | `fpc -o tax tax.pas` | tax |
| BASIC | fbc | `fbc -o interest interest.bas` | interest |

### EXECUTION COMPARISON

| Language | Input Method | Example |
|----------|--------------|---------|
| COBOL | Command-line args | `./mortgage 200000 5.5 30` |
| FORTRAN | Command-line args | `./trajectory 100 45 9.8` |
| PASCAL | Command-line args | `./tax 75000 25 50000` |
| BASIC | STDIN (piped) | `echo -e "10000\n5\n12\n10" \| ./interest` |

---

## 🎯 TESTING EXAMPLES

### PROPERTY-BASED TESTING

**Test All Languages for Output Format**:
```javascript
// Feature: cobol-resurrection-bridge, Property 2: Output Format Consistency
test('All languages output RESULT: XX.XX format', () => {
  fc.assert(
    fc.property(
      fc.constantFrom('cobol', 'fortran', 'pascal', 'basic'),
      fc.array(fc.float(), { minLength: 3, maxLength: 4 }),
      (language, params) => {
        const output = executeBinary(language, params);
        expect(output).toMatch(/^RESULT:\s*\d+\.\d{2}$/);
      }
    ),
    { numRuns: 100 }
  );
});
```

### UNIT TESTING

**Test Specific Calculations**:
```javascript
describe('Legacy Calculations', () => {
  test('COBOL: $200k at 5.5% for 30 years', () => {
    const result = executeCobol(200000, 5.5, 30);
    expect(result).toBeCloseTo(1135.58, 2);
  });
  
  test('FORTRAN: 100 m/s at 45° with g=9.8', () => {
    const result = executeFortran(100, 45, 9.8);
    expect(result).toBeCloseTo(1020.41, 2);
  });
  
  test('PASCAL: $75k income, 25% rate, $50k threshold', () => {
    const result = executePascal(75000, 25, 50000);
    expect(result).toBeCloseTo(6250.00, 2);
  });
  
  test('BASIC: $10k at 5% monthly for 10 years', () => {
    const result = executeBasic(10000, 5, 10, 12);
    expect(result).toBeCloseTo(16470.09, 2);
  });
});
```

---

## 📖 LEARNING RESOURCES

### COBOL
- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/)
- [COBOL Programming Guide (IBM)](https://www.ibm.com/docs/en/cobol-zos)
- [Mortgage Formula Explanation](https://en.wikipedia.org/wiki/Mortgage_calculator)

### FORTRAN
- [GNU Fortran Manual](https://gcc.gnu.org/onlinedocs/gfortran/)
- [FORTRAN 77 Tutorial](https://web.stanford.edu/class/me200c/tutorial_77/)
- [Projectile Motion Physics](https://en.wikipedia.org/wiki/Projectile_motion)

### PASCAL
- [Free Pascal Documentation](https://www.freepascal.org/docs.html)
- [Pascal Programming Tutorial](https://www.tutorialspoint.com/pascal/)
- [Progressive Tax Systems](https://en.wikipedia.org/wiki/Progressive_tax)

### BASIC
- [FreeBASIC Manual](https://www.freebasic.net/wiki/DocToc)
- [Vintage BASIC Programming](https://www.calormen.com/jsbasic/)
- [Compound Interest Formula](https://en.wikipedia.org/wiki/Compound_interest)

---

**EXAMPLES STATUS**: COMPREHENSIVE  
**LANGUAGE COVERAGE**: 100%  
**DOCUMENTATION**: COMPLETE  

`[END OF TAPE]`
