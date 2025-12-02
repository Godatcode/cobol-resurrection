# BASIC Compound Interest Calculator

## Overview

This is an AUTHENTIC 1983-style BASIC program that calculates compound interest using the formula:

```
A = P(1 + r/n)^(nt)
```

Where:
- **A** = Final amount
- **P** = Principal (initial investment)
- **r** = Annual interest rate (as decimal)
- **n** = Number of times interest is compounded per year
- **t** = Number of years

## Implementation

The program maintains VINTAGE BASIC SEMANTICS from 1983:

- **Line-numbered programming** (lines 10-5320)
- **GOSUB/RETURN subroutines** for structured flow
- **GOTO statements** for error handling (authentic vintage style)
- **REM comments** for documentation
- **PRINT USING** for formatted output

## Technical Details

Due to FreeBASIC compiler unavailability on ARM64 macOS, this implementation uses a **Node.js interpreter wrapper** that executes the BASIC logic while maintaining the authentic vintage semantics. The wrapper:

1. Preserves the original BASIC algorithm structure
2. Implements identical error handling
3. Produces identical output format
4. Maintains line-by-line correspondence with BASIC source

## Usage

```bash
# Calculate compound interest
./legacy/basic/interest <principal> <rate> <compounds_per_year> <years>

# Example: $10,000 at 5% compounded monthly for 10 years
./legacy/basic/interest 10000 5 12 10
# Output: RESULT: 16470.09
```

## Examples

```bash
# $5,000 at 3.5% compounded quarterly for 5 years
./legacy/basic/interest 5000 3.5 4 5
# Output: RESULT: 5951.70

# $1,000 at 10% compounded annually for 20 years
./legacy/basic/interest 1000 10 1 20
# Output: RESULT: 6727.50
```

## Error Handling

The program validates all inputs and provides clear error messages:

```bash
# Invalid principal
./legacy/basic/interest -1000 5 12 10
# ERROR: INVALID PRINCIPAL AMOUNT

# Invalid rate
./legacy/basic/interest 10000 0 12 10
# ERROR: INVALID INTEREST RATE

# Insufficient arguments
./legacy/basic/interest 10000 5
# ERROR: INSUFFICIENT ARGUMENTS
# USAGE: interest <principal> <rate> <compounds_per_year> <years>
```

## Output Format

All successful calculations output in the standard format:

```
RESULT: XXXXXXXX.XX
```

This format matches the universal interface used by all legacy language calculators (COBOL, FORTRAN, PASCAL, BASIC).

## Historical Context

BASIC (Beginner's All-purpose Symbolic Instruction Code) was designed in 1964 at Dartmouth College. By 1983, BASIC had evolved to include:

- Line numbers (10, 20, 30...)
- GOSUB/RETURN for subroutines
- PRINT USING for formatted output
- Structured programming constructs

This implementation captures the essence of 1983-era BASIC programming, including the liberal use of GOTO statements that were common practice at the time.

[END OF TAPE]
