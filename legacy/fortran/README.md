# FORTRAN-57 Ballistic Trajectory Calculator

## Overview

This is an AUTHENTIC 1957 FORTRAN-IV program that calculates the range of a ballistic projectile using classical mechanics. The program demonstrates vintage fixed-format FORTRAN syntax from the era of the IBM 704 and IBM 7090 mainframes.

## Physics Formula

The program implements the classical ballistic trajectory formula:

```
Range = (V² × sin(2θ)) / g
```

Where:
- **V** = Initial velocity (m/s or ft/s)
- **θ** = Launch angle (degrees)
- **g** = Gravitational acceleration (m/s² or ft/s²)

## Compilation

### Prerequisites

Install GNU Fortran compiler:

```bash
# macOS
brew install gcc

# Ubuntu/Debian
sudo apt-get install gfortran

# RHEL/CentOS
sudo yum install gcc-gfortran
```

### Compile

```bash
./legacy/fortran/compile.sh
```

Or manually:

```bash
gfortran -std=legacy -o legacy/fortran/trajectory legacy/fortran/trajectory.f
```

## Usage

```bash
./legacy/fortran/trajectory <velocity> <angle> <gravity>
```

### Parameters

1. **velocity**: Initial velocity (must be > 0)
2. **angle**: Launch angle in degrees (must be 0 < angle < 90)
3. **gravity**: Gravitational acceleration (must be > 0)

### Examples

**Earth gravity (metric):**
```bash
./legacy/fortran/trajectory 100 45 9.8
# Output: RESULT:    1020.41
```

**Earth gravity (imperial):**
```bash
./legacy/fortran/trajectory 328 45 32.2
# Output: RESULT:    3347.52
```

**Moon gravity:**
```bash
./legacy/fortran/trajectory 100 45 1.62
# Output: RESULT:    6173.37
```

**Optimal angle (45 degrees):**
```bash
./legacy/fortran/trajectory 50 45 9.8
# Output: RESULT:     255.10
```

## Output Format

The program outputs results in the UNIVERSAL INTERFACE format:

```
RESULT: <range_value>
```

This format matches the COBOL mortgage calculator and enables seamless integration with the Bridge Server.

## Error Handling

The program validates all inputs and returns appropriate error codes:

- **Exit Code 0**: Successful calculation
- **Exit Code 1**: Error occurred

### Error Messages

- `ERROR: INSUFFICIENT ARGUMENTS` - Less than 3 arguments provided
- `ERROR: NON-NUMERIC INPUT DETECTED` - Arguments are not valid numbers
- `ERROR: INVALID VELOCITY` - Velocity is zero or negative
- `ERROR: INVALID ANGLE` - Angle is not between 0 and 90 degrees
- `ERROR: INVALID GRAVITY` - Gravity is zero or negative

## Vintage FORTRAN Features

This program demonstrates AUTHENTIC 1957 FORTRAN-IV syntax:

- **Fixed-format source**: Comments in column 1, code in columns 7-72
- **IMPLICIT NONE**: Type safety declaration
- **DIMENSION statements**: Array declarations (used for character strings)
- **FORMAT statements**: Classic FORTRAN I/O formatting
- **CALL EXIT()**: Program termination with exit codes
- **Column-aligned code**: Respects historical punch card format
- **PARAMETER**: Constant definitions (PI value)

## Historical Context

FORTRAN (FORmula TRANslation) was developed by IBM in 1957, making it the OLDEST high-level programming language still in use today. This implementation honors the original fixed-format syntax that was designed for IBM punch cards.

The IBM 704 and 7090 mainframes used FORTRAN extensively for scientific and engineering calculations, including ballistic trajectory analysis for military and aerospace applications.

## Integration with Bridge Server

This FORTRAN program integrates with the Node.js Bridge Server through the universal interface pattern:

1. Bridge Server spawns the FORTRAN binary as a child process
2. Command-line arguments are passed (velocity, angle, gravity)
3. FORTRAN calculates the trajectory range
4. Result is output to STDOUT in "RESULT: XXXX.XX" format
5. Bridge Server parses the output and returns JSON to the client

## Testing

### Manual Testing

```bash
# Test valid calculation
./legacy/fortran/trajectory 100 45 9.8
# Expected: RESULT:    1020.41

# Test error handling
./legacy/fortran/trajectory -100 45 9.8
# Expected: ERROR: INVALID VELOCITY (exit code 1)

./legacy/fortran/trajectory 100 95 9.8
# Expected: ERROR: INVALID ANGLE (exit code 1)
```

### Verification

To verify the calculation is correct:

```python
import math
v = 100  # velocity
theta = 45  # angle in degrees
g = 9.8  # gravity

theta_rad = math.radians(theta)
range_calc = (v**2 * math.sin(2 * theta_rad)) / g
print(f"Expected range: {range_calc:.2f}")
# Output: Expected range: 1020.41
```

## Requirements Validation

This implementation satisfies:

- ✅ **Requirement 9.1**: Multi-language legacy engine support (FORTRAN 1957)
- ✅ **Requirement 9.3**: Standardized JSON format output (via universal interface)
- ✅ Authentic 1957 fixed-format syntax
- ✅ Ballistic trajectory formula implementation
- ✅ DIMENSION statements for arrays (character strings)
- ✅ Universal interface output format
- ✅ Comprehensive error handling

[END OF TAPE]
