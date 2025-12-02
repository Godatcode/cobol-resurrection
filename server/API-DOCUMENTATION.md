# UNIVERSAL NECRO-BRIDGE API DOCUMENTATION

## OVERVIEW

THE UNIVERSAL NECRO-BRIDGE SERVER PROVIDES REST API ENDPOINTS FOR EXECUTING CALCULATIONS IN FOUR VINTAGE PROGRAMMING LANGUAGES: COBOL (1959), FORTRAN (1957), PASCAL (1970), AND BASIC (1983).

## BASE URL

```
http://localhost:3001
```

## ENDPOINTS

### GET /api/health

HEALTH CHECK ENDPOINT TO VERIFY SERVER STATUS.

**Response:**
```json
{
  "status": "OPERATIONAL",
  "message": "UNIVERSAL BRIDGE SERVER ONLINE - READY TO SUMMON ANCIENT SPIRITS",
  "timestamp": "2024-12-02T13:54:20.123Z",
  "supported_languages": ["cobol", "fortran", "pascal", "basic"]
}
```

---

### GET /api/languages

RETURNS METADATA FOR ALL SUPPORTED LEGACY LANGUAGES.

**Response:**
```json
{
  "count": 4,
  "languages": [
    {
      "id": "cobol",
      "name": "COBOL",
      "year": 1959,
      "description": "MORTGAGE PAYMENT CALCULATOR",
      "parameters": ["principal", "rate", "term"]
    },
    {
      "id": "fortran",
      "name": "FORTRAN",
      "year": 1957,
      "description": "BALLISTIC TRAJECTORY CALCULATOR",
      "parameters": ["velocity", "angle", "gravity"]
    },
    {
      "id": "pascal",
      "name": "PASCAL",
      "year": 1970,
      "description": "PROGRESSIVE TAX CALCULATOR",
      "parameters": ["income", "bracket_rate", "bracket_threshold"]
    },
    {
      "id": "basic",
      "name": "BASIC",
      "year": 1983,
      "description": "COMPOUND INTEREST CALCULATOR",
      "parameters": ["principal", "rate", "time", "compounds"]
    }
  ]
}
```

---

### POST /api/calculate/cobol

EXECUTES MORTGAGE PAYMENT CALCULATION USING COBOL (1959).

**Formula:** M = P [ i(1 + i)^n ] / [ (1 + i)^n – 1 ]

**Request Body:**
```json
{
  "principal": 200000,
  "rate": 5.5,
  "term": 30
}
```

**Parameters:**
- `principal` (number): LOAN AMOUNT IN DOLLARS
- `rate` (number): ANNUAL INTEREST RATE AS PERCENTAGE
- `term` (number): LOAN TERM IN YEARS

**Success Response (200):**
```json
{
  "result": 1135.57,
  "source": "COBOL_LEGACY_ENGINE",
  "language": "cobol",
  "year": 1959,
  "calculation": "MORTGAGE PAYMENT CALCULATOR"
}
```

**Error Response (400/500):**
```json
{
  "error": "CORE DUMP DETECTED",
  "details": "ERROR: INVALID PRINCIPAL AMOUNT",
  "language": "cobol"
}
```

---

### POST /api/calculate/fortran

EXECUTES BALLISTIC TRAJECTORY CALCULATION USING FORTRAN (1957).

**Formula:** range = (v² × sin(2θ)) / g

**Request Body:**
```json
{
  "velocity": 100,
  "angle": 45,
  "gravity": 9.8
}
```

**Parameters:**
- `velocity` (number): INITIAL VELOCITY IN METERS PER SECOND
- `angle` (number): LAUNCH ANGLE IN DEGREES (0-90)
- `gravity` (number): GRAVITATIONAL ACCELERATION (TYPICALLY 9.8 m/s²)

**Success Response (200):**
```json
{
  "result": 1020.41,
  "source": "FORTRAN_LEGACY_ENGINE",
  "language": "fortran",
  "year": 1957,
  "calculation": "BALLISTIC TRAJECTORY CALCULATOR"
}
```

---

### POST /api/calculate/pascal

EXECUTES PROGRESSIVE TAX CALCULATION USING PASCAL (1970).

**Formula:** Tax = (Income - Threshold) × Rate / 100

**Request Body:**
```json
{
  "income": 75000,
  "bracket_rate": 25,
  "bracket_threshold": 50000
}
```

**Parameters:**
- `income` (number): TOTAL INCOME AMOUNT
- `bracket_rate` (number): TAX RATE PERCENTAGE FOR BRACKET
- `bracket_threshold` (number): INCOME THRESHOLD FOR BRACKET

**Success Response (200):**
```json
{
  "result": 6250,
  "source": "PASCAL_LEGACY_ENGINE",
  "language": "pascal",
  "year": 1970,
  "calculation": "PROGRESSIVE TAX CALCULATOR"
}
```

---

### POST /api/calculate/basic

EXECUTES COMPOUND INTEREST CALCULATION USING BASIC (1983).

**Formula:** A = P(1 + r/n)^(nt)

**Request Body:**
```json
{
  "principal": 10000,
  "rate": 5,
  "time": 10,
  "compounds": 12
}
```

**Parameters:**
- `principal` (number): INITIAL PRINCIPAL AMOUNT
- `rate` (number): ANNUAL INTEREST RATE AS PERCENTAGE
- `time` (number): TIME PERIOD IN YEARS
- `compounds` (number): NUMBER OF TIMES INTEREST COMPOUNDS PER YEAR

**Success Response (200):**
```json
{
  "result": 18193.97,
  "source": "BASIC_LEGACY_ENGINE",
  "language": "basic",
  "year": 1983,
  "calculation": "COMPOUND INTEREST CALCULATOR"
}
```

---

### POST /api/calculate (LEGACY ENDPOINT)

BACKWARD-COMPATIBLE ENDPOINT FOR ORIGINAL COBOL-ONLY IMPLEMENTATION.

**Request Body:**
```json
{
  "principal": 200000,
  "rate": 5.5,
  "term": 30
}
```

**Success Response (200):**
```json
{
  "monthly_payment": 1135.57,
  "source": "COBOL_LEGACY_ENGINE"
}
```

---

## ERROR HANDLING

ALL ENDPOINTS FOLLOW CONSISTENT ERROR RESPONSE FORMAT:

**Validation Errors (400):**
```json
{
  "error": "INVALID INPUT",
  "details": "MISSING REQUIRED PARAMETER: principal",
  "language": "cobol"
}
```

**Execution Errors (500):**
```json
{
  "error": "CORE DUMP DETECTED",
  "details": "ERROR: INVALID PRINCIPAL AMOUNT\n",
  "language": "cobol"
}
```

**Unsupported Language (400):**
```json
{
  "error": "UNSUPPORTED LANGUAGE",
  "details": "LANGUAGE 'python' NOT RECOGNIZED",
  "supported": ["cobol", "fortran", "pascal", "basic"]
}
```

---

## TESTING

RUN THE COMPREHENSIVE TEST SUITE:

```bash
npx vitest run tests/multi-language-endpoints.test.js
```

---

## ARCHITECTURE

THE SERVER USES THE BRIDGE PATTERN TO ABSTRACT LEGACY LANGUAGE EXECUTION:

```
CLIENT REQUEST
    ↓
EXPRESS ROUTER
    ↓
LANGUAGE DETECTION MIDDLEWARE
    ↓
BRIDGE FACTORY
    ↓
LANGUAGE-SPECIFIC BRIDGE (CobolBridge, FortranBridge, etc.)
    ↓
PROCESS SPAWNER (child_process.exec)
    ↓
LEGACY BINARY EXECUTION
    ↓
OUTPUT PARSER
    ↓
JSON RESPONSE
```

---

## REQUIREMENTS VALIDATION

✅ **REQUIREMENT 2.1**: BRIDGE SERVER SPAWNS LEGACY BINARIES VIA API
✅ **REQUIREMENT 9.2**: UNIVERSAL BRIDGE PATTERN SUPPORTS MULTIPLE LANGUAGES
✅ **REQUIREMENT 9.4**: UI CAN INDICATE WHICH LANGUAGE PROCESSED CALCULATION

[END OF TAPE]
