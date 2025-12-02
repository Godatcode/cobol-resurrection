# Error Handling and Logging Documentation

## Overview

The COBOL Resurrection Bridge implements comprehensive error handling and logging mechanisms to ensure robust operation across all legacy language bridges (COBOL, FORTRAN, PASCAL, BASIC).

## Components

### 1. Logger (`server/utils/logger.js`)

**Purpose:** Provides centralized logging for all bridge server operations.

**Features:**
- Request/response logging
- Process execution tracking
- Configurable log levels (DEBUG, INFO, WARN, ERROR)
- Mainframe-style timestamp formatting
- Structured JSON logging for complex data

**Usage:**
```javascript
const logger = require('./utils/logger');

logger.info('Operation completed', { result: data });
logger.error('Operation failed', { error: errorDetails });
```

**Environment Variables:**
- `LOG_LEVEL`: Set logging verbosity (DEBUG, INFO, WARN, ERROR)

### 2. Error Handler (`server/utils/errorHandler.js`)

**Purpose:** Provides comprehensive error classification, language-specific error messages, and recovery suggestions.

**Features:**
- Language-specific error messages for each legacy language
- Error classification (binary_not_found, timeout, invalid_output, runtime_error, etc.)
- Detailed recovery suggestions
- Validation error handling
- Express middleware for global error handling

**Error Types:**

#### Binary Not Found
- **Cause:** Legacy binary not compiled or not in expected location
- **Response:** Includes compilation command for the specific language
- **Example:** `COBOL BINARY NOT FOUND - ENSURE mortgage.cbl IS COMPILED WITH: cobc -x -o legacy/cobol/mortgage legacy/cobol/mortgage.cbl`

#### Timeout
- **Cause:** Process exceeded 5-second timeout
- **Response:** Indicates system overload or complex calculation
- **Recovery:** Reduce input complexity or check system resources

#### Invalid Output
- **Cause:** Legacy program output doesn't match expected format
- **Response:** Shows expected format and actual output
- **Recovery:** Verify program logic and output formatting

#### Runtime Error
- **Cause:** Non-zero exit code from legacy process
- **Response:** Includes stderr output and error details
- **Recovery:** Check input parameters and program logic

**Usage:**
```javascript
const errorHandler = require('./utils/errorHandler');

// Generate comprehensive error response
const errorResponse = errorHandler.generateErrorResponse(error, 'COBOL', params);

// Handle validation errors
const validationError = errorHandler.handleValidationError('Missing parameter: principal', 'COBOL');
```

### 3. Retry Handler (`server/utils/retryHandler.js`)

**Purpose:** Implements retry logic with exponential backoff for transient failures.

**Features:**
- Automatic retry for transient errors (timeouts, resource issues)
- Exponential backoff with jitter
- Configurable max retries (default: 3)
- Smart error classification (retryable vs. permanent)

**Retryable Errors:**
- Timeout errors
- Runtime errors with transient indicators (resource, busy, unavailable, temporary)

**Non-Retryable Errors:**
- Binary not found
- Invalid input
- Parsing errors
- Compilation errors

**Usage:**
```javascript
const retryHandler = require('./utils/retryHandler');

// Execute with retry
const result = await retryHandler.executeWithRetry(
  () => someAsyncOperation(),
  { context: 'operation_name' }
);
```

**Retry Strategy:**
- Attempt 1: Immediate
- Attempt 2: ~100-130ms delay
- Attempt 3: ~200-260ms delay
- Attempt 4: ~400-520ms delay (capped at 2000ms)

## Integration with Legacy Bridges

All legacy bridges (COBOL, FORTRAN, PASCAL, BASIC) automatically integrate with the error handling system:

1. **Request Logging:** Every calculation request is logged with parameters
2. **Process Execution Logging:** Process spawning is tracked with command details
3. **Error Classification:** Errors are automatically classified and enhanced with language-specific messages
4. **Recovery Suggestions:** All errors include actionable recovery suggestions
5. **Optional Retry:** Bridges support optional retry logic via `enableRetry` option

**Example:**
```javascript
// Execute with retry enabled
const result = await cobolBridge.execute(
  { principal: 200000, rate: 5.5, term: 30 },
  { enableRetry: true }
);
```

## Error Response Format

All errors follow a standardized format:

```json
{
  "error": "CORE DUMP DETECTED",
  "language": "cobol",
  "error_type": "timeout",
  "message": "COBOL PROCESS EXCEEDED TIMEOUT - MORTGAGE CALCULATION TOO COMPLEX OR SYSTEM OVERLOADED",
  "details": "Error details from process",
  "timestamp": "2024-12-02T10:30:00.000Z",
  "input_params": {
    "principal": 200000,
    "rate": 5.5,
    "term": 30
  },
  "recovery_suggestions": [
    "Reduce input parameter complexity",
    "Check system resources (CPU, memory)",
    "Increase timeout limit if calculations are legitimately complex"
  ]
}
```

## Validation Error Format

Input validation errors use a different format:

```json
{
  "error": "INVALID INPUT",
  "language": "cobol",
  "message": "MISSING PARAMETERS: principal, rate",
  "timestamp": "2024-12-02T10:30:00.000Z",
  "recovery_suggestions": [
    "Ensure all required parameters are provided",
    "Verify parameter types are numeric",
    "Check that all values are positive numbers"
  ]
}
```

## Request/Response Logging

All API requests and responses are automatically logged:

**Request Log:**
```
[2024-12-02T10:30:00.000Z] [INFO] INCOMING REQUEST: POST /api/calculate/cobol
{
  "method": "POST",
  "path": "/api/calculate/cobol",
  "language": "cobol",
  "params": { "principal": 200000, "rate": 5.5, "term": 30 },
  "ip": "::1",
  "userAgent": "Mozilla/5.0..."
}
```

**Response Log (Success):**
```
[2024-12-02T10:30:01.000Z] [INFO] RESPONSE SUCCESS: POST /api/calculate/cobol
{
  "method": "POST",
  "path": "/api/calculate/cobol",
  "language": "cobol",
  "statusCode": 200,
  "response": { "result": 1135.58, "source": "COBOL_LEGACY_ENGINE", ... }
}
```

**Response Log (Error):**
```
[2024-12-02T10:30:01.000Z] [ERROR] RESPONSE ERROR: POST /api/calculate/cobol
{
  "method": "POST",
  "path": "/api/calculate/cobol",
  "language": "cobol",
  "statusCode": 500,
  "response": { "error": "CORE DUMP DETECTED", ... }
}
```

## Language-Specific Error Messages

Each legacy language has tailored error messages:

### COBOL (1959)
- Binary: `legacy/cobol/mortgage`
- Compiler: `cobc -x -o legacy/cobol/mortgage legacy/cobol/mortgage.cbl`
- Output Format: `RESULT: XXXX.XX`

### FORTRAN (1957)
- Binary: `legacy/fortran/trajectory`
- Compiler: `gfortran -o legacy/fortran/trajectory legacy/fortran/trajectory.f`
- Output Format: `RESULT: <number>`

### PASCAL (1970)
- Binary: `legacy/pascal/tax`
- Compiler: `fpc -o legacy/pascal/tax legacy/pascal/tax.pas`
- Output Format: `RESULT: <number>`

### BASIC (1983)
- Binary: `legacy/basic/interest`
- Compiler: `fbc -o legacy/basic/interest legacy/basic/interest.bas`
- Output Format: `RESULT: <number>`

## Global Error Handling

The Express server includes global error handling middleware that catches any unhandled errors:

```javascript
app.use(errorHandler.expressErrorMiddleware());
```

This ensures that even unexpected errors are logged and returned in a consistent format.

## Best Practices

1. **Always check logs:** All operations are logged with timestamps and context
2. **Read recovery suggestions:** Error responses include actionable suggestions
3. **Use retry for transient errors:** Enable retry for operations that may fail temporarily
4. **Monitor error types:** Track error_type field to identify systemic issues
5. **Validate inputs early:** Input validation happens before process spawning
6. **Check binary compilation:** Most errors are due to missing or incorrectly compiled binaries

## Troubleshooting

### Problem: "Binary Not Found" errors
**Solution:** Compile the legacy program using the command shown in the error message

### Problem: Timeout errors
**Solution:** 
- Check system resources (CPU, memory)
- Verify input parameters are reasonable
- Consider enabling retry logic

### Problem: Invalid output errors
**Solution:**
- Verify the legacy program is outputting in the correct format
- Check for unexpected error messages in program output
- Review program logic

### Problem: Runtime errors
**Solution:**
- Validate input parameters are within acceptable ranges
- Check for edge cases (division by zero, overflow)
- Review stderr output for specific error details

## Configuration

### Log Level
Set the `LOG_LEVEL` environment variable to control verbosity:
```bash
export LOG_LEVEL=DEBUG  # Most verbose
export LOG_LEVEL=INFO   # Default
export LOG_LEVEL=WARN   # Warnings and errors only
export LOG_LEVEL=ERROR  # Errors only
```

### Retry Configuration
Modify `server/utils/retryHandler.js` to adjust retry behavior:
- `maxRetries`: Maximum number of retry attempts (default: 3)
- `baseDelay`: Base delay in milliseconds (default: 100)
- `maxDelay`: Maximum delay in milliseconds (default: 2000)

## Testing Error Handling

Test error handling with invalid inputs:

```bash
# Missing parameters
curl -X POST http://localhost:3001/api/calculate/cobol \
  -H "Content-Type: application/json" \
  -d '{"principal": 200000}'

# Invalid parameter types
curl -X POST http://localhost:3001/api/calculate/cobol \
  -H "Content-Type: application/json" \
  -d '{"principal": "invalid", "rate": 5.5, "term": 30}'

# Unsupported language
curl -X POST http://localhost:3001/api/calculate/ada \
  -H "Content-Type: application/json" \
  -d '{"principal": 200000, "rate": 5.5, "term": 30}'
```

All errors will be logged and returned with comprehensive error messages and recovery suggestions.
