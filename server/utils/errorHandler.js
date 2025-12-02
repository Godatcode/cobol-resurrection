/**
 * ═══════════════════════════════════════════════════════════
 * ERROR HANDLER - COMPREHENSIVE ERROR RECOVERY MECHANISMS
 * ═══════════════════════════════════════════════════════════
 * 
 * PROVIDES CENTRALIZED ERROR HANDLING AND RECOVERY STRATEGIES
 * GENERATES DETAILED ERROR MESSAGES FOR EACH LEGACY LANGUAGE
 * IMPLEMENTS GRACEFUL DEGRADATION AND RETRY LOGIC
 */

const logger = require('./logger');

class ErrorHandler {
  constructor() {
    // LANGUAGE-SPECIFIC ERROR MESSAGES
    this.languageErrors = {
      cobol: {
        binary_not_found: 'COBOL BINARY NOT FOUND - ENSURE mortgage.cbl IS COMPILED WITH: cobc -x -o legacy/cobol/mortgage legacy/cobol/mortgage.cbl',
        timeout: 'COBOL PROCESS EXCEEDED TIMEOUT - MORTGAGE CALCULATION TOO COMPLEX OR SYSTEM OVERLOADED',
        invalid_output: 'COBOL OUTPUT FORMAT INVALID - EXPECTED "RESULT: XXXX.XX" FORMAT',
        compilation_error: 'COBOL COMPILATION FAILED - CHECK SYNTAX IN mortgage.cbl',
        runtime_error: 'COBOL RUNTIME ERROR - CHECK INPUT PARAMETERS AND COBOL LOGIC'
      },
      fortran: {
        binary_not_found: 'FORTRAN BINARY NOT FOUND - ENSURE trajectory.f IS COMPILED WITH: gfortran -o legacy/fortran/trajectory legacy/fortran/trajectory.f',
        timeout: 'FORTRAN PROCESS EXCEEDED TIMEOUT - TRAJECTORY CALCULATION TOO COMPLEX OR SYSTEM OVERLOADED',
        invalid_output: 'FORTRAN OUTPUT FORMAT INVALID - EXPECTED "RESULT: <number>" FORMAT',
        compilation_error: 'FORTRAN COMPILATION FAILED - CHECK SYNTAX IN trajectory.f',
        runtime_error: 'FORTRAN RUNTIME ERROR - CHECK INPUT PARAMETERS AND FORTRAN LOGIC'
      },
      pascal: {
        binary_not_found: 'PASCAL BINARY NOT FOUND - ENSURE tax.pas IS COMPILED WITH: fpc -o legacy/pascal/tax legacy/pascal/tax.pas',
        timeout: 'PASCAL PROCESS EXCEEDED TIMEOUT - TAX CALCULATION TOO COMPLEX OR SYSTEM OVERLOADED',
        invalid_output: 'PASCAL OUTPUT FORMAT INVALID - EXPECTED "RESULT: <number>" FORMAT',
        compilation_error: 'PASCAL COMPILATION FAILED - CHECK SYNTAX IN tax.pas',
        runtime_error: 'PASCAL RUNTIME ERROR - CHECK INPUT PARAMETERS AND PASCAL LOGIC'
      },
      basic: {
        binary_not_found: 'BASIC BINARY NOT FOUND - ENSURE interest.bas IS COMPILED WITH: fbc -o legacy/basic/interest legacy/basic/interest.bas',
        timeout: 'BASIC PROCESS EXCEEDED TIMEOUT - INTEREST CALCULATION TOO COMPLEX OR SYSTEM OVERLOADED',
        invalid_output: 'BASIC OUTPUT FORMAT INVALID - EXPECTED "RESULT: <number>" FORMAT',
        compilation_error: 'BASIC COMPILATION FAILED - CHECK SYNTAX IN interest.bas',
        runtime_error: 'BASIC RUNTIME ERROR - CHECK INPUT PARAMETERS AND BASIC LOGIC'
      }
    };
  }
  
  /**
   * CLASSIFY ERROR TYPE BASED ON ERROR OBJECT
   */
  classifyError(error, language) {
    // TIMEOUT ERROR
    if (error.killed) {
      return 'timeout';
    }
    
    // BINARY NOT FOUND (ENOENT)
    if (error.code === 'ENOENT' || (error.message && error.message.includes('ENOENT'))) {
      return 'binary_not_found';
    }
    
    // PARSING ERROR
    if (error.message && error.message.includes('OUTPUT')) {
      return 'invalid_output';
    }
    
    // RUNTIME ERROR (NON-ZERO EXIT CODE)
    if (error.code && error.code !== 0 && error.code !== 'ENOENT') {
      return 'runtime_error';
    }
    
    // DEFAULT TO RUNTIME ERROR
    return 'runtime_error';
  }
  
  /**
   * GET LANGUAGE-SPECIFIC ERROR MESSAGE
   */
  getLanguageErrorMessage(language, errorType) {
    const languageKey = language.toLowerCase();
    
    if (this.languageErrors[languageKey] && this.languageErrors[languageKey][errorType]) {
      return this.languageErrors[languageKey][errorType];
    }
    
    // FALLBACK TO GENERIC ERROR MESSAGE
    return `${language.toUpperCase()} PROCESS FAILED - ${errorType.toUpperCase().replace('_', ' ')}`;
  }
  
  /**
   * GENERATE COMPREHENSIVE ERROR RESPONSE
   */
  generateErrorResponse(error, language, params = null) {
    const errorType = this.classifyError(error, language);
    const errorMessage = this.getLanguageErrorMessage(language, errorType);
    
    const errorResponse = {
      error: 'CORE DUMP DETECTED',
      language: language.toLowerCase(),
      error_type: errorType,
      message: errorMessage,
      details: error.message || error.toString(),
      timestamp: new Date().toISOString()
    };
    
    // ADD PARAMETERS IF PROVIDED (FOR DEBUGGING)
    if (params) {
      errorResponse.input_params = params;
    }
    
    // ADD STDERR IF AVAILABLE
    if (error.stderr) {
      errorResponse.stderr = error.stderr;
    }
    
    // ADD RAW OUTPUT IF AVAILABLE
    if (error.raw_output) {
      errorResponse.raw_output = error.raw_output;
    }
    
    // ADD RECOVERY SUGGESTIONS
    errorResponse.recovery_suggestions = this.getRecoverySuggestions(errorType, language);
    
    // LOG ERROR
    logger.error(`ERROR HANDLER: ${errorType} for ${language}`, errorResponse);
    
    return errorResponse;
  }
  
  /**
   * GET RECOVERY SUGGESTIONS BASED ON ERROR TYPE
   */
  getRecoverySuggestions(errorType, language) {
    const suggestions = [];
    
    switch (errorType) {
      case 'binary_not_found':
        suggestions.push(`Compile the ${language} program using the appropriate compiler`);
        suggestions.push('Verify the binary path is correct');
        suggestions.push('Check file permissions on the binary');
        break;
        
      case 'timeout':
        suggestions.push('Reduce input parameter complexity');
        suggestions.push('Check system resources (CPU, memory)');
        suggestions.push('Increase timeout limit if calculations are legitimately complex');
        break;
        
      case 'invalid_output':
        suggestions.push('Verify the legacy program outputs in the correct format');
        suggestions.push('Check for unexpected error messages in program output');
        suggestions.push('Review program logic for output formatting');
        break;
        
      case 'runtime_error':
        suggestions.push('Validate input parameters are within acceptable ranges');
        suggestions.push('Check for division by zero or overflow conditions');
        suggestions.push('Review program logic for edge cases');
        break;
        
      default:
        suggestions.push('Review error details and stderr output');
        suggestions.push('Check system logs for additional information');
    }
    
    return suggestions;
  }
  
  /**
   * HANDLE VALIDATION ERRORS
   */
  handleValidationError(validationError, language) {
    const errorResponse = {
      error: 'INVALID INPUT',
      language: language.toLowerCase(),
      message: validationError,
      timestamp: new Date().toISOString(),
      recovery_suggestions: [
        'Ensure all required parameters are provided',
        'Verify parameter types are numeric',
        'Check that all values are positive numbers'
      ]
    };
    
    logger.warn(`VALIDATION ERROR for ${language}`, errorResponse);
    
    return errorResponse;
  }
  
  /**
   * EXPRESS MIDDLEWARE FOR GLOBAL ERROR HANDLING
   */
  expressErrorMiddleware() {
    return (err, req, res, next) => {
      logger.error('UNHANDLED ERROR IN EXPRESS', {
        error: err.message,
        stack: err.stack,
        path: req.path,
        method: req.method
      });
      
      res.status(500).json({
        error: 'INTERNAL SERVER ERROR',
        message: 'AN UNEXPECTED ERROR OCCURRED IN THE BRIDGE SERVER',
        details: err.message,
        timestamp: new Date().toISOString()
      });
    };
  }
}

// EXPORT SINGLETON INSTANCE
module.exports = new ErrorHandler();
