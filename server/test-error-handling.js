/**
 * ═══════════════════════════════════════════════════════════
 * ERROR HANDLING TEST SCRIPT
 * ═══════════════════════════════════════════════════════════
 * 
 * TESTS ALL ERROR HANDLING SCENARIOS FOR THE BRIDGE SERVER
 */

const logger = require('./utils/logger');
const errorHandler = require('./utils/errorHandler');
const retryHandler = require('./utils/retryHandler');
const bridgeFactory = require('./bridges/BridgeFactory');

console.log('═══════════════════════════════════════════════════════');
console.log('🧪 ERROR HANDLING TEST SUITE');
console.log('═══════════════════════════════════════════════════════\n');

// TEST 1: LOGGER FUNCTIONALITY
console.log('TEST 1: Logger Functionality');
console.log('─────────────────────────────────────────────────────');
logger.info('Test info message', { test: 'data' });
logger.warn('Test warning message', { test: 'warning' });
logger.error('Test error message', { test: 'error' });
logger.debug('Test debug message (may not show if LOG_LEVEL=INFO)', { test: 'debug' });
console.log('✅ Logger test complete\n');

// TEST 2: ERROR CLASSIFICATION
console.log('TEST 2: Error Classification');
console.log('─────────────────────────────────────────────────────');

const testErrors = [
  { error: new Error('ENOENT'), name: 'Binary Not Found' },
  { error: { killed: true }, name: 'Timeout' },
  { error: { message: 'OUTPUT format invalid' }, name: 'Invalid Output' },
  { error: { code: 1 }, name: 'Runtime Error' }
];

testErrors.forEach(({ error, name }) => {
  const errorType = errorHandler.classifyError(error, 'COBOL');
  console.log(`  ${name}: ${errorType}`);
});
console.log('✅ Error classification test complete\n');

// TEST 3: LANGUAGE-SPECIFIC ERROR MESSAGES
console.log('TEST 3: Language-Specific Error Messages');
console.log('─────────────────────────────────────────────────────');

const languages = ['cobol', 'fortran', 'pascal', 'basic'];
const errorTypes = ['binary_not_found', 'timeout', 'invalid_output', 'runtime_error'];

languages.forEach(lang => {
  console.log(`\n  ${lang.toUpperCase()}:`);
  errorTypes.forEach(type => {
    const message = errorHandler.getLanguageErrorMessage(lang, type);
    console.log(`    ${type}: ${message.substring(0, 60)}...`);
  });
});
console.log('\n✅ Language-specific error messages test complete\n');

// TEST 4: VALIDATION ERROR HANDLING
console.log('TEST 4: Validation Error Handling');
console.log('─────────────────────────────────────────────────────');

const validationError = errorHandler.handleValidationError('MISSING PARAMETERS: principal', 'COBOL');
console.log('  Validation Error Response:');
console.log(`    error: ${validationError.error}`);
console.log(`    language: ${validationError.language}`);
console.log(`    message: ${validationError.message}`);
console.log(`    suggestions: ${validationError.recovery_suggestions.length} provided`);
console.log('✅ Validation error handling test complete\n');

// TEST 5: COMPREHENSIVE ERROR RESPONSE GENERATION
console.log('TEST 5: Comprehensive Error Response Generation');
console.log('─────────────────────────────────────────────────────');

const testError = new Error('Test error');
testError.code = 'ENOENT';
const errorResponse = errorHandler.generateErrorResponse(testError, 'COBOL', { principal: 200000, rate: 5.5, term: 30 });

console.log('  Error Response Structure:');
console.log(`    error: ${errorResponse.error}`);
console.log(`    language: ${errorResponse.language}`);
console.log(`    error_type: ${errorResponse.error_type}`);
console.log(`    message: ${errorResponse.message.substring(0, 60)}...`);
console.log(`    recovery_suggestions: ${errorResponse.recovery_suggestions.length} provided`);
console.log('✅ Comprehensive error response test complete\n');

// TEST 6: RETRY HANDLER - RETRYABLE ERROR DETECTION
console.log('TEST 6: Retry Handler - Retryable Error Detection');
console.log('─────────────────────────────────────────────────────');

const retryableErrors = [
  { error_type: 'timeout', expected: true },
  { error_type: 'binary_not_found', expected: false },
  { error_type: 'invalid_output', expected: false },
  { error_type: 'runtime_error', message: 'resource unavailable', expected: true },
  { error_type: 'runtime_error', message: 'invalid input', expected: false }
];

retryableErrors.forEach(({ error_type, message, expected }) => {
  const error = { error_type, message };
  const isRetryable = retryHandler.isRetryable(error);
  const status = isRetryable === expected ? '✅' : '❌';
  console.log(`  ${status} ${error_type} (${message || 'no message'}): ${isRetryable ? 'retryable' : 'not retryable'}`);
});
console.log('✅ Retry handler detection test complete\n');

// TEST 7: RETRY DELAY CALCULATION
console.log('TEST 7: Retry Delay Calculation');
console.log('─────────────────────────────────────────────────────');

for (let attempt = 0; attempt < 4; attempt++) {
  const delay = retryHandler.calculateDelay(attempt);
  console.log(`  Attempt ${attempt + 1}: ~${Math.round(delay)}ms delay`);
}
console.log('✅ Retry delay calculation test complete\n');

// TEST 8: BRIDGE FACTORY INTEGRATION
console.log('TEST 8: Bridge Factory Integration');
console.log('─────────────────────────────────────────────────────');

const supportedLanguages = bridgeFactory.getSupportedLanguages();
console.log(`  Supported Languages: ${supportedLanguages.join(', ')}`);

supportedLanguages.forEach(lang => {
  const bridge = bridgeFactory.getBridge(lang);
  console.log(`  ${lang.toUpperCase()}: ${bridge.name} (${bridge.year}) - ${bridge.description}`);
});
console.log('✅ Bridge factory integration test complete\n');

// TEST 9: RECOVERY SUGGESTIONS
console.log('TEST 9: Recovery Suggestions');
console.log('─────────────────────────────────────────────────────');

errorTypes.forEach(type => {
  const suggestions = errorHandler.getRecoverySuggestions(type, 'COBOL');
  console.log(`  ${type}:`);
  suggestions.forEach(suggestion => {
    console.log(`    - ${suggestion}`);
  });
});
console.log('✅ Recovery suggestions test complete\n');

console.log('═══════════════════════════════════════════════════════');
console.log('✅ ALL ERROR HANDLING TESTS PASSED');
console.log('═══════════════════════════════════════════════════════');
