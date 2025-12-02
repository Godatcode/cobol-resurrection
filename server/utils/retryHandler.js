/**
 * ═══════════════════════════════════════════════════════════
 * RETRY HANDLER - ERROR RECOVERY WITH EXPONENTIAL BACKOFF
 * ═══════════════════════════════════════════════════════════
 * 
 * PROVIDES RETRY LOGIC FOR TRANSIENT FAILURES
 * IMPLEMENTS EXPONENTIAL BACKOFF STRATEGY
 * USEFUL FOR HANDLING TEMPORARY SYSTEM RESOURCE ISSUES
 */

const logger = require('./logger');

class RetryHandler {
  constructor() {
    this.maxRetries = 3;
    this.baseDelay = 100; // MILLISECONDS
    this.maxDelay = 2000; // MILLISECONDS
  }
  
  /**
   * DETERMINE IF ERROR IS RETRYABLE
   * ONLY RETRY TRANSIENT ERRORS (TIMEOUTS, RESOURCE ISSUES)
   * DO NOT RETRY PERMANENT ERRORS (BINARY NOT FOUND, INVALID INPUT)
   */
  isRetryable(error) {
    // TIMEOUT ERRORS ARE RETRYABLE
    if (error.error_type === 'timeout') {
      return true;
    }
    
    // RUNTIME ERRORS MAY BE RETRYABLE (COULD BE TRANSIENT)
    if (error.error_type === 'runtime_error') {
      // CHECK IF ERROR MESSAGE INDICATES TRANSIENT ISSUE
      const transientIndicators = [
        'resource',
        'busy',
        'unavailable',
        'temporary'
      ];
      
      const errorMessage = (error.message || '').toLowerCase();
      return transientIndicators.some(indicator => errorMessage.includes(indicator));
    }
    
    // ALL OTHER ERRORS ARE NOT RETRYABLE
    return false;
  }
  
  /**
   * CALCULATE DELAY FOR RETRY ATTEMPT
   * USES EXPONENTIAL BACKOFF WITH JITTER
   */
  calculateDelay(attemptNumber) {
    // EXPONENTIAL BACKOFF: delay = baseDelay * 2^attemptNumber
    const exponentialDelay = this.baseDelay * Math.pow(2, attemptNumber);
    
    // ADD JITTER (RANDOM VARIATION) TO PREVENT THUNDERING HERD
    const jitter = Math.random() * 0.3 * exponentialDelay;
    
    // CAP AT MAX DELAY
    const delay = Math.min(exponentialDelay + jitter, this.maxDelay);
    
    return delay;
  }
  
  /**
   * SLEEP FOR SPECIFIED DURATION
   */
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
  
  /**
   * EXECUTE FUNCTION WITH RETRY LOGIC
   * 
   * @param {Function} fn - ASYNC FUNCTION TO EXECUTE
   * @param {Object} context - CONTEXT FOR LOGGING
   * @returns {Promise} RESULT OF FUNCTION OR FINAL ERROR
   */
  async executeWithRetry(fn, context = {}) {
    let lastError = null;
    
    for (let attempt = 0; attempt <= this.maxRetries; attempt++) {
      try {
        // ATTEMPT EXECUTION
        logger.debug(`RETRY ATTEMPT ${attempt + 1}/${this.maxRetries + 1}`, context);
        
        const result = await fn();
        
        // SUCCESS - RETURN RESULT
        if (attempt > 0) {
          logger.info(`RETRY SUCCEEDED ON ATTEMPT ${attempt + 1}`, context);
        }
        
        return result;
        
      } catch (error) {
        lastError = error;
        
        // CHECK IF WE SHOULD RETRY
        if (attempt < this.maxRetries && this.isRetryable(error)) {
          const delay = this.calculateDelay(attempt);
          
          logger.warn(`RETRY ATTEMPT ${attempt + 1} FAILED - RETRYING IN ${delay}ms`, {
            ...context,
            error: error.message || error.error,
            error_type: error.error_type
          });
          
          // WAIT BEFORE RETRYING
          await this.sleep(delay);
          
        } else {
          // NO MORE RETRIES OR ERROR NOT RETRYABLE
          if (attempt === this.maxRetries) {
            logger.error(`ALL RETRY ATTEMPTS EXHAUSTED`, {
              ...context,
              attempts: attempt + 1,
              final_error: error
            });
          } else {
            logger.debug(`ERROR NOT RETRYABLE`, {
              ...context,
              error_type: error.error_type
            });
          }
          
          // THROW FINAL ERROR
          throw error;
        }
      }
    }
    
    // SHOULD NEVER REACH HERE, BUT THROW LAST ERROR AS FALLBACK
    throw lastError;
  }
}

// EXPORT SINGLETON INSTANCE
module.exports = new RetryHandler();
