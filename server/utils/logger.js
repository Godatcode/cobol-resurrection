/**
 * ═══════════════════════════════════════════════════════════
 * LOGGING UTILITY - MAINFRAME-STYLE REQUEST/RESPONSE TRACKING
 * ═══════════════════════════════════════════════════════════
 * 
 * PROVIDES COMPREHENSIVE LOGGING FOR ALL BRIDGE SERVER OPERATIONS
 * TRACKS REQUEST/RESPONSE CYCLES, ERRORS, AND LEGACY PROCESS EXECUTION
 * 
 * LOG LEVELS:
 * - INFO: GENERAL OPERATIONAL MESSAGES
 * - WARN: NON-CRITICAL ISSUES
 * - ERROR: CRITICAL FAILURES
 * - DEBUG: DETAILED DIAGNOSTIC INFORMATION
 */

class Logger {
  constructor() {
    this.enabled = true;
    this.logLevel = process.env.LOG_LEVEL || 'INFO';
    this.levels = {
      DEBUG: 0,
      INFO: 1,
      WARN: 2,
      ERROR: 3
    };
  }
  
  /**
   * CHECK IF LOG LEVEL SHOULD BE PRINTED
   */
  shouldLog(level) {
    return this.levels[level] >= this.levels[this.logLevel];
  }
  
  /**
   * FORMAT TIMESTAMP IN MAINFRAME STYLE
   */
  getTimestamp() {
    const now = new Date();
    return now.toISOString();
  }
  
  /**
   * FORMAT LOG MESSAGE WITH TIMESTAMP AND LEVEL
   */
  formatMessage(level, message, data = null) {
    const timestamp = this.getTimestamp();
    let formatted = `[${timestamp}] [${level}] ${message}`;
    
    if (data) {
      formatted += `\n${JSON.stringify(data, null, 2)}`;
    }
    
    return formatted;
  }
  
  /**
   * LOG INFO MESSAGE
   */
  info(message, data = null) {
    if (this.shouldLog('INFO')) {
      console.log(this.formatMessage('INFO', message, data));
    }
  }
  
  /**
   * LOG WARNING MESSAGE
   */
  warn(message, data = null) {
    if (this.shouldLog('WARN')) {
      console.warn(this.formatMessage('WARN', message, data));
    }
  }
  
  /**
   * LOG ERROR MESSAGE
   */
  error(message, data = null) {
    if (this.shouldLog('ERROR')) {
      console.error(this.formatMessage('ERROR', message, data));
    }
  }
  
  /**
   * LOG DEBUG MESSAGE
   */
  debug(message, data = null) {
    if (this.shouldLog('DEBUG')) {
      console.log(this.formatMessage('DEBUG', message, data));
    }
  }
  
  /**
   * LOG REQUEST DETAILS
   */
  logRequest(req) {
    const requestData = {
      method: req.method,
      path: req.path,
      language: req.legacyLanguage || 'N/A',
      params: req.body,
      ip: req.ip,
      userAgent: req.get('user-agent')
    };
    
    this.info(`INCOMING REQUEST: ${req.method} ${req.path}`, requestData);
  }
  
  /**
   * LOG RESPONSE DETAILS
   */
  logResponse(req, statusCode, responseData) {
    const logData = {
      method: req.method,
      path: req.path,
      language: req.legacyLanguage || 'N/A',
      statusCode: statusCode,
      response: responseData
    };
    
    if (statusCode >= 400) {
      this.error(`RESPONSE ERROR: ${req.method} ${req.path}`, logData);
    } else {
      this.info(`RESPONSE SUCCESS: ${req.method} ${req.path}`, logData);
    }
  }
  
  /**
   * LOG LEGACY PROCESS EXECUTION
   */
  logProcessExecution(language, command, params) {
    this.debug(`SPAWNING ${language} PROCESS`, {
      command: command,
      params: params
    });
  }
  
  /**
   * LOG LEGACY PROCESS RESULT
   */
  logProcessResult(language, success, result, error = null) {
    if (success) {
      this.info(`${language} PROCESS COMPLETED SUCCESSFULLY`, { result });
    } else {
      this.error(`${language} PROCESS FAILED`, { error });
    }
  }
}

// EXPORT SINGLETON INSTANCE
module.exports = new Logger();
