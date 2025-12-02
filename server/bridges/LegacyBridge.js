/**
 * ═══════════════════════════════════════════════════════════
 * ABSTRACT LEGACY BRIDGE CLASS
 * ═══════════════════════════════════════════════════════════
 * 
 * UNIVERSAL PATTERN FOR RESURRECTING ANCIENT COMPUTING SPIRITS
 * PROVIDES STANDARDIZED INTERFACE FOR SPAWNING AND COMMUNICATING
 * WITH COMPILED LEGACY BINARIES FROM MULTIPLE ERAS (1950s-1980s)
 * 
 * DESIGN PATTERN: BRIDGE PATTERN
 * - SEPARATES ABSTRACTION FROM IMPLEMENTATION
 * - ENABLES ADDING NEW LEGACY LANGUAGES WITHOUT MODIFYING CORE LOGIC
 * - TREATS VINTAGE CODE AS MICROSERVICES
 */

const { exec } = require('child_process');
const path = require('path');
const logger = require('../utils/logger');
const errorHandler = require('../utils/errorHandler');
const retryHandler = require('../utils/retryHandler');

class LegacyBridge {
  /**
   * CONSTRUCT A NEW LEGACY BRIDGE INSTANCE
   * 
   * @param {Object} config - LANGUAGE CONFIGURATION
   * @param {string} config.name - LANGUAGE NAME (e.g., "COBOL")
   * @param {number} config.year - YEAR OF ORIGIN (e.g., 1959)
   * @param {string} config.binary - BINARY FILENAME (e.g., "mortgage")
   * @param {string} config.description - CALCULATION DESCRIPTION
   * @param {Array<string>} config.params - REQUIRED PARAMETER NAMES
   */
  constructor(config) {
    if (this.constructor === LegacyBridge) {
      throw new Error('CANNOT INSTANTIATE ABSTRACT CLASS LegacyBridge DIRECTLY');
    }
    
    this.config = config;
    this.name = config.name;
    this.year = config.year;
    this.binary = config.binary;
    this.description = config.description;
    this.params = config.params;
    this.timeout = 5000; // 5 SECOND TIMEOUT FOR ALL LEGACY PROCESSES
  }
  
  /**
   * VALIDATE INPUT PARAMETERS AGAINST LANGUAGE REQUIREMENTS
   * 
   * @param {Object} params - INPUT PARAMETERS FROM REQUEST
   * @returns {Object} { valid: boolean, error: string|null }
   */
  validateParams(params) {
    // CHECK FOR MISSING PARAMETERS
    const missingParams = this.params.filter(param => params[param] === undefined);
    if (missingParams.length > 0) {
      return {
        valid: false,
        error: `MISSING PARAMETERS: ${missingParams.join(', ')}`
      };
    }
    
    // CHECK PARAMETER TYPES AND VALUES
    for (const param of this.params) {
      if (typeof params[param] !== 'number') {
        return {
          valid: false,
          error: `PARAMETER '${param}' MUST BE NUMERIC`
        };
      }
      
      if (params[param] <= 0) {
        return {
          valid: false,
          error: `PARAMETER '${param}' MUST BE POSITIVE`
        };
      }
    }
    
    return { valid: true, error: null };
  }
  
  /**
   * GET PATH TO LEGACY BINARY
   * SUBCLASSES CAN OVERRIDE TO CUSTOMIZE BINARY LOCATION
   * 
   * @returns {string} ABSOLUTE PATH TO BINARY
   */
  getBinaryPath() {
    // DEFAULT: /legacy/<language>/<binary>
    const languageDir = this.name.toLowerCase();
    return path.join(__dirname, '..', '..', 'legacy', languageDir, this.binary);
  }
  
  /**
   * BUILD COMMAND STRING WITH PARAMETERS
   * SUBCLASSES CAN OVERRIDE TO CUSTOMIZE PARAMETER FORMATTING
   * 
   * @param {Object} params - INPUT PARAMETERS
   * @returns {string} COMMAND STRING TO EXECUTE
   */
  buildCommand(params) {
    const binaryPath = this.getBinaryPath();
    const paramValues = this.params.map(param => params[param]).join(' ');
    return `${binaryPath} ${paramValues}`;
  }
  
  /**
   * PARSE OUTPUT FROM LEGACY BINARY
   * SUBCLASSES MUST OVERRIDE THIS METHOD
   * 
   * @param {string} stdout - STANDARD OUTPUT FROM PROCESS
   * @returns {Object} { success: boolean, result: number|null, error: string|null }
   */
  parseOutput(stdout) {
    throw new Error('SUBCLASSES MUST IMPLEMENT parseOutput() METHOD');
  }
  
  /**
   * SPAWN LEGACY PROCESS AND EXECUTE CALCULATION
   * UNIVERSAL METHOD - HANDLES PROCESS SPAWNING, TIMEOUT, ERROR HANDLING
   * 
   * @param {Object} params - INPUT PARAMETERS
   * @param {Object} options - EXECUTION OPTIONS
   * @param {boolean} options.enableRetry - ENABLE RETRY LOGIC (DEFAULT: false)
   * @returns {Promise<Object>} CALCULATION RESULT OR ERROR
   */
  async execute(params, options = {}) {
    const { enableRetry = false } = options;
    
    // IF RETRY IS ENABLED, WRAP EXECUTION IN RETRY HANDLER
    if (enableRetry) {
      return retryHandler.executeWithRetry(
        () => this._executeOnce(params),
        { language: this.name, params }
      );
    }
    
    // OTHERWISE, EXECUTE ONCE
    return this._executeOnce(params);
  }
  
  /**
   * EXECUTE CALCULATION ONCE (WITHOUT RETRY)
   * INTERNAL METHOD CALLED BY execute()
   * 
   * @param {Object} params - INPUT PARAMETERS
   * @returns {Promise<Object>} CALCULATION RESULT OR ERROR
   */
  async _executeOnce(params) {
    return new Promise((resolve, reject) => {
      // LOG EXECUTION START
      logger.info(`EXECUTING ${this.name} CALCULATION`, {
        language: this.name,
        params: params
      });
      
      // VALIDATE PARAMETERS
      const validation = this.validateParams(params);
      if (!validation.valid) {
        logger.warn(`VALIDATION FAILED for ${this.name}`, {
          error: validation.error,
          params: params
        });
        
        const validationError = errorHandler.handleValidationError(validation.error, this.name);
        return reject(validationError);
      }
      
      // BUILD COMMAND
      const command = this.buildCommand(params);
      
      // LOG PROCESS SPAWNING
      logger.logProcessExecution(this.name, command, params);
      
      // SPAWN LEGACY PROCESS WITH TIMEOUT
      exec(command, { timeout: this.timeout }, (error, stdout, stderr) => {
        // HANDLE EXECUTION ERRORS
        if (error) {
          // ATTACH STDERR TO ERROR OBJECT
          error.stderr = stderr;
          
          // GENERATE COMPREHENSIVE ERROR RESPONSE
          const errorResponse = errorHandler.generateErrorResponse(error, this.name, params);
          
          // LOG PROCESS FAILURE
          logger.logProcessResult(this.name, false, null, errorResponse);
          
          return reject(errorResponse);
        }
        
        // PARSE OUTPUT USING SUBCLASS-SPECIFIC PARSER
        const parseResult = this.parseOutput(stdout);
        
        if (!parseResult.success) {
          // CREATE ERROR OBJECT FOR PARSING FAILURE
          const parseError = new Error(parseResult.error || `FAILED TO PARSE ${this.name} OUTPUT`);
          parseError.raw_output = stdout;
          parseError.stderr = stderr;
          
          // GENERATE COMPREHENSIVE ERROR RESPONSE
          const errorResponse = errorHandler.generateErrorResponse(parseError, this.name, params);
          
          // LOG PROCESS FAILURE
          logger.logProcessResult(this.name, false, null, errorResponse);
          
          return reject(errorResponse);
        }
        
        // BUILD SUCCESSFUL RESPONSE
        const result = {
          result: parseResult.result,
          source: `${this.name}_LEGACY_ENGINE`,
          language: this.name.toLowerCase(),
          year: this.year,
          calculation: this.description,
          timestamp: new Date().toISOString()
        };
        
        // LOG PROCESS SUCCESS
        logger.logProcessResult(this.name, true, result);
        
        // RETURN SUCCESSFUL RESPONSE
        resolve(result);
      });
    });
  }
}

module.exports = LegacyBridge;
