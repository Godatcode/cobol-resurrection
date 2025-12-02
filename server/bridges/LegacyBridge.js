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
   * @returns {Promise<Object>} CALCULATION RESULT OR ERROR
   */
  async execute(params) {
    return new Promise((resolve, reject) => {
      // VALIDATE PARAMETERS
      const validation = this.validateParams(params);
      if (!validation.valid) {
        return reject({
          error: 'INVALID INPUT',
          details: validation.error,
          language: this.name.toLowerCase()
        });
      }
      
      // BUILD COMMAND
      const command = this.buildCommand(params);
      
      // SPAWN LEGACY PROCESS WITH TIMEOUT
      exec(command, { timeout: this.timeout }, (error, stdout, stderr) => {
        // HANDLE EXECUTION ERRORS
        if (error) {
          // TIMEOUT ERROR
          if (error.killed) {
            return reject({
              error: 'CORE DUMP DETECTED',
              details: `${this.name} PROCESS EXCEEDED ${this.timeout / 1000} SECOND TIMEOUT`,
              language: this.name.toLowerCase()
            });
          }
          
          // BINARY NOT FOUND OR NON-ZERO EXIT CODE
          return reject({
            error: 'CORE DUMP DETECTED',
            details: stderr || error.message,
            language: this.name.toLowerCase()
          });
        }
        
        // PARSE OUTPUT USING SUBCLASS-SPECIFIC PARSER
        const parseResult = this.parseOutput(stdout);
        
        if (!parseResult.success) {
          return reject({
            error: 'CORE DUMP DETECTED',
            details: parseResult.error || `FAILED TO PARSE ${this.name} OUTPUT`,
            language: this.name.toLowerCase(),
            raw_output: stdout
          });
        }
        
        // RETURN SUCCESSFUL RESPONSE
        resolve({
          result: parseResult.result,
          source: `${this.name}_LEGACY_ENGINE`,
          language: this.name.toLowerCase(),
          year: this.year,
          calculation: this.description
        });
      });
    });
  }
}

module.exports = LegacyBridge;
