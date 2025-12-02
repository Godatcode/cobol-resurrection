/**
 * ═══════════════════════════════════════════════════════════
 * PASCAL BRIDGE - RESURRECTION OF 1970 STRUCTURED PROGRAMMING
 * ═══════════════════════════════════════════════════════════
 * 
 * SUMMONS THE ANCIENT SPIRIT OF PASCAL
 * DEVELOPED BY NIKLAUS WIRTH FOR TEACHING STRUCTURED PROGRAMMING
 * NAMED AFTER MATHEMATICIAN BLAISE PASCAL
 * 
 * CALCULATION: PROGRESSIVE TAX CALCULATION WITH BRACKETS
 */

const LegacyBridge = require('./LegacyBridge');
const path = require('path');

class PascalBridge extends LegacyBridge {
  constructor() {
    super({
      name: 'PASCAL',
      year: 1970,
      binary: 'tax',
      description: 'PROGRESSIVE TAX CALCULATOR',
      params: ['income', 'bracket_rate', 'bracket_threshold']
    });
  }
  
  /**
   * OVERRIDE BINARY PATH FOR PASCAL
   * PASCAL BINARY LOCATED IN /legacy/pascal/
   */
  getBinaryPath() {
    return path.join(__dirname, '..', '..', 'legacy', 'pascal', this.binary);
  }
  
  /**
   * PARSE PASCAL OUTPUT FORMAT: "RESULT: XXXX.XX"
   * PASCAL USES REAL TYPE FOR FLOATING-POINT NUMBERS
   * 
   * @param {string} stdout - STANDARD OUTPUT FROM PASCAL BINARY
   * @returns {Object} { success: boolean, result: number|null, error: string|null }
   */
  parseOutput(stdout) {
    // PASCAL OUTPUT PATTERN: "RESULT: " FOLLOWED BY DECIMAL NUMBER
    const resultPattern = /RESULT:\s*([\d.]+)/;
    const match = stdout.match(resultPattern);
    
    if (!match) {
      return {
        success: false,
        result: null,
        error: 'PASCAL OUTPUT DOES NOT MATCH EXPECTED FORMAT "RESULT: <number>"'
      };
    }
    
    const result = parseFloat(match[1]);
    
    // VALIDATE RESULT IS A VALID NUMBER
    if (isNaN(result)) {
      return {
        success: false,
        result: null,
        error: 'PASCAL OUTPUT CONTAINS INVALID NUMERIC VALUE'
      };
    }
    
    return {
      success: true,
      result: result,
      error: null
    };
  }
}

module.exports = PascalBridge;
