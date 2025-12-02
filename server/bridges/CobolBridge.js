/**
 * ═══════════════════════════════════════════════════════════
 * COBOL BRIDGE - RESURRECTION OF 1959 MAINFRAME COMPUTING
 * ═══════════════════════════════════════════════════════════
 * 
 * SUMMONS THE ANCIENT SPIRIT OF COBOL (COMMON BUSINESS-ORIENTED LANGUAGE)
 * DEVELOPED BY GRACE HOPPER AND THE CODASYL COMMITTEE
 * 
 * CALCULATION: MORTGAGE PAYMENT FORMULA
 * M = P [ i(1 + i)^n ] / [ (1 + i)^n – 1 ]
 */

const LegacyBridge = require('./LegacyBridge');

class CobolBridge extends LegacyBridge {
  constructor() {
    super({
      name: 'COBOL',
      year: 1959,
      binary: 'mortgage',
      description: 'MORTGAGE PAYMENT CALCULATOR',
      params: ['principal', 'rate', 'term']
    });
  }
  
  /**
   * PARSE COBOL OUTPUT FORMAT: "RESULT: XXXX.XX"
   * COBOL USES COMPUTATIONAL-2 (COMP-2) FOR FLOATING-POINT PRECISION
   * 
   * @param {string} stdout - STANDARD OUTPUT FROM COBOL BINARY
   * @returns {Object} { success: boolean, result: number|null, error: string|null }
   */
  parseOutput(stdout) {
    // COBOL OUTPUT PATTERN: "RESULT: " FOLLOWED BY DECIMAL NUMBER
    const resultPattern = /RESULT:\s*(\d+\.\d{2})/;
    const match = stdout.match(resultPattern);
    
    if (!match) {
      return {
        success: false,
        result: null,
        error: 'COBOL OUTPUT DOES NOT MATCH EXPECTED FORMAT "RESULT: XXXX.XX"'
      };
    }
    
    const result = parseFloat(match[1]);
    
    return {
      success: true,
      result: result,
      error: null
    };
  }
}

module.exports = CobolBridge;
