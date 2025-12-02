/**
 * ═══════════════════════════════════════════════════════════
 * BASIC BRIDGE - RESURRECTION OF 1983 HOME COMPUTING ERA
 * ═══════════════════════════════════════════════════════════
 * 
 * SUMMONS THE ANCIENT SPIRIT OF BASIC (BEGINNER'S ALL-PURPOSE SYMBOLIC INSTRUCTION CODE)
 * DEVELOPED AT DARTMOUTH COLLEGE BY JOHN KEMENY AND THOMAS KURTZ
 * POPULARIZED BY MICROSOFT BASIC IN THE 1980s HOME COMPUTER REVOLUTION
 * 
 * CALCULATION: COMPOUND INTEREST
 * A = P(1 + r/n)^(nt)
 */

const LegacyBridge = require('./LegacyBridge');
const path = require('path');

class BasicBridge extends LegacyBridge {
  constructor() {
    super({
      name: 'BASIC',
      year: 1983,
      binary: 'interest',
      description: 'COMPOUND INTEREST CALCULATOR',
      params: ['principal', 'rate', 'time', 'compounds']
    });
  }
  
  /**
   * OVERRIDE BINARY PATH FOR BASIC
   * BASIC BINARY LOCATED IN /legacy/basic/
   */
  getBinaryPath() {
    return path.join(__dirname, '..', '..', 'legacy', 'basic', this.binary);
  }
  
  /**
   * PARSE BASIC OUTPUT FORMAT: "RESULT: XXXX.XX"
   * BASIC USES SINGLE OR DOUBLE PRECISION FLOATING-POINT
   * 
   * @param {string} stdout - STANDARD OUTPUT FROM BASIC BINARY
   * @returns {Object} { success: boolean, result: number|null, error: string|null }
   */
  parseOutput(stdout) {
    // BASIC OUTPUT PATTERN: "RESULT: " FOLLOWED BY DECIMAL NUMBER
    const resultPattern = /RESULT:\s*([\d.]+)/;
    const match = stdout.match(resultPattern);
    
    if (!match) {
      return {
        success: false,
        result: null,
        error: 'BASIC OUTPUT DOES NOT MATCH EXPECTED FORMAT "RESULT: <number>"'
      };
    }
    
    const result = parseFloat(match[1]);
    
    // VALIDATE RESULT IS A VALID NUMBER
    if (isNaN(result)) {
      return {
        success: false,
        result: null,
        error: 'BASIC OUTPUT CONTAINS INVALID NUMERIC VALUE'
      };
    }
    
    return {
      success: true,
      result: result,
      error: null
    };
  }
}

module.exports = BasicBridge;
