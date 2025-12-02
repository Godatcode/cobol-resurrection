/**
 * ═══════════════════════════════════════════════════════════
 * FORTRAN BRIDGE - RESURRECTION OF 1957 SCIENTIFIC COMPUTING
 * ═══════════════════════════════════════════════════════════
 * 
 * SUMMONS THE ANCIENT SPIRIT OF FORTRAN (FORMULA TRANSLATION)
 * THE FIRST HIGH-LEVEL PROGRAMMING LANGUAGE
 * DEVELOPED BY IBM FOR SCIENTIFIC AND ENGINEERING CALCULATIONS
 * 
 * CALCULATION: BALLISTIC TRAJECTORY
 * range = (v² × sin(2θ)) / g
 */

const LegacyBridge = require('./LegacyBridge');
const path = require('path');

class FortranBridge extends LegacyBridge {
  constructor() {
    super({
      name: 'FORTRAN',
      year: 1957,
      binary: 'trajectory',
      description: 'BALLISTIC TRAJECTORY CALCULATOR',
      params: ['velocity', 'angle', 'gravity']
    });
  }
  
  /**
   * OVERRIDE BINARY PATH FOR FORTRAN
   * FORTRAN BINARY LOCATED IN /legacy/fortran/
   */
  getBinaryPath() {
    return path.join(__dirname, '..', '..', 'legacy', 'fortran', this.binary);
  }
  
  /**
   * PARSE FORTRAN OUTPUT FORMAT: "RESULT: XXXX.XX" OR "RESULT: XXXX.XXXX"
   * FORTRAN USES REAL*8 (DOUBLE PRECISION) FOR FLOATING-POINT
   * 
   * @param {string} stdout - STANDARD OUTPUT FROM FORTRAN BINARY
   * @returns {Object} { success: boolean, result: number|null, error: string|null }
   */
  parseOutput(stdout) {
    // FORTRAN OUTPUT PATTERN: "RESULT: " FOLLOWED BY DECIMAL NUMBER
    // MORE FLEXIBLE THAN COBOL - ALLOWS VARIABLE DECIMAL PLACES
    const resultPattern = /RESULT:\s*([\d.]+)/;
    const match = stdout.match(resultPattern);
    
    if (!match) {
      return {
        success: false,
        result: null,
        error: 'FORTRAN OUTPUT DOES NOT MATCH EXPECTED FORMAT "RESULT: <number>"'
      };
    }
    
    const result = parseFloat(match[1]);
    
    // VALIDATE RESULT IS A VALID NUMBER
    if (isNaN(result)) {
      return {
        success: false,
        result: null,
        error: 'FORTRAN OUTPUT CONTAINS INVALID NUMERIC VALUE'
      };
    }
    
    return {
      success: true,
      result: result,
      error: null
    };
  }
}

module.exports = FortranBridge;
