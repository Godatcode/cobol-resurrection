/**
 * ═══════════════════════════════════════════════════════════
 * BRIDGE FACTORY - UNIVERSAL SPIRIT SUMMONING MECHANISM
 * ═══════════════════════════════════════════════════════════
 * 
 * FACTORY PATTERN FOR INSTANTIATING LANGUAGE-SPECIFIC BRIDGES
 * CENTRALIZES BRIDGE CREATION AND LANGUAGE DETECTION
 * ENABLES EASY ADDITION OF NEW LEGACY LANGUAGES
 */

const CobolBridge = require('./CobolBridge');
const FortranBridge = require('./FortranBridge');
const PascalBridge = require('./PascalBridge');
const BasicBridge = require('./BasicBridge');

class BridgeFactory {
  constructor() {
    // REGISTRY OF AVAILABLE BRIDGES
    this.bridges = {
      cobol: CobolBridge,
      fortran: FortranBridge,
      pascal: PascalBridge,
      basic: BasicBridge
    };
    
    // CACHE OF INSTANTIATED BRIDGES (SINGLETON PATTERN)
    this.instances = {};
  }
  
  /**
   * GET LIST OF SUPPORTED LANGUAGES
   * 
   * @returns {Array<string>} ARRAY OF LANGUAGE IDENTIFIERS
   */
  getSupportedLanguages() {
    return Object.keys(this.bridges);
  }
  
  /**
   * CHECK IF LANGUAGE IS SUPPORTED
   * 
   * @param {string} language - LANGUAGE IDENTIFIER (e.g., "cobol")
   * @returns {boolean} TRUE IF LANGUAGE IS SUPPORTED
   */
  isSupported(language) {
    return this.bridges.hasOwnProperty(language.toLowerCase());
  }
  
  /**
   * CREATE OR RETRIEVE BRIDGE INSTANCE FOR SPECIFIED LANGUAGE
   * USES SINGLETON PATTERN - EACH BRIDGE IS INSTANTIATED ONCE
   * 
   * @param {string} language - LANGUAGE IDENTIFIER (e.g., "cobol")
   * @returns {LegacyBridge} BRIDGE INSTANCE
   * @throws {Error} IF LANGUAGE IS NOT SUPPORTED
   */
  getBridge(language) {
    const languageKey = language.toLowerCase();
    
    // VALIDATE LANGUAGE IS SUPPORTED
    if (!this.isSupported(languageKey)) {
      throw new Error(
        `UNSUPPORTED LANGUAGE: ${language}. ` +
        `SUPPORTED LANGUAGES: ${this.getSupportedLanguages().join(', ')}`
      );
    }
    
    // RETURN CACHED INSTANCE IF EXISTS
    if (this.instances[languageKey]) {
      return this.instances[languageKey];
    }
    
    // INSTANTIATE NEW BRIDGE
    const BridgeClass = this.bridges[languageKey];
    this.instances[languageKey] = new BridgeClass();
    
    return this.instances[languageKey];
  }
  
  /**
   * GET METADATA FOR ALL SUPPORTED LANGUAGES
   * 
   * @returns {Array<Object>} ARRAY OF LANGUAGE METADATA
   */
  getLanguageMetadata() {
    return this.getSupportedLanguages().map(lang => {
      const bridge = this.getBridge(lang);
      return {
        id: lang,
        name: bridge.name,
        year: bridge.year,
        description: bridge.description,
        parameters: bridge.params
      };
    });
  }
}

// EXPORT SINGLETON INSTANCE
module.exports = new BridgeFactory();
