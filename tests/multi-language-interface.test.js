/**
 * ═══════════════════════════════════════════════════════════
 * MULTI-LANGUAGE INTERFACE INTEGRATION TEST
 * ═══════════════════════════════════════════════════════════
 * 
 * VALIDATES TASK 21 REQUIREMENTS:
 * - Language selector dropdown (COBOL/FORTRAN/PASCAL/BASIC)
 * - Dynamic form inputs based on selected language
 * - Language-specific parameter labels
 * - Calculation history panel
 * - Show which language processed each result
 * 
 * Requirements: 9.4, 13.3
 */

const axios = require('axios');

const BASE_URL = 'http://localhost:3001';

// TEST DATA FOR EACH LANGUAGE
const TEST_CASES = {
  cobol: {
    params: { principal: 200000, rate: 5.5, term: 30 },
    expectedSource: 'COBOL_LEGACY_ENGINE',
    expectedLanguage: 'cobol',
    expectedYear: 1959
  },
  fortran: {
    params: { velocity: 100, angle: 45, gravity: 9.81 },
    expectedSource: 'FORTRAN_LEGACY_ENGINE',
    expectedLanguage: 'fortran',
    expectedYear: 1957
  },
  pascal: {
    params: { income: 75000, bracket_rate: 22, bracket_threshold: 50000 },
    expectedSource: 'PASCAL_LEGACY_ENGINE',
    expectedLanguage: 'pascal',
    expectedYear: 1970
  },
  basic: {
    params: { principal: 10000, rate: 5, time: 10, compounds: 12 },
    expectedSource: 'BASIC_LEGACY_ENGINE',
    expectedLanguage: 'basic',
    expectedYear: 1983
  }
};

describe('MULTI-LANGUAGE INTERFACE INTEGRATION TESTS', () => {
  
  describe('LANGUAGE SELECTOR FUNCTIONALITY', () => {
    test('SHOULD LIST ALL FOUR SUPPORTED LANGUAGES', async () => {
      const response = await axios.get(`${BASE_URL}/api/languages`);
      
      expect(response.status).toBe(200);
      expect(response.data.count).toBe(4);
      expect(response.data.languages).toHaveLength(4);
      
      const languageIds = response.data.languages.map(lang => lang.id);
      expect(languageIds).toContain('cobol');
      expect(languageIds).toContain('fortran');
      expect(languageIds).toContain('pascal');
      expect(languageIds).toContain('basic');
    });
    
    test('SHOULD PROVIDE METADATA FOR EACH LANGUAGE', async () => {
      const response = await axios.get(`${BASE_URL}/api/languages`);
      
      response.data.languages.forEach(lang => {
        expect(lang).toHaveProperty('id');
        expect(lang).toHaveProperty('name');
        expect(lang).toHaveProperty('year');
        expect(lang).toHaveProperty('description');
        expect(lang).toHaveProperty('parameters');
        expect(Array.isArray(lang.parameters)).toBe(true);
      });
    });
  });
  
  describe('DYNAMIC FORM INPUTS - LANGUAGE-SPECIFIC PARAMETERS', () => {
    test('COBOL SHOULD HAVE MORTGAGE-SPECIFIC PARAMETERS', async () => {
      const response = await axios.get(`${BASE_URL}/api/languages`);
      const cobol = response.data.languages.find(lang => lang.id === 'cobol');
      
      expect(cobol.parameters).toEqual(['principal', 'rate', 'term']);
      expect(cobol.description).toContain('MORTGAGE');
    });
    
    test('FORTRAN SHOULD HAVE TRAJECTORY-SPECIFIC PARAMETERS', async () => {
      const response = await axios.get(`${BASE_URL}/api/languages`);
      const fortran = response.data.languages.find(lang => lang.id === 'fortran');
      
      expect(fortran.parameters).toEqual(['velocity', 'angle', 'gravity']);
      expect(fortran.description).toContain('TRAJECTORY');
    });
    
    test('PASCAL SHOULD HAVE TAX-SPECIFIC PARAMETERS', async () => {
      const response = await axios.get(`${BASE_URL}/api/languages`);
      const pascal = response.data.languages.find(lang => lang.id === 'pascal');
      
      expect(pascal.parameters).toEqual(['income', 'bracket_rate', 'bracket_threshold']);
      expect(pascal.description).toContain('TAX');
    });
    
    test('BASIC SHOULD HAVE INTEREST-SPECIFIC PARAMETERS', async () => {
      const response = await axios.get(`${BASE_URL}/api/languages`);
      const basic = response.data.languages.find(lang => lang.id === 'basic');
      
      expect(basic.parameters).toEqual(['principal', 'rate', 'time', 'compounds']);
      expect(basic.description).toContain('INTEREST');
    });
  });
  
  describe('CALCULATION EXECUTION - ALL LANGUAGES', () => {
    Object.entries(TEST_CASES).forEach(([language, testCase]) => {
      test(`SHOULD EXECUTE ${language.toUpperCase()} CALCULATION SUCCESSFULLY`, async () => {
        const response = await axios.post(
          `${BASE_URL}/api/calculate/${language}`,
          testCase.params
        );
        
        expect(response.status).toBe(200);
        expect(response.data).toHaveProperty('result');
        expect(response.data).toHaveProperty('source');
        expect(response.data).toHaveProperty('language');
        expect(response.data).toHaveProperty('year');
        
        expect(response.data.source).toBe(testCase.expectedSource);
        expect(response.data.language).toBe(testCase.expectedLanguage);
        expect(response.data.year).toBe(testCase.expectedYear);
        expect(typeof response.data.result).toBe('number');
      });
    });
  });
  
  describe('CALCULATION HISTORY - SOURCE TRACKING', () => {
    test('SHOULD IDENTIFY WHICH LANGUAGE PROCESSED EACH RESULT', async () => {
      // EXECUTE CALCULATIONS IN ALL FOUR LANGUAGES
      const results = [];
      
      for (const [language, testCase] of Object.entries(TEST_CASES)) {
        const response = await axios.post(
          `${BASE_URL}/api/calculate/${language}`,
          testCase.params
        );
        
        results.push({
          language: response.data.language,
          source: response.data.source,
          year: response.data.year,
          result: response.data.result
        });
      }
      
      // VERIFY EACH RESULT HAS UNIQUE SOURCE IDENTIFIER
      expect(results).toHaveLength(4);
      
      const sources = results.map(r => r.source);
      expect(sources).toContain('COBOL_LEGACY_ENGINE');
      expect(sources).toContain('FORTRAN_LEGACY_ENGINE');
      expect(sources).toContain('PASCAL_LEGACY_ENGINE');
      expect(sources).toContain('BASIC_LEGACY_ENGINE');
      
      // VERIFY EACH RESULT HAS CORRECT YEAR
      const cobolResult = results.find(r => r.language === 'cobol');
      expect(cobolResult.year).toBe(1959);
      
      const fortranResult = results.find(r => r.language === 'fortran');
      expect(fortranResult.year).toBe(1957);
      
      const pascalResult = results.find(r => r.language === 'pascal');
      expect(pascalResult.year).toBe(1970);
      
      const basicResult = results.find(r => r.language === 'basic');
      expect(basicResult.year).toBe(1983);
    });
  });
  
  describe('ERROR HANDLING - INVALID LANGUAGE', () => {
    test('SHOULD REJECT UNSUPPORTED LANGUAGE', async () => {
      try {
        await axios.post(
          `${BASE_URL}/api/calculate/ada`,
          { param1: 100 }
        );
        fail('SHOULD HAVE THROWN ERROR');
      } catch (error) {
        expect(error.response.status).toBe(400);
        expect(error.response.data.error).toBe('UNSUPPORTED LANGUAGE');
      }
    });
  });
  
  describe('BACKWARD COMPATIBILITY', () => {
    test('SHOULD SUPPORT ORIGINAL /api/calculate ENDPOINT FOR COBOL', async () => {
      const response = await axios.post(
        `${BASE_URL}/api/calculate`,
        { principal: 200000, rate: 5.5, term: 30 }
      );
      
      expect(response.status).toBe(200);
      expect(response.data).toHaveProperty('monthly_payment');
      expect(response.data).toHaveProperty('source');
      expect(response.data.source).toBe('COBOL_LEGACY_ENGINE');
    });
  });
});
