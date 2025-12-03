/**
 * ═══════════════════════════════════════════════════════════
 * MULTI-LANGUAGE CALCULATOR INTEGRATION TESTS
 * ═══════════════════════════════════════════════════════════
 * 
 * VALIDATES TASK 21: MULTI-LANGUAGE CALCULATOR INTERFACE
 * TESTS THE BACKEND API ENDPOINTS FOR ALL 4 LANGUAGES
 */

import { describe, it, expect } from 'vitest';
import axios from 'axios';

const API_BASE = 'http://localhost:3001';

describe('MULTI-LANGUAGE CALCULATOR API INTEGRATION', () => {
  it('SHOULD LIST ALL 4 SUPPORTED LANGUAGES', async () => {
    const response = await axios.get(`${API_BASE}/api/languages`);
    
    expect(response.status).toBe(200);
    expect(response.data.count).toBe(4);
    expect(response.data.languages).toHaveLength(4);
    
    const languageIds = response.data.languages.map(lang => lang.id);
    expect(languageIds).toContain('cobol');
    expect(languageIds).toContain('fortran');
    expect(languageIds).toContain('pascal');
    expect(languageIds).toContain('basic');
  });

  it('SHOULD RETURN LANGUAGE METADATA WITH PARAMETERS', async () => {
    const response = await axios.get(`${API_BASE}/api/languages`);
    
    const cobol = response.data.languages.find(lang => lang.id === 'cobol');
    expect(cobol.name).toBe('COBOL');
    expect(cobol.year).toBe(1959);
    expect(cobol.description).toBe('MORTGAGE PAYMENT CALCULATOR');
    expect(cobol.parameters).toEqual(['principal', 'rate', 'term']);
    
    const fortran = response.data.languages.find(lang => lang.id === 'fortran');
    expect(fortran.name).toBe('FORTRAN');
    expect(fortran.year).toBe(1957);
    expect(fortran.parameters).toEqual(['velocity', 'angle', 'gravity']);
    
    const pascal = response.data.languages.find(lang => lang.id === 'pascal');
    expect(pascal.name).toBe('PASCAL');
    expect(pascal.year).toBe(1970);
    expect(pascal.parameters).toEqual(['income', 'bracket_rate', 'bracket_threshold']);
    
    const basic = response.data.languages.find(lang => lang.id === 'basic');
    expect(basic.name).toBe('BASIC');
    expect(basic.year).toBe(1983);
    expect(basic.parameters).toEqual(['principal', 'rate', 'time', 'compounds']);
  });

  it('SHOULD CALCULATE MORTGAGE USING COBOL ENDPOINT', async () => {
    const response = await axios.post(`${API_BASE}/api/calculate/cobol`, {
      principal: 200000,
      rate: 5.5,
      term: 30
    });
    
    expect(response.status).toBe(200);
    expect(response.data.result).toBeCloseTo(1135.57, 1);
    expect(response.data.source).toBe('COBOL_LEGACY_ENGINE');
    expect(response.data.language).toBe('cobol');
    expect(response.data.year).toBe(1959);
  });

  it('SHOULD CALCULATE TRAJECTORY USING FORTRAN ENDPOINT', async () => {
    const response = await axios.post(`${API_BASE}/api/calculate/fortran`, {
      velocity: 100,
      angle: 45,
      gravity: 9.81
    });
    
    expect(response.status).toBe(200);
    expect(response.data.result).toBeCloseTo(1019.37, 1);
    expect(response.data.source).toBe('FORTRAN_LEGACY_ENGINE');
    expect(response.data.language).toBe('fortran');
    expect(response.data.year).toBe(1957);
  });

  it('SHOULD CALCULATE TAX USING PASCAL ENDPOINT', async () => {
    const response = await axios.post(`${API_BASE}/api/calculate/pascal`, {
      income: 75000,
      bracket_rate: 22,
      bracket_threshold: 50000
    });
    
    expect(response.status).toBe(200);
    expect(response.data.result).toBe(5500);
    expect(response.data.source).toBe('PASCAL_LEGACY_ENGINE');
    expect(response.data.language).toBe('pascal');
    expect(response.data.year).toBe(1970);
  });

  it('SHOULD CALCULATE COMPOUND INTEREST USING BASIC ENDPOINT', async () => {
    const response = await axios.post(`${API_BASE}/api/calculate/basic`, {
      principal: 10000,
      rate: 5,
      time: 10,
      compounds: 12
    });
    
    expect(response.status).toBe(200);
    expect(response.data.result).toBeCloseTo(18193.97, 1);
    expect(response.data.source).toBe('BASIC_LEGACY_ENGINE');
    expect(response.data.language).toBe('basic');
    expect(response.data.year).toBe(1983);
  });

  it('SHOULD RETURN ERROR FOR UNSUPPORTED LANGUAGE', async () => {
    try {
      await axios.post(`${API_BASE}/api/calculate/python`, {
        x: 10,
        y: 20
      });
      expect.fail('Should have thrown error');
    } catch (error) {
      expect(error.response.status).toBe(400);
      expect(error.response.data.error).toBe('UNSUPPORTED LANGUAGE');
    }
  });

  it('SHOULD INCLUDE TIMESTAMP IN RESPONSE', async () => {
    const response = await axios.post(`${API_BASE}/api/calculate/cobol`, {
      principal: 100000,
      rate: 4.5,
      term: 15
    });
    
    expect(response.data.timestamp).toBeDefined();
    const timestamp = new Date(response.data.timestamp);
    expect(timestamp.getTime()).toBeGreaterThan(Date.now() - 5000); // Within last 5 seconds
  });

  it('SHOULD MAINTAIN BACKWARD COMPATIBILITY WITH ORIGINAL COBOL ENDPOINT', async () => {
    const response = await axios.post(`${API_BASE}/api/calculate`, {
      principal: 200000,
      rate: 5.5,
      term: 30
    });
    
    expect(response.status).toBe(200);
    expect(response.data.monthly_payment).toBeCloseTo(1135.57, 1);
    expect(response.data.source).toBe('COBOL_LEGACY_ENGINE');
  });
});
