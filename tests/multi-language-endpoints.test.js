/**
 * ═══════════════════════════════════════════════════════════
 * MULTI-LANGUAGE API ENDPOINT TESTS
 * ═══════════════════════════════════════════════════════════
 * 
 * VALIDATES ALL LANGUAGE-SPECIFIC ENDPOINTS
 * REQUIREMENTS: 2.1, 9.2, 9.4
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { spawn } from 'child_process';

const BASE_URL = 'http://localhost:3001';
let serverProcess;

// START SERVER BEFORE TESTS
beforeAll(async () => {
  return new Promise((resolve) => {
    serverProcess = spawn('node', ['server/server.js']);
    
    // WAIT FOR SERVER TO START
    setTimeout(resolve, 2000);
  });
});

// STOP SERVER AFTER TESTS
afterAll(() => {
  if (serverProcess) {
    serverProcess.kill();
  }
});

// HELPER FUNCTION TO MAKE API REQUESTS
async function makeRequest(endpoint, data) {
  const response = await fetch(`${BASE_URL}${endpoint}`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(data),
  });
  
  return {
    status: response.status,
    data: await response.json(),
  };
}

describe('GET /api/languages', () => {
  it('should return list of all available languages', async () => {
    const response = await fetch(`${BASE_URL}/api/languages`);
    const data = await response.json();
    
    expect(response.status).toBe(200);
    expect(data.count).toBe(4);
    expect(data.languages).toHaveLength(4);
    
    // VERIFY EACH LANGUAGE HAS REQUIRED METADATA
    const languageIds = data.languages.map(lang => lang.id);
    expect(languageIds).toContain('cobol');
    expect(languageIds).toContain('fortran');
    expect(languageIds).toContain('pascal');
    expect(languageIds).toContain('basic');
    
    // VERIFY METADATA STRUCTURE
    data.languages.forEach(lang => {
      expect(lang).toHaveProperty('id');
      expect(lang).toHaveProperty('name');
      expect(lang).toHaveProperty('year');
      expect(lang).toHaveProperty('description');
      expect(lang).toHaveProperty('parameters');
    });
  });
});

describe('POST /api/calculate/cobol', () => {
  it('should calculate mortgage payment using COBOL engine', async () => {
    const result = await makeRequest('/api/calculate/cobol', {
      principal: 200000,
      rate: 5.5,
      term: 30
    });
    
    expect(result.status).toBe(200);
    expect(result.data).toHaveProperty('result');
    expect(result.data).toHaveProperty('source', 'COBOL_LEGACY_ENGINE');
    expect(result.data).toHaveProperty('language', 'cobol');
    expect(result.data).toHaveProperty('year', 1959);
    expect(result.data.result).toBeCloseTo(1135.57, 1);
  });
  
  it('should handle invalid COBOL inputs', async () => {
    const result = await makeRequest('/api/calculate/cobol', {
      principal: -1000,
      rate: 5.5,
      term: 30
    });
    
    // SERVER RETURNS 400 FOR VALIDATION ERRORS, 500 FOR EXECUTION ERRORS
    expect([400, 500]).toContain(result.status);
    expect(result.data).toHaveProperty('error');
  });
});

describe('POST /api/calculate/fortran', () => {
  it('should calculate trajectory using FORTRAN engine', async () => {
    const result = await makeRequest('/api/calculate/fortran', {
      velocity: 100,
      angle: 45,
      gravity: 9.8
    });
    
    expect(result.status).toBe(200);
    expect(result.data).toHaveProperty('result');
    expect(result.data).toHaveProperty('source', 'FORTRAN_LEGACY_ENGINE');
    expect(result.data).toHaveProperty('language', 'fortran');
    expect(result.data).toHaveProperty('year', 1957);
    expect(result.data.result).toBeCloseTo(1020.41, 1);
  });
  
  it('should handle invalid FORTRAN inputs', async () => {
    const result = await makeRequest('/api/calculate/fortran', {
      velocity: -100,
      angle: 45,
      gravity: 9.8
    });
    
    // SERVER RETURNS 400 FOR VALIDATION ERRORS, 500 FOR EXECUTION ERRORS
    expect([400, 500]).toContain(result.status);
    expect(result.data).toHaveProperty('error');
  });
});

describe('POST /api/calculate/pascal', () => {
  it('should calculate tax using PASCAL engine', async () => {
    const result = await makeRequest('/api/calculate/pascal', {
      income: 75000,
      bracket_rate: 25,
      bracket_threshold: 50000
    });
    
    expect(result.status).toBe(200);
    expect(result.data).toHaveProperty('result');
    expect(result.data).toHaveProperty('source', 'PASCAL_LEGACY_ENGINE');
    expect(result.data).toHaveProperty('language', 'pascal');
    expect(result.data).toHaveProperty('year', 1970);
    expect(result.data.result).toBe(6250);
  });
  
  it('should handle invalid PASCAL inputs', async () => {
    const result = await makeRequest('/api/calculate/pascal', {
      income: -75000,
      bracket_rate: 25,
      bracket_threshold: 50000
    });
    
    // SERVER RETURNS 400 FOR VALIDATION ERRORS, 500 FOR EXECUTION ERRORS
    expect([400, 500]).toContain(result.status);
    expect(result.data).toHaveProperty('error');
  });
});

describe('POST /api/calculate/basic', () => {
  it('should calculate compound interest using BASIC engine', async () => {
    const result = await makeRequest('/api/calculate/basic', {
      principal: 10000,
      rate: 5,
      time: 10,
      compounds: 12
    });
    
    expect(result.status).toBe(200);
    expect(result.data).toHaveProperty('result');
    expect(result.data).toHaveProperty('source', 'BASIC_LEGACY_ENGINE');
    expect(result.data).toHaveProperty('language', 'basic');
    expect(result.data).toHaveProperty('year', 1983);
    expect(result.data.result).toBeCloseTo(18193.97, 1);
  });
  
  it('should handle invalid BASIC inputs', async () => {
    const result = await makeRequest('/api/calculate/basic', {
      principal: -10000,
      rate: 5,
      time: 10,
      compounds: 12
    });
    
    // SERVER RETURNS 400 FOR VALIDATION ERRORS, 500 FOR EXECUTION ERRORS
    expect([400, 500]).toContain(result.status);
    expect(result.data).toHaveProperty('error');
  });
});

describe('Language Detection Middleware', () => {
  it('should reject unsupported language', async () => {
    const result = await makeRequest('/api/calculate/python', {
      value: 100
    });
    
    expect(result.status).toBe(400);
    expect(result.data).toHaveProperty('error', 'UNSUPPORTED LANGUAGE');
  });
});
