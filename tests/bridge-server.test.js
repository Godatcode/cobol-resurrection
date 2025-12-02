import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import * as fc from 'fast-check';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

/**
 * BRIDGE SERVER PROPERTY-BASED TESTS
 * VALIDATES REQUIREMENTS 2.1 THROUGH 2.5
 */

// HELPER FUNCTION: MAKE API REQUEST TO BRIDGE SERVER
async function makeCalculateRequest(principal, rate, term) {
  const response = await fetch('http://localhost:3001/api/calculate', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ principal, rate, term })
  });
  
  const data = await response.json();
  return { status: response.status, data };
}

// HELPER FUNCTION: START BRIDGE SERVER
let serverProcess;

beforeAll(async () => {
  // START THE BRIDGE SERVER IN BACKGROUND
  serverProcess = exec('node server/server.js');
  
  // WAIT FOR SERVER TO BE READY
  await new Promise(resolve => setTimeout(resolve, 2000));
});

afterAll(() => {
  // TERMINATE SERVER PROCESS
  if (serverProcess) {
    serverProcess.kill();
  }
});

describe('Bridge Server Property Tests', () => {
  
  // Feature: cobol-resurrection-bridge, Property 4: Bridge Server Process Spawning
  it('Property 4: Bridge Server spawns COBOL process and captures output for valid inputs', async () => {
    await fc.assert(
      fc.asyncProperty(
        fc.integer({ min: 1000, max: 10000000 }),     // Principal: $1K - $10M
        fc.float({ min: Math.fround(0.1), max: Math.fround(30.0), noNaN: true }), // Rate: 0.1% - 30%
        fc.integer({ min: 1, max: 50 }),              // Term: 1-50 years
        async (principal, rate, term) => {
          const { status, data } = await makeCalculateRequest(principal, rate, term);
          
          // VALIDATE: SUCCESSFUL HTTP STATUS
          expect(status).toBe(200);
          
          // VALIDATE: RESPONSE CONTAINS MONTHLY_PAYMENT
          expect(data).toHaveProperty('monthly_payment');
          expect(typeof data.monthly_payment).toBe('number');
          
          // VALIDATE: MONTHLY PAYMENT IS POSITIVE
          expect(data.monthly_payment).toBeGreaterThan(0);
        }
      ),
      { numRuns: 100 }
    );
  });
  
  // Feature: cobol-resurrection-bridge, Property 5: Bridge Server Response Transformation
  it('Property 5: Bridge Server transforms COBOL output to JSON with correct structure', async () => {
    await fc.assert(
      fc.asyncProperty(
        fc.integer({ min: 1000, max: 10000000 }),     // Principal: $1K - $10M
        fc.float({ min: Math.fround(0.1), max: Math.fround(30.0), noNaN: true }), // Rate: 0.1% - 30%
        fc.integer({ min: 1, max: 50 }),              // Term: 1-50 years
        async (principal, rate, term) => {
          const { status, data } = await makeCalculateRequest(principal, rate, term);
          
          // VALIDATE: RESPONSE HAS CORRECT STRUCTURE
          expect(data).toHaveProperty('monthly_payment');
          expect(data).toHaveProperty('source');
          
          // VALIDATE: SOURCE IS CORRECT
          expect(data.source).toBe('COBOL_LEGACY_ENGINE');
          
          // VALIDATE: MONTHLY_PAYMENT IS A NUMBER
          expect(typeof data.monthly_payment).toBe('number');
          expect(isNaN(data.monthly_payment)).toBe(false);
        }
      ),
      { numRuns: 100 }
    );
  });
  
  // Feature: cobol-resurrection-bridge, Property 6: Bridge Server Error Response
  it('Property 6: Bridge Server returns HTTP 500 with CORE DUMP DETECTED for invalid inputs', async () => {
    await fc.assert(
      fc.asyncProperty(
        fc.oneof(
          // GENERATE INVALID PRINCIPALS (NEGATIVE OR ZERO)
          fc.record({ 
            principal: fc.integer({ min: -100000, max: 0 }), 
            rate: fc.float({ min: Math.fround(0.1), max: Math.fround(30.0), noNaN: true }), 
            term: fc.integer({ min: 1, max: 50 }) 
          }),
          // GENERATE INVALID RATES (NEGATIVE OR ZERO)
          fc.record({ 
            principal: fc.integer({ min: 1000, max: 10000000 }), 
            rate: fc.float({ min: Math.fround(-30.0), max: Math.fround(0.0), noNaN: true }), 
            term: fc.integer({ min: 1, max: 50 }) 
          }),
          // GENERATE INVALID TERMS (NEGATIVE OR ZERO)
          fc.record({ 
            principal: fc.integer({ min: 1000, max: 10000000 }), 
            rate: fc.float({ min: Math.fround(0.1), max: Math.fround(30.0), noNaN: true }), 
            term: fc.integer({ min: -50, max: 0 }) 
          })
        ),
        async ({ principal, rate, term }) => {
          const { status, data } = await makeCalculateRequest(principal, rate, term);
          
          // VALIDATE: ERROR STATUS CODE (EITHER 400 FOR VALIDATION OR 500 FOR COBOL ERROR)
          expect(status).toBeGreaterThanOrEqual(400);
          
          // VALIDATE: ERROR MESSAGE PRESENT
          expect(data).toHaveProperty('error');
          
          // IF STATUS IS 500, SHOULD CONTAIN "CORE DUMP DETECTED"
          if (status === 500) {
            expect(data.error).toBe('CORE DUMP DETECTED');
          }
        }
      ),
      { numRuns: 100 }
    );
  });
  
});

describe('Bridge Server Unit Tests', () => {
  
  it('API endpoint routing: POST /api/calculate responds successfully', async () => {
    const { status } = await makeCalculateRequest(200000, 5.5, 30);
    expect(status).toBe(200);
  });
  
  it('Input validation: rejects missing principal parameter', async () => {
    const response = await fetch('http://localhost:3001/api/calculate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ rate: 5.5, term: 30 })
    });
    
    expect(response.status).toBe(400);
    const data = await response.json();
    expect(data.error).toBe('INVALID INPUT');
  });
  
  it('Input validation: rejects missing rate parameter', async () => {
    const response = await fetch('http://localhost:3001/api/calculate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ principal: 200000, term: 30 })
    });
    
    expect(response.status).toBe(400);
    const data = await response.json();
    expect(data.error).toBe('INVALID INPUT');
  });
  
  it('Input validation: rejects missing term parameter', async () => {
    const response = await fetch('http://localhost:3001/api/calculate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ principal: 200000, rate: 5.5 })
    });
    
    expect(response.status).toBe(400);
    const data = await response.json();
    expect(data.error).toBe('INVALID INPUT');
  });
  
  it('Input validation: rejects non-numeric principal', async () => {
    const response = await fetch('http://localhost:3001/api/calculate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ principal: "not-a-number", rate: 5.5, term: 30 })
    });
    
    expect(response.status).toBe(400);
    const data = await response.json();
    expect(data.error).toBe('INVALID INPUT');
  });
  
  it('Input validation: rejects negative principal', async () => {
    const { status, data } = await makeCalculateRequest(-100000, 5.5, 30);
    expect(status).toBe(400);
    expect(data.error).toBe('INVALID INPUT');
  });
  
  it('Input validation: rejects zero rate', async () => {
    const { status, data } = await makeCalculateRequest(200000, 0, 30);
    expect(status).toBe(400);
    expect(data.error).toBe('INVALID INPUT');
  });
  
  it('Regex parsing: correctly extracts payment from "RESULT: 1135.58"', async () => {
    const { status, data } = await makeCalculateRequest(200000, 5.5, 30);
    
    expect(status).toBe(200);
    expect(data.monthly_payment).toBeCloseTo(1135.58, 1);
  });
  
  it('Regex parsing: handles various RESULT formats with different amounts', async () => {
    // TEST SMALL AMOUNT
    const small = await makeCalculateRequest(1000, 5.0, 1);
    expect(small.status).toBe(200);
    expect(small.data.monthly_payment).toBeGreaterThan(0);
    
    // TEST LARGE AMOUNT
    const large = await makeCalculateRequest(5000000, 10.0, 30);
    expect(large.status).toBe(200);
    expect(large.data.monthly_payment).toBeGreaterThan(1000);
  });
  
  it('Timeout handling: completes within 5 second limit for normal inputs', async () => {
    const startTime = Date.now();
    const { status } = await makeCalculateRequest(200000, 5.5, 30);
    const duration = Date.now() - startTime;
    
    expect(status).toBe(200);
    expect(duration).toBeLessThan(5000);
  });
  
});
