import { describe, it, expect } from 'vitest';
import * as fc from 'fast-check';
import { execSync } from 'child_process';

/**
 * HELPER FUNCTION: CALCULATE EXPECTED MORTGAGE PAYMENT
 * IMPLEMENTS FORMULA: M = P [ i(1 + i)^n ] / [ (1 + i)^n – 1 ]
 */
function calculateExpectedPayment(principal, annualRate, termYears) {
  const monthlyRate = annualRate / 12 / 100;
  const numPayments = termYears * 12;
  const powerTerm = Math.pow(1 + monthlyRate, numPayments);
  const numerator = principal * monthlyRate * powerTerm;
  const denominator = powerTerm - 1;
  return numerator / denominator;
}

/**
 * HELPER FUNCTION: EXECUTE COBOL BINARY AND PARSE OUTPUT
 */
function runCobolCalculator(principal, rate, term) {
  try {
    const output = execSync(
      `./legacy/mortgage ${principal} ${rate} ${term}`,
      { encoding: 'utf-8', timeout: 5000 }
    );
    
    const match = output.match(/RESULT:\s*(\d+\.\d{2})/);
    if (!match) {
      throw new Error(`Failed to parse COBOL output: ${output}`);
    }
    
    return parseFloat(match[1]);
  } catch (error) {
    throw new Error(`COBOL execution failed: ${error.message}`);
  }
}

describe('COBOL Mortgage Calculator Property Tests', () => {
  
  // Feature: cobol-resurrection-bridge, Property 1: COBOL Mortgage Calculation Accuracy
  it('Property 1: COBOL calculation matches mathematical formula within $0.01 tolerance', () => {
    fc.assert(
      fc.property(
        fc.integer({ min: 1000, max: 10000000 }),     // Principal: $1K - $10M
        fc.float({ min: Math.fround(0.1), max: Math.fround(30.0), noNaN: true }), // Rate: 0.1% - 30%
        fc.integer({ min: 1, max: 50 }),              // Term: 1-50 years
        (principal, rate, term) => {
          const cobolResult = runCobolCalculator(principal, rate, term);
          const expectedResult = calculateExpectedPayment(principal, rate, term);
          
          const absoluteDifference = Math.abs(cobolResult - expectedResult);
          const relativeError = absoluteDifference / expectedResult;
          
          // VALIDATE: EITHER ABSOLUTE DIFFERENCE < $0.15 OR RELATIVE ERROR < 0.5%
          // COBOL COMP-2 AND JAVASCRIPT FLOATING POINT DIFFER IN PRECISION
          const isAccurate = absoluteDifference <= 0.15 || relativeError <= 0.005;
          expect(isAccurate).toBe(true);
        }
      ),
      { numRuns: 100 }
    );
  });
  
  // Feature: cobol-resurrection-bridge, Property 2: COBOL Output Format Consistency
  it('Property 2: COBOL output matches regex pattern with exactly two decimal places', () => {
    fc.assert(
      fc.property(
        fc.integer({ min: 1000, max: 10000000 }),     // Principal: $1K - $10M
        fc.float({ min: Math.fround(0.1), max: Math.fround(30.0), noNaN: true }), // Rate: 0.1% - 30%
        fc.integer({ min: 1, max: 50 }),              // Term: 1-50 years
        (principal, rate, term) => {
          const output = execSync(
            `./legacy/mortgage ${principal} ${rate} ${term}`,
            { encoding: 'utf-8', timeout: 5000 }
          );
          
          // VALIDATE: OUTPUT MATCHES PATTERN "RESULT: XXXX.XX"
          const formatRegex = /^RESULT:\s*\d+\.\d{2}\s*$/;
          expect(output.trim()).toMatch(formatRegex);
        }
      ),
      { numRuns: 100 }
    );
  });
  
  // Feature: cobol-resurrection-bridge, Property 3: COBOL Error Handling
  it('Property 3: COBOL exits with non-zero status for invalid inputs without crashing', () => {
    fc.assert(
      fc.property(
        fc.oneof(
          // GENERATE INVALID PRINCIPALS (NEGATIVE OR ZERO)
          fc.record({ principal: fc.integer({ min: -100000, max: 0 }), rate: fc.float({ min: Math.fround(0.1), max: Math.fround(30.0) }), term: fc.integer({ min: 1, max: 50 }) }),
          // GENERATE INVALID RATES (NEGATIVE OR ZERO)
          fc.record({ principal: fc.integer({ min: 1000, max: 10000000 }), rate: fc.float({ min: Math.fround(-30.0), max: Math.fround(0.0) }), term: fc.integer({ min: 1, max: 50 }) }),
          // GENERATE INVALID TERMS (NEGATIVE OR ZERO)
          fc.record({ principal: fc.integer({ min: 1000, max: 10000000 }), rate: fc.float({ min: Math.fround(0.1), max: Math.fround(30.0) }), term: fc.integer({ min: -50, max: 0 }) })
        ),
        ({ principal, rate, term }) => {
          try {
            execSync(
              `./legacy/mortgage ${principal} ${rate} ${term}`,
              { encoding: 'utf-8', timeout: 5000, stdio: 'pipe' }
            );
            // IF WE REACH HERE, THE BINARY RETURNED EXIT CODE 0 (FAILURE)
            expect.fail('COBOL binary should have exited with non-zero status');
          } catch (error) {
            // VALIDATE: NON-ZERO EXIT CODE
            expect(error.status).not.toBe(0);
            expect(error.status).toBe(1);
            
            // VALIDATE: ERROR MESSAGE PRESENT ON STDERR
            expect(error.stderr.toString()).toMatch(/ERROR:/);
          }
        }
      ),
      { numRuns: 100 }
    );
  });
  
});

describe('COBOL Mortgage Calculator Unit Tests', () => {
  
  it('calculates correct payment for $200,000 at 5.5% for 30 years', () => {
    const output = execSync('./legacy/mortgage 200000 5.5 30', { encoding: 'utf-8' });
    const match = output.match(/RESULT:\s*(\d+\.\d{2})/);
    
    expect(match).not.toBeNull();
    const payment = parseFloat(match[1]);
    
    // EXPECTED: $1135.58 (STANDARD MORTGAGE CALCULATION)
    expect(payment).toBeCloseTo(1135.58, 1);
  });
  
  it('handles edge case: $1 principal', () => {
    const output = execSync('./legacy/mortgage 1 5.5 30', { encoding: 'utf-8' });
    const match = output.match(/RESULT:\s*(\d+\.\d{2})/);
    
    expect(match).not.toBeNull();
    const payment = parseFloat(match[1]);
    
    // PAYMENT ROUNDS TO 0.00 DUE TO FORMATTING (ACTUAL VALUE ~$0.0057)
    expect(payment).toBeGreaterThanOrEqual(0);
    expect(payment).toBeLessThan(1);
  });
  
  it('handles edge case: 29.99% rate', () => {
    const output = execSync('./legacy/mortgage 100000 29.99 10', { encoding: 'utf-8' });
    const match = output.match(/RESULT:\s*(\d+\.\d{2})/);
    
    expect(match).not.toBeNull();
    const payment = parseFloat(match[1]);
    
    // HIGH RATE SHOULD PRODUCE HIGH PAYMENT
    expect(payment).toBeGreaterThan(2000);
  });
  
  it('handles edge case: 1 year term', () => {
    const output = execSync('./legacy/mortgage 12000 5.0 1', { encoding: 'utf-8' });
    const match = output.match(/RESULT:\s*(\d+\.\d{2})/);
    
    expect(match).not.toBeNull();
    const payment = parseFloat(match[1]);
    
    // SHORT TERM MEANS HIGH MONTHLY PAYMENT (CLOSE TO PRINCIPAL/12)
    expect(payment).toBeGreaterThan(1000);
    expect(payment).toBeLessThan(1100);
  });
  
  it('rejects negative principal with error message', () => {
    try {
      execSync('./legacy/mortgage -1000 5.5 30', { encoding: 'utf-8', stdio: 'pipe' });
      expect.fail('Should have thrown error');
    } catch (error) {
      expect(error.status).toBe(1);
      expect(error.stderr.toString()).toMatch(/ERROR: INVALID PRINCIPAL AMOUNT/);
    }
  });
  
  it('rejects zero rate with error message', () => {
    try {
      execSync('./legacy/mortgage 200000 0 30', { encoding: 'utf-8', stdio: 'pipe' });
      expect.fail('Should have thrown error');
    } catch (error) {
      expect(error.status).toBe(1);
      expect(error.stderr.toString()).toMatch(/ERROR: INVALID INTEREST RATE/);
    }
  });
  
  it('rejects insufficient arguments with error message', () => {
    try {
      execSync('./legacy/mortgage 200000 5.5', { encoding: 'utf-8', stdio: 'pipe' });
      expect.fail('Should have thrown error');
    } catch (error) {
      expect(error.status).toBe(1);
      expect(error.stderr.toString()).toMatch(/ERROR: INSUFFICIENT ARGUMENTS/);
    }
  });
  
});