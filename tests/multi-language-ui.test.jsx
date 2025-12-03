/**
 * ═══════════════════════════════════════════════════════════
 * MULTI-LANGUAGE UI COMPONENT TEST
 * ═══════════════════════════════════════════════════════════
 * 
 * VALIDATES UI IMPLEMENTATION OF TASK 21:
 * - Language selector dropdown renders correctly
 * - Dynamic form inputs change based on language selection
 * - Language-specific parameter labels display correctly
 * - Calculation history panel renders and updates
 * - History shows which language processed each result
 */

import { describe, test, expect, vi } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import MortgageForm from '../client/src/components/MortgageForm';
import axios from 'axios';

// MOCK AXIOS
vi.mock('axios');

describe('MULTI-LANGUAGE UI COMPONENT TESTS', () => {
  
  describe('LANGUAGE SELECTOR DROPDOWN', () => {
    test('SHOULD RENDER LANGUAGE SELECTOR WITH ALL FOUR OPTIONS', () => {
      render(<MortgageForm />);
      
      const selector = screen.getByLabelText(/SELECT LEGACY LANGUAGE/i);
      expect(selector).toBeDefined();
      
      // VERIFY ALL FOUR LANGUAGES ARE PRESENT
      const options = selector.querySelectorAll('option');
      expect(options.length).toBe(4);
      
      const optionValues = Array.from(options).map(opt => opt.value);
      expect(optionValues).toContain('cobol');
      expect(optionValues).toContain('fortran');
      expect(optionValues).toContain('pascal');
      expect(optionValues).toContain('basic');
    });
    
    test('SHOULD DEFAULT TO COBOL LANGUAGE', () => {
      render(<MortgageForm />);
      
      const selector = screen.getByLabelText(/SELECT LEGACY LANGUAGE/i);
      expect(selector.value).toBe('cobol');
    });
  });
  
  describe('DYNAMIC FORM INPUTS - COBOL', () => {
    test('SHOULD DISPLAY COBOL-SPECIFIC PARAMETERS', () => {
      render(<MortgageForm />);
      
      expect(screen.getByLabelText(/PRINCIPAL/i)).toBeDefined();
      expect(screen.getByLabelText(/ANNUAL RATE/i)).toBeDefined();
      expect(screen.getByLabelText(/TERM \(YEARS\)/i)).toBeDefined();
    });
  });
  
  describe('DYNAMIC FORM INPUTS - FORTRAN', () => {
    test('SHOULD DISPLAY FORTRAN-SPECIFIC PARAMETERS WHEN SELECTED', () => {
      render(<MortgageForm />);
      
      const selector = screen.getByLabelText(/SELECT LEGACY LANGUAGE/i);
      fireEvent.change(selector, { target: { value: 'fortran' } });
      
      expect(screen.getByLabelText(/VELOCITY/i)).toBeDefined();
      expect(screen.getByLabelText(/ANGLE/i)).toBeDefined();
      expect(screen.getByLabelText(/GRAVITY/i)).toBeDefined();
    });
  });
  
  describe('DYNAMIC FORM INPUTS - PASCAL', () => {
    test('SHOULD DISPLAY PASCAL-SPECIFIC PARAMETERS WHEN SELECTED', () => {
      render(<MortgageForm />);
      
      const selector = screen.getByLabelText(/SELECT LEGACY LANGUAGE/i);
      fireEvent.change(selector, { target: { value: 'pascal' } });
      
      expect(screen.getByLabelText(/INCOME/i)).toBeDefined();
      expect(screen.getByLabelText(/TAX RATE/i)).toBeDefined();
      expect(screen.getByLabelText(/BRACKET THRESHOLD/i)).toBeDefined();
    });
  });
  
  describe('DYNAMIC FORM INPUTS - BASIC', () => {
    test('SHOULD DISPLAY BASIC-SPECIFIC PARAMETERS WHEN SELECTED', () => {
      render(<MortgageForm />);
      
      const selector = screen.getByLabelText(/SELECT LEGACY LANGUAGE/i);
      fireEvent.change(selector, { target: { value: 'basic' } });
      
      expect(screen.getByLabelText(/PRINCIPAL/i)).toBeDefined();
      expect(screen.getByLabelText(/ANNUAL RATE/i)).toBeDefined();
      expect(screen.getByLabelText(/TIME \(YEARS\)/i)).toBeDefined();
      expect(screen.getByLabelText(/COMPOUNDS\/YEAR/i)).toBeDefined();
    });
  });
  
  describe('FORM INPUT CLEARING ON LANGUAGE CHANGE', () => {
    test('SHOULD CLEAR FORM DATA WHEN LANGUAGE CHANGES', () => {
      render(<MortgageForm />);
      
      // FILL IN COBOL FORM
      const principalInput = screen.getByLabelText(/PRINCIPAL/i);
      fireEvent.change(principalInput, { target: { value: '200000' } });
      expect(principalInput.value).toBe('200000');
      
      // CHANGE LANGUAGE TO FORTRAN
      const selector = screen.getByLabelText(/SELECT LEGACY LANGUAGE/i);
      fireEvent.change(selector, { target: { value: 'fortran' } });
      
      // VERIFY NEW INPUTS ARE EMPTY
      const velocityInput = screen.getByLabelText(/VELOCITY/i);
      expect(velocityInput.value).toBe('');
    });
  });
  
  describe('CALCULATION HISTORY PANEL', () => {
    test('SHOULD RENDER CALCULATION HISTORY PANEL', () => {
      render(<MortgageForm />);
      
      expect(screen.getByText(/CALCULATION HISTORY/i)).toBeDefined();
    });
    
    test('SHOULD SHOW EMPTY STATE WHEN NO CALCULATIONS', () => {
      render(<MortgageForm />);
      
      expect(screen.getByText(/NO CALCULATIONS YET/i)).toBeDefined();
    });
    
    test('SHOULD ADD ENTRY TO HISTORY AFTER SUCCESSFUL CALCULATION', async () => {
      // MOCK SUCCESSFUL API RESPONSE
      axios.post.mockResolvedValueOnce({
        data: {
          result: 1135.57,
          source: 'COBOL_LEGACY_ENGINE',
          language: 'cobol',
          year: 1959
        }
      });
      
      const mockOnComplete = vi.fn();
      render(<MortgageForm onCalculationComplete={mockOnComplete} />);
      
      // FILL IN FORM
      fireEvent.change(screen.getByLabelText(/PRINCIPAL/i), { target: { value: '200000' } });
      fireEvent.change(screen.getByLabelText(/ANNUAL RATE/i), { target: { value: '5.5' } });
      fireEvent.change(screen.getByLabelText(/TERM \(YEARS\)/i), { target: { value: '30' } });
      
      // SUBMIT FORM
      const submitButton = screen.getByRole('button', { name: /EXECUTE/i });
      fireEvent.click(submitButton);
      
      // WAIT FOR HISTORY TO UPDATE
      await waitFor(() => {
        expect(screen.queryByText(/NO CALCULATIONS YET/i)).toBeNull();
      });
      
      // VERIFY HISTORY ENTRY SHOWS LANGUAGE
      expect(screen.getByText(/COBOL \(1959\)/i)).toBeDefined();
      expect(screen.getByText(/SOURCE: COBOL_LEGACY_ENGINE/i)).toBeDefined();
    });
    
    test('SHOULD DISPLAY LANGUAGE NAME IN HISTORY ENTRY', async () => {
      // MOCK SUCCESSFUL API RESPONSE
      axios.post.mockResolvedValueOnce({
        data: {
          result: 1019.37,
          source: 'FORTRAN_LEGACY_ENGINE',
          language: 'fortran',
          year: 1957
        }
      });
      
      render(<MortgageForm />);
      
      // CHANGE TO FORTRAN
      fireEvent.change(screen.getByLabelText(/SELECT LEGACY LANGUAGE/i), { target: { value: 'fortran' } });
      
      // FILL IN FORM
      fireEvent.change(screen.getByLabelText(/VELOCITY/i), { target: { value: '100' } });
      fireEvent.change(screen.getByLabelText(/ANGLE/i), { target: { value: '45' } });
      fireEvent.change(screen.getByLabelText(/GRAVITY/i), { target: { value: '9.81' } });
      
      // SUBMIT FORM
      const submitButton = screen.getByRole('button', { name: /EXECUTE/i });
      fireEvent.click(submitButton);
      
      // WAIT FOR HISTORY TO UPDATE
      await waitFor(() => {
        expect(screen.getByText(/FORTRAN \(1957\)/i)).toBeDefined();
      });
      
      expect(screen.getByText(/SOURCE: FORTRAN_LEGACY_ENGINE/i)).toBeDefined();
    });
    
    test('SHOULD LIMIT HISTORY TO 10 ENTRIES', async () => {
      render(<MortgageForm />);
      
      // MOCK 11 SUCCESSFUL CALCULATIONS
      for (let i = 0; i < 11; i++) {
        axios.post.mockResolvedValueOnce({
          data: {
            result: 1000 + i,
            source: 'COBOL_LEGACY_ENGINE',
            language: 'cobol',
            year: 1959
          }
        });
        
        // FILL AND SUBMIT FORM
        fireEvent.change(screen.getByLabelText(/PRINCIPAL/i), { target: { value: '200000' } });
        fireEvent.change(screen.getByLabelText(/ANNUAL RATE/i), { target: { value: '5.5' } });
        fireEvent.change(screen.getByLabelText(/TERM \(YEARS\)/i), { target: { value: '30' } });
        
        const submitButton = screen.getByRole('button', { name: /EXECUTE/i });
        fireEvent.click(submitButton);
        
        await waitFor(() => {
          expect(axios.post).toHaveBeenCalledTimes(i + 1);
        });
      }
      
      // VERIFY ONLY 10 ENTRIES ARE DISPLAYED
      const historyEntries = screen.getAllByText(/RESULT:/i);
      expect(historyEntries.length).toBeLessThanOrEqual(10);
    });
  });
  
  describe('EXECUTE BUTTON LABEL', () => {
    test('SHOULD UPDATE BUTTON LABEL BASED ON SELECTED LANGUAGE', () => {
      render(<MortgageForm />);
      
      // COBOL
      expect(screen.getByRole('button', { name: /EXECUTE COBOL \(1959\)/i })).toBeDefined();
      
      // FORTRAN
      fireEvent.change(screen.getByLabelText(/SELECT LEGACY LANGUAGE/i), { target: { value: 'fortran' } });
      expect(screen.getByRole('button', { name: /EXECUTE FORTRAN \(1957\)/i })).toBeDefined();
      
      // PASCAL
      fireEvent.change(screen.getByLabelText(/SELECT LEGACY LANGUAGE/i), { target: { value: 'pascal' } });
      expect(screen.getByRole('button', { name: /EXECUTE PASCAL \(1970\)/i })).toBeDefined();
      
      // BASIC
      fireEvent.change(screen.getByLabelText(/SELECT LEGACY LANGUAGE/i), { target: { value: 'basic' } });
      expect(screen.getByRole('button', { name: /EXECUTE BASIC \(1983\)/i })).toBeDefined();
    });
  });
});
