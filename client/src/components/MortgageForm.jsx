import React, { useState } from 'react';
import axios from 'axios';

function MortgageForm({ onCalculationStart, onCalculationComplete, onCalculationError }) {
  const [principal, setPrincipal] = useState('');
  const [rate, setRate] = useState('');
  const [term, setTerm] = useState('');
  const [errors, setErrors] = useState({});
  const [isCalculating, setIsCalculating] = useState(false);

  const validateInputs = () => {
    const newErrors = {};

    // Validate principal
    if (!principal || principal.trim() === '') {
      newErrors.principal = 'PRINCIPAL REQUIRED';
    } else if (isNaN(principal) || Number(principal) <= 0) {
      newErrors.principal = 'PRINCIPAL MUST BE POSITIVE NUMBER';
    }

    // Validate rate
    if (!rate || rate.trim() === '') {
      newErrors.rate = 'RATE REQUIRED';
    } else if (isNaN(rate) || Number(rate) <= 0) {
      newErrors.rate = 'RATE MUST BE POSITIVE NUMBER';
    }

    // Validate term
    if (!term || term.trim() === '') {
      newErrors.term = 'TERM REQUIRED';
    } else if (isNaN(term) || Number(term) <= 0 || !Number.isInteger(Number(term))) {
      newErrors.term = 'TERM MUST BE POSITIVE INTEGER';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();

    if (!validateInputs()) {
      return;
    }

    setIsCalculating(true);
    
    if (onCalculationStart) {
      onCalculationStart();
    }

    try {
      const response = await axios.post('http://localhost:3001/api/calculate', {
        principal: Number(principal),
        rate: Number(rate),
        term: Number(term)
      });

      if (onCalculationComplete) {
        onCalculationComplete(response.data);
      }
    } catch (error) {
      const errorMessage = error.response?.data?.error || 'SYSTEM FAILURE: UNABLE TO REACH MAINFRAME';
      if (onCalculationError) {
        onCalculationError(errorMessage);
      }
    } finally {
      setIsCalculating(false);
    }
  };

  return (
    <div className="max-w-2xl mx-auto p-6">
      <form onSubmit={handleSubmit} className="space-y-6">
        {/* Principal Input */}
        <div>
          <label 
            htmlFor="principal" 
            className="block text-mainframe-green font-mono text-sm mb-2"
          >
            PRINCIPAL ($):
          </label>
          <input
            id="principal"
            type="text"
            value={principal}
            onChange={(e) => setPrincipal(e.target.value)}
            className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono px-4 py-2 focus:outline-none focus:border-mainframe-green focus:ring-1 focus:ring-mainframe-green"
            placeholder="200000"
          />
          {errors.principal && (
            <p className="text-mainframe-green font-mono text-xs mt-1">
              {errors.principal}
            </p>
          )}
        </div>

        {/* Rate Input */}
        <div>
          <label 
            htmlFor="rate" 
            className="block text-mainframe-green font-mono text-sm mb-2"
          >
            ANNUAL RATE (%):
          </label>
          <input
            id="rate"
            type="text"
            value={rate}
            onChange={(e) => setRate(e.target.value)}
            className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono px-4 py-2 focus:outline-none focus:border-mainframe-green focus:ring-1 focus:ring-mainframe-green"
            placeholder="5.5"
          />
          {errors.rate && (
            <p className="text-mainframe-green font-mono text-xs mt-1">
              {errors.rate}
            </p>
          )}
        </div>

        {/* Term Input */}
        <div>
          <label 
            htmlFor="term" 
            className="block text-mainframe-green font-mono text-sm mb-2"
          >
            TERM (YEARS):
          </label>
          <input
            id="term"
            type="text"
            value={term}
            onChange={(e) => setTerm(e.target.value)}
            className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono px-4 py-2 focus:outline-none focus:border-mainframe-green focus:ring-1 focus:ring-mainframe-green"
            placeholder="30"
          />
          {errors.term && (
            <p className="text-mainframe-green font-mono text-xs mt-1">
              {errors.term}
            </p>
          )}
        </div>

        {/* Calculate Button */}
        <button
          type="submit"
          disabled={isCalculating}
          className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-3 px-6 hover:bg-mainframe-green hover:text-black transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {isCalculating ? 'CALCULATING...' : '>>> CALCULATE PAYMENT <<<'}
        </button>
      </form>
    </div>
  );
}

export default MortgageForm;
