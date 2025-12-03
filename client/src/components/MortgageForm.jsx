import React, { useState, useEffect } from 'react';
import axios from 'axios';

// LANGUAGE CONFIGURATION WITH PARAMETERS
const LANGUAGE_CONFIG = {
  cobol: {
    name: 'COBOL (1959)',
    description: 'MORTGAGE PAYMENT CALCULATOR',
    params: [
      { key: 'principal', label: 'PRINCIPAL ($)', placeholder: '200000', type: 'number' },
      { key: 'rate', label: 'ANNUAL RATE (%)', placeholder: '5.5', type: 'number' },
      { key: 'term', label: 'TERM (YEARS)', placeholder: '30', type: 'number' }
    ]
  },
  fortran: {
    name: 'FORTRAN (1957)',
    description: 'BALLISTIC TRAJECTORY CALCULATOR',
    params: [
      { key: 'velocity', label: 'VELOCITY (M/S)', placeholder: '100', type: 'number' },
      { key: 'angle', label: 'ANGLE (DEGREES)', placeholder: '45', type: 'number' },
      { key: 'gravity', label: 'GRAVITY (M/S²)', placeholder: '9.81', type: 'number' }
    ]
  },
  pascal: {
    name: 'PASCAL (1970)',
    description: 'PROGRESSIVE TAX CALCULATOR',
    params: [
      { key: 'income', label: 'INCOME ($)', placeholder: '75000', type: 'number' },
      { key: 'bracket_rate', label: 'TAX RATE (%)', placeholder: '22', type: 'number' },
      { key: 'bracket_threshold', label: 'BRACKET THRESHOLD ($)', placeholder: '50000', type: 'number' }
    ]
  },
  basic: {
    name: 'BASIC (1983)',
    description: 'COMPOUND INTEREST CALCULATOR',
    params: [
      { key: 'principal', label: 'PRINCIPAL ($)', placeholder: '10000', type: 'number' },
      { key: 'rate', label: 'ANNUAL RATE (%)', placeholder: '5', type: 'number' },
      { key: 'time', label: 'TIME (YEARS)', placeholder: '10', type: 'number' },
      { key: 'compounds', label: 'COMPOUNDS/YEAR', placeholder: '12', type: 'number' }
    ]
  }
};

function MortgageForm({ onCalculationStart, onCalculationComplete, onCalculationError }) {
  const [selectedLanguage, setSelectedLanguage] = useState('cobol');
  const [formData, setFormData] = useState({});
  const [errors, setErrors] = useState({});
  const [isCalculating, setIsCalculating] = useState(false);
  const [calculationHistory, setCalculationHistory] = useState([]);

  // RESET FORM DATA WHEN LANGUAGE CHANGES
  useEffect(() => {
    const initialData = {};
    const config = LANGUAGE_CONFIG[selectedLanguage];
    config.params.forEach(param => {
      initialData[param.key] = '';
    });
    setFormData(initialData);
    setErrors({});
  }, [selectedLanguage]);

  const validateInputs = () => {
    const newErrors = {};
    const config = LANGUAGE_CONFIG[selectedLanguage];

    // VALIDATE ALL PARAMETERS FOR SELECTED LANGUAGE
    config.params.forEach(param => {
      const value = formData[param.key];
      
      if (!value || value.toString().trim() === '') {
        newErrors[param.key] = `${param.label} REQUIRED`;
      } else if (isNaN(value) || Number(value) <= 0) {
        newErrors[param.key] = `${param.label} MUST BE POSITIVE NUMBER`;
      }
    });

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleInputChange = (key, value) => {
    setFormData(prev => ({
      ...prev,
      [key]: value
    }));
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
      // CONVERT FORM DATA TO NUMBERS
      const params = {};
      Object.keys(formData).forEach(key => {
        params[key] = Number(formData[key]);
      });

      // CALL LANGUAGE-SPECIFIC ENDPOINT
      const response = await axios.post(
        `http://localhost:3001/api/calculate/${selectedLanguage}`,
        params
      );

      // ADD TO CALCULATION HISTORY
      const historyEntry = {
        id: Date.now(),
        timestamp: new Date().toLocaleTimeString(),
        language: LANGUAGE_CONFIG[selectedLanguage].name,
        params: formData,
        result: response.data.result,
        source: response.data.source
      };
      
      setCalculationHistory(prev => [historyEntry, ...prev].slice(0, 10)); // KEEP LAST 10

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

  const currentConfig = LANGUAGE_CONFIG[selectedLanguage];

  return (
    <div className="max-w-4xl mx-auto p-6">
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* MAIN CALCULATOR FORM - 2/3 WIDTH */}
        <div className="lg:col-span-2">
          <form onSubmit={handleSubmit} className="space-y-6">
            {/* LANGUAGE SELECTOR */}
            <div>
              <label 
                htmlFor="language" 
                className="block text-mainframe-green font-mono text-sm mb-2"
              >
                SELECT LEGACY LANGUAGE:
              </label>
              <select
                id="language"
                value={selectedLanguage}
                onChange={(e) => setSelectedLanguage(e.target.value)}
                className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono px-4 py-2 focus:outline-none focus:border-mainframe-green focus:ring-1 focus:ring-mainframe-green"
              >
                {Object.keys(LANGUAGE_CONFIG).map(lang => (
                  <option key={lang} value={lang}>
                    {LANGUAGE_CONFIG[lang].name} - {LANGUAGE_CONFIG[lang].description}
                  </option>
                ))}
              </select>
            </div>

            {/* DYNAMIC PARAMETER INPUTS */}
            {currentConfig.params.map(param => (
              <div key={param.key}>
                <label 
                  htmlFor={param.key}
                  className="block text-mainframe-green font-mono text-sm mb-2"
                >
                  {param.label}:
                </label>
                <input
                  id={param.key}
                  type="text"
                  value={formData[param.key] || ''}
                  onChange={(e) => handleInputChange(param.key, e.target.value)}
                  className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono px-4 py-2 focus:outline-none focus:border-mainframe-green focus:ring-1 focus:ring-mainframe-green"
                  placeholder={param.placeholder}
                />
                {errors[param.key] && (
                  <p className="text-mainframe-green font-mono text-xs mt-1">
                    {errors[param.key]}
                  </p>
                )}
              </div>
            ))}

            {/* CALCULATE BUTTON */}
            <button
              type="submit"
              disabled={isCalculating}
              className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-3 px-6 hover:bg-mainframe-green hover:text-black transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {isCalculating ? 'CALCULATING...' : `>>> EXECUTE ${currentConfig.name} <<<`}
            </button>
          </form>
        </div>

        {/* CALCULATION HISTORY PANEL - 1/3 WIDTH */}
        <div className="lg:col-span-1">
          <div className="border-2 border-mainframe-green bg-black p-4">
            <h3 className="text-mainframe-green font-mono text-sm mb-4 border-b border-mainframe-green pb-2">
              CALCULATION HISTORY
            </h3>
            
            {calculationHistory.length === 0 ? (
              <p className="text-mainframe-green font-mono text-xs opacity-50">
                NO CALCULATIONS YET...
              </p>
            ) : (
              <div className="space-y-3 max-h-96 overflow-y-auto">
                {calculationHistory.map(entry => (
                  <div 
                    key={entry.id}
                    className="border border-mainframe-green p-2 text-xs"
                  >
                    <div className="text-mainframe-green font-mono opacity-70 mb-1">
                      [{entry.timestamp}]
                    </div>
                    <div className="text-mainframe-green font-mono font-bold mb-1">
                      {entry.language}
                    </div>
                    <div className="text-mainframe-green font-mono text-xs space-y-1">
                      {Object.entries(entry.params).map(([key, value]) => (
                        <div key={key}>
                          {key}: {value}
                        </div>
                      ))}
                    </div>
                    <div className="text-mainframe-green font-mono mt-2 pt-2 border-t border-mainframe-green">
                      RESULT: {typeof entry.result === 'number' ? entry.result.toFixed(2) : entry.result}
                    </div>
                    <div className="text-mainframe-green font-mono text-xs opacity-50 mt-1">
                      SOURCE: {entry.source}
                    </div>
                  </div>
                ))}
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}

export default MortgageForm;
