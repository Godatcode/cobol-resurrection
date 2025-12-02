import React, { useState } from 'react'
import Header from './components/Header'
import MortgageForm from './components/MortgageForm'
import TerminalWindow from './components/TerminalWindow'
import './App.css'

// Boot sequence configuration as per design document
const BOOT_SEQUENCE = [
  { delay: 0, message: "INITIALIZING TAPE DRIVE..." },
  { delay: 400, message: "MOUNTING VOLUME..." },
  { delay: 800, message: "LOADING COBOL RUNTIME..." },
  { delay: 1200, message: "EXECUTING LEGACY SUBROUTINE..." },
];

function App() {
  const [logs, setLogs] = useState([]);
  const [isCalculating, setIsCalculating] = useState(false);

  const formatTimestamp = () => {
    const now = new Date();
    return now.toTimeString().split(' ')[0];
  };

  const addLog = (message, type = 'info') => {
    setLogs(prevLogs => [...prevLogs, {
      timestamp: formatTimestamp(),
      message,
      type
    }]);
  };

  const handleCalculationStart = () => {
    setIsCalculating(true);
    
    // Execute boot sequence with delays
    BOOT_SEQUENCE.forEach(({ delay, message }) => {
      setTimeout(() => {
        addLog(message, 'info');
      }, delay);
    });
  };

  const handleCalculationComplete = (data) => {
    // Display result after boot sequence completes (1600ms as per design)
    setTimeout(() => {
      const payment = data.monthly_payment.toFixed(2);
      addLog(`RESULT: Monthly Payment = $${payment}`, 'info');
      setIsCalculating(false);
    }, 1600);
  };

  const handleCalculationError = (error) => {
    // Display error after boot sequence completes
    setTimeout(() => {
      addLog(`ERROR: ${error}`, 'error');
      setIsCalculating(false);
    }, 1600);
  };

  return (
    <div className="min-h-screen bg-mainframe-black">
      <Header />
      <MortgageForm 
        onCalculationStart={handleCalculationStart}
        onCalculationComplete={handleCalculationComplete}
        onCalculationError={handleCalculationError}
      />
      <TerminalWindow logs={logs} isCalculating={isCalculating} />
    </div>
  )
}

export default App
