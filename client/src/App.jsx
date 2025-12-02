import React, { useState } from 'react'
import Header from './components/Header'
import MortgageForm from './components/MortgageForm'
import TerminalWindow from './components/TerminalWindow'
import CodeGeneratorModal from './components/CodeGeneratorModal'
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
  const [isModalOpen, setIsModalOpen] = useState(false);

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

  const handleGenerationComplete = (data) => {
    addLog(`👻 ANCIENT SPIRIT SUMMONED: ${data.filename}`, 'info');
    addLog(`📁 FILE: ${data.file_path}`, 'info');
    addLog(`⚙️ COMPILATION: ${data.compilation.message}`, 'info');
  };

  return (
    <div className="min-h-screen bg-mainframe-black">
      <Header />
      
      {/* SUMMON ANCIENT SPIRIT BUTTON */}
      <div className="max-w-2xl mx-auto px-6 pt-6">
        <button
          onClick={() => setIsModalOpen(true)}
          className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-3 px-6 hover:bg-mainframe-green hover:text-black transition-colors mb-6"
        >
          👻 SUMMON ANCIENT SPIRIT (AI CODE GENERATOR)
        </button>
      </div>

      <MortgageForm 
        onCalculationStart={handleCalculationStart}
        onCalculationComplete={handleCalculationComplete}
        onCalculationError={handleCalculationError}
      />
      <TerminalWindow logs={logs} isCalculating={isCalculating} />
      
      {/* CODE GENERATOR MODAL */}
      <CodeGeneratorModal
        isOpen={isModalOpen}
        onClose={() => setIsModalOpen(false)}
        onGenerationComplete={handleGenerationComplete}
      />
    </div>
  )
}

export default App
