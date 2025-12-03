import React, { useState, useEffect } from 'react'
import Header from './components/Header'
import MortgageForm from './components/MortgageForm'
import TerminalWindow from './components/TerminalWindow'
import CodeGeneratorModal from './components/CodeGeneratorModal'
import TapeReel from './components/TapeReel'
import PanelLights from './components/PanelLights'
import MemoryDump from './components/MemoryDump'
import { VolumeControl } from './components/VolumeControl'
import { audioManager } from './services/AudioManager'
import './App.css'

// Boot sequence configuration as per design document
const BOOT_SEQUENCE = [
  { delay: 0, message: "INITIALIZING TAPE DRIVE..." },
  { delay: 400, message: "MOUNTING VOLUME..." },
  { delay: 800, message: "LOADING LEGACY RUNTIME..." },
  { delay: 1200, message: "EXECUTING ANCIENT SUBROUTINE..." },
];

function App() {
  const [logs, setLogs] = useState([]);
  const [isCalculating, setIsCalculating] = useState(false);
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [errorData, setErrorData] = useState(null);
  const [showMemoryDump, setShowMemoryDump] = useState(false);

  // Initialize audio manager on mount
  useEffect(() => {
    audioManager.initialize().catch(console.error);
  }, []);

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
    
    // Clear previous error state when starting new calculation
    setShowMemoryDump(false);
    setErrorData(null);
    
    // Play card reader sound when calculation starts
    audioManager.playCardReader();
    
    // Play tape drive sound during calculation
    setTimeout(() => {
      audioManager.playTapeDrive();
    }, 400);
    
    // Execute boot sequence with delays
    BOOT_SEQUENCE.forEach(({ delay, message }) => {
      setTimeout(() => {
        addLog(message, 'info');
        // Play teletype sound for each log message
        audioManager.playTeletype();
      }, delay);
    });
  };

  const handleCalculationComplete = (data) => {
    // Display result after boot sequence completes (1600ms as per design)
    setTimeout(() => {
      // HANDLE BOTH OLD FORMAT (monthly_payment) AND NEW FORMAT (result)
      const resultValue = data.result || data.monthly_payment;
      const resultFormatted = typeof resultValue === 'number' ? resultValue.toFixed(2) : resultValue;
      const source = data.source || 'LEGACY ENGINE';
      
      addLog(`RESULT: ${resultFormatted}`, 'result');
      addLog(`SOURCE: ${source}`, 'info');
      setIsCalculating(false);
      
      // Play bell sound on successful completion
      audioManager.playBell();
    }, 1600);
  };

  const handleCalculationError = (error) => {
    // Display error after boot sequence completes
    setTimeout(() => {
      addLog(`ERROR: ${error}`, 'error');
      setIsCalculating(false);
      
      // Play buzzer sound on error
      audioManager.playBuzzer();
      
      // Trigger memory dump visualization on error
      setErrorData({
        error: error,
        details: 'LEGACY ENGINE EXECUTION FAILED'
      });
      setShowMemoryDump(true);
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
      <div className="max-w-4xl mx-auto px-6 pt-6">
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
      
      {/* IBM 729 TAPE DRIVE VISUALIZATION */}
      <TapeReel isCalculating={isCalculating} />
      
      {/* IBM 7090 CONSOLE PANEL LIGHTS */}
      <div className="max-w-4xl mx-auto px-6 py-6">
        <PanelLights isCalculating={isCalculating} />
      </div>
      
      {/* CORE MEMORY DUMP (SHOWN ON ERROR) */}
      <MemoryDump errorData={errorData} isVisible={showMemoryDump} />
      
      <TerminalWindow logs={logs} isCalculating={isCalculating} />
      
      {/* CODE GENERATOR MODAL */}
      <CodeGeneratorModal
        isOpen={isModalOpen}
        onClose={() => setIsModalOpen(false)}
        onGenerationComplete={handleGenerationComplete}
      />
      
      {/* VOLUME CONTROL */}
      <VolumeControl />
    </div>
  )
}

export default App
