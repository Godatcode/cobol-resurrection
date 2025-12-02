import React, { useState } from 'react';
import axios from 'axios';
import PunchCard from './PunchCard';

/**
 * SUMMON ANCIENT SPIRIT MODAL
 * AI-Powered Legacy Code Generation Interface
 * Allows users to generate vintage code from natural language descriptions
 */
function CodeGeneratorModal({ isOpen, onClose, onGenerationComplete }) {
  const [description, setDescription] = useState('');
  const [language, setLanguage] = useState('COBOL');
  const [filename, setFilename] = useState('');
  const [generatedCode, setGeneratedCode] = useState('');
  const [isGenerating, setIsGenerating] = useState(false);
  const [isCompiling, setIsCompiling] = useState(false);
  const [compilationStatus, setCompilationStatus] = useState(null);
  const [error, setError] = useState('');
  const [step, setStep] = useState('input'); // 'input', 'generating', 'review', 'compiling', 'complete'

  const LANGUAGES = [
    { value: 'COBOL', label: 'COBOL (1959)', description: 'Business calculations' },
    { value: 'FORTRAN', label: 'FORTRAN (1957)', description: 'Scientific computing' },
    { value: 'PASCAL', label: 'PASCAL (1970)', description: 'Structured programming' },
    { value: 'BASIC', label: 'BASIC (1983)', description: 'General purpose' }
  ];

  const handleClose = () => {
    // Reset state when closing
    setDescription('');
    setLanguage('COBOL');
    setFilename('');
    setGeneratedCode('');
    setIsGenerating(false);
    setIsCompiling(false);
    setCompilationStatus(null);
    setError('');
    setStep('input');
    onClose();
  };

  const handleGenerate = () => {
    // Validate inputs
    if (!description.trim()) {
      setError('DESCRIPTION REQUIRED');
      return;
    }
    if (!filename.trim()) {
      setError('FILENAME REQUIRED');
      return;
    }

    setError('');
    setIsGenerating(true);
    setStep('generating');

    // Simulate AI code generation with a delay
    // In production, this would call an AI service
    setTimeout(() => {
      const code = generateMockCode(language, description, filename);
      setGeneratedCode(code);
      setIsGenerating(false);
      setStep('review');
    }, 2000);
  };

  const handleCompile = async () => {
    setIsCompiling(true);
    setStep('compiling');
    setError('');

    try {
      const response = await axios.post('http://localhost:3001/api/generate', {
        code: generatedCode,
        language: language,
        filename: filename
      });

      setCompilationStatus({
        success: true,
        message: response.data.message,
        filePath: response.data.file_path,
        compilation: response.data.compilation
      });
      setStep('complete');

      if (onGenerationComplete) {
        onGenerationComplete(response.data);
      }
    } catch (error) {
      const errorMessage = error.response?.data?.details || error.response?.data?.error || 'COMPILATION FAILED';
      setCompilationStatus({
        success: false,
        message: errorMessage
      });
      setError(errorMessage);
      setStep('review');
    } finally {
      setIsCompiling(false);
    }
  };

  // Mock code generator for demonstration
  const generateMockCode = (lang, desc, fname) => {
    switch (lang) {
      case 'COBOL':
        return `       IDENTIFICATION DIVISION.
       PROGRAM-ID. ${fname.toUpperCase()}.
       AUTHOR. NECRO-BRIDGE AI.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT        PIC 9(8)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "RESULT: 0000.00".
           STOP RUN.`;
      
      case 'FORTRAN':
        return `      PROGRAM ${fname.toUpperCase()}
C     ${desc}
      REAL RESULT
      
      RESULT = 0.0
      WRITE(*,*) 'RESULT: ', RESULT
      
      END`;
      
      case 'PASCAL':
        return `PROGRAM ${filename};
(* ${description} *)

VAR
  result: REAL;

BEGIN
  result := 0.0;
  WRITELN('RESULT: ', result:0:2);
END.`;
      
      case 'BASIC':
        return `10 REM ${description}
20 LET RESULT = 0
30 PRINT "RESULT: "; RESULT
40 END`;
      
      default:
        return '// Code generation not implemented';
    }
  };

  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 bg-black bg-opacity-90 flex items-center justify-center z-50 p-4">
      <div className="bg-black border-4 border-mainframe-green max-w-4xl w-full max-h-[90vh] overflow-y-auto">
        {/* HEADER */}
        <div className="border-b-2 border-mainframe-green p-4 flex justify-between items-center">
          <h2 className="text-2xl font-mono text-mainframe-green">
            👻 SUMMON ANCIENT SPIRIT
          </h2>
          <button
            onClick={handleClose}
            className="text-mainframe-green hover:text-black hover:bg-mainframe-green px-3 py-1 border-2 border-mainframe-green font-mono"
          >
            [X]
          </button>
        </div>

        {/* CONTENT */}
        <div className="p-6 space-y-6">
          {/* INPUT STEP */}
          {step === 'input' && (
            <>
              {/* DESCRIPTION INPUT */}
              <div>
                <label className="block text-mainframe-green font-mono text-sm mb-2">
                  DESCRIBE THE CALCULATION:
                </label>
                <textarea
                  value={description}
                  onChange={(e) => setDescription(e.target.value)}
                  className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono px-4 py-2 focus:outline-none focus:ring-1 focus:ring-mainframe-green h-32 resize-none"
                  placeholder="Calculate compound interest with principal, rate, and time..."
                />
              </div>

              {/* LANGUAGE SELECTOR */}
              <div>
                <label className="block text-mainframe-green font-mono text-sm mb-2">
                  SELECT ANCIENT LANGUAGE:
                </label>
                <div className="grid grid-cols-2 gap-4">
                  {LANGUAGES.map((lang) => (
                    <button
                      key={lang.value}
                      onClick={() => setLanguage(lang.value)}
                      className={`p-4 border-2 font-mono text-left transition-colors ${
                        language === lang.value
                          ? 'bg-mainframe-green text-black border-mainframe-green'
                          : 'bg-black text-mainframe-green border-mainframe-green hover:bg-mainframe-green hover:text-black'
                      }`}
                    >
                      <div className="font-bold">{lang.label}</div>
                      <div className="text-sm opacity-80">{lang.description}</div>
                    </button>
                  ))}
                </div>
              </div>

              {/* FILENAME INPUT */}
              <div>
                <label className="block text-mainframe-green font-mono text-sm mb-2">
                  FILENAME (WITHOUT EXTENSION):
                </label>
                <input
                  type="text"
                  value={filename}
                  onChange={(e) => setFilename(e.target.value)}
                  className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono px-4 py-2 focus:outline-none focus:ring-1 focus:ring-mainframe-green"
                  placeholder="compound-interest"
                />
              </div>

              {/* ERROR MESSAGE */}
              {error && (
                <div className="text-mainframe-green font-mono text-sm border-2 border-mainframe-green p-3">
                  ⚠️ ERROR: {error}
                </div>
              )}

              {/* GENERATE BUTTON */}
              <button
                onClick={handleGenerate}
                disabled={isGenerating}
                className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-3 px-6 hover:bg-mainframe-green hover:text-black transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {isGenerating ? '>>> SUMMONING SPIRIT...' : '>>> GENERATE CODE <<<'}
              </button>
            </>
          )}

          {/* GENERATING STEP */}
          {step === 'generating' && (
            <div className="text-center py-12">
              <div className="text-mainframe-green font-mono text-xl mb-4">
                👻 CHANNELING ANCIENT SPIRITS...
              </div>
              <div className="text-mainframe-green font-mono text-sm space-y-2">
                <div>CONSULTING PUNCH CARD ARCHIVES...</div>
                <div>DECODING MAGNETIC TAPE WISDOM...</div>
                <div>INVOKING {language} COMPILER SPIRITS...</div>
              </div>
            </div>
          )}

          {/* REVIEW STEP */}
          {step === 'review' && (
            <>
              <div className="text-mainframe-green font-mono text-sm mb-2">
                GENERATED {language} CODE:
              </div>
              
              {/* PUNCH CARD VISUALIZATION FOR COBOL */}
              {language === 'COBOL' && (
                <div className="mb-4">
                  <div className="text-mainframe-green font-mono text-xs mb-2 text-center">
                    📇 PUNCH CARD REPRESENTATION
                  </div>
                  <div className="max-h-96 overflow-y-auto">
                    {generatedCode.split('\n').slice(0, 10).map((line, idx) => (
                      <div key={idx} className="mb-2">
                        <PunchCard text={line} maxColumns={80} />
                      </div>
                    ))}
                  </div>
                  <div className="text-mainframe-green font-mono text-xs text-center mt-2">
                    (Showing first 10 lines as punch cards)
                  </div>
                </div>
              )}
              
              <div className="bg-black border-2 border-mainframe-green p-4 overflow-x-auto">
                <pre className="text-mainframe-green font-mono text-sm whitespace-pre">
                  {generatedCode}
                </pre>
              </div>

              {/* ERROR MESSAGE */}
              {error && (
                <div className="text-mainframe-green font-mono text-sm border-2 border-mainframe-green p-3">
                  ⚠️ COMPILATION ERROR: {error}
                </div>
              )}

              {/* ACTION BUTTONS */}
              <div className="flex gap-4">
                <button
                  onClick={() => setStep('input')}
                  className="flex-1 bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-3 px-6 hover:bg-mainframe-green hover:text-black transition-colors"
                >
                  &lt;&lt; REGENERATE
                </button>
                <button
                  onClick={handleCompile}
                  disabled={isCompiling}
                  className="flex-1 bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-3 px-6 hover:bg-mainframe-green hover:text-black transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  {isCompiling ? 'COMPILING...' : '>>> COMPILE & SAVE <<<'}
                </button>
              </div>
            </>
          )}

          {/* COMPILING STEP */}
          {step === 'compiling' && (
            <div className="text-center py-12">
              <div className="text-mainframe-green font-mono text-xl mb-4">
                ⚙️ BINDING SPIRIT TO BINARY...
              </div>
              <div className="text-mainframe-green font-mono text-sm space-y-2">
                <div>SAVING TO LEGACY DIRECTORY...</div>
                <div>INVOKING {language} COMPILER...</div>
                <div>GENERATING EXECUTABLE...</div>
              </div>
            </div>
          )}

          {/* COMPLETE STEP */}
          {step === 'complete' && compilationStatus && (
            <>
              <div className="text-center py-8">
                {compilationStatus.success ? (
                  <>
                    <div className="text-mainframe-green font-mono text-2xl mb-4">
                      ✨ ANCIENT SPIRIT SUMMONED!
                    </div>
                    <div className="text-mainframe-green font-mono text-sm space-y-2">
                      <div>{compilationStatus.message}</div>
                      <div>FILE: {compilationStatus.filePath}</div>
                      <div>COMPILER: {compilationStatus.compilation?.compiler}</div>
                      <div className="mt-4 p-4 border-2 border-mainframe-green">
                        {compilationStatus.compilation?.message}
                      </div>
                    </div>
                  </>
                ) : (
                  <>
                    <div className="text-mainframe-green font-mono text-2xl mb-4">
                      ⚠️ SUMMONING INCOMPLETE
                    </div>
                    <div className="text-mainframe-green font-mono text-sm">
                      {compilationStatus.message}
                    </div>
                  </>
                )}
              </div>

              <button
                onClick={handleClose}
                className="w-full bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-3 px-6 hover:bg-mainframe-green hover:text-black transition-colors"
              >
                &lt;&lt; CLOSE
              </button>
            </>
          )}
        </div>
      </div>
    </div>
  );
}

export default CodeGeneratorModal;
