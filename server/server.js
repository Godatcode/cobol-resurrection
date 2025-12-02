const express = require('express');
const cors = require('cors');
const { exec } = require('child_process');
const path = require('path');

const app = express();
const PORT = 3001;

// CONFIGURE CORS TO ALLOW REACT CLIENT COMMUNICATION
app.use(cors());

// PARSE JSON REQUEST BODIES
app.use(express.json());

// HEALTH CHECK ENDPOINT
app.get('/api/health', (req, res) => {
  res.json({ 
    status: 'OPERATIONAL', 
    message: 'BRIDGE SERVER ONLINE - READY TO SUMMON COBOL SPIRITS',
    timestamp: new Date().toISOString()
  });
});

// MORTGAGE CALCULATION ENDPOINT - SPAWNS COBOL BINARY
app.post('/api/calculate', (req, res) => {
  const { principal, rate, term } = req.body;

  // INPUT VALIDATION
  if (principal === undefined || rate === undefined || term === undefined) {
    return res.status(400).json({
      error: 'INVALID INPUT',
      details: 'PRINCIPAL, RATE, AND TERM ARE REQUIRED PARAMETERS'
    });
  }

  if (typeof principal !== 'number' || typeof rate !== 'number' || typeof term !== 'number') {
    return res.status(400).json({
      error: 'INVALID INPUT',
      details: 'ALL PARAMETERS MUST BE NUMERIC VALUES'
    });
  }

  if (principal <= 0 || rate <= 0 || term <= 0) {
    return res.status(400).json({
      error: 'INVALID INPUT',
      details: 'ALL PARAMETERS MUST BE POSITIVE VALUES'
    });
  }

  // CONSTRUCT PATH TO COBOL BINARY
  const cobolBinaryPath = path.join(__dirname, '..', 'legacy', 'mortgage');
  const command = `${cobolBinaryPath} ${principal} ${rate} ${term}`;

  // SPAWN COBOL PROCESS WITH TIMEOUT
  exec(command, { timeout: 5000 }, (error, stdout, stderr) => {
    // HANDLE EXECUTION ERRORS
    if (error) {
      // TIMEOUT ERROR
      if (error.killed) {
        return res.status(500).json({
          error: 'CORE DUMP DETECTED',
          details: 'COBOL PROCESS EXCEEDED 5 SECOND TIMEOUT'
        });
      }

      // BINARY NOT FOUND OR NON-ZERO EXIT CODE
      return res.status(500).json({
        error: 'CORE DUMP DETECTED',
        details: stderr || error.message
      });
    }

    // PARSE OUTPUT USING REGEX PATTERN
    const resultPattern = /RESULT:\s*(\d+\.\d{2})/;
    const match = stdout.match(resultPattern);

    if (!match) {
      return res.status(500).json({
        error: 'CORE DUMP DETECTED',
        details: 'FAILED TO PARSE COBOL OUTPUT'
      });
    }

    // EXTRACT MONTHLY PAYMENT VALUE
    const monthlyPayment = parseFloat(match[1]);

    // RETURN SUCCESSFUL RESPONSE
    res.json({
      monthly_payment: monthlyPayment,
      source: 'COBOL_LEGACY_ENGINE'
    });
  });
});

// START THE SERVER
app.listen(PORT, () => {
  console.log('═══════════════════════════════════════════════════════');
  console.log('👻 NECRO-BANK BRIDGE SERVER v1.0');
  console.log('═══════════════════════════════════════════════════════');
  console.log(`🔌 LISTENING ON PORT ${PORT}`);
  console.log('📡 CORS ENABLED FOR REACT CLIENT');
  console.log('⚡ READY TO RESURRECT COBOL BINARIES');
  console.log('═══════════════════════════════════════════════════════');
});
