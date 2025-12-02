const express = require('express');
const cors = require('cors');

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
