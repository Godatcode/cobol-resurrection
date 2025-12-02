const express = require('express');
const cors = require('cors');
const multer = require('multer');
const { exec } = require('child_process');
const path = require('path');
const bridgeFactory = require('./bridges/BridgeFactory');

const app = express();
const PORT = 3001;

// CONFIGURE MULTER FOR FILE UPLOADS (IN-MEMORY STORAGE)
const upload = multer({ storage: multer.memoryStorage() });

// CONFIGURE CORS TO ALLOW REACT CLIENT COMMUNICATION
app.use(cors());

// PARSE JSON REQUEST BODIES
app.use(express.json());

// ═══════════════════════════════════════════════════════════
// LANGUAGE DETECTION MIDDLEWARE
// ═══════════════════════════════════════════════════════════
const detectLanguage = (req, res, next) => {
  // EXTRACT LANGUAGE FROM URL PATH (e.g., /api/calculate/cobol)
  const languageFromPath = req.params.language;
  
  // EXTRACT LANGUAGE FROM REQUEST BODY
  const languageFromBody = req.body?.language;
  
  // DETERMINE LANGUAGE (PATH TAKES PRECEDENCE)
  const language = languageFromPath || languageFromBody || 'cobol';
  
  // VALIDATE LANGUAGE IS SUPPORTED USING BRIDGE FACTORY
  if (!bridgeFactory.isSupported(language)) {
    return res.status(400).json({
      error: 'UNSUPPORTED LANGUAGE',
      details: `LANGUAGE '${language}' NOT RECOGNIZED`,
      supported: bridgeFactory.getSupportedLanguages()
    });
  }
  
  // ATTACH LANGUAGE AND BRIDGE TO REQUEST OBJECT
  req.legacyLanguage = language.toLowerCase();
  req.legacyBridge = bridgeFactory.getBridge(language);
  
  next();
};

// ═══════════════════════════════════════════════════════════
// HEALTH CHECK ENDPOINT
// ═══════════════════════════════════════════════════════════
app.get('/api/health', (req, res) => {
  res.json({ 
    status: 'OPERATIONAL', 
    message: 'UNIVERSAL BRIDGE SERVER ONLINE - READY TO SUMMON ANCIENT SPIRITS',
    timestamp: new Date().toISOString(),
    supported_languages: bridgeFactory.getSupportedLanguages()
  });
});

// ═══════════════════════════════════════════════════════════
// LIST AVAILABLE LANGUAGES ENDPOINT
// ═══════════════════════════════════════════════════════════
app.get('/api/languages', (req, res) => {
  const languages = bridgeFactory.getLanguageMetadata();
  
  res.json({
    count: languages.length,
    languages: languages
  });
});

// ═══════════════════════════════════════════════════════════
// UNIVERSAL CALCULATION ENDPOINT (WITH LANGUAGE IN PATH)
// ═══════════════════════════════════════════════════════════
app.post('/api/calculate/:language', detectLanguage, async (req, res) => {
  const { legacyBridge } = req;
  const params = req.body;
  
  try {
    // EXECUTE CALCULATION USING BRIDGE PATTERN
    const result = await legacyBridge.execute(params);
    
    // RETURN SUCCESSFUL RESPONSE
    res.json(result);
  } catch (error) {
    // HANDLE ERRORS FROM BRIDGE
    const statusCode = error.error === 'INVALID INPUT' ? 400 : 500;
    res.status(statusCode).json(error);
  }
});

// ═══════════════════════════════════════════════════════════
// BACKWARD COMPATIBILITY - ORIGINAL COBOL ENDPOINT
// ═══════════════════════════════════════════════════════════
app.post('/api/calculate', async (req, res) => {
  const { principal, rate, term } = req.body;

  try {
    // GET COBOL BRIDGE
    const cobolBridge = bridgeFactory.getBridge('cobol');
    
    // EXECUTE CALCULATION USING BRIDGE PATTERN
    const result = await cobolBridge.execute({ principal, rate, term });
    
    // RETURN RESPONSE IN ORIGINAL FORMAT FOR BACKWARD COMPATIBILITY
    res.json({
      monthly_payment: result.result,
      source: 'COBOL_LEGACY_ENGINE'
    });
  } catch (error) {
    // HANDLE ERRORS FROM BRIDGE
    const statusCode = error.error === 'INVALID INPUT' ? 400 : 500;
    res.status(statusCode).json({
      error: error.error,
      details: error.details
    });
  }
});

// START THE SERVER
app.listen(PORT, () => {
  console.log('═══════════════════════════════════════════════════════');
  console.log('👻 UNIVERSAL NECRO-BRIDGE SERVER v2.0');
  console.log('═══════════════════════════════════════════════════════');
  console.log(`🔌 LISTENING ON PORT ${PORT}`);
  console.log('📡 CORS ENABLED FOR REACT CLIENT');
  console.log('📤 MULTER CONFIGURED FOR FILE UPLOADS');
  console.log('⚡ READY TO RESURRECT ANCIENT SPIRITS:');
  console.log('   • COBOL (1959) - MORTGAGE CALCULATOR');
  console.log('   • FORTRAN (1957) - TRAJECTORY CALCULATOR');
  console.log('   • PASCAL (1970) - TAX CALCULATOR');
  console.log('   • BASIC (1983) - INTEREST CALCULATOR');
  console.log('═══════════════════════════════════════════════════════');
});
