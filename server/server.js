const express = require('express');
const cors = require('cors');
const multer = require('multer');
const { exec } = require('child_process');
const path = require('path');
const bridgeFactory = require('./bridges/BridgeFactory');
const logger = require('./utils/logger');
const errorHandler = require('./utils/errorHandler');
const codeGenerator = require('./services/code-generator');

const app = express();
const PORT = 3001;

// CONFIGURE MULTER FOR FILE UPLOADS (IN-MEMORY STORAGE)
const upload = multer({ storage: multer.memoryStorage() });

// CONFIGURE CORS TO ALLOW REACT CLIENT COMMUNICATION
app.use(cors());

// PARSE JSON REQUEST BODIES
app.use(express.json());

// ═══════════════════════════════════════════════════════════
// REQUEST LOGGING MIDDLEWARE
// ═══════════════════════════════════════════════════════════
app.use((req, res, next) => {
  // LOG INCOMING REQUEST
  logger.logRequest(req);
  
  // CAPTURE ORIGINAL res.json TO LOG RESPONSES
  const originalJson = res.json.bind(res);
  res.json = function(data) {
    logger.logResponse(req, res.statusCode, data);
    return originalJson(data);
  };
  
  next();
});

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

// ═══════════════════════════════════════════════════════════
// AI CODE GENERATION ENDPOINT - SUMMON ANCIENT SPIRIT
// ═══════════════════════════════════════════════════════════
app.post('/api/generate', async (req, res) => {
  const { code, language, filename } = req.body;
  
  // VALIDATE INPUT
  if (!code || !language || !filename) {
    return res.status(400).json({
      error: 'INVALID REQUEST',
      details: 'MISSING REQUIRED PARAMETERS: code, language, filename',
      required: ['code', 'language', 'filename']
    });
  }
  
  // VALIDATE LANGUAGE
  const supportedLanguages = ['COBOL', 'FORTRAN', 'PASCAL', 'BASIC'];
  if (!supportedLanguages.includes(language.toUpperCase())) {
    return res.status(400).json({
      error: 'UNSUPPORTED LANGUAGE',
      details: `LANGUAGE '${language}' NOT SUPPORTED FOR CODE GENERATION`,
      supported: supportedLanguages
    });
  }
  
  try {
    logger.info('CODE GENERATION INITIATED', {
      language: language,
      filename: filename,
      code_length: code.length
    });
    
    // VALIDATE SYNTAX
    const validation = codeGenerator.validateSyntax(code, language);
    if (!validation.valid) {
      logger.warn('SYNTAX VALIDATION FAILED', {
        language: language,
        filename: filename,
        errors: validation.errors
      });
      
      return res.status(400).json({
        error: 'SYNTAX VALIDATION FAILED',
        details: validation.errors,
        message: '⚠️ GENERATED CODE CONTAINS SYNTAX ERRORS'
      });
    }
    
    // SUMMON THE ANCIENT SPIRIT (SAVE AND COMPILE)
    const result = await codeGenerator.summonAncientSpirit(code, language, filename);
    
    if (result.success) {
      logger.info('CODE GENERATION SUCCESSFUL', {
        language: language,
        filename: filename,
        file_path: result.saved.filePath
      });
      
      res.json({
        success: true,
        message: result.message,
        file_path: result.saved.filePath,
        language: language,
        filename: filename,
        compilation: {
          success: result.compiled.success,
          compiler: result.compiled.compiler,
          message: result.compiled.message
        }
      });
    } else {
      logger.error('CODE GENERATION FAILED', {
        language: language,
        filename: filename,
        error: result.error
      });
      
      res.status(500).json({
        success: false,
        error: 'CODE GENERATION FAILED',
        details: result.error,
        message: result.message
      });
    }
  } catch (error) {
    logger.error('CODE GENERATION ERROR', {
      language: language,
      filename: filename,
      error: error.message
    });
    
    res.status(500).json({
      error: 'CORE DUMP DETECTED',
      details: error.message,
      message: '❌ ANCIENT SPIRIT SUMMONING FAILED'
    });
  }
});

// ═══════════════════════════════════════════════════════════
// GET LANGUAGE TEMPLATE ENDPOINT
// ═══════════════════════════════════════════════════════════
app.get('/api/template/:language', (req, res) => {
  const { language } = req.params;
  
  try {
    const template = codeGenerator.loadTemplate(language);
    
    res.json({
      language: language,
      template: template,
      message: `📜 TEMPLATE LOADED FOR ${language.toUpperCase()}`
    });
  } catch (error) {
    res.status(400).json({
      error: 'TEMPLATE NOT FOUND',
      details: error.message,
      supported: ['COBOL', 'FORTRAN', 'PASCAL', 'BASIC']
    });
  }
});

// ═══════════════════════════════════════════════════════════
// GLOBAL ERROR HANDLING MIDDLEWARE (MUST BE LAST)
// ═══════════════════════════════════════════════════════════
app.use(errorHandler.expressErrorMiddleware());

// START THE SERVER
app.listen(PORT, () => {
  console.log('═══════════════════════════════════════════════════════');
  console.log('👻 UNIVERSAL NECRO-BRIDGE SERVER v2.0');
  console.log('═══════════════════════════════════════════════════════');
  console.log(`🔌 LISTENING ON PORT ${PORT}`);
  console.log('📡 CORS ENABLED FOR REACT CLIENT');
  console.log('📤 MULTER CONFIGURED FOR FILE UPLOADS');
  console.log('📋 COMPREHENSIVE LOGGING ENABLED');
  console.log('🛡️  ERROR RECOVERY MECHANISMS ACTIVE');
  console.log('⚡ READY TO RESURRECT ANCIENT SPIRITS:');
  console.log('   • COBOL (1959) - MORTGAGE CALCULATOR');
  console.log('   • FORTRAN (1957) - TRAJECTORY CALCULATOR');
  console.log('   • PASCAL (1970) - TAX CALCULATOR');
  console.log('   • BASIC (1983) - INTEREST CALCULATOR');
  console.log('═══════════════════════════════════════════════════════');
  
  logger.info('NECRO-BRIDGE SERVER STARTED', {
    port: PORT,
    supported_languages: bridgeFactory.getSupportedLanguages(),
    log_level: logger.logLevel
  });
});
