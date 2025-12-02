/**
 * NECRO-BRIDGE CODE GENERATOR SERVICE
 * AI-Powered Legacy Code Generation
 * Generates syntactically correct vintage code from natural language
 */

const fs = require('fs');
const path = require('path');
const { exec } = require('child_process');
const { promisify } = require('util');

const execAsync = promisify(exec);

// Language configuration
const LANGUAGE_CONFIG = {
  COBOL: {
    extension: '.cbl',
    directory: 'legacy/cobol',
    compiler: 'cobc',
    compileCommand: (filename) => `cobc -x -o legacy/cobol/${filename} legacy/cobol/${filename}.cbl`,
    templatePath: '.kiro/mcp/prompts/cobol-template.md'
  },
  FORTRAN: {
    extension: '.f',
    directory: 'legacy/fortran',
    compiler: 'gfortran',
    compileCommand: (filename) => `gfortran -o legacy/fortran/${filename} legacy/fortran/${filename}.f`,
    templatePath: '.kiro/mcp/prompts/fortran-template.md'
  },
  PASCAL: {
    extension: '.pas',
    directory: 'legacy/pascal',
    compiler: 'fpc',
    compileCommand: (filename) => `fpc -olegacy/pascal/${filename} legacy/pascal/${filename}.pas`,
    templatePath: '.kiro/mcp/prompts/pascal-template.md'
  },
  BASIC: {
    extension: '.bas',
    directory: 'legacy/basic',
    compiler: 'fbc',
    compileCommand: (filename) => `fbc -o legacy/basic/${filename} legacy/basic/${filename}.bas`,
    templatePath: '.kiro/mcp/prompts/basic-template.md'
  }
};

/**
 * Save generated code to file
 * @param {string} code - The generated source code
 * @param {string} language - Target language (COBOL, FORTRAN, PASCAL, BASIC)
 * @param {string} filename - Output filename without extension
 * @returns {Promise<object>} Result with file path and status
 */
async function saveGeneratedCode(code, language, filename) {
  const config = LANGUAGE_CONFIG[language.toUpperCase()];
  
  if (!config) {
    throw new Error(`Unsupported language: ${language}`);
  }
  
  // Ensure directory exists
  const dirPath = path.join(process.cwd(), config.directory);
  if (!fs.existsSync(dirPath)) {
    fs.mkdirSync(dirPath, { recursive: true });
  }
  
  // Construct file path
  const filePath = path.join(dirPath, `${filename}${config.extension}`);
  
  // Write code to file
  fs.writeFileSync(filePath, code, 'utf8');
  
  return {
    success: true,
    filePath: filePath,
    language: language,
    filename: filename
  };
}

/**
 * Compile the generated code
 * @param {string} language - Target language
 * @param {string} filename - Filename without extension
 * @returns {Promise<object>} Compilation result
 */
async function compileCode(language, filename) {
  const config = LANGUAGE_CONFIG[language.toUpperCase()];
  
  if (!config) {
    throw new Error(`Unsupported language: ${language}`);
  }
  
  const command = config.compileCommand(filename);
  
  try {
    const { stdout, stderr } = await execAsync(command);
    return {
      success: true,
      compiler: config.compiler,
      stdout: stdout,
      stderr: stderr,
      message: `👻 Ancient Spirit Rebound to Binary: ${filename}`
    };
  } catch (error) {
    return {
      success: false,
      compiler: config.compiler,
      error: error.message,
      stdout: error.stdout,
      stderr: error.stderr,
      message: `⚠️ Compilation Failed: ${error.message}`
    };
  }
}

/**
 * Load language template for AI prompting
 * @param {string} language - Target language
 * @returns {string} Template content
 */
function loadTemplate(language) {
  const config = LANGUAGE_CONFIG[language.toUpperCase()];
  
  if (!config) {
    throw new Error(`Unsupported language: ${language}`);
  }
  
  const templatePath = path.join(process.cwd(), config.templatePath);
  
  if (fs.existsSync(templatePath)) {
    return fs.readFileSync(templatePath, 'utf8');
  }
  
  return `# ${language} Template\nNo template found. Generate code following ${language} syntax conventions.`;
}

/**
 * Generate code using AI (placeholder for Kiro integration)
 * This function would be called by Kiro's AI with the appropriate prompts
 * @param {string} description - Natural language description
 * @param {string} language - Target language
 * @param {string} filename - Output filename
 * @returns {Promise<object>} Generation result
 */
async function generateCode(description, language, filename) {
  // Load the appropriate template
  const template = loadTemplate(language);
  
  // In actual implementation, this would call Kiro's AI
  // For now, return structure that Kiro can use
  return {
    template: template,
    description: description,
    language: language,
    filename: filename,
    instructions: [
      `Generate ${language} code that implements: ${description}`,
      `Follow the syntax rules in the template`,
      `Ensure output format is "RESULT: XXXX.XX"`,
      `Include proper error handling`,
      `Use era-appropriate conventions (${LANGUAGE_CONFIG[language.toUpperCase()].extension})`
    ]
  };
}

/**
 * Complete workflow: Generate, Save, and Compile
 * @param {string} code - Generated code
 * @param {string} language - Target language
 * @param {string} filename - Output filename
 * @returns {Promise<object>} Complete result
 */
async function summonAncientSpirit(code, language, filename) {
  try {
    // Save the generated code
    const saveResult = await saveGeneratedCode(code, language, filename);
    
    // Compile the code
    const compileResult = await compileCode(language, filename);
    
    return {
      success: compileResult.success,
      saved: saveResult,
      compiled: compileResult,
      message: compileResult.success 
        ? `✨ Ancient Spirit Summoned: ${filename}${LANGUAGE_CONFIG[language.toUpperCase()].extension}`
        : `⚠️ Spirit Summoned but Compilation Failed: ${compileResult.message}`
    };
  } catch (error) {
    return {
      success: false,
      error: error.message,
      message: `❌ Summoning Failed: ${error.message}`
    };
  }
}

/**
 * Validate code syntax (basic checks)
 * @param {string} code - Source code
 * @param {string} language - Target language
 * @returns {object} Validation result
 */
function validateSyntax(code, language) {
  const config = LANGUAGE_CONFIG[language.toUpperCase()];
  
  if (!config) {
    return { valid: false, errors: [`Unsupported language: ${language}`] };
  }
  
  const errors = [];
  
  // Basic validation checks
  if (!code || code.trim().length === 0) {
    errors.push('Code is empty');
  }
  
  // Language-specific checks
  switch (language.toUpperCase()) {
    case 'COBOL':
      if (!code.includes('IDENTIFICATION DIVISION')) {
        errors.push('Missing IDENTIFICATION DIVISION');
      }
      if (!code.includes('PROCEDURE DIVISION')) {
        errors.push('Missing PROCEDURE DIVISION');
      }
      break;
      
    case 'FORTRAN':
      if (!code.includes('PROGRAM ') && !code.includes('SUBROUTINE ')) {
        errors.push('Missing PROGRAM or SUBROUTINE declaration');
      }
      break;
      
    case 'PASCAL':
      if (!code.includes('PROGRAM ')) {
        errors.push('Missing PROGRAM declaration');
      }
      if (!code.includes('BEGIN')) {
        errors.push('Missing BEGIN statement');
      }
      if (!code.includes('END.')) {
        errors.push('Missing END. statement');
      }
      break;
      
    case 'BASIC':
      // Check for line numbers
      const lines = code.split('\n');
      const hasLineNumbers = lines.some(line => /^\d+\s/.test(line.trim()));
      if (!hasLineNumbers) {
        errors.push('Missing line numbers (vintage BASIC style)');
      }
      break;
  }
  
  return {
    valid: errors.length === 0,
    errors: errors
  };
}

module.exports = {
  saveGeneratedCode,
  compileCode,
  loadTemplate,
  generateCode,
  summonAncientSpirit,
  validateSyntax,
  LANGUAGE_CONFIG
};
