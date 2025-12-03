# 🤝 CONTRIBUTING GUIDELINES

WELCOME TO THE COBOL RESURRECTION BRIDGE PROJECT! WE APPRECIATE YOUR INTEREST IN CONTRIBUTING TO THIS CYBER-NECROMANCY ENDEAVOR.

---

## 📋 TABLE OF CONTENTS

1. [Code of Conduct](#code-of-conduct)
2. [Getting Started](#getting-started)
3. [Development Workflow](#development-workflow)
4. [Coding Standards](#coding-standards)
5. [Testing Requirements](#testing-requirements)
6. [Documentation Standards](#documentation-standards)
7. [Pull Request Process](#pull-request-process)
8. [Adding New Legacy Languages](#adding-new-legacy-languages)
9. [Community](#community)

---

## 📜 CODE OF CONDUCT

### OUR PLEDGE

WE ARE COMMITTED TO PROVIDING A WELCOMING AND INCLUSIVE ENVIRONMENT FOR ALL CONTRIBUTORS, REGARDLESS OF EXPERIENCE LEVEL, BACKGROUND, OR IDENTITY.

### EXPECTED BEHAVIOR

- ✅ BE RESPECTFUL AND CONSIDERATE
- ✅ PROVIDE CONSTRUCTIVE FEEDBACK
- ✅ FOCUS ON WHAT IS BEST FOR THE COMMUNITY
- ✅ SHOW EMPATHY TOWARDS OTHER CONTRIBUTORS
- ✅ EMBRACE VINTAGE COMPUTING HUMOR (MOTHS, NOT BUGS!)

### UNACCEPTABLE BEHAVIOR

- ❌ HARASSMENT OR DISCRIMINATORY LANGUAGE
- ❌ TROLLING OR INSULTING COMMENTS
- ❌ PERSONAL OR POLITICAL ATTACKS
- ❌ PUBLISHING OTHERS' PRIVATE INFORMATION

---

## 🚀 GETTING STARTED

### PREREQUISITES

BEFORE CONTRIBUTING, ENSURE YOU HAVE:

1. **Git** installed and configured
2. **Node.js 18+** and npm
3. **Legacy compilers** (at least one):
   - GnuCOBOL (cobc)
   - GNU Fortran (gfortran)
   - Free Pascal (fpc)
   - FreeBASIC (fbc)
4. **Kiro IDE** (optional, for testing automation features)

### FORK AND CLONE

```bash
# Fork the repository on GitHub
# Then clone your fork
git clone https://github.com/YOUR-USERNAME/cobol-resurrection-bridge.git
cd cobol-resurrection-bridge

# Add upstream remote
git remote add upstream https://github.com/ORIGINAL-OWNER/cobol-resurrection-bridge.git
```

### INSTALL DEPENDENCIES

```bash
# Root dependencies (testing)
npm install

# Server dependencies
cd server && npm install && cd ..

# Client dependencies
cd client && npm install && cd ..

# Toolkit dependencies (if contributing to toolkit)
cd toolkit && npm install && cd ..
```

### COMPILE LEGACY BINARIES

```bash
# Compile all languages
chmod +x compile-all.sh
./compile-all.sh

# Or compile individually
cd legacy/cobol && ./compile.sh && cd ../..
cd legacy/fortran && ./compile.sh && cd ../..
cd legacy/pascal && ./compile.sh && cd ../..
cd legacy/basic && ./compile.sh && cd ../..
```

### VERIFY SETUP

```bash
# Run tests
npm test

# Start development servers
# Terminal 1
cd server && npm start

# Terminal 2
cd client && npm run dev

# Terminal 3 - Test API
curl http://localhost:3001/api/health
```

---

## 🔄 DEVELOPMENT WORKFLOW

### BRANCHING STRATEGY

WE USE A SIMPLIFIED GIT FLOW:

```
main          - Production-ready code
develop       - Integration branch for features
feature/*     - New features
bugfix/*      - Bug fixes
hotfix/*      - Urgent production fixes
docs/*        - Documentation updates
```

### CREATING A FEATURE BRANCH

```bash
# Update your fork
git checkout develop
git pull upstream develop

# Create feature branch
git checkout -b feature/your-feature-name

# Make your changes
# ...

# Commit with descriptive messages
git add .
git commit -m "feat: Add support for Ada language"

# Push to your fork
git push origin feature/your-feature-name
```

### COMMIT MESSAGE CONVENTIONS

WE FOLLOW [CONVENTIONAL COMMITS](https://www.conventionalcommits.org/):

```
feat:     New feature
fix:      Bug fix
docs:     Documentation changes
style:    Code style changes (formatting, etc.)
refactor: Code refactoring
test:     Adding or updating tests
chore:    Maintenance tasks
perf:     Performance improvements
```

**EXAMPLES**:
```bash
git commit -m "feat: Add PL/I language support"
git commit -m "fix: Correct FORTRAN angle conversion"
git commit -m "docs: Update API documentation for PASCAL"
git commit -m "test: Add property tests for BASIC calculator"
git commit -m "refactor: Extract common bridge logic"
```

### KEEPING YOUR FORK UPDATED

```bash
# Fetch upstream changes
git fetch upstream

# Merge upstream develop into your branch
git checkout develop
git merge upstream/develop

# Rebase your feature branch
git checkout feature/your-feature-name
git rebase develop
```

---

## 💻 CODING STANDARDS

### JAVASCRIPT/NODE.JS

**STYLE GUIDE**: WE FOLLOW [AIRBNB JAVASCRIPT STYLE GUIDE](https://github.com/airbnb/javascript)

**KEY CONVENTIONS**:
```javascript
// Use const/let, not var
const principal = 200000;
let monthlyPayment = 0;

// Use arrow functions for callbacks
const calculate = (params) => {
  return processCalculation(params);
};

// Use template literals
const message = `RESULT: ${payment.toFixed(2)}`;

// Use async/await over callbacks
async function executeBinary(path, args) {
  const result = await spawn(path, args);
  return result;
}

// Destructure when appropriate
const { principal, rate, term } = req.body;

// Use meaningful variable names
const monthlyInterestRate = annualRate / 12 / 100;
```

**NAMING CONVENTIONS**:
- Variables/Functions: `camelCase`
- Classes: `PascalCase`
- Constants: `UPPER_SNAKE_CASE`
- Files: `kebab-case.js`

### REACT/JSX

**COMPONENT STRUCTURE**:
```jsx
// Functional components with hooks
import React, { useState, useEffect } from 'react';

const TerminalWindow = ({ logs, onClear }) => {
  const [isScrolled, setIsScrolled] = useState(false);
  
  useEffect(() => {
    // Side effects here
  }, [logs]);
  
  return (
    <div className="terminal-window">
      {logs.map((log, index) => (
        <div key={index} className="log-entry">
          {log.message}
        </div>
      ))}
    </div>
  );
};

export default TerminalWindow;
```

**PROP TYPES**:
```jsx
// Use TypeScript or PropTypes
TerminalWindow.propTypes = {
  logs: PropTypes.arrayOf(PropTypes.object).isRequired,
  onClear: PropTypes.func
};
```

### LEGACY LANGUAGES

**COBOL**:
```cobol
       * USE PROPER INDENTATION (COLUMNS 8-72)
       * UPPERCASE FOR KEYWORDS
       * DESCRIPTIVE VARIABLE NAMES WITH WS- PREFIX
       01 WS-MONTHLY-PAYMENT    COMP-2.
       
       * STRUCTURED PARAGRAPHS
       CALCULATE-MORTGAGE.
           COMPUTE WS-MONTHLY-PAYMENT = ...
           .
```

**FORTRAN**:
```fortran
C     COMMENTS START WITH C IN COLUMN 1
C     CODE STARTS IN COLUMN 7
C     UPPERCASE FOR KEYWORDS
      REAL VELOCITY, ANGLE, RANGE
      
C     DESCRIPTIVE VARIABLE NAMES
      PARAMETER (PI = 3.14159265)
```

**PASCAL**:
```pascal
{ Comments in curly braces }
{ PascalCase for identifiers }
VAR
    TotalAmount: Real;
    InterestRate: Real;

PROCEDURE CalculateTax(Income: Real);
BEGIN
    { Structured with BEGIN/END }
END;
```

**BASIC**:
```basic
10 REM COMMENTS WITH REM
20 REM LINE NUMBERS IN INCREMENTS OF 10
30 REM DESCRIPTIVE VARIABLE NAMES
40 LET PRINCIPAL = 10000
50 LET RATE = 5.0
```

---

## 🧪 TESTING REQUIREMENTS

### PROPERTY-BASED TESTS

**ALL NEW CALCULATIONS MUST HAVE PROPERTY TESTS**:

```javascript
// Feature: cobol-resurrection-bridge, Property X: Description
test('Property description', () => {
  fc.assert(
    fc.property(
      // Generators for input domain
      fc.integer({ min: 1000, max: 10000000 }),
      fc.float({ min: 0.01, max: 30.0 }),
      (principal, rate) => {
        // Test the property
        const result = calculate(principal, rate);
        expect(result).toSatisfy(someProperty);
      }
    ),
    { numRuns: 100 }  // Minimum 100 iterations
  );
});
```

**REQUIREMENTS**:
- Minimum 100 iterations per property
- Tag with feature name and property number
- Reference design document property
- Use appropriate generators for input domain
- Test universal properties, not specific examples

### UNIT TESTS

**COVER SPECIFIC EXAMPLES AND EDGE CASES**:

```javascript
describe('COBOL Mortgage Calculator', () => {
  test('Standard 30-year mortgage', () => {
    const result = executeCobol(200000, 5.5, 30);
    expect(result).toBeCloseTo(1135.58, 2);
  });
  
  test('Edge case: $1 principal', () => {
    const result = executeCobol(1, 5.5, 30);
    expect(result).toBeGreaterThan(0);
  });
  
  test('Error: Negative principal', () => {
    expect(() => executeCobol(-100, 5.5, 30))
      .toThrow('INVALID PRINCIPAL');
  });
});
```

### INTEGRATION TESTS

**TEST END-TO-END FLOWS**:

```javascript
describe('API Integration', () => {
  test('Complete calculation flow', async () => {
    const response = await request(app)
      .post('/api/calculate/cobol')
      .send({ principal: 200000, rate: 5.5, term: 30 });
    
    expect(response.status).toBe(200);
    expect(response.body.result).toBeCloseTo(1135.58, 2);
    expect(response.body.source).toBe('COBOL_LEGACY_ENGINE');
  });
});
```

### RUNNING TESTS

```bash
# Run all tests
npm test

# Run specific test file
npm test -- tests/cobol-mortgage.test.js

# Run with coverage
npm test -- --coverage

# Run in watch mode (development)
npm test -- --watch
```

### TEST COVERAGE REQUIREMENTS

- **Minimum 80% code coverage** for new code
- **100% coverage** for critical calculation logic
- **All error paths** must be tested
- **All API endpoints** must have integration tests

---

## 📚 DOCUMENTATION STANDARDS

### CODE COMMENTS

**JAVASCRIPT**:
```javascript
/**
 * Executes a legacy binary with provided arguments
 * 
 * @param {string} binaryPath - Path to compiled binary
 * @param {Array<number>} args - Command-line arguments
 * @returns {Promise<number>} - Calculated result
 * @throws {Error} - If binary execution fails
 */
async function executeBinary(binaryPath, args) {
  // Implementation
}
```

**LEGACY LANGUAGES**:
```cobol
      *****************************************************************
      * SUBROUTINE: CALCULATE-MORTGAGE                               *
      * PURPOSE:    COMPUTES MONTHLY PAYMENT USING AMORTIZATION      *
      * INPUTS:     WS-PRINCIPAL, WS-ANNUAL-RATE, WS-TERM-YEARS     *
      * OUTPUTS:    WS-MONTHLY-PAYMENT                               *
      * FORMULA:    M = P [ i(1 + i)^n ] / [ (1 + i)^n – 1 ]        *
      *****************************************************************
```

### API DOCUMENTATION

**UPDATE API-DOCUMENTATION.MD FOR NEW ENDPOINTS**:

```markdown
### POST /api/calculate/ada

EXECUTES ADA CALCULATION.

**Request Body:**
\`\`\`json
{
  "param1": 100,
  "param2": 50
}
\`\`\`

**Success Response (200):**
\`\`\`json
{
  "result": 150,
  "source": "ADA_LEGACY_ENGINE",
  "language": "ada",
  "year": 1980
}
\`\`\`
```

### README UPDATES

**KEEP README.MD CURRENT**:
- Update prerequisites if new compilers required
- Add new languages to supported list
- Update architecture diagrams if structure changes
- Add new API endpoints to documentation

### CHANGELOG

**MAINTAIN CHANGELOG.MD**:
```markdown
## [Unreleased]

### Added
- Support for Ada language (1980)
- New API endpoint: POST /api/calculate/ada
- Ada bridge implementation

### Changed
- Improved error handling in bridge factory

### Fixed
- FORTRAN angle conversion precision issue
```

---

## 🔀 PULL REQUEST PROCESS

### BEFORE SUBMITTING

**CHECKLIST**:
- [ ] Code follows style guidelines
- [ ] All tests pass (`npm test`)
- [ ] New tests added for new features
- [ ] Documentation updated
- [ ] Commit messages follow conventions
- [ ] Branch is up-to-date with develop
- [ ] No merge conflicts

### CREATING A PULL REQUEST

1. **PUSH YOUR BRANCH**:
   ```bash
   git push origin feature/your-feature-name
   ```

2. **OPEN PR ON GITHUB**:
   - Navigate to your fork on GitHub
   - Click "New Pull Request"
   - Base: `develop` ← Compare: `feature/your-feature-name`

3. **FILL OUT PR TEMPLATE**:
   ```markdown
   ## Description
   Brief description of changes
   
   ## Type of Change
   - [ ] Bug fix
   - [ ] New feature
   - [ ] Breaking change
   - [ ] Documentation update
   
   ## Testing
   - [ ] Unit tests added/updated
   - [ ] Property tests added/updated
   - [ ] Integration tests added/updated
   - [ ] Manual testing performed
   
   ## Checklist
   - [ ] Code follows style guidelines
   - [ ] Self-review completed
   - [ ] Documentation updated
   - [ ] No new warnings generated
   
   ## Related Issues
   Closes #123
   ```

### REVIEW PROCESS

1. **AUTOMATED CHECKS**:
   - CI/CD pipeline runs tests
   - Linting checks pass
   - Coverage requirements met

2. **CODE REVIEW**:
   - At least one maintainer approval required
   - Address all review comments
   - Make requested changes

3. **APPROVAL AND MERGE**:
   - Maintainer merges PR
   - Branch is deleted
   - Changes appear in develop

### AFTER MERGE

```bash
# Update your local repository
git checkout develop
git pull upstream develop

# Delete your feature branch
git branch -d feature/your-feature-name
git push origin --delete feature/your-feature-name
```

---

## 🆕 ADDING NEW LEGACY LANGUAGES

### STEP-BY-STEP GUIDE

**1. CREATE LEGACY PROGRAM**:

```bash
# Create directory
mkdir -p legacy/ada

# Write source code
# legacy/ada/calculation.adb

# Create compilation script
cat > legacy/ada/compile.sh << 'EOF'
#!/bin/bash
gnatmake -o calculation calculation.adb
EOF

chmod +x legacy/ada/compile.sh
```

**2. IMPLEMENT BRIDGE**:

```javascript
// server/bridges/AdaBridge.js
const LegacyBridge = require('./LegacyBridge');

class AdaBridge extends LegacyBridge {
  constructor() {
    super('./legacy/ada/calculation', 'ada', 1980);
  }
  
  validateParams(params) {
    // Validation logic
  }
  
  formatArgs(params) {
    // Format arguments for Ada binary
  }
}

module.exports = AdaBridge;
```

**3. UPDATE BRIDGE FACTORY**:

```javascript
// server/bridges/BridgeFactory.js
const AdaBridge = require('./AdaBridge');

class BridgeFactory {
  static createBridge(language) {
    switch(language) {
      // ... existing cases
      case 'ada': return new AdaBridge();
      default: throw new Error('Unsupported language');
    }
  }
}
```

**4. ADD API ENDPOINT**:

```javascript
// server/server.js
app.post('/api/calculate/ada', async (req, res) => {
  try {
    const bridge = BridgeFactory.createBridge('ada');
    const result = await bridge.execute(req.body);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: 'CORE DUMP DETECTED' });
  }
});
```

**5. ADD TESTS**:

```javascript
// tests/ada-calculation.test.js
describe('Ada Calculator', () => {
  test('Property: Calculation accuracy', () => {
    fc.assert(
      fc.property(
        fc.integer({ min: 1, max: 1000 }),
        (input) => {
          const result = executeAda(input);
          expect(result).toSatisfy(property);
        }
      ),
      { numRuns: 100 }
    );
  });
});
```

**6. UPDATE DOCUMENTATION**:

- Add to README.md supported languages
- Add to EXAMPLES.md with code samples
- Add to API-DOCUMENTATION.md
- Update ARCHITECTURE.md

**7. ADD TO TOOLKIT**:

```bash
# Create template
cp toolkit/templates/cobol-bridge.js toolkit/templates/ada-bridge.js
# Modify for Ada specifics

# Update CLI
# toolkit/src/cli/commands/init.ts
# Add 'ada' to language options
```

---

## 👥 COMMUNITY

### COMMUNICATION CHANNELS

- **GitHub Issues**: Bug reports and feature requests
- **GitHub Discussions**: General questions and ideas
- **Pull Requests**: Code contributions and reviews

### GETTING HELP

**STUCK ON SOMETHING?**

1. Check existing documentation (README, ARCHITECTURE, EXAMPLES)
2. Search GitHub Issues for similar problems
3. Ask in GitHub Discussions
4. Tag maintainers in your issue/PR

### RECOGNITION

**CONTRIBUTORS WILL BE**:
- Listed in CONTRIBUTORS.md
- Mentioned in release notes
- Credited in documentation

### MAINTAINERS

**CURRENT MAINTAINERS**:
- @maintainer1 - Project Lead
- @maintainer2 - Legacy Languages Expert
- @maintainer3 - UI/UX Specialist

---

## 🎯 CONTRIBUTION IDEAS

### GOOD FIRST ISSUES

**BEGINNER-FRIENDLY CONTRIBUTIONS**:
- Fix typos in documentation
- Add more test cases
- Improve error messages
- Add code comments
- Update dependencies

### INTERMEDIATE CONTRIBUTIONS

- Add new legacy language support
- Improve UI components
- Enhance error handling
- Optimize performance
- Add new API endpoints

### ADVANCED CONTRIBUTIONS

- Implement code translation between languages
- Add Docker support
- Create Kubernetes deployment
- Implement monitoring/observability
- Add CI/CD pipeline improvements

---

## 📜 LICENSE

BY CONTRIBUTING TO THIS PROJECT, YOU AGREE THAT YOUR CONTRIBUTIONS WILL BE LICENSED UNDER THE MIT LICENSE.

---

## 🙏 THANK YOU

THANK YOU FOR CONTRIBUTING TO THE COBOL RESURRECTION BRIDGE! YOUR EFFORTS HELP KEEP VINTAGE COMPUTING ALIVE AND ACCESSIBLE TO MODERN DEVELOPERS.

**TOGETHER, WE RESURRECT THE PAST TO BUILD THE FUTURE.**

---

**CONTRIBUTION STATUS**: WELCOME  
**COMMUNITY**: ACTIVE  
**LEGACY SYSTEMS**: IMMORTAL  

`[END OF TAPE]`
