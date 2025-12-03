# 📁 Recommended Directory Structure

This document shows the recommended directory structure for your legacy bridge project.

## Basic Single-Language Setup

```
my-legacy-bridge/
├── legacy/
│   └── cobol/
│       └── mortgage              # Your compiled COBOL binary
├── LegacyBridge.js              # Base class (from templates)
├── cobol-bridge.js              # Your customized bridge
├── package.json                 # NPM configuration
├── test-bridge.js               # Test script
└── README.md                    # Your project documentation
```

## Multi-Language Universal Setup

```
my-legacy-bridge/
├── legacy/
│   ├── cobol/
│   │   └── mortgage             # COBOL binary
│   ├── fortran/
│   │   └── trajectory           # FORTRAN binary
│   ├── pascal/
│   │   └── tax                  # PASCAL binary
│   └── basic/
│       └── interest             # BASIC binary
├── bridges/
│   ├── LegacyBridge.js         # Base class
│   ├── CobolBridge.js          # COBOL bridge
│   ├── FortranBridge.js        # FORTRAN bridge
│   ├── PascalBridge.js         # PASCAL bridge
│   └── BasicBridge.js          # BASIC bridge
├── universal-server.js          # Multi-language server
├── package.json                 # NPM configuration
├── test-bridge.js               # Test script
└── README.md                    # Your project documentation
```

## Production-Ready Setup

```
my-legacy-bridge/
├── legacy/                      # Legacy binaries
│   ├── cobol/
│   │   ├── mortgage
│   │   └── loan
│   ├── fortran/
│   │   └── trajectory
│   ├── pascal/
│   │   └── tax
│   └── basic/
│       └── interest
├── src/
│   ├── bridges/                 # Bridge implementations
│   │   ├── LegacyBridge.js
│   │   ├── CobolBridge.js
│   │   ├── FortranBridge.js
│   │   ├── PascalBridge.js
│   │   └── BasicBridge.js
│   ├── utils/                   # Utility functions
│   │   ├── logger.js
│   │   ├── errorHandler.js
│   │   └── validator.js
│   ├── middleware/              # Express middleware
│   │   ├── auth.js
│   │   └── rateLimit.js
│   └── server.js                # Main server file
├── tests/                       # Test files
│   ├── cobol.test.js
│   ├── fortran.test.js
│   ├── pascal.test.js
│   └── basic.test.js
├── config/                      # Configuration files
│   ├── development.json
│   ├── production.json
│   └── test.json
├── logs/                        # Log files (gitignored)
├── .env                         # Environment variables (gitignored)
├── .gitignore
├── package.json
├── README.md
└── LICENSE
```

## Docker Setup

```
my-legacy-bridge/
├── legacy/                      # Legacy binaries
│   └── ...
├── src/                         # Source code
│   └── ...
├── Dockerfile                   # Docker configuration
├── docker-compose.yml           # Docker Compose configuration
├── .dockerignore
├── package.json
└── README.md
```

**Dockerfile Example:**
```dockerfile
FROM node:18-alpine

# Install legacy compilers (if needed)
RUN apk add --no-cache \
    gnu-cobol \
    gfortran \
    fpc

WORKDIR /app

COPY package*.json ./
RUN npm install --production

COPY . .

EXPOSE 3001

CMD ["node", "src/server.js"]
```

## Key Principles

### 1. Separate Binaries by Language
```
legacy/
├── cobol/
├── fortran/
├── pascal/
└── basic/
```

This makes it easy to:
- Locate binaries
- Add new languages
- Manage different compiler outputs

### 2. Keep Bridges Organized
```
src/bridges/
├── LegacyBridge.js      # Base class
├── CobolBridge.js       # Language-specific
├── FortranBridge.js
└── ...
```

Benefits:
- Clear separation of concerns
- Easy to add new bridges
- Testable in isolation

### 3. Configuration Management
```
config/
├── development.json
├── production.json
└── test.json
```

Store environment-specific settings:
- Binary paths
- Timeouts
- API keys
- Database connections

### 4. Logging and Monitoring
```
logs/
├── error.log
├── access.log
└── debug.log
```

Essential for production:
- Track errors
- Monitor performance
- Debug issues

## File Naming Conventions

### Bridge Files
- Use PascalCase: `CobolBridge.js`, `FortranBridge.js`
- Match language name: `<Language>Bridge.js`

### Binary Files
- Use lowercase: `mortgage`, `trajectory`, `tax`
- No file extensions (Unix convention)
- Descriptive names: `mortgage-calculator` not `calc`

### Configuration Files
- Use lowercase with hyphens: `docker-compose.yml`
- Environment-specific: `config.development.json`

### Test Files
- Match source file: `CobolBridge.test.js`
- Or by feature: `mortgage-calculation.test.js`

## .gitignore Recommendations

```gitignore
# Dependencies
node_modules/

# Environment variables
.env
.env.local

# Logs
logs/
*.log

# Build outputs
dist/
build/

# OS files
.DS_Store
Thumbs.db

# IDE files
.vscode/
.idea/
*.swp

# Test coverage
coverage/

# Optional: Compiled binaries (if you compile them locally)
# legacy/*/
```

## Environment Variables

Create a `.env` file (don't commit this!):

```bash
# Server Configuration
PORT=3001
NODE_ENV=production

# Binary Paths (optional, if not using default)
COBOL_BINARY_PATH=/custom/path/to/cobol
FORTRAN_BINARY_PATH=/custom/path/to/fortran

# Timeouts (milliseconds)
EXECUTION_TIMEOUT=5000

# Logging
LOG_LEVEL=info
LOG_FILE=./logs/app.log

# Security (if using authentication)
API_KEY=your-secret-key
JWT_SECRET=your-jwt-secret
```

## Tips

1. **Start Simple** - Begin with the basic structure, add complexity as needed
2. **Use Absolute Paths** - Avoid relative path issues in production
3. **Document Everything** - Future you will thank present you
4. **Version Control** - Use git from day one
5. **Automate Testing** - Set up CI/CD early

## Next Steps

1. Choose a structure that fits your needs
2. Copy the template files
3. Organize your binaries
4. Configure your bridges
5. Test thoroughly
6. Deploy!

Happy organizing! 📦
