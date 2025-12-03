# @necro-bridge/core

> 🏛️ **Universal Bridge Pattern Framework for Legacy System Modernization**

Resurrect your ancient COBOL, FORTRAN, PASCAL, and BASIC programs by bridging them to modern web applications. Born from the COBOL Resurrection Bridge project, this toolkit enables cyber-necromancy at scale.

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![Node Version](https://img.shields.io/badge/node-%3E%3D18.0.0-brightgreen)](https://nodejs.org)

## 🎯 What is Necro-Bridge?

Necro-Bridge is a universal bridge pattern framework that allows you to:

- ✨ **Resurrect Legacy Code**: Keep your battle-tested COBOL/FORTRAN/PASCAL/BASIC logic alive
- 🌉 **Bridge to Modern APIs**: Expose legacy binaries through REST APIs
- 🤖 **Auto-Detection**: Automatically discover and configure legacy binaries
- 📦 **Template-Based**: Pre-built templates for common legacy languages
- 🔧 **CLI Tools**: Quick setup and testing from the command line

## 🚀 Quick Start

### Installation

```bash
npm install -g @necro-bridge/core
```

### Option 1: Interactive Setup Wizard (Recommended for First-Time Users)

```bash
necro-bridge setup
```

The setup wizard will guide you through:
- Project configuration
- Language selection
- Compiler detection
- Directory structure creation
- Example program generation

### Option 2: Manual Initialization

```bash
# Create a bridge for your COBOL program
necro-bridge init cobol

# Or for FORTRAN, PASCAL, BASIC
necro-bridge init fortran
necro-bridge init pascal
necro-bridge init basic
```

### Auto-Detect Legacy Binaries

```bash
necro-bridge detect
```

This will:
- Scan for compiled binaries
- Check installed compilers
- Generate configuration file
- Link binaries to source files

### Start the Bridge Server

```bash
necro-bridge serve
```

### Test Your Legacy Binary

```bash
necro-bridge test ./legacy/mortgage
```

## 📖 Usage Example

### 1. You Have a COBOL Program

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MORTGAGE-CALC.

PROCEDURE DIVISION.
    ACCEPT WS-PRINCIPAL.
    ACCEPT WS-RATE.
    ACCEPT WS-TERM.
    
    COMPUTE WS-PAYMENT = ...
    
    DISPLAY "RESULT: " WS-PAYMENT.
    STOP RUN.
```

### 2. Initialize the Bridge

```bash
necro-bridge init cobol
```

This creates:
- `bridge-server.js` - Express API wrapper
- `config.json` - Bridge configuration
- Example API endpoints

### 3. Start the Server

```bash
necro-bridge serve
```

### 4. Call Your Legacy Code via REST API

```bash
curl -X POST http://localhost:3001/api/calculate \
  -H "Content-Type: application/json" \
  -d '{"principal": 200000, "rate": 5.5, "term": 30}'
```

Response:
```json
{
  "monthly_payment": 1135.58,
  "source": "COBOL_LEGACY_ENGINE"
}
```

## 🏗️ Architecture

```
┌─────────────────────────────────────────┐
│     Modern Web Application (React)      │
└────────────────┬────────────────────────┘
                 │ HTTP REST API
                 ▼
┌─────────────────────────────────────────┐
│    Necro-Bridge Server (Node.js)        │
│  ┌──────────┐  ┌──────────┐            │
│  │ Router   │─▶│ Process  │            │
│  │          │  │ Spawner  │            │
│  └──────────┘  └──────────┘            │
└────────────────┬────────────────────────┘
                 │ child_process.exec()
                 ▼
┌─────────────────────────────────────────┐
│   Legacy Binary (COBOL/FORTRAN/etc)    │
│         Compiled Executable             │
└─────────────────────────────────────────┘
```

## 🛠️ CLI Commands

### `necro-bridge setup`

**Interactive setup wizard for first-time users.**

Guides you through project initialization with questions about:
- Project name and configuration
- Languages to support
- Directory structure
- Compiler availability
- Example program generation

**Example:**
```bash
necro-bridge setup
# Follow the interactive prompts
```

See [AUTO-DETECTION.md](./AUTO-DETECTION.md) for detailed documentation.

### `necro-bridge init <language>`

Initialize a bridge for a specific legacy language.

**Supported Languages:**
- `cobol` - COBOL (1959)
- `fortran` - FORTRAN (1957)
- `pascal` - Pascal (1970)
- `basic` - BASIC (1983)

**Example:**
```bash
necro-bridge init cobol
```

### `necro-bridge detect`

**Auto-detect legacy binaries in your project directory and generate configuration.**

Features:
- Scans directories recursively for executable binaries
- Detects language based on directory structure and source files
- Checks for installed compilers in system PATH
- Generates `necro-bridge.config.json` with intelligent defaults
- Links binaries to their source files

**Options:**
- `--directory <path>` - Directory to scan (default: `./legacy`)

**Example:**
```bash
necro-bridge detect
# 🔧 CHECKING FOR INSTALLED COMPILERS...
#    ✓ COBOL    INSTALLED (/usr/local/bin/cobc)
#    ✓ FORTRAN  INSTALLED (/usr/local/bin/gfortran)
# 
# 📡 SCANNING FOR LEGACY BINARIES...
#    ✓ Found: ./legacy/cobol/mortgage (COBOL)
#    ✓ Found: ./legacy/fortran/trajectory (FORTRAN)
# 
# ✅ FOUND 2 LEGACY BINARY(IES)
# ✓ Configuration saved to: necro-bridge.config.json
```

See [AUTO-DETECTION.md](./AUTO-DETECTION.md) for detailed documentation.

### `necro-bridge serve`

Start the bridge server with your configured legacy binaries.

**Options:**
- `--port <number>` - Server port (default: 3001)
- `--config <path>` - Config file path

**Example:**
```bash
necro-bridge serve --port 8080
```

### `necro-bridge test <binary>`

Test a legacy binary integration.

**Example:**
```bash
necro-bridge test ./legacy/mortgage 200000 5.5 30
# Testing: ./legacy/mortgage
# Output: RESULT: 1135.58
# ✓ Binary executed successfully
```

## 📦 Supported Languages

| Language | Era  | Compiler | Status |
|----------|------|----------|--------|
| COBOL    | 1959 | GnuCOBOL | ✅ Supported |
| FORTRAN  | 1957 | gfortran | ✅ Supported |
| Pascal   | 1970 | fpc      | ✅ Supported |
| BASIC    | 1983 | fbc      | ✅ Supported |

## 🎨 Features

### Universal Bridge Pattern

The bridge pattern treats legacy binaries as microservices:
- Spawn processes on demand
- Parse STDOUT/STDERR
- Transform to JSON responses
- Handle errors gracefully

### Template System

Pre-built templates for each language include:
- Express route handlers
- Process spawning logic
- Output parsing (regex-based)
- Error handling
- Input validation

### Auto-Detection

Automatically discovers:
- **Compiled binaries** in project directories (recursive scan)
- **Legacy source files** (.cbl, .cob, .f, .f90, .for, .pas, .pp, .bas, .bi)
- **Installed compilers** in system PATH with full paths
- **Language detection** based on directory structure and file extensions
- **Binary-to-source linking** for traceability
- **Intelligent configuration** with language-specific defaults

The auto-detection system eliminates manual configuration by scanning your project and generating a complete `necro-bridge.config.json` file. See [AUTO-DETECTION.md](./AUTO-DETECTION.md) for complete documentation.

## 🔧 Configuration

Create `necro-bridge.config.json`:

```json
{
  "binaries": [
    {
      "name": "mortgage",
      "path": "./legacy/mortgage",
      "language": "cobol",
      "endpoint": "/api/calculate",
      "parameters": ["principal", "rate", "term"],
      "outputPattern": "RESULT:\\s*(\\d+\\.\\d{2})"
    }
  ],
  "server": {
    "port": 3001,
    "timeout": 5000
  }
}
```

## 📚 API Documentation

### Standard Response Format

**Success:**
```json
{
  "result": <value>,
  "source": "<LANGUAGE>_LEGACY_ENGINE",
  "timestamp": "2025-12-03T10:30:00Z"
}
```

**Error:**
```json
{
  "error": "CORE DUMP DETECTED",
  "details": "Error message",
  "timestamp": "2025-12-03T10:30:00Z"
}
```

## 🧪 Testing

The toolkit includes testing utilities:

```javascript
const { testBinary } = require('@necro-bridge/core');

testBinary('./legacy/mortgage', [200000, 5.5, 30])
  .then(result => console.log('Success:', result))
  .catch(error => console.error('Error:', error));
```

## 🤝 Contributing

Contributions welcome! This project was born from a hackathon and thrives on community input.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

## 🎭 Project Origins

Born from the **COBOL Resurrection Bridge** hackathon project, demonstrating "cyber-necromancy" by resurrecting vintage programming languages (1950s-1980s) within modern web environments.

## 🔗 Links

- **GitHub**: https://github.com/your-username/cobol-resurrection-bridge
- **Demo**: https://necro-bridge-demo.vercel.app
- **Documentation**: https://necro-bridge.dev

## 💀 Philosophy

> *"The best code is code that never dies. Legacy systems contain decades of battle-tested business logic. Rather than rewriting from scratch, we resurrect and bridge."*

---

**Built with 👻 by the COBOL Resurrection Bridge Team**

*Bringing the dead back to life, one binary at a time.*
