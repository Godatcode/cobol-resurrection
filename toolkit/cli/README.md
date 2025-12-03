# Necro-Bridge CLI Documentation

## Overview

The Necro-Bridge CLI provides a command-line interface for initializing, detecting, serving, and testing legacy system bridges. It's the primary tool for working with the @necro-bridge/core toolkit.

## Installation

```bash
npm install -g @necro-bridge/core
```

## Commands

### `necro-bridge init <language>`

Initialize a bridge for a specific legacy language.

**Supported Languages:**
- `cobol` - COBOL (1959)
- `fortran` - FORTRAN (1957)
- `pascal` - Pascal (1970)
- `basic` - BASIC (1983)

**What it creates:**
- Directory structure (`legacy/<language>`, `server`, `server/bridges`)
- Bridge class file (`server/bridges/<Language>Bridge.js`)
- Server file (`server/server.js`)
- Configuration file (`necro-bridge.config.json`)
- Example legacy program (`legacy/<language>/example.<ext>`)
- Package.json for server dependencies

**Example:**
```bash
necro-bridge init cobol
```

**Output:**
```
═══════════════════════════════════════════════════════
👻 INITIALIZING COBOL BRIDGE
═══════════════════════════════════════════════════════
✓ Created directory: legacy
✓ Created directory: legacy/cobol
✓ Created directory: server/bridges
✓ Generated bridge: server/bridges/CobolBridge.js
✓ Generated server: server/server.js
✓ Generated config: necro-bridge.config.json
✓ Generated example: legacy/cobol/example.cbl
✓ Generated package.json: server/package.json
═══════════════════════════════════════════════════════
✅ INITIALIZATION COMPLETE
═══════════════════════════════════════════════════════
```

### `necro-bridge detect`

Auto-detect legacy binaries in your project directory.

**Options:**
- `-d, --directory <path>` - Directory to scan (default: `./legacy`)

**What it does:**
- Checks for installed compilers (cobc, gfortran, fpc, fbc)
- Scans directory for executable binaries
- Detects language based on directory structure and source files
- Generates configuration file with detected binaries

**Example:**
```bash
necro-bridge detect
necro-bridge detect --directory ./my-legacy-code
```

**Output:**
```
═══════════════════════════════════════════════════════
🔍 SCANNING FOR LEGACY BINARIES
═══════════════════════════════════════════════════════
📂 Directory: ./legacy

🔧 CHECKING FOR INSTALLED COMPILERS...
   ✓ COBOL    INSTALLED
   ✓ FORTRAN  INSTALLED
   ✓ PASCAL   INSTALLED
   ✗ BASIC    NOT FOUND

📡 SCANNING FOR LEGACY BINARIES...
   ✓ Found: ./legacy/cobol/mortgage (COBOL)
   ✓ Found: ./legacy/fortran/trajectory (FORTRAN)

✅ FOUND 2 LEGACY BINARY(IES):

1. mortgage
   Path: ./legacy/cobol/mortgage
   Language: COBOL
   Source: ./legacy/cobol/mortgage.cbl

2. trajectory
   Path: ./legacy/fortran/trajectory
   Language: FORTRAN
   Source: ./legacy/fortran/trajectory.f

✓ Configuration saved to: necro-bridge.config.json
```

### `necro-bridge serve`

Start the bridge server.

**Options:**
- `-p, --port <number>` - Server port (default: `3001`)
- `-c, --config <path>` - Config file path (default: `./necro-bridge.config.json`)

**What it does:**
- Loads configuration file
- Validates that all binaries exist
- Generates minimal server if needed
- Starts Express server with bridge endpoints

**Example:**
```bash
necro-bridge serve
necro-bridge serve --port 8080
necro-bridge serve --config ./custom-config.json
```

**Output:**
```
═══════════════════════════════════════════════════════
👻 STARTING NECRO-BRIDGE SERVER
═══════════════════════════════════════════════════════
🔌 Port: 3001
📋 Config: ./necro-bridge.config.json

🔍 VALIDATING BINARIES...
   ✓ mortgage (COBOL)
   ✓ trajectory (FORTRAN)

🚀 LAUNCHING SERVER...

═══════════════════════════════════════════════════════
👻 NECRO-BRIDGE SERVER ONLINE
═══════════════════════════════════════════════════════
🔌 Listening on port 3001
📡 Available endpoints:
   POST /api/calculate/cobol (COBOL)
   POST /api/calculate/fortran (FORTRAN)
═══════════════════════════════════════════════════════
```

### `necro-bridge test <binary> [args...]`

Test a legacy binary integration.

**Arguments:**
- `<binary>` - Path to the binary to test
- `[args...]` - Command-line arguments to pass to the binary

**What it does:**
- Checks if binary exists and is executable
- Executes binary with provided arguments
- Captures and displays output
- Parses result if it matches expected format
- Reports execution time

**Example:**
```bash
necro-bridge test ./legacy/cobol/mortgage 200000 5.5 30
```

**Output:**
```
═══════════════════════════════════════════════════════
🧪 TESTING LEGACY BINARY
═══════════════════════════════════════════════════════
📦 Binary: ./legacy/cobol/mortgage
📝 Arguments: 200000 5.5 30

🚀 EXECUTING BINARY...

═══════════════════════════════════════════════════════
📤 OUTPUT:
═══════════════════════════════════════════════════════
RESULT:    1135.57

═══════════════════════════════════════════════════════

✅ RESULT DETECTED: 1135.57

⏱️  Execution time: 20ms

═══════════════════════════════════════════════════════
✅ TEST SUCCESSFUL
═══════════════════════════════════════════════════════
```

## Configuration File Format

The `necro-bridge.config.json` file defines your legacy binaries and server settings:

```json
{
  "binaries": [
    {
      "name": "mortgage",
      "path": "./legacy/cobol/mortgage",
      "language": "cobol",
      "endpoint": "/api/calculate/cobol",
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

**Fields:**
- `name` - Identifier for the binary
- `path` - Path to the compiled executable
- `language` - Language identifier (cobol, fortran, pascal, basic)
- `endpoint` - API endpoint path
- `parameters` - Array of parameter names
- `outputPattern` - Regex pattern to extract result from output
- `server.port` - Server port number
- `server.timeout` - Execution timeout in milliseconds

## Workflow

### 1. Initialize a New Bridge

```bash
necro-bridge init cobol
```

### 2. Customize Your Legacy Program

Edit the generated example file or replace it with your own:

```bash
# Edit the example
vim legacy/cobol/example.cbl

# Or copy your existing program
cp /path/to/your/program.cbl legacy/cobol/mortgage.cbl
```

### 3. Compile Your Program

```bash
cobc -x -o legacy/cobol/mortgage legacy/cobol/mortgage.cbl
```

### 4. Test the Binary

```bash
necro-bridge test ./legacy/cobol/mortgage 200000 5.5 30
```

### 5. Install Server Dependencies

```bash
cd server && npm install
```

### 6. Start the Server

```bash
necro-bridge serve
```

### 7. Test via API

```bash
curl -X POST http://localhost:3001/api/calculate/cobol \
  -H "Content-Type: application/json" \
  -d '{"principal": 200000, "rate": 5.5, "term": 30}'
```

## Troubleshooting

### "Binary not found"

- Ensure you've compiled your legacy program
- Check the path in your configuration file
- Verify the binary is executable: `chmod +x <binary>`

### "Compiler not found"

Install the required compiler:

- **COBOL**: `brew install gnu-cobol` (macOS) or `apt-get install gnucobol` (Linux)
- **FORTRAN**: `brew install gcc` (macOS) or `apt-get install gfortran` (Linux)
- **Pascal**: `brew install fpc` (macOS) or `apt-get install fp-compiler` (Linux)
- **BASIC**: Download from https://www.freebasic.net/

### "Configuration file not found"

Run `necro-bridge detect` or `necro-bridge init <language>` to generate a configuration file.

### "Server fails to start"

- Check if port is already in use
- Ensure all binaries in config exist
- Install server dependencies: `cd server && npm install`

## Advanced Usage

### Custom Output Patterns

Modify the `outputPattern` in your config file to match your program's output:

```json
{
  "outputPattern": "TOTAL:\\s*(\\d+\\.\\d+)"
}
```

### Multiple Binaries

Add multiple binaries to your configuration:

```json
{
  "binaries": [
    {
      "name": "mortgage",
      "path": "./legacy/cobol/mortgage",
      "language": "cobol",
      "endpoint": "/api/calculate/mortgage"
    },
    {
      "name": "interest",
      "path": "./legacy/basic/interest",
      "language": "basic",
      "endpoint": "/api/calculate/interest"
    }
  ]
}
```

### Custom Server Port

```bash
necro-bridge serve --port 8080
```

## Templates

The CLI includes templates for each supported language in the `templates/` directory:

- `cobol-bridge.js` - COBOL bridge template
- `fortran-bridge.js` - FORTRAN bridge template
- `pascal-bridge.js` - Pascal bridge template
- `basic-bridge.js` - BASIC bridge template

These templates are used by the `init` command and can be customized for your specific needs.

## Development

### Building from Source

```bash
cd toolkit
npm install
npm run build
```

### Testing the CLI

```bash
node dist/cli/index.js --help
```

### Making the CLI Globally Available

```bash
npm link
necro-bridge --help
```

## License

MIT License - see LICENSE file for details.
