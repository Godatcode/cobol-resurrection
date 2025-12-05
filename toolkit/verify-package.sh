#!/bin/bash

# NECRO-BRIDGE PACKAGE VERIFICATION SCRIPT
# This script verifies the package is ready for NPM publication

echo "🏛️  NECRO-BRIDGE PACKAGE VERIFICATION"
echo "======================================"
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track overall status
ALL_PASSED=true

# Function to check status
check_status() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✓${NC} $2"
    else
        echo -e "${RED}✗${NC} $2"
        ALL_PASSED=false
    fi
}

# 1. Check if we're in the toolkit directory
echo "📁 Checking directory..."
if [ ! -f "package.json" ]; then
    echo -e "${RED}✗${NC} Not in toolkit directory. Please run from toolkit/"
    exit 1
fi
check_status 0 "In correct directory"
echo ""

# 2. Build the package
echo "🔨 Building package..."
npm run build > /dev/null 2>&1
check_status $? "TypeScript compilation"
echo ""

# 3. Check package.json fields
echo "📋 Verifying package.json..."
NAME=$(node -p "require('./package.json').name")
VERSION=$(node -p "require('./package.json').version")
LICENSE=$(node -p "require('./package.json').license")
MAIN=$(node -p "require('./package.json').main")
BIN=$(node -p "require('./package.json').bin['necro-bridge']")

check_status 0 "Name: $NAME"
check_status 0 "Version: $VERSION"
check_status 0 "License: $LICENSE"
check_status 0 "Main entry: $MAIN"
check_status 0 "Binary: $BIN"
echo ""

# 4. Check required files exist
echo "📄 Checking required files..."
[ -f "README.md" ] && check_status 0 "README.md exists" || check_status 1 "README.md missing"
[ -f "LICENSE" ] && check_status 0 "LICENSE exists" || check_status 1 "LICENSE missing"
[ -f "dist/index.js" ] && check_status 0 "dist/index.js exists" || check_status 1 "dist/index.js missing"
[ -f "dist/cli/index.js" ] && check_status 0 "dist/cli/index.js exists" || check_status 1 "dist/cli/index.js missing"
[ -d "templates" ] && check_status 0 "templates/ directory exists" || check_status 1 "templates/ missing"
echo ""

# 5. Create tarball
echo "📦 Creating tarball..."
npm pack > /dev/null 2>&1
check_status $? "Tarball created"

# Get tarball info
TARBALL=$(ls -1 necro-bridge-core-*.tgz 2>/dev/null | head -1)
if [ -n "$TARBALL" ]; then
    SIZE=$(ls -lh "$TARBALL" | awk '{print $5}')
    check_status 0 "Tarball size: $SIZE"
fi
echo ""

# 6. Test local installation
echo "🧪 Testing local installation..."
TEST_DIR="/tmp/necro-bridge-verify-$$"
mkdir -p "$TEST_DIR"
cd "$TEST_DIR"
npm init -y > /dev/null 2>&1
npm install "$OLDPWD/$TARBALL" > /dev/null 2>&1
INSTALL_STATUS=$?
cd "$OLDPWD"

check_status $INSTALL_STATUS "Local installation"

if [ $INSTALL_STATUS -eq 0 ]; then
    # Test CLI
    CLI_PATH="$TEST_DIR/node_modules/.bin/necro-bridge"
    if [ -x "$CLI_PATH" ]; then
        check_status 0 "CLI binary is executable"
        
        # Test help command
        "$CLI_PATH" --help > /dev/null 2>&1
        check_status $? "CLI --help command works"
    else
        check_status 1 "CLI binary not found or not executable"
    fi
fi

# Cleanup test directory
rm -rf "$TEST_DIR"
echo ""

# 7. Check for vulnerabilities
echo "🔒 Checking for vulnerabilities..."
npm audit --audit-level=moderate > /dev/null 2>&1
AUDIT_STATUS=$?
if [ $AUDIT_STATUS -eq 0 ]; then
    check_status 0 "No vulnerabilities found"
else
    echo -e "${YELLOW}⚠${NC}  Vulnerabilities detected (run 'npm audit' for details)"
fi
echo ""

# 8. Summary
echo "======================================"
if [ "$ALL_PASSED" = true ]; then
    echo -e "${GREEN}✓ ALL CHECKS PASSED${NC}"
    echo ""
    echo "📦 Package is ready for publication!"
    echo ""
    echo "To publish to NPM:"
    echo "  1. npm login"
    echo "  2. npm publish --access public"
    echo ""
    echo "Package details:"
    echo "  Name: $NAME"
    echo "  Version: $VERSION"
    echo "  Tarball: $TARBALL"
else
    echo -e "${RED}✗ SOME CHECKS FAILED${NC}"
    echo ""
    echo "Please fix the issues above before publishing."
    exit 1
fi
