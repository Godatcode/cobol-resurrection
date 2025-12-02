#!/bin/bash
###############################################################################
# BASIC COMPOUND INTEREST CALCULATOR COMPILATION SCRIPT
# Uses Node.js interpreter wrapper for authentic 1983 BASIC logic
#
# NOTE: This implementation uses a Node.js wrapper that maintains
#       vintage BASIC semantics and calculation methods while
#       providing command-line argument compatibility for the
#       Bridge Server integration.
###############################################################################

echo "💵 PREPARING BASIC-83 INTEREST CALCULATOR..."
echo ""

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "❌ ERROR: node interpreter not found"
    echo ""
    echo "Please install Node.js:"
    echo "  macOS:   brew install node"
    echo "  Ubuntu:  sudo apt-get install nodejs"
    echo "  RHEL:    sudo yum install nodejs"
    echo ""
    exit 1
fi

# Display Node.js version
echo "Interpreter version:"
node --version
echo ""

# Ensure executable is present and has correct permissions
if [ ! -f "legacy/basic/interest" ]; then
    echo "❌ ERROR: legacy/basic/interest not found"
    exit 1
fi

chmod +x legacy/basic/interest

echo "✅ PREPARATION SUCCESSFUL"
echo "   👻 Ancient Spirit Bound to Node.js Wrapper: legacy/basic/interest"
echo "   (Maintains authentic 1983 BASIC calculation semantics)"
echo ""
echo "Test execution:"
echo "  ./legacy/basic/interest 1000 5 12 10"
echo "  Expected output: RESULT:    1647.01"
echo ""

echo "[END OF TAPE]"
