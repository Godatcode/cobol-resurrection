#!/bin/bash
###############################################################################
# COBOL MORTGAGE CALCULATOR COMPILATION SCRIPT
# Requires: GnuCOBOL (cobc compiler)
#
# Installation on macOS:
#   brew install gnu-cobol
#
# Installation on Ubuntu/Debian:
#   sudo apt-get install gnucobol
#
# Installation on RHEL/CentOS:
#   sudo yum install gnucobol
###############################################################################

echo "🏦 COMPILING COBOL-59 MORTGAGE CALCULATOR..."
echo ""

# Check if cobc is installed
if ! command -v cobc &> /dev/null; then
    echo "❌ ERROR: cobc compiler not found"
    echo ""
    echo "Please install GnuCOBOL:"
    echo "  macOS:   brew install gnu-cobol"
    echo "  Ubuntu:  sudo apt-get install gnucobol"
    echo "  RHEL:    sudo yum install gnucobol"
    echo ""
    exit 1
fi

# Display compiler version
echo "Compiler version:"
cobc --version | head -1
echo ""

# Compile with executable flag
echo "Compiling legacy/cobol/mortgage.cbl..."
cobc -x -o legacy/cobol/mortgage legacy/cobol/mortgage.cbl

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ COMPILATION SUCCESSFUL"
    echo "   👻 Ancient Spirit Bound to Binary: legacy/cobol/mortgage"
    echo ""
    echo "Test execution:"
    echo "  ./legacy/cobol/mortgage 200000 5.5 30"
    echo "  Expected output: RESULT:    1135.57"
    echo ""
else
    echo ""
    echo "❌ COMPILATION FAILED"
    echo "   💀 Spirit Refused to Materialize"
    echo ""
    exit 1
fi

echo "[END OF TAPE]"
