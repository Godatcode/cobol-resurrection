#!/bin/bash
###############################################################################
# PASCAL TAX CALCULATOR COMPILATION SCRIPT
# Requires: Free Pascal Compiler (fpc)
#
# Installation on macOS:
#   brew install fpc
#
# Installation on Ubuntu/Debian:
#   sudo apt-get install fp-compiler
#
# Installation on RHEL/CentOS:
#   sudo yum install fpc
###############################################################################

echo "💰 COMPILING PASCAL-70 TAX CALCULATOR..."
echo ""

# Check if fpc is installed
if ! command -v fpc &> /dev/null; then
    echo "❌ ERROR: fpc compiler not found"
    echo ""
    echo "Please install Free Pascal Compiler:"
    echo "  macOS:   brew install fpc"
    echo "  Ubuntu:  sudo apt-get install fp-compiler"
    echo "  RHEL:    sudo yum install fpc"
    echo ""
    exit 1
fi

# Display compiler version
echo "Compiler version:"
fpc -iV
echo ""

# Compile with output directory flag
echo "Compiling legacy/pascal/tax.pas..."
fpc -FElegacy/pascal legacy/pascal/tax.pas

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ COMPILATION SUCCESSFUL"
    echo "   👻 Ancient Spirit Bound to Binary: legacy/pascal/tax"
    echo ""
    echo "Test execution:"
    echo "  ./legacy/pascal/tax 50000 25 10000"
    echo "  Expected output: RESULT: 10000.00"
    echo ""
else
    echo ""
    echo "❌ COMPILATION FAILED"
    echo "   💀 Spirit Refused to Materialize"
    echo ""
    exit 1
fi

echo "[END OF TAPE]"
