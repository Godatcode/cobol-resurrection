#!/bin/bash
###############################################################################
# FORTRAN TRAJECTORY CALCULATOR COMPILATION SCRIPT
# Requires: gfortran (GNU Fortran Compiler)
#
# Installation on macOS:
#   brew install gcc
#
# Installation on Ubuntu/Debian:
#   sudo apt-get install gfortran
#
# Installation on RHEL/CentOS:
#   sudo yum install gcc-gfortran
###############################################################################

echo "🚀 COMPILING FORTRAN-57 TRAJECTORY CALCULATOR..."
echo ""

# Check if gfortran is installed
if ! command -v gfortran &> /dev/null; then
    echo "❌ ERROR: gfortran compiler not found"
    echo ""
    echo "Please install GNU Fortran:"
    echo "  macOS:   brew install gcc"
    echo "  Ubuntu:  sudo apt-get install gfortran"
    echo "  RHEL:    sudo yum install gcc-gfortran"
    echo ""
    exit 1
fi

# Display compiler version
echo "Compiler version:"
gfortran --version | head -1
echo ""

# Compile with fixed-form source format and legacy standard
echo "Compiling legacy/fortran/trajectory.f..."
gfortran -std=legacy -o legacy/fortran/trajectory legacy/fortran/trajectory.f

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ COMPILATION SUCCESSFUL"
    echo "   👻 Ancient Spirit Bound to Binary: legacy/fortran/trajectory"
    echo ""
    echo "Test execution:"
    echo "  ./legacy/fortran/trajectory 100 45 9.8"
    echo "  Expected output: RESULT:    1020.41"
    echo ""
else
    echo ""
    echo "❌ COMPILATION FAILED"
    echo "   💀 Spirit Refused to Materialize"
    echo ""
    exit 1
fi

echo "[END OF TAPE]"
