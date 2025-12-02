#!/bin/bash
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

echo "🚀 COMPILING FORTRAN-57 TRAJECTORY CALCULATOR..."

# Check if gfortran is installed
if ! command -v gfortran &> /dev/null; then
    echo "ERROR: gfortran compiler not found"
    echo "Please install gfortran:"
    echo "  macOS:   brew install gcc"
    echo "  Ubuntu:  sudo apt-get install gfortran"
    exit 1
fi

# Compile with fixed-form source format
gfortran -std=legacy -o legacy/fortran/trajectory legacy/fortran/trajectory.f

if [ $? -eq 0 ]; then
    echo "✅ COMPILATION SUCCESSFUL"
    echo "Binary created: legacy/fortran/trajectory"
    echo ""
    echo "Test execution:"
    echo "  ./legacy/fortran/trajectory 100 45 9.8"
    echo "  Expected output: RESULT: 1020.41"
else
    echo "❌ COMPILATION FAILED"
    exit 1
fi
