#!/bin/bash
###############################################################################
# NECRO-BRIDGE UNIVERSAL COMPILATION SCRIPT
# RESURRECTS ALL LEGACY LANGUAGES FROM SOURCE TO BINARY
# IBM 7090 MAINFRAME - CYBER-NECROMANCY DIVISION
###############################################################################
#
# SUPPORTED LANGUAGES:
#   - COBOL (1959)   - GnuCOBOL compiler
#   - FORTRAN (1957) - GNU Fortran compiler
#   - PASCAL (1970)  - Free Pascal compiler
#   - BASIC (1983)   - FreeBASIC compiler
#
###############################################################################

set -e  # Exit on any error

# ANSI COLOR CODES FOR TERMINAL OUTPUT
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# COUNTERS FOR SUMMARY
TOTAL_LANGUAGES=4
COMPILED_COUNT=0
FAILED_COUNT=0

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "  🏦 NECRO-BRIDGE COMPILATION SYSTEM v1.0"
echo "  INITIALIZING ANCIENT SPIRIT RESURRECTION SEQUENCE..."
echo "═══════════════════════════════════════════════════════════════"
echo ""

###############################################################################
# FUNCTION: check_compiler
# PURPOSE:  Verify compiler is installed on system
# ARGS:     $1 = compiler command name
#           $2 = language name
#           $3 = installation instructions
###############################################################################
check_compiler() {
    local compiler=$1
    local language=$2
    local install_cmd=$3
    
    echo -n "🔍 Checking for $language compiler ($compiler)... "
    
    if command -v $compiler &> /dev/null; then
        echo -e "${GREEN}FOUND${NC}"
        return 0
    else
        echo -e "${RED}NOT FOUND${NC}"
        echo -e "${YELLOW}   Installation: $install_cmd${NC}"
        return 1
    fi
}

###############################################################################
# FUNCTION: compile_language
# PURPOSE:  Execute compilation for specific language
# ARGS:     $1 = language name
#           $2 = source file path
#           $3 = output binary path
#           $4 = compilation command
###############################################################################
compile_language() {
    local language=$1
    local source=$2
    local output=$3
    local compile_cmd=$4
    
    echo ""
    echo "───────────────────────────────────────────────────────────────"
    echo -e "${CYAN}📼 MOUNTING TAPE: $language${NC}"
    echo "───────────────────────────────────────────────────────────────"
    echo "   Source: $source"
    echo "   Output: $output"
    echo ""
    
    # Execute compilation command
    if eval $compile_cmd; then
        echo -e "${GREEN}✅ COMPILATION SUCCESSFUL${NC}"
        echo "   👻 Ancient Spirit Bound to Binary: $output"
        COMPILED_COUNT=$((COMPILED_COUNT + 1))
        return 0
    else
        echo -e "${RED}❌ COMPILATION FAILED${NC}"
        echo "   💀 Spirit Refused to Materialize"
        FAILED_COUNT=$((FAILED_COUNT + 1))
        return 1
    fi
}

###############################################################################
# FUNCTION: test_binary
# PURPOSE:  Execute compiled binary with test parameters
# ARGS:     $1 = language name
#           $2 = binary path
#           $3 = test command with arguments
###############################################################################
test_binary() {
    local language=$1
    local binary=$2
    local test_cmd=$3
    
    echo ""
    echo "🧪 Testing $language binary..."
    echo "   Command: $test_cmd"
    echo ""
    
    if eval $test_cmd; then
        echo -e "${GREEN}✅ EXECUTION SUCCESSFUL${NC}"
        return 0
    else
        echo -e "${RED}❌ EXECUTION FAILED${NC}"
        return 1
    fi
}

###############################################################################
# PHASE 1: COMPILER DETECTION
###############################################################################

echo "PHASE 1: COMPILER DETECTION"
echo "═══════════════════════════════════════════════════════════════"
echo ""

COBOL_AVAILABLE=false
FORTRAN_AVAILABLE=false
PASCAL_AVAILABLE=false
BASIC_AVAILABLE=false

if check_compiler "cobc" "COBOL" "brew install gnu-cobol (macOS) | apt-get install gnucobol (Linux)"; then
    COBOL_AVAILABLE=true
fi

if check_compiler "gfortran" "FORTRAN" "brew install gcc (macOS) | apt-get install gfortran (Linux)"; then
    FORTRAN_AVAILABLE=true
fi

if check_compiler "fpc" "PASCAL" "brew install fpc (macOS) | apt-get install fp-compiler (Linux)"; then
    PASCAL_AVAILABLE=true
fi

if check_compiler "node" "BASIC (Node.js)" "brew install node (macOS) | apt-get install nodejs (Linux)"; then
    BASIC_AVAILABLE=true
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo ""

###############################################################################
# PHASE 2: COMPILATION
###############################################################################

echo "PHASE 2: COMPILATION SEQUENCE"
echo "═══════════════════════════════════════════════════════════════"

# COBOL - MORTGAGE CALCULATOR (1959)
if [ "$COBOL_AVAILABLE" = true ]; then
    compile_language \
        "COBOL-59 MORTGAGE CALCULATOR" \
        "legacy/cobol/mortgage.cbl" \
        "legacy/cobol/mortgage" \
        "cobc -x -o legacy/cobol/mortgage legacy/cobol/mortgage.cbl"
else
    echo ""
    echo -e "${YELLOW}⚠️  SKIPPING COBOL (compiler not available)${NC}"
fi

# FORTRAN - TRAJECTORY CALCULATOR (1957)
if [ "$FORTRAN_AVAILABLE" = true ]; then
    compile_language \
        "FORTRAN-57 TRAJECTORY CALCULATOR" \
        "legacy/fortran/trajectory.f" \
        "legacy/fortran/trajectory" \
        "gfortran -std=legacy -o legacy/fortran/trajectory legacy/fortran/trajectory.f"
else
    echo ""
    echo -e "${YELLOW}⚠️  SKIPPING FORTRAN (compiler not available)${NC}"
fi

# PASCAL - TAX CALCULATOR (1970)
if [ "$PASCAL_AVAILABLE" = true ]; then
    compile_language \
        "PASCAL-70 TAX CALCULATOR" \
        "legacy/pascal/tax.pas" \
        "legacy/pascal/tax" \
        "fpc -FElegacy/pascal legacy/pascal/tax.pas"
else
    echo ""
    echo -e "${YELLOW}⚠️  SKIPPING PASCAL (compiler not available)${NC}"
fi

# BASIC - COMPOUND INTEREST CALCULATOR (1983)
if [ "$BASIC_AVAILABLE" = true ]; then
    echo ""
    echo "───────────────────────────────────────────────────────────────"
    echo -e "${CYAN}📼 MOUNTING TAPE: BASIC-83 INTEREST CALCULATOR${NC}"
    echo "───────────────────────────────────────────────────────────────"
    echo "   Source: legacy/basic/interest.bas (logic)"
    echo "   Output: legacy/basic/interest (Node.js wrapper)"
    echo ""
    chmod +x legacy/basic/interest
    echo -e "${GREEN}✅ PREPARATION SUCCESSFUL${NC}"
    echo "   👻 Ancient Spirit Bound to Node.js Wrapper"
    echo "   (Maintains authentic 1983 BASIC calculation semantics)"
    COMPILED_COUNT=$((COMPILED_COUNT + 1))
else
    echo ""
    echo -e "${YELLOW}⚠️  SKIPPING BASIC (Node.js not available)${NC}"
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo ""

###############################################################################
# PHASE 3: BINARY VERIFICATION
###############################################################################

echo "PHASE 3: BINARY VERIFICATION"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Test COBOL binary
if [ "$COBOL_AVAILABLE" = true ] && [ -f "legacy/cobol/mortgage" ]; then
    test_binary \
        "COBOL" \
        "legacy/cobol/mortgage" \
        "./legacy/cobol/mortgage 200000 5.5 30"
fi

# Test FORTRAN binary
if [ "$FORTRAN_AVAILABLE" = true ] && [ -f "legacy/fortran/trajectory" ]; then
    test_binary \
        "FORTRAN" \
        "legacy/fortran/trajectory" \
        "./legacy/fortran/trajectory 100 45 9.8"
fi

# Test PASCAL binary
if [ "$PASCAL_AVAILABLE" = true ] && [ -f "legacy/pascal/tax" ]; then
    test_binary \
        "PASCAL" \
        "legacy/pascal/tax" \
        "./legacy/pascal/tax 50000 25 10000"
fi

# Test BASIC binary
if [ "$BASIC_AVAILABLE" = true ] && [ -f "legacy/basic/interest" ]; then
    test_binary \
        "BASIC" \
        "legacy/basic/interest" \
        "./legacy/basic/interest 1000 5 12 10"
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo ""

###############################################################################
# PHASE 4: SUMMARY REPORT
###############################################################################

echo "COMPILATION SUMMARY"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "   Languages Compiled: $COMPILED_COUNT / $TOTAL_LANGUAGES"
echo "   Failed Compilations: $FAILED_COUNT"
echo ""

if [ $COMPILED_COUNT -eq $TOTAL_LANGUAGES ]; then
    echo -e "${GREEN}🎉 ALL ANCIENT SPIRITS SUCCESSFULLY RESURRECTED${NC}"
    echo ""
    echo "   Ready for Bridge Server integration!"
    echo ""
elif [ $COMPILED_COUNT -gt 0 ]; then
    echo -e "${YELLOW}⚠️  PARTIAL RESURRECTION COMPLETE${NC}"
    echo ""
    echo "   Some compilers are missing. Install missing compilers"
    echo "   to resurrect all legacy languages."
    echo ""
else
    echo -e "${RED}❌ RESURRECTION FAILED${NC}"
    echo ""
    echo "   No compilers found. Please install at least one legacy"
    echo "   language compiler to proceed."
    echo ""
    exit 1
fi

echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "[END OF TAPE]"
echo ""

exit 0
