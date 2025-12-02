# COBOL Code Generation Template (1959 Era)

## Syntax Rules
- Use GnuCOBOL dialect
- IDENTIFICATION DIVISION must come first
- DATA DIVISION for variable declarations
- PROCEDURE DIVISION for logic
- Use COMPUTATIONAL-2 for floating-point precision
- Column 1-6: Sequence numbers (optional)
- Column 7: Indicator area (* for comments, - for continuation)
- Column 8-11: Area A (division/section headers)
- Column 12-72: Area B (statements)
- All statements end with period

## Standard Structure
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM-NAME.
       AUTHOR. NECRO-BRIDGE GENERATOR.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INPUT-DATA.
          05 WS-VARIABLE-NAME    PIC 9(8)V99.
       
       01 WS-OUTPUT-DATA.
          05 WS-RESULT           COMP-2.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           ACCEPT WS-VARIABLE-NAME FROM COMMAND-LINE
           PERFORM CALCULATION
           DISPLAY "RESULT: " WS-RESULT
           STOP RUN.
       
       CALCULATION.
           COMPUTE WS-RESULT = [FORMULA]
           .
```

## Example: Mortgage Calculator
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORTGAGE.
       AUTHOR. NECRO-BRIDGE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INPUT-DATA.
          05 WS-PRINCIPAL        PIC 9(8)V99.
          05 WS-ANNUAL-RATE      PIC 99V99.
          05 WS-TERM-YEARS       PIC 99.
       
       01 WS-CALCULATED-DATA.
          05 WS-MONTHLY-RATE     COMP-2.
          05 WS-NUM-PAYMENTS     PIC 999.
          05 WS-MONTHLY-PAYMENT  COMP-2.
          05 WS-POWER-TERM       COMP-2.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           ACCEPT WS-PRINCIPAL FROM COMMAND-LINE
           ACCEPT WS-ANNUAL-RATE FROM COMMAND-LINE
           ACCEPT WS-TERM-YEARS FROM COMMAND-LINE
           
           PERFORM CALCULATE-MORTGAGE
           
           DISPLAY "RESULT: " WS-MONTHLY-PAYMENT
           STOP RUN.
       
       CALCULATE-MORTGAGE.
           COMPUTE WS-MONTHLY-RATE = WS-ANNUAL-RATE / 100 / 12
           COMPUTE WS-NUM-PAYMENTS = WS-TERM-YEARS * 12
           COMPUTE WS-POWER-TERM = (1 + WS-MONTHLY-RATE) ** WS-NUM-PAYMENTS
           COMPUTE WS-MONTHLY-PAYMENT = WS-PRINCIPAL * 
                   (WS-MONTHLY-RATE * WS-POWER-TERM) / 
                   (WS-POWER-TERM - 1)
           .
```

## Error Handling Pattern
```cobol
       IF WS-PRINCIPAL <= 0 THEN
           DISPLAY "ERROR: INVALID PRINCIPAL AMOUNT"
           MOVE 1 TO RETURN-CODE
           STOP RUN
       END-IF
```

## Output Format
- Always output in format: "RESULT: XXXX.XX"
- Use DISPLAY statement
- Ensure exactly 2 decimal places for currency

## Compilation Command
```bash
cobc -x -o legacy/cobol/{filename} legacy/cobol/{filename}.cbl
```
