{ ******************************************************************** }
{ * PASCAL TAX CALCULATOR - PROGRESSIVE BRACKET COMPUTATION         * }
{ * AUTHENTIC 1970 STRUCTURED PROGRAMMING SYNTAX                    * }
{ * DEMONSTRATES PROPER BEGIN/END BLOCKS AND PASCAL CONVENTIONS     * }
{ ******************************************************************** }
{                                                                      }
{ PROGRAM:  TAX                                                        }
{ AUTHOR:   IBM-7090-MAINFRAME                                         }
{ DATE:     1970-RESURRECTED-2024                                      }
{                                                                      }
{ PURPOSE:  CALCULATES PROGRESSIVE TAX BASED ON INCOME AND BRACKETS   }
{                                                                      }
{ ******************************************************************** }

PROGRAM TaxCalculator;

VAR
    Income: Real;
    BracketRate: Real;
    BracketThreshold: Real;
    TotalTax: Real;
    ErrorCode: Integer;
    InputStr: String;

{ ******************************************************************** }
{ * PROCEDURE: ValidateInput                                         * }
{ * PURPOSE:   Validates that all input parameters are positive      * }
{ ******************************************************************** }
PROCEDURE ValidateInput(Inc: Real; Rate: Real; Thresh: Real);
BEGIN
    IF Inc < 0.0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: INVALID INCOME AMOUNT');
        Halt(1);
    END;
    
    IF Rate < 0.0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: INVALID TAX RATE');
        Halt(1);
    END;
    
    IF Rate > 100.0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: TAX RATE EXCEEDS 100 PERCENT');
        Halt(1);
    END;
    
    IF Thresh < 0.0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: INVALID BRACKET THRESHOLD');
        Halt(1);
    END;
END;

{ ******************************************************************** }
{ * FUNCTION: CalculateProgressiveTax                                * }
{ * PURPOSE:  Computes tax using progressive bracket system          * }
{ * FORMULA:  Tax = (Income - Threshold) * Rate / 100                * }
{ *           Only income above threshold is taxed at bracket rate   * }
{ ******************************************************************** }
FUNCTION CalculateProgressiveTax(Inc: Real; Rate: Real; Thresh: Real): Real;
VAR
    TaxableAmount: Real;
    ComputedTax: Real;
BEGIN
    { DETERMINE TAXABLE INCOME ABOVE THRESHOLD }
    IF Inc <= Thresh THEN
    BEGIN
        { INCOME BELOW THRESHOLD - NO TAX IN THIS BRACKET }
        ComputedTax := 0.0;
    END
    ELSE
    BEGIN
        { CALCULATE TAX ON INCOME ABOVE THRESHOLD }
        TaxableAmount := Inc - Thresh;
        ComputedTax := TaxableAmount * Rate / 100.0;
    END;
    
    CalculateProgressiveTax := ComputedTax;
END;

{ ******************************************************************** }
{ * MAIN PROGRAM BLOCK                                               * }
{ ******************************************************************** }
BEGIN
    { VALIDATE COMMAND LINE ARGUMENT COUNT }
    IF ParamCount < 3 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: INSUFFICIENT ARGUMENTS');
        Halt(1);
    END;
    
    { PARSE COMMAND LINE ARGUMENTS WITH ERROR HANDLING }
    ErrorCode := 0;
    
    { ATTEMPT TO CONVERT INCOME PARAMETER }
    InputStr := ParamStr(1);
    Val(InputStr, Income, ErrorCode);
    IF ErrorCode <> 0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: NON-NUMERIC INCOME INPUT');
        Halt(1);
    END;
    
    { ATTEMPT TO CONVERT BRACKET RATE PARAMETER }
    InputStr := ParamStr(2);
    Val(InputStr, BracketRate, ErrorCode);
    IF ErrorCode <> 0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: NON-NUMERIC RATE INPUT');
        Halt(1);
    END;
    
    { ATTEMPT TO CONVERT BRACKET THRESHOLD PARAMETER }
    InputStr := ParamStr(3);
    Val(InputStr, BracketThreshold, ErrorCode);
    IF ErrorCode <> 0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: NON-NUMERIC THRESHOLD INPUT');
        Halt(1);
    END;
    
    { VALIDATE INPUT PARAMETERS }
    ValidateInput(Income, BracketRate, BracketThreshold);
    
    { PERFORM TAX CALCULATION }
    TotalTax := CalculateProgressiveTax(Income, BracketRate, BracketThreshold);
    
    { OUTPUT RESULT IN UNIVERSAL FORMAT }
    WriteLn('RESULT: ', TotalTax:0:2);
    
    { TERMINATE WITH SUCCESS CODE }
    Halt(0);
END.
