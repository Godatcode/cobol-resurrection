PROGRAM Subtract;

VAR
    Num1: Real;
    Num2: Real;
    Result: Real;
    ErrorCode: Integer;
    InputStr: String;

BEGIN
    IF ParamCount < 2 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: INSUFFICIENT ARGUMENTS');
        Halt(1);
    END;
    
    InputStr := ParamStr(1);
    Val(InputStr, Num1, ErrorCode);
    IF ErrorCode <> 0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: NON-NUMERIC INPUT');
        Halt(1);
    END;
    
    InputStr := ParamStr(2);
    Val(InputStr, Num2, ErrorCode);
    IF ErrorCode <> 0 THEN
    BEGIN
        WriteLn(StdErr, 'ERROR: NON-NUMERIC INPUT');
        Halt(1);
    END;
    
    Result := Num1 - Num2;
    
    WriteLn('RESULT: ', Result:0:2);
    
    Halt(0);
END.
