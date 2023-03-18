program ex123;

{
  Example to show that Initialize can be used on a variable that was already used, 
  and that only fields of managed type are affected.
}

type
  TTest = object
    I: Integer;
    S: AnsiString;
  end;

procedure Test;

var
  V: TTest;

begin
  V.I := 10;
  V.S := 'x';
  WriteLn(V.I);
  WriteLn(V.S);
  Initialize(V);
  WriteLn(V.I); // unchanged
  WriteLn(V.S); // empty
end;

begin
  Test;
end.        
