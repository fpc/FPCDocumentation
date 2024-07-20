program ex124;

{
  Example to show various error conditions for the Trunc() function.
  SysUtils is used to convert the runtime error to an exception that can be caught
  Math is used to have access to Nan and (Neg)Infinity
 
}

{$mode objfpc}
{$h+}
uses

  sysutils, math;
var
  D: Double;
  Q: QWord;
  L: Int64;
begin
  D := 2.0 * High(QWord);
  try
    Q := Trunc(D);
  except
    on E: Exception do writeln(E.ClassName,': ',E.Message);  //EInvalidOp: Invalid floating point operation
  end;
  D := NaN;
  try
    Q := Trunc(D);
  except
    on E: Exception do writeln(E.ClassName,': ',E.Message);  //EInvalidOp: Invalid floating point operation
  end;
  D := Infinity;
  try
    Q := Trunc(D);
  except
    on E: Exception do writeln(E.ClassName,': ',E.Message);  //EInvalidOp: Invalid floating point operation
  end;
  D := NegInfinity;
  try
    Q := Trunc(D);
  except
    on E: Exception do writeln(E.ClassName,': ',E.Message);  //EInvalidOp: Invalid floating point operation
  end;
  D := -2.0 * High(QWord);
  try
    L := Trunc(D);
  except
    on E: Exception do writeln(E.ClassName,': ',E.Message);  //EInvalidOp: Invalid floating point operation
  end;
end.
