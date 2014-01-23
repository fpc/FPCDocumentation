Program Example31;

{ Program to demonstrate the Hi function. }

var
  L : Longint;
  W : Word;
  B : Byte;
  
begin
  L:=1 Shl 16;     { = $10000 }
  W:=1 Shl 8;      { = $100 }
  B:=1 Shl 4;      { = $10  }
  Writeln (Hi(L)); { Prints 1 }
  Writeln (Hi(W)); { Prints 1 }
  Writeln (Hi(B)); { Prints 1 }
end.
