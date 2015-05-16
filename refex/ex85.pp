Program Example85;

{ Program to demonstrate the SetLength function. }

Var S : String;

begin
  Setlength(S,100);
  FillChar(S[1],100,#32);
  Writeln ('"',S,'"');
end.
