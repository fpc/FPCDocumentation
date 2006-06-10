Program Example40;

{ Program to demonstrate the stddev function. }

Uses Math;

Var
  I : Integer;
  ExArray : Array[1..10000] of Float;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=Randg(1,0.2);
  Writeln('StdDev     : ',StdDev(ExArray):8:4);
  Writeln('StdDev (b) : ',StdDev(@ExArray[0],10000):8:4);
end.
