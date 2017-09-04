Program Example40;

{ Program to demonstrate the randg function. }

Uses Math;

Var
  I : Integer;
  ExArray : Array[1..10000] of Float;
  Mean,stddev : Float;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=Randg(1,0.2);
  MeanAndStdDev(ExArray,Mean,StdDev);
  Writeln('Mean       : ',Mean:8:4);
  Writeln('StdDev     : ',StdDev:8:4);
end.
