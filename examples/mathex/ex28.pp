Program Example28;

{ Program to demonstrate the Meanandstddev function. }
{ @ should return typed pointer }
{$T+}

Uses math;

Type
  TExArray = Array[1..100] of Extended;

Var
  I : Integer;
  ExArray : TExArray;
  Mean,stddev : Extended;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=(Random-Random)*100;
  MeanAndStdDev(ExArray,Mean,StdDev);
  Writeln('Mean       : ',Mean:8:4);
  Writeln('StdDev     : ',StdDev:8:4);
  MeanAndStdDev(@ExArray[1],100,Mean,StdDev);
  Writeln('Mean   (b) : ',Mean:8:4);
  Writeln('StdDev (b) : ',StdDev:8:4);
end.
