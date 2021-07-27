Program Example50;

{ Program to demonstrate the Variance function. }
{ @ should return typed pointer }
{$T+}

Uses math;

Var
  I : 1..100;
  ExArray : Array[1..100] of Float;
  V : float;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=(Random-Random)*100;
  V:=Variance(ExArray);
  Writeln('Variance     : ',V:8:4);
  V:=Variance(@ExArray[1],100);
  Writeln('Variance (b) : ',V:8:4);
end.
