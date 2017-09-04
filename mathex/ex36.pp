Program Example36;

{ Program to demonstrate the PopnVariance function. }
{ @ should return typed pointer }
{$T+}

Uses math;

Var
  I : Integer;
  ExArray : Array[1..100] of Float;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=(Random-Random)*100;
  Writeln('Max           : ',MaxValue(ExArray):8:4);
  Writeln('Min           : ',MinValue(ExArray):8:4);
  Writeln('Pop. var.     : ',PopnVariance(ExArray):8:4);
  Writeln('Pop. var. (b) : ',PopnVariance(@ExArray[1],100):8:4);
end.
