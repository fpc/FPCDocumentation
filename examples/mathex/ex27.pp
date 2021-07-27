Program Example27;

{ Program to demonstrate the Mean function. }
{ @ should return typed pointer }
{$T+}
Uses math;

Type
  TExArray = Array[1..100] of Float;

Var
  I : Integer;
  ExArray : TExArray;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=(Random-Random)*100;
  Writeln('Max      : ',MaxValue(ExArray):8:4);
  Writeln('Min      : ',MinValue(ExArray):8:4);
  Writeln('Mean     : ',Mean(ExArray):8:4);
  Writeln('Mean (b) : ',Mean(PExtended(@ExArray[1]),100):8:4);
end.
