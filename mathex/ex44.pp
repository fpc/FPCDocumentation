Program Example44;

{ Program to demonstrate the Sum function. }

Uses math;

Var
  I : 1..100;
  ExArray : Array[1..100] of Float;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=(Random-Random)*100;
  Writeln('Max     : ',MaxValue(ExArray):8:4);
  Writeln('Min     : ',MinValue(ExArray):8:4);
  Writeln('Sum     : ',Sum(ExArray):8:4);
  Writeln('Sum (b) : ',Sum(@ExArray[1],100):8:4);
end.
