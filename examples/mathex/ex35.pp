Program Example35;

{ Program to demonstrate the PopnStdDev function. }
{ @ should return typed pointer }
{$T+}

Uses Math;

Type
  TExArray = Array[1..100] of Float;

Var
  I : Integer;
  ExArray : TExArray;

begin
  Randomize;
  for I:=low(ExArray) to high(ExArray) do
    ExArray[i]:=(Random-Random)*100;
  Writeln('Max              : ',MaxValue(ExArray):8:4);
  Writeln('Min              : ',MinValue(ExArray):8:4);
  Writeln('Pop. stddev.     : ',PopnStdDev(ExArray):8:4);
  Writeln('Pop. stddev. (b) : ',PopnStdDev(@ExArray[1],100):8:4);
end.
