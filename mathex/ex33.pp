program Example33;

{ Program to demonstrate the norm function. }

uses math;

var v:array[1..10] of Float;
    I:1..10;

begin
  for I:=low(v) to high(v) do
    v[i]:=random;
  writeln(norm(v));
end.
