program Example31;

{ Program to demonstrate the MinValue function. }

{ Make sure integer is 32 bit}
{$mode objfpc}

uses math;

var i:1..100;
    f_array:array[1..100] of Float;
    i_array:array[1..100] of Integer;
    Pf_array:Pfloat;
    PI_array:Pinteger;

begin
  randomize;

  Pf_array:=@f_array[1];
  Pi_array:=@i_array[1];

  for i:=low(f_array) to high(f_array) do
    f_array[i]:=(random-random)*100;
  for i:=low(i_array) to high(i_array) do
    i_array[i]:=random(I)-random(100);

  Writeln('Min Float       : ',MinValue(f_array):8:4);
  Writeln('Min Float   (b) : ',MinValue(Pf_array,100):8:4);
  Writeln('Min Integer     : ',MinValue(i_array):8);
  Writeln('Min Integer (b) : ',MinValue(Pi_array,100):8);
end.
