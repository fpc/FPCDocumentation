program Example26;

{ Program to demonstrate the MaxValue function. }

{ Make sore integer is 32 bit}
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

  Writeln('Max Float       : ',MaxValue(f_array):8:4);
  Writeln('Max Float   (b) : ',MaxValue(Pf_array,100):8:4);
  Writeln('Max Integer     : ',MaxValue(i_array):8);
  Writeln('Max Integer (b) : ',MaxValue(Pi_array,100):8);
end.
