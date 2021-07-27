Program ex120;

{ Program to demonstrate the dynamic array constructor for multiple dimensions }

Type
  TIntegerArray = Array of Integer;
  TIntegerArrayArray = Array of TIntegerArray;
  
var
  A : TIntegerArrayArray;
  
begin
  A:=TIntegerArrayArray.Create(TIntegerArray.Create(1,2,3),
                               TIntegerArray.Create(4,5,6),
                               TIntegerArray.Create(7,8,9));
  Writeln('Length ',length(A));
end.  