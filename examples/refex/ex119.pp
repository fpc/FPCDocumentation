Program ex119;

{ Program to demonstrate the dynamic array constructor }

Type
  TIntegerArray = Array of Integer;
  
var
  A : TIntegerArray;
  
begin
  A:=TIntegerArray.Create(1,2,3);
  Writeln('Length ',length(A));
end.  