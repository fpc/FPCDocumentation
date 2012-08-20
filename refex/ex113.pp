Program Example113;

{ Program to demonstrate the Slice function. }

procedure ShowArray(const A: array of Integer);
var
  I: Integer;
begin
  for I := Low(A) to High(A) do
    WriteLn(I, ' : ', A[I]);
end;

begin
  ShowArray(Slice([1,2,3,4],2));
end.
