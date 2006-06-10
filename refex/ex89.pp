Program Example89;

{ Program to demonstrate the FreeMem function. }

Var P : Pointer;

begin
  GetMem(P,10000);
  FreeMem(P);
end.
