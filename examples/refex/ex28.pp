Program Example28;

{ Program to demonstrate the FreeMem and GetMem functions. }

Var P : Pointer;
    MM : Longint;

begin
  { Get memory for P }
  GetMem (P,80);
  FillChar (P^,80,' ');
  FreeMem (P,80);
end.
