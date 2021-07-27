Program ex30;

{ Program to demonstrate the TCollection.Free method }

Uses Objects,MyObject; { For TMyObject definition and registration }

Var C : PCollection;
    M : PMyObject;
    I : Longint;

begin
  Randomize;
  C:=New(PCollection,Init(120,10));
  For I:=1 to 100 do
    begin
    M:=New(PMyObject,Init);
    M^.SetField(I-1);
    C^.Insert(M);
    end;
  Writeln ('Added 100 Items.');
  With C^ do
    While Count>0 do Free(At(Count-1));
  Writeln ('Freed all objects.');
  Dispose(C,Done);
end.