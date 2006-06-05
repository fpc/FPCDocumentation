Program ex29;

{
 Program to demonstrate the TCollection.DeleteAll method
 Compare with example 28, where FreeAll is used.
}

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
  C^.DeleteAll;
  Writeln ('Deleted all objects.');
  Dispose(C,Done);
end.