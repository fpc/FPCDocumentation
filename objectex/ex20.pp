Program ex20;

{ Program to demonstrate the TMemoryStream.Truncate method }

Uses Objects;

Var L : String;
    P : PString;
    S : PMemoryStream;
    I : Longint;

begin
  L:='Some constant string';
  { Buffer size of 100 }
  S:=New(PMemoryStream,Init(1000,100));
  Writeln ('Writing 100 times "',L,'" to stream.');
  For I:=1 to 100 do
    S^.WriteStr(@L);
  Writeln ('Finished.');
  S^.Seek(100);
  S^.Truncate;
  Writeln ('Truncated at byte 100.');
  Dispose (S,Done);
  Writeln ('Finished.');
end.