Program Example113;

uses oha,ohb;

{ Program to demonstrate the class helper scope. }

Var
  o : TObject;

begin
  O:=TObject.Create;
//  Writeln(O.AsString('O as a string : %s'));
  Writeln(O.MemoryLocation);
end.
