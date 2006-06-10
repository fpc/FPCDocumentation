program Example73;

{ Program to demonstrate the Lowercase function. }

var c:char;

begin
  for c:='A' to 'Z' do
    write(lowercase(c));
  Writeln;
  Writeln(Lowercase('ABCDEFGHIJKLMNOPQRSTUVWXYZ'));
end.
