Program ex40;

{ Program to demonstrate the NewStr function }

Uses Objects;

Var S : String;
    P : PString;

begin
  S:='Some really cute string';
  P:=NewStr(S);
  If P^<>S then
    Writeln ('Oh-oh... Something is wrong !!');
  DisposeStr(P);
end.