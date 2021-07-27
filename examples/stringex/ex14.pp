Program Example14;

Uses strings;

{ Program to demonstrate the StrLower and StrUpper functions. }

Const
    P1 : PChar = 'THIS IS AN UPPERCASE PCHAR STRING';
    P2 : PChar = 'this is a lowercase string';


begin
  P1:=StrNew(P1);
  P2:=strNew(P2);
  Writeln ('Uppercase : ',StrUpper(P2));
  StrLower(P1);
  Writeln ('Lowercase : ',P1);
  StrDispose(P1);
  StrDispose(P2);
end.
