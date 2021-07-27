Program Example51;
{ This program demonstrates the AnsiQuotedStr function }
Uses sysutils;

Var 
  S : AnsiString;
  P : PChar;

Begin
 S:='He said "Hello" and walked on';
 P:=Pchar(S);
 S:=AnsiQuotedStr(P,'"');
 Writeln (S);
 P:=Pchar(S);
 Writeln(AnsiExtractQuotedStr(P,'"'));
End.

