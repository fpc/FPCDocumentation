Program Example59;

{ This program demonstrates the AnsiStrLower function }

Uses sysutils,strings;

Procedure Testit (S : Pchar);

begin
 Writeln (S,' -> ',AnsiStrLower(StrNew(S)))
end;

Begin
  Testit('AN UPPERCASE STRING');
  Testit('Some mixed STring');
  Testit('a lowercase string');
End.