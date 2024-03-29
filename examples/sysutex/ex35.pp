Program Example35;

{ This program demonstrates the ExtractRelativePath function }

Uses sysutils;

Procedure Testit (FromDir,ToDir : String);

begin
  Write ('From "',FromDir,'" to "',ToDir,'" via "');
  Writeln (ExtractRelativePath(FromDir,ToDir),'"');
end;

Begin
 Testit ('/pp/src/compiler/','/pp/bin/win32/ppc386/');
 Testit ('/pp/bin/win32/ppc386/','/pp/src/compiler/');
 Testit ('/pp/bin/win32/','/pp/src/compiler/ppcx386/');
 Testit ('/pp/bin/win32/','/pp/src/compiler/ppcx386');
 Testit ('/pp/bin/win32','/pp/src/compiler/ppcx386');
 Testit ('e:/pp/bin/win32/ppc386/','d:/pp/src/compiler/');
 Testit ('e:\pp\bin\win32\ppc386/','d:\pp\src\compiler/');
 Testit ('C:\FPC\3.0.2\','C:\FPC\3.0.2\');
 Testit ('C:\FPC\3.0.2\','C:\FPC\3.0.4rc1\');
 Testit ('Q:\','Q:\FPC\3.0.4rc1\');
End.