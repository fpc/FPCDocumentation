{$mode objfpc}
{$h+}
program checkxml;

uses sysutils, dom, xmlread;

Var
  XML : TXMLDocument;
  I : Integer;

begin
  if ParamCount=0 then
    Writeln('Usage : ',ExtractFilePath(paramstr(0)),' file1...FileN')
  else
    begin
    for I:=1 to ParamCount do
      begin
      Write('Reading ',Paramstr(i),' : ');
      try
        ReadXMLFile(XML,ParamStr(i));
        FreeAndNil(XML);
        Writeln('OK.');
      except
        On E : Exception do
          Writeln('Found error :',E.Message);
      end;
      end;
    end;
end.
