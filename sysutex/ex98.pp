Program Example98;
{$mode objfpc}
{$h+}
{ This program demonstrates the  function }

Uses sysutils;

var
  count,i: Integer;
  f: Extended;
  s: String;
  
begin
  count:=SScanf('234 32.4 hello','%d %f %s',[@i,@f,@s]);
  writeln(count,' ',i,' ',f,' ',s);
End.