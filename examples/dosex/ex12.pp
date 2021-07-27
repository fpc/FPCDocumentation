program Example12;

uses Dos;

{ Program to demonstrate the FSplit function. }

var dir:dirstr;
    name:namestr;
    ext:extstr;

begin
  FSplit(ParamStr(1),dir,name,ext);
  WriteLn('Splitted ',ParamStr(1),' in:');
  WriteLn('Path     : ',dir);
  WriteLn('Name     : ',name);
  WriteLn('Extension: ',ext);
end.
