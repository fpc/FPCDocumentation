{$mode objfpc}
{$h+}
unit oha;

interface

Type
  TObjectHelper = class helper for TObject
    function AsString(const aFormat: String): String;
  end;
  
implementation

uses sysutils;
   
function TObjectHelper.AsString(const aFormat: String): String;

begin
  Result := Format(aFormat, [ToString]);
end;

end.     