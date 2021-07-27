{$mode objfpc}
{$h+}
unit ohb;

interface

Type
  TAObjectHelper = class helper for TObject
    function MemoryLocation: String;
  end;
  
implementation

uses sysutils;
   
function TAObjectHelper.MemoryLocation: String;

begin
  Result := format('%p',[pointer(Self)]);
end;

end.     