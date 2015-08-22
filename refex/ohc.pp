{$mode objfpc}
{$h+}
unit ohc;

interface

uses oha;

Type
  TAObjectHelper = class helper(TObjectHelper) for TObject
    function MemoryLocation: String;
  end;
  
implementation

uses sysutils;
   
function TAObjectHelper.MemoryLocation: String;

begin
  Result := format('%p',[pointer(Self)]);
end;

end.     