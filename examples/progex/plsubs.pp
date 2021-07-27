program testsubs;

uses dynlibs;

Type
  TSubStrFunc =
    function(const CString:PChar;FromPos,ToPos: longint):PChar;cdecl;

const
  baselibname = 'libsubs.';
 
{$ifdef windows} 
  libext = 'dll';
{$else}
{$ifdef macos}
  libext = 'dylib';  
{$else}
  libext = 'so';
{$endif}
{$endif}

var
  s: PChar;
  FromPos, ToPos: Integer;
  lib : TLibHandle;
  SubStr : TSubStrFunc;

begin
  s := 'Test';
  FromPos := 2;
  ToPos := 3;
  lib:=LoadLibrary('libsubs.so');
  Pointer(Substr):=GetProcAddress(lib,'SubStr');
  WriteLn(SubStr(s, FromPos, ToPos));
  unloadLibrary(lib);
end.
