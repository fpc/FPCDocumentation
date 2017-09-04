Program Example92;

{ Program to demonstrate the GetResourceStringName function. }
{$Mode Delphi}

ResourceString

  First  = 'First string';
  Second = 'Second String';

Var I,J : Longint;

begin
{$ifndef RESSTRSECTIONS}
  { Print names of all resourcestrings }
  For I:=0 to ResourceStringTableCount-1 do
    For J:=0 to ResourceStringCount(i)-1 do
      Writeln (I,',',J,' : ',GetResourceStringName(I,J));
{$else}
  Writeln('Functions ResuorceStringTableCount, ResourceStringCount and GetResourceStringCurrentValue are inot available for RESSTRSECTIONS');
{$endif}
end.
