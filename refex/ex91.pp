Program Example91;

{ Program to demonstrate the GetResourceStringDefaultValue function. }
{$Mode Delphi}

ResourceString

  First  = 'First string';
  Second = 'Second String';

Var I,J : Longint;

begin
{$ifndef RESSTRSECTIONS}
  { Print default values of all resourcestrings }
  For I:=0 to ResourceStringTableCount-1 do
    For J:=0 to ResourceStringCount(i)-1 do
      Writeln (I,',',J,' : ',GetResourceStringDefaultValue(I,J));
{$else}
  Writeln('Functions ResourceStringTableCount, ResourceStringCount and GetResourceStringCurrentValue are inot available for RESSTRSECTIONS');
{$endif}
end.
