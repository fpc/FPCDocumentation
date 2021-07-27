{$mode objfpc}
{$H+}
program cleanxml;

uses classes,sysutils;

Var
  FileNames : TStrings;

Function HasElementNode(FileName : String) : Boolean;

Var
  F : Text;
  L : String;
  
begin
  Result:=False;
  Assign(F,FileName);
  Reset(F);
  try
    While Not (Result or EOF(F)) do
      begin
      ReadLn(F,L);
      Result:=Pos('<element',L)<>0;
      end;
  Finally
    Close(F);
  end;  
end;

Procedure RemoveFile(FileName : String);

begin
  if DeleteFile(FileName) then
    Writeln('file ',FileName,' has no elements. Deleted.')
  else  
    Writeln('file ',FileName,' has no elements. Delete failed.');
end;


Procedure CleanFiles;

Var
  I : Integer;
  
begin
  For I:=0 to FileNames.Count-1 do
    if FileExists(FileNames[i]) then
      if Not (HasElementNode(FileNames[i])) then
        RemoveFile(FileNames[i]);   
  FreeAndNil(FileNames);
end;

Function ProcessArgs : boolean;

Var
  I : integer;
  Fn : String;
  
begin
  Result:=(ParamCount>0);
  If Result then
    begin
    FileNames:=TStringList.Create;
    For I:=1 to ParamCount do
      begin
      FN:=ParamStr(i);
      If FileExists(FN) then
        FileNames.Add(FN);
      end;
    end;    
end;


begin
  If ProcessArgs then
    CleanFiles;
end.