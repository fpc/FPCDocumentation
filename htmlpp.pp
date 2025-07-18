unit htmlpp;

{$mode ObjFPC}{$H+}

interface

uses
  Types, Classes, SysUtils;

Type
  { THTMLPostProcessor }
  TProcessorLogEvent = procedure(sender : TObject; const Msg : string) of object;

  THTMLPostProcessor = class(TComponent)
  private
    FBackup: Boolean;
    FBaseDir: String;
    FBaseDirNewIssueURL: String;
    FDirs: TStringDynArray;
    FNewIssueURL: String;
    FOnLog: TProcessorLogEvent;
    FRecurse: Boolean;
    FTimeStamp: Boolean;
    procedure GetFilesInDir(aDir: string; aFiles: TStrings);
  protected
    procedure DoLog(Const Msg : String); overload;
    procedure DoLog(Const Fmt : String; const Args : Array of const); overload;
  public
    constructor create(aOwner : TComponent); override;
    procedure processfile(const aBaseDir, aFileName : String);
    procedure execute; virtual;
    property TimeStamp : Boolean Read FTimeStamp Write FTimeStamp;
    property Backup : Boolean Read FBackup Write FBackup;
    property Recurse : Boolean Read FRecurse Write FRecurse;
    property Dirs : TStringDynArray read FDirs Write FDirs;
    property NewIssueURL : String Read FNewIssueURL Write FNewIssueURL;
    property BaseDir : String Read FBaseDirNewIssueURL Write FBaseDir;
    property OnLog : TProcessorLogEvent Read FOnLog Write FOnLog;
  end;

implementation

procedure THTMLPostProcessor.DoLog(const Msg: String);
begin
  if assigned(FOnLog) then
    FOnLog(Self,Msg);
end;

procedure THTMLPostProcessor.DoLog(const Fmt: String; const Args: array of const);
begin
  if assigned(FOnLog) then
    DoLog(Format(Fmt,Args));
end;

constructor THTMLPostProcessor.create(aOwner: TComponent);
begin
  inherited create(aOwner);
end;


procedure THTMLPostProcessor.GetFilesInDir(aDir: string; aFiles : TStrings);
var
  lInfo : TSearchRec;
  lCount : Integer;

begin
  lCount:=0;
  if FindFirst(aDir+'*.html',0,lInfo)=0 then
    try
      repeat
        aFiles.add(aDir+lInfo.Name);
        inc(lcount);
      until FindNext(lInfo)<>0;
    finally
      FindClose(lInfo);
    end;
  DoLog('Found %d files in directory "%s"',[lCount,aDir]);
  if Recurse then
    if FindFirst(aDir+AllFilesMask,faDirectory,lInfo)=0 then
      try
        repeat
          if ((lInfo.Attr and faDirectory)<>0)
             and (lInfo.Name<>'.')
             and (lInfo.Name<>'..') then
            GetFilesInDir(IncludeTrailingPathDelimiter(aDir+lInfo.Name),aFiles);
        until FindNext(lInfo)<>0;
      finally
        FindClose(lInfo);
      end;

end;


procedure THTMLPostProcessor.processfile(const aBaseDir, aFileName: String);
var
  lFile : TStrings;
  lLast : integer;

  procedure addLine(const aLine : string);
  begin
    lFile.Insert(lLast,aLine);
    inc(lLast);
  end;

var
  lLink,lReportFile : String;

begin
  lFile:=TStringList.Create;
  try
    lFile.LoadFromFile(aFileName);
    if Backup then
      lFile.SaveToFile(aFileName+'.bak');
    lLast:=lFile.Count-1;
    While (lLast>=0) and (Pos('</html>',lFile[lLast])=0) do
      Dec(lLast);
    While (lLast>=0) and (Pos('</body>',lFile[lLast])=0) do
      Dec(lLast);
    if lLast<0 then
      Raise Exception.Create('End of html not found while treating '+aFileName);
    AddLine('<footer>');
    AddLine('<hr>');
    if FTimeStamp then
      AddLine('<span>Page generated on '+FormatDateTime('yyyy-mm-dd',Date)+'.</span>&nbsp;');
    lReportFile:=ExtractRelativePath(aBaseDir,aFileName);
    lLink:=NewIssueURL+'?issue%5Btitle%5D=issue%20in%20page%20'+lReportFile;
    lLink:=Format('<a href="%s">Report a problem on this page</a>',[lLink]);
    AddLine(lLink);
    AddLine('</footer>');
    lFile.SaveToFile(aFileName);
  finally
    lFile.Free;
  end;
end;


procedure THTMLPostProcessor.execute;
var
  lFiles : TStrings;
  lDir, lFile : string;

begin
  if NewIssueURL='' then
    NewIssueURL:='https://gitlab.com/freepascal.org/fpc/documentation/-/issues/new';
  if FBaseDir<>'' then
    FBaseDir:=IncludeTrailingPathDelimiter(FBaseDir);
  lFiles:=TStringList.Create;
  try
    for lDir in FDirs do
      begin
      lFiles.Clear;
      GetFilesInDir(IncludeTrailingPathDelimiter(lDir),lFiles);
      For lFile in lFiles do
        begin
        DoLog('Processing file %s',[lFile]);
        try
          processfile(FBaseDir,lFile);
        except
          on E : Exception do
            DoLog('Exception %s processing file "%s": %s',[E.ClassName,lFile,E.Message]);
        end;
        end;
      end;
  finally
    lFiles.Free;
  end;
end;


end.

