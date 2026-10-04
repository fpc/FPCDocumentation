unit htmlpp;

{$mode ObjFPC}{$H+}

interface

uses
  Types, Classes, SysUtils, StrUtils;

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
    FSidebar: Boolean;
    FTimeStamp: Boolean;
    procedure GetFilesInDir(aDir: string; aFiles: TStrings);
  protected
    procedure DoLog(Const Msg : String); overload;
    procedure DoLog(Const Fmt : String; const Args : Array of const); overload;
  public
    constructor create(aOwner : TComponent); override;
    procedure processfile(const aBaseDir, aFileName : String);
    // Copies the contents list of a manual to a javascript file for the sidebar.
    procedure WriteTocFile(const aDir : String);
    procedure execute; virtual;
    // Write the contents list of every processed directory to fpc-toc.js.
    property Sidebar : Boolean Read FSidebar Write FSidebar;
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
  lLink,lReportFile,lFooterLine : String;
  lFooter,lFooterPos : Integer;

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
    lFooter:=lLast;
    While (lFooter>=0) and (Pos('</footer>',lFile[lFooter])=0) do
      Dec(lFooter);
    if lFooter<>-1 then
      begin
      // Make sure the closing tag is on a line by it's own, so we can insert correctly.
      lFooterLine:=lFile[lFooter];
      lFooterPos:=Pos('</footer>',lFooterLine);
      if lFooterPos>1 then
        begin
        lFile[lFooter]:=Copy(lFooterLine,1,lFooterPos-1);
        Delete(lFooterLine,1,lFooterPos-1);
        inc(lFooter);
        lFile.Insert(lFooter,lFooterLine);
        end;
      lLast:=lFooter;
      end;
    if lLast<0 then
      Raise Exception.Create('End of html not found while treating '+aFileName);
    if lFooter=0 then
      begin
      AddLine('<footer>');
      AddLine('<hr>');
      end;
    if FTimeStamp then
      AddLine('<span class="timestamp">Page generated on '+FormatDateTime('yyyy-mm-dd',Date)+'.</span>&nbsp;');
    lReportFile:=ExtractRelativePath(aBaseDir,aFileName);
    lLink:=NewIssueURL+'?issue%5Btitle%5D=issue%20in%20page%20'+lReportFile;
    lLink:=Format('<a class="reportissue-link" href="%s">Report a problem on this page</a>',[lLink]);
    AddLine(lLink);
    if lFooter=0 then
      AddLine('</footer>');
    lFile.SaveToFile(aFileName);
  finally
    lFile.Free;
  end;
end;


procedure THTMLPostProcessor.WriteTocFile(const aDir: String);

const
  SMarker = '<div class="tableofcontents"';

var
  lHTML : TStrings;
  lName,lMain,lContent,lToc : String;
  lStart,lOpen,lStop,lLevel,lIdx : Integer;

begin
  lName:=ExtractFileName(ExcludeTrailingPathDelimiter(aDir));
  lMain:=IncludeTrailingPathDelimiter(aDir)+lName+'.html';
  if not FileExists(lMain) then
    begin
    DoLog('No main page "%s", no contents list written',[lMain]);
    Exit;
    end;
  lHTML:=TStringList.Create;
  try
    lHTML.LoadFromFile(lMain);
    lContent:=lHTML.Text;
    lStart:=Pos(SMarker,lContent);
    if lStart=0 then
      begin
      DoLog('No contents list in "%s", nothing written',[lMain]);
      Exit;
      end;
    lOpen:=PosEx('>',lContent,lStart);
    if lOpen=0 then
      begin
      DoLog('Unfinished contents list in "%s", nothing written',[lMain]);
      Exit;
      end;
    // Look for the tag closing the list, skipping any list nested in it.
    lLevel:=1;
    lStop:=0;
    lIdx:=lOpen+1;
    While (lIdx<=Length(lContent)) and (lStop=0) do
      begin
      if Copy(lContent,lIdx,4)='<div' then
        Inc(lLevel)
      else if Copy(lContent,lIdx,6)='</div>' then
        begin
        Dec(lLevel);
        if lLevel=0 then
          lStop:=lIdx;
        end;
      Inc(lIdx);
      end;
    if lStop=0 then
      begin
      DoLog('Unclosed contents list in "%s", nothing written',[lMain]);
      Exit;
      end;
    lToc:=Copy(lContent,lOpen+1,lStop-lOpen-1);
    // Tags are spread over several lines, a space keeps them apart.
    lToc:=StringReplace(lToc,#13,' ',[rfReplaceAll]);
    lToc:=StringReplace(lToc,#10,' ',[rfReplaceAll]);
    lToc:=StringReplace(lToc,'\','\\',[rfReplaceAll]);
    lToc:=StringReplace(lToc,'"','\"',[rfReplaceAll]);
    lHTML.Clear;
    lHTML.Add('/* Contents of this manual, used by fpc-theme-switch.js. */');
    lHTML.Add('window.fpcDocToc = "'+lToc+'";');
    lHTML.SaveToFile(IncludeTrailingPathDelimiter(aDir)+'fpc-toc.js');
    DoLog('Wrote contents list of "%s"',[lName]);
  finally
    lHTML.Free;
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
      if FSidebar then
        WriteTocFile(lDir);
      end;
  finally
    lFiles.Free;
  end;
end;


end.

