program checkemptytags;

{$mode objfpc}{$H+}

uses
  Types, Classes, SysUtils, CustApp, DOM, XMLRead;

type
  { TCheckEmptyTagsApplication }

  { TLineInfo }

  TLineInfo = Class(TObject)
    FileName: String;
    Open,Close : Integer;
    constructor create(const aFileName : String; aOpen,aClose : Integer);
  end;

  TCheckEmptyTagsApplication = class(TCustomApplication)
  private
    FCheckDescr: Boolean;
    procedure AddLineNumbers(aList: TStrings; const aFileName: string);
    function CheckFile(const aFileName: string; aDescr, aLineNo: Boolean): boolean;
    procedure ReportTags(aList: TStrings; const aFileName: String);
    procedure SetCheckDescr(AValue: Boolean);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(aError : String);
    property CheckDescr: Boolean read FCheckDescr write SetCheckDescr;
  end;

{ TLineInfo }

constructor TLineInfo.create(const aFileName: String; aOpen, aClose: Integer);
begin
  FileName:=aFileName;
  Open:=aOpen;
  Close:=aClose;
end;

{ TCheckEmptyTagsApplication }

procedure TCheckEmptyTagsApplication.SetCheckDescr(AValue: Boolean);
begin
  if FCheckDescr=AValue then exit;
  FCheckDescr:=AValue;
end;

procedure TCheckEmptyTagsApplication.AddLineNumbers(aList : TStrings; const aFileName : string);
var
  lLines : TStrings;
  lLine,lName : String;
  i,idx,p,lStart,lEnd : Integer;
begin
  lStart:=0;
  lEnd:=0;
  lLines:=TstringList.Create;
  try
    lLines.LoadFromFile(aFileName);
    I:=0;
    While I<lLines.Count-1 do
      begin
      lLine:=lLines[i];
      if (pos('<element ',lLine)<>0) and (pos('/>',lLine)=0) then
        begin
        if (lStart<>0) then
           Writeln(stdErr,'Error: element start without ending element at ',aFileName,':',lStart);
        lStart:=i+1;
        p:=Pos('name',lLine);
        Delete(lLine,1,p+3);
        p:=Pos('"',lLine);
        Delete(lLine,1,p);
        p:=Pos('"',lLine);
        lName:=copy(lLine,1,p-1);
        end
      else
        if pos('</element',lLine)<>0 then
          begin
          if lStart=0 then
            Writeln(stdErr,' Error: element end without ending element at ',aFileName,':',i+1);
          lEnd:=i+1;
          idx:=aList.IndexOf(lName);
          if idx<>-1 then
              aList.Objects[Idx]:=TLineInfo.Create(aFileName,lStart,lEnd);
          lStart:=0;
          lEnd:=0;
          lName:='';
          end;
      inc(I);
      end;
    if lName<>'' then
      Writeln(stdErr,'Error: element ',lName,' start without ending element at ',aFileName,':',lStart);
  finally
    lLines.Free;
  end;
end;


procedure TCheckEmptyTagsApplication.ReportTags(aList : TStrings; const aFileName : String);
var
  i : integer;
  lInfo : TLineInfo;
begin
  for I:=0 to aList.Count-1 do
    begin
    lInfo:=TLineInfo(aList.Objects[i]);
    if not assigned(lInfo) then
      Write(aFileName,':')
    else
      With lInfo do
        Write(Format('%s:(%d - %d) ',[FileName,Open,Close]));
    Writeln(aList[i]);
    end;
end;

Function TCheckEmptyTagsApplication.CheckFile(const aFileName : string; aDescr,aLineNo : Boolean) : boolean;

var
  lList : TStrings;
  Elements, Tags: TDOMNodeList;
  i: Integer;
  XMLDoc: TXMLDocument;
  ElementName, TagName: String;
  CheckNode: TDOMNode;

begin
  Result:=False;
  if not FileExists(aFileName) then
    begin
    WriteLn('Error: File not found - ' + aFileName);
    exit;
    end;
  if aDescr then
    TagName := 'descr'
  else
    TagName := 'short';
   // Read and parse the XML file
  try
    ReadXMLFile(XMLDoc, aFileName);
  except
    on E: Exception do
      begin
      WriteLn('Error parsing XML file: ' + E.Message);
      Exit;
      end;
  end;
  lList:=Nil;
  try
    lList:=TStringList.Create;
    // Get all <element> tags
    Elements := XMLDoc.GetElementsByTagName('element');
    for i := 0 to Elements.Count - 1 do
      begin
      if Elements[i].NodeType <> ELEMENT_NODE then
        Continue;
      ElementName := UTF8Encode((Elements[i] as TDOMElement).GetAttribute('name'));
      Tags := (Elements[i] as TDOMElement).GetElementsByTagName(UTF8Decode(TagName));
      if Tags.Count > 0 then
        begin
        CheckNode := Tags[0];
        if (CheckNode.FirstChild = nil) then
          lList.Add(ElementName)
        else if (CheckNode.FirstChild.NodeType=TEXT_NODE)
                and (Trim(CheckNode.FirstChild.NodeValue) = '') then
          lList.Add(ElementName);
        end;
      end;
    if (lList.Count=0) then
      Writeln('All ',TagName,' tags have content in file ',aFileName)
    else
      begin
      if aLineNo then
        AddLineNumbers(lList,aFileName);
      ReportTags(lList,aFileName);
      end;
    Result:=True;
  finally
    XMLDoc.Free;
    for I:=0 to lList.Count-1 do
       (lList.Objects[i]).Free;
    lList.Free;
  end;
end;

procedure TCheckEmptyTagsApplication.DoRun;
const
  Short : String = 'dlh';
  Long : Array of string = ('description','line-no','help');
var
  aFileName,ErrorMsg: String;
  XMLFileNames: TStringDynArray;
  ShowLineNumbers, ShowDescription : Boolean;

begin
  Terminate;
  ErrorMsg:=CheckOptions(Short,Long);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    exit;
    end;
  ShowLineNumbers:=HasOption('l','line-no');
  ShowDescription:=HasOption('d','description');
  XMLFileNames:=GetNonOptions(Short,Long);
  if Length(XMLFileNames)=0 then
    begin
    Usage('Need at least 1 filename');
    exit;
    end;
  for aFileName in XMLFileNames do
    if not CheckFile(aFileName,ShowDescription,ShowLineNumbers) then
      break;
end;

constructor TCheckEmptyTagsApplication.Create(TheOwner: TComponent);
var
  I : Integer;
begin
  inherited Create(TheOwner);
  StopOnException := True;
  CheckDescr:=False;
  // Parameter parsing is now done in DoRun for better flexibility.
  // This constructor initializes the application, DoRun handles specific argument processing.
end;

destructor TCheckEmptyTagsApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TCheckEmptyTagsApplication.Usage(aError: String);
begin
  if aError<>'' then
    Writeln('Error: ',aError);
  WriteLn('Usage: ' + ExeName + ' [options] <xmlfile> [<xmlfile2>]');
  WriteLn('Where options is one or more of:');
  Writeln('-h --help           This help');
  Writeln('-d --description    Check description <descr> tags. Default is to check <short> tags');
  Writeln('-l --line-no        add filename:<startlineno>-<endlineno>');
  ExitCode:=Ord(aError<>'');
end;

var
  Application: TCheckEmptyTagsApplication;
begin
  Application := TCheckEmptyTagsApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
