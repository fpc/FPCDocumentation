program postproc;

{$mode objfpc}
{$h+}

uses classes, types, custapp, htmlpp;

type
  { TPostProcessorApp }

  TPostProcessorApp = class(TCustomApplication)
  private
    FProcessor : THTMLPostProcessor;
    procedure DoProcessorLog(sender: TObject; const Msg: string);
    procedure Usage(const Msg: string);
  protected
    procedure DoRun; override;
  public
    constructor create(aOwner : TComponent); override;
  end;

{ THTMLPostProcessor }

procedure TPostProcessorApp.DoProcessorLog(sender: TObject; const Msg: string);
begin
  Writeln(Msg);
end;

constructor TPostProcessorApp.create(aOwner: TComponent);

begin
  Inherited create(aOwner);
  FProcessor:=THTMLPostProcessor.Create(Self);
end;

procedure TPostProcessorApp.Usage(const Msg : string);

begin
  if Msg<>'' then
    Writeln('Error: ',Msg);
  Writeln('Usage ',ExeName,' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help                   this page');
  Writeln('-b --base-dir=DIR           directory to use as base dir for all relative names.');
  Writeln('-c --backup                 create a backup of each processed file.');
  Writeln('-d --directory=DIR          a directory with HTML files to process. This option can be specified multiple times.');
  Writeln('-n --new-issue-url=URL      the URL to report a new issue');
  Writeln('-t --timestamp              add timestamp to page footer');
  Writeln('-r --recurse                recurse into subdirectories');
  ExitCode:=Ord(Msg<>'');
end;

procedure TPostProcessorApp.DoRun;

var
  lErr : String;

begin
  Terminate;
  lErr:=checkoptions('hvtcrd:n:b:',['help','directory:','new-issue-url:','base-dir:','verbose','timestamp','backup','recurse']);
  if lErr<>'' then
    Usage(lErr);
  FProcessor.Dirs:=GetOptionValues('d','directory');
  FProcessor.NewIssueURL:=GetOptionValue('i','issue-url');
  FProcessor.BaseDir:=GetOptionValue('b','base-dir');
  FProcessor.TimeStamp:=HasOption('t','timestamp');
  FProcessor.Backup:=HasOption('c','backup');
  FProcessor.Recurse:=HasOption('r','recurse');
  if hasOption('v','verbose') then
    FProcessor.OnLog:=@DoProcessorLog;
  FProcessor.Execute;
end;

var
  Application : TPostProcessorApp; 
  
begin
  Application:=TPostProcessorApp.Create(Nil);  
  Application.Initialize;
  Application.Run;
  Application.Free;  
end.  
