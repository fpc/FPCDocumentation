program EventTemplateExample;

{$mode objfpc}{$h+}

uses
  fpTemplate, SysUtils;

type
  TEventHandler = class
    procedure GetParamValue(Sender: TObject; const ParamName: string; out AValue: string);
  end;

procedure TEventHandler.GetParamValue(Sender: TObject; const ParamName: string; out AValue: string);
begin
  case ParamName of
    'CurrentDate': AValue := DateToStr(Now);
    'CurrentTime': AValue := TimeToStr(Now);
    'UserName': AValue := GetEnvironmentVariable('USER');
  else
    AValue := 'Unknown';
  end;
end;

var
  Parser: TTemplateParser;
  Handler: TEventHandler;
  Template, Result: string;
begin
  Parser := TTemplateParser.Create;
  Handler := TEventHandler.Create;
  try
    // Assign event handler
    Parser.OnGetParam := @Handler.GetParamValue;

    // Define template text
    Template := 'Date: {CurrentDate}, Time: {CurrentTime}, User: {UserName}';

    // Parse the template
    Result := Parser.ParseString(Template);

    WriteLn(Result);
  finally
    Handler.Free;
    Parser.Free;
  end;
end.