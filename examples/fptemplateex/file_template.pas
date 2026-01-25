program FileTemplateExample;

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
    'Title': AValue := 'Welcome Page';
    'Content': AValue := 'This is the main content of the page.';
    'Footer': AValue := 'Copyright 2024 Example Company';
  else
    AValue := '';
  end;
end;

var
  Template: TFPTemplate;
  Handler: TEventHandler;
  Result: string;
begin
  Template := TFPTemplate.Create;
  Handler := TEventHandler.Create;
  try
    // Set template content directly
    Template.Template := '<html><head><title>{Title}</title></head>' +
                         '<body><h1>{Title}</h1><p>{Content}</p>' +
                         '<footer>{Footer}</footer></body></html>';

    // Assign event handler
    Template.OnGetParam := @Handler.GetParamValue;

    // Get parsed content
    Result := Template.GetContent;

    WriteLn(Result);
  finally
    Handler.Free;
    Template.Free;
  end;
end.