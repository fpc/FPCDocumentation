program ParametrizedTFPTemplateExample;

{$mode objfpc}{$h+}

uses
  fpTemplate, SysUtils, Classes;

type
  TEventHandler = class
    procedure ReplaceTagHandler(Sender: TObject; const TagString: string;
      TagParams: TStringList; out ReplaceText: string);
  end;

procedure TEventHandler.ReplaceTagHandler(Sender: TObject; const TagString: string;
  TagParams: TStringList; out ReplaceText: string);
var
  i: Integer;
  ParamName, ParamValue: string;
begin
  if TagString = 'LINK' then
  begin
    // Handle LINK tag with url and text parameters
    ReplaceText := '#';
    for i := 0 to TagParams.Count - 1 do
    begin
      ParamName := TagParams.Names[i];
      ParamValue := TagParams.ValueFromIndex[i];
      if ParamName = 'url' then
        ReplaceText := '<a href="' + ParamValue + '">'
      else if ParamName = 'text' then
        ReplaceText := ReplaceText + ParamValue + '</a>';
    end;
  end
  else if TagString = 'IMG' then
  begin
    // Handle IMG tag with src and alt parameters
    ReplaceText := '<img';
    for i := 0 to TagParams.Count - 1 do
    begin
      ParamName := TagParams.Names[i];
      ParamValue := TagParams.ValueFromIndex[i];
      ReplaceText := ReplaceText + ' ' + ParamName + '="' + ParamValue + '"';
    end;
    ReplaceText := ReplaceText + ' />';
  end
  else
    ReplaceText := '';
end;

var
  Template: TFPTemplate;
  Handler: TEventHandler;
  Result: string;
begin
  Template := TFPTemplate.Create;
  Handler := TEventHandler.Create;
  try
    // Enable parametrized templates
    Template.AllowTagParams := True;

    // Set template content
    Template.Template := '<p>Visit our {LINK[-url=http://www.freepascal.org-][-text=website-]} for more information.</p>' + LineEnding +
                         '<p>Logo: {IMG[-src=logo.png-][-alt=FreePascal Logo-]}</p>';

    // Assign tag replacement handler
    Template.OnReplaceTag := @Handler.ReplaceTagHandler;

    // Get parsed content
    Result := Template.GetContent;

    WriteLn(Result);
  finally
    Handler.Free;
    Template.Free;
  end;
end.