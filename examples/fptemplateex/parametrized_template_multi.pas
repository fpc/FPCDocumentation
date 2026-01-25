program ParametrizedTemplateExample;

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
  if TagString = 'FORMAT' then
  begin
    // Handle FORMAT tag with parameters
    ReplaceText := 'Formatted text';
    ParamValue:=TagParams.Values['style'];
    Case ParamValue of
    'bold':  ReplaceText := '**' + ReplaceText + '**';
    'italic':  ReplaceText := '*' + ReplaceText + '*';
    end;
    ParamValue:=TagParams.Values['color'];
    if ParamValue<>'' then 
      ReplaceText := '[' + ParamValue + ']' + ReplaceText + '[/' + ParamValue + ']';
  end
  else if TagString = 'REPEAT' then
  begin
    // Handle REPEAT tag with count parameter
    ReplaceText := StringOfChar('*', StrToIntDef(TagParams.Values['count'], 1));
  end
  else if TagString = 'TABLE' then
  begin
    // Handle TABLE tag with rows and cols parameters
    ReplaceText := 'Table';
    ReplaceText := ReplaceText + '(' + TagParams.Values['rows'] + ' rows';
    ReplaceText := ReplaceText + ', ' + TagParams.Values['cols']+' cols)';
  end
  else
    ReplaceText := 'Unknown tag: ' + TagString;
end;

var
  Parser: TTemplateParser;
  Handler: TEventHandler;
  Template, Result: string;
begin
  Parser := TTemplateParser.Create;
  Handler := TEventHandler.Create;
  try
    // Enable parametrized templates
    Parser.AllowTagParams := True;

    // Assign tag replacement handler
    Parser.OnReplaceTag := @Handler.ReplaceTagHandler;

    // Define template text with parametrized tags
    Template := 'This is {FORMAT[-style=bold-]} text with {FORMAT[-style=italic-][-color=red-]} formatting.' + LineEnding +
                'Here are some {REPEAT[-count=5-]} stars.' + LineEnding +
                'And here is a {TABLE[-rows=3-][-cols=4-]} definition.';

    // Parse the template
    Result := Parser.ParseString(Template);

    WriteLn(Result);
  finally
    Handler.Free;
    Parser.Free;
  end;
end.