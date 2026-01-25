program BasicTemplateExample;

{$mode objfpc}{$h+}

uses
  fpTemplate, SysUtils;

var
  Parser: TTemplateParser;
  Template, Result: string;
begin
  Parser := TTemplateParser.Create;
  try
    // Set up template values
    Parser.Values['Name'] := 'John Doe';
    Parser.Values['Age'] := '30';
    Parser.Values['City'] := 'Amsterdam';

    // Define template text
    Template := 'Hello {Name}! You are {Age} years old and live in {City}.';

    // Parse the template
    Result := Parser.ParseString(Template);

    WriteLn(Result);
  finally
    Parser.Free;
  end;
end.