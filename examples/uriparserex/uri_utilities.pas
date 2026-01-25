program URIUtilitiesExample;

{$mode objfpc}{$h+}

uses
  URIParser, SysUtils;

var
  BaseURI, RelativeURI, AbsoluteURI: string;
  Filename: string;
  URI: string;

begin
  WriteLn('=== Relative URI Resolution ===');
  BaseURI := 'https://www.example.com/docs/manual/';
  RelativeURI := '../images/logo.png';

  if ResolveRelativeURI(BaseURI, RelativeURI, AbsoluteURI) then
  begin
    WriteLn('Base URI: ', BaseURI);
    WriteLn('Relative URI: ', RelativeURI);
    WriteLn('Absolute URI: ', AbsoluteURI);
  end
  else
    WriteLn('Failed to resolve relative URI');

  WriteLn;
  WriteLn('=== File/URI Conversion ===');

  // Convert filename to URI
  Filename := '/home/user/documents/readme.txt';
  URI := FilenameToURI(Filename);
  WriteLn('Filename: ', Filename);
  WriteLn('File URI: ', URI);

  // Convert URI back to filename
  if URIToFilename(URI, Filename) then
  begin
    WriteLn('Converted back to filename: ', Filename);
  end
  else
    WriteLn('Failed to convert URI to filename');

  WriteLn;
  WriteLn('=== URI Type Checking ===');

  // Check if URIs are absolute
  URI := 'https://www.example.com/page.html';
  WriteLn('URI: ', URI, ' - Is absolute: ', IsAbsoluteURI(URI));

  URI := '/relative/path.html';
  WriteLn('URI: ', URI, ' - Is absolute: ', IsAbsoluteURI(URI));

  URI := 'mailto:test@example.com';
  WriteLn('URI: ', URI, ' - Is absolute: ', IsAbsoluteURI(URI));
end.