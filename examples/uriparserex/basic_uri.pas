program BasicURIExample;

{$mode objfpc}{$h+}

uses
  URIParser, SysUtils;

procedure ParseAndDisplay(const URIString: string);
var
  URI: TURI;
begin
  WriteLn('Original URI: ', URIString);

  // Parse the URI
  URI := ParseURI(URIString);

  // Display all components
  WriteLn('  Protocol: ', URI.Protocol);
  WriteLn('  Username: ', URI.Username);
  WriteLn('  Password: ', URI.Password);
  WriteLn('  Host: ', URI.Host);
  WriteLn('  Port: ', URI.Port);
  WriteLn('  Path: ', URI.Path);
  WriteLn('  Document: ', URI.Document);
  WriteLn('  Params: ', URI.Params);
  WriteLn('  Bookmark: ', URI.Bookmark);
  WriteLn('  Has Authority: ', URI.HasAuthority);

  // Reconstruct the URI
  WriteLn('  Reconstructed: ', EncodeURI(URI));
  WriteLn;
end;

begin
  // Parse different types of URIs
  ParseAndDisplay('https://user:pass@www.example.com:8080/path/to/page.html?param1=value1&param2=value2#section1');
  ParseAndDisplay('ftp://files.example.org/pub/software/myfile.tar.gz');
  ParseAndDisplay('mailto:user@example.com');
  ParseAndDisplay('file:///home/user/document.txt');
  ParseAndDisplay('/relative/path/file.html');
end.