program DefaultsURIExample;

{$mode objfpc}{$h+}

uses
  URIParser, SysUtils;

procedure ParseWithDefaults(const URIString: string; const DefaultProtocol: string; DefaultPort: Word);
var
  URI: TURI;
begin
  WriteLn('Parsing: ', URIString);
  WriteLn('Default protocol: ', DefaultProtocol);
  WriteLn('Default port: ', DefaultPort);

  // Parse with default protocol and port
  URI := ParseURI(URIString, DefaultProtocol, DefaultPort);

  WriteLn('Results:');
  WriteLn('  Protocol: ', URI.Protocol);
  WriteLn('  Host: ', URI.Host);
  WriteLn('  Port: ', URI.Port);
  WriteLn('  Path: ', URI.Path);
  WriteLn('  Document: ', URI.Document);
  WriteLn('  Full URI: ', EncodeURI(URI));
  WriteLn;
end;

begin
  // Parse URIs with missing protocol/port using defaults
  ParseWithDefaults('www.example.com/index.html', 'http', 80);
  ParseWithDefaults('ftp.example.org/pub/files/', 'ftp', 21);
  ParseWithDefaults('mail.example.com:993/inbox', 'imaps', 993);

  // Parse complete URIs (defaults are ignored)
  ParseWithDefaults('https://secure.example.com:443/login', 'http', 80);
end.