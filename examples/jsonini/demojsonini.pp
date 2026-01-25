program DemoJsonIni;

uses
  JsonIni, SysUtils;

var
  IniFile: TJSONIniFile;
begin
  // Create or open a JSON ini file
  IniFile := TJSONIniFile.Create('config.json');
  try
    // Write configuration values
    IniFile.WriteString('Database', 'Host', 'localhost');
    IniFile.WriteInteger('Database', 'Port', 5432);
    IniFile.WriteBool('Database', 'UseSSL', True);

    IniFile.WriteString('UI', 'Theme', 'Dark');
    IniFile.WriteInteger('UI', 'Width', 1024);
    IniFile.WriteInteger('UI', 'Height', 768);
    
    IniFile.UpdateFile;

    // Read configuration values with defaults
    WriteLn('Database Host: ', IniFile.ReadString('Database', 'Host', 'unknown'));
    WriteLn('Database Port: ', IniFile.ReadInteger('Database', 'Port', 0));
    WriteLn('Use SSL: ', IniFile.ReadBool('Database', 'UseSSL', False));

    WriteLn('UI Theme: ', IniFile.ReadString('UI', 'Theme', 'Light'));
    WriteLn('Window Size: ', IniFile.ReadInteger('UI', 'Width', 800), 'x',
            IniFile.ReadInteger('UI', 'Height', 600));

  finally
    IniFile.Free;
  end;
end.
