Program Example95;

{ This program demonstrates the IncludeLeadingPathDelimiter function }

Uses sysutils;

Begin
  // Will print "/this/path"
  Writeln(IncludeLeadingPathDelimiter('this/path'));
  // The same result 
  Writeln(IncludeLeadingPathDelimiter('/this/path'));
End.