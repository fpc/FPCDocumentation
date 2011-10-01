program ex96;

{ This program demonstrates the Concatpaths function }

uses sysutils;

begin
  // will write /this/path/more/levels/
  Writeln(ConcatPaths(['/this/','path','more/levels/']));
  // will write this/path/more/levels/
  Writeln(ConcatPaths(['this/','path','more/levels/']));
  // will write this/path/more/levels
  Writeln(ConcatPaths(['this/','path','more/levels']));
end.