program fpmake2;

uses fpmkunit;

begin
  With Installer do
    begin
    AddPackageFromDir('mypackage','src');
    Run;
    end;
end.
