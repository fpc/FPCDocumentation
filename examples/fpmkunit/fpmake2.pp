program fpmake2;

uses fpmkunit;

var
  P: TPackage;
  
begin
  With Installer do
    begin
    Defaults.Options.Add('-O2');
    P:=Packages.AddPackage('mypackage');
    P.Dependencies.Clear;
    P.Targets.AddUnit('src/unit1.pp');
    Run;
    end;
end.
