program fpmake1;

uses fpmkunit;

var
  P: TPackage;
  
begin
  With Installer do
    begin
    P:=Packages.AddPackage('mypackage');
    P.Dependencies.Clear;
    P.Targets.AddUnit('src/unit1.pp');
    Run;
    end;
end.