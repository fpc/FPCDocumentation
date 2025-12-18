program fpmake4;

uses fpmkunit;

var
  P: TPackage;
  T : TTarget;
  
begin
  With Installer do
    begin
    P:=Packages.AddPackage('mypackage');
    P.Dependencies.Clear;
    T:=P.Targets.AddUnit('src2/unita.pp');
    T.Dependencies.AddUnit('unitb');
    P.Targets.AddUnit('src2/unitb.pp');
    Run;
    end;
end.
