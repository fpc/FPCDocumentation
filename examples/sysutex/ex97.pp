{ This example demonstrates the generic IfThen function }

{$mode delphi}
uses sysutils;

type
  Ttest = function:string;

function SomeTestOne:string;
begin
 writestr(result,'Test1');
end;

function SomeTestTwo:string;
begin
 writestr(result,'Test2')
end;

var
  a:integer = 100;
  b:integer = 200;
  c:single = 0.001;
  d:single = 1.000;
  e:TTest = SomeTestOne;
  f:TTest = SomeTestTwo;
begin
  writeln(ifthen<integer>(true,a,b));
  writeln(ifthen<integer>(false,a,b));
  writeln(ifthen<single>(true,c,d):2:3);
  writeln(ifthen<single>(false,c,d):2:3);
  writeln(ifthen<Ttest>(true,e,f)); // executes e
  writeln(ifthen<Ttest>(false,e,f)); // executes f
end.
