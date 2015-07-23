program testfail;

{$mode objfpc}

Type
  TMyClass = Class
    Constructor Create;
  end;
  
  
Constructor TMyClass.Create;    

begin
  Fail;
end;

var
  M : TMyClass;
  
begin
  M:=TMyClass.Create;
  Writeln('M is nil : ',Not Assigned(M));
end.  