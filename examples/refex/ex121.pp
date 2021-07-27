{ 
  Example 121: 
  Continue, break and exit are system procedures.
  They can be redefined
}

procedure continue;

begin
  Writeln('Continue');
end;

Procedure Exit;

begin
  Writeln('exit');
end;

Procedure Break;

begin
  Writeln('Break');
end;

begin
  Repeat 
    Continue;
    Break;
    exit;
  Until True;  
end.