Program Example1;

{ Program to demonstrate the fptime function. }

Uses baseunix;

begin
  Write ('Second past the start of the Epoch (1970-01-01T00:00): ');
  Writeln (fptime);
end.
