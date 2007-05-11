Program Example1;

{ Program to demonstrate the fptime function. }

Uses baseunix;

begin
  Write ('Secs past the start of the Epoch (00:00 1/1/1980) : ');
  Writeln (fptime);
end.
