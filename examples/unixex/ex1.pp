Program Example1;

{ Program to demonstrate the GetEpochTime function. }

Uses Unix;

begin
  Write ('Secs past the start of the Epoch (1970-01-01T00:00) : ');
  Writeln (GetEpochTime);
end.
