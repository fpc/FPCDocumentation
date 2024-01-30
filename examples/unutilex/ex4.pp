Program Example4;

{ Program to demonstrate the LocalToEpoch function. }

Uses UnixUtil;

Var year,month,day,hour,minute,second : Word;

begin
  Write ('Year    : ');readln(Year);
  Write ('Month   : ');readln(Month);
  Write ('Day     : ');readln(Day);
  Write ('Hour    : ');readln(Hour);
  Write ('Minute  : ');readln(Minute);
  Write ('Seonds  : ');readln(Second);
  Write ('This is : ');
  Write (LocalToEpoch(year,month,day,hour,minute,second));
  Writeln (' seconds past 1970-01-01T00:00');
end.
