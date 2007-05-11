Program Example25;

{ Program to demonstrate the UTime function. }

Uses Dos,BaseUnix,Unix,UnixUtil;

Var utim : utimbuf;
    dow,msec,year,month,day,hour,minute,second : Word;

begin
  { Set access and modification time of executable source }
  GetTime (hour,minute,second,msec);
  GetDate (year,month,day,dow);
  utim.actime:=LocalToEpoch(year,month,day,hour,minute,second);
  utim.modtime:=utim.actime;
  if Fputime('ex25.pp',@utim)<>0 then
    writeln ('Call to UTime failed !')
  else
    begin
    Write ('Set access and modification times to : ');
    Write (Hour:2,':',minute:2,':',second,', ');
    Writeln (Day:2,'/',month:2,'/',year:4);
    end;
end.
