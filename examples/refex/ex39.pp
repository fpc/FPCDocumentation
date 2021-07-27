Program Example39;

{ Program to demonstrate the Mark and Release functions. }

Var P,PP,PPP,MM : Pointer;

begin
  {$ifdef msdos}
  Getmem (P,100);
  Mark (MM);
  Writeln ('Getmem 100   : Memory available : ',MemAvail,' (marked)');
  GetMem (PP,1000);
  Writeln ('Getmem 1000  : Memory available : ',MemAvail);
  GetMem (PPP,100000);
  Writeln ('Getmem 10000 : Memory available : ',MemAvail);
  Release (MM);
  Writeln ('Released     : Memory available : ',MemAvail);
  { At this point, PP and PPP are invalid ! }
  {$else}
  Writeln('Mark and Release functions are only present in msdos rtl, for TP compatibility');
  {$endif}
end.
