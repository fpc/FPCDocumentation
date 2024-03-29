Program Example41;

{ Program to demonstrate the MemAvail function. }

Var
  P, PP : Pointer;

begin
  GetMem (P,100);
  GetMem (PP,10000);
  FreeMem (P,100);
  {$ifdef msdos}
  { Due to the heap fragmentation introduced
    By the previous calls, the maximum amount of memory
    isn't equal to the maximum block size available. }
  Writeln ('Total heap available    (Bytes) : ',MemAvail);
  Writeln ('Largest block available (Bytes) : ',MaxAvail);
  {$else}
  Writeln('MemAvail and MaxAvail functions are only present in msdos rtl, for TP compatibility');
  {$endif}
end.
