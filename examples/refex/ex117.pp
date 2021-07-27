{
  This example demonstrates the use of the Initialize and Finalize functions
  used to initialize (and clean up) any RTTI-enabled data not allocated with 
  New or Create.
}

{$mode objfpc}
{$h+} // use ansistrings, they need to be initialized.
Type
  PData = ^TData;
  TData = record
    Street,City,Zip,Country, Tel: String;
    StreetNumber : Integer;
  end;
                    
var
  Data: PData;

begin
  // Do not use New.
  GetMem(Data,SizeOf(TData));
  Try
    { Initialize the structure in memory, using Run-Time Type Information}
    Initialize(Data^);
    { Assign some string data to the ansistring contents. 
      Note that this only works because the record was zeroed out by Initialize}
    Data^.Street:='Sesame Street';
    Data^.City:='Heaven';
    Data^.Zip:='7777777';
    Data^.Country:='Spain';
    Data^.StreetNumber:=3;
  Finally
    { Clean up the record contents.
      Again, the structure of the record is detected through 
      Run-time Type Information }
    Finalize(Data^);
    FreeMem(Data);
  end;
end.  