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
  // We use the fact that a pointer is also usable as an array.
  GetMem(Data,SizeOf(TData)*2);
  Try
    { Initialize the structure in memory, using Run-Time Type Information}
    Initialize(Data^,2);
    { Assign some string data to the ansistring contents. 
      Note that this only works because the record was zeroed out by Initialize}
    Data[0].Street:='Sesame Street';
    Data[0].City:='Heaven';
    Data[0].Zip:='7777777';
    Data[0].Country:='Spain';
    Data[0].StreetNumber:=3;
    // Second, well known street
    Data[1].Street:='Wall Street';
    Data[1].City:='New York';
    Data[1].Zip:='10005';
    Data[1].Country:='USA';
    Data[1].StreetNumber:=11;
  Finally
    { Clean up the record contents.
      Again, the structure of the record is detected through 
      Run-time Type Information }
    Finalize(Data^);
    FreeMem(Data);
  end;
end.  