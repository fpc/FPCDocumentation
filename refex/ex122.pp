Program Example122;

{ Program to demonstrate the BitSizeOf function. }

type
  TB = bitpacked record
    B1,B2,B3,B4 :   Boolean;
  end;  
  
  TNB = record
    B1,B2,B3,B4 :   Boolean;
  end;  

Var
  I : Longint;
  S : String [10];
  

begin
  Writeln (BitSizeOf(I));    { Prints 32  }
  Writeln (BitSizeOf(S));    { Prints 88 }
  Writeln (BitSizeOf(TB));   { Prints 8 }
  Writeln (BitSizeOf(TNB));  { Prints 32 }
end.
