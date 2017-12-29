Program Example51;

{ 
  Program to demonstrate the Variance function.
  It demonstrates the absence of large errors in the calculation.
}

Uses math;

const 
  Size = 1000000;
  
var 
  dataS: array of Single;
  dataD: array of Double;
  dataE: array of Extended;
  i,n: longint;
  
begin
  WriteLn('Each run should return a value near unity.');
  WriteLn('Single:');
  SetLength( dataS, Size );
  for n := 1 to 4 do
  begin
    for i := 0 to Size - 1 do
    begin
      dataS[i] := 10000000 + RandG(0,1);
    end;
    WriteLn( Math.Variance( dataS ):5:3 );
  end;

  WriteLn('Double:');
  SetLength( dataD, Size );
  for n := 1 to 4 do
  begin
    for i := 0 to Size - 1 do
    begin
      dataD[i] := 1000000000000000 + RandG(0,1);
    end;
    WriteLn( Math.Variance( dataD ):5:3 );
  end;

  WriteLn('Extended:');
  SetLength( dataE, Size );
  for n := 1 to 4 do
  begin
    for i := 0 to Size - 1 do
    begin
      dataE[i] := 1000000000000000000 + RandG(0,1);
    end;
    WriteLn( Math.Variance( dataE ):5:3 );
  end;
end.
