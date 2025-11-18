Program Example68;

{ This program demonstrates the FloatToStrF function }

Uses sysutils;

Const Fmt : Array [TFloatFormat] of string[10] =
         ('general','exponent','fixed','number','Currency');

Procedure Testit (Value :  Extended);

Var I,J : longint;
    FF : TFloatFormat;

begin
  For I:=5 to 15 do
    For J:=1 to 4 do
      For FF:=ffGeneral to {ffCurrency} ffGeneral do
        begin
        Write (Value,'(Prec: ',I:2,', Dig: ',J,', fmt : ',Fmt[ff],') : ');
        Writeln (FloatToStrF(Value,FF,I,J));
        Write (-Value,'(Prec: ',I:2,', Dig: ',J,', fmt : ',Fmt[ff],') : ');
        Writeln (FloatToStrF(-Value,FF,I,J));
        end;
end;

procedure Test2;

var
  F : Double;

begin
  F:=0.99411;
  Writeln('0,0: ',floattostrF(F,FFgeneral,0,0));//=> 0.99,
  Writeln('1,0: ',floattostrF(F,FFgeneral,1,0));//=> 0.99
  Writeln('2,0: ',floattostrF(F,FFgeneral,2,0));//=> 0.99
  Writeln('3,0: ',floattostrF(F,FFgeneral,3,0));//=> 0.994
  F:=0.99511;
  Writeln('0,0: ',floattostrF(F,FFgeneral,0,0));//=> 1
  Writeln('1,0: ',floattostrF(F,FFgeneral,1,0));//=> 1
  Writeln('2,0: ',floattostrF(F,FFgeneral,2,0));//=> 1
  Writeln('3,0: ',floattostrF(F,FFgeneral,3,0));//=> 0.995
  F:=12.145;
  Writeln('0,0: ',floattostrF(F,FFgeneral,0,0));//=> 12
  Writeln('1,0: ',floattostrF(F,FFgeneral,1,0));//=> 12
  Writeln('2,0: ',floattostrF(F,FFgeneral,2,0));//=> 12
  Writeln('3,0: ',floattostrF(F,FFgeneral,3,0));//=> 12.1
  Writeln('4,0: ',floattostrF(F,FFgeneral,4,0));//=> 12.15
  Writeln('5,0: ',floattostrF(F,FFgeneral,5,0));//=> 12.145
end;

Begin
  Testit (1.1));
  Testit (1.1E1));
  Testit (1.1E-1));
  Testit (1.1E5));
  Testit (1.1E-5));
  Testit (1.1E10));
  Testit (1.1E-10));
  Testit (1.1E15));
  Testit (1.1E-15));
  Testit (1.1E100));
  Testit (1.1E-100));
End.
