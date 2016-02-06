program example8;

{ This program demonstrates the SetToString function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;

Const
  ConstSet = 'mefirst,methird';

Var
  S : TMyEnums;

begin
  O:=TMyTestObject.Create;
  O.SetField:=[mefirst,meSecond,meThird];
  PI:=GetPropInfo(O,'SetField');
  Writeln('SetToString (brackets) : ',SetToString(Pi,Byte(O.SetField),True));
  Writeln('SetToString (default)  : ',SetToString(Pi,Byte(O.SetField)));
  O.SetField := TMyEnums(Byte(StringToSet(PI,ConstSet)));
  Writeln('StringToSet            : ',SetToString(PI,Byte(O.SetField)));
  O.Free;
end.