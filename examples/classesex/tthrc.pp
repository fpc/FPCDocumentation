program tthrc;

uses cthreads, sysutils, classes;

Var
  D : Integer;

Procedure DoneThread(Sender : TObject; AData : Pointer);

begin
  Writeln('Thread ',TThread(Sender).ThreadID,' done. D is currently: ', PInteger(AData)^);
end;

Procedure DoThread(AData : Pointer);

Var
  I : integer;

begin
  for I:=1 to 10 do
    begin
    Sleep(10*Random(30));
    Writeln('Thread ',TThread.CurrentThread.ThreadID,' ping ',I);
    Inc(PInteger(AData)^,i);
    end;
end;

Var
  T1,T2 : TThread;

begin
  T1:=TThread.ExecuteInThread(@DoThread,@D,@DoneThread);
  T2:=TThread.ExecuteInThread(@DoThread,@D,@DoneThread);
  Writeln('Main thread done');
  T1.WaitFor;
  T2.WaitFor;
end.