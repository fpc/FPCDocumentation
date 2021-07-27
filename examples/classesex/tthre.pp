program tthre;

{$mode objfpc}
{$H+}

uses cthreads, sysutils, classes;

Type
  TTestThread = Class(TObject)
    D : Integer;
    Procedure DoneThread(Sender : TObject);
    Procedure DoThread;
    Procedure Run;
  end;
  



Procedure TTestThread.DoneThread(Sender : TObject);

begin
  Writeln('Thread ',TThread(Sender).ThreadID,' done. D is currently: ', D);
end;

Procedure TTestThread.DoThread;

Var
  I : integer;

begin
  for I:=1 to 10 do
    begin
    Sleep(10*Random(30));
    Writeln('Thread ',TThread.CurrentThread.ThreadID,' ping ',I);
    Inc(D,i);
    end;
end;

Procedure TTestThread.Run;


Var
  T1,T2 : TThread;

begin
  T1:=TThread.ExecuteInThread(@DoThread,@DoneThread);
  T2:=TThread.ExecuteInThread(@DoThread,@DoneThread);
  Writeln('Main thread done');
  T1.WaitFor;
  T2.WaitFor;
end;

begin
  With TTestThread.Create do
    try
      Run;
    finally
      Free;
    end;  
end.