program tthrc;

{$mode objfpc}
{$H+}

uses cthreads, sysutils, classes;

Type
  TTestThread = Class(TObject)
    D : Integer;
    DoneThreads : integer;
    Procedure DoneThread(Sender : TObject);
    Procedure ReportThreadStatus(Sender : TThread; Const status : String);
    Procedure DoThread(Report: TThreadReportStatus);
    Procedure Run;
  end;


Procedure TTestThread.DoneThread(Sender : TObject);

begin
  Inc(DoneThreads);
  Writeln('Thread ',TThread(Sender).ThreadID,' done. D is currently: ', D);
end;

Procedure TTestThread.ReportThreadStatus(Sender : TThread; Const status : String);

begin
   Writeln('Thread ',Sender.ThreadID,' Status report : ',Status);
end;

Procedure TTestThread.DoThread(Report : TThreadReportStatus);

Var
  I : integer;

begin
  for I:=1 to 10 do
    begin
    Sleep(10*Random(30));
    Report('Ping '+IntToStr(i));
    Inc(D,i);
    end;
end;

Procedure TTestThread.Run;


Var
  T1,T2 : TThread;

begin
  DoneThreads:=0;
  T1:=TThread.ExecuteInThread(@DoThread,@ReportThreadStatus,@DoneThread);
  T2:=TThread.ExecuteInThread(@DoThread,@ReportThreadStatus,@DoneThread);
  Writeln('Main thread loop');
  While DoneThreads<2 do
    begin
    Sleep(10);
    CheckSynchronize;
    end;
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