program tthrcs;
{$h+}
uses cthreads, sysutils, classes;

Var
  D : Integer;
  DoneThreads : INteger;
  
Procedure DoneThread(Sender : TObject; Data : Pointer);

begin
  Inc(DoneThreads);
  Writeln('Thread ',TThread(Sender).ThreadID,' done. D is currently: ', PInteger(Data)^);
end;

Procedure ReportThreadStatus(Sender : TThread; AData : Pointer;Const status : String);

begin
   Writeln('Thread ',Sender.ThreadID,' Status report : ',Status);
end;


Procedure DoThread(AData : Pointer; Report : TThreadReportStatus);

Var
  I : integer;

begin
  for I:=1 to 10 do
    begin
    Sleep(10*Random(30));
    Report('Ping '+IntToStr(i));
    Inc(PInteger(AData)^,i);
    end;
end;

Var
  T1,T2 : TThread;

begin
  DoneThreads:=0;
  T1:=TThread.ExecuteInThread(@DoThread,@ReportThreadStatus,@D,@DoneThread);
  T2:=TThread.ExecuteInThread(@DoThread,@ReportThreadStatus,@D,@DoneThread);
  Writeln('Main thread loop');
  While DoneThreads<2 do
    begin
    Sleep(10);
    CheckSynchronize;
    end;
  T1.WaitFor;
  T2.WaitFor;
end.