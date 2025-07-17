Program server;

{$mode objfpc}
{$h+}

{
  Program to test Sockets unit by Michael van Canneyt and Peter Vreman
  Server Version, First Run sock_svr to let it create a socket and then
  sock_cli to connect to that socket
}
uses Sockets;

Var
  FromName : string;
  Buffer   : string;
  S,fd    : Longint;
  len : longword;
  SAddr,SRemote : TInetSockAddr;

procedure perror (const Msg:string);
begin
  writeln (Msg,': ',SocketError);
  halt(100);
end;

procedure send(const msg : string);

begin
  fpSend(fd,@msg[1],length(msg),0);
end;

function Receive(var S : string) : boolean;

var
  nRead : integer;

begin
  SetLength(S,255);
  nRead:=fpRecv(fd,@S[1],255,0);
  Result:=nRead>0;
  if Result then
    SetLength(S,nRead)
  else
    SetLength(S,0);  
end;


begin
  S:=fpSocket (AF_INET,SOCK_STREAM,0);
  if SocketError<>0 then
   Perror ('Server : Socket : ');
  SAddr.sin_family:=AF_INET;
  { port 5000 in network order }
  SAddr.sin_port:=htons(5000);
  SAddr.sin_addr.s_addr:=0;
  if fpBind(S,@SAddr,sizeof(saddr))=-1 then
   PError ('Server : Bind : ');
  if fpListen (S,1)=-1 then
   PError ('Server : Listen : ');
  Writeln('Waiting for Connect from Client, run now sock_cli in an other tty');
  len:=sizeof(SRemote);
  fd:=fpAccept(S, @SRemote,@len);
  if fd=-1 then
    PError ('Server : Accepting connection')
  else
    begin 
    Send('Message From Server');
    while Receive(Buffer) do
      begin
      Writeln('Server : read : ',Buffer);
      end;
    fpShutdown(fd,2);
    end;  
  CloseSocket(s);
end.
