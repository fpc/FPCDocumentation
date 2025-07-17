Program Client;

{
  Program to test Sockets unit by Michael van Canneyt and Peter Vreman
  Client Version, First Run sock_svr to let it create a socket and then
  sock_cli to connect to that socket
}
{$mode objfpc}
{$h+}
uses Sockets;

procedure PError(const S : string);
begin
  writeln(S,SocketError);
  halt(100);
end;


Var
  SAddr    : TInetSockAddr;
  Buffer   : string;
  fd,err        : Longint;
  i        : integer;
  
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
  fd:=fpSocket (AF_INET,SOCK_STREAM,0);
  if fd=-1 then
   Perror('Client : Socket : ');
  SAddr.sin_family:=AF_INET;
  { port 50000 in network order }
  SAddr.sin_port:=htons(5000);
  { localhost : 127.0.0.1 in network order }
  SAddr.sin_addr.s_addr:=HostToNet((127 shl 24) or 1);
  err:=fpConnect(fd,@SAddr,Sizeof(SAddr));
  if err=-1 then
   PError('Client : Connect : ')
  else  
    begin
    if Receive(Buffer) then
       WriteLn(Buffer);
    Buffer:='This is a textstring sent by the Client.';
    Send(Buffer);
    end;   
  CloseSocket(fd);
end.
