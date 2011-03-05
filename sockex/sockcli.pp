Program Client;

{
  Program to test Sockets unit by Michael van Canneyt and Peter Vreman
  Client Version, First Run sock_svr to let it create a socket and then
  sock_cli to connect to that socket
}

uses Sockets;

procedure PError(const S : string);
begin
  writeln(S,SocketError);
  halt(100);
end;


Var
  SAddr    : TInetSockAddr;
  Buffer   : string [255];
  S        : Longint;
  Sin,Sout : Text;
  i        : integer;

begin
  S:=fpSocket (AF_INET,SOCK_STREAM,0);
  if s=-1 then
   Perror('Client : Socket : ');
  SAddr.sin_family:=AF_INET;
  { port 50000 in network order }
  SAddr.sin_port:=htons(5000);
  { localhost : 127.0.0.1 in network order }
  SAddr.sin_addr.s_addr:=HostToNet((127 shl 24) or 1);
  if not Connect (S,SAddr,Sin,Sout) then
   PError('Client : Connect : ');
  Reset(Sin);
  ReWrite(Sout);
  Buffer:='This is a textstring sent by the Client.';
  for i:=1 to 10 do
    Writeln(Sout,Buffer);
  Flush(Sout);
  Readln(SIn,Buffer);
  WriteLn(Buffer);
  Close(sout);
end.
