program MethodRouter;

{$mode objfpc}{$H+}

uses
  httproute, httpdefs;

procedure HandleGetUsers(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := '{"users": ["alice", "bob", "charlie"]}';
  AResponse.ContentType := 'application/json';
end;

procedure HandleCreateUser(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := '{"status": "user created", "id": 123}';
  AResponse.ContentType := 'application/json';
  AResponse.Code := 201;
end;

procedure HandleUpdateUser(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := '{"status": "user updated"}';
  AResponse.ContentType := 'application/json';
end;

procedure HandleDeleteUser(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Code := 204; // No Content
end;

begin
  HTTPRouter.RegisterRoute('/users', rmGet, @HandleGetUsers, False);
  HTTPRouter.RegisterRoute('/users', rmPost, @HandleCreateUser, False);
  HTTPRouter.RegisterRoute('/users/:id', rmPut, @HandleUpdateUser, False);
  HTTPRouter.RegisterRoute('/users/:id', rmDelete, @HandleDeleteUser, False);

  Writeln('RESTful API routing:');
  Writeln('GET /users - List users');
  Writeln('POST /users - Create user');
  Writeln('PUT /users/:id - Update user');
  Writeln('DELETE /users/:id - Delete user');
  Writeln('Router created with ', HTTPRouter.RouteCount, ' RESTful routes');
end.