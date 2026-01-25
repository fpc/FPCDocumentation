program ParameterRouter;

{$mode objfpc}{$H+}

uses
  httproute, httpdefs, Classes, SysUtils;

procedure HandleUser(ARequest: TRequest; AResponse: TResponse);
var
  Params: TStringList;
  UserID: String;
begin
  Params := TStringList.Create;
  try
    // Extract parameters from the route
    if HTTPRouter.GetHTTPRoute(ARequest.PathInfo, rmGet, Params) <> nil then
    begin
      if Params.Count > 0 then
        UserID := Params[0]
      else
        UserID := 'unknown';
      AResponse.Content := 'User ID: ' + UserID;
    end
    else
      AResponse.Content := 'No route found';
  finally
    Params.Free;
  end;
end;

procedure HandleUserPosts(ARequest: TRequest; AResponse: TResponse);
var
  Params: TStringList;
  UserID, PostID: String;
begin
  Params := TStringList.Create;
  try
    if HTTPRouter.GetHTTPRoute(ARequest.PathInfo, rmGet, Params) <> nil then
    begin
      UserID := 'unknown';
      PostID := 'unknown';
      if Params.Count > 0 then UserID := Params[0];
      if Params.Count > 1 then PostID := Params[1];
      AResponse.Content := Format('User %s, Post %s', [UserID, PostID]);
    end
    else
      AResponse.Content := 'No route found';
  finally
    Params.Free;
  end;
end;

begin
  HTTPRouter.RegisterRoute('/user/:id', rmGet, @HandleUser, False);
  HTTPRouter.RegisterRoute('/user/:uid/post/:pid', rmGet, @HandleUserPosts, False);

  Writeln('Parameter routing examples:');
  Writeln('GET /user/123 - Shows user 123');
  Writeln('GET /user/456/post/789 - Shows post 789 for user 456');
  Writeln('Router created with ', HTTPRouter.RouteCount, ' parameterized routes');
end.