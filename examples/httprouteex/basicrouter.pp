program BasicRouter;

{$mode objfpc}{$H+}

uses
  httproute, httpdefs;

procedure HandleHome(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'Welcome to the homepage!';
end;

procedure HandleAPI(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'API endpoint response';
end;

begin
  // Register routes
  HTTPRouter.RegisterRoute('/', rmGet, @HandleHome, False);
  HTTPRouter.RegisterRoute('/api', rmGet, @HandleAPI, False);

  Writeln('HTTP Router configured with routes:');
  Writeln('GET / - Homepage');
  Writeln('GET /api - API endpoint');
  Writeln('Router created successfully with ', HTTPRouter.RouteCount, ' routes');
end.