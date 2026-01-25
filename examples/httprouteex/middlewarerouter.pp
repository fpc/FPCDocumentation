program MiddlewareRouter;

{$mode objfpc}{$H+}

uses
  httproute, httpdefs, SysUtils;

type
  TMiddlewareHelper = class
  public
    procedure LoggingInterceptor(ARequest: TRequest; AResponse: TResponse; var aContinue: Boolean);
    procedure CORSInterceptor(ARequest: TRequest; AResponse: TResponse; var aContinue: Boolean);
  end;

procedure TMiddlewareHelper.LoggingInterceptor(ARequest: TRequest;
  AResponse: TResponse; var aContinue: Boolean);
begin
  Writeln(Format('[%s] %s %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    RouteMethodToString(THTTPRouter.StringToRouteMethod(ARequest.Method)),
    ARequest.PathInfo]));
  aContinue := True;
end;

procedure TMiddlewareHelper.CORSInterceptor(ARequest: TRequest;
  AResponse: TResponse; var aContinue: Boolean);
begin
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
  AResponse.SetCustomHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE');
  aContinue := True;
end;

procedure HandleAPI(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := '{"message": "API response with middleware"}';
  AResponse.ContentType := 'application/json';
end;

var
  Helper: TMiddlewareHelper;
begin
  Helper := TMiddlewareHelper.Create;
  try
    // Register interceptors (middleware)
    HTTPRouter.RegisterInterceptor('logging', @Helper.LoggingInterceptor, iaBefore);
    HTTPRouter.RegisterInterceptor('cors', @Helper.CORSInterceptor, iaAfter);

    // Register API route
    HTTPRouter.RegisterRoute('/api/data', rmGet, @HandleAPI, False);

    Writeln('Middleware router with logging and CORS support');
    Writeln('GET /api/data - Returns JSON with proper headers');
    Writeln('Router created with middleware interceptors and ', HTTPRouter.RouteCount, ' routes');
  finally
    Helper.Free;
  end;
end.
