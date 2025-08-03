{ 
  Demo program to stand up a simple HTTP web server. Not actually needed for LSP. 
}
program SimpleWebServer;

uses
  fphttpapp, httpdefs, httproute;

procedure DoHello(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := '<html><head><title>Simple Web Server</title></head>'+
  '<body><h1>Hello,World!</h1></body></html>'
end;

begin
  HTTPRouter.RegisterRoute('*', @DoHello);
  Application.Port := 9000;
  Application.Initialize;
  Application.Run;
end.