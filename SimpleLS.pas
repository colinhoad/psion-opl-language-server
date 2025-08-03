{ 
  Demo language server using the Bash example from Jeff Chupp. This just exchanges
  a simple initialization handshake. Only works if entire request is on a single line.
}
program SimpleLS;

uses
  fpjson, jsonparser, SysUtils, regexpr;

function Response(var message: Text) : String;

var 
  re: TRegExpr;
  jRequest, jResponse: TJSONData;
  sRequest, sResponse, method, params: String;
  id, contentLength: Integer;

begin

  // read stdin
  readln(message,sRequest);

  // use regex to get content length value of request
  re := TRegExpr.Create('Content-Length: ([0-9]+)');
  if re.Exec(sRequest) then
    contentLength := StrToInt(re.Match[1]);

  // get JSON portion of request by starting from end of 
  // content length header
  jRequest := GetJSON(copy(sRequest,(length(IntToStr(contentLength))+17),contentLength));

  // parse JSON request to get the id, minuend and subtrahend
  id := jRequest.FindPath('id').AsInteger;
  method := jRequest.FindPath('method').AsString;

  // generate the JSON response message
  sResponse := '{"jsonrpc": "2.0"' +
               ',"id": ' + IntToStr(id) + 
               ',"result": {"capabilities": {}' + '}' + '}';
  // prettify output to print out in terminal
  jResponse := GetJSON(sResponse);
  sResponse := jResponse.FormatJSON;

  // cleanup
  jRequest.Free;
  jResponse.Free;
  
  // prepare response message with content header
  contentLength := length(sResponse);
  sResponse := 'Content-Length: ' + IntToStr(contentLength) + sResponse;

  Result := sResponse;

end;

begin
  //while not eof(input) do
  writeln(Response(input));
end.