{ 
  Demo program to test the theory of how JSON-RPC 2.0 request and response should work.
  Simple example based on the Wikipedia entry for JSON-RPC where the method is 
  'subtract' and the parameters in the request are a minuend and subtrahend with a
  result of the subtract method provided in the response.
}
program JSONRPCtest;

uses
  fpjson, jsonparser, SysUtils;

procedure Subtract(var message: Text);

var 
  jRequest, jResponse: TJSONData;
  sRequest, sResponse, sLine: String;
  minuend, subtrahend, difference, id: Integer;

begin

  // read .json file into string
  sRequest := '';
  while not eof(message) do 
  begin
    readln(message,sLine);
    sRequest := sRequest + sLine;
  end;

  // create from string
  jRequest := GetJSON(sRequest);

  // parse JSON request to get the id, minuend and subtrahend
  id := jRequest.FindPath('id').AsInteger;
  minuend := jRequest.FindPath('params.minuend').AsInteger;
  subtrahend := jRequest.FindPath('params.subtrahend').AsInteger;

  // carry out subtraction operation
  difference := minuend - subtrahend;

  // generate the JSON response message
  sResponse := '{"jsonrpc": "2.0"' +
               ',"result": ' + IntToStr(difference) +
               ',"id": ' + IntToStr(id) + '}';
  jResponse := GetJSON(sResponse);
  
  // prettify output to print out in terminal
  sResponse := jResponse.FormatJSON;
  writeln(sResponse);

  // cleanup
  jRequest.Free;
  jResponse.Free;

end;

begin

  Subtract(input);

end.