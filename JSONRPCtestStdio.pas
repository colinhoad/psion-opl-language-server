{ 
  Demo program similar to JSONRPCtest but which uses stdin and stdout rather than
  a file to exchange JSON-RPC messages. The program runs forever until a user 
  cancels it (CTRL+C) or a malformed JSON string is entered.
}
program JSONRPCtestStdio;

uses
  fpjson, jsonparser, SysUtils;

type 
  EEmptyString = Class(Exception);

function Subtract(var message: Text) : String;

var 
  jRequest, jResponse: TJSONData;
  sRequest, sResponse: String;
  minuend, subtrahend, difference, id: Integer;

begin

  // read .json file into string
  readln(message,sRequest);
  
  // create from string
  try

    if length(sRequest) = 0 then
      raise EEmptyString.create('ERROR: Empty string detected');

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

    // cleanup
    jRequest.Free;
    jResponse.Free;
  
  except
    on E: EJSONParser do
    begin
      sResponse := 'Invalid JSON string, please try again';
    end;
    on E: EEmptyString do
    begin
      sResponse := 'Empty string input, please try again'
    end;
  end;

  Result := sResponse;

end;

begin
  while not eof(input) do
  begin
    writeln(Subtract(input));
  end;
end.