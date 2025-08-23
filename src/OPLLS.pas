{ 
  Author: CJ Hoad
  Date:   2025.08.17
  Info:
  Master program for starting the OPL language server.
}
program OPLLS;
{$mode objfpc}{$H+}
uses
  fpjson, jsonparser, sysutils, strutils, regexpr, pipes, 
  LSConfig, LSLogger;

var
  oLSConfig = TLSConfig;
  oLSLogger = TLSLogger;
  re: TRegExpr;
  inpStream: TInputPipeStream;
  outStream: TOutputPipeStream;
  jRequest, jResponse: TJSONData;
  sRequest, sResponse: String;


// strips off the leading linebreaks of a string
function stripLinebreaks(inputString: String) : String;
var
  returnString: String;
begin
  returnString := inputString.SubString(inputString.IndexOf('{'));
  Result := returnString;
end;

function requestHandler(lspRequest: String) : String;

var
  method, resultArgument: String;
  id, contentLength: Integer;

begin

  // initialise the resultArgument to empty string
  resultArgument := '';

  // get JSON portion of request
  jRequest := GetJSON(lspRequest);

  // parse JSON request to get the method
  method := jRequest.FindPath('method').AsString;

  // check the method and respond with the capabilities
  if method = 'initialize' then
    resultArgument := '"capabilities": {"completionProvider": {}}';
  
  if method = 'textDocument/completion' then
    resultArgument := '"isIncomplete": false, "items": [' +
    '{ "label": "PROC" },' +
    '{ "label": "ENDP" },' +
    '{ "label": "ELSE" },' +
    '{ "label": "ELSEIF" },' +
    '{ "label": "ENDIF" },' +
    '{ "label": "ENDV" },' +
    '{ "label": "ENDWH" }' +
    ']';

  if resultArgument <> '' then
  begin
    // get the id to respond to
    id := jRequest.FindPath('id').AsInteger;
    // generate the JSON response message
    sResponse := '{"jsonrpc": "2.0"' +
    ',"id": ' + IntToStr(id) + 
    ',"result": {' + resultArgument + '}}';

    // prettify output to print out in terminal
    jResponse := GetJSON(sResponse);
    //sResponse := jResponse.FormatJSON;

    // cleanup
    jRequest.Free;
    jResponse.Free;
  
    // prepare response message with content header
    contentLength := length(sResponse);
    sResponse := 'Content-Length: ' + 
                  IntToStr(contentLength) + #13#10#13#10 +
                  sResponse;
  end
  else
    sResponse := 'UNIDENTIFIED_METHOD';

  Result := sResponse;

end;

begin

  // initialise configuration file
  oLSConfig := TLSConfig.Create;
  // initialise log file
  oLSLogger := TLSLogger.Create(oLSConfig.GetConfigValue('logging-path'),
                                oLSConfig.GetConfigValue('logging-mode'))


  // initialise input and output pipes
  inpStream := TInputPipeStream.Create(StdInputHandle);
  outStream := TOutputPipeStream.Create(StdOutputHandle);

  // create log file
  AssignFile(tfLog, oLSConfig.GetConfigValue('logging-mode') +);
  rewrite(tfLog);
  CloseFile(tfLog);

  // initialise regex expression for Content-Length header
  re := TRegExpr.Create('Content-Length: ([0-9]+)');

  // set up continuous loop to ensure LS keeps checking for new LSP requests
  while 1=1 do
  begin

    // initialise requestLength (the length of the JSON portion of the LSP request message)
    sRequest := '';
    sResponse := '';

    // read request from input pipe, the first line of which should be the Content-Length header
    if inpStream.NumBytesAvailable > 0 then
    begin
        SetLength(sRequest, inpStream.NumBytesAvailable);
        inpStream.Read(sRequest[1], length(sRequest));
        logToFile(sRequest);
    end;

    // if request message matches Content-Length regex... 
    if re.Exec(sRequest) then
    begin
      // initialise response message variable to be passed into function for
      // parsing the JSON request and returning the LSP response message
      sRequest := sRequest.SubString(sRequest.IndexOf(re.Match[1])+Length(re.Match[1]));

      // check for leading line break characters and strip them off if they exist
      sRequest := stripLinebreaks(sRequest);

      // pass in the JSON component of the request to the function
      // for parsing and determining the LSP response message
      if sRequest <> '' then
      begin
        sResponse := requestHandler(sRequest);
        logToFile(sResponse);
      end;

      // send response to the output pipe
      if sResponse <> 'UNIDENTIFIED_METHOD' then
      begin
        logToFile(sResponse);
        outStream.Write(sResponse[1],Length(sResponse));
        sResponse := ''; // prevent infinite write to output pipe
      end;

    end

    // error handling in case first line of LSP request message is *not* a Content-Length header
    //else
      //logToFile('ERROR: Invalid Content-Length header!');

  end;
  logToFile('INFO: LSP stopped listening.');

end.