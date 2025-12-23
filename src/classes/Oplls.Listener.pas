{*************************************************************}
{                                                             }
{  OPL LS Listener class                                      }
{                                                             }
{  (c) Colin Hoad, 2025                                       }
{                                                             }
{  Class file for creating an instance of the OPL LS listener }
{  which handles all requests and responses.                  }
{                                                             }
{*************************************************************}
unit Oplls.Listener;
{$mode objfpc}{$H+}

interface

uses
  Oplls.Logger, Oplls.ConfigFile, Pipes, RegExpr, SysUtils;


type
  TListener = class
  {
   base class for creating an object that represents the OPL LS listener
  }
    strict private
      const
        C_RXHEADER = 'Content-Length: ([0-9]+)';
      var
        ListenerLog: TLogger;
        ListenerConfig: TConfigFile;
        ReContentHeader: TRegExpr;
        InputStream: TInputPipeStream;
        OutputStream: TOutputPipeStream;
      function RequestHandler(Request: String) : String;
      function StripMessage(inputString: String) : String;

    public

      constructor Create(VAR LsLog: TLogger; VAR LsConfig: TLSConfigFile);
      procedure Listen;

  end;

implementation

uses
  StrUtils, fpjson, jsonparser;


// TListener base class methods

constructor TListener.Create(VAR LsLog: TLogger; VAR LsConfig: TLSConfigFile);
{
 constructor to create a new TListener object to manage LS requests and responses
}
begin
  // instance variables for the logger and config file objects
  ListenerLog := LsLog;
  ListenerConfig := LsConfig;
  // define regex for matching Content-Length header
  ReContentHeader := TRegExpr.Create(C_RXHEADER);
  // initialise input and output pipe streams
  InputStream := TInputPipeStream.Create(StdInputHandle);
  OutputStream := TOutputPipeStream.Create(StdOutputHandle);
  // write to log file to indicate OPL LS is ready
  ListenerLog.WriteLine('STANDARD', 'OPL LS initialised');
  // initiate handshake between OPL LS and the IDE
  Listen;
end;

procedure TListener.Listen;
{
 control method for the listener to parse requests and provide a response to well-formed requests 
}
  var
    LsRequest: String;
    LsResponse: String;

  begin

    // reset LsRequest and LsResponse to empty strings
    LsRequest := '';
    LsResponse := '';

    // read request from input pipe stream, the first line of which should be 
    // the Content-Length header
    if InputStream.NumBytesAvailable > 0 then
    begin
        ListenerLog.WriteLine('VERBOSE','INFO: Bytes available to read = '  + IntToStr(InputStream.NumBytesAvailable));
        SetLength(LsRequest, InputStream.NumBytesAvailable);
        InputStream.Read(LsRequest[1], Length(LsRequest));
        ListenerLog.WriteLine('VERBOSE', 'REQUEST: '+LsRequest);
    end;

    // if request message matches Content-Length regex... 
    if ReContentHeader.Exec(LsRequest) then
    begin
      // initialise response message variable to be passed into function for
      // parsing the JSON request and returning the LSP response message
      LsRequest := LsRequest.SubString(LsRequest.IndexOf(
                                                  ReContentHeader.Match[1]
                                                  ) + 
                                        Length(ReContentHeader.Match[1])
                                      );

      // check for leading line break characters and strip them off if they exist
      LsRequest := StripMessage(LsRequest);

      // pass in the JSON component of the request to the function
      // for parsing and determining the LSP response message
      if LsRequest <> '' then
      begin
        LsResponse := RequestHandler(LsRequest);
        if LsResponse = 'SHUTDOWN' then
        begin
          ListenerLog.WriteLine('CONCISE', 'INFO: LSP stopped listening.');
          InputStream.Destroy;
          OutputStream.Destroy;
          Halt(0); // stops OPL LS
        end
      end;

      // send response to the output pipe
      if LsResponse <> 'UNIDENTIFIED_METHOD' then
      begin
        ListenerLog.WriteLine('STANDARD', 'RESPONSE: '+LsResponse);
        OutputStream.Write(LsResponse[1], Length(LsResponse));
        LsResponse := ''; // prevent infinite write to output pipe
      end
      else
        ListenerLog.WriteLine('VERBOSE', 'INFO: Requested method not supported by OPL LS');

    end

  end;

function TListener.RequestHandler(Request: String) : String;
{
 identifies the method from the incoming request message and
 generates an appropriate response message for that method
}
var
  Method, ResultArgument, Response: String;
  Id, ContentLength: Integer;
  JsonRequest: TJSONData;

begin

  // initialise the resultArgument to empty string
  ResultArgument := '';

  // get JSON portion of request
  JsonRequest := GetJSON(Request);

  // parse JSON request to get the method
  Method := JsonRequest.FindPath('method').AsString;

  // check the method and respond with the capabilities
  if Method = 'initialize' then
    ResultArgument := '"capabilities": {"completionProvider": {}}';
  
  if Method = 'textDocument/completion' then
    ResultArgument := '"isIncomplete": false, "items": [' +
    '{ "label": "PROC" },' +
    '{ "label": "ENDP" },' +
    '{ "label": "ELSE" },' +
    '{ "label": "ELSEIF" },' +
    '{ "label": "ENDIF" },' +
    '{ "label": "ENDV" },' +
    '{ "label": "ENDWH" }' +
    ']';

  if ResultArgument <> '' then
  begin
    // get the id to respond to
    Id := JsonRequest.FindPath('id').AsInteger;
    // generate the JSON response message
    Response := '{"jsonrpc": "2.0"' +
    ',"id": ' + IntToStr(Id) + 
    ',"result": {' + ResultArgument + '}}';

    // cleanup
    JsonRequest.Free;
  
    // prepare response message with content header
    ContentLength := Length(Response);
    Response := 'Content-Length: ' + 
                IntToStr(ContentLength) + #13#10#13#10 +
                Response;
  end
  else
    // handle shutdown requests
    if Method = 'shutdown' then
      Response := 'SHUTDOWN'
    // indicate when a method is not currently handled by OPL LS
    else
      Response := 'UNIDENTIFIED_METHOD';

  Result := Response;

end;

function TListener.StripMessage(inputString: String) : String;
{
 strips off any leading characters (e.g. line breaks) from
 a request message
}
var
  returnString: String;
begin
  returnString := inputString.SubString(inputString.IndexOf('{'));
  Result := returnString;
end;

end.