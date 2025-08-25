{*****************************************************************}
{                                                                 }
{  OPL LS Helpers unit                                            }
{                                                                 }
{  (c) Colin Hoad, 2025                                           }
{                                                                 }
{  Unit file comprising helper functions, types, records and      }
{  other useful components that do not belong to any particular   }
{  class                                                          } 
{                                                                 }
{*****************************************************************}
unit Oplls.Helpers;
{$mode objfpc}{$H+}

interface

function GetConfigPath(RelativePath: String) : String;
function StripMessage(inputString: String) : String;
function RequestHandler(Request: String) : String;

implementation

uses
  fpjson, jsonparser, SysUtils;

function GetConfigPath(RelativePath: String) : String;
{
 gets the absolute path to the configuration file based on the
 location of the OPL LS binary executable and a path relative
 to that executable's location
}
begin
  Result := ExtractFilePath(ParamStr(0)) + RelativePath;
end;

function StripMessage(inputString: String) : String;
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

function RequestHandler(Request: String) : String;
{
 identifies the method from the incoming request message and
 generates an appropriate response message for that method

 TO DO: Convert to class!
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
    Response := 'UNIDENTIFIED_METHOD';

  Result := Response;

end;

end.