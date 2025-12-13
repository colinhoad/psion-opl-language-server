{*************************************************************}
{                                                             }
{  OPL Language Server program                                }
{                                                             }
{  (c) Colin Hoad, 2025                                       }
{                                                             }
{  Master program for running the OPL Language Server         }
{                                                             }
{*************************************************************}
program Oplls;
{$mode objfpc}{$H+}

uses
  SysUtils, StrUtils, RegExpr, Pipes, 
  Oplls.ConfigFile, Oplls.Logger, Oplls.Helpers;

const
  C_CONFIGPATH = 'config/';
  C_CONFIGFILE = 'config.ini';
  C_LOGPATH = 'logging-path';
  C_LOGMODE = 'logging-mode';
  C_LOGNAME = 'oplls';
  C_RXHEADER = 'Content-Length: ([0-9]+)';

var
  LsConfig: TLSConfigFile;
  LsLog: TLogger;
  ReContentHeader: TRegExpr;
  InputStream: TInputPipeStream;
  OutputStream: TOutputPipeStream;
  LsRequest, LsResponse: String;

procedure LsCheck;

  begin

    // initialise input and output pipe streams
    InputStream := TInputPipeStream.Create(StdInputHandle);
    OutputStream := TOutputPipeStream.Create(StdOutputHandle);

    // reset LsRequest and LsResponse to empty strings
    LsRequest := '';
    LsResponse := '';

    // read request from input pipe stream, the first line of which should be 
    // the Content-Length header
    if InputStream.NumBytesAvailable > 0 then
    begin
        SetLength(LsRequest, InputStream.NumBytesAvailable);
        InputStream.Read(LsRequest[1], Length(LsRequest));
        LsLog.WriteLine('VERBOSE', 'REQUEST: '+LsRequest);
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
        LsLog.WriteLine('STANDARD', 'RESPONSE: '+LsResponse);
        if LsResponse = 'SHUTDOWN' then
        begin
          LsLog.WriteLine('CONCISE', 'INFO: LSP stopped listening.');
          InputStream.Destroy;
          OutputStream.Destroy;
          Halt(0); // stops OPL LS
        end
      end;

      // send response to the output pipe
      if LsResponse <> 'UNIDENTIFIED_METHOD' then
      begin
        LsLog.WriteLine('STANDARD', 'RESPONSE: '+LsResponse);
        OutputStream.Write(LsResponse[1], Length(LsResponse));
        LsResponse := ''; // prevent infinite write to output pipe
      end;

    end

  end;

begin

  try

    // initialise configuration file
    try
      LsConfig := TLSConfigFile.Create(GetConfigPath(C_CONFIGPATH), C_CONFIGFILE);
    except 
      on E: EFileNotFoundException do
        WriteLn(StdErr, 'Unable to read OPL LS configuration file: ', E.Message);
    end;

    // initialise log file
    try
      LsLog := TLogger.Create(LsConfig.GetConfigValue(C_LOGMODE),
                              LsConfig.GetConfigValue(C_LOGPATH),
                              C_LOGNAME);
    except
      on E: EFileNotFoundException do
        WriteLn(StdErr, 'Unable to create OPL LS log file: ', E.Message);
    end;

    // define regex for matching Content-Length header
    ReContentHeader := TRegExpr.Create(C_RXHEADER);

    // call LsCheck to ensure the handshake between OPL LS and IDE takes place
    LsCheck;
    // now loop until EOF to ensure OPL LS keeps checking for new requests
    while not EOF do
    begin
      if InputStream.NumBytesAvailable > 0 then
        LsCheck
      else
        sleep(1); // ensures CPU doesn't get carried away
      end;

  finally
    LsLog.WriteLine('CONCISE', 'INFO: LSP stopped listening.');
  end;

end.