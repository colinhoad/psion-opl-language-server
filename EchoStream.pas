{ 
  Demo program to play with TIOStream. 
}
program EchoStream;

uses
  iostream, classes, fpjson, jsonparser, SysUtils, regexpr;

var 
  re: TRegExpr;
  stdInput, stdOutput: TIOStream;
  stdMemory: TMemoryStream;
  sRequest, streamBuffer: String;
  countStream, contentLength: Integer;

begin

  contentLength := 0;
  stdMemory := TMemoryStream.Create;

  // read stdin
  readln(input,sRequest);

  // use regex to get content length value of request
  re := TRegExpr.Create('Content-Length: ([0-9]+)');
  if re.Exec(sRequest) then
  begin
    contentLength := StrToInt(re.Match[1]);
    stdInput := TIOStream.Create(iosInput);
    //stdInput.Read(streamBuffer, contentLength);
    SetLength(streamBuffer, contentLength);
    repeat
      countStream := stdInput.Read(streamBuffer[1], Length(streamBuffer));
      writeln('streamBuffer[1]: ' + streamBuffer[1]);
      writeln('countStream: ' + IntToStr(countStream));
      writeln('Length(streamBuffer): ' + IntToStr(Length(streamBuffer)));
      if countStream > 0 then stdMemory.Write(streamBuffer[1], countStream);
    until countStream = 0;
    stdOutput := TIOStream.Create(iosOutput);
    stdOutput.Write(streamBuffer,contentLength);
  end
  else
    writeln('Content-Length header missing.');

end.