{ 
  Demo program to test the theory of passing in a text file and getting a response that simply 
  writes out each line of the text file with its line number and a content length header that  
  gives the total length in characters of that text file. 
}
program Response;

uses
  SysUtils;

procedure Response(var message: Text);

var 
  s, r: String;
  lineNum: Int64;
  contentLength: Int64;

begin
  lineNum := 0;
  contentLength := 0;
  r := '';
  while not eof(message) do 
  begin
    lineNum := lineNum + 1;
    readln(message,s);
    contentLength := contentLength + Length(s);
    r := r + IntToStr(lineNum) + ': ' + s + #13#10;
  end;
  r := 'Content Length: ' + IntToStr(contentLength) + #13#10 + r;
  writeln(r);
end;

begin

  Response(input);

end.