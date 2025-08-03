{ 
  Demo program to simply echo stdin to stdout. 
}
program EchoStdio;
var s :string;
begin
  while not eof(input) do 
  begin
    readln(input,s);
    s := 'Echo: '+s;
    writeln(s);
  end;
end.