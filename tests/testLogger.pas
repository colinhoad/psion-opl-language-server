program testLogger;
{
  unit tests for the Logger class
}
uses
  Logger, sysutils;

var
  testTLogger: TLogger;

begin
  
  writeln('START: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

  // test default constructor
  writeln('Creating testTLogger object using default TLogger constructor...');
  testTLogger := TLogger.Create('VERBOSE', '/tmp/');
  writeln('Checking mode of testTLogger object...');
  writeln(testTLogger.GetMode);
  writeln('Checking path of testTLogger object...');
  writeln(testTLogger.GetFullPath);
  writeln('Writing message to testTLogger object...');
  testTLogger.WriteMsg('Test');

  writeln('END: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

end.