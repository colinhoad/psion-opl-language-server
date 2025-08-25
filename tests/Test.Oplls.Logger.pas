program Test.Oplls.Logger;
{
  unit tests for the Oplls.Logger class
}
uses
  Oplls.Logger, SysUtils, Classes;

var
  TestTLogger: TLogger;

begin

  WriteLn('*** UNIT TEST: Oplls.Logger ***');
  WriteLn('*******************************');
  WriteLn('*** START: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

  // test default constructor
  WriteLn('Creating TestTLogger object using default TLogger constructor...');
  TestTLogger := TLogger.Create('STANDARD', '/tmp/', 'oplls');
  WriteLn('Checking mode of TestTLogger object...');
  WriteLn(TestTLogger.GetMode);
  WriteLn('Checking path of TestTLogger object...');
  WriteLn(TestTLogger.GetFullPath);
  WriteLn('Writing line to TestTLogger object...');
  TestTLogger.WriteLine('VERBOSE', 'Test Line 1');
  TestTLogger.WriteLine('CONCISE', 'Test Line 2');
  TestTLogger.WriteLine('STANDARD', 'Test Line 3');
  TestTLogger.WriteLine('VERBOSE', 'Test Line 4');
  TestTLogger.WriteLine('CONCISE', 'Test Line 5');
  WriteLn('Reading line from TestTLogger object...');
  try
    WriteLn(TestTLogger.ReadLine(6));
  except
    on E: EStringListError do
      WriteLn(StdErr, 'ERROR: Unable to read log: ', E.Message);
    on E: EArgumentException do
      WriteLn(StdErr, 'ERROR: Unable to execute: ', E.Message);
  end;
  WriteLn('****************************');

  WriteLn('*** END: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

end.