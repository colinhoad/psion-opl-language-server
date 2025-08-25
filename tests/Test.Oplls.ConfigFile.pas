program Test.Oplls.ConfigFile;
{
  unit tests for the Oplls.ConfigFile class
}
uses
  Oplls.ConfigFile, SysUtils;

var
  TestTConfigFile: TConfigFile;
  TestTLSConfigFile: TLSConfigFile;

begin
  
  WriteLn('*** UNIT TEST: Oplls.ConfigFile ***');
  WriteLn('***********************************');
  WriteLn('*** START: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

  // test class TConfigFile constructor
  try
    WriteLn('Creating TestTConfigFile object using overloaded TConfigFile constructor...');
    TestTConfigFile := TConfigFile.Create('../../config/', 'config.ini');
    WriteLn('Get logging-mode value:');
    WriteLn(TestTConfigFile.GetConfigValue('logging-mode'));
    WriteLn('Get logging-path value:');
    WriteLn(TestTConfigFile.GetConfigValue('logging-path'));
    WriteLn('Get text-autocomplete value:');
    WriteLn(TestTConfigFile.GetConfigValue('text-autocomplete'));
  except 
    on E: EFileNotFoundException do
      WriteLn(StdErr, 'Unable to create object: ', E.Message);
  end;
  WriteLn('****************************');
  // test subclass TLSConfigFile constructor
  WriteLn('Creating TestTLSConfigFile object using TLSConfigFile constructor...');
  try
    TestTLSConfigFile := TLSConfigFile.Create('../../config/', 'config.ini');
    WriteLn('Get logging-mode value:');
    WriteLn(TestTLSConfigFile.GetConfigValue('logging-mode'));
    WriteLn('Get logging-path value:');
    WriteLn(TestTLSConfigFile.GetConfigValue('logging-path'));
    WriteLn('Get text-autocomplete value:');
    WriteLn(TestTLSConfigFile.GetConfigValue('text-autocomplete'));
  except 
    on E: EFileNotFoundException do
      WriteLn(StdErr, 'Unable to create object: ', E.Message);
    on E: ERangeError do
      WriteLn(StdErr, 'Unable to create object: ', E.Message);
  end;
  WriteLn('****************************');

  WriteLn('*** END: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

end.