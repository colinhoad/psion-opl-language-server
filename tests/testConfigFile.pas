program testConfigFile;
{
  unit tests for the ConfigFile class
}
uses
  ConfigFile, sysutils;

var
  testTConfigFile: TConfigFile;
  testTLSConfigFile: TLSConfigFile;

begin
  
  writeln('START: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

  // test class TConfigFile constructor
  try
    writeln('Creating testTConfigFile object using overloaded TConfigFile constructor...');
    testTConfigFile := TConfigFile.Create('../../config/', 'config.ini');
    writeln('Get logging-mode value:');
    writeln(testTConfigFile.GetConfigValue('logging-mode'));
    writeln('Get logging-path value:');
    writeln(testTConfigFile.GetConfigValue('logging-path'));
    writeln('Get text-autocomplete value:');
    writeln(testTConfigFile.GetConfigValue('text-autocomplete'));
  except 
    on E: EFileNotFoundException do
      writeln('Unable to create object: ', E.Message);
  end;
  writeln('***********************');
  // test subclass TLSConfigFile constructor
  writeln('Creating testTLSConfigFile object using TLSConfigFile constructor...');
  try
    testTLSConfigFile := TLSConfigFile.Create('../../config/', 'config.ini');
    writeln('Get logging-mode value:');
    writeln(testTLSConfigFile.GetConfigValue('logging-mode'));
    writeln('Get logging-path value:');
    writeln(testTLSConfigFile.GetConfigValue('logging-path'));
    writeln('Get text-autocomplete value:');
    writeln(testTLSConfigFile.GetConfigValue('text-autocomplete'));
  except 
    on E: EFileNotFoundException do
      writeln('Unable to create object: ', E.Message);
    on E: ERangeError do
      writeln('Unable to create object: ', E.Message);
  end;
  writeln('***********************');

  writeln('END: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

end.