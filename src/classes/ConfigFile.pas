unit ConfigFile;
{$mode objfpc}{$H+}
interface

uses
  contnrs, sysutils, strutils, regexpr;

type
  TConfigFile = class
  {
   base class for creating an object that represents the values in config.ini
  }
    private

      type TConfigKeyValue = record 
        Key, Value: String;
      end;
      const
        RX = '^(\s+|)(\S+)(\s+|)(:)(\s+|)("|)([a-zA-Z0-9_\\/\-\s]+)("|)(\s+|)$';
      var
        Path: String;
        Filename: String;
        FullPath: String;
        ConfigFile: TextFile;
        ConfigTable: TFPStringHashTable;

      function EntryDecoder(inputString: String) : TConfigKeyValue;
      procedure ReadConfigData;

    public
      constructor Create(sPath: String; sFilename: String);
      function GetConfigValue(key: String) : String;

  end;
  
  TLSConfigFile = class(TConfigFile)
  {
   subclass for creating and validating a specific LS config file
  }

    const
      LOG_MODES: array [1..4] of string =
        ('CONCISE', 'SILENT', 'STANDARD', 'VERBOSE');

    private
      function ValidateLoggingMode : Boolean;

    public
      constructor Create(sPath: String; sFilename: String);
      
  end;

implementation

// TConfigFile base class methods

constructor TConfigFile.Create(sPath: String; sFilename: String);
{
 constructor to create a new TConfigFile object using a custom config.ini file
}
begin
  Path := sPath;
  Filename := sFilename;
  FullPath := Path + Filename;

  // initialise config file
  if FileExists(FullPath) then
  begin
    AssignFile(ConfigFile, FullPath);
  end
  else
    raise EFileNotFoundException.Create('No config file found at ' + FullPath);

  // read data from config file into hash table
  ReadConfigData;
  CloseFile(ConfigFile);

end;

function TConfigFile.EntryDecoder(inputString: String) : TConfigKeyValue;
{
 reads a line from config.ini and returns a record of the key and its value
}
var
  re: TRegExpr;
begin
  // regex to obtain the key and value assuming a non-quoted string (the key)
  // followed by a colon separator and a quoted string (the value). The key
  // is match group 2 and the value is match group 7. The regex is sufficiently 
  // relaxed to allow one or more whitespace characters before and after the 
  // key as well as after the colon and before and after the value. Failure to 
  // match the regex will result in an exception being thrown due to the line 
  // in config.ini not being a syntactically valid key:value pair.
  re := TRegExpr.Create(RX);
  if re.Exec(inputString) then
  begin
    Result.Key := re.Match[2];
    Result.Value := re.Match[7];
  end
  else
    raise ENotSupportedException.Create('Unable to parse key:value pair [' +
    inputString + '] in config file');
 end;

procedure TConfigFile.ReadConfigData;
{
 read the contents of the config.ini file into the ConfigTable hash
}
var
  s: String;
  kv: TConfigKeyValue;
begin
  try
    begin
      // initialise hash list
      ConfigTable := TFPStringHashTable.Create;
      // open the file for reading
      reset(ConfigFile);
      // keep reading lines until the end of the file is reached
      while not eof(ConfigFile) do
      begin
        readln(ConfigFile, s);
        // ignore comment lines and empty lines
        if (Length(s) > 0) then
        begin
          if (s[1] <> '#') then
          begin
            kv := EntryDecoder(s);
            ConfigTable.Add(kv.Key, kv.Value);
          end;
        end;
      end;
    end;
  except
    raise EInOutError.Create('File handling error occurred during read of config ' +
    'file at ' + FullPath);
  end;
end;

function TConfigFile.GetConfigValue(key: String) : String;
{
 return the config value based on its key
}
var
  s: String;
begin
  s := ConfigTable.Items[key];
  if s = '' then
    raise ERangeError.Create('No such key [' + key + '] found in configuration data')
  else
    Result := s;
end;

// TLSConfigFile subclass methods

constructor TLSConfigFile.Create(sPath: String; sFilename: String);
begin
  inherited Create(sPath, sFilename);
  if not ValidateLoggingMode then
    raise ERangeError.Create('Config entry for logging-mode is invalid');
end;

function TLSConfigFile.ValidateLoggingMode : Boolean;
{
 validates logging-mode entry 
}
var
  sTest: String;
  bResult: Boolean;
  i: Integer;
begin

  // initialise to False
  bResult := False;

  // loop through constant string array LOG_MODES until config entry 
  // for logging-mode matches the value in the config file
  sTest := GetConfigValue('logging-mode');
  i := 0;
  repeat
  begin
    i := i + 1;
    if sTest = LOG_MODES[i] then
      bResult := True;
  end;
  until (i = Length(LOG_MODES)) or (bResult = True);
  Result := bResult;
end;

end.