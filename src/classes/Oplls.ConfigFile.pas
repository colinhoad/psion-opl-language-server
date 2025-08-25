{*************************************************************}
{                                                             }
{  OPL LS Config File class                                   }
{                                                             }
{  (c) Colin Hoad, 2025                                       }
{                                                             }
{  Class file for reading key:value pairs from the config.ini }
{  configuration file underpinning the OPL language server    }
{                                                             }
{*************************************************************}
unit Oplls.ConfigFile;
{$mode objfpc}{$H+}

interface

uses
  Contnrs;

type
  TConfigFile = class
  {
   base class for creating an object that represents the values in config.ini
  }
    const
      C_RX = '^(\s+|)(\S+)(\s+|)(:)(\s+|)("|)([a-zA-Z0-9_\\/\-\s]+)("|)(\s+|)$';

    protected

      type TConfigKeyValue = record 
        Key, Value: String;
      end;

      var
        FPath: String;
        FFilename: String;
        FFullPath: String;
        FConfigFile: TextFile;
        FConfigTable: TFPStringHashTable;

      function EntryDecoder(InputString: String) : TConfigKeyValue;
      procedure ReadConfigData;

    public
      constructor Create(Path: String; Filename: String);
      function GetConfigValue(Key: String) : String;

  end;
  
  TLSConfigFile = class(TConfigFile)
  {
   subclass for creating and validating a specific LS config file
  }

    const
      C_LOGMODES: array [1..4] of string =
        ('CONCISE', 'SILENT', 'STANDARD', 'VERBOSE');

    private
      function IsLoggingModeValid : Boolean;

    public
      constructor Create(Path: String; Filename: String);
      
  end;

implementation

uses
  SysUtils, StrUtils, RegExpr;

// TConfigFile base class methods

constructor TConfigFile.Create(Path: String; Filename: String);
{
 constructor to create a new TConfigFile object using a custom config.ini file
}
begin
  FPath := Path;
  FFilename := Filename;
  FFullPath := FPath + FFilename;

  // initialise config file
  if FileExists(FFullPath) then
  begin
    AssignFile(FConfigFile, FFullPath);
  end
  else
    raise EFileNotFoundException.Create('No config file found at ' + FFullPath);

  // read data from config file into hash table
  ReadConfigData;
  CloseFile(FConfigFile);

end;

function TConfigFile.EntryDecoder(InputString: String) : TConfigKeyValue;
{
 reads a line from config.ini and returns a record of the key and its value
}
var
  Re: TRegExpr;
begin
  // regex to obtain the key and value assuming a non-quoted string (the key)
  // followed by a colon separator and a quoted string (the value). The key
  // is match group 2 and the value is match group 7. The regex is sufficiently 
  // relaxed to allow one or more whitespace characters before and after the 
  // key as well as after the colon and before and after the value. Failure to 
  // match the regex will result in an exception being thrown due to the line 
  // in config.ini not being a syntactically valid key:value pair.
  Re := TRegExpr.Create(C_RX);
  if Re.Exec(InputString) then
  begin
    Result.Key := Re.Match[2];
    Result.Value := Re.Match[7];
  end
  else
    raise ENotSupportedException.Create('Unable to parse key:value pair [' +
    InputString + '] in config file');
 end;

procedure TConfigFile.ReadConfigData;
{
 read the contents of the config.ini file into the ConfigTable hash
}
var
  Line: String;
  KeyValue: TConfigKeyValue;
begin
  try
    begin
      // initialise hash list
      FConfigTable := TFPStringHashTable.Create;
      // open the file for reading
      Reset(FConfigFile);
      // keep reading lines until the end of the file is reached
      while not eof(FConfigFile) do
      begin
        ReadLn(FConfigFile, Line);
        // ignore comment lines and empty lines
        if (Length(Line) > 0) then
        begin
          if (Line[1] <> '#') then
          begin
            KeyValue := EntryDecoder(Line);
            FConfigTable.Add(KeyValue.Key, KeyValue.Value);
          end;
        end;
      end;
    end;
  except
    raise EInOutError.Create('File handling error occurred during read of config ' +
    'file at ' + FFullPath);
  end;
end;

function TConfigFile.GetConfigValue(Key: String) : String;
{
 return the config value based on its key
}
var
  Value: String;
begin
  Value := FConfigTable.Items[key];
  if Value = '' then
    raise ERangeError.Create('No such key [' + Key + '] found in configuration data')
  else
    Result := Value;
end;

// TLSConfigFile subclass methods

constructor TLSConfigFile.Create(Path: String; Filename: String);
begin
  inherited Create(Path, Filename);
  if not IsLoggingModeValid then
    raise ERangeError.Create('Config entry for logging-mode is invalid');
end;

function TLSConfigFile.IsLoggingModeValid : Boolean;
{
 validates logging-mode entry 
}
var
  Test: String;
  IsValid: Boolean;
  Iterator: Integer;
begin

  // initialise to False
  IsValid := False;

  // loop through constant string array LOG_MODES until config entry 
  // for logging-mode matches the value in the config file
  Test := GetConfigValue('logging-mode');
  Iterator := 0;
  repeat
  begin
    Iterator := Iterator + 1;
    if Test = C_LOGMODES[Iterator] then
      IsValid := True;
  end;
  until (Iterator = Length(C_LOGMODES)) or (IsValid = True);
  Result := IsValid;
end;

end.