{*****************************************************************}
{                                                                 }
{  OPL LS Logger class                                            }
{                                                                 }
{  (c) Colin Hoad, 2025                                           }
{                                                                 }
{  Class file for creating, writing to and reading from a simple  }
{  text-based log file for tracking the requests and responses    }
{  made to and sent from the OPL Language Server                  } 
{                                                                 }
{*****************************************************************}
unit Oplls.Logger;
{$mode objfpc}{$H+}

interface

type

  TLogger = class
  {
   class for creating, writing to and reading from a log file
  }
    strict private

      var
        FMode: SmallInt;
        FPath: String;
        FFilename: String;
        FFullPath: String;
        FLogFile: TextFile;
      function CodifyMode(ModeName: String) : Byte;

    public

      constructor Create(Mode: String; Path: String; LogName: String);
      function GetMode : String;
      function GetFullPath : String;
      procedure WriteLine(ModeName: String; Line: String);
      function ReadLine(Offset: Integer = 1) : String;

  end;

implementation

uses
  SysUtils, StrUtils, Classes;

constructor TLogger.Create(Mode: String; Path: String; LogName: String);
{
 constructor to create a new TLogger object
}
begin
  // initialise instance variables
  FMode := CodifyMode(Mode);
  if FMode > CodifyMode('SILENT') then
  begin
    FPath := Path;
    // create log file
    if DirectoryExists(FPath) then
    begin
      FFilename := FormatDateTime('YYYY-MM-DD_hh.mm.ss', Now) +
                  '_' + LogName + '.log';
      FFullPath := FPath + FFilename;
      AssignFile(FLogFile, FFullPath);
      Rewrite(FLogFile);
      CloseFile(FLogFile);
    end
    else
    begin
      FMode := CodifyMode('SILENT');
    end;
  end;
end;

function TLogger.CodifyMode(ModeName: String) : Byte;
begin
  if ModeName = 'VERBOSE' then
    Result := 3
  else 
  if ModeName = 'STANDARD' then
    Result := 2
  else 
  if ModeName = 'CONCISE' then
    Result := 1
  else
    Result := 0
end;

function TLogger.GetMode : String;
{
 Returns the current logging mode in human friendly format
}
begin
  if FMode = 3 then
    Result := 'VERBOSE'
  else 
  if FMode = 2 then
    Result := 'STANDARD'
  else 
  if FMode = 1 then
    Result := 'CONCISE'
  else
    Result := 'SILENT'
end;

function TLogger.GetFullPath : String;
{
 Returns the full path to the log file
}
begin
  Result := FFullPath;
end;

procedure TLogger.WriteLine(ModeName: String; Line: String);
{
 Writes a line to the log file if not in SILENT mode
}
var
  Mode: Byte;
begin
  // only consider logging if object's logging mode is
  // not set to SILENT
  if FMode > CodifyMode('SILENT') then
  begin
    // get the logging level of the line to be logged
    Mode := CodifyMode(ModeName);
    // check if line should be logged based on the logging mode
    // of the object compared to the logging level of the line
    // to be logged
    if Mode <= FMode then
      begin
      try
        try
          begin

              // Open the file for appending, write Line to it and close it.
              Append(FLogFile);
              Write(FLogFile, Line + LineEnding);
            end;
        except
          raise EInOutError.Create('Unexpected log file handling error occurred ' +
          'during write!');
        end;
      finally
        CloseFile(FLogFile);
      end;
    end;
  end;
end;

function TLogger.ReadLine(Offset: Integer = 1) : String;
{
 Reads x lines from the bottom of the log file, defined by the offset
}
var
  LogLines: TStringList;
  LineCount, LineIndex: Integer;
  Lines: String;
begin
  Lines := '';
  if FMode > CodifyMode('SILENT') then
  begin
    try
      try
        // temporarily load log file into TStringList variable
        LogLines := TStringList.Create;
        LogLines.LoadFromFile(FFullPath);
        // count the number of lines
        LineCount := LogLines.Count;
        if Offset > LineCount then
          raise EArgumentException.Create('Offset is greater than length ' +
          'of log file!');
        // subtract the offset from the line count to get the starting
        // index for reading lines
        LineIndex := LineCount-Offset;
        // now loop for as many times as it takes to get to the end of the 
        // number of lines, starting from the offset
        repeat
        begin
          Lines := Lines + LogLines[LineIndex];
          LineIndex := LineIndex + 1;
          if LineIndex <> LineCount then 
            Lines := Lines + LineEnding;
        end;
        until LineIndex >= LineCount;
      except
        on E: EStringListError do
        WriteLn('Attempted to read beyond length of log file! ', E.Message);
      end;
    finally
      LogLines.Free;
    end;  
  end;
  Result := Lines;
end;

end.