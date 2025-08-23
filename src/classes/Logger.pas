unit Logger;
{$mode objfpc}{$H+}
interface

uses
  sysutils, strutils, regexpr;

type
  {
   class for creating and writing to a log file
  }
  TLogger = class

    private

      var
        Mode: String;
        Path: String;
        Filename: String;
        FullPath: String;
        LogFile: TextFile;

    public
      constructor Create(sMode: String; sPath: String);
      function GetMode : String;
      function GetFullPath : String;
      procedure WriteMsg(msg: String);

  end;

implementation

constructor TLogger.Create(sMode: String; sPath: String);
{
 constructor to create a new TLogger object
}
begin
  // initialise instance variables
  Mode := sMode;
  if Mode <> 'SILENT' then
  begin
    Path := sPath;
    // create log file
    if DirectoryExists(Path) then
    begin
      Filename := FormatDateTime('YYYY-MM-DD_hh.mm.ss', Now) +
                  '_oplls.log';
      FullPath := Path + Filename;
      AssignFile(LogFile, FullPath);
      rewrite(LogFile);
      CloseFile(LogFile);
    end
    else
    begin
      Mode := 'SILENT';
    end;
  end;
end;

function TLogger.GetMode : String;
{
 Returns the current logging mode
}
begin
  Result := Mode;
end;

function TLogger.GetFullPath : String;
{
 Returns the current logging mode
}
begin
  Result := FullPath;
end;

procedure TLogger.WriteMsg(msg: String);
{
 Writes a line to the log file if not in SILENT mode
}
begin
  if Mode <> 'SILENT' then
  begin
    try
      begin
        // Open the file for appending, write message to it and close it.
        append(LogFile);
        write(LogFile, msg);
        CloseFile(LogFile);
      end;
    except
      raise EInOutError.Create('Unexpectd log file handling error occurred!');
    end;
  end;
end;

end.