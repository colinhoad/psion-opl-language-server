{*****************************************************************}
{                                                                 }
{  OPL LS Helpers unit                                            }
{                                                                 }
{  (c) Colin Hoad, 2025                                           }
{                                                                 }
{  Unit file comprising helper functions, types, records and      }
{  other useful components that do not belong to any particular   }
{  class                                                          } 
{                                                                 }
{*****************************************************************}
unit Oplls.Helpers;
{$mode objfpc}{$H+}

interface

function GetConfigPath(RelativePath: String) : String;

implementation

uses
  SysUtils;

function GetConfigPath(RelativePath: String) : String;
{
 gets the absolute path to the configuration file based on the
 location of the OPL LS binary executable and a path relative
 to that executable's location
}
begin
  Result := ExtractFilePath(ParamStr(0)) + RelativePath;
end;

end.