{*************************************************************}
{                                                             }
{  OPL Language Server program                                }
{                                                             }
{  (c) Colin Hoad, 2025                                       }
{                                                             }
{  Master program for running the OPL Language Server         }
{                                                             }
{*************************************************************}
program Oplls;
{$mode objfpc}{$H+}

uses
  SysUtils, BaseUnix,
  Oplls.ConfigFile, Oplls.Logger, Oplls.Listener, Oplls.Helpers;

const
  C_CONFIGPATH = 'config/';
  C_CONFIGFILE = 'config.ini';
  C_LOGPATH = 'logging-path';
  C_LOGMODE = 'logging-mode';
  C_LOGNAME = 'oplls';

var
  LsConfig: TLSConfigFile;
  LsLog: TLogger;
  LsListener: TListener;
  FDS: TFdset;

begin

  try

    // initialise configuration file
    try
      LsConfig := TLSConfigFile.Create(GetConfigPath(C_CONFIGPATH), C_CONFIGFILE);
    except 
      on E: EFileNotFoundException do
        WriteLn(StdErr, 'Unable to read OPL LS configuration file: ', E.Message);
    end;

    // initialise log file
    try
      LsLog := TLogger.Create(LsConfig.GetConfigValue(C_LOGMODE),
                              LsConfig.GetConfigValue(C_LOGPATH),
                              C_LOGNAME);
    except
      on E: EFileNotFoundException do
        WriteLn(StdErr, 'Unable to create OPL LS log file: ', E.Message);
    end;

    // initialise the OPL LS listener
    LsListener := TListener.Create(LsLog, LsConfig);

    // now loop until EOF to ensure OPL LS listener keeps checking for new requests
    while not EOF do
    begin
      fpfd_zero(FDS); // clear all file descriptors (i.e. set them to 0) in my file descriptor array
      fpfd_set(0, FDS); // sets the stdin (identity=0) bit in my file descriptor array to 1
      fpSelect (1, @FDS, nil, nil, nil); // wait until stdin changes (i.e. input received), ignore stdout and stderr, and make timeout nil (i.e. infinite)
      LsListener.Listen; // listen to request and provide a response
      end;

  finally
    LsLog.WriteLine('CONCISE', 'INFO: LSP stopped listening.');
  end;

end.