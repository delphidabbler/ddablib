{
 * Timed.dpr
 *
 * Whole source code for Timed.exe helper program for DelphiDabbler Console
 * Application Runner Classes Demos. Program runs for a specified number of
 * seconds.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Timed;

{
  Application that runs for the number of seconds specified as the first
  parameter on the command line. If no parameter is specified the application
  runs for 5 seconds.

  A full stop is written to standard output approx every 1/10th second followed
  by "Done" when it completes.

  Program exit code is always 0.

  Usage:
    Timed [time-to-run]
  Eg: to run the application for 6 seconds use
    Timed 6
}

{$APPTYPE CONSOLE}

uses
  // Delphi
  SysUtils, Windows;

var
  TimeToRun: Integer; // time program is to run for in ms
  StartTick: Integer; // tick count when program starts
  TickNow: Integer;   // tick count during each program loops
begin
  TimeToRun := 1000 * StrToIntDef(ParamStr(1), 5);
  ExitCode := 0;
  WriteLn('TIMED: Running for ', TimeToRun div 1000, ' seconds');
  StartTick := GetTickCount;
  repeat
    TickNow := GetTickCount;
    Sleep(100);
    Write('.');
  until TickNow - StartTick >= TimeToRun;
  WriteLn;
  WriteLn('Done');
end.

