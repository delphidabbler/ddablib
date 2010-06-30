{
  DelphiDabbler Console Application Runner Classes

  Demo helper program: Timed.exe.

  Timed.dpr
    v1.0 of 03 Oct 2007  - Whole application. Original version.

  Written by Peter Johnson (www.delphidabbler.com).
  This demo program and its source code is placed in the public domain with no
  restrictions as to use.

  THE PROGRAM IS PROVIDED "AS-IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTY. IN
  NO EVENT SHALL THE AUTHOR BE HELD LIABLE FOR ANY DAMAGES ARISING FROM THE USE
  OF THIS LIBRARY.
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

