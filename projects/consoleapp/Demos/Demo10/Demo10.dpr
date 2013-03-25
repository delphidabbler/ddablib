{
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #10: TPJConsoleApp from console applications.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo10;

{$APPTYPE CONSOLE}

{
  Demo10.exe: shows how to use TPJConsoleApp from a console application.

  Usage:
    Demo10.exe [-v] [-n] [<title>]
      -v Child app is visible
      -n Use new console window
      <title> Title of new console window: enclose title with spaces in quotes
}

uses
  SysUtils,
  UMain in 'UMain.pas';

begin
  try
    WriteLn('TPJConsoleApp Demo 10');
    with TMain.Create do
      try
        Execute;
      finally
        Free;
      end;
    WriteLn('TPJConsoleApp Demo 10: Done');
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end.

