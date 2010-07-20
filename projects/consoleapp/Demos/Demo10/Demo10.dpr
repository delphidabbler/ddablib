{
 * Demo10.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #10: TPJConsoleApp & Console Applications.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
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
  PJConsoleApp in '..\..\PJConsoleApp.pas',
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

