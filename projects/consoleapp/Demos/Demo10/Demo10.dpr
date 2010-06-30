{
  DelphiDabbler Console Application Runner Classes

  Demo Program 10: TPJConsoleApp & Console Applications.

  Demo10.dpr
    v1.0 of 03 Oct 2007  - Project file. Runs the application. Original version.

  Written by Peter Johnson (www.delphidabbler.com).
  This demo program and its source code is placed in the public domain with no
  restrictions as to use.

  THE PROGRAM IS PROVIDED "AS-IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTY. IN
  NO EVENT SHALL THE AUTHOR BE HELD LIABLE FOR ANY DAMAGES ARISING FROM THE USE
  OF THIS LIBRARY.
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

