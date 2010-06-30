{
  DelphiDabbler Console Application Runner Classes

  Demo Program 10: TPJConsoleApp & Console Applications.

  UMain.dpr
    v1.0 of 03 Oct 2007  - Implements class that encapsulates main program
                           functionality. Original version.

  Written by Peter Johnson (www.delphidabbler.com).
  This demo program and its source code is placed in the public domain with no
  restrictions as to use.

  THE PROGRAM IS PROVIDED "AS-IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTY. IN
  NO EVENT SHALL THE AUTHOR BE HELD LIABLE FOR ANY DAMAGES ARISING FROM THE USE
  OF THIS LIBRARY.
}

unit UMain;

interface

type
  TMain = class(TObject)
  private
    fVisible: Boolean;
    fNewConsole: Boolean;
    fTitle: string;
    procedure WorkHandler(Sender: TObject);
    procedure ParseCommandLine;
  public
    procedure Execute;
  end;

implementation

uses
  SysUtils, PJConsoleApp;

{ TMain }

procedure TMain.Execute;
var
  App: TPJConsoleApp;
begin
  ParseCommandLine;
  App := TPJConsoleApp.Create;
  try
    // Set properties based on parameters
    App.Visible := fVisible;
    App.UseNewConsole := fNewConsole;
    App.ConsoleTitle := fTitle;
    // Set OnWork handler
    App.OnWork := WorkHandler;
    // Run the application
    WriteLn('Starting Timed.exe');
    if not App.Execute('Timed 3') then
      raise Exception.CreateFmt('%X: %s', [App.ErrorCode, App.ErrorMessage]);
    WriteLn('Timed.exe completed with exit code: ', App.ExitCode);
  finally
    App.Free;
  end;
end;

procedure TMain.ParseCommandLine;
var
  Param: string;
  Idx: Integer;
begin
  for Idx := 1 to ParamCount do
  begin
    Param := ParamStr(Idx);
    if Param = '-v' then
      fVisible := True
    else if Param = '-n' then
      fNewConsole := True
    else if (Param <> '') and (Param[1] <> '-') then
      fTitle := Param
    else
      raise Exception.CreateFmt('Unknown parameter "%s"', [Param]);
  end;
end;

procedure TMain.WorkHandler(Sender: TObject);
var
  App: TPJConsoleApp;
begin
  App := Sender as TPJConsoleApp;
  if App.UseNewConsole then
    WriteLn('Waited ', App.ElapsedTime, ' ms for Timed.exe');
end;

end.

