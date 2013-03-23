{
 * UMain.pas
 *
 * Class implementing main program functionality for DelphiDabbler Console
 * Application Runner Classes demo program #10: TPJConsoleApp from console
 * applications.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
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

