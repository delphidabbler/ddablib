{
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #3: Indicating Progress.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
  private
    procedure WorkHandler(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses
  PJConsoleApp;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  App: TPJConsoleApp;
begin
  App := TPJConsoleApp.Create;
  try
    App.MaxExecTime := INFINITE;  // don't time out
    App.TimeSlice := 100;         // yield to main app every 1/10 second
    App.Visible := True;          // ensure we see the app
    App.OnWork := WorkHandler;    // assign the event handler
    if not App.Execute('Timed 5') then  // run Timed.exe for 5 seconds
      ShowMessage('Failed to run Timed.exe');
  finally
    App.Free;
  end;
end;

procedure TForm1.WorkHandler(Sender: TObject);
begin
  if ProgressBar1.Position = ProgressBar1.Max then
    ProgressBar1.Position := 0
  else
     ProgressBar1.Position := ProgressBar1.Position + 1;
  Application.ProcessMessages;
end;

end.

