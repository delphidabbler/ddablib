{
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #2: A better ExecAndWait.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
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

{ TForm1 }

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
  Application.ProcessMessages;
end;

end.

