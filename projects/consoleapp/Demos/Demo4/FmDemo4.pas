{
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #4: Timing Out.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Label2: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
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
    App.MaxExecTime := StrToInt(Edit1.Text);
    App.KillTimedOutProcess := Checkbox1.Checked;
    App.TimeSlice := 200;
    App.Visible := True;
    App.OnWork := WorkHandler;
    if App.Execute('Timed 5') then
      Memo1.Lines.Add('Application completed normally')
    else
      Memo1.Lines.Add(App.ErrorMessage)
  finally
    App.Free;
  end;
end;

procedure TForm1.WorkHandler(Sender: TObject);
var
  App: TPJConsoleApp;
begin
  App := Sender as TPJConsoleApp;
  Memo1.Lines.Add(
    Format(
      'Elapsed time: %dms, Time to live: %dms',
      [App.ElapsedTime, App.TimeToLive]
    )
  );
  Application.ProcessMessages;  // keeps GUI interactive
end;

end.

