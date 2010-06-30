{
  DelphiDabbler Console Application Runner Classes

  Demo Program 4: Timing Out.

  FmDemo4.dpr
    v1.0 of 03 Oct 2007  - Main form. Original version.

  Written by Peter Johnson (www.delphidabbler.com).
  This demo program and its source code is placed in the public domain with no
  restrictions as to use.

  THE PROGRAM IS PROVIDED "AS-IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTY. IN
  NO EVENT SHALL THE AUTHOR BE HELD LIABLE FOR ANY DAMAGES ARISING FROM THE USE
  OF THIS LIBRARY.
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

