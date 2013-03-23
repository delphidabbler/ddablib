{
 * FmDemo11.pas
 *
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #11: Customising the appearance of the console.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo11;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  PJConsoleApp;

type
  TForm1 = class(TForm)
    btnRun: TButton;
    gbConsoleColours: TGroupBox;
    lblForeground: TLabel;
    cbForeground: TColorBox;
    lblBackground: TLabel;
    cbBackground: TColorBox;
    lblTitle: TLabel;
    edTitle: TEdit;
    gbWindowSize: TGroupBox;
    lblWindowWidth: TLabel;
    lblWindowHeight: TLabel;
    cbDefWindowSize: TCheckBox;
    edWindowWidth: TEdit;
    edWindowHeight: TEdit;
    gbWindowPos: TGroupBox;
    lblWindowLeft: TLabel;
    lblWindowTop: TLabel;
    cbDefWindowPos: TCheckBox;
    edWindowLeft: TEdit;
    edWindowTop: TEdit;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EdNumberFilter(Sender: TObject; var Key: Char);
    procedure cbDefWindowSizeClick(Sender: TObject);
    procedure cbDefWindowPosClick(Sender: TObject);
  private
    procedure WorkHandler(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.btnRunClick(Sender: TObject);
var
  App: TPJConsoleApp;
begin
  btnRun.Enabled := False;
  try
    App := TPJConsoleApp.Create;
    try
      App.OnWork := WorkHandler;
      App.Visible := True;
      App.ConsoleTitle := edTitle.Text;
      if not cbDefWindowSize.Checked then
        App.WindowSize := MakeSize(
          StrToInt(edWindowWidth.Text), StrToInt(edWindowHeight.Text)
        );
      if not cbDefWindowPos.Checked then
        App.WindowPosition := Point(
          StrToInt(edWindowLeft.Text), StrToInt(edWindowTop.Text)
        );
      App.ConsoleColors := MakeConsoleColors(
        cbForeground.Selected, cbBackground.Selected
      );
      if not App.Execute('Timed 2') then
        raise Exception.CreateFmt(
          'Can''t execute program: error %d - "%s"',
          [App.ErrorCode, App.ErrorMessage]
        );
    finally
      App.Free;
    end;
  finally
    btnRun.Enabled := True;
  end;
end;

procedure TForm1.cbDefWindowPosClick(Sender: TObject);
begin
  lblWindowLeft.Enabled := not cbDefWindowPos.Checked;
  lblWindowTop.Enabled := not cbDefWindowPos.Checked;
  edWindowLeft.Enabled := not cbDefWindowPos.Checked;
  edWindowTop.Enabled := not cbDefWindowPos.Checked;
end;

procedure TForm1.cbDefWindowSizeClick(Sender: TObject);
begin
  lblWindowWidth.Enabled := not cbDefWindowSize.Checked;
  lblWindowHeight.Enabled := not cbDefWindowSize.Checked;
  edWindowWidth.Enabled := not cbDefWindowSize.Checked;
  edWindowHeight.Enabled := not cbDefWindowSize.Checked;
end;

procedure TForm1.EdNumberFilter(Sender: TObject; var Key: Char);
begin
  {$IFDEF UNICODE}
  if not CharInSet(Key, ['0'..'9', #8]) then
  {$ELSE}
  if not (Key in ['0'..'9', #8]) then
  {$ENDIF}
    Key := #0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edTitle.Text := Caption;
end;

procedure TForm1.WorkHandler(Sender: TObject);
begin
  // makes GUI app response while console app is running
  Application.ProcessMessages;
end;

end.

