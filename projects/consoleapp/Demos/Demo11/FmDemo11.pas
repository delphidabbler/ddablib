{
 * FmDemo11.pas
 *
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #11: Customising the console window.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2011.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
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
    btnDefaultColours: TButton;
    edTitle: TEdit;
    gbScrBufferSize: TGroupBox;
    cbDefScrBufferSize: TCheckBox;
    edScrBufX: TEdit;
    lblScrBufX: TLabel;
    lblScrBufY: TLabel;
    edScrBufY: TEdit;
    procedure btnRunClick(Sender: TObject);
    procedure btnDefaultColoursClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbDefScrBufferSizeClick(Sender: TObject);
    procedure EdNumberFilter(Sender: TObject; var Key: Char);
  private
    procedure WorkHandler(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function ConvertColor(const C: TColor): TPJConsoleColor;
begin
  case C of
    clBlack: Result := ccBlack;
    clNavy: Result := ccNavy;
    clGreen: Result := ccGreen;
    clTeal: Result := ccTeal;
    clMaroon: Result := ccMaroon;
    clPurple: Result := ccPurple;
    clOlive: Result := ccOlive;
    clSilver: Result := ccSilver;
    clGray: Result := ccGray;
    clBlue: Result := ccBlue;
    clLime: Result := ccLime;
    clAqua: Result := ccAqua;
    clRed: Result := ccRed;
    clFuchsia: Result := ccFuchsia;
    clYellow: Result := ccYellow;
    clWhite: Result := ccWhite;
    else raise Exception.Create('Unsupported colour');
  end;
end;

{ TForm1 }

procedure TForm1.btnDefaultColoursClick(Sender: TObject);
begin
  cbForeground.Selected := clWhite;
  cbBackground.Selected := clBlack;
end;

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
      if not cbDefScrBufferSize.Checked then
        App.ScreenBufferSize := MakeSize(
          StrToInt(edScrBufX.Text), StrToInt(edScrBufY.Text)
        );
      App.ConsoleColors := MakeConsoleColors(
        cbForeground.Selected, cbBackground.Selected
      );
      App.Execute('Timed 2');
    finally
      App.Free;
    end;
  finally
    btnRun.Enabled := True;
  end;
end;

procedure TForm1.cbDefScrBufferSizeClick(Sender: TObject);
begin
  lblScrBufX.Enabled := not cbDefScrBufferSize.Checked;
  lblScrBufY.Enabled := not cbDefScrBufferSize.Checked;
  edScrBufX.Enabled := not cbDefScrBufferSize.Checked;
  edScrBufY.Enabled := not cbDefScrBufferSize.Checked;
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

