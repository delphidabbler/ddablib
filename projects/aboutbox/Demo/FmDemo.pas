{
 * FmDemo.pas
 *
 * Main form for About Box Component demo program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}


unit FmDemo;

interface

{$UNDEF RTLNAMESPACES}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE RTLNAMESPACES}
  {$IFEND}
{$ENDIF}

uses
  {$IFNDEF RTLNAMESPACES}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, 
  {$ELSE}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Samples.Spin,
  {$ENDIF}
  PJAbout, PJVersionInfo;

type
  TForm1 = class(TForm)
    btnExecute: TButton;
    btnFont: TButton;
    cbButtonGlyph: TComboBox;
    cbButtonKind: TComboBox;
    cbButtonPlacing: TComboBox;
    cbDlgText: TComboBox;
    cbPosition: TComboBox;
    chkCentreDlg: TCheckBox;
    dlgAbout: TPJAboutBoxDlg;
    gpButton: TGroupBox;
    gpPositioning: TGroupBox;
    lblButtonGlyph: TLabel;
    lblButtonHeight: TLabel;
    lblButtonKind: TLabel;
    lblButtonPlacing: TLabel;
    lblButtonWidth: TLabel;
    lblDlgLeft: TLabel;
    lblDlgText: TLabel;
    lblDlgTop: TLabel;
    lblPosition: TLabel;
    sedButtonHeight: TSpinEdit;
    sedButtonWidth: TSpinEdit;
    sedDlgLeft: TSpinEdit;
    sedDlgTop: TSpinEdit;
    viAbout: TPJVersionInfo;
    chkProgramName: TCheckBox;
    chkUseOwnerAsParent: TCheckBox;
    chkUseOSStdFonts: TCheckBox;
    procedure btnExecuteClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbDlgTextChange(Sender: TObject);
    procedure chkCentreDlgClick(Sender: TObject);
    procedure chkUseOSStdFontsClick(Sender: TObject);
  private
    fFont: TFont;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnExecuteClick(Sender: TObject);
begin
  // Update property values
  dlgAbout.ButtonGlyph := TPJAboutBtnGlyphs(cbButtonGlyph.ItemIndex);
  dlgAbout.ButtonHeight := sedButtonHeight.Value;
  dlgAbout.ButtonKind := TPJAboutBtnKinds(cbButtonKind.ItemIndex);
  dlgAbout.ButtonPlacing := TPJAboutBtnPlacing(cbButtonPlacing.ItemIndex);
  dlgAbout.ButtonWidth := sedButtonWidth.Value;
  dlgAbout.CentreDlg := chkCentreDlg.Checked;
  dlgAbout.DlgLeft := sedDlgLeft.Value;
  dlgAbout.DlgTop := sedDlgTop.Value;
  dlgAbout.Position := TPJAboutPosition(cbPosition.ItemIndex);
  if chkProgramName.Checked then
    dlgAbout.ProgramName := 'AboutBoxDemo.exe'
  else
    dlgAbout.ProgramName := '';
  if cbDlgText.ItemIndex = 0 then
    dlgAbout.VersionInfo := nil
  else
    dlgAbout.VersionInfo := viAbout;
  dlgAbout.UseOSStdFonts := chkUseOSStdFonts.Checked;
  dlgAbout.Font := fFont;
  dlgAbout.UseOwnerAsParent := chkUseOwnerAsParent.Checked;
  // Display dialog
  dlgAbout.Execute;
end;

procedure TForm1.btnFontClick(Sender: TObject);
var
  FontDlg: TFontDialog;
begin
  FontDlg := TFontDialog.Create(Self);
  try
    FontDlg.Font := fFont;
    if FontDlg.Execute then
    begin
      fFont.Assign(FontDlg.Font);
    end;
  finally
    FontDlg.Free;
  end;
end;

procedure TForm1.cbDlgTextChange(Sender: TObject);
begin
  chkProgramName.Enabled := cbDlgText.ItemIndex = 0;
end;

procedure TForm1.chkCentreDlgClick(Sender: TObject);
begin
  // Enable / disable offset property controls per CentreDlg setting
  sedDlgLeft.Enabled := not chkCentreDlg.Checked;
  sedDlgTop.Enabled := not chkCentreDlg.Checked;
  lblDlgLeft.Enabled := not chkCentreDlg.Checked;
  lblDlgTop.Enabled := not chkCentreDlg.Checked;
end;

procedure TForm1.chkUseOSStdFontsClick(Sender: TObject);
begin
  btnFont.Enabled := not chkUseOSStdFonts.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  // Text for combo boxes
  cButtonGlyphNames: array[TPJAboutBtnGlyphs] of string =
    ('OK', 'Cancel', 'Ignore', 'Close', 'None');
  cButtonNames: array[TPJAboutBtnKinds] of string =
    ('OK', 'Done', 'Close', 'Cancel');
  cButtonPlacing: array[TPJAboutBtnPlacing] of string =
    ('Left', 'Centre', 'Right');
  cPosition: array[TPJAboutPosition] of string =
    ('Screen', 'Desktop', 'Owner');
var
  I: Integer; // loops thru enum types
begin
  // Set up form's properties

  Caption := Application.Title;

  // Set up controls per dialog box component

  // dialog text
  if Assigned(dlgAbout.VersionInfo) then
    cbDlgText.ItemIndex := 1
  else
    cbDlgText.ItemIndex := 0;

  chkProgramName.Enabled := not Assigned(dlgAbout.VersionInfo);
  chkProgramName.Checked := dlgAbout.ProgramName <> '';

  // dialog positioning

  for I := Ord(Low(TPJAboutPosition)) to Ord(High(TPJAboutPosition)) do
    cbPosition.Items.Add(cPosition[TPJAboutPosition(I)]);
  cbPosition.ItemIndex := Ord(dlgAbout.Position);

  chkCentreDlg.Checked := dlgAbout.CentreDlg;

  sedDlgLeft.Value := dlgAbout.DlgLeft;
  sedDlgLeft.Enabled := not dlgAbout.CentreDlg;
  lblDlgLeft.Enabled := sedDlgLeft.Enabled;

  sedDlgTop.Value := dlgAbout.DlgTop;
  sedDlgTop.Enabled := not dlgAbout.CentreDlg;
  lblDlgTop.Enabled := sedDlgTop.Enabled;

  // button configuration

  for I := Ord(Low(TPJAboutBtnGlyphs)) to Ord(High(TPJAboutBtnGlyphs)) do
    cbButtonGlyph.Items.Add(cButtonGlyphNames[TPJAboutBtnGlyphs(I)]);
  cbButtonGlyph.ItemIndex := Ord(dlgAbout.ButtonGlyph);

  for I := Ord(Low(TPJAboutBtnKinds)) to Ord(High(TPJAboutBtnKinds)) do
    cbButtonKind.Items.Add(cButtonNames[TPJAboutBtnKinds(I)]);
  cbButtonKind.ItemIndex := Ord(dlgAbout.ButtonKind);

  for I := Ord(Low(TPJAboutBtnPlacing)) to Ord(High(TPJAboutBtnPlacing)) do
    cbButtonPlacing.Items.Add(cButtonPlacing[TPJAboutBtnPlacing(I)]);
  cbButtonPlacing.ItemIndex := Ord(dlgAbout.ButtonPlacing);

  sedButtonWidth.Value := dlgAbout.ButtonWidth;

  sedButtonHeight.Value := dlgAbout.ButtonHeight;

  fFont := TFont.Create;
  fFont.Assign(dlgAbout.Font);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fFont.Free;
end;

end.
