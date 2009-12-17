{
 * FmDemo.pas
 *
 * Main form for demo program for DelphiDabbler About Box Component.
 *
 * v1.0 of 08 Nov 2005  - Original version.
 * v1.1 of 16 Jun 2008  - Added support for UseOwnerAsParent and UseOSStdFonts
 *                        added to component at v3.4.
 *
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is FmDemo.pas from AboutBoxDemo.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmDemo;

{$IFDEF VER90}
  // Delphi 2
  {$DEFINE DELPHI3ANDBELOW}
{$ENDIF}
{$IFDEF VER100}
  // Delphi 3
  {$DEFINE DELPHI3ANDBELOW}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, PJAbout, PJVersionInfo, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    btnExecute: TButton;
    cbButtonGlyph: TComboBox;
    cbButtonKind: TComboBox;
    cbButtonPlacing: TComboBox;
    cbDlgText: TComboBox;
    cbPosition: TComboBox;
    chkAutoDetect: TCheckBox;
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
    procedure FormCreate(Sender: TObject);
    procedure cbDlgTextChange(Sender: TObject);
    procedure chkCentreDlgClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnExecuteClick(Sender: TObject);
begin
  // Update property values
  dlgAbout.AutoDetectGlyphs := chkAutoDetect.Checked;
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
  dlgAbout.UseOwnerAsParent := chkUseOwnerAsParent.Checked;
  // Display dialog
  dlgAbout.Execute;
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

  chkAutoDetect.Checked := dlgAbout.AutoDetectGlyphs;

  for I := Ord(Low(TPJAboutBtnKinds)) to Ord(High(TPJAboutBtnKinds)) do
    cbButtonKind.Items.Add(cButtonNames[TPJAboutBtnKinds(I)]);
  cbButtonKind.ItemIndex := Ord(dlgAbout.ButtonKind);

  for I := Ord(Low(TPJAboutBtnPlacing)) to Ord(High(TPJAboutBtnPlacing)) do
    cbButtonPlacing.Items.Add(cButtonPlacing[TPJAboutBtnPlacing(I)]);
  cbButtonPlacing.ItemIndex := Ord(dlgAbout.ButtonPlacing);

  sedButtonWidth.Value := dlgAbout.ButtonWidth;

  sedButtonHeight.Value := dlgAbout.ButtonHeight;
end;

end.
