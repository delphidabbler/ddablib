{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2004-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Enhanced string property editor for the Delphi IDE that enables multi-line
 * strings to be edited and assigned to any components' string & TCaption
 * properties at design time.
}


unit PJStringPE;


interface


// Requires Delphi 6 or later

{$IF CompilerVersion >= 15.0} // Delphi 7 and higher
  {$WARN UNSAFE_CODE OFF}
{$IFEND}
{$IF CompilerVersion >= 23.0} // Delphi XE2
  {$DEFINE RTLNAMESPACES}
{$IFEND}
{$IF CompilerVersion >= 24.0} // Delphi XE3
  {$DEFINE TSCROLLSTYLEMOVED}
{$IFEND}


uses
  // Delphi
  {$IFDEF RTLNAMESPACES}
  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, System.Classes, Vcl.Dialogs,
  Vcl.Forms, Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ActnList,
  {$ELSE}
  StdCtrls, Controls, ExtCtrls, Classes, Dialogs, Forms, ImgList, ComCtrls,
  ToolWin, ActnList,
  {$ENDIF}
  DesignIntf, DesignEditors;


type

  ///  <summary>
  ///  Dialog box used to edit multi-line string properties.
  ///  </summary>
  ///  <remarks>
  ///  Instantiated by TPJStringPE.
  ///  </remarks>
  TPJStringPEDlg = class(TForm)
    toolBar: TToolBar;
    tbSelectAll: TToolButton;
    tbClearText: TToolButton;
    tbSeparator1: TToolButton;
    tbPasteOver: TToolButton;
    tbCopyAll: TToolButton;
    tbSeparator2: TToolButton;
    tbUndo: TToolButton;
    tbSeparator3: TToolButton;
    tbLoad: TToolButton;
    tbSave: TToolButton;
    edText: TMemo;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    actionList: TActionList;
    actLoad: TAction;
    actSave: TAction;
    actClear: TAction;
    actSelectAll: TAction;
    actPasteOver: TAction;
    actCopyAll: TAction;
    actUndo: TAction;
    imageList: TImageList;
    pnlButton: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    cbWordWrap: TCheckBox;
    actHelp: TAction;
    tbSeparator4: TToolButton;
    tbHelp: TToolButton;
    ///  <summary>Copies all text in editor to clipboard.</summary>
    procedure actCopyAllExecute(Sender: TObject);
    ///  <summary>Disables Copy All action if no text in editor.</summary>
    procedure actCopyAllUpdate(Sender: TObject);
    ///  <summary>Clears all text from editor.</summary>
    procedure actClearExecute(Sender: TObject);
    ///  <summary>Displays property editor's online documentation wiki.
    ///  </summary>
    procedure actHelpExecute(Sender: TObject);
    ///  <summary>Replaces text in editor with text on clipboard.</summary>
    procedure actPasteOverExecute(Sender: TObject);
    ///  <summary>Disables Paste Over action if there is no text on clipboard.
    ///  </summary>
    procedure actPasteOverUpdate(Sender: TObject);
    ///  <summary>Loads text from a file specified by user into editor.
    ///  </summary>
    procedure actLoadExecute(Sender: TObject);
    ///  <summary>Saves text from editor to file specified by user.</summary>
    procedure actSaveExecute(Sender: TObject);
    ///  <summary>Selects all text in editor.</summary>
    procedure actSelectAllExecute(Sender: TObject);
    ///  <summary>Disables Select All action if there is no text in editor.
    ///  </summary>
    procedure actSelectAllUpdate(Sender: TObject);
    ///  <summary>Undoes last change in editor.</summary>
    procedure actUndoExecute(Sender: TObject);
    ///  <summary>Disables Undo action if editor can't undo last edit.</summary>
    procedure actUndoUpdate(Sender: TObject);
    ///  <summary>Toggles word wrapping in editor on and off depending on state
    ///  of check box.</summary>
    procedure cbWordWrapClick(Sender: TObject);
    ///  <summary>Initialises form.</summary>
    ///  <remarks>Sets default font.</remarks>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Saves persistent settings when form is destroyed.</summary>
    ///  <remarks>Word wrap setting and window location are persisted.</remarks>
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Cause unmodified ESC key and Ctrl+RETURN key presses to have
    ///  same effect as clicking Cancel and OK buttons respectively.</summary>
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    ///  <summary>Reads and acts on persistent settings from registry.</summary>
    ///  <remarks>Updates word-wrap setting and locates window.</remarks>
    procedure FormShow(Sender: TObject);
  private
    ///  <summary>Saves a setting as binary data in registry.</summary>
    ///  <param name="ID">string [in] Name of registry value.</param>
    ///  <param name="Value">Untyped [in] Value to be stored.</param>
    ///  <param name="Size">Integer [in] Size of Value in bytes.</param>
    ///  <returns>Boolean. True if setting is written successfully or False on
    ///  failure.</returns>
    function SaveSetting(const ID: string; var Value;
      const Size: Integer): Boolean;
    ///  <summary>Reads binary data for a given setting.</summary>
    ///  <param name="ID">string [in] Name of registry value.</param>
    ///  <param name="Value">Untypes [in/out] Untyped value that receives
    ///  setting data. Any existing value is overwritten.</param>
    ///  <param name="Size">Integer [in] Size of Value in bytes.</param>
    ///  <returns>Boolean. True if setting is read successfully or False on
    ///  failure.</returns>
    function ReadSetting(const ID: string; var Value;
      const Size: Integer): Boolean;
    ///  <summary>Updates editor's word wrap setting and state of check box.
    ///  </summary>
    procedure UpdateWordWrap(Flag: Boolean);
  end;


  ///  <summary>
  ///  Extended string property editor.
  ///  </summary>
  TPJStringPE = class(TStringProperty)
  public
    ///  <summary>Called by IDE to get attributes of property editor.</summary>
    ///  <remarks>Informs IDE that property editor displays a dialog box in
    ///  addition to standard string property editor.</remarks>
    function GetAttributes: TPropertyAttributes; override;
    ///  <summary>Called by IDE when property editor dialog box is to be
    ///  displayed.</summary>
    procedure Edit; override;
  end;


///  <summary>Registers property editor for all string and TCaption properties
///  of all components.</summary>
procedure Register;


implementation


uses
  // Delphi
  {$IFDEF RTLNAMESPACES}
  System.SysUtils, Winapi.Windows, System.Win.Registry, Vcl.ClipBrd,
  Winapi.Messages, Winapi.ShellAPI
  {$ELSE}
  SysUtils, Windows, Registry, ClipBrd, Messages, ShellAPI
  {$ENDIF}
  {$IFDEF TSCROLLSTYLEMOVED}
  , System.UITypes
  {$ENDIF}
  ;


{$R *.DFM}


procedure Register;
begin
  // Register property editor for any string or TCaption property of any
  // component.
  RegisterPropertyEditor(TypeInfo(string), nil, '', TPJStringPE);
  RegisterPropertyEditor(TypeInfo(TCaption), nil, '', TPJStringPE);
end;

{ TPJStringPE }

procedure TPJStringPE.Edit;
begin
  with TPJStringPEDlg.Create(Application) do
    try
      edText.Text := GetStrValue;
      if ShowModal = mrOK then
        SetStrValue(edText.Text);
    finally
      Free;
    end;
end;

function TPJStringPE.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TPJStringPEDlg }

const
  // Registry key where property editor's settings are stored
  cRegKey = '\Software\DelphiDabbler\Experts\StringPE';
  // Names of registry values used for settings
  cWordWrapSetting = 'WordWrap';               // whether word wrap is enabled
  cWindowPlacementSetting = 'WindowPlacement'; // size and location of window

procedure TPJStringPEDlg.actClearExecute(Sender: TObject);
begin
  edText.Clear;
  edText.SetFocus;
end;

procedure TPJStringPEDlg.actCopyAllExecute(Sender: TObject);
begin
  Clipboard.AsText := edText.Text;
end;

procedure TPJStringPEDlg.actCopyAllUpdate(Sender: TObject);
begin
  actCopyAll.Enabled := edText.Text <> '';
end;

procedure TPJStringPEDlg.actHelpExecute(Sender: TObject);
begin
  ShellExecute(
    Handle,
    'open',
    'http://www.delphidabbler.com/url/stringpe-wiki',
    nil,
    nil,
    SW_SHOWNORMAL
  );
end;

procedure TPJStringPEDlg.actLoadExecute(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    edText.Lines.LoadFromFile(dlgOpen.FileName);
    edText.SetFocus;
    edText.SelStart := 0;
    edText.SelLength := 0;
  end;
end;

procedure TPJStringPEDlg.actPasteOverExecute(Sender: TObject);
begin
  edText.Text := Clipboard.AsText;
  edText.SetFocus;
  edText.SelStart := 0;
  edText.SelLength := 0;
end;

procedure TPJStringPEDlg.actPasteOverUpdate(Sender: TObject);
begin
  {$IFDEF UNICODE}
  actPasteOver.Enabled := ClipBoard.HasFormat(CF_TEXT)
    or Clipboard.HasFormat(CF_UNICODETEXT);
  {$ELSE}
  actPasteOver.Enabled := ClipBoard.HasFormat(CF_TEXT);
  {$ENDIF}
end;

procedure TPJStringPEDlg.actSaveExecute(Sender: TObject);
begin
  if dlgSave.Execute then
    edText.Lines.SaveToFile(dlgSave.FileName);
end;

procedure TPJStringPEDlg.actSelectAllExecute(Sender: TObject);
begin
  edText.SetFocus;
  edText.SelectAll;
end;

procedure TPJStringPEDlg.actSelectAllUpdate(Sender: TObject);
begin
  actSelectAll.Enabled := edText.Text <> '';
end;

procedure TPJStringPEDlg.actUndoExecute(Sender: TObject);
begin
  edText.Perform(EM_UNDO, 0, 0);
end;

procedure TPJStringPEDlg.actUndoUpdate(Sender: TObject);
begin
  actUndo.Enabled := edText.Perform(EM_CANUNDO, 0, 0) <> 0;
end;

procedure TPJStringPEDlg.cbWordWrapClick(Sender: TObject);
begin
  UpdateWordWrap(cbWordWrap.Checked);
end;

procedure TPJStringPEDlg.FormCreate(Sender: TObject);
const
  VistaFontName = 'Segoe UI';
  XPFontName = 'Tahoma';
begin
  if Screen.Fonts.IndexOf(VistaFontName) >= 0 then
  begin
    Font.Name := VistaFontName;
    Font.Size := 9;
  end
  else if Screen.Fonts.IndexOf(XPFontName) >= 0 then
    Font.Name := XPFontName;
end;

procedure TPJStringPEDlg.FormDestroy(Sender: TObject);
var
  WordWrap: Boolean;    // whether editor word wraps
  Pl: TWindowPlacement; // placement of editor window
begin
  // Save word wrap value
  WordWrap := cbWordWrap.Checked;
  SaveSetting(cWordWrapSetting, WordWrap, SizeOf(WordWrap));
  // Save window placement
  FillChar(Pl, 0, SizeOf(Pl));
  Pl.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Self.Handle, @Pl);
  SaveSetting(
    cWindowPlacementSetting, Pl.rcNormalPosition, SizeOf(Pl.rcNormalPosition)
  );
end;

procedure TPJStringPEDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  cShiftKeys = [ssCtrl, ssAlt, ssShift];  // set of modifier keys
begin
  case Key of
    VK_ESCAPE:
      if Shift * cShiftKeys = [] then
        // unmodified ESC key - cancel dialog
        ModalResult := mrCancel;
    VK_RETURN:
    begin
      if Shift * cShiftKeys = [ssCtrl] then
        // CTRL+RETURN - OK the dialog
        ModalResult := mrOK;
    end;
  end;
end;

procedure TPJStringPEDlg.FormShow(Sender: TObject);
var
  WordWrap: Boolean;    // whether editor word wraps
  Pl: TWindowPlacement; // placement of editor window
  WorkArea: TRect;      // desktop work area
begin
  // Get word wrapping setting and update accordingly
  if not ReadSetting(cWordWrapSetting, WordWrap, SizeOf(WordWrap)) then
    WordWrap := False;
  UpdateWordWrap(WordWrap);
  // Get window placement setting and place window accordingly
  FillChar(Pl, SizeOf(Pl), #0);
  if ReadSetting(
    cWindowPlacementSetting, Pl.rcNormalPosition, SizeOf(Pl.rcNormalPosition)
  ) then
  begin
    // we have read settings: position and size window accordingly
    Pl.Length := SizeOf(TWindowPlacement);
    Pl.showCmd := SW_SHOW;      // needed when restore called late in start-up
    SetWindowPlacement(Self.Handle, @Pl);
  end
  else
  begin
    // we have no settings: centre window on Windows workarea
    if SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0) then
    begin
      Left := WorkArea.Left + (WorkArea.Right - WorkArea.Left - Width) div 2;
      Top := WorkArea.Top + (WorkArea.Bottom - WorkArea.Top - Height) div 2;
      if Left < WorkArea.Left then
        Left := WorkArea.Left;
      if Top < WorkArea.Top then
        Top := WorkArea.Top;
    end;
  end;
end;

function TPJStringPEDlg.ReadSetting(const ID: string; var Value;
  const Size: Integer): Boolean;
begin
  with TRegistry.Create do
    try
      try
        Result := OpenKeyReadOnly(cRegKey) and ValueExists(ID);
        if Result then
          Result := ReadBinaryData(ID, Value, Size) = Size;
      except
        Result := False;
      end;
    finally
      Free;
    end;
end;

function TPJStringPEDlg.SaveSetting(const ID: string; var Value;
  const Size: Integer): Boolean;
begin
  with TRegistry.Create do
    try
      try
        Result := OpenKey(cRegKey, True);
        if Result then
          WriteBinaryData(ID, Value, Size);
      except
        Result := False;
      end;
    finally
      Free;
    end;
end;

procedure TPJStringPEDlg.UpdateWordWrap(Flag: Boolean);
const
  // map of word wrap flag to TMemo.Scrollbars property value
  cScrollBars: array[Boolean] of TScrollStyle = (ssBoth, ssVertical);
begin
  // update check box
  cbWordWrap.Checked := Flag;
  // update editor memo control
  edText.WordWrap := Flag;
  edText.ScrollBars := cScrollBars[Flag];
end;

end.

