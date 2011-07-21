{
 * PJStringPE.pas
 *
 * Extended String Property Editor source code. Property editor for the Delphi
 * IDE that enables multi-line strings to be edited and assigned to component
 * string properties at design time.
 *
 * $Rev$
 * $Date$
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
 * The Original Code is PJStringPE.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2004-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   Richard C Haven (Ctrl+Return and Esc key functionality)
 *   Bino (some of copy, paste, select and clear functionality)
 *
 * ***** END LICENSE BLOCK *****
}


unit PJStringPE;


interface


{ Find if we have a Delphi 6 or higher compiler }
{$UNDEF DELPHI6ANDUP}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and higher
    {$DEFINE DELPHI6ANDUP}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and higher
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
{$ENDIF}


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes, Dialogs, Forms, ImgList, ComCtrls,
  ToolWin, ActnList,
  {$IFDEF DELPHI6ANDUP}
  DesignIntf, DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}


type

  {
  TPJStringPEDlg:
    Dialog box used to edit multi-line string properties. Instantiated by
    TPJStringPE.
  }
  TPJStringPEDlg = class(TForm)
    edText: TMemo;
    pnlButton: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    toolBar: TToolBar;
    tbSelectAll: TToolButton;
    imageList: TImageList;
    tbSave: TToolButton;
    tbLoad: TToolButton;
    tbClearText: TToolButton;
    tbPasteOver: TToolButton;
    tbCopyAll: TToolButton;
    actionList: TActionList;
    actLoad: TAction;
    actSave: TAction;
    actClear: TAction;
    actSelectAll: TAction;
    actPasteOver: TAction;
    actCopyAll: TAction;
    tbSeparator1: TToolButton;
    tbSeparator2: TToolButton;
    cbWordWrap: TCheckBox;
    tbUndo: TToolButton;
    actUndo: TAction;
    tbSeparator3: TToolButton;
    procedure cbWordWrapClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actLoadExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actCopyAllExecute(Sender: TObject);
    procedure actPasteOverExecute(Sender: TObject);
    procedure actPasteOverUpdate(Sender: TObject);
    procedure actCopyAllUpdate(Sender: TObject);
    procedure actSelectAllUpdate(Sender: TObject);
    procedure actClearUpdate(Sender: TObject);
    procedure actUndoUpdate(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
  private
    function SaveSetting(const ID: string; var Value;
      const Size: Integer): Boolean;
      {Saves given setting as binary data in registry.
        @param ID [in] Name of the value's registry value.
        @param Value [in/out] Untyped value to be written to registry.
          Unmodified (var parameter type required by referenced TRegistry
          method).
        @param Size [in] Size of Value in bytes.
        @return True on success or false on error.
      }
    function ReadSetting(const ID: string; var Value;
      const Size: Integer): Boolean;
      {Reads data for a given setting.
        @param ID [in] Name of the registry value storing setting data.
        @param Value [in/out] Untyped value that recieves setting data. Any
          existing input value is overwritten.
        @param Size [in] Size of Value in bytes.
        @return True if setting read successfully, false on error.
      }
    procedure UpdateWordWrap(Flag: Boolean);
      {Updates editor's word wrap settings and check box.
        @param Flag [in] Indicates whether we require word wrapping.
      }
  end;


  {
  TPJStringPE:
    Extended string property editor.
  }
  TPJStringPE = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
      {Tell object inspector that editor displays a dialog box in addition to
      other attributes of a standard string property editor.
        @return A set of values describing attributes of property editor.
      }
    procedure Edit; override;
      {Display property editor dialog box to edit the property.
      }
  end;


procedure Register;
  {Registers property editor for all string and TCaption properties of all
  components.
  }


implementation


uses
  // Delphi
  SysUtils, Windows, Registry, ClipBrd, Messages;


{$R *.DFM}    // links the property editor form


procedure Register;
  {Registers property editor for all string and TCaption properties of all
  components.
  }
begin
  RegisterPropertyEditor(
    TypeInfo(string),             // use for any string component
    nil,                          // use for any component
    '',                           // use for any property
    TPJStringPE                   // property editor class
  );
  RegisterPropertyEditor(
    TypeInfo(TCaption),           // use for any TCaption component
    nil,                          // use for any component
    '',                           // use for any property
    TPJStringPE                   // property editor class
  );
end;


{ TPJStringPE }

procedure TPJStringPE.Edit;
  {Display property editor dialog box to edit the property.
  }
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
  {Tell object inspector that editor displays a dialog box in addition to other
  attributes of a standard string property editor.
    @return A set of values describing attributes of property editor.
  }
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

procedure TPJStringPEDlg.actClearUpdate(Sender: TObject);
begin
  actClear.Enabled := edText.Text <> '';
end;

procedure TPJStringPEDlg.actCopyAllExecute(Sender: TObject);
begin
  Clipboard.AsText := edText.Text;
end;

procedure TPJStringPEDlg.actCopyAllUpdate(Sender: TObject);
begin
  actCopyAll.Enabled := edText.Text <> '';
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
  {Event handler for "Word Wrap" check box. Updates word wrapping in editor.
    @param Sender [in] Object generating event. Not used.
  }
begin
  UpdateWordWrap(cbWordWrap.Checked);
end;

procedure TPJStringPEDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Saves persistent settings.
    @param Sender [in] Object generating event. Not used.
  }
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
  {Form key down event handler. Handles unmodified ESC and CTRL+RETURN key
  presses to simulate clicking of Cancel and OK buttons respectively.
    @param Sender [in] Object generating event. Not used.
    @param Key [in/out] Code of key pressed. Unchanged.
    @param Shift [in] Combination of shift keys pressed.
  }
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
  {Form show event handler. Reads persistent settings and applies them.
    @param Sender [in] Object generating event. Not used.
  }
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
  {Reads data for a given setting.
    @param ID [in] Name of the registry value storing setting data.
    @param Value [in/out] Untyped value that recieves setting data. Any existing
      input value is overwritten.
    @param Size [in] Size of Value in bytes.
    @return True if setting read successfully, false on error.
  }
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
  {Saves given setting as binary data in registry.
    @param ID [in] Name of the value's registry value.
    @param Value [in/out] Untyped value to be written to registry. Unmodified
      (var parameter type required by referenced TRegistry method).
    @param Size [in] Size of Value in bytes.
    @return True on success or false on error.
  }
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
  {Updates editor's word wrap settings and check box.
    @param Flag [in] Indicates whether we require word wrapping.
  }
const
  // map of flag to TMemo.Scrollbars property value
  cScrollBars: array[Boolean] of TScrollStyle = (ssBoth, ssVertical);
begin
  // update check box
  cbWordWrap.Checked := Flag;
  // update editor memo control
  edText.WordWrap := Flag;
  edText.ScrollBars := cScrollBars[Flag];
end;

end.

