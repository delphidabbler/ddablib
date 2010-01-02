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
 * Portions created by the Initial Developer are Copyright (C) 2004-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   Richard C Haven (Ctrl+Return and Esc key functionality)
 *
 * ***** END LICENSE BLOCK *****
}


unit PJStringPE;


interface


{ Find if we have a Delphi 6 or higher compiler }
{$DEFINE DELPHI6ANDUP}
{$IFDEF VER80}  {Delphi 1}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER90}  {Delphi 2}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER93}  {C++ Builder 1}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER100} {Delphi 3}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER110} {C++ Builder 3}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER120} {Delphi 4}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER125} {C++ Builder 4}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER130} {Delphi 5}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes, Dialogs, Forms,
  {$IFDEF DELPHI6ANDUP}
    DesignIntf, DesignEditors;
  {$ELSE}
    DsgnIntf;
  {$ENDIF}


type

  {
  TPJStringPEDlg:
    Dialog box used to edit multi-line string properties.
  }
  TPJStringPEDlg = class(TForm)
    edText: TMemo;
    pnlButton: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    bvlFrame: TBevel;
    cbWordWrap: TCheckBox;
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbWordWrapClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    function SaveSetting(const ID: string; var Value;
      const Size: Integer): Boolean;
      {Saves given setting as binary data in registry.
        @param ID name of the value's registry value.
        @param Value untyped value to be written to registry.
        @param Size size of Value in bytes.
        @return True on success or false on error.
      }
    function ReadSetting(const ID: string; var Value;
      const Size: Integer): Boolean;
      {Reads data for a given setting.
        @param ID name of the registry value storing setting data.
        @param Value untyped value to recieve setting data.
        @param Size size of Value in bytes.
        @return True if setting read successfully, false on error.
      }
    procedure UpdateWordWrap(Flag: Boolean);
      {Updates editor's word wrap settings and check box.
        @param Flag whether we require word wrapping.
      }
  end;


  {*
  TPJStringPE:
    Property editor for hot text component's Code property.
  }
  TPJStringPE = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
      {Tell object inspector that editor displays a dialog box in addition to
      other attributes of a standard string property editor.
        @return a set of values describing attributes of property editor.
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
  SysUtils, Windows, Registry;


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
    TPJStringPE);                 // property editor class
  RegisterPropertyEditor(
    TypeInfo(TCaption),           // use for any TCaption component
    nil,                          // use for any component
    '',                           // use for any property
    TPJStringPE);                 // property editor class
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
    @return a set of values describing attributes of property editor.
  }
begin
  Result := inherited GetAttributes + [paDialog];
end;


{ TPJStringPEDlg }

const
  // Registry key where property editors settings are stored
  cRegKey = '\Software\delphiDabbler\Experts\StringPE';

procedure TPJStringPEDlg.btnLoadClick(Sender: TObject);
  {Event handler for "Load" button. Gets file from user and loads
  its contents in editor.
    @param Sender object generating event.
  }
begin
  if dlgOpen.Execute then
    edText.Lines.LoadFromFile(dlgOpen.FileName);
end;

procedure TPJStringPEDlg.btnSaveClick(Sender: TObject);
  {Event handler for "Save" button. Saves contents of editor to file specified
  by user.
    @param Sender object generating event.
  }
begin
  if dlgSave.Execute then
    edText.Lines.SaveToFile(dlgSave.FileName);
end;

procedure TPJStringPEDlg.cbWordWrapClick(Sender: TObject);
  {Event handler for "Word Wrap" check box. Updates word wrapping in editor.
    @param Sender object generating event.
  }
begin
  UpdateWordWrap(cbWordWrap.Checked);
end;

procedure TPJStringPEDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Saves persistent settings.
    @param Sender object generating event.
  }
var
  WordWrap: Boolean;    // Whether editor word wraps
  Pl: TWindowPlacement; // Placement of editor window
begin
  // Save word wrap value
  WordWrap := cbWordWrap.Checked;
  SaveSetting('WordWrap', WordWrap, SizeOf(WordWrap));
  // Save window placement
  FillChar(Pl, 0, SizeOf(Pl));
  Pl.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Self.Handle, @Pl);
  SaveSetting(
    'WindowPlacement', Pl.rcNormalPosition, SizeOf(Pl.rcNormalPosition)
  );
end;

procedure TPJStringPEDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Form key down event handler. Handles ESC and CTRL+RETURN key presses to
  simulate clicking of Cancel and OK buttons respectively.
    @param Sender object generating event.
  }
begin
  case Key of
    VK_ESCAPE:  // ESC key - cancel dialog
      ModalResult := mrCancel;
    VK_RETURN:  // CTRL+RETURN - OK the dialog
    begin
      if Shift = [ssCtrl] then
        ModalResult := mrOK;
    end;
  end;
end;

procedure TPJStringPEDlg.FormShow(Sender: TObject);
  {Form show event handler. Reads persistent settings and applies them.
    @param Sender object generating event.
  }
var
  WordWrap: Boolean;    // whether editor word wraps
  Pl: TWindowPlacement; // placement of editor window
  WorkArea: TRect;      // desktop work area
begin
  // Get word wrapping setting and update accordingly
  if not ReadSetting('WordWrap', WordWrap, SizeOf(WordWrap)) then
    WordWrap := False;
  UpdateWordWrap(WordWrap);
  // Get window placement setting and place window accordingly
  FillChar(Pl, SizeOf(Pl), #0);
  if ReadSetting(
    'WindowPlacement', Pl.rcNormalPosition, SizeOf(Pl.rcNormalPosition)
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
    @param ID name of the registry value storing setting data.
    @param Value untyped value to recieve setting data.
    @param Size size of Value in bytes.
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
    @param ID name of the value's registry value.
    @param Value untyped value to be written to registry.
    @param Size size of Value in bytes.
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
    @param Flag whether we require word wrapping.
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
