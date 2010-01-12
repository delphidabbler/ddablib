{
 * PJDropFilesDsgn.pas
 *
 * Component and property editors and component registration for Drop Files
 * Components.
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
 * The Original Code is PJDropFilesDsgn.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 1998-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit PJDropFilesDsgn;


interface


// Find if we have a Delphi 6 or higher compiler
{$UNDEF DELPHI4ANDUP}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$DEFINE DELPHI6ANDUP}
  {$IFEND}
{$ENDIF}


uses
  {$IFDEF DELPHI6ANDUP}
  DesignIntf, DesignEditors, TopLevels,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  Forms, StdCtrls, Classes, Controls;


type
  {
  TPJExtFileFilterExtPEDlg:
    Implements the dialog box used by the TPJExtFileFilterExtPE property editor.
  }
  TPJExtFileFilterExtPEDlg = class(TForm)
    edExtension: TEdit;
    btnAdd: TButton;
    btnDelete: TButton;
    lbExtensions: TListBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure edExtensionChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure lbExtensionsClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject); 
  private
    function GetExtensions: string;
      {Return ; separated list of extensions from list box.
        @return the string of extensions.
      }
    procedure SetExtensions(const Value: string);
      {Store extensions in list box from given ; separated list.
        @param Value is a ; separated list of extensions.
      }
  public
    property Extensions: string read GetExtensions write SetExtensions;
      {';' delimited list of extensions to be edited}
  end;

  {
  TPJExtFileFilterExtPE:
    Property editor for the Extensions property of the TPJExtFileFilter
    component that displays a dialog box where extensions can be added and
    removed and saved in the correct ; delimited format.
  }
  TPJExtFileFilterExtPE = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
      {Tell object inspector that editor displays a dialog box in addition to
      other attributes of a standard string property editor.
        @return a set of values describing attributes of property editor.
      }
    procedure Edit; override;
      {Display property editor dialog box to edit the property.}
  end;

  {
  TPJDropFilesCE:
    Component editor that causes an event handler for the OnDropFiles event of
    the TPJDropFiles and TPJFormDropFiles components to be opened in the
    designer when the components are double-clicked. The default action without
    this component editor is to open the rarely used OnBeforeDrop event handler,
    which is not very useful.
  }
  TPJDropFilesCE = class(TDefaultEditor)
  protected
    procedure EditProperty(
      {$IFDEF DELPHI6ANDUP}
      const PropertyEditor: IProperty; var Continue: Boolean
      {$ELSE}
      PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean
      {$ENDIF}
      ); override;
      {Gets called (indirectly) by component Edit method for each property
      editor of the component. Ignores all but OnDropFiles property editor which
      is passed to inherited method to record as the default proptery editor to
      be used when the component is double clicked.
        @param PropertyEditor represents a property editor for a component
        property or event to be checked.
        @param Continue whether to continue to iterate through the properties.
        [@param FreeEditor (Delphi <= 4 only) shows whether to free the property
        editor instance.]
      }
  end;


procedure Register;
  {Registers the components and component/property editors with Delphi}


implementation


uses
  // Delphi
  SysUtils, TypInfo,
  // DelphiDabbler library
  PJDropFiles;

{$R *.DFM}

{ Helper routines: from the CodeSnip database at www.delphidabbler.com }

function SplitStr(const S: string; Delim: Char; out S1, S2: string): Boolean;
  {Splits the string S at the first occurence of delimiter character Delim and
  sets S1 to the sub-string before Delim and S2 to substring following Delim.
  If Delim is found in string True is returned, while if Delim is not in string
  False is returned, S1 is set to S and S2 is set to ''.}
var
  DelimPos: Integer;  // position of delimiter in source string
begin
  // Find position of first occurence of delimter in string
  DelimPos := SysUtils.AnsiPos(Delim, S);
  if DelimPos > 0 then
  begin
    // Delimiter found: do split and return True
    S1 := Copy(S, 1, DelimPos - 1);
    S2 := Copy(S, DelimPos + 1, MaxInt);
    Result := True;
  end
  else
  begin
    // Delimeter not found: return false and set S1 to whole string
    S1 := S;
    S2 := '';
    Result := False;
  end;
end;

function ExplodeStr(S: string; const Delim: Char; const List: Classes.TStrings;
  const AllowEmpty: Boolean): Integer;
  {Splits the string S into a list of strings, separated by Delim, and returns
  the number of strings in the list. If AllowEmpty is true then any empty
  strings are added to the list, while they are ignored if AllowEmpty is
  false.}
var
  Item: string;       // current delimted text
  Remainder: string;  // remaining unconsumed part of string
begin
  // Clear the list
  List.Clear;
  // Check we have some entries in the string
  if S <> '' then
  begin
    // Repeatedly split string until we have no more entries
    while SplitStr(S, Delim, Item, Remainder) do
    begin
      // Add the current string, is required
      if (Item <> '') or AllowEmpty then
        List.Add(Item);
      // Go round again with remainder of string
      S := Remainder;
    end;
    // Add any terminal item
    if (Item <> '') or AllowEmpty then
      List.Add(Item);
  end;
  // Return number of items read
  Result := List.Count;
end;

function JoinStr(const SL: Classes.TStrings; const Delim: Char;
  const AllowEmpty: Boolean): string;
  {Joins all strings in given string list together into single string separated
  by given delimiter. If AllowEmpty is true then any empty strings are included
  in output string, but are ignored if false.}
var
  Idx: Integer; // loops thru all items in string list
begin
  Result := '';
  for Idx := 0 to Pred(SL.Count) do
  begin
    if (SL[Idx] <> '') or AllowEmpty then
      if Result = '' then
        Result := SL[Idx]
      else
        Result := Result + Delim + SL[Idx];
  end;
end;

{ TPJExtFileFilterExtPEDlg }

procedure TPJExtFileFilterExtPEDlg.btnAddClick(Sender: TObject);
  {Handle click on Add button to add new extension to list}
var
  Ext: string;  // an extension
begin
  // Get new extension from edit box and "normalise" ie
  Ext := edExtension.Text;
  if AnsiPos('.', Ext) <> 1 then
    Ext := '.' + Ext;
  // Add extension to list if not already present and not blank
  if (Ext <> '.') and (lbExtensions.Items.IndexOf(edExtension.Text) = -1) then
  begin
    lbExtensions.Items.Add(Ext);
    // Clear extension edit box and return focus to it
    edExtension.Text := '';
    edExtension.SetFocus;
  end;
end;

procedure TPJExtFileFilterExtPEDlg.btnDeleteClick(Sender: TObject);
  {Handle click on Delete button by removing highlighted extension from list}
var
  SelIdx: Integer;  // index of selected list item
begin
  // Get selected list item and check their is one
  SelIdx := lbExtensions.ItemIndex;
  if SelIdx >= 0 then
  begin
    // Delete the extension from list
    lbExtensions.Items.Delete(SelIdx);
    // Select next item in list
    if SelIdx = lbExtensions.Items.Count then
      Dec(SelIdx);
    lbExtensions.ItemIndex := SelIdx;
    // Enable/disable Delete button according to if there's a selected item
    btnDelete.Enabled := SelIdx >= 0;
  end;
end;

procedure TPJExtFileFilterExtPEDlg.edExtensionChange(Sender: TObject);
  {Handle text changes in extension edit control}
begin
  // Enable / disable Add button according to if there is text in this box
  btnAdd.Enabled := edExtension.Text <> '';
end;

function TPJExtFileFilterExtPEDlg.GetExtensions: string;
  {Return ; separated list of extensions from list box}
begin
  // Get extension list from list box
  Result := JoinStr(lbExtensions.Items, ';', False);
end;

procedure TPJExtFileFilterExtPEDlg.lbExtensionsClick(Sender: TObject);
  {Handle click on item in extension list: enable Delete button}
begin
  btnDelete.Enabled := True;
end;

procedure TPJExtFileFilterExtPEDlg.SetExtensions(const Value: string);
  {Store extensions in list box from given ; separated list}
var
  ExtStr: string; // normalised list of extensions
  Filter: TPJExtFileFilter; // extension filter comp used to normalise Value
begin
  // Normalise the extension list using filter component
  Filter := TPJExtFileFilter.Create(nil);
  try
    Filter.Extensions := Value;    // writing property normalises
    ExtStr := Filter.Extensions;   // read out normalised string
  finally
    Filter.Free;
  end;
  // Store extensions in list box
  lbExtensions.Clear;
  ExplodeStr(ExtStr, ';', lbExtensions.Items, False);
end;

{ TPJExtFileFilterExtPE }

procedure TPJExtFileFilterExtPE.Edit;
  {Display property editor dialog box to edit the property.}
begin
  // Create property editor form
  with TPJExtFileFilterExtPEDlg.Create(Application) do
    try
      // Copy current property value to editor dialog
      Extensions := GetStrValue;
      // Display dialog and update property if user OKs
      if ShowModal = mrOK then
        SetStrValue(Extensions);
    finally
      Free;
    end;
end;

function TPJExtFileFilterExtPE.GetAttributes: TPropertyAttributes;
  {Tell object inspector that editor displays a dialog box in addition to
  other attributes of a standard string property editor}
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TPJDropFilesCE }

procedure TPJDropFilesCE.EditProperty(
  {$IFDEF DELPHI6ANDUP}
  const PropertyEditor: IProperty; var Continue: Boolean
  {$ELSE}
  PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean
  {$ENDIF}
  );
  {Gets called (indirectly) by component Edit method for each property editor
  of the component. Ignores all but OnDropFiles property editor which is passed
  to inherited method to record as the default proptery editor to be used when
  the component is double clicked}
begin
  if CompareText(PropertyEditor.GetName, 'OnDropFiles') = 0 then
    inherited;
end;

{ Component, property editor and component editor registration }

procedure Register;
  {Registers the components and component/property editors with Delphi}
begin
  // Register the components
  RegisterComponents(
    'DelphiDabbler',
    [TPJDropFiles, TPJFormDropFiles, TPJCtrlDropFiles,
    TPJExtFileFilter, TPJWildCardFileFilter]
  );
  // Register component editor for both components with OnDropFiles event
  RegisterComponentEditor(TPJDropFiles, TPJDropFilesCE);
  RegisterComponentEditor(TPJFormDropFiles, TPJDropFilesCE);
  RegisterComponentEditor(TPJCtrlDropFiles, TPJDropFilesCE);
  // Register property editor for TPJExtFileFilter.Extensions property
  RegisterPropertyEditor(
    TypeInfo(string),             // use for any string component
    TPJExtFileFilter,             // use only for this component
    'Extensions',                 // and this property
    TPJExtFileFilterExtPE         // property editor class
  );
  // Restore standard string editor for TPJWildCardFileFilter.WildCard property
  // (only has effect if string property editor has been replaced by
  // DelphiDabbler extended string property editor or similar. We do this to
  // prevent line breaks being entered in wild card)
  RegisterPropertyEditor(
    TypeInfo(string),             // use for any string component
    TPJWildCardFileFilter,        // use only for this component
    'WildCard',                   // and this property
    TStringProperty               // property editor class
  );
end;

end.

