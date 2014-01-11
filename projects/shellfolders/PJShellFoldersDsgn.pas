{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2003-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Component registration code and property editor for TPJBrowseDialog and
 * TPJSpecialFolderInfo components defined in PJShellFolders.pas.
}


unit PJShellFoldersDsgn;


interface


// Determine compiler
{$UNDEF DELPHI6ANDUP}
{$UNDEF RTLNameSpaces}      // Don't qualify RTL units names with namespaces
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE RTLNameSpaces}
  {$IFEND}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$DEFINE DELPHI6ANDUP}
  {$IFEND}
{$ENDIF}


uses
  {$IFNDEF RTLNameSpaces}
  // Delphi
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  {$IFDEF DELPHI6ANDUP}
  DesignIntf, DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}


type
  {
  TPJFolderIDPE:
    Property editor for special folder ID properties. Permits choice of
    predefined special folders IDs.
  }
  TPJFolderIDPE = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
      {Tell object inspector that a list of read only values should be
      displayed}
    procedure GetValues(Proc: TGetStrProc); override;
      {Returns all available folder ids for display in drop-down list}
    procedure SetValue(const Value: string); override;
      {Returns actual numeric value for given folder id constant}
    function GetValue: string; override;
      {Returns the name of the folder id constant for the given identifier
      value}
  end;


procedure Register;
  {Delphi component library registration routine}


implementation


uses
  {$IFNDEF RTLNameSpaces}
  // Delphi
  SysUtils, ShlObj,
  {$ELSE}
  System.SysUtils, Winapi.ShlObj,
  {$ENDIF}
  // Project
  PJShellFolders;


// -----------------------------------------------------------------------------
// TPJFolderIDPE
// -----------------------------------------------------------------------------

function TPJFolderIDPE.GetAttributes: TPropertyAttributes;
  {Tell object inspector that a list of read only values should be displayed}
begin
  Result := [paValueList];
end;

function TPJFolderIDPE.GetValue: string;
  {Returns the name of the folder id constant for the given identifier value}
begin
  Result := SpecialFolderIDToStr(GetOrdValue);
end;

procedure TPJFolderIDPE.GetValues(Proc: TGetStrProc);
  {Returns all available folder ids for display in drop-down list}
var
  Enum: IPJSpecialFolderEnum; // enumerator for folder ids
begin
  Enum := TPJSpecialFolderEnum.Create;
  Enum.Init;
  while not Enum.AtEnd do
    Proc(SpecialFolderIdToStr(Enum.Next));
end;

procedure TPJFolderIDPE.SetValue(const Value: string);
  {Returns actual numeric value for given folder id constant}
begin
  SetOrdValue(StrToSpecialFolderID(Value));
end;


// -----------------------------------------------------------------------------
// Delphi registration routine
// -----------------------------------------------------------------------------

procedure Register;
  {Registers components and property editor with Delphi}
begin
  // Register the unit's components
  RegisterComponents('DelphiDabbler', [TPJSpecialFolderInfo, TPJBrowseDialog]);

  // Register special folder id property editor:
  // .. for TPJSpecialFolderInfo.FolderID
  RegisterPropertyEditor(
    TypeInfo(Integer),        // type information about property we edit
    TPJSpecialFolderInfo,     // work with component of this type
    'FolderID',               // only for use with this property
    TPJFolderIDPE);           // property editor class
  // .. for TPJBrowseDlg.RootFolderID
  RegisterPropertyEditor(
    TypeInfo(Integer),        // type information about property we edit
    TPJBrowseDialog,          // work with component of this type
    'RootFolderID',           // only for use with this property
    TPJFolderIDPE);           // property editor class
end;

end.
