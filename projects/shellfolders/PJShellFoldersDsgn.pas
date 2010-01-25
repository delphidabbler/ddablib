{
 * PJShellFoldersDsgn.pas
 *
 * Component registration code and property editor for TPJBrowseDialog and
 * TPJSpecialFolderInfo components defined in PJShellFolders.pas.
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
 * The Original Code is PJShellFoldersDsgn.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2003-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   Philippe Lucarz
 *   Ryan Fischbach
 *
 * ***** END LICENSE BLOCK *****
}


unit PJShellFoldersDsgn;


interface


// Determine compiler
{$DEFINE DELPHI6ANDUP}
{$DEFINE DELPHI7ANDUP}
{$IFDEF VER80}  {Delphi 1}
  {$UNDEF DELPHI6ANDUP}
  {$UNDEF DELPHI7ANDUP}
{$ENDIF}
{$IFDEF VER90}  {Delphi 2}
  {$UNDEF DELPHI6ANDUP}
  {$UNDEF DELPHI7ANDUP}
{$ENDIF}
{$IFDEF VER93}  {C++ Builder 1}
  {$UNDEF DELPHI6ANDUP}
  {$UNDEF DELPHI7ANDUP}
{$ENDIF}
{$IFDEF VER100} {Delphi 3}
  {$UNDEF DELPHI6ANDUP}
  {$UNDEF DELPHI7ANDUP}
{$ENDIF}
{$IFDEF VER110} {C++ Builder 3}
  {$UNDEF DELPHI6ANDUP}
  {$UNDEF DELPHI7ANDUP}
{$ENDIF}
{$IFDEF VER120} {Delphi 4}
  {$UNDEF DELPHI6ANDUP}
  {$UNDEF DELPHI7ANDUP}
{$ENDIF}
{$IFDEF VER125} {C++ Builder 4}
  {$UNDEF DELPHI6ANDUP}
  {$UNDEF DELPHI7ANDUP}
{$ENDIF}
{$IFDEF VER130} {Delphi 5}
  {$UNDEF DELPHI6ANDUP}
  {$UNDEF DELPHI7ANDUP}
{$ENDIF}
{$IFDEF VER140} {Delphi 6}
  {$UNDEF DELPHI7ANDUP}
{$ENDIF}


uses
  // Delphi
  Classes,
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
  // Delphi
  SysUtils, ShlObj,
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
