{ ##
  @PROJECT_NAME             HKEY Property Editor
  @PROJECT_DESC             Property editor for properties of type HKEY: i.e.
                            registry root keys. Provides a drop down list of all
                            root keys for which Delphi provides constants in
                            Windows.pas.
  @FILE                     PJHKEYPropEdit.pas
  @COMMENTS                 Property editor source code.
  @AUTHOR                   Peter Johnson, LLANARTH, Ceredigion, Wales, UK
  @EMAIL                    peter.johnson@openlink.org
  @WEBSITE                  http://www.delphidabbler.com/
  @COPYRIGHT                © Peter D Johnson, 2000-2003.
  @LEGAL_NOTICE             This property editor and its source code are placed
                            in the public domain. They may be freely copied and
                            circulated on a not for profit basis providing that
                            the code is unmodified and this notice and
                            information about the author and his copyright
                            remains attached to the source code.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 28/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.0.1
      @DATE                 08/10/2000
      @COMMENTS             Fixed bug which was allowing user to enter an
                            unknown root key by name (or to delete current
                            value). This was causing a root key of 0 to be used.
                            Now whenever an invalid value is entered it is
                            replaced by HKEY_CURRENT_USER.
    )
    @REVISION(
      @VERSION              1.0.2
      @DATE                 30/10/2003
      @COMMENTS             Added conditional compilation to allow for changes
                            to unit names for units defining property editor
                            classes and associated types from Delphi 6.
    )
    @REVISION(
      @VERSION              1.0.3
      @DATE                 16/12/2005
      @COMMENTS             Fixed error where DELPHI6ANDUP compiler define was
                            not defined on Delphi 2005 and later leading to
                            compilation failure.
    )
  )
}


{
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
 * The Original Code is PJHKEYPropEdit.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit PJHKeyPropEdit;


interface


// Find if we have a Delphi 6 or higher compiler
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
  {$IFDEF DELPHI6ANDUP}
    DesignIntf, DesignEditors,
  {$ELSE}
    DsgnIntf,
  {$ENDIF}
  Classes;


type

  {
  TPJHKEYPropEditor:
    Delphi property editor that allows registry root keys to be selected from a
    list of symbols.
  }
  TPJHKEYPropEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
      {Tell object inspector that a list of values should be displayed}
    procedure GetValues(Proc: TGetStrProc); override;
      {Returns all available HKEY values for display in drop-down list}
    procedure SetValue(const Value: string); override;
      {Returns actual numeric value for given symbolic value}
    function GetValue: string; override;
      {Returns the symbolic value for the currently selected value}
  end;


procedure Register;
  {Delphi registration routine}


implementation


uses
  // Delphi
  Windows;


{ Delphi registration routine }

procedure Register;
  {Register the property editor with Delphi}
begin
  RegisterPropertyEditor(
    TypeInfo(HKEY),               // type information about property we edit
    nil,                          // work with any component with this prop type
    '',                           // properties can have any name
    TPJHKEYPropEditor);           // property editor class
end;


{ Helper functions and lookup table }

const
  cNumKeys = 7;
    {Number of root keys we're supporting}
  cLookup: array[1..cNumKeys] of record
    {Table mapping symbolic representation of root keys to their values}
    Str: string;
    Code: HKEY;
  end =
  (
    (Str: 'HKEY_CLASSES_ROOT';      Code: HKEY_CLASSES_ROOT),
    (Str: 'HKEY_CURRENT_USER';      Code: HKEY_CURRENT_USER),
    (Str: 'HKEY_LOCAL_MACHINE';     Code: HKEY_LOCAL_MACHINE),
    (Str: 'HKEY_USERS';             Code: HKEY_USERS),
    (Str: 'HKEY_PERFORMANCE_DATA';  Code: HKEY_PERFORMANCE_DATA),
    (Str: 'HKEY_CURRENT_CONFIG';    Code: HKEY_CURRENT_CONFIG),
    (Str: 'HKEY_DYN_DATA';          Code: HKEY_DYN_DATA)
  );

function StrToCode(Str: string): HKEY;
  {Returns the key represented by the given symbolic name}
var
  I: Integer;   // loops through all keys
begin
  Result := HKEY_CURRENT_USER;
  for I := 1 to cNumKeys do
    if cLookup[I].Str = Str then
    begin
      Result := cLookup[I].Code;
      Break;
    end;
end;

function CodeToStr(Code: HKEY): string;
  {Returns the symbolic name associated with the given key}
var
  I: Integer;   // loops through all keys
begin
  Result := '';
  for I := 1 to cNumKeys do
    if cLookup[I].Code = Code then
    begin
      Result := cLookup[I].Str;
      Break;
    end;
end;


{ TPJHKEYPropEditor }

function TPJHKEYPropEditor.GetAttributes: TPropertyAttributes;
  {Tell object inspector that a list of values should be displayed}
begin
  Result := [paValueList];
end;

function TPJHKEYPropEditor.GetValue: string;
  {Returns the symbolic value for the currently selected value}
begin
  // Get symbolic value from lookup table
  Result := CodeToStr(GetOrdValue);
end;

procedure TPJHKEYPropEditor.GetValues(Proc: TGetStrProc);
  {Returns all available HKEY values for display in drop-down list}
var
  I: Integer;   // loops through all symbols
begin
  for I := 1 to cNumKeys do
    Proc(cLookup[I].Str);
end;

procedure TPJHKEYPropEditor.SetValue(const Value: string);
  {Returns actual numeric value for given symbolic value}
begin
  // Get numeric value from lookup table
  SetOrdValue(StrToCode(Value));
end;

end.