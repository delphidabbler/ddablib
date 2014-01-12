{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2000-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Property editor for properties of type HKEY: i.e. registry root keys.
 * Provides a drop down list of all root keys for which Delphi provides
 * constants in Windows.pas.
}


unit PJHKeyPropEdit;


interface


// Find if we have a Delphi 6 or higher compiler
{$UNDEF NewDesignUnits}
{$UNDEF RTLNameSpaces}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE RTLNameSpaces}
  {$IFEND}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$DEFINE NewDesignUnits}
  {$IFEND}
{$ENDIF}


uses
  // Delphi
  {$IFDEF NewDesignUnits}
    DesignIntf, DesignEditors,
  {$ELSE}
    DsgnIntf,
  {$ENDIF}
  {$IFNDEF RTLNameSpaces}
  Classes;
  {$ELSE}
  System.Classes;
  {$ENDIF}


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
  {$IFNDEF RTLNameSpaces}
  Windows;
  {$ELSE}
  Winapi.Windows;
  {$ENDIF}

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
