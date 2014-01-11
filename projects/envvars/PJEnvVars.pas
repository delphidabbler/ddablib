{
 * PJEnvVars.pas
 *
 * Source code for environment variable routines and component for processing
 * and managing environment variables.
 *
 * Documentation of this unit is at
 * http://www.delphidabbler.com/url/envvars-wiki
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
 * The Original Code is PJEnvVars.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2014 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   e.e (fixed buffer size error in ExpandEnvVars function)
 *
 * ***** END LICENSE BLOCK *****
}


unit PJEnvVars;


// Set conditional symbols & switch off unsafe warnings where supported
{$UNDEF SUPPORTS_EOSERROR}
{$UNDEF HAS_TYPES_UNIT}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    // Switch off unsafe warnings
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$DEFINE SUPPORTS_EOSERROR} // SysUtils defines EOSError
    {$DEFINE HAS_TYPES_UNIT}    // Types unit is available
  {$IFEND}
{$ENDIF}


interface


uses
  // Delphi
  SysUtils, Classes  {$IFDEF HAS_TYPES_UNIT}, Types {$ENDIF};

{$IFNDEF HAS_TYPES_UNIT}
type
  {
  TStringDynArray:
    Dynamic array of strings. Defined here if Types unit not available: Types
    unit defines this type.
  }
  TStringDynArray = array of string;
{$ENDIF}

function GetEnvVarValue(const VarName: string): string;
  {Gets the value of an environment variable.
    @param VarName [in] Name of environment variable.
    @return Environment variable or '' if the variable does not exist.
  }

function SetEnvVarValue(const VarName, VarValue: string): Integer;
  {Sets the value of an environment variable.
    @param VarName [in] Name of environment variable.
    @param VarValue [in] New value of environment variable. Passing '' deletes
      the environment variable.
    @return 0 on success or a Windows error code on error.
  }

function DeleteEnvVar(const VarName: string): Integer;
  {Deletes an environment variable.
    @param VarName [in] Name of environment variable.
    @return 0 on success or a Windows error code on error.
  }

function CreateEnvBlock(const NewEnv: TStrings; const IncludeCurrent: Boolean;
  const Buffer: Pointer; const BufSize: Integer): Integer;
  {Creates a new custom environment block.
    @param NewEnv [in] List of environment variables in Name=Value format to
      include in new environment block. If nil then no new environment variables
      are included.
    @param IncludeCurrent [in] Flag indicating whether environment variables
      from current process are included in the new environment block.
    @param Buffer [in] Buffer that receives new environment block. Pass nil to
      to find required buffer size without creating a block.
    @param BufSize [in] Size of Buffer in bytes. Pass 0 if Buffer is nil.
    @return Size (or required size) of environment block in characters. Multiply
      by SizeOf(Char) to get buffer size in bytes.
  }

function ExpandEnvVars(const Str: string): string;
  {Replaces any environment variables in a string with their values. Environment
  variables should be delimited by % characters thus: %ENVVAR%.
    @param Str [in] String containing environment variables to be replaced.
    @return Converted string.
  }

function GetAllEnvVars(const Vars: TStrings): Integer;
  {Gets all the environment variables available to the current process.
    @param Vars [in] String list that receives all environment variables in
      Name=Value format. Set to nil if all you need is the size of the
      environment block.
    @return Size of environment block that contains all environment variables
      in characters. Multiply by SizeOf(Char) to get size in bytes.
  }

procedure GetAllEnvVarNames(const Names: TStrings); overload;
  {Stores all environment variable names in a string list. Empty names are
  ignored.
    @param Names [in] String list to receive names. Any previous contents are
      discarded.
  }

function GetAllEnvVarNames: TStringDynArray; overload;
  {Creates a string array containing all environment variables. Empty names are
  ignored.
    @return Array of environment variable names.
  }

function EnvBlockSize: Integer;
  {Calculates size of environment block in characters.
    @return Size of environment block in characters. Multiply by SizeOf(Char) to
      get size in bytes.
  }

type

  {
  TPJEnvVarsEnum:
    Callback method type used in TPJEnvVars.EnumNames method: called for
    each environment variable by name, passing user-supplied data.
      @param VarName [in] Name of an environment variable.
      @param Data [in] User-specified pointer as passed to TPJEnvVars.EnumNames.
  }
  TPJEnvVarsEnum = procedure(const VarName: string; Data: Pointer) of object;


  {
  TPJEnvVarsEnumerator:
    Class of enumerator used to enumerator all environment names in TPJEnvVars.
    Provided as an alternative to the EnumNames method of TPJENvVars that
    supports the for..in construct in Delphi 2005 and later.
  }
  TPJEnvVarsEnumerator = class(TObject)
  private
    fEnvVarNames: TStrings; // List of names of env vars being enumerated
    fIndex: Integer;        // Index of current env var in list
  public
    constructor Create;
      {Object constructor. Initialises enumeration.
      }
    destructor Destroy; override;
      {Object destructor. Tidies up enumeration, freeing resources.
      }
    function GetCurrent: string;
      {Gets name of current environment variable name.
        @return Required name.
      }
    function MoveNext: Boolean;
      {Moves to next environment variable name in enumeration.
        @return True if there is a next item, False if beyond last item.
      }
    property Current: string read GetCurrent;
      {Name of current environment variable}
  end;

  {
  TPJEnvVars:
    Component that encapsulates environment variables available to a program,
    permitting access to, and modification of, the variables. Only one instance
    of the component can be placed on a form or owned by another control.
  }
  TPJEnvVars = class(TComponent)
  private
    function GetCount: Integer;
      {Read access method for Count property.
        @return Number of environment variables in current process.
      }
    function GetValue(Name: string): string;
      {Read access method for Values property.
        @param Name [in] Name of environment variable.
        @return Value of environment variable specified by Name.
      }
    procedure SetValue(Name: string; const Value: string);
      {Write access method for Values property.
        @param Name [in] Name of environment variable.
        @param Value [in] Value of environment variable.
        @raise EPJEnvVars if environment variable can't be set.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Object contructor. Ensures only one instance of the component is placed
      on a form or owned by another component.
        @param AOwner [in] Owning component. May be nil if no owner.
      }
    procedure EnumNames(Callback: TPJEnvVarsEnum; Data: Pointer);
      {Enumerates names of all environment variables in current process.
        @param Callback [in] Callback function called once for each environment
          variable, passing name and value of Data parameter.
        @param Data [in] User specified data passed to callback function.
      }
    function GetEnumerator: TPJEnvVarsEnumerator;
      {Creates and returns a new enumerator for all environment variable names.
        @return Required enumerator. Caller is responsible for freeing the
          enumerator instance.
          NOTE: If used by for..in loop (Delphi 2005 and later) compiler takes
          care of freeing the enumerator.
      }
    procedure DeleteVar(const Name: string);
      {Deletes an environment variable.
        @param Name [in] Name of environment variable to delete.
        @except EPJEnvVars raised if environment variable can't be deleted.
      }
    property Count: Integer read GetCount;
      {Count of number of environment variables in current process}
    property Values[Name: string]: string read GetValue write SetValue;
      {Array of values of each environment variable, referenced by the
      variable's name. Referencing an unknown variable returns the empty string.
      Setting a value for an unknown variable creates it. If it is not possible
      to set a variable value then an exception is raised}
  end;

  {
  EPJEnvVars:
    Exception raised by TPJEnvVars when an environment variable error is
    encountered. Class derives from either EOSError if defined in SysUtils or
    EWin32Error otherwise.
  }
  {$IFDEF SUPPORTS_EOSERROR}
  EPJEnvVars = class(EOSError);
  {$ELSE}
  EPJEnvVars = class(EWin32Error);
  {$ENDIF}


procedure Register;
  {Registers component with Delphi.
  }


implementation


uses
  // Delphi
  Windows;


procedure Register;
  {Registers component with Delphi.
  }
begin
  RegisterComponents('DelphiDabbler', [TPJEnvVars]);
end;

function GetEnvVarValue(const VarName: string): string;
  {Gets the value of an environment variable
    @param VarName [in] Name of environment variable
    @return Environment variable or '' if the variable does not exist.
  }
var
  BufSize: Integer;  // size (in chars) of value + terminal #0
begin
  // Get required buffer size (including terminal #0)
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    // Env var exists: read value into result string
    SetLength(Result, BufSize - 1); // space for terminal #0 automatically added
    GetEnvironmentVariable(PChar(VarName), PChar(Result), BufSize);
  end
  else
    // Env var does not exist
    Result := '';
end;

function SetEnvVarValue(const VarName, VarValue: string): Integer;
  {Sets the value of an environment variable.
    @param VarName [in] Name of environment variable.
    @param VarValue [in] New value of environment variable. Passing '' deletes
      the environment variable.
    @return 0 on success or a Windows error code on error.
  }
begin
  if SetEnvironmentVariable(PChar(VarName), PChar(VarValue)) then
    Result := 0
  else
    Result := GetLastError;
end;

function DeleteEnvVar(const VarName: string): Integer;
  {Deletes an environment variable.
    @param VarName [in] Name of environment variable.
    @return 0 on success or a Windows error code on error.
  }
begin
  if SetEnvironmentVariable(PChar(VarName), nil) then
    Result := 0
  else
    Result := GetLastError;
end;

function CreateEnvBlock(const NewEnv: TStrings; const IncludeCurrent: Boolean;
  const Buffer: Pointer; const BufSize: Integer): Integer;
  {Creates a new custom environment block.
    @param NewEnv [in] List of environment variables in Name=Value format to
      include in new environment block. If nil then no new environment variables
      are included.
    @param IncludeCurrent [in] Flag indicating whether environment variables
      from current process are included in the new environment block.
    @param Buffer [in] Buffer that receives new environment block. Pass nil to
      to find required buffer size without creating a block.
    @param BufSize [in] Size of Buffer in bytes. Pass 0 if Buffer is nil.
    @return Size (or required size) of environment block in characters. Multiply
      by SizeOf(Char) to get buffer size in bytes.
  }
var
  EnvVars: TStringList; // list of env vars in new block
  Idx: Integer;         // loops through all env vars in new block
  PBuf: PChar;          // points to start of each env var entry in block
begin
  // Create string list to hold all new environment vars
  EnvVars := TStringList.Create;
  try
    // include copy of current environment block if required
    if IncludeCurrent then
      GetAllEnvVars(EnvVars);
    // add any additional environment strings
    if Assigned(NewEnv) then
      EnvVars.AddStrings(NewEnv);
    // Calculate size of new environment block: block consists of #0 separated
    // list of environment variables terminated by #0#0, e.g.
    // Var1=Value1#0Var2=Value2#0Var3=Value3#0#0
    Result := 0;
    for Idx := 0 to Pred(EnvVars.Count) do
      Inc(Result, Length(EnvVars[Idx]) + 1);  // +1 allows for #0 separator
    Inc(Result);  // allow for terminating #0
    // Check if provided buffer is large enough and create block in it if so
    if (Buffer <> nil) and (BufSize >= Result) then
    begin
      // new environment blocks are always sorted
      EnvVars.Sorted := True;
      // do the copying
      PBuf := Buffer;
      for Idx := 0 to Pred(EnvVars.Count) do
      begin
        StrPCopy(PBuf, EnvVars[Idx]);   // includes terminating #0
        Inc(PBuf, Length(EnvVars[Idx]) + 1);
      end;
      // terminate block with additional #0
      PBuf^ := #0;
    end;
  finally
    EnvVars.Free;
  end;
end;

function ExpandEnvVars(const Str: string): string;
  {Replaces any environment variables in a string with their values. Environment
  variables should be delimited by % characters thus: %ENVVAR%.
    @param Str [in] String containing environment variables to be replaced.
    @return Converted string.
  }
var
  BufSize: Integer; // size of expanded string in chars + terminal #0
begin
  // Get required buffer size (including terminal #0)
  BufSize := ExpandEnvironmentStrings(PChar(Str), nil, 0);
  if BufSize > 0 then
  begin
    // Read expanded string into result string
    SetLength(Result, BufSize - 1); // space for terminal #0 automatically added
    ExpandEnvironmentStrings(PChar(Str), PChar(Result), BufSize);
  end
  else
    // Trying to expand empty string
    Result := '';
end;

function GetAllEnvVars(const Vars: TStrings): Integer;
  {Gets all the environment variables available to the current process.
    @param Vars [in] String list that receives all environment variables in
      Name=Value format. Set to nil if all you need is the size of the
      environment block.
    @return Size of environment block that contains all environment variables
      in characters. Multiply by SizeOf(Char) to get size in bytes.
  }
var
  PEnvVars: PChar;    // pointer to start of environment block
  PEnvEntry: PChar;   // pointer to an environment string in block
begin
  // Clear any list
  if Assigned(Vars) then
    Vars.Clear;
  // Get reference to environment block for this process
  PEnvVars := GetEnvironmentStrings;
  if PEnvVars <> nil then
  begin
    // We have a block: extract strings from it
    // Env strings are #0 separated and list ends with #0#0
    PEnvEntry := PEnvVars;
    try
      while PEnvEntry^ <> #0 do
      begin
        if Assigned(Vars) then
          Vars.Add(PEnvEntry);
        Inc(PEnvEntry, StrLen(PEnvEntry) + 1);
      end;
      // Calculate length of block
      Result := (PEnvEntry - PEnvVars) + 1;
    finally
      // Dispose of the memory block
      FreeEnvironmentStrings(PEnvVars);
    end;
  end
  else
    // No block => zero length
    Result := 0;
end;

procedure GetAllEnvVarNames(const Names: TStrings); overload;
  {Stores all environment variable names in a string list. Empty names are
  ignored.
    @param Names [in] String list to receive names. Any previous contents are
      discarded.
  }
var
  AllEnvVars: TStrings; // list of all environment variables
  Name: string;         // an environment variable name
  Idx: Integer;         // loops thru indices of AllEnvVars
begin
  Assert(Assigned(Names));
  Names.Clear;
  AllEnvVars := TStringList.Create;
  try
    GetAllEnvVars(AllEnvVars);
    for Idx := 0 to Pred(AllEnvVars.Count) do
    begin
      Name := Trim(AllEnvVars.Names[Idx]);
      if Name <> '' then
        Names.Add(Name);
    end;
  finally
    AllEnvVars.Free;
  end;
end;

function GetAllEnvVarNames: TStringDynArray; overload;
  {Creates a string array containing all environment variables. Empty names are
  ignored.
    @return Array of environment variable names.
  }
var
  Names: TStrings;  // string list of all environment variable names
  Idx: Integer;     // loops through indices of Names
begin
  Names := TStringList.Create;
  try
    GetAllEnvVarNames(Names);
    SetLength(Result, Names.Count);
    for Idx := 0 to Pred(Names.Count) do
      Result[Idx] := Names[Idx];
  finally
    Names.Free;
  end;
end;

function EnvBlockSize: Integer;
  {Calculates size of environment block in characters.
    @return Size of environment block in characters. Multiply by SizeOf(Char) to
      get size in bytes.
  }
begin
  Result := GetAllEnvVars(nil); // this function returns required block size
end;

procedure ErrorCheck(Code: Integer);
  {Checks a return value from one of TPJEnvVars methods that return Windows
  error codes.
    @param Code [in] Code to check. Does nothing if 0. If <> 0 exception is
      raised.
    @except EPJEnvVars raised if Code is none-zero and Code stored in exception
      object.
  }
var
  Err: EPJEnvVars;  // reference to exception beinbg raised
begin
  if Code <> 0 then
  begin
    Err := EPJEnvVars.Create(SysErrorMessage(Code));
    Err.ErrorCode := Code;
    raise Err;
  end;
end;

{ TPJEnvVars }

resourcestring
  // Error messages
  sSingleInstanceErr = 'Only one %s component is permitted on a form: ' +
    '%0:s is already present on %1:s';

constructor TPJEnvVars.Create(AOwner: TComponent);
  {Object contructor. Ensures only one instance of the component is placed on a
  form, or owned by another component.
    @param AOwner [in] Owning component. May be nil if no owner.
  }
var
  Idx: Integer; // loops thru components on Owner form
begin
  if Assigned(AOwner) then
  begin
    // Ensure that component is unique
    for Idx := 0 to Pred(AOwner.ComponentCount) do
      if AOwner.Components[Idx] is ClassType then
        raise Exception.CreateFmt(sSingleInstanceErr,
          [ClassName, AOwner.Components[Idx].Name, AOwner.Name]);
  end;
  // All OK: go ahead and create component
  inherited;
end;

procedure TPJEnvVars.DeleteVar(const Name: string);
  {Deletes an environment variable.
    @param Name [in] Name of environment variable to delete.
    @except EPJEnvVars raised if environment variable can't be deleted.
  }
begin
  ErrorCheck(DeleteEnvVar(Name));
end;

procedure TPJEnvVars.EnumNames(Callback: TPJEnvVarsEnum; Data: Pointer);
  {Enumerates names of all environment variables in current process.
    @param Callback [in] Callback function called once for each environment
      variable, passing name and value of Data parameter.
    @param Data [in] User specified data passed to callback function.
  }
var
  Idx: Integer;         // loops thru env var list
  EnvList: TStringList; // list of env vars in form NAME=VALUE
begin
  Assert(Assigned(Callback));
  // Create list to hold env vars
  EnvList := TStringList.Create;
  try
    // Get environment and call callback with name of each variable
    GetAllEnvVars(EnvList);
    // Call callback proc for each name in environment, with user supplied data
    for Idx := 0 to Pred(EnvList.Count) do
      Callback(EnvList.Names[Idx], Data);
  finally
    EnvList.Free;
  end;
end;

function TPJEnvVars.GetCount: Integer;
  {Read access method for Count property.
    @return Number of environment variables in current process.
  }
var
  EnvList: TStringList; // list of all environment variables
begin
  EnvList := TStringList.Create;
  try
    GetAllEnvVars(EnvList);
    Result := EnvList.Count;
  finally
    EnvList.Free;
  end;
end;

function TPJEnvVars.GetEnumerator: TPJEnvVarsEnumerator;
  {Creates and returns a new enumerator for all environment variable names.
    @return Required enumerator. Caller is responsible for freeing the
      enumerator instance.
      NOTE: If used by for..in loop (Delphi 2005 and later) compiler takes care
      of freeing the enumerator.
  }
begin
  Result := TPJEnvVarsEnumerator.Create;
end;

function TPJEnvVars.GetValue(Name: string): string;
  {Read access method for Values property.
    @param Name [in] Name of environment variable.
    @return Value of environment variable specified by Name.
  }
begin
  Result := GetEnvVarValue(Name);
end;

procedure TPJEnvVars.SetValue(Name: string; const Value: string);
  {Write access method for Values property.
    @param Name [in] Name of environment variable.
    @param Value [in] Value of environment variable.
    @raise EPJEnvVars if environment variable can't be set.
  }
begin
  ErrorCheck(SetEnvVarValue(Name, Value));
end;

{ TPJEnvVarsEnumerator }

constructor TPJEnvVarsEnumerator.Create;
  {Object constructor. Initialises enumeration.
  }
begin
  fEnvVarNames := TStringList.Create;
  GetAllEnvVarNames(fEnvVarNames);
  fIndex := -1;
end;

destructor TPJEnvVarsEnumerator.Destroy;
  {Object destructor. Tidies up enumeration, freeing resources.
  }
begin
  fEnvVarNames.Free;
  inherited;
end;

function TPJEnvVarsEnumerator.GetCurrent: string;
  {Gets name of current environment variable name.
    @return Required name.
  }
begin
  Result := fEnvVarNames[fIndex];
end;

function TPJEnvVarsEnumerator.MoveNext: Boolean;
  {Moves to next environment variable name in enumeration.
    @return True if there is a next item, False if beyond last item.
  }
begin
  Result := fIndex < Pred(fEnvVarNames.Count);
  if Result then
    Inc(fIndex);
end;

end.

