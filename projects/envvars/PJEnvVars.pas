{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * DelphiDabbler Environment Variables unit. Contains a component and several
 * routines and component for processing and managing environment variables.
 *
 * Documented at http://www.delphidabbler.com/url/envvars-docs
 *
 * ACKNOWLEDGEMENTS
 *
 * Thanks to "e.e" for bug fix in v1.3.2
 *
 * ***** END LICENSE BLOCK *****
}


unit PJEnvVars;


// Set conditional symbols & switch off unsafe warnings where supported
{$UNDEF Supports_EOSError}
{$UNDEF Has_Types_Unit}
{$UNDEF Supports_ENoConstructException}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 20.0} // Delphi 2009 and later
    {$DEFINE Supports_ENoConstructException}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    // Switch off unsafe warnings
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$DEFINE Supports_EOSError} // SysUtils defines EOSError
    {$DEFINE Has_Types_Unit}    // Types unit is available
  {$IFEND}
{$ENDIF}


interface


uses
  // Delphi
  SysUtils, Classes  {$IFDEF Has_Types_Unit}, Types {$ENDIF};

{$IFNDEF Has_Types_Unit}
type
  {
  TStringDynArray:
    Dynamic array of strings. Defined here if Types unit not available: Types
    unit defines this type.
  }
  TStringDynArray = array of string;
{$ENDIF}

{$IFNDEF Supports_ENoConstructException}
type
  ENoConstructException = class(Exception);
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
    @param BufSize [in] Size of Buffer in characters. Pass 0 if Buffer is nil.
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

  TPJEnvironmentVar = record
    Name: string;
    Value: string;
  end;

  TPJEnvironmentVarArray = array of TPJEnvironmentVar;

  {
  TPJEnvVarsEnum:
    Callback method type used in TPJEnvVars.EnumNames method: called for
    each environment variable by name, passing user-supplied data.
      @param VarName [in] Name of an environment variable.
      @param Data [in] User-specified pointer as passed to TPJEnvVars.EnumNames.
  }
  TPJEnvVarsEnum = procedure(const VarName: string; Data: Pointer) of object;

  TPJEnvVarsEnumEx = procedure(const EnvVar: TPJEnvironmentVar; Data: Pointer)
    of object;

  ///  <summary>Static class providing class methods for interogating,
  ///  manipulating and modifying the environment variables available to the
  ///  current process.</summary>
  ///  <remarks>This class cannot be constructed.</remarks>
  TPJEnvironmentVars = class(TObject)
  public
    ///  <summary>Prevents construction of instances of this static class.
    ///  </summary>
    ///  <exception>Always raises ENoConstructException when called.</exception>
    constructor Create;
    ///  <summary>Returns the number of environment variables in the current
    ///  process.</summary>
    class function Count: Integer;
    ///  <summary>Returns the value of the environment variable with the given
    ///  name.</summary>
    class function GetValue(const VarName: string): string;
    ///  <summary>Sets the value of the environment variable with the given name
    ///  to the given value.</summary>
    ///  <remarks>Returns 0 on success or a Windows error code on failure.
    ///  </remarks>
    class function SetValue(const VarName, VarValue: string): Integer;
    ///  <summary>Deletes the environment variable with the given name.
    ///  </summary>
    ///  <remarks>Returns 0 on success or a Windows error code on failure.
    ///  </remarks>
    class function Delete(const VarName: string): Integer;
    ///  <summary>Creates a new custom environment block.</summary>
    ///  <param name="NewEnv">TStrings [in] List of environment variables in
    ///  Name=Value format to be included in the new environment block. If nil
    ///  then no new environment variables are included in the block.</param>
    ///  <param name="IncludeCurrent">Boolean [in] Flag indicating whether the
    ///  environment variables from the current process are included in the new
    ///  environment block.</param>
    ///  <param name="Buffer">Pointer [in] Pointer to a memory block that
    ///  receives the new environment block. If nil then no block is created. If
    ///  none-nil the buffer must be at least BufSize * SizeOf(Char) bytes.
    ///  </param>
    ///  <param name="BufSize"> [in] The number of characters that can be stored
    ///  in the memory pointed to by Buffer or if Buffer is nil.</param>
    ///  <returns>Integer. The size of the environment block in characters. If
    ///  Buffer is nil this is the required size of the buffer, in characters.
    ///  Multiply this value by SizeOf(Char) to find the required buffer size in
    ///  bytes.</returns>
    ///  <remarks>
    ///  <para>To find the required buffer size call this method with Buffer =
    ///  nil and it will return the required block size, in characters. Now
    ///  allocate a buffer large enough to hold the required number of
    ///  characters and call this method again, this time passing the buffer and
    ///  its size in characters.</para>
    ///  <para>The environment blocks created by this method are suitable for
    ///  passing to processes created with the CreateProcess API function. How
    ///  this is done depends on whether the block is Unicode (as created with
    ///  Unicode Delphis) or ANSI (as created by non-Unicode Delphis). To see
    ///  how see http://delphidabbler.com/articles?article=6#createenvblock
    ///  </para>
    ///  </remarks>
    class function CreateBlock(const NewEnv: TStrings;
      const IncludeCurrent: Boolean; const Buffer: Pointer;
      const BufSize: Integer): Integer;
    ///  <summary>Calculates and returns the size of the current process'
    ///  environment block in characters.</summary>
    ///  <remarks>Multiply the returned size by SizeOf(Char) to get the block
    ///  size in Bytes.</remarks>
    class function BlockSize: Integer;
    ///  <summary>Expands a string containing environment variables by replacing
    ///  each environment variable name with its value.</summary>
    ///  <param name="Str">string [in] String containing environment variables
    ///  to be expanded. Each environment variable name must be enclosed by
    ///  single % characters, e.g. %FOO%.</param>
    ///  <returns>string. The expanded string.</returns>
    class function Expand(const Str: string): string;
    ///  <summary>Gets all the environment variables available to the current
    ///  process and returns the size of the environment block.</summary>
    ///  <param name="Vars">TStrings [in] Receives all environment variables in
    ///  Name=Value format. Any previous contents are discarded. If nil is
    ///  passed to this parameter no environment variables are fetched.</param>
    ///  <returns>Integer. The minimum size of a environment block that contains
    ///  all the environment variables, in characters. Multiply by SizeOf(Char)
    ///  to get the size in bytes.</returns>
    ///  <remarks>If you need to find the block size without fetching any
    ///  environment variables just call the method and pass nil as the
    ///  parameter.</remarks>
    class function GetAll(const Vars: TStrings): Integer; overload;
    ///  <summary>Gets all the environment variables available to the current
    ///  process.</summary>
    ///  <returns>TPJEnvironmentVarArray. A dynamic array of records containing
    ///  the names and values of each environment variable.</returns>
    class function GetAll: TPJEnvironmentVarArray; overload;
    ///  <summary>Gets the names of all environment variables available to the
    ///  current process.</summary>
    ///  <param name="Names">TStrings [in] Receives all the environment variable
    ///  names. Any existing content is discarded. Must not be nil.</param>
    class procedure GetAllNames(const Names: TStrings); overload;
    ///  <summary>Gets the names of all environment variables available to the
    ///  current process.</summary>
    ///  <returns>TStringDynArray. Dynamic array containing the environment
    ///  variable names.</returns>
    class function GetAllNames: TStringDynArray; overload;
    ///  <summary>Enumerates the names of all environment variables in the
    ///  current process.</summary>
    ///  <param name="Callback">TPJEnvVarsEnum [in] Callback method called once
    ///  for each environment variable, passing its name and the value of the
    ///  Data pointer as parameters.</param>
    ///  <param name="Data">Pointer [in] Data to be passed to Callback function
    ///  each time it is called.</param>
    class procedure EnumNames(Callback: TPJEnvVarsEnum; Data: Pointer);
    ///  <summary>Enumerates all the environment variables available to the
    ///  current process.</summary>
    ///  <param name="Callback">TPJEnvVarsEnumEx [in] Callback method called
    ///  once for each environment variable, passing a record containing its
    ///  name and value along with the the value of the Data pointer as
    ///  parameters.</param>
    ///  <param name="Data">Pointer [in] Data to be passed to Callback function
    ///  each time it is called.</param>
    class procedure EnumVars(Callback: TPJEnvVarsEnumEx; Data: Pointer);
  end;

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
  {$IFDEF Supports_EOSError}
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
  {$IFDEF Supports_ENoConstructException}
  RTLConsts,
  {$ENDIF}
  Windows;

{$IFNDEF Supports_ENoConstructException}
resourcestring
  sNoConstruct = 'Class %s is not intended to be constructed';
{$ENDIF}

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
begin
  Result := TPJEnvironmentVars.GetValue(VarName);
end;

function SetEnvVarValue(const VarName, VarValue: string): Integer;
  {Sets the value of an environment variable.
    @param VarName [in] Name of environment variable.
    @param VarValue [in] New value of environment variable. Passing '' deletes
      the environment variable.
    @return 0 on success or a Windows error code on error.
  }
begin
  Result := TPJEnvironmentVars.SetValue(VarName, VarValue);
end;

function DeleteEnvVar(const VarName: string): Integer;
  {Deletes an environment variable.
    @param VarName [in] Name of environment variable.
    @return 0 on success or a Windows error code on error.
  }
begin
  Result := TPJEnvironmentVars.Delete(VarName);
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
begin
  Result := TPJEnvironmentVars.CreateBlock(
    NewEnv, IncludeCurrent, Buffer, BufSize
  );
end;

function ExpandEnvVars(const Str: string): string;
  {Replaces any environment variables in a string with their values. Environment
  variables should be delimited by % characters thus: %ENVVAR%.
    @param Str [in] String containing environment variables to be replaced.
    @return Converted string.
  }
begin
  Result := TPJEnvironmentVars.Expand(Str);
end;

function GetAllEnvVars(const Vars: TStrings): Integer;
  {Gets all the environment variables available to the current process.
    @param Vars [in] String list that receives all environment variables in
      Name=Value format. Set to nil if all you need is the size of the
      environment block.
    @return Size of environment block that contains all environment variables
      in characters. Multiply by SizeOf(Char) to get size in bytes.
  }
begin
  Result := TPJEnvironmentVars.GetAll(Vars);
end;

procedure GetAllEnvVarNames(const Names: TStrings); overload;
  {Stores all environment variable names in a string list. Empty names are
  ignored.
    @param Names [in] String list to receive names. Any previous contents are
      discarded.
  }
begin
  TPJEnvironmentVars.GetAllNames(Names);
end;

function GetAllEnvVarNames: TStringDynArray; overload;
  {Creates a string array containing all environment variables. Empty names are
  ignored.
    @return Array of environment variable names.
  }
begin
  Result := TPJEnvironmentVars.GetAllNames;
end;

function EnvBlockSize: Integer;
  {Calculates size of environment block in characters.
    @return Size of environment block in characters. Multiply by SizeOf(Char) to
      get size in bytes.
  }
begin
  Result := TPJEnvironmentVars.BlockSize;
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
  ErrorCheck(TPJEnvironmentVars.Delete(Name));
end;

procedure TPJEnvVars.EnumNames(Callback: TPJEnvVarsEnum; Data: Pointer);
  {Enumerates names of all environment variables in current process.
    @param Callback [in] Callback function called once for each environment
      variable, passing name and value of Data parameter.
    @param Data [in] User specified data passed to callback function.
  }
begin
  TPJEnvironmentVars.EnumNames(Callback, Data);
end;

function TPJEnvVars.GetCount: Integer;
  {Read access method for Count property.
    @return Number of environment variables in current process.
  }
begin
  Result := TPJEnvironmentVars.Count;
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
  Result := TPJEnvironmentVars.GetValue(Name);
end;

procedure TPJEnvVars.SetValue(Name: string; const Value: string);
  {Write access method for Values property.
    @param Name [in] Name of environment variable.
    @param Value [in] Value of environment variable.
    @raise EPJEnvVars if environment variable can't be set.
  }
begin
  ErrorCheck(TPJEnvironmentVars.SetValue(Name, Value));
end;

{ TPJEnvVarsEnumerator }

constructor TPJEnvVarsEnumerator.Create;
  {Object constructor. Initialises enumeration.
  }
begin
  fEnvVarNames := TStringList.Create;
  TPJEnvironmentVars.GetAllNames(fEnvVarNames);
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

{ TPJEnvironmentVars }

class function TPJEnvironmentVars.BlockSize: Integer;
begin
  Result := GetAll(nil); // this function returns required block size
end;

class function TPJEnvironmentVars.Count: Integer;
var
  EnvList: TStringList; // list of all environment variables
begin
  EnvList := TStringList.Create;
  try
    GetAll(EnvList);
    Result := EnvList.Count;
  finally
    EnvList.Free;
  end;
end;

constructor TPJEnvironmentVars.Create;
begin
  raise ENoConstructException.CreateFmt(sNoConstruct, [ClassName]);
end;

class function TPJEnvironmentVars.CreateBlock(const NewEnv: TStrings;
  const IncludeCurrent: Boolean; const Buffer: Pointer;
  const BufSize: Integer): Integer;
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
      GetAll(EnvVars);
    // add any additional environment strings
    if Assigned(NewEnv) then
      EnvVars.AddStrings(NewEnv);
    // Calculate size of new environment block: block consists of #0 separated
    // list of environment variables terminated by #0#0, e.g.
    // Foo=Lorem#0Bar=Ipsum#0Raboof=Dolore#0#0
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

class function TPJEnvironmentVars.Delete(const VarName: string): Integer;
begin
  if SetEnvironmentVariable(PChar(VarName), nil) then
    Result := 0
  else
    Result := GetLastError;
end;

class procedure TPJEnvironmentVars.EnumNames(Callback: TPJEnvVarsEnum;
  Data: Pointer);
var
  Idx: Integer;
  EnvList: TStringList;
begin
  Assert(Assigned(Callback));
  EnvList := TStringList.Create;
  try
    GetAll(EnvList);
    for Idx := 0 to Pred(EnvList.Count) do
      Callback(EnvList.Names[Idx], Data);
  finally
    EnvList.Free;
  end;
end;

class procedure TPJEnvironmentVars.EnumVars(Callback: TPJEnvVarsEnumEx;
  Data: Pointer);
var
  Idx: Integer;
  AllEnvVars: TPJEnvironmentVarArray;
begin
  Assert(Assigned(Callback));
  AllEnvVars := GetAll;
  for Idx := Low(AllEnvVars) to High(AllEnvVars) do
    Callback(AllEnvVars[Idx], Data);
end;

class function TPJEnvironmentVars.Expand(const Str: string): string;
var
  BufSize: Integer;
begin
  // Get required buffer size (including terminal #0)
  BufSize := ExpandEnvironmentStrings(PChar(Str), nil, 0);
  if BufSize > 0 then
  begin
    SetLength(Result, BufSize - 1); // space for terminal #0 automatically added
    ExpandEnvironmentStrings(PChar(Str), PChar(Result), BufSize);
  end
  else
    Result := ''; // tried to expand an empty string
end;

class function TPJEnvironmentVars.GetAll: TPJEnvironmentVarArray;
var
  AllEnvVars: TStringList;
  Idx: Integer;
  EnvVar: TPJEnvironmentVar;
begin
  AllEnvVars := TStringList.Create;
  try
    GetAll(AllEnvVars);
    SetLength(Result, AllEnvVars.Count);
    for Idx := 0 to Pred(AllEnvVars.Count) do
    begin
      EnvVar.Name := AllEnvVars.Names[Idx];
      EnvVar.Value := AllEnvVars.ValueFromIndex[Idx];
      Result[Idx] := EnvVar;
    end;
  finally
    AllEnvVars.Free;
  end;
end;

class function TPJEnvironmentVars.GetAll(const Vars: TStrings): Integer;
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
    // Env strings are #0 terminated and list ends an additional #0, e.g.:
    // Foo=Lorem#0Bar=Ipsum#0Raboof=Dolore#0#0
    PEnvEntry := PEnvVars;
    try
      while PEnvEntry^ <> #0 do
      begin
        if Assigned(Vars) then
          Vars.Add(PEnvEntry);
        Inc(PEnvEntry, StrLen(PEnvEntry) + 1);  // +1 to skip terminating #0
      end;
      // Calculate length of block
      Result := (PEnvEntry - PEnvVars) + 1;     // + 1 to allow for final #0
    finally
      FreeEnvironmentStrings(PEnvVars);
    end;
  end
  else
    // No block => zero length
    Result := 0;
end;

class procedure TPJEnvironmentVars.GetAllNames(const Names: TStrings);
var
  AllEnvVars: TStrings;
  Idx: Integer;
begin
  Assert(Assigned(Names));
  Names.Clear;
  AllEnvVars := TStringList.Create;
  try
    GetAll(AllEnvVars);
    for Idx := 0 to Pred(AllEnvVars.Count) do
      Names.Add(AllEnvVars.Names[Idx]);
  finally
    AllEnvVars.Free;
  end;
end;

class function TPJEnvironmentVars.GetAllNames: TStringDynArray;
var
  Names: TStrings;
  Idx: Integer;
begin
  Names := TStringList.Create;
  try
    GetAllNames(Names);
    SetLength(Result, Names.Count);
    for Idx := 0 to Pred(Names.Count) do
      Result[Idx] := Names[Idx];
  finally
    Names.Free;
  end;
end;

class function TPJEnvironmentVars.GetValue(const VarName: string): string;
var
  BufSize: Integer;
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

class function TPJEnvironmentVars.SetValue(const VarName, VarValue: string):
  Integer;
begin
  if SetEnvironmentVariable(PChar(VarName), PChar(VarValue)) then
    Result := 0
  else
    Result := GetLastError;
end;

end.

