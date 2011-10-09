{
 * PJFileHandle.pas
 *
 * Class that can open or create files with specified security and provides
 * access to the file's handle. Enables opening and creation of files with
 * inheritable handles.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is PJFileHandle.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK *****
}

unit PJFileHandle;

{$UNDEF COMPILERSUPPORTED}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 15.0}   // Delphi 7
    {$DEFINE COMPILERSUPPORTED}
  {$IFEND}
{$ENDIF}

{$IFNDEF COMPILERSUPPORTED}
  {$MESSAGE FATAL 'Minimum compiler version is Delphi 7'}
{$ENDIF}

{$WARN UNSAFE_CODE OFF}

interface

uses
  // Delphi
  Windows;

type

  ///  <summary>
  ///  Class that can create or open files in various access and sharing modes
  ///  with specified security and provide access to the file handle.
  ///  </summary>
  ///  <remarks>
  ///  <para>The class is provided specifically to make it easier to obtain
  ///  inheritable file handles for use when redirecting to and from files with
  ///  using TPJConsoleApp. As such the class provides no methods for accessing
  ///  the file.</para>
  ///  <para>If there is a need to read or write the file pass the file handle
  ///  to Windows or SysUtils file routines or get stream access by creating a
  ///  THandleStream object for the file's handle.</para>
  ///  </remarks>
  TPJFileHandle = class(TObject)
  private
    ///  <summary>File handle.</summary>
    fHandle: THandle;
    ///  <summary>Opens or creates a file, updating Handle property.</summary>
    ///  <param name="FileName">string [in] Name of file to open / create.
    ///  </param>
    ///  <param name="Mode">LongWord [in] File open or create mode. This is a
    ///  bitmask made by oring access mode flags with sharing flags.</param>
    ///  <param name="Security">PSecurityAttributes [in] Pointer to security
    ///  attributes to be applied to file.</param>
    ///  <remarks>Raise EFCreateError or EFOpenError if file cannot be created
    ///  or opened respectively.</remarks>
    procedure OpenFile(const FileName: string; const Mode: LongWord;
      const Security: PSecurityAttributes);
  public
    ///  <summary>Object constructor. Create or opens specified file.</summary>
    ///  <param name="FileName">string [in] Name of file to open / create.
    ///  </param>
    ///  <param name="Mode">LongWord [in] File open or create mode. This is a
    ///  bitmask made by oring access mode flags with sharing flags.</param>
    ///  <param name="Inheritable">Boolean [in] Indicates whether file handle is
    ///  to be inheritable.</param>
    ///  <remarks>Raise EFCreateError or EFOpenError if file cannot be created
    ///  or opened respectively.</remarks>
    constructor Create(const FileName: string; const Mode: LongWord;
      const Inheritable: Boolean = True); overload;
    ///  <summary>Object constructor. Create or opens specified file.</summary>
    ///  <param name="FileName">string [in] Name of file to open / create.
    ///  </param>
    ///  <param name="Mode">LongWord [in] File open or create mode. This is a
    ///  bitmask made by oring access mode flags with sharing flags.</param>
    ///  <param name="Security">TSecurityAttributes [in] Required security for
    ///  file. If handle is to be inheritable set the bInheritHandle field to
    ///  True.</param>
    ///  <remarks>Raise EFCreateError or EFOpenError if file cannot be created
    ///  or opened respectively.</remarks>
    constructor Create(const FileName: string; const Mode: LongWord;
      const Security: PSecurityAttributes); overload;
    ///  <summary>Object destructor. Closes file handle.</summary>
    destructor Destroy; override;
    ///  <summary>Handle used to access file.</summary>
    property Handle: THandle read fHandle;
  end;


implementation


uses
  // Delphi
  SysUtils, Classes, RTLConsts;


resourcestring
  // Error messages
  sBadShareMode = 'Invalid sharing mode';
  sBadOpenMode = 'Invalid file open mode';

{ TPJFileHandle }

constructor TPJFileHandle.Create(const FileName: string; const Mode: LongWord;
  const Security: PSecurityAttributes);
begin
  inherited Create;
  OpenFile(FileName, Mode, Security);
end;

constructor TPJFileHandle.Create(const FileName: string; const Mode: LongWord;
  const Inheritable: Boolean);
var
  Security: TSecurityAttributes;  // file's security attributes
begin
  inherited Create;
  if Inheritable then
  begin
    // Set up security structure so file handle is inheritable (for Windows NT)
    Security.nLength := SizeOf(Security);
    Security.lpSecurityDescriptor := nil;
    Security.bInheritHandle := True;
    OpenFile(FileName, Mode, @Security);
  end
  else
    OpenFile(FileName, Mode, nil);
end;

destructor TPJFileHandle.Destroy;
begin
  if fHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(fHandle);
  inherited;
end;

procedure TPJFileHandle.OpenFile(const FileName: string; const Mode: LongWord;
  const Security: PSecurityAttributes);
const
  /// Map of share modes to Windows API sharing flags
  ShareModes: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE
  );
  // Map of access modes to Windows API access flags
  AccessModes: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE
  );
var
  ShareMode: LongWord;  // sharing mode
  AccessMode: LongWord; // access mode
begin
  fHandle := INVALID_HANDLE_VALUE;
  if (Mode and fmCreate) = fmCreate then
  begin
    // Get share mode. This is encoded in bits 4..7 of Mode. Values above
    // fmShareDenyNone ($40) are not permitted.
    ShareMode := Mode and $F0;
    if ShareMode = $F0 then
      // for compatibility: earlier versions of Delphi define fmCreate as $FFFF
      // while in later version it is $FF00 and can be or'd with sharing info.
      // If we get Mode = $FFFF we get a sharing mode of $F0, which is not valid
      // so we replace it with fmShareExclusive.
      ShareMode := fmShareExclusive;
    if ShareMode > fmShareDenyNone then
      raise EFCreateError.CreateResFmt(
        @SFCreateErrorEx, [FileName, sBadShareMode]
      );
    fHandle := CreateFile(
      PChar(FileName),
      GENERIC_READ or GENERIC_WRITE,
      ShareModes[ShareMode shr 4],
      Security,
      CREATE_ALWAYS,
      FILE_ATTRIBUTE_NORMAL,
      0
    );
    if fHandle = INVALID_HANDLE_VALUE then
      raise EFCreateError.CreateResFmt(
        @SFCreateErrorEx, [FileName, SysErrorMessage(GetLastError)]
      );
  end
  else
  begin
    // Get access mode. This is encoded in bits 0 and 1 of Mode. Possible values
    // are 0..3, of which fmOpenReadWrite is highest permitted value.
    AccessMode := Mode and 3;
    if AccessMode > fmOpenReadWrite then
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [FileName, sBadOpenMode]);
    // Get share mode. This is encoded in bits 4..7 of Mode. Values above
    // fmShareDenyNone ($40) are not permitted.
    ShareMode := Mode and $F0;
    if ShareMode > fmShareDenyNone then
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [FileName, sBadShareMode]);
    fHandle := CreateFile(
      PChar(FileName),
      AccessModes[AccessMode],
      ShareModes[ShareMode shr 4],
      Security,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
    );
    if fHandle = INVALID_HANDLE_VALUE then
      raise EFOpenError.CreateResFmt(
        @SFOpenErrorEx, [FileName, SysErrorMessage(GetLastError)]
      );
  end;
end;

end.

