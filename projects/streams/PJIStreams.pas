{
 * PJIStreams.pas
 *
 * Classes which provides various implementations of the IStream interface.
 *
 * $Rev$
 * $Date$
 *
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
 * The Original Code is PJIStreams.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit PJIStreams;


interface


uses
  // Delphi
  Classes, Windows, ActiveX;

{$UNDEF SUPPORTS_STRICT}
{$UNDEF SUPPORTS_TSTREAM64}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 14.0} // >= Delphi 6
    {$DEFINE SUPPORTS_TSTREAM64}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // >= Delphi 7
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$IFEND}
  {$IF CompilerVersion >= 18.0} // >= Delphi 2006
    {$DEFINE SUPPORTS_STRICT}
  {$IFEND}
{$ENDIF}

type

  {
  TPJIStreamWrapper:
    Class that implements the IStream interface for a wrapped TStream object.
  }
  TPJIStreamWrapper = class(TInterfacedObject, IStream)
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF}
  private
    fCloseStream: Boolean; // Flags if wrapped stream is freed in destructor
    fBaseStream: TStream;  // Reference to wrapped stream
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF}
  protected
    function GetStreamNameAsString: string; virtual;
      {Gets the name of the stream as a Delphi string: used by the GetStreamName
      function. Returns the name of the wrapper class followed by the name of
      the wrapped class in parentheses. Descendant classes can override this
      method if they use a different name for the stream}
    function GetStreamName: POleStr; virtual;
      {Uses the task allocator to allocate memory for name of stream as a wide
      string and returns a pointer to it. Used by Stat method. Caller of stat
      method must use the task allocator free the memory. The name used is that
      returned by the GetStreamNameAsString virtual method. This method can be
      overridden if there is a need to change the allocation method. To change
      the name returned, override GetStreamNameAsString instead}
    property BaseStream: TStream read fBaseStream;
      {Reference to wrapped TStream object}
  public
    constructor Create(const Stream: TStream;
      const CloseStream: Boolean = False);
      {Class constructor: the given stream is given an IStream interface and is
      freed when this object is destroyed if CloseStream is true}
    destructor Destroy; override;
      {Class destructor: frees wrapped stream if CloseStream parameter to
      constructor was true}
    { IStream methods }
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
      virtual; stdcall;
      {Reads a specified number of bytes from the stream object into memory
      starting at the current seek pointer. Sets pcbRead, if not nil, to number
      of bytes actually read}
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
      virtual; stdcall;
      {Writes a specified number of bytes into the stream object starting at the
      current seek pointer. The number of bytes actually written is returned in
      pcbWritten if this is non nil}
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; virtual; stdcall;
      {Changes the seek pointer to a new location relative to the beginning of
      the stream, the end of the stream, or the current seek pointer. Returns
      the new seek pointer position in libNewPosition}
    function SetSize(libNewSize: Largeint): HResult; virtual; stdcall;
      {Changes the size of the stream object}
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; virtual; stdcall;
      {Copies a specified number of bytes from the current seek pointer in the
      stream to the current seek pointer in another stream. The number of bytes
      actually read and written is recorded in cbRead and cbWritten. If the
      source stream has less than the required number of bytes available then
      all remaining bytes are written}
    function Commit(grfCommitFlags: Longint): HResult; virtual; stdcall;
      {Provided in IStream implementations that support transacted streams to
      ensure that any changes made to a stream object open in transacted mode
      are reflected in the parent storage object. Since we don't support
      transacted mode there's nothing to do here}
    function Revert: HResult; virtual; stdcall;
      {Discards all changes that have been made to a transacted stream since the
      last IStream::Commit call. Since we don't supported transacted streams we
      just return that we've reverted the stream}
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; virtual; stdcall;
      {Restricts access to a specified range of bytes in the stream. It is
      optional to support this method, and we don't!}
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; virtual; stdcall;
      {Removes the access restriction on a range of bytes previously restricted
      with IStream::LockRegion. We don't support locking}
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      virtual; stdcall;
      {Retrieves the STATSTG structure for this stream. grfStatFlag can be
      STATFLAG_DEFAULT, which omits the stream name from the structure, or
      STATFLAG_NORMAL, which includes the stream name. In the latter case the
      name should be freed using the task allocator}
    function Clone(out stm: IStream): HResult; virtual; stdcall;
      {Not implemented. (Where implemented Clone creates a new stream object
      that references the same bytes as the original stream but provides a
      separate seek pointer to those bytes)}
  end;

  {
  TPJHandleIStreamWrapper:
    Class that implements an IStream interface for a wrapped THandleStream
    object (or descendant such as TFileStream). Acts in a similar way to
    TPJIStreamWrapper except that file date stamps are returned by the Stat
    method.
  }
  TPJHandleIStreamWrapper = class(TPJIStreamWrapper, IStream)
  public
    constructor Create(const Stream: THandleStream;
      const CloseStream: Boolean = False);
      {Class constructor: the given handle stream is given an IStream interface
      and is freed when this object is destroyed if CloseStream is true}
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      override; stdcall;
      {Retrieves the STATSTG structure for this stream. grfStatFlag can be
      STATFLAG_DEFAULT, which omits the stream name from the structure, or
      STATFLAG_NORMAL, which includes the stream name. In the latter case the
      name should be freed using the task allocator. This method returns the
      underlying file's modification, access and creation times if available}
  end;

  {
  TPJFileIStream:
    Class that implements a IStream interface on a file.
  }
  TPJFileIStream = class(TPJHandleIStreamWrapper, IStream)
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF}
  private
    fFileName: string;  // Name of file accessed by this object
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF}
  protected
    function GetStreamNameAsString: string; override;
      {Returns the name of the underlying file as the stream name}
  public
    constructor Create(const FileName: string; Mode: Word);
      {Class constructor: opens the file and records name}
  end;


implementation


uses
  // Delphi
  Math, SysUtils,
  // Library
  PJStreamWrapper;


{ TPJIStreamWrapper }

function TPJIStreamWrapper.Clone(out stm: IStream): HResult;
  {Not implemented. (Where implemented Clone creates a new stream object that
  references the same bytes as the original stream but provides a separate seek
  pointer to those bytes)}
begin
  Result := E_NOTIMPL;
end;

function TPJIStreamWrapper.Commit(grfCommitFlags: Integer): HResult;
  {Provided in IStream implementations that support transacted streams to ensure
  that any changes made to a stream object open in transacted mode are reflected
  in the parent storage object. Since we don't support transacted mode there's
  nothing to do here}
begin
  Result := S_OK;
end;

function TPJIStreamWrapper.CopyTo(stm: IStream; cb: Largeint; out cbRead,
  cbWritten: Largeint): HResult;
  {Copies a specified number of bytes from the current seek pointer in the
  stream to the current seek pointer in another stream. The number of bytes
  actually read and written is recorded in cbRead and cbWritten. If the source
  stream has less than the required number of bytes available then all remaining
  bytes are written}
var
  BytesRead: Integer;     // number of bytes read in a chunk
  BytesWritten: Integer;  // number of bytes written in a chunk
  BufSize: Integer;       // size of buffer used for copying data
  Buffer: PByte;          // buffer used for copying data
const
  MaxBufSize = 1024 * 1024; // maximum size of copy buffer
begin
  // Assume no bytes anre read / written
  if Assigned(@cbRead) then
    cbRead := 0;
  if Assigned(@cbWritten) then
    cbWritten := 0;
  // Check parameters for validity
  if not Assigned(stm) then
  begin
    Result := STG_E_INVALIDPOINTER;
    Exit;
  end;
  // Do copy
  Result := S_OK;
  try
    BufSize := Min(MaxBufSize, cb);
    GetMem(Buffer, BufSize);
    try
      while cb > 0 do
      begin
        BytesRead := BaseStream.Read(Buffer^, Min(BufSize, cb));
        if BytesRead = 0 then
          // end of stream reached
          Exit;
        if Assigned(@cbRead) then
          Inc(cbRead, BytesRead);
        Result := stm.Write(Buffer, BytesRead, @BytesWritten);
        if Assigned(@cbWritten) then
          Inc(cbWritten, BytesWritten);
        if Succeeded(Result) and (BytesRead <> BytesWritten) then
          // couldn't write all data: probably medium or stream full
          Result := STG_E_MEDIUMFULL;
        if Failed(Result) then
          Exit; // write error
        Dec(cb, BytesRead);
      end;
    finally
      FreeMem(Buffer, BufSize);
    end;
  except
    // exception during copy
    Result := E_UNEXPECTED;
  end;
end;

constructor TPJIStreamWrapper.Create(const Stream: TStream;
  const CloseStream: Boolean = False);
  {Class constructor: the given stream is given an IStream interface and is
  freed when this object is destroyed if CloseStream is true}
begin
  inherited Create;
  fBaseStream := Stream;
  fCloseStream := CloseStream;
end;

destructor TPJIStreamWrapper.Destroy;
  {Class destructor: frees wrapped stream if CloseStream parameter to
  constructor was true}
begin
  if fCloseStream then
    fBaseStream.Free;
  inherited Destroy;
end;

function TPJIStreamWrapper.GetStreamName: POleStr;
  {Uses the task allocator to allocate memory for name of stream as a wide
  string and returns a pointer to it. Used by Stat method. Caller of stat method
  must use the task allocator free the memory. The name used is that returned by
  the GetStreamNameAsString virtual method. This method can be overridden if
  there is a need to change the allocation method. To change the name returned,
  override GetStreamNameAsString instead}
var
  Name: string;     // name of stream
begin
  Name := GetStreamNameAsString;
  Result := CoTaskMemAlloc(SizeOf(WideChar) * (Length(Name) + 1));
  StringToWideChar(Name, Result, Length(Name) + 1);
end;

function TPJIStreamWrapper.GetStreamNameAsString: string;
  {Gets the name of the stream as a Delphi string: used by the GetStreamName
  function. Returns the name of the wrapper class followed by the name of the
  wrapped class in parentheses. Descendant classes can override this method if
  they use a different name for the stream}
begin
  Result := ClassName + '(' + fBaseStream.ClassName + ')';
end;

function TPJIStreamWrapper.LockRegion(libOffset, cb: Largeint;
  dwLockType: Integer): HResult;
  {Restricts access to a specified range of bytes in the stream. It is optional
  to support this method, and we don't!}
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TPJIStreamWrapper.Read(pv: Pointer; cb: Integer;
  pcbRead: PLongint): HResult;
  {Reads a specified number of bytes from the stream object into memory starting
  at the current seek pointer. Sets pcbRead, if not nil, to number of bytes
  actually read}
var
  Read: LongInt;  // number of bytes read
begin
  // Check params
  if Assigned(pv) then
  begin
    try
      // Read the data into the buffer
      Read := fBaseStream.Read(pv^, cb);
      if Assigned(pcbRead) then
        pcbRead^ := Read;
      Result := S_OK
    except
      Result := S_FALSE;
    end;
  end
  else
    // Nil buffer pointer
    Result := STG_E_INVALIDPOINTER;
end;

function TPJIStreamWrapper.Revert: HResult;
  {Discards all changes that have been made to a transacted stream since the
  last IStream::Commit call. Since we don't supported transacted streams we just
  return that we've reverted the stream}
begin
  Result := STG_E_REVERTED;
end;

function TPJIStreamWrapper.Seek(dlibMove: Largeint; dwOrigin: Integer;
  out libNewPosition: Largeint): HResult;
  {Changes the seek pointer to a new location relative to the beginning of the
  stream, the end of the stream, or the current seek pointer. Returns the new
  seek pointer position in libNewPosition}
var
  {$IFDEF SUPPORTS_TSTREAM64}
  Origin: TSeekOrigin;
  {$ELSE}
  Origin: Word;               // seek origin in terms of TStream
  {$ENDIF}
  NewPosition: Largeint;      // new file pointer position after seek
  Wrapper: TPJStreamWrapper;  // stream wrapper to perform actual seek
begin
  // Translate origin from IStream constant to TStream constant
  case dwOrigin of
    STREAM_SEEK_SET:
      {$IFDEF SUPPORTS_TSTREAM64}
      Origin := soBeginning;
      {$ELSE}
      Origin := soFromBeginning;
      {$ENDIF}
    STREAM_SEEK_CUR:
      {$IFDEF SUPPORTS_TSTREAM64}
      Origin := soCurrent;
      {$ELSE}
      Origin := soFromCurrent;
      {$ENDIF}
    STREAM_SEEK_END:
      {$IFDEF SUPPORTS_TSTREAM64}
      Origin := soEnd;
      {$ELSE}
      Origin := soFromEnd;
      {$ENDIF}
    else
    begin
      Result := STG_E_INVALIDFUNCTION;
      Exit;
    end
  end;
  try
    // We use a TPJStreamWrapper to perform seek so that it can fix any problems
    // with seek on a TStringStream using STREAM_SEEK_END origin (see comments
    // in PJStreamWrapper unit for details).
    Wrapper := TPJStreamWrapper.Create(fBaseStream);
    try
      // Valid origin: do seek and record new position if it's assigned
      NewPosition := Wrapper.Seek(dlibMove, Origin);
      if Assigned(@libNewPosition) then
        libNewPosition := NewPosition;
      // seek succeeded
      Result := S_OK;
    finally
      Wrapper.Free;
    end;
  except
    // seek failed
    Result := STG_E_INVALIDPOINTER;
  end;
end;

function TPJIStreamWrapper.SetSize(libNewSize: Largeint): HResult;
  {Changes the size of the stream object}
begin
  try
    // Set the stream size and compare actual new size to requested
    fBaseStream.Size := libNewSize;
    if libNewSize = fBaseStream.Size then
      Result := S_OK
    else
      Result := E_FAIL;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TPJIStreamWrapper.Stat(out statstg: TStatStg;
  grfStatFlag: Integer): HResult;
  {Retrieves the STATSTG structure for this stream. grfStatFlag can be
  STATFLAG_DEFAULT, which omits the stream name from the structure, or
  STATFLAG_NORMAL, which includes the stream name. In the latter case the name
  should be freed using the task allocator}
begin
  // Check parameters for validity
  if Assigned(@statstg) then
  begin
    if (grfStatFlag = STATFLAG_DEFAULT)
      or (grfStatFlag = STATFLAG_NONAME) then
    begin
      // Update TStatStg structure
      try
        FillChar(statstg, SizeOf(TStatStg), 0);
        with statstg do
        begin
          dwType := STGTY_STREAM;
          if grfStatFlag <> STATFLAG_NONAME then
            pwcsName := GetStreamName;
          cbSize := fBaseStream.Size;
        end;
        Result := S_OK;
      except
        Result := E_UNEXPECTED;
      end;
    end
    else
      // Bad flag
      Result := STG_E_INVALIDFLAG;
  end
  else
    // TStatStg pointer is nil
    Result := STG_E_INVALIDPOINTER;
end;

function TPJIStreamWrapper.UnlockRegion(libOffset, cb: Largeint;
  dwLockType: Integer): HResult;
  {Removes the access restriction on a range of bytes previously restricted with
  IStream::LockRegion. We don't support locking}
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TPJIStreamWrapper.Write(pv: Pointer; cb: Integer;
  pcbWritten: PLongint): HResult;
  {Writes a specified number of bytes into the stream object starting at the
  current seek pointer. The number of bytes actually written is returned in
  pcbWritten if this is non nil}
var
  Written: LongInt; // number of bytes written
begin
  // Check parameters
  if Assigned(pv) then
  begin
    try
      // Attempt to write the data from the buffer to the stream
      Written := fBaseStream.Write(pv^, cb);
      if Assigned(pcbWritten) then
        pcbWritten^ := Written;
      Result := S_OK;
    except
      Result := STG_E_CANTSAVE;
    end;
  end
  else
    // Bad buffer: nil pointer specified
    Result := STG_E_INVALIDPOINTER;
end;

{ TPJHandleIStreamWrapper }

constructor TPJHandleIStreamWrapper.Create(const Stream: THandleStream;
  const CloseStream: Boolean);
  {Class constructor: the given handle stream is given an IStream interface and
  is freed when this object is destroyed if CloseStream is true}
begin
  inherited Create(Stream, CloseStream);
end;

function TPJHandleIStreamWrapper.Stat(out statstg: TStatStg;
  grfStatFlag: Integer): HResult;
  {Retrieves the STATSTG structure for this stream. grfStatFlag can be
  STATFLAG_DEFAULT, which omits the stream name from the structure, or
  STATFLAG_NORMAL, which includes the stream name. In the latter case the name
  should be freed using the task allocator. This method returns the underlying
  file's modification, access and creation times if available}
var
  FileInfo: TByHandleFileInformation; // info about file associated with handle
begin
  // Get other stats per ancestor method
  Result := inherited Stat(statstg, grfStatFlag);
  // Get file info and record file date info in stats
  if GetFileInformationByHandle(
    (BaseStream as THandleStream).Handle,
    FileInfo) and Assigned(@statstg) then
  begin
    statstg.mtime := FileInfo.ftLastWriteTime;
    statstg.ctime := FileInfo.ftCreationTime;
    statstg.atime := FileInfo.ftLastAccessTime;
  end;
end;

{ TPJFileIStream }

constructor TPJFileIStream.Create(const FileName: string; Mode: Word);
  {Class constructor: opens the file and records name}
begin
  // Open stream to file: gets closed automatically when this object freed
  inherited Create(TFileStream.Create(FileName, Mode), True);
  fFileName := FileName;
end;

function TPJFileIStream.GetStreamNameAsString: string;
  {Returns the name of the underlying file as the stream name}
begin
  Result := fFileName;
end;

end.

