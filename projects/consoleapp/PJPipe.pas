{
 * PJPipe.pas
 *
 * Class that encapsulates an unamed pipe and can read and write the pipe.
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
 * The Original Code is PJPipe.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}

unit PJPipe;

{$WARN UNSAFE_CODE OFF}

interface


uses
  // Delphi
  SysUtils, Classes, Windows;


// Ensure TBytes is defined
{$IF not Declared(TBytes)}
type
  TBytes = array of Byte;
{$IFEND}

type

  {
  TPJPipe:
    Class that encapsulates an unamed pipe and can read and write the pipe.
  }
  TPJPipe = class(TObject)
  private
    fReadHandle: THandle;
      {Handle used to read the pipe}
    fWriteHandle: THandle;
      {Handle used to write the pipe}
    procedure CheckWriteHandle;
      {Check that pipe's write handle hasn't been closed.
        @except EInOutError raised if handle is 0.
      }
    procedure CreatePipe(const Size: LongWord;
      const Security: PSecurityAttributes);
      {Creates Windows pipe using Windows API and records pipe handles.
        @param Size [in] Size of pipe. If Size is 0 default size pipe is
          created.
        @param Security [in] Pointer to security structure: may be nil.
        @except Raises EInOutError if pipe can't be created.
      }
  public
    constructor Create(const Size: LongWord; const Inheritable: Boolean = True);
      overload;
      {Class constructor. Creates pipe object with specified size.
        @param Size [in] Required size of pipe. If Size is 0 then default pipe
          size used.
        @param Inheritable [in] Whether pipe handles are to be inheritable.
        @except Raises EInOutError if pipe can't be created.
      }
    constructor Create(const Inheritable: Boolean = True);
      overload;
      {Class constructor. Creates pipe object with default size.
        @param Inheritable [in] Whether pipe handles are to be inheritable.
        @except Raises EInOutError if pipe can't be created.
      }
    constructor Create(const Size: LongWord;
      const Security: TSecurityAttributes);
      overload;
      {Class constructor. Creates pipe object with specified size and security.
        @param Size [in] Required size of pipe. If Size is 0 then default pipe
          size used.
        @param Security [in] Required security for pipe. If handles to be
          inheritable set the bInheritHandle field to true.
        @except Raises EInOutError if pipe can't be created.
      }
    constructor Create(const Security: TSecurityAttributes);
      overload;
      {Class constructor. Creates pipe object with default size pipe and
      specified security.
        @param Security [in] Required security for pipe. If handles to be
          inheritable set the bInheritHandle field to true.
        @except Raises EInOutError if pipe can't be created.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function AvailableDataSize: LongWord;
      {Gets size of data available for reading from pipe.
        @return Number of bytes of available data.
        @except EInOutError raised if there is an error peeking pipe.
      }
    function ReadData(out Buf; const BufSize: LongWord;
      out BytesRead: LongWord): Boolean;
      {Reads data from pipe into a buffer.
        @param Buf [out] Buffer receiving data. Must have capacity of at least
          BufSize bytes.
        @param BufSize [in] Size of buffer or number of bytes requested.
        @param BytesRead [out] Set to number of bytes actually read.
        @return True if some data was read, false if not.
        @except EInOutError raised if there is an error peeking or reading pipe.
      }
    function ReadBytes(const Count: LongWord = 0): TBytes;
      {Reads data from pipe into a byte array.
        @param Count [in] Number of bytes to read from pipe. If Count = 0 or is
          greater than number of available bytes then all the data from the pipe
          is read.
        @return Byte array containing data.
        @except EInOutError raised if there is an error peeking or reading pipe.
      }
    procedure CopyToStream(const Stm: TStream; Count: LongWord = 0);
      {Copies data from pipe to a stream.
        @param Stm [in] Stream that receives data.
        @param Count [in] Number of bytes to copy. If 0 then all remaining data
          in pipe is copied to stream.
        @except EInOutError raised if there is an error peeking or reading pipe.
        @except EWriteError raised if fail to write to stream.
      }
    procedure CopyFromStream(const Stm: TStream; Count: LongWord = 0);
      {Copies data from a stream into the pipe.
        @param Stm [in] Stream from which to copy data.
        @param Count [in] Number of bytes to copy. If 0 then all remaining data
          in stream is copied.
        @except EInOutError raised if pipe's write handle has been closed or if
          can't write to pipe.
        @except EReadError raised if fail to read from stream.
      }
    procedure WriteBytes(const Bytes: TBytes);
      {Writes the whole content of a byte array to pipe.
        @except EInOutError raised if pipe's write handle has been closed or if
          can't write to pipe.
      }
    function WriteData(const Buf; const BufSize: LongWord): LongWord;
      {Writes data from buffer to pipe.
        @param Buf [in] Buffer containing data to be written.
        @param BufSize [in] Number of bytes to write from buffer. Buf must have
          capacity of at least BufSize bytes.
        @return Number of bytes actually written.
        @except EInOutError raised if pipe's write handle has been closed or if
          can't write to pipe.
      }
    procedure CloseWriteHandle;
      {Closes the pipe's write handle if it is open. This effectively signals
      EOF to any reader of the pipe. After calling this method no further data
      may be written to the pipe.
      }
    property ReadHandle: THandle read fReadHandle;
      {Handle used to read data from the pipe. Should be non-zero}
    property WriteHandle: THandle read fWriteHandle;
      {Handle used to write data to the pipe. Should not be used when 0.
      CloseWriteHandle closes and zeros this handle}
  end;


implementation


resourcestring
  // Error messages
  sCantCreatePipe = 'Can''t create pipe: %s';
  sCantPeekPipe   = 'Can''t read pipe: peek attempt failed';
  sPipeReadError  = 'Error reading pipe';
  sBadWriteHandle = 'Can''t write pipe: handle closed';
  sPipeWriteError = 'Error writing pipe';


{ TPJPipe }

function TPJPipe.AvailableDataSize: LongWord;
  {Gets size of data available for reading from pipe.
    @return Number of bytes of available data.
    @except EInOutError raised if there is an error peeking pipe.
  }
begin
  if not PeekNamedPipe(fReadHandle, nil, 0, nil, @Result, nil) then
    raise EInOutError.Create(sCantPeekPipe);
end;

procedure TPJPipe.CheckWriteHandle;
  {Check that pipe's write handle hasn't been closed.
    @except EInOutError raised if handle is 0.
  }
begin
  if fWriteHandle = 0 then
    raise EInOutError.Create(sBadWriteHandle);
end;

procedure TPJPipe.CloseWriteHandle;
  {Closes the pipe's write handle if it is open. This effectively signals EOF to
  any reader of the pipe. After calling this method no further data may be
  written to the pipe.
  }
begin
  if fWriteHandle <> 0 then
  begin
    CloseHandle(fWriteHandle);
    fWriteHandle := 0;
  end;
end;

procedure TPJPipe.CopyFromStream(const Stm: TStream; Count: LongWord);
  {Copies data from a stream into the pipe.
    @param Stm [in] Stream from which to copy data.
    @param Count [in] Number of bytes to copy. If 0 then all remaining data in
      stream is copied.
    @except EInOutError raised if pipe's write handle has been closed or if
      can't write to pipe.
    @except EReadError raised if fail to read from stream.
  }
var
  BytesToWrite: LongWord;       // adjusted number of bytes to write
  Buf: array[0..4095] of Byte;  // buffer used in copying from pipe to stream
begin
  // Determine how much to copy
  if Count = 0 then
    Count := Stm.Size - Stm.Position;
  // Copy data one bufferful at a time
  while Count > 0 do
  begin
    if Count > SizeOf(Buf) then
      BytesToWrite := SizeOf(Buf)
    else
      BytesToWrite := Count;
    Stm.ReadBuffer(Buf, BytesToWrite);
    WriteData(Buf, BytesToWrite);
    Dec(Count, BytesToWrite);
  end;
end;

procedure TPJPipe.CopyToStream(const Stm: TStream; Count: LongWord);
  {Copies data from pipe to a stream.
    @param Stm [in] Stream that receives data.
    @param Count [in] Number of bytes to copy. If 0 then all remaining data in
      pipe is copied to stream.
    @except EInOutError raised if there is an error peeking or reading pipe.
    @except EWriteError raised if fail to write to stream.
  }
var
  AvailBytes: LongWord;           // number of bytes in pipe
  BytesToRead: LongWord;          // decreasing count of remaining bytes
  BytesRead: LongWord;            // bytes read in each loop
  Buf: array[0..4095] of Byte;    // buffer used to read from stream
begin
  // Determine how much should be read
  AvailBytes := AvailableDataSize;
  if (Count = 0) or (Count > AvailBytes) then
    Count := AvailBytes;
  // Copy data one bufferful at a time
  while Count > 0 do
  begin
    if Count > SizeOf(Buf) then
      BytesToRead := SizeOf(Buf)
    else
      BytesToRead := Count;
    ReadData(Buf, BytesToRead, BytesRead);
    if BytesRead <> BytesToRead then
      raise EInOutError.Create(sPipeReadError);
    Stm.WriteBuffer(Buf, BytesRead);
    Dec(Count, BytesRead);
  end;
end;

constructor TPJPipe.Create(const Size: LongWord; const Inheritable: Boolean);
  {Class constructor. Creates pipe object with specified size.
    @param Size [in] Required size of pipe. If Size is 0 then default pipe size
      used.
    @param Inheritable [in] Whether pipe handles are to be inheritable.
    @except Raises EInOutError if pipe can't be created.
  }
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
    CreatePipe(Size, @Security);
  end
  else
    CreatePipe(Size, nil);
end;

constructor TPJPipe.Create(const Size: LongWord;
  const Security: TSecurityAttributes);
  {Class constructor. Creates pipe object with specified size and security.
    @param Size [in] Required size of pipe. If Size is 0 then default pipe size
      used.
    @param Security [in] Required security for pipe. If handles to be
      inheritable set the bInheritHandle field to true.
    @except Raises EInOutError if pipe can't be created.
  }
begin
  inherited Create;
  CreatePipe(Size, @Security);
end;

constructor TPJPipe.Create(const Security: TSecurityAttributes);
  {Class constructor. Creates pipe object with default size pipe and specified
  security.
    @param Security [in] Required security for pipe. If handles to be
      inheritable set the bInheritHandle field to true.
    @except Raises EInOutError if pipe can't be created.
  }
begin
  Create(0, Security);
end;

constructor TPJPipe.Create(const Inheritable: Boolean);
  {Class constructor. Creates pipe object with default size.
    @param Inheritable [in] Whether pipe handles are to be inheritable.
    @except Raises EInOutError if pipe can't be created.
  }
begin
  Create(0, Inheritable);
end;

procedure TPJPipe.CreatePipe(const Size: LongWord;
  const Security: PSecurityAttributes);
  {Creates Windows pipe using Windows API and records pipe handles.
    @param Size [in] Size of pipe. If Size is 0 default size pipe is created.
    @param Security [in] Pointer to security structure: may be nil.
    @except Raises EInOutError if pipe can't be created.
  }
begin
  if not Windows.CreatePipe(fReadHandle, fWriteHandle, Security, Size) then
    raise EInOutError.CreateFmt(
      sCantCreatePipe, [SysErrorMessage(GetLastError)]
    );
end;

destructor TPJPipe.Destroy;
  {Class destructor. Tears down object.
  }
begin
  CloseHandle(fReadHandle);
  CloseWriteHandle;
  inherited;
end;

function TPJPipe.ReadBytes(const Count: LongWord): TBytes;
  {Reads data from pipe into a byte array.
    @param Count [in] Number of bytes to read from pipe. If Count = 0 or is
      greater than number of available bytes then all the data from the pipe is
      read.
    @return Byte array containing data.
    @except EInOutError raised if there is an error peeking or reading pipe.
  }
var
  AvailBytes: LongWord; // number of bytes in pipe
  BytesRead: LongWord;  // number of bytes actually read from pipe
begin
  AvailBytes := AvailableDataSize;
  if (Count = 0) or (Count > AvailBytes) then
    SetLength(Result, AvailBytes)
  else
    SetLength(Result, Count);
  ReadData(Pointer(Result)^, Length(Result), BytesRead);
  if BytesRead <> LongWord(Length(Result)) then
    raise EInOutError.Create(sPipeReadError);
end;

function TPJPipe.ReadData(out Buf; const BufSize: LongWord;
  out BytesRead: LongWord): Boolean;
  {Reads data from pipe into a buffer.
    @param Buf [out] Buffer receiving data. Must have capacity of at least
      BufSize bytes.
    @param BufSize [in] Size of buffer or number of bytes requested.
    @param BytesRead [out] Set to number of bytes actually read.
    @return True if some data was read, false if not.
    @except EInOutError raised if there is an error peeking pipe.
  }
var
  BytesToRead: DWORD;   // number of bytes to actually read
begin
  BytesToRead := AvailableDataSize;
  if BytesToRead > 0 then
  begin
    if BytesToRead > BufSize then
      BytesToRead := BufSize;
    // we don't check return value: sometimes fails with BytesRead = 0
    ReadFile(fReadHandle, Buf, BytesToRead, BytesRead, nil);
    Result := BytesRead > 0;
  end
  else
  begin
    Result := False;
    BytesRead := 0;
  end;
end;

procedure TPJPipe.WriteBytes(const Bytes: TBytes);
  {Writes the whole content of a byte array to pipe.
    @except EInOutError raised if pipe's write handle has been closed or if
      can't write to pipe.
  }
begin
  WriteData(Pointer(Bytes)^, Length(Bytes));
end;

function TPJPipe.WriteData(const Buf; const BufSize: LongWord): LongWord;
  {Writes data from buffer to pipe.
    @param Buf [in] Buffer containing data to be written.
    @param BufSize [in] Number of bytes to write from buffer. Buf must have
      capacity of at least BufSize bytes.
    @return Number of bytes actually written.
    @except EInOutError raised if pipe's write handle has been closed or if
      can't write to pipe.
  }
begin
  CheckWriteHandle;
  if not WriteFile(fWriteHandle, Buf, BufSize, Result, nil) then
    raise EInOutError.Create(sPipeWriteError);
end;

end.

