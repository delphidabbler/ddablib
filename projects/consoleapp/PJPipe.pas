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
  SysUtils, Classes, Windows;


// Ensure TBytes is defined
{$IF not Declared(TBytes)}
type
  TBytes = array of Byte;
{$IFEND}
// Ensure UnicodeString is defined
{$IFNDEF UNICODE}
type
  UnicodeString = WideString;
{$ENDIF}

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


type
  ///  <summary>Type of text read event triggered by classes that read Unicode
  ///  strings from text reader objects.</summary>
  ///  <param name="Sender">TObject [in] Object that triggers event.</param>
  ///  <param name="Text">UnicodeString [in] Text for which event was triggered.
  ///  </param>
  ///  <remarks>Type of OnText and OnLineEnd events.</remarks>
  TPJUnicodeTextReadEvent = procedure(Sender: TObject;
    const Text: UnicodeString) of object;

type
  ///  <summary>
  ///  Constructs valid Unicode strings from chunks of data that may or may not
  ///  be split on valid Unicode character boundaries. An event is triggered for
  ///  each string read. Strings are also parsed into lines that are split by
  ///  a specified end of line marker.
  ///  </summary>
  ///  <remarks>
  ///  The class only works for Unicode text from the basic multingual plane
  ///  (see http://bit.ly/f3Ardu).
  ///  </remarks>
  TPJUnicodeBMPTextReader = class(TObject)
  private
    ///  Records any bytes that were not processed in last call to AddData
    ///  because they are not part of a valid Unicode character. The data is
    ///  prepended to the next chunk of data read.
    fUnprocessedBytes: TBytes;
    ///  Text data buffered awaiting next end of line.
    fPartialLine: UnicodeString;
    ///  Stores reference to OnText event handler.
    fOnText: TPJUnicodeTextReadEvent;
    ///  Stores reference to OnLineEnd event handler.
    fOnLineEnd: TPJUnicodeTextReadEvent;
    ///  Value of EOLMarker property.
    fEOLMarker: UnicodeString;
    ///  <summary>Triggers OnText event for a given Unicode string.</summary>
    procedure DoText(const Text: UnicodeString);
    ///  <summary>Triggers OnLineEnd event for a given Unicode string.</summary>
    procedure DoLineEnd(const Text: UnicodeString);
  public
    ///  <summary>Object constructor. Sets default properties.</summary>
    constructor Create;
    ///  <summary>Object destructor. Flushes any un-flushed text line.</summary>
    destructor Destroy; override;
    ///  <summary>Adds a chunk of bytes to the object, extracting the longest
    ///  valid Unicode string that is represented by the data. Also parses the
    ///  text into logical lines.</summary>
    ///  <remarks>
    ///  <para>An OnText event is triggered for the extracted text and an
    ///  OnLineEnd event is triggered for each end of line marker
    ///  encountered.</para>
    ///  <para>Any data not used is buffered for use in the next call to this
    ///  method.</para>
    ///  </remarks>
    procedure AddData(const Data: TBytes);
    ///  <summary>Flushes any un-reported line of text, triggering OnLineEnd
    ///  event for it.</summary>
    procedure Flush;
    ///  <summary>Checks if object currently contains any un processed data.
    ///  </summary>
    ///  <remarks>Call this after last call to AddData to test if there is any
    ///  data left un-processed. If so the data stream is not valid Unicode.
    ///  </remarks>
    function HaveUnprocessedData: Boolean;
    ///  <summary>Character(s) to be used as end of line marker used when
    ///  parsing text into lines.</summary>
    ///  <remarks>Defaults to CRLF.</remarks>
    property EOLMarker: UnicodeString read fEOLMarker write fEOLMarker;
    ///  <summary>Event triggered whenever valid text is read in AddData.
    ///  Contains text up to and including last valid Unicode character in given
    ///  data.</summary>
    property OnText: TPJUnicodeTextReadEvent read fOnText write fOnText;
    ///  <summary>Event triggered when each end of line is reached. Also
    ///  triggered for any pending text when Flush is called, or object is
    ///  destroyed.</summary>
    property OnLineEnd: TPJUnicodeTextReadEvent
      read fOnLineEnd write fOnLineEnd;
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

{ TPJUnicodeBMPTextReader }

procedure TPJUnicodeBMPTextReader.AddData(const Data: TBytes);
var
  UnprocessedByteCount: Integer;  // number of byte un-processed (partial chars)
  Text: UnicodeString;            // text read
  EOLPos: Integer;                // position of EOL in Text
  Buffer: TBytes;                 // bytes to be processed
begin
  if Length(Data) = 0 then
    Exit;
  // Set up data to be processed in buffer. Starts with any as yet unprocessed
  // bytes followed by new data
  SetLength(Buffer, Length(fUnprocessedBytes) + Length(Data));
  if Length(fUnprocessedBytes) > 0 then
    Move(
      fUnprocessedBytes[0], Buffer[0], Length(fUnprocessedBytes)
    );
  Move(
    Data[0], Buffer[Length(fUnprocessedBytes)], Length(Data)
  );
  // Decide if there are any bytes not to be processed this time. In basic
  // multilingual plane all Unicode chars are 2 bytes, to we only need to store
  // any odd trailing bytes for later processing.
  UnprocessedByteCount := Length(Buffer) mod SizeOf(WideChar);
  if UnprocessedByteCount <> 0 then
    // we have bytes we won't process this time: record for use in next AddData
    fUnprocessedBytes := Copy(
      Buffer, Length(Buffer) - UnprocessedByteCount, UnprocessedByteCount
    )
  else
    // no unprocessed bytes
    SetLength(fUnprocessedBytes, 0);
  // Record text from valid bytes
  {$IFDEF UNICODE}
  Text := TEncoding.Unicode.GetString(
    Buffer, 0, Length(Buffer) - UnprocessedByteCount
  );
  {$ELSE}
  SetLength(Text, Length(Buffer) div SizeOf(WideChar));
  Move(Pointer(Buffer)^, Pointer(Text)^, Length(Buffer) - UnprocessedByteCount);
  {$ENDIF}
  // Trigger OnText event for read text
  DoText(Text);
  fPartialLine := fPartialLine + Text;
  // Break text into lines separated by EOLMarker, trigger OnLineEnd for each
  EOLPos := Pos(fEOLMarker, fPartialLine);
  while EOLPos > 0 do
  begin
    DoLineEnd(Copy(fPartialLine, 1, EOLPos - 1));
    fPartialLine := Copy(
      fPartialLine, EOLPos + Length(fEOLMarker), MaxInt
    );
    EOLPos := Pos(fEOLMarker, fPartialLine);
  end;
end;

constructor TPJUnicodeBMPTextReader.Create;
begin
  inherited Create;
  fEOLMarker := #13#10; // Default EOL marker
end;

destructor TPJUnicodeBMPTextReader.Destroy;
begin
  Flush;
  inherited;
end;

procedure TPJUnicodeBMPTextReader.DoLineEnd(const Text: UnicodeString);
begin
  if Assigned(OnLineEnd) then
    OnLineEnd(Self, Text);
end;

procedure TPJUnicodeBMPTextReader.DoText(const Text: UnicodeString);
begin
  if Assigned(OnText) then
    OnText(Self, Text);
end;

procedure TPJUnicodeBMPTextReader.Flush;
begin
  if fPartialLine <> '' then
  begin
    DoLineEnd(fPartialLine);
    fPartialLine := '';
  end;
end;

function TPJUnicodeBMPTextReader.HaveUnprocessedData: Boolean;
begin
  Result := Length(fUnprocessedBytes) <> 0;
end;

end.

