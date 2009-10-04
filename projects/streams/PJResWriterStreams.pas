{
 * PJResWriterStreams.pas
 *
 * Classes that can write data to resource files.
 *
 * Deprecated. Do not use in new projects. Use the Resource File Unit instead.
 * Available from http://www.delphidabbler.com/software/resfile.
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
 * The Original Code is PJResWriterStreams.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit PJResWriterStreams;

interface

uses
  // Delphi
  SysUtils, Classes, Windows,
  // Stream Extension Classes unit
  PJStreamWrapper;

type

  TPJResHeader = class;


  {
  EPJResStreamWriter:
    Exceptions raised by the TPJResStreamWriter class.
  }
  EPJResStreamWriter = class(Exception);


  {
  TPJCustomResWriterStream:
    Base class for all resource stream writer classes. Provides some basic
    functionality required by all streams involved in writing resource files.
  }
  TPJCustomResWriterStream = class(TPJStreamWrapper)
  private // properties
    fLogicalStart: LongInt;
  protected
    property LogicalStart: LongInt read fLogicalStart write fLogicalStart;
      {Exposes logical start point of stream (in terms of physical stream) to
      descendant classes}
  public
    function Read(var Buffer; Count: Longint): Longint; override;
      {Raises exception - this is a write-only class}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
      {Moves the stream's position to Offset bytes from the given origin - see
      TStream for values of Origin. This operation may cause an exception if the
      underlying stream does not support Seek. This method adjusts seek
      operations on underlying stream to be relative to logical start of stream
      - which excludes header records and any other data in a resource file}
  end;


  {
  TPJResWriterStream:
    Class that writes a resource file in correct format. This class should not
    be used on its own, but should be wrapped by a TPJResDataStream class that
    writes individual resources to the resource file/stream.
  }
  TPJResWriterStream = class(TPJCustomResWriterStream)
  public
    constructor Create(const Stream: TStream;
      const CloseStream: Boolean = False); override;
      {Constructor creates object to use underlying TStream object for all
      output to resource file. Writes the resource file's header record. If
      CloseStream is true the given underlying stream is freed when this object
      is freed}
  end;


  {
  TPJResDataStream:
    Class that writes data to a resource of a specified type within a resource
    file. A wrapped TPJResWriterStream object is used to handle underlying file.
  }
  TPJResDataStream = class(TPJCustomResWriterStream)
  private // properties
    fLanguageID: WORD;
  private
    fHeaderPos: LongInt;
      {The location of the header in the underlying stream}
    fResHeader: TPJResHeader;
      {The resource header}
  public
    constructor Create(const ResId: PChar; const ResType: PChar;
      const Stream: TPJResWriterStream; const CloseStream: Boolean = False);
      reintroduce; overload;
    constructor Create(const ResId: string; const ResType: PChar;
      const Stream: TPJResWriterStream; const CloseStream: Boolean = False);
      reintroduce; overload;
    constructor Create(const ResId: Integer; const ResType: PChar;
      const Stream: TPJResWriterStream; const CloseStream: Boolean = False);
      reintroduce; overload;
      {Class constructors to create object to write a resource of type ResType
      with id ResId using underlying TPJResWriterStream object for all i/o.
      Writes a placeholder for the resource's header record. If CloseStream is
      true the underlying TPJResWriterStream object is freed when this object is
      freed. The overloaded constructors allow ResId to be specified as a
      string, a PChar or an integer (LoWord only used). The methods are re-
      introduced - they hide an inherited virtual constructor}
    destructor Destroy; override;
      {Destructor that overwrites resource header record placeholder (written by
      constructor) with one that contains all required information. Also pads
      resource out to a DWORD boundary}
    property LanguageID: WORD read fLanguageID write fLanguageID;
      {The language ID written to the header record of the resource}
  end;


  {
  TPJResHeaderPrefix:
    Record containing fixed size information at start of a resource header,
    before the variable size part.
  }
  TPJResHeaderPrefix = record
    DataSize: DWORD;        // size of resource data exc padding before next res
    HeaderSize: DWORD;      // size of resource data header
  end;


  {
  TPJResHeaderPostfix:
    Record containing fixed size information at end of a resoure header,
    following the variable size part.
  }
  TPJResHeaderPostfix = record
    DataVersion: DWORD;     // version of the data resource
    MemoryFlags: WORD;      // describe the state of the resource
    LanguageId: WORD;       // language for the resource
    Version: DWORD;         // user defined res version
    Characteristics: DWORD; // user defined info about res
  end;


  {
  TPJResHeader:
    Class used by other classes to create a resource file header record of
    required kind. Properties of the header are exposed and the class can write
    the header to the current location in any given stream.
  }
  TPJResHeader = class(TObject)
  private // properties
    function GetCharacteristics: DWORD;
    function GetDataSize: DWORD;
    function GetDataVersion: DWORD;
    function GetHeaderSize: DWORD;
    function GetLanguageID: WORD;
    function GetMemoryFlags: WORD;
    function GetVersion: DWORD;
    procedure SetCharacteristics(const Value: DWORD);
    procedure SetDataSize(const Value: DWORD);
    procedure SetDataVersion(const Value: DWORD);
    procedure SetLanguageID(const Value: WORD);
    procedure SetVersion(const Value: DWORD);
  private
    fPResName: PWideChar;
      {Pointer to start of resource name in header}
    fPResType: PWideChar;
      {Pointer to start of resoure type in header}
    fPResHeaderPrefix: ^TPJResHeaderPrefix;
      {Pointer to fixed-size prefix record in header}
    fPResHeaderPostfix: ^TPJResHeaderPostfix;
      {Pointer to fixed-size postfix record in header}
    fHeaderRecSize: Integer;
      {Size of the header record}
    fPHeader: Pointer;
      {Pointer to the whole header record}
    function NameOrTypeBufSize(ResNameOrType: PChar): Integer;
      {Size of buffer required to hold given resource name or resource type}
    procedure StoreNameOrType(ResNameOrType: PChar; Buf: PWideChar);
      {Stores given resource name or type in the given location in the resource
      header}
    procedure AllocateHeader(const ResName, ResType: PChar);
      {Allocates storage for the resource header}
    procedure DeallocateHeader;
      {Deallocates storage used for resource header}
  public
    constructor Create(const ResName: PChar; const ResType: PChar;
      const MemFlags: WORD);
      {Class constructor - creates a header record for the given resource name
      and type, with the memory flags set to the given value. All other
      properties are set to zero}
    destructor Destroy; override;
      {Frees storage used for header record}
    procedure WriteToStream(const Stream: TStream);
      {Writes header record to stream}
    property DataSize: DWORD
      read GetDataSize write SetDataSize;
      {The size of the resource data, excluding padding}
    property HeaderSize: DWORD
      read GetHeaderSize;
      {The size of the header record}
    property DataVersion: DWORD
      read GetDataVersion write SetDataVersion;
      {Predefined data resource version information}
    property MemoryFlags: WORD
      read GetMemoryFlags;
      {Attribute flags specifying state of resource - many ignored under Win 32}
    property LanguageID: WORD
      read GetLanguageID write SetLanguageID;
      {The language for the resource}
    property Version: DWORD
      read GetVersion write SetVersion;
      {User specified version number for the resource data}
    property Characteristics: DWORD
      read GetCharacteristics write SetCharacteristics;
      {User defined information about the resource}
  end;


implementation


resourcestring
  {Error messages}
  sCantRead = 'Can''t read from a %s';


{ Helper function }

function PaddingRequired(const ANum: LongInt; const PadTo: Integer): Integer;
  {Returns number of bytes padding required to increase ANum to a multiple of
  PadTo}
begin
  if ANum mod PadTo = 0 then
    Result := 0
  else
    Result := PadTo - ANum mod PadTo;
end;


{ TPJCustomResWriterStream }

function TPJCustomResWriterStream.Read(var Buffer; Count: Integer): Longint;
  {Raises exception - this is a write-only class}
begin
  // We can't read from this stream - so raise exception if anyone tries
  raise EPJResStreamWriter.CreateFmt(sCantRead, [ClassName]);
end;

function TPJCustomResWriterStream.Seek(Offset: Integer; Origin: Word): Longint;
  {Moves the stream's position to Offset bytes from the given origin - see
  TStream for values of Origin. This operation may cause an exception if the
  underlying stream does not support Seek. This method adjusts seek operations
  on underlying stream to be relative to logical start of stream - which
  excludes header records and any other data in a resource file}
begin
  // Adjust Offset to allow for logical start point of stream being different
  // to physical start
  Result := 0;            // prevents compiler warnings
  case Origin of
    soFromBeginning:      // seeking from start - adjust for logical start
    begin
      // if offset is -ve, set to zero
      if Offset < 0 then
        Offset := 0;
      // now increase offset by start of logical data
      Inc(Offset, fLogicalStart);
      Result := BaseStream.Seek(Offset, soFromBeginning);
    end;
    soFromCurrent:      // seeking from current pos
    begin
      if BaseStream.Position + Offset < fLogicalStart then
        // offset takes before logical start of data, so move to there
        Result := BaseStream.Seek(fLogicalStart, soFromBeginning)
      else
        // offset is not a problem, apply it
        Result := BaseStream.Seek(Offset, soFromCurrent);
    end;
    soFromEnd:          // seeking from end
    begin
      if BaseStream.Size + Offset < fLogicalStart then
        // offset takes before logical start of data, so move to there
        Result := BaseStream.Seek(fLogicalStart, soFromBeginning)
      else
        // offset is not a problem, apply it
        Result := BaseStream.Seek(Offset, soFromEnd);
    end;
  end;
  // Adjust result to be relative to logical start of stream
  Result := Result - fLogicalStart;
end;


{ TPJResWriterStream }

constructor TPJResWriterStream.Create(const Stream: TStream;
  const CloseStream: Boolean);
  {Constructor creates object to use underlying TStream object for all output to
  resource file. Writes the resource file's header record. If CloseStream is
  true the given underlying stream is freed when this object is freed}
var
  Header: TPJResHeader;      // the resource file header
begin
  inherited Create(Stream, CloseStream);
  // Create a suitable header record for resource file and write it
  Header := TPJResHeader.Create(MakeIntResource(0), MakeIntResource(0), 0);
  try
    Header.DataSize := 0;
    Header.WriteToStream(BaseStream);
  finally
    Header.Free;
  end;
  // Record logical start of resource stream (just after header record)
  LogicalStart := BaseStream.Position;
end;


{ TPJResDataStream }

constructor TPJResDataStream.Create(const ResId: Integer; const ResType: PChar;
  const Stream: TPJResWriterStream; const CloseStream: Boolean);
  {Constructor creates object to write a resource with id ResId. This id is
  converted into a suitable PChar value}
begin
  Create(MakeIntResource(ResId), ResType, Stream, CloseStream);
end;

constructor TPJResDataStream.Create(const ResId: PChar; const ResType: PChar;
  const Stream: TPJResWriterStream; const CloseStream: Boolean);
  {Constructor creates object to write a resource with id ResId using underlying
  TPJResWriterStream object for all i/o. Writes a placeholder for the resource's
  header record. If CloseStream is true the underlying TPJResWriterStream object
  is freed when this object is freed.}
begin
  inherited Create(Stream, CloseStream);
  // Record current position in underlying stream - where header will be
  fHeaderPos := BaseStream.Position;
  // Write placeholder header for resource stream
  fResHeader := TPJResHeader.Create(ResId, ResType, $0030);
  fResHeader.WriteToStream(BaseStream);
  // Record logical start of stream - just after resource header
  LogicalStart := BaseStream.Position;
end;

constructor TPJResDataStream.Create(const ResId: string; const ResType: PChar;
  const Stream: TPJResWriterStream; const CloseStream: Boolean);
  {Constructor creates object to write a resource with id string ResId. This id
  is converted into a suitable PChar value}
begin
  Create(PChar(ResId), ResType, Stream, CloseStream);
end;

destructor TPJResDataStream.Destroy;
  {Destructor that overwrites resource header record placeholder (written by
  constructor) with one that contains all required information. Also pads
  resource out to a DWORD boundary}
var
  Pad: array[1..SizeOf(DWORD)] of Byte;     // padding of zero bytes
  Padding: Integer;         // number of bytes of padding required
begin
  // Update header with size of data and any lanaguage ID assigned by user and
  // re-write it
  fResHeader.DataSize := Size;
  fResHeader.LanguageID := fLanguageID;
  // return to position of header in underlying stream to rewrite header
  // (header has to be written *before* logical start of this stream)
  BaseStream.Seek(fHeaderPos, soFromBeginning);
  fResHeader.WriteToStream(BaseStream);
  // we've done with the header now
  fResHeader.Free;
  // Move back to end of stream
  BaseStream.Seek(0, soFromEnd);
  // Pad data to a DWORD boundary
  Padding := PaddingRequired(Size, SizeOf(DWORD));
  if Padding <> 0 then
  begin
    FillChar(Pad, SizeOf(Pad), 0);
    BaseStream.Write(Pad, Padding);
  end;
  // Call inherited destructor to close underlying stream if required
  inherited Destroy;
end;


{ TPJResHeader }

procedure TPJResHeader.AllocateHeader(const ResName, ResType: PChar);
  {Allocates storage for the resource header}
var
  ResNameSize: Integer;   // size of resource name buffer
  ResTypeSize: Integer;   // size of resource type buffer
  Padding: Integer;       // padding require in header after resource name
  P: PByte;               // points to start of various header sub-records
begin
  // Calculate size of header required
  ResNameSize := NameOrTypeBufSize(ResName);
  ResTypeSize := NameOrTypeBufSize(ResType);
  Padding := PaddingRequired(ResNameSize + ResTypeSize, SizeOf(DWORD));
  fHeaderRecSize := SizeOf(TPJResHeaderPrefix) + SizeOf(TPJResHeaderPostfix)
    + ResNameSize + ResTypeSize + Padding;
  // Allocate header and set bytes to zero
  GetMem(fPHeader, fHeaderRecSize);
  FillChar(fPHeader^, fHeaderRecSize, #0);
  // Record pointers to data items
  // .. resource header prefix record
  P := fPHeader;
  fPResHeaderPrefix := Pointer(P);
  // .. resource type
  Inc(P, SizeOf(TPJResHeaderPrefix));
  fPResType := Pointer(P);
  // .. resource name
  Inc(P, ResTypeSize);
  fPResName := Pointer(P);
  // .. resource header postfix record
  Inc(P, ResNameSize);
  Inc(P, PaddingRequired(ResTypeSize + ResNameSize, SizeOf(DWORD)));
  fPResHeaderPostfix := Pointer(P);
  // Record header buffer size in buffer
  fPResHeaderPrefix^.HeaderSize := fHeaderRecSize;
end;

constructor TPJResHeader.Create(const ResName, ResType: PChar;
  const MemFlags: WORD);
  {Class constructor - creates a header record for the given resource name and
  type, with the memory flags set to the given value. All other properties are
  set to zero}
begin
  inherited Create;
  AllocateHeader(ResName, ResType);             // create the header
  StoreNameOrType(ResName, fPResName);          // store res name in header
  StoreNameOrType(ResType, fPResType);          // store res type in header
  fPResHeaderPostfix^.MemoryFlags := MemFlags;  // store memory flags in header
end;

procedure TPJResHeader.DeallocateHeader;
  {Deallocates storage used for resource header}
begin
  if fHeaderRecSize <> 0 then
    FreeMem(fPHeader, fHeaderRecSize);
end;

destructor TPJResHeader.Destroy;
  {Frees storage used for header record}
begin
  DeallocateHeader;
  inherited Destroy;
end;

function TPJResHeader.GetCharacteristics: DWORD;
  {Read access method for Characteristics property}
begin
  Result := fPResHeaderPostfix^.Characteristics;
end;

function TPJResHeader.GetDataSize: DWORD;
  {Read access method for DataSize property}
begin
  Result := fPResHeaderPrefix^.DataSize;
end;

function TPJResHeader.GetDataVersion: DWORD;
  {Read access method for DataVersion property}
begin
  Result := fPResHeaderPostfix^.DataVersion;
end;

function TPJResHeader.GetHeaderSize: DWORD;
  {Read access method for HeaderSize property}
begin
  Result := fPResHeaderPrefix^.HeaderSize;
end;

function TPJResHeader.GetLanguageID: WORD;
  {Read access method for LanguageID property}
begin
  Result := fPResHeaderPostfix^.LanguageId;
end;

function TPJResHeader.GetMemoryFlags: WORD;
  {Read access method for MemoryFlags property}
begin
  Result := fPResHeaderPostfix^.MemoryFlags;
end;

function TPJResHeader.GetVersion: DWORD;
  {Read access method for Version property}
begin
  Result := fPResHeaderPostfix^.Version;
end;

function TPJResHeader.NameOrTypeBufSize(ResNameOrType: PChar): Integer;
  {Size of buffer required to hold given resource name or resource type}
begin
  if HiWord(DWORD(ResNameOrType)) = 0 then
    // This is an integer valued id - we store it in a DWORD size record
    Result := SizeOf(DWORD)
  else
    // This is a string valued id - we store it as 0 terminated Unicode
    Result := SizeOf(WideChar) * (StrLen(ResNameOrType) + 1);
end;

procedure TPJResHeader.SetCharacteristics(const Value: DWORD);
  {Write access method for Characteristics property}
begin
  fPResHeaderPostfix^.Characteristics := Value;
end;

procedure TPJResHeader.SetDataSize(const Value: DWORD);
  {Write access method for DataSize property}
begin
  fPResHeaderPrefix^.DataSize := Value;
end;

procedure TPJResHeader.SetDataVersion(const Value: DWORD);
  {Write access method for DataVersion property}
begin
  fPResHeaderPostfix^.DataVersion := Value;
end;

procedure TPJResHeader.SetLanguageID(const Value: WORD);
  {Write access method for LanguageID property}
begin
  fPResHeaderPostfix^.LanguageId := Value;
end;

procedure TPJResHeader.SetVersion(const Value: DWORD);
  {Write access method for Version property}
begin
  fPResHeaderPostfix^.Version := Value;
end;

procedure TPJResHeader.StoreNameOrType(ResNameOrType: PChar; Buf: PWideChar);
  {Stores given resource name or type in the given location in the resource
  header}
var
  Size: Integer;    // size of string (for string valued id)
  Str: string;      // the string to be stored (for string valued id)
  Value: DWORD;     // the value to be stored (for integer valued id)
begin
  if HiWord(DWORD(ResNameOrType)) = 0 then
  begin
    // This is an integer valued id - we store it with FFFF as high word
    Value := $0000FFFF or (DWORD(LoWord(ResNameOrType)) shl 16);
    Move(Value, Buf^, SizeOf(DWORD));
  end
  else
  begin
    // This is string valued id - we store it as a 0 terminated Unicode string
    Str := StrPas(ResNameOrType);       // convert to string
    Size := Length(ResNameOrType) + 1;  // record buffer size
    StringToWideChar(Str, Buf, Size);   // convert Unicode
  end;
end;

procedure TPJResHeader.WriteToStream(const Stream: TStream);
  {Writes header record to stream}
var
  PHeader: PByte;     // points to header record
begin
  // Record pointer to array of bytes that is header record
  PHeader := fPHeader;
  // Write the array of bytes to stream
  Stream.WriteBuffer(PHeader^, fHeaderRecSize);
end;

end.
