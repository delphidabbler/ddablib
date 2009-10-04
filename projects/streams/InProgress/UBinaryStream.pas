{##
  @PROJECT_NAME   SI Tools
  @PROJECT_DESC   Simple program installation tools - project manager and
                  creator with installation and un-installation programs.

  @FILE           UBinaryStream.pas
  @VERSION        1.0
  @DATE           09/03/2000
  @COMMENTS       This unit is shared among two or more SITools applications.
                  It defines the UBinaryStream class. This descendant of
                  TDataStream implements methods that read and write to/from
                  binary representations of integers strings using a wrapped
                  TStream object for i/o.

  @FILE_HISTORY(
    @REVISION(
      @VERSION    1.0
      @DATE       09/03/2000
      @COMMENTS   Original version.
    )
  )
}


unit UBinaryStream;

interface

uses
  // Project
  UDataStream;

type
  {TDataStream descendant that reads and writes to/from binary representations
  of integers and strings using wrapped TStream object}
  TBinaryStream = class(TDataStream)
  protected
    function ReadChar: Char; virtual;
      {Reads a character from underlying stream and returns it}
    procedure WriteChar(Value: Char); virtual;
      {Writes the given character to the underlying stream}
  public
    function ReadBoolean: Boolean; override;
      {Reads a Boolean value from underlying stream and returns it}
    function ReadInt8: ShortInt; override;
      {Reads 8 bit value from underlying stream and returns it as a signed
      value}
    function ReadInt16: SmallInt; override;
      {Reads 16 bit value from underlying stream and returns it as a signed
      value}
    function ReadInt32: Integer; override;
      {Reads 32 bit value from underlying stream and returns it as a signed
      value}
    function ReadZString: AnsiString; override;
      {Reads a zero terminated string from underlying stream. Returns the
      string}
    procedure WriteBoolean(Value: Boolean); override;
      {Writes the given Boolean value to the underlying stream}
    procedure WriteInt8(Value: ShortInt); override;
      {Writes the given 8 bit value to the underlying stream}
    procedure WriteInt16(Value: SmallInt); override;
      {Writes the given 16 bit value to the underlying stream}
    procedure WriteInt32(Value: Integer); override;
      {Writes the given 32 bit value to the underlying stream}
    procedure WriteZString(Value: AnsiString); override;
      {Writes the given string to the underlying stream. The string is
      terminated in the stream by a #0 character}
  end;

implementation

uses
  // VCL
  SysUtils;

{ TBinaryStream }

function TBinaryStream.ReadBoolean: Boolean;
  {Reads a Boolean value from underlying stream and returns it}
begin
  // Read a Boolean value from the underlying stream
  ReadBuffer(Result, SizeOf(Boolean));
end;

function TBinaryStream.ReadChar: Char;
  {Reads a character from underlying stream and returns it}
begin
  // Read a char from the underlying stream
  ReadBuffer(Result, SizeOf(Char));
end;

function TBinaryStream.ReadInt16: SmallInt;
  {Reads 16 bit value from underlying stream and returns it as a signed value}
begin
  // Read a 16 bit integer from the underlying stream
  ReadBuffer(Result, SizeOf(SmallInt));
end;

function TBinaryStream.ReadInt32: Integer;
  {Reads 32 bit value from underlying stream and returns it as a signed value}
begin
  // Read a 32 bit integer from the underlying stream
  ReadBuffer(Result, SizeOf(Integer));
end;

function TBinaryStream.ReadInt8: ShortInt;
  {Reads 8 bit value from underlying stream and returns it as a signed value}
begin
  // Read a 8 bit integer from the underlying stream
  ReadBuffer(Result, SizeOf(ShortInt));
end;

function TBinaryStream.ReadZString: AnsiString;
  {Reads a zero terminated string from underlying stream. Returns the string}
var
  Ch: Char;     // stores each char read from stream
begin
  // Set result to empty string
  Result := '';
  // Read first char
  Ch := ReadChar;
  // Keep adding characters to string until a zero char is read
  while Ch <> #0 do
  begin
    Result := Result + Ch;
    Ch := ReadChar;
  end;
end;

procedure TBinaryStream.WriteBoolean(Value: Boolean);
  {Writes the given Boolean value to the underlying stream}
begin
  // Write the Boolean value to the underlying stream
  WriteBuffer(Value, SizeOf(Boolean));
end;

procedure TBinaryStream.WriteChar(Value: Char);
  {Writes the given character to the underlying stream}
begin
  // Write the character to the underlying stream
  WriteBuffer(Value, SizeOf(Char));
end;

procedure TBinaryStream.WriteInt16(Value: SmallInt);
  {Writes the given 16 bit value to the underlying stream}
begin
  // Write a 16 bit value to the underlying stream
  WriteBuffer(Value, SizeOf(SmallInt));
end;

procedure TBinaryStream.WriteInt32(Value: Integer);
  {Writes the given 32 bit value to the underlying stream}
begin
  // Write a 32 bit value to the underlying stream
  WriteBuffer(Value, SizeOf(Integer));
end;

procedure TBinaryStream.WriteInt8(Value: ShortInt);
  {Writes the given 8 bit value to the underlying stream}
begin
  // Write a 8 bit value to the underlying stream
  WriteBuffer(Value, SizeOf(ShortInt));
end;

procedure TBinaryStream.WriteZString(Value: AnsiString);
  {Writes the given string to the underlying stream. The string is terminated in
  the stream by a #0 character}
var
  PBuf: PChar;        // pointer to string's characters
begin
  // Write string's characters to stream
  PBuf := PChar(Value);
  WriteBuffer(PBuf^, Length(Value));
  // Write terminating 0
  WriteChar(#0);
end;

end.
