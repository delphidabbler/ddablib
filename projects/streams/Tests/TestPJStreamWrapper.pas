{
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit TestPJStreamWrapper;

{$UNDEF SUPPORTS_TSTREAM64}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 14.0} // >= Delphi 6
    {$DEFINE SUPPORTS_TSTREAM64}
  {$IFEND}
{$ENDIF}

interface

uses
  // DUnit
  TestFrameWork,
  // Delphi
  Classes,
  // Project
  PJStreamWrapper;

type

  // Special class used un tests that records itself in list when created and
  // removes itself when destroyed
  TTestStream = class(TStringStream)
  public
    constructor Create(const AString: string);
    destructor Destroy; override;
  end;

  TTestPJStreamWrapper = class(TTestCase)
  private
    MS: TMemoryStream;
    SS: TStringStream;
    FS: TFileStream;
    function OneKbFileName: string;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateDestroy;
      {Test that wrapped owned streams are freed on destruction and non-owned
      streams are not}
    procedure TestSeek;
      {Test Seek method}
    procedure TestRead;
      {Test Read method}
    procedure TestWrite;
      {Test Write method}
    procedure TestSize;
      {Test Size property}
  end;

implementation

uses
  SysUtils, Windows {for inlining};

var
  ObList: TList;

{Clears global list of test objects, freeing items in list}
procedure ClearObList;
var
  ObIdx: Integer;
begin
  for ObIdx := Pred(ObList.Count) downto 0 do
    TTestStream(ObList[ObIdx]).Free;
end;

{ TTestPJStreamWrapper }

function TTestPJStreamWrapper.OneKbFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + '1KbTestFile';
end;

procedure TTestPJStreamWrapper.SetUp;
var
  S: AnsiString;
  Buf: array of Byte;
  I: Integer;
  NFS: TFileStream;
const
  CS: AnsiString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
begin
  S := '';
  while Length(S) < 1024 do
    S := S + CS;
  SetLength(S, 1024);
  SS := TStringStream.Create(S);
  Assert(SS.Size = 1024);

  SetLength(Buf, 1024);
  for I := Low(Buf) to High(Buf) do
    Buf[I] := I mod 256;
  MS := TMemoryStream.Create;
  MS.WriteBuffer(Pointer(Buf)^, Length(Buf));
  Assert(MS.Size = 1024);

  NFS := TFileStream.Create(OneKbFileName, fmCreate);
  try
    NFS.WriteBuffer(Pointer(Buf)^, Length(Buf));
  finally
    NFS.Free;
  end;
  FS := TFileStream.Create(OneKbFileName, fmOpenRead or fmShareDenyWrite);
end;

procedure TTestPJStreamWrapper.TearDown;
begin
  FS.Free;
  MS.Free;
  SS.Free;
  SysUtils.DeleteFile(OneKbFileName);
end;

procedure TTestPJStreamWrapper.TestCreateDestroy;
var
  S: TTestStream;
  W: TPJStreamWrapper;
begin
  ClearObList;
  S := TTestStream.Create(AnsiString('test'));
  // Check freeing wrapper stream doesn't free wrapper stream when not owned
  W := TPJStreamWrapper.Create(S, False);
  W.Free;
  CheckEquals(1, ObList.Count, 'Test 1');
  // Check freeing wrapper stream does free an owned wrapped stream
  W := TPJStreamWrapper.Create(S, True);
  W.Free;
  CheckEquals(0, ObList.Count, 'Test 2');
end;

procedure TTestPJStreamWrapper.TestRead;
var
  WS: TPJStreamWrapper;
  InStr: AnsiString;
  Count: Longint;
begin
  Assert(SS.Size = 1024);
  SS.Position := 0;
  WS := TPJStreamWrapper.Create(SS, False);
  try
    SetLength(InStr, 5);
    Count := WS.Read(Pointer(InStr)^, 5);
    CheckEquals(AnsiString('ABCDE'), InStr, 'Test 1a');
    CheckEquals(5, Count, 'Test 1b');

    SetLength(InStr, 1);
    Count := WS.Read(Pointer(InStr)^, 1);
    CheckEquals(AnsiString('F'), InStr, 'Test 2a');
    CheckEquals(1, Count, 'Test 2b');

    SetLength(InStr, 1024);
    Count := WS.Read(Pointer(InStr)^, 1024);
    CheckEquals(Count, 1018, 'Test 3');

    SS.Position := 9;
    SetLength(InStr, 4);
    Count := WS.Read(Pointer(InStr)^, 4);
    CheckEquals(AnsiString('JKLM'), InStr, 'Test 4a');
    CheckEquals(4, Count, 'Test 4b');
  finally
    WS.Free;
  end;
end;

procedure TTestPJStreamWrapper.TestSeek;

  procedure DoTest(const Stm: TStream);
  var
    WS: TPJStreamWrapper;
    Pos: Int64;
  begin
    WS := TPJStreamWrapper.Create(Stm, False);
    try
      Pos := WS.Seek(0, soFromBeginning);
      CheckEquals(0, Pos, Stm.ClassName + ': Test 1');
      Pos := WS.Seek(100, soFromBeginning);
      CheckEquals(100, Pos, Stm.ClassName + ': Test 2');
      Pos := WS.Seek(100, soFromCurrent);
      CheckEquals(200, Pos, Stm.ClassName + ': Test 3');
      Pos := WS.Seek(-50, soFromCurrent);
      CheckEquals(150, Pos, Stm.ClassName + ': Test 4');
      Pos := WS.Seek(1024, soFromBeginning);
      CheckEquals(1024, Pos, Stm.ClassName + ': Test 5');
      Pos := WS.Seek(0, soFromEnd);
      CheckEquals(1024, Pos, Stm.ClassName + ': Test 6');
      Pos := WS.Seek(-24, soFromEnd);
      CheckEquals(1000, Pos, Stm.ClassName + ': Test 7');
      Pos := WS.Seek(-1024, soFromEnd);
      CheckEquals(0, Pos, Stm.ClassName + ': Test 8');
      {$IFDEF SUPPORTS_TSTREAM64}
      Pos := WS.Seek(0, soBeginning);
      CheckEquals(0, Pos, Stm.ClassName + ': Test A1');
      Pos := WS.Seek(100, soBeginning);
      CheckEquals(100, Pos, Stm.ClassName + ': Test A2');
      Pos := WS.Seek(100, soCurrent);
      CheckEquals(200, Pos, Stm.ClassName + ': Test A3');
      Pos := WS.Seek(-50, soCurrent);
      CheckEquals(150, Pos, Stm.ClassName + ': Test A4');
      Pos := WS.Seek(1024, soBeginning);
      CheckEquals(1024, Pos, Stm.ClassName + ': Test A5');
      Pos := WS.Seek(0, soEnd);
      CheckEquals(1024, Pos, Stm.ClassName + ': Test A6');
      Pos := WS.Seek(-24, soEnd);
      CheckEquals(1000, Pos, Stm.ClassName + ': Test A7');
      Pos := WS.Seek(-1024, soEnd);
      CheckEquals(0, Pos, Stm.ClassName + ': Test A8');
      {$ENDIF}
    finally
      WS.Free;
    end;
  end;
begin
  DoTest(MS);
  DoTest(FS);
  DoTest(SS);
end;

procedure TTestPJStreamWrapper.TestSize;
var
  S: TStringStream;
  W: TPJStreamWrapper;
begin
  S := TStringStream.Create(AnsiString('Hello'));
  W := TPJStreamWrapper.Create(S, True);
  try
    // Check size starts as 5 (test stream length and data string length)
    CheckEquals(5, W.Seek(0, soFromEnd), 'Test 1a');
    CheckEquals(5, Length(S.DataString), 'Test 1b');
    // Check size starts as 3 (test stream length and data string length)
    W.Size := 3;
    CheckEquals(3, W.Size, 'Test 2a');
    CheckEquals(3, Length(S.DataString), 'Test 2b');
    // Check size starts as 64 (test stream length and data string length)
    W.Size := 64;
    CheckEquals(64, W.Size, 'Test 3a');
    CheckEquals(64, Length(S.DataString), 'Test 3b');
  finally
    W.Free;
  end;
end;

procedure TTestPJStreamWrapper.TestWrite;
var
  SS: TStringStream;
  WS: TPJStreamWrapper;
  OutStr: AnsiString;
begin
  SS := TStringStream.Create(AnsiString(''));
  WS := TPJStreamWrapper.Create(SS, True);
  try
    // Write 'Hello' to empty string stream
    OutStr := 'Hello';
    CheckEquals(5, WS.Write(Pointer(OutStr)^, 5), 'Test 1a');
    CheckEquals('Hello', SS.DataString, 'Test 1b');
    // Write ' there' to current stream
    OutStr := ' there';
    CheckEquals(6, WS.Write(Pointer(OutStr)^, 6), 'Test 2a');
    CheckEquals('Hello there', SS.DataString, 'Test 2b');
    // Now overwrite 'there' with 'World': use Position on underlying stream to
    // reposition stream pointer: we don't use WS.Seek or WS.Position because
    // that must also be tested
    SS.Position := 6;
    OutStr := 'World';
    WS.Write(Pointer(OutStr)^, 5);
    CheckEquals(SS.Position, WS.Position, 'Test 3a');
    CheckEquals('Hello World', SS.DataString, 'Test 3b');
  finally
    WS.Free;
  end;
end;

{ TTestStream }

constructor TTestStream.Create(const AString: string);
begin
  inherited;
  ObList.Add(Self);
end;

destructor TTestStream.Destroy;
begin
  ObList.Remove(Self);
  inherited;
end;

initialization

ObList := TList.Create;

TestFramework.RegisterTest(TTestPJStreamWrapper.Suite);

finalization

ClearObList;
ObList.Free;

end.

