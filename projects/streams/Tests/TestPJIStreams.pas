unit TestPJIStreams;

interface

uses
  // DUnit
  TestFrameWork,
  // Delphi
  Classes,
  // Project
  PJIStreams;

type

  // Special class used un tests that records itself in list when created and
  // removes itself when destroyed
  TTestStream = class(TStringStream)
  public
    constructor Create(const AString: string);
    destructor Destroy; override;
  end;

  TTestPJIStreamWrapper = class(TTestCase)
  published
    procedure TestCreateDestroy;
      {Test that wrapped owned streams are freed on destruction and non-owned
      streams are not}
    procedure TestReadAndSeek;
      {Tests Read and Seek methods of IStream}
    procedure TestWriteAndSeek;
      {Test Write and Seek methods of IStream}
    procedure TestSize;
      {Test SetSize method of IStream}
    procedure TestCopyTo;
      {Text CopyTo methof of IStream}
    procedure TestNonImpl;
      {Test the return values of the non-implemented IStream methods are as
      expected}
    procedure TestStat;
      {Test Stat method of IStream}
  end;

  TTestPJHandleIStreamWrapper = class(TTestCase)
    // All methods of TPJHandleIStreamWrapper are inherited from
    // TPJIStreamWrapper except for Stat => we only test Stat here
  published
    procedure TestStat;
  end;

  TTestPJFileIStream = class(TTestCase)
    // All methods of TPJFileIStream are inherited from TPJIStreamWrapper except
    // for the protected GetStreamName which is overridden. Because of this we
    // just provide tests to test base stream name and for Stat
  published
    procedure TestStat;
  end;

implementation

uses
  // Delphi
  SysUtils, Windows, ActiveX;


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


// Some helper functions

function FTIsZero(FT: FILETIME): Boolean;
begin
  Result := (FT.dwLowDateTime = 0) and (FT.dwHighDateTime = 0);
end;

function GUIDIsZero(G: TGUID): Boolean;
var
  I: Integer;
begin
  Result := (G.D1 = 0) and (G.D2 = 0) and (G.D3 = 0);
  if Result then
    for I := Low(G.D4) to High(G.D4) do
      if G.D4[I] <> 0 then
      begin
        Result := False;
        Break;
      end;
end;

function GetTestFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + '..\TestStreamFile.txt';
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


{ TTestPJIStreamWrapper }

procedure TTestPJIStreamWrapper.TestCopyTo;
var
  S1, S2: TStringStream;
  Stm1, Stm2: IStream;
  R, W: LargeInt;
begin
  // We copy 'Dabbler' from S2 and overwrite 'World' in S1
  S1 := TStringStream.Create(AnsiString('Hello World'));
  S2 := TStringStream.Create(AnsiString('delphiDabbler!'));
  Stm1 := TPJIStreamWrapper.Create(S1, True);
  Stm2 := TPJIStreamWrapper.Create(S2, True);
  Stm1.Seek(6, STREAM_SEEK_SET, Largeint(nil^));  // start of "World"
  Stm2.Seek(6, STREAM_SEEK_SET, Largeint(nil^));  // start of "Dabbler"
  // do the copy
  Stm2.CopyTo(Stm1, 7, R, W);
  CheckEquals(7, R, 'Test 1');   // bytes read
  CheckEquals(7, W, 'Test 2');   // bytes written
  CheckEquals('Hello Dabbler', S1.DataString, 'Test 3');
  // Now we try to copy all of S2 + a few extra bytes to same place in S1
  Stm2.Seek(0, STREAM_SEEK_SET, Largeint(nil^));  // wind to start of S2
  Stm1.Seek(6, STREAM_SEEK_SET, Largeint(nil^));  // start of "Dabbler"
  Stm2.CopyTo(Stm1, 64, R, W);
  CheckEquals(Length('delphiDabbler!'), R, 'Test 4');
  CheckEquals(True, W = R, 'Test 5');
  CheckEquals('Hello delphiDabbler!', S1.DataString, 'Test 6');
end;

procedure TTestPJIStreamWrapper.TestCreateDestroy;
var
  S: TTestStream;
  Stm: IStream;
begin
  ClearObList;
  S := TTestStream.Create('test');
  // Check freeing wrapper stream doesn't free owned stream when not required
  Stm := TPJIStreamWrapper.Create(S, False);
  Stm := nil;
  CheckEquals(1, ObList.Count, 'Test 1');
  // Check freeing wrapper stream does free owned stream when required
  Stm := TPJIStreamWrapper.Create(S, True);
  Stm := nil;
  CheckEquals(0, ObList.Count, 'Test 2');
end;

procedure TTestPJIStreamWrapper.TestNonImpl;
var
  S: TStringStream;
  Stm: IStream;
begin
  S := TStringStream.Create(AnsiString('Hello World'));
  Stm := TPJIStreamWrapper.Create(S, True);
  CheckEquals(S_OK, Stm.Commit(0), 'Test 1');
  CheckEquals(STG_E_REVERTED, Stm.Revert, 'Test 2');
  CheckEquals(STG_E_INVALIDFUNCTION, Stm.LockRegion(0, 0, 0), 'Test 3');
  CheckEquals(STG_E_INVALIDFUNCTION, Stm.UnlockRegion(0, 0, 0), 'Test 4');
end;

procedure TTestPJIStreamWrapper.TestReadAndSeek;
var
  S: TStringStream;
  Stm: IStream;
  InStr: AnsiString;
  R: LongInt;
  P: Largeint;
begin
  S := TStringStream.Create(AnsiString('Hello World'));
  Stm := TPJIStreamWrapper.Create(S, True);
  // Read 1st five chars('Hello');
  SetLength(InStr, 5);
  Stm.Read(PAnsiChar(InStr), 5, @R);
  CheckEquals(AnsiString('Hello'), InStr, 'Test 1a');
  CheckEquals(5, R, 'Test 1b');
  // Move to one char from end (pos should be 10)
  Stm.Seek(1, STREAM_SEEK_END, P);
  CheckEquals(10, P, 'Test 2');
  // Read one char: should be last one = 'd'
  SetLength(InStr, 1);
  Stm.Read(PAnsiChar(InStr), 1, nil);
  CheckEquals(AnsiString('d'), InStr, 'Test 3');
  // We should now be at end (P = 11)
  Stm.Seek(0, STREAM_SEEK_CUR, P);
  CheckEquals(11, P, 'Test 4');
  // Seek 6 chars in from start
  Stm.Seek(6, STREAM_SEEK_SET, P);
  CheckEquals(6, P, 'Test 5');
  // Try to read 64 chars from here: should read just last 5 = 'World'
  SetLength(InStr, 64);
  Stm.Read(PAnsiChar(InStr), 64, @R);
  CheckEquals(5, R, 'Test 6');
  SetLength(InStr, 5);
  CheckEquals(AnsiString('World'), InStr, 'Test 7');
end;

procedure TTestPJIStreamWrapper.TestSize;
var
  S: TStringStream;
  Stm: IStream;
  P: Largeint;
begin
  S := TStringStream.Create(AnsiString('Hello'));
  Stm := TPJIStreamWrapper.Create(S, True);
  // Check size starts as 5 (test stream length and data string length)
  Stm.Seek(0, STREAM_SEEK_END, P);
  CheckEquals(5, P, 'Test 1a');
  CheckEquals(5, Length(S.DataString), 'Test 1b');
  // Check size starts as 3 (test stream length and data string length)
  Stm.SetSize(3);
  Stm.Seek(0, STREAM_SEEK_END, P);
  CheckEquals(3, P, 'Test 2a');
  CheckEquals(3, Length(S.DataString), 'Test 2b');
  // Check size starts as 64 (test stream length and data string length)
  Stm.SetSize(64);
  Stm.Seek(0, STREAM_SEEK_END, P);
  CheckEquals(64, P, 'Test 3a');
  CheckEquals(64, Length(S.DataString), 'Test 3b');
end;

procedure TTestPJIStreamWrapper.TestStat;
var
  S: TStringStream;
  Stm: IStream;
  Stg: TStatStg;
begin
  S := TStringStream.Create(AnsiString('Hello World'));
  Stm := TPJIStreamWrapper.Create(S, True);
  // Check bad call with nil statstg param
  CheckEquals(STG_E_INVALIDPOINTER, Stm.Stat(TStatStg(nil^), STATFLAG_DEFAULT),
    'Test 1');
  // Check bad call with unrecognised grStatFlag param
  CheckEquals(STG_E_INVALIDFLAG, Stm.Stat(Stg, $FFFF), 'Test 2');
  // Check call with no name returned
  CheckEquals(S_OK, Stm.Stat(Stg, STATFLAG_NONAME), 'Test 3');
  Check(Stg.pwcsName = nil, 'Test 4');                       // no name returned
  CheckEquals(STGTY_STREAM, Stg.dwType, 'Test 5');             // type is stream
  CheckEquals(S.Size, Stg.cbSize, 'Test 6');        // size of underlying stream
  CheckEquals(True, FTIsZero(Stg.mtime), 'Test 7');      // no modification time
  CheckEquals(True, FTIsZero(Stg.ctime), 'Test 8');          // no creation time
  CheckEquals(True, FTIsZero(Stg.atime), 'Test 9');            // no access time
  CheckEquals(0, Stg.grfMode, 'Test 10');            // no access mode specified
  CheckEquals(0, Stg.grfLocksSupported, 'Test 11');      // locks not supported
  CheckEquals(True, GUIDIsZero(Stg.clsid), 'Test 12');          // clsid is zero
  CheckEquals(0, Stg.grfStateBits, 'Test 13');            // not used for stream
  CheckEquals(0, Stg.reserved, 'Test 14');                           // reserved
  // Check call with name returned
  CheckEquals(S_OK, Stm.Stat(Stg, STATFLAG_DEFAULT), 'Test 15');
  CheckEquals('TPJIStreamWrapper(TStringStream)', string(Stg.pwcsName),
    'Test 16');
  CoTaskMemFree(Stg.pwcsName);
  CheckEquals(STGTY_STREAM, Stg.dwType, 'Test 17');            // type is stream
  CheckEquals(S.Size, Stg.cbSize, 'Test 18');       // size of underlying stream
  CheckEquals(True, FTIsZero(Stg.mtime), 'Test 19');     // no modification time
  CheckEquals(True, FTIsZero(Stg.ctime), 'Test 20');         // no creation time
  CheckEquals(True, FTIsZero(Stg.atime), 'Test 21');           // no access time
  CheckEquals(0, Stg.grfMode, 'Test 22');            // no access mode specified
  CheckEquals(0, Stg.grfLocksSupported, 'Test 23');       // locks not supported
  CheckEquals(True, GUIDIsZero(Stg.clsid), 'Test 24');          // clsid is zero
  CheckEquals(0, Stg.grfStateBits, 'Test 25');            // not used for stream
  CheckEquals(0, Stg.reserved, 'Test 26');                           // reserved
end;

procedure TTestPJIStreamWrapper.TestWriteAndSeek;
var
  S: TStringStream;
  Stm: IStream;
  OutStr: AnsiString;
  W: LongInt;
begin
  S := TStringStream.Create(AnsiString(''));
  Stm := TPJIStreamWrapper.Create(S, True);
  // Write 'Hello' to empty string stream
  OutStr := 'Hello';
  Stm.Write(PAnsiChar(OutStr), 5, @W);
  CheckEquals(5, W, 'Test 1a');
  CheckEquals('Hello', S.DataString, 'Test 1b');
  // Write ' there' to current stream
  OutStr := ' there';
  Stm.Write(PAnsiChar(OutStr), 6, @W);
  CheckEquals(6, W, 'Test 2a');
  CheckEquals('Hello there', S.DataString, 'Test 2b');
  // Now overwrite 'there' with 'World'
  Stm.Seek(6, STREAM_SEEK_SET, Largeint(nil^));
  OutStr := 'World';
  Stm.Write(PAnsiChar(OutStr), 5, nil);
  CheckEquals('Hello World', S.DataString, 'Test 3');
end;

{ TTestPJHandleIStreamWrapper }

procedure TTestPJHandleIStreamWrapper.TestStat;
var
  S: THandleStream;
  Stm: IStream;
  Stg: TStatStg;
  H: Integer;
begin
  H := FileOpen(GetTestFileName, fmOpenRead);
  if H = -1 then
    raise Exception.Create('Can''t open test file: ' + GetTestFileName);
  try
    S := THandleStream.Create(H);
    Stm := TPJHandleIStreamWrapper.Create(S, True);
    // Check bad call with nil statstg param
    CheckEquals(STG_E_INVALIDPOINTER,
      Stm.Stat(TStatStg(nil^), STATFLAG_DEFAULT), 'Test 1');
    // Check bad call with unrecognised grStatFlag param
    CheckEquals(STG_E_INVALIDFLAG, Stm.Stat(Stg, $FFFF), 'Test 2');
    // Check call with no name returned
    CheckEquals(S_OK, Stm.Stat(Stg, STATFLAG_NONAME), 'Test 3');
    Check(Stg.pwcsName = nil, 'Test 4');                     // no name returned
    CheckEquals(STGTY_STREAM, Stg.dwType, 'Test 5');           // type is stream
    CheckEquals(S.Size, Stg.cbSize, 'Test 6');      // size of underlying stream
    CheckEquals(False, FTIsZero(Stg.mtime), 'Test 7');      // modification time
    CheckEquals(False, FTIsZero(Stg.ctime), 'Test 8');          // creation time
    CheckEquals(False, FTIsZero(Stg.atime), 'Test 9');   // non-zero access time
    CheckEquals(0, Stg.grfMode, 'Test 10');          // no access mode specified
    CheckEquals(0, Stg.grfLocksSupported, 'Test 11');     // locks not supported
    CheckEquals(True, GUIDIsZero(Stg.clsid), 'Test 12');        // clsid is zero
    CheckEquals(0, Stg.grfStateBits, 'Test 13');          // not used for stream
    CheckEquals(0, Stg.reserved, 'Test 14');                         // reserved

    // Check call with name returned
    CheckEquals(S_OK, Stm.Stat(Stg, STATFLAG_DEFAULT), 'Test 15');
    CheckEquals('TPJHandleIStreamWrapper(THandleStream)', string(Stg.pwcsName),
      'Test 16');
    CoTaskMemFree(Stg.pwcsName);
    CheckEquals(STGTY_STREAM, Stg.dwType, 'Test 17');          // type is stream
    CheckEquals(S.Size, Stg.cbSize, 'Test 18');     // size of underlying stream
    CheckEquals(False, FTIsZero(Stg.mtime), 'Test 19');     // modification time
    CheckEquals(False, FTIsZero(Stg.ctime), 'Test 20');         // creation time
    CheckEquals(False, FTIsZero(Stg.atime), 'Test 21');  // non-zero access time
    CheckEquals(0, Stg.grfMode, 'Test 22');          // no access mode specified
    CheckEquals(0, Stg.grfLocksSupported, 'Test 23');     // locks not supported
    CheckEquals(True, GUIDIsZero(Stg.clsid), 'Test 24');        // clsid is zero
    CheckEquals(0, Stg.grfStateBits, 'Test 25');          // not used for stream
    CheckEquals(0, Stg.reserved, 'Test 26');                         // reserved
  finally
    FileClose(H);
  end;
end;

{ TTestPJFileIStream }

procedure TTestPJFileIStream.TestStat;
var
  FS: TFileStream;
  Stm: IStream;
  Stg: TStatStg;
begin
  // Create file IStream
  Stm := TPJFileIStream.Create(GetTestFileName, fmOpenRead + fmShareDenyNone);
  // Create normal file stream just for size for comparison
  FS := TFileStream.Create(GetTestFileName, fmOpenRead + fmShareDenyNone);
  try
    // Check bad call with nil statstg param
    CheckEquals(STG_E_INVALIDPOINTER,
      Stm.Stat(TStatStg(nil^), STATFLAG_DEFAULT), 'Test 1');
    // CheckEquals bad call with unrecognised grStatFlag param
    CheckEquals(STG_E_INVALIDFLAG, Stm.Stat(Stg, $FFFF), 'Test 2');
    // CheckEquals call with no name returned
    CheckEquals(S_OK, Stm.Stat(Stg, STATFLAG_NONAME), 'Test 3');
    Check(Stg.pwcsName = nil, 'Test 4');                     // no name returned
    CheckEquals(STGTY_STREAM, Stg.dwType, 'Test 5');           // type is stream
    CheckEquals(FS.Size, Stg.cbSize, 'Test 6');                  // size of file
    CheckEquals(False, FTIsZero(Stg.mtime), 'Test 7');      // modification time
    CheckEquals(False, FTIsZero(Stg.ctime), 'Test 8');          // creation time
    CheckEquals(False, FTIsZero(Stg.atime), 'Test 9');            // access time
    CheckEquals(0, Stg.grfMode, 'Test 10');          // no access mode specified
    CheckEquals(0, Stg.grfLocksSupported, 'Test 11');     // locks not supported
    CheckEquals(True, GUIDIsZero(Stg.clsid), 'Test 12');        // clsid is zero
    CheckEquals(0, Stg.grfStateBits, 'Test 13');          // not used for stream
    CheckEquals(0, Stg.reserved, 'Test 14');                         // reserved

    // CheckEquals call with name returned
    CheckEquals(S_OK, Stm.Stat(Stg, STATFLAG_DEFAULT), 'Test 15');
    CheckEquals(GetTestFileName, string(Stg.pwcsName), 'Test 16');
    CoTaskMemFree(Stg.pwcsName);
    CheckEquals(STGTY_STREAM, Stg.dwType, 'Test 17');          // type is stream
    CheckEquals(FS.Size, Stg.cbSize, 'Test 18');                 // size of file
    CheckEquals(False, FTIsZero(Stg.mtime), 'Test 19');     // modification time
    CheckEquals(False, FTIsZero(Stg.ctime), 'Test 20');         // creation time
    CheckEquals(False, FTIsZero(Stg.atime), 'Test 21');           // access time
    CheckEquals(0, Stg.grfMode, 'Test 22');          // no access mode specified
    CheckEquals(0, Stg.grfLocksSupported, 'Test 23');     // locks not supported
    CheckEquals(True, GUIDIsZero(Stg.clsid), 'Test 24');        // clsid is zero
    CheckEquals(0, Stg.grfStateBits, 'Test 25');          // not used for stream
    CheckEquals(0, Stg.reserved, 'Test 26');                         // reserved
  finally
    FS.Free;
  end;
end;

initialization

ObList := TList.Create;

TestFramework.RegisterTests(
  [TTestPJIStreamWrapper.Suite, TTestPJHandleIStreamWrapper.Suite,
  TTestPJFileIStream.Suite]
);

finalization

ClearObList;
ObList.Free;

end.

