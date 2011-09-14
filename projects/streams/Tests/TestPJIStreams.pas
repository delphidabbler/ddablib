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
  S1 := TStringStream.Create('Hello World');
  S2 := TStringStream.Create('delphiDabbler!');
  Stm1 := TPJIStreamWrapper.Create(S1, True);
  Stm2 := TPJIStreamWrapper.Create(S2, True);
  Stm1.Seek(6, STREAM_SEEK_SET, Largeint(nil^));  // start of "World"
  Stm2.Seek(6, STREAM_SEEK_SET, Largeint(nil^));  // start of "Dabbler"
  // do the copy
  Stm2.CopyTo(Stm1, 7, R, W);
  Check(R = 7);   // bytes read
  Check(W = 7);   // bytes written
  Check(S1.DataString = 'Hello Dabbler');
  // Now we try to copy all of S2 + a few extra bytes to same place in S1
  Stm2.Seek(0, STREAM_SEEK_SET, Largeint(nil^));  // wind to start of S2
  Stm1.Seek(6, STREAM_SEEK_SET, Largeint(nil^));  // start of "Dabbler"
  Stm2.CopyTo(Stm1, 64, R, W);
  Check(R = Length('delphiDabbler!'));
  Check(W = R);
  Check(S1.DataString = 'Hello delphiDabbler!');
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
  Check(ObList.Count = 1);  // TTestStream object still exists
  // Check freeing wrapper stream does free owned stream when required
  Stm := TPJIStreamWrapper.Create(S, True);
  Stm := nil;
  Check(ObList.Count = 0);  // TTestStream object has been freed
end;

procedure TTestPJIStreamWrapper.TestNonImpl;
var
  S: TStringStream;
  Stm: IStream;
begin
  S := TStringStream.Create('Hello World');
  Stm := TPJIStreamWrapper.Create(S, True);
  Check(Stm.Commit(0) = S_OK);
  Check(Stm.Revert = STG_E_REVERTED);
  Check(Stm.LockRegion(0, 0, 0) = STG_E_INVALIDFUNCTION);
  Check(Stm.UnlockRegion(0, 0, 0) = STG_E_INVALIDFUNCTION);
end;

procedure TTestPJIStreamWrapper.TestReadAndSeek;
var
  S: TStringStream;
  Stm: IStream;
  InStr: string;
  R: LongInt;
  P: Largeint;
begin
  S := TStringStream.Create('Hello World');
  Stm := TPJIStreamWrapper.Create(S, True);
  // Read 1st five chars('Hello');
  SetLength(InStr, 5);
  Stm.Read(PChar(InStr), 5, @R);
  Check((InStr = 'Hello') and (R = 5));
  // Move to one char from end (pos should be 10)
  Stm.Seek(1, STREAM_SEEK_END, P);
  Check(P = 10);
  // Read one char: should be last one = 'd'
  SetLength(InStr, 1);
  Stm.Read(PChar(InStr), 1, nil);
  Check(InStr = 'd');
  // We should now be at end (P = 11)
  Stm.Seek(0, STREAM_SEEK_CUR, P);
  Check(P = 11);
  // Seek 6 chars in from start
  Stm.Seek(6, STREAM_SEEK_SET, P);
  Check (P = 6);
  // Try to read 64 chars from here: should read just last 5 = 'World'
  SetLength(InStr, 64);
  Stm.Read(PChar(InStr), 64, @R);
  Check(R = 5);
  SetLength(InStr, 5);
  Check(InStr = 'World');
end;

procedure TTestPJIStreamWrapper.TestSize;
var
  S: TStringStream;
  Stm: IStream;
  P: Largeint;
begin
  S := TStringStream.Create('Hello');
  Stm := TPJIStreamWrapper.Create(S, True);
  // Check size starts as 5 (test stream length and data string length)
  Stm.Seek(0, STREAM_SEEK_END, P);
  Check(P = 5);
  Check(Length(S.DataString) = 5);
  // Check size starts as 3 (test stream length and data string length)
  Stm.SetSize(3);
  Stm.Seek(0, STREAM_SEEK_END, P);
  Check(P = 3);
  Check(Length(S.DataString) = 3);
  // Check size starts as 64 (test stream length and data string length)
  Stm.SetSize(64);
  Stm.Seek(0, STREAM_SEEK_END, P);
  Check(P = 64);
  Check(Length(S.DataString) = 64);
end;

procedure TTestPJIStreamWrapper.TestStat;
var
  S: TStringStream;
  Stm: IStream;
  Stg: TStatStg;
begin
  S := TStringStream.Create('Hello World');
  Stm := TPJIStreamWrapper.Create(S, True);
  // Check bad call with nil statstg param
  Check(Stm.Stat(TStatStg(nil^), STATFLAG_DEFAULT) = STG_E_INVALIDPOINTER);
  // Check bad call with unrecognised grStatFlag param
  Check(Stm.Stat(Stg, $FFFF) = STG_E_INVALIDFLAG);
  // Check call with no name returned
  Check(Stm.Stat(Stg, STATFLAG_NONAME) = S_OK);
  Check(Stg.pwcsName = nil);        // no name returned
  Check(Stg.dwType = STGTY_STREAM); // type is stream
  Check(Stg.cbSize = S.Size);       // size of underlying stream
  Check(FTIsZero(Stg.mtime));       // no modification time
  Check(FTIsZero(Stg.ctime));       // no creation time
  Check(FTIsZero(Stg.atime));       // no access time
  Check(Stg.grfMode = 0);           // no access mode specified
  Check(Stg.grfLocksSupported = 0); // locks not supported
  Check(GUIDIsZero(Stg.clsid));     // clsid is zero: not used for stream
  Check(Stg.grfStateBits = 0);      // not used for stream
  Check(Stg.reserved = 0);          // reserved
  // Check call with name returned
  Check(Stm.Stat(Stg, STATFLAG_DEFAULT) = S_OK);
  Check(string(Stg.pwcsName) = 'TPJIStreamWrapper(TStringStream)');
                                    // required name
  CoTaskMemFree(Stg.pwcsName);      //    free the name memory
  Check(Stg.dwType = STGTY_STREAM); // type is stream
  Check(Stg.cbSize = S.Size);       // size of underlying stream
  Check(FTIsZero(Stg.mtime));       // no modification time
  Check(FTIsZero(Stg.ctime));       // no creation time
  Check(FTIsZero(Stg.atime));       // no access time
  Check(Stg.grfMode = 0);           // no access mode specified
  Check(Stg.grfLocksSupported = 0); // locks not supported
  Check(GUIDIsZero(Stg.clsid));     // clsid is zero: not used for stream
  Check(Stg.grfStateBits = 0);      // not used for stream
  Check(Stg.reserved = 0);          // reserved
end;

procedure TTestPJIStreamWrapper.TestWriteAndSeek;
var
  S: TStringStream;
  Stm: IStream;
  OutStr: string;
  W: LongInt;
begin
  S := TStringStream.Create('');
  Stm := TPJIStreamWrapper.Create(S, True);
  // Write 'Hello' to empty string stream
  OutStr := 'Hello';
  Stm.Write(PChar(OutStr), 5, @W);
  Check(W = 5);
  Check(S.DataString = 'Hello');
  // Write ' there' to current stream
  OutStr := ' there';
  Stm.Write(PChar(OutStr), 6, @W);
  Check(W = 6);
  Check(S.DataString = 'Hello there');
  // Now overwrite 'there' with 'World'
  Stm.Seek(6, STREAM_SEEK_SET, Largeint(nil^));
  OutStr := 'World';
  Stm.Write(PChar(OutStr), 5, nil);
  Check(S.DataString = 'Hello World');
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
    Check(Stm.Stat(TStatStg(nil^), STATFLAG_DEFAULT) = STG_E_INVALIDPOINTER);
    // Check bad call with unrecognised grStatFlag param
    Check(Stm.Stat(Stg, $FFFF) = STG_E_INVALIDFLAG);
    // Check call with no name returned
    Check(Stm.Stat(Stg, STATFLAG_NONAME) = S_OK);
    Check(Stg.pwcsName = nil);        // no name returned
    Check(Stg.dwType = STGTY_STREAM); // type is stream
    Check(Stg.cbSize = S.Size);       // size of underlying stream
    Check(not FTIsZero(Stg.mtime));   // non-zero modification time
    Check(not FTIsZero(Stg.ctime));   // non-zero creation time
    Check(not FTIsZero(Stg.atime));   // non-zero access time
    Check(Stg.grfMode = 0);           // no access mode specified
    Check(Stg.grfLocksSupported = 0); // locks not supported
    Check(GUIDIsZero(Stg.clsid));     // clsid is zero: not used for stream
    Check(Stg.grfStateBits = 0);      // not used for stream
    Check(Stg.reserved = 0);          // reserved

    // Check call with name returned
    Check(Stm.Stat(Stg, STATFLAG_DEFAULT) = S_OK);
    Check(string(Stg.pwcsName) = 'TPJHandleIStreamWrapper(THandleStream)');
                                      // required name
    CoTaskMemFree(Stg.pwcsName);      //    free the name memory
    Check(Stg.dwType = STGTY_STREAM); // type is stream
    Check(Stg.cbSize = S.Size);       // size of underlying stream
    Check(not FTIsZero(Stg.mtime));   // non-zero modification time
    Check(not FTIsZero(Stg.ctime));   // non-zero creation time
    Check(not FTIsZero(Stg.atime));   // non-zero access time
    Check(Stg.grfMode = 0);           // no access mode specified
    Check(Stg.grfLocksSupported = 0); // locks not supported
    Check(GUIDIsZero(Stg.clsid));     // clsid is zero: not used for stream
    Check(Stg.grfStateBits = 0);      // not used for stream
    Check(Stg.reserved = 0);          // reserved
  finally
    FileClose(H);
  end;
end;

{ TTestPJFileIStream }

procedure TTestPJFileIStream.TestStat;
var
//  S: THandleStream;
  FS: TFileStream;
  Stm: IStream;
  Stg: TStatStg;
//  H: Integer;
begin
  // Create file IStream
  Stm := TPJFileIStream.Create(GetTestFileName, fmOpenRead + fmShareDenyNone);
  // Create normal file stream just for size for comparison
  FS := TFileStream.Create(GetTestFileName, fmOpenRead + fmShareDenyNone);
  try
    // Check bad call with nil statstg param
    Check(Stm.Stat(TStatStg(nil^), STATFLAG_DEFAULT) = STG_E_INVALIDPOINTER);
    // Check bad call with unrecognised grStatFlag param
    Check(Stm.Stat(Stg, $FFFF) = STG_E_INVALIDFLAG);
    // Check call with no name returned
    Check(Stm.Stat(Stg, STATFLAG_NONAME) = S_OK);
    Check(Stg.pwcsName = nil);        // no name returned
    Check(Stg.dwType = STGTY_STREAM); // type is stream
    Check(Stg.cbSize = FS.Size);      // size of file
    Check(not FTIsZero(Stg.mtime));   // non-zero modification time
    Check(not FTIsZero(Stg.ctime));   // non-zero creation time
    Check(not FTIsZero(Stg.atime));   // non-zero access time
    Check(Stg.grfMode = 0);           // no access mode specified
    Check(Stg.grfLocksSupported = 0); // locks not supported
    Check(GUIDIsZero(Stg.clsid));     // clsid is zero: not used for stream
    Check(Stg.grfStateBits = 0);      // not used for stream
    Check(Stg.reserved = 0);          // reserved

    // Check call with name returned
    Check(Stm.Stat(Stg, STATFLAG_DEFAULT) = S_OK);
    Check(string(Stg.pwcsName) = GetTestFileName);
                                      // required name
    CoTaskMemFree(Stg.pwcsName);      //    free the name memory
    Check(Stg.dwType = STGTY_STREAM); // type is stream
    Check(Stg.cbSize = FS.Size);      // size of file
    Check(not FTIsZero(Stg.mtime));   // non-zero modification time
    Check(not FTIsZero(Stg.ctime));   // non-zero creation time
    Check(not FTIsZero(Stg.atime));   // non-zero access time
    Check(Stg.grfMode = 0);           // no access mode specified
    Check(Stg.grfLocksSupported = 0); // locks not supported
    Check(GUIDIsZero(Stg.clsid));     // clsid is zero: not used for stream
    Check(Stg.grfStateBits = 0);      // not used for stream
    Check(Stg.reserved = 0);          // reserved
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
