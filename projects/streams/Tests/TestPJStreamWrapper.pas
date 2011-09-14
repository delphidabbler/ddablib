unit TestPJStreamWrapper;

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
  published
    procedure TestCreateDestroy;
      {Test that wrapped owned streams are freed on destruction and non-owned
      streams are not}
    procedure TestReadAndSeek;
      {Test Read and Seek methods}
    procedure TestWriteAndSeek;
      {Test Write and Seek methods}
    procedure TestSize;
      {Test Size property}
  end;

implementation

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

procedure TTestPJStreamWrapper.TestCreateDestroy;
var
  S: TTestStream;
  W: TPJStreamWrapper;
begin
  ClearObList;
  S := TTestStream.Create('test');
  // Check freeing wrapper stream doesn't free wrapper stream when not owned
  W := TPJStreamWrapper.Create(S, False);
  W.Free;
  CheckEquals(1, ObList.Count, 'Test 1');
  // Check freeing wrapper stream does free an owned wrapped stream
  W := TPJStreamWrapper.Create(S, True);
  W.Free;
  CheckEquals(0, ObList.Count, 'Test 2');
end;

procedure TTestPJStreamWrapper.TestReadAndSeek;
var
  S: TStringStream;
  W: TPJStreamWrapper;
  InStr: string;
begin
  ClearObList;
  S := TTestStream.Create('Hello World');
  W := TPJStreamWrapper.Create(S, True);
  try
    // Read 1st five chars('Hello');
    SetLength(InStr, 5);
    CheckEquals(5, W.Read(PChar(InStr)^, 5), 'Test 1');
    CheckEquals('Hello', InStr, 'Test 2');
    // Move to one char from end (pos should be 10)
    CheckEquals(10, W.Seek(1, soFromEnd), 'Test 3');
    // Read one char: should be last one = 'd'
    SetLength(InStr, 1);
    W.Read(PChar(InStr)^, 1);
    CheckEquals('d', InStr, 'Test 4');
    // We should now be at end (P = 11)
    CheckEquals(11, W.Position, 'Test 5');
    // Seek 6 chars in from start
    CheckEquals(6, W.Seek(6, soFromBeginning), 'Test 6');
    // Try to read 64 chars from here: should read just last 5 = 'World'
    SetLength(InStr, 64);
    CheckEquals(5, W.Read(PChar(InStr)^, 64), 'Test 7');
    SetLength(InStr, 5);
    status(instr);
    CheckEquals('World', InStr, 'Test 8');
  finally
    W.Free;
  end;
end;

procedure TTestPJStreamWrapper.TestSize;
var
  S: TStringStream;
  W: TPJStreamWrapper;
begin
  S := TStringStream.Create('Hello');
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

procedure TTestPJStreamWrapper.TestWriteAndSeek;
var
  S: TStringStream;
  W: TPJStreamWrapper;
  OutStr: string;
begin
  S := TStringStream.Create('');
  W := TPJStreamWrapper.Create(S, True);
  try
    // Write 'Hello' to empty string stream
    OutStr := 'Hello';
    CheckEquals(5, W.Write(PChar(OutStr)^, 5), 'Test 1a');
    CheckEquals('Hello', S.DataString, 'Test 1b');
    // Write ' there' to current stream
    OutStr := ' there';
    CheckEquals(6, W.Write(PChar(OutStr)^, 6), 'Test 2a');
    CheckEquals('Hello there', S.DataString, 'Test 2b');
    // Now overwrite 'there' with 'World': use Position instead of Seek
    W.Position := 6;
    OutStr := 'World';
    W.Write(PChar(OutStr)^, 5);
    CheckEquals(S.Position, W.Position, 'Test 3a');
    CheckEquals('Hello World', S.DataString, 'Test 3b');
  finally
    W.Free;
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

