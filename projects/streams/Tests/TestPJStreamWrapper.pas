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
  Check(ObList.Count = 1);  // TTestStream object still exists
  // Check freeing wrapper stream does free an owned wrapped stream
  W := TPJStreamWrapper.Create(S, True);
  W.Free;
  Check(ObList.Count = 0);  // TTestStream object has now been freed
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
    Check(W.Read(PChar(InStr)^, 5) = 5);
    Check(InStr = 'Hello');
    // Move to one char from end (pos should be 10)
    Check(W.Seek(1, soFromEnd) = 10);
    // Read one char: should be last one = 'd'
    SetLength(InStr, 1);
    W.Read(PChar(InStr)^, 1);
    Check(InStr = 'd');
    // We should now be at end (P = 11)
    Check(W.Position = 11);
    // Seek 6 chars in from start
    Check(W.Seek(6, soFromBeginning) = 6);
    // Try to read 64 chars from here: should read just last 5 = 'World'
    SetLength(InStr, 64);
    Check(W.Read(PChar(InStr)^, 64) = 5);
    SetLength(InStr, 5);
    status(instr);
    Check(InStr = 'World');
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
    Check(W.Seek(0, soFromEnd) = 5);
    Check(Length(S.DataString) = 5);
    // Check size starts as 3 (test stream length and data string length)
    W.Size := 3;
    Check(W.Size = 3);
    Check(Length(S.DataString) = 3);
    // Check size starts as 64 (test stream length and data string length)
    W.Size := 64;
    Check(W.Size = 64);
    Check(Length(S.DataString) = 64);
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
    Check(W.Write(PChar(OutStr)^, 5) = 5);
    Check(S.DataString = 'Hello');
    // Write ' there' to current stream
    OutStr := ' there';
    Check(W.Write(PChar(OutStr)^, 6) = 6);
    Check(S.DataString = 'Hello there');
    // Now overwrite 'there' with 'World': use Position instead of Seek
    W.Position := 6;
    OutStr := 'World';
    W.Write(PChar(OutStr)^, 5);
    Check(W.Position = S.Position);
    Check(S.DataString = 'Hello World');
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
