{
 * Delphi DUnit Test Case for PJMD5.pas
 * ------------------------------------
 * This unit contains a test case class for the TPJMD5 class in PJMD5.pas. The
 * MD5 algorithm is tested against all seven test cases from RFC 1321 plus some
 * others.
 *
 * The expected results were obtained from either the RFC or by running a third
 * party MD5 console app on the required input data.
 *
 * Delphi 2009 or later is required to compile: requires Unicode support and
 * anonymous methods.
 *
 * -----------------------------------------------------------------------------
 * $Rev$
 * $Date$
 * -----------------------------------------------------------------------------
}

unit TestPJMD5;

interface

// Delphi 2009 or later is required to compile: requires Unicode support and
// anonymous methods
{$UNDEF CANCOMPILE}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 20.0}
    {$DEFINE CANCOMPILE}
  {$IFEND}
{$ENDIF}
{$IFNDEF CANCOMPILE}
  {$MESSAGE FATAL 'Delphi 2009 or later required'}
{$ENDIF}

{$WARN UNSAFE_CODE OFF}

uses
  TestFramework, Classes, SysUtils, PJMD5;

type
  // Record providing info about an RFC test
  TRFCTest = record
    ID: string;
    Data: AnsiString;
    ResultStr: string;
    ResultBin: array[$0..$F] of Byte;
    function SizeOfData: Cardinal;
    function DataAsByteArray: TBytes;
    function PointerToData: Pointer;
    function IsEqualResult(const S: string): Boolean; overload;
    function IsEqualResult(const Bytes: array of Byte): Boolean; overload;
    function IsEqualResult(const D: TPJMD5Digest): Boolean; overload;
    procedure CopyDataToStream(const Stm: TStream);
  end;

  TProcessMethodCall = reference to procedure(const MD5: TPJMD5;
    const RFCTest: TRFCTest);

  // Tests TPJMD5Digest record
  TestTPJMD5Digest = class(TTestCase)
  private
    function ByteArrayToBytes(const A: array of Byte): TBytes;
  published
    procedure TestVariantRecordParts;
    procedure TestDefaultProperty;
    procedure TestImplicitCasts;
    procedure TestEquality;
    procedure TestInEquality;
  end;

  // Tests TPJMD5 object
  TestTPJMD5 = class(TTestCase)
  strict private
    // Provides path file used in TestProcessFile
    function TestFilePath: string;
    // Runs one of the RFC tests on a specified Process... method
    procedure RunRFCTests(MethodCall: TProcessMethodCall);
  public
    // Sets up test: creates file used by TestProcessFile
    procedure SetUp; override;
    // Tears down test.
    procedure TearDown; override;
    // Runs Finalize test that raises exception
    procedure RunFinalizeTest;
  published
    // ** NOTE: Order of these tests is important **

    // Tests Process(array of Byte, Cardinal) method and Digest and DigestString
    // properties.
    // Process(array of Byte, Cardinal) is called internally by all other
    // Process... methods, so is tested first. The method is also called by some
    // test methods.
    // This test runs all RFC tests
    procedure TestProcessArrayOfByteSize;

    // Tests Finalize method and Finalized property
    // This test calls Process(array of Byte, Cardinal) so should be run after
    // TestProcessArrayOfByteSize
    procedure TestFinalize;

    // Tests Reset method.
    // This test calls Process(array of Byte, Cardinal) so should be run after
    // TestProcessArrayOfByteSize
    procedure TestReset;

    // Tests Process(array of Byte) method.
    // This test runs all RFC tests
    procedure TestProcessArrayOfByte;

    // Tests Process(Untyped, Cardinal) method
    // This test runs all RFC tests
    procedure TestProcessBuf;

    // Tests Process(AnsiString) method
    // This test runs all RFC tests
    procedure TestProcessAnsiString;

    // Tests Process(UnicodeString) method
    // RFC Tests cannot be used with Process(UnicodeString). A special unicode
    // test string is used.
    procedure TestProcessUnicodeString;

    // Tests Process(UnicodeString, TEncoding) method
    // RFC Tests cannot be used with Process(UnicodeString, TEncoding). A
    // special unicode test string is used.
    procedure TestProcessUnicodeStringWithEncoding;

    // Tests Process(TStream) method
    // This test runs all RFC tests
    procedure TestProcessStream;

    // Tests ProcessFile method
    // RFC Tests are not used in this test. Test file has same contents as the
    // unicode test string
    procedure TestProcessFile;

  end;

implementation

const
  // RFC 1321 Test cases
  // See http://www.faqs.org/rfcs/rfc1321.html
  RFCTests: array[1..7] of TRFCTest = (
    (
      ID: '1';
      Data: '';
      ResultStr: 'd41d8cd98f00b204e9800998ecf8427e';
      ResultBin: (
        $D4, $1D, $8C, $D9, $8F, $00, $B2, $04,
        $E9, $80, $09, $98, $EC, $F8, $42, $7E
      );
    ),
    (
      ID: '2';
      Data: 'a';
      ResultStr: '0cc175b9c0f1b6a831c399e269772661';
      ResultBin: (
        $0C, $C1, $75, $B9, $C0, $F1, $B6, $A8,
        $31, $C3, $99, $E2, $69, $77, $26, $61
      );
    ),
    (
      ID: '3';
      Data: 'abc';
      ResultStr: '900150983cd24fb0d6963f7d28e17f72';
      ResultBin: (
        $90, $01, $50, $98, $3C, $D2, $4F, $B0,
        $D6, $96, $3F, $7D, $28, $E1, $7F, $72
      );
    ),
    (
      ID: '4';
      Data: 'message digest';
      ResultStr: 'f96b697d7cb7938d525a2f31aaf161d0';
      ResultBin: (
        $F9, $6B, $69, $7D, $7C, $B7, $93, $8D,
        $52, $5A, $2F, $31, $AA, $F1, $61, $D0
      );
    ),
    (
      ID: '5';
      Data: 'abcdefghijklmnopqrstuvwxyz';
      ResultStr: 'c3fcd3d76192e4007dfb496cca67e13b';
      ResultBin: (
        $C3, $FC, $D3, $D7, $61, $92, $E4, $00,
        $7D, $FB, $49, $6C, $CA, $67, $E1, $3B
      );
    ),
    (
      ID: '6';
      Data: 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
      ResultStr: 'd174ab98d277d9f5a5611c2c9f419d9f';
      ResultBin: (
        $D1, $74, $AB, $98, $D2, $77, $D9, $F5,
        $A5, $61, $1C, $2C, $9F, $41, $9D, $9F
      );
    ),
    (
      ID: '7';
      Data: '12345678901234567890123456789012345678901234567890123456789012345'
        + '678901234567890';
      ResultStr: '57edf4a22be3c955ac49da2e2107b67a';
      ResultBin: (
        $57, $ED, $F4, $A2, $2B, $E3, $C9, $55,
        $AC, $49, $DA, $2E, $21, $07, $B6, $7A
      );
    )
  );

  // Tests for unicode strings: can't use RFC tests
  // test string for unicode encoding
  UnicodeTestString = 'QWERTY - '#$0444#$1D42' - qwerty - '#$2030' - end.';
  UnicodeTestStringRes = 'a65a476239c6806880acde8ba93fa4a8';
  // test string for ASCII encoding
  ASCIITestString = 'QWERTYUIOPASDFGHJKLZXCVBNM0123456789';
  ASCIITestStringRes = '79d291b4afd040c2a18f69e9c832fa1b';

  // Tests for files
  // test file containing same content as UnicodeTestString const
  TestFileName = 'Test.tmp';
  TestFileRes = UnicodeTestStringRes;

  // Digest when no data processed = RFCTests[0].ResultStr
  NoProcessingRes = 'd41d8cd98f00b204e9800998ecf8427e';

  // Default test
  DefaultRFCTest = 6;

  // Tests for TPJMD5Digest
  MD5Str1 = 'd174ab98d277d9f5a5611c2c9f419d9f';
  MD5Bytes1: array[0..15] of Byte = (
    $D1, $74, $AB, $98, $D2, $77, $D9, $F5,
    $A5, $61, $1C, $2C, $9F, $41, $9D, $9F
  );
  Digest1: TPJMD5Digest = (
    Bytes: (
      $D1, $74, $AB, $98, $D2, $77, $D9, $F5,
      $A5, $61, $1C, $2C, $9F, $41, $9D, $9F
    )
  );
  MD5Str2 = 'f96b697d7cb7938d525a2f31aaf161d0';
  MD5Bytes2: array[0..15] of Byte = (
    $F9, $6B, $69, $7D, $7C, $B7, $93, $8D,
    $52, $5A, $2F, $31, $AA, $F1, $61, $D0
  );
  Digest2: TPJMD5Digest = (
    Bytes: (
      $F9, $6B, $69, $7D, $7C, $B7, $93, $8D,
      $52, $5A, $2F, $31, $AA, $F1, $61, $D0
    )
  );
  MD5Str3 = 'd174ab98d277d9f5a5611c2c9f419d9f';
  MD5Bytes3: array[0..15] of Byte = (
    $D1, $74, $AB, $98, $D2, $77, $D9, $F5,
    $A5, $61, $1C, $2C, $9F, $41, $9D, $9F
  );
  Digest3: TPJMD5Digest = (
    Bytes: (
      $D1, $74, $AB, $98, $D2, $77, $D9, $F5,
      $A5, $61, $1C, $2C, $9F, $41, $9D, $9F
    )
  );

procedure TestTPJMD5.RunFinalizeTest;
var
  MD5: TPJMD5;
begin
  MD5 := TPJMD5.Create;
  try
    Check(not MD5.Finalized, TPJMD5.ClassName + '.Finalized should be False');
    MD5.Process(
      RFCTests[DefaultRFCTest].ResultBin, RFCTests[DefaultRFCTest].SizeOfData
    );
    Check(not MD5.Finalized, TPJMD5.ClassName + '.Finalized should be False');
    MD5.Finalize;
    Check(MD5.Finalized, TPJMD5.ClassName + '.Finalized should be False');
    // following line should raise exception: can't update digest when finalized
    MD5.Process(
      RFCTests[DefaultRFCTest].ResultBin, RFCTests[DefaultRFCTest].SizeOfData
    );
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.RunRFCTests(MethodCall: TProcessMethodCall);
var
  Test: TRFCTest;
  MD5: TPJMD5;
begin
  for Test in RFCTests do
  begin
    MD5 := TPJMD5.Create;
    try
      MethodCall(MD5, Test); // calls a ProcessXXX method
      Check(
        Test.IsEqualResult(MD5.Digest),
        'RFC Test #' + Test.ID + ' Failed - Digest property wrong value'
      );
      Check(
        Test.IsEqualResult(MD5.DigestAsString),
        'RFC Test #' + Test.ID + ' Failed - DigestAsString property wrong value'
      );
    finally
      MD5.Free;
    end;
  end;
end;

procedure TestTPJMD5.SetUp;
var
  FS: TFileStream;
  B: TBytes;
begin
  // Create test file
  FS := TFileStream.Create(TestFilePath + TestFileName, fmCreate);
  try
    B := TEncoding.Unicode.GetBytes(UnicodeTestString);
    FS.WriteBuffer(Pointer(B)^, Length(B));
  finally
    FS.Free;
  end;
end;

procedure TestTPJMD5.TearDown;
begin
end;

function TestTPJMD5.TestFilePath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure TestTPJMD5.TestFinalize;
begin
  CheckException(RunFinalizeTest, EPJMD5, EPJMD5.ClassName + ' Exception expected');
end;

procedure TestTPJMD5.TestProcessAnsiString;
var
  Fn: TProcessMethodCall;
begin
  Fn := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    MD5.Process(Test.Data);
  end;
  RunRFCTests(Fn);
end;

procedure TestTPJMD5.TestProcessArrayOfByte;
var
  Fn: TProcessMethodCall;
begin
  Fn := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    MD5.Process(Test.DataAsByteArray);
  end;
  RunRFCTests(Fn);
end;

procedure TestTPJMD5.TestProcessArrayOfByteSize;
var
  Fn: TProcessMethodCall;
begin
  Fn := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    MD5.Process(Test.DataAsByteArray, Test.SizeOfData);
  end;
  RunRFCTests(Fn);
end;

procedure TestTPJMD5.TestProcessBuf;
var
  Fn: TProcessMethodCall;
begin
  Fn := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    MD5.Process(Test.PointerToData^, Test.SizeOfData);
  end;
  RunRFCTests(Fn);
end;

procedure TestTPJMD5.TestProcessFile;
var
  MD5: TPJMD5;
begin
  MD5 := TPJMD5.Create;
  try
    MD5.ReadBufferSize := 16;
    MD5.ProcessFile(TestFilePath + TestFileName);
    Check(SameStr(MD5.DigestAsString, TestFileRes), 'File ' + TestFileName);
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.TestProcessStream;
var
  Fn: TProcessMethodCall;
  Stream: TStream;
begin
  Fn := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    Stream.Size := 0;
    Test.CopyDataToStream(Stream);
    MD5.ReadBufferSize := 48;
    MD5.Process(Stream);
  end;
  Stream := TMemoryStream.Create;
  try
    RunRFCTests(Fn);
  finally
    Stream.Free;
  end;
end;

procedure TestTPJMD5.TestProcessUnicodeString;
var
  MD5: TPJMD5;
begin
  // tests using default encoding: assumes default encoding writes same bytes
  // as ansi string
  MD5 := TPJMD5.Create;
  try
    MD5.Process(ASCIITestString);
    Check(
      SameStr(MD5.DigestAsString, ASCIITestStringRes),
      'Unicode ASCII string test (default encoding)'
    );
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.TestProcessUnicodeStringWithEncoding;
var
  MD5: TPJMD5;
begin
  // tests using default encoding: assumes default encoding writes same bytes
  // as ansi string
  MD5 := TPJMD5.Create;
  try
    MD5.Process(ASCIITestString, TEncoding.ASCII);
    Check(
      SameStr(MD5.DigestAsString, ASCIITestStringRes),
      'Unicode ASCII string test (ASCII encoding)'
    );
    MD5.Reset;
    MD5.Process(UnicodeTestString, TEncoding.Unicode);
    Check(
      SameStr(MD5.DigestAsString, UnicodeTestStringRes),
      'Unicode string test (Unicode encoding)'
    );
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.TestReset;
var
  MD5: TPJMD5;
begin
  MD5 := TPJMD5.Create;
  try
    MD5.Process(
      RFCTests[DefaultRFCTest].DataAsByteArray,
      RFCTests[DefaultRFCTest].SizeOfData
    );
    Check(
      not SameStr(MD5.DigestAsString, NoProcessingRes), 'Unexpected digest'
    );
    Check(MD5.Finalized, TPJMD5.ClassName + '.Finalized should be True');
    MD5.Reset;
    Check(
      not MD5.Finalized, TPJMD5.ClassName + '.Finalized should be False'
    );
    Check(
      SameStr(MD5.DigestAsString, NoProcessingRes), 'Digest not reset correctly'
    );
  finally
    MD5.Free;
  end;
end;

{ TRFCTest }

procedure TRFCTest.CopyDataToStream(const Stm: TStream);
var
  SavedPos: Int64;
begin
  SavedPos := Stm.Position;
  Stm.WriteBuffer(PointerToData^, SizeOfData);
  Stm.Position := SavedPos;
end;

function TRFCTest.DataAsByteArray: TBytes;
var
  I: Cardinal;
begin
  SetLength(Result, Length(Data));
  for I := 1 to Length(Data) do
    Result[I - 1] := Byte(Data[I]);
end;

function TRFCTest.IsEqualResult(const D: TPJMD5Digest): Boolean;
begin
  Result := CompareMem(@ResultBin[0], @D, SizeOf(D));
end;

function TRFCTest.IsEqualResult(const S: string): Boolean;
begin
  Result := S = ResultStr;
end;

function TRFCTest.IsEqualResult(const Bytes: array of Byte): Boolean;
begin
  if Length(Bytes) = Length(ResultBin) then
    Result := CompareMem(@ResultBin[0], @Bytes[0], Length(Bytes))
  else
    Result := False;
end;

function TRFCTest.PointerToData: Pointer;
begin
  Result := Pointer(Data);
end;

function TRFCTest.SizeOfData: Cardinal;
begin
  Result := Length(Data);
end;

{ TestTPJMD5Digest }

function TestTPJMD5Digest.ByteArrayToBytes(const A: array of Byte): TBytes;
var
  Idx: Integer;
begin
  SetLength(Result, Length(A));
  for Idx := Low(A) to High(A) do
    Result[Idx - Low(A)] := A[Idx];
end;

procedure TestTPJMD5Digest.TestDefaultProperty;
var
  Digest: TPJMD5Digest;
begin
  Digest.A := 42;
  Digest.B := $42;
  Digest.C := High(LongWord);
  Digest.D := 1000000000;
  Check(Digest.Parts[0] = Digest.A, '.A <> .Parts[0]');
  Check(Digest.Parts[1] = Digest.B, '.B <> .Parts[1]');
  Check(Digest.Parts[2] = Digest.C, '.C <> .Parts[2]');
  Check(Digest.Parts[3] = Digest.D, '.D <> .Parts[3]');
  Check(Digest[0] = Digest.A, '.A <> [0]');
  Check(Digest[1] = Digest.B, '.B <> [1]');
  Check(Digest[2] = Digest.C, '.C <> [2]');
  Check(Digest[3] = Digest.D, '.D <> [3]');
end;

procedure TestTPJMD5Digest.TestEquality;
var
  B3: TBytes;
begin
  Check(Digest1 = Digest3, 'Digest direct equality test failed');
  Check(Digest1 = MD5Str3, 'Digest string equality test failed');
  Check(MD5Str3 = Digest1, 'Digest string equality test failed');
  B3 := ByteArrayToBytes(MD5Bytes3);
  Check(Digest1 = B3, 'Digest TBytes equality test failed');
  Check(B3 = Digest1, 'Digest TBytes equality test failed');
end;

procedure TestTPJMD5Digest.TestImplicitCasts;
var
  S: string;
  B: TBytes;
  D: TPJMD5Digest;
begin
  S := Digest1; // implicit string cast
  CheckEqualsString(S, MD5Str1);
  D := MD5Str2;
  CheckEqualsMem(@D.Bytes[0], @MD5Bytes2[0], Length(MD5Bytes2));
  B := Digest1; // implicit TBytes cast
  CheckEqualsMem(@B[0], @MD5Bytes1[0], Length(MD5Bytes1));
  D := ByteArrayToBytes(MD5Bytes2); // cast from TBytes
  CheckEqualsMem(@D.Bytes[0], @MD5Bytes2[0], Length(MD5Bytes2));
end;

procedure TestTPJMD5Digest.TestInEquality;
var
  B2: TBytes;
begin
  Check(Digest1 <> Digest2, 'Digest direct inequality test failed');
  Check(Digest1 <> MD5Str2, 'Digest string inequality test failed');
  Check(MD5Str2 <> Digest1, 'Digest string inequality test failed');
  B2 := ByteArrayToBytes(MD5Bytes2);
  Check(Digest1 <> B2, 'Digest TBytes inequality test failed');
  Check(B2 <> Digest1, 'Digest TBytes inequality test failed');
end;

procedure TestTPJMD5Digest.TestVariantRecordParts;
var
  Digest: TPJMD5Digest;
begin
  Digest.A := 42;
  Digest.B := $42;
  Digest.C := High(LongWord);
  Digest.D := 1000000000;
  Check(Digest.LongWords[0] = Digest.A, '.A <> .LongWords[0]');
  Check(Digest.LongWords[1] = Digest.B, '.B <> .LongWords[1]');
  Check(Digest.LongWords[2] = Digest.C, '.C <> .LongWords[2]');
  Check(Digest.LongWords[3] = Digest.D, '.D <> .LongWords[3]');
  Check(Digest.Bytes[0] = (Digest.A shr 0) and $FF);
  Check(Digest.Bytes[1] = (Digest.A shr 8) and $FF);
  Check(Digest.Bytes[2] = (Digest.A shr 16) and $FF);
  Check(Digest.Bytes[3] = (Digest.A shr 24) and $FF);
  Check(Digest.Bytes[4] = (Digest.B shr 0) and $FF);
  Check(Digest.Bytes[5] = (Digest.B shr 8) and $FF);
  Check(Digest.Bytes[6] = (Digest.B shr 16) and $FF);
  Check(Digest.Bytes[7] = (Digest.B shr 24) and $FF);
  Check(Digest.Bytes[8] = (Digest.C shr 0) and $FF);
  Check(Digest.Bytes[9] = (Digest.C shr 8) and $FF);
  Check(Digest.Bytes[10] = (Digest.C shr 16) and $FF);
  Check(Digest.Bytes[11] = (Digest.C shr 24) and $FF);
  Check(Digest.Bytes[12] = (Digest.D shr 0) and $FF);
  Check(Digest.Bytes[13] = (Digest.D shr 8) and $FF);
  Check(Digest.Bytes[14] = (Digest.D shr 16) and $FF);
  Check(Digest.Bytes[15] = (Digest.D shr 24) and $FF);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTPJMD5.Suite);
  RegisterTest(TestTPJMD5Digest.Suite);
end.

