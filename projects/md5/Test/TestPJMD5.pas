﻿{
 * Delphi DUnit Test Case for PJMD5.pas
 * ------------------------------------
 * This unit contains a test case class for the TPJMD5 class and the
 * TPJMD5Digest record in PJMD5.pas.
 *
 * The MD5 algorithm is tested against all seven test cases from RFC 1321 plus
 * some others.
 *
 * The expected results were obtained from either the RFC or by running a third
 * party MD5 console app on the required input data.
 *
 * Delphi 2009 or later is required to compile: Unicode support and anonymous
 * methods are required.
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
 *
 * -----------------------------------------------------------------------------
}

unit TestPJMD5;

interface

// Delphi 2009 or later is required to compile: requires Unicode support and
// anonymous methods
{$UNDEF CANCOMPILE}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 20.0} // Delphi 2009 and later
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
    function DataAsShortString: ShortString;
    function PointerToData: Pointer;
    function IsEqualResult(const S: string): Boolean; overload;
    function IsEqualResult(const Bytes: array of Byte): Boolean; overload;
    function IsEqualResult(const D: TPJMD5Digest): Boolean; overload;
    procedure CopyDataToStream(const Stm: TStream);
  end;

  TProcessMethodCall = reference to procedure(const MD5: TPJMD5;
    const RFCTest: TRFCTest);

  TCalculateMethodCall = reference to function(
    const RFCTest: TRFCTest): TPJMD5Digest;

  // Tests TPJMD5Digest record
  TestTPJMD5Digest = class(TTestCase)
  private
    fErrorImplicitCastId: Integer;
    procedure ErrorImplicitCast;
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
    // Runs of the the RFC tests on a specified Calculate... method
    procedure RunRFCCalcTests(MethodCall: TCalculateMethodCall);
    // Checks if two digests are equal (doesn't use TPJMD5Digest = operator)
    function SameDigests(const D1, D2: TPJMD5Digest): Boolean;
    // Copies an indeterminate number of byte arrays to a stream, which is first
    // emptied
    procedure BytesArraysToStream(const BAs: array of TBytes;
      const Stm: TStream);
  public
    // Sets up test: creates file used by TestProcessFile
    procedure SetUp; override;
    // Tears down test.
    procedure TearDown; override;
    // Runs Finalize test that raises exception
    procedure RunFinalizeTest;
    // Runs test for TestProcessStreamCount that should raise exception
    procedure ErrorProcessStreamCount;
    // Runs test for TestCalculateStreamCount that should raise exception
    procedure ErrorCalculateStreamCount;
    // Run tests for TestProcessArrayOfByteSize that should raise exception
    procedure ErrorProcessArrayOfByteSize;
    procedure ErrorProcessEmptyArrayOfByteSize;
    // Runds test for TestProcessArrayOfByteSizeIndex that should raise
    // exception
    procedure ErrorProcessArrayOfByteSizeIndex;
    // Run tests for TestCalculateArrayOfByteSize that should raise exception
    procedure ErrorCalculateArrayOfByteSize;
    procedure ErrorCalculateEmptyArrayOfByteSize;
    // Runs test for TestCalculateArrayOfByteSizeIndex that should raise
    // exception
    procedure ErrorCalculateArrayOfByteSizeIndex;
  published
    // ** NOTE: Order of these tests is important **

    // Tests Process(TBytes, Cardinal) and Calculate(TBytes, Cardinal) methods
    // and Digest property.
    // Process(TBytes, Cardinal) is called internally by other Process...
    // methods, so must be tested first. The method is also called by some other
    // test methods.
    // This test runs all RFC tests
    procedure TestProcessArrayOfByteSize;
    procedure TestCalculateArrayOfByteSize;

    // Tests Finalize method and Finalized property
    // This test calls Process(array of Byte, Cardinal) so should be run after
    // TestProcessArrayOfByteSize
    procedure TestFinalize;

    // Tests Reset method.
    // This test calls Process(array of Byte, Cardinal) so should be run after
    // TestProcessArrayOfByteSize
    procedure TestReset;

    // Tests Process(TBytes) and Calculate(TBytes) methods.
    // This test runs all RFC tests
    procedure TestProcessArrayOfByte;
    procedure TestCalculateArrayOfByte;

    // Tests Process(TBytes,Cardinal,Cardinal) and
    // Calculate(TBytes,Cardinal,Cardinal) methods.
    procedure TestCalculateArrayOfByteSizeIndex;
    procedure TestProcessArrayOfByteSizeIndex;

    // Tests Process(Untyped, Cardinal) and Calculate(Untyped, Cardinal) methods
    // This test runs all RFC tests
    procedure TestProcessBuf;
    procedure TestCalculateBuf;

    // Tests Process(AnsiString) and Calculate(AnsiString) methods
    // This test runs all RFC tests
    procedure TestProcessRawByteString;
    procedure TestCalculateRawByteString;

    // Tests Process(ShortString) and Calculate(ShortString) methods
    procedure TestProcessShortString;
    procedure TestCalculateShortString;

    // Test Process(WideString) and Calculate(WideString) methods
    procedure TestProcessWideString;
    procedure TestCalculateWideString;

    // Tests Process(UnicodeString) and Calculate(UnicodeString) methods
    // RFC Tests cannot be used with these methods. A special unicode test
    // string is used.
    procedure TestProcessUnicodeString;
    procedure TestCalculateUnicodeString;

    // Tests Process(UnicodeString, TEncoding) and
    // Calculate(UnicodeString, TEncoding) methods
    // RFC Tests cannot be used with these methods. Special unicode test strings
    // are used.
    procedure TestProcessUnicodeStringWithEncoding;
    procedure TestCalculateUnicodeStringWithEncoding;

    // Tests Process(TStream, Int64) and Calculate(TStream, Int64) methods
    // This test runs all RFC tests
    procedure TestProcessStreamCount;
    procedure TestCalculateStreamCount;

    // Tests Process(TStream) and Calculate(TStream) methods
    // This test runs all RFC tests
    procedure TestProcessStream;
    procedure TestCalculateStream;

    // Tests ProcessFile(TFileName) and Calculate(TFileName) methods
    // RFC Tests are not used in this test. Test file has same contents as the
    // unicode test string
    procedure TestProcessFile;
    procedure TestCalculateFile;

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
  UnicodeTestResultDigest: TPJMD5Digest = (
    Bytes: (
      $a6, $5a, $47, $62, $39, $c6, $80, $68,
      $80, $ac, $de, $8b, $a9, $3f, $a4, $a8
    )
  );
  // test string for ASCII encoding
  ASCIITestString = 'QWERTYUIOPASDFGHJKLZXCVBNM0123456789';
  ASCIITestStringRes = '79d291b4afd040c2a18f69e9c832fa1b';
  ASCIITestResultDigest: TPJMD5Digest = (
    Bytes: (
      $79, $d2, $91, $b4, $af, $d0, $40, $c2,
      $a1, $8f, $69, $e9, $c8, $32, $fa, $1b
    )
  );

  // Tests for wide strings: can't use RFC tests
  WideStringTestString: WideString =
    'QWERTY - '#$0444#$1D42' - qwerty - '#$2030' - end.';
  WideStringTestStringRes = 'a65a476239c6806880acde8ba93fa4a8';
  WideStringTestResultDigest: TPJMD5Digest = (
    Bytes: (
      $a6, $5a, $47, $62, $39, $c6, $80, $68,
      $80, $ac, $de, $8b, $a9, $3f, $a4, $a8
    )
  );

  // Tests for files
  // test file containing same content as UnicodeTestString const
  TestFileName = 'Test.tmp';
  TestFileRes = UnicodeTestStringRes;

  // Digest when no data processed = RFCTests[0].ResultStr
  NoProcessingRes = 'd41d8cd98f00b204e9800998ecf8427e';
  NoProcessingDigest: TPJMD5Digest = (
    Bytes: (
      $D4, $1D, $8C, $D9, $8F, $00, $B2, $04,
      $E9, $80, $09, $98, $EC, $F8, $42, $7E
    )
  );


  // Default test
  DefaultRFCTest = 6;

  // Padding bytes (used when testing streams)
  PaddingBytes: array[0..7] of Byte = (1, 2, 3, 4, 5, 6, 7, 8);

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
  cLongArray: array[0..17] of Byte = (
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18
  );

var
  // global, unitialised byte array
  EmptyByteArray: TBytes;

function ByteArrayToBytes(const A: array of Byte): TBytes;
var
  Idx: Integer;
begin
  SetLength(Result, Length(A));
  for Idx := Low(A) to High(A) do
    Result[Idx - Low(A)] := A[Idx];
end;

function ConcatByteArrays(const BAs: array of TBytes): TBytes;
var
  A: TBytes;
  Len: Integer;
  ResIdx, Idx: Integer;
begin
  Len := 0;
  for A in BAs do
    Inc(Len, Length(A));
  SetLength(Result, Len);
  ResIdx := 0;
  for A in BAs do
  begin
    for Idx := Low(A) to High(A) do
    begin
      Result[ResIdx] := A[Idx];
      Inc(ResIdx);
    end;
  end;
end;

procedure TestTPJMD5.BytesArraysToStream(const BAs: array of TBytes;
  const Stm: TStream);
var
  Idx: Integer;
begin
  Stm.Size := 0;  // empties stream
  for Idx := 0 to Pred(Length(BAs)) do
    Stm.WriteBuffer(BAs[Idx][0], Length(BAs[Idx]));
end;

procedure TestTPJMD5.ErrorCalculateArrayOfByteSize;
var
  Bytes: TBytes;
  D: TPJMD5Digest;
  Count: Cardinal;
begin
  Bytes := RFCTests[DefaultRFCTest].DataAsByteArray;
  Count := Length(Bytes) + 1; // count is too large
  D := TPJMD5.Calculate(Bytes, Count); // should raise exception
end;

procedure TestTPJMD5.ErrorCalculateArrayOfByteSizeIndex;
var
  Bytes: TBytes;
  D: TPJMD5Digest;
const
  StartIdx = 3;
  Count = 4;
begin
  Bytes := TBytes.Create(1,2,3,4,5,6);
  D := TPJMD5.Calculate(Bytes, StartIdx, Count);  // should raise exception
end;

procedure TestTPJMD5.ErrorCalculateEmptyArrayOfByteSize;
var
  D: TPJMD5Digest;
const
  Count = 1;
begin
  D := TPJMD5.Calculate(EmptyByteArray, Count); // should raise exception
end;

procedure TestTPJMD5.ErrorCalculateStreamCount;
var
  Stm: TBytesStream;
  Count: Int64;
  D: TPJMD5Digest;
begin
  Stm := TBytesStream.Create(RFCTests[DefaultRFCTest].DataAsByteArray);
  try
    Stm.Position := 0;
    Count := Stm.Size + 1; // even with start Stm.Position = 0, Count is too big
    D := TPJMD5.Calculate(Stm, Count); // should get exception here
  finally
    Stm.Free;
  end;
end;

procedure TestTPJMD5.ErrorProcessArrayOfByteSize;
var
  Bytes: TBytes;
  Count: Cardinal;
  MD5: TPJMD5;
begin
  Bytes := RFCTests[DefaultRFCTest].DataAsByteArray;
  Count := Length(Bytes) + 1; // count is too large
  MD5 := TPJMD5.Create;
  try
    MD5.Process(Bytes, Count);  // should raise exception
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.ErrorProcessArrayOfByteSizeIndex;
var
  Bytes: TBytes;
  MD5: TPJMD5;
const
  StartIdx = 3;
  Count = 4;
begin
  Bytes := TBytes.Create(1,2,3,4,5,6);
  MD5 := TPJMD5.Create;
  try
    MD5.Process(Bytes, StartIdx, Count);  // should raise exception
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.ErrorProcessEmptyArrayOfByteSize;
var
  MD5: TPJMD5;
const
  Count = 1;
begin
  MD5 := TPJMD5.Create;
  try
    MD5.Process(EmptyByteArray, Count);  // should raise exception
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.ErrorProcessStreamCount;
var
  Stm: TBytesStream;
  MD5: TPJMD5;
  Count: Int64;
begin
  Stm := TBytesStream.Create(RFCTests[DefaultRFCTest].DataAsByteArray);
  try
    Stm.Position := 8;
    MD5 := TPJMD5.Create;
    try
      Count := Stm.Size; // with start Stm.Position = 8, Count is too big
      MD5.Process(Stm, Count);  // should get exception here
    finally
      MD5.Free;
    end;
  finally
    Stm.Free;
  end;
end;

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

procedure TestTPJMD5.RunRFCCalcTests(MethodCall: TCalculateMethodCall);
var
  Test: TRFCTest;
  ResultDigest: TPJMD5Digest;
begin
  for Test in RFCTests do
  begin
    ResultDigest := MethodCall(Test); // calls a CalculateXXX method
    Check(
      Test.IsEqualResult(ResultDigest),
      'RFC Test #' + Test.ID + ' Failed - Digest property wrong value'
    );
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
        Test.IsEqualResult(MD5.Digest),
        'RFC Test #' + Test.ID + ' Failed - Digest property wrong string value'
      );
    finally
      MD5.Free;
    end;
  end;
end;

function TestTPJMD5.SameDigests(const D1, D2: TPJMD5Digest): Boolean;
begin
  Result := (D1.A = D2.A) and (D1.B = D2.B) and (D1.C = D2.C) and (D1.D = D2.D);
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

procedure TestTPJMD5.TestCalculateArrayOfByte;
var
  Fn: TCalculateMethodCall;
  D: TPJMD5Digest;
  A: TBytes;
begin
  Fn := function(const Test: TRFCTest): TPJMD5Digest
  begin
    Result := TPJMD5.Calculate(Test.DataAsByteArray);
  end;
  RunRFCCalcTests(Fn);

  D := TPJMD5.Calculate(EmptyByteArray);
  Check(D = RFCTests[1].ResultStr, 'MD5 of EmptyByteArray (0 bytes) expected');

  SetLength(A, 0);
  D := TPJMD5.Calculate(A);
  Check(D = RFCTests[1].ResultStr,
    'MD5 of zero length array (0 bytes) expected');
end;

procedure TestTPJMD5.TestCalculateArrayOfByteSize;
var
  Fn1, Fn2: TCalculateMethodCall;
  A: TBytes;
  Postfix: TBytes;
  D: TPJMD5Digest;
begin
  Fn1 := function(const Test: TRFCTest): TPJMD5Digest
  begin
    Result := TPJMD5.Calculate(Test.DataAsByteArray, Test.SizeOfData);
  end;
  RunRFCCalcTests(Fn1);

  Postfix := TBytes.Create(42, 69, 56, 42);
  Fn2 := function(const Test: TRFCTest): TPJMD5Digest
  begin
    A := ConcatByteArrays([Test.DataAsByteArray, Postfix]);
    Result := TPJMD5.Calculate(A, Test.SizeOfData);
  end;
  RunRFCCalcTests(Fn2);

  A := TBytes.Create(10,20,30);
  D := TPJMD5.Calculate(A, 0);
  Check(D = RFCTests[1].ResultStr, 'MD5 of 0 bytes expected');

  SetLength(A, 0);
  D := TPJMD5.Calculate(A, 0);
  Check(D = RFCTests[1].ResultStr, 'MD5 of A length 0 (0 bytes) expected');

  D := TPJMD5.Calculate(EmptyByteArray, 0);
  Check(D = RFCTests[1].ResultStr, 'MD5 of EmptyByteArray (0 bytes) expected');

  CheckException(ErrorCalculateArrayOfByteSize, EPJMD5);
  CheckException(ErrorCalculateEmptyArrayOfByteSize, EPJMD5);
end;

procedure TestTPJMD5.TestCalculateArrayOfByteSizeIndex;
var
  Prefix, Postfix: TBytes;
  Fn1, Fn2, Fn3, Fn4: TCalculateMethodCall;
  A: TBytes;
  D: TPJMD5Digest;
begin
  Prefix := TBytes.Create(1,2,3,4);
  Postfix := TBytes.Create(5,6,7,8);

  Fn1 := function(const Test: TRFCTest): TPJMD5Digest
  begin
    Result := TPJMD5.Calculate(Test.DataAsByteArray, 0, Test.SizeOfData);
  end;
  RunRFCCalcTests(Fn1);

  Fn2 := function(const Test: TRFCTest): TPJMD5Digest
  begin
    A := ConcatByteArrays([Prefix, Test.DataAsByteArray, Postfix]);
    Result := TPJMD5.Calculate(A, Length(Prefix), Test.SizeOfData);
  end;
  RunRFCCalcTests(Fn2);

  Fn3 := function(const Test: TRFCTest): TPJMD5Digest
  begin
    A := ConcatByteArrays([Prefix, Test.DataAsByteArray]);
    Result := TPJMD5.Calculate(A, Length(Prefix), Test.SizeOfData);
  end;
  RunRFCCalcTests(Fn3);

  Fn4 := function(const Test: TRFCTest): TPJMD5Digest
  begin
    A := ConcatByteArrays([Test.DataAsByteArray, Postfix]);
    Result := TPJMD5.Calculate(A, 0, Test.SizeOfData);
  end;
  RunRFCCalcTests(Fn4);

  A := TBytes.Create(10,20,30);
  D := TPJMD5.Calculate(A, 3, 1);
  Check(D = RFCTests[1].ResultStr, 'MD5 of 0 bytes expected (A, 3, 1)');

  A := TBytes.Create(10,20,30);
  D := TPJMD5.Calculate(A, 0, 0);
  Check(D = RFCTests[1].ResultStr, 'MD5 of 0 bytes expected (A, 0, 0)');

  SetLength(A, 0);
  D := TPJMD5.Calculate(A, 0, 0);
  Check(D = RFCTests[1].ResultStr, 'MD5 of 0 bytes expected (A-length-0, 0, 0');

  D := TPJMD5.Calculate(EmptyByteArray, 0, 0);
  Check(D = RFCTests[1].ResultStr,
    'MD5 of 0 bytes expected (EmptyByteArray, 0, 0)');

  CheckException(ErrorCalculateArrayOfByteSizeIndex, EPJMD5);
end;

procedure TestTPJMD5.TestCalculateBuf;
var
  Fn: TCalculateMethodCall;
begin
  Fn := function(const Test: TRFCTest): TPJMD5Digest
  begin
    Result := TPJMD5.Calculate(Test.PointerToData^, Test.SizeOfData);
  end;
  RunRFCCalcTests(Fn);
end;

procedure TestTPJMD5.TestCalculateFile;
var
  ResultDigest: TPJMD5Digest;
  ExpectedDigest: TPJMD5Digest;
begin
  ExpectedDigest := UnicodeTestResultDigest;  // file === unicode test string
  ResultDigest := TPJMD5.CalculateFile(TestFilePath + TestFileName);
  Check(SameDigests(ExpectedDigest, ResultDigest), 'File ' + TestFileName);
end;

procedure TestTPJMD5.TestCalculateRawByteString;
var
  Fn: TCalculateMethodCall;
begin
  Fn := function(const Test: TRFCTest): TPJMD5Digest
  begin
    Result := TPJMD5.Calculate(Test.Data);
  end;
  RunRFCCalcTests(Fn);
end;

procedure TestTPJMD5.TestCalculateShortString;
var
  Fn: TCalculateMethodCall;
begin
  Fn := function(const Test: TRFCTest): TPJMD5Digest
  begin
    Result := TPJMD5.Calculate(Test.DataAsShortString);
  end;
  RunRFCCalcTests(Fn);
end;

procedure TestTPJMD5.TestCalculateStream;
var
  Fn: TCalculateMethodCall;
  Stream: TStream;
begin
  Fn := function(const Test: TRFCTest): TPJMD5Digest
  begin
    Stream.Size := 0;
    Test.CopyDataToStream(Stream);
    Result := TPJMD5.Calculate(Stream);
  end;
  Stream := TMemoryStream.Create;
  try
    RunRFCCalcTests(Fn);
  finally
    Stream.Free;
  end;
end;

procedure TestTPJMD5.TestCalculateStreamCount;
var
  Fn: TCalculateMethodCall;
  Stream: TStream;
  Count: Int64;
begin
  // Required data is sandwiched in between two blocks of padding. Stream start
  // position is just after first block of padding. Count = length of test data
  Fn := function(const Test: TRFCTest): TPJMD5Digest
  begin
    // Stream is Padding + TestData + Padding
    BytesArraysToStream(
      [
        ByteArrayToBytes(PaddingBytes),
        Test.DataAsByteArray,
        ByteArrayToBytes(PaddingBytes)
      ],
      Stream
    );
    Count := Test.SizeOfData;
    Stream.Position := Length(PaddingBytes);
    Count := Test.SizeOfData;
    Result := TPJMD5.Calculate(Stream, Count);
  end;
  Stream := TMemoryStream.Create;
  try
    RunRFCCalcTests(Fn);
  finally
    Stream.Free;
  end;
  CheckException(ErrorCalculateStreamCount, EPJMD5);
end;

procedure TestTPJMD5.TestCalculateUnicodeString;
var
  ResultDigest: TPJMD5Digest;
  ExpectedDigest: TPJMD5Digest;
begin
  // tests using default encoding: assumes default encoding writes same bytes
  // as ansi string
  ExpectedDigest := ASCIITestResultDigest;
  ResultDigest :=  TPJMD5.Calculate(ASCIITestString);
  Check(
    SameDigests(ResultDigest, ExpectedDigest),
    'Calculate(UnicodeString): ASCII string'
  );

  ResultDigest := TPJMD5.Calculate(EmptyStr);
  Check(
    SameDigests(ResultDigest, NoProcessingDigest),
    'Calculate(UnicodeString): empty string'
  );
end;

procedure TestTPJMD5.TestCalculateUnicodeStringWithEncoding;
var
  ResultDigest: TPJMD5Digest;
  ExpectedDigest: TPJMD5Digest;
begin
  // tests using default encoding: assumes default encoding writes same bytes
  // as ansi string
  ExpectedDigest := ASCIITestResultDigest;
  ResultDigest :=  TPJMD5.Calculate(ASCIITestString, TEncoding.ASCII);
  Check(
    SameDigests(ResultDigest, ExpectedDigest),
    'Calculate(UnicodeString, TEncoding.ASCII): ASCIITestString'
  );

  ResultDigest := TPJMD5.Calculate(EmptyStr, TEncoding.ASCII);
  Check(
    SameDigests(ResultDigest, NoProcessingDigest),
    'Calculate(UnicodeString, TEncoding.ASCII): empty string'
  );

  ExpectedDigest := UnicodeTestResultDigest;
  ResultDigest :=  TPJMD5.Calculate(UnicodeTestString, TEncoding.Unicode);
  Check(
    SameDigests(ResultDigest, ExpectedDigest),
    'Calculate(UnicodeString, TEncoding.Unicode): UnicodeTestString'
  );

  ResultDigest := TPJMD5.Calculate(EmptyStr, TEncoding.Unicode);
  Check(
    SameDigests(ResultDigest, NoProcessingDigest),
    'Calculate(UnicodeString, TEncoding.Unicode): empty string'
  );
end;

procedure TestTPJMD5.TestCalculateWideString;
var
  ResultDigest: TPJMD5Digest;
begin
  ResultDigest := TPJMD5.Calculate(WideStringTestString);
  Check(
    SameDigests(ResultDigest, WideStringTestResultDigest),
    'Calculate(WideString): WideStringTestString'
  );

  ResultDigest := TPJMD5.Calculate(EmptyWideStr);
  Check(
    SameDigests(ResultDigest, NoProcessingDigest),
    'Calculate(WideString): empty string'
  );
end;

function TestTPJMD5.TestFilePath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure TestTPJMD5.TestFinalize;
begin
  CheckException(RunFinalizeTest, EPJMD5,
    EPJMD5.ClassName + ' Exception expected');
end;

procedure TestTPJMD5.TestProcessArrayOfByte;
var
  Fn: TProcessMethodCall;
  MD5: TPJMD5;
  A: TBytes;
begin
  Fn := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    MD5.Process(Test.DataAsByteArray);
  end;
  RunRFCTests(Fn);

  MD5 := TPJMD5.Create;
  try
    MD5.Process(EmptyByteArray);
    Check(MD5.Digest = RFCTests[1].ResultStr,
      'MD5 of EmptyByteArray (0 bytes) expected');
  finally
    MD5.Free;
  end;

  MD5 := TPJMD5.Create;
  try
    SetLength(A, 0);
    MD5.Process(A);
    Check(MD5.Digest = RFCTests[1].ResultStr,
      'MD5 of zero length array (0 bytes) expected');
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.TestProcessArrayOfByteSize;
var
  Fn1, Fn2: TProcessMethodCall;
  Postfix: TBytes;
  A: TBytes;
  MD5: TPJMD5;
begin
  Fn1 := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    MD5.Process(Test.DataAsByteArray, Test.SizeOfData);
  end;
  RunRFCTests(Fn1);

  Postfix := TBytes.Create(42, 56, 69, 42);
  Fn2 := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    A := ConcatByteArrays([Test.DataAsByteArray, Postfix]);
    MD5.Process(A, Test.SizeOfData);
  end;
  RunRFCTests(Fn2);

  A := TBytes.Create(10,20,30);
  MD5 := TPJMD5.Create;
  try
    MD5.Process(A, 0);
    Check(MD5.Digest = RFCTests[1].ResultStr, 'MD5 of 0 bytes expected');
  finally
    MD5.Free;
  end;

  SetLength(A, 0);
  MD5 := TPJMD5.Create;
  try
    MD5.Process(A, 0);
    Check(MD5.Digest = RFCTests[1].ResultStr,
      'A (length 0) MD5 of 0 bytes expected');
  finally
    MD5.Free;
  end;

  MD5 := TPJMD5.Create;
  try
    MD5.Process(EmptyByteArray, 0);
    Check(MD5.Digest = RFCTests[1].ResultStr,
      'EmptyByteArray MD5 of 0 bytes expected');
  finally
    MD5.Free;
  end;

  CheckException(ErrorProcessArrayOfByteSize, EPJMD5);
  CheckException(ErrorProcessEmptyArrayOfByteSize, EPJMD5);
end;

procedure TestTPJMD5.TestProcessArrayOfByteSizeIndex;
var
  Prefix, Postfix: TBytes;
  Fn1, Fn2, Fn3, Fn4: TProcessMethodCall;
  A: TBytes;
  MD5: TPJMD5;
begin
  Prefix := TBytes.Create(1,2,3,4);
  Postfix := TBytes.Create(5,6,7,8);

  Fn1 := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    MD5.Process(Test.DataAsByteArray, 0, Test.SizeOfData);
  end;
  RunRFCTests(Fn1);

  Fn2 := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    A := ConcatByteArrays([Prefix, Test.DataAsByteArray, Postfix]);
    MD5.Process(A, Length(Prefix), Test.SizeOfData);
  end;
  RunRFCTests(Fn2);

  Fn3 := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    A := ConcatByteArrays([Prefix, Test.DataAsByteArray]);
    MD5.Process(A, Length(Prefix), Test.SizeOfData);
  end;
  RunRFCTests(Fn3);

  Fn4 := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    A := ConcatByteArrays([Test.DataAsByteArray, Postfix]);
    MD5.Process(A, 0, Test.SizeOfData);
  end;
  RunRFCTests(Fn4);

  A := TBytes.Create(10,20,30);
  MD5 := TPJMD5.Create;
  try
    MD5.Process(A, 3, 1);
    Check(MD5.Digest = RFCTests[1].ResultStr,
      'MD5 of 0 bytes expected (A, 3, 1)');
  finally
    MD5.Free;
  end;

  A := TBytes.Create(10,20,30);
  MD5 := TPJMD5.Create;
  try
    MD5.Process(A, 0, 0);
    Check(MD5.Digest = RFCTests[1].ResultStr,
      'MD5 of 0 bytes expected (A, 0, 0)');
  finally
    MD5.Free;
  end;

  SetLength(A, 0);
  MD5 := TPJMD5.Create;
  try
    MD5.Process(A, 0, 0);
    Check(MD5.Digest = RFCTests[1].ResultStr,
      'MD5 of 0 bytes expected (A-length-0, 0, 0)');
  finally
    MD5.Free;
  end;

  MD5 := TPJMD5.Create;
  try
    MD5.Process(EmptyByteArray, 0, 0);
    Check(MD5.Digest = RFCTests[1].ResultStr,
      'MD5 of 0 bytes expected (EmptyByteArray, 0, 0)');
  finally
    MD5.Free;
  end;

  CheckException(ErrorProcessArrayOfByteSizeIndex, EPJMD5);
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
    Check(SameStr(MD5.Digest, TestFileRes), 'File ' + TestFileName);
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.TestProcessRawByteString;
var
  Fn: TProcessMethodCall;
begin
  Fn := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    MD5.Process(Test.Data);
  end;
  RunRFCTests(Fn);
end;

procedure TestTPJMD5.TestProcessShortString;
var
  Fn: TProcessMethodCall;
begin
  Fn := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    MD5.Process(Test.DataAsShortString);
  end;
  RunRFCTests(Fn);
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

procedure TestTPJMD5.TestProcessStreamCount;
var
  Fn, Fn2: TProcessMethodCall;
  Stream: TStream;
  Count: Int64;
begin
  Fn := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    // stream is TestData + Padding
    MD5.ReadBufferSize := 48;
    BytesArraysToStream(
      [Test.DataAsByteArray, ByteArrayToBytes(PaddingBytes)], Stream
    );
    Count := Test.SizeOfData;
    Stream.Position := 0;
    MD5.Process(Stream, Count);
  end;

  Fn2 := procedure(const MD5: TPJMD5; const Test: TRFCTest)
  begin
    // Stream is Padding + TestData + Padding
    BytesArraysToStream(
      [
        ByteArrayToBytes(PaddingBytes),
        Test.DataAsByteArray,
        ByteArrayToBytes(PaddingBytes)
      ],
      Stream
    );
    Count := Test.SizeOfData;
    Stream.Position := Length(PaddingBytes);
    MD5.Process(Stream, Count);
  end;

  Stream := TMemoryStream.Create;
  try
    RunRFCTests(Fn);
    RunRFCTests(Fn2);
  finally
    Stream.Free;
  end;
  CheckException(ErrorProcessStreamCount, EPJMD5);
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
      SameStr(MD5.Digest, ASCIITestStringRes),
      'Unicode ASCII string test (default encoding)'
    );
  finally
    MD5.Free;
  end;

  // empty string test
  MD5 := TPJMD5.Create;
  try
    MD5.Process(EmptyStr);
    Check(
      SameStr(MD5.Digest, NoProcessingRes),
      'Unicode empty string test (default encoding)'
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
      SameStr(MD5.Digest, ASCIITestStringRes),
      'Unicode ASCII string test (ASCII encoding)'
    );
  finally
    MD5.Free;
  end;

  // empty string test
  MD5 := TPJMD5.Create;
  try
    MD5.Process(EmptyStr, TEncoding.ASCII);
    Check(
      SameStr(MD5.Digest, NoProcessingRes),
      'Unicode empty string test (ASCII encoding)'
    );
  finally
    MD5.Free;
  end;

  MD5 := TPJMD5.Create;
  try
    MD5.Process(UnicodeTestString, TEncoding.Unicode);
    Check(
      SameStr(MD5.Digest, UnicodeTestStringRes),
      'Unicode string test (Unicode encoding)'
    );
  finally
    MD5.Free;
  end;

  // empty string test
  MD5 := TPJMD5.Create;
  try
    MD5.Process(EmptyStr, TEncoding.Unicode);
    Check(
      SameStr(MD5.Digest, NoProcessingRes),
      'Unicode empty string test (Unicode encoding)'
    );
  finally
    MD5.Free;
  end;
end;

procedure TestTPJMD5.TestProcessWideString;
var
  MD5: TPJMD5;
begin
  MD5 := TPJMD5.Create;
  try
    MD5.Process(WideStringTestString);;
    Check(
      SameStr(MD5.Digest, WideStringTestStringRes),
      'Process(WideString) WideStringTestString'
    );
  finally
    MD5.Free;
  end;

  MD5 := TPJMD5.Create;
  try
    MD5.Process(EmptyWideStr);;
    Check(
      SameStr(MD5.Digest, NoProcessingRes),
      'Process(WideString) empty string'
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
      not SameStr(MD5.Digest, NoProcessingRes), 'Unexpected digest'
    );
    Check(MD5.Finalized, TPJMD5.ClassName + '.Finalized should be True');
    MD5.Reset;
    Check(
      not MD5.Finalized, TPJMD5.ClassName + '.Finalized should be False'
    );
    Check(
      SameStr(MD5.Digest, NoProcessingRes), 'Digest not reset correctly'
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

function TRFCTest.DataAsShortString: ShortString;
begin
  Result := Data;
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

procedure TestTPJMD5Digest.ErrorImplicitCast;
var
  D: TPJMD5Digest;
  B: TBytes;
  S1, S2: string;
begin
  case fErrorImplicitCastId of
    0:
    begin
      SetLength(B, 12);
      D := B;   // too small error
    end;
    1:
    begin
      S1 := 'foo';
      D := S1;  // wrong size
    end;
    2:
    begin
      S2 := 'd174ab98d27Xd9f5a5611c2c9f419d9f';
      D := S2;  // bad hex
    end;
  end;
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
  B: TBytes;
  S: string;
begin
  // comparing two digests
  Check(Digest1 = Digest3, 'Digest direct equality test failed');

  // comparing valid byte array with digest
  B := ByteArrayToBytes(MD5Bytes3);
  Check(Digest1 = B, 'Digest TBytes equality test failed');
  Check(B = Digest1, 'Digest TBytes equality test failed');

  // comparing invalid byte array with digest: array wrong size
  B := ByteArrayToBytes(cLongArray);
  Check(not (B = Digest1), 'Digest TBytes equality test failed');
  Check(not (Digest1 = B), 'Digest TBytes equality test failed');

  // comparing valid string with digest
  Check(Digest1 = MD5Str3, 'Digest string equality test failed');
  Check(MD5Str3 = Digest1, 'Digest string equality test failed');

  // comparing invalid strings with digest: string wrong size and bad hex
  S := 'foo'; // too short
  Check(not (S = Digest1), 'Digest string equality test failed');
  Check(not (Digest1 = S), 'Digest string equality test failed');
  S := 'd174ab98d27Xd9f5a5611c2c9f419d9f';  // bad hex
  Check(not (S = Digest1), 'Digest string equality test failed');
  Check(not (Digest1 = S), 'Digest string equality test failed');
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

  fErrorImplicitCastId := 0;
  CheckException(ErrorImplicitCast, EPJMD5, 'TBytes too long');
  fErrorImplicitCastId := 1;
  CheckException(ErrorImplicitCast, EPJMD5, 'String too long');
  fErrorImplicitCastId := 2;
  CheckException(ErrorImplicitCast, EPJMD5, 'String bad hex');

end;

procedure TestTPJMD5Digest.TestInEquality;
var
  B: TBytes;
  S: string;
begin
  // comparing two digests
  Check(Digest1 <> Digest2, 'Digest direct inequality test failed');

  // comparing valid byte array with digest
  B := ByteArrayToBytes(MD5Bytes2);
  Check(Digest1 <> B, 'Digest TBytes inequality test failed');
  Check(B <> Digest1, 'Digest TBytes inequality test failed');

  // comparing invalid byte array with digest: array wrong size
  B := ByteArrayToBytes(cLongArray);
  Check(B <> Digest1, 'Digest TBytes inequality test failed');
  Check(Digest1 <> B, 'Digest TBytes inequality test failed');

  // comparing valid string with digest
  Check(Digest1 <> MD5Str2, 'Digest string inequality test failed');
  Check(MD5Str2 <> Digest1, 'Digest string inequality test failed');

  // comparing invalid strings with digest: string wrong size and bad hex
  S := 'foo'; // too short
  Check(S <> Digest1, 'Digest string equality test failed');
  Check(Digest1 <> S, 'Digest string equality test failed');
  S := 'd174ab98d27Xd9f5a5611c2c9f419d9f';  // bad hex
  Check(S <> Digest1, 'Digest string equality test failed');
  Check(Digest1 <> S, 'Digest string equality test failed');
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

