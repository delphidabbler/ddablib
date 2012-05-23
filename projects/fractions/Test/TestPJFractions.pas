{
 * -----------------------------------------------------------------------------
 * Delphi DUnit test cases for the PJFractions unit.
 *
 * These test cases were created with Delphi 2010 and haven't been checked with
 * any other compiler.
 *
 * $Rev$
 * $Date$
 * -----------------------------------------------------------------------------
}

unit TestPJFractions;

interface

uses
  TestFramework, PJFractions;

type
  // Test case for TFraction
  TestTFraction = class(TTestCase)
  published
    // Order of tests is important since some later tests assume some of methods
    // tested earlier work correctly.
    procedure TestConstructors;
    procedure TestImplicit;
    procedure TestIsProper;
    procedure TestIsWholeNumber;
    procedure TestLCD;
    procedure TestCompare;
    procedure TestCompareTo;
    procedure TestSimplify;
    procedure TestReciprocal;
    procedure TestSign;
    procedure TestIsCommonFactor;
    procedure TestComparisonOps;
    procedure TestTruncOp;
    procedure TestRoundOp;
    procedure TestUnaryPlusOp;
    procedure TestUnaryMinusOp;
    procedure TestAddOp;
    procedure TestSubtractOp;
    procedure TestMultiplyOp;
    procedure TestDivideOp;
    procedure TestIntDivideOp;
    procedure TestModulusOp;
    procedure TestMax;
    procedure TestMin;
    procedure TestConvert;
    procedure TestRoundToMulitiple;
    procedure TestTruncateToMultiple;
  end;

  // Test case for TMixedFraction
  TestTMixedFraction = class(TTestCase)
  published
    procedure TestConstructors;
    procedure TestImplicit;
    procedure TestIsWholeNumber;
    procedure TestSign;
    procedure TestCompare;
    procedure TestCompareTo;
    procedure TestComparisonOps;
    procedure TestUnaryPlusOp;
    procedure TestUnaryMinusOp;
    procedure TestReciprocal;
    procedure TestAddOp;
    procedure TestSubtractOp;
    procedure TestMultiplyOp;
    procedure TestDivideOp;
    procedure TestIntDivideOp;
    procedure TestModulusOp;
    procedure TestTruncOp;
    procedure TestRoundOp;
    procedure TestMax;
    procedure TestMin;
    procedure TestIsCommonFactor;
    procedure TestConvert;
  end;

implementation

uses
  Types, Math;

{ TestTFraction }

procedure TestTFraction.TestAddOp;
var
  FRes, F1, F2: TFraction;
begin
  // 1/3 + 1/3 = 2/3
  F1 := TFraction.Create(1, 3);
  F2 := TFraction.Create(1, 3);
  FRes := F1 + F2;
  CheckEquals(2, FRes.Numerator, 'Test 1 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 1 Denominator');
  // 2/3 + 5/6 = 4/6 + 5/6 = 9/6 = 3/2
  F1 := TFraction.Create(2, 3);
  F2 := TFraction.Create(5, 6);
  FRes := F1 + F2;
  CheckEquals(3, FRes.Numerator, 'Test 2 Numerator');
  CheckEquals(2, FRes.Denominator, 'Test 2 Denominator');
  // 2/9 + 7/15 = 10/45 + 21/45 = 31/45
  F1 := TFraction.Create(2, 9);
  F2 := TFraction.Create(7, 15);
  FRes := F1 + F2;
  CheckEquals(31, FRes.Numerator, 'Test 3 Numerator');
  CheckEquals(45, FRes.Denominator, 'Test 3 Denominator');
  // 2/5 + 6/7 = 14/35 + 30/35 = 44/35
  F1 := TFraction.Create(2, 5);
  F2 := TFraction.Create(6, 7);
  FRes := F1 + F2;
  CheckEquals(44, FRes.Numerator, 'Test 4 Numerator');
  CheckEquals(35, FRes.Denominator, 'Test 4 Denominator');
  // -1/3 + 1/3 = 0
  F1 := TFraction.Create(-1, 3);
  F2 := TFraction.Create(1, 3);
  FRes := F1 + F2;
  CheckEquals(0, FRes.Numerator, 'Test 5 Numerator');
  CheckEquals(1, FRes.Denominator, 'Test 5 Denominator');
  // -1/3 + 2/3 = 1/3
  F1 := TFraction.Create(-1, 3);
  F2 := TFraction.Create(2, 3);
  FRes := F1 + F2;
  CheckEquals(1, FRes.Numerator, 'Test 6 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 6 Denominator');
  // 1/3 + -2/3 = -1/3
  F1 := TFraction.Create(1, 3);
  F2 := TFraction.Create(-2, 3);
  FRes := F1 + F2;
  CheckEquals(-1, FRes.Numerator, 'Test 7 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 7 Denominator');
  // -5/6 + -5/8 = -20/24 + -15/24 = -35/24
  F1 := TFraction.Create(-5, 6);
  F2 := TFraction.Create(-5, 8);
  FRes := F1 + F2;
  CheckEquals(-35, FRes.Numerator, 'Test 8 Numerator');
  CheckEquals(24, FRes.Denominator, 'Test 8 Denominator');
  // 1 + -5/6 = 6/6 + -5/6 = 1/6
  F1 := 1;
  F2 := TFraction.Create(-5, 6);
  FRes := F1 + F2;
  CheckEquals(1, FRes.Numerator, 'Test 9 Numerator');
  CheckEquals(6, FRes.Denominator, 'Test 9 Denominator');
  // 2 + 5/6 = 12/6 + 5/6 = 17/6
  F2 := TFraction.Create(5, 6);
  FRes := 2 + F2;
  CheckEquals(17, FRes.Numerator, 'Test 10 Numerator');
  CheckEquals(6, FRes.Denominator, 'Test 10 Denominator');
  // 5/13 + -3 = 5/13 + -39/13 = -34/13
  F1 := TFraction.Create(5, 13);
  FRes := F1 + -3;
  CheckEquals(-34, FRes.Numerator, 'Test 11 Numerator');
  CheckEquals(13, FRes.Denominator, 'Test 11 Denominator');
end;

procedure TestTFraction.TestCompare;
var
  F1, F2, F3, F4, F5, F6, F7, F8: TFraction;
begin
  F1 := TFraction.Create(3, 4);
  F2 := F1;
  F3 := TFraction.Create(-2, 4);
  F4 := TFraction.Create(7, 9);
  F5 := TFraction.Create(12, 16);
  F6 := TFraction.Create(16, 12);
  F7 := 4;
  F8 := 0;
  CheckEquals(EqualsValue, TFraction.Compare(F1, F2), 'Test 1');
  CheckEquals(LessThanValue, TFraction.Compare(F1, F4), 'Test 2');
  CheckEquals(GreaterThanValue, TFraction.Compare(F1, F3), 'Test 3');
  CheckEquals(EqualsValue, TFraction.Compare(F1, F5), 'Test 4');
  CheckEquals(LessThanValue, TFraction.Compare(F5, F6), 'Test 5');
  CheckEquals(GreaterThanValue, TFraction.Compare(F7, F8), 'Test 6');
  CheckEquals(LessThanValue, TFraction.Compare(F3, F8), 'Test 7');
  CheckEquals(GreaterThanValue, TFraction.Compare(F8, F3), 'Test 8');
end;

procedure TestTFraction.TestCompareTo;
var
  F1, F2, F3, F4, F5, F6, F7, F8: TFraction;
begin
  F1 := TFraction.Create(3, 4);
  F2 := F1;
  F3 := TFraction.Create(-2, 4);
  F4 := TFraction.Create(7, 9);
  F5 := TFraction.Create(12, 16);
  F6 := TFraction.Create(16, 12);
  F7 := 4;
  F8 := 0;
  CheckEquals(EqualsValue, F1.CompareTo(F2), 'Test 1');
  CheckEquals(LessThanValue, F1.CompareTo(F4), 'Test 2');
  CheckEquals(GreaterThanValue, F1.CompareTo(F3), 'Test 3');
  CheckEquals(EqualsValue, F1.CompareTo(F5), 'Test 4');
  CheckEquals(LessThanValue, F5.CompareTo(F6), 'Test 5');
  CheckEquals(GreaterThanValue, F7.CompareTo(F8), 'Test 6');
  CheckEquals(LessThanValue, F3.CompareTo(F8), 'Test 7');
  CheckEquals(GreaterThanValue, F8.CompareTo(F3), 'Test 8');
end;

procedure TestTFraction.TestComparisonOps;
var
  F1, F2: TFraction;
begin
  // F1 = F2 
  F1 := TFraction.Create(1, 2);
  F2 := TFraction.Create(1, 2);
  CheckTrue(F1 = F2, 'Test 1a (=)');
  CheckFalse(F1 <> F2, 'Test 1a (<>)');
  CheckFalse(F1 < F2, 'Test 1a (<)');
  CheckTrue(F1 <= F2, 'Test 1a (<=)');
  CheckFalse(F1 > F2, 'Test 1a (>)');
  CheckTrue(F1 >= F2, 'Test 1a (>=)');

  // F1 = F2
  F1 := TFraction.Create(6, 5);
  F2 := TFraction.Create(30, 25);
  CheckTrue(F1 = F2, 'Test 1b (=)');
  CheckFalse(F1 <> F2, 'Test 1b (<>)');
  CheckFalse(F1 < F2, 'Test 1b (<)');
  CheckTrue(F1 <= F2, 'Test 1b (<=)');
  CheckFalse(F1 > F2, 'Test 1b (>)');
  CheckTrue(F1 >= F2, 'Test 1b (>=)');

  // F1 = F2 
  F1 := TFraction.Create(-6, 5);
  F2 := TFraction.Create(30, -25);
  CheckTrue(F1 = F2, 'Test 1c (=)');
  CheckFalse(F1 <> F2, 'Test 1c (<>)');
  CheckFalse(F1 < F2, 'Test 1c (<)');
  CheckTrue(F1 <= F2, 'Test 1c (<=)');
  CheckFalse(F1 > F2, 'Test 1c (>)');
  CheckTrue(F1 >= F2, 'Test 1c (>=)');

  // F1 = F2 
  F1 := TFraction.Create(6, 5);
  F2 := TFraction.Create(-30, -25);
  CheckTrue(F1 = F2, 'Test 1d (=)');
  CheckFalse(F1 <> F2, 'Test 1d (<>)');
  CheckFalse(F1 < F2, 'Test 1d (<)');
  CheckTrue(F1 <= F2, 'Test 1d (<=)');
  CheckFalse(F1 > F2, 'Test 1d (>)');
  CheckTrue(F1 >= F2, 'Test 1d (>=)');

  // F1 > F2 
  F1 := TFraction.Create(4, 5);
  F2 := TFraction.Create(3, 5);
  CheckFalse(F1 = F2, 'Test 2a (=)');
  CheckTrue(F1 <> F2, 'Test 2a (<>)');
  CheckFalse(F1 < F2, 'Test 2a (<)');
  CheckFalse(F1 <= F2, 'Test 2a (<=)');
  CheckTrue(F1 > F2, 'Test 2a (>)');
  CheckTrue(F1 >= F2, 'Test 2a (>=)');

  // F1 > F2 
  F1 := TFraction.Create(1, 2);
  F2 := TFraction.Create(3, 8);
  CheckFalse(F1 = F2, 'Test 2b (=)');
  CheckTrue(F1 <> F2, 'Test 2b (<>)');
  CheckFalse(F1 < F2, 'Test 2b (<)');
  CheckFalse(F1 <= F2, 'Test 2b (<=)');
  CheckTrue(F1 > F2, 'Test 2b (>)');
  CheckTrue(F1 >= F2, 'Test 2b (>=)');

  // F1 > F2 
  F1 := TFraction.Create(-3, 8);
  F2 := TFraction.Create(-1, 2);
  CheckFalse(F1 = F2, 'Test 2c (=)');
  CheckTrue(F1 <> F2, 'Test 2c (<>)');
  CheckFalse(F1 < F2, 'Test 2c (<)');
  CheckFalse(F1 <= F2, 'Test 2c (<=)');
  CheckTrue(F1 > F2, 'Test 2c (>)');
  CheckTrue(F1 >= F2, 'Test 2c (>=)');

  // F1 > F2 
  F1 := TFraction.Create(-1, -2);
  F2 := TFraction.Create(3, 8);
  CheckFalse(F1 = F2, 'Test 2d (=)');
  CheckTrue(F1 <> F2, 'Test 2d (<>)');
  CheckFalse(F1 < F2, 'Test 2d (<)');
  CheckFalse(F1 <= F2, 'Test 2d (<=)');
  CheckTrue(F1 > F2, 'Test 2d (>)');
  CheckTrue(F1 >= F2, 'Test 2d (>=)');

  // F1 < F2 
  F1 := TFraction.Create(-1, 5);
  F2 := TFraction.Create(-1, 9);
  CheckFalse(F1 = F2, 'Test 3a (=)');
  CheckTrue(F1 <> F2, 'Test 3a (<>)');
  CheckTrue(F1 < F2, 'Test 3a (<)');
  CheckTrue(F1 <= F2, 'Test 3a (<=)');
  CheckFalse(F1 > F2, 'Test 3a (>)');
  CheckFalse(F1 >= F2, 'Test 3a (>=)');

  // F1 < F2 
  F1 := TFraction.Create(3, 5);
  F2 := TFraction.Create(8, 11);
  CheckFalse(F1 = F2, 'Test 3b (=)');
  CheckTrue(F1 <> F2, 'Test 3b (<>)');
  CheckTrue(F1 < F2, 'Test 3b (<)');
  CheckTrue(F1 <= F2, 'Test 3b (<=)');
  CheckFalse(F1 > F2, 'Test 3b (>)');
  CheckFalse(F1 >= F2, 'Test 3b (>=)');

  // F1 < F2 
  F1 := TFraction.Create(-11, 9);
  F2 := TFraction.Create(3, -8);
  CheckFalse(F1 = F2, 'Test 3c (=)');
  CheckTrue(F1 <> F2, 'Test 3c (<>)');
  CheckTrue(F1 < F2, 'Test 3c (<)');
  CheckTrue(F1 <= F2, 'Test 3c (<=)');
  CheckFalse(F1 > F2, 'Test 3c (>)');
  CheckFalse(F1 >= F2, 'Test 3c (>=)');

  // F1 < F2 
  F1 := TFraction.Create(3, 4);
  F2 := TFraction.Create(-7, -8);
  CheckFalse(F1 = F2, 'Test 3c (=)');
  CheckTrue(F1 <> F2, 'Test 3c (<>)');
  CheckTrue(F1 < F2, 'Test 3c (<)');
  CheckTrue(F1 <= F2, 'Test 3c (<=)');
  CheckFalse(F1 > F2, 'Test 3c (>)');
  CheckFalse(F1 >= F2, 'Test 3c (>=)');
end;

procedure TestTFraction.TestConstructors;
var
  F: TFraction;
begin
  F := TFraction.Create(3, 5);
  CheckEquals(3, F.Numerator, 'Test 1a: Numerator');
  CheckEquals(5, F.Denominator, 'Test 1a: Denominator');
  F := TFraction.Create(-3, 5);
  CheckEquals(-3, F.Numerator, 'Test 1b: Numerator');
  CheckEquals(5, F.Denominator, 'Test 1b: Denominator');
  F := TFraction.Create(3, -5);
  CheckEquals(-3, F.Numerator, 'Test 1c: Numerator');
  CheckEquals(5, F.Denominator, 'Test 1c: Denominator');
  F := TFraction.Create(-3, -5);
  CheckEquals(3, F.Numerator, 'Test 1d: Numerator');
  CheckEquals(5, F.Denominator, 'Test 1d: Denominator');
end;

procedure TestTFraction.TestConvert;
var
  F, FC: TFraction;
begin
  F := TFraction.Create(1, 2);
  FC := F.Convert(6);
  CheckEquals(6, FC.Numerator, 'Test 1 Numerator');
  CheckEquals(12, FC.Denominator, 'Test 1 Denominator');
  F := TFraction.Create(-5, 13);
  FC := F.Convert(3);
  CheckEquals(-15, FC.Numerator, 'Test 2 Numerator');
  CheckEquals(39, FC.Denominator, 'Test 2 Denominator');
  F := 0;
  FC := F.Convert(7);
  CheckEquals(0, FC.Numerator, 'Test 3 Numerator');
  CheckEquals(7, FC.Denominator, 'Test 3 Denominator');
  F := 3;
  FC := F.Convert(3);
  CheckEquals(9, FC.Numerator, 'Test 4 Numerator');
  CheckEquals(3, FC.Denominator, 'Test 4 Denominator');
end;

procedure TestTFraction.TestDivideOp;
var
  F1, F2, FRes: TFraction;
begin
  // 2/3 / 5/6 = 2/3 * 6/5 = 12/15 = 4/5
  F1 := TFraction.Create(2, 3);
  F2 := TFraction.Create(5, 6);
  FRes := F1 / F2;
  CheckEquals(4, FRes.Numerator, 'Test 1 Numerator');
  CheckEquals(5, FRes.Denominator, 'Test 1 Denominator');
  // 5/6 / -2/3 = 5/6 * -3/2 = -15/12 = -5/4
  F1 := TFraction.Create(5, 6);
  F2 := TFraction.Create(-2, 3);
  FRes := F1 / F2;
  CheckEquals(-5, FRes.Numerator, 'Test 2 Numerator');
  CheckEquals(4, FRes.Denominator, 'Test 2 Denominator');
  // -3/7 / -3/5 = -3/7 * -5/3 = 15/21 = 5/7
  F1 := TFraction.Create(-3, 7);
  F2 := TFraction.Create(-3, 5);
  FRes := F1 / F2;
  CheckEquals(5, FRes.Numerator, 'Test 3 Numerator');
  CheckEquals(7, FRes.Denominator, 'Test 3 Denominator');
  // 1/2 / 1/2 = 1/2 * 2/1 = 2/2 = 1/1
  F1 := TFraction.Create(1, 2);
  F2 := TFraction.Create(1, 2);
  FRes := F1 / F2;
  CheckEquals(1, FRes.Numerator, 'Test 4 Numerator');
  CheckEquals(1, FRes.Denominator, 'Test 4 Denominator');
  // 5/8 / 3 = 5/8 * 1/3 = 5/24
  F1 := TFraction.Create(5, 8);
  FRes := F1 / 3;
  CheckEquals(5, FRes.Numerator, 'Test 5 Numerator');
  CheckEquals(24, FRes.Denominator, 'Test 5 Denominator');
  // 5 / 12/5 = 5 * 5/12 = 25/12
  F2 := TFraction.Create(12, 5);
  FRes := 5 / F2;
  CheckEquals(25, FRes.Numerator, 'Test 6 Numerator');
  CheckEquals(12, FRes.Denominator, 'Test 6 Denominator');
  // 1 / -2/3 = 1 * -3/2 = -3/2
  F2 := TFraction.Create(-2, 3);
  FRes := 1 / F2;
  CheckEquals(-3, FRes.Numerator, 'Test 7 Numerator');
  CheckEquals(2, FRes.Denominator, 'Test 7 Denominator');
  // 3/4 / 1 = 3/4 * 1/1 = 3/4
  F1 := TFraction.Create(3, 4);
  FRes := F1 / 1;
  CheckEquals(3, FRes.Numerator, 'Test 8 Numerator');
  CheckEquals(4, FRes.Denominator, 'Test 8 Denominator');
end;

procedure TestTFraction.TestImplicit;
var
  F: TFraction;
  E1, E2: Extended;
  D1, D2: Double;
begin
  // Integer => TFraction
  F := 12;
  CheckEquals(12, F.Numerator, 'Test 1 Numerator');
  CheckEquals(1, F.Denominator, 'Test 1 Denominator');
  F := -42;
  CheckEquals(-42, F.Numerator, 'Test 2 Numerator');
  CheckEquals(1, F.Denominator, 'Test 2 Denominator');
  F := 0;
  CheckEquals(0, F.Numerator, 'Test 3 Numerator');
  CheckEquals(1, F.Denominator, 'Test 3 Denominator');

  // TFraction => Extended
  F := TFraction.Create(5, 27);
  E1 := 5 / 27;
  E2 := F;
  D1 := 5 / 27;
  D2 := F;
  CheckEquals(EqualsValue, CompareValue(D1, D2), 'Test 4 (Double)');
  CheckEquals(EqualsValue, CompareValue(E1, E2), 'Test 4 (Extended)');

  // Extended => TFraction
  F := 1 / 3;
  CheckEquals(1, F.Numerator, 'Test 5 Numerator');
  CheckEquals(3, F.Denominator, 'Test 5 Denominator');
  F := 4 / 6;
  CheckEquals(2, F.Numerator, 'Test 6 Numerator');
  CheckEquals(3, F.Denominator, 'Test 6 Denominator');
  E1 := -200/350;
  F := E1;
  E2 := F;
  CheckEquals(-4, F.Numerator, 'Test 7 Numerator');
  CheckEquals(7, F.Denominator, 'Test 7 Denominator');
  CheckEquals(E1, E2, 'Test 7 decimal');
  F := 1.07407407;
  CheckEquals(29, F.Numerator, 'Test 6 Numerator');
  CheckEquals(27, F.Denominator, 'Test 6 Denominator');
  // Check that explicit works by implication
  F := TFraction(6);
  CheckEquals(6, F.Numerator, 'Test External 1 Numerator');
  CheckEquals(1, F.Denominator, 'Test External 1 Denominator');
end;

procedure TestTFraction.TestIntDivideOp;
var
  F1, F2: TFraction;
begin
  // 7/8 div 1/3 = Trunc(7/8 * 3/1) = Trunc(21/8) = 2
  F1 := TFraction.Create(7, 8);
  F2 := TFraction.Create(1, 3);
  CheckEquals(2, F1 div F2, 'Test 1');
  // 11/3 div -2/3 = Trunc(11/3 * -3/2) = Trunc(-33/6) = -5
  F1 := TFraction.Create(11, 3);
  F2 := TFraction.Create(-2, 3);
  CheckEquals(-5, F1 div F2, 'Test 2');
  // 2/3 div 2/3 = Trunc(2/3 * 3/2) = Trunc(6/6) = 1
  F1 := TFraction.Create(2, 3);
  F2 := TFraction.Create(2, 3);
  CheckEquals(1, F1 div F2, 'Test 3');
  // 2/3 div 3/4 = Trunc(2/3 * 4/3) = Trunc(6/9) = 0
  F1 := TFraction.Create(2, 3);
  F2 := TFraction.Create(3, 4);
  CheckEquals(0, F1 div F2, 'Test 4');
  // 32/5 div 3 = Trunc(32/5 * 1/3) = Trunc(32/15) = 2
  F1 := TFraction.Create(32, 5);
  CheckEquals(2, F1 div 3, 'Test 5');
  // 5 div 2/3 = Trunc(5/1 * 3/2) = Trunc(15/2) = 7
  F2 := TFraction.Create(2, 3);
  CheckEquals(7, 5 div F2, 'Test 6');
  // 4/3 div 2/3 = Trunc(4/3 * 3/2) = Trunc(12/6) = 2
  F1 := TFraction.Create(4, 3);
  F2 := TFraction.Create(2, 3);
  CheckEquals(2, F1 div F2, 'Test 7');
  // 10/3 div 1 = Trunc(10/3 * 1/1) = Trunc(10/3) = 3
  F1 := TFraction.Create(10, 3);
  CheckEquals(3, F1 div 1, 'Test 8');
end;

procedure TestTFraction.TestIsCommonFactor;
var
  F: TFraction;
begin
  F := TFraction.Create(32, 48);
  CheckTrue(F.IsCommonFactor(8), 'Test 1');
  CheckTrue(F.IsCommonFactor(16), 'Test 2');
  CheckTrue(F.IsCommonFactor(-2), 'Test 3');
  CheckFalse(F.IsCommonFactor(5), 'Test 4');
  CheckFalse(F.IsCommonFactor(0), 'Test 5');
end;

procedure TestTFraction.TestIsProper;
var
  F: TFraction;
begin
  F := TFraction.Create(3, 4);
  CheckTrue(F.IsProper, 'Test 1');
  F := TFraction.Create(4, 3);
  CheckFalse(F.IsProper, 'Test 2');
  F := 5;
  CheckFalse(F.IsProper, 'Test 3');
  F := 0;
  CheckTrue(F.IsProper, 'Test 4');
end;

procedure TestTFraction.TestIsWholeNumber;
var
  F: TFraction;
begin
  F := 12;
  CheckTrue(F.IsWholeNumber, 'Test 1');
  F := TFraction.Create(36, 13);
  CheckFalse(F.IsWholeNumber, 'Test 2');
  F := TFraction.Create(-5, 1);
  CheckTrue(F.IsWholeNumber, 'Test 3');
  F := TFraction.Create(1, -5);
  CheckFalse(F.IsWholeNumber, 'Test 4');
  F := 0;
  CheckTrue(F.IsWholeNumber, 'Test 5');
  F := TFraction.Create(0, 5);
  CheckTrue(F.IsWholeNumber, 'Test 6');
  F := TFraction.Create(4, 4);
  CheckTrue(F.IsWholeNumber, 'Test 7');
  F := TFraction.Create(-4, 4);
  CheckTrue(F.IsWholeNumber, 'Test 8');
  F := TFraction.Create(4, -4);
  CheckTrue(F.IsWholeNumber, 'Test 9');
  F := TFraction.Create(-4, -4);
  CheckTrue(F.IsWholeNumber, 'Test 10');
  F := TFraction.Create(4, -1);
  CheckTrue(F.IsWholeNumber, 'Test 12');
  F := TFraction.Create(75, 5);
  CheckTrue(F.IsWholeNumber, 'Test 13');
end;

procedure TestTFraction.TestLCD;
var
  F1, F2: TFraction;
begin
  F1 := TFraction.Create(1, 3);
  F2 := TFraction.Create(5, 6);
  CheckEquals(6, TFraction.LCD(F1, F2), 'Test 1');
  F1 := TFraction.Create(1, 6);
  F2 := TFraction.Create(7, 15);
  CheckEquals(30, TFraction.LCD(F1, F2), 'Test 2');
  F1 := TFraction.Create(3, 8);
  F2 := TFraction.Create(5, 12);
  CheckEquals(24, TFraction.LCD(F1, F2), 'Test 3');
  F1 := TFraction.Create(3, 8);
  F2 := TFraction.Create(-5, 12);
  CheckEquals(24, TFraction.LCD(F1, F2), 'Test 4');
  F1 := TFraction.Create(-3, 8);
  F2 := TFraction.Create(-5, 12);
  CheckEquals(24, TFraction.LCD(F1, F2), 'Test 5');
end;

procedure TestTFraction.TestMax;
var
  A: TArray<TFraction>;
begin
  A := TArray<TFraction>.Create(
    TFraction.Create(3,4),
    TFraction.Create(5,12),
    TFraction.Create(-4,3),
    1,
    TFraction.Create(23, 5)
  );
  CheckEquals(A[0], TFraction.Max(A[0], A[1]), 'Test 1');
  CheckEquals(A[1], TFraction.Max(A[2], A[1]), 'Test 2');
  CheckEquals(A[4], TFraction.Max(A[3], A[4]), 'Test 3');
  CheckEquals(A[3], TFraction.Max(A[3], A[1]), 'Test 4');
  CheckEquals(A[4], TFraction.Max(A), 'Test 5');
end;

procedure TestTFraction.TestMin;
var
  A: TArray<TFraction>;
begin
  A := TArray<TFraction>.Create(
    TFraction.Create(3,4),
    TFraction.Create(5,12),
    TFraction.Create(-4,3),
    1,
    TFraction.Create(23, 5)
  );
  CheckEquals(A[1], TFraction.Min(A[0], A[1]), 'Test 1');
  CheckEquals(A[2], TFraction.Min(A[2], A[1]), 'Test 2');
  CheckEquals(A[3], TFraction.Min(A[3], A[4]), 'Test 3');
  CheckEquals(A[1], TFraction.Min(A[3], A[1]), 'Test 4');
  CheckEquals(A[2], TFraction.Min(A), 'Test 5');
end;

procedure TestTFraction.TestModulusOp;
var
  F1, F2, FRes: TFraction;
begin
  // 7/8 mod 1/3 = 7/8 - 2 * 1/3 = 7/8 - 2/3 = 21/24-16/24 = 5/24
  F1 := TFraction.Create(7, 8);
  F2 := TFraction.Create(1, 3);
  FRes := F1 mod F2;
  CheckEquals(5, FRes.Numerator, 'Test 1 Numerator');
  CheckEquals(24, FRes.Denominator, 'Test 1 Denominator');
  // 11/3 mod -2/3 = 11/3 - -5 * -2/3 = 11/3 - 10/3 = 1/3
  F1 := TFraction.Create(11, 3);
  F2 := TFraction.Create(-2, 3);
  FRes := F1 mod F2;
  CheckEquals(1, FRes.Numerator, 'Test 2 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 2 Denominator');
  // 2/3 mod 2/3 = 0
  F1 := TFraction.Create(2, 3);
  F2 := TFraction.Create(2, 3);
  FRes := F1 mod F2;
  CheckEquals(0, FRes.Numerator, 'Test 3 Numerator');
  CheckEquals(1, FRes.Denominator, 'Test 3 Denominator');
  // 2/3 mod 3/4 = 2/3 - 0 * 3/4 = 2/3
  F1 := TFraction.Create(2, 3);
  F2 := TFraction.Create(3, 4);
  FRes := F1 mod F2;
  CheckEquals(2, FRes.Numerator, 'Test 4 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 4 Denominator');
  // 32/5 mod 3 = 32/5 - 2 * 3 = 32/5 - 6/1 = 32/5 - 30/5 = 2/5
  F1 := TFraction.Create(32, 5);
  FRes := F1 mod 3;
  CheckEquals(2, FRes.Numerator, 'Test 5 Numerator');
  CheckEquals(5, FRes.Denominator, 'Test 5 Denominator');
  // 5 mod 2/3 = 5 - 7 * 2/3 = 5/1 - 14/3 = 15/3 - 14/3 = 1/3
  F2 := TFraction.Create(2, 3);
  FRes := 5 mod F2;
  CheckEquals(1, FRes.Numerator, 'Test 6 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 6 Denominator');
  // 4/3 mod 2/3 = 4/3 - 2 * 2/3 = 4/3 - 4/3 = 0
  F1 := TFraction.Create(4, 3);
  F2 := TFraction.Create(2, 3);
  FRes := F1 mod F2;
  CheckEquals(0, FRes.Numerator, 'Test 7 Numerator');
  CheckEquals(1, FRes.Denominator, 'Test 7 Denominator');
  // 10/3 mod 1 = 1/3
  F1 := TFraction.Create(10, 3);
  FRes := F1 mod 1;
  CheckEquals(1, FRes.Numerator, 'Test 8 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 8 Denominator');
  // 10 7/8 mod 3 3/4 = 87/8 mod 15/4 = 87/8 mod 30/8 = 87/8 - 60/8 = 27/8
  F1 := TFraction.Create(87, 8);
  F2 := TFraction.Create(15, 4);
  FRes := F1 mod F2;
  CheckEquals(27, FRes.Numerator, 'Test 9 Numerator');
  CheckEquals(8, FRes.Denominator, 'Test 9 Denominator');
end;

procedure TestTFraction.TestMultiplyOp;
var
  F1, F2, FRes: TFraction;
begin
  // 3/4 * 2/3 = 6/12 = 1/2
  F1 := TFraction.Create(3, 4);
  F2 := TFraction.Create(2, 3);
  FRes := F1 * F2;
  CheckEquals(1, FRes.Numerator, 'Test 1a Numerator');
  CheckEquals(2, FRes.Denominator, 'Test 1a Denominator');
  // check for commutativity
  FRes := F2 * F1;
  CheckEquals(1, FRes.Numerator, 'Test 1b Numerator');
  CheckEquals(2, FRes.Denominator, 'Test 1b Denominator');
  // -8/7 * 9/10 = -72/70 = -36/35
  F1 := TFraction.Create(-8, 7);
  F2 := TFraction.Create(9, 10);
  FRes := F1 * F2;
  CheckEquals(-36, FRes.Numerator, 'Test 2 Numerator');
  CheckEquals(35, FRes.Denominator, 'Test 2 Denominator');
  // 2 * 5/6 = 2/1 * 5/6 = 10/6 = 5/3
  F2 := TFraction.Create(5, 6);
  FRes := 2 * F2;
  CheckEquals(5, FRes.Numerator, 'Test 3 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 3 Denominator');
  // 7/9 * -3 = 7/9 * -3/1 = -21/9 = -7/3
  F1 := TFraction.Create(7, 9);
  FRes := F1 * -3;
  CheckEquals(-7, FRes.Numerator, 'Test 4 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 4 Denominator');
end;

procedure TestTFraction.TestReciprocal;
var
  F, FR: TFraction;
begin
  F := TFraction.Create(2, 3);
  FR := F.Reciprocal;
  CheckEquals(3, FR.Numerator, 'Test 1 Numerator');
  CheckEquals(2, FR.Denominator, 'Test 1 Denominator');
  F := TFraction.Create(-6, 9);
  FR := F.Reciprocal;
  CheckEquals(-9, FR.Numerator, 'Test 2 Numerator');
  CheckEquals(6, FR.Denominator, 'Test 2 Denominator');
  F := 42;
  FR := F.Reciprocal;
  CheckEquals(1, FR.Numerator, 'Test 3 Numerator');
  CheckEquals(42, FR.Denominator, 'Test 3 Denominator');
end;

procedure TestTFraction.TestRoundOp;
var
  F: TFraction;
begin
  F := 0;
  CheckEquals(0, Round(F), 'Test 1');
  F := 3;
  CheckEquals(3, Round(F), 'Test 2');
  F := -3;
  CheckEquals(-3, Round(F), 'Test 3');

  F := TFraction.Create(1, 3);
  CheckEquals(0, Round(F), 'Test 4');
  F := TFraction.Create(-1, 3);
  CheckEquals(0, Round(F), 'Test 5');

  F := TFraction.Create(2, 3);
  CheckEquals(1, Round(F), 'Test 6');
  F := TFraction.Create(-2, 3);
  CheckEquals(-1, Round(F), 'Test 7');

  F := TFraction.Create(7, 2);
  CheckEquals(4, Round(F), 'Test 8');
  F := TFraction.Create(-7, 2);
  CheckEquals(-4, Round(F), 'Test 9');

  F := TFraction.Create(17, 3);
  CheckEquals(6, Round(F), 'Test 10');
  F := TFraction.Create(-17, 3);
  CheckEquals(-6, Round(F), 'Test 11');

  F := TFraction.Create(58, 7);
  CheckEquals(8, Round(F), 'Test 12');
  F := TFraction.Create(-58, 7);
  CheckEquals(-8, Round(F), 'Test 13');
end;

procedure TestTFraction.TestRoundToMulitiple;
var
  F1, F2, FR: TFraction;
begin
  F1 := TFraction.Create(11, 16);
  F2 := TFraction.Create(1, 8);
  FR := F1.RoundToMulitiple(F2);
  CheckEquals(6, FR.Numerator, 'Test 1 Numerator');
  CheckEquals(8, FR.Denominator, 'Test 1 Denominator');
  F1 := TFraction.Create(11, 16);
  F2 := TFraction.Create(1, 4);
  FR := F1.RoundToMulitiple(F2);
  CheckEquals(3, FR.Numerator, 'Test 2 Numerator');
  CheckEquals(4, FR.Denominator, 'Test 2 Denominator');
  F1 := TFraction.Create(13, 7);
  F2 := TFraction.Create(3, 5);
  FR := F1.RoundToMulitiple(F2);
  CheckEquals(9, FR.Numerator, 'Test 3 Numerator');
  CheckEquals(5, FR.Denominator, 'Test 3 Denominator');
  F1 := TFraction.Create(13, 7);
  F2 := TFraction.Create(7, 10);
  FR := F1.RoundToMulitiple(F2);
  CheckEquals(21, FR.Numerator, 'Test 4 Numerator');
  CheckEquals(10, FR.Denominator, 'Test 4 Denominator');
  F1 := 5;
  F2 := TFraction.Create(12, 8);
  FR := F1.RoundToMulitiple(F2);
  CheckEquals(36, FR.Numerator, 'Test 5 Numerator');
  CheckEquals(8, FR.Denominator, 'Test 5 Denominator');
  F1 := TFraction.Create(18, 4);
  F2 := 2;
  FR := F1.RoundToMulitiple(F2);
  CheckEquals(4, FR.Numerator, 'Test 6 Numerator');
  CheckEquals(1, FR.Denominator, 'Test 6 Denominator');
  F1 := TFraction.Create(36, 8);
  F2 := TFraction.Create(12, 8);
  FR := F1.RoundToMulitiple(F2);
  CheckEquals(36, FR.Numerator, 'Test 7 Numerator');
  CheckEquals(8, FR.Denominator, 'Test 7 Denominator');
  F1 := TFraction.Create(9, 8);
  F2 := TFraction.Create(3, 4);
  FR := F1.RoundToMulitiple(F2);
  CheckEquals(6, FR.Numerator, 'Test 8 Numerator');
  CheckEquals(4, FR.Denominator, 'Test 8 Denominator');
end;

procedure TestTFraction.TestSign;
var
  F: TFraction;
begin
  F := 0;
  CheckEquals(ZeroValue, F.Sign, 'Test 1');
  F := TFraction.Create(2, -3);
  CheckEquals(NegativeValue, F.Sign, 'Test 2');
  F := TFraction.Create(5, 6);
  CheckEquals(PositiveValue, F.Sign, 'Test 3');
end;

procedure TestTFraction.TestSimplify;
var
  F, G: TFraction;
begin
  // testing Simplify(CommonFactor)
  F := TFraction.Create(24, 32);
  G := F.Simplify(2);
  CheckEquals(12, G.Numerator, 'Test A1 Numerator');
  CheckEquals(16, G.Denominator, 'Test A1 Denominator');
  F := TFraction.Create(-24, 32);
  G := F.Simplify(-2);
  CheckEquals(-12, G.Numerator, 'Test A2 Numerator');
  CheckEquals(16, G.Denominator, 'Test A2 Denominator');
  F := TFraction.Create(24, 36);
  G := F.Simplify(6);
  CheckEquals(4, G.Numerator, 'Test A3 Numerator');
  CheckEquals(6, G.Denominator, 'Test A3 Denominator');
  F := TFraction.Create(24, 36);
  G := F.Simplify(1);
  CheckEquals(24, G.Numerator, 'Test A4 Numerator');
  CheckEquals(36, G.Denominator, 'Test A4 Denominator');
  F := TFraction.Create(24, 36);
  G := F.Simplify(-12);
  CheckEquals(2, G.Numerator, 'Test A5 Numerator');
  CheckEquals(3, G.Denominator, 'Test A5 Denominator');

  // testing Simplify()
  F := TFraction.Create(2, 4);
  G := F.Simplify;
  CheckEquals(1, G.Numerator, 'Test B1 Numerator');
  CheckEquals(2, G.Denominator, 'Test B1 Denominator');
  F := TFraction.Create(2, -4);
  G := F.Simplify;
  CheckEquals(-1, G.Numerator, 'Test B2 Numerator');
  CheckEquals(2, G.Denominator, 'Test B2 Denominator');
  F := TFraction.Create(12, 15);
  G := F.Simplify;
  CheckEquals(4, G.Numerator, 'Test B3 Numerator');
  CheckEquals(5, G.Denominator, 'Test B3 Denominator');
  F := TFraction.Create(11, 12);
  G := F.Simplify;
  CheckEquals(11, G.Numerator, 'Test B4 Numerator');
  CheckEquals(12, G.Denominator, 'Test B4 Denominator');
end;

procedure TestTFraction.TestSubtractOp;
var
  FRes, F1, F2: TFraction;
begin
  // 4/3 - 1/3 = 3/3 = 1
  F1 := TFraction.Create(4, 3);
  F2 := TFraction.Create(1, 3);
  FRes := F1 - F2;
  CheckEquals(1, FRes.Numerator, 'Test 1 Numerator');
  CheckEquals(1, FRes.Denominator, 'Test 1 Denominator');
  // 2/3 - 5/6 = 4/6 - 5/6 = -1/6
  F1 := TFraction.Create(2, 3);
  F2 := TFraction.Create(5, 6);
  FRes := F1 - F2;
  CheckEquals(-1, FRes.Numerator, 'Test 2 Numerator');
  CheckEquals(6, FRes.Denominator, 'Test 2 Denominator');
  // 7/15 - 2/9 = 21/45 - 10/45= 11/45
  F1 := TFraction.Create(7, 15);
  F2 := TFraction.Create(2, 9);
  FRes := F1 - F2;
  CheckEquals(11, FRes.Numerator, 'Test 3 Numerator');
  CheckEquals(45, FRes.Denominator, 'Test 3 Denominator');
  // 2/5 - 6/7 = 14/35 - 30/35 = -16/35
  F1 := TFraction.Create(2, 5);
  F2 := TFraction.Create(6, 7);
  FRes := F1 - F2;
  CheckEquals(-16, FRes.Numerator, 'Test 4 Numerator');
  CheckEquals(35, FRes.Denominator, 'Test 4 Denominator');
  // -1/3 - 1/3 = -2/3
  F1 := TFraction.Create(-1, 3);
  F2 := TFraction.Create(1, 3);
  FRes := F1 - F2;
  CheckEquals(-2, FRes.Numerator, 'Test 5 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 5 Denominator');
  // -1/3 + 2/3 = 1/3
  F1 := TFraction.Create(-1, 3);
  F2 := TFraction.Create(2, 3);
  FRes := F1 + F2;
  CheckEquals(1, FRes.Numerator, 'Test 6 Numerator');
  CheckEquals(3, FRes.Denominator, 'Test 6 Denominator');
  // 2/3 - 2/3 = 0
  F1 := TFraction.Create(2, 3);
  F2 := TFraction.Create(2, 3);
  FRes := F1 - F2;
  CheckEquals(0, FRes.Numerator, 'Test 7 Numerator');
  CheckEquals(1, FRes.Denominator, 'Test 7 Denominator');
  // -5/6 - -5/8 = -20/24 - -15/24 = -5/24
  F1 := TFraction.Create(-5, 6);
  F2 := TFraction.Create(-5, 8);
  FRes := F1 - F2;
  CheckEquals(-5, FRes.Numerator, 'Test 8 Numerator');
  CheckEquals(24, FRes.Denominator, 'Test 8 Denominator');
  // 1 - -5/6 = 6/6 - -5/6 = 11/6
  F1 := 1;
  F2 := TFraction.Create(-5, 6);
  FRes := F1 - F2;
  CheckEquals(11, FRes.Numerator, 'Test 9 Numerator');
  CheckEquals(6, FRes.Denominator, 'Test 9 Denominator');
  // 2 - 5/6 = 12/6 - 5/6 = 7/6
  F2 := TFraction.Create(5, 6);
  FRes := 2 - F2;
  CheckEquals(7, FRes.Numerator, 'Test 10 Numerator');
  CheckEquals(6, FRes.Denominator, 'Test 10 Denominator');
  // 5/13 - -3 = 5/13 - -39/13 = 44/13
  F1 := TFraction.Create(5, 13);
  FRes := F1 - -3;
  CheckEquals(44, FRes.Numerator, 'Test 11 Numerator');
  CheckEquals(13, FRes.Denominator, 'Test 11 Denominator');
end;

procedure TestTFraction.TestTruncateToMultiple;
var
  F1, F2, FT: TFraction;
begin
  F1 := TFraction.Create(11, 16);
  F2 := TFraction.Create(1, 8);
  FT := F1.TruncateToMultiple(F2);
  CheckEquals(5, FT.Numerator, 'Test 1 Numerator');
  CheckEquals(8, FT.Denominator, 'Test 1 Denominator');
  F1 := TFraction.Create(11, 16);
  F2 := TFraction.Create(1, 4);
  FT := F1.TruncateToMultiple(F2);
  CheckEquals(2, FT.Numerator, 'Test 2 Numerator');
  CheckEquals(4, FT.Denominator, 'Test 2 Denominator');
  F1 := TFraction.Create(13, 7);
  F2 := TFraction.Create(3, 5);
  FT := F1.TruncateToMultiple(F2);
  CheckEquals(9, FT.Numerator, 'Test 3 Numerator');
  CheckEquals(5, FT.Denominator, 'Test 3 Denominator');
  F1 := TFraction.Create(13, 7);
  F2 := TFraction.Create(7, 10);
  FT := F1.TruncateToMultiple(F2);
  CheckEquals(14, FT.Numerator, 'Test 4 Numerator');
  CheckEquals(10, FT.Denominator, 'Test 4 Denominator');
  F1 := 5;
  F2 := TFraction.Create(12, 8);
  FT := F1.TruncateToMultiple(F2);
  CheckEquals(36, FT.Numerator, 'Test 5 Numerator');
  CheckEquals(8, FT.Denominator, 'Test 5 Denominator');
  F1 := TFraction.Create(18, 4);
  F2 := 2;
  FT := F1.TruncateToMultiple(F2);
  CheckEquals(4, FT.Numerator, 'Test 6 Numerator');
  CheckEquals(1, FT.Denominator, 'Test 6 Denominator');
  F1 := TFraction.Create(36, 8);
  F2 := TFraction.Create(12, 8);
  FT := F1.TruncateToMultiple(F2);
  CheckEquals(36, FT.Numerator, 'Test 7 Numerator');
  CheckEquals(8, FT.Denominator, 'Test 7 Denominator');
  F1 := TFraction.Create(9, 8);
  F2 := TFraction.Create(3, 4);
  FT := F1.TruncateToMultiple(F2);
  CheckEquals(3, FT.Numerator, 'Test 8 Numerator');
  CheckEquals(4, FT.Denominator, 'Test 8 Denominator');
end;

procedure TestTFraction.TestTruncOp;
var
  F: TFraction;
begin
  F := 0;
  CheckEquals(0, Trunc(F), 'Test 1');
  F := 3;
  CheckEquals(3, Trunc(F), 'Test 2');
  F := -3;
  CheckEquals(-3, Trunc(F), 'Test 3');

  F := TFraction.Create(1, 3);
  CheckEquals(0, Trunc(F), 'Test 4');
  F := TFraction.Create(-1, 3);
  CheckEquals(0, Trunc(F), 'Test 5');

  F := TFraction.Create(2, 3);
  CheckEquals(0, Trunc(F), 'Test 6');
  F := TFraction.Create(-2, 3);
  CheckEquals(0, Trunc(F), 'Test 7');

  F := TFraction.Create(7, 2);
  CheckEquals(3, Trunc(F), 'Test 8');
  F := TFraction.Create(-7, 2);
  CheckEquals(-3, Trunc(F), 'Test 9');

  F := TFraction.Create(17, 3);
  CheckEquals(5, Trunc(F), 'Test 10');
  F := TFraction.Create(-17, 3);
  CheckEquals(-5, Trunc(F), 'Test 11');

  F := TFraction.Create(58, 7);
  CheckEquals(8, Trunc(F), 'Test 12');
  F := TFraction.Create(-58, 7);
  CheckEquals(-8, Trunc(F), 'Test 13');
end;

procedure TestTFraction.TestUnaryMinusOp;
var
  F, G: TFraction;
begin
  F := TFraction.Create(4, 5);
  G := -F;
  CheckEquals(-4, G.Numerator, 'Test 1: Numerator');
  CheckEquals(5, G.Denominator, 'Test 1: Denominator');
  F := TFraction.Create(-8, 5);
  G := -F;
  CheckEquals(8, G.Numerator, 'Test 2: Numerator');
  CheckEquals(5, G.Denominator, 'Test 2: Denominator');
  F := 42;
  G := -F;
  CheckEquals(-42, G.Numerator, 'Test 3: Numerator');
  CheckEquals(1, G.Denominator, 'Test 3: Denominator');
  F := -56;
  G := -F;
  CheckEquals(56, G.Numerator, 'Test 4: Numerator');
  CheckEquals(1, G.Denominator, 'Test 4: Denominator');
end;

procedure TestTFraction.TestUnaryPlusOp;
var
  F, G: TFraction;
begin
  F := TFraction.Create(4, 5);
  G := +F;
  CheckEquals(4, G.Numerator, 'Test 1: Numerator');
  CheckEquals(5, G.Denominator, 'Test 1: Denominator');
  F := TFraction.Create(-8, 5);
  G := +F;
  CheckEquals(-8, G.Numerator, 'Test 2: Numerator');
  CheckEquals(5, G.Denominator, 'Test 2: Denominator');
  F := 42;
  G := +F;
  CheckEquals(42, G.Numerator, 'Test 3: Numerator');
  CheckEquals(1, G.Denominator, 'Test 3: Denominator');
  F := -56;
  G := +F;
  CheckEquals(-56, G.Numerator, 'Test 4: Numerator');
  CheckEquals(1, G.Denominator, 'Test 4: Denominator');
end;

{ TestTMixedFraction }

procedure TestTMixedFraction.TestAddOp;
var
  M1, M2, MRes: TMixedFraction;
begin
  // 1/3 + 1/3 = 2/3
  M1 := TMixedFraction.Create(0, 1, 3);
  M2 := TMixedFraction.Create(0, 1, 3);
  MRes := M1 + M2;
  CheckEquals(0, MRes.WholePart, 'Test 1 Whole number');
  CheckEquals(2, MRes.FractionalPart.Numerator, 'Test 1 Numerator');
  CheckEquals(3, MRes.FractionalPart.Denominator, 'Test 1 Denominator');
  // 1 2/3 + 1 5/6 = 10/6 + 11/6 = 21/6 = 7/2 = 3 1/2
  M1 := TMixedFraction.Create(1, 2, 3);
  M2 := TMixedFraction.Create(1, 5, 6);
  MRes := M1 + M2;
  CheckEquals(3, MRes.WholePart, 'Test 2 Whole number');
  CheckEquals(1, MRes.FractionalPart.Numerator, 'Test 2 Numerator');
  CheckEquals(2, MRes.FractionalPart.Denominator, 'Test 2 Denominator');
  // 2/9 + 14/15 = 10/45 + 42/45 = 52/45 = 1 7/45
  M1 := TMixedFraction.Create(0, 2, 9);
  M2 := TMixedFraction.Create(0, 14, 15);
  MRes := M1 + M2;
  CheckEquals(1, MRes.WholePart, 'Test 3 Whole number');
  CheckEquals(7, MRes.FractionalPart.Numerator, 'Test 3 Numerator');
  CheckEquals(45, MRes.FractionalPart.Denominator, 'Test 3 Denominator');
  // -1 -1/3 + 1 1/3 = 0
  M1 := TMixedFraction.Create(-1, -1, 3);
  M2 := TMixedFraction.Create(1, 1, 3);
  MRes := M1 + M2;
  CheckEquals(0, MRes.WholePart, 'Test 4 Whole number');
  CheckEquals(0, MRes.FractionalPart.Numerator, 'Test 4 Numerator');
  CheckEquals(1, MRes.FractionalPart.Denominator, 'Test 4 Denominator');
  // -1 -1/3 + 5 2/3 = 4 1/3
  M1 := TMixedFraction.Create(-1, -1, 3);
  M2 := TMixedFraction.Create(5, 2, 3);
  MRes := M1 + M2;
  CheckEquals(4, MRes.WholePart, 'Test 5 Whole number');
  CheckEquals(1, MRes.FractionalPart.Numerator, 'Test 5 Numerator');
  CheckEquals(3, MRes.FractionalPart.Denominator, 'Test 5 Denominator');
  // -5/6 + -5/8 = -20/24 + -15/24 = -35/24 = -1 -11/24
  M1 := TMixedFraction.Create(0, -5, 6);
  M2 := TMixedFraction.Create(0, -5, 8);
  MRes := M1 + M2;
  CheckEquals(-1, MRes.WholePart, 'Test 6 Whole number');
  CheckEquals(-11, MRes.FractionalPart.Numerator, 'Test 6 Numerator');
  CheckEquals(24, MRes.FractionalPart.Denominator, 'Test 6 Denominator');
  // 2 5/13 + -3 = 31/13 + -39/13 = -8/13
  M1 := TMixedFraction.Create(2, 5, 13);
  MRes := M1 + -3;
  CheckEquals(0, MRes.WholePart, 'Test 7 Whole number');
  CheckEquals(-8, MRes.FractionalPart.Numerator, 'Test 7 Numerator');
  CheckEquals(13, MRes.FractionalPart.Denominator, 'Test 7 Denominator');
end;

procedure TestTMixedFraction.TestCompare;
var
  M1, M2, M3, M4, M5, M6, M7, M8: TMixedFraction;
  F: TFraction;
begin
  M1 := TMixedFraction.Create(1, 3, 4);
  M2 := M1;
  M3 := TMixedFraction.Create(-5, -2, 4);
  M4 := TMixedFraction.Create(1, 7, 9);
  M5 := TMixedFraction.Create(1, 12, 16);
  M6 := TMixedFraction.Create(2, 1, 12);
  M7 := 4;
  M8 := 0;
  F := TFraction.Create(16, 9);
  CheckEquals(EqualsValue, TMixedFraction.Compare(M1, M2), 'Test 1');
  CheckEquals(LessThanValue, TMixedFraction.Compare(M1, M4), 'Test 2');
  CheckEquals(GreaterThanValue, TMixedFraction.Compare(M1, M3), 'Test 3');
  CheckEquals(EqualsValue, TMixedFraction.Compare(M1, M5), 'Test 4');
  CheckEquals(LessThanValue, TMixedFraction.Compare(M5, M6), 'Test 5');
  CheckEquals(GreaterThanValue, TMixedFraction.Compare(M7, M8), 'Test 6');
  CheckEquals(LessThanValue, TMixedFraction.Compare(M3, M8), 'Test 7');
  CheckEquals(GreaterThanValue, TMixedFraction.Compare(M8, M3), 'Test 8');
  CheckEquals(EqualsValue, TMixedFraction.Compare(M4, F), 'Test 9');
  CheckEquals(GreaterThanValue, TMixedFraction.Compare(F, M1), 'Test 10');
  CheckEquals(LessThanValue, TFraction.Compare(M3, F), 'Test 11');
  CheckEquals(EqualsValue, TMixedFraction.Compare(M7, 4), 'Test 12');
  CheckEquals(EqualsValue, TMixedFraction.Compare(4, M7), 'Test 13');
  CheckEquals(LessThanValue, TMixedFraction.Compare(M3, 1.75), 'Test 14');
  CheckEquals(GreaterThanValue, TMixedFraction.Compare(1.75, M3), 'Test 15');
end;

procedure TestTMixedFraction.TestCompareTo;
var
  M1, M2, M3, M4, M5, M6, M7, M8: TMixedFraction;
  F: TFraction;
begin
  M1 := TMixedFraction.Create(1, 3, 4);
  M2 := M1;
  M3 := TMixedFraction.Create(-5, -2, 4);
  M4 := TMixedFraction.Create(1, 7, 9);
  M5 := TMixedFraction.Create(1, 12, 16);
  M6 := TMixedFraction.Create(2, 1, 12);
  M7 := 4;
  M8 := 0;
  F := TFraction.Create(16, 9);
  CheckEquals(EqualsValue, M1.CompareTo(M2), 'Test 1');
  CheckEquals(LessThanValue, M1.CompareTo(M4), 'Test 2');
  CheckEquals(GreaterThanValue, M1.CompareTo(M3), 'Test 3');
  CheckEquals(EqualsValue, M1.CompareTo(M5), 'Test 4');
  CheckEquals(LessThanValue, M5.CompareTo(M6), 'Test 5');
  CheckEquals(GreaterThanValue, M7.CompareTo(M8), 'Test 6');
  CheckEquals(LessThanValue, M3.CompareTo(M8), 'Test 7');
  CheckEquals(GreaterThanValue, M8.CompareTo(M3), 'Test 8');
  CheckEquals(EqualsValue, M4.CompareTo(F), 'Test 9');
  CheckEquals(GreaterThanValue, F.CompareTo(M1), 'Test 10');
  CheckEquals(LessThanValue, M3.CompareTo(F), 'Test 11');
  CheckEquals(EqualsValue, M7.CompareTo(4), 'Test 12');
  CheckEquals(LessThanValue, M3.CompareTo(1.75), 'Test 13');
end;

procedure TestTMixedFraction.TestComparisonOps;
var
  M1, M2: TMixedFraction;
begin
  // M1 = M2
  M1 := TMixedFraction.Create(1, 2, 3);
  M2 := TMixedFraction.Create(1, 2, 3);
  CheckTrue(M1 = M2, 'Test 1a (=)');
  CheckFalse(M1 <> M2, 'Test 1a (<>)');
  CheckFalse(M1 < M2, 'Test 1a (<)');
  CheckTrue(M1 <= M2, 'Test 1a (<=)');
  CheckFalse(M1 > M2, 'Test 1a (>)');
  CheckTrue(M1 >= M2, 'Test 1a (>=)');

  // M1 = M2
  M1 := TMixedFraction.Create(2, 6, 5);
  M2 := TMixedFraction.Create(2, -30, -25);
  CheckTrue(M1 = M2, 'Test 1d (=)');
  CheckFalse(M1 <> M2, 'Test 1d (<>)');
  CheckFalse(M1 < M2, 'Test 1d (<)');
  CheckTrue(M1 <= M2, 'Test 1d (<=)');
  CheckFalse(M1 > M2, 'Test 1d (>)');
  CheckTrue(M1 >= M2, 'Test 1d (>=)');

  // M1 > M2
  M1 := TMixedFraction.Create(1, 4, 5);
  M2 := TMixedFraction.Create(-2, 3, 5);
  CheckFalse(M1 = M2, 'Test 2a (=)');
  CheckTrue(M1 <> M2, 'Test 2a (<>)');
  CheckFalse(M1 < M2, 'Test 2a (<)');
  CheckFalse(M1 <= M2, 'Test 2a (<=)');
  CheckTrue(M1 > M2, 'Test 2a (>)');
  CheckTrue(M1 >= M2, 'Test 2a (>=)');

  // M1 < M2
  M1 := TMixedFraction.Create(-3, -1, 5);
  M2 := TMixedFraction.Create(-2, -1, 9);
  CheckFalse(M1 = M2, 'Test 3a (=)');
  CheckTrue(M1 <> M2, 'Test 3a (<>)');
  CheckTrue(M1 < M2, 'Test 3a (<)');
  CheckTrue(M1 <= M2, 'Test 3a (<=)');
  CheckFalse(M1 > M2, 'Test 3a (>)');
  CheckFalse(M1 >= M2, 'Test 3a (>=)');
end;

procedure TestTMixedFraction.TestConstructors;
var
  M: TMixedFraction;
begin
  // Test Create(const WholeNumber: Int64; const Fraction: TFraction);
  M := TMixedFraction.Create(7, TFraction.Create(50, 15));
  CheckEquals(10, M.WholePart, 'Test A1: Whole part');
  CheckEquals(5, M.FractionalPart.Numerator, 'Test A1: Numerator');
  CheckEquals(15, M.FractionalPart.Denominator, 'Test A1: Denominator');
  M := TMixedFraction.Create(7, TFraction.Create(37, -15));
  CheckEquals(4, M.WholePart, 'Test A2: Whole part');
  CheckEquals(8, M.FractionalPart.Numerator, 'Test A2: Numerator');
  CheckEquals(15, M.FractionalPart.Denominator, 'Test A2: Denominator');
  M := TMixedFraction.Create(-7, TFraction.Create(17, 15));
  CheckEquals(-5, M.WholePart, 'Test A3: Whole part');
  CheckEquals(-13, M.FractionalPart.Numerator, 'Test A3: Numerator');
  CheckEquals(15, M.FractionalPart.Denominator, 'Test A3: Denominator');
  M := TMixedFraction.Create(-7, TFraction.Create(37, -15));
  CheckEquals(-9, M.WholePart, 'Test A4: Whole part');
  CheckEquals(-7, M.FractionalPart.Numerator, 'Test A4: Numerator');
  CheckEquals(15, M.FractionalPart.Denominator, 'Test A4: Denominator');
  M := TMixedFraction.Create(0, TFraction.Create(14, 2));
  CheckEquals(7, M.WholePart, 'Test A5: Whole part');
  CheckEquals(0, M.FractionalPart.Numerator, 'Test A5: Numerator');
  CheckEquals(2, M.FractionalPart.Denominator, 'Test A5: Denominator');
  M := TMixedFraction.Create(42, TFraction.Create(12, 16).Simplify);
  CheckEquals(42, M.WholePart, 'Test A6: Whole part');
  CheckEquals(3, M.FractionalPart.Numerator, 'Test A6: Numerator');
  CheckEquals(4, M.FractionalPart.Denominator, 'Test A6: Denominator');
  M := TMixedFraction.Create(0, TFraction.Create(0, 2));
  CheckEquals(0, M.WholePart, 'Test A7: Whole part');
  CheckEquals(0, M.FractionalPart.Numerator, 'Test A7: Numerator');
  CheckEquals(2, M.FractionalPart.Denominator, 'Test A7: Denominator');
  M := TMixedFraction.Create(12, TFraction.Create(24, 6));
  CheckEquals(16, M.WholePart, 'Test A8: Whole part');
  CheckEquals(0, M.FractionalPart.Numerator, 'Test A8: Numerator');
  CheckEquals(6, M.FractionalPart.Denominator, 'Test A8: Denominator');
  M := TMixedFraction.Create(0, TFraction.Create(17, -5));
  CheckEquals(-3, M.WholePart, 'Test A9: Whole part');
  CheckEquals(-2, M.FractionalPart.Numerator, 'Test A9: Numerator');
  CheckEquals(5, M.FractionalPart.Denominator, 'Test A9: Denominator');

  // Test Create(const WholeNumber, Numerator, Denominator: Int64);
  // this calls above overload, so only minimal tests
  M := TMixedFraction.Create(7, 37, 15);
  CheckEquals(9, M.WholePart, 'Test B1: Whole part');
  CheckEquals(7, M.FractionalPart.Numerator, 'Test B1: Numerator');
  CheckEquals(15, M.FractionalPart.Denominator, 'Test B1: Denominator');
  M := TMixedFraction.Create(7, 37, -15);
  CheckEquals(4, M.WholePart, 'Test B2: Whole part');
  CheckEquals(8, M.FractionalPart.Numerator, 'Test B2: Numerator');
  CheckEquals(15, M.FractionalPart.Denominator, 'Test B2: Denominator');
end;

procedure TestTMixedFraction.TestConvert;
var
  M, MC: TMixedFraction;
begin
  M := TMixedFraction.Create(0, 1, 2);
  MC := M.Convert(6);
  CheckEquals(0, MC.WholePart, 'Test 1 Whole number');
  CheckEquals(6, MC.FractionalPart.Numerator, 'Test 1 Numerator');
  CheckEquals(12, MC.FractionalPart.Denominator, 'Test 1 Denominator');
  M := TMixedFraction.Create(-7, -5, 13);
  MC := M.Convert(3);
  CheckEquals(-7, MC.WholePart, 'Test 2 Whole number');
  CheckEquals(-15, MC.FractionalPart.Numerator, 'Test 2 Numerator');
  CheckEquals(39, MC.FractionalPart.Denominator, 'Test 2 Denominator');
  M := 0;
  MC := M.Convert(7);
  CheckEquals(0, MC.WholePart, 'Test 3 Whole number');
  CheckEquals(0, MC.FractionalPart.Numerator, 'Test 3 Numerator');
  CheckEquals(7, MC.FractionalPart.Denominator, 'Test 3 Denominator');
  M := 3;
  MC := M.Convert(3);
  CheckEquals(3, MC.WholePart, 'Test 4 Whole number');
  CheckEquals(0, MC.FractionalPart.Numerator, 'Test 4 Numerator');
  CheckEquals(3, MC.FractionalPart.Denominator, 'Test 4 Denominator');
end;

procedure TestTMixedFraction.TestDivideOp;
var
  M1, M2, MRes: TMixedFraction;
begin
  // 2 2/3 / 5/6 = 8/3 * 6/5 = 48/15 = 16/5 = 3 1/5
  M1 := TMixedFraction.Create(2, 2, 3);
  M2 := TMixedFraction.Create(0, 5, 6);
  MRes := M1 / M2;
  CheckEquals(3, MRes.WholePart, 'Test 1 Whole number');
  CheckEquals(1, MRes.FractionalPart.Numerator, 'Test 1 Numerator');
  CheckEquals(5, MRes.FractionalPart.Denominator, 'Test 1 Denominator');
  // 5/6 / -2/3 = 5/6 * -3/2 = -15/12 = -5/4 = -1 -1/4
  M1 := TMixedFraction.Create(0, 5, 6);
  M2 := TMixedFraction.Create(0, -2, 3);
  MRes := M1 / M2;
  CheckEquals(-1, MRes.WholePart, 'Test 2 Whole number');
  CheckEquals(-1, MRes.FractionalPart.Numerator, 'Test 2 Numerator');
  CheckEquals(4, MRes.FractionalPart.Denominator, 'Test 2 Denominator');
  // -3/7 / -3/5 = -3/7 * -5/3 = 15/21 = 5/7
  M1 := TMixedFraction.Create(0, -3, 7);
  M2 := TMixedFraction.Create(0, -3, 5);
  MRes := M1 / M2;
  CheckEquals(0, MRes.WholePart, 'Test 3 Whole number');
  CheckEquals(5, MRes.FractionalPart.Numerator, 'Test 3 Numerator');
  CheckEquals(7, MRes.FractionalPart.Denominator, 'Test 3 Denominator');
  // 3 1/2 / 3 1/2 = 1
  M1 := TMixedFraction.Create(3, 1, 2);
  M2 := TMixedFraction.Create(3, 1, 2);
  MRes := M1 / M2;
  CheckEquals(1, MRes.WholePart, 'Test 4 Whole number');
  CheckEquals(0, MRes.FractionalPart.Numerator, 'Test 4 Numerator');
  CheckEquals(1, MRes.FractionalPart.Denominator, 'Test 4 Denominator');
  // 10 5/8 / 4 = 85/8 * 1/4 = 85/32 = 2 21/32
  M1 := TMixedFraction.Create(10, 5, 8);
  MRes := M1 / 4;
  CheckEquals(2, MRes.WholePart, 'Test 5 Whole number');
  CheckEquals(21, MRes.FractionalPart.Numerator, 'Test 5 Numerator');
  CheckEquals(32, MRes.FractionalPart.Denominator, 'Test 5 Denominator');
  // 5 / 2 2/5 = 5 * 5/12 = 25/12 = 2 1/12
  M2 := TMixedFraction.Create(2, 2, 5);
  MRes := 5 / M2;
  CheckEquals(2, MRes.WholePart, 'Test 6 Whole number');
  CheckEquals(1, MRes.FractionalPart.Numerator, 'Test 6 Numerator');
  CheckEquals(12, MRes.FractionalPart.Denominator, 'Test 6 Denominator');
  // 1 / -1 -1/3 = 1 * -3/4 = -3/4
  M2 := TMixedFraction.Create(-1, -1, 3);
  MRes := 1 / M2;
  CheckEquals(0, MRes.WholePart, 'Test 7 Whole number');
  CheckEquals(-3, MRes.FractionalPart.Numerator, 'Test 7 Numerator');
  CheckEquals(4, MRes.FractionalPart.Denominator, 'Test 7 Denominator');
  // 12 14/20 / 1 = 12 7/10
  M1 := TMixedFraction.Create(12, 14, 20);
  MRes := M1 / 1;
  CheckEquals(12, MRes.WholePart, 'Test 8 Whole number');
  CheckEquals(7, MRes.FractionalPart.Numerator, 'Test 8 Numerator');
  CheckEquals(10, MRes.FractionalPart.Denominator, 'Test 8 Denominator');
end;

procedure TestTMixedFraction.TestImplicit;
var
  F: TFraction;
  M: TMixedFraction;
  S, Sf: Single;
  D, Df: Double;
  E, Ef: Extended;
begin
  // Test TFraction => TMixedFraction
  F := TFraction.Create(49, 15);
  M := F;
  CheckEquals(3, M.WholePart, 'Test A1: Whole');
  CheckEquals(4, M.FractionalPart.Numerator, 'Test A1: Numerator');
  CheckEquals(15, M.FractionalPart.Denominator, 'Test A1: Denominator');
  F := TFraction.Create(-25, 15);
  M := F;
  CheckEquals(-1, M.WholePart, 'Test A2: Whole');
  CheckEquals(-10, M.FractionalPart.Numerator, 'Test A2: Numerator');
  CheckEquals(15, M.FractionalPart.Denominator, 'Test A2: Denominator');
  F := TFraction.Create(1, 3);
  M := F;
  CheckEquals(0, M.WholePart, 'Test A3: Whole');
  CheckEquals(1, M.FractionalPart.Numerator, 'Test A3: Numerator');
  CheckEquals(3, M.FractionalPart.Denominator, 'Test A3: Denominator');
  F := TFraction.Create(0, -4);
  M := F;
  CheckEquals(0, M.WholePart, 'Test A4: Whole');
  CheckEquals(0, M.FractionalPart.Numerator, 'Test A4: Numerator');
  CheckEquals(4, M.FractionalPart.Denominator, 'Test A4: Denominator');

  // Test TMixedFraction => TFraction
  M := TMixedFraction.Create(1, 5, 8);
  F := M;
  CheckEquals(13, F.Numerator, 'Test B1: Numerator');
  CheckEquals(8, F.Denominator, 'Test B1: Denominator');
  M := TMixedFraction.Create(3, -5, 8);
  F := M;
  CheckEquals(19, F.Numerator, 'Test B2: Numerator');
  CheckEquals(8, F.Denominator, 'Test B2: Denominator');
  M := TMixedFraction.Create(0, -5, 20);
  F := M;
  CheckEquals(-5, F.Numerator, 'Test B3: Numerator');
  CheckEquals(20, F.Denominator, 'Test B3: Denominator');
  M := TMixedFraction.Create(0, 50, 20);
  F := M;
  CheckEquals(50, F.Numerator, 'Test B4: Numerator');
  CheckEquals(20, F.Denominator, 'Test B4: Denominator');
  M := TMixedFraction.Create(0, 4, 4);
  F := M;
  CheckEquals(4, F.Numerator, 'Test B5: Numerator');
  CheckEquals(4, F.Denominator, 'Test B5: Denominator');
  M := TMixedFraction.Create(12, 0, 2);
  F := M;
  CheckEquals(24, F.Numerator, 'Test B6: Numerator');
  CheckEquals(2, F.Denominator, 'Test B6: Denominator');

  // Test Integer => TMixedFraction
  M := 42;
  CheckEquals(42, M.WholePart, 'Test C1: Whole');
  CheckEquals(0, M.FractionalPart.Numerator, 'Test C1: Numerator');
  CheckEquals(1, M.FractionalPart.Denominator, 'Test C1: Denominator');
  M := -56;
  CheckEquals(-56, M.WholePart, 'Test C2: Whole');
  CheckEquals(0, M.FractionalPart.Numerator, 'Test C2: Numerator');
  CheckEquals(1, M.FractionalPart.Denominator, 'Test C2: Denominator');
  M := 0;
  CheckEquals(0, M.WholePart, 'Test C3: Whole');
  CheckEquals(0, M.FractionalPart.Numerator, 'Test C3: Numerator');
  CheckEquals(1, M.FractionalPart.Denominator, 'Test C3: Denominator');

  // Test Float => TMixedFraction
  S := 1/3;
  M := S;
  CheckEquals(0, M.WholePart, 'Test D1: Whole');
  CheckEquals(1, M.FractionalPart.Numerator, 'Test D1: Numerator');
  CheckEquals(3, M.FractionalPart.Denominator, 'Test D1: Denominator');
  D := -34/6;
  M := D;
  CheckEquals(-5, M.WholePart, 'Test D2: Whole');
  CheckEquals(-2, M.FractionalPart.Numerator, 'Test D2: Numerator');
  CheckEquals(3, M.FractionalPart.Denominator, 'Test D2: Denominator');
  E := 1.07407407;
  M := E;
  CheckEquals(1, M.WholePart, 'Test D3: Whole');
  CheckEquals(2, M.FractionalPart.Numerator, 'Test D3: Numerator');
  CheckEquals(27, M.FractionalPart.Denominator, 'Test D3: Denominator');
  E := -2000000/350;
  M := E;
  CheckEquals(-5714, M.WholePart, 'Test D4: Whole');
  CheckEquals(-2, M.FractionalPart.Numerator, 'Test D4: Numerator');
  CheckEquals(7, M.FractionalPart.Denominator, 'Test D4: Denominator');

  // Test TMixedFraction => Float
  M := TMixedFraction.Create(5000, 14, 27);
  E := 5000.0 + 14 / 27;
  Ef := M;
  CheckEquals(EqualsValue, CompareValue(E, Ef), 'Test E1 (Extended)');

  M := TMixedFraction.Create(0, -14, 27);
  D := -14 / 27;
  Df := M;
  CheckEquals(EqualsValue, CompareValue(D, Df), 'Test E2 (Double)');

  M := TMixedFraction.Create(-200, 4, 6);
  S := -200 + 2 / 3;
  Sf := M;
  CheckEquals(EqualsValue, CompareValue(S, Sf), 'Test E2 (Single)');
end;

procedure TestTMixedFraction.TestIntDivideOp;
var
  M1, M2: TMixedFraction;
begin
  // 7/8 div 1/3 = Trunc(7/8 * 3/1) = Trunc(21/8) = 2
  M1 := TMixedFraction.Create(0, 7, 8);
  M2 := TMixedFraction.Create(0, 1, 3);
  CheckEquals(2, M1 div M2, 'Test 1');
  // 3 2/3 div -2/3 = 11/3 div -2/3 = Trunc(11/3 * -3/2) = Trunc(-33/6) = -5
  M1 := TMixedFraction.Create(3, 2, 3);
  M2 := TMixedFraction.Create(0, -2, 3);
  CheckEquals(-5, M1 div M2, 'Test 2');
  // 5 2/3 div 5 2/3 = Trunc(17/3 * 3/17) = Trunc(51/51) = 1
  M1 := TMixedFraction.Create(5, 2, 3);
  M2 := TMixedFraction.Create(5, 2, 3);
  CheckEquals(1, M1 div M2, 'Test 3');
  // 2 2/3 div 2 3/4 = 8/3 div 11/4 = Trunc(8/3 * 4/11) = Trunc(32/11) = 0
  M1 := TMixedFraction.Create(2, 2, 3);
  M2 := TMixedFraction.Create(2, 3, 4);
  CheckEquals(0, M1 div M2, 'Test 4');
  // 6 2/5 div 3 = 32/5 div 3 = Trunc(32/5 * 1/3) = Trunc(32/15) = 2
  M1 := TMixedFraction.Create(6, 2, 5);
  CheckEquals(2, M1 div 3, 'Test 5');
  // 10 div 1 1/3 = 10 div 4/3 = Trunc(10/1 * 3/4) = Trunc(30/4) = 7
  M2 := TMixedFraction.Create(1, 1, 3);
  CheckEquals(7, 10 div M2, 'Test 6');
  // -1 -1/3 div 2/3 = -4/3 div 2/3 = Trunc(-4/3 * 3/2) = Trunc(-12/6) = -2
  M1 := TMixedFraction.Create(-1, -1, 3);
  M2 := TMixedFraction.Create(0, 2, 3);
  CheckEquals(-2, M1 div M2, 'Test 7');
  // 3 2/3 div 1 = 11/3 div 1 = Trunc(11/3 * 1/1) = Trunc(11/3) = 3
  M1 := TMixedFraction.Create(3, 2, 3);
  CheckEquals(3, M1 div 1, 'Test 8');
end;

procedure TestTMixedFraction.TestIsCommonFactor;
var
  M: TMixedFraction;
begin
  M := TMixedFraction.Create(2, 32, 48);
  CheckTrue(M.IsCommonFactor(8), 'Test 1');
  CheckTrue(M.IsCommonFactor(16), 'Test 2');
  CheckTrue(M.IsCommonFactor(-2), 'Test 3');
  CheckFalse(M.IsCommonFactor(5), 'Test 4');
  CheckFalse(M.IsCommonFactor(0), 'Test 5');
end;

procedure TestTMixedFraction.TestIsWholeNumber;
var
  M: TMixedFraction;
begin
  M := 12;
  CheckTrue(M.IsWholeNumber, 'Test 1');
  M := TMixedFraction.Create(0, 12, 13);
  CheckFalse(M.IsWholeNumber, 'Test 2');
  M := TMixedFraction.Create(-3, -5, 1);
  CheckTrue(M.IsWholeNumber, 'Test 3');
  M := TMixedFraction.Create(2, 1, -5);
  CheckFalse(M.IsWholeNumber, 'Test 4');
  M := 0;
  CheckTrue(M.IsWholeNumber, 'Test 5');
  M := TMixedFraction.Create(15, 0, 5);
  CheckTrue(M.IsWholeNumber, 'Test 6');
  M := TMixedFraction.Create(4, 4, 4);
  CheckTrue(M.IsWholeNumber, 'Test 7');
  M := TMixedFraction.Create(0, -4, 4);
  CheckTrue(M.IsWholeNumber, 'Test 8');
  M := TMixedFraction.Create(5, 4, -4);
  CheckTrue(M.IsWholeNumber, 'Test 9');
  M := TMixedFraction.Create(-12, -4, -4);
  CheckTrue(M.IsWholeNumber, 'Test 10');
  M := TMixedFraction.Create(0, 4, -1);
  CheckTrue(M.IsWholeNumber, 'Test 12');
  M := TMixedFraction.Create(12, 75, 5);
  CheckTrue(M.IsWholeNumber, 'Test 13');
end;

procedure TestTMixedFraction.TestMax;
var
  A: TArray<TMixedFraction>;
begin
  A := TArray<TMixedFraction>.Create(
    TMixedFraction.Create(1, 3, 4),
    TMixedFraction.Create(0, 5, 12),
    TMixedFraction.Create(-2, 7, 9),
    2,
    TMixedFraction.Create(4, 3, 5)
  );
  CheckEquals(A[0], TMixedFraction.Max(A[0], A[1]), 'Test 1');
  CheckEquals(A[1], TMixedFraction.Max(A[2], A[1]), 'Test 2');
  CheckEquals(A[4], TMixedFraction.Max(A[3], A[4]), 'Test 3');
  CheckEquals(A[3], TMixedFraction.Max(A[3], A[1]), 'Test 4');
  CheckEquals(A[4], TMixedFraction.Max(A), 'Test 5');
end;

procedure TestTMixedFraction.TestMin;
var
  A: TArray<TMixedFraction>;
begin
  A := TArray<TMixedFraction>.Create(
    TMixedFraction.Create(1, 3, 4),
    TMixedFraction.Create(0, 5, 12),
    TMixedFraction.Create(-2, 7, 9),
    2,
    TMixedFraction.Create(4, 3, 5)
  );
  CheckEquals(A[1], TMixedFraction.Min(A[0], A[1]), 'Test 1');
  CheckEquals(A[2], TMixedFraction.Min(A[2], A[1]), 'Test 2');
  CheckEquals(A[3], TMixedFraction.Min(A[3], A[4]), 'Test 3');
  CheckEquals(A[1], TMixedFraction.Min(A[3], A[1]), 'Test 4');
  CheckEquals(A[2], TMixedFraction.Min(A), 'Test 5');
end;

procedure TestTMixedFraction.TestModulusOp;
var
  M1, M2, MRes: TMixedFraction;
begin
  // 7/8 mod 1/3 = 7/8 - 2 * 1/3 = 7/8 - 2/3 = 21/24-16/24 = 5/24
  M1 := TMixedFraction.Create(0, 7, 8);
  M2 := TMixedFraction.Create(0, 1, 3);
  MRes := M1 mod M2;
  CheckEquals(0, MRes.WholePart, 'Test 1 Whole number');
  CheckEquals(5, MRes.FractionalPart.Numerator, 'Test 1 Numerator');
  CheckEquals(24, MRes.FractionalPart.Denominator, 'Test 1 Denominator');
  // 3 2/3 mod -2/3 = 11/3 mod -2/3 = 11/3 - -5 * -2/3 = 11/3 - 10/3 = 1/3
  M1 := TMixedFraction.Create(3, 2, 3);
  M2 := TMixedFraction.Create(0, -2, 3);
  MRes := M1 mod M2;
  CheckEquals(0, MRes.WholePart, 'Test 2 Whole number');
  CheckEquals(1, MRes.FractionalPart.Numerator, 'Test 2 Numerator');
  CheckEquals(3, MRes.FractionalPart.Denominator, 'Test 2 Denominator');
  // 5 3/5 mod 5 3/5 = 0
  M1 := TMixedFraction.Create(5, 3, 5);
  M2 := TMixedFraction.Create(5, 3, 5);
  MRes := M1 mod M2;
  CheckEquals(0, MRes.WholePart, 'Test 3 Whole number');
  CheckEquals(0, MRes.FractionalPart.Numerator, 'Test 3 Numerator');
  CheckEquals(1, MRes.FractionalPart.Denominator, 'Test 3 Denominator');
  // 5 3/5 mod 5 2/5 = 1/5
  M1 := TMixedFraction.Create(5, 3, 5);
  M2 := TMixedFraction.Create(5, 2, 5);
  MRes := M1 mod M2;
  CheckEquals(0, MRes.WholePart, 'Test 4 Whole number');
  CheckEquals(1, MRes.FractionalPart.Numerator, 'Test 4 Numerator');
  CheckEquals(5, MRes.FractionalPart.Denominator, 'Test 4 Denominator');
  // 6 2/5 mod 3 = 32/5 mod 3 = 32/5 - 2 * 3 = 32/5 - 6/1 = 32/5 - 30/5 = 2/5
  M1 := TMixedFraction.Create(6, 2, 5);
  MRes := M1 mod 3;
  CheckEquals(0, MRes.WholePart, 'Test 5 Whole number');
  CheckEquals(2, MRes.FractionalPart.Numerator, 'Test 5 Numerator');
  CheckEquals(5, MRes.FractionalPart.Denominator, 'Test 5 Denominator');
  // 5 mod 1 1/3 = 5 mod 4/3 = 5 - 3 * 4/3 = 5/1 - 12/3 = 15/3 - 12/3 = 3/3 = 1
  M2 := TMixedFraction.Create(1, 1, 3);
  MRes := 5 mod M2;
  CheckEquals(1, MRes.WholePart, 'Test 6 Whole number');
  CheckEquals(0, MRes.FractionalPart.Numerator, 'Test 6 Numerator');
  CheckEquals(1, MRes.FractionalPart.Denominator, 'Test 6 Denominator');
  // 5 1/3 mod 2 2/3 = 16/3 mod 8/3 = 16/3 - 2 * 8/3 = 16/3 - 16/3 = 0
  M1 := TMixedFraction.Create(5, 1, 3);
  M2 := TMixedFraction.Create(2, 2, 3);
  MRes := M1 mod M2;
  CheckEquals(0, MRes.WholePart, 'Test 7 Whole number');
  CheckEquals(0, MRes.FractionalPart.Numerator, 'Test 7 Numerator');
  CheckEquals(1, MRes.FractionalPart.Denominator, 'Test 7 Denominator');
  // -3 -2/5 mod 1 = -2/5
  M1 := TMixedFraction.Create(-3, -2, 5);
  MRes := M1 mod 1;
  CheckEquals(0, MRes.WholePart, 'Test 8 Whole number');
  CheckEquals(-2, MRes.FractionalPart.Numerator, 'Test 8 Numerator');
  CheckEquals(5, MRes.FractionalPart.Denominator, 'Test 8 Denominator');
  // 10 7/8 mod 3 3/4 = 87/8 mod 15/4 = 87/8 mod 30/8 = 87/8 - 60/8 = 27/8
  M1 := TMixedFraction.Create(10, 7, 8);
  M2 := TMixedFraction.Create(3, 3, 4);
  MRes := M1 mod M2;
  CheckEquals(3, MRes.WholePart, 'Test 9 Whole number');
  CheckEquals(3, MRes.FractionalPart.Numerator, 'Test 9 Numerator');
  CheckEquals(8, MRes.FractionalPart.Denominator, 'Test 9 Denominator');
end;

procedure TestTMixedFraction.TestMultiplyOp;
var
  M1, M2, MRes: TMixedFraction;
begin
  // 4 3/4 * 2 2/3 = 19/4 * 8/3 = 152/12 = 38/3 = 12 2/3
  M1 := TMixedFraction.Create(4, 3, 4);
  M2 := TMixedFraction.Create(2, 2, 3);
  MRes := M1 * M2;
  CheckEquals(12, MRes.WholePart, 'Test 1a: Whole number');
  CheckEquals(2, MRes.FractionalPart.Numerator, 'Test 1a Numerator');
  CheckEquals(3, MRes.FractionalPart.Denominator, 'Test 1a Denominator');
  // check for commutativity
  MRes := M2 * M1;
  CheckEquals(12, MRes.WholePart, 'Test 1b: Whole number');
  CheckEquals(2, MRes.FractionalPart.Numerator, 'Test 1b Numerator');
  CheckEquals(3, MRes.FractionalPart.Denominator, 'Test 1b Denominator');
  // -1 -1/7 * 9/10 = -72/70 = -36/35 = -1 -1/35
  M1 := TMixedFraction.Create(-1, -1, 7);
  M2 := TMixedFraction.Create(0, 9, 10);
  MRes := M1 * M2;
  CheckEquals(-1, MRes.WholePart, 'Test 2: Whole number');
  CheckEquals(-1, MRes.FractionalPart.Numerator, 'Test 2 Numerator');
  CheckEquals(35, MRes.FractionalPart.Denominator, 'Test 2 Denominator');
  // 8 * 1 5/6 = 8/1 * 11/6 = 88/6 = 44/3 = 14 2/3
  M2 := TMixedFraction.Create(1, 5, 6);
  MRes := 8 * M2;
  CheckEquals(14, MRes.WholePart, 'Test 3: Whole number');
  CheckEquals(2, MRes.FractionalPart.Numerator, 'Test 3 Numerator');
  CheckEquals(3, MRes.FractionalPart.Denominator, 'Test 3 Denominator');
  // 7/9 * -3 = 7/9 * -3/1 = -21/9 = -7/3 = -2 -1/3
  M1 := TMixedFraction.Create(0, 7, 9);
  MRes := M1 * -3;
  CheckEquals(-2, MRes.WholePart, 'Test 4: Whole number');
  CheckEquals(-1, MRes.FractionalPart.Numerator, 'Test 4 Numerator');
  CheckEquals(3, MRes.FractionalPart.Denominator, 'Test 4 Denominator');
end;

procedure TestTMixedFraction.TestReciprocal;
var
  M, MR: TMixedFraction;
begin
  M := TMixedFraction.Create(0, 2, 3);
  MR := M.Reciprocal;
  CheckEquals(1, MR.WholePart, 'Test 1: Whole number');
  CheckEquals(1, MR.FractionalPart.Numerator, 'Test 1 Numerator');
  CheckEquals(2, MR.FractionalPart.Denominator, 'Test 1 Denominator');
  M := TMixedFraction.Create(-2, -6, 9);
  MR := M.Reciprocal;
  CheckEquals(0, MR.WholePart, 'Test 2: Whole number');
  CheckEquals(-9, MR.FractionalPart.Numerator, 'Test 2 Numerator');
  CheckEquals(24, MR.FractionalPart.Denominator, 'Test 2 Denominator');
  M := 42;
  MR := M.Reciprocal;
  CheckEquals(0, MR.WholePart, 'Test 3: Whole number');
  CheckEquals(1, MR.FractionalPart.Numerator, 'Test 3 Numerator');
  CheckEquals(42, MR.FractionalPart.Denominator, 'Test 3 Denominator');
  M := TMixedFraction.Create(0, 3, 24);
  MR := M.Reciprocal;
  CheckEquals(8, MR.WholePart, 'Test 4: Whole number');
  CheckEquals(0, MR.FractionalPart.Numerator, 'Test 4 Numerator');
  CheckEquals(3, MR.FractionalPart.Denominator, 'Test 4 Denominator');
  M := TMixedFraction.Create(2, 3, 12);
  MR := M.Reciprocal;
  CheckEquals(0, MR.WholePart, 'Test 5: Whole number');
  CheckEquals(12, MR.FractionalPart.Numerator, 'Test 5 Numerator');
  CheckEquals(27, MR.FractionalPart.Denominator, 'Test 5 Denominator');
end;

procedure TestTMixedFraction.TestRoundOp;
var
  M: TMixedFraction;
begin
  M := 0;
  CheckEquals(0, Round(M), 'Test 1');
  M := 3;
  CheckEquals(3, Round(M), 'Test 2');
  M := -3;
  CheckEquals(-3, Round(M), 'Test 3');

  M := TMixedFraction.Create(0, 1, 3);
  CheckEquals(0, Round(M), 'Test 4');
  M := TMixedFraction.Create(0, -1, 3);
  CheckEquals(0, Round(M), 'Test 5');

  M := TMixedFraction.Create(0, 2, 3);
  CheckEquals(1, Round(M), 'Test 6');
  M := TMixedFraction.Create(0, -2, 3);
  CheckEquals(-1, Round(M), 'Test 7');

  M := TMixedFraction.Create(3, 1, 2);
  CheckEquals(4, Round(M), 'Test 8');
  M := TMixedFraction.Create(-3, -1, 2);
  CheckEquals(-4, Round(M), 'Test 9');

  M := TMixedFraction.Create(5, 2, 3);
  CheckEquals(6, Round(M), 'Test 10');
  M := TMixedFraction.Create(-5, -2, 3);
  CheckEquals(-6, Round(M), 'Test 11');

  M := TMixedFraction.Create(8, 2, 7);
  CheckEquals(8, Round(M), 'Test 12');
  M := TMixedFraction.Create(-8, -2, 7);
  CheckEquals(-8, Round(M), 'Test 13');
end;

procedure TestTMixedFraction.TestSign;
var
  M: TMixedFraction;
begin
  M := TMixedFraction.Create(3, 4, 5);
  CheckEquals(PositiveValue, M.Sign, 'Test 1');
  M := TMixedFraction.Create(-2, -4, 5);
  CheckEquals(NegativeValue, M.Sign, 'Test 2');
  M := 0;
  CheckEquals(ZeroValue, M.Sign, 'Test 3');
  M := TMixedFraction.Create(0, 1, 5);
  CheckEquals(PositiveValue, M.Sign, 'Test 4');
  M := TMixedFraction.Create(0, -1, 5);
  CheckEquals(NegativeValue, M.Sign, 'Test 5');
  M := TMixedFraction.Create(-1, 7, 5);
  CheckEquals(PositiveValue, M.Sign, 'Test 6');
  M := TMixedFraction.Create(1, 7, -5);
  CheckEquals(NegativeValue, M.Sign, 'Test 7');
end;

procedure TestTMixedFraction.TestSubtractOp;
var
  MRes, M1, M2: TMixedFraction;
begin
  // 1 1/3 - 1/3 = 1
  M1 := TMixedFraction.Create(1, 1, 3);
  M2 := TMixedFraction.Create(0, 1, 3);
  MRes := M1 - M2;
  CheckEquals(1, MRes.WholePart, 'Test 1: Whole number');
  CheckEquals(0, MRes.FractionalPart.Numerator, 'Test 1 Numerator');
  CheckEquals(1, MRes.FractionalPart.Denominator, 'Test 1 Denominator');
  // 1 2/3 - 2 1/6 = 5/3 - 13/6 = 10/6 - 13/6 = -3/6 = -1/2
  M1 := TMixedFraction.Create(1, 2, 3);
  M2 := TMixedFraction.Create(2, 1, 6);
  MRes := M1 - M2;
  CheckEquals(0, MRes.WholePart, 'Test 2: Whole number');
  CheckEquals(-1, MRes.FractionalPart.Numerator, 'Test 2 Numerator');
  CheckEquals(2, MRes.FractionalPart.Denominator, 'Test 2 Denominator');
  // 2 7/15 - 1 2/9 = 37/15 - 11/9 = 111/45 - 55/45 = 56/45 = 1 11/45
  M1 := TMixedFraction.Create(2, 7, 15);
  M2 := TMixedFraction.Create(1, 2, 9);
  MRes := M1 - M2;
  CheckEquals(1, MRes.WholePart, 'Test 3: Whole number');
  CheckEquals(11, MRes.FractionalPart.Numerator, 'Test 3 Numerator');
  CheckEquals(45, MRes.FractionalPart.Denominator, 'Test 3 Denominator');
  // 2/5 - 2 6/7 = 14/35 - 100/35 = --86/35 = -2 -16/35
  M1 := TMixedFraction.Create(0, 2, 5);
  M2 := TMixedFraction.Create(2, 6, 7);
  MRes := M1 - M2;
  CheckEquals(-2, MRes.WholePart, 'Test 4: Whole number');
  CheckEquals(-16, MRes.FractionalPart.Numerator, 'Test 4 Numerator');
  CheckEquals(35, MRes.FractionalPart.Denominator, 'Test 4 Denominator');
  // -2/3 - 4/6 = -4/6 - 4/6 = -8/6 = -4/3 = -1 -1/3
  M1 := TMixedFraction.Create(0, -2, 3);
  M2 := TMixedFraction.Create(0, 4, 6);
  MRes := M1 - M2;
  CheckEquals(-1, MRes.WholePart, 'Test 5: Whole number');
  CheckEquals(-1, MRes.FractionalPart.Numerator, 'Test 5 Numerator');
  CheckEquals(3, MRes.FractionalPart.Denominator, 'Test 5 Denominator');
  // 3 2/3 - 3 2/3 = 0
  M1 := TMixedFraction.Create(3, 2, 3);
  M2 := TMixedFraction.Create(3, 2, 3);
  MRes := M1 - M2;
  CheckEquals(0, MRes.WholePart, 'Test 6: Whole number');
  CheckEquals(0, MRes.FractionalPart.Numerator, 'Test 6 Numerator');
  CheckEquals(1, MRes.FractionalPart.Denominator, 'Test 6 Denominator');
  // -5/6 - -3 -5/8 = -5/6 - -29/8 = -20/24 - -87/24 = 67/24 = 2 19/24
  M1 := TMixedFraction.Create(0, -5, 6);
  M2 := TMixedFraction.Create(-3, -5, 8);
  MRes := M1 - M2;
  CheckEquals(2, MRes.WholePart, 'Test 7: Whole number');
  CheckEquals(19, MRes.FractionalPart.Numerator, 'Test 7 Numerator');
  CheckEquals(24, MRes.FractionalPart.Denominator, 'Test 7 Denominator');
  // 1 - -5/6 = 6/6 - -5/6 = 11/6 = 1 5/6
  M2 := TMixedFraction.Create(0, -5, 6);
  MRes := 1 - M2;
  CheckEquals(1, MRes.WholePart, 'Test 8: Whole number');
  CheckEquals(5, MRes.FractionalPart.Numerator, 'Test 8 Numerator');
  CheckEquals(6, MRes.FractionalPart.Denominator, 'Test 8 Denominator');
  // 2 - 5/6 = 12/6 - 5/6 = 7/6 = 1 1/6
  M2 := TMixedFraction.Create(0, 5, 6);
  MRes := 2 - M2;
  CheckEquals(1, MRes.WholePart, 'Test 9: Whole number');
  CheckEquals(1, MRes.FractionalPart.Numerator, 'Test 9 Numerator');
  CheckEquals(6, MRes.FractionalPart.Denominator, 'Test 9 Denominator');
  // 5/13 - -3 = 5/13 - -39/13 = 44/13 = 3 5/13
  M1 := TMixedFraction.Create(0, 5, 13);
  MRes := M1 - -3;
  CheckEquals(3, MRes.WholePart, 'Test 10: Whole number');
  CheckEquals(5, MRes.FractionalPart.Numerator, 'Test 10 Numerator');
  CheckEquals(13, MRes.FractionalPart.Denominator, 'Test 10 Denominator');
end;

procedure TestTMixedFraction.TestTruncOp;
var
  M: TMixedFraction;
begin
  M := 0;
  CheckEquals(0, Trunc(M), 'Test 1');
  M := 3;
  CheckEquals(3, Trunc(M), 'Test 2');
  M := -3;
  CheckEquals(-3, Trunc(M), 'Test 3');

  M := TMixedFraction.Create(0, 1, 3);
  CheckEquals(0, Trunc(M), 'Test 4');
  M := TMixedFraction.Create(0, -1, 3);
  CheckEquals(0, Trunc(M), 'Test 5');

  M := TMixedFraction.Create(0, 2, 3);
  CheckEquals(0, Trunc(M), 'Test 6');
  M := TMixedFraction.Create(0, -2, 3);
  CheckEquals(0, Trunc(M), 'Test 7');

  M := TMixedFraction.Create(3, 1, 2);
  CheckEquals(3, Trunc(M), 'Test 8');
  M := TMixedFraction.Create(-3, -1, 2);
  CheckEquals(-3, Trunc(M), 'Test 9');

  M := TMixedFraction.Create(5, 2, 3);
  CheckEquals(5, Trunc(M), 'Test 10');
  M := TMixedFraction.Create(-5, -2, 3);
  CheckEquals(-5, Trunc(M), 'Test 11');

  M := TMixedFraction.Create(8, 2, 7);
  CheckEquals(8, Trunc(M), 'Test 12');
  M := TMixedFraction.Create(-8, -2, 7);
  CheckEquals(-8, Trunc(M), 'Test 13');
end;

procedure TestTMixedFraction.TestUnaryMinusOp;
var
  F, G: TMixedFraction;
begin
  F := TMixedFraction.Create(7, 4, 5);
  G := -F;
  CheckEquals(-7, G.WholePart, 'Test 1: Whole number');
  CheckEquals(-4, G.FractionalPart.Numerator, 'Test 1: Numerator');
  CheckEquals(5, G.FractionalPart.Denominator, 'Test 1: Denominator');
  F := TMixedFraction.Create(-2, -3, 5);
  G := -F;
  CheckEquals(2, G.WholePart, 'Test 2: Whole number');
  CheckEquals(3, G.FractionalPart.Numerator, 'Test 2: Numerator');
  CheckEquals(5, G.FractionalPart.Denominator, 'Test 2: Denominator');
  F := 42;
  G := -F;
  CheckEquals(-42, G.WholePart, 'Test 3: Whole number');
  CheckEquals(0, G.FractionalPart.Numerator, 'Test 3: Numerator');
  CheckEquals(1, G.FractionalPart.Denominator, 'Test 3: Denominator');
  F := -56;
  G := -F;
  CheckEquals(56, G.WholePart, 'Test 4: Whole number');
  CheckEquals(0, G.FractionalPart.Numerator, 'Test 4: Numerator');
  CheckEquals(1, G.FractionalPart.Denominator, 'Test 4: Denominator');
end;

procedure TestTMixedFraction.TestUnaryPlusOp;
var
  F, G: TMixedFraction;
begin
  F := TMixedFraction.Create(7, 4, 5);
  G := +F;
  CheckEquals(7, G.WholePart, 'Test 1: Whole number');
  CheckEquals(4, G.FractionalPart.Numerator, 'Test 1: Numerator');
  CheckEquals(5, G.FractionalPart.Denominator, 'Test 1: Denominator');
  F := TMixedFraction.Create(-2, -3, 5);
  G := +F;
  CheckEquals(-2, G.WholePart, 'Test 2: Whole number');
  CheckEquals(-3, G.FractionalPart.Numerator, 'Test 2: Numerator');
  CheckEquals(5, G.FractionalPart.Denominator, 'Test 2: Denominator');
  F := 42;
  G := +F;
  CheckEquals(42, G.WholePart, 'Test 3: Whole number');
  CheckEquals(0, G.FractionalPart.Numerator, 'Test 3: Numerator');
  CheckEquals(1, G.FractionalPart.Denominator, 'Test 3: Denominator');
  F := -56;
  G := +F;
  CheckEquals(-56, G.WholePart, 'Test 4: Whole number');
  CheckEquals(0, G.FractionalPart.Numerator, 'Test 4: Numerator');
  CheckEquals(1, G.FractionalPart.Denominator, 'Test 4: Denominator');
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTFraction.Suite);
RegisterTest(TestTMixedFraction.Suite);

end.

