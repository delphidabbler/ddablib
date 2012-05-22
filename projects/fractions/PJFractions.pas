{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines types that encapsulate fractions and operations on them.
 *
 * Acknowledgements:
 *   o The information on fractions in the Mathematics Help Facility at
 *     http://www.themathleague.com/ was useful in writing this code.
 *   o The GCD and LCM routines were taken from a UseNet post by Hans van
 *     Kruijssen: see http://www.efg2.com/Lab/Library/UseNet/2000/0315b.txt.
 *   o The DecimalToFraction was adapted from the Turbo Pascal code presented in
 *     "Algorithm To Convert A Decimal To A Fraction" by John Kennedy,
 *     Mathematics Department, Santa Monica College. See
 *     http://homepage.smc.edu/kennedy_john/DEC2FRAC.PDF
}


{$RANGECHECKS ON}

unit PJFractions;

interface

uses
  Types, Math;

type
  ///  <summary>Encapsulates a vulgar fraction its various mathematical
  ///  operations.</summary>
  TFraction = record
  strict private
    var
      ///  <summary>Value of Numerator property.</summary>
      fNumerator: Int64;
      ///  <summary>Value of Denominator property.</summary>
      fDenominator: Int64;
    ///  <summary>Returns the 1st cross product of two fractions.</summary>
    class function FirstCrossProduct(const F1, F2: TFraction): Int64; static;
      inline;
    ///  <summary>Returns the 2nd cross product of two fractions.</summary>
    class function SecondCrossProduct(const F1, F2: TFraction): Int64; static;
      inline;
    ///  <summary>Returns a floating point value that is used to calculate
    ///  the given number of decimal places of accuracy to use when converting
    ///  floating point values to fractions.</summary>
    class function DecimalConversionAccuracy(const Places: Byte): Extended;
      static; inline;
  public
    ///  <summary>Constructs a fraction with given numerator and denominator.
    ///  </summary>
    ///  <remarks>
    ///  <para>Denominator must not be zero.</para>
    ///  <para>If denominator is negative then it is made positive and sign of
    ///  numerator is changed.</para>
    ///  </remarks>
    constructor Create(const Numerator, Denominator: Int64);

    ///  <summary>This fraction's numerator.</summary>
    ///  <remarks>Can be positive, negative or zero.</remarks>
    property Numerator: Int64 read fNumerator;

    ///  <summary>This fraction's denominator.</summary>
    ///  <remarks>Always positive.</remarks>
    property Denominator: Int64 read fDenominator;

    ///  <summary>Checks if this fraction is a proper fraction.</summary>
    ///  <remarks>A proper fraction is one where the absolute value of the
    ///  numerator is less than the denominator.</remarks>
    function IsProper: Boolean;

    ///  <summary>Checks if this fraction represents a whole number.</summary>
    function IsWholeNumber: Boolean;

    ///  <summary>Returns a value representing the sign of this fraction.
    ///  </summary>
    ///  <remarks>Returns ZeroValue if the fraction is zero, PositiveValue if it
    ///  is positive and Negative value if negative.</remarks>
    function Sign: TValueSign;

    ///  <summary>Compares this fraction to given fraction F and returns a value
    ///  representing their relationship.</summary>
    ///  <remarks>Returns EqualsValue if the two fractions are equivalent,
    ///  GreaterThanValue if this fraction is greater that F or LessThanValue if
    ///  this fraction is less than F.</remarks>
    function CompareTo(const F: TFraction): TValueRelationship;

    ///  <summary>Converts this fraction to an equivalent fraction whose
    ///  numerator and denominator are multiples of those of this fraction.
    ///  </summary>
    ///  <param name="Multiplier">Int64 [in] Factor by which to multiply
    ///  numerator and denominator.</param>
    ///  <returns>TFraction. Converted fraction.</returns>
    function Convert(const Multiplier: Int64): TFraction;

    ///  <summary>Checks if a given non-zero value is a common factor of this
    ///  fraction.</summary>
    function IsCommonFactor(const Factor: Int64): Boolean;

    ///  <summary>Reduces this fraction to its lowest terms.</summary>
    function Simplify: TFraction; overload;

    ///  <summary>Reduces this fraction by a given common factor.</summary>
    ///  <remarks>CommonFactor must be a valid common factor of this fraction
    ///  and must not be zero.</remarks>
    function Simplify(const CommonFactor: Int64): TFraction; overload;

    ///  <summary>Returns reciprocal of this fraction.</summary>
    function Reciprocal: TFraction;

    ///  <summary>Truncates this fraction to a whole number multiple of given
    ///  fraction F.</summary>
    ///  <remarks>Result has same denominator as F.</remarks>
    function TruncateToMultiple(const F: TFraction): TFraction;

    ///  <summary>Rounds fraction to nearest whole number multiple of given
    ///  fraction F.</summary>
    ///  <remarks>Result has same denominator as F.</remarks>
    function RoundToMulitiple(const F: TFraction): TFraction;

    ///  <summary>Returns the least common denominator of two fractions.
    ///  </summary>
    class function LCD(const F1, F2: TFraction): Int64; static;

    ///  <summary>Compares two fractions and returns a value indicating their
    ///  relationship.</summary>
    ///  <remarks>Possible return values are: EqualsValue when F1 equals F2,
    ///  GreaterThanValue when F1 is greater than F2 and LessThanValue when F1
    ///  is less than F2.</remarks>
    class function Compare(const F1, F2: TFraction): TValueRelationship; static;

    ///  <summary>Returns the greatest of two given fractions.</summary>
    class function Max(const F1, F2: TFraction): TFraction; overload; static;

    ///  <summary>Returns the greatest fraction from a given array of fractions.
    ///  </summary>
    ///  <remarks>FA must contain at least one fraction.</remarks>
    class function Max(const FA: array of TFraction): TFraction; overload;
      static;

    ///  <summary>Returns the smallest of two given fractions.</summary>
    class function Min(const F1, F2: TFraction): TFraction; overload; static;

    ///  <summary>Returns the smallest fraction from a given array of fractions.
    ///  </summary>
    ///  <remarks>FA must contain at least one fraction.</remarks>
    class function Min(const FA: array of TFraction): TFraction; overload;
      static;

    ///  <summary>Enables assignment of an integer to a fraction.</summary>
    ///  <remarks>Resulting fraction will have numerator=I and denominator=1.
    ///  </remarks>
    class operator Implicit(const I: Integer): TFraction;

    ///  <summary>Enables assignment of a fraction to a floating point value.
    ///  </summary>
    class operator Implicit(const F: TFraction): Extended;

    ///  <summary>Enables assignment of a floating point number to a fraction.
    ///  </summary>
    ///  <remarks>
    ///  <para>Absolute value of floating point number must be in range
    ///  1.0E-19 to 1.0E+19, or be 0.</para>
    ///  <para>Conversions are accurate to within five decimal places.</para>
    ///  </remarks>
    class operator Implicit(const E: Extended): TFraction;

    ///  <summary>Enables two fractions to be tested for equality using the
    ///  equals operator.</summary>
    ///  <remarks>Two fractions are equal if they are the same when reduced to
    ///  common terms.</remarks>
    class operator Equal(const F1, F2: TFraction): Boolean;

    ///  <summary>Enables two fractions to be tested for inequality using the
    ///  not-equals operator.</summary>
    class operator NotEqual(const F1, F2: TFraction): Boolean;

    ///  <summary>Enables the less-than operator to be used to compare two
    ///  fractions.</summary>
    class operator LessThan(const F1, F2: TFraction): Boolean;

    ///  <summary>Enables the less-than-or-equals operator to be used to compare
    ///  two fractions.</summary>
    class operator LessThanOrEqual(const F1, F2: TFraction): Boolean;

    ///  <summary>Enables the greater-than operator to be used to compare two
    ///  fractions.</summary>
    class operator GreaterThan(const F1, F2: TFraction): Boolean;

    ///  <summary>Enables the greater-than-or-equals operator to be used to
    ///  compare two fractions.</summary>
    class operator GreaterThanOrEqual(const F1, F2: TFraction): Boolean;

    ///  <summary>Enables the unary minus operator to negate a
    ///  fraction.</summary>
    class operator Negative(const F: TFraction): TFraction;

    ///  <summary>Enables unary plus operator to be used with a fraction.
    ///  </summary>
    ///  <remarks>This is a no-op.</remarks>
    class operator Positive(const F: TFraction): TFraction;

    ///  <summary>Overload of Trunc() operator that truncate a fraction to the
    ///  largest whole number less than or equal to the fraction.</summary>
    class operator Trunc(const F: TFraction): Int64;

    ///  <summary>Overload of Round() operator that rounds a fraction to the
    ///  nearest whole number value.</summary>
    class operator Round(const F: TFraction): Int64;

    ///  <summary>Enables addition operator to be used with fractions.</summary>
    class operator Add(const F1, F2: TFraction): TFraction;

    ///  <summary>Enables subtraction operator to be used with
    ///  fractions.</summary>
    class operator Subtract(const F1, F2: TFraction): TFraction;

    ///  <summary>Enables multiplication operator to be used with
    ///  fractions.</summary>
    class operator Multiply(const F1, F2: TFraction): TFraction;

    ///  <summary>Enables division operator to be used with fractions.</summary>
    class operator Divide(const F1, F2: TFraction): TFraction;

    ///  <summary>Overload of div operator that divides left hand operand by
    ///  right hand operand and returns the largest whole number less than or
    ///  equal to the result of the division.</summary>
    class operator IntDivide(const F1, F2: TFraction): Int64;

    ///  <summary>Overload of Mod operator that returns the fractional remainder
    ///  after dividing the left hand operand by the right hand operand.
    ///  </summary>
    class operator Modulus(const F1, F2: TFraction): TFraction;
  end;

type
  TMixedFraction = record
  strict private
    var
      ///  <summary>Records mixed fraction as a vulgar fractions.</summary>
      fFraction: TFraction;
    ///  <summary>Read accessor FractionalPart property.</summary>
    function GetFractionalPart: TFraction; inline;
    ///  <summary>Read accessor for WholePart property.</summary>
    function GetWholePart: Int64; inline;
  public
    ///  <summary>Constructs a mixed fraction with given whole number and
    ///  fractional part.</summary>
    ///  <remarks>
    ///  <para>Fractional part is added to whole number part which has the
    ///  following implications:</para>
    ///  <para>To create negative mixed fractions, both whole number part and
    ///  fractional part should be negative otherwise the result may not be as
    ///  expected.</para>
    ///  <para>Fractional part may be a vulgar fraction in which case whole
    ///  number part is adjusted.</para>
    ///  </remarks>
    constructor Create(const WholeNumber: Int64; const Fraction: TFraction);
      overload;

    ///  <summary>Constructs a mixed fraction with given whole number and a
    ///  fractional part composed from given numerator and denominator.
    ///  </summary>
    ///  <remarks>
    ///  <para>Denominator must not be negative.</para>
    ///  <para>To create a negative mixed fractions, both whole number part and
    ///  either numerator or denominator should be negative otherwise the result
    ///  may not be as expected.</para>
    ///  <para>If numerator is greater than denominator the whole number part is
    ///  adjusted so that fractional part is a proper fraction.</para>
    ///  </remarks>
    constructor Create(const WholeNumber, Numerator, Denominator: Int64);
      overload;

    ///  <summary>This fraction's whole number part.</summary>
    property WholePart: Int64 read GetWholePart;

    ///  <summary>This fraction's fractional part.</summary>
    ///  <remarks>This is always a proper fraction.</remarks>
    property FractionalPart: TFraction read GetFractionalPart;

    ///  <summary>Checks if this fraction represents a whole number.</summary>
    function IsWholeNumber: Boolean;

    ///  <summary>Returns a value representing the sign of this fraction.
    ///  </summary>
    ///  <remarks>Returns ZeroValue if the fraction is zero, PositiveValue if it
    ///  is positive and Negative value if negative.</remarks>
    function Sign: TValueSign;

    ///  <summary>Enables conversion and assignment of a TFraction to a mixed
    ///  fraction.</summary>
    class operator Implicit(const Fraction: TFraction): TMixedFraction;
    ///  <summary>Enables conversion and assignment of a mixed fraction to a
    ///  TFraction.</summary>
    class operator Implicit(const Mixed: TMixedFraction): TFraction;
    ///  <summary>Enables conversion and assignment of an integer to a mixed
    ///  fraction.</summary>
    ///  <remarks>Resulting fraction will have whole number part equal to I,
    ///  denominator of 1 and numerator of 0.</remarks>
    class operator Implicit(const I: Integer): TMixedFraction;
    ///  <summary>Enables conversion and assignment of floating point types to a
    ///  mixed fraction.</summary>
    ///  <remarks>
    ///  <para>Absolute value of floating point number must be in range
    ///  1.0E-19 to 1.0E+19, or be 0.</para>
    ///  <para>Conversions are accurate to within five decimal places.</para>
    ///  </remarks>
    class operator Implicit(const E: Extended): TMixedFraction;
    ///  <summary>Enables conversion and assignment of a mixed fraction to a
    ///  floating point value.</summary>
    class operator Implicit(const Mixed: TMixedFraction): Extended;
  end;

implementation

uses
  SysUtils;

///  <summary>Calculates the greatest common divisor of two given integers.
///  </summary>
function GCD(A, B: Int64): Int64;
var
  Temp: Int64;
begin
  while B <> 0 do
  begin
    Temp := B;
    B := A mod Temp;
    A := Temp;
  end;
  Result := A;
end;

///  <summary>Calculates the least common multiple of two integers.</summary>
function LCM(A, B: Int64): Int64;
begin
  Result := (A * B) div GCD(A, B);
end;

///  <summary>Converts a decimal to a fraction.</summary>
///  <param name="Decimal">Extended [in] Decimal to convert.</param>
///  <param name="FractionNumerator">Extended [out] Set to numerator of
///  required fraction.</param>
///  <param name="FractionDenominator">Extended [out] Set to denominator of
///  required fraction.</param>
///  <param name="AccuracyFactor">Extended [in] Determines how accurate
///  conversion is to be. E.g. 0.0005 requires accuracy of 3 decimal places and
///  0.000005 requires accuracy of 5 decimal places.</param>
procedure DecimalToFraction (Decimal: Extended;
  out FractionNumerator: Extended;
  out FractionDenominator: Extended;
  const AccuracyFactor: Extended);
var
  DecimalSign: Extended;
  Z: Extended;
  PreviousDenominator: Extended;
  ScratchValue: Extended;
resourcestring
  sTooSmall = 'Decimal too small to convert to fraction';
  sTooLarge = 'Decimal too large to convert to fraction';
const
  LargestDecimal: Extended = 1.0E+19;
  SmallestDecimal: Extended = 1.0E-19;
begin
  if Decimal < 0.0 then
    DecimalSign := -1.0
  else
    DecimalSign := 1.0;
  Decimal := Abs(Decimal);
  if SameValue(Decimal, Int(Decimal)) then
  begin
    FractionNumerator := Decimal * DecimalSign;
    FractionDenominator := 1.0;
    Exit;
  end;
  if (Decimal < SmallestDecimal) then // X = 0 already taken care of
    raise EConvertError.Create(sTooSmall);
  if (Decimal > LargestDecimal) then
    raise EConvertError.Create(sTooLarge);
  Z := Decimal;
  PreviousDenominator := 0.0;
  FractionDenominator := 1.0;
  repeat
    Z := 1.0 / (Z - Int(Z));
    ScratchValue := FractionDenominator;
    FractionDenominator := FractionDenominator * Int(Z) + PreviousDenominator;
    PreviousDenominator := ScratchValue;
    FractionNumerator := Int(Decimal * FractionDenominator + 0.5) // Rounding
  until
    (
      Abs(
        Decimal - (FractionNumerator / FractionDenominator)
      ) < AccuracyFactor
    )
    or (Z = Int(Z));
  FractionNumerator := DecimalSign * FractionNumerator;
end;

{ TFraction }

class operator TFraction.Add(const F1, F2: TFraction): TFraction;
var
  CommonDenom: Int64;
  F1S, F2S: TFraction;
  Numerator1, Numerator2: Integer;
begin
  // simply addends to reduce size of multiplication results if possible
  F1S := F1.Simplify;
  F2S := F2.Simplify;
  CommonDenom := LCM(F1S.Denominator, F2S.Denominator);
  Numerator1 := F1S.Numerator * (CommonDenom div F1S.Denominator);
  Numerator2 := F2S.Numerator * (CommonDenom div F2S.Denominator);
  Result := TFraction.Create(Numerator1 + Numerator2, CommonDenom).Simplify;
end;

class function TFraction.Compare(const F1, F2: TFraction): TValueRelationship;
var
  X1, X2: Int64;  // 1st & 2nd cross products of F1 & F2
begin
  if F1.fDenominator = F2.fDenominator then
  begin
    if F1.Numerator > F2.Numerator then
      Result := GreaterThanValue
    else if F1.Numerator < F2.Numerator then
      Result := LessThanValue
    else
      Result := EqualsValue;
  end
  else
  begin
    X1 := FirstCrossProduct(F1, F2);
    X2 := SecondCrossProduct(F1, F2);
    if X1 > X2 then
      Result := GreaterThanValue
    else if X1 < X2 then
      Result := LessThanValue
    else
      Result := EqualsValue;
  end;
end;

function TFraction.CompareTo(const F: TFraction): TValueRelationship;
begin
  Result := Compare(Self, F);
end;

function TFraction.Convert(const Multiplier: Int64): TFraction;
begin
  Assert(Multiplier > 0, 'TFraction.Convert: Multiplier <= 0');
  Result := TFraction.Create(Multiplier * Numerator, Multiplier * Denominator);
end;

constructor TFraction.Create(const Numerator, Denominator: Int64);
begin
  Assert(Denominator <> 0, 'TFraction.Create: Denominator is 0');
  fNumerator := Numerator;
  fDenominator := Denominator;
  if fDenominator < 0 then
  begin
    fNumerator := -fNumerator;
    fDenominator := -fDenominator;
  end;
end;

class function TFraction.DecimalConversionAccuracy(
  const Places: Byte): Extended;
begin
  Result := 0.5 * IntPower(10, -Places);
end;

class operator TFraction.Divide(const F1, F2: TFraction): TFraction;
begin
  Result := F1 * F2.Reciprocal;
end;

class operator TFraction.Equal(const F1, F2: TFraction): Boolean;
begin
  Result := Compare(F1, F2) = EqualsValue;
end;

class function TFraction.FirstCrossProduct(const F1, F2: TFraction): Int64;
begin
  Result := F1.Numerator * F2.Denominator;
end;

class operator TFraction.GreaterThan(const F1, F2: TFraction): Boolean;
begin
  Result := Compare(F1, F2) = GreaterThanValue;
end;

class operator TFraction.GreaterThanOrEqual(const F1, F2: TFraction): Boolean;
var
  CompareResult: TValueRelationship;
begin
  CompareResult := Compare(F1, F2);
  Result := CompareResult <> LessThanValue;
end;

class operator TFraction.Implicit(const I: Integer): TFraction;
begin
  Result := TFraction.Create(I, 1);
end;

class operator TFraction.Implicit(const F: TFraction): Extended;
begin
  Result := F.Numerator / F.Denominator;
end;

class operator TFraction.Implicit(const E: Extended): TFraction;
resourcestring
  sCantConvert = 'Can''t convert %f to fraction: out of range';
const
  LargestNumerator: Extended = High(Int64) - 1.0;
  LargestDenominator: Extended = High(Int64) - 1.0;
  DecimalPlaces: Byte = 5; // 5 deicmal places of accuracy in conversion
var
  FNumerator: Extended;     // numerator as decimal
  FDenominator: Extended;   // numerator as decimal
begin
  DecimalToFraction(
    E, FNumerator, FDenominator, DecimalConversionAccuracy(DecimalPlaces)
  );
  if (Abs(FNumerator) >= LargestNumerator)
    or (Abs(FDenominator) >= LargestDenominator) then
    raise EConvertError.Create(sCantConvert);
  Result := TFraction.Create(Round(FNumerator), Round(FDenominator)).Simplify;
end;

class operator TFraction.IntDivide(const F1, F2: TFraction): Int64;
var
  F: TFraction;
begin
  F := F1 / F2;
  Result := Trunc(F);
end;

function TFraction.IsCommonFactor(const Factor: Int64): Boolean;
begin
  Assert(Factor <> 0, 'TFraction.IsCommonFactor: Factor can''t be 0');
  Result := (Numerator mod Factor = 0) and (Denominator mod Factor = 0);
end;

function TFraction.IsProper: Boolean;
begin
  Result := fNumerator < fDenominator;
end;

function TFraction.IsWholeNumber: Boolean;
begin
  Result := fNumerator mod fDenominator = 0;
end;

class function TFraction.LCD(const F1, F2: TFraction): Int64;
begin
  Result := LCM(F1.Denominator, F2.Denominator);
end;

class operator TFraction.LessThan(const F1, F2: TFraction): Boolean;
begin
  Result := Compare(F1, F2) = LessThanValue;
end;

class operator TFraction.LessThanOrEqual(const F1, F2: TFraction): Boolean;
var
  CompareResult: TValueRelationship;
begin
  CompareResult := Compare(F1, F2);
  Result := CompareResult <> GreaterThanValue;
end;

class function TFraction.Max(const F1, F2: TFraction): TFraction;
begin
  if TFraction.Compare(F1, F2) = GreaterThanValue then
    Result := F1
  else
    Result := F2;
end;

class function TFraction.Max(const FA: array of TFraction): TFraction;
var
  Idx: Integer;
begin
  Assert(Length(FA) > 0, 'TFraction.Max: FA is empty array');
  Result := FA[0];
  for Idx := 1 to Pred(Length(FA)) do
    Result := Max(Result, FA[Idx]);
end;

class function TFraction.Min(const F1, F2: TFraction): TFraction;
begin
  if TFraction.Compare(F1, F2) = LessThanValue then
    Result := F1
  else
    Result := F2;
end;

class function TFraction.Min(const FA: array of TFraction): TFraction;
var
  Idx: Integer;
begin
  Assert(Length(FA) > 0, 'TFraction.Min: FA is empty array');
  Result := FA[0];
  for Idx := 1 to Pred(Length(FA)) do
    Result := Min(Result, FA[Idx]);
end;

class operator TFraction.Modulus(const F1, F2: TFraction): TFraction;
var
  D: Int64;
begin
  D := F1 div F2;
  Result := F1 - D * F2;  // this is simplified by - operator
end;

class operator TFraction.Multiply(const F1, F2: TFraction): TFraction;
begin
  Result := TFraction.Create(
    F1.Numerator * F2.Numerator, F1.Denominator * F2.Denominator
  ).Simplify;
end;

class operator TFraction.Negative(const F: TFraction): TFraction;
begin
  Assert(F.Denominator <> 0, 'TFraction.Negative: F.Denominator is 0');
  Result := TFraction.Create(-F.fNumerator, F.Denominator);
end;

class operator TFraction.NotEqual(const F1, F2: TFraction): Boolean;
begin
  Result := Compare(F1, F2) <> EqualsValue;
end;

class operator TFraction.Positive(const F: TFraction): TFraction;
begin
  Result := F;
end;

function TFraction.Reciprocal: TFraction;
begin
  Assert(Numerator <> 0, 'TFraction.Reciprocal: Fraction is 0');
  // swap denomitator and numerator
  Result := TFraction.Create(Denominator, Numerator);
end;

class operator TFraction.Round(const F: TFraction): Int64;
begin
  Result := Round(F.Numerator / F.Denominator);
end;

function TFraction.RoundToMulitiple(const F: TFraction): TFraction;
var
  FLo, FHi: TFraction;
  MultLo: Int64;
  FDiffLo, FDiffHi: TFraction;
begin
  MultLo := Self div F;
  FLo := MultLo * F;
  FHi := FLo + F;
  FDiffLo := Self - FLo;
  FDiffHi := FHi - Self;
  if FDiffLo < FDiffHi then
    Result := FLo.Convert(F.Denominator div FLo.Denominator)
  else
    Result := FHi.Convert(F.Denominator div FHi.Denominator);
end;

class function TFraction.SecondCrossProduct(const F1, F2: TFraction): Int64;
begin
  Result := F2.Numerator * F1.Denominator;
end;

function TFraction.Sign: TValueSign;
begin
  Assert(Denominator > 0, 'TFraction.Sign: Denominator is -ve');
  if Numerator < 0 then
    Result := NegativeValue
  else if Self > 0 then
    Result := PositiveValue
  else
    Result := ZeroValue;
end;

function TFraction.Simplify(const CommonFactor: Int64): TFraction;
begin
  Assert(CommonFactor <> 0, 'TFraction.Simplify: CommonFactor = 0');
  Assert(IsCommonFactor(CommonFactor),
    'TFraction.Simplify: CommonFactor is not a common factor of this fraction');
  Result := TFraction.Create(
    Numerator div Abs(CommonFactor), Denominator div Abs(CommonFactor)
  );
end;

function TFraction.Simplify: TFraction;
begin
  Result := Simplify(GCD(Numerator, Denominator));
end;

class operator TFraction.Subtract(const F1, F2: TFraction): TFraction;
begin
  Result := F1 + -F2;
end;

class operator TFraction.Trunc(const F: TFraction): Int64;
begin
  Result := F.Numerator div F.Denominator;
end;

function TFraction.TruncateToMultiple(const F: TFraction): TFraction;
var
  SimplifiedResult: TFraction;
begin
  SimplifiedResult := (Self div F) * F;
  Result := SimplifiedResult.Convert(
    F.Denominator div SimplifiedResult.Denominator
  );
end;

{ TMixedFraction }

constructor TMixedFraction.Create(const WholeNumber: Int64;
  const Fraction: TFraction);
begin
  // Don't use + operator to add WholeNumber to Fraction because that simplifies
  // result and we want to preserve denominator as given. If simplified fraction
  // is required, call Simplify method on Fraction in constructor call:
  // TMixedFraction.Create(42, TFraction.Create(12, 16).Simplify);
  fFraction := TFraction.Create(
    WholeNumber * Fraction.Denominator + Fraction.Numerator,
    Fraction.Denominator
  );
end;

constructor TMixedFraction.Create(const WholeNumber, Numerator,
  Denominator: Int64);
begin
  Create(WholeNumber, TFraction.Create(Numerator, Denominator));
end;

function TMixedFraction.GetFractionalPart: TFraction;
begin
  Result := TFraction.Create(
    fFraction.Numerator mod fFraction.Denominator, fFraction.Denominator
  );
end;

function TMixedFraction.GetWholePart: Int64;
begin
  Result := Trunc(fFraction);
end;

class operator TMixedFraction.Implicit(const I: Integer): TMixedFraction;
begin
  Result.fFraction := I;      // uses implicit cast of Integer to TFraction
end;

class operator TMixedFraction.Implicit(const E: Extended): TMixedFraction;
begin
  Result.fFraction := E;      // uses implicit cast of Extended to TFraction
end;

class operator TMixedFraction.Implicit(const Mixed: TMixedFraction): Extended;
begin
  Result := Mixed.fFraction;  // uses implicit cast of TFraction to Extended
end;

function TMixedFraction.IsWholeNumber: Boolean;
begin
  Result := fFraction.IsWholeNumber;
end;

function TMixedFraction.Sign: TValueSign;
begin
  Result := fFraction.Sign;
end;

class operator TMixedFraction.Implicit(const Fraction: TFraction):
  TMixedFraction;
begin
  Result.fFraction := Fraction;
end;

class operator TMixedFraction.Implicit(const Mixed: TMixedFraction): TFraction;
begin
  Result := Mixed.fFraction;
end;

end.

