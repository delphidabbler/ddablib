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
 *   o The DecimalToFraction routine was adapted from the Turbo Pascal code
 *     presented in "Algorithm To Convert A Decimal To A Fraction" by John
 *     Kennedy, Mathematics Department, Santa Monica College. See
 *     http://homepage.smc.edu/kennedy_john/DEC2FRAC.PDF
}


{$RANGECHECKS ON}


unit DelphiDabbler.Lib.Fractions;


// Delphi 2009 or later is required to compile
{$UNDEF CANCOMPILE}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 20.0}
    {$DEFINE CANCOMPILE}
  {$IFEND}
{$ENDIF}
{$IFNDEF CANCOMPILE}
  {$MESSAGE FATAL 'Delphi 2009 or later required'}
{$ENDIF}


interface


uses
  // RTL / VCL units
  Types, Math;


type
  ///  <summary>Encapsulates a vulgar fraction and mathematical operations on
  ///  it.</summary>
  TFraction = record
  strict private
    var
      ///  <summary>Value of Numerator property.</summary>
      fNumerator: Int64;
      ///  <summary>Value of Denominator property.</summary>
      fDenominator: Int64;
    ///  <summary>Returns the 1st cross product of two fractions.</summary>
    class function FirstCrossProduct(const A, B: TFraction): Int64; static;
      inline;
    ///  <summary>Returns the 2nd cross product of two fractions.</summary>
    class function SecondCrossProduct(const A, B: TFraction): Int64; static;
      inline;
    ///  <summary>Read accessor for WholeNumberPart property.</summary>
    function GetWholeNumberPart: Int64;
    ///  <summary>Read accessor for FractionalPart property.</summary>
    function GetFractionalPart: TFraction;
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

    ///  <summary>The whole number part of this fraction when viewed as a mixed
    ///  fraction.</summary>
    property WholeNumberPart: Int64 read GetWholeNumberPart;

    ///  <summary>The fractional part of this fraction when viewed as a mixed
    ///  fraction.</summary>
    ///  <remarks>This fraction is always proper.</remarks>
    property FractionalPart: TFraction read GetFractionalPart;

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
    function HasCommonFactor(const Factor: Int64): Boolean;

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
    function RoundToMultiple(const F: TFraction): TFraction;

    ///  <summary>Returns the least common denominator of two fractions.
    ///  </summary>
    class function LCD(const A, B: TFraction): Int64; static;

    ///  <summary>Compares two fractions and returns a value indicating their
    ///  relationship.</summary>
    ///  <remarks>Possible return values are: EqualsValue when A equals B,
    ///  GreaterThanValue when A is greater than B and LessThanValue when A
    ///  is less than B.</remarks>
    class function Compare(const A, B: TFraction): TValueRelationship; static;

    ///  <summary>Returns the greatest of two given fractions.</summary>
    class function Max(const A, B: TFraction): TFraction; overload; static;

    ///  <summary>Returns the greatest fraction from a given array of fractions.
    ///  </summary>
    ///  <remarks>FA must contain at least one fraction.</remarks>
    class function Max(const FA: array of TFraction): TFraction; overload;
      static;

    ///  <summary>Returns the smallest of two given fractions.</summary>
    class function Min(const A, B: TFraction): TFraction; overload; static;

    ///  <summary>Returns the smallest fraction from a given array of fractions.
    ///  </summary>
    ///  <remarks>FA must contain at least one fraction.</remarks>
    class function Min(const FA: array of TFraction): TFraction; overload;
      static;

    ///  <summary>Returns the given fraction raised to the given power.
    ///  </summary>
    class function Power(const F: TFraction; Exponent: ShortInt): TFraction;
      static;

    ///  <summary>Returns the absoulte value of the given fraction.</summary>
    class function Abs(const F: TFraction): TFraction; static;

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
    class operator Equal(const A, B: TFraction): Boolean;

    ///  <summary>Enables two fractions to be tested for inequality using the
    ///  not-equals operator.</summary>
    class operator NotEqual(const A, B: TFraction): Boolean;

    ///  <summary>Enables the less-than operator to be used to compare two
    ///  fractions.</summary>
    class operator LessThan(const A, B: TFraction): Boolean;

    ///  <summary>Enables the less-than-or-equals operator to be used to compare
    ///  two fractions.</summary>
    class operator LessThanOrEqual(const A, B: TFraction): Boolean;

    ///  <summary>Enables the greater-than operator to be used to compare two
    ///  fractions.</summary>
    class operator GreaterThan(const A, B: TFraction): Boolean;

    ///  <summary>Enables the greater-than-or-equals operator to be used to
    ///  compare two fractions.</summary>
    class operator GreaterThanOrEqual(const A, B: TFraction): Boolean;

    ///  <summary>Enables the unary minus operator to negate a fraction.
    ///  </summary>
    class operator Negative(const F: TFraction): TFraction;

    ///  <summary>Enables the unary plus operator to be used with a fraction.
    ///  </summary>
    ///  <remarks>This is a no-op.</remarks>
    class operator Positive(const F: TFraction): TFraction;

    ///  <summary>Overload of Trunc() operator that truncate a fraction to the
    ///  nearest whole number in the direction of zero.</summary>
    class operator Trunc(const F: TFraction): Int64;

    ///  <summary>Overload of Round() operator that rounds a fraction to the
    ///  nearest whole number value.</summary>
    class operator Round(const F: TFraction): Int64;

    ///  <summary>Enables addition operator to be used with fractions.</summary>
    class operator Add(const A, B: TFraction): TFraction;

    ///  <summary>Enables subtraction operator to be used with fractions.
    ///  </summary>
    class operator Subtract(const A, B: TFraction): TFraction;

    ///  <summary>Enables multiplication operator to be used with fractions.
    ///  </summary>
    class operator Multiply(const A, B: TFraction): TFraction;

    ///  <summary>Enables division operator to be used with fractions.</summary>
    class operator Divide(const A, B: TFraction): TFraction;

    ///  <summary>Overload of div operator that divides left hand operand by
    ///  right hand operand and returns the largest whole number less than or
    ///  equal to the result of the division.</summary>
    class operator IntDivide(const A, B: TFraction): Int64;

    ///  <summary>Overload of Mod operator that returns the fractional remainder
    ///  after dividing the left hand operand by the right hand operand.
    ///  </summary>
    class operator Modulus(const A, B: TFraction): TFraction;
  end;


implementation


uses
  // RTL / VCL units
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
///  <param name="PlacesOfAccuracy">Byte [in] Specifies number of decimal places
///  conversion is to be accurate to.</param>
procedure DecimalToFraction(Decimal: Extended; out FractionNumerator: Extended;
  out FractionDenominator: Extended; const PlacesOfAccuracy: Byte);
var
  DecimalSign: Extended;
  Z: Extended;
  PreviousDenominator: Extended;
  ScratchValue: Extended;
  AccuracyFactor: Extended;
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
  AccuracyFactor := 0.5 * IntPower(10, -PlacesOfAccuracy);
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

///  <summary>Raises integer Base to non-negative integer power Exponent.
///  </summary>
function Pow(const Base: Int64; const Exponent: Byte): Int64;
var
  I: Byte;
begin
  Result := 1;
  for I := 1 to Exponent do
    Result := Result * Base;
end;

{ TFraction }

class function TFraction.Abs(const F: TFraction): TFraction;
begin
  Result := TFraction.Create(System.Abs(F.Numerator), F.Denominator);
end;

class operator TFraction.Add(const A, B: TFraction): TFraction;
var
  CommonDenom: Int64;
  AR, BR: TFraction;
  Numerator1, Numerator2: Integer;
begin
  // simplify addends to reduce size of multiplication results if possible
  AR := A.Simplify;
  BR := B.Simplify;
  CommonDenom := LCM(AR.Denominator, BR.Denominator);
  Numerator1 := AR.Numerator * (CommonDenom div AR.Denominator);
  Numerator2 := BR.Numerator * (CommonDenom div BR.Denominator);
  Result := TFraction.Create(Numerator1 + Numerator2, CommonDenom).Simplify;
end;

class function TFraction.Compare(const A, B: TFraction): TValueRelationship;
var
  X1, X2: Int64;  // 1st & 2nd cross products of A & B
begin
  if A.fDenominator = B.fDenominator then
  begin
    if A.Numerator > B.Numerator then
      Result := GreaterThanValue
    else if A.Numerator < B.Numerator then
      Result := LessThanValue
    else
      Result := EqualsValue;
  end
  else
  begin
    X1 := FirstCrossProduct(A, B);
    X2 := SecondCrossProduct(A, B);
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

class operator TFraction.Divide(const A, B: TFraction): TFraction;
begin
  Result := A * B.Reciprocal;
end;

class operator TFraction.Equal(const A, B: TFraction): Boolean;
begin
  Result := Compare(A, B) = EqualsValue;
end;

class function TFraction.FirstCrossProduct(const A, B: TFraction): Int64;
begin
  Result := A.Numerator * B.Denominator;
end;

function TFraction.GetFractionalPart: TFraction;
begin
  Result := TFraction.Create(fNumerator mod fDenominator, fDenominator);
end;

function TFraction.GetWholeNumberPart: Int64;
begin
  Result := fNumerator div fDenominator;
end;

class operator TFraction.GreaterThan(const A, B: TFraction): Boolean;
begin
  Result := Compare(A, B) = GreaterThanValue;
end;

class operator TFraction.GreaterThanOrEqual(const A, B: TFraction): Boolean;
begin
  Result := Compare(A, B) <> LessThanValue;
end;

function TFraction.HasCommonFactor(const Factor: Int64): Boolean;
begin
  if Factor = 0 then
    Exit(False);
  Result := (Numerator mod Factor = 0) and (Denominator mod Factor = 0);
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
  DecimalPlaces: Byte = 5; // 5 decimal places of accuracy in conversion
var
  FNumerator: Extended;     // numerator as decimal
  FDenominator: Extended;   // numerator as decimal
begin
  DecimalToFraction(E, FNumerator, FDenominator, DecimalPlaces);
  if (System.Abs(FNumerator) >= LargestNumerator)
    or (System.Abs(FDenominator) >= LargestDenominator) then
    raise EConvertError.Create(sCantConvert);
  Result := TFraction.Create(Round(FNumerator), Round(FDenominator)).Simplify;
end;

class operator TFraction.IntDivide(const A, B: TFraction): Int64;
var
  F: TFraction;
begin
  F := A / B;
  Result := Trunc(F);
end;

function TFraction.IsProper: Boolean;
begin
  Result := fNumerator < fDenominator;
end;

function TFraction.IsWholeNumber: Boolean;
begin
  Result := fNumerator mod fDenominator = 0;
end;

class function TFraction.LCD(const A, B: TFraction): Int64;
begin
  Result := LCM(A.Denominator, B.Denominator);
end;

class operator TFraction.LessThan(const A, B: TFraction): Boolean;
begin
  Result := Compare(A, B) = LessThanValue;
end;

class operator TFraction.LessThanOrEqual(const A, B: TFraction): Boolean;
begin
  Result := Compare(A, B) <> GreaterThanValue;
end;

class function TFraction.Max(const A, B: TFraction): TFraction;
begin
  if TFraction.Compare(A, B) = GreaterThanValue then
    Result := A
  else
    Result := B;
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

class function TFraction.Min(const A, B: TFraction): TFraction;
begin
  if TFraction.Compare(A, B) = LessThanValue then
    Result := A
  else
    Result := B;
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

class operator TFraction.Modulus(const A, B: TFraction): TFraction;
var
  D: Int64;
begin
  D := A div B;
  Result := A - D * B;  // this is simplified by - operator
end;

class operator TFraction.Multiply(const A, B: TFraction): TFraction;
begin
  Result := TFraction.Create(
    A.Numerator * B.Numerator, A.Denominator * B.Denominator
  ).Simplify;
end;

class operator TFraction.Negative(const F: TFraction): TFraction;
begin
  Result := TFraction.Create(-F.fNumerator, F.Denominator);
end;

class operator TFraction.NotEqual(const A, B: TFraction): Boolean;
begin
  Result := Compare(A, B) <> EqualsValue;
end;

class operator TFraction.Positive(const F: TFraction): TFraction;
begin
  Result := F;
end;

class function TFraction.Power(const F: TFraction; Exponent: ShortInt):
  TFraction;
var
  X: TFraction;
begin
  Assert((Exponent >= 0) or (F.Numerator <> 0),
    'TFraction.Power: Numerator = 0 and Exponent < 0');
  // simplify before we start to work with smallest possible numbers
  X := F.Simplify;
  // F ^ Power where Power < 0 is equivalent to 1/F ^ -Power
  if Exponent < 0 then
  begin
    Exponent := -Exponent;
    X := X.Reciprocal;
  end;
  Result := TFraction.Create(
    Pow(X.Numerator, Exponent), Pow(X.Denominator, Exponent)
  );
end;

function TFraction.Reciprocal: TFraction;
begin
  Assert(Numerator <> 0, 'TFraction.Reciprocal: Fraction is 0');
  // swap denominator and numerator
  Result := TFraction.Create(Denominator, Numerator);
end;

class operator TFraction.Round(const F: TFraction): Int64;
begin
  Result := Round(F.Numerator / F.Denominator);
end;

function TFraction.RoundToMultiple(const F: TFraction): TFraction;
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

class function TFraction.SecondCrossProduct(const A, B: TFraction): Int64;
begin
  Result := B.Numerator * A.Denominator;
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
  Assert(HasCommonFactor(CommonFactor),
    'TFraction.Simplify: CommonFactor is not a common factor of this fraction');
  Result := TFraction.Create(
    Numerator div System.Abs(CommonFactor),
    Denominator div System.Abs(CommonFactor)
  );
end;

function TFraction.Simplify: TFraction;
begin
  Result := Simplify(GCD(Numerator, Denominator));
end;

class operator TFraction.Subtract(const A, B: TFraction): TFraction;
begin
  Result := A + -B;
end;

class operator TFraction.Trunc(const F: TFraction): Int64;
begin
  Result := F.WholeNumberPart;
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

end.

