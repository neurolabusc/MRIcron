{ **********************************************************************
  *                            Unit FMATH.PAS                          *
  *                              Version 2.8                           *
  *                       (c) J. Debord, July 2003                     *
  **********************************************************************
     This unit implements some mathematical functions in Turbo Pascal
  **********************************************************************
  Notes:

  1) The default real type is DOUBLE (8-byte real)
     Other types may be selected by defining the symbols:

     --------------------------------
     Symbol        Type
     --------------------------------
     SINGLEREAL    Single   ( 4-byte)
     PASCALREAL    Real     ( 6-byte)
     EXTENDEDREAL  Extended (10-byte)
     --------------------------------

  2) Error handling: The function MathError returns the error code from
     the last function evaluation. It must be checked immediately after
     a function call:

       Y := f(X);        (* f is one of the functions of the library *)
       if MathError = FN_OK then ...

     The possible error codes, and the default values attributed to the
     function, are the following:

     ------------------------------------------------------------------
     Error code   Value  Significance            Function default value
     ------------------------------------------------------------------
     FN_OK          0    No error
     FN_DOMAIN     -1    Argument domain error   0
     FN_SING       -2    Function singularity    +/- MAXNUM
     FN_OVERFLOW   -3    Overflow range error    MAXNUM
     FN_UNDERFLOW  -4    Underflow range error   0
     ------------------------------------------------------------------

     where MAXNUM is a constant defining the highest number which may be
     represented within the chosen floating point type.

     The standard functions Exp and Ln have been redefined according to
     the above conventions as Expo and Log.

  3) Assembler functions: some functions are written in assembler.
     These functions may be selected by defining the symbol CPU387

     Once you have selected these functions you have two possibilities:

     * Call the Pascal functions (e.g. Expo, ArcSin...). This will
       provide some acceleration while keeping the error handling.

     * Call the assembler functions directly (e.g. fExp, fArcSin...)
       This will provide further acceleration but without error handling.
       Thus it is the responsibility of the calling program to check the
       arguments passed to the function. See the interface file MATH387.INT
       for a list of available functions.

  ********************************************************************** }

unit fmath;

interface

{ ----------------------------------------------------------------------
  Floating point type (Default = Double)
  ---------------------------------------------------------------------- }

{$IFDEF PASCALREAL}
  type Float = Real;
{$ELSE}
{$IFDEF SINGLEREAL}
  type Float = Single;
{$ELSE}
{$IFDEF EXTENDEDREAL}
  type Float = Extended;
{$ELSE}
  {$DEFINE DOUBLEREAL}
  type Float = Double;
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ ----------------------------------------------------------------------
  Mathematical constants
  ---------------------------------------------------------------------- }

const
  PI         = 3.14159265358979323846;  { Pi }
  LN2        = 0.69314718055994530942;  { Ln(2) }
  LN10       = 2.30258509299404568402;  { Ln(10) }
  LNPI       = 1.14472988584940017414;  { Ln(Pi) }
  INVLN2     = 1.44269504088896340736;  { 1/Ln(2) }
  INVLN10    = 0.43429448190325182765;  { 1/Ln(10) }
  TWOPI      = 6.28318530717958647693;  { 2*Pi }
  PIDIV2     = 1.57079632679489661923;  { Pi/2 }
  SQRTPI     = 1.77245385090551602730;  { Sqrt(Pi) }
  SQRT2PI    = 2.50662827463100050242;  { Sqrt(2*Pi) }
  INVSQRT2PI = 0.39894228040143267794;  { 1/Sqrt(2*Pi) }
  LNSQRT2PI  = 0.91893853320467274178;  { Ln(Sqrt(2*Pi)) }
  LN2PIDIV2  = 0.91893853320467274178;  { Ln(2*Pi)/2 }
  SQRT2      = 1.41421356237309504880;  { Sqrt(2) }
  SQRT2DIV2  = 0.70710678118654752440;  { Sqrt(2)/2 }
  GOLD       = 1.61803398874989484821;  { Golden Mean = (1 + Sqrt(5))/2 }
  CGOLD      = 0.38196601125010515179;  { 2 - GOLD }

{ ----------------------------------------------------------------------
  Machine-dependent constants
  ---------------------------------------------------------------------- }

{$IFDEF SINGLEREAL}
const
  MACHEP = 1.192093E-7;               { Floating point precision: 2^(-23) }
  MAXNUM = 3.402823E+38;              { Max. floating point number: 2^128 }
  MINNUM = 1.175495E-38;              { Min. floating point number: 2^(-126) }
  MAXLOG = 88.72283;                  { Max. argument for Exp = Ln(MAXNUM) }
  MINLOG = -87.33655;                 { Min. argument for Exp = Ln(MINNUM) }
  MAXFAC = 33;                        { Max. argument for Factorial }
  MAXGAM = 34.648;                    { Max. argument for Gamma }
  MAXLGM = 1.0383E+36;                { Max. argument for LnGamma }
{$ELSE}
{$IFDEF DOUBLEREAL}
const
  MACHEP = 2.220446049250313E-16;     { 2^(-52) }
  MAXNUM = 1.797693134862315E+308;    { 2^1024 }
  MINNUM = 2.225073858507202E-308;    { 2^(-1022) }
  MAXLOG = 709.7827128933840;
  MINLOG = -708.3964185322641;
  MAXFAC = 170;
  MAXGAM = 171.624376956302;
  MAXLGM = 2.556348E+305;
{$ELSE}
{$IFDEF EXTENDEDREAL}
const
  MACHEP = 1.08420217248550444E-19;   { 2^(-63) }
  MAXNUM = 1.18973149535723103E+4932; { 2^16384 }
  MINNUM = 3.36210314311209558E-4932; { 2^(-16382) }
  MAXLOG = 11356.5234062941439;
  MINLOG = - 11355.137111933024;
  MAXFAC = 1754;
  MAXGAM = 1755.455;
  MAXLGM = 1.04848146839019521E+4928;
{$ELSE}
{$IFDEF PASCALREAL}
const
  MACHEP = 1.818989404E-12;           { 2^(-39) }
  MAXNUM = 4.253529586E+37;           { 2^126 }
  MINNUM = 2.350988703E-38;           { 2^(-125) }
  MAXLOG = 8.664339757E+01;
  MINLOG = - 4.253529586E+01;
  MAXFAC = 33;
  MAXGAM = 34.64809785;
  MAXLGM = 1.038324114E+36;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ ----------------------------------------------------------------------
  Error codes for mathematical functions
  ---------------------------------------------------------------------- }
  
const
  FN_OK        =   0;  { No error }
  FN_DOMAIN    = - 1;  { Argument domain error }
  FN_SING      = - 2;  { Function singularity }
  FN_OVERFLOW  = - 3;  { Overflow range error }
  FN_UNDERFLOW = - 4;  { Underflow range error }
  FN_TLOSS     = - 5;  { Total loss of precision }
  FN_PLOSS     = - 6;  { Partial loss of precision }

{ ----------------------------------------------------------------------
  Global variables and constants
  ---------------------------------------------------------------------- }

const
  NFACT = 33;  { The factorials of the first NFACT integers are stored
                 in a table }
var
  MathErr : Integer;  { Error code from the latest function evaluation }

  FactArray : array[0..NFACT] of Float;  { Table of factorials }

{ ----------------------------------------------------------------------
  Functional type
  ---------------------------------------------------------------------- }

type
  TFunc = function(X : Float) : Float;

{ ----------------------------------------------------------------------
  Error handling function
  ---------------------------------------------------------------------- }

function MathError : Integer;  { Error code from the last function call }

{ ----------------------------------------------------------------------
  Minimum, maximum, sign and exchange
  ---------------------------------------------------------------------- }

function FMin(X, Y : Float) : Float;      { Minimum of 2 reals }
function FMax(X, Y : Float) : Float;      { Maximum of 2 reals }
function IMin(X, Y : Integer) : Integer;  { Minimum of 2 integers }
function IMax(X, Y : Integer) : Integer;  { Maximum of 2 integers }
function Sgn(X : Float) : Integer;        { Sign (returns 1 if X = 0) }
function Sgn0(X : Float) : Integer;       { Sign (returns 0 if X = 0) }
function DSgn(A, B : Float) : Float;      { Sgn(B) * |A| }

procedure FSwap(var X, Y : Float);        { Exchange 2 reals }
procedure ISwap(var X, Y : Integer);      { Exchange 2 integers }

{ ----------------------------------------------------------------------
  Assembler functions
  ---------------------------------------------------------------------- }

{$IFDEF CPU387}
  {$I math387.int}
{$ENDIF}

{ ----------------------------------------------------------------------
  Sign, logarithms, exponentials and power
  ---------------------------------------------------------------------- }

function Expo(X : Float) : Float;                   { Exponential }
function Exp2(X : Float) : Float;                   { 2^X }
function Exp10(X : Float) : Float;                  { 10^X }
function Log(X : Float) : Float;                    { Natural log }
function Log2(X : Float) : Float;                   { Log, base 2 }
function Log10(X : Float) : Float;                  { Decimal log }
function LogA(X, A : Float) : Float;                { Log, base A }
function IntPower(X : Float; N : Integer) : Float;  { X^N }
function Power(X, Y : Float) : Float;               { X^Y, X >= 0 }
function Pythag(X, Y : Float) : Float;              { Sqrt(X^2 + Y^2) }

{ ----------------------------------------------------------------------
  Trigonometric and inverse trigonometric functions
  ---------------------------------------------------------------------- }

function FixAngle(Theta : Float) : Float;  { Set Theta in -Pi..Pi }
function Tan(X : Float) : Float;           { Tangent }
function ArcSin(X : Float) : Float;        { Arc sinus }
function ArcCos(X : Float) : Float;        { Arc cosinus }
function ArcTan2(Y, X : Float) : Float;    { Angle (Ox, OM) with M(X,Y) }

procedure SinCos(X : Float; var SinX, CosX : Float);  { Sin & Cos }

{ ----------------------------------------------------------------------
  Hyperbolic and inverse hyperbolic functions
  ---------------------------------------------------------------------- }

function Sinh(X : Float) : Float;     { Hyperbolic sine }
function Cosh(X : Float) : Float;     { Hyperbolic cosine }
function Tanh(X : Float) : Float;     { Hyperbolic tangent }
function ArcSinh(X : Float) : Float;  { Inverse hyperbolic sine }
function ArcCosh(X : Float) : Float;  { Inverse hyperbolic cosine }
function ArcTanh(X : Float) : Float;  { Inverse hyperbolic tangent }

procedure SinhCosh(X : Float; var SinhX, CoshX : Float);  { Sinh & Cosh }

{ ----------------------------------------------------------------------
  Special functions
  ---------------------------------------------------------------------- }

function Fact(N : Integer) : Float;         { Factorial }
function Binomial(N, K : Integer) : Float;  { Binomial coef. C(N,K) }
function Gamma(X : Float) : Float;          { Gamma function }
function SgnGamma(X : Float) : Integer;     { Sign of Gamma function }
function LnGamma(X : Float) : Float;        { Log(|Gamma(X)|) }
function IGamma(A, X : Float) : Float;      { Incomplete Gamma function }
function JGamma(A, X : Float) : Float;      { Complement of IGamma }
function Beta(X, Y : Float) : Float;        { Beta function }
function IBeta(A, B, X : Float) : Float;    { Incomplete Beta function }
function Erf(X : Float) : Float;            { Error function }
function Erfc(X : Float) : Float;           { Complement of Erf }

{ ----------------------------------------------------------------------
  Binomial distribution with probability P and number of repetitions N
  ---------------------------------------------------------------------- }

function PBinom(N : Integer; P : Float; K : Integer) : Float; { Prob(X = K) }
function FBinom(N : Integer; P : Float; K : Integer) : Float; { Prob(X <= K) }

{ ----------------------------------------------------------------------
  Poisson distribution with mean Mu
  ---------------------------------------------------------------------- }

function PPoisson(Mu : Float; K : Integer) : Float;  { Prob(X = K) }
function FPoisson(Mu : Float; K : Integer) : Float;  { Prob(X <= K) }

{ ----------------------------------------------------------------------
  Standard normal distribution
  ---------------------------------------------------------------------- }

function DNorm(X : Float) : Float;    { Density of standard normal }
function FNorm(X : Float) : Float;    { Prob(U <= X) }
function PNorm(X : Float) : Float;    { Prob(|U| >= |X|) }
function InvNorm(P : Float) : Float;  { Inverse of FNorm : returns X
                                        such that Prob(U <= X) = P}

{ ----------------------------------------------------------------------
  Student distribution with Nu d.o.f.
  ---------------------------------------------------------------------- }

function DStudent(Nu : Integer; X : Float) : Float;  { Density of t }
function FStudent(Nu : Integer; X : Float) : Float;  { Prob(t <= X) }
function PStudent(Nu : Integer; X : Float) : Float;  { Prob(|t| >= |X|) }

{ ----------------------------------------------------------------------
  Khi-2 distribution with Nu d.o.f.
  ---------------------------------------------------------------------- }

function DKhi2(Nu : Integer; X : Float) : Float;  { Density of Khi2 }
function FKhi2(Nu : Integer; X : Float) : Float;  { Prob(Khi2 <= X) }
function PKhi2(Nu : Integer; X : Float) : Float;  { Prob(Khi2 >= X) }

{ ----------------------------------------------------------------------
  Fisher-Snedecor distribution with Nu1 and Nu2 d.o.f.
  ---------------------------------------------------------------------- }

function DSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;  { Density of F }
function FSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;  { Prob(F <= X) }
function PSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;  { Prob(F >= X) }

{ ----------------------------------------------------------------------
  Exponential distribution
  ---------------------------------------------------------------------- }

function DExpo(A, X : Float) : Float;  { Density of exponential distrib. }
function FExpo(A, X : Float) : Float;  { Prob( <= X) }

{ ----------------------------------------------------------------------
  Beta distribution
  ---------------------------------------------------------------------- }

function DBeta(A, B, X : Float) : Float;   { Density of Beta distribution }
function FBeta(A, B, X : Float) : Float;   { Prob( <= X) }

{ ----------------------------------------------------------------------
  Gamma distribution
  ---------------------------------------------------------------------- }

function DGamma(A, B, X : Float) : Float;  { Density of Gamma distribution }
function FGamma(A, B, X : Float) : Float;  { Prob( <= X) }

{ ----------------------------------------------------------------------
  Random numbers
  ---------------------------------------------------------------------- }

procedure RMarIn(Seed1, Seed2 : Integer);
{ Initializes the random number generator.
  The default initialization corresponds to RMarIn(1802, 9373) }

function IRanMar : LongInt;
{ Returns a 32 bit random number in [ -2,147,483,648 ; 2,147,483,647 ] }

function RanMar : Float;
{ Returns a random number in [0, 1[ }

function RanGaussStd : Float;
{ Returns a random number from the standard normal distribution
  (i.e. the Gaussian distribution with zero mean and unit variance) }

function RanGauss(Mu, Sigma : Float) : Float;
{ Returns a random number from a Gaussian distribution
  with mean Mu and standard deviation Sigma             }

{ ********************************************************************** }

implementation

{ ----------------------------------------------------------------------
  Error handling functions
  ---------------------------------------------------------------------- }

  function DefaultVal(ErrCode : Integer) : Float;
  { Sets the global variable MathErr and the function default value
    according to the error code }
  begin
    MathErr := ErrCode;
    case ErrCode of
      FN_DOMAIN    : DefaultVal := 0.0;
      FN_SING      : DefaultVal := MAXNUM;
      FN_OVERFLOW  : DefaultVal := MAXNUM;
      FN_UNDERFLOW : DefaultVal := 0.0;
    else
      DefaultVal := 0.0;
    end;
  end;

  function MathError : Integer;
  begin
    MathError := MathErr;
  end;

{ ----------------------------------------------------------------------
  Minimum, maximum and sign
  ---------------------------------------------------------------------- }

  function FMin(X, Y : Float) : Float;
  begin
    if X <= Y then
      FMin := X
    else
      FMin := Y;
  end;

  function FMax(X, Y : Float) : Float;
  begin
    if X >= Y then
      FMax := X
    else
      FMax := Y;
  end;

  function IMin(X, Y : Integer) : Integer;
  begin
    if X <= Y then
      IMin := X
    else
      IMin := Y;
  end;

  function IMax(X, Y : Integer) : Integer;
  begin
    if X >= Y then
      IMax := X
    else
      IMax := Y;
  end;

  procedure FSwap(var X, Y : Float);
  var
    Temp : Float;
  begin
    Temp := X;
    X := Y;
    Y := Temp;
  end;

  procedure ISwap(var X, Y : Integer);
  var
    Temp : Integer;
  begin
    Temp := X;
    X := Y;
    Y := Temp;
  end;

  function Sgn(X : Float) : Integer;
  begin
    if X >= 0.0 then
      Sgn := 1
    else
      Sgn := - 1;
  end;

  function Sgn0(X : Float) : Integer;
  begin
    if X > 0.0 then
      Sgn0 := 1
    else if X = 0.0 then
      Sgn0 := 0
    else
      Sgn0 := - 1;
  end;

  function DSgn(A, B : Float) : Float;
  begin
    if B < 0.0 then DSgn := - Abs(A) else DSgn := Abs(A)
  end;

{ ----------------------------------------------------------------------
  Assembler functions
  ---------------------------------------------------------------------- }

{$IFDEF CPU387}
  {$I math387.inc}
{$ENDIF}

{ ----------------------------------------------------------------------
  Elementary functions
  ---------------------------------------------------------------------- }

  function Expo(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < MINLOG then
      Expo := DefaultVal(FN_UNDERFLOW)
    else if X > MAXLOG then
      Expo := DefaultVal(FN_OVERFLOW)
    else
      Expo := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(X);
  end;

  function Exp2(X : Float) : Float;
  var
    XLn2 : Float;
  begin
    MathErr := FN_OK;
    XLn2 := X * LN2;
    if XLn2 < MINLOG then
      Exp2 := DefaultVal(FN_UNDERFLOW)
    else if XLn2 > MAXLOG then
      Exp2 := DefaultVal(FN_OVERFLOW)
    else
      Exp2 := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(XLn2);
  end;

  function Exp10(X : Float) : Float;
  var
    XLn10 : Float;
  begin
    MathErr := FN_OK;
    XLn10 := X * LN10;
    if XLn10 < MINLOG then
      Exp10 := DefaultVal(FN_UNDERFLOW)
    else if XLn10 > MAXLOG then
      Exp10 := DefaultVal(FN_OVERFLOW)
    else
      Exp10 := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(XLn10);
  end;

  function Log(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < 0.0 then
      Log := - DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      Log := - DefaultVal(FN_SING)
    else
      Log := {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X);
  end;

  function Log10(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < 0.0 then
      Log10 := - DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      Log10 := - DefaultVal(FN_SING)
    else
      Log10 := {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X) * INVLN10;
  end;

  function Log2(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < 0.0 then
      Log2 := - DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      Log2 := - DefaultVal(FN_SING)
    else
      Log2 := {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X) * INVLN2;
  end;

  function LogA(X, A : Float) : Float;
  begin
    MathErr := FN_OK;
    if (X < 0.0) or (A <= 0.0) or (A = 1.0) then
      LogA := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      LogA := Sgn(1.0 - A) * DefaultVal(FN_SING)
    else
      {$IFDEF CPU387}
      LogA := fLn(X) / fLn(A);
      {$ELSE}
      LogA := Ln(X) / Ln(A);
      {$ENDIF}
  end;

{ ----------------------------------------------------------------------
  Power functions

  Thanks to Volker Walter <vw@metrohm.ch>
  for suggesting improvements to Power and IntPower
  ---------------------------------------------------------------------- }

  function PowerTests(X, Y : Float; var Res : Float) : Boolean;
  { Tests the cases X=0, Y=0 and Y=1. Returns X^Y in Res }
  begin
    if X = 0.0 then
      begin
        PowerTests := True;
        if Y = 0.0 then       { 0^0 = lim  X^X = 1 }
          Res := 1.0          {       X->0         }
        else if Y > 0.0 then
          Res := 0.0          { 0^Y = 0 }
        else
          Res := DefaultVal(FN_SING);
      end
    else if Y = 0.0 then
      begin
        Res := 1.0;           { X^0 = 1 }
        PowerTests := True;
      end
    else if Y = 1.0 then
      begin
        Res := X;             { X^1 = X }
        PowerTests := True;
      end
    else
      PowerTests := False;
  end;

  function IntPower(X : Float; N : Integer) : Float;
  { Computes X^N by repeated multiplications }
  const
    InverseMaxNum = 1.0 / MAXNUM;
  var
    T      : Float;
    M      : Integer;
    Invert : Boolean;
  begin
    if PowerTests(X, N, T) then
      begin
        IntPower := T;
        Exit;
      end;

    Invert := (N < 0);    { Test if inverting is needed }
    if 1.0 < Abs(X) then  { Test for 0 ..|x| .. 1 }
      begin
        X := 1.0 / X;
        Invert := not Invert;
      end;

    { Legendre's algorithm for
      minimizing the number of multiplications }
    T := 1.0; M := Abs(N);
    while 0 < M do
      begin
        if Odd(M) then T := T * X;
        X := Sqr(X);
        M := M div 2;
      end;

    if Invert then
      if Abs(T) < InverseMaxNum then  { Only here overflow }
        T := DefaultVal(FN_OVERFLOW)
      else
        T := 1.0 / T;

    IntPower := T;
  end;

  function Power(X, Y : Float) : Float;
  { Computes X^Y = Exp(Y * Ln(X)), for X >= 0
    Resorts to IntPower if Y is integer }
  var
    Res  : Float;
    YLnX : Float;
  begin
    if PowerTests(X, Y, Res) then
      Power := Res
    else if (Abs(Y) < MaxInt) and (Trunc(Y) = Y) then  { Integer exponent }
      Power := IntPower(X, Trunc(Y))
    else if X < 0.0 then
      Power := DefaultVal(FN_DOMAIN)
    else
      begin
        YLnX := Y * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X);
        if YLnX < MINLOG then
          Power := DefaultVal(FN_UNDERFLOW)
        else if YLnX > MAXLOG then
          Power := DefaultVal(FN_OVERFLOW)
        else
          Power := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(YLnX);
      end;
  end;

  function Pythag(X, Y : Float) : Float;
  { Computes Sqrt(X^2 + Y^2) without destructive underflow or overflow }
  var
    AbsX, AbsY : Float;
  begin
    MathErr := FN_OK;
    AbsX := Abs(X);
    AbsY := Abs(Y);
    if AbsX > AbsY then
      Pythag := AbsX * Sqrt(1.0 + Sqr(AbsY / AbsX))
    else if AbsY = 0.0 then
      Pythag := 0.0
    else
      Pythag := AbsY * Sqrt(1.0 + Sqr(AbsX / AbsY));
  end;

{ ----------------------------------------------------------------------
  Trigonometric functions
  ---------------------------------------------------------------------- }

  procedure SinCos(X : Float; var SinX, CosX : Float);
  begin
    MathErr := FN_OK;
    SinX := {$IFDEF CPU387}fSin{$ELSE}Sin{$ENDIF}(X);
    CosX := {$IFDEF CPU387}fCos{$ELSE}Cos{$ENDIF}(X);
  end;

  function FixAngle(Theta : Float) : Float;
  begin
    MathErr := FN_OK;
    while Theta > PI do
      Theta := Theta - TWOPI;
    while Theta <= - PI do
      Theta := Theta + TWOPI;
    FixAngle := Theta;
  end;

  function Tan(X : Float) : Float;
  var
    SinX, CosX : Float;
  begin
    MathErr := FN_OK;
    SinX := {$IFDEF CPU387}fSin{$ELSE}Sin{$ENDIF}(X);
    CosX := {$IFDEF CPU387}fCos{$ELSE}Cos{$ENDIF}(X);
    if CosX = 0.0 then
      Tan := Sgn(SinX) * DefaultVal(FN_SING)
    else
      Tan := SinX / CosX;
  end;

  function ArcSin(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (X < - 1.0) or (X > 1.0) then
      ArcSin := DefaultVal(FN_DOMAIN)
    else if X = 1.0 then
      ArcSin := PIDIV2
    else if X = - 1.0 then
      ArcSin := - PIDIV2
    else
      ArcSin := {$IFDEF CPU387}fArcTan{$ELSE}ArcTan{$ENDIF}(X / Sqrt(1.0 - Sqr(X)));
  end;

  function ArcCos(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (X < - 1.0) or (X > 1.0) then
      ArcCos := DefaultVal(FN_DOMAIN)
    else if X = 1.0 then
      ArcCos := 0.0
    else if X = - 1.0 then
      ArcCos := PI
    else
      ArcCos := PIDIV2 - {$IFDEF CPU387}fArcTan{$ELSE}ArcTan{$ENDIF}(X / Sqrt(1.0 - Sqr(X)));
  end;

  function ArcTan2(Y, X : Float) : Float;
  var
    Theta : Float;
  begin
    MathErr := FN_OK;
    if X = 0.0 then
      if Y = 0.0 then
        ArcTan2 := 0.0
      else if Y > 0.0 then
        ArcTan2 := PIDIV2
      else
        ArcTan2 := - PIDIV2
    else
      begin
        { 4th/1st quadrant -PI/2..PI/2 }
        Theta := {$IFDEF CPU387}fArcTan{$ELSE}ArcTan{$ENDIF}(Y / X);

        { 2nd/3rd quadrants }
        if X < 0.0 then
          if Y >= 0.0 then
            Theta := Theta + PI   { 2nd quadrant:  PI/2..PI }
          else
            Theta := Theta - PI;  { 3rd quadrant: -PI..-PI/2 }
        ArcTan2 := Theta;
      end;
  end;

{ ----------------------------------------------------------------------
  Hyperbolic functions
  ---------------------------------------------------------------------- }

  function Sinh(X : Float) : Float;
  var
    ExpX : Float;
  begin
    MathErr := FN_OK;
    if (X < MINLOG) or (X > MAXLOG) then
      Sinh := Sgn(X) * DefaultVal(FN_OVERFLOW)
    else
      begin
        ExpX := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(X);
        Sinh := 0.5 * (ExpX - 1.0 / ExpX);
      end;
  end;

  function Cosh(X : Float) : Float;
  var
    ExpX : Float;
  begin
    MathErr := FN_OK;
    if (X < MINLOG) or (X > MAXLOG) then
      Cosh := DefaultVal(FN_OVERFLOW)
    else
      begin
        ExpX := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(X);
        Cosh := 0.5 * (ExpX + 1.0 / ExpX);
      end;
  end;

  procedure SinhCosh(X : Float; var SinhX, CoshX : Float);
  var
    ExpX, ExpMinusX : Float;
  begin
    MathErr := FN_OK;
    if (X < MINLOG) or (X > MAXLOG) then
      begin
        CoshX := DefaultVal(FN_OVERFLOW);
        SinhX := Sgn(X) * CoshX;
      end
    else
      begin
        ExpX := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(X);
        ExpMinusX := 1.0 / ExpX;
        SinhX := 0.5 * (ExpX - ExpMinusX);
        CoshX := 0.5 * (ExpX + ExpMinusX);
      end;
  end;

  function Tanh(X : Float) : Float;
  var
    SinhX, CoshX : Float;
  begin
    SinhCosh(X, SinhX, CoshX);
    Tanh := SinhX / CoshX;
  end;

  function ArcSinh(X : Float) : Float;
  begin
    MathErr := FN_OK;
    ArcSinh := {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X + Sqrt(Sqr(X) + 1.0));
  end;

  function ArcCosh(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if X < 1.0 then
      ArcCosh := DefaultVal(FN_DOMAIN)
    else
      ArcCosh := {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X + Sqrt(Sqr(X) - 1.0));
  end;

  function ArcTanh(X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (X < - 1.0) or (X > 1.0) then
      ArcTanh := DefaultVal(FN_DOMAIN)
    else if (X = - 1.0) or (X = 1.0) then
      ArcTanh := Sgn(X) * DefaultVal(FN_SING)
    else
      ArcTanh := 0.5 * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}((1.0 + X) / (1.0 - X));
  end;

{ ----------------------------------------------------------------------
  Special functions. Translated from Cephes math library by S. Moshier:
  http://www.moshier.net
  ---------------------------------------------------------------------- }

const { Used by IGamma and IBeta }
  BIG    = 9.223372036854775808E18;
  BIGINV = 1.084202172485504434007E-19;

type
  TabCoef = array[0..9] of Float;

  function PolEvl(var X : Float; Coef : TabCoef; N : Integer) : Float;
{ ----------------------------------------------------------------------
  Evaluates polynomial of degree N:

                        2          N
    y  =  C  + C x + C x  +...+ C x
           0    1     2          N

  Coefficients are stored in reverse order:

  Coef[0] = C  , ..., Coef[N] = C
             N                   0

  The function P1Evl() assumes that Coef[N] = 1.0 and is
  omitted from the array. Its calling arguments are
  otherwise the same as PolEvl().
  ---------------------------------------------------------------------- }
  var
    Ans : Float;
    I : Integer;
  begin
    Ans := Coef[0];
    for I := 1 to N do
      Ans := Ans * X + Coef[I];
    PolEvl := Ans;
  end;

  function P1Evl(var X : Float; Coef : TabCoef; N : Integer) : Float;
{ ----------------------------------------------------------------------
  Evaluate polynomial when coefficient of X is 1.0.
  Otherwise same as PolEvl.
  ---------------------------------------------------------------------- }
  var
    Ans : Float;
    I : Integer;
  begin
    Ans := X + Coef[0];
    for I := 1 to N - 1 do
      Ans := Ans * X + Coef[I];
    P1Evl := Ans;
  end;

  function SgnGamma(X : Float) : Integer;
  begin
    if X > 0.0 then
      SgnGamma := 1
    else if Odd(Trunc(Abs(X))) then
      SgnGamma := 1
    else
      SgnGamma := - 1;
  end;

  function Stirf(X : Float) : Float;
  { Stirling's formula for the gamma function
    Gamma(x) = Sqrt(2*Pi) x^(x-.5) exp(-x) (1 + 1/x P(1/x))
    where P(x) is a polynomial }
  const
    STIR : TabCoef = (
        7.147391378143610789273E-4,
      - 2.363848809501759061727E-5,
      - 5.950237554056330156018E-4,
        6.989332260623193171870E-5,
        7.840334842744753003862E-4,
      - 2.294719747873185405699E-4,
      - 2.681327161876304418288E-3,
        3.472222222230075327854E-3,
        8.333333333333331800504E-2,
        0);

  var
    W, P : Float;
  begin
    W := 1.0 / X;
    if X > 1024.0 then
      begin
        P := 6.97281375836585777429E-5 * W + 7.84039221720066627474E-4;
        P := P * W - 2.29472093621399176955E-4;
        P := P * W - 2.68132716049382716049E-3;
        P := P * W + 3.47222222222222222222E-3;
        P := P * W + 8.33333333333333333333E-2;
      end
    else
      P := PolEvl(W, STIR, 8);
    {$IFDEF CPU387}
    Stirf := SQRT2PI * fExp((X - 0.5) * fLn(X) - X) * (1.0 + W * P);
    {$ELSE}
    Stirf := SQRT2PI * Exp((X - 0.5) * Ln(X) - X) * (1.0 + W * P);
    {$ENDIF}
  end;

  function GamSmall(X1, Z : Float) : Float;
  { Gamma function for small values of the argument }
  const
    S : TabCoef = (
      - 1.193945051381510095614E-3,
        7.220599478036909672331E-3,
      - 9.622023360406271645744E-3,
      - 4.219773360705915470089E-2,
        1.665386113720805206758E-1,
      - 4.200263503403344054473E-2,
      - 6.558780715202540684668E-1,
        5.772156649015328608253E-1,
        1.000000000000000000000E0,
        0);

    SN : TabCoef = (
        1.133374167243894382010E-3,
        7.220837261893170325704E-3,
        9.621911155035976733706E-3,
      - 4.219773343731191721664E-2,
      - 1.665386113944413519335E-1,
      - 4.200263503402112910504E-2,
        6.558780715202536547116E-1,
        5.772156649015328608727E-1,
      - 1.000000000000000000000E0,
        0);

  var
    P : Float;
  begin
    if X1 = 0.0 then
      begin
        GamSmall := DefaultVal(FN_SING);
        Exit;
      end;
    if X1 < 0.0 then
      begin
        X1 := - X1;
        P := PolEvl(X1, SN, 8);
      end
    else
      P := PolEvl(X1, S, 8);
    GamSmall := Z / (X1 * P);
  end;

  function StirfL(X : Float) : Float;
  { Approximate Ln(Gamma) by Stirling's formula, for X >= 13 }
  const
    P : TabCoef = (
        4.885026142432270781165E-3,
      - 1.880801938119376907179E-3,
        8.412723297322498080632E-4,
      - 5.952345851765688514613E-4,
        7.936507795855070755671E-4,
      - 2.777777777750349603440E-3,
        8.333333333333331447505E-2,
        0, 0, 0);

  var
    Q, W : Float;
  begin
    Q := {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X) * (X - 0.5) - X;
    Q := Q + LNSQRT2PI;
    if X > 1.0E+10 then
      StirfL := Q
    else
      begin
        W := 1.0 / Sqr(X);
        StirfL := Q + PolEvl(W, P, 6) / X;
      end;
  end;

  function Gamma(X : Float) : Float;
  const
    P : TabCoef = (
      4.212760487471622013093E-5,
      4.542931960608009155600E-4,
      4.092666828394035500949E-3,
      2.385363243461108252554E-2,
      1.113062816019361559013E-1,
      3.629515436640239168939E-1,
      8.378004301573126728826E-1,
      1.000000000000000000009E0,
      0, 0);

    Q : TabCoef = (
      - 1.397148517476170440917E-5,
        2.346584059160635244282E-4,
      - 1.237799246653152231188E-3,
      - 7.955933682494738320586E-4,
        2.773706565840072979165E-2,
      - 4.633887671244534213831E-2,
      - 2.243510905670329164562E-1,
        4.150160950588455434583E-1,
        9.999999999999999999908E-1,
        0);

  var
    SgnGam, N : Integer;
    A, X1, Z : Float;
  begin
    MathErr := FN_OK;
    SgnGam := SgnGamma(X);

    if (X = 0.0) or ((X < 0.0) and (Frac(X) = 0.0)) then
      begin
        Gamma := SgnGam * DefaultVal(FN_SING);
        Exit;
      end;

    if X > MAXGAM then
      begin
        Gamma := DefaultVal(FN_OVERFLOW);
        Exit;
      end;

    A := Abs(X);
    if A > 13.0 then
      begin
        if X < 0.0 then
          begin
            N := Trunc(A);
            Z := A - N;
            if Z > 0.5 then
              begin
                N := N + 1;
                Z := A - N;
              end;
            Z := Abs(A * {$IFDEF CPU387}fSin{$ELSE}Sin{$ENDIF}(PI * Z)) * Stirf(A);
            if Z <= PI / MAXNUM then
              begin
                Gamma := SgnGam * DefaultVal(FN_OVERFLOW);
                Exit;
              end;
            Z := PI / Z;
          end
        else
          Z := Stirf(X);
        Gamma := SgnGam * Z;
      end
    else
      begin
        Z := 1.0;
        X1 := X;
        while X1 >= 3.0 do
          begin
            X1 := X1 - 1.0;
            Z := Z * X1;
          end;
        while X1 < - 0.03125 do
          begin
            Z := Z / X1;
            X1 := X1 + 1.0;
          end;
        if X1 <= 0.03125 then
          Gamma := GamSmall(X1, Z)
        else
          begin
            while X1 < 2.0 do
              begin
                Z := Z / X1;
                X1 := X1 + 1.0;
              end;
            if (X1 = 2.0) or (X1 = 3.0) then
              Gamma := Z
            else
              begin
                X1 := X1 - 2.0;
                Gamma := Z * PolEvl(X1, P, 7) / PolEvl(X1, Q, 8);
              end;
          end;
      end;
  end;

  function LnGamma(X : Float) : Float;
  const
    P : TabCoef = (
      - 2.163690827643812857640E3,
      - 8.723871522843511459790E4,
      - 1.104326814691464261197E6,
      - 6.111225012005214299996E6,
      - 1.625568062543700591014E7,
      - 2.003937418103815175475E7,
      - 8.875666783650703802159E6,
        0, 0, 0);

    Q : TabCoef = (
      - 5.139481484435370143617E2,
      - 3.403570840534304670537E4,
      - 6.227441164066219501697E5,
      - 4.814940379411882186630E6,
      - 1.785433287045078156959E7,
      - 3.138646407656182662088E7,
      - 2.099336717757895876142E7,
        0, 0, 0);

  var
    N : Integer;
    A, X1, Z : Float;
  begin
    MathErr := FN_OK;

    if (X = 0.0) or ((X < 0.0) and (Frac(X) = 0.0)) then
      begin
        LnGamma := DefaultVal(FN_SING);
        Exit;
      end;

    if X > MAXLGM then
      begin
        LnGamma := DefaultVal(FN_OVERFLOW);
        Exit;
      end;

    A := Abs(X);
    if A > 34.0 then
      begin
        if X < 0.0 then
          begin
            N := Trunc(A);
            Z := A - N;
            if Z > 0.5 then
              begin
                N := N + 1;
                Z := N - A;
              end;
            Z := A * {$IFDEF CPU387}fSin{$ELSE}Sin{$ENDIF}(PI * Z);
            if Z = 0.0 then
              begin
                LnGamma := DefaultVal(FN_OVERFLOW);
                Exit;
              end;
            Z := LNPI - {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(Z) - StirfL(A);
          end
        else
          Z := StirfL(X);
        LnGamma := Z;
      end
    else if X < 13.0 then
      begin
        Z := 1.0;
        X1 := X;
        while X1 >= 3 do
          begin
            X1 := X1 - 1.0;
            Z := Z * X1;
          end;
        while X1 < 2.0 do
          begin
            if Abs(X1) <= 0.03125 then
              begin
                LnGamma := {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(Abs(GamSmall(X1, Z)));
                Exit;
              end;
            Z := Z / X1;
            X1 := X1 + 1.0;
          end;
        if Z < 0.0 then Z := - Z;
        if X1 = 2.0 then
          LnGamma := {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(Z)
        else
          begin
            X1 := X1 - 2.0;
            LnGamma := X1 * PolEvl(X1, P, 6) / P1Evl(X1, Q, 7) +
                       {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(Z);
          end;
      end
    else
      LnGamma := StirfL(X);
  end;

  function IGamma(A, X : Float) : Float;
  var
    Ans, Ax, C, R : Float;
  begin
    MathErr := FN_OK;

    if (X <= 0.0) or (A <= 0.0) then
      begin
        IGamma := 0.0;
        Exit;
      end;

    if (X > 1.0) and (X > A) then
      begin
        IGamma := 1.0 - JGamma(A, X);
        Exit;
      end;

    Ax := A * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X) - X - LnGamma(A);
    if Ax < MINLOG then
      begin
        IGamma := DefaultVal(FN_UNDERFLOW);
        Exit;
      end;

    Ax := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(Ax);

    { power series }
    R := A;
    C := 1.0;
    Ans := 1.0;

    repeat
      R := R + 1.0;
      C := C * X / R;
      Ans := Ans + C;
    until C / Ans <= MACHEP;

    IGamma := Ans * Ax / A;
  end;

  function JGamma(A, X : Float) : Float;
  var
    Ans, C, Yc, Ax, Y, Z, R, T,
    Pk, Pkm1, Pkm2, Qk, Qkm1, Qkm2 : Float;
  begin
    MathErr := FN_OK;

    if (X <= 0.0) or (A <= 0.0) then
      begin
        JGamma := 1.0;
        Exit;
      end;

    if (X < 1.0) or (X < A) then
      begin
        JGamma := 1.0 - IGamma(A, X);
        Exit;
      end;

    Ax := A * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X) - X - LnGamma(A);

    if Ax < MINLOG then
      begin
        JGamma := DefaultVal(FN_UNDERFLOW);
        Exit;
      end;

    Ax := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(Ax);

    { continued fraction }
    Y := 1.0 - A;
    Z := X + Y + 1.0;
    C := 0.0;
    Pkm2 := 1.0;
    Qkm2 := X;
    Pkm1 := X + 1.0;
    Qkm1 := Z * X;
    Ans := Pkm1 / Qkm1;

    repeat
      C := C + 1.0;
      Y := Y + 1.0;
      Z := Z + 2.0;
      Yc := Y * C;
      Pk := Pkm1 * Z - Pkm2 * Yc;
      Qk := Qkm1 * Z - Qkm2 * Yc;
      if Qk <> 0.0 then
        begin
          R := Pk / Qk;
          T := Abs((Ans - R) / R);
          Ans := R;
        end
      else
        T := 1.0;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;
      if Abs(Pk) > BIG then
        begin
          Pkm2 := Pkm2 / BIG;
          Pkm1 := Pkm1 / BIG;
          Qkm2 := Qkm2 / BIG;
          Qkm1 := Qkm1 / BIG;
        end;
    until T <= MACHEP;

    JGamma := Ans * Ax;
  end;

  function Fact(N : Integer) : Float;
  begin
    MathErr := FN_OK;
    if N < 0 then
      Fact := DefaultVal(FN_DOMAIN)
    else if N > MAXFAC then
      Fact := DefaultVal(FN_OVERFLOW)
    else if N <= NFACT then
      Fact := FactArray[N]
    else
      Fact := Gamma(N + 1);
  end;

  function Binomial(N, K : Integer) : Float;
  var
    I, N1 : Integer;
    Prod : Float;
  begin
    MathErr := FN_OK;
    if K < 0 then
      Binomial := 0.0
    else if (K = 0) or (K = N) then
      Binomial := 1.0
    else if (K = 1) or (K = N - 1) then
      Binomial := N
    else
      begin
        if K > N - K then K := N - K;
        N1 := Succ(N);
        Prod := N;
        for I := 2 to K do
          Prod := Prod * (Int(N1 - I) / Int(I));
        Binomial := Int(0.5 + Prod);
      end;
  end;

  function Beta(X, Y : Float) : Float;
  { Computes Beta(X, Y) = Gamma(X) * Gamma(Y) / Gamma(X + Y) }
  var
    Lx, Ly, Lxy : Float;
    SgnBeta : Integer;
  begin
    MathErr := FN_OK;
    SgnBeta := SgnGamma(X) * SgnGamma(Y) * SgnGamma(X + Y);
    Lxy := LnGamma(X + Y);
    if MathErr <> FN_OK then
      begin
        Beta := 0.0;
        Exit;
      end;
    Lx := LnGamma(X);
    if MathErr <> FN_OK then
      begin
        Beta := SgnBeta * MAXNUM;
        Exit;
      end;
    Ly := LnGamma(Y);
    if MathErr <> FN_OK then
      begin
        Beta := SgnBeta * MAXNUM;
        Exit;
      end;
    Beta := SgnBeta * {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(Lx + Ly - Lxy);
  end;

  function PSeries(A, B, X : Float) : Float;
  { Power series for incomplete beta integral. Use when B*X is small }
  var
    S, T, U, V, T1, Z, Ai : Float;
    N : Integer;
  begin
    Ai := 1.0 / A;
    U := (1.0 - B) * X;
    V := U / (A + 1.0);
    T1 := V;
    T := U;
    N := 2;
    S := 0.0;
    Z := MACHEP * Ai;
    while Abs(V) > Z do
      begin
        U := (N - B) * X / N;
        T := T * U;
        V := T / (A + N);
        S := S + V;
        N := N + 1;
      end;
    S := S + T1;
    S := S + Ai;

    U := A * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X);
    if (A + B < MAXGAM) and (Abs(U) < MAXLOG) then
      begin
        T := Gamma(A + B) / (Gamma(A) * Gamma(B));
        S := S * T * Power(X, A);
      end
    else
      begin
        T := LnGamma(A + B) - LnGamma(A) - LnGamma(B)
             + U + {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(S);
        if T < MINLOG then
          S := 0.0
        else
          S := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(T);
      end;
    PSeries := S;
  end;

  function CFrac1(A, B, X : Float) : Float;
  { Continued fraction expansion #1 for incomplete beta integral }
  var
    Xk, Pk, Pkm1, Pkm2, Qk, Qkm1, Qkm2,
    K1, K2, K3, K4, K5, K6, K7, K8,
    R, T, Ans, Thresh : Float;
    N : Integer;
  label
    CDone;
  begin
    K1 := A;
    K2 := A + B;
    K3 := A;
    K4 := A + 1.0;
    K5 := 1.0;
    K6 := B - 1.0;
    K7 := K4;
    K8 := A + 2.0;

    Pkm2 := 0.0;
    Qkm2 := 1.0;
    Pkm1 := 1.0;
    Qkm1 := 1.0;
    Ans := 1.0;
    R := 1.0;
    N := 0;
    Thresh := 3.0 * MACHEP;

    repeat
      Xk := - (X * K1 * K2) / (K3 * K4);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      Xk := (X * K5 * K6) / (K7 * K8);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      if Qk <> 0.0 then R := Pk / Qk;

      if R <> 0.0 then
        begin
          T := Abs((Ans - R) / R);
          Ans := R;
        end
      else
        T := 1.0;

      if T < Thresh then goto CDone;

      K1 := K1 + 1.0;
      K2 := K2 + 1.0;
      K3 := K3 + 2.0;
      K4 := K4 + 2.0;
      K5 := K5 + 1.0;
      K6 := K6 - 1.0;
      K7 := K7 + 2.0;
      K8 := K8 + 2.0;

      if Abs(Qk) + Abs(Pk) > BIG then
        begin
          Pkm2 := Pkm2 * BIGINV;
          Pkm1 := Pkm1 * BIGINV;
          Qkm2 := Qkm2 * BIGINV;
          Qkm1 := Qkm1 * BIGINV;
        end;

      if (Abs(Qk) < BIGINV) or (Abs(Pk) < BIGINV) then
        begin
          Pkm2 := Pkm2 * BIG;
          Pkm1 := Pkm1 * BIG;
          Qkm2 := Qkm2 * BIG;
          Qkm1 := Qkm1 * BIG;
        end;
      N := N + 1;
    until N > 400;
    MathErr := FN_PLOSS;

CDone:
    CFrac1 := Ans;
  end;

  function CFrac2(A, B, X : Float) : Float;
  { Continued fraction expansion #2 for incomplete beta integral }
  var
    Xk, Pk, Pkm1, Pkm2, Qk, Qkm1, Qkm2,
    K1, K2, K3, K4, K5, K6, K7, K8,
    R, T, Z, Ans, Thresh : Float;
    N : Integer;
  label
    CDone;
  begin
    K1 := A;
    K2 := B - 1.0;
    K3 := A;
    K4 := A + 1.0;
    K5 := 1.0;
    K6 := A + B;
    K7 := A + 1.0;
    K8 := A + 2.0;

    Pkm2 := 0.0;
    Qkm2 := 1.0;
    Pkm1 := 1.0;
    Qkm1 := 1.0;
    Z := X / (1.0 - X);
    Ans := 1.0;
    R := 1.0;
    N := 0;
    Thresh := 3.0 * MACHEP;

    repeat
      Xk := - (Z * K1 * K2) / (K3 * K4);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      Xk := (Z * K5 * K6) / (K7 * K8);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      if Qk <> 0.0 then R := Pk / Qk;

      if R <> 0.0 then
        begin
          T := Abs((Ans - R) / R);
          Ans := R;
        end
      else
        T := 1.0;

      if T < Thresh then goto CDone;

      K1 := K1 + 1.0;
      K2 := K2 - 1.0;
      K3 := K3 + 2.0;
      K4 := K4 + 2.0;
      K5 := K5 + 1.0;
      K6 := K6 + 1.0;
      K7 := K7 + 2.0;
      K8 := K8 + 2.0;

      if Abs(Qk) + Abs(Pk) > BIG then
        begin
          Pkm2 := Pkm2 * BIGINV;
          Pkm1 := Pkm1 * BIGINV;
          Qkm2 := Qkm2 * BIGINV;
          Qkm1 := Qkm1 * BIGINV;
        end;

      if (Abs(Qk) < BIGINV) or (Abs(Pk) < BIGINV) then
        begin
          Pkm2 := Pkm2 * BIG;
          Pkm1 := Pkm1 * BIG;
          Qkm2 := Qkm2 * BIG;
          Qkm1 := Qkm1 * BIG;
        end;
      N := N + 1;
    until N > 400;
    MathErr := FN_PLOSS;

CDone:
    CFrac2 := Ans;
  end;

  function IBeta(A, B, X : Float) : Float;
  var
    A1, B1, X1, T, W, Xc, Y : Float;
    Flag : Boolean;
  label
    Done;
  begin
    MathErr := FN_OK;

    if (A <= 0.0) or (B <= 0.0) or (X < 0.0) or (X > 1.0) then
      begin
        IBeta := DefaultVal(FN_DOMAIN);
        Exit;
      end;

    if (X = 0.0) or (X = 1.0) then
      begin
        IBeta := X;
        Exit;
      end;

    Flag := False;
    if (B * X <= 1.0) and (X <= 0.95) then
      begin
        T := PSeries(A, B, X);
        goto Done;
      end;

    W := 1.0 - X;

    { Reverse a and b if x is greater than the mean. }
    if X > A / (A + B) then
      begin
        Flag := True;
        A1 := B;
        B1 := A;
        Xc := X;
        X1 := W;
      end
    else
      begin
        A1 := A;
        B1 := B;
        Xc := W;
        X1 := X;
      end;

    if Flag and (B1 * X1 <= 1.0) and (X1 <= 0.95) then
      begin
        T := PSeries(A1, B1, X1);
        goto Done;
      end;

    { Choose expansion for optimal convergence }
    Y := X1 * (A1 + B1 - 2.0) - (A1 - 1.0);
    if Y < 0.0 then
      W := CFrac1(A1, B1, X1)
    else
      W := CFrac2(A1, B1, X1) / Xc;

    { Multiply w by the factor
     a      b   _             _     _
    x  (1-x)   | (a+b) / ( a | (a) | (b) )    }

    Y := A1 * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X1);
    T := B1 * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(Xc);
    if (A1 + B1 < MAXGAM) and (Abs(Y) < MAXLOG) and (Abs(T) < MAXLOG) then
      begin
        T := Power(Xc, B1) ;
        T := T * Power(X1, A1);
        T := T / A1;
        T := T * W;
        T := T * Gamma(A1 + B1) / (Gamma(A1) * Gamma(B1));
      end
    else
      begin
        { Resort to logarithms }
        Y := Y + T + LnGamma(A1 + B1) - LnGamma(A1) - LnGamma(B1)
               + {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(W / A1);
        if Y < MINLOG then
          T := 0.0
        else
          T := {$IFDEF CPU387}fExp{$ELSE}Exp{$ENDIF}(Y);
      end;

Done:
    if Flag then
      if T <= MACHEP then
        T := 1.0 - MACHEP
      else
        T := 1.0 - T;

    IBeta := T;
  end;

  function Erf(X : Float) : Float;
  begin
    if X < 0.0 then
      Erf := - IGamma(0.5, Sqr(X))
    else
      Erf := IGamma(0.5, Sqr(X));
  end;

  function Erfc(X : Float) : Float;
  begin
    if X < 0.0 then
      Erfc := 1.0 + IGamma(0.5, Sqr(X))
    else
      Erfc := JGamma(0.5, Sqr(X));
  end;

{ ----------------------------------------------------------------------
  Probability functions
  ---------------------------------------------------------------------- }

  function PBinom(N : Integer; P : Float; K : Integer) : Float;
  begin
    MathErr := FN_OK;
    if (P < 0.0) or (P > 1.0) or (N <= 0) or (N < K) then
      PBinom := DefaultVal(FN_DOMAIN)
    else if K = 0 then
      PBinom := IntPower(1.0 - P, N)
    else if K = N then
      PBinom := IntPower(P, N)
    else
      PBinom := Binomial(N, K) * IntPower(P, K) * IntPower(1.0 - P, N - K);
  end;

  function FBinom(N : Integer; P : Float; K : Integer) : Float;
  begin
    MathErr := FN_OK;
    if (P < 0.0) or (P > 1.0) or (N <= 0) or (N < K) then
      FBinom := DefaultVal(FN_DOMAIN)
    else if K = 0 then
      FBinom := IntPower(1.0 - P, N)
    else if K = N then
      FBinom := 1.0
    else
      FBinom := 1.0 - IBeta(K + 1, N - K, P);
  end;

  function PPoisson(Mu : Float; K : Integer) : Float;
  var
    P : Float;
    I : Integer;
  begin
    MathErr := FN_OK;
    if (Mu <= 0.0) or (K < 0) then
      PPoisson := DefaultVal(FN_DOMAIN)
    else if K = 0 then
      PPoisson := Expo(- Mu)
    else
      begin
        P := Mu;
        for I := 2 to K do
          P := P * Mu / I;
        PPoisson := Expo(- Mu) * P;
      end;
  end;

  function FPoisson(Mu : Float; K : Integer) : Float;
  begin
    MathErr := FN_OK;
    if (Mu <= 0.0) or (K < 0) then
      FPoisson := DefaultVal(FN_DOMAIN)
    else if K = 0 then
      FPoisson := Expo(- Mu)
    else
      FPoisson := JGamma(K + 1, Mu);
  end;

  function DNorm(X : Float) : Float;
  begin
    DNorm := INVSQRT2PI * Expo(- 0.5 * Sqr(X));
  end;

  function FNorm(X : Float) : Float;
  begin
    FNorm := 0.5 * (1.0 + Erf(X * SQRT2DIV2));
  end;

  function InvNorm(P : Float) : Float;
{ ----------------------------------------------------------------------
  Inverse of Normal distribution function

  Returns the argument, X, for which the area under the Gaussian
  probability density function (integrated from minus infinity to X)
  is equal to P.

  Translated from Cephes library.
  ---------------------------------------------------------------------- }
  const
    P0 : TabCoef = (
        8.779679420055069160496E-3,
      - 7.649544967784380691785E-1,
        2.971493676711545292135E0,
      - 4.144980036933753828858E0,
        2.765359913000830285937E0,
      - 9.570456817794268907847E-1,
        1.659219375097958322098E-1,
      - 1.140013969885358273307E-2,
        0, 0);

    Q0 : TabCoef = (
      - 5.303846964603721860329E0,
        9.908875375256718220854E0,
      - 9.031318655459381388888E0,
        4.496118508523213950686E0,
      - 1.250016921424819972516E0,
        1.823840725000038842075E-1,
      - 1.088633151006419263153E-2,
        0, 0, 0);

    P1 : TabCoef = (
      4.302849750435552180717E0,
      4.360209451837096682600E1,
      9.454613328844768318162E1,
      9.336735653151873871756E1,
      5.305046472191852391737E1,
      1.775851836288460008093E1,
      3.640308340137013109859E0,
      3.691354900171224122390E-1,
      1.403530274998072987187E-2,
      1.377145111380960566197E-4);

    Q1 : TabCoef = (
      2.001425109170530136741E1,
      7.079893963891488254284E1,
      8.033277265194672063478E1,
      5.034715121553662712917E1,
      1.779820137342627204153E1,
      3.845554944954699547539E0,
      3.993627390181238962857E-1,
      1.526870689522191191380E-2,
      1.498700676286675466900E-4,
      0);

    P2 : TabCoef = (
      3.244525725312906932464E0,
      6.856256488128415760904E0,
      3.765479340423144482796E0,
      1.240893301734538935324E0,
      1.740282292791367834724E-1,
      9.082834200993107441750E-3,
      1.617870121822776093899E-4,
      7.377405643054504178605E-7,
      0, 0);

    Q2 : TabCoef = (
      6.021509481727510630722E0,
      3.528463857156936773982E0,
      1.289185315656302878699E0,
      1.874290142615703609510E-1,
      9.867655920899636109122E-3,
      1.760452434084258930442E-4,
      8.028288500688538331773E-7,
      0, 0, 0);

    P3 : TabCoef = (
        2.020331091302772535752E0,
        2.133020661587413053144E0,
        2.114822217898707063183E-1,
      - 6.500909615246067985872E-3,
      - 7.279315200737344309241E-4,
      - 1.275404675610280787619E-5,
      - 6.433966387613344714022E-8,
      - 7.772828380948163386917E-11,
        0, 0);

    Q3 : TabCoef = (
        2.278210997153449199574E0,
        2.345321838870438196534E-1,
      - 6.916708899719964982855E-3,
      - 7.908542088737858288849E-4,
      - 1.387652389480217178984E-5,
      - 7.001476867559193780666E-8,
      - 8.458494263787680376729E-11,
        0, 0, 0);

  var
    X, Y, Z, Y2, X0, X1 : Float;
    Code : Integer;
  begin
    if (P <= 0.0) or (P >= 1.0) then
      begin
        InvNorm := DefaultVal(FN_DOMAIN);
        Exit;
      end;

    Code := 1;
    Y := P;
    if Y > (1.0 - 0.13533528323661269189) then { 0.135... = exp(-2) }
      begin
        Y := 1.0 - Y;
        Code := 0;
      end;
    if Y > 0.13533528323661269189 then
      begin
        Y := Y - 0.5;
        Y2 := Y * Y;
        X := Y + Y * (Y2 * PolEvl(Y2, P0, 7) / P1Evl(Y2, Q0, 7));
        X := X * SQRT2PI;
        InvNorm := X;
        Exit;
      end;

    X := Sqrt(- 2.0 * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(Y));
    X0 := X - {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X) / X;
    Z := 1.0 / X;
    if X < 8.0 then
      X1 := Z * PolEvl(Z, P1, 9) / P1Evl(Z, Q1, 9)
    else if X < 32.0 then
      X1 := Z * PolEvl(Z, P2, 7) / P1Evl(Z, Q2, 7)
    else
      X1 := Z * PolEvl(Z, P3, 7) / P1Evl(Z, Q3, 7);
    X := X0 - X1;
    if Code <> 0 then
      X := - X;
    InvNorm := X;
  end;

  function PNorm(X : Float) : Float;
  var
    A : Float;
  begin
    A := Abs(X);
    MathErr := FN_OK;
    if A = 0.0 then
      PNorm := 1.0
    else if A < 1.0 then
      PNorm := 1.0 - Erf(A * SQRT2DIV2)
    else
      PNorm := Erfc(A * SQRT2DIV2);
  end;

  function DStudent(Nu : Integer; X : Float) : Float;
  var
    L, P, Q : Float;
  begin
    MathErr := FN_OK;
    if Nu < 1 then
      DStudent := DefaultVal(FN_DOMAIN)
    else
      begin
        P := 0.5 * (Nu + 1);
        Q := 0.5 * Nu;
        L := LnGamma(P) - LnGamma(Q)
             - 0.5 * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(Nu * PI)
             - P * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(1.0 + Sqr(X) / Nu);
        DStudent := Expo(L);
      end;
  end;

  function FStudent(Nu : Integer; X : Float) : Float;
  var
    F : Float;
  begin
    MathErr := FN_OK;
    if Nu < 1 then
      FStudent := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      FStudent := 0.5
    else
      begin
        F := 0.5 * IBeta(0.5 * Nu, 0.5, Nu / (Nu + Sqr(X)));
        if X < 0.0 then FStudent := F else FStudent := 1.0 - F;
      end;
  end;

  function PStudent(Nu : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if Nu < 1 then
      PStudent := DefaultVal(FN_DOMAIN)
    else
      PStudent := IBeta(0.5 * Nu, 0.5, Nu / (Nu + Sqr(X)));
  end;

  function DKhi2(Nu : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    DKhi2 := DGamma(0.5 * Nu, 0.5, X);
  end;

  function FKhi2(Nu : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (Nu < 1) or (X <= 0.0) then
      FKhi2 := DefaultVal(FN_DOMAIN)
    else
      FKhi2 := IGamma(0.5 * Nu, 0.5 * X);
  end;

  function PKhi2(Nu : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (Nu < 1) or (X <= 0.0) then
      PKhi2 := DefaultVal(FN_DOMAIN)
    else
      PKhi2 := JGamma(0.5 * Nu, 0.5 * X);
  end;

  function DSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
  var
    P1, P2, R, S, L : Float;
  begin
    MathErr := FN_OK;
    if (Nu1 < 1) or (Nu2 < 1) or (X <= 0.0) then
      DSnedecor := DefaultVal(FN_DOMAIN)
    else
      begin
        R := Int(Nu1) / Int(Nu2);
        P1 := 0.5 * Nu1;
        P2 := 0.5 * Nu2;
        S := P1 + P2;
        L := LnGamma(S) - LnGamma(P1) - LnGamma(P2)
             + P1 * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(R);
        L := L + (P1 - 1.0) * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X)
               - S * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(1.0 + R * X);
        DSnedecor := Expo(L);
      end;
  end;

  function FSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (Nu1 < 1) or (Nu2 < 1) or (X <= 0.0) then
      FSnedecor := DefaultVal(FN_DOMAIN)
    else
      FSnedecor := 1.0 - IBeta(0.5 * Nu2, 0.5 * Nu1, Nu2 / (Nu2 + Nu1 * X));
  end;

  function PSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
  begin
    MathErr := FN_OK;
    if (Nu1 < 1) or (Nu2 < 1) or (X <= 0.0) then
      PSnedecor := DefaultVal(FN_DOMAIN)
    else
      PSnedecor := IBeta(0.5 * Nu2, 0.5 * Nu1, Nu2 / (Nu2 + Nu1 * X));
  end;

  function DExpo(A, X : Float) : Float;
  begin
    if (A <= 0.0) or (X < 0.0) then
      DExpo := DefaultVal(FN_DOMAIN)
    else
      DExpo := A * Expo(- A * X);
  end;

  function FExpo(A, X : Float) : Float;
  begin
    if (A <= 0.0) or (X < 0.0) then
      FExpo := DefaultVal(FN_DOMAIN)
    else
      FExpo := 1.0 - Expo(- A * X);
  end;

  function DBeta(A, B, X : Float) : Float;
  var
    L : Float;
  begin
    MathErr := FN_OK;
    if (A <= 0.0) or (B <= 0.0) or (X < 0.0) or (X > 1.0) then
      DBeta := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      if A < 1.0 then DBeta := DefaultVal(FN_SING) else DBeta := 0.0
    else if X = 1.0 then
      if B < 1.0 then DBeta := DefaultVal(FN_SING) else DBeta := 0.0
    else
      begin
        L := LnGamma(A + B) - LnGamma(A) - LnGamma(B);
        L := L + (A - 1.0) * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X)
               + (B - 1.0) * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(1.0 - X);
        DBeta := Expo(L);
      end;
  end;

  function FBeta(A, B, X : Float) : Float;
  begin
    FBeta := IBeta(A, B, X);
  end;

  function DGamma(A, B, X : Float) : Float;
  var
    L : Float;
  begin
    MathErr := FN_OK;
    if (A <= 0.0) or (B <= 0.0) or (X < 0.0) then
      DGamma := DefaultVal(FN_DOMAIN)
    else if X = 0.0 then
      if A < 1.0 then
        DGamma := DefaultVal(FN_SING)
      else if A = 1.0 then
        DGamma := B
      else
        DGamma := 0.0
    else
      begin
        L := A * Ln(B) - LnGamma(A)
             + (A - 1.0) * {$IFDEF CPU387}fLn{$ELSE}Ln{$ENDIF}(X) - B * X;
        DGamma := Expo(L);
      end;
  end;

  function FGamma(A, B, X : Float) : Float;
  begin
    FGamma := IGamma(A, B * X);
  end;

{ ----------------------------------------------------------------------
  Random numbers
  ---------------------------------------------------------------------- }

var
  X1, X2     : LongInt;  { Uniform random integers }
  C1, C2     : LongInt;  { Carries }
  Gauss_Save : Float;    { Saves a gaussian random number }
  Gauss_Set  : Boolean;  { Flags if a gaussian number has been saved }

  procedure RMarIn(Seed1, Seed2 : Integer);
  begin
    X1 := Seed1;
    X2 := Seed2;
    C1 := 0;
    C2 := 0;
  end;

  function IRanMar : LongInt;
  var
    Y1, Y2 : LongInt;
  begin
    Y1 := 18000 * X1 + C1;
    X1 := Y1 and 65535;
    C1 := Y1 shr 16;
    Y2 := 30903 * X2 + C2;
    X2 := Y2 and 65535;
    C2 := Y2 shr 16;
    IRanMar := (X1 shl 16) + (X2 and 65535);
  end;

  function RanMar : Float;
  begin
    RanMar := (IRanMar + 2147483648.0) / 4294967296.0;
  end;

  function RanGaussStd : Float;
  { Computes 2 random numbers from the standard normal distribution,
    returns one and saves the other for the next call }
  var
    R, Theta, SinTheta, CosTheta : Float;
  begin
    if not Gauss_Set then
      begin
        R := Sqrt(- 2.0 * Log(RanMar));
        Theta := TWOPI * RanMar;
        SinCos(Theta, SinTheta, CosTheta);
        RanGaussStd := R * CosTheta;        { Return 1st number }
        Gauss_Save := R * SinTheta;         { Save 2nd number }
      end
    else
      RanGaussStd := Gauss_Save;            { Return saved number }
    Gauss_Set := not Gauss_Set;
  end;

  function RanGauss(Mu, Sigma : Float) : Float;
  { Returns a random number from the normal distribution
    with mean Mu and standard deviation Sigma }
  begin
    RanGauss := Mu + Sigma * RanGaussStd;
  end;

{ ----------------------------------------------------------------------
  Initialization code
  ---------------------------------------------------------------------- }

var
  I : Integer;

begin
  { Initialize MathErr }
  MathErr := FN_OK;

  { Store the factorials of the first NFACT integers in a table }
  FactArray[0] := 1.0;
  FactArray[1] := 1.0;
  FactArray[2] := 2.0;
  for I := 3 to NFACT do
    FactArray[I] := FactArray[I - 1] * I;

  { Initialize random number generators }
  Gauss_Save := 0.0;
  Gauss_Set := False;
  RMarIn(1802, 9373);
end.
