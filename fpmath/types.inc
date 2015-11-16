{ ------------------------------------------------------------------
  Floating point type (Default = Double)
  ------------------------------------------------------------------ }

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

{ ------------------------------------------------------------------
  Mathematical constants
  ------------------------------------------------------------------ }

const
  Pi         = 3.14159265358979323846;  { Pi }
  Ln2        = 0.69314718055994530942;  { Ln(2) }
  Ln10       = 2.30258509299404568402;  { Ln(10) }
  LnPi       = 1.14472988584940017414;  { Ln(Pi) }
  InvLn2     = 1.44269504088896340736;  { 1/Ln(2) }
  InvLn10    = 0.43429448190325182765;  { 1/Ln(10) }
  TwoPi      = 6.28318530717958647693;  { 2*Pi }
  PiDiv2     = 1.57079632679489661923;  { Pi/2 }
  SqrtPi     = 1.77245385090551602730;  { Sqrt(Pi) }
  Sqrt2Pi    = 2.50662827463100050242;  { Sqrt(2*Pi) }
  InvSqrt2Pi = 0.39894228040143267794;  { 1/Sqrt(2*Pi) }
  LnSqrt2Pi  = 0.91893853320467274178;  { Ln(Sqrt(2*Pi)) }
  Ln2PiDiv2  = 0.91893853320467274178;  { Ln(2*Pi)/2 }
  Sqrt2      = 1.41421356237309504880;  { Sqrt(2) }
  Sqrt2Div2  = 0.70710678118654752440;  { Sqrt(2)/2 }
  Gold       = 1.61803398874989484821;  { Golden Mean = (1 + Sqrt(5))/2 }
  CGold      = 0.38196601125010515179;  { 2 - GOLD }

{ ------------------------------------------------------------------
  Machine-dependent constants
  ------------------------------------------------------------------ }

{$IFDEF SINGLEREAL}
const
  MachEp = 1.192093E-7;   { Floating point precision: 2^(-23) }
  MaxNum = 3.402823E+38;  { Max. floating point number: 2^128 }
  MinNum = 1.175495E-38;  { Min. floating point number: 2^(-126) }
  MaxLog = 88.72283;      { Max. argument for Exp = Ln(MaxNum) }
  MinLog = -87.33655;     { Min. argument for Exp = Ln(MinNum) }
  MaxFac = 33;            { Max. argument for Factorial }
  MaxGam = 34.648;        { Max. argument for Gamma }
  MaxLgm = 1.0383E+36;    { Max. argument for LnGamma }
{$ENDIF}

{$IFDEF DOUBLEREAL}
const
  MachEp = 2.220446049250313E-16;   { 2^(-52) }
  MaxNum = 1.797693134862315E+308;  { 2^1024 }
  MinNum = 2.225073858507202E-308;  { 2^(-1022) }
  MaxLog = 709.7827128933840;
  MinLog = -708.3964185322641;
  MaxFac = 170;
  MaxGam = 171.624376956302;
  MaxLgm = 2.556348E+305;
{$ENDIF}

{$IFDEF EXTENDEDREAL}
const
  MachEp = 1.08420217248550444E-19;      { 2^(-63) }
  MaxNum = 5.9486574767861588254E+4931;  { 2^16383 }
  MinNum = 6.7242062862241870125E-4932;  { 2^(-16381) }
  MaxLog = 11355.830259113584004;
  MinLog = -11354.443964752464114;

  MaxFac = 1754;
  MaxGam = 1755.455;
  MaxLgm = 1.04848146839019521E+4928;
{$ENDIF}

{ ------------------------------------------------------------------
  Error codes for mathematical functions
  ------------------------------------------------------------------ }

const
  FOk        =   0;  { No error }
  FDomain    = - 1;  { Argument domain error }
  FSing      = - 2;  { Function singularity }
  FOverflow  = - 3;  { Overflow range error }
  FUnderflow = - 4;  { Underflow range error }
  FTLoss     = - 5;  { Total loss of precision }
  FPLoss     = - 6;  { Partial loss of precision }

{ ------------------------------------------------------------------
  Error codes for matrix computations
  ------------------------------------------------------------------ }

const
  MatOk      =  0;  { No error }
  MatNonConv = -1;  { Non-convergence }
  MatSing    = -2;  { Quasi-singular matrix }
  MatErrDim  = -3;  { Non-compatible dimensions }
  MatNotPD   = -4;  { Matrix not positive definite }

{ ------------------------------------------------------------------
  Error codes for optimization and nonlinear equations
  ------------------------------------------------------------------ }

const
  OptOk        =  0;  { No error }
  OptNonConv   = -1;  { Non-convergence }
  OptSing      = -2;  { Quasi-singular hessian matrix }
  OptBigLambda = -5;  { Too high Marquardt parameter }

{ ------------------------------------------------------------------
  Error codes for nonlinear regression
  ------------------------------------------------------------------ }

const
  NLMaxPar = -6;  { Max. number of parameters exceeded }

{ ------------------------------------------------------------------
  Complex numbers
  ------------------------------------------------------------------ }

type Complex = record
  X, Y : Float;
end;

{ ------------------------------------------------------------------
  Vectors and matrices.
  ------------------------------------------------------------------ }

const                    { Maximal array size }
{$IFDEF _16BIT}
  MaxSize = 65536;       { 64 kilobytes : 2^16 }
{$ELSE}
  MaxSize = 2147483648;  { 2 gigabytes : 2^31 }
{$ENDIF}

  FltSize  = SizeOf(Float);
  CompSize = SizeOf(Complex);
  IntSize  = SizeOf(Integer);
  BoolSize = SizeOf(Boolean);
  StrSize  = SizeOf(String);
  PtrSize  = SizeOf(Pointer);

  MAX_FLT  = Trunc(MaxSize / FltSize)  - 2;
  MAX_COMP = Trunc(MaxSize / CompSize) - 2;
  MAX_INT  = Trunc(MaxSize / IntSize)  - 2;
  MAX_BOOL = Trunc(MaxSize / BoolSize) - 2;
  MAX_STR  = Trunc(MaxSize / StrSize)  - 2;
  MAX_VEC  = Trunc(MaxSize / PtrSize)  - 2;

type
  TVector     = array[0..MAX_FLT]  of Float;
  TIntVector  = array[0..MAX_INT]  of Integer;
  TCompVector = array[0..MAX_COMP] of Complex;
  TBoolVector = array[0..MAX_BOOL] of Boolean;
  TStrVector  = array[0..MAX_STR]  of String;

  PVector     = ^TVector;
  PIntVector  = ^TIntVector;
  PBoolVector = ^TBoolVector;
  PCompVector = ^TCompVector;
  PStrVector  = ^TStrVector;

type
  XTMatrix     = array[0..MAX_VEC] of PVector;
  TIntMatrix  = array[0..MAX_VEC] of PIntVector;
  TBoolMatrix = array[0..MAX_VEC] of PBoolVector;
  TCompMatrix = array[0..MAX_VEC] of PCompVector;
  TStrMatrix  = array[0..MAX_VEC] of PStrVector;

  PMatrix     = ^XTMatrix;
  PIntMatrix  = ^TIntMatrix;
  PBoolMatrix = ^TBoolMatrix;
  PCompMatrix = ^TCompMatrix;
  PStrMatrix  = ^TStrMatrix;

{ ------------------------------------------------------------------
  Functional types
  ------------------------------------------------------------------ }

{ Function of one variable }
type TFunc = function(X : Float) : Float;

{ Function of several variables }
type TFuncNVar = function(X : PVector) : Float;

{ Nonlinear equation system }
type TEquations = procedure(X, F : PVector);

{ Differential equation system }
type TDiffEqs = procedure(X : Float; Y, Yp : PVector);

{ Jacobian }
type TJacobian = procedure(X : PVector; D : PMatrix);

{ Gradient }
type TGradient = procedure(X, G : PVector);

{ Hessian and Gradient }
type THessGrad = procedure(X, G : PVector; H : PMatrix);

{ ------------------------------------------------------------------
  Random number generators
  ------------------------------------------------------------------ }

type RNG_Type =
  (RNG_MWC,      { Multiply-With-Carry }
   RNG_MT,       { Mersenne Twister }
   RNG_UVAG);    { Universal Virtual Array Generator }

{ ------------------------------------------------------------------
  Statistics
  ------------------------------------------------------------------ }

type StatClass = record  { Statistical class }
  Inf : Float;           { Lower bound }
  Sup : Float;           { Upper bound }
  N   : Integer;         { Number of values }
  F   : Float;           { Frequency }
  D   : Float;           { Density }
end;

const
  MAX_CLS = 1000;  { Max. number of statistical classes }

type
  TStatClassVector = array[0..MAX_CLS]  of StatClass;
  PStatClassVector = ^TStatClassVector;

{ ------------------------------------------------------------------
  Curve fit
  ------------------------------------------------------------------ }

type
  TRegTest = record      { Test of regression }
    Vr       : Float;    { Residual variance }
    R2       : Float;    { Coefficient of determination }
    R2a      : Float;    { Adjusted coeff. of determination }
    F        : Float;    { Variance ratio (explained/residual) }
    Nu1, Nu2 : Integer;  { Degrees of freedom }
  end;

{ Optimization algorithms for nonlinear regression }
type
  TOptAlgo = (
    NL_MARQ,       { Marquardt algorithm }
    NL_SIMP,       { Simplex algorithm }
    NL_BFGS,       { BFGS algorithm }
    NL_SA,         { Simulated annealing }
    NL_GA);        { Genetic algorithm }

{ Regression function }
type
  TRegFunc = function(X : Float; B : PVector) : Float;

{ Procedure to compute the derivatives of the regression function
  with respect to the regression parameters }
type
  TDerivProc = procedure(X, Y : Float; B, D : PVector);

{ ------------------------------------------------------------------
  Graphics
  ------------------------------------------------------------------ }

type
  Str30  = String[30];
  TScale = (LinScale, LogScale);
  TGrid  = (NoGrid, HorizGrid, VertiGrid, BothGrid);
