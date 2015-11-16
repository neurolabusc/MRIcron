{ ******************************************************************
  Unit TPMATH - Interface for TPMATH.DLL
  ****************************************************************** }

unit tpmath;

interface

{ ------------------------------------------------------------------
  Types and constants
  ------------------------------------------------------------------ }

{$i types.inc}

{ ------------------------------------------------------------------
  Error handling
  ------------------------------------------------------------------ }

procedure SetErrCode(ErrCode : Integer); external 'tpmath';
{ Sets the error code }

function DefaultVal(ErrCode : Integer; DefVal : Float) : Float; external 'tpmath';
{ Sets error code and default function value }

function MathErr : Integer; external 'tpmath';
{ Returns the error code }

{ ------------------------------------------------------------------
  Dynamic arrays
  ------------------------------------------------------------------ }

procedure SetAutoInit(AutoInit : Boolean); external 'tpmath';
{ Sets the auto-initialization of arrays }

procedure DimVector(var V : PVector; Ub : Integer); external 'tpmath';
{ Creates floating point vector V[0..Ub] }

procedure DimIntVector(var V : PIntVector; Ub : Integer); external 'tpmath';
{ Creates integer vector V[0..Ub] }

procedure DimCompVector(var V : PCompVector; Ub : Integer); external 'tpmath';
{ Creates complex vector V[0..Ub] }

procedure DimBoolVector(var V : PBoolVector; Ub : Integer); external 'tpmath';
{ Creates boolean vector V[0..Ub] }

procedure DimStrVector(var V : PStrVector; Ub : Integer); external 'tpmath';
{ Creates string vector V[0..Ub] }

procedure DimMatrix(var A : PMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Creates floating point matrix A[0..Ub1, 0..Ub2] }

procedure DimIntMatrix(var A : PIntMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Creates integer matrix A[0..Ub1, 0..Ub2] }

procedure DimCompMatrix(var A : PCompMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Creates complex matrix A[0..Ub1, 0..Ub2] }

procedure DimBoolMatrix(var A : PBoolMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Creates boolean matrix A[0..Ub1, 0..Ub2] }

procedure DimStrMatrix(var A : PStrMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Creates string matrix A[0..Ub1, 0..Ub2] }

procedure DelVector(var V : PVector; Ub : Integer); external 'tpmath';
{ Deletes floating point vector V[0..Ub] }

procedure DelIntVector(var V : PIntVector; Ub : Integer); external 'tpmath';
{ Deletes integer vector V[0..Ub] }

procedure DelCompVector(var V : PCompVector; Ub : Integer); external 'tpmath';
{ Deletes complex vector V[0..Ub] }

procedure DelBoolVector(var V : PBoolVector; Ub : Integer); external 'tpmath';
{ Deletes boolean vector V[0..Ub] }

procedure DelStrVector(var V : PStrVector; Ub : Integer); external 'tpmath';
{ Deletes string vector V[0..Ub] }

procedure DelMatrix(var A : PMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Deletes floating point matrix A[0..Ub1, 0..Ub2] }

procedure DelIntMatrix(var A : PIntMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Deletes integer matrix A[0..Ub1, 0..Ub2] }

procedure DelCompMatrix(var A : PCompMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Deletes complex matrix A[0..Ub1, 0..Ub2] }

procedure DelBoolMatrix(var A : PBoolMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Deletes boolean matrix A[0..Ub1, 0..Ub2] }

procedure DelStrMatrix(var A : PStrMatrix; Ub1, Ub2 : Integer); external 'tpmath';
{ Deletes string matrix A[0..Ub1, 0..Ub2] }

{ ------------------------------------------------------------------
  Minimum, maximum, sign and exchange
  ------------------------------------------------------------------ }

function FMin(X, Y : Float) : Float; external 'tpmath';
{ Minimum of 2 reals }

function FMax(X, Y : Float) : Float; external 'tpmath';
{ Maximum of 2 reals }

function IMin(X, Y : Integer) : Integer; external 'tpmath';
{ Minimum of 2 integers }

function IMax(X, Y : Integer) : Integer; external 'tpmath';
{ Maximum of 2 integers }

function Sgn(X : Float) : Integer; external 'tpmath';
{ Sign (returns 1 if X = 0) }

function Sgn0(X : Float) : Integer; external 'tpmath';
{ Sign (returns 0 if X = 0) }

function DSgn(A, B : Float) : Float; external 'tpmath';
{ Sgn(B) * |A| }

procedure FSwap(var X, Y : Float); external 'tpmath';
{ Exchange 2 reals }

procedure ISwap(var X, Y : Integer); external 'tpmath';
{ Exchange 2 integers }

{ ------------------------------------------------------------------
  Rounding functions
  ------------------------------------------------------------------ }

function RoundN(X : Float; N : Integer) : Float; external 'tpmath';
{ Rounds X to N decimal places }

function Ceil(X : Float) : Integer; external 'tpmath';
{ Ceiling function }

function Floor(X : Float) : Integer; external 'tpmath';
{ Floor function }

{ ------------------------------------------------------------------
  Logarithms, exponentials and power
  ------------------------------------------------------------------ }

function Expo(X : Float) : Float; external 'tpmath';
{ Exponential }

function Exp2(X : Float) : Float; external 'tpmath';
{ 2^X }

function Exp10(X : Float) : Float; external 'tpmath';
{ 10^X }

function Log(X : Float) : Float; external 'tpmath';
{ Natural log }

function Log2(X : Float) : Float; external 'tpmath';
{ Log, base 2 }

function Log10(X : Float) : Float; external 'tpmath';
{ Decimal log }

function LogA(X, A : Float) : Float; external 'tpmath';
{ Log, base A }

function IntPower(X : Float; N : Integer) : Float; external 'tpmath';
{ X^N }

function Power(X, Y : Float) : Float; external 'tpmath';
{ X^Y, X >= 0 }

{ ------------------------------------------------------------------
  Trigonometric functions
  ------------------------------------------------------------------ }

function Pythag(X, Y : Float) : Float; external 'tpmath';
{ Sqrt(X^2 + Y^2) }

function FixAngle(Theta : Float) : Float; external 'tpmath';
{ Set Theta in -Pi..Pi }

function Tan(X : Float) : Float; external 'tpmath';
{ Tangent }

function ArcSin(X : Float) : Float; external 'tpmath';
{ Arc sinus }

function ArcCos(X : Float) : Float; external 'tpmath';
{ Arc cosinus }

function ArcTan2(Y, X : Float) : Float; external 'tpmath';
{ Angle (Ox, OM) with M(X,Y) }

{ ------------------------------------------------------------------
  Hyperbolic functions
  ------------------------------------------------------------------ }

function Sinh(X : Float) : Float; external 'tpmath';
{ Hyperbolic sine }

function Cosh(X : Float) : Float; external 'tpmath';
{ Hyperbolic cosine }

function Tanh(X : Float) : Float; external 'tpmath';
{ Hyperbolic tangent }

function ArcSinh(X : Float) : Float; external 'tpmath';
{ Inverse hyperbolic sine }

function ArcCosh(X : Float) : Float; external 'tpmath';
{ Inverse hyperbolic cosine }

function ArcTanh(X : Float) : Float; external 'tpmath';
{ Inverse hyperbolic tangent }

procedure SinhCosh(X : Float; var SinhX, CoshX : Float); external 'tpmath';
{ Sinh & Cosh }

{ ------------------------------------------------------------------
  Gamma function and related functions
  ------------------------------------------------------------------ }

function Fact(N : Integer) : Float; external 'tpmath';
{ Factorial }

function SgnGamma(X : Float) : Integer; external 'tpmath';
{ Sign of Gamma function }

function Gamma(X : Float) : Float; external 'tpmath';
{ Gamma function }

function LnGamma(X : Float) : Float; external 'tpmath';
{ Logarithm of Gamma function }

function Stirling(X : Float) : Float; external 'tpmath';
{ Stirling's formula for the Gamma function }

function StirLog(X : Float) : Float; external 'tpmath';
{ Approximate Ln(Gamma) by Stirling's formula, for X >= 13 }

function DiGamma(X : Float ) : Float; external 'tpmath';
{ Digamma function }

function TriGamma(X : Float ) : Float; external 'tpmath';
{ Trigamma function }

function IGamma(A, X : Float) : Float; external 'tpmath';
{ Incomplete Gamma function}

function JGamma(A, X : Float) : Float; external 'tpmath';
{ Complement of incomplete Gamma function }

function InvGamma(A, P : Float) : Float; external 'tpmath';
{ Inverse of incomplete Gamma function }

function Erf(X : Float) : Float; external 'tpmath';
{ Error function }

function Erfc(X : Float) : Float; external 'tpmath';
{ Complement of error function }

{ ------------------------------------------------------------------
  Beta function and related functions
  ------------------------------------------------------------------ }

function Beta(X, Y : Float) : Float; external 'tpmath';
{ Beta function }

function IBeta(A, B, X : Float) : Float; external 'tpmath';
{ Incomplete Beta function }

function InvBeta(A, B, Y : Float) : Float; external 'tpmath';
{ Inverse of incomplete Beta function }

{ ------------------------------------------------------------------
  Lambert's function
  ------------------------------------------------------------------ }

function LambertW(X : Float; UBranch, Offset : Boolean) : Float; external 'tpmath';

{ ------------------------------------------------------------------
  Binomial distribution
  ------------------------------------------------------------------ }

function Binomial(N, K : Integer) : Float; external 'tpmath';
{ Binomial coefficient C(N,K) }

function PBinom(N : Integer; P : Float; K : Integer) : Float; external 'tpmath';
{ Probability of binomial distribution }

function FBinom(N : Integer; P : Float; K : Integer) : Float; external 'tpmath';
{ Cumulative probability for binomial distrib. }

{ ------------------------------------------------------------------
  Poisson distribution
  ------------------------------------------------------------------ }

function PPoisson(Mu : Float; K : Integer) : Float; external 'tpmath';
{ Probability of Poisson distribution }

function FPoisson(Mu : Float; K : Integer) : Float; external 'tpmath';
{ Cumulative probability for Poisson distrib. }

{ ------------------------------------------------------------------
  Exponential distribution
  ------------------------------------------------------------------ }

function DExpo(A, X : Float) : Float; external 'tpmath';
{ Density of exponential distribution with parameter A }

function FExpo(A, X : Float) : Float; external 'tpmath';
{ Cumulative probability function for exponential dist. with parameter A }

{ ------------------------------------------------------------------
  Standard normal distribution
  ------------------------------------------------------------------ }

function DNorm(X : Float) : Float; external 'tpmath';
{ Density of standard normal distribution }

function FNorm(X : Float) : Float; external 'tpmath';
{ Cumulative probability for standard normal distrib. }

function PNorm(X : Float) : Float; external 'tpmath';
{ Prob(|U| > X) for standard normal distrib. }

function InvNorm(P : Float) : Float; external 'tpmath';
{ Inverse of standard normal distribution }

{ ------------------------------------------------------------------
  Student's distribution
  ------------------------------------------------------------------ }

function DStudent(Nu : Integer; X : Float) : Float; external 'tpmath';
{ Density of Student distribution with Nu d.o.f. }

function FStudent(Nu : Integer; X : Float) : Float; external 'tpmath';
{ Cumulative probability for Student distrib. with Nu d.o.f. }

function PStudent(Nu : Integer; X : Float) : Float; external 'tpmath';
{ Prob(|t| > X) for Student distrib. with Nu d.o.f. }

function InvStudent(Nu : Integer; P : Float) : Float; external 'tpmath';
{ Inverse of Student's t-distribution function }

{ ------------------------------------------------------------------
  Khi-2 distribution
  ------------------------------------------------------------------ }

function DKhi2(Nu : Integer; X : Float) : Float; external 'tpmath';
{ Density of Khi-2 distribution with Nu d.o.f. }

function FKhi2(Nu : Integer; X : Float) : Float; external 'tpmath';
{ Cumulative prob. for Khi-2 distrib. with Nu d.o.f. }

function PKhi2(Nu : Integer; X : Float) : Float; external 'tpmath';
{ Prob(Khi2 > X) for Khi-2 distrib. with Nu d.o.f. }

function InvKhi2(Nu : Integer; P : Float) : Float; external 'tpmath';
{ Inverse of Khi-2 distribution function }

{ ------------------------------------------------------------------
  Fisher-Snedecor distribution
  ------------------------------------------------------------------ }

function DSnedecor(Nu1, Nu2 : Integer; X : Float) : Float; external 'tpmath';
{ Density of Fisher-Snedecor distribution with Nu1 and Nu2 d.o.f. }

function FSnedecor(Nu1, Nu2 : Integer; X : Float) : Float; external 'tpmath';
{ Cumulative prob. for Fisher-Snedecor distrib. with Nu1 and Nu2 d.o.f. }

function PSnedecor(Nu1, Nu2 : Integer; X : Float) : Float; external 'tpmath';
{ Prob(F > X) for Fisher-Snedecor distrib. with Nu1 and Nu2 d.o.f. }

function InvSnedecor(Nu1, Nu2 : Integer; P : Float) : Float; external 'tpmath';
{ Inverse of Snedecor's F-distribution function }

{ ------------------------------------------------------------------
  Beta distribution
  ------------------------------------------------------------------ }

function DBeta(A, B, X : Float) : Float; external 'tpmath';
{ Density of Beta distribution with parameters A and B }

function FBeta(A, B, X : Float) : Float; external 'tpmath';
{ Cumulative probability for Beta distrib. with param. A and B }

{ ------------------------------------------------------------------
  Gamma distribution
  ------------------------------------------------------------------ }

function DGamma(A, B, X : Float) : Float; external 'tpmath';
{ Density of Gamma distribution with parameters A and B }

function FGamma(A, B, X : Float) : Float; external 'tpmath';
{ Cumulative probability for Gamma distrib. with param. A and B }

{ ------------------------------------------------------------------
  Matrices and linear equations
  ------------------------------------------------------------------ }

procedure GaussJordan(A            : PMatrix;
                      Lb, Ub1, Ub2 : Integer;
                      var Det      : Float); external 'tpmath';
{ Transforms a matrix according to the Gauss-Jordan method }

procedure LinEq(A       : PMatrix;
                B       : PVector;
                Lb, Ub  : Integer;
                var Det : Float); external 'tpmath';
{ Solves a linear system according to the Gauss-Jordan method }

procedure Cholesky(A, L : PMatrix; Lb, Ub : Integer); external 'tpmath';
{ Cholesky factorization of a positive definite symmetric matrix }

procedure LU_Decomp(A : PMatrix; Lb, Ub : Integer); external 'tpmath';
{ LU decomposition }

procedure LU_Solve(A      : PMatrix;
                   B      : PVector;
                   Lb, Ub : Integer;
                   X      : PVector); external 'tpmath';
{ Solution of linear system from LU decomposition }

procedure QR_Decomp(A            : PMatrix;
                    Lb, Ub1, Ub2 : Integer;
                    R            : PMatrix); external 'tpmath';
{ QR decomposition }

procedure QR_Solve(Q, R         : PMatrix;
                   B            : PVector;
                   Lb, Ub1, Ub2 : Integer;
                   X            : PVector); external 'tpmath';
{ Solution of linear system from QR decomposition }

procedure SV_Decomp(A            : PMatrix;
                    Lb, Ub1, Ub2 : Integer;
                    S            : PVector;
                    V            : PMatrix); external 'tpmath';
{ Singular value decomposition }

procedure SV_SetZero(S      : PVector;
                     Lb, Ub : Integer;
                     Tol    : Float); external 'tpmath';
{ Set lowest singular values to zero }

procedure SV_Solve(U            : PMatrix;
                   S            : PVector;
                   V            : PMatrix;
                   B            : PVector;
                   Lb, Ub1, Ub2 : Integer;
                   X            : PVector); external 'tpmath';
{ Solution of linear system from SVD }

procedure SV_Approx(U            : PMatrix;
                    S            : PVector;
                    V            : PMatrix;
                    Lb, Ub1, Ub2 : Integer;
                    A            : PMatrix); external 'tpmath';
{ Matrix approximation from SVD }

procedure EigenVals(A      : PMatrix;
                    Lb, Ub : Integer;
                    Lambda : PCompVector); external 'tpmath';
{ Eigenvalues of a general square matrix }

procedure EigenVect(A      : PMatrix;
                    Lb, Ub : Integer;
                    Lambda : PCompVector;
                    V      : PMatrix); external 'tpmath';
{ Eigenvalues and eigenvectors of a general square matrix }

procedure Jacobi(A               : PMatrix;
                 Lb, Ub, MaxIter : Integer;
                 Tol             : Float;
                 Lambda          : PVector;
                 V               : PMatrix); external 'tpmath';
{ Eigenvalues and eigenvectors of a symmetric matrix }

{ ------------------------------------------------------------------
  Optimization
  ------------------------------------------------------------------ }

procedure MinBrack(Func                    : TFunc;
                   var A, B, C, Fa, Fb, Fc : Float); external 'tpmath';
{ Brackets a minimum of a function }

procedure GoldSearch(Func           : TFunc;
                     A, B           : Float;
                     MaxIter        : Integer;
                     Tol            : Float;
                     var Xmin, Ymin : Float); external 'tpmath';
{ Minimization of a function of one variable (golden search) }

procedure LinMin(Func      : TFuncNVar;
                 X, DeltaX : PVector;
                 Lb, Ub    : Integer;
                 var R     : Float;
                 MaxIter   : Integer;
                 Tol       : Float;
                 var F_min : Float); external 'tpmath';
{ Minimization of a function of several variables along a line }

procedure Newton(Func      : TFuncNVar;
                 HessGrad  : THessGrad;
                 X         : PVector;
                 Lb, Ub    : Integer;
                 MaxIter   : Integer;
                 Tol       : Float;
                 var F_min : Float;
                 G         : PVector;
                 H_inv     : PMatrix;
                 var Det   : Float); external 'tpmath';
{ Minimization of a function of several variables (Newton's method) }

procedure SaveNewton(FileName : string); external 'tpmath';
{ Save Newton iterations in a file }

procedure Marquardt(Func      : TFuncNVar;
                    HessGrad  : THessGrad;
                    X         : PVector;
                    Lb, Ub    : Integer;
                    MaxIter   : Integer;
                    Tol       : Float;
                    var F_min : Float;
                    G         : PVector;
                    H_inv     : PMatrix;
                    var Det   : Float); external 'tpmath';
{ Minimization of a function of several variables (Marquardt's method) }

procedure SaveMarquardt(FileName : string); external 'tpmath';
{ Save Marquardt iterations in a file }

procedure BFGS(Func      : TFuncNVar;
               Gradient  : TGradient;
               X         : PVector;
               Lb, Ub    : Integer;
               MaxIter   : Integer;
               Tol       : Float;
               var F_min : Float;
               G         : PVector;
               H_inv     : PMatrix); external 'tpmath';
{ Minimization of a function of several variables (BFGS method) }

procedure SaveBFGS(FileName : string); external 'tpmath';
{ Save BFGS iterations in a file }

procedure Simplex(Func      : TFuncNVar;
                  X         : PVector;
                  Lb, Ub    : Integer;
                  MaxIter   : Integer;
                  Tol       : Float;
                  var F_min : Float); external 'tpmath';
{ Minimization of a function of several variables (Simplex) }

procedure SaveSimplex(FileName : string); external 'tpmath';
{ Save Simplex iterations in a file }

{ ------------------------------------------------------------------
  Nonlinear equations
  ------------------------------------------------------------------ }

procedure RootBrack(Func             : TFunc;
                    var X, Y, FX, FY : Float); external 'tpmath';
{ Brackets a root of function Func between X and Y }

procedure Bisect(Func     : TFunc;
                 var X, Y : Float;
                 MaxIter  : Integer;
                 Tol      : Float;
                 var F    : Float); external 'tpmath';
{ Bisection method }

procedure Secant(Func     : TFunc;
                 var X, Y : Float;
                 MaxIter  : Integer;
                 Tol      : Float;
                 var F    : Float); external 'tpmath';
{ Secant method }

procedure NewtEq(Func, Deriv : TFunc;
                 var X       : Float;
                 MaxIter     : Integer;
                 Tol         : Float;
                 var F       : Float); external 'tpmath';
{ Newton-Raphson method for a single nonlinear equation }

procedure NewtEqs(Equations : TEquations;
                  Jacobian  : TJacobian;
                  X, F      : PVector;
                  Lb, Ub    : Integer;
                  MaxIter   : Integer;
                  Tol       : Float); external 'tpmath';
{ Newton-Raphson method for a system of nonlinear equations }

procedure Broyden(Equations : TEquations;
                  X, F      : PVector;
                  Lb, Ub    : Integer;
                  MaxIter   : Integer;
                  Tol       : Float); external 'tpmath';
{ Broyden's method for a system of nonlinear equations }

{ ------------------------------------------------------------------
  Polynomials and rational fractions
  ------------------------------------------------------------------ }

function Poly(X    : Float;
              Coef : PVector;
              Deg  : Integer) : Float; external 'tpmath';
{ Evaluates a polynomial }

function RFrac(X          : Float;
               Coef       : PVector;
               Deg1, Deg2 : Integer) : Float; external 'tpmath';
{ Evaluates a rational fraction }

function RootPol1(A, B  : Float;
                  var X : Float) : Integer; external 'tpmath';
{ Solves the linear equation A + B * X = 0 }

function RootPol2(Coef : PVector;
                  Z    : PCompVector) : Integer; external 'tpmath';
{ Solves a quadratic equation }

function RootPol3(Coef : PVector;
                  Z    : PCompVector) : Integer; external 'tpmath';
{ Solves a cubic equation }

function RootPol4(Coef : PVector;
                  Z    : PCompVector) : Integer; external 'tpmath';
{ Solves a quartic equation }

function RootPol(Coef : PVector;
                 Deg  : Integer;
                 Z    : PCompVector) : Integer; external 'tpmath';
{ Solves a polynomial equation }

function SetRealRoots(Deg : Integer;
                      Z   : PCompVector;
                      Tol : Float) : Integer; external 'tpmath';
{ Set the imaginary part of a root to zero }

procedure SortRoots(Deg : Integer;
                    Z   : PCompVector); external 'tpmath';
{ Sorts the roots of a polynomial }

{ ------------------------------------------------------------------
  Numerical integration and differential equations
  ------------------------------------------------------------------ }

function TrapInt(X, Y : PVector; N : Integer) : Float; external 'tpmath';
{ Integration by trapezoidal rule }

function GausLeg(Func : TFunc; A, B : Float) : Float; external 'tpmath';
{ Integral from A to B }

function GausLeg0(Func : TFunc; B : Float) : Float; external 'tpmath';
{ Integral from 0 to B }

function Convol(Func1, Func2 : TFunc; T : Float) : Float; external 'tpmath';
{ Convolution product at time T }

procedure RKF45(F                    : TDiffEqs;
                Neqn                 : Integer;
                Y, Yp                : PVector;
                var T                : Float;
                Tout, RelErr, AbsErr : Float;
                var Flag             : Integer); external 'tpmath';
{ Integration of a system of differential equations }

{ ------------------------------------------------------------------
  Fast Fourier Transform
  ------------------------------------------------------------------ }

procedure FFT(NumSamples        : LongInt;
              InArray, OutArray : PCompVector); external 'tpmath';
{ Fast Fourier Transform }

procedure IFFT(NumSamples        : LongInt;
               InArray, OutArray : PCompVector); external 'tpmath';
{ Inverse Fast Fourier Transform }

procedure FFT_Integer(NumSamples     : LongInt;
                      RealIn, ImagIn : PIntVector;
                      OutArray       : PCompVector); external 'tpmath';
{ Fast Fourier Transform for integer data }

procedure FFT_Integer_Cleanup; external 'tpmath';
{ Clear memory after a call to FFT_Integer }

procedure CalcFrequency(NumSamples,
                        FrequencyIndex : LongInt;
                        InArray        : PCompVector;
                        var FFT        : Complex); external 'tpmath';
{ Direct computation of Fourier transform }

{ ------------------------------------------------------------------
  Random numbers
  ------------------------------------------------------------------ }

procedure SetRNG(RNG : RNG_Type); external 'tpmath';
{ Select generator }

procedure InitGen(Seed : LongInt); external 'tpmath';
{ Initialize generator }

function IRanGen : LongInt; external 'tpmath';
{ 32-bit random integer in [-2^31 .. 2^31 - 1] }

function IRanGen31 : LongInt; external 'tpmath';
{ 31-bit random integer in [0 .. 2^31 - 1] }

function RanGen1 : Float; external 'tpmath';
{ 32-bit random real in [0,1] }

function RanGen2 : Float; external 'tpmath';
{ 32-bit random real in [0,1) }

function RanGen3 : Float; external 'tpmath';
{ 32-bit random real in (0,1) }

function RanGen53 : Float; external 'tpmath';
{ 53-bit random real in [0,1) }

procedure InitMWC(Seed : LongInt); external 'tpmath';
{ Initializes the 'Multiply with carry' random number generator }

function IRanMWC : LongInt; external 'tpmath';
{ Returns a 32 bit random number in [-2^31 ; 2^31-1] }

procedure InitMT(Seed : LongInt); external 'tpmath';
{ Initializes Mersenne Twister generator with a seed }

procedure InitMTbyArray(InitKey : array of LongInt; KeyLength : Word); external 'tpmath';
{ Initialize MT generator with an array InitKey[0..(KeyLength - 1)] }

function IRanMT : LongInt; external 'tpmath';
{ Random integer from MT generator }

procedure InitUVAGbyString(KeyPhrase : string); external 'tpmath';
{ Initializes the UVAG generator with a string }

procedure InitUVAG(Seed : LongInt); external 'tpmath';
{ Initializes the UVAG generator with an integer }

function IRanUVAG : LongInt; external 'tpmath';
{ Random integer from UVAG generator }

function RanGaussStd : Float; external 'tpmath';
{ Random number from standard normal distribution }

function RanGauss(Mu, Sigma : Float) : Float; external 'tpmath';
{ Random number from normal distrib. with mean Mu and S. D. Sigma }

procedure RanMult(M      : PVector;
                  L      : PMatrix;
                  Lb, Ub : Integer;
                  X      : PVector); external 'tpmath';
{ Random vector from multinormal distribution (correlated) }

procedure RanMultIndep(M, S   : PVector;
                       Lb, Ub : Integer;
                       X      : PVector); external 'tpmath';
{ Random vector from multinormal distribution (uncorrelated) }

procedure InitMHParams(NCycles, MaxSim, SavedSim : Integer); external 'tpmath';
{ Initializes Metropolis-Hastings parameters }

procedure GetMHParams(var NCycles, MaxSim, SavedSim : Integer); external 'tpmath';
{ Returns Metropolis-Hastings parameters }

procedure Hastings(Func      : TFuncNVar;
                   T         : Float;
                   X         : PVector;
                   V         : PMatrix;
                   Lb, Ub    : Integer;
                   Xmat      : PMatrix;
                   X_min     : PVector;
                   var F_min : Float); external 'tpmath';
{ Simulation of a probability density function by Metropolis-Hastings }

procedure InitSAParams(NT, NS, NCycles : Integer; RT : Float); external 'tpmath';
{ Initializes Simulated Annealing parameters }

procedure SA_CreateLogFile(FileName : String); external 'tpmath';
{ Initializes log file }

procedure SimAnn(Func          : TFuncNVar;
                 X, Xmin, Xmax : PVector;
                 Lb, Ub        : Integer;
                 var F_min     : Float); external 'tpmath';
{ Minimization of a function of several var. by simulated annealing }

procedure InitGAParams(NP, NG : Integer; SR, MR, HR : Float); external 'tpmath';
{ Initializes Genetic Algorithm parameters }

procedure GA_CreateLogFile(FileName : String); external 'tpmath';
{ Initializes log file }

procedure GenAlg(Func          : TFuncNVar;
                 X, Xmin, Xmax : PVector;
                 Lb, Ub        : Integer;
                 var F_min     : Float); external 'tpmath';
{ Minimization of a function of several var. by genetic algorithm }

{ ------------------------------------------------------------------
  Statistics
  ------------------------------------------------------------------ }

function Mean(X : PVector; Lb, Ub : Integer) : Float; external 'tpmath';
{ Mean of sample X }

function Median(X : PVector; Lb, Ub : Integer; Sorted : Boolean) : Float; external 'tpmath';
{ Median of sample X }

function StDev(X : PVector; Lb, Ub : Integer; M : Float) : Float; external 'tpmath';
{ Standard deviation estimated from sample X }

function StDevP(X : PVector; Lb, Ub : Integer; M : Float) : Float; external 'tpmath';
{ Standard deviation of population }

function Correl(X, Y : PVector; Lb, Ub : Integer) : Float; external 'tpmath';
{ Correlation coefficient }

function Skewness(X : PVector; Lb, Ub : Integer; M, Sigma : Float) : Float; external 'tpmath';
{ Skewness of sample X }

function Kurtosis(X : PVector; Lb, Ub : Integer; M, Sigma : Float) : Float; external 'tpmath';
{ Kurtosis of sample X }

procedure QSort(X : PVector; Lb, Ub : Integer); external 'tpmath';
{ Quick sort (ascending order) }

procedure DQSort(X : PVector; Lb, Ub : Integer); external 'tpmath';
{ Quick sort (descending order) }

procedure Interval(X1, X2             : Float;
                   MinDiv, MaxDiv     : Integer;
                   var Min, Max, Step : Float); external 'tpmath';
{ Determines an interval for a set of values }

procedure StudIndep(N1, N2         : Integer;
                    M1, M2, S1, S2 : Float;
                    var T          : Float;
                    var DoF        : Integer); external 'tpmath';
{ Student t-test for independent samples }

procedure StudPaired(X, Y    : PVector;
                     Lb, Ub  : Integer;
                     var T   : Float;
                     var DoF : Integer); external 'tpmath';
{ Student t-test for paired samples }

procedure AnOVa1(Ns               : Integer;
                 N                : PIntVector;
                 M, S             : PVector;
                 var V_f, V_r, F  : Float;
                 var DoF_f, DoF_r : Integer); external 'tpmath';
{ One-way analysis of variance }

procedure AnOVa2(NA, NB, Nobs : Integer;
                 M, S         : PMatrix;
                 V, F         : PVector;
                 DoF          : PIntVector); external 'tpmath';
{ Two-way analysis of variance }

procedure Snedecor(N1, N2         : Integer;
                   S1, S2         : Float;
                   var F          : Float;
                   var DoF1, DoF2 : Integer); external 'tpmath';
{ Snedecor's F-test (comparison of two variances) }

procedure Bartlett(Ns       : Integer;
                   N        : PIntVector;
                   S        : PVector;
                   var Khi2 : Float;
                   var DoF  : Integer); external 'tpmath';
{ Bartlett's test (comparison of several variances) }

procedure Mann_Whitney(N1, N2     : Integer;
                       X1, X2     : PVector;
                       var U, Eps : Float); external 'tpmath';
{ Mann-Whitney test}

procedure Wilcoxon(X, Y       : PVector;
                   Lb, Ub     : Integer;
                   var Ndiff  : Integer;
                   var T, Eps : Float); external 'tpmath';
{ Wilcoxon test }

procedure Kruskal_Wallis(Ns      : Integer;
                         N       : PIntVector;
                         X       : PMatrix;
                         var H   : Float;
                         var DoF : Integer); external 'tpmath';
{ Kruskal-Wallis test }

procedure Khi2_Conform(N_cls    : Integer;
                       N_estim  : Integer;
                       Obs      : PIntVector;
                       Calc     : PVector;
                       var Khi2 : Float;
                       var DoF  : Integer); external 'tpmath';
{ Khi-2 test for conformity }

procedure Khi2_Indep(N_lin    : Integer;
                     N_col    : Integer;
                     Obs      : PIntMatrix;
                     var Khi2 : Float;
                     var DoF  : Integer); external 'tpmath';
{ Khi-2 test for independence }

procedure Woolf_Conform(N_cls   : Integer;
                        N_estim : Integer;
                        Obs     : PIntVector;
                        Calc    : PVector;
                        var G   : Float;
                        var DoF : Integer); external 'tpmath';
{ Woolf's test for conformity }

procedure Woolf_Indep(N_lin   : Integer;
                      N_col   : Integer;
                      Obs     : PIntMatrix;
                      var G   : Float;
                      var DoF : Integer); external 'tpmath';
{ Woolf's test for independence }

procedure DimStatClassVector(var C : PStatClassVector; 
                             Ub    : Integer); external 'tpmath'; 
{ Allocates an array of statistical classes: C[0..Ub] }

procedure DelStatClassVector(var C : PStatClassVector; 
                             Ub    : Integer); external 'tpmath';
{ Deallocates an array of statistical classes: C[0..Ub] }

procedure Distrib(X       : PVector;
                  Lb, Ub  : Integer;
                  A, B, H : Float;
                  C       : PStatClassVector); external 'tpmath';
{ Distributes an array X[Lb..Ub] into statistical classes }

{ ------------------------------------------------------------------
  Curve fit
  ------------------------------------------------------------------ }

procedure LinFit(X, Y   : PVector;
                 Lb, Ub : Integer;
                 B      : PVector;
                 V      : PMatrix); external 'tpmath';
{ Linear regression : Y = B(0) + B(1) * X }

procedure WLinFit(X, Y, S : PVector;
                  Lb, Ub  : Integer;
                  B       : PVector;
                  V       : PMatrix); external 'tpmath';
{ Weighted linear regression : Y = B(0) + B(1) * X }

procedure PolFit(X, Y        : PVector;
                 Lb, Ub, Deg : Integer;
                 B           : PVector;
                 V           : PMatrix); external 'tpmath';
{ Linear regression : Y = B(0) + B(1) * X + B(2) * X² + ...}

procedure WPolFit(X, Y, S     : PVector;
                  Lb, Ub, Deg : Integer;
                  B           : PVector;
                  V           : PMatrix); external 'tpmath';
{ Weighted linear regression : Y = B(0) + B(1) * X + B(2) * X² + ...}

procedure MulFit(X            : PMatrix;
                 Y            : PVector;
                 Lb, Ub, Nvar : Integer;
                 ConsTerm     : Boolean;
                 B            : PVector;
                 V            : PMatrix); external 'tpmath';
{ Multiple linear regression by Gauss-Jordan method }

procedure WMulFit(X            : PMatrix;
                  Y, S         : PVector;
                  Lb, Ub, Nvar : Integer;
                  ConsTerm     : Boolean;
                  B            : PVector;
                  V            : PMatrix); external 'tpmath';
{ Weighted multiple linear regression by Gauss-Jordan method }

procedure SVDFit(X            : PMatrix;
                 Y            : PVector;
                 Lb, Ub, Nvar : Integer;
                 ConsTerm     : Boolean;
                 SVDTol       : Float;
                 B            : PVector;
                 V            : PMatrix); external 'tpmath';
{ Multiple linear regression by SVD method }

procedure WSVDFit(X            : PMatrix;
                  Y, S         : PVector;
                  Lb, Ub, Nvar : Integer;
                  ConsTerm     : Boolean;
                  SVDTol       : Float;
                  B            : PVector;
                  V            : PMatrix); external 'tpmath';
{ Weighted multiple linear regression by SVD method }

procedure SetOptAlgo(Algo : TOptAlgo); external 'tpmath';
{ Sets the optimization algorithm for nonlinear regression }

procedure SetMaxParam(N : Byte); external 'tpmath';
{ Sets the maximum number of regression parameters for nonlinear regression }

procedure SetParamBounds(I : Byte; ParamMin, ParamMax : Float); external 'tpmath';
{ Sets the bounds on the I-th regression parameter }

procedure NLFit(RegFunc   : TRegFunc;
                DerivProc : TDerivProc;
                X, Y      : PVector;
                Lb, Ub    : Integer;
                MaxIter   : Integer;
                Tol       : Float;
                B         : PVector;
                FirstPar,
                LastPar   : Integer;
                V         : PMatrix); external 'tpmath';
{ Unweighted nonlinear regression }

procedure WNLFit(RegFunc   : TRegFunc;
                 DerivProc : TDerivProc;
                 X, Y, S   : PVector;
                 Lb, Ub    : Integer;
                 MaxIter   : Integer;
                 Tol       : Float;
                 B         : PVector;
                 FirstPar,
                 LastPar   : Integer;
                 V         : PMatrix); external 'tpmath';
{ Weighted nonlinear regression }

procedure SetMCFile(FileName : String); external 'tpmath';
{ Set file for saving MCMC simulations }

procedure SimFit(RegFunc   : TRegFunc;
                 X, Y      : PVector;
                 Lb, Ub    : Integer;
                 B         : PVector;
                 FirstPar,
                 LastPar   : Integer;
                 V         : PMatrix); external 'tpmath';
{ Simulation of unweighted nonlinear regression by MCMC }

procedure WSimFit(RegFunc   : TRegFunc;
                  X, Y, S   : PVector;
                  Lb, Ub    : Integer;
                  B         : PVector;
                  FirstPar,
                  LastPar   : Integer;
                  V         : PMatrix); external 'tpmath';
{ Simulation of weighted nonlinear regression by MCMC }

procedure RegTest(Y, Ycalc : PVector;
                  LbY, UbY : Integer;
                  V        : PMatrix;
                  LbV, UbV : Integer;
                  var Test : TRegTest); external 'tpmath';
{ Test of unweighted regression }

procedure WRegTest(Y, Ycalc, S : PVector;
                   LbY, UbY    : Integer;
                   V           : PMatrix;
                   LbV, UbV    : Integer;
                   var Test    : TRegTest); external 'tpmath';
{ Test of weighted regression }

{ ------------------------------------------------------------------
  Principal component analysis
  ------------------------------------------------------------------ }

procedure VecMean(X            : PMatrix;
                  Lb, Ub, Nvar : Integer;
                  M            : PVector); external 'tpmath';
{ Computes the mean vector M from matrix X }

procedure VecSD(X            : PMatrix;
                Lb, Ub, Nvar : Integer;
                M, S         : PVector); external 'tpmath';
{ Computes the vector of standard deviations S from matrix X }

procedure MatVarCov(X            : PMatrix;
                    Lb, Ub, Nvar : Integer;
                    M            : PVector;
                    V            : PMatrix); external 'tpmath';
{ Computes the variance-covariance matrix V from matrix X }

procedure MatCorrel(V    : PMatrix;
                    Nvar : Integer;
                    R    : PMatrix); external 'tpmath';
{ Computes the correlation matrix R from the var-cov matrix V }

procedure PCA(R       : PMatrix;
              Nvar    : Integer;
              MaxIter : Integer;
              Tol     : Float;
              Lambda  : PVector;
              C, Rc   : PMatrix); external 'tpmath';
{ Performs a principal component analysis of the correlation matrix R }

procedure ScaleVar(X            : PMatrix;
                   Lb, Ub, Nvar : Integer;
                   M, S         : PVector;
                   Z            : PMatrix); external 'tpmath';
{ Scales a set of variables by subtracting means and dividing by SD's }

procedure PrinFac(Z            : PMatrix;
                  Lb, Ub, Nvar : Integer;
                  C, F         : PMatrix); external 'tpmath';
{ Computes principal factors }

{ ------------------------------------------------------------------
  Strings
  ------------------------------------------------------------------ }

function LTrim(S : String) : String; external 'tpmath';
{ Removes leading blanks }

function RTrim(S : String) : String; external 'tpmath';
{ Removes trailing blanks }

function Trim(S : String) : String; external 'tpmath';
{ Removes leading and trailing blanks }

function StrChar(N : Byte; C : Char) : String; external 'tpmath';
{ Returns a string made of character C repeated N times }

function RFill(S : String; L : Byte) : String; external 'tpmath';
{ Completes string S with trailing blanks for a total length L }

function LFill(S : String; L : Byte) : String; external 'tpmath';
{ Completes string S with leading blanks for a total length L }

function CFill(S : String; L : Byte) : String; external 'tpmath';
{ Centers string S on a total length L }

function Replace(S : String; C1, C2 : Char) : String; external 'tpmath';
{ Replaces in string S all the occurences of C1 by C2 }

function Extract(S : String; var Index : Byte; Delim : Char) : String; external 'tpmath';
{ Extracts a field from a string }

procedure Parse(S : String; Delim : Char; Field : PStrVector; var N : Byte); external 'tpmath';
{ Parses a string into its constitutive fields }

procedure SetFormat(NumLength, MaxDec : Integer;
                    FloatPoint, NSZero : Boolean); external 'tpmath';
{ Sets the numeric format }

function FloatStr(X : Float) : String; external 'tpmath';
{ Converts a real to a string according to the numeric format }

function IntStr(N : LongInt) : String; external 'tpmath';
{ Converts an integer to a string }

function CompStr(Z : Complex) : String; external 'tpmath';
{ Converts a complex number to a string }

implementation

end.

