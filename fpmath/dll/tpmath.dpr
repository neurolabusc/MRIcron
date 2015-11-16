library tpmath;

uses
  utypes,   uminmax,  uround,   umath,    utrigo,   uhyper,   ugamma,
  udigamma, uigamma,  ubeta,    uibeta,   ulambert, ufact,    ubinom,
  upoidist, uexpdist, unormal,  ugamdist, uibtdist, uigmdist, uinvnorm,
  uinvgam,  uinvbeta, ugausjor, ulineq,   ucholesk, ulu,      uqr,
  usvd,     ueigval,  ueigvec,  ujacobi,  uminbrak, ugoldsrc, ulinmin,
  unewton,  umarq,    ubfgs,    usimplex, ubisect,  unewteq,  usecant,
  unewteqs, ubroyden, upolynom, urtpol1,  urtpol2,  urtpol3,  urtpol4,
  urootpol, upolutil, utrapint, ugausleg, urkf,     ufft,     urandom,
  uranmwc,  uranmt,   uranuvag, urangaus, uranmult, umcmc,    usimann,
  ugenalg,  umeansd,  ucorrel,  uqsort,   umedian,  uskew,    uinterv,
  ustudind, ustdpair, uanova1,  uanova2,  usnedeco, ubartlet, ukhi2,
  uwoolf,   unonpar,  udistrib, ulinfit,  upolfit,  umulfit,  usvdfit,
  unlfit,   uregtest, upca,     ustrings;

exports
  SetErrCode,              { Sets error code }
  DefaultVal,              { Sets error code and default function value }
  MathErr,                 { Returns the error code }
  SetAutoInit,             { Sets automatic array initialization }
  DimVector,               { Allocates a real vector }
  DimIntVector,            { Allocates an integer vector }
  DimCompVector,           { Allocates a complex vector }
  DimBoolVector,           { Allocates a boolean vector }
  DimStrVector,            { Allocates a string vector }
  DimMatrix,               { Allocates a real matrix }
  DimIntMatrix,            { Allocates an integer matrix }
  DimCompMatrix,           { Allocates a complex matrix }
  DimBoolMatrix,           { Allocates a boolean matrix }
  DimStrMatrix,            { Allocates a string matrix }
  DelVector,               { Deallocates a real vector }
  DelIntVector,            { Deallocates an integer vector }
  DelCompVector,           { Deallocates a complex vector }
  DelBoolVector,           { Deallocates a boolean vector }
  DelStrVector,            { Deallocates a string vector }
  DelMatrix,               { Deallocates a real matrix }
  DelIntMatrix,            { Deallocates an integer matrix }
  DelCompMatrix,           { Deallocates a complex matrix }
  DelBoolMatrix,           { Deallocates a boolean matrix }
  DelStrMatrix,            { Deallocates a string matrix }
  FMin,                    { Minimum of 2 reals }
  FMax,                    { Maximum of 2 reals }
  IMin,                    { Minimum of 2 integers }
  IMax,                    { Maximum of 2 integers }
  Sgn,                     { Sign, Sgn(0) = 1 }
  Sgn0,                    { Sign, Sgn(0) = 0 }
  DSgn,                    { DSgn(A, B) = Sgn(B) * |A| }
  FSwap,                   { Exchanges 2 reals }
  ISwap,                   { Exchanges 2 integers }
  RoundN,                  { Rounds a number to N decimal places }
  Ceil,                    { Ceiling function }
  Floor,                   { Floor function }
  Expo,                    { Exponential (with bound checking) }
  Exp2,                    { Exponential, base 2 }
  Exp10,                   { Exponential, base 10 }
  Log,                     { Natural log (with bound checking) }
  Log2,                    { Log, base 2 }
  Log10,                   { Log, base 10 }
  LogA,                    { Log, base A }
  IntPower,                { Power (integer exponent) }
  Power,                   { Power (real exponent) }
  Pythag,                  { Sqrt(X^2 + Y^2) }
  FixAngle,                { Set argument in -Pi..Pi }
  Tan,                     { Tangent }
  ArcSin,                  { Arc sinus }
  ArcCos,                  { Arc cosinus }
  ArcTan2,                 { Angle (Ox, OM) with M(X,Y) }
  Sinh,                    { Hyperbolic sine }
  Cosh,                    { Hyperbolic cosine }
  Tanh,                    { Hyperbolic tangent }
  ArcSinh,                 { Inverse hyperbolic sine }
  ArcCosh,                 { Inverse hyperbolic cosine }
  ArcTanh,                 { Inverse hyperbolic tangent }
  SinhCosh,                { Sinh and Cosh }
  Gamma,                   { Gamma function }
  LnGamma,                 { Logarithm of Gamma function }
  SgnGamma,                { Sign of Gamma function }
  Stirling,                { Stirling's formula for Gamma }
  StirLog,                 { Stirling's formula for LnGamma }
  DiGamma,                 { DiGamma function }
  TriGamma,                { TriGamma function }
  IGamma,                  { Incomplete Gamma function }
  JGamma,                  { Complement of incomplete Gamma function }
  Erf,                     { Error function }
  Erfc,                    { Complement of error function }
  Beta,                    { Beta function }
  IBeta,                   { Incomplete Beta function }
  LambertW,                { Lambert's W-function }
  Fact,                    { Factorial }
  Binomial,                { Binomial coefficient }
  PBinom,                  { Probability of binomial distribution }
  PPoisson,                { Probability of Poisson distribution }
  DExpo,                   { Density of exponential distribution }
  FExpo,                   { Cumulative prob. of exponential dist. }
  DNorm,                   { Density of standard normal distribution }
  DBeta,                   { Density of Beta distribution }
  DGamma,                  { Density of Gamma distribution }
  DKhi2,                   { Density of Khi-2 distribution }
  DStudent,                { Density of Student's distribution }
  DSnedecor,               { Density of Fisher-Snedecor distribution }
  FBeta,                   { Cumulative prob. of Beta distribution }
  FBinom,                  { Cumulative prob. of Binomial distribution }
  FStudent,                { Cumulative prob. of Student's distribution }
  PStudent,                { Prob(|t| > X) for Student's distribution }
  FSnedecor,               { Cumulative prob. of Fisher-Snedecor distribution }
  PSnedecor,               { Prob(F > X) for Fisher-Snedecor distribution }
  FGamma,                  { Cumulative prob. of Gamma distribution }
  FPoisson,                { Cumulative prob. of Poisson distribution }
  FNorm,                   { Cumulative prob. of standard normal distribution }
  PNorm,                   { Prob(|U| > X) for standard normal distribution }
  FKhi2,                   { Cumulative prob. of Khi-2 distribution }
  PKhi2,                   { Prob(Khi2 > X) for Khi-2 distribution }
  InvNorm,                 { Inverse of normal distribution }
  InvGamma,                { Inverse of incomplete Gamma function }
  InvKhi2,                 { Inverse of khi-2 distribution }
  InvBeta,                 { Inverse of incomplete Beta function }
  InvStudent,              { Inverse of Student's t-distribution }
  InvSnedecor,             { Inverse of Snedecor's F-distribution }
  GaussJordan,             { Linear equation system (Gauss-Jordan method) }
  LinEq,                   { Linear equation system (Gauss-Jordan method) }
  Cholesky,                { Cholesky factorization }
  LU_Decomp,               { LU decomposition }
  LU_Solve,                { Linear equation system (LU method) }
  QR_Decomp,               { QR decomposition }
  QR_Solve,                { Linear equation system (QR method) }
  SV_Decomp,               { Singular value decomposition }
  SV_Solve,                { Linear equation system (SVD method) }
  SV_SetZero,              { Set lowest singular values to zero }
  SV_Approx,               { Matrix approximation from SVD }
  EigenVals,               { Eigenvalues of a general square matrix }
  EigenVect,               { Eigenvalues and eigenvectors of a general square matrix }
  Jacobi,                  { Eigenvalues and eigenvectors of a symmetric matrix }
  MinBrack,                { Brackets the minimum of a function }
  GoldSearch,              { Minimization of a function of one variable (golden search) }
  LinMin,                  { Minimization of a function of several variables along a line }
  Newton,                  { Minimization of a function of several var. (Newton's method) }
  SaveNewton,              { Save Newton iterations in a file }
  Marquardt,               { Minimization of a function of several var. (Marquardt's method) }
  SaveMarquardt,           { Save Marquardt iterations in a file }
  BFGS,                    { Minimization of a function of several var. (BFGS method) }
  SaveBFGS,                { Save BFGS iterations in a file }
  Simplex,                 { Minimization of a function of several variables (Simplex) }
  SaveSimplex,             { Save Simplex iterations in a file }
  RootBrack,               { Brackets solution of equation }
  Bisect,                  { Nonlinear equation (bisection method) }
  NewtEq,                  { Nonlinear equation (Newton-Raphson method) }
  Secant,                  { Nonlinear equation (secant method) }
  NewtEqs,                 { Nonlinear equation system (Newton-Raphson method) }
  Broyden,                 { Nonlinear equation system (Broyden's method) }
  Poly,                    { Evaluates a polynomial }
  RFrac,                   { Evaluates a rational fraction }
  RootPol1,                { Root of linear equation }
  RootPol2,                { Roots of quadratic equation }
  RootPol3,                { Roots of cubic equation }
  RootPol4,                { Roots of quartic equation }
  RootPol,                 { Roots of polynomial from companion matrix }
  SetRealRoots,            { Set the imaginary part of a root to zero }
  SortRoots,               { Sorts the roots of a polynomial }
  TrapInt,                 { Integration by trapezoidal rule }
  GausLeg,                 { Gauss-Legendre integration }
  GausLeg0,                { Gauss-Legendre integration (lower bound=0) }
  Convol,                  { Convolution product }
  RKF45,                   { Integration of a system of differential equations }
  FFT,                     { Fast Fourier Transform }
  IFFT,                    { Inverse Fast Fourier Transform }
  FFT_Integer,             { Fast Fourier Transform for integer data }
  FFT_Integer_Cleanup,     { Clear memory after a call to FFT_Integer }
  CalcFrequency,           { Direct computation of Fourier Transform }
  SetRNG,                  { Select random number generator }
  InitGen,                 { Initialize random number generator }
  IRanGen,                 { 32-bit random integer in [-2^31 .. 2^31 - 1] }
  IRanGen31,               { 31-bit random integer in [0 .. 2^31 - 1] }
  RanGen1,                 { 32-bit random real in [0,1] }
  RanGen2,                 { 32-bit random real in [0,1) }
  RanGen3,                 { 32-bit random real in (0,1) }
  RanGen53,                { 53-bit random real in [0,1) }
  InitMWC,                 { Initialize Multiply-With-Carry generator }
  IRanMWC,                 { 32-bit random integer from MWC generator }
  InitMT,                  { Initialize Mersenne Twister generator with a seed }
  InitMTbyArray,           { Initialize MT generator with an array }
  IRanMT,                  { 32-bit random integer from MT generator }
  InitUVAG,                { Initialize UVAG generator with a seed }
  InitUVAGbyString,        { Initialize UVAG generator with a string }
  IRanUVAG,                { 32-bit random integer from UVAG generator }
  RanGaussStd,             { Random number from standard normal distribution }
  RanGauss,                { Random number from normal distribution }
  RanMult,                 { Random vector from multinormal distrib. (correlated) }
  RanMultIndep,            { Random vector from multinormal distrib. (uncorrelated) }
  InitMHParams,            { Initialize Metropolis-Hastings parameters }
  GetMHParams,             { Returns Metropolis-Hastings parameters }
  Hastings,                { Simulation of a p.d.f. by Metropolis-Hastings }
  InitSAParams,            { Initialize Simulated Annealing parameters }
  SA_CreateLogFile,        { Initialize log file for Simulated Annealing }
  SimAnn,                  { Minimization of a function of several var. by Simulated Annealing }
  InitGAParams,            { Initialize Genetic Algorithm parameters }
  GA_CreateLogFile,        { Initialize log file for Genetic Algorithm }
  GenAlg,                  { Minimization of a function of several var. by Genetic Algorithm }
  Mean,                    { Sample mean }
  Median,                  { Sample median }
  StDev,                   { Standard deviation estimated from sample }
  StDevP,                  { Standard deviation of population }
  Correl,                  { Correlation coefficient }
  Skewness,                { Sample skewness }
  Kurtosis,                { Sample kurtosis }
  QSort,                   { Quick sort (ascending order) }
  DQSort,                  { Quick sort (descending order) }
  Interval,                { Determines an interval for a set of values }
  StudIndep,               { Student t-test for independent samples }
  StudPaired,              { Student t-test for paired samples }
  AnOVa1,                  { One-way analysis of variance }
  AnOVa2,                  { Two-way analysis of variance }
  Snedecor,                { Comparison of two variances }
  Bartlett,                { Comparison of several variances }
  Khi2_Conform,            { Khi-2 test for conformity }
  Khi2_Indep,              { Khi-2 test for independence }
  Woolf_Conform,           { Woolf's test for conformity }
  Woolf_Indep,             { Woolf's test for independence }
  Mann_Whitney,            { Mann-Whitney test }
  Wilcoxon,                { Wilcoxon test }
  Kruskal_Wallis,          { Kruskal-Wallis test }
  DimStatClassVector,      { Allocates an array of statistical classes }
  DelStatClassVector,      { Deallocates an array of statistical classes }
  Distrib,                 { Distributes an array into statistical classes }
  LinFit,                  { Linear regression }
  WLinFit,                 { Weighted linear regression }
  PolFit,                  { Polynomial regression }
  WPolFit,                 { Weighted polynomial regression }
  MulFit,                  { Multiple linear regression by Gauss-Jordan method }
  WMulFit,                 { Weighted multiple linear regression by Gauss-Jordan method }
  SVDFit,                  { Multiple linear regression by SVD method }
  WSVDFit,                 { Weighted multiple linear regression by SVD method }
  SetOptAlgo,              { Selects optimization algorithm for nonlinear regression }
  SetMaxParam,             { Sets the maximal number of regression parameters }
  SetParamBounds,          { Sets the bounds on a regression parameter }
  NLFit,                   { Nonlinear regression }
  WNLFit,                  { Weighted nonlinear regression }
  SetMCFile,               { Set file for saving MCMC simulations }
  SimFit,                  { Simulation of unweighted nonlinear regression by MCMC }
  WSimFit,                 { Simulation of weighted nonlinear regression by MCMC }
  RegTest,                 { Test of unweighted regression }
  WRegTest,                { Test of weighted regression }
  VecMean,                 { Computes mean vector }
  VecSD,                   { Computes vector of standard deviations }
  MatVarCov,               { Computes variance-covariance matrix }
  MatCorrel,               { Computes correlation matrix }
  PCA,                     { Principal component analysis of correlation matrix }
  ScaleVar,                { Scales a set of variables }
  PrinFac,                 { Computes principal factors }
  LTrim,                   { Remove leading blanks }
  RTrim,                   { Remove trailing blanks }
  Trim,                    { Remove leading and trailing blanks }
  StrChar,                 { Generate string by repeating a character }
  RFill,                   { Complete string with trailing blanks }
  LFill,                   { Complete string with leading blanks }
  CFill,                   { Center string }
  Replace,                 { Replace a character }
  Extract,                 { Extract field from string }
  Parse,                   { Parse string into several fields }
  SetFormat,               { Set numeric format }
  FloatStr,                { Convert real number to string }
  IntStr,                  { Convert integer to string }
  CompStr;                 { Convert complex number to string }

begin
end.

