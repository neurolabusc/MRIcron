{ ******************************************************************
  Correlation and principal component analysis
  ******************************************************************
  Example taken from:
  P. DAGNELIE, Analyse statistique a plusieurs variables,
  Presses Agronomiques de Gembloux, Belgique, 1982
  ****************************************************************** }

program pcatest;

uses
  tpmath;

const
  N    = 11;  { Number of observations }
  Nvar = 4;   { Number of variables }

{ Data }
const X : array[1..N, 1..Nvar] of Float =
(( 87.9, 19.6,   1  , 1661),
 ( 89.9, 15.2,  90.1,  968),
 (153  , 19.7,  56.6, 1353),
 (132.1, 17  ,  91  , 1293),
 ( 88.8, 18.3,  93.7, 1153),
 (220.9, 17.8, 106.9, 1286),
 (117.7, 17.8,  65.5, 1104),
 (109  , 18.3,  41.8, 1574),
 (156.1, 17.8,  57.4, 1222),
 (181.5, 16.8, 140.6,  902),
 (181.4, 17  ,  74.3, 1150));

var
  XX     : PMatrix;  { Data }
  M      : PVector;  { Mean vector }
  V      : PMatrix;  { Variance-covariance matrix }
  R      : PMatrix;  { Correlation matrix }
  S      : PVector;  { Standard deviations }
  Lambda : PVector;  { Eigenvalues of correlation matrix }
  C      : PMatrix;  { Eigenvectors of correlation matrix }
  Rc     : PMatrix;  { Correlation factors/variables }
  Z      : PMatrix;  { Scaled variables }
  F      : PMatrix;  { Principal factors }
  I, J   : Integer;  { Loop variables }

procedure PrintMatrix(Title : String; A : PMatrix; Ub1, Ub2 : Integer);
{ ------------------------------------------------------------------
  Print matrix A[1..Ub1, 1..Ub2]
  ------------------------------------------------------------------ }
var
  I, J : Integer;
begin
  Writeln; Writeln(Title); Writeln;

  for I := 1 to Ub1 do
    begin
      for J := 1 to Ub2 do
        Write(A^[I]^[J]:12:4);
      Writeln;
    end;
end;

procedure PrintVector(Title : String; B : PVector; Ub : Integer);
{ ------------------------------------------------------------------
  Print vector B[1..Ub]
  ------------------------------------------------------------------ }
var
  I : Integer;
begin
  Writeln; Writeln(Title); Writeln;

  for I := 1 to Ub do
    Writeln(B^[I]:12:4);
end;

begin
  DimMatrix(XX, N, Nvar);
  DimVector(M, Nvar);
  DimMatrix(V, Nvar, Nvar);
  DimMatrix(R, Nvar, Nvar);
  DimVector(S, Nvar);
  DimVector(Lambda, Nvar);
  DimMatrix(C, Nvar, Nvar);
  DimMatrix(Rc, Nvar, Nvar);
  DimMatrix(Z, N, Nvar);
  DimMatrix(F, N, Nvar);

  { Read data }
  for I := 1 to N do
    for J := 1 to Nvar do
      XX^[I]^[J] := X[I,J];

  { Compute mean vector }
  VecMean(XX, 1, N, Nvar, M);

  { Compute variance-covariance matrix }
  MatVarCov(XX, 1, N, Nvar, M, V);

  { Compute correlation matrix }
  MatCorrel(V, Nvar, R);

  { Display results }
  Writeln;
  PrintVector('Mean vector', M, Nvar);
  PrintMatrix('Variance-covariance matrix', V, Nvar, Nvar);
  PrintMatrix('Correlation matrix', R, Nvar, Nvar);

  { Compute standard deviations }
  VecSD(XX, 1, N, Nvar, M, S);

  { Scale variables }
  ScaleVar(XX, 1, N, Nvar, M, S, Z);

  { Perform principal component analysis
    The original matrix R is destroyed }
  PCA(R, Nvar, 1000, 1.0E-10, Lambda, C, Rc);

  if MathErr = MatNonConv then
    begin
      Writeln('Non-convergence of eigenvalue computation');
      Exit;
    end;

  { Compute principal factors }
  PrinFac(Z, 1, N, Nvar, C, F);

  { Display results }
  Writeln;
  PrintVector('Eigenvalues of correlation matrix', Lambda, Nvar);
  PrintMatrix('Eigenvectors (columns) of correlation matrix', C, Nvar, Nvar);
  PrintMatrix('Correlations between factors (columns) and variables (lines)', Rc, Nvar, Nvar);
  PrintMatrix('Principal factors', F, N, Nvar);
end.

