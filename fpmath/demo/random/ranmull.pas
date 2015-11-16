{ ******************************************************************
  This program simulates a multi-lognormal distribution. The mean
  vector and variance-covariance matrix are stored in a data file
  with the following structure:

    Line 1            : Name of distribution.
    Line 2            : Size of distribution (N), e.g. 2 for bi-lognormal
    Line 3 to (N + 2) : Means and standard deviations.
    Next lines        : Correlation coefficients, in
                        lower triangular matrix form.

  The file RANMULL.DAT is an example data file.

  The results are stored in an output file
  (one random vector by line)
  ****************************************************************** }

program ranmull;

uses
  tpmath;

const
  NSIM = 400;  { Number of simulations }

var
  Name : String;   { Name of distribution }
  N    : Integer;  { Dimension of distribution }
  M    : PVector;  { Mean vector of original lognormal distribution }
  V    : PMatrix;  { Variance-covariance matrix of original lognormal dist. }
  M0   : PVector;  { Mean vector of auxiliary normal dist. }
  V0   : PMatrix;  { Variance-covariance matrix of auxiliary normal dist. }
  L    : PMatrix;  { Cholesky factor of V0 }
  Z    : PVector;  { Random vector from the auxiliary normal dist. }
  X    : PVector;  { Random vector from the original lognormal dist. }
  F    : Text;     { Output file }
  I, J : Integer;  { Loop variables }

  procedure ReadParam(FileName : String; var Name : String; var N : Integer;
                      var M : PVector; var V : PMatrix);
  var
    F    : Text;     { Data file }
    I, J : Integer;  { Loop variables }
    S    : PVector;  { Standard deviations }
    R    : Float;    { Correlation coefficient }
  begin
    Assign(F, FileName);
    Reset(F);

    Readln(F, Name);
    Readln(F, N);

    DimVector(M, N);
    DimVector(S, N);
    DimMatrix(V, N, N);

    { Read means and standard deviations. Compute variances }
    for I := 1 to N do
      begin
        Read(F, M^[I], S^[I]);
        V^[I]^[I] := Sqr(S^[I]);
      end;

    { Read correlation coefficients and compute covariances }
    for I := 2 to N do
      for J := 1 to Pred(I) do
        begin
          Read(F, R);
          V^[I]^[J] := R * S^[I] * S^[J];
          V^[J]^[I] := V^[I]^[J];
        end;

    Close(F);
    DelVector(S, N);
  end;

begin
  { Read parameters of log-normal distribution LN(M, V) }
  ReadParam('ranmull.dat', Name, N, M, V);

  DimVector(X, N);
  DimVector(Z, N);
  DimVector(M0, N);
  DimMatrix(V0, N, N);
  DimMatrix(L, N, N);

  { Define auxiliary normal distribution N(M0, V0) }
  for I := 1 to N do
    begin
      for J := 1 to N do
        V0^[I]^[J] := Ln(V^[I]^[J] / (M^[I] * M^[J]) + 1.0);
      M0^[I] := Ln(M^[I]) - 0.5 * V0^[I]^[I];
    end;

  { Perform Cholesky decomposition of variance-covariance matrix }
  Cholesky(V0, L, 1, N);
  if MathErr = MatNotPD then
    begin
      WriteLn('Variance-covariance matrix is not positive definite.');
      Exit;
    end;

  SetRNG(RNG_MT);

  Assign(F, 'ranmull.out');
  Rewrite(F);
  
  for I := 1 to NSIM do
    begin
      { Pick random vector from auxiliary normal distribution }
      RanMult(M0, L, 1, N, Z);

      { Convert to lognormal }
      for J := 1 to N do
        X^[J] := Exp(Z^[J]);

      { Output result to file }
      for J := 1 to N do
        Write(F, X^[J]:12:6);
      Writeln(F);
    end;
  Close(F);
end.
