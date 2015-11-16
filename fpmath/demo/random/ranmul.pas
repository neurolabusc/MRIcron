{ ******************************************************************
  This program simulates a multinormal distribution. The mean vector
  and the variance-covariance matrix are stored in a data file with
  the following structure:

    Line 1            : Name of distribution.
    Line 2            : Size of distribution (N), e.g. 2 for binormal
    Line 3 to (N + 2) : Means and standard deviations.
    Next lines        : Correlation coefficients, in
                        lower triangular matrix form.

  The file RANMUL.DAT is an example data file.

  The results are stored in an output file
  (one random vector by line)
  ****************************************************************** }

program ranmul;

uses
  tpmath;

const
  NSIM = 100;  { Number of simulations }

var
  Name : String;   { Name of distribution }
  N    : Integer;  { Size of distribution }
  M    : PVector;  { Mean vector }
  V    : PMatrix;  { Variance-covariance matrix }
  L    : PMatrix;  { Cholesky factor of V }
  X    : PVector;  { Random vector }
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
  ReadParam('ranmul.dat', Name, N, M, V);

  DimVector(X, N);
  DimMatrix(L, N, N);

  { Perform Cholesky decomposition of variance-covariance matrix }

  Cholesky(V, L, 1, N);
  if MathErr = MatNotPD then
    begin
      WriteLn('Variance-covariance matrix is not positive definite.');
      Exit;
    end;

  SetRNG(RNG_MT);

  Assign(F, 'ranmul.out');
  Rewrite(F);

  for I := 1 to NSIM do
    begin
      { Pick random vector }
      RanMult(M, L, 1, N, X);

      { Output result to file }
      for J := 1 to N do
        Write(F, X^[J]:12:6);
      Writeln(F);
    end;
  Close(F);
end.

