{ ******************************************************************
  This program simulates a multinormal distribution by Markov Chain
  Monte Carlo (MCMC) using the Hastings-Metropolis algorithm.

  Although MCMC is best used when there is no direct way to simulate
  the distribution, it is used here for demonstration purposes since
  its results can be compared to those of the direct method (program
  RANMUL.PAS).

  The pdf P(X) of the multinormal distribution is such that:

                        P(X) = C * Exp(- F(X) / T)

  where F(X) = (X - M)' * V^(-1) * (X - M)

        C = 1/sqrt(|V| * (2*Pi)^N)

        T = 2

        M is the mean vector and V the variance-covariance matrix of
        the distribution. N is the dimension of the distribution.

        The constant C is not used in the simulation.

  The mean vector and variance-covariance matrix are stored in a data
  file with the following structure:

    Line 1            : Title of study
    Line 2            : Number of variables (N), e.g. 2 for binormal
    Line 3 to (N + 2) : Means and standard deviations
    Next lines        : Correlation coefficients, in
                        lower triangular matrix form

  The file TESTMCMC.DAT is an example data file.

  The results are stored in the output file TESTMCMC.TXT
  ****************************************************************** }

program testmcmc;

uses
  tpmath;

const
  Temp     = 2.0;   { Temperature }
  NCycles  = 10;    { Number of cycles }
  MaxSim   = 1000;  { Max nb of simulations }
  SavedSim = 1000;  { Nb of saved simulations }
var
  Title : String;   { Title of study }
  N     : Integer;  { Number of variables }
  M     : PVector;  { Mean vector of original distribution }
  V     : PMatrix;  { Variance-covariance matrix of original distribution }
  V_inv : PMatrix;  { Inverse variance-covariance matrix }
  Xmat  : PMatrix;  { Matrix of simulated vectors }
  Msim  : PVector;  { Mean of simulated distribution }
  Vsim  : PMatrix;  { Variance-covariance matrix of simulated distrib. }
  X_min : PVector;  { Coordinates of the minimum of F(X)
                        = mode of simulated distribution }
  F_min : Float;    { Value of F(X) at minimum }
  I     : Integer;  { Loop variable }

  function ReadParam(FileName : String; var Title : String;
                     var N : Integer; var M : PVector;
                     var V, V_inv : PMatrix) : Integer;
  var
    F    : Text;     { Data file }
    I, J : Integer;  { Loop variables }
    S    : PVector;  { Standard deviations }
    R    : Float;    { Correlation coefficient }
    Det  : Float;    { Determinant of var-cov. matrix }
  begin
    Assign(F, FileName);
    Reset(F);

    Readln(F, Title);
    Readln(F, N);

    DimVector(M, N);
    DimVector(S, N);
    DimMatrix(V, N, N);
    DimMatrix(V_inv, N, N);

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

    { Initialize inverse var-cov. matrix }
    for I := 1 to N do
      for J := 1 to N do
        V_inv^[I]^[J] := V^[I]^[J];

    { Compute the inverse of the variance-covariance matrix }
    GaussJordan(V_inv, 1, N, N, Det);
    ReadParam := MathErr;

    Close(F);
    DelVector(S, N);
  end;

  function ObjFunc(X : PVector) : Float;
  { Computes the function F(X) }
  var
    Sum1, Sum2 : Float;
    I, J       : Integer;
    D          : PVector;
  begin
    DimVector(D, N);

    for I := 1 to N do
      D^[I] := X^[I] - M^[I];

    Sum1 := 0.0;
    for I := 1 to N do
      Sum1 := Sum1 + V_inv^[I]^[I] * Sqr(D^[I]);

    Sum2 := 0.0;
    for I := 2 to N do
      for J := 1 to Pred(I) do
        Sum2 := Sum2 + V_inv^[I]^[J] * D^[I] * D^[J];

    ObjFunc := Sum1 + 2.0 * Sum2;

    DelVector(D, N);
  end;

  procedure WriteResults(Title : String; M : PVector;
                         V : PMatrix; N : Integer);
  var
    I, J : Integer;
    S    : PVector;
    R    : Float;
  begin
    WriteLn;
    WriteLn(Title);
    WriteLn;

    WriteLn('      Mean      S.D.');
    WriteLn('--------------------');

    DimVector(S, N);
    for I := 1 to N do
      begin
        S^[I] := Sqrt(V^[I]^[I]);
        Writeln(M^[I]:10:4, S^[I]:10:4);
      end;

    WriteLn;
    WriteLn('Correlation matrix:');
    WriteLn;

    for I := 2 to N do
      begin
        for J := 1 to Pred(I) do
          begin
            R := V^[I]^[J] / (S^[I] * S^[J]);
            Write(R:10:4);
          end;
        WriteLn;
      end;

    DelVector(S, N);
  end;

  procedure WriteOutputFile(Title : String; Xmat : PMatrix; N : Integer);
  var
    F    : Text;
    I, J : Integer;
  begin
    Assign(F, 'testmcmc.txt');
    Rewrite(F);

    WriteLn(F, Title);
    Write(F, ' Iter');
    for I := 1 to N do
      Write(F, '        X', I);
    WriteLn(F);

    for I := 1 to SavedSim do
      begin
        Write(F, I:5);
        for J := 1 to N do
          Write(F, Xmat^[I]^[J]:10:4);
        WriteLn(F);
      end;

    Close(F);
  end;

begin
  if ReadParam('testmcmc.dat', Title, N, M, V, V_inv) = MatSing then
    begin
      WriteLn('Variance-covariance matrix is singular!');
      Exit;
    end;

  DimVector(Msim, N);
  DimVector(X_min, N);
  DimMatrix(Vsim, N, N);
  DimMatrix(Xmat, SavedSim, N);

  { Select random number generator }
  SetRNG(RNG_MT);

  { Initialize Metropolis-Hastings parameters }
  InitMHParams(NCycles, MaxSim, SavedSim);

  { Initialize the mean vector and the variance-covariance matrix.
    For the sake of demonstration we start at a distance from the
    true mean and with enhanced standard deviations. }
  for I := 1 to N do
    begin
      Msim^[I] := 3.0 * M^[I];
      Vsim^[I]^[I] := 10.0 * V^[I]^[I];
    end;

  { Perform Metropolis-Hastings simulations }
  Write('Running. Please wait...');

  {$IFDEF FPC}
  Hastings(@ObjFunc, Temp, Msim, Vsim, 1, N, Xmat, X_min, F_min);
  {$ELSE}
  Hastings(ObjFunc, Temp, Msim, Vsim, 1, N, Xmat, X_min, F_min);
  {$ENDIF}

  if MathErr = MatOk then
    begin
      WriteResults('Original distribution', M, V, N);
      WriteResults('Simulated distribution', Msim, Vsim, N);
      WriteOutputFile(Title, Xmat, N);
    end
  else
    WriteLn('Variance-covariance matrix is not positive definite!');
end.

