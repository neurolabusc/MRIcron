{ **********************************************************************
  *                             Unit MCMC.PAS                          *
  *                              Version 1.2                           *
  *                       (c) J. Debord, June 2001                     *
  **********************************************************************
  Simulation by Markov Chain Monte Carlo (MCMC) with the
  Metropolis-Hastings algorithm.

  This algorithm simulates the probability density function (pdf) of a
  vector X. The pdf P(X) is written as:

                       P(X) = C * Exp(- F(X) / T)

  Simulating P by the Metropolis-Hastings algorithm is equivalent to
  minimizing F by simulated annealing at the constant temperature T.
  The constant C is not used in the simulation.

  The series of random vectors generated during the annealing step
  constitutes a Markov chain which tends towards the pdf to be simulated.

  It is possible to run several cycles of the algorithm.
  The variance-covariance matrix of the simulated distribution is
  re-evaluated at the end of each cycle and used for the next cycle.
  ********************************************************************** }

unit MCMC;

interface

uses
  FMath, Matrices, Optim, Regress;


{ **********************************************************************
  Metropolis-Hastings parameters
  ********************************************************************** }

const
  MH_NCycles  : Integer = 1;    { Number of cycles }
  MH_MaxSim   : Integer = 1000; { Max nb of simulations at each cycle }
  MH_SavedSim : Integer = 200;  { Nb of simulations to be saved }

{ **********************************************************************
  Simulation routine
  ********************************************************************** }

  function Hastings(Func           : TFuncNVar;
                    T              : Float;
                    X              : PVector;
                    V              : PMatrix;
                    Lbound, Ubound : Integer;
                    Xmat           : PMatrix;
                    X_min          : PVector;
                    var F_min      : Float) : Integer;
{ ----------------------------------------------------------------------
  Simulation of a probability density function by the
  Metropolis-Hastings algorithm
  ----------------------------------------------------------------------
  Input parameters :  Func   = Function such that the pdf is
                                 P(X) = C * Exp(- Func(X) / T)
                      T      = Temperature
                      X      = Initial mean vector
                      V      = Initial variance-covariance matrix
                      Lbound,
                      Ubound = Indices of first and last variables
  ----------------------------------------------------------------------
  Output parameters : Xmat  = Matrix of simulated vectors, stored
                              columnwise, i.e.
                              Xmat[Lbound..Ubound, 1..MH_SavedSim]
                      X     = Mean of distribution
                      V     = Variance-covariance matrix of distribution
                      X_min = Coordinates of minimum of F(X)
                                (mode of the distribution)
                      F_min = Value of F(X) at minimum
  ----------------------------------------------------------------------
  Possible results : MAT_OK     : No error
                     MAT_NOT_PD : The variance-covariance matrix
                                  is not positive definite
  ---------------------------------------------------------------------- }

implementation

  function CalcSD(V              : PMatrix;
                  Lbound, Ubound : Integer;
                  L              : PMatrix) : Integer;
{ ----------------------------------------------------------------------
  Computes the standard deviations for independent random numbers
  from the variance-covariance matrix.
  ---------------------------------------------------------------------- }
  var
    I, ErrCode : Integer;
  begin
    I := LBound;
    ErrCode := 0;
    repeat
      if V^[I]^[I] > 0.0 then
        L^[I]^[I] := Sqrt(V^[I]^[I])
      else
        ErrCode := MAT_NOT_PD;
      Inc(I);
    until (ErrCode <> 0) or (I > Ubound);
    CalcSD := ErrCode;
  end;

  procedure GenIndepRandomVector(X              : PVector;
                                 L              : PMatrix;
                                 Lbound, Ubound : Integer;
                                 X1             : PVector);
{ ----------------------------------------------------------------------
  Generates a random vector X1 from X, using independent gaussian random
  increments. L is the diagonal matrix of the standard deviations.
  ---------------------------------------------------------------------- }
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      X1^[I] := RanGauss(X^[I], L^[I]^[I]);
  end;

  procedure GenRandomVector(X              : PVector;
                            L              : PMatrix;
                            Lbound, Ubound : Integer;
                            X1             : PVector);
{ ----------------------------------------------------------------------
  Generates a random vector X1 from X, using correlated gaussian random
  increments. L is the Cholesky factor of the variance-covariance matrix
  ---------------------------------------------------------------------- }
  var
    U    : PVector;
    I, J : Integer;
  begin
    { Form a vector U of independent standard normal variates }
    DimVector(U, Ubound);
    for I := Lbound to Ubound do
      U^[I] := RanGaussStd;

    { Form X1 = X + L*U, which follows the multinormal distribution }
    for I := Lbound to Ubound do
      begin
        X1^[I] := X^[I];
        for J := Lbound to I do
          X1^[I] := X1^[I] + L^[I]^[J] * U^[J];
      end;
    DelVector(U, Ubound);
  end;

  function Accept(DeltaF, T : Float) : Boolean;
{ ----------------------------------------------------------------------
  Checks if a variation DeltaF of the function at temperature T is
  acceptable.
  ---------------------------------------------------------------------- }
  begin
    Accept := (DeltaF < 0.0) or (Expo(- DeltaF / T) > RanMar);
  end;

  function HastingsCycle(Func           : TFuncNVar;
                         T              : Float;
                         X              : PVector;
                         V              : PMatrix;
                         Lbound, Ubound : Integer;
                         Indep          : Boolean;
                         Xmat           : PMatrix;
                         X_min          : PVector;
                         var F_min      : Float) : Integer;
{ ----------------------------------------------------------------------
  Performs one cycle of the Metropolis-Hastings algorithm
  ---------------------------------------------------------------------- }
  var
    F, F1         : Float;    { Function values }
    DeltaF        : Float;    { Variation of function }
    X1            : PVector;  { New coordinates }
    L             : PMatrix;  { Standard dev. or Cholesky factor }
    I, K          : Integer;  { Loop variable }
    Iter          : Integer;  { Iteration count }
    FirstSavedSim : Integer;  { Index of first simulation to be saved }
    ErrCode       : Integer;  { Error code }
  begin
    { Dimension arrays }
    DimVector(X1, Ubound);
    DimMatrix(L, Ubound, Ubound);

    { Compute SD's or Cholesky factor }
    if Indep then
      ErrCode := CalcSD(V, Lbound, Ubound, L)
    else
      ErrCode := Cholesky(V, Lbound, Ubound, L);

    HastingsCycle := ErrCode;
    if ErrCode = MAT_NOT_PD then Exit;

    { Compute initial function value }
    F := Func(X);

    { Perform MH_MaxSim simulations at constant temperature }
    FirstSavedSim := MH_MaxSim - MH_SavedSim + 1;
    Iter := 1;
    K := 1;

    repeat
      { Generate new vector }
      if Indep then
        GenIndepRandomVector(X, L, Lbound, Ubound, X1)
      else
        GenRandomVector(X, L, Lbound, Ubound, X1);

      { Compute new function value }
      F1 := Func(X1);
      DeltaF := F1 - F;

      { Check for acceptance }
      if Accept(DeltaF, T) then
        begin
          CopyVector(X, X1, Lbound, Ubound);

          if Iter >= FirstSavedSim then
            begin
              { Save simulated vector into column K of matrix Xmat }
              CopyColFromVector(Xmat, X1, Lbound, Ubound, K);
              Inc(K);
            end;

          if F1 < F_min then
            begin
              { Update minimum }
              CopyVector(X_min, X1, Lbound, Ubound);
              F_min := F1;
            end;

          F := F1;
          Inc(Iter);
        end;
    until Iter > MH_MaxSim;

    { Update mean vector and variance-covariance matrix }
    VecMean(Xmat, MH_SavedSim, Lbound, Ubound, X);
    MatVarCov(Xmat, MH_SavedSim, Lbound, Ubound, X, V);

    DelVector(X1, Ubound);
    DelMatrix(L, Ubound, Ubound);
  end;

  function Hastings(Func           : TFuncNVar;
                    T              : Float;
                    X              : PVector;
                    V              : PMatrix;
                    Lbound, Ubound : Integer;
                    Xmat           : PMatrix;
                    X_min          : PVector;
                    var F_min      : Float) : Integer;
  var
    K, ErrCode : Integer;
    Indep : Boolean;
  begin
    { Initialize the Marsaglia random number generator
      using the standard Pascal generator }
    Randomize;
    RMarIn(System.Random(10000), System.Random(10000));

    K := 1;
    Indep := True;
    F_min := MAXNUM;

    repeat
      ErrCode := HastingsCycle(Func, T, X, V, Lbound, Ubound,
                               Indep, Xmat, X_min, F_min);
      Indep := False;
      Inc(K);
    until (ErrCode <> 0) or (K > MH_NCycles);

    Hastings := ErrCode;
  end;

end.