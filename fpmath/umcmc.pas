{ ******************************************************************
  Simulation by Markov Chain Monte Carlo (MCMC) with the
  Metropolis-Hastings algorithm.

  This algorithm simulates the probability density function (pdf) of
  a vector X. The pdf P(X) is written as:

                       P(X) = C * Exp(- F(X) / T)

  Simulating P by the Metropolis-Hastings algorithm is equivalent to
  minimizing F by simulated annealing at the constant temperature T.
  The constant C is not used in the simulation.

  The series of random vectors generated during the annealing step
  constitutes a Markov chain which tends towards the pdf to be
  simulated.

  It is possible to run several cycles of the algorithm.
  The variance-covariance matrix of the simulated distribution is
  re-evaluated at the end of each cycle and used for the next cycle.
  ****************************************************************** }

unit umcmc;

interface

uses
  utypes, ucholesk, urandom, uranmult;

procedure InitMHParams(NCycles, MaxSim, SavedSim : Integer);
{ ------------------------------------------------------------------
  Initializes Metropolis-Hastings parameters
  ------------------------------------------------------------------ }

procedure GetMHParams(var NCycles, MaxSim, SavedSim : Integer);
{ ------------------------------------------------------------------
  Returns Metropolis-Hastings parameters
  ------------------------------------------------------------------ }

procedure Hastings(Func      : TFuncNVar;
                   T         : Float;
                   X         : PVector;
                   V         : PMatrix;
                   Lb, Ub    : Integer;
                   Xmat      : PMatrix;
                   X_min     : PVector;
                   var F_min : Float);
{ ------------------------------------------------------------------
  Simulation of a probability density function by the
  Metropolis-Hastings algorithm
  ------------------------------------------------------------------
  Input parameters :  Func   = Function such that the pdf is
                                 P(X) = C * Exp(- Func(X) / T)
                      T      = Temperature
                      X      = Initial mean vector
                      V      = Initial variance-covariance matrix
                      Lb, Ub = Indices of first and last variables
  ------------------------------------------------------------------
  Output parameters : Xmat  = Matrix of simulated vectors, stored
                              row-wise, i.e.
                              Xmat[1..MH_SavedSim, Lb..Ub]
                      X     = Mean of distribution
                      V     = Variance-covariance matrix of distribution
                      X_min = Coordinates of minimum of F(X)
                                (mode of the distribution)
                      F_min = Value of F(X) at minimum
  ------------------------------------------------------------------
  Possible results : MatOk     : No error
                     MatNotPD  : The variance-covariance matrix
                                 is not positive definite
  ------------------------------------------------------------------ }

implementation

const
  MH_NCycles  : Integer =   10;  { Number of cycles }
  MH_MaxSim   : Integer = 1000;  { Max nb of simulations at each cycle }
  MH_SavedSim : Integer = 1000;  { Nb of simulations to be saved }

  procedure InitMHParams(NCycles, MaxSim, SavedSim : Integer);
  begin
    if NCycles > 0 then MH_NCycles := NCycles;
    if MaxSim > 0 then MH_MaxSim := MaxSim;
    if (SavedSim > 0) and (SavedSim <= MaxSim) then
      MH_SavedSim := SavedSim;
  end;

  procedure GetMHParams(var NCycles, MaxSim, SavedSim : Integer);
  begin
    NCycles := MH_NCycles;
    MaxSim := MH_MaxSim;
    SavedSim := MH_SavedSim;
  end;

  procedure CalcSD(V      : PMatrix;
                   S      : PVector;
                   Lb, Ub : Integer);
{ ------------------------------------------------------------------
  Computes the standard deviations for independent random numbers
  from the variance-covariance matrix.
  ------------------------------------------------------------------ }
  var
    I, ErrCode : Integer;
  begin
    I := Lb;
    ErrCode := MatOk;
    repeat
      if V^[I]^[I] > 0.0 then
        S^[I] := Sqrt(V^[I]^[I])
      else
        ErrCode := MatNotPD;
      Inc(I);
    until (ErrCode <> MatOk) or (I > Ub);
    SetErrCode(ErrCode);
  end;

  function Accept(DeltaF, T : Float) : Boolean;
{ ------------------------------------------------------------------
  Checks if a variation DeltaF of the function at temperature T is
  acceptable.
  ------------------------------------------------------------------ }
  var
    X : Float;
  begin
    if DeltaF < 0.0 then
      begin
        Accept := True;
        Exit;
      end;

    X := DeltaF / T;

    if X >= MaxLog then  { Exp(- X) ~ 0 }
      Accept := False
    else
      Accept := (Exp(- X) > RanGen3);
  end;

  procedure HastingsCycle(Func      : TFuncNVar;
                          T         : Float;
                          X         : PVector;
                          V         : PMatrix;
                          Lb, Ub    : Integer;
                          Indep     : Boolean;
                          Xmat      : PMatrix;
                          X_min     : PVector;
                          var F_min : Float);
{ ------------------------------------------------------------------
  Performs one cycle of the Metropolis-Hastings algorithm
  ------------------------------------------------------------------ }
  var
    F, F1         : Float;    { Function values }
    DeltaF        : Float;    { Variation of function }
    Sum           : Float;    { Statistical sum }
    X1            : PVector;  { New coordinates }
    L             : PMatrix;  { Standard dev. or Cholesky factor }
    S             : PVector;  { Standard deviations }
    I, J, K       : Integer;  { Loop variables }
    Iter          : Integer;  { Iteration count }
    FirstSavedSim : Integer;  { Index of first simulation to be saved }
  begin
    { Dimension arrays }
    DimVector(S, Ub);
    DimVector(X1, Ub);
    DimMatrix(L, Ub, Ub);

    { Compute SD's or Cholesky factor }
    if Indep then
      CalcSD(V, S, Lb, Ub)
    else
      Cholesky(V, L, Lb, Ub);

    if MathErr = MatNotPD then Exit;

    { Compute initial function value }
    F := Func(X);

    { Perform MH_MaxSim simulations at constant temperature }
    FirstSavedSim := MH_MaxSim - MH_SavedSim + 1;
    Iter := 1;
    K := 1;

    repeat
      { Generate new vector }
      if Indep then
        RanMultIndep(X, S, Lb, Ub, X1)
      else
        RanMult(X, L, Lb, Ub, X1);

      { Compute new function value }
      F1 := Func(X1);
      DeltaF := F1 - F;

      { Check for acceptance }
      if Accept(DeltaF, T) then
        begin
          Write('.');  { Only for command-line programs }

          for I := Lb to Ub do
            X^[I] := X1^[I];

          if Iter >= FirstSavedSim then
            begin
              { Save simulated vector into line K of matrix Xmat }
              for I := Lb to Ub do
                Xmat^[K]^[I] := X1^[I];
              Inc(K);
            end;

          if F1 < F_min then
            begin
              { Update minimum }
              for I := Lb to Ub do
                X_min^[I] := X1^[I];
              F_min := F1;
            end;

          F := F1;
          Inc(Iter);
        end;
    until Iter > MH_MaxSim;

    { Update mean vector }
    for I := Lb to Ub do
      begin
        Sum := 0.0;
        for K := 1 to MH_SavedSim do
          Sum := Sum + Xmat^[K]^[I];
        X^[I] := Sum / MH_SavedSim;
      end;

    { Update variance-covariance matrix }
    for I := Lb to Ub do
      for J := I to Ub do
        begin
          Sum := 0.0;
          for K := 1 to MH_SavedSim do
            Sum := Sum + (Xmat^[K]^[I] - X^[I]) * (Xmat^[K]^[J] - X^[J]);
          V^[I]^[J] := Sum / MH_SavedSim;
        end;

    for I := Succ(Lb) to Ub do
      for J := Lb to Pred(I) do
        V^[I]^[J] := V^[J]^[I];

    DelVector(S, Ub);
    DelVector(X1, Ub);
    DelMatrix(L, Ub, Ub);
  end;

  procedure Hastings(Func      : TFuncNVar;
                     T         : Float;
                     X         : PVector;
                     V         : PMatrix;
                     Lb, Ub    : Integer;
                     Xmat      : PMatrix;
                     X_min     : PVector;
                     var F_min : Float);
  var
    K     : Integer;
    Indep : Boolean;
  begin
    { Initialize the Marsaglia random number generator
      using the standard Pascal generator }
    Randomize;
    InitGen(Trunc(Random * 1.0E+8));

    K := 1;
    Indep := True;
    F_min := MaxNum;

    repeat
      HastingsCycle(Func, T, X, V, Lb, Ub, Indep, Xmat, X_min, F_min);
      Indep := False;
      Inc(K);
    until (MathErr <> MatOk) or (K > MH_NCycles);
  end;

end.