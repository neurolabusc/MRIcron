{ **********************************************************************
  *                          Unit FITEXPO.PAS                          *
  *                            Version 1.4                             *
  *                    (c) J. Debord, August 2000                      *
  **********************************************************************
  This unit fits a sum of decreasing exponentials :

      y = Ymin + A1.exp(-a1.x) + A2.exp(-a2.x) + A3.exp(-a3.x) + ...

  ********************************************************************** }

unit FitExpo;

{$F+}

interface

uses
  FMath, Matrices, Polynom, Stat, Regress;

const
  NO_REAL_ROOT = - 2;  { No real exponent }

function FuncName : String;

function FirstParam : Integer;

function LastParam : Integer;

function ParamName(I : Integer) : String;

function RegFunc(X : Float; B : PVector) : Float;

procedure DerivProc(X : Float; B, D : PVector);

function FitModel(Method : Integer; X, Y, W : PVector;
                  N : Integer; B : PVector) : Integer;

procedure InitModel(CstPar : PVector);


implementation

const
  N_exp : Integer = 1;        { Number of exponentials }
  ConsTerm : Boolean = True;  { Flags the presence of a constant term Ymin }

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function
    -------------------------------------------------------------------- }
  var
    I : Integer;
    Name, S : String;
  begin
    Name := 'y = ';
    if ConsTerm then
      Name := Name + 'Ymin + ';
    Name := Name + 'A1.exp(-a1.x)';
    for I := 2 to N_exp do
      begin
        Str(I, S);
        Name := Name + ' + A' + S + '.exp(-a' + S + '.x)';
      end;
    FuncName := Name;
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    (0 if there is a constant term Ymin, 1 otherwise)
    -------------------------------------------------------------------- }
  begin
    if ConsTerm then
      FirstParam := 0
    else
      FirstParam := 1;
  end;

  function LastParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the last parameter to be fitted
    -------------------------------------------------------------------- }
  begin
    LastParam := 2 * N_exp;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter
    -------------------------------------------------------------------- }
  var
    S : String;
  begin
    if I = 0 then
      ParamName := 'Ymin'
    else if Odd(I) then
      begin
        Str(Succ(I) div 2, S);
        ParamName := 'A' + S;
      end
    else
      begin
        Str(I div 2, S);
        ParamName := 'a' + S;
      end;
  end;

  function RegFunc(X : Float; B : PVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :
    B^[0] = Ymin
    B^[1] = A1         B^[2] = a1
    ...............................
    B^[2*i-1] = Ai     B^[2*i] = ai     i = 1..N_exp
    -------------------------------------------------------------------- }
  var
    I : Integer;
    S : Float;
  begin
    if ConsTerm then
      S := B^[0]
    else
      S := 0.0;
    for I := 1 to N_exp do
      S := S + B^[2 * I - 1] * Expo(- B^[2 * I] * X);
    RegFunc := S;
  end;

  procedure DerivProc(X : Float; B, D : PVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point X
    with respect to the parameters B. The results are returned in D.
    D^[I] contains the derivative with respect to the I-th parameter.
    -------------------------------------------------------------------- }
  var
    I, P, Q : Integer;
    E : Float;
  begin
    D^[0] := 1.0;                  { dy/dYmin = 1 }
    for I := 1 to N_exp do
      begin
        Q := 2 * I;
        P := Pred(Q);
        E := Expo(- B^[Q] * X);
        D^[P] := E;                { dy/dAi = exp(-ai.x) }
        D^[Q] := - X * B^[P] * E;  { dy/dai = -x.Ai.exp(-ai.x) }
      end;
  end;

  function FitModel(Method : Integer; X, Y, W : PVector;
                    N : Integer; B : PVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of a sum of exponentials by linear regression
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
    --------------------------------------------------------------------
    Ref. : R. GOMENI & C. GOMENI, Automod : A polyalgorithm for an
           integrated analysis of linear pharmacokinetic models
           Comput. Biol. Med., 1979, 9, 39-48
    -------------------------------------------------------------------- }
  var
    I, K, M : Integer;
    X1, Y1 : PVector;   { Modified coordinates }
    U : PMatrix;        { Variables for linear regression }
    P : PVector;        { Linear regression parameters }
    C, Z : PVector;     { Coefficients and roots of polynomial }
    V : PMatrix;        { Variance-covariance matrix }
    H : Float;          { Integration step }
    ErrCode : Integer;  { Error code }
  begin
    M := Pred(2 * N_exp);
    DimVector(X1, N);
    DimVector(Y1, N);
    DimMatrix(U, M, N);
    DimMatrix(V, M, M);
    DimVector(P, M);
    DimVector(C, N_exp);
    DimVector(Z, N_exp);
    CopyVector(X1, X, 1, N);
    CopyVector(Y1, Y, 1, N);

    { Change scale so that the X's begin at zero }
    if X^[1] <> 0.0 then
      for K := 1 to N do
        X1^[K] := X1^[K] - X^[1];

    { Estimate the constant term at 90% of the lowest observed value,
      then subtract it from each Y value }
    if ConsTerm then
      begin
        B^[0] := 0.9 * Min(Y1, 1, N);
        for K := 1 to N do
          Y1^[K] := Y1^[K] - B^[0];
      end;

    { ------------------------------------------------------------------
      Fit the linearized form of the function :

      y = p(0) + p(1) * x + p(2) * x^2 + ... + p(N_exp-1) * x^(N_exp-1)

                    (x                          (x    (x
         + p(N_exp) | y dx + ... + p(2*N_exp-1) | ....| y dx
                    )0                          )0    )0
      ------------------------------------------------------------------ }

    { Compute increasing powers of X }
    if N_exp > 1 then
      for K := 2 to N do
        begin
          U^[1]^[K] := X1^[K];
          for I := 2 to Pred(N_exp) do
            U^[I]^[K] := U^[I - 1]^[K] * X1^[K];
        end;

    { Compute integrals by the trapezoidal rule }
    for K := 2 to N do
      begin
        H := 0.5 * (X1^[K] - X1^[K - 1]);
        U^[N_exp]^[K] := U^[N_exp]^[K - 1] + (Y1^[K] + Y1^[K - 1]) * H;
        for I := Succ(N_exp) to M do
          U^[I]^[K] := U^[I]^[K - 1] + (U^[I - 1]^[K] + U^[I - 1]^[K - 1]) * H;
      end;

    { Fit the equation }
    case Method of
      0 : ErrCode := MulFit(U, Y1, N, M, True, P, V);
      1 : ErrCode := WMulFit(U, Y1, W, N, M, True, P, V);
    end;

    if ErrCode = MAT_SINGUL then
      FitModel := ErrCode
    else
      begin
      { ----------------------------------------------------------------
        The exponents are the roots of the polynomial :
        x^N_exp + p(N_exp) * x^(N_exp-1) - p(N_exp+1) * x^(N_exp-2) +...
        ---------------------------------------------------------------- }

        { Compute polynomial coefficients }
        C^[N_exp] := 1.0;
        for I := 1 to N_exp do
          if Odd(I) then
            C^[N_exp - I] := P^[N_exp + I - 1]
          else
            C^[N_exp - I] := - P^[N_exp + I - 1];

        { Solve polynomial }
        if RRootPol(C, N_exp, Z) <> N_exp then
          FitModel := NO_REAL_ROOT
        else
          begin
            { Sort exponents in decreasing order }
            DQSort(Z, 1, N_exp);

            { Compute the coefficients of the exponentials by
              linear regression on the exponential terms }
            for I := 1 to N_exp do
              for K := 1 to N do
                U^[I]^[K] := Expo(- Z^[I] * X1^[K]);

            case Method of
              0 : ErrCode := MulFit(U, Y1, N, N_exp, False, P, V);
              1 : ErrCode := WMulFit(U, Y1, W, N, N_exp, False, P, V);
            end;

            if ErrCode = MAT_SINGUL then
              FitModel := ErrCode
            else
              begin
                { Extract model parameters }
                for I := 1 to N_exp do
                  begin
                    { Correct for scale change if necessary }
                    if X^[1] <> 0.0 then
                      P^[I] := P^[I] * Expo(Z^[I] * X^[1]);

                    { Extract coefficients and exponents }
                    B^[2 * I - 1] := P^[I];  { Coefficients }
                    B^[2 * I] := Z^[I];      { Exponents }
                  end;
                FitModel := MAT_OK;
              end;
          end;
      end;

    DelVector(X1, N);
    DelVector(Y1, N);
    DelMatrix(U, M, N);
    DelMatrix(V, M, M);
    DelVector(P, M);
    DelVector(C, N_exp);
    DelVector(Z, N_exp);
  end;

  procedure InitModel(CstPar : PVector);
  { --------------------------------------------------------------------
    Initializes the global variables of the unit
    --------------------------------------------------------------------
    CstPar^[0] = number of exponentials
    CstPar^[1] = 1 to include a constant term (Ymin)
    -------------------------------------------------------------------- }
  var
    N : Integer;
  begin
    N := Round(CstPar^[0]);
    if N > 0 then N_exp := N;
    ConsTerm := (CstPar^[1] = 1);
  end;

end.
