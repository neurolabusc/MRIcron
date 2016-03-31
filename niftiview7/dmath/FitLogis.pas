{ **********************************************************************
  *                         Unit FITLOGIS.PAS                          *
  *                            Version 1.4                             *
  *                    (c) J. Debord, August 2000                      *
  **********************************************************************
  This unit fits the logistic function :

                                      B - A
                        y = A + -----------------
                                1 + exp(-a.x + b)

  ********************************************************************** }

unit FitLogis;

{$F+}

interface

uses
  FMath, Matrices, Stat, Regress;

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

var
  ConsTerm : Boolean = True;  { Flags the presence of a constant term A }

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function.
    -------------------------------------------------------------------- }
  begin
    if ConsTerm then
      FuncName := 'y = A + (B - A) / [1 + exp(-a.x + b)]'
    else
      FuncName := 'y = B / [1 + exp(-a.x + b)]';
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    (0 if there is a constant term A, 1 otherwise)
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
    LastParam := 3;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter.
    -------------------------------------------------------------------- }
  begin
    case I of
      0 : ParamName := 'A';
      1 : ParamName := 'B';
      2 : ParamName := 'a';
      3 : ParamName := 'b';
    end;
  end;

  function RegFunc(X : Float; B : PVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X.
    B is the vector of parameters, such that :
    B^[0] = A     B^[1] = B     B^[2] = a     B^[3] = b
    -------------------------------------------------------------------- }
  begin
    if ConsTerm then
      RegFunc := B^[0] + (B^[1] - B^[0]) / (1.0 + Expo(- B^[2] * X + B^[3]))
    else
      RegFunc := B^[1] / (1.0 + Expo(- B^[2] * X + B^[3]));
  end;

  procedure DerivProc(X : Float; B, D : PVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point X
    with respect to the parameters B. The results are returned in D.
    D^[I] contains the derivative with respect to the I-th parameter
    -------------------------------------------------------------------- }
  var
    Q, R : Float;
  begin
    Q := Expo(- B^[2] * X + B^[3]);  { exp(-ax+b) }
    R := 1.0 / (1.0 + Q);            { 1 / [1 + exp(-ax+b)] }

    D^[0] := 1.0 - R;  { dy/dA = 1 - 1 / [1 + exp(-ax+b)] }
    D^[1] := R;        { dy/dB = 1 / [1 + exp(-ax+b)] }

    { dy/db = (A-B).exp(-ax+b) / [1 + exp(-ax+b)]^2 }
    D^[3] := (B^[0] - B^[1]) * Q * Sqr(R);

    { dy/da = (B-A).x.exp(-ax+b) / [1 + exp(-ax+b)]^2 }
    D^[2] := - D^[3] * X;
  end;

  procedure SortPoints(X, Y : PVector; N : Integer);
  { ----------------------------------------------------------------------
    Sort points by increasing X values
    ---------------------------------------------------------------------- }
  var
    I, J, K : Integer;
    A : Float;
  begin
    for I := 1 to Pred(N) do
      begin
        K := I;
        A := X^[I];
        for J := Succ(I) to N do
          if X^[J] < A then
            begin
              K := J;
              A := X^[J];
            end;
        FSwap(X^[I], X^[K]);
        FSwap(Y^[I], Y^[K]);
      end;
  end;

  function FitModel(Method : Integer; X, Y, W : PVector;
                    N : Integer; B : PVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of a logistic function by linear regression:
    Ln[(B - A)/(y - A) - 1] = -ax + b
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    XX : PVector;       { Transformed X coordinates }
    YY : PVector;       { Transformed Y coordinates }
    WW : PVector;       { Weights }
    A : PVector;        { Linear regression parameters }
    V : PMatrix;        { Variance-covariance matrix }
    P : Integer;        { Number of points for linear regression }
    K : Integer;        { Loop variable }
    ErrCode : Integer;  { Error code }
    D : Float;          { B - A }
  begin
    DimVector(XX, N);
    DimVector(YY, N);
    DimVector(WW, N);
    DimVector(A, 1);
    DimMatrix(V, 1, 1);

    SortPoints(X, Y, N);

    if ConsTerm then
      B^[0] := Y^[1]
    else
      B^[0] := 0.0;
    B^[1] := Y^[N];

    P := 0;
    D := B^[1] - B^[0];
    for K := 1 to N do
      if (X^[K] > X^[1]) and (X^[K] < X^[N]) then
        begin
          Inc(P);
          XX^[P] := X^[K];
          YY^[P] := Log(D / (Y^[K] - B^[0]) - 1.0);
          WW^[P] := Sqr((Y^[K] - B^[0]) * (Y^[K] - B^[1]) / D);
          if Method = 1 then WW^[P] := WW^[P] * W^[K];
        end;

    ErrCode := WLinFit(XX, YY, WW, P, A, V);

    if ErrCode = MAT_OK then
      begin
        B^[2] := - A^[1];
        B^[3] := A^[0];
      end;

    FitModel := ErrCode;

    DelVector(XX, N);
    DelVector(YY, N);
    DelVector(WW, N);
    DelVector(A, 1);
    DelMatrix(V, 1, 1);
  end;

  procedure InitModel(CstPar : PVector);
  { --------------------------------------------------------------------
    Initializes the global variables of the unit.
    --------------------------------------------------------------------
    CstPar^[0] = 1 to include a constant term (A)
    -------------------------------------------------------------------- }
  begin
    ConsTerm := (CstPar^[0] = 1);
  end;

  end.
