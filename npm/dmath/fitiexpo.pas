{ **********************************************************************
  *                         Unit FITIEXPO.PAS                          *
  *                            Version 1.2                             *
  *                    (c) J. Debord, August 2000                      *
  **********************************************************************
  This unit fits the increasing exponential :

                          y = A.[1 - exp(-k.x)]

  ********************************************************************** }

unit FitIExpo;

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


implementation

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function
    -------------------------------------------------------------------- }
  begin
    FuncName := 'y = A[1 - exp(-k.x)]';
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    -------------------------------------------------------------------- }
  begin
    FirstParam := 0;
  end;

  function LastParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the last parameter to be fitted
    -------------------------------------------------------------------- }
  begin
    LastParam := 1;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter
    -------------------------------------------------------------------- }
  begin
    case I of
      0 : ParamName := 'A';
      1 : ParamName := 'k';
    end;
  end;

  function RegFunc(X : Float; B : PVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B^[0] = A     B^[1] = k
    -------------------------------------------------------------------- }
  begin
    RegFunc := B^[0] * (1.0 - Expo(- B^[1] * X));
  end;

  procedure DerivProc(X : Float; B, D : PVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point X
    with respect to the parameters B. The results are returned in D.
    D^[I] contains the derivative with respect to the I-th parameter.
    -------------------------------------------------------------------- }
  var
    E : Float;
  begin
    E := Expo(- B^[1] * X);  { exp(-k.x) }
    D^[0] := 1.0 - E;        { dy/dA = 1 - exp(-k.x) }
    D^[1] := B^[0] * X * E;  { dy/dk = A.x.exp(-k.x) }
  end;

  function FitModel(Method : Integer; X, Y, W : PVector;
                    N : Integer; B : PVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of the increasing exponential by linear regression:
    Ln(1 - y/A) = -k.x
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    Y1 : PVector;       { Transformed ordinates }
    W1 : PVector;       { Weights }
    A : PVector;        { Linear regression parameters }
    V : PMatrix;        { Variance-covariance matrix }
    K : Integer;        { Loop variable }
    ErrCode : Integer;  { Error code }
  begin
    DimVector(Y1, N);
    DimVector(W1, N);
    DimVector(A, 1);
    DimMatrix(V, 1, 1);

    { Estimation of A }
    B^[0] := 1.1 * Max(Y, 1, N);

    for K := 1 to N do
      begin
        Y1^[K] := Log(1.0 - Y^[K] / B^[0]);
        W1^[K] := Sqr(Y^[K] - B^[0]);
        if Method = 1 then W1^[K] := W1^[K] * W^[K];
      end;

    ErrCode := WLinFit(X, Y1, W1, N, A, V);

    if ErrCode = MAT_OK then
      B^[1] := - A^[1];

    FitModel := ErrCode;

    DelVector(Y1, N);
    DelVector(W1, N);
    DelVector(A, 1);
    DelMatrix(V, 1, 1);
  end;

  end.
