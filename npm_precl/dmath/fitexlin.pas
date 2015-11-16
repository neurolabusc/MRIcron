{ **********************************************************************
  *                         Unit FITEXLIN.PAS                          *
  *                            Version 1.1                             *
  *                    (c) J. Debord, August 2000                      *
  **********************************************************************
  This unit fits the "exponential + linear" model:

                        y = A.[1 - exp(-k.x)] + B.x

  ********************************************************************** }

unit FitExLin;

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

function FitModel(X, Y : PVector; N : Integer; B : PVector) : Integer;


implementation

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function
    -------------------------------------------------------------------- }
  begin
    FuncName := 'y = A[1 - exp(-k.x)] + B.x';
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
    LastParam := 2;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter
    -------------------------------------------------------------------- }
  begin
    case I of
      0 : ParamName := 'A';
      1 : ParamName := 'k';
      2 : ParamName := 'B';
    end;
  end;

  function RegFunc(X : Float; B : PVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B^[0] = A     B^[1] = k     B^[2] = B
    -------------------------------------------------------------------- }
  begin
    RegFunc := B^[0] * (1.0 - Expo(- B^[1] * X)) + B^[2] * X;
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
    D^[2] := X;              { dy/dB = x }
  end;

  function FitModel(X, Y : PVector; N : Integer; B : PVector) : Integer;
  { --------------------------------------------------------------------
    Computes initial estimates of the regression parameters
    --------------------------------------------------------------------
    Input :  N    = number of points
             X, Y = point coordinates
    Output : B    = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    K : Integer;
    D : Float;
  begin
    { B is the slope of the last (linear) part of the curve }
    K := Round(0.9 * N);
    if K = N then K := Pred(N);
    B^[2] := (Y^[N] - Y^[K]) / (X^[N] - X^[K]);

    { A is the intercept of the linear part }
    B^[0] := Y^[N] - B^[2] * X^[N];

    { Slope of the tangent at origin = B + k.A }
    K := Round(0.1 * N);
    if K = 1 then K := 2;
    D := (Y^[K] - Y^[1]) / (X^[K] - X^[1]);
    B^[1] := (D - B^[1]) / B^[0];

    FitModel := 0;
  end;

  end.
