{ **********************************************************************
  *                          Unit FITMICH.PAS                          *
  *                            Version 1.0                             *
  *                     (c) J. Debord, April 1998                      *
  **********************************************************************
  This unit fits the Michaelis equation :

                                  Ymax . x
                              y = --------
                                   Km + x

  ********************************************************************** }

unit FitMich;

{$F+}

interface

uses
  FMath, Matrices, Stat, Regress;

function FuncName : String;

function FirstParam : Integer;

function LastParam : Integer;

function ParamName(I : Integer) : String;

function RegFunc(X : Float; B : PVector) : Float;

procedure DerivProc(X, Y : Float; B, D : PVector);

function FitModel(Method : Integer; X, Y, W : PVector;
                  N : Integer; B : PVector) : Integer;

implementation

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function
    -------------------------------------------------------------------- }
  begin
    FuncName := 'y = Ymax . x / (Km + x)';
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
      0 : ParamName := 'Ymax';
      1 : ParamName := 'Km  ';
    end;
  end;

  function RegFunc(X : Float; B : PVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B^[0] = Ymax     B^[1] = Km
    -------------------------------------------------------------------- }
  begin
    RegFunc := B^[0] * X / (B^[1] + X);
  end;

  procedure DerivProc(X, Y : Float; B, D : PVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point (X,Y)
    with respect to the parameters B. The results are returned in D.
    D^[I] contains the derivative with respect to the I-th parameter
    -------------------------------------------------------------------- }
  begin
    D^[0] := Y / B^[0];          { dy/dYmax = x / (Km + x) }
    D^[1] := - Y / (B^[1] + X);  { dy/dKm = - Ymax.x / (Km + x)^2 }
  end;

  function FitModel(Method : Integer; X, Y, W : PVector;
                    N : Integer; B : PVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of the Michaelis equation by linear regression:
    1/y = 1/Ymax + (Km/Ymax) * (1/x)
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    X1, Y1 : PVector;   { Transformed coordinates }
    W1 : PVector;       { Weights }
    A : PVector;        { Linear regression parameters }
    V : PMatrix;        { Variance-covariance matrix }
    P : Integer;        { Number of points for linear regression }
    K : Integer;        { Loop variable }
    ErrCode : Integer;  { Error code }
  begin
    DimVector(X1, N);
    DimVector(Y1, N);
    DimVector(W1, N);
    DimVector(A, 1);
    DimMatrix(V, 1, 1);

    P := 0;
    for K := 1 to N do
      if (X^[K] > 0.0) and (Y^[K] > 0.0) then
        begin
          Inc(P);
          X1^[P] := 1.0 / X^[K];
          Y1^[P] := 1.0 / Y^[K];
          W1^[P] := Sqr(Sqr(Y^[K]));
          if Method = 1 then W1^[P] := W1^[P] * W^[K];
        end;

    ErrCode := WLinFit(X1, Y1, W1, P, A, V);

    if ErrCode = MAT_OK then
      begin
        B^[0] := 1.0 / A^[0];
        B^[1] := A^[1] / A^[0];
      end;

    FitModel := ErrCode;

    DelVector(X1, N);
    DelVector(Y1, N);
    DelVector(W1, N);
    DelVector(A, 1);
    DelMatrix(V, 1, 1);
  end;

  end.
