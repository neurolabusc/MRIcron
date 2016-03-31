{ **********************************************************************
  *                         Unit FITPOWER.PAS                          *
  *                            Version 1.1                             *
  *                    (c) J. Debord, August 2000                      *
  **********************************************************************
  This unit fits a power function :

                                y = A.x^n

  ********************************************************************** }

unit FitPower;

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
    Returns the name of the regression function.
    -------------------------------------------------------------------- }
  begin
    FuncName := 'y = A.x^n';
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted.
    -------------------------------------------------------------------- }
  begin
    FirstParam := 0;
  end;

  function LastParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the last parameter to be fitted.
    -------------------------------------------------------------------- }
  begin
    LastParam := 1;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter.
    -------------------------------------------------------------------- }
  begin
    case I of
      0 : ParamName := 'A';
      1 : ParamName := 'n';
    end;
  end;

  function RegFunc(X : Float; B : PVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X.
    B is the vector of parameters, such that :

    B^[0] = A     B^[1] = n
    -------------------------------------------------------------------- }
  begin
    RegFunc := B^[0] * Power(X, B^[1]);
  end;

  procedure DerivProc(X, Y : Float; B, D : PVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point (X,Y)
    with respect to the parameters B. The results are returned in D.
    D^[I] contains the derivative with respect to the I-th parameter.
    -------------------------------------------------------------------- }
  begin
    D^[0] := Y / B^[0];   { dy/dA = x^n }
    D^[1] := Y * Log(X);  { dy/dk = A.x^n.Ln(x) }
  end;

  function FitModel(Method : Integer; X, Y, W : PVector;
                     N : Integer; B : PVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of a power function by linear regression:
    Ln(y) = Ln(A) + n.Ln(x)
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
          X1^[P] := Log(X^[K]);
          Y1^[P] := Log(Y^[K]);
          W1^[P] := Sqr(Y^[K]);
          if Method = 1 then W1^[P] := W1^[P] * W^[K];
        end;

    ErrCode := WLinFit(X1, Y1, W1, P, A, V);

    if ErrCode = MAT_OK then
      begin
        B^[0] := Expo(A^[0]);
        B^[1] := A^[1];
      end;

    FitModel := ErrCode;

    DelVector(X1, N);
    DelVector(Y1, N);
    DelVector(W1, N);
    DelVector(A, 1);
    DelMatrix(V, 1, 1);
  end;

  end.
