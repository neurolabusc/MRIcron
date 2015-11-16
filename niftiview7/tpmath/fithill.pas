{ **********************************************************************
  *                          Unit FITHILL.PAS                          *
  *                            Version 1.1                             *
  *                     (c) J. Debord, August 2000                     *
  **********************************************************************
  This unit fits the Hill equation :

                                  Ymax . x^n
                              y = ----------
                                  K^n + x^n

  ********************************************************************** }

unit FitHill;

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
    FuncName := 'y = Ymax . x^n / (K^n + x^n)';
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
      0 : ParamName := 'Ymax';
      1 : ParamName := 'K   ';
      2 : ParamName := 'n   ';
    end;
  end;

  function RegFunc(X : Float; B : PVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B^[0] = Ymax     B^[1] = K     B^[2] = n
    -------------------------------------------------------------------- }
  begin
    if X = 0.0 then
      if B^[2] > 0.0 then RegFunc := 0.0 else RegFunc := B^[0]
    else
      { Compute function according to y = Ymax / [1 + (K/x)^n] }
      RegFunc := B^[0] / (1.0 + Power(B^[1] / X, B^[2]));
  end;

  procedure DerivProc(X, Y : Float; B, D : PVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point (X,Y)
    with respect to the parameters B. The results are returned in D.
    D^[I] contains the derivative with respect to the I-th parameter
    -------------------------------------------------------------------- }
  var
    Q, R, S : Float;
  begin
    if X = 0.0 then
      begin
        if B^[2] > 0.0 then D^[0] := 0.0 else D^[0] := 1.0;
        D^[1] := 0.0;
        D^[2] := 0.0;
      end
    else
      begin
        Q := Power(B^[1] / X, B^[2]);  { (K/x)^n }
        R := 1.0 / (1.0 + Q);          { 1 / [1 + (K/x)^n] }
        S := - Y * R * Q;              { -Ymax.(K/x)^n / [1 + (K/x)^n]^2 }

        { dy/dYmax = 1 / [1 + (K/x)^n] }
        D^[0] := R;

        { dy/dK = -Ymax.(K/x)^n.(n/K)/[1 + (K/x)^n]^2 }
        D^[1] := S * B^[2] / B^[1];

        { dy/dn = -Ymax.(K/x)^n.Ln(K/x)/[1 + (K/x)^n]^2 }
        D^[2] := S * Log(B^[1] / X);
      end;
  end;

  function FitModel(Method : Integer; X, Y, W : PVector;
                    N : Integer; B : PVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of the Hill equation by linear regression:
    Ln(Ymax/y - 1) = n.Ln(K) - n.Ln(x)
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    Ymax : Float;       { Estimated value of Ymax }
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
    Ymax := Max(Y, 1, N);
    for K := 1 to N do
      if (X^[K] > 0.0) and (Y^[K] > 0.0) and (Y^[K] < Ymax) then
        begin
          Inc(P);
          X1^[P] := Log(X^[K]);
          Y1^[P] := Log(Ymax / Y^[K] - 1.0);
          W1^[P] := Sqr(Y^[K] * (1.0 - Y^[K] / Ymax));
          if Method = 1 then W1^[P] := W1^[P] * W^[K];
        end;

    ErrCode := WLinFit(X1, Y1, W1, P, A, V);

    if ErrCode = MAT_OK then
      begin
        B^[0] := Ymax;
        B^[1] := Expo(- A^[0] / A^[1]);
        B^[2] := - A^[1];
      end;

    FitModel := ErrCode;

    DelVector(X1, N);
    DelVector(Y1, N);
    DelVector(W1, N);
    DelVector(A, 1);
    DelMatrix(V, 1, 1);
  end;

  end.
