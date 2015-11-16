{ **********************************************************************
  *                          Unit FITPKA.PAS                           *
  *                            Version 1.1                             *
  *                     (c) J. Debord, July 1999                       *
  **********************************************************************
  This unit fits the acid/base titration function :

                                    B - A
                       y = A + ----------------
                               1 + 10^(pKa - x)

  where x   is pH
        y   is some property (e.g. absorbance) which depends on the
            ratio of the acidic and basic forms of the compound
        A   is the property for the pure acidic form
        B   is the property for the pure basic form
        pKa is the acidity constant
  ********************************************************************** }

unit FitPKa;

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
    FuncName := 'y = A + (B - A) / [1 + 10^(pKa - x)]'
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    (0 if there is a constant term A, 1 otherwise)
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
      1 : ParamName := 'B';
      2 : ParamName := 'pKa';
    end;
  end;

  function RegFunc(X : Float; B : PVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :
    B^[0] = A     B^[1] = B     B^[2] = pKa
    -------------------------------------------------------------------- }
  begin
    RegFunc := B^[0] + (B^[1] - B^[0]) / (1.0 + Exp10(B^[2] - X));
  end;

  procedure DerivProc(X : Float; B, D : PVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point X
    with respect to the parameters B. The results are returned in D.
    D^[I] contains the derivative with respect to the I-th parameter.
    -------------------------------------------------------------------- }
  var
    Q, R : Float;
  begin
    Q := Exp10(B^[2] - X);  { 10^(pKa - x) }
    R := 1.0 / (1.0 + Q);   { 1/[1 + 10^(pKa - x)] }

    D^[0] := 1.0 - R;       { dy/dA = 1 - 1/[1 + 10^(pKa - x)] }
    D^[1] := R;             { dy/dB = 1/[1 + 10^(pKa - x)] }

    { dy/dpKa = (A-B).10^(pKa - x).Ln(10) / [1 + 10^(pKa - x)]^2 }
    D^[2] := (B^[0] - B^[1]) * Q * LN10 * Sqr(R);
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

  function FitModel(X, Y : PVector; N : Integer; B : PVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of the acid/base titration function
    --------------------------------------------------------------------
    Input :  X, Y = point coordinates
             N    = number of points
    Output : B    = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    K : Integer;  { Loop variable }
    Z : Float;    { (A + B) / 2 }
  begin
    SortPoints(X, Y, N);

    B^[0] := Y^[1];
    B^[1] := Y^[N];

    Z := 0.5 * (B^[0] + B^[1]);
    for K := 2 to N - 1 do
      if Y^[K] = Z then
        B^[2] := X^[K]
      else if ((Y^[K] < Z) and (Y^[K + 1] > Z)) or
              ((Y^[K] > Z) and (Y^[K + 1] < Z)) then
                B^[2] := 0.5 * (X^[K] + X^[K + 1]);

    FitModel := 0;
  end;

end.
