{ ******************************************************************
  Multiple linear regression (Gauss-Jordan method)
  ****************************************************************** }

unit umulfit;

interface

uses
  utypes, ulineq;


procedure MulFit(X            : PMatrix;
                 Y            : PVector;
                 Lb, Ub, Nvar : Integer;
                 ConsTerm     : Boolean;
                 B            : PVector;
                 V            : PMatrix);
{ ------------------------------------------------------------------
  Multiple linear regression: Y = B(0) + B(1) * X + B(2) * X2 + ...
  ------------------------------------------------------------------
  Input parameters:  X        = matrix of independent variables
                     Y        = vector of dependent variable
                     Lb, Ub   = array bounds
                     Nvar     = number of independent variables
                     ConsTerm = presence of constant term B(0)
  Output parameters: B        = regression parameters
                     V        = inverse matrix
  ------------------------------------------------------------------ }

procedure WMulFit(X            : PMatrix;
                  Y, S         : PVector;
                  Lb, Ub, Nvar : Integer;
                  ConsTerm     : Boolean;
                  B            : PVector;
                  V            : PMatrix);
{ ----------------------------------------------------------------------
  Weighted multiple linear regression
  ----------------------------------------------------------------------
  S = standard deviations of observations
  Other parameters as in MulFit
  ---------------------------------------------------------------------- }

implementation

procedure MulFit(X            : PMatrix;
                 Y            : PVector;
                 Lb, Ub, Nvar : Integer;
                 ConsTerm     : Boolean;
                 B            : PVector;
                 V            : PMatrix);

  var
    Lb1     : Integer;  { Index of first param. (0 if cst term, 1 otherwise) }
    I, J, K : Integer;  { Loop variables }
    Det     : Float;    { Determinant }

  begin
    if Ub - Lb < Nvar then
      begin
        SetErrCode(MatErrDim);
        Exit;
      end;

    { Initialize }
    for I := 0 to Nvar do
      begin
        for J := 0 to Nvar do
          V^[I]^[J] := 0.0;
        B^[I] := 0.0;
      end;

    { If constant term, set line 0 and column 0 of matrix V }
    if ConsTerm then
      begin
        V^[0]^[0] := Int(Ub - Lb + 1);
        for K := Lb to Ub do
          begin
            for J := 1 to Nvar do
              V^[0]^[J] := V^[0]^[J] + X^[K]^[J];
            B^[0] := B^[0] + Y^[K];
          end;
        for J := 1 to Nvar do
          V^[J]^[0] := V^[0]^[J];
      end;

    { Set other elements of V }
    for K := Lb to Ub do
      for I := 1 to Nvar do
        begin
          for J := I to Nvar do
            V^[I]^[J] := V^[I]^[J] + X^[K]^[I] * X^[K]^[J];
          B^[I] := B^[I] + X^[K]^[I] * Y^[K];
        end;

    { Fill in symmetric matrix }
    for I := 2 to Nvar do
      for J := 1 to Pred(I) do
        V^[I]^[J] := V^[J]^[I];

    { Solve normal equations }
    if ConsTerm then Lb1 := 0 else Lb1 := 1;
    LinEq(V, B, Lb1, Nvar, Det);
  end;

procedure WMulFit(X            : PMatrix;
                  Y, S         : PVector;
                  Lb, Ub, Nvar : Integer;
                  ConsTerm     : Boolean;
                  B            : PVector;
                  V            : PMatrix);

  var
    Lb1     : Integer;  { Index of first param. (0 if cst term, 1 otherwise) }
    I, J, K : Integer;  { Loop variables }
    W       : PVector;  { Vector of weights }
    WX      : Float;    { W * X }
    Det     : Float;    { Determinant }

  begin
    if Ub - Lb < Nvar then
      begin
        SetErrCode(MatErrDim);
        Exit;
      end;

    for K := Lb to Ub do
      if S^[K] <= 0.0 then
        begin
          SetErrCode(MatSing);
          Exit;
        end;

    DimVector(W, Ub);

    for K := Lb to Ub do
      W^[K] := 1.0 / Sqr(S^[K]);

    { Initialize }
    for I := 0 to Nvar do
      begin
        for J := 0 to Nvar do
          V^[I]^[J] := 0.0;
        B^[I] := 0.0;
      end;

    { If constant term, set line 0 and column 0 of matrix V }
    if ConsTerm then
      begin
        for K := Lb to Ub do
          begin
            V^[0]^[0] := V^[0]^[0] + W^[K];
            for J := 1 to Nvar do
              V^[0]^[J] := V^[0]^[J] + W^[K] * X^[K]^[J];
            B^[0] := B^[0] + W^[K] * Y^[K];
          end;
        for J := 1 to Nvar do
          V^[J]^[0] := V^[0]^[J];
      end;

    { Set other elements of V }
    for K := Lb to Ub do
      for I := 1 to Nvar do
        begin
          WX := W^[K] * X^[K]^[I];
          for J := I to Nvar do
            V^[I]^[J] := V^[I]^[J] + WX * X^[K]^[J];
          B^[I] := B^[I] + WX * Y^[K];
        end;

    { Fill in symmetric matrix }
    for I := 2 to Nvar do
      for J := 1 to Pred(I) do
        V^[I]^[J] := V^[J]^[I];

    { Solve normal equations }
    if ConsTerm then Lb1 := 0 else Lb1 := 1;
    LinEq(V, B, Lb1, Nvar, Det);

    DelVector(W, Ub);
  end;

end.