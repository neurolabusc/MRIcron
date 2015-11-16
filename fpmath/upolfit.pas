{ ******************************************************************
  Polynomial regression : Y = B(0) + B(1) * X + B(2) * X^2 + ...
  ****************************************************************** }

unit upolfit;

interface

uses
  utypes, ulineq;

procedure PolFit(X, Y        : PVector;
                 Lb, Ub, Deg : Integer;
                 B           : PVector;
                 V           : PMatrix);
{ ------------------------------------------------------------------
  Unweighted polynomial regression
  ------------------------------------------------------------------
  Input parameters:  X, Y   = point coordinates
                     Lb, Ub = array bounds
                     Deg    = degree of polynomial
  Output parameters: B      = regression parameters
                     V      = inverse matrix
  ------------------------------------------------------------------ }

procedure WPolFit(X, Y, S     : PVector;
                  Lb, Ub, Deg : Integer;
                  B           : PVector;
                  V           : PMatrix);
{ ------------------------------------------------------------------
  Weighted polynomial regression
  ------------------------------------------------------------------
  Additional input parameter:
  S = standard deviations of observations
  ------------------------------------------------------------------ }

implementation

procedure PolFit(X, Y        : PVector;
                 Lb, Ub, Deg : Integer;
                 B           : PVector;
                 V           : PMatrix);
var
  I, I1, J, K, D1 : Integer;
  XI, Det         : Float;

begin
  if Ub - Lb < Deg then
    begin
      SetErrCode(MatErrDim);
      Exit;
    end;

  { Initialize }
  for I := 0 to Deg do
    begin
      for J := 0 to Deg do
        V^[I]^[J] := 0.0;
      B^[I] := 0.0;
    end;

  V^[0]^[0] := Ub - Lb + 1;

  for K := Lb to Ub do
  begin
    XI := X^[K];                 { x^i }
    B^[0] := B^[0] + Y^[K];
    V^[0]^[1] := V^[0]^[1] + XI;
    B^[1] := B^[1] + XI * Y^[K];

    for I := 2 to Deg do
    begin
      XI := XI * X^[K];
      V^[0]^[I] := V^[0]^[I] + XI;   { First line of matrix: 1 --> x^d }
      B^[I] := B^[I] + XI * Y^[K];   { Constant vector: y --> x^d.y  }
    end;

    for I := 1 to Deg do
    begin
      XI := XI * X^[K];
      V^[I]^[Deg] := V^[I]^[Deg] + XI;  { Last col. of matrix: x^d --> x^2d }
    end;
  end;

  { Fill lower matrix }
  D1 := Deg - 1;
  for I := 1 to Deg do
  begin
    I1 := I - 1;
    for J := 0 to D1 do
      V^[I]^[J] := V^[I1]^[J + 1];
  end;

  { Solve system }
  LinEq(V, B, 0, Deg, Det);
end;

procedure WPolFit(X, Y, S     : PVector;
                  Lb, Ub, Deg : Integer;
                  B           : PVector;
                  V           : PMatrix);
var
  I, I1, J, K, D1 : Integer;
  W, WXI, Det     : Float;

begin
  if Ub - Lb < Deg then
    begin
      SetErrCode(MatErrDim);
      Exit;
    end;

  { Initialize }
  for I := 0 to Deg do
    begin
      for J := 0 to Deg do
        V^[I]^[J] := 0.0;
      B^[I] := 0.0;
    end;

  for K := Lb to Ub do
  begin
    if S^[K] <= 0.0 then
      begin
        SetErrCode(MatSing);
        Exit;
      end;

    W := 1.0 / Sqr(S^[K]);
    WXI := W * X^[K];                 { w.x^i }
    V^[0]^[0] := V^[0]^[0] + W;
    B^[0] := B^[0] + W * Y^[K];
    V^[0]^[1] := V^[0]^[1] + WXI;
    B^[1] := B^[1] + WXI * Y^[K];

    for I := 2 to Deg do
    begin
      WXI := WXI * X^[K];
      V^[0]^[I] := V^[0]^[I] + WXI;   { First line of matrix: w --> w.x^d }
      B^[I] := B^[I] + WXI * Y^[K];   { Constant vector: w.y --> w.x^d.y  }
    end;

    for I := 1 to Deg do
    begin
      WXI := WXI * X^[K];
      V^[I]^[Deg] := V^[I]^[Deg] + WXI;  { Last col. of matrix: w.x^d --> w.x^2d }
    end;
  end;

  { Fill lower matrix }
  D1 := Deg - 1;
  for I := 1 to Deg do
  begin
    I1 := I - 1;
    for J := 0 to D1 do
      V^[I]^[J] := V^[I1]^[J + 1];
  end;

  { Solve system }
  LinEq(V, B, 0, Deg, Det);
end;

end.
