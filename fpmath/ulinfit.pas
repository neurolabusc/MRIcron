{ ******************************************************************
  Linear regression : Y = B(0) + B(1) * X
  ****************************************************************** }

unit ulinfit;

interface

uses
  utypes;

procedure LinFit(X, Y   : PVector;
                 Lb, Ub : Integer;
                 B      : PVector;
                 V      : PMatrix);
{ ------------------------------------------------------------------
  Unweighted linear regression
  ------------------------------------------------------------------
  Input parameters:  X, Y   = point coordinates
                     Lb, Ub = array bounds
  Output parameters: B      = regression parameters
                     V      = inverse matrix
  ------------------------------------------------------------------ }

procedure WLinFit(X, Y, S : PVector;
                  Lb, Ub  : Integer;
                  B       : PVector;
                  V       : PMatrix);
{ ------------------------------------------------------------------
  Weighted linear regression
  ------------------------------------------------------------------
  Additional input parameter:
  S = standard deviations of observations
  ------------------------------------------------------------------ }

implementation

procedure LinFit(X, Y   : PVector;
                 Lb, Ub : Integer;
                 B      : PVector;
                 V      : PMatrix);

  var
    SX, SY, SX2, SXY, D : Float;
    K, N                : Integer;

  begin
    N := Ub - Lb + 1;

    SX  := 0.0;
    SY  := 0.0;
    SX2 := 0.0;
    SXY := 0.0;

    for K := Lb to Ub do
      begin
        SX  := SX + X^[K];
        SY  := SY + Y^[K];
        SX2 := SX2 + Sqr(X^[K]);
        SXY := SXY + X^[K] * Y^[K];
      end;

    D := N * SX2 - Sqr(SX);

    if D <= 0.0 then
      begin
        SetErrCode(MatSing);
        Exit;
      end;

    SetErrCode(MatOk);

    V^[0]^[0] := SX2 / D;
    V^[0]^[1] := - SX / D;
    V^[1]^[0] := V^[0]^[1];
    V^[1]^[1] := N / D;

    B^[0] := V^[0]^[0] * SY + V^[0]^[1] * SXY;
    B^[1] := V^[1]^[0] * SY + V^[1]^[1] * SXY;
  end;

procedure WLinFit(X, Y, S : PVector;
                  Lb, Ub  : Integer;
                  B       : PVector;
                  V       : PMatrix);

  var
    W, WX, SW, SWX, SWY, SWX2, SWXY, D : Float;
    K                                  : Integer;

  begin
    SW   := 0.0;
    SWX  := 0.0;
    SWY  := 0.0;
    SWX2 := 0.0;
    SWXY := 0.0;

    for K := Lb to Ub do
      begin
        if S^[K] <= 0.0 then
          begin
            SetErrCode(MatSing);
            Exit;
          end;

        W := 1.0 / Sqr(S^[K]);
        WX := W * X^[K];

        SW   := SW + W;
        SWX  := SWX + WX;
        SWY  := SWY + W * Y^[K];
        SWX2 := SWX2 + WX * X^[K];
        SWXY := SWXY + WX * Y^[K];
      end;

    D := SW * SWX2 - Sqr(SWX);

    if D <= 0.0 then
      begin
        SetErrCode(MatSing);
        Exit;
      end;

    SetErrCode(MatOk);

    V^[0]^[0] := SWX2 / D;
    V^[0]^[1] := - SWX / D;
    V^[1]^[0] := V^[0]^[1];
    V^[1]^[1] := SW / D;

    B^[0] := V^[0]^[0] * SWY + V^[0]^[1] * SWXY;
    B^[1] := V^[1]^[0] * SWY + V^[1]^[1] * SWXY;
  end;

end.

