{ ******************************************************************
  Test of regression
  ****************************************************************** }

unit uregtest;

interface

uses
  utypes, umeansd;

procedure RegTest(Y, Ycalc : PVector;
                  LbY, UbY : Integer;
                  V        : PMatrix;
                  LbV, UbV : Integer;
                  var Test : TRegTest);
{ ------------------------------------------------------------------
  Test of unweighted regression
  ------------------------------------------------------------------
  Input parameters:  Y, Ycalc = observed and calculated Y values
                     LbY, UbY = bounds of Y and Ycalc
                     V        = inverse matrix
                     LbV, UbV = bounds of V
  Output parameters: V        = variance-covariance matrix
                     Test     = test results
  ------------------------------------------------------------------ }

procedure WRegTest(Y, Ycalc, S : PVector;
                   LbY, UbY    : Integer;
                   V           : PMatrix;
                   LbV, UbV    : Integer;
                   var Test    : TRegTest);
{ ------------------------------------------------------------------
  Test of weighted regression
  ------------------------------------------------------------------
  Additional input parameter:
  S = standard deviations of observations
  ------------------------------------------------------------------ }

implementation

procedure RegTest(Y, Ycalc : PVector;
                  LbY, UbY : Integer;
                  V        : PMatrix;
                  LbV, UbV : Integer;
                  var Test : TRegTest);

  var
    Ybar    : Float;    { Average Y value }
    D       : Float;    { Difference }
    SSt     : Float;    { Total sum of squares }
    SSe     : Float;    { Explained sum of squares }
    SSr     : Float;    { Residual sum of squares }
    Nobs    : Integer;  { Number of observations }
    Npar    : Integer;  { Number of fitted parameters }
    I, J, K : Integer;  { Loop variables }

  begin
    Nobs := UbY - LbY + 1;
    Npar := UbV - LbV + 1;

    if Nobs <= Npar then
      begin
        SetErrCode(MatSing);
        Exit;
      end;

    SetErrCode(MatOk);

    Ybar := Mean(Y, LbY, UbY);

    SSt := 0.0;
    SSe := 0.0;
    SSr := 0.0;

    for K := LbY to UbY do
      begin
        D := Y^[K] - Ybar;
        SSt := SSt + Sqr(D);
        D := Ycalc^[K] - Ybar;
        SSe := SSe + Sqr(D);
        D := Y^[K] - Ycalc^[K];
        SSr := SSr + Sqr(D);
      end;

    with Test do
      begin
        Nu1 := Npar - 1;
        Nu2 := Nobs - Npar;
        R2 := SSe / SSt;
        R2a := 1.0 - (1.0 - R2) * (Nobs - 1) / Nu2;
        Vr := SSr / Nu2;

        if Vr = 0.0 then
          F := MaxNum
        else
          F := (SSe / Nu1) / Vr;
      end;

    { Compute variance-covariance matrix }
    for I := LbV to UbV do
      for J := I to UbV do
        V^[I]^[J] := V^[I]^[J] * Test.Vr;
    for I := Succ(LbV) to UbV do
      for J := LbV to Pred(I) do
        V^[I]^[J] := V^[J]^[I];
  end;

procedure WRegTest(Y, Ycalc, S : PVector;
                   LbY, UbY    : Integer;
                   V           : PMatrix;
                   LbV, UbV    : Integer;
                   var Test    : TRegTest);

  var
    Ybar    : Float;    { Average Y value }
    D       : Float;    { Difference }
    SW, SWY : Float;    { Statistical sums }
    SSt     : Float;    { Total sum of squares }
    SSe     : Float;    { Explained sum of squares }
    SSr     : Float;    { Residual sum of squares }
    Nobs    : Integer;  { Number of observations }
    Npar    : Integer;  { Number of fitted parameters }
    I, J, K : Integer;  { Loop variables }
    W       : PVector;  { Weights }

  begin
    Nobs := UbY - LbY + 1;
    Npar := UbV - LbV + 1;

    if Nobs <= Npar then
      begin
        SetErrCode(MatSing);
        Exit;
      end;

    DimVector(W, UbY);

    SW  := 0.0;
    SWY := 0.0;

    for K := LbY to UbY do
      begin
        if S^[K] <= 0.0 then
          begin
            SetErrCode(MatSing);
            DelVector(W, UbY);
            Exit;
          end;

        W^[K] := 1.0 / Sqr(S^[K]);

        SW  := SW + W^[K];
        SWY := SWY + W^[K] * Y^[K];
      end;

    Ybar := SWY / SW;

    SetErrCode(MatOk);

    SSt := 0.0;
    SSe := 0.0;
    SSr := 0.0;

    for K := LbY to UbY do
      begin
        D := Y^[K] - Ybar;
        SSt := SSt + W^[K] * Sqr(D);
        D := Ycalc^[K] - Ybar;
        SSe := SSe + W^[K] * Sqr(D);
        D := Y^[K] - Ycalc^[K];
        SSr := SSr + W^[K] * Sqr(D);
      end;

    with Test do
      begin
        Nu1 := Npar - 1;
        Nu2 := Nobs - Npar;
        R2 := SSe / SSt;
        R2a := 1.0 - (1.0 - R2) * (Nobs - 1) / Nu2;
        Vr := SSr / Nu2;

        if Vr = 0.0 then
          F := MaxNum
        else
          F := (SSe / Nu1) / Vr;
      end;

    { Compute variance-covariance matrix }
    for I := LbV to UbV do
      for J := I to UbV do
        V^[I]^[J] := V^[I]^[J] * Test.Vr;
    for I := Succ(LbV) to UbV do
      for J := LbV to Pred(I) do
        V^[I]^[J] := V^[J]^[I];
  end;

end.

