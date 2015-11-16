{ ******************************************************************
  Two-way analysis of variance
  ****************************************************************** }

unit uanova2;

interface

uses
  utypes;

procedure AnOVa2(NA, NB, Nobs : Integer;
                 M, S         : PMatrix;
                 V, F         : PVector;
                 DoF          : PIntVector);
{ ------------------------------------------------------------------
  Input parameters : NA   = number of modalities for factor A
                     NB   = number of modalities for factor B
                     Nobs = number of observations for each sample
                     M    = matrix of means
                            (factor A as lines, factor B as columns)
                     S    = matrix of standard deviations
  Output parameters: V    = variances
                            (factor A, factor B, interaction, residual)
                     F    = variance ratios
                            (factor A, factor B, interaction)
                     DoF  = degrees of freedom
                            (factor A, factor B, interaction, residual)
  ------------------------------------------------------------------ }

implementation

procedure AnOVa2(NA, NB, Nobs : Integer;
                 M, S         : PMatrix;
                 V, F         : PVector;
                 DoF          : PIntVector);

var
  I, J, P : Integer;
  Xbar    : Float;    { Global mean }
  D       : Float;    { Difference of means }
  Sum     : Float;    { Intermediate sum }
  ML, MC  : PVector;  { Line and columns means }
  SS      : PVector;  { Sum of squares }

begin
  if (NA < 2) or (NB < 2) or (Nobs < 1) then
    begin
      SetErrCode(MatErrDim);
      Exit
    end;

  DimVector(ML, NA);
  DimVector(MC, NB);
  DimVector(SS, 3);

  SetErrCode(MatOk);

  { Line means }
  for I := 1 to NA do
    begin
      Sum := 0.0;
      for J := 1 to NB do
        Sum := Sum + M^[I]^[J];
      ML^[I] := Sum / NB;
    end;

  { Column means }
  for J := 1 to NB do
    begin
      Sum := 0.0;
      for I := 1 to NA do
        Sum := Sum + M^[I]^[J];
      MC^[J] := Sum / NA;
    end;

  { Global mean }
  Sum := 0.0;
  for I := 1 to NA do
    Sum := Sum + ML^[I];

  Xbar := Sum / NA;

  { Residual variance }
  if Nobs = 1 then
    V^[4] := 0.0
  else
    begin
      Sum := 0.0;
      for I := 1 to NA do
        for J := 1 to NB do
          Sum := Sum + Sqr(S^[I]^[J]);
      P := NA * NB;
      DoF^[4] := P * (Nobs - 1);
      V^[4] := Sum / P;
    end;

  { Factorial sum of squares }
  Sum := 0.0;
  for I := 1 to NA do
    for J := 1 to NB do
      begin
        D := M^[I]^[J] - Xbar;
        Sum := Sum + Sqr(D)
      end;
  SS^[0] := Nobs * Sum;

  { Factorial variance (factor A) }
  Sum := 0.0;
  for I := 1 to NA do
    begin
      D := ML^[I] - Xbar;
      Sum := Sum + Sqr(D)
    end;
  SS^[1] := NB * Nobs * Sum;
  DoF^[1] := NA - 1;
  V^[1] := SS^[1] / DoF^[1];

  { Factorial variance (factor B) }
  Sum := 0.0;
  for J := 1 to NB do
    begin
      D := MC^[J] - Xbar;
      Sum := Sum + Sqr(D)
    end;
  SS^[2] := NA * Nobs * Sum;
  DoF^[2] := NB - 1;
  V^[2] := SS^[2] / DoF^[2];

  { Factorial variance (interaction) }
  SS^[3] := SS^[0] - SS^[1] - SS^[2];
  DoF^[3] := DoF^[1] * DoF^[2];
  V^[3] := SS^[3] / DoF^[3];

  { Variance ratios }
  if Nobs = 1 then
    begin
      F^[1] := V^[1] / V^[3];
      F^[2] := V^[2] / V^[3]
    end
  else
    begin
      F^[1] := V^[1] / V^[4];
      F^[2] := V^[2] / V^[4];
      F^[3] := V^[3] / V^[4];
    end;
end;

end.