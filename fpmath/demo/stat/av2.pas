{ ******************************************************************
  Two-way analysis of variance (several observations per sample)
  ****************************************************************** }

program av2;

uses
  tpmath;

const
  NA    =  2;           { Number of modalities of factor A }
  NB    =  2;           { Number of modalities of factor B }
  N     = 12;           { Number of observations for each sample }
  Alpha =  0.05;        { Significance level }
  Prob  = 1.0 - Alpha;  { Probability }

{ The samples are stored in a 3D array Z, such that
  Z[I, J, K] contains the K-th observation for the
  I-th modality of factor A and the J-th modality of factor B }

const
  Z : array[1..NA, 1..NB, 1..N] of Float =
(((4.9, 2.9, 2.7, 3.9, 4.6, 3.3, 5.9, 4.8, 4.1, 3.5, 7.2, 6.1),
  (2.1, 2.2, 1.1, 2.9, 5.0, 3.5, 2.4, 4.4, 2.1, 3.0, 3.9, 5.6)),
 ((4.5, 6.9, 4.0, 5.4, 1.9, 3.6, 4.8, 3.3, 7.5, 5.8, 4.4, 6.0),
  (2.4, 3.6, 4.8, 3.9, 5.5, 5.0, 6.8, 2.2, 3.1, 5.0, 4.1, 4.7)));

var
  X    : PVector;     { Sample }
  M, S : PMatrix;     { Means and SD }
  V    : PVector;     { Variances (A, B, interaction, residual) }
  DoF  : PIntVector;  { Degrees of freedom (A, B, interaction, residual) }
  F    : PVector;     { Variance ratios (A, B, interaction) }
  Fc   : PVector;     { Critical values }
  I, J : Integer;     { Loop variables }

procedure GetSample(I, J : Integer; X : PVector);
{ Get sample [I,J] from array Z into vector X }
var
  K : Integer;
begin
  for K := 1 to N do
    X^[K] := Z[I, J, K];
end;

begin
  DimVector(X, N);
  DimMatrix(M, NA, NB);
  DimMatrix(S, NA, NB);

  DimVector(V, 4);
  DimIntVector(DoF, 4);
  DimVector(F, 3);
  DimVector(Fc, 3);

  { Compute means and SD's }
  for I := 1 to NA do
    for J := 1 to NB do
      begin
        GetSample(I, J, X);
        M^[I]^[J] := Mean(X, 1, N);
        S^[I]^[J] := StDev(X, 1, N, M^[I]^[J]);
      end;

  { Compare means }
  AnOVa2(NA, NB, N, M, S, V, F, DoF);

  { Compute critical values }
  for I := 1 to 3 do
    Fc^[I] := InvSnedecor(DoF^[I], DoF^[4], Prob);

  { Print results }
  WriteLn('Two-way ANOVA');
  WriteLn;
  WriteLn('Source         Variance    D.o.F.     F      F(p = ', Alpha:4:2, ')');
  WriteLn('--------------------------------------------------------');
  WriteLn('Factor A     ', V^[1]:10:4, DoF^[1]:10, F^[1]:10:4, Fc^[1]:10:4);
  WriteLn('Factor B     ', V^[2]:10:4, DoF^[2]:10, F^[2]:10:4, Fc^[2]:10:4);
  WriteLn('Interaction  ', V^[3]:10:4, DoF^[3]:10, F^[3]:10:4, Fc^[3]:10:4);
  WriteLn('Residual     ', V^[4]:10:4, DoF^[4]:10);
end.
