{ ******************************************************************
  Two-way analysis of variance (one observation per sample)
  ****************************************************************** }

program av2a;

uses
  tpmath;

const
  NA    = 3;            { Number of modalities of factor A }
  NB    = 4;            { Number of modalities of factor B }
  Alpha = 0.05;         { Significance level }
  Prob  = 1.0 - Alpha;  { Probability }

{ The samples are stored in a matrix Z, such that
  Z[I, J] contains the observation for the I-th
  modality of factor A and the J-th modality of factor B }

const
  Z : array[1..NA, 1..NB] of Float =
((2, 1, 3, 1),
 (3, 2, 3, 2),
 (3, 4, 5, 3));

var
  M : PMatrix;  { Means }
  S : PMatrix;  { Standard deviations }

  { Note: The S matrix does not need to be dimensioned if there is
    only one observation per sample. However, it must be declared. }

  V    : PVector;     { Variances (A, B, interaction) }
  DoF  : PIntVector;  { Degrees of freedom (A, B, interaction) }
  F    : PVector;     { Variance ratios (A, B) }
  Fc   : PVector;     { Critical values }
  I, J : Integer;     { Loop variables }

begin
  DimMatrix(M, NA, NB);  { Means }
  DimMatrix(S, NA, NB);  { Standard deviations }
  DimVector(V, 3);       { Variances (A, B, interaction) }
  DimIntVector(DoF, 3);  { Degrees of freedom (A, B, interaction) }
  DimVector(F, 2);       { Variance ratios (A, B) }
  DimVector(Fc, 2);      { Critical values }

  { Compare means. The matrix of means is equal to the data matrix.
    The matrix of standard deviations will be ignored. }

  for I := 1 to NA do
    for J := 1 to NB do
      M^[I]^[J] := Z[I,J];

  AnOVa2(NA, NB, 1, M, S, V, F, DoF);

  { Compute critical values }
  for I := 1 to 2 do
    Fc^[I] := InvSnedecor(DoF^[I], DoF^[3], Prob);

  { Print results }
  WriteLn('Two-way ANOVA');
  WriteLn;
  WriteLn('Source         Variance    D.o.F.     F      F(p = ', Alpha:4:2, ')');
  WriteLn('--------------------------------------------------------');
  WriteLn('Factor A     ', V^[1]:10:4, DoF^[1]:10, F^[1]:10:4, Fc^[1]:10:4);
  WriteLn('Factor B     ', V^[2]:10:4, DoF^[2]:10, F^[2]:10:4, Fc^[2]:10:4);
  WriteLn('Interaction  ', V^[3]:10:4, DoF^[3]:10);
end.

