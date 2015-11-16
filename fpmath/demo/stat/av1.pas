{ ******************************************************************
  One-way analysis of variance
  ****************************************************************** }

program av1;

uses
  tpmath;

const
  Nsamples =  5;           { Number of samples }
  Nmax     = 12;           { Max. number of observations per sample }
  Alpha    =  0.05;        { Significance level }
  Prob     = 1.0 - Alpha;  { Probability }

{ Sample matrix (one sample per column) }
const
  A : array[1..Nmax, 1..Nsamples] of Float =
((7.2,  4.9, 10.4,  4.6,  6.1),
 (4.3,  4.8,  4.6,  5.6, 11.4),
 (5.5,  4.7,  8.4,  8.3,  8.2),
 (4.6,  5.4,  6.1,  6.9,  5.7),
 (4.7,  4.7,  8.1,  4.5,  6.6),
 (5.5,  4.7,  5.4,  4.7,  6.6),
 (6.6,  6.2,  6.7,  6.7,  6.3),
 (5.3,  5.6,  7.5,  4.8,  5.9),
 (5.4,  3.2,  6.4,  5.0,  5.8),
 (3.9,  6.1,  5.6,  5.0,  4.8),
 (5.5,  6.7,  6.3,  5.3,  9.1),
 (2.7,  5.5,  7.7,  7.8, 13.2));

var
  X : PVector;     { Sample }
  Z : PMatrix;     { Sample matrix }
  N : PIntVector;  { Sizes }
  M : PVector;     { Means }
  S : PVector;     { Standard dev. }

  V_f, V_r, F : Float;  { Variances and variance ratio }
  Khi2        : Float;  { Bartlett's khi-2 }
  H           : Float;  { Kruskal-Wallis H }
  Fc, K2c     : Float;  { Critical values }

  DoF_f, DoF_r, DoF : Integer;  { Degrees of freedom }
  J                 : Integer;  { Loop variable }

procedure GetSample(J : Integer; X : PVector);
{ Get sample J from matrix A into vector X }
var
  I : Integer;
begin
  for I := 1 to Nmax do
    X^[I] := A[I, J];
end;

procedure GetSampleMatrix(Z : PMatrix);
{ Get sample matrix into Z }
var
  I, J : Integer;
begin
  for I := 1 to Nmax do
    for J := 1 to Nsamples do
      Z^[I]^[J] := A[I, J];
end;

begin
  { Dimension arrays }
  DimVector(X, Nmax);            { Sample }
  DimMatrix(Z, Nmax, Nsamples);  { Sample matrix }
  DimIntVector(N, Nsamples);     { Sizes }
  DimVector(M, Nsamples);        { Means }
  DimVector(S, Nsamples);        { Standard dev. }

  { Compute sizes, means and SD's }
  for J := 1 to Nsamples do
    begin
      GetSample(J, X);
      N^[J] := Nmax;
      M^[J] := Mean(X, 1, N^[J]);
      S^[J] := StDev(X, 1, N^[J], M^[J]);
    end;

  { Compare means and variances (parametric tests) }
  AnOVa1(Nsamples, N, M, S, V_f, V_r, F, DoF_f, DoF_r);
  Bartlett(Nsamples, N, S, Khi2, DoF);

  { Compare means (non-parametric test) }
  GetSampleMatrix(Z);
  Kruskal_Wallis(Nsamples, N, Z, H, DoF);

  { Compute critical values }
  Fc := InvSnedecor(DoF_f, DoF_r, Prob);
  K2c := InvKhi2(DoF, Prob);

  Writeln('Sample     Mean     St.Dev.');
  Writeln('---------------------------');

  for J := 1 to Nsamples do
    Writeln(J:6, M^[J]:10:4, S^[J]:10:4);

  Writeln;

  Writeln('Comparison of means (One-way analysis of variance):');
  Writeln;
  Writeln('Factorial variance        = ', V_f:10:4, '  (', DoF_f, ' DoF)');
  Writeln('Residual variance         = ', V_r:10:4, '  (', DoF_r, ' DoF)');
  Writeln('Variance ratio            = ', F:10:4);
  Writeln('Critical value (p = ', Alpha:4:2, ') = ', Fc:10:4);

  Writeln;

  Writeln('Comparison of variances:');
  Writeln;
  Writeln('Bartlett''s Khi-2          = ', Khi2:10:4, '  (', DoF, ' DoF)');
  Writeln('Critical value (p = ', Alpha:4:2, ') = ', K2c:10:4);

  Writeln;

  Writeln('Comparison of means (Non-parametric test):');
  Writeln;
  Writeln('Kruskal-Wallis H          = ', H:10:4, '  (', DoF, ' DoF)');
  Writeln('Critical value (p = ', Alpha:4:2, ') = ', K2c:10:4);
end.
