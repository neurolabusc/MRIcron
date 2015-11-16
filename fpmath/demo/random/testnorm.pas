{ ******************************************************************
  Test of Gaussian random number generator.

  This program picks a random sample of size N from a gaussian
  distribution with known mean and standard deviation (SD),
  estimates mean and SD from the sample, and computes a 95%
  confidence interval (CI) for the mean (i.e. an interval which
  has a probability of 0.95 to include the true mean).
  ****************************************************************** }

program testnorm;

uses
  tpmath;

const
  Mu    = 10.0;  { Mean of Gaussian distribution }
  Sigma = 2.0;   { Standard deviation of Gaussian distribution }
  N     = 100;   { Sample size, must be > 30 }

var
  X      : PVector;  { Sample values }
  M, S   : Float;    { Sample mean & SD }
  Delta  : Float;    { Half-width of CI }
  M1, M2 : Float;    { Bounds of CI }
  I      : Integer;  { Loop variable }

begin
  { Select generator }
  SetRNG(RNG_MT);

  { Dimension array }
  DimVector(X, N);

  { Pick sample values }
  for I := 1 to N do
    X^[I] := RanGauss(Mu, Sigma);

  { Estimate mean and SD from sample }
  M := Mean(X, 1, N);
  S := StDev(X, 1, N, M);

  { Compute 95% CI, assuming that the sample mean is normally distributed.
    This requires N > 30 }
  Delta := 1.96 * S / Sqrt(N);
  M1 := M - Delta;
  M2 := M + Delta;

  { Output results }
  WriteLn;
  WriteLn('Population mean = ', Mu:10:4);
  WriteLn('Population SD   = ', Sigma:10:4);
  WriteLn;
  WriteLn('Sample size     = ', N:10);
  WriteLn('Sample mean     = ', M:10:4);
  WriteLn('Sample SD       = ', S:10:4);
  WriteLn;
  WriteLn('95% CI of mean  = [', M1:10:4, ' , ', M2:10:4, ' ]');
end.
