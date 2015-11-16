{ ******************************************************************
  Comparison of means for paired samples
  ****************************************************************** }

program student;

uses
  tpmath;

const
  N     = 12;    { Number of values }
  Alpha = 0.05;  { Significance level }

{ First sample }
const X : array[1..N] of Float =
(9.2, 10, 9, 9.4, 10.1, 9.5, 10, 10.3, 10.2, 10.2, 9.8, 10.1);

{ Second sample }
const Y : array[1..N] of Float =
(9.5, 9, 8.8, 9.5, 9.1, 10, 10.1, 9.3, 9, 9.7, 9.1, 9.3);

var
  XX, YY  : PVector;  { Data }
  MX, MY  : Float;    { Means }
  SX, SY  : Float;    { Standard deviations }
  T       : Float;    { Student's t }
  WT, Eps : Float;    { Wilcoxon's T and assoc. standard normal }
  Nd      : Integer;  { Number of nonzero differences }
  DoF     : Integer;  { Degrees of freedom }
  P       : Float;    { Probability }
  Tc, Ec  : Float;    { Critical values }

procedure GetData(XX, YY : PVector);
{ Get data into arrays }
var
  I : Integer;
begin
  for I := 1 to N do
    begin
      XX^[I] := X[I];
      YY^[I] := Y[I];
    end;
end;

begin
  DimVector(XX, N);
  DimVector(YY, N);

  GetData(XX, YY);

  { Compute statistical parameters }

  MX := Mean(XX, 1, N);
  MY := Mean(YY, 1, N);

  SX := StDev(XX, 1, N, MX);
  SY := StDev(YY, 1, N, MY);

  { Compare means (parametric test) }

  StudPaired(XX, YY, 1, N, T, DoF);

  { Compare means (non-parametric test) }

  Wilcoxon(XX, YY, 1, N, Nd, WT, Eps);

  { Compute critical values }

  P := 1.0 - 0.5 * Alpha;

  Tc := InvStudent(DoF, P);
  Ec := InvNorm(P);

  WriteLn('                X          Y');
  WriteLn;
  WriteLn('Mean     :', MX:10:4, MY:10:4);
  WriteLn('St. dev. :', SX:10:4, SY:10:4);

  WriteLn;

  WriteLn('Comparison of means (paired samples, parametric test)');
  WriteLn;
  WriteLn('Student''s t                = ', T:10:4, '  (', DoF, ' DoF)');
  WriteLn('Critical value (p = ', Alpha:5:3, ') = ', Tc:10:4);

  WriteLn;

  WriteLn('Comparison of means (paired samples, non-parametric test)');
  WriteLn;
  WriteLn('Wilcoxon''s T               = ', WT:10:4);
  WriteLn('Associated standard normal = ', Eps:10:4);
  WriteLn('Critical value (p = ', Alpha:5:3, ') = ', Ec:10:4);
end.