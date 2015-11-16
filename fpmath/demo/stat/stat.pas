{ ******************************************************************
  Statistics: Concentration of hemoglobin in blood (mg/L) in men
  and women.
  ****************************************************************** }

program stat;

uses
  tpmath;

const
  N     = 30;    { Number of values }
  Alpha = 0.05;  { Significance level }

{ Concentrations in men }
const HbM : array[1..N] of Float =
(141, 144, 146, 148, 149, 150, 150, 151, 153, 153,
 153, 154, 155, 156, 156, 160, 160, 160, 163, 164,
 164, 165, 166, 168, 168, 170, 172, 172, 176, 179);

{ Concentrations in women }
const HbW : array[1..N] of Float =
(105, 110, 112, 112, 118, 119, 120, 120, 125, 126,
 127, 128, 130, 132, 133, 134, 135, 138, 138, 138,
 138, 142, 145, 148, 148, 150, 151, 154, 154, 158);

var
  XX, YY           : PVector;  { Data }
  MM, MW, SM, SW   : Float;    { Means and standard deviations }
  SkM, SkW, KM, KW : Float;    { Skewness and kurtosis }
  T, F             : Float;    { Student's t and Snedecor's F }
  U, Eps           : Float;    { Mann-Whitney's U and assoc. standard normal }
  DoF, DoF1, DoF2  : Integer;  { Degrees of freedom }
  Tc, Fc, Ec       : Float;    { Critical values }
  P                : Float;    { Probability }

procedure GetData(XX, YY : PVector);
{ Get data into arrays }
var
  I : Integer;
begin
  for I := 1 to N do
    begin
      XX^[I] := HbM[I];
      YY^[I] := HbW[I];
    end;
end;

begin
  DimVector(XX, N);
  DimVector(YY, N);

  GetData(XX, YY);

  { Compute statistical parameters }

  MM := Mean(XX, 1, N);
  MW := Mean(YY, 1, N);

  SM := StDev(XX, 1, N, MM);
  SW := StDev(YY, 1, N, MW);

  SkM := Skewness(XX, 1, N, MM, SM);
  SkW := Skewness(YY, 1, N, MW, SW);

  KM := Kurtosis(XX, 1, N, MM, SM);
  KW := Kurtosis(YY, 1, N, MW, SW);

  { Compare means and variances (parametric tests) }

  StudIndep(N, N, MM, MW, SM, SW, T, DoF);
  Snedecor(N, N, SM, SW, F, DoF1, DoF2);

  { Compare means (non-parametric test) }

  Mann_Whitney(N, N, XX, YY, U, Eps);

  { Compute critical values }

  P := 1.0 - 0.5 * Alpha;

  Tc := InvStudent(DoF, P);
  Fc := InvSnedecor(DoF1, DoF2, P);
  Ec := InvNorm(P);

  WriteLn('Hemoglobin in blood');
  WriteLn;
  WriteLn('                 Men     Women');
  WriteLn;
  WriteLn('Mean     : ', MM:10:4, MW:10:4);
  WriteLn('St. dev. : ', SM:10:4, SW:10:4);
  WriteLn('Skewness : ', SkM:10:4, SkW:10:4);
  WriteLn('Kurtosis : ', KM:10:4, KW:10:4);

  WriteLn;

  WriteLn('Comparison of means (parametric test):');
  WriteLn;
  WriteLn('Student''s t                = ', T:10:4, '  (', DoF, ' DoF)');
  WriteLn('Critical value (p = ', Alpha:5:3, ') = ', Tc:10:4);

  WriteLn;

  WriteLn('Comparison of variances:');
  WriteLn;
  WriteLn('Snedecor''s F               = ', F:10:4, '  (', DoF1, ' and ', DoF2, ' DoF)');
  WriteLn('Critical value (p = ', Alpha:5:3, ') = ', Fc:10:4);

  WriteLn;

  WriteLn('Comparison of means (non-parametric test):');
  WriteLn;
  WriteLn('Mann-Whitney U             = ', U:10:4);
  WriteLn('Associated standard normal = ', Eps:10:4);
  WriteLn('Critical value (p = ', Alpha:5:3, ') = ', Ec:10:4);

  WriteLn;
end.
