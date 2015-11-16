{ ******************************************************************
  This program performs a least squares fit of a polynomial:
               Y = B(0) + B(1) * X + B(2) * Xý + ...
  ****************************************************************** }

program regpoly;

uses
  tpmath, tpgraph;

const
  N     = 12;    { Number of points }
  Deg   = 2;     { Degree of polynomial }
  Alpha = 0.05;  { Significance level }

{ Data }
const
  X : array[1..N] of Float =
      (0,      0.0136, 0.023,  0.0352, 0.048,  0.075,
       0.1067, 0.1374, 0.1734, 0.2139, 0.2594, 0.3113);

  Y : array[1..N] of Float =
      (3.97, 4.03, 4.1,  4.2,  4.28, 4.47,
       4.66, 4.83, 4.99, 5.12, 5.25, 5.37);

var
  B : PVector;  { Regression parameters }

function PltFunc(X : Float) : Float;
{ ------------------------------------------------------------------
  Function to be plotted
  ------------------------------------------------------------------ }

begin
  PltFunc := Poly(X, B, Deg);
end;

procedure WriteResults(X, Y, Ycalc, B : PVector;
                       V              : PMatrix;
                       Test           : TRegTest;
                       Tc, Fc         : Float);
{ ------------------------------------------------------------------
  Writes results to screen
  ------------------------------------------------------------------ }

var
  Line1,
  Line2 : String;   { Separating lines }
  Delta : Float;    { Residual }
  Sr    : Float;    { Residual standard deviation }
  SB    : Float;    { Standard deviations of parameters }
  I     : Integer;  { Loop variable }

begin
  Line1 := StrChar(73, '-');
  Line2 := StrChar(73, '=');

  WriteLn(Line2);
  WriteLn('Polynomial regression: Y = B(0) + B(1) * X + B(2) * Xý + ...');
  WriteLn(Line1);

  WriteLn('Parameter    Est.value         Std.dev.        ',
          (100 * (1 - Alpha)):2:0, '% Confidence Interval');

  WriteLn(Line1);

  for I := 0 to Deg do
    begin
      SB := Sqrt(V^[I]^[I]);
      WriteLn('B(', I:1, ')', B^[I]:17:8, SB:17:8,
              (B^[I] - Tc * SB):17:8, ';', (B^[I] + Tc * SB):17:8);
    end;

  WriteLn(Line1);

  WriteLn('Number of observations            : n           = ', N:5);

  with Test do
    begin
      Sr := Sqrt(Vr);
      WriteLn('Residual error                    : s           = ', Sr:10:4);
      WriteLn('Coefficient of correlation        : r           = ', (Sqrt(R2)):10:4);
      WriteLn('Coefficient of determination      : r2          = ', R2:10:4);
      WriteLn('Adjusted coeff. of determination  : r2a         = ', R2a:10:4);
      WriteLn('Variance ratio (explained/resid.) : F(', Nu1:3, ', ', Nu2:3, ') = ', F:10:4);
      WriteLn('Critical variance ratio           : F(p = ', (1 - Alpha):4:2, ') = ', Fc:10:4);
    end;

  WriteLn(Line1);
  WriteLn('  i        Y obs.       Y calc.      Residual      Std.dev.      Std.res.');
  WriteLn(Line1);

  for I := 1 to N do
    begin
      Delta := Y^[I] - Ycalc^[I];
      WriteLn(I:3, Y^[I]:14:4, Ycalc^[I]:14:4, Delta:14:4, Sr:14:4, (Delta / Sr):14:4);
    end;

  WriteLn(Line2);
end;

procedure PlotGraph(X, Y, B : PVector);
{ ------------------------------------------------------------------
  Plots points and fitted curve
  ------------------------------------------------------------------ }

var
  Xmin, Xmax, Xstep : Float;    { Ox scale }
  Ymin, Ymax, Ystep : Float;    { Oy scale }

begin
  if not InitGraphics(9, 2, 'c:\tp\bgi') then  { 640x480 16 color }
    begin
      Writeln('Unable to set graphic mode');
      Exit;
    end;

  SetWindow(15, 85, 15, 85, True);

  AutoScale(X, 1, N, LinScale, Xmin, Xmax, Xstep);
  AutoScale(Y, 1, N, LinScale, Ymin, Ymax, Ystep);

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetGraphTitle('Polynomial Regression');
  SetOxTitle('X');
  SetOyTitle('Y');

  PlotOxAxis;
  PlotOyAxis;

  WriteGraphTitle;

  SetClipping(True);

  SetLineParam(1, 0, 0, 0);  { Don't connect points }
  PlotCurve(X, Y, 1, N, 1);

  PlotFunc({$IFDEF FPC}@{$ENDIF}PltFunc, Xmin, Xmax, 2);

  Readln;

  LeaveGraphics;
end;

{ ******************************************************************
  Main program
  ****************************************************************** }

var
  XX, YY : PVector;   { Data }
  Ycalc  : PVector;   { Computed Y values }
  V      : PMatrix;   { Variance-covariance matrix }
  Test   : TRegTest;  { Statistical tests }
  Tc     : Float;     { Critical t value }
  Fc     : Float;     { Critical F value }
  I      : Integer;   { Loop variable }

begin
  { Dimension arrays }
  DimVector(XX, N);
  DimVector(YY, N);
  DimVector(Ycalc, N);
  DimVector(B, Deg);
  DimMatrix(V, Deg, Deg);

  { Read data }
  for I := 1 to N do
    begin
      XX^[I] := X[I];
      YY^[I] := Y[I];
    end;

  { Perform regression }
  PolFit(XX, YY, 1, N, Deg, B, V);

  { Compute predicted Y values }
  for I := 1 to N do
    Ycalc^[I] := Poly(XX^[I], B, Deg);

  { Update variance-covariance matrix and compute statistical tests }
  RegTest(YY, Ycalc, 1, N, V, 0, Deg, Test);

  { Compute Student's t and Snedecor's F }
  Tc := InvStudent(Test.Nu2, 1 - 0.5 * Alpha);
  Fc := InvSnedecor(Test.Nu1, Test.Nu2, 1 - Alpha);

  { Write results }
  WriteResults(XX, YY, Ycalc, B, V, Test, Tc, Fc);

  { Plot curve }
  PlotGraph(XX, YY, B);
end.
