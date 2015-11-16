{ ******************************************************************
  This program performs a weighted least squares fit
  of a straight line:
                         Y = B(0) + B(1) * X
  ****************************************************************** }

program wreglin;

uses
  tpmath, tpgraph;

const
  N     = 6;     { Number of points }
  Alpha = 0.05;  { Significance level }

{ Data (S = standard deviations of observed Y values) }
const
  X : array[1..N] of Float = (3.195, 3.247, 3.3,  3.356, 3.413, 3.472);
  Y : array[1..N] of Float = (1.8,   1.61,  1.38, 0.98,  0.81,  0.56);
  S : array[1..N] of Float = (0.03,  0.03,  0.02, 0.03,  0.01,  0.06);

var
  B : PVector;  { Regression parameters }

function PltFunc(X : Float) : Float;
{ ------------------------------------------------------------------
  Function to be plotted
  ------------------------------------------------------------------ }

begin
  PltFunc := B^[0] + B^[1] * X
end;

procedure WriteResults(X, Y, S, Ycalc, B : PVector;
                       V                 : PMatrix;
                       Test              : TRegTest;
                       Tc, Fc            : Float);
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
  WriteLn('Linear regression: Y = B(0) + B(1) * X');
  WriteLn(Line1);

  WriteLn('Parameter    Est.value         Std.dev.        ',
          (100 * (1 - Alpha)):2:0, '% Confidence Interval');

  WriteLn(Line1);

  for I := 0 to 1 do
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
      WriteLn('Coefficient of correlation        : r           = ', (Sgn(B^[1]) * Sqrt(R2)):10:4);
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
      WriteLn(I:3, Y^[I]:14:4, Ycalc^[I]:14:4, Delta:14:4,
                   S^[I]:14:4, (Delta / S^[I]):14:4);
    end;

  WriteLn(Line2);
end;

procedure PlotGraph(X, Y, S, B : PVector);
{ ------------------------------------------------------------------
  Plots histogram and normal curve
  ------------------------------------------------------------------ }

var
  Xmin, Xmax, Xstep : Float;  { Ox scale }
  Ymin, Ymax, Ystep : Float;  { Oy scale }

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

  SetGraphTitle('Weighted Linear Regression');
  SetOxTitle('X');
  SetOyTitle('Y');

  PlotOxAxis;
  PlotOyAxis;

  WriteGraphTitle;

  SetClipping(True);

  SetPointParam(1, 1, 3, 12);
  SetLineParam(1, 0, 0, 12);   { Don't connect points }

  PlotCurveWithErrorBars(X, Y, S, 1, 1, N, 1);

  PlotFunc({$IFDEF FPC}@{$ENDIF}PltFunc, Xmin, Xmax, 1);

  Readln;

  LeaveGraphics;
end;

{ ******************************************************************
  Main program
  ****************************************************************** }

var
  XX, YY : PVector;   { Data }
  SS     : PVector;   { Standard deviations of observed Y values }
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
  DimVector(SS, N);
  DimVector(Ycalc, N);
  DimVector(B, 1);
  DimMatrix(V, 1, 1);

  { Read data }
  for I := 1 to N do
    begin
      XX^[I] := X[I];
      YY^[I] := Y[I];
      SS^[I] := S[I];
    end;

  { Perform regression }
  WLinFit(XX, YY, SS, 1, N, B, V);

  { Compute predicted Y values }
  for I := 1 to N do
    Ycalc^[I] := B^[0] + B^[1] * XX^[I];

  { Update variance-covariance matrix and compute statistical tests }
  WRegTest(YY, Ycalc, SS, 1, N, V, 0, 1, Test);

  { Compute Student's t and Snedecor's F }
  Tc := InvStudent(N - 2, 1 - 0.5 * Alpha);
  Fc := InvSnedecor(1, N - 2, 1 - Alpha);

  { Write results }
  WriteResults(XX, YY, SS, Ycalc, B, V, Test, Tc, Fc);

  Readln;

  { Plot curve }
  PlotGraph(XX, YY, SS, B);
end.
