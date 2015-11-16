{ ******************************************************************
  This program performs a multiple linear least squares fit:
  Y = B(0) + B(1) * X1 + B(2) * X2 + ...
  ****************************************************************** }

program regmult;

uses
  tpmath, tpgraph;

const
  N        = 23;    { Number of observations }
  Nvar     = 4;     { Number of independent variables }
  Alpha    = 0.05;  { Significance level }
  ConsTerm = True;  { Include a constant term B(0) }

{ Data }
const X : array[1..N, 1..Nvar] of Float =
((-0.27, 7.327, 0, 0),
 (-0.55, 7.4  , 0, 0),
 (-0.53, 7.74 , 0, 0),
 (-0.57, 7.95 , 0, 0),
 (-0.87, 7.9  , 0, 0),
 (-1.36, 7.931, 0, 0),
 (-0.39, 6.849, 0, 0),
 (-0.66, 7.508, 0, 0),
 (-0.33, 7.419, 0, 0),
 (-1.7 , 7.496, 0, 0),
 (-0.68, 7.027, 0, 0),
 (-0.79, 8.15 , 0, 0),
 (-0.82, 8.822, 0, 0),
 (-0.66, 8.334, 0, 0),
 ( 0.02, 7.421, 0, 0),
 ( 0.06, 7.862, 1, 0),
 (-0.3 , 8.483, 1, 0),
 ( 0.07, 9.82 , 0, 0),
 ( 0   , 7.641, 1, 0),
 (-0.8 , 7.601, 0, 1),
 (-1.05, 7.565, 0, 1),
 (-0.35, 7.993, 0, 0),
 (-0.11, 7.13 , 0, 0));

const Y : array[1..N] of Float =
(3.21,
 3.94,
 3.66,
 3.99,
 4.06,
 4.09,
 3.36,
 3.92,
 3.58,
 4.26,
 3.06,
 4.13,
 4.27,
 4.36,
 3.72,
 3.89,
 4.39,
 3.92,
 3.89,
 5.1 ,
 5.14,
 3.68,
 3.7);

{ ******************************************************************
  Subprograms
  ****************************************************************** }

procedure WriteResults(Y, Ycalc : PVector;
                       B        : PVector;
                       V        : PMatrix;
                       Test     : TRegTest;
                       Tc, Fc   : Float);
{ ------------------------------------------------------------------
  Writes results to screen
  ------------------------------------------------------------------ }

var
  Line1,
  Line2 : String;   { Separating lines }
  Delta : Float;    { Residual }
  Sr    : Float;    { Residual standard deviation }
  SB    : Float;    { Standard deviations of parameters }
  Lb    : Integer;  { Index of first parameter }
  I     : Integer;  { Loop variable }

begin
  Line1 := StrChar(73, '-');
  Line2 := StrChar(73, '=');

  WriteLn(Line2);
  Write('Multiple linear regression: Y = ');
  if ConsTerm then Write('B(0) + ');
  WriteLn('B(1) * X1 + B(2) * X2 + ...');
  WriteLn(Line1);

  WriteLn('Parameter    Est.value         Std.dev.        ',
          (100 * (1 - Alpha)):2:0, '% Confidence Interval');

  WriteLn(Line1);

  if ConsTerm then Lb := 0 else Lb := 1;

  for I := Lb to Nvar do
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
      WriteLn(I:3, Y^[I]:14:4, Ycalc^[I]:14:4, Delta:14:4, Sr:14:4, (Delta / Sr):14:4);
    end;

  WriteLn(Line2);
end;

procedure PlotGraph(Y, Ycalc : PVector);
{ ------------------------------------------------------------------
  Plots observed vs calculated Y values
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

  AutoScale(Y, 1, N, LinScale, Xmin, Xmax, Xstep);
  AutoScale(Ycalc, 1, N, LinScale, Ymin, Ymax, Ystep);

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetGraphTitle('Multiple Linear Regression');
  SetOxTitle('Y obs.');
  SetOyTitle('Y calc.');

  PlotOxAxis;
  PlotOyAxis;

  WriteGraphTitle;

  SetClipping(True);

  SetLineParam(1, 0, 0, 0);      { Don't connect points }
  PlotCurve(Y, Ycalc, 1, N, 1);

  Readln;

  LeaveGraphics;
end;

{ ******************************************************************
  Main program
  ****************************************************************** }

var
  XX    : PMatrix;   { Independent variables }
  YY    : PVector;   { Dependent variable }
  Ycalc : PVector;   { Computed Y values }
  B     : PVector;   { Fitted parameters }
  V     : PMatrix;   { Variance-covariance matrix }
  Test  : TRegTest;  { Statistical tests }
  Tc    : Float;     { Critical t value }
  Fc    : Float;     { Critical F value }
  Lb    : Integer;   { Index of first parameter }
  I, J  : Integer;   { Loop variable }

begin
  { Dimension arrays }
  DimMatrix(XX, N, Nvar);
  DimVector(YY, N);
  DimVector(Ycalc, N);
  DimVector(B, Nvar);
  DimMatrix(V, Nvar, Nvar);

  { Read data }
  for I := 1 to N do
    begin
      for J := 1 to Nvar do
        XX^[I]^[J] := X[I,J];
      YY^[I] := Y[I];
    end;

  { Perform regression }
  { MulFit(XX, YY, 1, N, Nvar, ConsTerm, B, V); }
  SVDFit(XX, YY, 1, N, Nvar, ConsTerm, 1.0E-8, B, V);

  { Compute predicted Y values }
  for I := 1 to N do
    begin
      if ConsTerm then Ycalc^[I] := B^[0] else Ycalc^[I] := 0.0;
      for J := 1 to Nvar do
        Ycalc^[I] := Ycalc^[I] + B^[J] * XX^[I]^[J];
    end;

  { Update variance-covariance matrix and compute statistical tests }
  if ConsTerm then Lb := 0 else Lb := 1;
  RegTest(YY, Ycalc, 1, N, V, Lb, Nvar, Test);

  { Compute Student's t and Snedecor's F }
  Tc := InvStudent(Test.Nu2, 1 - 0.5 * Alpha);
  Fc := InvSnedecor(Test.Nu1, Test.Nu2, 1 - Alpha);

  { Write results }
  WriteResults(YY, Ycalc, B, V, Test, Tc, Fc);

  Readln;

  { Plot curve }
  PlotGraph(YY, Ycalc);
end.
