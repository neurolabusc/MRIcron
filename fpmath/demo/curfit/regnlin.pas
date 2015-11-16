{ ******************************************************************
  Nonlinear regression
  ****************************************************************** }

program regnlin;

uses
  tpmath, tpgraph;

const
  FuncName = 'Y = B(1) * Exp(- B(2) * X)';

  N        = 10;      { Number of points }
  FirstPar = 1;       { Index of first fitted parameter }
  LastPar  = 2;       { Index of last fitted parameter }
  MaxIter  = 1000;    { Max. number of iterations }
  Tol      = 1.0E-3;  { Required precision }
  Alpha    = 0.05;    { Significance level }

{ Data }
const
  X : array[1..N] of Float = (  1,   2,   3,   4,   5,   6,  7,  8,  9, 10);
  Y : array[1..N] of Float = (416, 319, 244, 188, 144, 113, 85, 66, 50, 41);

var
  B : PVector;  { Regression parameters }

procedure ApproxFit(B : PVector);
{ ------------------------------------------------------------------
  Approximate fit of the exponential model by weighted linear
  regression: Ln(Y) = Ln(B(1)) - B(2) * X
  ------------------------------------------------------------------ }

var
  P      : Integer;  { Nb. of points for linear reg. }
  K      : Integer;  { Loop variable }
  X1, Y1 : PVector;  { Transformed coordinates }
  S1     : PVector;  { Standard deviations }
  A      : PVector;  { Linear regression param. Y = A^[0] + A^[1] * X }
  V      : PMatrix;  { Variance-covariance matrix }

begin
  P := 0;                       { Count the number of points }
  for K := 1 to N do            { which can be transformed }
    if Y[K] > 0.0 then Inc(P);

  DimVector(X1, P);
  DimVector(Y1, P);
  DimVector(S1, P);

  DimVector(A, 1);
  DimMatrix(V, 1, 1);

  P := 0;
  for K := 1 to N do
    if Y[K] > 0.0 then
      begin
        Inc(P);
        X1^[P] := X[K];
        Y1^[P] := Ln(Y[K]);
        S1^[P] := 1.0 / Y[K];
      end;

  WLinFit(X1, Y1, S1, 1, P, A, V);

  if MathErr = MatOk then
    begin
      B^[1] := Exp(A^[0]);
      B^[2] := - A^[1];
    end;

  DelVector(A, 1);
  DelMatrix(V, 1, 1);
end;

function RegFunc(X : Float; B : PVector) : Float;
begin
  RegFunc := B^[1] * Exp(- B^[2] * X);
end;

procedure DerivProc(X, Y : Float; B, D : PVector);
begin
  D^[1] := Exp(- B^[2] * X);
  D^[2] := - B^[1] * X * D^[1];
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
  WriteLn('Nonlinear regression: ', FuncName);
  WriteLn(Line1);

  WriteLn('Parameter    Est.value         Std.dev.        ',
          (100 * (1 - Alpha)):2:0, '% Confidence Interval');

  WriteLn(Line1);

  for I := FirstPar to LastPar do
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

      if R2 <= 1.0 then
        begin
          WriteLn('Coefficient of correlation        : r           = ', (Sqrt(R2)):10:4);
          WriteLn('Coefficient of determination      : r2          = ', R2:10:4);
          WriteLn('Adjusted coeff. of determination  : r2a         = ', R2a:10:4);
        end;

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

function PltFunc(X : Float) : Float;
{ ------------------------------------------------------------------
  Function to be plotted
  ------------------------------------------------------------------ }
begin
  PltFunc := RegFunc(X, B);
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

var
  XX, YY : PVector;   { Data }
  Ycalc  : PVector;   { Computed Y values }
  V      : PMatrix;   { Variance-covariance matrix }
  Test   : TRegTest;  { Statistical tests }
  Tc     : Float;     { Critical t value }
  Fc     : Float;     { Critical F value }
  I      : Integer;   { Loop variable }

begin
  DimVector(XX, N);
  DimVector(YY, N);
  DimVector(Ycalc, N);
  DimVector(B, LastPar);
  DimMatrix(V, LastPar, LastPar);

  { Read data }
  for I := 1 to N do
    begin
      XX^[I] := X[I];
      YY^[I] := Y[I];
    end;

  ApproxFit(B);

  NLFit({$IFDEF FPC}@{$ENDIF}RegFunc, {$IFDEF FPC}@{$ENDIF}DerivProc,
        XX, YY, 1, N, MaxIter, Tol, B, FirstPar, LastPar, V);

  if MathErr = MatOk then
    begin
      { Compute predicted Y values }
      for I := 1 to N do
        Ycalc^[I] := RegFunc(XX^[I], B);

      { Update variance-covariance matrix and compute statistical tests }
      RegTest(YY, Ycalc, 1, N, V, FirstPar, LastPar, Test);

      { Compute Student's t and Snedecor's F }
      Tc := InvStudent(Test.Nu2, 1 - 0.5 * Alpha);
      Fc := InvSnedecor(Test.Nu1, Test.Nu2, 1 - Alpha);

      { Write results }
      WriteResults(XX, YY, Ycalc, B, V, Test, Tc, Fc);

      { Plot curve }
      PlotGraph(XX, YY, B);
    end
  else
    Writeln('Unable to fit curve!');

  DelVector(XX, N);
  DelVector(YY, N);
  DelVector(Ycalc, N);
  DelVector(B, LastPar);
  DelMatrix(V, LastPar, LastPar);
end.
