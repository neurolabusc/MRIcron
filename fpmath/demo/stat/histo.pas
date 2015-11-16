{ ******************************************************************
  Statistical distribution and Histogram
  ****************************************************************** }

program histo;

uses
  tpmath, tpgraph;

const
  N     = 30;    { Number of values }
  Alpha = 0.05;  { Significance level }

{ Hemoglobin concentrations in men }
const HbM : array[1..N] of Float =
(141, 144, 146, 148, 149, 150, 150, 151, 153, 153,
 153, 154, 155, 156, 156, 160, 160, 160, 163, 164,
 164, 165, 166, 168, 168, 170, 172, 172, 176, 179);

var
  M, S : Float;  { Mean and standard deviation }

function PltFunc(X : Float) : Float;
{ ------------------------------------------------------------------
  Function to be plotted (density of normal distribution)
  ------------------------------------------------------------------ }

begin
  PltFunc := DNorm((X - M) / S) / S;
end;

procedure WriteResults(C       : PStatClassVector;
                       Ncls    : Integer;
                       Calc    : PVector;
                       Khi2, G : Float;
                       DoF     : Integer;
                       K2c     : Float);
{ ------------------------------------------------------------------
  Writes results to screen
  ------------------------------------------------------------------ }

const
  Line1 = '-----------------------------';

var
  Sum : Float;
  I   : Integer;

begin
  Writeln('Statistical distribution');

  Writeln(Line1);
  Writeln('     Inf    Sup      N  Ncalc');
  Writeln(Line1);

  Sum := 0.0;
  for I := 1 to Ncls do
    begin
      Writeln(C^[I].Inf:8:0, C^[I].Sup:7:0, C^[I].N:7, Calc^[I]:7:2);
      Sum := Sum + Calc^[I];
    end;

  Writeln(Line1);
  Writeln('Total            ', N:5, Sum:7:2);
  Writeln(Line1);

  Writeln;
  Writeln('Comparison with normal distribution:');
  Writeln;

  Writeln('Pearson''s Khi-2           = ', Khi2:10:4, '  (', DoF, ' DoF)');
  Writeln('Woolf''s G                 = ', G:10:4, '  (', DoF, ' DoF)');
  Writeln('Critical value (p = ', Alpha:4:2, ') = ', K2c:10:4);
end;

procedure PlotGraph(C                  : PStatClassVector;
                    Ncls               : Integer;
                    Xmin , Xmax, Xstep : Float);
{ ------------------------------------------------------------------
  Plots histogram and normal curve
  ------------------------------------------------------------------ }

var
  Ymin, Ymax, Ystep : Float;    { Oy scale }
  Npts              : Integer;  { Number of points }
  X, Y              : PVector;  { Point coordinates }
  I, J              : Integer;  { Loop variables }

begin
  if not InitGraphics(9, 2, 'c:\tp\bgi') then  { 640x480 16 color }
    begin
      Writeln('Unable to set graphic mode');
      Exit;
    end;

  SetWindow(15, 85, 15, 85, True);

  { The histogram is plotted as a continuous curve.
    Each bar of the histogram is defined by 4 points }

  Npts := 4 * Ncls;

  DimVector(X, Npts);
  DimVector(Y, Npts);

  for I := 1 to Ncls do
    with C^[I] do
      begin
        J := 4 * (I - 1) + 1; X^[J] := Inf;  { Y^[J] := 0 }
        Inc(J); X^[J] := Inf; Y^[J] := D;
        Inc(J); X^[J] := Sup; Y^[J] := D;
        Inc(J); X^[J] := Sup;                { Y^[J] := 0 }
      end;

  { Set scale on Oy, making sure that it starts from 0 }

  AutoScale(Y, 1, Ncls, LinScale, Ymin, Ymax, Ystep);

  if Ymin <> 0.0 then Ymin := 0.0;
  Ymax := Ymax + Ystep;

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetGraphTitle('Statistical Distribution');
  SetOxTitle('X');
  SetOyTitle('Frequency Density');

  PlotOxAxis;
  PlotOyAxis;

  WriteGraphTitle;

  SetClipping(True);

  { Plot histogram and normal curve }

  SetPointParam(1, 0, 0, 0);  { Don't show points on histogram }
  SetLineParam(1, 1, 3, 1);   { Use thick lines }

  PlotCurve(X, Y, 1, Npts, 1);

  PlotFunc({$IFDEF FPC}@{$ENDIF}PltFunc, Xmin, Xmax, 2);

  DelVector(X, Npts);
  DelVector(Y, Npts);

  Readln;

  LeaveGraphics;
end;

{ ******************************************************************
  Main program
  ****************************************************************** }

var
  X     : PVector;           { Data }
  C     : PStatClassVector;  { Statistical classes }
  Ncls  : Integer;           { Number of classes }
  Obs   : PIntVector;        { Observed frequencies }
  Calc  : PVector;           { Calculated frequencies }
  Khi2  : Float;             { Pearson's Khi-2 }
  G     : Float;             { Woolf's G }
  K2c   : Float;             { Theoretical Khi-2 }
  DoF   : Integer;           { Degrees of freedom }
  T     : Float;             { Standard normal variable }
  F0, F : Float;             { Cumulative probability }
  XMin,
  XMax,
  XStep : Float;             { Scale on Ox }
  I     : Integer;           { Loop variable }

begin
  { Read data }
  DimVector(X, N);
  for I := 1 to N do
    X^[I] := HbM[I];

  { Sort data if necessary }
  { QSort(X, 1, N);        }

  { Compute an appropriate interval for the set of values }
  Interval(X^[1], X^[N], 5, 10, XMin, XMax, XStep);

  { Compute number of classes and dimension arrays }
  Ncls := Round((Xmax - Xmin) / XStep);

  DimStatClassVector(C, Ncls);
  DimIntVector(Obs, Ncls);
  DimVector(Calc, Ncls);

  { Compute distribution }
  Distrib(X, 1, N, Xmin, Xmax, XStep, C);

  { Compute mean and S.D. }
  M := Mean(X, 1, N);
  S := StDev(X, 1, N, M);

  { Compute theoretical values }
  F0 := 0.0;
  for I := 1 to Ncls do
    begin
      if I = Ncls then
        F := 1.0
      else
        begin
          T := (C^[I].Sup - M) / S;
          F := FNorm(T);
        end;
      Calc^[I] := N * (F - F0);
      Obs^[I] := C^[I].N;
      F0 := F;
    end;

  { Perform Khi-2 and Woolf tests }
  Khi2_Conform(Ncls, 2, Obs, Calc, Khi2, DoF);
  Woolf_Conform(Ncls, 2, Obs, Calc, G, DoF);

  { Compute critical value }
  K2c := InvKhi2(DoF, 1.0 - Alpha);

  { Print results }
  WriteResults(C, Ncls, Calc, Khi2, G, DoF, K2c);

  Readln;

  { Plot histogram }
  PlotGraph(C, Ncls, Xmin - Xstep, Xmax + Xstep, Xstep);
end.
