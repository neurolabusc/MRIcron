{ ******************************************************************
  Contour plot of a two-dimensional function
  ****************************************************************** }

program contour;

uses
  tpmath, tpgraph;

{ ******************************************************************
  Define here the function to be plotted
  ****************************************************************** }

const
  FuncName = 'Sin(Sqrt(X^2 + Y^2)) + 0.5 / Sqrt((X + 3.05)^2 + Y^2)';

const
  Xmin = - TwoPi; Xmax = TwoPi; Xstep = Pi;  { Ox scale }
  Ymin = - TwoPi; Ymax = TwoPi; Ystep = Pi;  { Oy scale }

const
  NpX = 60; NpY = 60;  { Number of grid points }

const
  Nc = 30;  { Number of contours }

function Func(X, Y : Float) : Float;

const
  C = 3.05;

var
  X2, Y2, Xc, Xc2 : Float;

begin
  X2 := X * X;
  Y2 := Y * Y;
  Xc := X + C;
  Xc2 := Xc * Xc;

  Func := Sin(Sqrt(X2 + Y2)) + 0.5 / Sqrt(Xc2 + Y2)
end;

{ ******************************************************************
  Main program
  ****************************************************************** }

var
  Dx, Dy     : Float;       { Increments of X and Y }
  Fmin, Fmax : Float;       { Min. and max. function values }
  H          : Float;       { Increment for levels }
  X, Y       : PVector;     { Point coordinates }
  Z          : PVector;     { Contour array }
  F          : PMatrix;     { Function values }
  I, J       : Integer;     { Loop variables }

begin
  { Initialize graphics }

  if not InitGraphics(9, 2,    { 640x480 16 color }
                     'c:\tp\bgi') then Exit;

  { Set graphic area so that it will look approximately square }

  SetWindow(24, 76, 15, 85, True);

  { Dimension arrays }

  DimVector(X, NpX);
  DimVector(Y, NpY);

  DimVector(Z, Nc - 1);

  DimMatrix(F, NpX, NpY);

  { Set scales and plot axes }

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  PlotOxAxis;
  PlotOyAxis;

  PlotGrid(BothGrid);

  SetGraphTitle(FuncName);
  WriteGraphTitle;

  { Generate grid points }

  Dx := (Xmax - Xmin) / NpX;
  Dy := (Ymax - Ymin) / NpY;

  X^[0] := Xmin;
  for I := 1 to NpX do
    X^[I] := X^[I - 1] + Dx;

  Y^[0] := Ymin;
  for J := 1 to NpY do
    Y^[J] := Y^[J - 1] + Dy;

  { Compute function values }

  Fmin := Func(Xmin, Ymin);
  Fmax := Fmin;

  for I := 0 to NpX do
    for J := 0 to NpY do
      begin
        F^[I]^[J] := Func(X^[I], Y^[J]);
        if F^[I]^[J] < Fmin then
          Fmin := F^[I]^[J]
        else if F^[I]^[J] > Fmax then
          Fmax := F^[I]^[J];
      end;

  { Define levels }

  H := (Fmax - Fmin) / (Nc + 1);

  for I := 0 to Nc - 1 do
    Z^[I] := Fmin + (I + 1) * H;

  { Plot contour }

  ConRec(NpX, NpY, Nc, X, Y, Z, F);

  ReadLn;

  LeaveGraphics;
end.
