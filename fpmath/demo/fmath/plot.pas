{ ******************************************************************
  This program plots a function in either linear or logarithmic
  coordinates.
  ****************************************************************** }

program plot;

uses
  tpmath, tpgraph;

function Func(X : Float) : Float;
{ Square root function (becomes linear in log-log coordinates) }
begin
  Func := Sqrt(X)
end;

begin
{ Plot in linear coordinates, using default parameter values }

  if not InitGraphics(9, 2,    { 640x480 16 color }
                     'c:\tp\bgi') then Exit;

  SetWindow(15, 85, 15, 85, True);

  PlotOxAxis;
  PlotOyAxis;

  PlotGrid(BothGrid);

  SetGraphTitle('SQUARE ROOT FUNCTION IN LINEAR COORDINATES');

  WriteGraphTitle;

  SetClipping(True);

  PlotFunc({$IFDEF FPC}@{$ENDIF}Func, 0.0, 1.0, 1);

  ReadLn;

  LeaveGraphics;

{ Plot in logarithmic coordinates }

  if not InitGraphics(9, 2,    { 640x480 16 color }
                     'c:\tp\bgi') then Exit;

  SetWindow(15, 85, 15, 85, True);

  SetOxScale(LogScale, 0.01, 100, 1);
  SetOyScale(LogScale, 0.1, 10, 1);

  PlotOxAxis;
  PlotOyAxis;

  PlotGrid(BothGrid);

  SetGraphTitle('SQUARE ROOT FUNCTION IN LOGARITHMIC COORDINATES');

  WriteGraphTitle;

  SetClipping(True);

  PlotFunc({$IFDEF FPC}@{$ENDIF}Func, 0.01, 100, 2);

  ReadLn;

  LeaveGraphics;
end.
