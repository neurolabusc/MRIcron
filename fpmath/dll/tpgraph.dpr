library tpgraph;

uses
{$IFDEF DELPHI}
  uwinplot;
{$ELSE}
  uplot;
{$ENDIF}

exports
  InitGraphics,            { Initializes the graphic }
  SetWindow,               { Sets the graphic window }
  AutoScale,               { Automatic scale determination }
  SetOxScale,              { Sets the scale on the Ox axis }
  SetOyScale,              { Sets the scale on the Oy axis }
  SetGraphTitle,           { Sets the graph title }
  SetOxTitle,              { Sets the title for the Ox axis }
  SetOyTitle,              { Sets the title for the Oy axis }
{$IFNDEF DELPHI}
  SetTitleFont,            { Sets the font for the main graph title }
  SetOxFont,               { Sets the font for the Ox axis }
  SetOyFont,               { Sets the font for the Oy axis }
  SetLgdFont,              { Sets the font for the legends }
  SetClipping,             { Limits the graphic to the current viewport }
{$ENDIF}
  PlotOxAxis,              { Plots the X axis }
  PlotOyAxis,              { Plots the Y axis }
  WriteGraphTitle,         { Writes title of graph }
  PlotGrid,                { Plots a grid on the graph }
  SetMaxCurv,              { Sets maximum number of curves }
  SetPointParam,           { Sets point parameters }
  SetLineParam,            { Sets line parameters }
  SetCurvLegend,           { Sets curve legend }
  SetCurvStep,             { Sets curve step }
  PlotPoint,               { Plots a point }
  PlotCurve,               { Plots a curve }
  PlotCurveWithErrorBars,  { Plots a curve with error bars }
  PlotFunc,                { Plots a function }
  WriteLegend,             { Writes the legends for the plotted curves }
  ConRec,                  { Contour plot }
  Xpixel,                  { Converts user abscissa X to screen coordinate }
  Ypixel,                  { Converts user ordinate Y to screen coordinate }
  Xuser,                   { Converts screen coordinate X to user abscissa }
  Yuser,                   { Converts screen coordinate Y to user ordinate }
  LeaveGraphics;           { Quits the graphic mode }

begin
end.

