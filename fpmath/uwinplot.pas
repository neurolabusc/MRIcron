{ ******************************************************************
  Plotting routines for Delphi
  ****************************************************************** }

unit uwinplot;

interface

uses
  Classes, Graphics,
  utypes, umath, uround, uinterv, ustrings;

function InitGraphics(Canvas : TCanvas; Width, Height : Integer) : Boolean;

{ ------------------------------------------------------------------
  Enters graphic mode.
  ------------------------------------------------------------------
  The parameters Width and Height refer to the object on which the
  graphic is plotted.

  Examples:

  To draw on a TImage object:
  InitGraph(Image1.Canvas, Image1.Width, Image1.Height)

  To print the graphic:
  InitGraph(Printer.Canvas, Printer.PageWidth, Printer.PageHeight)
  ------------------------------------------------------------------ }

procedure SetWindow(Canvas         : TCanvas;
                    X1, X2, Y1, Y2 : Integer;
                    GraphBorder    : Boolean);
{ ------------------------------------------------------------------
  Sets the graphic window

  X1, X2, Y1, Y2 : Window coordinates in % of maximum
  GraphBorder    : Flag for drawing the window border
  ------------------------------------------------------------------ }

procedure AutoScale(X                     : PVector;
                    Lb, Ub                : Integer;
                    Scale                 : TScale;
                    var XMin, XMax, XStep : Float);
{ ------------------------------------------------------------------
  Finds an appropriate scale for plotting the data in X[Lb..Ub]
  ------------------------------------------------------------------ }

procedure SetOxScale(Scale                : TScale;
                     OxMin, OxMax, OxStep : Float);
{ ------------------------------------------------------------------
  Sets the scale on the Ox axis
  ------------------------------------------------------------------ }

procedure SetOyScale(Scale                : TScale;
                     OyMin, OyMax, OyStep : Float);
{ ------------------------------------------------------------------
  Sets the scale on the Oy axis
  ------------------------------------------------------------------ }

procedure SetGraphTitle(Title : String);
{ ------------------------------------------------------------------
  Sets the title for the graph
  ------------------------------------------------------------------ }

procedure SetOxTitle(Title : String);
{ ------------------------------------------------------------------
  Sets the title for the Ox axis
  ------------------------------------------------------------------ }

procedure SetOyTitle(Title : String);
{ ------------------------------------------------------------------
  Sets the title for the Oy axis
  ------------------------------------------------------------------ }

procedure PlotOxAxis(Canvas : TCanvas);
{ ------------------------------------------------------------------
  Plots the horizontal axis
  ------------------------------------------------------------------ }

procedure PlotOyAxis(Canvas : TCanvas);
{ ------------------------------------------------------------------
  Plots the vertical axis
  ------------------------------------------------------------------ }

procedure PlotGrid(Canvas : TCanvas; Grid : TGrid);
{ ------------------------------------------------------------------
  Plots a grid on the graph
  ------------------------------------------------------------------ }

procedure WriteGraphTitle(Canvas : TCanvas);
{ ------------------------------------------------------------------
  Writes the title of the graph
  ------------------------------------------------------------------ }

procedure SetMaxCurv(NCurv : Byte);
{ ------------------------------------------------------------------
  Sets the maximum number of curves and re-initializes their
  parameters
  ------------------------------------------------------------------ }

procedure SetPointParam(CurvIndex, Symbol, Size : Integer;
                        Color                   : TColor);
{ ------------------------------------------------------------------
  Sets the point parameters for curve # CurvIndex
  ------------------------------------------------------------------ }

procedure SetLineParam(CurvIndex : Integer;
                       Style     : TPenStyle;
                       Width     : Integer;
                       Color     : TColor);
{ ------------------------------------------------------------------
  Sets the line parameters for curve # CurvIndex
  ------------------------------------------------------------------ }

procedure SetCurvLegend(CurvIndex : Integer; Legend : String);
{ ------------------------------------------------------------------
  Sets the legend for curve # CurvIndex
  ------------------------------------------------------------------ }

procedure SetCurvStep(CurvIndex, Step : Integer);
{ ------------------------------------------------------------------
  Sets the step for curve # CurvIndex
  ------------------------------------------------------------------ }

procedure PlotPoint(Canvas    : TCanvas;
                    X, Y      : Float;
                    CurvIndex : Integer);
{ ------------------------------------------------------------------
  Plots a point on the screen
  ------------------------------------------------------------------
  Input parameters : X, Y      = point coordinates
                     CurvIndex = index of curve parameters
                                  (Symbol, Size, Color)
  ------------------------------------------------------------------ }

procedure PlotCurve(Canvas    : TCanvas;
                    X, Y      : PVector;
                    Lb, Ub,
                    CurvIndex : Integer);
{ ------------------------------------------------------------------
  Plots a curve
  ------------------------------------------------------------------
  Input parameters : X, Y      = point coordinates
                     Lb, Ub    = indices of first and last points
                     CurvIndex = index of curve parameters
  ------------------------------------------------------------------ }

procedure PlotCurveWithErrorBars(Canvas    : TCanvas;
                                 X, Y, S   : PVector;
                                 Ns, Lb, Ub,
                                 CurvIndex : Integer);
{ ------------------------------------------------------------------
  Plots a curve with error bars
  ------------------------------------------------------------------
  Input parameters : X, Y      = point coordinates
                     S         = errors
                     Ns        = number of SD to be plotted
                     Lb, Ub    = indices of first and last points
                     CurvIndex = index of curve parameters
  ------------------------------------------------------------------ }

procedure PlotFunc(Canvas     : TCanvas;
                   Func       : TFunc;
                   Xmin, Xmax : Float;
                   Npt,
                   CurvIndex  : Integer);
{ ------------------------------------------------------------------
  Plots a function
  ------------------------------------------------------------------
  Input parameters:
    Func       = function to be plotted
    Xmin, Xmax = abscissae of 1st and last point to plot
    Npt        = number of points
    CurvIndex  = index of curve parameters (Width, Style, Color)
  ------------------------------------------------------------------
  The function must be programmed as :
  function Func(X : Float) : Float;
  ------------------------------------------------------------------ }

procedure WriteLegend(Canvas    : TCanvas;
                      NCurv     : Integer;
                      ShowPoints,
                      ShowLines : Boolean);
{ ------------------------------------------------------------------
  Writes the legends for the plotted curves
  ------------------------------------------------------------------
  NCurv      : number of curves (1 to MaxCurv)
  ShowPoints : for displaying points
  ShowLines  : for displaying lines
  ------------------------------------------------------------------ }

procedure ConRec(Canvas     : TCanvas;
                 Nx, Ny, Nc : Integer;
                 X, Y, Z    : PVector;
                 F          : PMatrix);
{ ------------------------------------------------------------------
  Contour plot
  Adapted from Paul Bourke, Byte, June 1987
  http://astronomy.swin.edu.au/~pbourke/projection/conrec/
  ------------------------------------------------------------------
  Input parameters:
  Nx, Ny             = number of steps on Ox and Oy
  Nc                 = number of contour levels
  X[0..Nx], Y[0..Ny] = point coordinates in pixels
  Z[0..(Nc - 1)]     = contour levels in increasing order
  F[0..Nx, 0..Ny]    = function values, such that F[I,J] is the
                       function value at (X[I], Y[I])
  ------------------------------------------------------------------ }

function Xpixel(X : Float) : Integer;
{ ------------------------------------------------------------------
  Converts user abscissa X to screen coordinate
  ------------------------------------------------------------------ }

function Ypixel(Y : Float) : Integer;
{ ------------------------------------------------------------------
  Converts user ordinate Y to screen coordinate
  ------------------------------------------------------------------ }

function Xuser(X : Integer) : Float;
{ ------------------------------------------------------------------
  Converts screen coordinate X to user abscissa
  ------------------------------------------------------------------ }

function Yuser(Y : Integer) : Float;
{ ------------------------------------------------------------------
  Converts screen coordinate Y to user ordinate
  ------------------------------------------------------------------ }

procedure LeaveGraphics;
{ ------------------------------------------------------------------
  Quits graphic mode
  ------------------------------------------------------------------ }


implementation

const
  MaxSymbol    = 9;          { Max. number of symbols for plotting curves }
  MaxCurvColor = 9;          { Max. number of colors for curves }
  Eps          = 1.0E-10;    { Lower limit for an axis label }
  MaxColor     = $02FFFFFF;  { Max. color value for Delphi }

const
  CurvColor : array[1..MaxCurvColor] of TColor =
    (clRed,
     clGreen,
     clBlue,
     clFuchsia,
     clAqua,
     clLime,
     clNavy,
     clOlive,
     clPurple);

type
  TAxis = record        { Coordinate axis }
    Scale : TScale;
    Min   : Float;
    Max   : Float;
    Step  : Float;
  end;

  TPointParam = record  { Point parameters                            }
    Symbol : Integer;   { Symbol: 0: point (.)                        }
    Size   : Integer;   {         1: solid circle    2: open circle   }
    Color  : TColor;    {         3: solid square    4: open square   }
  end;                  {         5: solid triangle  6: open triangle }
                        {         7: plus (+)        8: multiply (x)  }
                        {         9: star (* )                        }

  TLineParam = record   { Line parameters }
    Style : TPenStyle;
    Width : Integer;
    Color : TColor;
  end;

  TCurvParam = record          { Curve parameters }
    PointParam : TPointParam;
    LineParam  : TLineParam;
    Legend     : Str30;        { Legend of curve }
    Step       : Integer;      { Plot 1 point every Step points }
  end;

  TCurvParamVector = array[1..255] of TCurvParam;
  PCurvParamVector = ^TCurvParamVector;

var
  Xwin1, Xwin2, Ywin1, Ywin2 : Integer;
  XminPixel, XmaxPixel       : Integer;
  YminPixel, YmaxPixel       : Integer;
  FactX, FactY               : Float;
  XAxis, YAxis               : TAxis;
  GraphTitle, XTitle, YTitle : String;
  MaxCurv                    : Integer;
  CurvParam                  : PCurvParamVector;
  GraphWidth, GraphHeight    : Integer;
  SymbolSizeUnit             : Integer;
  PenWidth                   : Integer;
  PenStyle                   : TPenStyle;
  PenColor, BrushColor       : TColor;
  BrushStyle                 : TBrushStyle;

procedure DimCurvParamVector(var CurvParam : PCurvParamVector; Ub : Byte);
var
  I : Integer;
begin
  { Allocate vector }
  GetMem(CurvParam, Ub * SizeOf(TCurvParam));
  if CurvParam = nil then Exit;

  MaxCurv := Ub;

  { Initialize curve parameters }
  for I := 1 to Ub do
    with CurvParam^[I] do
      begin
        PointParam.Symbol := (I - 1) mod MaxSymbol + 1;
        PointParam.Size := 2;
        PointParam.Color := CurvColor[(I - 1) mod MaxCurvColor + 1];
        Legend := 'Curve ' + LTrim(IntStr(I));
        LineParam.Width := 1;
        LineParam.Style := psSolid;
        LineParam.Color := PointParam.Color;
        Step := 1;
      end;
end;

procedure DelCurvParamVector(var CurvParam : PCurvParamVector; Ub : Byte);
begin
  if CurvParam <> nil then
    begin
      FreeMem(CurvParam, Ub * SizeOf(TCurvParam));
      CurvParam := nil;
      MaxCurv := 0;
    end;
end;

function InitGraphics(Canvas : TCanvas; Width, Height : Integer) : Boolean;
begin
  GraphWidth := Width;
  GraphHeight := Height;
  SymbolSizeUnit := GraphWidth div 250;

  XmaxPixel := Width;
  YmaxPixel := Height;

  XminPixel := 0;
  YminPixel := 0;

  XTitle     := 'X';
  YTitle     := 'Y';
  GraphTitle := '';

  MaxCurv := MaxSymbol;
  DimCurvParamVector(CurvParam, MaxCurv);

  InitGraphics := True;
end;

procedure SetWindow(Canvas         : TCanvas;
                    X1, X2, Y1, Y2 : Integer;
                    GraphBorder    : Boolean);
var
  R : Float;
begin
  if (X1 >= 0) and (X2 <= 100) and (X1 < X2) then
    begin
      Xwin1 := X1;
      Xwin2 := X2;
      R := 0.01 * GraphWidth;
      XminPixel := Round(X1 * R);
      XmaxPixel := Round(X2 * R);
    end;

  if (Y1 >= 0) and (Y2 <= 100) and (Y1 < Y2) then
    begin
      Ywin1 := Y1;
      Ywin2 := Y2;
      R := 0.01 * GraphHeight;
      YminPixel := Round(Y1 * R);
      YmaxPixel := Round(Y2 * R);
    end;

  XAxis.Scale := LinScale;
  XAxis.Min   := 0.0;
  XAxis.Max   := 1.0;
  XAxis.Step  := 0.2;

  YAxis.Scale := LinScale;
  YAxis.Min   := 0.0;
  YAxis.Max   := 1.0;
  YAxis.Step  := 0.2;

  FactX := (XmaxPixel - XminPixel) / (XAxis.Max - XAxis.Min);
  FactY := (YmaxPixel - YminPixel) / (YAxis.Max - YAxis.Min);

  if GraphBorder then
    Canvas.Rectangle(XminPixel, YminPixel, Succ(XmaxPixel), Succ(YmaxPixel));
end;

procedure AutoScale(X : PVector; Lb, Ub : Integer; Scale : TScale;
                    var XMin, XMax, XStep : Float);
var
  I      : Integer;
  X1, X2 : Float;
begin
  { Minimum and maximum of X }

  X1 := X^[Lb];
  X2 := X1;
  for I := Lb to Ub do
    if X^[I] < X1 then
      X1 := X^[I]
    else if X^[I] > X2 then
      X2 := X^[I];

  { Linear scale }

  if Scale = LinScale then
    begin
      Interval(X1, X2, 2, 6, XMin, XMax, XStep);
      Exit;
    end;

  { Logarithmic scale }

  XMin := 1.0E-3;
  XMax := 1.0E+3;
  XStep := 10.0;

  if X1 <= 0.0 then Exit;

  XMin := Int(Log10(X1)); if X1 < 1.0 then XMin := XMin - 1.0;
  XMax := Int(Log10(X2)); if X2 > 1.0 then XMax := XMax + 1.0;
  XMin := Exp10(XMin);
  XMax := Exp10(XMax);
end;

procedure SetOxScale(Scale : TScale; OxMin, OxMax, OxStep : Float);
begin
  XAxis.Scale := Scale;
  case Scale of
    LinScale :
      begin
        if OxMin < OxMax then
          begin
            XAxis.Min := OxMin;
            XAxis.Max := OxMax;
          end;
        if OxStep > 0.0 then XAxis.Step := OxStep;
      end;
    LogScale :
      begin
        if (OxMin > 0.0) and (OxMin < OxMax) then
          begin
            XAxis.Min := Floor(Log10(OxMin));
            XAxis.Max := Ceil(Log10(OxMax));
          end;
        XAxis.Step := 1.0;
      end;
  end;
  FactX := (XmaxPixel - XminPixel) / (XAxis.Max - XAxis.Min);
end;

procedure SetOyScale(Scale : TScale; OyMin, OyMax, OyStep : Float);
begin
  YAxis.Scale := Scale;
  case Scale of
    LinScale :
      begin
        if OyMin < OyMax then
          begin
            YAxis.Min := OyMin;
            YAxis.Max := OyMax;
          end;
        if OyStep > 0.0 then YAxis.Step := OyStep;
      end;
    LogScale :
      begin
        if (OyMin > 0.0) and (OyMin < OyMax) then
          begin
            YAxis.Min := Floor(Log10(OyMin));
            YAxis.Max := Ceil(Log10(OyMax));
          end;
        YAxis.Step := 1.0;
      end;
  end;
  FactY := (YmaxPixel - YminPixel) / (YAxis.Max - YAxis.Min);
end;

function Xpixel(X : Float) : Integer;
var
  P : Float;
begin
  P := FactX * (X - XAxis.Min);
  if Abs(P) > 30000 then
    Xpixel := 30000
  else
    Xpixel := Round(P) + XminPixel;
end;

function Ypixel(Y : Float) : Integer;
var
  P : Float;
begin
  P := FactY * (YAxis.Max - Y);
  if Abs(P) > 30000 then
    Ypixel := 30000
  else
    Ypixel := Round(P) + YminPixel;
end;

function Xuser(X : Integer) : Float;
begin
  Xuser := XAxis.Min + (X - XminPixel) / FactX;
end;

function Yuser(Y : Integer) : Float;
begin
  Yuser := YAxis.Max - (Y - YminPixel) / FactY;
end;

procedure SetGraphTitle(Title : String);
begin
  GraphTitle := Title;
end;

procedure SetOxTitle(Title : String);
begin
  XTitle := Title;
end;

procedure SetOyTitle(Title : String);
begin
  YTitle := Title;
end;

procedure PlotLine(Canvas : TCanvas; X1, Y1, X2, Y2 : Integer);
begin
  Canvas.MoveTo(X1, Y1);
  Canvas.LineTo(X2, Y2);
end;

procedure PlotOxAxis(Canvas : TCanvas);
var
  W, X, Z          : Float;
  Wp, Xp, Yp1, Yp2 : Integer;
  N, I, J          : Integer;
  TickLength       : Integer;
  MinorTickLength  : Integer;
  XLabel           : String;
begin
  TickLength := Canvas.TextHeight('M') div 2;
  MinorTickLength := Round(0.67 * TickLength);

  PlotLine(Canvas, XminPixel, YmaxPixel, XmaxPixel, YmaxPixel);

  N := Round((XAxis.Max - XAxis.Min) / XAxis.Step);  { Nb of intervals }
  X := XAxis.Min;                                    { Tick mark position }

  Yp1 := YmaxPixel + TickLength;       { End of tick mark }
  Yp2 := YmaxPixel + MinorTickLength;  { End of minor tick mark (log scale) }

  for I := 0 to N do  { Label axis }
    begin
      if (XAxis.Scale = LinScale) and (Abs(X) < Eps) then X := 0.0;

      Xp := Xpixel(X);

      PlotLine(Canvas, Xp, YmaxPixel, Xp, Yp1);

      if XAxis.Scale = LinScale then Z := X else Z := Exp10(X);

      XLabel := Trim(FloatStr(Z));

      Canvas.TextOut(Xp - Canvas.TextWidth(XLabel) div 2, Yp1, XLabel);

      { Plot minor divisions on logarithmic scale }

      if (XAxis.Scale = LogScale) and (I < N) then
        for J := 2 to 9 do
          begin
            W := X + Log10(J);
            Wp := Xpixel(W);
            PlotLine(Canvas, Wp, YmaxPixel, Wp, Yp2);
          end;

      X := X + XAxis.Step;
    end;

  { Write axis title }

  if XTitle <> '' then
    Canvas.TextOut(XminPixel + (XmaxPixel - XminPixel -
                   Canvas.TextWidth(XTitle)) div 2,
                   YmaxPixel + 4 * TickLength, XTitle);
end;

procedure PlotOyAxis(Canvas : TCanvas);
var
  W, Y, Z          : Float;
  Wp, Xp1, Xp2, Yp : Integer;
  N, I, J          : Integer;
  TickLength       : Integer;
  MinorTickLength  : Integer;
  Yoffset          : Integer;
  YLabel           : String;
begin
  TickLength := Canvas.TextWidth('M') div 2;
  MinorTickLength := Round(0.67 * TickLength);
  Yoffset := Canvas.TextHeight('M') div 2;

  PlotLine(Canvas, XminPixel, YminPixel, XminPixel, YmaxPixel);

  N := Round((YAxis.Max - YAxis.Min) / YAxis.Step);
  Y := YAxis.Min;

  Xp1 := XminPixel - TickLength;
  Xp2 := XminPixel - MinorTickLength;

  for I := 0 to N do
    begin
      if (YAxis.Scale = LinScale) and (Abs(Y) < Eps) then Y := 0.0;

      Yp := Ypixel(Y);

      PlotLine(Canvas, XminPixel, Yp, Xp1, Yp);

      if YAxis.Scale = LinScale then Z := Y else Z := Exp10(Y);

      YLabel := Trim(FloatStr(Z));

      Canvas.TextOut(Xp1 - Canvas.TextWidth(YLabel), Yp - Yoffset, YLabel);

      if (YAxis.Scale = LogScale) and (I < N) then
        for J := 2 to 9 do
          begin
            W := Y + Log10(J);
            Wp := Ypixel(W);
            PlotLine(Canvas, XminPixel, Wp, Xp2, Wp);
          end;

      Y := Y + YAxis.Step;
    end;

  if YTitle <> '' then
    Canvas.TextOut(XminPixel, YminPixel - 3 * Yoffset, YTitle);
end;

procedure PlotGrid(Canvas : TCanvas; Grid : TGrid);
var
  X, Y         : Float;
  I, N, Xp, Yp : Integer;

var
  PenStyle : TpenStyle;

begin
  PenStyle := Canvas.Pen.Style;
  Canvas.Pen.Style := psDot;

  if Grid in [HorizGrid, BothGrid] then  { Horizontal lines }
    begin
      N := Round((YAxis.Max - YAxis.Min) / YAxis.Step);  { Nb of intervals }
      for I := 1 to Pred(N) do
        begin
          Y := YAxis.Min + I * YAxis.Step;  { Origin of line }
          Yp := Ypixel(Y);
          PlotLine(Canvas, XminPixel, Yp, XmaxPixel, Yp);
        end;
    end;

  if Grid in [VertiGrid, BothGrid] then  { Vertical lines }
    begin
      N := Round((XAxis.Max - XAxis.Min) / XAxis.Step);
      for I := 1 to Pred(N) do
        begin
          X := XAxis.Min + I * XAxis.Step;
          Xp := Xpixel(X);
          PlotLine(Canvas, Xp, YminPixel, Xp, YmaxPixel);
        end;
    end;

  Canvas.Pen.Style := PenStyle;
end;

procedure WriteGraphTitle(Canvas : TCanvas);
begin
  if GraphTitle <> '' then
    with Canvas do
      TextOut((XminPixel + XmaxPixel - TextWidth(GraphTitle)) div 2,
               YminPixel - 2 * TextHeight(GraphTitle), GraphTitle);
end;

procedure SetMaxCurv(NCurv : Byte);
begin
  if NCurv < 1 then Exit;
  DelCurvParamVector(CurvParam, MaxCurv);
  MaxCurv := NCurv;
  DimCurvParamVector(CurvParam, MaxCurv);
end;

procedure SetPointParam(CurvIndex, Symbol, Size : Integer;
                        Color                   : TColor);
begin
  if (CurvIndex < 1) or (CurvIndex > MaxCurv) then Exit;

  if (Symbol >= 0) and (Symbol <= MaxSymbol) then
    CurvParam^[CurvIndex].PointParam.Symbol := Symbol;

  if Size > 0 then
    CurvParam^[CurvIndex].PointParam.Size := Size;

  if (Color >= 0) and (Color <= MaxColor) then
    CurvParam^[CurvIndex].PointParam.Color := Color;
end;

procedure SetLineParam(CurvIndex : Integer;
                       Style     : TPenStyle;
                       Width     : Integer;
                       Color     : TColor);

begin
  if (CurvIndex < 1) or (CurvIndex > MaxCurv) then Exit;

  CurvParam^[CurvIndex].LineParam.Style := Style;

  if Width > 0 then
    CurvParam^[CurvIndex].LineParam.Width := Width;

  if (Color >= 0) and (Color <= MaxColor) then
    CurvParam^[CurvIndex].LineParam.Color := Color;
end;

procedure SetCurvLegend(CurvIndex : Integer; Legend : String);
begin
  if (CurvIndex >= 1) and (CurvIndex <= MaxCurv) then
    CurvParam^[CurvIndex].Legend := Legend;
end;

procedure SetCurvStep(CurvIndex, Step : Integer);
begin
  if (CurvIndex >= 1) and (CurvIndex <= MaxCurv) and (Step > 0) then
    CurvParam^[CurvIndex].Step := Step;
end;

function XOutOfBounds(X : Integer) : Boolean;
{ Checks if an absissa is outside the graphic bounds }
begin
  XOutOfBounds := (X < XminPixel) or (X > XmaxPixel);
end;

function YOutOfBounds(Y : Integer) : Boolean;
{ Checks if an ordinate is outside the graphic bounds }
begin
  YOutOfBounds := (Y < YminPixel) or (Y > YmaxPixel);
end;

function CheckPoint(X, Y       : Float;
                    var Xp, Yp : Integer) : Boolean;
{ Computes the pixel coordinates of a point and
  checks if it is enclosed within the graph limits }
begin
  Xp := Xpixel(X);
  Yp := Ypixel(Y);
  CheckPoint := not(XOutOfBounds(Xp) or YOutOfBounds(Yp));
end;

procedure PlotSymbol(Canvas    : TCanvas;
                     Xp, Yp,
                     CurvIndex : Integer);
var
  Xp1, Xp2, Yp1, Yp2, Size : Integer;
begin
  Size := CurvParam^[CurvIndex].PointParam.Size * SymbolSizeUnit;

  Xp1 := Xp - Size;
  Yp1 := Yp - Size;
  Xp2 := Xp + Size + 1;
  Yp2 := Yp + Size + 1;

  with Canvas do
    case CurvParam^[CurvIndex].PointParam.Symbol of
      0    : Pixels[Xp, Yp] := Brush.Color;
      1, 2 : Ellipse(Xp1, Yp1, Xp2, Yp2);          { Circle }
      3, 4 : Rectangle(Xp1, Yp1, Xp2, Yp2);        { Square }
      5, 6 : Polygon([Point(Xp1, Yp2 - 1),
                      Point(Xp2, Yp2 - 1),
                      Point(Xp, Yp1 - 1)]);        { Triangle }
      7 : begin                                    { + }
            PlotLine(Canvas, Xp, Yp1, Xp, Yp2);
            PlotLine(Canvas, Xp1, Yp, Xp2, Yp);
          end;
      8 : begin                                    { x }
            PlotLine(Canvas, Xp1, Yp1, Xp2, Yp2);
            PlotLine(Canvas, Xp1, Yp2 - 1, Xp2, Yp1 - 1);
          end;
      9 : begin                                    { * }
            PlotLine(Canvas, Xp, Yp1, Xp, Yp2);
            PlotLine(Canvas, Xp1, Yp, Xp2, Yp);
            PlotLine(Canvas, Xp1, Yp1, Xp2, Yp2);
            PlotLine(Canvas, Xp1, Yp2 - 1, Xp2, Yp1 - 1);
          end;
      end;
end;

procedure SetGraphSettings(Canvas : TCanvas; CurvIndex : Integer);
{ Saves the current graphic properties of the Canvas
  and sets them to the values of curve # CurvIndex }
begin
  PenColor   := Canvas.Pen.Color;
  PenStyle   := Canvas.Pen.Style;
  PenWidth   := Canvas.Pen.Width;
  BrushColor := Canvas.Brush.Color;
  BrushStyle := Canvas.Brush.Style;

  Canvas.Pen.Color   := CurvParam^[CurvIndex].LineParam.Color;
  Canvas.Pen.Style   := CurvParam^[CurvIndex].LineParam.Style;
  Canvas.Pen.Width   := CurvParam^[CurvIndex].LineParam.Width;
  Canvas.Brush.Color := CurvParam^[CurvIndex].PointParam.Color;

  if CurvParam^[CurvIndex].PointParam.Symbol in [0, 1, 3, 5] then
    Canvas.Brush.Style := bsSolid
  else
    Canvas.Brush.Style := bsClear;
end;

procedure RestoreGraphSettings(Canvas : TCanvas);
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Pen.Style   := PenStyle;
  Canvas.Pen.Width   := PenWidth;
  Canvas.Brush.Color := BrushColor;
  Canvas.Brush.Style := BrushStyle;
end;

procedure PlotPoint(Canvas    : TCanvas;
                    X, Y      : Float;
                    CurvIndex : Integer);
var
  Xp, Yp : Integer;
begin
  if XAxis.Scale = LogScale then X := Log10(X);
  if YAxis.Scale = LogScale then Y := Log10(Y);

  if not CheckPoint(X, Y, Xp, Yp) then Exit;

  SetGraphSettings(Canvas, CurvIndex);
  PlotSymbol(Canvas, Xp, Yp, CurvIndex);
  RestoreGraphSettings(Canvas);
end;

procedure PlotErrorBar(Canvas       : TCanvas;
                       Y, S         : Float;
                       Ns,
                       Xp, Yp, Size : Integer);
{ Plots an error bar with the current canvas settings }
var
  Delta, Y1 : Float;
  Yp1       : Integer;
  PenStyle  : TPenStyle;
begin
  Size := Size * SymbolSizeUnit;
  PenStyle := Canvas.Pen.Style;
  Canvas.Pen.Style := psSolid;

  Delta := Ns * S;
  Y1 := Y - Delta;
  if YAxis.Scale = LogScale then Y1 := Log10(Y1);
  Yp1 := Ypixel(Y1);

  if Yp1 <= YmaxPixel then
    begin
      PlotLine(Canvas, Xp - Size, Yp1, Xp + Size + 1, Yp1);
      PlotLine(Canvas, Xp, Yp, Xp, Yp1);
    end
  else
    PlotLine(Canvas, Xp, Yp, Xp, YmaxPixel);

  Y1 := Y + Delta;
  if YAxis.Scale = LogScale then Y1 := Log10(Y1);
  Yp1 := Ypixel(Y1);

  if Yp1 >= YminPixel then
    begin
      PlotLine(Canvas, Xp - Size, Yp1, Xp + Size + 1, Yp1);
      PlotLine(Canvas, Xp, Yp, Xp, Yp1);
    end
  else
    PlotLine(Canvas, Xp, Yp, Xp, YminPixel);

  Canvas.Pen.Style := PenStyle;
end;

procedure GenPlotCurve(Canvas    : TCanvas;
                       X, Y, S   : PVector;
                       Ns,
                       Lb, Ub,
                       CurvIndex : Integer;
                       ErrorBars : Boolean);
{ General curve plotting routine }
var
  X1, Y1, X2, Y2     : Float;
  Xp1, Yp1, Xp2, Yp2 : Integer;
  I                  : Integer;
  Flag1, Flag2       : Boolean;
begin
  SetGraphSettings(Canvas, CurvIndex);

  { Plot first point }

  X1 := X^[Lb]; if XAxis.Scale = LogScale then X1 := Log10(X1);
  Y1 := Y^[Lb]; if YAxis.Scale = LogScale then Y1 := Log10(Y1);

  Flag1 := CheckPoint(X1, Y1, Xp1, Yp1);

  if Flag1 then
    begin
      PlotSymbol(Canvas, Xp1, Yp1, CurvIndex);
      if ErrorBars and (S^[Lb] > 0.0) then
        PlotErrorBar(Canvas, Y^[Lb], S^[Lb], Ns, Xp1, Yp1, CurvIndex);
    end;

  { Plot other points and connect them by lines if necessary }

  I := Lb + CurvParam^[CurvIndex].Step;

  while I <= Ub do
    begin
      X2 := X^[I]; if XAxis.Scale = LogScale then X2 := Log10(X2);
      Y2 := Y^[I]; if YAxis.Scale = LogScale then Y2 := Log10(Y2);

      Flag2 := CheckPoint(X2, Y2, Xp2, Yp2);

      if Flag2 then
        begin
          PlotSymbol(Canvas, Xp2, Yp2, CurvIndex);
          if ErrorBars and (S^[I] > 0.0) then
            PlotErrorBar(Canvas, Y^[I], S^[I], Ns, Xp2, Yp2, CurvIndex);
          if (CurvParam^[CurvIndex].LineParam.Style <> psClear) and Flag1 then
            PlotLine(Canvas, Xp1, Yp1, Xp2, Yp2);
        end;

      Xp1 := Xp2;
      Yp1 := Yp2;
      Flag1 := Flag2;
      Inc(I, CurvParam^[CurvIndex].Step);
    end;

  RestoreGraphSettings(Canvas);
end;

procedure PlotCurve(Canvas    : TCanvas;
                    X, Y      : PVector;
                    Lb, Ub    : Integer;
                    CurvIndex : Integer);
begin
  GenPlotCurve(Canvas, X, Y, nil, 0, Lb, Ub, CurvIndex, False);
end;

procedure PlotCurveWithErrorBars(Canvas    : TCanvas;
                                 X, Y, S   : PVector;
                                 Ns        : Integer;
                                 Lb, Ub    : Integer;
                                 CurvIndex : Integer);
begin
  GenPlotCurve(Canvas, X, Y, S, Ns, Lb, Ub, CurvIndex, True);
end;

procedure PlotFunc(Canvas     : TCanvas;
                   Func       : TFunc;
                   Xmin, Xmax : Float;
                   Npt        : Integer;
                   CurvIndex  : Integer);
var
  X1, Y1, X2, Y2, H  : Float;
  Xp1, Yp1, Xp2, Yp2 : Integer;
  Flag1, Flag2       : Boolean;
  I                  : Integer;
begin
  if (Npt < 2) or (CurvParam^[CurvIndex].LineParam.Style = psClear) then Exit;

  if Xmin >= Xmax then
    begin
      Xmin := XAxis.Min;
      Xmax := XAxis.Max;
    end;

  H := (Xmax - Xmin) / Npt;

  SetGraphSettings(Canvas, CurvIndex);

  { Check first point }
  X1 := Xmin;
  if XAxis.Scale = LinScale then
    Y1 := Func(X1)
  else
    Y1 := Func(Exp10(X1));

  if YAxis.Scale = LogScale then Y1 := Log10(Y1);
  Flag1 := CheckPoint(X1, Y1, Xp1, Yp1);

  { Check other points and plot lines if possible }
  for I := 1 to Npt do
    begin
      X2 := X1 + H;
      if XAxis.Scale = LinScale then
        Y2 := Func(X2)
      else
        Y2 := Func(Exp10(X2));

      if YAxis.Scale = LogScale then Y2 := Log10(Y2);

      Flag2 := CheckPoint(X2, Y2, Xp2, Yp2);

      if Flag1 and Flag2 then
        PlotLine(Canvas, Xp1, Yp1, Xp2, Yp2);

      X1 := X2;
      Xp1 := Xp2;
      Yp1 := Yp2;
      Flag1 := Flag2;
    end;

   RestoreGraphSettings(Canvas);
end;

procedure WriteLegend(Canvas     : TCanvas;
                      NCurv      : Integer;
                      ShowPoints,
                      ShowLines  : Boolean);

var
  CharHeight, I, L, Lmax : Integer;
  N, Nmax, Xp, Xl, Y     : Integer;
begin
  N := 0;     { Nb of legends to be plotted  }
  Lmax := 0;  { Length of the longest legend }

  for I := 1 to NCurv do
    if CurvParam^[I].Legend <> '' then
      begin
        Inc(N);
        L := Canvas.TextWidth(CurvParam^[I].Legend);
        if L > Lmax then Lmax := L;
      end;

  if (N = 0) or (Lmax = 0) then Exit;

  { Character height }
  CharHeight := Canvas.TextHeight('M');

  { Max. number of legends which may be plotted }
  Nmax := Round((YmaxPixel - YminPixel) / CharHeight) - 1;
  if N > Nmax then N := Nmax;

  { Draw rectangle around the legends }
  Canvas.Rectangle(XmaxPixel + Round(0.02 * GraphWidth), YminPixel,
                   XmaxPixel + Round(0.12 * GraphWidth) + Lmax,
                   YminPixel + (N + 1) * CharHeight);

  L := Round(0.02 * GraphWidth);  { Half-length of line }
  Xp := XmaxPixel + 3 * L;        { Position of symbol  }
  Xl := XmaxPixel + 5 * L;        { Position of legend  }

  if NCurv <= Nmax then N := NCurv else N := Nmax;

  for I := 1 to N do
    with Canvas do
      begin
        SetGraphSettings(Canvas, I);

        { Plot point and line }
        Y := YminPixel + I * CharHeight;
        if ShowPoints then
          PlotSymbol(Canvas, Xp, Y, I);
        if ShowLines then
          PlotLine(Canvas, Xp - L, Y, Xp + L, Y);

        { Write legend }
        Brush.Style := bsClear;
        Canvas.TextOut(Xl, Y - CharHeight div 2, CurvParam^[I].Legend);
      end;

  RestoreGraphSettings(Canvas);
end;

procedure ConRec(Canvas     : TCanvas;
                 Nx, Ny, Nc : Integer;
                 X, Y, Z    : PVector;
                 F          : PMatrix);

const
  { Mapping from vertex numbers to X offsets }
  Im : array[0..3] of Integer = (0, 1, 1, 0);

  { Mapping from vertex numbers to Y offsets }
  Jm : array[0..3] of Integer = (0, 0, 1, 1);

  { Case switch table }
  CasTab : array[0..2, 0..2, 0..2] of Integer =
  (((0,0,8), (0,2,5), (7,6,9)),
   ((0,3,4), (1,3,1), (4,3,0)),
   ((9,6,7), (5,2,0), (8,0,0)));

var
  I, J, K, M, M1, M2, M3 : Integer;
  X1, X2, Y1, Y2         : Float;
  Fmin, Fmax             : Float;
  Xp, Yp                 : PIntVector;
  PrmErr                 : Boolean;

var
  H   : array[0..4] of Float;    { Relative heights of the box above contour }
  Ish : array[0..4] of Integer;  { Sign of H() }
  Xh  : array[0..4] of Integer;  { X coordinates of box }
  Yh  : array[0..4] of Integer;  { Y coordinates of box }

label
  Case0, NoneInTri, NoneInBox;

begin
  { Check the input parameters for validity }

  PrmErr := False;
  SetErrCode(MatOk);

  if (Nx <= 0) or (Ny <= 0) or (Nc <= 0) then PrmErr := True;

  for K := 1 to Nc - 1 do
    if Z^[K] <= Z^[K - 1] then PrmErr := True;

  if PrmErr then
    begin
      SetErrCode(MatErrDim);
      Exit;
    end;

  { Convert user coordinates to pixels }

  DimIntVector(Xp, Nx);
  DimIntVector(Yp, Ny);

  for I := 0 to Nx do
    Xp^[I] := Xpixel(X^[I]);

  for J := 0 to Ny do
    Yp^[J] := Ypixel(Y^[J]);

  { Scan the array, top down, left to right }

  for J := Ny - 1 downto 0 do
  begin
    for I := 0 to Nx - 1 do
    begin
      { Find the lowest vertex }
      if F^[I]^[J] < F^[I]^[J + 1] then
        Fmin := F^[I]^[J]
      else
        Fmin := F^[I]^[J + 1];

      if F^[I + 1]^[J] < Fmin then
        Fmin := F^[I + 1]^[J];

      if F^[I + 1]^[J + 1] < Fmin then
        Fmin := F^[I + 1]^[J + 1];

      { Find the highest vertex }
      if F^[I]^[J] > F^[I]^[J + 1] then
        Fmax := F^[I]^[J]
      else
        Fmax := F^[I]^[J + 1];

      if F^[I + 1]^[J] > Fmax then
        Fmax := F^[I + 1]^[J];

      if F^[I + 1]^[J + 1] > Fmax then
        Fmax := F^[I + 1]^[J + 1];

      if (Fmax < Z^[0]) or (Fmin > Z^[Nc - 1]) then
        goto NoneInBox;

      { Draw each contour within this box }
      for K := 0 to Nc - 1 do
      begin
        if (Z^[K] < Fmin) or (Z^[K] > Fmax) then
          goto NoneInTri;

        for M := 4 downto 0 do
        begin
          if M > 0 then
          begin
            H[M] := F^[I + Im[M - 1]]^[J + Jm[M - 1]] - Z^[K];
            Xh[M] := Xp^[I + Im[M - 1]];
            Yh[M] := Yp^[J + Jm[M - 1]];
          end;

          if M = 0 then
          begin
            H[0] := (H[1] + H[2] + H[3] + H[4]) / 4;
            Xh[0] := (Xp^[I] + Xp^[I + 1]) div 2;
            Yh[0] := (Yp^[J] + Yp^[J + 1]) div 2;
          end;

          if H[M] > 0 then Ish[M] := 2;
          if H[M] < 0 then Ish[M] := 0;
          if H[M] = 0 then Ish[M] := 1;
        end; { next M }

        { Scan each triangle in the box }
        X1 := 0.0;
        X2 := 0.0;
        Y1 := 0.0;
        Y2 := 0.0;
        for M := 1 to 4 do
        begin
          M1 := M; M2 := 0; M3 := M + 1;
          if M3 = 5 then M3 := 1;

          case CasTab[Ish[M1], Ish[M2], Ish[M3]] of
            0 :
              goto Case0;

            { Line between vertices M1 and M2 }
            1 : begin
              X1 := Xh[M1];
              Y1 := Yh[M1];
              X2 := Xh[M2];
              Y2 := Yh[M2];
            end;

            { Line between vertices M2 and M3 }
            2 : begin
              X1 := Xh[M2];
              Y1 := Yh[M2];
              X2 := Xh[M3];
              Y2 := Yh[M3];
            end;

            { Line between vertices M3 and M1 }
            3 : begin
              X1 := Xh[M3];
              Y1 := Yh[M3];
              X2 := Xh[M1];
              Y2 := Yh[M1];
            end;

            { Line between vertex M1 and side M2-M3 }
            4 : begin
              X1 := Xh[M1];
              Y1 := Yh[M1];
              X2 := (H[M3] * Xh[M2] - H[M2] * Xh[M3]) / (H[M3] - H[M2]);
              Y2 := (H[M3] * Yh[M2] - H[M2] * Yh[M3]) / (H[M3] - H[M2]);
            end;

            { Line between vertex M2 and side M3-M1 }
            5 : begin
              X1 := Xh[M2];
              Y1 := Yh[M2];
              X2 := (H[M1] * Xh[M3] - H[M3] * Xh[M1]) / (H[M1] - H[M3]);
              Y2 := (H[M1] * Yh[M3] - H[M3] * Yh[M1]) / (H[M1] - H[M3]);
            end;

            { Line between vertex M3 and side M1-M2 }
            6 : begin
              X1 := Xh[M3];
              Y1 := Yh[M3];
              X2 := (H[M2] * Xh[M1] - H[M1] * Xh[M2]) / (H[M2] - H[M1]);
              Y2 := (H[M2] * Yh[M1] - H[M1] * Yh[M2]) / (H[M2] - H[M1]);
            end;

            { Line between sides M1-M2 and M2-M3 }
            7 : begin
              X1 := (H[M2] * Xh[M1] - H[M1] * Xh[M2]) / (H[M2] - H[M1]);
              Y1 := (H[M2] * Yh[M1] - H[M1] * Yh[M2]) / (H[M2] - H[M1]);
              X2 := (H[M3] * Xh[M2] - H[M2] * Xh[M3]) / (H[M3] - H[M2]);
              Y2 := (H[M3] * Yh[M2] - H[M2] * Yh[M3]) / (H[M3] - H[M2]);
            end;

            { Line between sides M2-M3 and M3-M1 }
            8 : begin
              X1 := (H[M3] * Xh[M2] - H[M2] * Xh[M3]) / (H[M3] - H[M2]);
              Y1 := (H[M3] * Yh[M2] - H[M2] * Yh[M3]) / (H[M3] - H[M2]);
              X2 := (H[M1] * Xh[M3] - H[M3] * Xh[M1]) / (H[M1] - H[M3]);
              Y2 := (H[M1] * Yh[M3] - H[M3] * Yh[M1]) / (H[M1] - H[M3]);
            end;

            { Line between sides M3-M1 and M1-M2 }
            9 : begin
              X1 := (H[M1] * Xh[M3] - H[M3] * Xh[M1]) / (H[M1] - H[M3]);
              Y1 := (H[M1] * Yh[M3] - H[M3] * Yh[M1]) / (H[M1] - H[M3]);
              X2 := (H[M2] * Xh[M1] - H[M1] * Xh[M2]) / (H[M2] - H[M1]);
              Y2 := (H[M2] * Yh[M1] - H[M1] * Yh[M2]) / (H[M2] - H[M1]);
            end;
          end;  { case }

          Canvas.Pen.Color := CurvParam^[K mod MaxCurv + 1].LineParam.Color;
          PlotLine(Canvas, Trunc(X1), Trunc(Y1), Trunc(X2), Trunc(Y2));
Case0:
        end;  { next M }
NoneInTri:
      end;  { next K }
NoneInBox:
    end;  { next I }
  end;  { next J }
end;

procedure LeaveGraphics;
begin
  DelCurvParamVector(CurvParam, MaxCurv);
end;

end.
