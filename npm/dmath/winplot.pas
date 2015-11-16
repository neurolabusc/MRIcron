{ **********************************************************************
  *                          Unit WINPLOT.PAS                          *
  *                            Version 1.1                             *
  *                    (c) J. Debord, October 1999                     *
  **********************************************************************
                      Plotting routines for DELPHI
  ********************************************************************** }

unit WinPlot;

interface

uses
  { DELPHI units }
  WinTypes,
  Graphics,
  { TPMath units }
  FMath,
  Matrices,
  Stat,
  PaString;

{ ************************* Constants and types ************************ }

const
  MAXCURV   = 255;      { Max. number of curves which may be plotted }
  MAXSYMBOL = 9;        { Max. number of symbols for plotting curves }
  EPS       = 1.0E-10;  { Lower limit for an axis label }

type
  TScale = (LIN_SCALE,         { Scale }
            LOG_SCALE);

  TGrid = (NO_GRID,            { Grid }
           HORIZ_GRID,
           VERTIC_GRID,
           BOTH_GRID);

  TAxis = record               { Coordinate axis }
    Scale          : TScale;
    Min, Max, Step : Float;
    Title          : String;
  end;

  TPointParam = record         { Point parameters }
    Symbol : Integer;          { Symbol index }
    Size   : Integer;          { Symbol size in 1/250 of graphic width }
    Color  : TColor;
  end;

  TLineParam = record          { Line parameters }
    Width : Integer;
    Style : TPenStyle;
    Color : TColor;
  end;

  TCurvParam = record          { Curve parameters }
    PointParam : TPointParam;
    LineParam  : TLineParam;
    Legend     : String[30];   { Legend of curve }
    Step       : Integer;      { Plot 1 point every Step points }
    Connect    : Boolean;      { Connect points with line? }
  end;

  TCurvParamArray = array[1..MAXCURV] of TCurvParam;

  PCurvParamArray = ^TCurvParamArray;

{ ******** Global variables defining the appearance of the graph ******* }

const
  Xwin1 : Integer = 15;  { Window coordinates in percent of maximum }
  Ywin1 : Integer = 15;
  Xwin2 : Integer = 75;
  Ywin2 : Integer = 75;

  GraphBorder : Boolean = True;  { For plotting a rectangle around the graph }

  XAxis : TAxis = (Scale : LIN_SCALE;  { Horizontal axis }
                   Min   : 0.0;
                   Max   : 1.0;
                   Step  : 0.2;
                   Title : 'X');

  YAxis : TAxis = (Scale : LIN_SCALE;  { Vertical axis }
                   Min   : 0.0;
                   Max   : 1.0;
                   Step  : 0.2;
                   Title : 'Y');

  Grid : TGrid = BOTH_GRID;  { Grid }

  GraphTitle : String = '';  { Title of graph }

{ ************************** Graphic routines ************************** }

procedure InitGraph(Canvas        : TCanvas;
                    Width, Height : Integer);
{ ----------------------------------------------------------------------
  Initializes the graphic
  ----------------------------------------------------------------------
  The parameters refer to the object on which the graphic is plotted.

  Examples:

  To draw on a TImage object:
  InitGraph(Image1.Canvas, Image1.Width, Image1.Height);

  To print the graphic:
  InitGraph(Printer.Canvas, Printer.PageWidth, Printer.PageHeight);
  ---------------------------------------------------------------------- }

procedure PlotXAxis(Canvas : TCanvas);
{ ----------------------------------------------------------------------
  Plots the X axis
  ---------------------------------------------------------------------- }

procedure PlotYAxis(Canvas : TCanvas);
{ ----------------------------------------------------------------------
  Plots the Y axis
  ---------------------------------------------------------------------- }

procedure WriteTitle(Canvas : TCanvas);
{ ----------------------------------------------------------------------
  Writes the title of the graph
  ---------------------------------------------------------------------- }

procedure PlotGrid(Canvas : TCanvas);
{ ----------------------------------------------------------------------
  Plots a grid on the graph
  ---------------------------------------------------------------------- }

procedure PlotPoint(Canvas     : TCanvas;
                    X, Y       : Float;
                    PointParam : TPointParam);
{ ----------------------------------------------------------------------
  Plots a point
  ----------------------------------------------------------------------
  X, Y       : point coordinates
  PointParam : point parameters
  ---------------------------------------------------------------------- }

procedure PlotCurve(Canvas         : TCanvas;
                    X, Y           : PVector;
                    Lbound, Ubound : Integer;
                    CurvParam      : TCurvParam);
{ ----------------------------------------------------------------------
  Plots a curve
  ----------------------------------------------------------------------
  X, Y           : point coordinates
  Lbound, Ubound : indices of first and last points
  CurvParam      : curve parameters
  ---------------------------------------------------------------------- }

procedure PlotCurveWithErrorBars(Canvas         : TCanvas;
                                 X, Y, S        : PVector;
                                 Ns             : Integer;
                                 Lbound, Ubound : Integer;
                                 CurvParam      : TCurvParam);
{ ----------------------------------------------------------------------
  Plots a curve with error bars
  ----------------------------------------------------------------------
  X, Y           : point coordinates
  S              : errors (e.g. standard deviations)
  Ns             : error multiplier (e.g. 2 for plotting 2 SD's) 
  Lbound, Ubound : indices of first and last points
  CurvParam      : curve parameters
  ---------------------------------------------------------------------- }

procedure PlotFunc(Canvas     : TCanvas;
                   Func       : TFunc;
                   Xmin, Xmax : Float;
                   Npt        : Integer;
                   LineParam  : TLineParam);
{ ----------------------------------------------------------------------
  Plots a function
  ----------------------------------------------------------------------
  Func       : function to be plotted
               must be programmed as: function Func(X : Float) : Float;
  Xmin, Xmax : abscissae of 1st and last point to plot
  Npt        : number of points
  LineParam  : line parameters
  ---------------------------------------------------------------------- }

procedure WriteLegend(Canvas     : TCanvas;
                      NCurv      : Integer;
                      CurvParam  : PCurvParamArray;
                      ShowPoints,
                      ShowLines  : Boolean);
{ ----------------------------------------------------------------------
  Writes the legends for the plotted curves
  ----------------------------------------------------------------------
  NCurv      : number of curves (1 to MAXCURV)
  CurvParam  : curve parameters
  ShowPoints : for displaying points
  ShowLines  : for displaying lines
  ---------------------------------------------------------------------- }


{ *********** The following routines are defined in PLOT.INC *********** }

procedure Interval(X1, X2             : Float;
                   MinDiv, MaxDiv     : Integer;
                   var Min, Max, Step : Float);
{ ----------------------------------------------------------------------
  Determines an interval [Min, Max] including the values from X1 to X2,
  and a subdivision Step of this interval
  ----------------------------------------------------------------------
  Input parameters  : X1, X2 = min. & max. values to be included
                      MinDiv = minimum nb of subdivisions
                      MaxDiv = maximum nb of subdivisions
  ----------------------------------------------------------------------
  Output parameters : Min, Max, Step
  ---------------------------------------------------------------------- }

procedure AutoScale(Z              : PVector;
                    Lbound, Ubound : Integer;
                    var Axis       : TAxis);
{ ----------------------------------------------------------------------
  Determines the scale of an axis
  ----------------------------------------------------------------------
  Input parameters  : Z      = array of values to be plotted
                      Lbound,
                      Ubound = indices of first and last elements of Z
  ----------------------------------------------------------------------
  Output parameters : Axis
  ---------------------------------------------------------------------- }

function Xpixel(X : Float) : Integer;
{ ----------------------------------------------------------------------
  Converts user abscissa X to screen coordinate
  ---------------------------------------------------------------------- }

function Ypixel(Y : Float) : Integer;
{ ----------------------------------------------------------------------
  Converts user ordinate Y to screen coordinate
  ---------------------------------------------------------------------- }

function Xuser(X : Integer) : Float;
{ ----------------------------------------------------------------------
  Converts screen coordinate X to user abscissa
  ---------------------------------------------------------------------- }

function Yuser(Y : Integer) : Float;
{ ----------------------------------------------------------------------
  Converts screen coordinate Y to user ordinate
  ---------------------------------------------------------------------- }

implementation

uses
  Classes;

var
  GraphWidth, GraphHeight, SymbolSizeUnit : Integer;

{ ----------------------------------------------------------------------
  Include the variables and routines common to PLOT.PAS and WINPLOT.PAS
  ---------------------------------------------------------------------- }

  {$I PLOT.INC}

{ ---------------------------------------------------------------------- }

procedure PlotXAxis(Canvas : TCanvas);
  var
    W, X, Z : Float;
    N, I, J, TickLength, MinorTickLength, Wp, Xp : Integer;
    XLabel : String;
    NSZ : Boolean;
  begin
    TickLength := Canvas.TextHeight('M') div 2;
    MinorTickLength := Round(0.67 * TickLength);  { For log scale }

    { Draw axis }
    Canvas.MoveTo(XminPixel, YmaxPixel);
    Canvas.LineTo(XmaxPixel, YmaxPixel);

    NSZ := NSZero;
    NSZero := False;    { Don't write non significant zero's }

    N := Round((XAxis.Max - XAxis.Min) / XAxis.Step); { Nb of intervals }

    X := XAxis.Min;     { Tick mark position }
    for I := 0 to N do  { Label axis }
      begin
        if (XAxis.Scale = LIN_SCALE) and (Abs(X) < EPS) then X := 0.0;
        Xp := Xpixel(X);

        { Draw tick mark }
        Canvas.MoveTo(Xp, YmaxPixel);
        Canvas.LineTo(Xp, YmaxPixel + TickLength);

        { Write label }
        if XAxis.Scale = LIN_SCALE then Z := X else Z := Exp10(X);
        XLabel := Trim(PaString.FloatToStr(Z));
        Canvas.TextOut(Xp - Canvas.TextWidth(XLabel) div 2,
                       YmaxPixel + TickLength, XLabel);

        { Plot minor divisions on logarithmic scale }
        if (XAxis.Scale = LOG_SCALE) and (I < N) then
          for J := 2 to 9 do
            begin
              W := X + Log10(J);
              Wp := Xpixel(W);
              Canvas.MoveTo(Wp, YmaxPixel);
              Canvas.LineTo(Wp, YmaxPixel + MinorTickLength);
            end;
        X := X + XAxis.Step;
      end;

    NSZero := NSZ;

    { Write axis title }
    if XAxis.Title <> '' then
      Canvas.TextOut(XminPixel + (XmaxPixel - XminPixel -
                          Canvas.TextWidth(XAxis.Title)) div 2,
                     YmaxPixel + 2 * Canvas.TextHeight('M'),
                     XAxis.Title);
  end;

  procedure PlotYAxis(Canvas : TCanvas);
  var
    W, Y, Z : Float;
    N, I, J, Wp, Yp : Integer;
    TickLength, MinorTickLength, Yoffset : Integer;
    YLabel : String;
    NSZ : Boolean;
  begin
    TickLength := Canvas.TextWidth('M') div 2;
    MinorTickLength := Round(0.67 * TickLength);  { For log scale }

    Yoffset := Canvas.TextHeight('M') div 2;

    { Draw axis }
    Canvas.MoveTo(XminPixel, YminPixel);
    Canvas.LineTo(XminPixel, YmaxPixel);

    NSZ := NSZero;
    NSZero := False;    { Don't write non significant zero's }

    N := Round((YAxis.Max - YAxis.Min) / YAxis.Step); { Nb of intervals }

    Y := YAxis.Min;     { Tick mark position }
    for I := 0 to N do  { Label axis }
      begin
        if (YAxis.Scale = LIN_SCALE) and (Abs(Y) < EPS) then Y := 0.0;
        Yp := Ypixel(Y);

        { Draw tick mark }
        Canvas.MoveTo(XminPixel, Yp);
        Canvas.LineTo(XminPixel - TickLength, Yp);

        { Write label }
        if YAxis.Scale = LIN_SCALE then Z := Y else Z := Exp10(Y);
        YLabel := Trim(PaString.FloatToStr(Z));
        Canvas.TextOut(XminPixel - TickLength - Canvas.TextWidth(YLabel),
                       Yp - Yoffset, YLabel);

        { Plot minor divisions on logarithmic scale }
        if (YAxis.Scale = LOG_SCALE) and (I < N) then
          for J := 2 to 9 do
            begin
              W := Y + Log10(J);
              Wp := Ypixel(W);
              Canvas.MoveTo(XminPixel, Wp);
              Canvas.LineTo(XminPixel - MinorTickLength, Wp);
            end;
        Y := Y + YAxis.Step;
      end;

    NSZero := NSZ;

    { Write axis title }
    if YAxis.Title <> '' then
      Canvas.TextOut(XminPixel, YminPixel - 3 * Yoffset, YAxis.Title);
  end;

  procedure InitGraph(Canvas : TCanvas; Width, Height : Integer);
  begin
    GraphWidth := Width;
    GraphHeight := Height;
    SymbolSizeUnit := GraphWidth div 250;

    XminPixel := Round(Xwin1 / 100 * Width);
    YminPixel := Round(Ywin1 / 100 * Height);
    XmaxPixel := Round(Xwin2 / 100 * Width);
    YmaxPixel := Round(Ywin2 / 100 * Height);

    FactX := (XmaxPixel - XminPixel) / (XAxis.Max - XAxis.Min);
    FactY := (YmaxPixel - YminPixel) / (YAxis.Max - YAxis.Min);

    if GraphBorder then
      Canvas.Rectangle(XminPixel, YminPixel, Succ(XmaxPixel), Succ(YmaxPixel));
  end;

  procedure WriteTitle(Canvas : TCanvas);
  begin
    if GraphTitle <> '' then
      with Canvas do
        TextOut((XminPixel + XmaxPixel - TextWidth(GraphTitle)) div 2,
                 YminPixel - 2 * TextHeight(GraphTitle), GraphTitle);
  end;

  procedure PlotGrid(Canvas : TCanvas);
  var
    X, Y : Float;
    I, N, Xp, Yp : Integer;
    PenStyle : TpenStyle;
  begin
    { Save current settings }
    PenStyle := Canvas.Pen.Style;
    Canvas.Pen.Style := psDot;

    if Grid in [HORIZ_GRID, BOTH_GRID] then  { Horizontal lines }
      begin
        N := Round((YAxis.Max - YAxis.Min) / YAxis.Step);  { Nb of intervals }
        for I := 1 to Pred(N) do
          begin
            Y := YAxis.Min + I * YAxis.Step;  { Origin of line }
            Yp := Ypixel(Y);
            Canvas.MoveTo(XminPixel, Yp);
            Canvas.LineTo(XmaxPixel, Yp);
          end;
      end;

    if Grid in [VERTIC_GRID, BOTH_GRID] then  { Vertical lines }
      begin
        N := Round((XAxis.Max - XAxis.Min) / XAxis.Step);
        for I := 1 to Pred(N) do
          begin
            X := XAxis.Min + I * XAxis.Step;
            Xp := Xpixel(X);
            Canvas.MoveTo(Xp, YminPixel);
            Canvas.LineTo(Xp, YmaxPixel);
          end;
      end;

    { Restore settings }
    Canvas.Pen.Style := PenStyle;
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

  procedure PlotSymbol(Canvas       : TCanvas;
                       Xp, Yp       : Integer;
                       Symbol, Size : Integer);
  { Plots a symbol at pixel coordinates (Xp, Yp)
    with the current canvas settings }
  var
    Xp1, Xp2, Yp1, Yp2 : Integer;
  begin
    if Symbol > 0 then
      begin
        Size := Size * SymbolSizeUnit;
        Xp1 := Xp - Size;
        Yp1 := Yp - Size;
        Xp2 := Xp + Size + 1;
        Yp2 := Yp + Size + 1;
      end;

    with Canvas do
      case Symbol of
        0    : Pixels[Xp, Yp] := Brush.Color;
        1, 2 : Ellipse(Xp1, Yp1, Xp2, Yp2);    { Circle }
        3, 4 : Rectangle(Xp1, Yp1, Xp2, Yp2);  { Square }
        5, 6 : Polygon([Point(Xp1, Yp2 - 1),
                        Point(Xp2, Yp2 - 1),
                        Point(Xp, Yp1 - 1)]);  { Triangle }
        7 : begin                              { + }
              MoveTo(Xp, Yp1);
              LineTo(Xp, Yp2);
              MoveTo(Xp1, Yp);
              LineTo(Xp2, Yp);
            end;
        8 : begin                              { x }
              MoveTo(Xp1, Yp1);
              LineTo(Xp2, Yp2);
              MoveTo(Xp1, Yp2 - 1);
              LineTo(Xp2, Yp1 - 1);
            end;
        9 : begin                              { * }
              MoveTo(Xp, Yp1);
              LineTo(Xp, Yp2);
              MoveTo(Xp1, Yp);
              LineTo(Xp2, Yp);
              MoveTo(Xp1, Yp1);
              LineTo(Xp2, Yp2);
              MoveTo(Xp1, Yp2 - 1);
              LineTo(Xp2, Yp1 - 1);
            end;
        end;
  end;

  procedure PlotLine(Canvas             : TCanvas;
                     Xp1, Yp1, Xp2, Yp2 : Integer);
  { Plots a line with the current canvas settings }
  begin
    Canvas.MoveTo(Xp1, Yp1);
    Canvas.LineTo(Xp2, Yp2);
  end;

  procedure PlotPoint(Canvas     : TCanvas;
                      X, Y       : Float;
                      PointParam : TPointParam);
  var
    Xp, Yp : Integer;
    BrushStyle : TBrushStyle;
    PenColor, BrushColor : TColor;
  begin
    if XAxis.Scale = LOG_SCALE then X := Log10(X);
    if YAxis.Scale = LOG_SCALE then Y := Log10(Y);

    if not CheckPoint(X, Y, Xp, Yp) then Exit;

    with Canvas do
      begin
        { Save current settings }
        PenColor := Pen.Color;
        BrushColor := Brush.Color;
        BrushStyle := Brush.Style;

        Pen.Color := PointParam.Color;
        Brush.Color := PointParam.Color;
        if PointParam.Symbol in [0, 1, 3, 5] then
          Brush.Style := bsSolid
        else
          Brush.Style := bsClear;

        PlotSymbol(Canvas, Xp, Yp, PointParam.Symbol, PointParam.Size);

        { Restore settings }
        Pen.Color := PenColor;
        Brush.Color := BrushColor;
        Brush.Style := BrushStyle;
      end;
  end;

  procedure PlotErrorBar(Canvas       : TCanvas;
                         Y, S         : Float;
                         Ns           : Integer;
                         Xp, Yp, Size : Integer);
  { Plots an error bar with the current canvas settings }
  var
    Delta, Y1 : Float;
    Yp1 : Integer;
  begin
    Size := Size * SymbolSizeUnit;

    Delta := Ns * S;
    Y1 := Y - Delta;
    if YAxis.Scale = LOG_SCALE then Y1 := Log10(Y1);
    Yp1 := Ypixel(Y1);

    if Yp1 <= YmaxPixel then
      begin
        PlotLine(Canvas, Xp - Size, Yp1, Xp + Size + 1, Yp1);
        PlotLine(Canvas, Xp, Yp, Xp, Yp1);
      end
    else
      PlotLine(Canvas, Xp, Yp, Xp, YmaxPixel);

    Y1 := Y + Delta;
    if YAxis.Scale = LOG_SCALE then Y1 := Log10(Y1);
    Yp1 := Ypixel(Y1);

    if Yp1 >= YminPixel then
      begin
        PlotLine(Canvas, Xp - Size, Yp1, Xp + Size + 1, Yp1);
        PlotLine(Canvas, Xp, Yp, Xp, Yp1);
      end
    else
      PlotLine(Canvas, Xp, Yp, Xp, YminPixel);
  end;

  procedure GenPlotCurve(Canvas         : TCanvas;
                         X, Y, S        : PVector;
                         Ns             : Integer;
                         Lbound, Ubound : Integer;
                         CurvParam      : TCurvParam;
                         ErrorBars      : Boolean);
  { General curve plotting routine }
  var
    X1, Y1, X2, Y2 : Float;
    Xp1, Yp1, Xp2, Yp2 : Integer;
    I : Integer;
    Flag1, Flag2 : Boolean;
    PenWidth : Integer;
    PenStyle : TpenStyle;
    PenColor, BrushColor : TColor;
    BrushStyle : TBrushStyle;
  begin
    with Canvas do
      begin
        { Save current settings }
        PenColor := Pen.Color;
        PenStyle := Pen.Style;
        PenWidth := Pen.Width;
        BrushColor := Brush.Color;
        BrushStyle := Brush.Style;

        Pen.Color := CurvParam.LineParam.Color;
        Pen.Style := CurvParam.LineParam.Style;
        Pen.Width := CurvParam.LineParam.Width;
        Brush.Color := CurvParam.PointParam.Color;

        if CurvParam.PointParam.Symbol in [0, 1, 3, 5] then
          Brush.Style := bsSolid
        else
          Brush.Style := bsClear;

        { Plot first point }
        X1 := X^[Lbound]; if XAxis.Scale = LOG_SCALE then X1 := Log10(X1);
        Y1 := Y^[Lbound]; if YAxis.Scale = LOG_SCALE then Y1 := Log10(Y1);
        Flag1 := CheckPoint(X1, Y1, Xp1, Yp1);
        if Flag1 then
          begin
            PlotSymbol(Canvas, Xp1, Yp1, CurvParam.PointParam.Symbol,
                                 CurvParam.PointParam.Size);
            if ErrorBars and (S^[Lbound] > 0.0) then
              PlotErrorBar(Canvas, Y^[Lbound], S^[Lbound], Ns, Xp1, Yp1,
                                 CurvParam.PointParam.Size);
          end;

        { Plot other points and connect them by lines if necessary }
        I := Lbound + CurvParam.Step;
        while I <= Ubound do
          begin
            X2 := X^[I]; if XAxis.Scale = LOG_SCALE then X2 := Log10(X2);
            Y2 := Y^[I]; if YAxis.Scale = LOG_SCALE then Y2 := Log10(Y2);
            Flag2 := CheckPoint(X2, Y2, Xp2, Yp2);
            if Flag2 then
              begin
                PlotSymbol(Canvas, Xp2, Yp2, CurvParam.PointParam.Symbol,
                                       CurvParam.PointParam.Size);
                if ErrorBars and (S^[I] > 0.0) then
                  PlotErrorBar(Canvas, Y^[I], S^[I], Ns, Xp2, Yp2,
                                  CurvParam.PointParam.Size);
                if CurvParam.Connect and Flag1 then
                  PlotLine(Canvas, Xp1, Yp1, Xp2, Yp2);
              end;

            Xp1 := Xp2;
            Yp1 := Yp2;
            Flag1 := Flag2;
            Inc(I, CurvParam.Step);
          end;

        { Restore settings }
        Pen.Color := PenColor;
        Pen.Style := PenStyle;
        Pen.Width := PenWidth;
        Brush.Color := BrushColor;
        Brush.Style := BrushStyle;
      end;
  end;

  procedure PlotCurve(Canvas         : TCanvas;
                      X, Y           : PVector;
                      Lbound, Ubound : Integer;
                      CurvParam      : TCurvParam);
  var
    Ns : Integer;  { Dummy variables }
    S : PVector;
  begin
    GenPlotCurve(Canvas, X, Y, S, Ns, Lbound, Ubound, CurvParam, False);
  end;

  procedure PlotCurveWithErrorBars(Canvas         : TCanvas;
                                   X, Y, S        : PVector;
                                   Ns             : Integer;
                                   Lbound, Ubound : Integer;
                                   CurvParam      : TCurvParam);
  begin
    GenPlotCurve(Canvas, X, Y, S, Ns, Lbound, Ubound, CurvParam, True);
  end;

  procedure PlotFunc(Canvas     : TCanvas;
                     Func       : TFunc;
                     Xmin, Xmax : Float;
                     Npt        : Integer;
                     LineParam  : TLineParam);
  var
    PenColor : TColor;
    PenStyle : TpenStyle;
    PenWidth : Integer;
    X1, Y1, X2, Y2, H : Float;
    Xp1, Yp1, Xp2, Yp2 : Integer;
    Flag1, Flag2 : Boolean;
    I : Integer;
  begin
    if (Npt < 2) or (LineParam.Style = psClear) then Exit;

    if Xmin >= Xmax then
      begin
        Xmin := XAxis.Min;
        Xmax := XAxis.Max;
      end;

    H := (Xmax - Xmin) / Npt;

    with Canvas do
      begin
        { Save current settings }
        PenColor := Pen.Color;
        PenStyle := Pen.Style;
        PenWidth := Pen.Width;

        Pen.Color := LineParam.Color;
        Pen.Style := LineParam.Style;
        Pen.Width := LineParam.Width;

        { Check first point }
        X1 := Xmin;
        if XAxis.Scale = LIN_SCALE then
          Y1 := Func(X1)
        else
          Y1 := Func(Exp10(X1));
        if YAxis.Scale = LOG_SCALE then Y1 := Log10(Y1);
        Flag1 := CheckPoint(X1, Y1, Xp1, Yp1);

        { Check other points and plot lines if possible }
        for I := 1 to Npt do
          begin
            X2 := X1 + H;
            if XAxis.Scale = LIN_SCALE then
              Y2 := Func(X2)
            else
              Y2 := Func(Exp10(X2));
            if YAxis.Scale = LOG_SCALE then Y2 := Log10(Y2);
            Flag2 := CheckPoint(X2, Y2, Xp2, Yp2);
            if Flag1 and Flag2 then
              PlotLine(Canvas, Xp1, Yp1, Xp2, Yp2);
            X1 := X2;
            Xp1 := Xp2;
            Yp1 := Yp2;
            Flag1 := Flag2;
          end;

        { Restore settings }
        Pen.Color := PenColor;
        Pen.Style := PenStyle;
        Pen.Width := PenWidth;
      end;
  end;

  procedure WriteLegend(Canvas     : TCanvas;
                        NCurv      : Integer;
                        CurvParam  : PCurvParamArray;
                        ShowPoints,
                        ShowLines  : Boolean);

  var
    CharHeight, I, L, Lmax, N, Nmax, Xp, Xl, Y : Integer;
    PenWidth : Integer;
    PenStyle : TpenStyle;
    PenColor, BrushColor : TColor;
    BrushStyle : TBrushStyle;
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

    { Save current settings }
    with Canvas do
      begin
        PenColor := Pen.Color;
        PenStyle := Pen.Style;
        PenWidth := Pen.Width;
        BrushColor := Brush.Color;
        BrushStyle := Brush.Style;
      end;

    for I := 1 to IMin(NCurv, Nmax) do
      with Canvas do
        begin
          Pen.Color := CurvParam^[I].LineParam.Color;
          Pen.Style := CurvParam^[I].LineParam.Style;
          Pen.Width := CurvParam^[I].LineParam.Width;
          Brush.Color := CurvParam^[I].PointParam.Color;

          if CurvParam^[I].PointParam.Symbol in [0, 1, 3, 5] then
            Brush.Style := bsSolid
          else
            Brush.Style := bsClear;

          { Plot point and line }
          Y := YminPixel + I * CharHeight;
          if ShowPoints then
            PlotSymbol(Canvas, Xp, Y, CurvParam^[I].PointParam.Symbol,
                                      CurvParam^[I].PointParam.Size);
          if ShowLines then
            PlotLine(Canvas, Xp - L, Y, Xp + L, Y);

          { Write legend }
          Brush.Style := bsClear;
          Canvas.TextOut(Xl, Y - CharHeight div 2, CurvParam^[I].Legend);
        end;

    { Restore settings }
    with Canvas do
      begin
        Pen.Color := PenColor;
        Pen.Style := PenStyle;
        Pen.Width := PenWidth;
        Brush.Color := BrushColor;
        Brush.Style := BrushStyle;
      end;
  end;

end.
