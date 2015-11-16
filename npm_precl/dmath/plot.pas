{ **********************************************************************
  *                           Unit PLOT.PAS                            *
  *                            Version 1.7                             *
  *                     (c) J. Debord, June 2001                       *
  **********************************************************************
                   Plotting routines for Turbo Pascal
  ********************************************************************** }

unit Plot;

interface

uses
  Graph, FMath, Matrices, PaString;

const
  BGIPath     : String = 'C:\BP\BGI';  { Access path for graphic drivers }
  DefSymbSize : Integer = 3;           { Default symbol size }


{ ********************** Include global variables ********************** }

  {$I PLOTVAR.INC}

{ ************************** Graphic routines ************************** }

function GraphOk : Boolean;
{ ----------------------------------------------------------------------
  Initializes high resolution graphics and plots the axes
  ---------------------------------------------------------------------- }

procedure PlotGrid;
{ ----------------------------------------------------------------------
  Plots a grid on the graph
  ---------------------------------------------------------------------- }

procedure WriteLegend(NCurv : Integer);
{ ----------------------------------------------------------------------
  Writes the graph title and the legends for the plotted curves
  Input parameter : NCurv = number of curves (1 to MAXCURV)
  ---------------------------------------------------------------------- }

procedure SetClipping(Clip : Boolean);
{ ----------------------------------------------------------------------
  Determines whether drawings are clipped at the current viewport
  boundaries, according to the value of the Boolean parameter Clip
  ---------------------------------------------------------------------- }

procedure PlotPoint(Xp, Yp, Symbol, Size, Trace : Integer);
{ ----------------------------------------------------------------------
  Plots a point on the screen
  ----------------------------------------------------------------------
  Input parameters : Xp, Yp : point coordinates in pixels
                     Symbol : 0 = point (.)
                              1 = solid circle    2 = open circle
                              3 = solid square    4 = open square
                              5 = solid triangle  6 = open triangle
                              7 = plus (+)        8 = multiply (x)
                              9 = star (*)
                     Size   : symbol size
                     Trace  : type of line between points
                              0 = none
                              1 = solid
                              2 = dotted
                              3 = centered
                              4 = dashed
  ---------------------------------------------------------------------- }

procedure PlotCurve(X, Y : PVector; Lbound, Ubound, Symbol, Trace : Integer);
{ ----------------------------------------------------------------------
  Plots a curve
  ----------------------------------------------------------------------
  Input parameters : X, Y           = point coordinates
                     Lbound, Ubound = indices of first and last points
                     Symbol, Trace  = as in PlotPoint
  ---------------------------------------------------------------------- }

procedure PlotCurveWithErrorBars(X, Y, S : PVector;
                                 Lbound, Ubound, Symbol, Trace : Integer);
{ ----------------------------------------------------------------------
  Plots a curve with error bars
  ----------------------------------------------------------------------
  Input parameters : X, Y           = point coordinates
                     S              = errors (standard deviations)
                     Lbound, Ubound = indices of first and last points
                     Symbol, Trace  = as in PlotPoint
  ---------------------------------------------------------------------- }

procedure PlotFunc(Func : TFunc; X1, X2 : Float; Trace : Integer);
{ ----------------------------------------------------------------------
  Plots a function
  ----------------------------------------------------------------------
  Input parameters : Func   = function to be plotted
                     X1, X2 = abscissae of 1st and last point to plot
                     Trace  = as in PlotPoint
  ----------------------------------------------------------------------
  The function must be programmed as : function Func(X : Float) : Float;
  ---------------------------------------------------------------------- }

{ *********** The following routines are defined in PLOT.INC *********** }

procedure Interval(X1, X2 : Float; MinDiv, MaxDiv : Integer;
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

procedure AutoScale(Z : PVector; Lbound, Ubound : Integer;
                    var Axis : TAxis);
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

{ ----------------------------------------------------------------------
  Include the variables and routines common to PLOT.PAS and WINPLOT.PAS
  ---------------------------------------------------------------------- }

  {$I PLOT.INC}

{ ---------------------------------------------------------------------- }

  procedure PlotXAxis;
  var
    W, X, Z : Float;
    N, I, J : Integer;
    NSZ : Boolean;
  begin
    Line(XminPixel, YmaxPixel, XmaxPixel, YmaxPixel);
    SetTextStyle(XTitle.Font, HorizDir, 1);
    SetUserCharSize(XTitle.CharWidth, 100, XTitle.CharHeight, 100);
    SetTextJustify(CenterText, TopText);
    N := Round((XAxis.Max - XAxis.Min) / XAxis.Step); { Nb of intervals }
    X := XAxis.Min;                                   { Tick mark position }
    NSZ := NSZero;
    NSZero := False;    { Don't write non significant zero's }
    for I := 0 to N do  { Label axis }
      begin
        if (XAxis.Scale = LIN_SCALE) and (Abs(X) < EPS) then X := 0.0;
        MoveTo(Xpixel(X), YmaxPixel);
        LineRel(0, 5);                                { Plot tick mark }
        if XAxis.Scale = LIN_SCALE then Z := X else Z := Exp10(X);
        OutText(Trim(FloatToStr(Z)));
        if (XAxis.Scale = LOG_SCALE) and (I < N) then
          for J := 2 to 9 do                          { Plot minor divisions }
            begin                                     { on logarithmic scale }
              W := X + Log10(J);
              MoveTo(Xpixel(W), YmaxPixel);
              LineRel(0, 3);
            end;
        X := X + XAxis.Step;
      end;
    if XTitle.Text <> '' then                       { Plot axis title }
      OutTextXY((XminPixel + XmaxPixel) div 2,
                 YmaxPixel + GetMaxY div 12, XTitle.Text);
    NSZero := NSZ;
  end;

  procedure PlotYAxis;
  var
    W, Y, Z : Float;
    N, I, J : Integer;
    NSZ : Boolean;
  begin
    Line(XminPixel, YminPixel, XminPixel, YmaxPixel);
    SetTextStyle(YTitle.Font, HorizDir, 1);
    SetUserCharSize(YTitle.CharWidth, 100, YTitle.CharHeight, 100);
    SetTextJustify(RightText, CenterText);
    N := Round((YAxis.Max - YAxis.Min) / YAxis.Step);
    Y := YAxis.Min;
    NSZ := NSZero;
    NSZero := False;
    for I := 0 to N do
      begin
        if (YAxis.Scale = LIN_SCALE) and (Abs(Y) < EPS) then Y := 0.0;
        MoveTo(XminPixel, Ypixel(Y));
        LineRel(- 5, 0);
        MoveRel(- 2, - 2);
        if YAxis.Scale = LIN_SCALE then Z := Y else Z := Exp10(Y);
        OutText(Trim(FloatToStr(Z)));
        if (YAxis.Scale = LOG_SCALE) and (I < N) then
          for J := 2 to 9 do
            begin
              W := Y + Log10(J);
              MoveTo(XminPixel, Ypixel(W));
              LineRel(- 3, 0);
            end;
        Y := Y + YAxis.Step;
      end;
    if YTitle.Text <> '' then
      begin
        SetTextStyle(YTitle.Font, VertDir, 1);
        SetUserCharSize(YTitle.CharWidth, 100, YTitle.CharHeight, 100);
        OutTextXY(XminPixel - GetMaxX div 8,
                 (YminPixel + YmaxPixel) div 2, YTitle.Text);
      end;
    NSZero := NSZ;
  end;

  function GraphOk : Boolean;
  var
    Pilot, Mode : Integer;
  begin
    Pilot := Detect;
    InitGraph(Pilot, Mode, BGIPath);
    if GraphResult <> 0 then
      begin
        GraphOk := False;
        Exit;
      end;
    GraphOk := True;
    XminPixel := Round(Xwin1 / 100 * GetMaxX);
    YminPixel := Round(Ywin1 / 100 * GetMaxY);
    XmaxPixel := Round(Xwin2 / 100 * GetMaxX);
    YmaxPixel := Round(Ywin2 / 100 * GetMaxY);
    FactX := (XmaxPixel - XminPixel) / (XAxis.Max - XAxis.Min);
    FactY := (YmaxPixel - YminPixel) / (YAxis.Max - YAxis.Min);
    if GraphBorder then
      Rectangle(XminPixel, YminPixel, XmaxPixel, YmaxPixel);
    PlotXAxis;
    PlotYAxis;
  end;

  procedure PlotGrid;
  var
    X, Y : Float;
    I, N, Xp, Yp : Integer;
  begin
    SetLineStyle(DottedLn, 0, NormWidth);
    if Grid in [HORIZ_GRID, BOTH_GRID] then   { Horizontal lines }
      begin
        N := Round((YAxis.Max - YAxis.Min) / YAxis.Step);  { Nb of intervals }
        for I := 1 to Pred(N) do
          begin
            Y := YAxis.Min + I * YAxis.Step;  { Origin of line }
            Yp := Ypixel(Y);
            Line(XminPixel, Yp, XmaxPixel, Yp);
          end;
      end;
    if Grid in [VERTIC_GRID, BOTH_GRID] then  { Vertical lines }
      begin
        N := Round((XAxis.Max - XAxis.Min) / XAxis.Step);
        for I := 1 to Pred(N) do
          begin
            X := XAxis.Min + I * XAxis.Step;
            Xp := Xpixel(X);
            Line(Xp, YminPixel, Xp, YmaxPixel);
          end;
      end;
    SetLineStyle(SolidLn, 0, NormWidth);
  end;

  procedure PlotPoint(Xp, Yp, Symbol, Size, Trace : Integer);
  var
    Xasp, Yasp, Xp1, Xp2, Yp1, Yp2, Dx, Dy : Word;
    R : Float;
    Triangle : array[1..4] of PointType;
    Square : array[1..5] of PointType;
  begin
    if Trace = 0 then
      MoveTo(Xp, Yp)
    else
      begin
        SetLineStyle(Pred(Trace), 0, NormWidth);
        LineTo(Xp, Yp);
        SetLineStyle(0, 0, 1);
      end;
    GetAspectRatio(Xasp, Yasp);
    R := 0.0001 * Size;
    Dx := Round(R * Yasp);
    Dy := Round(R * Xasp);
    Xp1 := Xp - Size; Xp2 := Xp + Size;
    Yp1 := Yp - Size; Yp2 := Yp + Size;
    if Symbol in [3, 4] then
      begin
        Square[1].X := Xp1; Square[1].Y := Yp1;
        Square[2].X := Xp1; Square[2].Y := Yp2;
        Square[3].X := Xp2; Square[3].Y := Yp2;
        Square[4].X := Xp2; Square[4].Y := Yp1;
        Square[5].X := Xp1; Square[5].Y := Yp1;
      end;
    if Symbol in [5, 6] then
      begin
        Triangle[1].X := Xp; Triangle[1].Y := Yp1;
        Triangle[2].X := Xp2; Triangle[2].Y := Yp2;
        Triangle[3].X := Xp1; Triangle[3].Y := Yp2;
        Triangle[4].X := Xp; Triangle[4].Y := Yp1;
      end;
    case Symbol of
      0 : PutPixel(Xp, Yp, GetColor);       { ù }
      1 : PieSlice(Xp, Yp, 0, 360, Dx);     { Solid circle }
      2 : Ellipse(Xp, Yp, 0, 360, Dx, Dy);  { Open circle }
      3 : FillPoly(5, Square);              { Solid square }
      4 : DrawPoly(5, Square);              { Open square }
      5 : FillPoly(4, Triangle);            { Solid triangle }
      6 : DrawPoly(4, Triangle);            { Open triangle }
      7 : begin                             { + }
            Line(Xp, Yp1, Xp, Yp2);
            Line(Xp1, Yp, Xp2, Yp);
          end;
      8 : begin                             { x }
            Line(Xp1, Yp1, Xp2, Yp2);
            Line(Xp1, Yp2, Xp2, Yp1);
          end;
      9 : begin
            Line(Xp, Yp1, Xp, Yp2);         { * }
            Line(Xp1, Yp, Xp2, Yp);
            Line(Xp1, Yp1, Xp2, Yp2);
            Line(Xp1, Yp2, Xp2, Yp1);
          end;
    end;
  end;

  procedure WriteLegend(NCurv : Integer);
  var
    I, Xp, Yp, Dy : Integer;
  begin
    with GraphTitle do
      if Text <> '' then
        begin
          SetTextStyle(Font, HorizDir, 1);
          SetUserCharSize(CharWidth, 100, CharHeight, 100);
          SetTextJustify(CenterText, TopText);
          OutTextXY((XminPixel + XmaxPixel) div 2,
                    YminPixel - GetMaxY div 10, Text);
        end;
    with Legend do
      begin
        SetTextStyle(Font, HorizDir, 1);
        SetUserCharSize(CharWidth, 100, CharHeight, 100);
        SetTextJustify(LeftText, CenterText);
        Dy := (YmaxPixel - YminPixel) div 10;
        Xp := XmaxPixel + 30;
        Yp := YminPixel + Dy;
        for I := 1 to NCurv do
          if Text[I] <> '' then
            begin
              PlotPoint(Xp, Yp, I, SymbolSize, 0);
              OutTextXY(Xp + 20, Yp, Text[I]);
              Yp := Yp + Dy;
            end;
      end;
  end;

  procedure SetClipping(Clip : Boolean);
  begin
    if XminPixel = 0 then
      begin
        XminPixel := Round(Xwin1 / 100 * GetMaxX);
        YminPixel := Round(Ywin1 / 100 * GetMaxY);
        XmaxPixel := Round(Xwin2 / 100 * GetMaxX);
        YmaxPixel := Round(Ywin2 / 100 * GetMaxY);
      end;
    SetViewPort(XminPixel, YminPixel, XmaxPixel, YmaxPixel, Clip);
    XmaxPixel := XmaxPixel - XminPixel; XminPixel := 0;
    YmaxPixel := YmaxPixel - YminPixel; YminPixel := 0;
  end;

  procedure PlotCurve(X, Y : PVector;
                      Lbound, Ubound, Symbol, Trace : Integer);
  var
    XI, YI : Float;
    I, NL : Integer;
  begin
    NL := 0;
    for I := Lbound to Ubound do
      begin
        XI := X^[I];
        if XAxis.Scale = LOG_SCALE then XI := Log10(XI);
        YI := Y^[I];
        if YAxis.Scale = LOG_SCALE then YI := Log10(YI);
        PlotPoint(Xpixel(XI), Ypixel(YI), Symbol, DefSymbSize, NL);
        NL := Trace;
      end;
  end;

  procedure PlotCurveWithErrorBars(X, Y, S : PVector;
                                   Lbound, Ubound, Symbol, Trace : Integer);
  var
    XI, YI, Y1, Y2 : Float;
    I, NL, Xp, Yp, Yp1, Yp2 : Integer;
  begin
    NL := 0;
    for I := Lbound to Ubound do
      begin
        XI := X^[I];
        if XAxis.Scale = LOG_SCALE then XI := Log10(XI);
        YI := Y^[I];
        if YAxis.Scale = LOG_SCALE then YI := Log10(YI);
        Xp := Xpixel(XI); Yp := Ypixel(YI);
        PlotPoint(Xp, Yp, Symbol, DefSymbSize, NL);
        if S^[I] > 0 then
          begin
            Y1 := Y^[I] - S^[I];
            if YAxis.Scale = LOG_SCALE then Y1 := Log10(Y1);
            Y2 := Y^[I] + S^[I];
            if YAxis.Scale = LOG_SCALE then Y2 := Log10(Y2);
            Yp1 := Ypixel(Y1); Yp2 := Ypixel(Y2);
            Line(Xp - 5, Yp1, Xp + 5, Yp1);
            Line(Xp - 5, Yp2, Xp + 5, Yp2);
            Line(Xp, Yp1, Xp, Yp2);
          end;
        NL := Trace;
      end;
  end;

  procedure PlotFunc(Func : TFunc; X1, X2 : Float; Trace : Integer);
  var
    X, Y, H : Float;
    I, Npt, NL, Xp, Yp : Integer;
  begin
    NL := 0;  { Indicates if a line must be drawn from the previous point }
    X := X1;

    { Nb of points to be plotted = number of pixels between X1 and X2 }
    Npt := Xpixel(X2) - Xpixel(X1);

    H := (X2 - X1) / Npt;
    for I := 0 to Npt do
      begin
        X := X1 + I * H;
        if XAxis.Scale = LIN_SCALE then
          Y := Func(X)
        else
          Y := Func(Exp10(X));
        if MathError <> FN_OK then
          NL := 0
        else
          begin
            if YAxis.Scale = LOG_SCALE then Y := Log10(Y);
            Xp := Xpixel(X);
            Yp := Ypixel(Y);
            PlotPoint(Xp, Yp, 0, 0, NL);
            NL := Trace;
          end;
      end;
  end;

end.
