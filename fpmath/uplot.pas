{ ******************************************************************
  Plotting routines for TP/FPC/GPC (based on the Graph unit)
  ****************************************************************** }

unit uplot;

interface

uses
  graph, utypes, umath, uround, uinterv, ustrings;

function InitGraphics(Pilot, Mode : Integer; BGIPath : String) : Boolean;
{ ------------------------------------------------------------------
  Enters graphic mode
  ------------------------------------------------------------------ }

procedure SetWindow(X1, X2, Y1, Y2 : Integer; GraphBorder : Boolean);
{ ------------------------------------------------------------------
  Sets the graphic window

  X1, X2, Y1, Y2 : Window coordinates in % of maximum
  GraphBorder    : Flag for drawing the window border
  ------------------------------------------------------------------ }

procedure AutoScale(X : PVector; Lb, Ub : Integer; Scale : TScale;
                    var XMin, XMax, XStep : Float);
{ ------------------------------------------------------------------
  Finds an appropriate scale for plotting the data in X[Lb..Ub]
  ------------------------------------------------------------------ }

procedure SetOxScale(Scale : TScale; OxMin, OxMax, OxStep : Float);
{ ------------------------------------------------------------------
  Sets the scale on the Ox axis
  ------------------------------------------------------------------ }

procedure SetOyScale(Scale : TScale; OyMin, OyMax, OyStep : Float);
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

procedure SetTitleFont(FontIndex, Width, Height : Integer);
{ ------------------------------------------------------------------
  Sets the font for the main graph title
  ------------------------------------------------------------------ }

procedure SetOxFont(FontIndex, Width, Height : Integer);
{ ------------------------------------------------------------------
  Sets the font for the Ox axis (title and labels)
  ------------------------------------------------------------------ }

procedure SetOyFont(FontIndex, Width, Height : Integer);
{ ------------------------------------------------------------------
  Sets the font for the Oy axis (title and labels)
  ------------------------------------------------------------------ }

procedure SetLgdFont(FontIndex, Width, Height : Integer);
{ ------------------------------------------------------------------
  Sets the font for the legends
  ------------------------------------------------------------------ }

procedure PlotOxAxis;
{ ------------------------------------------------------------------
  Plots the horizontal axis
  ------------------------------------------------------------------ }

procedure PlotOyAxis;
{ ------------------------------------------------------------------
  Plots the vertical axis
  ------------------------------------------------------------------ }

procedure PlotGrid(Grid : TGrid);
{ ------------------------------------------------------------------
  Plots a grid on the graph
  ------------------------------------------------------------------ }

procedure WriteGraphTitle;
{ ------------------------------------------------------------------
  Writes the title of the graph
  ------------------------------------------------------------------ }

procedure SetClipping(Clip : Boolean);
{ ------------------------------------------------------------------
  Determines whether drawings are clipped at the current viewport
  boundaries, according to the value of the Boolean parameter Clip
  ------------------------------------------------------------------ }

procedure SetMaxCurv(NCurv : Byte);
{ ------------------------------------------------------------------
  Sets the maximum number of curves and re-initializes their
  parameters
  ------------------------------------------------------------------ }

procedure SetPointParam(CurvIndex, Symbol, Size, Color : Integer);
{ ------------------------------------------------------------------
  Sets the point parameters for curve # CurvIndex
  ------------------------------------------------------------------ }

procedure SetLineParam(CurvIndex, Style, Width, Color : Integer);
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

procedure PlotPoint(Xp, Yp, CurvIndex : Integer);
{ ------------------------------------------------------------------
  Plots a point on the screen
  ------------------------------------------------------------------
  Input parameters : Xp, Yp    = point coordinates in pixels
                     CurvIndex = index of curve parameters
                                  (Symbol, Size, Color)
  ------------------------------------------------------------------ }

procedure PlotCurve(X, Y : PVector; Lb, Ub, CurvIndex : Integer);
{ ------------------------------------------------------------------
  Plots a curve
  ------------------------------------------------------------------
  Input parameters : X, Y      = point coordinates
                     Lb, Ub    = indices of first and last points
                     CurvIndex = index of curve parameters
  ------------------------------------------------------------------ }

procedure PlotCurveWithErrorBars(X, Y, S : PVector;
                                 Ns, Lb, Ub, CurvIndex : Integer);
{ ------------------------------------------------------------------
  Plots a curve with error bars
  ------------------------------------------------------------------
  Input parameters : X, Y      = point coordinates
                     S         = errors
                     Lb, Ub    = indices of first and last points
                     CurvIndex = index of curve parameters
  ------------------------------------------------------------------ }

procedure PlotFunc(Func : TFunc; X1, X2 : Float; CurvIndex : Integer);
{ ------------------------------------------------------------------
  Plots a function
  ------------------------------------------------------------------
  Input parameters:
    Func      = function to be plotted
    X1, X2    = abscissae of 1st and last point to plot
    CurvIndex = index of curve parameters (Width, Style, Color)
  ------------------------------------------------------------------
  The function must be programmed as :
  function Func(X : Float) : Float;
  ------------------------------------------------------------------ }

procedure WriteLegend(NCurv : Integer; ShowPoints, ShowLines : Boolean);
{ ------------------------------------------------------------------
  Writes the legends for the plotted curves
  ------------------------------------------------------------------
  NCurv      : number of curves (1 to MaxCurv)
  ShowPoints : for displaying points
  ShowLines  : for displaying lines
  ------------------------------------------------------------------ }

procedure ConRec(Nx, Ny, Nc : Integer;
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
  MaxSymbol    = 9;        { Max. number of symbols for plotting curves }
  MaxCurvColor = 9;        { Max. number of colors for curves }
  Eps          = 1.0E-10;  { Lower limit for an axis label }

  CurvColor : array[1..MaxCurvColor] of Integer =
    (12, { LightRed }
     14, { Yellow }
     10, { LightGreen }
      9, { LightBlue }
     11, { LightCyan }
     13, { LightMagenta }
      4, { Red }
      2, { Green }
      1  { Blue });

type
  TAxis = record        { Coordinate axis }
    Scale : TScale;
    Min   : Float;
    Max   : Float;
    Step  : Float;
  end;

  TFont = record        { Font for titles and legends }
    Index  : Integer;
    Width  : Integer;
    Height : Integer;
  end;

  TPointParam = record  { Point parameters                            }
    Symbol : Integer;   { Symbol: 0: point (.)                        }
    Size   : Integer;   {         1: solid circle    2: open circle   }
    Color  : Integer;   {         3: solid square    4: open square   }
  end;                  {         5: solid triangle  6: open triangle }
                        {         7: plus (+)        8: multiply (x)  }
                        {         9: star (* )                        }

  TLineParam = record   { Line parameters }
    Style : Integer;    { 0: none, 1: solid, 2: dotted, 3: centered, 4: dashed }
    Width : Integer;    { 1: normal, 3: thick }
    Color : Integer;
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
  Xwin1, Xwin2, Ywin1, Ywin2       : Integer;
  XminPixel, XmaxPixel             : Integer;
  YminPixel, YmaxPixel             : Integer;
  FactX, FactY                     : Float;
  XAxis, YAxis                     : TAxis;
  GraphTitle, XTitle, YTitle       : String;
  TitleFont, XFont, YFont, LgdFont : TFont;
  MaxCurv                          : Integer;
  CurvParam                        : PCurvParamVector;

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
        LineParam.Style := 1;
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

function InitGraphics(Pilot, Mode : Integer; BGIPath : String) : Boolean;
var
  P, M : {$IFDEF FPC}Smallint{$ELSE}Integer{$ENDIF};
begin
  P := Pilot;
  M := Mode;

  InitGraph(P, M, BGIPath);

  if GraphResult <> 0 then
    begin
      InitGraphics := False;
      Exit;
    end;

  InitGraphics := True;

  MaxCurv := MaxSymbol;
  DimCurvParamVector(CurvParam, MaxCurv);

  { Obtain info about current mode }
  XminPixel := 0; XmaxPixel := GetMaxX;
  YminPixel := 0; YmaxPixel := GetMaxY;
end;

procedure SetWindow(X1, X2, Y1, Y2 : Integer; GraphBorder : Boolean);
var
  R : Float;
begin
  if (X1 >= 0) and (X2 <= 100) and (X1 < X2) then
    begin
      Xwin1 := X1;
      Xwin2 := X2;
      R := 0.01 * GetMaxX;
      XminPixel := Round(X1 * R);
      XmaxPixel := Round(X2 * R);
    end;

  if (Y1 >= 0) and (Y2 <= 100) and (Y1 < Y2) then
    begin
      Ywin1 := Y1;
      Ywin2 := Y2;
      R := 0.01 * GetMaxY;
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

  XTitle       := 'X';
  XFont.Index  := 2;
  XFont.Width  := 150;
  XFont.Height := 150;

  YTitle       := 'Y';
  YFont.Index  := 2;
  YFont.Width  := 150;
  YFont.Height := 150;

  GraphTitle       := '';
  TitleFont.Index  := 2;
  TitleFont.Width  := 175;
  TitleFont.Height := 175;

  LgdFont.Index  := 2;
  LgdFont.Width  := 150;
  LgdFont.Height := 150;

  if GraphBorder then
    Rectangle(XminPixel, YminPixel, XmaxPixel, YmaxPixel);
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

procedure SetFont(Select, Index, Width, Height : Integer);
var
  Font : TFont;
begin
  if Index in [0..10] then Font.Index := Index;
  if Width > 0 then Font.Width := Width;
  if Height > 0 then Font.Height := Height;

  case Select of
    0 : TitleFont := Font;
    1 : XFont := Font;
    2 : YFont := Font;
    3 : LgdFont := Font;
  end;
end;

procedure SetTitleFont(FontIndex, Width, Height : Integer);
begin
  SetFont(0, FontIndex, Width, Height);
end;

procedure SetOxFont(FontIndex, Width, Height : Integer);
begin
  SetFont(1, FontIndex, Width, Height);
end;

procedure SetOyFont(FontIndex, Width, Height : Integer);
begin
  SetFont(2, FontIndex, Width, Height);
end;

procedure SetLgdFont(FontIndex, Width, Height : Integer);
begin
  SetFont(3, FontIndex, Width, Height);
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

procedure PlotOxAxis;
var
  W, X, Z : Float;
  N, I, J : Integer;
begin
  Line(XminPixel, YmaxPixel, XmaxPixel, YmaxPixel);

  SetTextStyle(XFont.Index, HorizDir, 1);
  SetUserCharSize(XFont.Width, 100, XFont.Height, 100);
  SetTextJustify(CenterText, TopText);

  N := Round((XAxis.Max - XAxis.Min) / XAxis.Step); { Nb of intervals }
  X := XAxis.Min;                                   { Tick mark position }

  for I := 0 to N do  { Label axis }
    begin
      if (XAxis.Scale = LinScale) and (Abs(X) < Eps) then X := 0.0;

      MoveTo(Xpixel(X), YmaxPixel);
      LineRel(0, 5);                                { Plot tick mark }

      if XAxis.Scale = LinScale then Z := X else Z := Exp10(X);
      OutText(Trim(FloatStr(Z)));

      if (XAxis.Scale = LogScale) and (I < N) then
        for J := 2 to 9 do                          { Plot minor divisions }
          begin                                     { on logarithmic scale }
            W := X + Log10(J);
            MoveTo(Xpixel(W), YmaxPixel);
            LineRel(0, 3);
          end;

      X := X + XAxis.Step;
    end;

  if XTitle <> '' then                       { Plot axis title }
    OutTextXY((XminPixel + XmaxPixel) div 2,
               YmaxPixel + GetMaxY div 12, XTitle);
end;

procedure PlotOyAxis;
var
  W, Y, Z : Float;
  N, I, J : Integer;
begin
  Line(XminPixel, YminPixel, XminPixel, YmaxPixel);

  SetTextStyle(YFont.Index, HorizDir, 1);
  SetUserCharSize(YFont.Width, 100, YFont.Height, 100);
  SetTextJustify(RightText, CenterText);

  N := Round((YAxis.Max - YAxis.Min) / YAxis.Step);
  Y := YAxis.Min;

  for I := 0 to N do
    begin
      if (YAxis.Scale = LinScale) and (Abs(Y) < Eps) then Y := 0.0;

      MoveTo(XminPixel, Ypixel(Y));
      LineRel(- 5, 0);
      MoveRel(- 2, - 2);

      if YAxis.Scale = LinScale then Z := Y else Z := Exp10(Y);
      OutText(Trim(FloatStr(Z)));

      if (YAxis.Scale = LogScale) and (I < N) then
        for J := 2 to 9 do
          begin
            W := Y + Log10(J);
            MoveTo(XminPixel, Ypixel(W));
            LineRel(- 3, 0);
          end;

      Y := Y + YAxis.Step;
    end;

  if YTitle <> '' then
    begin
      SetTextStyle(YFont.Index, VertDir, 1);
      SetUserCharSize(YFont.Width, 100, YFont.Height, 100);
      OutTextXY(XminPixel - GetMaxX div 8,
               (YminPixel + YmaxPixel) div 2, YTitle);
    end;
end;

procedure PlotGrid(Grid : TGrid);
var
  X, Y         : Float;
  I, N, Xp, Yp : Integer;
begin
  SetLineStyle(DottedLn, 0, NormWidth);

  if Grid in [HorizGrid, BothGrid] then  { Horizontal lines }
    begin
      N := Round((YAxis.Max - YAxis.Min) / YAxis.Step);  { Nb of intervals }
      for I := 1 to Pred(N) do
        begin
          Y := YAxis.Min + I * YAxis.Step;  { Origin of line }
          Yp := Ypixel(Y);
          Line(XminPixel, Yp, XmaxPixel, Yp);
        end;
    end;

  if Grid in [VertiGrid, BothGrid] then  { Vertical lines }
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

procedure WriteGraphTitle;
begin
  if GraphTitle = '' then Exit;

  SetTextStyle(TitleFont.Index, HorizDir, 1);
  SetUserCharSize(TitleFont.Width, 100, TitleFont.Height, 100);
  SetTextJustify(CenterText, TopText);

  OutTextXY((XminPixel + XmaxPixel) div 2,
             YminPixel - GetMaxY div 10,
             GraphTitle);
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

procedure SetMaxCurv(NCurv : Byte);
begin
  if NCurv < 1 then Exit;
  DelCurvParamVector(CurvParam, MaxCurv);
  MaxCurv := NCurv;
  DimCurvParamVector(CurvParam, MaxCurv);
end;

procedure SetPointParam(CurvIndex, Symbol, Size, Color : Integer);
begin
  if (CurvIndex < 1) or (CurvIndex > MaxCurv) then Exit;

  if (Symbol >= 0) and (Symbol <= MaxSymbol) then
    CurvParam^[CurvIndex].PointParam.Symbol := Symbol;

  if Size > 0 then
    CurvParam^[CurvIndex].PointParam.Size := Size;

  if (Color >= 0) and (Color <= GetMaxColor) then
    CurvParam^[CurvIndex].PointParam.Color := Color;
end;

procedure SetLineParam(CurvIndex, Style, Width, Color : Integer);
begin
  if (CurvIndex < 1) or (CurvIndex > MaxCurv) then Exit;

  if (Style >= 0) and (Style <= 4) then
    CurvParam^[CurvIndex].LineParam.Style := Style;

  if (Width = 1) or (Width = 3) then
    CurvParam^[CurvIndex].LineParam.Width := Width;

  if (Color >= 0) and (Color <= GetMaxColor) then
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

procedure PlotPoint(Xp, Yp, CurvIndex : Integer);
var
  Xasp, Yasp : {$IFDEF __GPC__}Integer{$ELSE}Word{$ENDIF};
  Size       : Integer;
  Xp1, Xp2   : Word;
  Yp1, Yp2   : Word;
  Dx, Dy     : Word;
  R          : Float;
  Triangle   : array[1..4] of PointType;
  Square     : array[1..5] of PointType;
begin
  GetAspectRatio(Xasp, Yasp);

  Size := CurvParam^[CurvIndex].PointParam.Size;

  R := 0.0001 * Size;

  Dx := Round(R * Yasp);
  Dy := Round(R * Xasp);

  Xp1 := Xp - Size;
  Xp2 := Xp + Size;
  Yp1 := Yp - Size;
  Yp2 := Yp + Size;

  if CurvParam^[CurvIndex].PointParam.Symbol in [3, 4] then
    begin
      Square[1].X := Xp1; Square[1].Y := Yp1;
      Square[2].X := Xp1; Square[2].Y := Yp2;
      Square[3].X := Xp2; Square[3].Y := Yp2;
      Square[4].X := Xp2; Square[4].Y := Yp1;
      Square[5].X := Xp1; Square[5].Y := Yp1;
    end;

  if CurvParam^[CurvIndex].PointParam.Symbol in [5, 6] then
    begin
      Triangle[1].X := Xp;  Triangle[1].Y := Yp1;
      Triangle[2].X := Xp2; Triangle[2].Y := Yp2;
      Triangle[3].X := Xp1; Triangle[3].Y := Yp2;
      Triangle[4].X := Xp;  Triangle[4].Y := Yp1;
    end;

  MoveTo(Xp, Yp);
  SetColor(CurvParam^[CurvIndex].PointParam.Color);
  SetFillStyle(SolidFill, CurvParam^[CurvIndex].PointParam.Color);

  case CurvParam^[CurvIndex].PointParam.Symbol of
    0 : PutPixel(Xp, Yp, GetColor);       { . }
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

procedure PlotCurve(X, Y : PVector; Lb, Ub, CurvIndex : Integer);
var
  XI, YI    : Float;
  I, Xp, Yp : Integer;
  Connect   : Boolean;
begin
  Connect := CurvParam^[CurvIndex].LineParam.Style > 0;

  if Connect then
    SetLineStyle(Pred(CurvParam^[CurvIndex].LineParam.Style),
                 0, CurvParam^[CurvIndex].LineParam.Width);

  I := Lb;

  repeat
    XI := X^[I]; if XAxis.Scale = LogScale then XI := Log10(XI);
    YI := Y^[I]; if YAxis.Scale = LogScale then YI := Log10(YI);

    Xp := Xpixel(XI);
    Yp := Ypixel(YI);

    if Connect then
      begin
        SetColor(CurvParam^[CurvIndex].LineParam.Color);
        if I = Lb then MoveTo(Xp, Yp) else LineTo(Xp, Yp);
      end;

    PlotPoint(Xp, Yp, CurvIndex);

    I := I + CurvParam^[CurvIndex].Step;
  until I > Ub;
end;

procedure PlotCurveWithErrorBars(X, Y, S : PVector;
                                 Ns, Lb, Ub, CurvIndex : Integer);
var
  Delta, XI, YI, Y1, Y2 : Float;
  I, Xp, Yp, Yp1, Yp2   : Integer;
  Connect               : Boolean;
begin
  Connect := CurvParam^[CurvIndex].LineParam.Style > 0;

  SetColor(CurvParam^[CurvIndex].LineParam.Color);

  I := Lb;

  repeat
    XI := X^[I]; if XAxis.Scale = LogScale then XI := Log10(XI);
    YI := Y^[I]; if YAxis.Scale = LogScale then YI := Log10(YI);

    Xp := Xpixel(XI); Yp := Ypixel(YI);

    PlotPoint(Xp, Yp, CurvIndex);

    if S^[I] > 0 then
      begin
        Delta := Ns * S^[I];

        Y1 := Y^[I] - Delta; if YAxis.Scale = LogScale then Y1 := Log10(Y1);
        Y2 := Y^[I] + Delta; if YAxis.Scale = LogScale then Y2 := Log10(Y2);

        Yp1 := Ypixel(Y1);
        Yp2 := Ypixel(Y2);

        SetColor(CurvParam^[CurvIndex].LineParam.Color);
        SetLineStyle(SolidLn, 0, CurvParam^[CurvIndex].LineParam.Width);

        Line(Xp - 5, Yp1, Xp + 5, Yp1);
        Line(Xp - 5, Yp2, Xp + 5, Yp2);
        Line(Xp, Yp1, Xp, Yp2);
      end;

    if Connect then
      begin
        SetLineStyle(Pred(CurvParam^[CurvIndex].LineParam.Style),
                     0, CurvParam^[CurvIndex].LineParam.Width);
        if I = Lb then MoveTo(Xp, Yp) else LineTo(Xp, Yp);
      end;

    I := I + CurvParam^[CurvIndex].Step;

  until I > Ub;
end;

procedure PlotFunc(Func : TFunc; X1, X2 : Float; CurvIndex : Integer);
var
  X, Y, H        : Float;
  I, Npt, Xp, Yp : Integer;
begin
    if X1 >= X2 then Exit;

  if XAxis.Scale = LogScale then
    begin
      X1 := Log10(X1);
      X2 := Log10(X2);
    end;

  SetColor(CurvParam^[CurvIndex].LineParam.Color);
  SetLineStyle(Pred(CurvParam^[CurvIndex].LineParam.Style), 0,
               CurvParam^[CurvIndex].LineParam.Width);

  { Nb of points to be plotted = number of pixels between X1 and X2 }
  Npt := Xpixel(X2) - Xpixel(X1);

  H := (X2 - X1) / Npt;

  X := X1;
  for I := 0 to Npt do
    begin
      if XAxis.Scale = LinScale then Y := Func(X) else Y := Func(Exp10(X));

      if MathErr = FOk then
        begin
          if YAxis.Scale = LogScale then Y := Log10(Y);
          Xp := Xpixel(X);
          Yp := Ypixel(Y);
          if I = 0 then MoveTo(Xp, Yp) else LineTo(Xp, Yp);
        end;

      X := X + H;
    end;
end;

procedure WriteLegend(NCurv : Integer; ShowPoints, ShowLines  : Boolean);

var
  CharHeight, I, L, Lmax : Integer;
  N, Nmax, Xp, Xl, Yp    : Integer;
begin
  SetTextStyle(LgdFont.Index, HorizDir, 1);
  SetUserCharSize(LgdFont.Width, 100, LgdFont.Height, 100);
  SetTextJustify(LeftText, CenterText);

  N := 0;     { Nb of legends to be plotted  }
  Lmax := 0;  { Length of the longest legend }

  for I := 1 to NCurv do
    if CurvParam^[I].Legend <> '' then
      begin
        Inc(N);
        L := TextWidth(CurvParam^[I].Legend);
        if L > Lmax then Lmax := L;
      end;

  if (N = 0) or (Lmax = 0) then Exit;

  { Character height }
  CharHeight := TextHeight('M') + 3;

  { Max. number of legends which may be plotted }
  Nmax := Round((YmaxPixel - YminPixel) / CharHeight) - 1;
  if N > Nmax then N := Nmax;

  { Draw rectangle around the legends }
  Rectangle(XmaxPixel + Round(0.02 * GetMaxX), YminPixel,
            XmaxPixel + Round(0.12 * GetMaxX) + Lmax,
            YminPixel + (N + 1) * CharHeight);

  L := Round(0.02 * GetMaxX);  { Half-length of line }
  Xp := XmaxPixel + 3 * L;     { Position of symbol  }
  Xl := XmaxPixel + 5 * L;     { Position of legend  }

  if NCurv <= Nmax then N := NCurv else N := Nmax;

  for I := 1 to N do
    begin
      Yp := YminPixel + I * CharHeight;

      if ShowLines and (CurvParam^[I].LineParam.Style > 0) then
        begin
          SetLineStyle(Pred(CurvParam^[I].LineParam.Style),
                       0, CurvParam^[I].LineParam.Width);
          SetColor(CurvParam^[I].LineParam.Color);
          Line(Xp - L, Yp, Xp + L, Yp);
        end;

      if ShowPoints then
        PlotPoint(Xp, Yp, I);

      OutTextXY(Xl, Yp, CurvParam^[I].Legend);
    end;
end;

procedure ConRec(Nx, Ny, Nc : Integer;
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

          SetColor(CurvParam^[K mod MaxCurv + 1].LineParam.Color);
          Line(Trunc(X1), Trunc(Y1), Trunc(X2), Trunc(Y2));
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
  CloseGraph;
end;

end.
