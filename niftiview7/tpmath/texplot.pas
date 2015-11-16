{ **********************************************************************
  *                          Unit TEXPLOT.PAS                          *
  *                            Version 1.1                             *
  *                      (c) J. Debord, June 2001                      *
  **********************************************************************
                    Plotting routines for TeX/PSTricks
  ********************************************************************** }

unit TexPlot;

interface

uses
  FMath, Matrices, PaString;

{ ********************** Include global variables ********************** }

  {$I PLOTVAR.INC}

{ ************************** Graphic routines ************************** }

procedure InitTexGraph(var F : Text; FileName : String);
{ ----------------------------------------------------------------------
  Initializes TeX graphics.
  Writes a border around the graph according to the value
  of the global variable GraphBorder (defined in PLOTVAR.INC)
  ----------------------------------------------------------------------
  F        : file to be written
  FileName : name of TeX file (e.g. 'figure.tex')
  ---------------------------------------------------------------------- }

function Xcm(X : Float) : Float;
{ ----------------------------------------------------------------------
  Converts user coordinate X to cm
  ---------------------------------------------------------------------- }

function Ycm(Y : Float) : Float;
{ ----------------------------------------------------------------------
  Converts user coordinate Y to cm
  ---------------------------------------------------------------------- }

procedure WriteXAxis(var F : Text);
{ ----------------------------------------------------------------------
  Writes horizontal axis (global variable XAxis in PLOTVAR.INC)
  ---------------------------------------------------------------------- }

procedure WriteYAxis(var F : Text);
{ ----------------------------------------------------------------------
  Writes vertical axis (global variable YAxis in PLOTVAR.INC)
  ---------------------------------------------------------------------- }

procedure WriteGrid(var F : Text);
{ ----------------------------------------------------------------------
  Writes a grid (global variable Grid in PLOTVAR.INC)
  ---------------------------------------------------------------------- }

procedure WriteLine(var F : Text; X1, Y1, X2, Y2 : Float; Style : String);
{ ----------------------------------------------------------------------
  Writes a line between two points
  ----------------------------------------------------------------------
  F      : output file

  X1, Y1 : coordinates of first point

  X2, Y2 : coordinates of second point

  Style  : line style (must be 'solid', 'dotted' or 'dashed')
  ---------------------------------------------------------------------- }

procedure WritePoints(var F : Text; X, Y : PVector;
                      Lbound, Ubound, Symbol, Size : Integer);
{ ----------------------------------------------------------------------
  Writes a set of points
  ----------------------------------------------------------------------
  F              : output file

  X, Y           : point coordinates

  Lbound, Ubound : indices of first and last point

  Symbol         : 1 = solid circle    2 = open circle
                   3 = solid square    4 = open square
                   5 = solid triangle  6 = open triangle
                   7 = plus (+)        8 = multiply (x)
                   9 = star (*)

  Size           : size of points
  ---------------------------------------------------------------------- }

procedure WriteText(var F : Text; Place : String; X, Y : Float; S : String);
{ ----------------------------------------------------------------------
  Writes a text
  ----------------------------------------------------------------------
  F     : output file

  Place : defines the position of point (X,Y) with respect
          to the box enclosing the text

          the possible values are
          'tl', 't', 'tr', 'l', 'r', 'Bl', 'B', 'Br', 'bl', 'b', 'br'
          according to the following scheme:

                             t
               tl +---------------------+ tr
                  |                     |
                  |                     |
                l |                     | r
                  |                     |
               Bl |----------B----------| Br
               bl +---------------------+ br
                             b

  X, Y  : position of text

  S     : text to be written
  ---------------------------------------------------------------------- }

procedure WriteNumber(var F : Text; Place : String; X, Y, Z : Float);
{ ----------------------------------------------------------------------
  Writes a number
  ----------------------------------------------------------------------
  Z is the number to be written
  Other parameters as in WriteText
  ---------------------------------------------------------------------- }

procedure WriteCurve(var F : Text; X, Y : PVector;
                     Lbound, Ubound, Width : Integer;
                     Style : String; Smooth : Boolean);
{ ----------------------------------------------------------------------
  Writes a curve
  ----------------------------------------------------------------------
  F              : output file

  X, Y           : point coordinates

  Lbound, Ubound : indices of first and last point

  Width          : curve width in units of 0.01 cm

  Style          : curve style (must be 'solid', 'dotted' or 'dashed')

  Smooth         : indicates if the curve must be smoothed
  ---------------------------------------------------------------------- }

procedure WriteFunc(var F : Text; Func : TFunc; X1, X2 : Float;
                    Npt, Width : Integer; Style : String);
{ ----------------------------------------------------------------------
  Writes the curve representing a function
  ----------------------------------------------------------------------
  F            : output file

  Func         : function to be plotted

  X1, X2       : abscissae of 1st and last point to plot

  Npt          : number of points

  Width, Style : width of curve (as in WriteCurve)
  ----------------------------------------------------------------------
  The function must be programmed as: function Func(X : Float) : Float;
  ---------------------------------------------------------------------- }

procedure CloseTexGraph(var F : Text);
{ ----------------------------------------------------------------------
  Close graphics
  ---------------------------------------------------------------------- }

implementation

const
  PAGEWIDTH  = 13;         { Graph width in cm  }
  PAGEHEIGHT = 10;         { Graph height in cm }

var
  XminCm, YminCm : Float;  { Coord. of lower left corner in cm }
  XmaxCm, YmaxCm : Float;  { Coord. of upper right corner in cm }
  FactX, FactY   : Float;  { Scaling factors }

  function Xcm(X : Float) : Float;
  { Converts user coordinate X to cm }
  begin
    Xcm := XminCm + FactX * (X - XAxis.Min);
  end;

  function Ycm(Y : Float) : Float;
  { Converts user coordinate Y to cm }
  begin
    Ycm := YminCm + FactY * (Y - YAxis.Min);
  end;

  procedure WriteHeader(var F : Text);
  begin
    WriteLn(F, '\documentclass[12pt,a4paper]{article}');
    WriteLn(F, '\usepackage{t1enc}');
    WriteLn(F, '\usepackage{pst-plot}');
    WriteLn(F, '\begin{document}');
    WriteLn(F);
    WriteLn(F, '\begin{pspicture}(', PAGEWIDTH, ',', PAGEHEIGHT, ')');
  end;

  procedure WriteCoord(var F : Text; X, Y : Float);
  { Writes the coordinates (in cm) of a point }
  var
    NSZ : Boolean;
  begin
    NSZ := NSZEro;
    NSZero := False;
    Write(F, '(', Trim(FloatToStr(X)), ',', Trim(FloatToStr(Y)), ')');
    NSZEro := NSZ;
  end;

  procedure WriteLine(var F : Text; X1, Y1, X2, Y2 : Float; Style : String);
  begin
    Write(F, '\psline');
    if Style <> '' then
      Write(F, '[linestyle=', Style, ']');
    WriteCoord(F, X1, Y1);
    WriteCoord(F, X2, Y2);
    WriteLn(F);
  end;

  procedure WriteText(var F : Text; Place : String; X, Y : Float; S : String);
  begin
    Write(F, '\rput[', Place, ']');
    WriteCoord(F, X, Y);
    WriteLn(F, '{', S, '}');
  end;

  procedure WriteNumber(var F : Text; Place : String; X, Y, Z : Float);
  begin
    Write(F, '\rput[', Place, ']');
    WriteCoord(F, X, Y);
    WriteLn(F, '{', Trim(FloatToStr(Z)), '}');
  end;

  procedure WriteXAxis(var F: Text);
  var
    W, X, Xc, Z : Float;
    N, I, J     : Integer;
    NSZ         : Boolean;
  begin
    WriteLine(F, XminCm, YminCm, XmaxCm, YminCm, '');

    N := Round((XAxis.Max - XAxis.Min) / XAxis.Step); { Nb of intervals }
    X := XAxis.Min;                                   { Tick mark position }

    NSZ := NSZero;
    NSZero := False;    { Don't write non significant zero's }

    for I := 0 to N do  { Label axis }
      begin
        if (XAxis.Scale = LIN_SCALE) and (Abs(X) < EPS) then X := 0.0;

        Xc := Xcm(X);
        WriteLine(F, Xc, YminCm, Xc, YminCm - 0.25, '');  { Tick mark }

        if XAxis.Scale = LIN_SCALE then
          Z := X
        else
          Z := Exp10(X);
        WriteNumber(F, 't', Xc, YminCm - 0.35, Z);  { Label }

        if (XAxis.Scale = LOG_SCALE) and (I < N) then
          for J := 2 to 9 do                           { Plot minor divisions }
            begin                                      { on logarithmic scale }
              W := X + Log10(J);
              Xc := Xcm(W);
              WriteLine(F, Xc, YminCm, Xc, YminCm - 0.15, '');
            end;

        X := X + XAxis.Step;
      end;

    { Write axis title }
    if XTitle.Text <> '' then
      WriteText(F, 't', 0.5 * (XminCm + XmaxCm), YminCm - 1.0, XTitle.Text);

    NSZero := NSZ;
  end;

  procedure WriteYAxis(var F : Text);
  var
    W, Y, Yc, Z : Float;
    N, I, J     : Integer;
    NSZ         : Boolean;
  begin
    WriteLine(F, XminCm, YminCm, XminCm, YmaxCm, '');

    N := Round((YAxis.Max - YAxis.Min) / YAxis.Step);
    Y := YAxis.Min;

    NSZ := NSZero;
    NSZero := False;

    for I := 0 to N do
      begin
        if (YAxis.Scale = LIN_SCALE) and (Abs(Y) < EPS) then Y := 0.0;

        Yc := Ycm(Y);
        WriteLine(F, XminCm, Yc, XminCm - 0.25, Yc, '');

        if YAxis.Scale = LIN_SCALE then Z := Y else Z := Exp10(Y);
        WriteNumber(F, 'r', XminCm - 0.35, Yc, Z);

        if (YAxis.Scale = LOG_SCALE) and (I < N) then
          for J := 2 to 9 do
            begin
              W := Y + Log10(J);
              Yc := Ycm(W);
              WriteLine(F, XminCm, Yc, XminCm - 0.15, Yc, '');
            end;

        Y := Y + YAxis.Step;
      end;

    { Write axis title }
    if YTitle.Text <> '' then
      WriteText(F, 'l', XminCm, YmaxCm + 0.5, YTitle.Text);

    NSZero := NSZ;
  end;

  procedure WriteGrid(var F : Text);
  var
    X, Y, Xc, Yc : Float;
    I, N         : Integer;
  begin
    { Horizontal lines }
    if Grid in [HORIZ_GRID, BOTH_GRID] then
      begin
        N := Round((YAxis.Max - YAxis.Min) / YAxis.Step);  { Nb of intervals }
        for I := 1 to Pred(N) do
          begin
            Y := YAxis.Min + I * YAxis.Step;               { Origin of line }
            Yc := Ycm(Y);
            WriteLine(F, XminCm, Yc, XmaxCm, Yc, 'dotted');
          end;
      end;

    { Vertical lines }
    if Grid in [VERTIC_GRID, BOTH_GRID] then
      begin
        N := Round((XAxis.Max - XAxis.Min) / XAxis.Step);
        for I := 1 to Pred(N) do
          begin
            X := XAxis.Min + I * XAxis.Step;
            Xc := Xcm(X);
            WriteLine(F, Xc, YminCm, Xc, YmaxCm, 'dotted');
          end;
      end;
  end;

  procedure InitTexGraph(var F : Text; Filename : String);
  begin
    XminCm := 0.01 * Xwin1 * PAGEWIDTH;
    XmaxCm := 0.01 * Xwin2 * PAGEWIDTH;
    YminCm := 0.01 * Ywin1 * PAGEHEIGHT;
    YmaxCm := 0.01 * Ywin2 * PAGEHEIGHT;

    FactX := (XmaxCm - XminCm) / (XAxis.Max - XAxis.Min);
    FactY := (YmaxCm - YminCm) / (YAxis.Max - YAxis.Min);

    Assign(F, FileName);
    Rewrite(F);

    WriteHeader(F);

    if GraphBorder then
      begin
        Write(F, '\pspolygon');
        WriteCoord(F, XminCm, YminCm);
        WriteCoord(F, XmaxCm, YminCm);
        WriteCoord(F, XmaxCm, YmaxCm);
        WriteCoord(F, XminCm, YmaxCm);
        WriteLn(F);
      end;
  end;

  procedure WritePoint(var F : Text; X, Y : Float);
  var
    Xc, Yc : Float;
  begin
    if XAxis.Scale = LOG_SCALE then X := Log10(X);
    if YAxis.Scale = LOG_SCALE then Y := Log10(Y);

    Xc := Xcm(X);
    Yc := Ycm(Y);

    if (Xc >= XminCm) and (Xc <= XmaxCm) and
       (Yc >= YminCm) and (Yc <= YmaxCm) then
         WriteCoord(F, Xc, Yc);
  end;

  procedure WritePoints(var F : Text; X, Y : PVector;
                        Lbound, Ubound, Symbol, Size : Integer);
  var
    I, N : Integer;
  begin
    Write(F, '\psdots[dotscale=', Size, ' ', Size, ', dotstyle=');
    case Symbol of
      1 : Write(F, '*');
      2 : Write(F, 'o');
      3 : Write(F, 'square*');
      4 : Write(F, 'square');
      5 : Write(F, 'triangle*');
      6 : Write(F, 'triangle');
      7 : Write(F, '+');
      8 : Write(F, 'x');
      9 : Write(F, 'asterisk');
    end;
    WriteLn(F, ']%');

    I := Lbound;
    repeat
      WritePoint(F, X^[I], Y^[I]);
      if (I > 0) and (I < Ubound) and (I mod 5 = 0) then WriteLn(F, '%');
      Inc(I);
    until I > Ubound;
    WriteLn(F);
  end;

  procedure WriteCurve(var F : Text; X, Y : PVector;
                       Lbound, Ubound, Width : Integer;
                       Style : String; Smooth : Boolean);
  var
    I, N : Integer;
    W    : Float;
    Ws   : String;
  begin
    W := 0.01 * Width;
    Str(W:5:2, Ws);
    Ws := Trim(Ws);

    if Smooth then Write(F, '\pscurve') else Write(F, '\psline');
    WriteLn(F, '[linewidth=', Ws, ', linestyle=', Style, ']%');

    I := Lbound;
    repeat
      WritePoint(F, X^[I], Y^[I]);
      if (I > 0) and (I < Ubound) and (I mod 5 = 0) then WriteLn(F, '%');
      Inc(I);
    until I > Ubound;
    WriteLn(F);
  end;

  procedure WriteFunc(var F : Text; Func : TFunc; X1, X2 : Float;
                      Npt, Width : Integer; Style : String);
  const
    X : PVector = nil;
    Y : PVector = nil;
    N : Integer = 0;
  var
    H : Float;
    I : Integer;
  begin
    if Npt <> N then
      begin
        DelVector(X, N);
        DelVector(Y, N);
        DimVector(X, Npt);
        DimVector(Y, Npt);
        N := Npt;
      end;

    H := (X2 - X1) / N;
    for I := 0 to N do
      begin
        X^[I] := X1 + I * H;
        if XAxis.Scale = LIN_SCALE then
          Y^[I] := Func(X^[I])
        else
          Y^[I] := Func(Exp10(X^[I]));
      end;

    WriteCurve(F, X, Y, 0, N, Width, Style, True);
  end;

  procedure CloseTexGraph(var F: Text);
  begin
    WriteLn(F, '\end{pspicture}');
    WriteLn(F);
    WriteLn(F, '\end{document}');
    Close(F);
  end;

end.


