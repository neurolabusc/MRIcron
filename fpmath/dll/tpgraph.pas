{ ******************************************************************
  Unit TPGRAPH - Interface for TPGRAPH.DLL
  ****************************************************************** }

unit tpgraph;

interface

uses
  tpmath {$IFDEF DELPHI}, Graphics{$ENDIF};

function InitGraphics
{$IFDEF DELPHI}
(Canvas : TCanvas; Width, Height : Integer) : Boolean;
{$ELSE}
(Pilot, Mode : Integer; BGIPath : String) : Boolean;
{$ENDIF}
external 'tpgraph';
{ Enters graphic mode }

procedure SetWindow({$IFDEF DELPHI}Canvas : TCanvas;{$ENDIF}
                    X1, X2, Y1, Y2 : Integer; GraphBorder : Boolean);
external 'tpgraph';
{ Sets the graphic window }

procedure AutoScale(X                     : PVector;
                    Lb, Ub                : Integer;
                    Scale                 : TScale;
                    var XMin, XMax, XStep : Float);
external 'tpgraph';
{ Finds an appropriate scale for plotting the data in X[Lb..Ub] }

procedure SetOxScale(Scale                : TScale;
                     OxMin, OxMax, OxStep : Float);
external 'tpgraph';
{ Sets the scale on the Ox axis }

procedure SetOyScale(Scale                : TScale;
                     OyMin, OyMax, OyStep : Float);
external 'tpgraph';
{ Sets the scale on the Oy axis }

procedure SetGraphTitle(Title : String); external 'tpgraph';
{ Sets the title for the graph }

procedure SetOxTitle(Title : String); external 'tpgraph';
{ Sets the title for the Ox axis }

procedure SetOyTitle(Title : String); external 'tpgraph';
{ Sets the title for the Oy axis }

{$IFNDEF DELPHI}

procedure SetTitleFont(FontIndex, Width, Height : Integer);
external 'tpgraph';
{ Sets the font for the main graph title }

procedure SetOxFont(FontIndex, Width, Height : Integer);
external 'tpgraph';
{ Sets the font for the Ox axis (title and labels) }

procedure SetOyFont(FontIndex, Width, Height : Integer);
external 'tpgraph';
{ Sets the font for the Oy axis (title and labels) }

procedure SetLgdFont(FontIndex, Width, Height : Integer);
external 'tpgraph';
{ Sets the font for the legends }

procedure SetClipping(Clip : Boolean);
external 'tpgraph';
{ Determines whether drawings are clipped at the current viewport
  boundaries, according to the value of the Boolean parameter Clip }

{$ENDIF}

procedure PlotOxAxis{$IFDEF DELPHI}(Canvas : TCanvas){$ENDIF};
external 'tpgraph';
{ Plots the horizontal axis }

procedure PlotOyAxis{$IFDEF DELPHI}(Canvas : TCanvas){$ENDIF};
external 'tpgraph';
{ Plots the vertical axis }

procedure PlotGrid({$IFDEF DELPHI}Canvas : TCanvas;{$ENDIF} Grid : TGrid);
external 'tpgraph';
{ Plots a grid on the graph }

procedure WriteGraphTitle{$IFDEF DELPHI}(Canvas : TCanvas){$ENDIF};
external 'tpgraph';
{ Writes the title of the graph }

procedure SetMaxCurv(NCurv : Byte); external 'tpgraph';
{ Sets the maximum number of curves and re-initializes their parameters }

procedure SetPointParam
{$IFDEF DELPHI}
(CurvIndex, Symbol, Size : Integer; Color : TColor);
{$ELSE}
(CurvIndex, Symbol, Size, Color : Integer);
{$ENDIF}
external 'tpgraph';
{ Sets the point parameters for curve # CurvIndex }

procedure SetLineParam
{$IFDEF DELPHI}
(CurvIndex : Integer; Style : TPenStyle; Width : Integer; Color : TColor);
{$ELSE}
(CurvIndex, Style, Width, Color : Integer);
{$ENDIF}
external 'tpgraph';
{ Sets the line parameters for curve # CurvIndex }

procedure SetCurvLegend(CurvIndex : Integer; Legend : String);
external 'tpgraph';
{ Sets the legend for curve # CurvIndex }

procedure SetCurvStep(CurvIndex, Step : Integer);
external 'tpgraph';
{ Sets the step for curve # CurvIndex }

procedure PlotPoint({$IFDEF DELPHI}Canvas : TCanvas;{$ENDIF}
                    X, Y                  : Float;
                    CurvIndex             : Integer);
external 'tpgraph';
{ Plots a point on the screen }

procedure PlotCurve({$IFDEF DELPHI}Canvas : TCanvas;{$ENDIF}
                    X, Y                  : PVector;
                    Lb, Ub, CurvIndex     : Integer);
external 'tpgraph';
{ Plots a curve }

procedure PlotCurveWithErrorBars({$IFDEF DELPHI}Canvas : TCanvas;{$ENDIF}
                                 X, Y, S               : PVector;
                                 Ns, Lb, Ub, CurvIndex : Integer);
external 'tpgraph';
{ Plots a curve with error bars }

procedure PlotFunc({$IFDEF DELPHI}Canvas : TCanvas;{$ENDIF}
                   Func                  : TFunc;
                   Xmin, Xmax            : Float;
                   {$IFDEF DELPHI}Npt    : Integer;{$ENDIF}
                   CurvIndex             : Integer);
external 'tpgraph';
{ Plots a function }

procedure WriteLegend({$IFDEF DELPHI}Canvas : TCanvas;{$ENDIF}
                      NCurv                 : Integer;
                      ShowPoints, ShowLines : Boolean);
external 'tpgraph';
{ Writes the legends for the plotted curves }

procedure ConRec({$IFDEF DELPHI}Canvas : TCanvas;{$ENDIF}
                 Nx, Ny, Nc            : Integer;
                 X, Y, Z               : PVector;
                 F                     : PMatrix);
external 'tpgraph';
{ Contour plot }

function Xpixel(X : Float) : Integer; external 'tpgraph';
{ Converts user abscissa X to screen coordinate }

function Ypixel(Y : Float) : Integer; external 'tpgraph';
{ Converts user ordinate Y to screen coordinate }

function Xuser(X : Integer) : Float; external 'tpgraph';
{ Converts screen coordinate X to user abscissa }

function Yuser(Y : Integer) : Float; external 'tpgraph';
{ Converts screen coordinate Y to user ordinate }

procedure LeaveGraphics; external 'tpgraph';
{ Quits graphic mode }

implementation

end.

