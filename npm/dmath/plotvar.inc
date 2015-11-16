{ **********************************************************************
  *                             PLOTVAR.INC                            *
  **********************************************************************
                  Constants, types and global variables
                   common to PLOT.PAS and TEXPLOT.PAS
  ********************************************************************** }

const
  MAXSYMBOL = 9;        { Max. number of graphic symbols }
  EPS       = 1.0E-10;  { Lower limit for an axis label }

type
  TScale  = (LIN_SCALE,        { Scale }
             LOG_SCALE);

  TGrid   = (NO_GRID,          { Grid }
             HORIZ_GRID,
             VERTIC_GRID,
             BOTH_GRID);

  TAxis   = record             { Coordinate axis }
              Scale : TScale;
              Min   : Float;
              Max   : Float;
              Step  : Float;
            end;

  TTitle  = record             { Title for main graph or axis }
              Text       : String[70];
              Font       : Integer;
              CharWidth  : Integer;
              CharHeight : Integer;
            end;

  TLegend = record             { Legends of plotted curves }
              Text       : array[1..MAXSYMBOL] of String[40];
              Font       : Integer;
              CharWidth  : Integer;
              CharHeight : Integer;
              SymbolSize : Integer;
            end;

{ ******** Global variables defining the appearance of the graph ******* }

const
  Xwin1       : Integer = 15;                       { Window limits in % }
  Ywin1       : Integer = 15;
  Xwin2       : Integer = 85;
  Ywin2       : Integer = 85;

  GraphBorder : Boolean = True;                     { Plot graph border }

  XAxis       : TAxis   = (Scale      : LIN_SCALE;  { Horizontal axis }
                           Min        : 0.0;
                           Max        : 1.0;
                           Step       : 0.2);

  YAxis       : TAxis   = (Scale      : LIN_SCALE;  { Vertical axis }
                           Min        : 0.0;
                           Max        : 1.0;
                           Step       : 0.2);

  Grid        : TGrid   = NO_GRID;                  { Grid }

  GraphTitle  : TTitle  = (Text       : '';         { Title of graph }
                           Font       : 2;
                           CharWidth  : 300;
                           CharHeight : 350);

  XTitle      : TTitle  = (Text       : 'X';        { Title of X axis }
                           Font       : 2;
                           CharWidth  : 200;
                           CharHeight : 250);

  YTitle      : TTitle  = (Text       : 'Y';        { Title of Y axis }
                           Font       : 2;
                           CharWidth  : 200;
                           CharHeight : 250);

  Legend      : TLegend = (Text       : ('A',       { Legends of curves }
                                         'B',
                                         'C',
                                         'D',
                                         'E',
                                         'F',
                                         'G',
                                         'H',
                                         'I');
                           Font       : 2;
                           CharWidth  : 50;
                           CharHeight : 50;
                           SymbolSize : 3);

