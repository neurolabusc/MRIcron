{ ******************************************************************
  Monte-Carlo simulation of the statistical distribution
  of the regression parameters for the exponential model:
  Y = B(1) * Exp(- B(2) * X)
  ****************************************************************** }

program mcsim;

uses
  tpmath, tpgraph;

const
  FuncName = 'Y = B(1) * Exp(- B(2) * X)';

const
  NCycles  =   10;         { Number of cycles }
  MaxSim   = 1000;         { Max nb of simulations at each cycle }
  SavedSim = 1000;         { Nb of simulations to be saved }
  MCFile   = 'mcsim.txt';  { File for storing simulation results }

const
  N        = 10;  { Number of points }
  FirstPar = 1;   { Index of first fitted parameter }
  LastPar  = 2;   { Index of last fitted parameter }

{ Data }
const
  X : array[1..N] of Float = (  1,   2,   3,   4,   5,   6,  7,  8,  9, 10);
  Y : array[1..N] of Float = (416, 319, 244, 188, 144, 113, 85, 66, 50, 41);

function RegFunc(X : Float; B : PVector) : Float;
begin
  RegFunc := B^[1] * Exp(- B^[2] * X);
end;

procedure WriteResults(B : PVector; V : PMatrix);
{ ------------------------------------------------------------------
  Writes results to screen
  ------------------------------------------------------------------ }

var
  Line1,
  Line2 : String;   { Separating lines }
  SB    : Float;    { Standard deviations of parameters }
  I     : Integer;  { Loop variable }

begin
  Line1 := StrChar(73, '-');
  Line2 := StrChar(73, '=');

  WriteLn(Line2);
  WriteLn('Monte-Carlo simulation : ', FuncName);
  WriteLn(Line1);

  WriteLn('Parameter    Est.value         Std.dev.        ');

  WriteLn(Line1);

  for I := FirstPar to LastPar do
    begin
      SB := Sqrt(V^[I]^[I]);
      WriteLn('B(', I:1, ')', B^[I]:17:8, SB:17:8);
    end;

  WriteLn(Line2);
end;

procedure PlotGraph(B1, B2 : PVector);
{ ------------------------------------------------------------------
  Plots simulation results
  ------------------------------------------------------------------ }

var
  Xmin, Xmax, Xstep : Float;    { Ox scale }
  Ymin, Ymax, Ystep : Float;    { Oy scale }

begin
  if not InitGraphics(9, 2, 'c:\tp\bgi') then  { 640x480 16 color }
    begin
      Writeln('Unable to set graphic mode');
      Exit;
    end;

  SetWindow(15, 85, 15, 85, True);

  AutoScale(B1, 1, SavedSim, LinScale, Xmin, Xmax, Xstep);
  AutoScale(B2, 1, SavedSim, LinScale, Ymin, Ymax, Ystep);

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetGraphTitle('Monte-Carlo simulation : ' + FuncName);
  SetOxTitle('B(1)');
  SetOyTitle('B(2)');

  SetFormat(10, 4, False, False);

  PlotOxAxis;
  PlotOyAxis;

  WriteGraphTitle;

  SetClipping(True);

  SetLineParam(1, 0, 0, 0);           { Don't connect points }
  PlotCurve(B1, B2, 1, SavedSim, 1);

  Readln;

  LeaveGraphics;
end;

var
  XX, YY : PVector;  { Data }
  B      : PVector;  { Regression parameters }
  V      : PMatrix;  { Variance-covariance matrix }
  B1, B2 : PVector;  { Simulated parameters }
  F      : Text;     { Output file }
  Iter   : Integer;  { Iteration number }
  I      : Integer;  { Loop variable }

begin
  DimVector(XX, N);
  DimVector(YY, N);
  DimVector(B, LastPar);
  DimMatrix(V, LastPar, LastPar);
  DimVector(B1, SavedSim);
  DimVector(B2, SavedSim);

  { Read data }
  for I := 1 to N do
    begin
      XX^[I] := X[I];
      YY^[I] := Y[I];
    end;

  { Initialize parameters }
  SetParamBounds(1, 100, 1000);
  SetParamBounds(2, 0.1, 1);

  { Set Metropolis-Hastings parameters }
  InitMHParams(NCycles, MaxSim, SavedSim);

  { Set output file and numeric format }
  SetMCFile(MCFile);
  SetFormat(10, 4, False, True);

  { Perform simulation }
  SimFit({$IFDEF FPC}@{$ENDIF}RegFunc, XX, YY, 1, N, B, FirstPar, LastPar, V);

  { Retrieve simulation results into vectors B1 and B2 }
  Assign(F, MCFile);
  Reset(F);
  for I := 1 to SavedSim do
    ReadLn(F, Iter, B1^[I], B2^[I]);
  Close(F);

  { Write results }
  WriteResults(B, V);

  { Plot curve }
  PlotGraph(B1, B2);

  DelVector(XX, N);
  DelVector(YY, N);
  DelVector(B, LastPar);
  DelMatrix(V, LastPar, LastPar);
  DelVector(B1, SavedSim);
  DelVector(B2, SavedSim);
end.
