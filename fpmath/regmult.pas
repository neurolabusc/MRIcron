{ **********************************************************************
  *                        Program REGMULT.PAS                         *
  *                            Version 1.1                             *
  *                    (c) J. Debord, August 2000                      *
  **********************************************************************
  This program performs a weighted multiple linear least squares fit :

                     y = b0 + b1 * x1 + b2 * x2 + ...

  The following parameters are passed on the command line :

    1st parameter = name of input file (default extension = .DAT)
    2nd parameter = 1 if the equation includes a constant term b0

  Input files are ASCII files with the following structure :

    Line 1     : Title of study
    Line 2     : Number of variables (must be >= 2 here !)
    Next lines : Names of variables x1, x2, ..., y
    Next line  : Number of observations (must be > number of variables !)

    The next lines contain the coordinates (x1, x2, ..., y) of the
    observations (1 observation by line). The coordinates must be
    separated by spaces or tabulations.

  The file INHIB.DAT is an example of data relating the inhibition of an
  enzyme to the physico-chemical properties of the inhibitors (J. DEBORD,
  P. N'DIAYE, J. C. BOLLINGER et al, J. Enzyme Inhib., 1997, 12, 13-26).
  The program parameters are : INHIB 1

  The program may be executed from Turbo Pascal's integrated environment,
  in which case the parameters are entered through the "Parameters" option
  of the menu, or from DOS (after compilation into an executable file),
  in which case the parameters are entered on the command line (e.g.
  REGMULT INHIB 1).
  ********************************************************************** }

unit regmult;
interface
uses
  SysUtils,utypes,umulfit,classes,define_types,dialogs;

function MultipleRegressionVec (lnObservations,lnFactors: integer; var X: PMatrix; var Y: PVector; var lOutT,lOutSlope: DoubleP0): boolean;
function MultipleRegression (lnObservations,lnFactors: integer; var X:  PMatrix;
         var lImgIntensity: DoubleP0; var lOutT: DoubleP0): boolean;
implementation

uses usvdfit,uregtest;

(*procedure WriteResults(NVar: integer;  var lOutT,lOutSlope: DoubleP0);
var
  I: integer;
  lStr: string;
begin
  for I := 0 to Nvar do begin
     lStr := floattostr(lOutT^[I]) +' '+floattostr(lOutSlope[I]);
     Form1.memo1.lines.add(lStr);
  end;
end; *)



function MultipleRegressionVec (lnObservations,lnFactors: integer;
         var X: PMatrix; var Y: PVector; var lOutT,lOutSlope: DoubleP0): boolean;
var
  //lmax,lmin: float;
  XX    : PMatrix;   { Independent variables }
  //YY    : PVector;   { Dependent variable }
  Ycalc : PVector;   { Computed Y values }
  B     : PVector;   { Fitted parameters }
  V     : PMatrix;   { Variance-covariance matrix }
  Test  : TRegTest;  { Statistical tests }
  lVar    : Float;     { Variance for t value }
  Lb,I, J  : Integer;   { Loop variable }
  ConsTerm : boolean;  { Include a constant term B(0) }
begin
  result := false;
  ConsTerm := true;
  { Dimension arrays }
  DimMatrix(XX, lnObservations, lnFactors);
  //DimVector(YY, lnObservations);
  DimVector(Ycalc, lnObservations);
  DimVector(B, lnFactors);
  DimMatrix(V, lnFactors, lnFactors);
  { Read data }
  for I := 1 to lnObservations do begin
      for J := 1 to lnFactors do begin
        XX^[I]^[J] := X^[J]^[I];//Designed to be compatible with old versions of fpmath
      end;

  end;
  
  (*  lmin := X^[1]^[1];
  lmax := X^[1]^[1];
  for I := 1 to lnFactors do begin
      for J := 1 to lnObservations do begin
          if X^[I]^[J] > lmax then
             lMax := X^[I]^[J];
          if X^[I]^[J] < lmin then
             lMin := X^[I]^[J];
      end;
  end;
  fx(lmin,lmax);*)
  
  { Perform regression }
  // MulFit(XX, Y, 1, lnObservations, lnFactors, ConsTerm, B, V);
  SVDFit(XX, Y, 1, lnObservations, lnFactors, ConsTerm, 1.0E-8, B, V);
  { Compute predicted Y values }
  for I := 1 to lnObservations do begin
      if ConsTerm then Ycalc^[I] := B^[0] else Ycalc^[I] := 0.0;
      for J := 1 to lnFactors do
        Ycalc^[I] := Ycalc^[I] + B^[J] * XX^[I]^[J];
  end;
  { Update variance-covariance matrix and compute statistical tests }
  if ConsTerm then Lb := 0 else Lb := 1;
  RegTest(Y, Ycalc, 1, lnObservations, V, Lb, lnFactors, Test);
  //output Slopes
  for I := 0 to (lnFactors-1) do
      lOutSlope^[I] := B^[I+1];//first parameter is global fit
  lOutSlope^[lnFactors] := B^[0];//global fit
  //T scores
  for I := 0 to (lnFactors-1) do begin

      lVar :=Sqrt(V^[I+1]^[I+1]);
      //fx(lVar,B^[I+1]);
      if lVar <> 0 then
         lOutT^[I] := B^[I+1] / lVar
      else
          lOutT^[I] := 0;
  end;
  lvar := Sqrt(V^[0]^[0]);
  if lvar <> 0 then
     lOutT^[lnFactors] := B^[0]/ lvar //global fit
  else
      lOutT^[lnFactors] := 0;
  //cleanup
  DelMatrix(XX, lnObservations, lnFactors);
  //DelVector(YY, lnObservations);
  DelVector(Ycalc, lnObservations);
  DelVector(B, lnFactors);
  DelMatrix(V, lnFactors,lnFactors);
  result := true;
end;

function MultipleRegression (lnObservations,lnFactors: integer; var X:  PMatrix;
         var lImgIntensity: DoubleP0; var lOutT: DoubleP0): boolean;
var
  I: integer;
  Y    : PVector;   { Dependent variable }
  lOutSlope: DoubleP0;
begin
  DimVector(Y, lnObservations);
  Getmem(lOutSlope,(lnObservations+1)*sizeof(double));
  { Read data }
  for I := 1 to lnObservations do
      Y^[I] := lImgIntensity^[I-1];
  result := MultipleRegressionVec (lnObservations,lnFactors,X, Y, lOutT,lOutSlope);
  Freemem(lOutslope);
  DelVector(Y,lnObservations);
end;

end.
