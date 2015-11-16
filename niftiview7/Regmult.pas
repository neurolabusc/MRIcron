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

unit RegMult;
interface
uses
  SysUtils,FMath, Matrices, Regress, Models, PaString,messages,dialogs,classes,define_types;
const
kMaxRA = 127;
kCR = chr (13);
kMaxObs = 100;
kMaxFact = 64;

function MultipleRegressionVec (lnObservations,lnFactors: integer; var X: PMatrix; var Y: PVector; var lOutT,lOutSlope: DoubleP0): boolean;


implementation
procedure ComputeRegress (N,lnFactors                      : Integer;
                            var Y, CstPar, Ycalc, S, B : PVector;
                            var V                      : PMatrix;
                            var Test                   : TRegTest; var lOutT: DoubleP0);
var
    I: integer;
    SB       : PVector;  { Standard deviations of parameters }
    T        : PVector;  { Student's t }
    Prob     : PVector;  { Probabilities }
begin
    DimVector(SB, LastParam);
    DimVector(T, LastParam);
    DimVector(Prob, LastParam);
    { Perform tests on parameters }
    ParamTest(B, V, N, FirstParam, LastParam, SB, T, Prob);
    for I := 0 to (lnFactors-1) do
            lOutT[I] := T^[FirstParam+I+1];//first parameter is global fit

    lOutT[lnFactors] := T^[FirstParam];//global fit

    //for I := FirstParam to LastParam do
    //    Showmessage(floattostr(T^[I]) );
    DelVector(SB, LastParam);
    DelVector(T, LastParam);
    DelVector(Prob, LastParam);

end;

function MultipleRegression (lnObservations,lnFactors: integer; var X: PMatrix; var lImgIntensity: DoubleP0; var lOutT: DoubleP0): boolean;
var
   K : Integer;  { Nb of independent variables }
  //X            : PMatrix;     { Matrix of independent variables }
  Y            : PVector;     { Vector of dependent variable }
  Z            : PVector;     { Vector of independent variable (not used here) }
  Ycalc        : PVector;     { Expected Y values }
  S            : PVector;     { Standard deviations of Y values }
  CstPar       : PVector;     { Constant parameters }
  B            : PVector;     { Regression parameters }
  B_min, B_max : PVector;     { Parameter bounds (not used, but must be
                                declared in order to use the WLSFit routine ) }
  V            : PMatrix;     { Variance-covariance matrix of regression parameters }
  Theta        : PVector;     { Variance parameters }
  lRegTest      : TRegTest;    { Regression tests }
  gErrCode      : Integer;     { Error code }
begin
  result := false;
  if lnObservations < 5 then begin
     showmessage('At least 5 samples required for 3D registration.');
     exit;
  end;
  DimVector(CstPar, 1);
  DimVector(Y, lnObservations);
  CstPar^[1] := 1;
  CstPar^[0] := lnFactors;
    for K := 1 to lnObservations do
           Y^[K] := lImgIntensity[K-1];
  { Initialize regression and variance models.}
  InitModel(REG_MULT,VAR_CONST,{ Here we use a constant variance }CstPar);
  { Set the regression algorithm which must be GAUSS_JORDAN or SVD.
    The default algorithm is SVD. Comment off the following line if
    you wish to change the algorithm. }
  { SetRegAlgo(GAUSS_JORDAN); }
  DimVector(Theta, LastVarParam);
  DimVector(B, LastParam);
  DimMatrix(V, LastParam, LastParam);
  DimVector(Ycalc, lnObservations);
  DimVector(S, lnObservations);
  { Perform regression. The numbers 1 and 0.1 denote the maximal number
    of iterations and the tolerance on the parameters. They are purely
    formal values here since the multiple linear regression does not use
    an iterative minimization algorithm. }
  gErrCode := WLSFit(Z, X, Y, lnObservations, True, 1, 0.1, Theta, B,B_min, B_max, V, Ycalc, S, lRegTest);
  { Write results }
  case gErrCode of
       MAT_OK : begin
             //ScreenOutputFile({XName,}YName,lnObservations,lDim, Y, CstPar, Ycalc, S, B, V, lRegTest,lStr);
             //Showmessage(lStr);
             ComputeRegress(lnObservations,lnFactors, Y, CstPar, Ycalc, S, B, V, lRegTest,lOutT);
       end;
{    MAT_OK       : WriteOutputFile(InFName, Title, XName, YName,
                                   N, Y, CstPar, Ycalc, S, B, V, RegTest);
 }   MAT_SINGUL   : Showmessage('Singular matrix !');
    MAT_NON_CONV : Showmessage('Non-convergence of SVD algorithm !');
  end;
    DelVector(CstPar, 1);
    DelVector(Y, lnObservations);

    DelVector(Theta, LastVarParam);
    DelVector(B, LastParam);
    DelMatrix(V, LastParam, LastParam);
    DelVector(Ycalc, lnObservations);
    DelVector(S, lnObservations);
    result := true;

end;

function MultipleRegressionVec (lnObservations,lnFactors: integer; var X: PMatrix; var Y: PVector; var lOutT,lOutSlope: DoubleP0): boolean;
var
  //i,j: integer;lmin,lmax: float;
   K : Integer;  { Nb of independent variables }
  Z            : PVector;     { Vector of independent variable (not used here) }
  Ycalc        : PVector;     { Expected Y values }
  S            : PVector;     { Standard deviations of Y values }
  CstPar       : PVector;     { Constant parameters }
  B            : PVector;     { Regression parameters }
  B_min, B_max : PVector;     { Parameter bounds (not used, but must be
                                declared in order to use the WLSFit routine ) }
  V            : PMatrix;     { Variance-covariance matrix of regression parameters }
  Theta        : PVector;     { Variance parameters }
  lRegTest      : TRegTest;    { Regression tests }
  gErrCode      : Integer;     { Error code }
begin
  (*lmin := X^[1]^[1];
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

  result := false;
  if lnObservations < 5 then begin
     showmessage('At least 5 samples required for 3D registration.');
     exit;
  end;
  DimVector(CstPar, 1);
  CstPar^[1] := 1;
  CstPar^[0] := lnFactors;
  { Initialize regression and variance models.}
  InitModel(REG_MULT,VAR_CONST,{ Here we use a constant variance }CstPar);
  { Set the regression algorithm which must be GAUSS_JORDAN or SVD.
    The default algorithm is SVD. Comment off the following line if
    you wish to change the algorithm. }
  { SetRegAlgo(GAUSS_JORDAN); }
  DimVector(Theta, LastVarParam);
  DimVector(B, LastParam);
  DimMatrix(V, LastParam, LastParam);
  DimVector(Ycalc, lnObservations);
  DimVector(S, lnObservations);
  { Perform regression. The numbers 1 and 0.1 denote the maximal number
    of iterations and the tolerance on the parameters. They are purely
    formal values here since the multiple linear regression does not use
    an iterative minimization algorithm. }
  gErrCode := WLSFit(Z, X, Y, lnObservations, True, 1, 0.1, Theta, B,B_min, B_max, V, Ycalc, S, lRegTest);
  { Write results }
  //showmessage(inttostr(xx));
  case gErrCode of
       MAT_OK : begin
             //ScreenOutputFile({XName,}YName,lnObservations,lDim, Y, CstPar, Ycalc, S, B, V, lRegTest,lStr);
             //Showmessage(lStr);
             ComputeRegress(lnObservations,lnFactors, Y, CstPar, Ycalc, S, B, V, lRegTest,lOutT);
       end;
{    MAT_OK       : WriteOutputFile(InFName, Title, XName, YName,
                                   N, Y, CstPar, Ycalc, S, B, V, RegTest);
 }   MAT_SINGUL   : Showmessage('Singular matrix !');
    MAT_NON_CONV : Showmessage('Non-convergence of SVD algorithm !');
  end;
    for K := 0 to (lnFactors-1) do
            lOutSlope^[K] := B^[FirstParam+K+1];//first parameter is global fit

    lOutSlope^[lnFactors] := B^[FirstParam];//global fit

    DelVector(CstPar, 1);
    //DelVector(Y, lnObservations);
    //DelStrVector(XName,lnXFactors);

    DelVector(Theta, LastVarParam);
    DelVector(B, LastParam);
    DelMatrix(V, LastParam, LastParam);
    DelVector(Ycalc, lnObservations);
    DelVector(S, lnObservations);
    result := true;

end;


end.
