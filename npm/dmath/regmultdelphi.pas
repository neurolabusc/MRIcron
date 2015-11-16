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

unit RegMultDelphi;
interface
uses
  SysUtils,FMath, Matrices, Regress, Models, PaString,messages,dialogs,classes,define_types;
const
kMaxRA = 127;
kCR = chr (13);
kMaxObs = 100;
kMaxFact = 64;
//type
//  TIVra = array [1..kMaxFact,1..kMaxObs] of integer;
  {SpaceType = record
        mrix,mriy,mriz,fobx,foby,fobz: integer;
  end;}
function MultipleRegression (lnObservations,lnFactors: integer; var X:  PMatrix; var lImgIntensity: DoubleP0; var lOutT: DoubleP0): boolean;
function MultipleRegressionVec (lnObservations,lnFactors: integer; var X: PMatrix; var Y: PVector; var lOutT,lOutSlope: DoubleP0): boolean;

//var
//  gMRIFOBra: array [1..kMaxRA] of SpaceType;
//  gCoregRA: array[1..3,0..3] of double; {MRIx,y,z, Offset,FOBx,FOBy,FOBz}

implementation
(*var
  InFName      : String;      { Name of input file }
  Title        : String;      { Title of study }
  XName        : PStrVector;  { Names of independent variables }
  YName        : String;      { Name of dependent variable }
  N            : Integer;     { Number of observations }
  X            : PMatrix;     { Matrix of independent variables }
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
  RegTest      : TRegTest;    { Regression tests }
  gErrCode      : Integer;     { Error code }
  *)

(*  procedure ReadCmdLine(var InFName : String; var CstPar : PVector);
{ ----------------------------------------------------------------------
  Reads command line parameters. Stores constant parameters in CstPar,
  such that :

    CstPar^[0] = Number of independent variables
                 (this one is set by ReadInputFile)
    CstPar^[1] = 1 to include a constant term (b0)

  The contents of CstPar are defined in the unit FITMULT.PAS,
  in the subdirectory REG of the TP Math units directory.
  ---------------------------------------------------------------------- }
  var
     I : Integer;
  begin
    DimVector(CstPar, 1);

    { Name of input file }
    InFName := ParamStr(1);
    if Pos('.', InFName) = 0 then InFName := InFName + '.dat';

    { Presence of constant term }
    //I := 0;
    Val(ParamStr(2), I, gErrCode);
    CstPar^[1] := I;
  end;

  function ReadInputFile(InFName   : String;
                         var Title : String;
                         var XName : PStrVector;
                         var YName : String;
                         var N     : Integer;
                         var X     : PMatrix;
                         var Y     : PVector;
                         CstPar    : PVector) : Integer;
  var
    InF  : Textfile;     { Input file }
    Nvar : Integer;  { Nb of independent variables }
    I, K : Integer;  { Loop variables }
  begin
    Assign(InF, InFName);
    Reset(InF);

    ReadLn(InF, Title);
    ReadLn(InF, Nvar);  { Total number of variables }
    if Nvar < 2 then
      begin
        showmessage('Data file must contain at least 2 variables !');
        ReadInputFile := - 1;
        Exit;
      end;
    Nvar := Pred(Nvar);
    showmessage('trap3x'+inttostr(NVar));
    DimStrVector(XName, Nvar);{crashes here}
    showmessage('trap4x'+inttostr(NVar));
    for I := 1 to Nvar do begin
      ReadLn(InF, XName^[I]);
      showmessage(XName^[I]);
    end;

    ReadLn(InF, YName);
    ReadLn(InF, N);

    DimMatrix(X, Nvar, N);
    DimVector(Y, N);

    for K := 1 to N do
      begin
        for I := 1 to Nvar do
          Read(InF, X^[I]^[K]);
        Read(InF, Y^[K]);
      end;

    Close(InF);
    CstPar^[0] := Nvar;
    ReadInputFile := 0;
  end;

  procedure WriteOutputFile(InFName, Title         : String;
                            XName                  : PStrVector;
                            YName                  : String;
                            N                      : Integer;
                            Y, CstPar, Ycalc, S, B : PVector;
                            V                      : PMatrix;
                            Test                   : TRegTest);
  var
    OutFName : String;   { Name of output file }
    OutF     : TextFile;     { Output file }
    Line1,
    Line2    : String;   { Separating lines }
    Nvar     : Integer;  { Nb of independent variables }
    Delta    : Float;    { Residual }
    Sr       : Float;    { Residual error }
    SB       : PVector;  { Standard deviations of parameters }
    T        : PVector;  { Student's t }
    Prob     : PVector;  { Probabilities }
    I, K     : Integer;  { Loop variables }
  begin
    Nvar := Round(CstPar^[0]);

    DimVector(SB, LastParam);
    DimVector(T, LastParam);
    DimVector(Prob, LastParam);

    K := Pos('.', InFName);
    OutFName := Copy(InFName, 1, Pred(K)) + '.out';
    Assign(OutF, OutFName);
    Rewrite(OutF);

    Line1 := StrChar(73, '-');
    Line2 := StrChar(73, '=');

    WriteLn(OutF, Line2);
    WriteLn(OutF, 'Data file  : ', InFName);
    WriteLn(OutF, 'Study name : ', Title);
    for I := 1 to Nvar do
      WriteLn(OutF, 'x', I:1, '         : ', XName^[I]);
    WriteLn(OutF, 'y          : ', YName);
    WriteLn(OutF, 'Function   : ', FuncName);

    { Perform tests on parameters }
    ParamTest(B, V, N, FirstParam, LastParam, SB, T, Prob);

    WriteLn(OutF, Line1);
    WriteLn(OutF, 'Parameter    Est.value         Std.dev.        t Student       Prob(>|t|)');
    WriteLn(OutF, Line1);
    showmessage(inttostr(nVar)+':'+inttostr(FirstParam)+':'+inttostr(LastParam));
    for I := FirstParam to LastParam do
      if SB^[I] > 0.0 then
        WriteLn(OutF, ParamName(I):5, B^[I]:17:8, SB^[I]:17:8, T^[I]:17:2, Prob^[I]:17:4)
      else
        WriteLn(OutF, ParamName(I):5, B^[I]:17:8);

    WriteLn(OutF, Line1);
    WriteLn(OutF, 'Number of observations            : n   = ', N:5);

    with Test do
      begin
        Sr := Sqrt(Vr);
        WriteLn(OutF, 'Residual error                    : s   = ', Sr:10:8);
        if (R2 >= 0.0) and (R2 <= 1.0) then
          WriteLn(OutF, 'Coefficient of determination      : r2  = ', R2:10:8);
        if (R2a >= 0.0) and (R2a <= 1.0) then
          WriteLn(OutF, 'Adjusted coeff. of determination  : r2a = ', R2a:10:8);
        Write(OutF, 'Variance ratio (explained/resid.) : F   = ', F:10:4);
        WriteLn(OutF, '    Prob(>F) = ', Prob:6:4);
      end;

    WriteLn(OutF, Line1);
    WriteLn(OutF, '  i        Y obs.       Y calc.      Residual      Std.dev.      Std.res.');
    WriteLn(OutF, Line1);

    for K := 1 to N do
      begin
        Delta := Y^[K] - Ycalc^[K];
        WriteLn(OutF, K:3, Y^[K]:14:4, Ycalc^[K]:14:4, Delta:14:4, S^[K]:14:4, (Delta / S^[K]):14:4);
      end;
    WriteLn(OutF, Line2);

    Close(OutF);
    Showmessage('Results written to file '+OutFName);

    DelVector(SB, LastParam);
    DelVector(T, LastParam);
    DelVector(Prob, LastParam);
  end;

{ *************************** Main program ***************************** }
procedure RunReg;
begin
  { Read command line parameters }
  //ReadCmdLine(InFName, CstPar);
  InFName := 'C:\inhib.dat';
  DimVector(CstPar, 1);
  CstPar^[1] := 1;
  { Read input file }

  if ReadInputFile(InFName, Title, XName, YName, N, X, Y, CstPar) <> 0 then
    begin
      showmessage('Error reading file '+ InFName);
      exit;
    end;
  { Initialize regression and variance models.
    See MODELS.PAS in the REG subdirectory for a list of available models }
  InitModel(REG_MULT,
            VAR_CONST,  { Here we use a constant variance }
            CstPar);

  { Set the regression algorithm which must be GAUSS_JORDAN or SVD.
    The default algorithm is SVD. Comment off the following line if
    you wish to change the algorithm. }

  { SetRegAlgo(GAUSS_JORDAN); }

  { Dimension arrays.
    Note: the variance parameters Theta^[1]..Theta^[LastVarParam]
    must be supplied if we use a non-constant variance model }
  DimVector(Theta, LastVarParam);
  DimVector(B, LastParam);
  DimMatrix(V, LastParam, LastParam);
  DimVector(Ycalc, N);
  DimVector(S, N);

  { Perform regression. The numbers 1 and 0.1 denote the maximal number
    of iterations and the tolerance on the parameters. They are purely
    formal values here since the multiple linear regression does not use
    an iterative minimization algorithm. }
  gErrCode := WLSFit(Z, X, Y, N, True, 1, 0.1, Theta, B,
                    B_min, B_max, V, Ycalc, S, RegTest);

  { Write results }
  case gErrCode of
    MAT_OK       : WriteOutputFile(InFName, Title, XName, YName,
                                   N, Y, CstPar, Ycalc, S, B, V, RegTest);
    MAT_SINGUL   : WriteLn('Singular matrix !');
    MAT_NON_CONV : WriteLn('Non-convergence of SVD algorithm !');
  end;
end;
  *)

  //ComputeRegress(lnObservations,lnFactors, Y, CstPar, Ycalc, S, B, V, lRegTest);
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

(*  procedure ScreenOutputFile(
                            var YName                  : String;
                            N,ldimension                      : Integer;
                            var Y, CstPar, Ycalc, S, B : PVector;
                            var V                      : PMatrix;
                            var Test                   : TRegTest;
                            var lDynStr: String);
  var
    lA,lB,lC,lD : String;   { Name of output file }
    Nvar     : Integer;  { Nb of independent variables }
    Delta    : Float;    { Residual }
    Sr       : Float;    { Residual error }
    SB       : PVector;  { Standard deviations of parameters }
    T        : PVector;  { Student's t }
    Prob     : PVector;  { Probabilities }
    I, K     : Integer;  { Loop variables }
  begin
    Nvar := Round(CstPar^[0]);

    DimVector(SB, LastParam);
    DimVector(T, LastParam);
    DimVector(Prob, LastParam);
    { Perform tests on parameters }
    ParamTest(B, V, N, FirstParam, LastParam, SB, T, Prob);
    lDynStr:=lDynStr+'|'+( 'Parameter    Est.value         Std.dev.        t Student       Prob(>|t|)');
    //showmessage(inttostr(nVar)+':'+inttostr(FirstParam)+':'+inttostr(LastParam));
    for I := FirstParam to LastParam do begin
      if SB^[I] > 0.0 then begin
         Str(B^[I]:17:8,lA);
         Str(SB^[I]:17:8,lB);
         Str(T^[I]:17:2,lC);
         Str(Prob^[I]:17:4,lD);
         lDynStr:=lDynStr+'|'+(ParamName(I)+lA+lB+'T='+lC+lD);
      end else begin
         B^[I]:= 0;
         Str(B^[I]:17:8,lA);
         lDynStr:=lDynStr+'|'+(ParamName(I)+lA);
      end;
      //gCoregRA[lDImension,I]:= B^[I];
    end;
    DelVector(SB, LastParam);
    DelVector(T, LastParam);
    DelVector(Prob, LastParam);
  end; *)


//function PredictData(lnObservations: integer; var lStr: tstringlist): boolean;
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
    DelVector(CstPar, 1);
    DelVector(Y, lnObservations);
    //DelStrVector(XName,lnXFactors);

    DelVector(Theta, LastVarParam);
    DelVector(B, LastParam);
    DelMatrix(V, LastParam, LastParam);
    DelVector(Ycalc, lnObservations);
    DelVector(S, lnObservations);
    result := true;

end;

function MultipleRegressionVec (lnObservations,lnFactors: integer; var X: PMatrix; var Y: PVector; var lOutT,lOutSlope: DoubleP0): boolean;
var
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
