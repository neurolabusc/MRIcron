{ **********************************************************************
  *                          Unit MODELS.PAS                           *
  *                            Version 1.4                             *
  *                    (c) J. Debord, August 2000                      *
  **********************************************************************
                Library of regression and variance models
  ********************************************************************** }

unit Models;

{$F+}

interface

uses
  FMath,
  Matrices,
  Regress,
  FitLin,
  FitMult,
  FitPoly,
  FitFrac,
  FitExpo,
  FitIExpo,
  FitExLin,
  FitPower,
  FitMich,
  FitHill,
  FitLogis,
  FitPKa;

{ ---------------------------------------------------------------------
  Highest index of regression models
  --------------------------------------------------------------------- }
const
  MAXMODEL = 11;

{ ---------------------------------------------------------------------
  Highest index of variance models
  --------------------------------------------------------------------- }
const
  MAXVARMODEL = 5;

{ ---------------------------------------------------------------------
  Definition of regression models
  --------------------------------------------------------------------- }
const
  REG_LIN   = 0;   { Linear }
  REG_MULT  = 1;   { Multiple linear }
  REG_POL   = 2;   { Polynomial }
  REG_FRAC  = 3;   { Rational fraction }
  REG_EXPO  = 4;   { Sum of exponentials }
  REG_IEXPO = 5;   { Increasing exponential }
  REG_EXLIN = 6;   { Exponential + linear }
  REG_POWER = 7;   { Power }
  REG_MICH  = 8;   { Michaelis }
  REG_HILL  = 9;   { Hill }
  REG_LOGIS = 10;  { Logistic }
  REG_PKA   = 11;  { Acid/Base titration curve }

{ ---------------------------------------------------------------------
  Definition of variance models
  --------------------------------------------------------------------- }
const
  VAR_CONST = 0;  { Constant }
  VAR_LIN   = 1;  { Linear }
  VAR_POL2  = 2;  { 2nd degree polynomial }
  VAR_POL3  = 3;  { 3rd degree polynomial }
  VAR_EXPO  = 4;  { Exponential }
  VAR_POWER = 5;  { Power }

{ ---------------------------------------------------------------------
  Names of regression models
  --------------------------------------------------------------------- }

const
  MODELNAME : array[0..MAXMODEL] of String =
{$IFDEF FRENCH}
  ('Lineaire',
   'Lineaire multiple',
   'Polynomial',
   'Fraction rationnelle',
   'Somme d''exponentielles',
   'Exponentielle croissante',
   'Exponentielle + lineaire',
   'Puissance',
   'Michaelis',
   'Hill',
   'Logistique',
   'Titrage acide/base');
{$ELSE}
  ('Linear',
   'Multiple linear',
   'Polynomial',
   'Rational fraction',
   'Sum of exponentials',
   'Increasing exponential',
   'Exponential + linear',
   'Power',
   'Michaelis',
   'Hill',
   'Logistic',
   'Acid/Base titration curve');
{$ENDIF}

{ ---------------------------------------------------------------------
  Names of variance models
  --------------------------------------------------------------------- }

const
  VARMODELNAME : array[0..MAXVARMODEL] of String =
{$IFDEF FRENCH}
  ('Constante',
   'Lineaire',
   'Polynome de degre 2',
   'Polynome de degre 3',
   'Exponentielle',
   'Puissance');
{$ELSE}
  ('Constant',
   'Linear',
   '2nd degree polynomial',
   '3rd degree polynomial',
   'Exponential',
   'Power');
{$ENDIF}

function FuncName : String;
{ --------------------------------------------------------------------
  Returns the name of the regression function
  -------------------------------------------------------------------- }

function FirstParam : Integer;
{ --------------------------------------------------------------------
  Returns the index of the first fitted parameter
  -------------------------------------------------------------------- }

function LastParam : Integer;
{ --------------------------------------------------------------------
  Returns the index of the last fitted parameter
  -------------------------------------------------------------------- }

function ParamName(I : Integer) : String;
{ --------------------------------------------------------------------
  Returns the name of the I-th fitted parameter
  -------------------------------------------------------------------- }

function RegFunc(X : Float; B : PVector) : Float;
{ --------------------------------------------------------------------
  Computes the regression function for one independent variable
  B is the vector of parameters
  -------------------------------------------------------------------- }

function RegFuncNVar(X, B : PVector) : Float;
{ --------------------------------------------------------------------
  Computes the regression function for several independent variables
  B is the vector of parameters
  -------------------------------------------------------------------- }

procedure DerivProc(RegFunc : TRegFunc; X, Y : Float; B, D : PVector);
{ --------------------------------------------------------------------
  Computes the derivatives of the regression function at point (X,Y)
  with respect to the parameters B. The results are returned in D.
  D^[I] contains the derivative with respect to the I-th parameter.
  -------------------------------------------------------------------- }

procedure InitModel(Reg_Model, Var_Model : Integer; CstPar : PVector);
{ --------------------------------------------------------------------
  Initializes the regression and variance models. Constant parameters
  (e.g. degree of polynomial) are passed in vector CstPar.
  -------------------------------------------------------------------- }

function WLSFit(X            : PVector;
                U            : PMatrix;
                Y            : PVector;
                N            : Integer;
                Init         : Boolean;
                MaxIter      : Integer;
                Tol          : Float;
                Theta, B     : PVector;
                B_min, B_max : PVector;
                V            : PMatrix;
                Ycalc, S     : PVector;
                var Test     : TRegTest) : Integer;
{ ----------------------------------------------------------------------
  Fits the regression function and computes the regression tests
  ----------------------------------------------------------------------
  Input :  X, U         = vector or matrix of independent variable(s)
           Y            = vector of dependent variable
           N            = number of observations
           Init         = TRUE to compute initial parameter estimates
                          FALSE to use the current values
           MaxIter      = maximum number of iterations
                          (if 0 the parameters will not be refined)
           Tol          = required parameter precision
           Theta        = variance parameters
           B            = initial parameters values
           B_min, B_max = parameter bounds
  --------------------------------------------------------------------
  Output : Theta = updated variance parameters
                   (residual variance stored in Theta^[0])
           B     = regression parameters
           V     = variance-covariance matrix
           Ycalc = estimated Y values
           S     = standard deviations of Y
           Test  = regression tests
  --------------------------------------------------------------------
  Possible results = OPT_OK         : no error
                     OPT_SING       : singular matrix
                     OPT_BIG_LAMBDA : too high Marquardt's parameter
                     OPT_NON_CONV   : non-convergence
  -------------------------------------------------------------------- }

function VarFuncName : String;
{ --------------------------------------------------------------------
  Returns the name of the variance function
  -------------------------------------------------------------------- }

function LastVarParam : Integer;
{ ----------------------------------------------------------------------
  Returns the index of the last variance parameter (upper bound of Theta)
  ---------------------------------------------------------------------- }

function VarFunc(Y : Float; Theta : PVector) : Float;
{ --------------------------------------------------------------------
  Computes the variance of an observation Y. The parameters are
  Theta^[1], Theta^[2],... The true variance is Theta^[0] * VarFunc,
  where Theta^[0] (equal to the residual variance Vr) is estimated by
  the regression program.
  -------------------------------------------------------------------- }

implementation

const
  RegModel : Integer = 0;  { Index of regression model }
  VarModel : Integer = 0;  { Index of variance model }

  function FuncName : String;
  begin
    case RegModel of
      REG_LIN   : FuncName := FitLin.FuncName;
      REG_MULT  : FuncName := FitMult.FuncName;
      REG_POL   : FuncName := FitPoly.FuncName;
      REG_FRAC  : FuncName := FitFrac.FuncName;
      REG_EXPO  : FuncName := FitExpo.FuncName;
      REG_IEXPO : FuncName := FitIExpo.FuncName;
      REG_EXLIN : FuncName := FitExLin.FuncName;
      REG_POWER : FuncName := FitPower.FuncName;
      REG_MICH  : FuncName := FitMich.FuncName;
      REG_HILL  : FuncName := FitHill.FuncName;
      REG_LOGIS : FuncName := FitLogis.FuncName;
      REG_PKA   : FuncName := FitPKa.FuncName;
    end;
  end;

  function FirstParam : Integer;
  begin
    case RegModel of
      REG_LIN   : FirstParam := FitLin.FirstParam;
      REG_MULT  : FirstParam := FitMult.FirstParam;
      REG_POL   : FirstParam := FitPoly.FirstParam;
      REG_FRAC  : FirstParam := FitFrac.FirstParam;
      REG_EXPO  : FirstParam := FitExpo.FirstParam;
      REG_IEXPO : FirstParam := FitIExpo.FirstParam;
      REG_EXLIN : FirstParam := FitExLin.FirstParam;
      REG_POWER : FirstParam := FitPower.FirstParam;
      REG_MICH  : FirstParam := FitMich.FirstParam;
      REG_HILL  : FirstParam := FitHill.FirstParam;
      REG_LOGIS : FirstParam := FitLogis.FirstParam;
      REG_PKA   : FirstParam := FitPKa.FirstParam;
    end;
  end;

  function LastParam : Integer;
  begin
    case RegModel of
      REG_LIN   : LastParam := FitLin.LastParam;
      REG_MULT  : LastParam := FitMult.LastParam;
      REG_POL   : LastParam := FitPoly.LastParam;
      REG_FRAC  : LastParam := FitFrac.LastParam;
      REG_EXPO  : LastParam := FitExpo.LastParam;
      REG_IEXPO : LastParam := FitIExpo.LastParam;
      REG_EXLIN : LastParam := FitExLin.LastParam;
      REG_POWER : LastParam := FitPower.LastParam;
      REG_MICH  : LastParam := FitMich.LastParam;
      REG_HILL  : LastParam := FitHill.LastParam;
      REG_LOGIS : LastParam := FitLogis.LastParam;
      REG_PKA   : LastParam := FitPKa.LastParam;
    end;
  end;

  function ParamName(I : Integer) : String;
  begin
    case RegModel of
      REG_LIN   : ParamName := FitLin.ParamName(I);
      REG_MULT  : ParamName := FitMult.ParamName(I);
      REG_POL   : ParamName := FitPoly.ParamName(I);
      REG_FRAC  : ParamName := FitFrac.ParamName(I);
      REG_EXPO  : ParamName := FitExpo.ParamName(I);
      REG_IEXPO : ParamName := FitIExpo.ParamName(I);
      REG_EXLIN : ParamName := FitExLin.ParamName(I);
      REG_POWER : ParamName := FitPower.ParamName(I);
      REG_MICH  : ParamName := FitMich.ParamName(I);
      REG_HILL  : ParamName := FitHill.ParamName(I);
      REG_LOGIS : ParamName := FitLogis.ParamName(I);
      REG_PKA   : ParamName := FitPKa.ParamName(I);
    end;
  end;

  function RegFunc(X : Float; B : PVector) : Float;
  begin
    case RegModel of
      REG_LIN   : RegFunc := FitLin.RegFunc(X, B);
      REG_POL   : RegFunc := FitPoly.RegFunc(X, B);
      REG_FRAC  : RegFunc := FitFrac.RegFunc(X, B);
      REG_EXPO  : RegFunc := FitExpo.RegFunc(X, B);
      REG_IEXPO : RegFunc := FitIExpo.RegFunc(X, B);
      REG_EXLIN : RegFunc := FitExLin.RegFunc(X, B);
      REG_POWER : RegFunc := FitPower.RegFunc(X, B);
      REG_MICH  : RegFunc := FitMich.RegFunc(X, B);
      REG_HILL  : RegFunc := FitHill.RegFunc(X, B);
      REG_LOGIS : RegFunc := FitLogis.RegFunc(X, B);
      REG_PKA   : RegFunc := FitPKa.RegFunc(X, B);
    end;
  end;

  function RegFuncNVar(X, B : PVector) : Float;
  begin
    case RegModel of
      REG_MULT : RegFuncNVar := FitMult.RegFunc(X, B);
    end;
  end;

  procedure DerivProc(RegFunc : TRegFunc; X, Y : Float; B, D : PVector);
  begin
    case RegModel of
      REG_FRAC  : FitFrac.DerivProc(X, Y, B, D);
      REG_EXPO  : FitExpo.DerivProc(X, B, D);
      REG_IEXPO : FitIExpo.DerivProc(X, B, D);
      REG_EXLIN : FitExLin.DerivProc(X, B, D);
      REG_POWER : FitPower.DerivProc(X, Y, B, D);
      REG_MICH  : FitMich.DerivProc(X, Y, B, D);
      REG_HILL  : FitHill.DerivProc(X, Y, B, D);
      REG_LOGIS : FitLogis.DerivProc(X, B, D);
      REG_PKA   : FitPKa.DerivProc(X, B, D);
    else
      NumDeriv(RegFunc, X, Y, B, D);
    end;
  end;

  procedure InitModel(Reg_Model, Var_Model : Integer; CstPar : PVector);
  begin
    RegModel := Reg_Model;
    VarModel := Var_Model;
    case RegModel of
      REG_MULT  : FitMult.InitModel(CstPar);
      REG_POL   : FitPoly.InitModel(CstPar);
      REG_FRAC  : FitFrac.InitModel(CstPar);
      REG_EXPO  : FitExpo.InitModel(CstPar);
      REG_LOGIS : FitLogis.InitModel(CstPar);
    end;
  end;

  function FitModel(Method : Integer;
                    X      : PVector;
                    U      : PMatrix;
                    Y, W   : PVector;
                    N      : Integer;
                    B      : PVector;
                    V      : PMatrix) : Integer;
{ --------------------------------------------------------------------
  Fits the regression model by unweighted linear least squares. For
  nonlinear models, this is only an approximate fit, to be refined by
  the nonlinear regression procedure WLSFit
  --------------------------------------------------------------------
  Input :  Method = 0 for unweighted regression, 1 for weighted
           X, U   = vector or matrix of independent variable(s)
           Y      = vector of dependent variable
           W      = weights
           N      = number of observations
  --------------------------------------------------------------------
  Output : B      = estimated regression parameters
           V      = unscaled variance-covariance matrix (for linear
                    and polynomial models only). The true matrix will
                    be Vr * V, where Vr is the residual variance.
  --------------------------------------------------------------------
  The function returns 0 if no error occurred
  -------------------------------------------------------------------- }
  begin
    case RegModel of
      REG_LIN   : FitModel := FitLin.FitModel(Method, X, Y, W, N, B, V);
      REG_MULT  : FitModel := FitMult.FitModel(Method, U, Y, W, N, B, V);
      REG_POL   : FitModel := FitPoly.FitModel(Method, X, Y, W, N, B, V);
      REG_FRAC  : FitModel := FitFrac.FitModel(Method, X, Y, W, N, B);
      REG_EXPO  : FitModel := FitExpo.FitModel(Method, X, Y, W, N, B);
      REG_IEXPO : FitModel := FitIExpo.FitModel(Method, X, Y, W, N, B);
      REG_EXLIN : FitModel := FitExLin.FitModel(X, Y, N, B);
      REG_POWER : FitModel := FitPower.FitModel(Method, X, Y, W, N, B);
      REG_MICH  : FitModel := FitMich.FitModel(Method, X, Y, W, N, B);
      REG_HILL  : FitModel := FitHill.FitModel(Method, X, Y, W, N, B);
      REG_LOGIS : FitModel := FitLogis.FitModel(Method, X, Y, W, N, B);
      REG_PKA   : FitModel := FitPKa.FitModel(X, Y, N, B);
    end;
  end;

  function WLSFit(X            : PVector;
                  U            : PMatrix;
                  Y            : PVector;
                  N            : Integer;
                  Init         : Boolean;
                  MaxIter      : Integer;
                  Tol          : Float;
                  Theta, B     : PVector;
                  B_min, B_max : PVector;
                  V            : PMatrix;
                  Ycalc, S     : PVector;
                  var Test     : TRegTest) : Integer;
  var
    Method : Integer;   { Regression method }
    W : PVector;        { Weights }
    Xk : PVector;       { Vector of variables for observation k }
    Sr : Float;         { Residual standard deviation }
    ErrCode : Integer;  { Error code }
    K : Integer;        { Loop variable }
  begin
    DimVector(W, N);
    DimVector(Xk, LastParam);

    { Determine regression method }
    if VarModel = VAR_CONST then Method := 0 else Method := 1;

    { Compute weights if necessary }
    if Method = 1 then
      for K := 1 to N do
        W^[K] := 1.0 / VarFunc(Y^[K], Theta);

    { Compute initial parameter estimates if necessary }
    if Init then
      ErrCode := FitModel(Method, X, U, Y, W, N, B, V)
    else
      ErrCode := 0;

    { Refine parameters if necessary }
    if not(RegModel in [REG_LIN, REG_MULT, REG_POL]) and
       (MaxIter > 0) and (ErrCode = 0) then
      if VarModel = VAR_CONST then
        ErrCode := NLFit({$IFDEF FPK}@{$ENDIF}RegFunc,
                         {$IFDEF FPK}@{$ENDIF}DerivProc,
                         X, Y, N, FirstParam, LastParam,
                         MaxIter, Tol, B, B_min, B_max, V)
      else
        ErrCode := WNLFit({$IFDEF FPK}@{$ENDIF}RegFunc,
                          {$IFDEF FPK}@{$ENDIF}DerivProc,
                          X, Y, W, N, FirstParam, LastParam,
                          MaxIter, Tol, B, B_min, B_max, V);

    if ErrCode = 0 then
      begin
        { Estimate Y values }
        if RegModel = REG_MULT then
          for K := 1 to N do
            begin
              CopyVectorFromCol(Xk, U, FirstParam, LastParam, K);
              Ycalc^[K] := RegFuncNVar(Xk, B);
            end
        else
          for K := 1 to N do
            Ycalc^[K] := RegFunc(X^[K], B);

        { Compute regression tests and update variance-covariance matrix }
        if VarModel = VAR_CONST then
          RegTest(Y, Ycalc, N, FirstParam, LastParam, V, Test)
        else
          WRegTest(Y, Ycalc, W, N, FirstParam, LastParam, V, Test);

        { Store residual variance in Theta^[0] }
        Theta^[0] := Test.Vr;

        { Compute standard deviations }
        Sr := Sqrt(Test.Vr);
        for K := 1 to N do
          S^[K] := Sr;
        if VarModel <> VAR_CONST then
          for K := 1 to N do
            S^[K] := S^[K] / Sqrt(W^[K]);
      end;

    DelVector(W, N);
    DelVector(Xk, LastParam);

    WLSFit := ErrCode;
  end;

  function VarFuncName : String;
  begin
    case VarModel of
      VAR_CONST : VarFuncName := 'v = e0';
      VAR_LIN   : VarFuncName := 'v = e0.(1 + e1.y)';
      VAR_POL2  : VarFuncName := 'v = e0.(1 + e1.y + e2.y^2)';
      VAR_POL3  : VarFuncName := 'v = e0.(1 + e1.y + e2.y^2 + e3.y^3)';
      VAR_EXPO  : VarFuncName := 'v = e0.exp(e1.y)';
      VAR_POWER : VarFuncName := 'v = e0.y^e1';
    end;
  end;

  function VarFunc(Y : Float; Theta : PVector) : Float;
  begin
    case VarModel of
      VAR_CONST : VarFunc := 1.0;
      VAR_LIN   : VarFunc := 1.0 + Theta^[1] * Y;
      VAR_POL2  : VarFunc := 1.0 + Y * (Theta^[1] + Theta^[2] * Y);
      VAR_POL3  : VarFunc := 1.0 + Y * (Theta^[1] + Y * (Theta^[2] + Theta^[3] * Y));
      VAR_EXPO  : VarFunc := Exp(Theta^[1] * Y);
      VAR_POWER : VarFunc := Power(Y, Theta^[1]);
    end;
  end;

  function LastVarParam : Integer;
  begin
    case VarModel of
      VAR_CONST : LastVarParam := 0;
      VAR_LIN   : LastVarParam := 1;
      VAR_POL2  : LastVarParam := 2;
      VAR_POL3  : LastVarParam := 3;
      VAR_EXPO  : LastVarParam := 1;
      VAR_POWER : LastVarParam := 1;
    end;
  end;

end.
