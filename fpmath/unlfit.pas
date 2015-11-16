{ ******************************************************************
  Nonlinear regression
  ****************************************************************** }

unit unlfit;

interface

uses
  utypes, ugausjor, umarq, ubfgs, usimplex,
  usimann, ugenalg, umcmc, ustrings;

procedure SetOptAlgo(Algo : TOptAlgo);
{ ----------------------------------------------------------------------
  Sets the optimization algorithm according to Algo, which must be
  NL_MARQ, NL_SIMP, NL_BFGS, NL_SA, NL_GA. Default is NL_MARQ
  ---------------------------------------------------------------------- }

procedure SetMaxParam(N : Byte);
{ ----------------------------------------------------------------------
  Sets the maximum number of regression parameters
  ---------------------------------------------------------------------- }

procedure SetParamBounds(I : Byte; ParamMin, ParamMax : Float);
{ ----------------------------------------------------------------------
  Sets the bounds on the I-th regression parameter
  ---------------------------------------------------------------------- }

procedure NLFit(RegFunc   : TRegFunc;
                DerivProc : TDerivProc;
                X, Y      : PVector;
                Lb, Ub    : Integer;
                MaxIter   : Integer;
                Tol       : Float;
                B         : PVector;
                FirstPar,
                LastPar   : Integer;
                V         : PMatrix);
{ ------------------------------------------------------------------
  Unweighted nonlinear regression
  ------------------------------------------------------------------
  Input parameters:  RegFunc   = regression function
                     DerivProc = procedure to compute derivatives
                     X, Y      = point coordinates
                     Lb, Ub    = array bounds
                     MaxIter   = max. number of iterations
                     Tol       = tolerance on parameters
                     B         = initial parameter values
                     FirstPar  = index of first regression parameter
                     LasttPar  = index of last regression parameter
  Output parameters: B         = fitted regression parameters
                     V         = inverse matrix
  ------------------------------------------------------------------ }

procedure WNLFit(RegFunc   : TRegFunc;
                 DerivProc : TDerivProc;
                 X, Y, S   : PVector;
                 Lb, Ub    : Integer;
                 MaxIter   : Integer;
                 Tol       : Float;
                 B         : PVector;
                 FirstPar,
                 LastPar   : Integer;
                 V         : PMatrix);
{ ----------------------------------------------------------------------
  Weighted nonlinear regression
  ----------------------------------------------------------------------
  S = standard deviations of observations
  Other parameters as in NLFit
  ---------------------------------------------------------------------- }

procedure SetMCFile(FileName : String);
{ ----------------------------------------------------------------------
  Set file for saving MCMC simulations
  ---------------------------------------------------------------------- }

procedure SimFit(RegFunc   : TRegFunc;
                 X, Y      : PVector;
                 Lb, Ub    : Integer;
                 B         : PVector;
                 FirstPar,
                 LastPar   : Integer;
                 V         : PMatrix);
{ ------------------------------------------------------------------
  Simulation of unweighted nonlinear regression by MCMC
  ------------------------------------------------------------------ }

procedure WSimFit(RegFunc   : TRegFunc;
                  X, Y, S   : PVector;
                  Lb, Ub    : Integer;
                  B         : PVector;
                  FirstPar,
                  LastPar   : Integer;
                  V         : PMatrix);
{ ----------------------------------------------------------------------
  Simulation of weighted nonlinear regression by MCMC
  ------------------------------------------------------------------ }


implementation

type
  TRegMode = (UNWEIGHTED, WEIGHTED);

const
  MAX_BOUND = 1.0E+6;   { Default parameter bound }
  MAX_FUNC  = 1.0E+30;  { Max. value for objective function
                          (used to prevent overflow) }
const
  MaxParam : Byte     = 10;          { Max. index of fitted parameter }
  OptAlgo  : TOptAlgo = NL_MARQ;     { Optimization algorithm }
  MCFile   : String   = 'mcmc.txt';  { File for saving MCMC simulations }

{ Global variables used by the nonlinear regression routines }
const
  gLb       : Integer = 0;    { Index of first point }
  gUb       : Integer = 0;    { Index of last point }
  gX        : PVector = nil;  { X coordinates }
  gY        : PVector = nil;  { Y coordinates }
  gW        : PVector = nil;  { Weights }
  gYcalc    : PVector = nil;  { Estimated Y values }
  gR        : PVector = nil;  { Residuals (Y - Ycalc) }
  gFirstPar : Integer = 0;    { Index of first fitted parameter }
  gLastPar  : Integer = 0;    { Index of last fitted parameter }
  gBmin     : PVector = nil;  { Lower bounds on parameters }
  gBmax     : PVector = nil;  { Higher bounds on parameters }
  gD        : PVector = nil;  { Derivatives of regression function }

var
  gRegFunc   : TRegFunc;    { Regression function }
  gDerivProc : TDerivProc;  { Derivation procedure }

  procedure SetOptAlgo(Algo : TOptAlgo);
  begin
    OptAlgo := Algo;
  end;

  procedure SetMaxParam(N : Byte);
  begin
    if N < 1 then Exit;

    DelVector(gBmin, MaxParam);
    DelVector(gBmax, MaxParam);

    DimVector(gBmin, N);
    DimVector(gBmax, N);

    MaxParam := N;
  end;

  procedure SetParamBounds(I : Byte; ParamMin, ParamMax : Float);
  begin
    if gBmin = nil then
      DimVector(gBmin, MaxParam);

    if gBmax = nil then
      DimVector(gBmax, MaxParam);

    if (I < 0) or (I > MaxParam) or (ParamMin >= ParamMax) then Exit;

    gBmin^[I] := ParamMin;
    gBmax^[I] := ParamMax;
  end;

  procedure SetGlobalVar(Mode      : TRegMode;
                         RegFunc   : TRegFunc;
                         DerivProc : TDerivProc;
                         X, Y, S   : PVector;
                         Lb, Ub    : Integer;
                         FirstPar,
                         LastPar   : Integer);

  { Checks the data and sets the global variables }

  var
    I, Npar, Npts : Integer;

  begin
    if LastPar > MaxParam then
      begin
        SetErrCode(NLMaxPar);
        Exit;
      end;

    Npts := Ub - Lb + 1;             { Number of points }
    Npar := LastPar - FirstPar + 1;  { Number of parameters }

    if Npts <= Npar then
      begin
        SetErrCode(MatErrDim);
        Exit;
      end;

    if Mode = WEIGHTED then
      for I := Lb to Ub do
        if S^[I] <= 0.0 then
          begin
            SetErrCode(MatSing);
            Exit;
          end;

    DelVector(gX, gUb);
    DelVector(gY, gUb);
    DelVector(gW, gUb);
    DelVector(gYcalc, gUb);
    DelVector(gR, gUb);

    DimVector(gX, Ub);
    DimVector(gY, Ub);
    DimVector(gW, Ub);
    DimVector(gYcalc, Ub);
    DimVector(gR, Ub);

    for I := Lb to Ub do
      begin
        gX^[I] := X^[I];
        gY^[I] := Y^[I];
      end;

    if Mode = WEIGHTED then
      for I := Lb to Ub do
        gW^[I] := 1.0 / Sqr(S^[I]);

    if gBmin = nil then
      DimVector(gBmin, MaxParam);

    if gBmax = nil then
      DimVector(gBmax, MaxParam);

    for I := FirstPar to LastPar do
      if gBmin^[I] >= gBmax^[I] then
        begin
          gBmin^[I] := - MAX_BOUND;
          gBmax^[I] :=   MAX_BOUND;
        end;

    DelVector(gD, gLastPar);
    DimVector(gD, LastPar);

    gLb := Lb;
    gUb := Ub;

    gFirstPar := FirstPar;
    gLastPar := LastPar;

    gRegFunc := RegFunc;
    gDerivProc := DerivProc;

    SetErrCode(MatOk);
  end;

  function OutOfBounds(B : PVector) : Boolean;
  { Check if the parameters are inside the bounds }
  var
    I   : Integer;
    OoB : Boolean;
  begin
    I := gFirstPar;
    repeat
      OoB := (B^[I] < gBmin^[I]) or (B^[I] > gBmax^[I]);
      Inc(I);
    until OoB or (I > gLastPar);
    OutOfBounds := OoB;
  end;

  function OLS_ObjFunc(B : PVector) : Float;
  { Objective function for unweighted nonlinear regression }
  var
    K : Integer;
    S : Float;
  begin
    if OutOfBounds(B) then
      begin
        OLS_ObjFunc := MAX_FUNC;
        Exit;
      end;

    S := 0.0;
    K := gLb;

    repeat
      gYcalc^[K] := gRegFunc(gX^[K], B);
      gR^[K] := gY^[K] - gYcalc^[K];
      S := S + Sqr(gR^[K]);
      Inc(K);
    until (K > gUb) or (S > MAX_FUNC);

    if S > MAX_FUNC then S := MAX_FUNC;
    OLS_ObjFunc := S;
  end;

  procedure OLS_Gradient(B, G : PVector);
  { Gradient for unweighted nonlinear regression }
  var
    I, K : Integer;  { Loop variables }
  begin
    { Initialization }
    for I := gFirstPar to gLastPar do
      G^[I] := 0.0;

    { Compute Gradient }
    for K := gLb to gUb do
      begin
        gDerivProc(gX^[K], gYcalc^[K], B, gD);
        for I := gFirstPar to gLastPar do
          G^[I] := G^[I] - gD^[I] * gR^[K];
      end;

    for I := gFirstPar to gLastPar do
      G^[I] := 2.0 * G^[I];
  end;

  procedure OLS_HessGrad(B, G : PVector; H : PMatrix);
  { Gradient and Hessian for unweighted nonlinear regression }
  var
    I, J, K : Integer;  { Loop variables }
  begin
    { Initializations }
    for I := gFirstPar to gLastPar do
      begin
        G^[I] := 0.0;
        for J := I to gLastPar do
          H^[I]^[J] := 0.0;
      end;

    { Compute Gradient & Hessian }
    for K := gLb to gUb do
      begin
        gDerivProc(gX^[K], gYcalc^[K], B, gD);
        for I := gFirstPar to gLastPar do
          begin
            G^[I] := G^[I] - gD^[I] * gR^[K];
            for J := I to gLastPar do
              H^[I]^[J] := H^[I]^[J] + gD^[I] * gD^[J];
          end;
      end;

    { Fill in symmetric matrix }
    for I := Succ(gFirstPar) to gLastPar do
      for J := gFirstPar to Pred(I) do
        H^[I]^[J] := H^[J]^[I];
  end;

  function WLS_ObjFunc(B : PVector) : Float;
  { Objective function for weighted nonlinear regression }
  var
    K : Integer;
    S : Float;
  begin
    if OutOfBounds(B) then
      begin
        WLS_ObjFunc := MAX_FUNC;
        Exit;
      end;

    S := 0.0;
    K := gLb;

    repeat
      gYcalc^[K] := gRegFunc(gX^[K], B);
      gR^[K] := gY^[K] - gYcalc^[K];
      S := S + gW^[K] * Sqr(gR^[K]);
      Inc(K);
    until (K > gUb) or (S > MAX_FUNC);

    if S > MAX_FUNC then S := MAX_FUNC;
    WLS_ObjFunc := S;
  end;

  procedure WLS_Gradient(B, G : PVector);
  { Gradient for weighted nonlinear regression }
  var
    I, K : Integer;  { Loop variables }
    WR   : Float;    { Weighted residual }
  begin
    { Initialization }
    for I := gFirstPar to gLastPar do
      G^[I] := 0.0;

    { Compute Gradient }
    for K := gLb to gUb do
      begin
        WR := gW^[K] * gR^[K];
        gDerivProc(gX^[K], gYcalc^[K], B, gD);
        for I := gFirstPar to gLastPar do
          G^[I] := G^[I] - gD^[I] * WR;
      end;

    for I := gFirstPar to gLastPar do
      G^[I] := 2.0 * G^[I];
  end;

  procedure WLS_HessGrad(B, G : PVector; H : PMatrix);
  { Gradient and Hessian for weighted nonlinear regression }
  var
    I, J, K : Integer;  { Loop variables }
    WR, WD  : Float;    { Weighted residual and derivative }
  begin
    { Initializations }
    for I := gFirstPar to gLastPar do
      begin
        G^[I] := 0.0;
        for J := I to gLastPar do
          H^[I]^[J] := 0.0;
      end;

    { Compute Gradient & Hessian }
    for K := gLb to gUb do
      begin
        WR := gW^[K] * gR^[K];
        gDerivProc(gX^[K], gYcalc^[K], B, gD);
        for I := gFirstPar to gLastPar do
          begin
            G^[I] := G^[I] - gD^[I] * WR;
            WD := gW^[K] * gD^[I];
            for J := I to gLastPar do
              H^[I]^[J] := H^[I]^[J] + WD * gD^[J];
          end;
      end;

    { Fill in symmetric matrix }
    for I := Succ(gFirstPar) to gLastPar do
      for J := gFirstPar to Pred(I) do
        H^[I]^[J] := H^[J]^[I];
  end;

  procedure GenNLFit(Mode      : TRegMode;
                     RegFunc   : TRegFunc;
                     DerivProc : TDerivProc;
                     X, Y, S   : PVector;
                     Lb, Ub    : Integer;
                     MaxIter   : Integer;
                     Tol       : Float;
                     B         : PVector;
                     FirstPar,
                     LastPar   : Integer;
                     V         : PMatrix);
  { --------------------------------------------------------------------
    General nonlinear regression routine
    -------------------------------------------------------------------- }
  var
    F_min    : Float;      { Value of objective function at minimum }
    G        : PVector;    { Gradient vector }
    Det      : Float;      { Determinant of Hessian matrix }
    ObjFunc  : TFuncNVar;  { Objective function }
    GradProc : TGradient;  { Procedure to compute gradient }
    HessProc : THessGrad;  { Procedure to compute gradient and hessian }

  begin
    SetGlobalVar(Mode, RegFunc, DerivProc, X, Y, S,
                 Lb, Ub, FirstPar, LastPar);

    if MathErr <> MatOk then Exit;

    if Mode = UNWEIGHTED then
      begin
        ObjFunc  := {$IFDEF FPC}@{$ENDIF}OLS_ObjFunc;
        GradProc := {$IFDEF FPC}@{$ENDIF}OLS_Gradient;
        HessProc := {$IFDEF FPC}@{$ENDIF}OLS_HessGrad;
      end
    else
      begin
        ObjFunc  := {$IFDEF FPC}@{$ENDIF}WLS_ObjFunc;
        GradProc := {$IFDEF FPC}@{$ENDIF}WLS_Gradient;
        HessProc := {$IFDEF FPC}@{$ENDIF}WLS_HessGrad;
      end;

    DimVector(G, LastPar);

    case OptAlgo of
      NL_MARQ : Marquardt(ObjFunc, HessProc, B, FirstPar, LastPar,
                          MaxIter, Tol, F_min, G, V, Det);
      NL_SIMP : Simplex(ObjFunc, B, FirstPar, LastPar,
                        MaxIter, Tol, F_min);
      NL_BFGS : BFGS(ObjFunc, GradProc, B, FirstPar, LastPar,
                     MaxIter, Tol, F_min, G, V);
      NL_SA   : SimAnn(ObjFunc, B, gBmin, gBmax, FirstPar, LastPar, F_min);
      NL_GA   : GenAlg(ObjFunc, B, gBmin, gBmax, FirstPar, LastPar, F_min);
    end;

    if (OptAlgo <> NL_MARQ) and (MathErr = MatOk) then
      begin
        { Compute the Hessian matrix and its inverse }
        HessProc(B, G, V);
        GaussJordan(V, FirstPar, LastPar, LastPar, Det);
      end;

    DelVector(G, LastPar);
  end;

  procedure NLFit(RegFunc   : TRegFunc;
                  DerivProc : TDerivProc;
                  X, Y      : PVector;
                  Lb, Ub    : Integer;
                  MaxIter   : Integer;
                  Tol       : Float;
                  B         : PVector;
                  FirstPar,
                  LastPar   : Integer;
                  V         : PMatrix);
  begin
    GenNLFit(UNWEIGHTED, RegFunc, DerivProc, X, Y, nil, Lb, Ub,
             MaxIter, Tol, B, FirstPar, LastPar, V);
  end;

  procedure WNLFit(RegFunc   : TRegFunc;
                   DerivProc : TDerivProc;
                   X, Y, S   : PVector;
                   Lb, Ub    : Integer;
                   MaxIter   : Integer;
                   Tol       : Float;
                   B         : PVector;
                   FirstPar,
                   LastPar   : Integer;
                   V         : PMatrix);
  begin
    GenNLFit(WEIGHTED, RegFunc, DerivProc, X, Y, S, Lb, Ub,
             MaxIter, Tol, B, FirstPar, LastPar, V);
  end;

  procedure SetMCFile(FileName : String);
  begin
    MCFile := FileName;
  end;

  procedure GenSimFit(Mode      : TRegMode;
                      RegFunc   : TRegFunc;
                      X, Y, S   : PVector;
                      Lb, Ub    : Integer;
                      B         : PVector;
                      FirstPar,
                      LastPar   : Integer;
                      V         : PMatrix);
  var
    ObjFunc  : TFuncNVar;  { Objective function }
    NCycles,
    MaxSim,
    SavedSim : Integer;    { Metropolis-Hastings parameters }
    Xmat     : PMatrix;    { Matrix of simulated parameters }
    F_min    : Float;      { Value of objective function at minimum }
    B_min    : PVector;    { Parameter values at minimum }
    R        : Float;      { Range of parameter values }
    I, J     : Integer;    { Loop variables }
    F        : Text;       { File for storing MCMC simulations }

  begin
    SetGlobalVar(Mode, RegFunc, nil, X, Y, S,
                 Lb, Ub, FirstPar, LastPar);

    if MathErr <> MatOk then Exit;

    { Initialize variance-covariance matrix }
    for I := FirstPar to LastPar do
      begin
        R := gBmax^[I] - gBmin^[I];
        B^[I] := gBmin^[I] + 0.5 * R;
        for J := FirstPar to LastPar do
          if I = J then
            { The parameter range is assumed to cover 6 SD's }
            V^[I]^[J] := R * R / 36.0
          else
            V^[I]^[J] := 0.0;
      end;

    if Mode = UNWEIGHTED then
      ObjFunc := {$IFDEF FPC}@{$ENDIF}OLS_ObjFunc
    else
      ObjFunc := {$IFDEF FPC}@{$ENDIF}WLS_ObjFunc;

    GetMHParams(NCycles, MaxSim, SavedSim);

    DimMatrix(Xmat, SavedSim, LastPar);
    DimVector(B_min, LastPar);

    Hastings(ObjFunc, 2.0, B, V, FirstPar, LastPar, Xmat, B_min, F_min);

    if MathErr = MatOk then  { Save simulations }
      begin
        Assign(F, MCFile);
        Rewrite(F);
        for I := 1 to SavedSim do
          begin
            Write(F, IntStr(I));
            for J := FirstPar to LastPar do
              Write(F, FloatStr(Xmat^[I]^[J]));
            Writeln(F);
          end;
        Close(F);
      end;

    DelMatrix(Xmat, SavedSim, LastPar);
  end;

  procedure SimFit(RegFunc   : TRegFunc;
                   X, Y      : PVector;
                   Lb, Ub    : Integer;
                   B         : PVector;
                   FirstPar,
                   LastPar   : Integer;
                   V         : PMatrix);
  begin
    GenSimFit(UNWEIGHTED, RegFunc, X, Y, nil, Lb, Ub, B, FirstPar, LastPar, V);
  end;

  procedure WSimFit(RegFunc   : TRegFunc;
                    X, Y, S   : PVector;
                    Lb, Ub    : Integer;
                    B         : PVector;
                    FirstPar,
                    LastPar   : Integer;
                    V         : PMatrix);
  begin
    GenSimFit(WEIGHTED, RegFunc, X, Y, S, Lb, Ub, B, FirstPar, LastPar, V);
  end;

end.
