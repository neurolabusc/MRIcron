{ **********************************************************************
  *                            Unit OPTIM.PAS                          *
  *                              Version 2.1                           *
  *                       (c) J. Debord, June 2001                     *
  **********************************************************************
  This unit implements the following methods for function minimization:

    * Golden search for a function of one variable
    * Simplex, Marquardt, BFGS for a function of several variables
  **********************************************************************
  References:
  1) 'Numerical Recipes' by Press et al.
  2) D. W. MARQUARDT, J. Soc. Indust. Appl. Math., 1963, 11, 431-441
  3) J. A. NELDER & R. MEAD, Comput. J., 1964, 7, 308-313
  4) R. O'NEILL, Appl. Statist., 1971, 20, 338-345
  ********************************************************************** }

unit Optim;

interface

uses
  FMath, Matrices;

{ **********************************************************************
  Error codes
  ********************************************************************** }

const
  OPT_OK         =   0;  { No error }
  OPT_SING       = - 1;  { Singular hessian matrix }
  OPT_BIG_LAMBDA = - 2;  { Too high Marquardt's parameter }
  OPT_NON_CONV   = - 3;  { Non-convergence }

{ **********************************************************************
  Functional types
  ********************************************************************** }

type
  { Function of several variables }
  TFuncNVar = function(X : PVector) : Float;

  { Procedure to compute gradient vector }
  TGradient = procedure(Func           : TFuncNVar;
                        X              : PVector;
                        Lbound, Ubound : Integer;
                        G              : PVector);

  { Procedure to compute gradient vector and hessian matrix }
  THessGrad = procedure(Func           : TFuncNVar;
                        X              : PVector;
                        Lbound, Ubound : Integer;
                        G              : PVector;
                        H              : PMatrix);

{ **********************************************************************
  Log file
  ********************************************************************** }

const
  WriteLogFile : Boolean = False;        { Write iteration info to log file }
  LogFileName  : String  = 'optim.log';  { Name of log file }

{ **********************************************************************
  Minimization routines
  ********************************************************************** }

function GoldSearch(Func           : TFunc;
                    A, B           : Float;
                    MaxIter        : Integer;
                    Tol            : Float;
                    var Xmin, Ymin : Float) : Integer;
{ ----------------------------------------------------------------------
  Performs a golden search for the minimum of function Func
  ----------------------------------------------------------------------
  Input parameters  : Func    = objective function
                      A, B    = two points near the minimum
                      MaxIter = maximum number of iterations
                      Tol     = required precision (should not be less than
                                the square root of the machine precision)
  ----------------------------------------------------------------------
  Output parameters : Xmin, Ymin = coordinates of minimum
  ----------------------------------------------------------------------
  Possible results  : OPT_OK
                      OPT_NON_CONV
  ---------------------------------------------------------------------- }

function LinMin(Func           : TFuncNVar;
                X, DeltaX      : PVector;
                Lbound, Ubound : Integer;
                MaxIter        : Integer;
                Tol            : Float;
                var F_min      : Float) : Integer;

{ ----------------------------------------------------------------------
  Minimizes function Func from point X in the direction specified by
  DeltaX
  ----------------------------------------------------------------------
  Input parameters  : Func    = objective function
                      X       = initial minimum coordinates
                      DeltaX  = direction in which minimum is searched
                      Lbound,
                      Ubound  = indices of first and last variables
                      MaxIter = maximum number of iterations
                      Tol     = required precision
  ----------------------------------------------------------------------
  Output parameters : X     = refined minimum coordinates
                      F_min = function value at minimum
  ----------------------------------------------------------------------
  Possible results  : OPT_OK
                      OPT_NON_CONV
  ---------------------------------------------------------------------- }

function Simplex(Func           : TFuncNVar;
                 X              : PVector;
                 Lbound, Ubound : Integer;
                 MaxIter        : Integer;
                 Tol            : Float;
                 var F_min      : Float) : Integer;
{ ----------------------------------------------------------------------
  Minimization of a function of several variables by the simplex method
  of Nelder and Mead
  ----------------------------------------------------------------------
  Input parameters  : Func    = objective function
                      X       = initial minimum coordinates
                      Lbound,
                      Ubound  = indices of first and last variables
                      MaxIter = maximum number of iterations
                      Tol     = required precision
  ----------------------------------------------------------------------
  Output parameters : X     = refined minimum coordinates
                      F_min = function value at minimum
  ----------------------------------------------------------------------
  Possible results : OPT_OK
                     OPT_NON_CONV
  ---------------------------------------------------------------------- }

procedure NumGradient(Func           : TFuncNVar;
                      X              : PVector;
                      Lbound, Ubound : Integer;
                      G              : PVector);
{ ----------------------------------------------------------------------
  Computes the gradient vector of a function of several variables by
  numerical differentiation
  ----------------------------------------------------------------------
  Input parameters  : Func    = function of several variables
                      X       = vector of variables
                      Lbound,
                      Ubound  = indices of first and last variables
  ----------------------------------------------------------------------
  Output parameter  : G       = gradient vector
  ---------------------------------------------------------------------- }

procedure NumHessGrad(Func           : TFuncNVar;
                      X              : PVector;
                      Lbound, Ubound : Integer;
                      G              : PVector;
                      H              : PMatrix);
{ ----------------------------------------------------------------------
  Computes gradient vector & hessian matrix by numerical differentiation
  ----------------------------------------------------------------------
  Input parameters  : as in NumGradient
  ----------------------------------------------------------------------
  Output parameters : G = gradient vector
                      H = hessian matrix
  ---------------------------------------------------------------------- }

function Marquardt(Func           : TFuncNVar;
                   HessGrad       : THessGrad;
                   X              : PVector;
                   Lbound, Ubound : Integer;
                   MaxIter        : Integer;
                   Tol            : Float;
                   var F_min      : Float;
                   H_inv          : PMatrix) : Integer;
{ ----------------------------------------------------------------------
  Minimization of a function of several variables by Marquardt's method
  ----------------------------------------------------------------------
  Input parameters  : Func     = objective function
                      HessGrad = procedure to compute gradient & hessian
                      X        = initial minimum coordinates
                      Lbound,
                      Ubound   = indices of first and last variables
                      MaxIter  = maximum number of iterations
                      Tol      = required precision
  ----------------------------------------------------------------------
  Output parameters : X     = refined minimum coordinates
                      F_min = function value at minimum
                      H_inv = inverse hessian matrix
  ----------------------------------------------------------------------
  Possible results  : OPT_OK
                      OPT_SING
                      OPT_BIG_LAMBDA
                      OPT_NON_CONV
  ---------------------------------------------------------------------- }

function BFGS(Func           : TFuncNVar;
              Gradient       : TGradient;
              X              : PVector;
              Lbound, Ubound : Integer;
              MaxIter        : Integer;
              Tol            : Float;
              var F_min      : Float;
              H_inv          : PMatrix) : Integer;
{ ----------------------------------------------------------------------
  Minimization of a function of several variables by the
  Broyden-Fletcher-Goldfarb-Shanno method
  ----------------------------------------------------------------------
  Parameters : Gradient = procedure to compute gradient vector
               Other parameters as in Marquardt
  ----------------------------------------------------------------------
  Possible results : OPT_OK
                     OPT_NON_CONV
  ---------------------------------------------------------------------- }

implementation

var
  Eps              : Float;      { Fractional increment for numer. derivation }
  X1               : PVector;    { Initial point for line minimization }
  DeltaX1          : PVector;    { Direction for line minimization }
  Lbound1, Ubound1 : Integer;    { Bounds of X1 and DeltaX1 }
  LinObjFunc       : TFuncNVar;  { Objective function for line minimization }
  LogFile          : Text;       { Stores the result of each minimization step }


  procedure MinBrack(Func : TFunc; var A, B, C, Fa, Fb, Fc : Float);
{ ----------------------------------------------------------------------
  Given two points (A, B) this procedure finds a triplet (A, B, C)
  such that:

  1) A < B < C
  2) A, B, C are within the golden ratio
  3) Func(B) < Func(A) and Func(B) < Func(C).

  The corresponding function values are returned in Fa, Fb, Fc
  ---------------------------------------------------------------------- }

  begin
    if A > B then
      FSwap(A, B);
    Fa := Func(A);
    Fb := Func(B);
    if Fb > Fa then
      begin
        FSwap(A, B);
        FSwap(Fa, Fb);
      end;
    C := B + GOLD * (B - A);
    Fc := Func(C);
    while Fc < Fb do
      begin
        A := B;
        B := C;
        Fa := Fb;
        Fb := Fc;
        C := B + GOLD * (B - A);
        Fc := Func(C);
      end;
    if A > C then
      begin
        FSwap(A, C);
        FSwap(Fa, Fc);
      end;
  end;

  function GoldSearch(Func           : TFunc;
                      A, B           : Float;
                      MaxIter        : Integer;
                      Tol            : Float;
                      var Xmin, Ymin : Float) : Integer;
  var
    C, Fa, Fb, Fc, F1, F2, MinTol, X0, X1, X2, X3 : Float;
    Iter : Integer;
  begin
    MinTol := Sqrt(MACHEP);
    if Tol < MinTol then Tol := MinTol;
    MinBrack(Func, A, B, C, Fa, Fb, Fc);
    X0 := A;
    X3 := C;
    if (C - B) > (B - A) then
      begin
        X1 := B;
        X2 := B + CGOLD * (C - B);
        F1 := Fb;
        F2 := Func(X2);
      end
    else
      begin
        X1 := B - CGOLD * (B - A);
        X2 := B;
        F1 := Func(X1);
        F2 := Fb;
      end;
    Iter := 0;
    while (Iter <= MaxIter) and (Abs(X3 - X0) > Tol * (Abs(X1) + Abs(X2))) do
      if F2 < F1 then
        begin
          X0 := X1;
          X1 := X2;
          F1 := F2;
          X2 := X1 + CGOLD * (X3 - X1);
          F2 := Func(X2);
          Inc(Iter);
        end
      else
        begin
          X3 := X2;
          X2 := X1;
          F2 := F1;
          X1 := X2 - CGOLD * (X2 - X0);
          F1 := Func(X1);
          Inc(Iter);
        end;
    if F1 < F2 then
      begin
        Xmin := X1;
        Ymin := F1;
      end
    else
      begin
        Xmin := X2;
        Ymin := F2;
      end;
    if Iter > MaxIter then
      GoldSearch := OPT_NON_CONV
    else
      GoldSearch := OPT_OK;
  end;

  procedure CreateLogFile;
  begin
    Assign(LogFile, LogFileName);
    Rewrite(LogFile);
  end;

  function Simplex(Func           : TFuncNVar;
                   X              : PVector;
                   Lbound, Ubound : Integer;
                   MaxIter        : Integer;
                   Tol            : Float;
                   var F_min      : Float) : Integer;
  const
    STEP = 1.50;  { Step used to construct the initial simplex }
  var
    P             : PMatrix;  { Simplex coordinates }
    F             : PVector;  { Function values }
    Pbar          : PVector;  { Centroid coordinates }
    Pstar, P2star : PVector;  { New vertices }
    Ystar, Y2star : Float;    { New function values }
    F0            : Float;    { Function value at minimum }
    N             : Integer;  { Number of parameters }
    M             : Integer;  { Index of last vertex }
    L, H          : Integer;  { Vertices with lowest & highest F values }
    I, J          : Integer;  { Loop variables }
    Iter          : Integer;  { Iteration count }
    Corr, MaxCorr : Float;    { Corrections }
    Sum           : Float;
    Flag          : Boolean;

    procedure UpdateSimplex(Y : Float; Q : PVector);
    { Update "worst" vertex and function value }
    begin
      F^[H] := Y;
      CopyVector(P^[H], Q, Lbound, Ubound);
    end;

  begin
    if WriteLogFile then
      begin
        CreateLogFile;
        WriteLn(LogFile, 'Simplex');
        WriteLn(LogFile, 'Iter         F');
      end;

    N := Ubound - Lbound + 1;
    M := Succ(Ubound);

    DimMatrix(P, M, Ubound);
    DimVector(F, M);
    DimVector(Pbar, Ubound);
    DimVector(Pstar, Ubound);
    DimVector(P2star, Ubound);

    Iter := 1;
    F0 := MAXNUM;

    { Construct initial simplex }
    for I := Lbound to M do
      CopyVector(P^[I], X, Lbound, Ubound);
    for I := Lbound to Ubound do
      P^[I]^[I] := P^[I]^[I] * STEP;

    { Evaluate function at each vertex }
    for I := Lbound to M do
      F^[I] := Func(P^[I]);

    repeat
      { Find vertices (L,H) having the lowest and highest
        function values, i.e. "best" and "worst" vertices }
      L := Lbound;
      H := Lbound;
      for I := Succ(Lbound) to M do
        if F^[I] < F^[L] then
          L := I
        else if F^[I] > F^[H] then
          H := I;
      if F^[L] < F0 then
        F0 := F^[L];

      if WriteLogFile then
        WriteLn(LogFile, Iter:4, '   ', F0:12);

      { Find centroid of points other than P(H) }
      for J := Lbound to Ubound do
        begin
          Sum := 0.0;
          for I := Lbound to M do
            if I <> H then Sum := Sum + P^[I]^[J];
          Pbar^[J] := Sum / N;
        end;

      { Reflect worst vertex through centroid }
      for J := Lbound to Ubound do
        Pstar^[J] := 2.0 * Pbar^[J] - P^[H]^[J];
      Ystar := Func(Pstar);

      { If reflection successful, try extension }
      if Ystar < F^[L] then
        begin
          for J := Lbound to Ubound do
            P2star^[J] := 3.0 * Pstar^[J] - 2.0 * Pbar^[J];
          Y2star := Func(P2star);

          { Retain extension or contraction }
          if Y2star < F^[L] then
            UpdateSimplex(Y2star, P2star)
          else
            UpdateSimplex(Ystar, Pstar);
        end
      else
        begin
          I := Lbound;
          Flag := False;
          repeat
            if (I <> H) and (F^[I] > Ystar) then Flag := True;
            Inc(I);
          until Flag or (I > M);
          if Flag then
            UpdateSimplex(Ystar, Pstar)
          else
            begin
              { Contraction on the reflection side of the centroid }
              if Ystar <= F^[H] then
                UpdateSimplex(Ystar, Pstar);

              { Contraction on the opposite side of the centroid }
              for J := Lbound to Ubound do
                P2star^[J] := 0.5 * (P^[H]^[J] + Pbar^[J]);
              Y2star := Func(P2star);
              if Y2star <= F^[H] then
                UpdateSimplex(Y2star, P2star)
              else
                { Contract whole simplex }
                for I := Lbound to M do
                  for J := Lbound to Ubound do
                    P^[I]^[J] := 0.5 * (P^[I]^[J] + P^[L]^[J]);
            end;
        end;

      { Test convergence }
      MaxCorr := 0.0;
      for J := Lbound to Ubound do
        begin
          Corr := Abs(P^[H]^[J] - P^[L]^[J]);
          if Corr > MaxCorr then MaxCorr := Corr;
        end;
      Inc(Iter);
    until (MaxCorr < Tol) or (Iter > MaxIter);

    CopyVector(X, P^[L], Lbound, Ubound);
    F_min := F^[L];

    DelMatrix(P, M, Ubound);
    DelVector(F, M);
    DelVector(Pbar, Ubound);
    DelVector(Pstar, Ubound);
    DelVector(P2star, Ubound);

    if WriteLogFile then
      Close(LogFile);

    if Iter > MaxIter then
      Simplex := OPT_NON_CONV
    else
      Simplex := OPT_OK;
  end;

  {$F+}
  function F1dim(R : Float) : Float;
{ ----------------------------------------------------------------------
  Function used by LinMin to find the minimum of the objective function
  LinObjFunc in the direction specified by the global variables X1 and
  DeltaX1. R is the step in this direction.
  ---------------------------------------------------------------------- }
  const
    Xt : PVector = nil;
  var
    I : Integer;
  begin
    if Xt = nil then
      DimVector(Xt, Ubound1);
    for I := Lbound1 to Ubound1 do
      Xt^[I] := X1^[I] + R * DeltaX1^[I];
    F1dim := LinObjFunc(Xt);
  end;
  {$F-}

  function LinMin(Func           : TFuncNVar;
                  X, DeltaX      : PVector;
                  Lbound, Ubound : Integer;
                  MaxIter        : Integer;
                  Tol            : Float;
                  var F_min      : Float) : Integer;
  var
    I, ErrCode : Integer;
    R : Float;
  begin
    { Redimension global vectors }
    DelVector(X1, Ubound1);
    DelVector(DeltaX1, Ubound1);
    DimVector(X1, Ubound);
    DimVector(DeltaX1, Ubound);

    Lbound1 := Lbound;
    Ubound1 := Ubound;

    { Initialize global variables }
    LinObjFunc := Func;
    for I := Lbound to Ubound do
      begin
        X1^[I] := X^[I];
        DeltaX1^[I] := DeltaX^[I]
      end;

    { Perform golden search }
    ErrCode := GoldSearch({$IFDEF FPK}@{$ENDIF}F1dim,
                          0.0, 1.0, MaxIter, Tol, R, F_min);

    { Update variables }
    if ErrCode = OPT_OK then
      for I := Lbound to Ubound do
        X^[I] := X^[I] + R * DeltaX^[I];

    LinMin := ErrCode;
  end;

  {$F+}
  procedure NumGradient(Func           : TFuncNVar;
                        X              : PVector;
                        Lbound, Ubound : Integer;
                        G              : PVector);
  var
    Temp, Delta, Fplus, Fminus : Float;
    I                          : Integer;
  begin
    for I := Lbound to Ubound do
      begin
        Temp := X^[I];
        if Temp <> 0.0 then Delta := Eps * Abs(Temp) else Delta := Eps;
        X^[I] := Temp - Delta;
        Fminus := Func(X);
        X^[I] := Temp + Delta;
        Fplus := Func(X);
        G^[I] := (Fplus - Fminus) / (2.0 * Delta);
        X^[I] := Temp;
      end;
  end;
  {$F-}

  {$F+}
  procedure NumHessGrad(Func           : TFuncNVar;
                        X              : PVector;
                        Lbound, Ubound : Integer;
                        G              : PVector;
                        H              : PMatrix);
  var
    Delta, Xminus, Xplus, Fminus, Fplus : PVector;
    Temp1, Temp2, F, F2plus             : Float;
    I, J                                : Integer;
  begin
    DimVector(Delta, Ubound);   { Increments   }
    DimVector(Xminus, Ubound);  { X - Delta    }
    DimVector(Xplus, Ubound);   { X + Delta    }
    DimVector(Fminus, Ubound);  { F(X - Delta) }
    DimVector(Fplus, Ubound);   { F(X + Delta) }

    F := Func(X);

    for I := Lbound to Ubound do
      begin
        if X^[I] <> 0.0 then
          Delta^[I] := Eps * Abs(X^[I])
        else
          Delta^[I] := Eps;
        Xplus^[I] := X^[I] + Delta^[I];
        Xminus^[I] := X^[I] - Delta^[I];
      end;

    for I := Lbound to Ubound do
      begin
        Temp1 := X^[I];
        X^[I] := Xminus^[I];
        Fminus^[I] := Func(X);
        X^[I] := Xplus^[I];
        Fplus^[I] := Func(X);
        X^[I] := Temp1;
      end;

    for I := Lbound to Ubound do
      begin
        G^[I] := (Fplus^[I] - Fminus^[I]) / (2.0 * Delta^[I]);
        H^[I]^[I] := (Fplus^[I] + Fminus^[I] - 2.0 * F) / Sqr(Delta^[I]);
      end;

    for I := Lbound to Pred(Ubound) do
      begin
        Temp1 := X^[I];
        X^[I] := Xplus^[I];
        for J := Succ(I) to Ubound do
          begin
            Temp2 := X^[J];
            X^[J] := Xplus^[J];
            F2plus := Func(X);
            H^[I]^[J] := (F2plus - Fplus^[I] - Fplus^[J] + F) / (Delta^[I] * Delta^[J]);
            H^[J]^[I] := H^[I]^[J];
            X^[J] := Temp2;
          end;
        X^[I] := Temp1;
      end;

    DelVector(Delta, Ubound);
    DelVector(Xminus, Ubound);
    DelVector(Xplus, Ubound);
    DelVector(Fminus, Ubound);
    DelVector(Fplus, Ubound);
  end;
  {$F-}

  function ParamConv(OldX, X        : PVector;
                     Lbound, Ubound : Integer;
                     Tol            : Float) : Boolean;
{ ----------------------------------------------------------------------
  Check for convergence on parameters
  ---------------------------------------------------------------------- }
  var
    I : Integer;
    Conv : Boolean;
  begin
    I := Lbound;
    Conv := True;
    repeat
      Conv := Conv and (Abs(X^[I] - OldX^[I]) < FMax(Tol, Tol * Abs(OldX^[I])));
      Inc(I);
    until (Conv = False) or (I > Ubound);
    ParamConv := Conv;
  end;

  function Marquardt(Func           : TFuncNVar;
                     HessGrad       : THessGrad;
                     X              : PVector;
                     Lbound, Ubound : Integer;
                     MaxIter        : Integer;
                     Tol            : Float;
                     var F_min      : Float;
                     H_inv          : PMatrix) : Integer;
  const
    LAMBDA0   = 1.0E-2;   { Initial lambda value }
    LAMBDAMAX = 1.0E+3;   { Highest lambda value }
    FTOL      = 1.0E-10;  { Tolerance on function decrease }
  var
    Lambda,
    Lambda1   : Float;    { Marquardt's lambda }
    I         : Integer;  { Loop variable }
    OldX      : PVector;  { Old parameters }
    G         : PVector;  { Gradient vector }
    H         : PMatrix;  { Hessian matrix }
    A         : PMatrix;  { Modified Hessian matrix }
    DeltaX    : PVector;  { New search direction }
    F1        : Float;    { New minimum }
    Lambda_Ok : Boolean;  { Successful Lambda decrease }
    Conv      : Boolean;  { Convergence reached }
    Done      : Boolean;  { Iterations done }
    Iter      : Integer;  { Iteration count }
    ErrCode   : Integer;  { Error code }
  begin
    if WriteLogFile then
      begin
        CreateLogFile;
        WriteLn(LogFile, 'Marquardt');
        WriteLn(LogFile, 'Iter         F            Lambda');
      end;

    Lambda := LAMBDA0;
    ErrCode := OPT_OK;

    DimVector(OldX, Ubound);
    DimVector(G, Ubound);
    DimMatrix(H, Ubound, Ubound);
    DimMatrix(A, Ubound, Ubound);
    DimVector(DeltaX, Ubound);

    F_min := Func(X);    { Initial function value }
    LinObjFunc := Func;  { Function for line minimization }

    Iter := 1;
    Conv := False;
    Done := False;

    repeat
      if WriteLogFile then
        WriteLn(LogFile, Iter:4, '   ', F_min:12, '   ', Lambda:12);

      { Save current parameters }
      CopyVector(OldX, X, Lbound, Ubound);

      { Compute Gradient and Hessian }
      HessGrad(Func, X, Lbound, Ubound, G, H);
      CopyMatrix(A, H, Lbound, Lbound, Ubound, Ubound);

      { Change sign of gradient }
      for I := Lbound to Ubound do
        G^[I] := - G^[I];

      if Conv then  { Newton-Raphson iteration }
        begin
          ErrCode := GaussJordan(A, G, Lbound, Ubound, H_inv, DeltaX);
          if ErrCode = MAT_OK then
            for I := Lbound to Ubound do
              X^[I] := OldX^[I] + DeltaX^[I];
          Done := True;
        end
      else          { Marquardt iteration }
        begin
          repeat
            { Multiply each diagonal term of H by (1 + Lambda) }
            Lambda1 := 1.0 + Lambda;
            for I := Lbound to Ubound do
              A^[I]^[I] := Lambda1 * H^[I]^[I];

            ErrCode := GaussJordan(A, G, Lbound, Ubound, H_inv, DeltaX);

            if ErrCode = MAT_OK then
              begin
                { Initialize parameters }
                CopyVector(X, OldX, Lbound, Ubound);

                { Minimize in the direction specified by DeltaX }
                ErrCode := LinMin(Func, X, DeltaX,
                                  Lbound, Ubound, 100, 0.01, F1);

                { Check that the function has decreased. Otherwise
                  increase Lambda, without exceeding LAMBDAMAX }
                Lambda_Ok := (F1 - F_min) < F_min * FTOL;
                if not Lambda_Ok then Lambda := 10.0 * Lambda;
                if Lambda > LAMBDAMAX then ErrCode := OPT_BIG_LAMBDA;
              end;
          until Lambda_Ok or (ErrCode <> MAT_OK);

          { Check for convergence }
          Conv := ParamConv(OldX, X, Lbound, Ubound, Tol);

          { Prepare next iteration }
          Lambda := 0.1 * Lambda;
          F_min := F1;
        end;

      Inc(Iter);
      if Iter > MaxIter then ErrCode := OPT_NON_CONV;
    until Done or (ErrCode <> OPT_OK);

    DelVector(OldX, Ubound);
    DelVector(G, Ubound);
    DelMatrix(H, Ubound, Ubound);
    DelMatrix(A, Ubound, Ubound);
    DelVector(DeltaX, Ubound);

    if WriteLogFile then
      Close(LogFile);

    if ErrCode = MAT_SINGUL then ErrCode := OPT_SING;
    Marquardt := ErrCode;
  end;

  function BFGS(Func           : TFuncNVar;
                Gradient       : TGradient;
                X              : PVector;
                Lbound, Ubound : Integer;
                MaxIter        : Integer;
                Tol            : Float;
                var F_min      : Float;
                H_inv          : PMatrix) : Integer;
  var
    I, J, Iter, ErrCode : Integer;
    DeltaXmax, Gmax, P1, P2, R1, R2 : Float;
    OldX, DeltaX, dX, G, OldG, dG, HdG, R1dX, R2HdG, U, P2U : PVector;
    Conv : Boolean;

  function AbsMax(V : PVector; Lbound, Ubound : Integer) : Float;
  { Returns the component with maximum absolute value }
  var
    I : Integer;
    AbsV : PVector;
  begin
    DimVector(AbsV, Ubound);
    for I := Lbound to Ubound do
      AbsV^[I] := Abs(V^[I]);
    AbsMax := Max(AbsV, Lbound, Ubound);
    DelVector(AbsV, Ubound);
  end;

  begin
    if WriteLogFile then
      begin
        CreateLogFile;
        WriteLn(LogFile, 'BFGS');
        WriteLn(LogFile, 'Iter         F');
      end;

    DimVector(OldX, Ubound);
    DimVector(DeltaX, Ubound);
    DimVector(dX, Ubound);
    DimVector(G, Ubound);
    DimVector(OldG, Ubound);
    DimVector(dG, Ubound);
    DimVector(HdG, Ubound);
    DimVector(R1dX, Ubound);
    DimVector(R2HdG, Ubound);
    DimVector(U, Ubound);
    DimVector(P2U, Ubound);

    Iter := 0;
    Conv := False;
    LinObjFunc := Func;  { Function for line minimization }

    { Initialize function }
    F_min := Func(X);

    { Initialize inverse hessian to unit matrix }
    for I := Lbound to Ubound do
      for J := Lbound to Ubound do
        if I = J then H_inv^[I]^[J] := 1.0 else H_inv^[I]^[J] := 0.0;

    { Initialize gradient }
    Gradient(Func, X, Lbound, Ubound, G);
    Gmax := AbsMax(G, Lbound, Ubound);

    { Initialize search direction }
    if Gmax > MACHEP then
      for I := Lbound to Ubound do
        DeltaX^[I] := - G^[I]
    else
      Conv := True;  { Quit if gradient is already small }

    while (not Conv) and (Iter < MaxIter) do
      begin
        if WriteLogFile then
          WriteLn(LogFile, Iter:4, '   ', F_min:12);

        { Normalize search direction to avoid excessive displacements }
        DeltaXmax := AbsMax(DeltaX, Lbound, Ubound);
        if DeltaXmax > 1.0 then
          for I := Lbound to Ubound do
            DeltaX^[I] := DeltaX^[I] / DeltaXmax;

        { Save old parameters and gradient }
        CopyVector(OldX, X, Lbound, Ubound);
        CopyVector(OldG, G, Lbound, Ubound);

        { Minimize along the direction specified by DeltaX }
        ErrCode := LinMin(Func, X, DeltaX, Lbound, Ubound, 100, 0.01, F_min);

        { Compute new gradient }
        Gradient(Func, X, Lbound, Ubound, G);

        { Compute differences between two successive
          estimations of parameter vector and gradient vector }
        for I := Lbound to Ubound do
          begin
            dX^[I] := X^[I] - OldX^[I];
            dG^[I] := G^[I] - OldG^[I];
          end;

        { Multiply by inverse hessian }
        for I := Lbound to Ubound do
          begin
            HdG^[I] := 0.0;
            for J := Lbound to Ubound do
              HdG^[I] := HdG^[I] + H_inv^[I]^[J] * dG^[J];
          end;

        { Scalar products in denominator of BFGS formula }
        P1 := 0.0; P2 := 0.0;
          for I := Lbound to Ubound do
            begin
              P1 := P1 + dX^[I] * dG^[I];
              P2 := P2 + dG^[I] * HdG^[I];
            end;

        if (P1 = 0.0) or (P2 = 0.0) then
          Conv := True
        else
          begin
            { Inverses of scalar products }
            R1 := 1.0 / P1; R2 := 1.0 / P2;

            { Compute BFGS correction terms }
            for I := Lbound to Ubound do
              begin
                R1dX^[I] := R1 * dX^[I];
                R2HdG^[I] := R2 * HdG^[I];
                U^[I] := R1dX^[I] - R2HdG^[I];
                P2U^[I] := P2 * U^[I];
              end;

            { Update inverse hessian }
            for I := Lbound to Ubound do
              for J := Lbound to Ubound do
                H_inv^[I]^[J] := H_inv^[I]^[J] + R1dX^[I] * dX^[J]
                                 - R2HdG^[I] * HdG^[J] + P2U^[I] * U^[J];

            { Update search direction }
            for I := Lbound to Ubound do
              begin
                DeltaX^[I] := 0.0;
                for J := Lbound to Ubound do
                  DeltaX^[I] := DeltaX^[I] - H_inv^[I]^[J] * G^[J];
              end;

            { Test convergence and update iteration count }
            Conv := ParamConv(OldX, X, Lbound, Ubound, Tol);
            Inc(Iter);
          end;
      end;

    DelVector(OldX, Ubound);
    DelVector(DeltaX, Ubound);
    DelVector(dX, Ubound);
    DelVector(G, Ubound);
    DelVector(OldG, Ubound);
    DelVector(dG, Ubound);
    DelVector(HdG, Ubound);
    DelVector(R1dX, Ubound);
    DelVector(R2HdG, Ubound);
    DelVector(U, Ubound);
    DelVector(P2U, Ubound);

    if WriteLogFile then
      Close(LogFile);

    if Iter > MaxIter then
      BFGS := OPT_NON_CONV
    else
      BFGS := OPT_OK;
  end;

begin
  X1 := nil;
  DeltaX1 := nil;
  Ubound1 := 1;
  Eps := Power(MACHEP, 0.333);
end.
