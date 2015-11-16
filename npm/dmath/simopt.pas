{ **********************************************************************
  *                           Unit SIMOPT.PAS                          *
  *                             Version 1.0                            *
  *                     (c) J. Debord, August 2000                     *
  **********************************************************************
  This unit implements simulated annealing for function minimization
  **********************************************************************
  Reference: Program SIMANN.FOR by Bill Goffe
  (http://www.netlib.org/simann)
  ********************************************************************** }

unit SimOpt;

interface

uses
  FMath, Matrices, Optim, Stat;

const
  SA_Nt      : Integer = 5;    { Number of loops at constant temperature }
  SA_Ns      : Integer = 15;   { Number of loops before step adjustment }
  SA_Rt      : Float   = 0.9;  { Temperature reduction factor }
  SA_NCycles : Integer = 1;    { Number of cycles }

function SimAnn(Func           : TFuncNVar;
                X, Xmin, Xmax  : PVector;
                Lbound, Ubound : Integer;
                MaxIter        : Integer;
                Tol            : Float;
                var F_min      : Float) : Integer;
{ ----------------------------------------------------------------------
  Minimization of a function of several variables by simulated annealing
  ----------------------------------------------------------------------
  Input parameters : Func    = objective function to be minimized
                     X       = initial minimum coordinates
                     Xmin    = minimum value of X
                     Xmax    = maximum value of X
                     Lbound,
                     Ubound  = indices of first and last variables
                     MaxIter = max number of annealing steps
                     Tol     = required precision
  ----------------------------------------------------------------------
  Output parameter : X       = refined minimum coordinates
                     F_min   = function value at minimum
  ----------------------------------------------------------------------
  Possible results : OPT_OK
                     OPT_NON_CONV
  ---------------------------------------------------------------------- }

implementation

var
  LogFile : Text;  { Stores the result of each minimization step }

  procedure CreateLogFile;
  begin
    Assign(LogFile, LogFileName);
    Rewrite(LogFile);
  end;

  function InitTemp(Func           : TFuncNVar;
                    X, Xmin, Range : PVector;
                    Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Computes the initial temperature so that the probability
  of accepting an increase of the function is about 0.5
  ---------------------------------------------------------------------- }
  const
    N_EVAL = 50;  { Number of function evaluations }
  var
    T : Float;         { Temperature }
    F, F1 : Float;     { Function values }
    DeltaF : PVector;  { Function increases }
    N_inc : Integer;   { Number of function increases }
    I : Integer;       { Index of function evaluation }
    K : Integer;       { Index of parameter }
  begin
    DimVector(DeltaF, N_EVAL);

    T := 0.0;
    N_inc := 0;
    F := Func(X);

    { Compute N_EVAL function values, changing each parameter in turn }
    K := Lbound;
    for I := 1 to N_EVAL do
      begin
        X^[K] := Xmin^[K] + RanMar * Range^[K];
        F1 := Func(X);
        if F1 > F then
          begin
            Inc(N_inc);
            DeltaF^[N_inc] := F1 - F;
          end;
        F := F1;
        Inc(K);
        if K > Ubound then K := Lbound;
      end;

    { The median M of these N_eval values has a probability of 1/2.
      From Boltzmann's formula: Exp(-M/T) = 1/2 ==> T = M / Ln(2) }
    T := Median(DeltaF, 1, N_inc) / LN2;
    if T = 0.0 then T := 1.0;
    InitTemp := T;

    DelVector(DeltaF, N_EVAL);
  end;

  function ParamConv(X, Step        : PVector;
                     Lbound, Ubound : Integer;
                     Tol            : Float) : Boolean;
{ ----------------------------------------------------------------------
  Checks for convergence on parameters
  ---------------------------------------------------------------------- }
  var
    I : Integer;
    Conv : Boolean;
  begin
    I := Lbound;
    Conv := True;
    repeat
      Conv := Conv and (Step^[I] < FMax(Tol, Tol * Abs(X^[I])));
      Inc(I);
    until (Conv = False) or (I > Ubound);
    ParamConv := Conv;
  end;

  function Accept(DeltaF, T        : Float;
                  var N_inc, N_acc : Integer) : Boolean;
{ ----------------------------------------------------------------------
  Checks if a variation DeltaF of the function at temperature T is
  acceptable. Updates the counters N_inc (number of increases of the
  function) and N_acc (number of accepted increases).
  ---------------------------------------------------------------------- }
  begin
    if DeltaF < 0.0 then
      Accept := True
    else
      begin
        Inc(N_inc);
        if Expo(- DeltaF / T) > RanMar then
          begin
            Accept := True;
            Inc(N_acc);
          end
        else
          Accept := False;
      end;
  end;

  function SimAnnCycle(Func           : TFuncNVar;
                       X, Xmin, Xmax  : PVector;
                       Lbound, Ubound : Integer;
                       MaxIter        : Integer;
                       Tol            : Float;
                       var LogFile    : Text;
                       var F_min      : Float) : Integer;
{ ----------------------------------------------------------------------
  Performs one cycle of simulated annealing
  ---------------------------------------------------------------------- }
  const
    N_FACT = 2.0;  { Factor for step reduction }
  var
    I, Iter, J, K, N_inc, N_acc : Integer;
    F, F1, DeltaF, Ratio, T, OldX : Float;
    Range, Step, Xopt : PVector;
    Nacc : PIntVector;
  begin
    DimVector(Step, Ubound);
    DimVector(Xopt, Ubound);
    DimVector(Range, Ubound);
    DimIntVector(Nacc, Ubound);

    { Determine parameter range, step and optimum }
    for K := Lbound to Ubound do
      begin
        Range^[K] := Xmax^[K] - Xmin^[K];
        Step^[K] := 0.5 * Range^[K];
        Xopt^[K] := X^[K];
      end;

    { Initialize function values }
    F := Func(X);
    F_min := F;

    { Initialize temperature and iteration count }
    T := InitTemp(Func, X, Xmin, Range, Lbound, Ubound);
    Iter := 0;

    repeat
      { Perform SA_Nt evaluations at constant temperature }
      N_inc := 0; N_acc := 0;
      for I := 1 to SA_Nt do
        begin
          for J := 1 to SA_Ns do
            for K := Lbound to Ubound do
              begin
                { Save current parameter value }
                OldX := X^[K];

                { Pick new value, keeping it within Range }
                X^[K] := X^[K] + (2.0 * RanMar - 1.0) * Step^[K];
                if (X^[K] < Xmin^[K]) or (X^[K] > Xmax^[K]) then
                  X^[K] := Xmin^[K] + RanMar * Range^[K];

                { Compute new function value }
                F1 := Func(X);
                DeltaF := F1 - F;

                { Check for acceptance }
                if Accept(DeltaF, T, N_inc, N_acc) then
                  begin
                    Inc(Nacc^[K]);
                    F := F1;
                  end
                else
                  { Restore parameter value }
                  X^[K] := OldX;

                { Update minimum if necessary }
                if F < F_min then
                  begin
                    Xopt^[K] := X^[K];
                    F_min := F;
                  end;
              end;

          { Ajust step length to maintain an acceptance
            ratio of about 50% for each parameter }
          for K := Lbound to Ubound do
            begin
              Ratio := Int(Nacc^[K]) / Int(SA_Ns);
              if Ratio > 0.6 then
                begin
                  { Increase step length, keeping it within Range }
                  Step^[K] := Step^[K] * (1.0 + ((Ratio - 0.6) / 0.4) * N_FACT);
                  if Step^[K] > Range^[K] then Step^[K] := Range^[K];
                end
              else if Ratio < 0.4 then
                { Reduce step length }
                Step^[K] := Step^[K] / (1.0 + ((0.4 - Ratio) / 0.4) * N_FACT);

              { Restore counter }
              Nacc^[K] := 0;
            end;
        end;

      if WriteLogFile then
        WriteLn(LogFile, Iter:4, '   ', T:12, '   ', F:12, N_inc:6, N_acc:6);

      { Update temperature and iteration count }
      T := T * SA_Rt;
      Inc(Iter);
    until ParamConv(Xopt, Step, Lbound, Ubound, Tol) or (Iter > MaxIter);

    for K := Lbound to Ubound do
      X^[K] := Xopt^[K];

    DelVector(Step, Ubound);
    DelVector(Xopt, Ubound);
    DelVector(Range, Ubound);
    DelIntVector(Nacc, Ubound);

    if Iter > MaxIter then
      SimAnnCycle := OPT_NON_CONV
    else
      SimAnnCycle := OPT_OK;
  end;

  function SimAnn(Func           : TFuncNVar;
                  X, Xmin, Xmax  : PVector;
                  Lbound, Ubound : Integer;
                  MaxIter        : Integer;
                  Tol            : Float;
                  var F_min      : Float) : Integer;
  var
    Cycle, ErrCode : Integer;
  begin
    if WriteLogFile then
      CreateLogFile;

    { Initialize the Marsaglia random number generator
      using the standard Pascal generator }
    Randomize;
    RMarIn(System.Random(10000), System.Random(10000));

    Cycle := 1;
    repeat
      if WriteLogFile then
        begin
          WriteLn(LogFile, 'Simulated annealing: Cycle ', Cycle);
          WriteLn(LogFile);
          WriteLn(LogFile, 'Iter         T              F        Inc   Acc');
        end;

      ErrCode := SimAnnCycle(Func, X, Xmin, Xmax, Lbound, Ubound,
                             MaxIter, Tol, LogFile, F_min);

      Inc(Cycle);
    until (Cycle > SA_NCycles) or (ErrCode <> OPT_OK);

    if WriteLogFile then
      Close(LogFile);

    SimAnn := ErrCode;
  end;

end.
