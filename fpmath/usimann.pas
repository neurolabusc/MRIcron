{ ******************************************************************
  Optimization by Simulated Annealing
  ******************************************************************
  Adapted from Fortran program SIMANN by Bill Goffe:
  http://www.netlib.org/opt/simann.f
  ****************************************************************** }

unit usimann;

interface

uses
  utypes, urandom, umedian;

procedure InitSAParams(NT, NS, NCycles : Integer; RT : Float);
{ ------------------------------------------------------------------
  Initialize simulated annealing parameters
  ------------------------------------------------------------------
  NT      : Number of loops at constant temperature
  NS      : Number of loops before step adjustment
  NCycles : Number of cycles
  RT      : Temperature reduction factor
  ------------------------------------------------------------------ }

procedure SA_CreateLogFile(FileName : String);
{ ------------------------------------------------------------------
  Initialize log file
  ------------------------------------------------------------------ }

procedure SimAnn(Func          : TFuncNVar;
                 X, Xmin, Xmax : PVector;
                 Lb, Ub        : Integer;
                 var F_min     : Float);
{ ------------------------------------------------------------------
  Minimization of a function of several var. by simulated annealing
  ------------------------------------------------------------------
  Input parameters : Func   = objective function to be minimized
                     X      = initial minimum coordinates
                     Xmin   = minimum value of X
                     Xmax   = maximum value of X
                     Lb, Ub = indices of first and last variables
  ------------------------------------------------------------------
  Output parameter : X      = refined minimum coordinates
                     F_min  = function value at minimum
  ------------------------------------------------------------------ }

implementation

{ Log file headers }
const
  Hdr1 = 'Simulated annealing: Cycle ';
  Hdr2 = 'Iter         T              F        Inc    Acc';

const
  SA_NT        : Integer =  5;    { Number of loops at constant temperature }
  SA_NS        : Integer = 15;    { Number of loops before step adjustment }
  SA_RT        : Float   =  0.9;  { Temperature reduction factor }
  SA_NCycles   : Integer =  1;    { Number of cycles }
  WriteLogFile : Boolean = False;

var
  LogFile : Text;

  procedure InitSAParams(NT, NS, NCycles : Integer; RT : Float);
  begin
    if NT > 0 then SA_NT := NT;
    if NS > 0 then SA_NS := NS;
    if NCycles > 1 then SA_NCycles := NCycles;
    if (RT > 0.0) and (RT < 1.0) then SA_RT := RT;
  end;

  procedure SA_CreateLogFile(FileName : String);
  begin
    Assign(LogFile, FileName);
    Rewrite(LogFile);
    WriteLogFile := True;
  end;

  function InitTemp(Func           : TFuncNVar;
                    X, Xmin, Range : PVector;
                    Lb, Ub         : Integer) : Float;
{ ------------------------------------------------------------------
  Computes the initial temperature so that the probability
  of accepting an increase of the function is about 0.5
  ------------------------------------------------------------------ }
  const
    N_EVAL = 50;  { Number of function evaluations }
  var
    F, F1  : Float;    { Function values }
    DeltaF : PVector;  { Function increases }
    N_inc  : Integer;  { Number of function increases }
    I      : Integer;  { Index of function evaluation }
    K      : Integer;  { Index of parameter }
  begin
    DimVector(DeltaF, N_EVAL);

    N_inc := 0;
    F := Func(X);

    { Compute N_EVAL function values, changing each parameter in turn }
    K := Lb;
    for I := 1 to N_EVAL do
      begin
        X^[K] := Xmin^[K] + RanGen3 * Range^[K];
        F1 := Func(X);
        if F1 > F then
          begin
            Inc(N_inc);
            DeltaF^[N_inc] := F1 - F;
          end;
        F := F1;
        Inc(K);
        if K > Ub then K := Lb;
      end;

    { The median M of these N_inc increases has a probability of 1/2.
      From Boltzmann's formula: Exp(-M/T) = 1/2 ==> T = M / Ln(2) }
    if N_inc > 0 then
      InitTemp := Median(DeltaF, 1, N_inc, False) * InvLn2
    else
      InitTemp := 1.0;

    DelVector(DeltaF, N_EVAL);
  end;

  function Accept(DeltaF, T        : Float;
                  var N_inc, N_acc : Integer) : Boolean;
{ ----------------------------------------------------------------------
  Checks if a variation DeltaF of the function at temperature T is
  acceptable. Updates the counters N_inc (number of increases of the
  function) and N_acc (number of accepted increases).
  ---------------------------------------------------------------------- }
  var
    X : Float;
  begin
    if DeltaF < 0.0 then
      begin
        Accept := True;
        Exit;
      end;

    Inc(N_inc);
    X := DeltaF / T;

    if X > MaxLog then  { Exp(- X) ~ 0 }
      begin
        Accept := False;
        Exit;
      end;

    if Exp(- X) > RanGen3 then
      begin
        Accept := True;
        Inc(N_acc);
      end
    else
      Accept := False;
  end;

  procedure SimAnnCycle(Func          : TFuncNVar;
                        X, Xmin, Xmax : PVector;
                        Lb, Ub        : Integer;
                        var F_min     : Float);
{ ------------------------------------------------------------------
  Performs one cycle of simulated annealing
  ------------------------------------------------------------------ }
  const
    SFact   = 2.0;      { Factor for step reduction }
    MinTemp = 1.0E-30;  { Min. temperature }
    MinFunc = 1.0E-30;  { Min. function value }
  var
    I, Iter, J, K, N_inc, N_acc   : Integer;
    F, F1, DeltaF, Ratio, T, OldX : Float;
    Range, DeltaX, Xopt           : PVector;
    Nacc                          : PIntVector;
  begin
    DimVector(Range, Ub);
    DimVector(DeltaX, Ub);
    DimVector(Xopt, Ub);
    DimIntVector(Nacc, Ub);

    { Determine parameter range, step and optimum }
    for K := Lb to Ub do
      begin
        Range^[K] := Xmax^[K] - Xmin^[K];
        DeltaX^[K] := 0.5 * Range^[K];
        Xopt^[K] := X^[K];
      end;

    { Initialize function values }
    F := Func(X);
    F_min := F;

    { Initialize temperature and iteration count }
    T := InitTemp(Func, X, Xmin, Range, Lb, Ub);
    Iter := 0;

    repeat
      N_inc := 0;
      N_acc := 0;

      { Perform SA_NT evaluations at constant temperature }
      for I := 1 to SA_NT do
        begin
          for J := 1 to SA_NS do
            for K := Lb to Ub do
              begin
                { Save current parameter value }
                OldX := X^[K];

                { Pick new value, keeping it within Range }
                X^[K] := X^[K] + (2.0 * RanGen3 - 1.0) * DeltaX^[K];
                if (X^[K] < Xmin^[K]) or (X^[K] > Xmax^[K]) then
                  X^[K] := Xmin^[K] + RanGen3 * Range^[K];

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
          for K := Lb to Ub do
            begin
              Ratio := Nacc^[K] / SA_NS;
              if Ratio > 0.6 then
                begin
                  { Increase step length, keeping it within Range }
                  DeltaX^[K] := DeltaX^[K] * (1.0 + ((Ratio - 0.6) / 0.4) * SFact);
                  if DeltaX^[K] > Range^[K] then DeltaX^[K] := Range^[K];
                end
              else if Ratio < 0.4 then
                { Reduce step length }
                DeltaX^[K] := DeltaX^[K] / (1.0 + ((0.4 - Ratio) / 0.4) * SFact);

              { Restore counter }
              Nacc^[K] := 0;
            end;
        end;

      if WriteLogFile then
        WriteLn(LogFile, Iter:4, '   ', T:12, '   ', F:12, N_inc:6, N_acc:6);

      { Update temperature and iteration count }
      T := T * SA_RT;
      Inc(Iter);
    until (N_acc = 0) or (T < MinTemp) or (Abs(F_min) < MinFunc);

    for K := Lb to Ub do
      X^[K] := Xopt^[K];

    DelVector(Range, Ub);
    DelVector(DeltaX, Ub);
    DelVector(Xopt, Ub);
    DelIntVector(Nacc, Ub);
  end;

  procedure SimAnn(Func          : TFuncNVar;
                   X, Xmin, Xmax : PVector;
                   Lb, Ub        : Integer;
                   var F_min     : Float);
  var
    Cycle : Integer;
  begin
    SetErrCode(OptOk);

    { Initialize the random number generator
      using the standard Pascal generator }
    Randomize;
    InitGen(Trunc(Random * 1.0E+8));

    for Cycle := 1 to SA_NCycles do
      begin
        if WriteLogFile then
          begin
            WriteLn(LogFile, Hdr1, Cycle);
            WriteLn(LogFile);
            WriteLn(LogFile, Hdr2);
          end;

        SimAnnCycle(Func, X, Xmin, Xmax, Lb, Ub, F_min);
      end;

    if WriteLogFile then
      begin
        Close(LogFile);
        WriteLogFile := False;
      end;
  end;

end.
