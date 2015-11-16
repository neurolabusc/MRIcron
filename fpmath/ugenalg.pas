{ ******************************************************************
  Optimization by Genetic Algorithm
  ******************************************************************
  Ref.:  E. Perrin, A. Mandrille, M. Oumoun, C. Fonteix & I. Marc
         Optimisation globale par strategie d'evolution
         Technique utilisant la genetique des individus diploides
         Recherche operationnelle / Operations Research
         1997, 31, 161-201
  Thanks to Magali Camut for her contribution
  ****************************************************************** }

unit ugenalg;

interface

uses
  utypes, uminmax, urandom;

procedure InitGAParams(NP, NG : Integer; SR, MR, HR : Float);
{ ------------------------------------------------------------------
  Initialize Genetic Algorithm parameters
  ------------------------------------------------------------------
    NP : Population size
    NG : Max number of generations
    SR : Survival rate
    MR : Mutation rate
    HR : Proportion of homozygotes
  ------------------------------------------------------------------ }

procedure GA_CreateLogFile(LogFileName : String);
{ ------------------------------------------------------------------
  Initialize log file
  ------------------------------------------------------------------ }

procedure GenAlg(Func          : TFuncNVar;
                 X, Xmin, Xmax : PVector;
                 Lb, Ub        : Integer;
                 var F_min      : Float);
{ ------------------------------------------------------------------
  Minimization of a function of several variables
  by genetic algorithm
  ------------------------------------------------------------------
  Input parameters : Func   = objective function to be minimized
                     X      = initial minimum coordinates
                     Xmin   = minimum value of X
                     Xmax   = maximum value of X
                     Lb, Ub =
  ------------------------------------------------------------------
  Output parameters: X    = refined minimum coordinates
                     F_min = function value at minimum
  ------------------------------------------------------------------ }

implementation

const
  GA_NP : Integer = 200;  { Population size }
  GA_NG : Integer =  40;  { Max number of generations }
  GA_SR : Float   = 0.6;  { Survival rate }
  GA_MR : Float   = 0.1;  { Mutation rate }
  GA_HR : Float   = 0.5;  { Proportion of homozygotes }

  WriteLogFile : Boolean = False;

var
  LogFile : Text;

procedure InitGAParams(NP, NG : Integer; SR, MR, HR : Float);
begin
  if NP > 0 then GA_NP := NP;
  if NG > 0 then GA_NG := NG;

  if (SR > 0.0) and (SR < 1.0) then GA_SR := SR;
  if (MR > 0.0) and (MR < 1.0) then GA_MR := MR;
  if (HR > 0.0) and (HR < 1.0) then GA_HR := HR;
end;

procedure GA_CreateLogFile(LogFileName : String);
begin
  Assign(LogFile, LogFileName);
  Rewrite(LogFile);
  Writeln(LogFile, 'Genetic Algorithm');
  Writeln(LogFile, ' Iter          F ');
  WriteLogFile := True;
end;

procedure Mutate(I            : Integer;
                 C1, C2, D, P : PMatrix;
                 Xmin, Range  : PVector;
                 Lb, Ub       : Integer);
{ ------------------------------------------------------------------
  Mutate individual I
  ------------------------------------------------------------------ }
var
  J : Integer;
begin
  for J := Lb to Ub do
    begin
      C1^[I]^[J] := Xmin^[J] + RanGen3 * Range^[J];
      C2^[I]^[J] := Xmin^[J] + RanGen3 * Range^[J];
      D^[I]^[J] := RanGen3;
      P^[I]^[J] := D^[I]^[J] * C1^[I]^[J] + (1.0 - D^[I]^[J]) * C2^[I]^[J];
    end;
end;

procedure Cross(I1, I2, I    : Integer;
                C1, C2, D, P : PMatrix;
                Lb, Ub       : Integer);
{ ------------------------------------------------------------------
  Cross two individuals I1 and I2 --> new individual I
  ------------------------------------------------------------------ }
var
  J, K : Integer;
begin
  for J := Lb to Ub do
    begin
      if RanGen3 < 0.5 then K := I1 else K := I2;
      C1^[I]^[J] := C1^[K]^[J];

      if RanGen3 < 0.5 then K := I1 else K := I2;
      C2^[I]^[J] := C2^[K]^[J];

      D^[I]^[J] := RanGen3;

      P^[I]^[J] := D^[I]^[J] * C1^[I]^[J] + (1.0 - D^[I]^[J]) * C2^[I]^[J];
    end;
end;

procedure Homozygote(I         : Integer;
                     C1, C2, P : PMatrix;
                     Lb, Ub    : Integer);
{ ------------------------------------------------------------------
  Make individual I homozygous
  ------------------------------------------------------------------ }
var
  J : Integer;
begin
  for J := Lb to Ub do
    begin
      C1^[I]^[J] := P^[I]^[J];
      C2^[I]^[J] := P^[I]^[J];
    end;
end;

function GA_Func(Func   : TFuncNVar;
                 I      : Integer;
                 P      : PMatrix;
                 Lb, Ub : Integer) : Float;
{ ------------------------------------------------------------------
  Computes objective function for individual I
  ------------------------------------------------------------------ }
var
  J : Integer;
  X : PVector;
begin
  DimVector(X, Ub);

  for J := Lb to Ub do
    X^[J] := P^[I]^[J];

  GA_Func := Func(X);

  DelVector(X, Ub);
end;

procedure CompFunc(Func         : TFuncNVar;
                   X            : PVector;
                   C1, C2, D, P : PMatrix;
                   F            : PVector;
                   Lb, Ub       : Integer;
                   var Iter     : Integer;
                   var F_min    : Float);
{ ------------------------------------------------------------------
  Computes function values
  ------------------------------------------------------------------ }
var
  I, J, K : Integer;
  A       : Float;
begin
  { Compute function values }
  for I := 1 to GA_NP do
    F^[I] := GA_Func(Func, I, P, Lb, Ub);

  { Sort population according to function values }
  for I := 1 to GA_NP - 1 do
  begin
    K := I;
    A := F^[I];

    for J := I + 1 to GA_NP do
      if F^[J] < A then
      begin
        K := J;
        A := F^[J];
      end;

    FSwap(F^[I], F^[K]);

    for J := Lb to Ub do
    begin
      FSwap(C1^[I]^[J], C1^[K]^[J]);
      FSwap(C2^[I]^[J], C2^[K]^[J]);
      FSwap(D^[I]^[J], D^[K]^[J]);
      FSwap(P^[I]^[J], P^[K]^[J]);
    end;
  end;

  { Update log file if necessary }
  if WriteLogFile then
    Writeln(LogFile, Iter:5, F^[1]:12);

  { Update minimum }
  if F^[1] < F_min then
    begin
      F_min := F^[1];
      for J := Lb to Ub do
        X^[J] := P^[1]^[J];
    end;

  Inc(Iter);
end;

procedure GenPop(Func           : TFuncNVar;
                 NS             : Integer;
                 C1, C2, D, P   : PMatrix;
                 F, Xmin, Range : PVector;
                 Lb, Ub         : Integer);
{ ------------------------------------------------------------------
  Generates new population
  ------------------------------------------------------------------ }
var
  I, I1, I2 : Integer;
  F0        : Float;
begin
  for I := NS + 1 to GA_NP do
  begin
    I1 := Trunc(RanGen3 * NS) + 1;

    repeat
      I2 := Trunc(RanGen3 * NS) + 1
    until I2 <> I1;

    F0 := FMax(F^[I1], F^[I2]);

    repeat
      Cross(I1, I2, I, C1, C2, D, P, Lb, Ub);
    until GA_Func(Func, I, P, Lb, Ub) <= F0;
  end;

  for I := 1 to GA_NP do
  begin
    if RanGen3 < GA_MR then
      Mutate(I, C1, C2, D, P, Xmin, Range, Lb, Ub);
    if RanGen3 < GA_HR then
      Homozygote(I, C1, C2, P, Lb, Ub);
  end;
end;

procedure GenAlg(Func          : TFuncNVar;
                 X, Xmin, Xmax : PVector;
                 Lb, Ub        : Integer;
                 var F_min     : Float);
{ ------------------------------------------------------------------
  Minimization of a function of several variables
  by genetic algorithm
  ------------------------------------------------------------------
  Input parameters : Func   = objective function to be minimized
                     X      = initial minimum coordinates
                     Xmin   = minimum value of X
                     Xmax   = maximum value of X
                     Lb, Ub =
  ------------------------------------------------------------------
  Output parameters: X    = refined minimum coordinates
                     F_min = function value at minimum
  ------------------------------------------------------------------ }
var
  I, NS, Iter  : Integer;
  C1, C2, D, P : PMatrix;
  Range, F     : PVector;

begin
  SetErrCode(OptOk);

  { Initialize the random number generator
    using the standard generator }
  Randomize;
  InitGen(Trunc(Random * 1.0E+8));

  { Dimension arrays }
  DimMatrix(C1, GA_NP, Ub);
  DimMatrix(C2, GA_NP, Ub);
  DimMatrix(D, GA_NP, Ub);
  DimMatrix(P, GA_NP, Ub);

  DimVector(F, GA_NP);
  DimVector(Range, Ub);

  for I := Lb to Ub do
    Range^[I] := Xmax^[I] - Xmin^[I];

  NS := Trunc(GA_NP * GA_SR);  { Number of survivors }

  Iter := 0;
  F_min := MaxNum;

  for I := 1 to GA_NP do
    Mutate(I, C1, C2, D, P, Xmin, Range, Lb, Ub);

  CompFunc(Func, X, C1, C2, D, P, F, Lb, Ub, Iter, F_min);

  for I := 1 to GA_NG do
    begin
      GenPop(Func, NS, C1, C2, D, P, F, Xmin, Range, Lb, Ub);
      CompFunc(Func, X, C1, C2, D, P, F, Lb, Ub, Iter, F_min);
    end;

  if WriteLogFile then
    begin
      Close(LogFile);
      WriteLogFile := False;
    end;

  DelMatrix(C1, GA_NP, Ub);
  DelMatrix(C2, GA_NP, Ub);
  DelMatrix(D, GA_NP, Ub);
  DelMatrix(P, GA_NP, Ub);

  DelVector(F, GA_NP);
  DelVector(Range, Ub);
end;

end.
