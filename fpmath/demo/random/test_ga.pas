{ ******************************************************************
  Optimization by Genetic Algorithm
  ****************************************************************** }

program test_ga;

uses
  tpmath;

function Func1(X : PVector) : Float;
{ ------------------------------------------------------------------
  Example taken from 'Numerical Recipes'
  True minimum is at (-2.0, +/-0.89442719)
  ------------------------------------------------------------------ }
  var
    A, AA, B, BB : Float;
  begin
    A := Sqr(X^[2]) * (3.0 - X^[1]) - Sqr(X^[1]) * (3.0 + X^[1]);
    B := 2.0 + X^[1];
    AA := Sqr(A);
    BB := Sqr(B);
    Func1 := 10.0 * AA + BB / (1.0 + BB);
  end;

function Func2(X : PVector) : Float;
{ ------------------------------------------------------------------
  Example taken from 'Numerical Recipes'
  True minimum is at (0, 0, 0, 0), F = 1.0
  ------------------------------------------------------------------ }
  const
    Nvar = 4;
    Rad  = 0.3;
    Aug  = 2.0;
    Wid  : array[1..Nvar] of Float = (1.0, 3.0, 10.0, 30.0);
  var
    J                      : Integer;
    Q, R, Rad2, Sumd, Sumr : Float;
  begin
    Sumd := 0.0;
    Sumr := 0.0;
    Rad2 := Sqr(Rad);
    for J := 1 to Nvar do
      begin
        Q := X^[J] * Wid[J];
        if Q >= 0 then R := Int(Q + 0.5) else R := Int(Q - 0.5);
        Sumr := Sumr + Sqr(Q);
        Sumd := Sumd + Sqr(Q - R);
      end;
    if Sumd > Rad2 then
      Func2 := 1.0 + Sumr * (1.0 + Aug)
    else
      Func2 := 1.0 + Sumr * (1.0 + Aug * Sumd / Rad2);
  end;

function Func3(X : PVector) : Float;
{ ------------------------------------------------------------------
  Rosenbrock function.

  True minimum is at (1, 1), F = 0

  Ref: H. Rosenbrock, Comput. J., 1960, 3, 175
  ------------------------------------------------------------------ }
  begin
    Func3 := 100.0 * Sqr(X^[2] - Sqr(X^[1])) + Sqr(1.0 - X^[1]);
  end;

function Func4(X : PVector) : Float;
{ ------------------------------------------------------------------
  Powell function.

  True minimum is at (0, 0, 0, 0), F = 0

  Ref: M.J.D. Powell, Comput. J., 1962, 5, 147
  ------------------------------------------------------------------ }
  begin
    Func4 := Sqr(X^[1] + 10.0 * X^[2]) + 5.0 * Sqr(X^[3] - X^[4])
           + Sqr(Sqr(X^[2] - 2.0 * X^[3])) + 10.0 * Sqr(Sqr(X^[1] - X^[4]));
  end;

function Func5(X : PVector) : Float;
{ ------------------------------------------------------------------
  Another Powell function.

  Multiple minima at x1 = x2 = x3 = +/- Sqrt(4*n+1), n integer, F = -3

  Ref: M.J.D. Powell, Comput. J., 1964, 7, 155

  NB: The original reference maximizes F. Here we shall minimize -F.
  ------------------------------------------------------------------ }
  begin
    Func5 := - 1.0 / (1.0 + Sqr(X^[1] - X^[2])) - Sin(PiDiv2 * X^[2] * X^[3])
             - Expo(- Sqr((X^[1] + X^[3]) / X^[2] - 2.0));
  end;

function Func6(X : PVector) : Float;
{ ------------------------------------------------------------------
  Fletcher & Powell function.

  True minimum is at (1, 0, 0), F = 0

  Ref: R. Fletcher & M.J.D. Powell, Comput. J., 1964, 7, 155
  ------------------------------------------------------------------ }
  var
    R, Theta : Float;
  begin
    R := Pythag(X^[1], X^[2]);
    Theta := ArcTan2(X^[2], X^[1]) / TwoPi;
    Func6 := 100.0 * (Sqr(X^[3] - 10.0 * Theta) + Sqr(R - 1.0)) + Sqr(X^[3]);
  end;

function Func7(X : PVector) : Float;
{ ------------------------------------------------------------------
  Colville function (Extension of Rosenbrock function)

  True minimum is at (1, 1, 1, 1), F = 0

  Ref: R. J. Van Iwaarden, PhD Thesis, U. Denver, 1996
  ------------------------------------------------------------------ }
  begin
    Func7 := 100.0 * Sqr(X^[2] - Sqr(X^[1])) + Sqr(1.0 - X^[1]) +
              90.0 * Sqr(X^[4] - Sqr(X^[3])) + Sqr(1.0 - X^[3]) +
              10.1 * ((Sqr(X^[2] - 1.0) + Sqr(X^[4] - 1.0))) +
              19.8 * (X^[2] - 1.0) * (X^[4] - 1.0);
  end;

function Func8(X : PVector) : Float;
{ ------------------------------------------------------------------
  Griewank function.

  True minimum is at (0, 0), F = 0

  Ref: R. J. Van Iwaarden, PhD Thesis, U. Denver, 1996
  ------------------------------------------------------------------ }
  begin
    Func8 := (Sqr(X^[1]) + Sqr(X^[2])) / 200.0
            - Cos(X^[1]) * Cos(X^[2] / Sqrt2) + 1.0;
  end;

function Func9(X : PVector) : Float;
{ ------------------------------------------------------------------
  Chichinadze function.

  True minimum is at (5.90133, 0.5), F = -43.3159

  Ref: R. J. Van Iwaarden, PhD Thesis, U. Denver, 1996
  ------------------------------------------------------------------ }
  const
    FivePi   = 15.707963267948966193;   { 5 * Pi      }
    InvSqrt5 = 0.44721359549995793928;  { 1 / Sqrt(5) }
  begin
    Func9 := X^[1] * (X^[1] - 12.0) + 11.0
             + 10.0 * Cos(PIDIV2 * X^[1]) + 8.0 * Sin(FivePi * X^[1])
             - InvSqrt5 * Expo(- 0.5 * Sqr(X^[2] - 0.5));
  end;

function Func10(X : PVector) : Float;
{ ------------------------------------------------------------------
  Rastrigin function.

  True minimum is at (0, 0), F = -2

  Ref: R. J. Van Iwaarden, PhD Thesis, U. Denver, 1996
  ------------------------------------------------------------------ }
  begin
    Func10 := Sqr(X^[1]) + Sqr(X^[2]) - Cos(12.0 * X^[1]) - Cos(18.0 * X^[2]);
  end;

procedure Pause;
  begin
    WriteLn;
    Write('Press <Enter> to continue');
    ReadLn;
    WriteLn;
  end;

const
  NFunc   = 10;  { Number of functions }
  MaxNvar = 4;   { Maximum number of variables }

const
  FuncName : array[1..NFunc] of String[70] =
  ('Numerical Recipes Example 1: Minimum at (-2.0, +/-0.89442719), F = 0 ',
   'Numerical Recipes Example 2: Minimum at (0, 0, 0, 0), F = 1          ',
   'Rosenbrock function: Minimum at (1, 1), F = 0                        ',
   'Powell function: Minimum at (0, 0, 0, 0), F = 0                      ',
   'Another Powell function: Minimum at x1=x2=x3= +/- Sqrt(4*n+1), F = -3',
   'Fletcher & Powell function: Minimum at (1, 0, 0), F = 0              ',
   'Colville function: Minimum at (1, 1, 1, 1), F = 0                    ',
   'Griewank function: Minimum at (0, 0), F = 0                          ',
   'Chichinadze function: Minimum at (5.90133, 0.5), F = -43.3159        ',
   'Rastrigin function: Minimum at (0, 0), F = -2                        ');

const
  Nvar : array[1..NFunc] of Integer =
         (2, 4, 2, 4, 3, 3, 4, 2, 2, 2);  { Number of variables }

var
  Func          : array[1..NFunc] of TFuncNVar;  { Functions }
  X, Xmin, Xmax : PVector;                       { Variables and limit values }
  F_min         : Float;                         { Function value at minimum }
  I, J          : Integer;                       { Loop variables }

begin
  WriteLn;

  { Initialize function array }
  Func[ 1] := {$IFDEF FPC}@{$ENDIF}Func1;
  Func[ 2] := {$IFDEF FPC}@{$ENDIF}Func2;
  Func[ 3] := {$IFDEF FPC}@{$ENDIF}Func3;
  Func[ 4] := {$IFDEF FPC}@{$ENDIF}Func4;
  Func[ 5] := {$IFDEF FPC}@{$ENDIF}Func5;
  Func[ 6] := {$IFDEF FPC}@{$ENDIF}Func6;
  Func[ 7] := {$IFDEF FPC}@{$ENDIF}Func7;
  Func[ 8] := {$IFDEF FPC}@{$ENDIF}Func8;
  Func[ 9] := {$IFDEF FPC}@{$ENDIF}Func9;
  Func[10] := {$IFDEF FPC}@{$ENDIF}Func10;

  { Allocate arrays }
  DimVector(X, MaxNvar);
  DimVector(Xmin, MaxNvar);
  DimVector(Xmax, MaxNvar);

  { Select random number generator }
  SetRNG(RNG_MT);

  for I := 1 to NFunc do
    begin
      { Initialize limits }
      for J := 1 to Nvar[I] do
        begin
          Xmin^[J] := - 10.0;
          Xmax^[J] := 10.0;
        end;

      { Approximate global minimum with genetic algorithm }
      GA_CreateLogFile('genalg.txt');
      GenAlg(Func[I], X, Xmin, Xmax, 1, Nvar[I], F_min);

      { Display results }
      Writeln(FuncName[I]);
      Writeln;
      for J := 1 to Nvar[I] do
        Writeln('X(', J, ') = ', X^[J]:12:6);
      Writeln;
      Writeln('F    = ', F_min:10);
      Writeln;
      Pause;
    end;

  { Deallocate arrays }
  DelVector(X, MaxNvar);
  DelVector(Xmin, MaxNvar);
  DelVector(Xmax, MaxNvar);
end.

