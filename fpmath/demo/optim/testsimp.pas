{ ******************************************************************
  Minimization of a function of several variables by simplex method.

  Example: Rosenbrock's function:

  F(X, Y) = 100 * (Y - X^2)^2 + (1 - X)^2

  True minimum is at (1, 1), F = 0

  Ref: H. Rosenbrock, Comput. J., 1960, 3, 175
  ****************************************************************** }

program testsimp;

uses
  tpmath;

{ ------------------------------------------------------------------
  Define number of variables, number of iterations, and precision
  ------------------------------------------------------------------ }

const
  Nvar    = 2;       { Number of variables }
  MaxIter = 1000;    { Max number of iterations }
  Tol     = 1.0E-6;  { Required precision }

{ ------------------------------------------------------------------
  Define the function to be minimized
  ------------------------------------------------------------------ }

function Func(X : PVector) : Float;
begin
  Func := 100.0 * Sqr(X^[2] - Sqr(X^[1])) + Sqr(1.0 - X^[1]);
end;

{ ------------------------------------------------------------------
  Main program
  ------------------------------------------------------------------ }

var
  X    : PVector;  { Variables: X^[1] = X, X^[2] = Y }
  Fmin : Float;    { Function value at minimum }
  I    : Integer;  { Loop variable }

begin
  DimVector(X, Nvar);

  X^[1] := 2.0;
  X^[2] := 2.0;

  { Save Simplex iterations in a file }
  SaveSimplex('simplex.txt');

  { Perform minimization }

  {$IFDEF FPC}
  Simplex(@Func, X, 1, Nvar, MaxIter, Tol, Fmin);
  {$ELSE}
  Simplex(Func, X, 1, Nvar, MaxIter, Tol, Fmin);
  {$ENDIF}

  if MathErr = OptNonConv then
    begin
      Write('Non-convergence!');
      Halt;
    end;

  WriteLn('Minimization of Rosenbrock''s function (simplex method)');
  WriteLn('------------------------------------------------------');
  WriteLn;

  WriteLn('Coordinates of minimum:'); WriteLn;

  for I := 1 to Nvar do
    WriteLn('X(', I, ') = ', X^[I]:12:6);

  WriteLn;
  WriteLn('Function value:');
  WriteLn;

  WriteLn('Fmin  = ', Fmin);
end.
