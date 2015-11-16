{ ******************************************************************
  Solution to a nonlinear equation by the secant method.

  Example:

  F(X) = X * Ln(X) - 1 = 0

  True Solution is X = 1.763222834...
  ****************************************************************** }

program testsec;

uses
  tpmath;

{ ******************************************************************
  Define the function
  ****************************************************************** }

function Func(X : Float) : Float;
begin
  Func := X * Ln(X) - 1
end;

{ ******************************************************************
  Define number of iterations and precision
  ****************************************************************** }

const
  MaxIter = 1000;  { Max number of iterations }
  Tol     = 1E-6;  { Required precision }

{ ******************************************************************
  Main program
  ****************************************************************** }

var
  F, X, Y : Float;

begin
  { Give two starting points near the root }
  X := 1;
  Y := 2;

  {$IFDEF FPC}
  Secant(@Func, X, Y, MaxIter, Tol, F);
  {$ELSE}
  Secant(Func, X, Y, MaxIter, Tol, F);
  {$ENDIF}

  if MathErr = OptNonConv then
    begin
      writeln('Non-convergence!');
      halt;
    end;

  writeln;
  writeln('Solution to nonlinear equation (Secant method)');
  writeln('----------------------------------------------');
  writeln;

  writeln('Root:          ', X:12:6);
  writeln;
  writeln('Function value:', F:12:6);
  writeln;
end.

