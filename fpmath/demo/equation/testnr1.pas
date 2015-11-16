{ ******************************************************************
  Solution to a nonlinear equation by the Newton-Raphson method.

  Example:

  F(X) = X * Ln(X) - 1 = 0

  Derivative: F'(X) = Ln(X) + 1

  True Solution is X = 1.763222834...
  ****************************************************************** }

program testnr1;

uses
  tpmath;

{ ******************************************************************
  Define the function and its derivative
  ****************************************************************** }

function Func(X : Float) : Float;
begin
  Func := X * Ln(X) - 1
end;

function Deriv(X : Float) : Float;
begin
  Deriv := Ln(X) + 1
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
  F, X : Float;

begin
  { Define a starting point near the root }
  X := 1;

  {$IFDEF FPC}
  NewtEq(@Func, @Deriv, X, MaxIter, Tol, F);
  {$ELSE}
  NewtEq(Func, Deriv, X, MaxIter, Tol, F);
  {$ENDIF}

  case MathErr of
    OptNonConv :
      begin
        writeln('Non-convergence!');
        halt;
      end;
    OptSing :
      begin
        writeln('Null derivative!');
        halt;
      end;
  end;

  writeln;
  writeln('Solution to nonlinear equation (Newton-Raphson method)');
  writeln('------------------------------------------------------');
  writeln;

  writeln('Root:          ', X:12:6);
  writeln;
  writeln('Function value:', F:12:6);
  writeln;
end.

