{ ******************************************************************
  Solution to a system of nonlinear equations by Broyden's method.

  Example (from Numerical Recipes Example Book):

  F(X, Y) = X^2 + Y^2 - 2 = 0
  G(X, Y) = EXP(X - 1) + Y^3 - 2 = 0

                ( 2 * X          2 * Y   )
  Jacobian: D = (                        )
                ( EXP(X - 1)     3 * Y^2 )

  True Solution is at (1, 1)
  ****************************************************************** }

program testbrdn;

uses
  tpmath;

{ ******************************************************************
  Define the system of equations to be solved
  ****************************************************************** }

procedure Equations(X, F : PVector);
var
  X1p2, X2p2, X2p3 : Float;
begin
  X1p2 := X^[1] * X^[1];
  X2p2 := X^[2] * X^[2];
  X2p3 := X^[2] * X2p2;

  F^[1] := X1p2 + X2p2 - 2;
  F^[2] := Exp(X^[1] - 1) + X2p3 - 2;
end;

{ ******************************************************************
  Define number of variables, number of iterations, and precision
  ****************************************************************** }

const
  Nvar    = 2;       { Number of variables }
  MaxIter = 1000;    { Max number of iterations }
  Tol     = 1.0E-6;  { Required precision (must be > Sqrt(MachEp)) }

var
  X, F : PVector;    { Variables: X^[1] = X, X^[2] = Y }
  I    : Integer;    { Loop variable }

{ ******************************************************************
  Main program
  ****************************************************************** }

begin
  DimVector(X, Nvar);
  DimVector(F, Nvar);

{ Define starting point }
  X^[1] := 2;
  X^[2] := 0.5;

  {$IFDEF FPC}
  Broyden(@Equations, X, F, 1, Nvar, MaxIter, Tol);
  {$ELSE}
  Broyden(Equations, X, F, 1, Nvar, MaxIter, Tol);
  {$ENDIF}

  if MathErr = OptNonConv then
    begin
      writeln('Non-convergence!');
      halt;
    end;

  writeln;
  writeln('Solution to nonlinear equation system (Broyden''s method)');
  writeln('--------------------------------------------------------');
  writeln;
  writeln('Solution vector:');
  writeln;

  for I := 1 to Nvar do
    writeln('X(', I, ') = ', X^[I]:10:6);

  writeln;
  writeln('Function values:');
  writeln;

  for I := 1 to Nvar do
    writeln('F(', I, ') = ', F^[I]:10:6);

  writeln;
end.

