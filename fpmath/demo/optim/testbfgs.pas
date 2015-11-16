{ ******************************************************************
  Minimization of a function of several variables by the Broyden-
  Fletcher-Goldfarb-Shanno (BFGS) method.

  Example: Rosenbrock's function:

  F(X, Y) = 100 * (Y - X^2)^2 + (1 - X)^2

                ( -400 * (Y - X^2) * X - 2 + 2 * X )
  Gradient: G = (                                  )
                (  200 * Y - 200 * X^2             )

                ( 1200 * X^2 - 400 * Y + 2     -400 * X )
  Hessian:  H = (                                       )
                ( -400 * X                      200     )

  True minimum is at (1, 1), F = 0

  The inverse hessian at the minimum is:

    ( 1/2     1       )
    ( 1       401/200 )

  Ref: H. Rosenbrock, Comput. J., 1960, 3, 175
  ****************************************************************** }

program testbfgs;

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
  Define the subroutine which computes the gradient of the function.
  It is recommended to use analytical derivatives whenever possible.
  Otherwise you can use the alternative code provided in numgrad.inc
  ------------------------------------------------------------------ }

procedure Gradient(X, G : PVector);
var
  A, B : Float;
begin
  A := X^[2] - Sqr(X^[1]);
  B := 1.0 - X^[1];

  G^[1] := - 400.0 * X^[1] * A - 2.0 * B;
  G^[2] := 200.0 * A;
end;


{ ------------------------------------------------------------------
  Alternative code when analytical derivatives are not available
  ------------------------------------------------------------------ }

(*

{$i numgrad.inc}

*)

{ ------------------------------------------------------------------
  Main program
  ------------------------------------------------------------------ }

var
  X     : PVector;  { Variables: X^[1] = X, X^[2] = Y }
  G     : PVector;  { Gradient vector }
  H_inv : PMatrix;  { Inverse Hessian matrix }
  F_min : Float;    { Function value at minimum }
  I, J  : Integer;  { Loop variables }

begin
  DimVector(X, Nvar);
  DimVector(G, Nvar);
  DimMatrix(H_inv, Nvar, Nvar);

  X^[1] := 2.0;
  X^[2] := 2.0;

  { Save BFGS iterations in a file }
  SaveBFGS('bfgs.txt');

  { Perform minimization }

  {$IFDEF FPC}
  BFGS(@Func, @Gradient, X, 1, Nvar, MaxIter, Tol, F_min, G, H_inv);
  {$ELSE}
  BFGS(Func, Gradient, X, 1, Nvar, MaxIter, Tol, F_min, G, H_inv);
  {$ENDIF}

  if MathErr = OptNonConv then
    begin
      Write('Non-convergence!');
      Halt;
    end;

  WriteLn('Minimization of Rosenbrock''s function (BFGS method)');
  WriteLn('----------------------------------------------------');
  WriteLn;

  WriteLn('Coordinates of minimum:'); WriteLn;

  for I := 1 to Nvar do
    WriteLn(X^[I]:12:6);

  WriteLn; WriteLn('Function value:'); WriteLn;

  WriteLn('Fmin  = ', F_min);

  WriteLn; WriteLn('Gradient:'); WriteLn;

  for I := 1 to Nvar do
    WriteLn(G^[I]:12:6);

  WriteLn; WriteLn('Inverse Hessian matrix:'); WriteLn;

  for I := 1 to Nvar do
    begin
      for J := 1 to Nvar do
        Write(H_inv^[I]^[J]:12:6);
      WriteLn;
    end;

end.
