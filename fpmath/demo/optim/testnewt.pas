{ ******************************************************************
  Minimization of a function of several variables by the Newton-
  Raphson method.

  Example: Rosenbrock's function:

  F(X, Y) = 100 * (Y - X^2)^2 + (1 - X)^2

                ( -400 * (Y - X^2) * X - 2 + 2 * X )
  Gradient: G = (                                  )
                (  200 * Y - 200 * X^2             )

                ( 1200 * X^2 - 400 * Y + 2     -400 * X )
  Hessian:  H = (                                       )
                ( -400 * X                      200     )

  Det(H) = 80000 * (X^2 - Y) + 400

  True minimum is at (1, 1), F = 0

  The inverse hessian at the minimum is:

    ( 1/2     1       )
    ( 1       401/200 )

  Ref: H. Rosenbrock, Comput. J., 1960, 3, 175
  ****************************************************************** }

program testnewt;

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
  Define the subroutine which computes the gradient and hessian of
  the function. It is recommended to use analytical derivatives
  whenever possible. Otherwise you can use the alternative code
  provided in numhess.inc
  ------------------------------------------------------------------ }

procedure HessGrad(X, G : PVector; H : PMatrix);
var
  A, B, C : Float;
begin
  C := Sqr(X^[1]);
  A := X^[2] - C;
  B := 1.0 - X^[1];

  G^[1] := - 400.0 * X^[1] * A - 2.0 * B;
  G^[2] := 200.0 * A;

  H^[1]^[1] := 1200.0 * C - 400.0 * X^[2] + 2;
  H^[1]^[2] := - 400.0 * X^[1];
  H^[2]^[1] := H^[1]^[2];
  H^[2]^[2] := 200.0;
end;

{ ------------------------------------------------------------------
  Alternative code when analytical derivatives are not available
  ------------------------------------------------------------------ }

(*

{$i numhess.inc}

*)

{ ------------------------------------------------------------------
  Main program
  ------------------------------------------------------------------ }

var
  X     : PVector;  { Variables: X^[1] = X, X^[2] = Y }
  G     : PVector;  { Gradient vector }
  H_inv : PMatrix;  { Inverse Hessian matrix }
  F_min : Float;    { Function value at minimum }
  Det   : Float;    { Determinant of hessian }
  I, J  : Integer;  { Loop variables }

begin
  DimVector(X, Nvar);
  DimVector(G, Nvar);
  DimMatrix(H_inv, Nvar, Nvar);

  X^[1] := 2.0;
  X^[2] := 2.0;

  { Save Newton-Raphson iterations in a file }
  SaveNewton('newton.txt');

  { Perform minimization }

  {$IFDEF FPC}
  Newton(@Func, @HessGrad, X, 1, Nvar, MaxIter, Tol, F_min, G, H_inv, Det);
  {$ELSE}
  Newton(Func, HessGrad, X, 1, Nvar, MaxIter, Tol, F_min, G, H_inv, Det);
  {$ENDIF}

  case MathErr of
    OptNonConv :
      begin
        Write('Non-convergence!');
        Halt;
      end;
    OptSing :
      begin
        Write('Singular Hessian matrix!');
        Halt;
      end;
  end;

  WriteLn('Minimization of Rosenbrock''s function (Newton-Raphson method)');
  WriteLn('--------------------------------------------------------------');
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

  WriteLn; WriteLn('Determinant of Hessian:'); WriteLn;

  WriteLn(Det:12:6);
end.
