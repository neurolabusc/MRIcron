{ ******************************************************************
  Newton-Raphson solver for nonlinear equation
  ****************************************************************** }

unit unewteq;

interface

uses
  utypes;

procedure NewtEq (Func, Deriv : TFunc;
                  var X       : Float;
                  MaxIter     : Integer;
                  Tol         : Float;
                  var F       : Float);
{ ------------------------------------------------------------------
  Solves a nonlinear equation by Newton's method
  ------------------------------------------------------------------
  Input parameters  : Func    = function to be solved
                      Deriv   = derivative
                      X       = initial root
                      MaxIter = maximum number of iterations
                      Tol     = required precision
  ------------------------------------------------------------------
  Output parameters : X = refined root
                      F = function value
  ------------------------------------------------------------------
  Possible results : OptOk      = no error
                     OptNonConv = non-convergence
                     OptSing    = singularity (null derivative)
  ------------------------------------------------------------------ }

implementation

procedure NewtEq (Func, Deriv : TFunc;
                  var X       : Float;
                  MaxIter     : Integer;
                  Tol         : Float;
                  var F       : Float);

var
  Iter : Integer;  { Iteration count }
  OldX : Float;    { Old root }
  D    : Float;    { Derivative }
  Xtol : Float;    { Tolerance }

begin
  Iter := 0;
  SetErrCode(OptOk);

  F := Func(X);

  if MaxIter < 1 then Exit;

  repeat
    { Compute derivative }
    D := Deriv(X);

    if D = 0.0 then
      begin
        SetErrCode(OptSing);
        Exit;
      end;

    { Prepare next iteration }
    Iter := Iter + 1;
    if Iter > MaxIter then
      begin
        SetErrCode(OptNonConv);
        Exit;
      end;

    { Save current root and compute new one }
    OldX := X;
    X := X - F / D;
    F := Func(X);

    Xtol := Tol * Abs(X);
    if Xtol < MachEp then Xtol := MachEp;
  until Abs(OldX - X) < Xtol;
end;

end.