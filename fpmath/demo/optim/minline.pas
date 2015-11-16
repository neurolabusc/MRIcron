{ ******************************************************************
  Minimization of a function of several variables along a line

  Example taken from "Numerical Recipes":

  Func = (X(1)-1)^2 + (X(2)-1)^2 + (X(3)-1)^2

  The minimum is F = 0 at (1, 1, 1), i. e. for a step R = 1 from
  X = (0, 0, 0) in the direction DeltaX = (1, 1, 1)

  The program tries a series of directions:

           ( Sqrt(2) * Cos [(Pi / 2) * (I / 10)] )
  DeltaX = ( Sqrt(2) * Sin [(Pi / 2) * (I / 10)] )
           ( 1                                   )

  For each pass, the location of the minimum, and the value of the
  function at the minimum, are printed. The minimum is at I = 5
  ****************************************************************** }

program minline;

uses
  tpmath;

const
  Nvar    = 3;                   { Number of variables }
  MaxIter = 1000;                { Max number of iterations }
  Tol     = 1.0E-7;              { Required precision }
  PiDiv20 = 0.1570796326794897;  { Pi / 20 }

var
  X      : PVector;              { Starting point }
  DeltaX : PVector;              { Search direction }
  R      : Float;                { Initial step }
  F_min  : Float;                { Function value at minimum }
  I      : Integer;              { Loop variable }
  Z      : Float;                { Auxiliary variable }

function Func(X : PVector) : Float;
{ Function to be minimized }
begin
  Func := Sqr(X^[1] - 1.0) + Sqr(X^[2] - 1.0) + Sqr(X^[3] - 1.0);
end;

procedure PrintResult(I : Integer; X : PVector; F_min : Float);
var
  J : Integer;
begin
  Write(I:3);

  for J := 1 to Nvar do
    Write(X^[J]:12:6);

  WriteLn('    ', F_min);
end;

begin
  WriteLn;
  WriteLn('  I      X(1)        X(2)        X(3)               Fmin');
  WriteLn('------------------------------------------------------------------');

  DimVector(X, Nvar);
  DimVector(DeltaX, Nvar);

  for I := 0 to 10 do
    begin
      X^[1] := 0.0;
      X^[2] := 0.0;
      X^[3] := 0.0;

      Z := I * PiDiv20;

      DeltaX^[1] := Sqrt2 * Cos(Z);
      DeltaX^[2] := Sqrt2 * Sin(Z);
      DeltaX^[3] := 1.0;

      R := 0.1;

      {$IFDEF FPC}
      LinMin(@Func, X, DeltaX, 1, Nvar, R, MaxIter, Tol, F_min);
      {$ELSE}
      LinMin(Func, X, DeltaX, 1, Nvar, R, MaxIter, Tol, F_min);
      {$ENDIF}

      if MathErr = OptOk then
        PrintResult(I, X, F_min)
      else
        WriteLn('Non-convergence!');
    end;
end.
