{ ******************************************************************
  This program evaluates a polynomial

  Example polynomial is P(X) = 1 + X + 2*X^2 + 3*X^3 + 4*X^4
  ****************************************************************** }

program evalpoly;

uses
  tpmath;

var
 Coef : PVector;
 Deg  : Integer;
 X, Y : Float;

begin

{ ------------------------------------------------------------------
  Define polynomial here, in the form:
  Coef(0) + Coef(1) * X + Coef(2) * X^2 + ... + Coef(Deg) * X^Deg
  ------------------------------------------------------------------ }

  Deg := 4;

  DimVector(Coef, Deg);

  Coef^[0] := 1;
  Coef^[1] := 1;
  Coef^[2] := 2;
  Coef^[3] := 3;
  Coef^[4] := 4;

{ ------------------------------------------------------------------ }

  repeat
    Write('X    =  ');
    ReadLn(X);
    Y := Poly(X, Coef, Deg);
    WriteLn('P(X) = ', Y:12:6);
  until X = 0;

end.
