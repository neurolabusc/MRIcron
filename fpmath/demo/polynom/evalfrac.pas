{ ******************************************************************
  This program evaluates a rational fraction

                             1 + X + 2*X^2 + 3*X^3 + 4*X^4
  Example fraction is F(X) = -------------------------------
                             1 + 2*X - 3*X^2 + 4*X^3 - 5*X^4

  ****************************************************************** }

program evalfrac;

uses
  tpmath;

var
 Coef       : PVector;
 Deg1, Deg2 : Integer;
 X, Y       : Float;

begin

{ ------------------------------------------------------------------
  Define fraction here, in the form:

         Coef(0) + Coef(1) * X + ... + Coef(Deg1) * X^Deg1
  F(X) = -----------------------------------------------------
         1 + Coef(Deg1+1) * X + ... + Coef(Deg1+Deg2) * X^Deg2

  Note that the first coefficient of the denominator must be 1
  ------------------------------------------------------------------ }

  Deg1 := 4;
  Deg2 := 4;

  DimVector(Coef, Deg1 + Deg2);

  Coef^[0] :=  1;
  Coef^[1] :=  1;
  Coef^[2] :=  2;
  Coef^[3] :=  3;
  Coef^[4] :=  4;
  Coef^[5] :=  2;
  Coef^[6] := -3;
  Coef^[7] :=  4;
  Coef^[8] := -5;

{ ------------------------------------------------------------------ }

  repeat
    Write('X    =  ');
    ReadLn(X);
    Y := RFrac(X, Coef, Deg1, Deg2);
    WriteLn('F(X) = ', Y:12:6);
  until X = 0;

end.
