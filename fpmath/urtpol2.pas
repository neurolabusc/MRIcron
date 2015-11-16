{ ******************************************************************
  Quadratic equation
  ****************************************************************** }

unit urtpol2;

interface

uses
  utypes, urtpol1;

function RootPol2(Coef : PVector; Z : PCompVector) : Integer;
{ ------------------------------------------------------------------
  Solves the quadratic equation:
  Coef^[0] + Coef^[1] * X + Coef^[2] * X^2 = 0
  ------------------------------------------------------------------ }

implementation

function RootPol2(Coef : PVector; Z : PCompVector) : Integer;
var
  Delta, F, Q : Float;

begin
  Z^[1].X := 0.0; Z^[1].Y := 0.0;
  Z^[2].X := 0.0; Z^[2].Y := 0.0;

  if Coef^[2] = 0.0 then
    begin
      RootPol2 := RootPol1(Coef^[0], Coef^[1], Z^[1].X);
      Exit;
    end;

  if Coef^[0] = 0.0 then
    begin
      { 0 is root. Eq. becomes linear }
      if RootPol1(Coef^[1], Coef^[2], Z^[1].X) = 1 then
        { Linear eq. has 1 solution }
        RootPol2 := 2
      else
        { Linear eq. is undetermined or impossible }
        RootPol2 := 1;
      Exit;
    end;

  Delta := Sqr(Coef^[1]) - 4.0 * Coef^[0] * Coef^[2];

  { 2 real roots }
  if Delta > 0.0 then
    begin
      RootPol2 := 2;

      { Algorithm for minimizing roundoff errors }
      { See `Numerical Recipes'                  }
      if Coef^[1] >= 0.0 then
        Q := - 0.5 * (Coef^[1] + Sqrt(Delta))
      else
        Q := - 0.5 * (Coef^[1] - Sqrt(Delta));

      Z^[1].X := Q / Coef^[2];
      Z^[2].X := Coef^[0] / Q;

      Exit;
    end;

  { Double real root }
  if Delta = 0.0 then
    begin
      RootPol2 := 2;
      Z^[1].X := - 0.5 * Coef^[1] / Coef^[2];
      Z^[2].X := Z^[1].X;
      Exit;
    end;

  { 2 complex roots }
  RootPol2 := 0;
  F := 0.5 / Coef^[2];
  Z^[1].X := - F * Coef^[1];
  Z^[1].Y := Abs(F) * Sqrt(- Delta);
  Z^[2].X := Z^[1].X;
  Z^[2].Y := - Z^[1].Y;
end;

end.
