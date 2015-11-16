{ ******************************************************************
  Cubic equation
  ****************************************************************** }

unit urtpol3;

interface

uses
  utypes, urtpol2;

function RootPol3(Coef : PVector; Z : PCompVector) : Integer;
{ ------------------------------------------------------------------
  Solves the cubic equation:
  Coef^[0] + Coef^[1] * X + Coef^[2] * X^2 + Coef^[3] * X^3 = 0
  ------------------------------------------------------------------ }

implementation

function RootPol3(Coef : PVector; Z : PCompVector) : Integer;
const
  OneThird  = 0.333333333333333333;  { 1 / 3       }
  TwoPiDiv3 = 2.09439510239319549;   { 2 Pi / 3    }
  Sqrt3Div2 = 0.866025403784438647;  { Sqrt(3) / 2 }

var
  A, AA, B, C   : Float;
  Q, QQQ, R, RR : Float;
  S, T, U       : Float;
  I             : Integer;
  Cf            : PVector;

begin
  for I := 1 to 3 do
    begin
      Z^[I].X := 0.0;
      Z^[I].Y := 0.0;
    end;

  if Coef^[3] = 0.0 then
    begin
      RootPol3 := RootPol2(Coef, Z);
      Exit;
    end;

  if Coef^[0] = 0.0 then
    begin
      DimVector(Cf, 2);

      { 0 is root. Equation becomes quadratic }
      Cf^[0] := Coef^[1]; Cf^[1] := Coef^[2]; Cf^[2] := Coef^[3];

      { Solve quadratic equation }
      RootPol3 := RootPol2(Cf, Z) + 1;

      DelVector(Cf, 2);
      Exit;
    end;

  if Coef^[3] = 1.0 then
    begin
      A := Coef^[2] * OneThird;
      B := Coef^[1];
      C := Coef^[0];
    end
  else
    begin
      A := Coef^[2] / Coef^[3] * OneThird;
      B := Coef^[1] / Coef^[3];
      C := Coef^[0] / Coef^[3];
    end;

  AA := A * A;

  Q := AA - OneThird * B;
  R := A * (AA - 0.5 * B) + 0.5 * C;
  RR := Sqr(R); QQQ := Q * Sqr(Q);

  if RR < QQQ then  { 3 real roots }
    begin
      RootPol3 := 3;
      S := Sqrt(Q);
      T := R / (Q * S);
      T := PiDiv2 - ArcTan(T / Sqrt(1.0 - T * T));  { ArcCos(T) }
      T := OneThird * T;
      S := - 2.0 * S;
      Z^[1].X := S * Cos(T) - A;
      Z^[2].X := S * Cos(T + TwoPiDiv3) - A;
      Z^[3].X := S * Cos(T - TwoPiDiv3) - A;
    end
  else     { 1 real root }
    begin
      RootPol3 := 1;
      S := Abs(R) + Sqrt(RR - QQQ);
      if S > 0.0 then S := Exp(OneThird * Ln(S));
      if R > 0.0 then S := - S;
      if S = 0.0 then T := 0.0 else T := Q / S;
      U := S + T;
      Z^[1].X := U - A;          { Real root }
      Z^[2].X := - 0.5 * U - A;
      Z^[2].Y := Sqrt3Div2 * Abs(S - T);
      Z^[3].X := Z^[2].X; Z^[3].Y := - Z^[2].Y;
    end;
end;

end.
