{ ******************************************************************
  Quartic equation
  ****************************************************************** }

unit urtpol4;

interface

uses
  utypes, urtpol2, urtpol3;

function RootPol4(Coef : PVector; Z : PCompVector) : Integer;
{ ------------------------------------------------------------------
  Solves the quartic equation:
  Coef^[0] + Coef^[1] * X + Coef^[2] * X^2 + Coef^[3] * X^3 +
                                             Coef^[4] * X^4 = 0
  ------------------------------------------------------------------ }

implementation

function RootPol4(Coef : PVector; Z : PCompVector) : Integer;
var
  A, AA, B, C, D : Float;
  Q , R , S      : Float;
  K , KK, L, M   : Float;
  I, N1, N2      : Integer;
  Cf             : PVector;
  Z1, Z2         : PCompVector;

  function HighestRealRoot(Deg : Integer; Z : PCompVector) : Float;
  { Find the highest real root among the roots of a polynomial }
  var
    I : Integer;
    R : Float;
  begin
    R := - MaxNum;
    for I := 1 to Deg do
      if (Z^[I].Y = 0.0) and (Z^[I].X > R) then
        R := Z^[I].X;
    HighestRealRoot := R;
  end;

begin
  for I := 1 to 4 do
    begin
      Z^[I].X := 0.0;
      Z^[I].Y := 0.0;
    end;

  if Coef^[4] = 0 then
    begin
      RootPol4 := RootPol3(Coef, Z);
      Exit;
    end;

  DimVector(Cf, 3);

  if Coef^[0] = 0.0 then
    begin
      { 0 is root. Equation becomes cubic }
      Cf^[0] := Coef^[1]; Cf^[1] := Coef^[2]; Cf^[2] := Coef^[3];

      { Solve cubic equation }
      RootPol4 := RootPol3(Cf, Z) + 1;

      DelVector(Cf, 3);
      Exit;
    end;

  if Coef^[4] = 1.0 then
    begin
      A := Coef^[3] * 0.25;
      B := Coef^[2];
      C := Coef^[1];
      D := Coef^[0];
    end
  else
    begin
      A := Coef^[3] / Coef^[4] * 0.25;
      B := Coef^[2] / Coef^[4];
      C := Coef^[1] / Coef^[4];
      D := Coef^[0] / Coef^[4];
    end;

  AA := A * A;

  Q := B - 6.0 * AA;
  R := C + A * (8.0 * AA - 2.0 * B);
  S := D - A * C + AA * (B - 3.0 * AA);

  { Compute coefficients of cubic equation }
  Cf^[3] := 1.0;
  Cf^[2] := 0.5 * Q;
  Cf^[1] := 0.25 * (Sqr(Cf^[2]) - S);

  { Solve cubic equation and set KK = highest real root }
  if (R = 0.0) and (Cf^[1] < 0.0) then
    begin
      { Eq. becomes quadratic with 2 real roots }
      Cf^[0] := Cf^[1]; Cf^[1] := Cf^[2]; Cf^[2] := 1.0;
      N1 := RootPol2(Cf, Z);
      KK := HighestRealRoot(2, Z);
    end
  else
    begin
      Cf^[0] := - 0.015625 * Sqr(R);
      N1 := RootPol3(Cf, Z);
      KK := HighestRealRoot(3, Z);
    end;

  K := Sqrt(KK);
  if K = 0.0 then
    R := Sqrt(Sqr(Q) - 4.0 * S)
  else
    begin
      Q := Q + 4.0 * KK;
      R := 0.5 * R / K;
    end;

  L := 0.5 * (Q - R);
  M := 0.5 * (Q + R);

  { Solve quadratic equation: Y^2 + 2KY + L = 0 }
  DimCompVector(Z1, 2);
  Cf^[0] := L; Cf^[1] := 2.0 * K; Cf^[2] := 1.0;
  N1 := RootPol2(Cf, Z1);

  { Solve quadratic equation: Z^2 - 2KZ + M = 0 }
  DimCompVector(Z2, 2);
  Cf^[0] := M; Cf^[1] := -Cf^[1];
  N2 := RootPol2(Cf, Z2);

  { Transfer roots into vectors Xr and Xi }
  Z^[1].X := Z1^[1].X - A; Z^[1].Y := Z1^[1].Y;
  Z^[2].X := Z1^[2].X - A; Z^[2].Y := Z1^[2].Y;
  Z^[3].X := Z2^[1].X - A; Z^[3].Y := Z2^[1].Y;
  Z^[4].X := Z2^[2].X - A; Z^[4].Y := Z2^[2].Y;

  RootPol4 := N1 + N2;

  DelVector(Cf, 3);
  DelCompVector(Z1, 2);
  DelCompVector(Z2, 2);
end;

end.
