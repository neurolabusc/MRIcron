{ ******************************************************************
  Polynomials and rational fractions
  ****************************************************************** }

unit upolynom;

interface

uses
  utypes;

function Poly(X : Float; Coef : PVector; Deg : Integer) : Float;
{ ------------------------------------------------------------------
  Evaluates the polynomial :
  P(X) = Coef[0] + Coef[1] * X + Coef[2] * X^2 + ...
       + Coef[Deg] * X^Deg
  ------------------------------------------------------------------ }

function RFrac(X : Float; Coef : PVector; Deg1, Deg2 : Integer) : Float;
{ ------------------------------------------------------------------
  Evaluates the rational fraction :

           Coef[0] + Coef[1] * X + ... + Coef[Deg1] * X^Deg1
  F(X) = -----------------------------------------------------
         1 + Coef[Deg1+1] * X + ... + Coef[Deg1+Deg2] * X^Deg2
  ------------------------------------------------------------------ }

implementation

function Poly(X : Float; Coef : PVector; Deg : Integer) : Float;
var
  I : Integer;
  P : Float;
begin
  P := Coef^[Deg];
  for I := Pred(Deg) downto 0 do
    P := P * X + Coef^[I];
  Poly := P;
end;

function RFrac(X : Float; Coef : PVector; Deg1, Deg2 : Integer) : Float;
var
  I    : Integer;
  P, Q : Float;
begin
  P := Coef^[Deg1];
  for I := Pred(Deg1) downto 0 do
    P := P * X + Coef^[I];
  Q := 0.0;
  for I := (Deg1 + Deg2) downto Succ(Deg1) do
    Q := (Q + Coef^[I]) * X;
  RFrac := P / (1.0 + Q);
end;

end.
