{ ******************************************************************
  Probability functions related to the incomplete Gamma function
  ****************************************************************** }

unit uigmdist;

interface

uses
  utypes, uigamma;

function FGamma(A, B, X : Float) : Float;
{ Cumulative probability for Gamma distrib. with param. A and B }

function FPoisson(Mu : Float; K : Integer) : Float;
{ Cumulative probability for Poisson distrib. }

function FNorm(X : Float) : Float;
{ Cumulative probability for standard normal distrib. }

function PNorm(X : Float) : Float;
{ Prob(|U| > X) for standard normal distrib. }

function FKhi2(Nu : Integer; X : Float) : Float;
{ Cumulative prob. for khi-2 distrib. with Nu d.o.f. }

function PKhi2(Nu : Integer; X : Float) : Float;
{ Prob(Khi2 > X) for khi-2 distrib. with Nu d.o.f. }

implementation

function FGamma(A, B, X : Float) : Float;
begin
  FGamma := IGamma(A, B * X);
end;

function FPoisson(Mu : Float; K : Integer) : Float;
begin
  if (Mu <= 0.0) or (K < 0) then
    FPoisson := DefaultVal(FDomain, 0.0)
  else if K = 0 then
    if (- Mu) < MinLog then
      FPoisson := DefaultVal(FUnderflow, 0.0)
    else
      FPoisson := DefaultVal(FOk, Exp(- Mu))
  else
    FPoisson := 1.0 - IGamma(K + 1, Mu);
end;

function FNorm(X : Float) : Float;
begin
  FNorm := 0.5 * (1.0 + Erf(X * Sqrt2div2));
end;

function PNorm(X : Float) : Float;
var
  A : Float;
begin
  A := Abs(X);
  if A = 0.0 then
    PNorm := DefaultVal(FOk, 1.0)
  else if A < 1.0 then
    PNorm := 1.0 - Erf(A * Sqrt2div2)
  else
    PNorm := Erfc(A * Sqrt2div2);
end;

function FKhi2(Nu : Integer; X : Float) : Float;
begin
  if (Nu < 1) or (X <= 0) then
    FKhi2 := DefaultVal(FDomain, 0.0)
  else
    FKhi2 := IGamma(0.5 * Nu, 0.5 * X);
end;

function PKhi2(Nu : Integer; X : Float) : Float;
begin
  if (Nu < 1) or (X <= 0) then
    PKhi2 := DefaultVal(FDomain, 0.0)
  else
    PKhi2 := 1.0 - IGamma(0.5 * Nu, 0.5 * X);
end;

end.