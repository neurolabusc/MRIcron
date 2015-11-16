{ ******************************************************************
  Probability functions related to the Gamma function
  ****************************************************************** }

unit ugamdist;

interface

uses
  utypes, ugamma;

function DBeta(A, B, X : Float) : Float;
{ Density of Beta distribution with parameters A and B }

function DGamma(A, B, X : Float) : Float;
{ Density of Gamma distribution with parameters A and B }

function DKhi2(Nu : Integer; X : Float) : Float;
{ Density of Khi-2 distribution with Nu d.o.f. }

function DStudent(Nu : Integer; X : Float) : Float;
{ Density of Student distribution with Nu d.o.f. }

function DSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
{ Density of Fisher-Snedecor distribution with Nu1 and Nu2 d.o.f. }

implementation

function DBeta(A, B, X : Float) : Float;
var
  L : Float;
begin
  if (A <= 0.0) or (B <= 0.0) or (X < 0.0) or (X > 1.0) then
    begin
      DBeta := DefaultVal(FDomain, 0.0);
      Exit;
    end;

  if X = 0.0 then
    begin
      if A < 1.0 then
        DBeta := DefaultVal(FSing, MaxNum)
      else
        DBeta := DefaultVal(FOk, 0.0);
      Exit;
    end;

  if X = 1.0 then
    begin
      if B < 1.0 then
        DBeta := DefaultVal(FSing, MaxNum)
      else
        DBeta := DefaultVal(FOk, 0.0);
      Exit;
    end;

  L := LnGamma(A + B) - LnGamma(A) - LnGamma(B) +
         (A - 1.0) * Ln(X) + (B - 1.0) * Ln(1.0 - X);

  if L < MinLog then
    DBeta := DefaultVal(FUnderflow, 0.0)
  else
    DBeta := DefaultVal(FOk, Exp(L));
end;

function DGamma(A, B, X : Float) : Float;
var
  L : Float;
begin
  if (A <= 0.0) or (B <= 0.0) or (X < 0.0) then
    begin
      DGamma := DefaultVal(FDomain, 0.0);
      Exit;
    end;

  if X = 0.0 then
    begin
      if A < 1.0 then
        DGamma := DefaultVal(FSing, MaxNum)
      else if A = 1.0 then
        DGamma := DefaultVal(FOk, B)
      else
        DGamma := DefaultVal(FOk, 0.0);
      Exit;
    end;

  L := A * Ln(B) - LnGamma(A) + (A - 1.0) * Ln(X) - B * X;

  if L < MinLog then
    DGamma := DefaultVal(FUnderflow, 0.0)
  else
    DGamma := DefaultVal(FOk, Exp(L));
end;

function DKhi2(Nu : Integer; X : Float) : Float;
begin
  DKhi2 := DGamma(0.5 * Nu, 0.5, X)
end;

function DStudent(Nu : Integer; X : Float) : Float;
var
  L, P, Q : Float;
begin
  if Nu < 1 then
    begin
      DStudent := DefaultVal(FDomain, 0.0);
      Exit;
    end;

  P := 0.5 * (Nu + 1);
  Q := 0.5 * Nu;

  L := LnGamma(P) - LnGamma(Q) - 0.5 * Ln(Nu * Pi) -
         P * Ln(1.0 + X * X / Nu);

  if L < MinLog then
    DStudent := DefaultVal(FUnderflow, 0.0)
  else
    DStudent := DefaultVal(FOk, Exp(L));
end;

function DSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
var
  L, P1, P2, R, S : Float;
begin
  if (Nu1 < 1) or (Nu2 < 1) or (X <= 0.0) then
    begin
      DSnedecor := DefaultVal(FDomain, 0.0);
      Exit;
    end;

  R := Nu1 / Nu2;
  P1 := 0.5 * Nu1;
  P2 := 0.5 * Nu2;
  S := P1 + P2;
  L := LnGamma(S) - LnGamma(P1) - LnGamma(P2) +
         P1 * Ln(R) + (P1 - 1.0) * Ln(X) - S * Ln(1.0 + R * X);

  if L < MinLog then
    DSnedecor := DefaultVal(FUnderflow, 0.0)
  else
    DSnedecor := DefaultVal(FOk, Exp(L));
end;

end.