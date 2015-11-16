{ ******************************************************************
  Probability functions related to the incomplete Beta function
  ****************************************************************** }

unit uibtdist;

interface

uses
  utypes, umath, uibeta;

function FBeta(A, B, X : Float) : Float;
{ Cumulative probability for Beta distrib. with param. A and B }

function FBinom(N : Integer; P : Float; K : Integer) : Float;
{ Cumulative probability for binomial distrib. }

function FStudent(Nu : Integer; X : Float) : Float;
{ Cumulative probability for Student distrib. with Nu d.o.f. }

function PStudent(Nu : Integer; X : Float) : Float;
{ Prob(|t| > X) for Student distrib. with Nu d.o.f. }

function FSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
{ Cumulative prob. for Fisher-Snedecor distrib. with Nu1 and Nu2 d.o.f. }

function PSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
{ Prob(F > X) for Fisher-Snedecor distrib. with Nu1 and Nu2 d.o.f. }

implementation

function FBeta(A, B, X : Float) : Float;
begin
  FBeta := IBeta(A, B, X);
end;

function FBinom(N : Integer; P : Float; K : Integer) : Float;
begin
  if (P < 0.0) or (P > 1.0) or (N <= 0) or (N < K) then
    FBinom := DefaultVal(FDomain, 0.0)
  else if K = 0 then
    FBinom := DefaultVal(FOk, Power(1.0 - P, N))
  else if K = N then
    FBinom := DefaultVal(FOk, 1.0)
  else
    FBinom := 1.0 - IBeta(K + 1, N - K, P);
end;

function FStudent(Nu : Integer; X : Float) : Float;
var
  F : Float;
begin
  if Nu < 1 then
    FStudent := DefaultVal(FDomain, 0.0)
  else if X = 0 then
    FStudent := DefaultVal(FOk, 0.5)
  else
    begin
      F := 0.5 * IBeta(0.5 * Nu, 0.5, Nu / (Nu + X * X));
      if X < 0.0 then FStudent := F else FStudent := 1.0 - F;
    end;
end;

function PStudent(Nu : Integer; X : Float) : Float;
begin
  if Nu < 1 then
    PStudent := DefaultVal(FDomain, 0.0)
  else
    PStudent := IBeta(0.5 * Nu, 0.5, Nu / (Nu + X * X));
end;

function FSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
begin
  if (Nu1 < 1) or (Nu2 < 1) or (X <= 0) then
    FSnedecor := DefaultVal(FDomain, 0.0)
  else
    FSnedecor := 1.0 - IBeta(0.5 * Nu2, 0.5 * Nu1, Nu2 / (Nu2 + Nu1 * X));
end;

function PSnedecor(Nu1, Nu2 : Integer; X : Float) : Float;
begin
  if (Nu1 < 1) or (Nu2 < 1) or (X <= 0) then
    PSnedecor := DefaultVal(FDomain, 0.0)
  else
    PSnedecor := IBeta(0.5 * Nu2, 0.5 * Nu1, Nu2 / (Nu2 + Nu1 * X));
end;

end.