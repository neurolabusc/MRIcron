{ ******************************************************************
  Mean and standard deviations
  ****************************************************************** }

unit umeansd;

interface

uses
  utypes;

function Mean(X : PVector; Lb, Ub : Integer) : Float;
{ Mean of sample X }

function StDev(X : PVector; Lb, Ub : Integer; M : Float) : Float;
{ Standard deviation estimated from sample X }

function StDevP(X : PVector; Lb, Ub : Integer; M : Float) : Float;
{ Standard deviation of population }

implementation

function Mean(X : PVector; Lb, Ub : Integer) : Float;
var
  SX : Float;
  I  : Integer;
begin
  SX := 0.0;

  for I := Lb to Ub do
    SX := SX + X^[I];

  Mean := SX / (Ub - Lb + 1);
end;

function StDev(X : PVector; Lb, Ub : Integer; M : Float) : Float;
var
  D, SD, SD2, V : Float;
  I, N          : Integer;
begin
  N := Ub - Lb + 1;

  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }

  for I := Lb to Ub do
  begin
    D := X^[I] - M;
    SD := SD + D;
    SD2 := SD2 + Sqr(D)
  end;

  V := (SD2 - Sqr(SD) / N) / (N - 1);  { Variance }
  StDev := Sqrt(V);
end;

function StDevP(X : PVector; Lb, Ub : Integer; M : Float) : Float;
var
  D, SD, SD2, V : Float;
  I, N          : Integer;
begin
  N := Ub - Lb + 1;

  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }

  for I := Lb to Ub do
  begin
    D := X^[I] - M;
    SD := SD + D;
    SD2 := SD2 + Sqr(D)
  end;

  V := (SD2 - Sqr(SD) / N) / N;  { Variance }
  StDevP := Sqrt(V);
end;

end.
