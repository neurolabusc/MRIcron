{ ******************************************************************
  Skewness and kurtosis
  ****************************************************************** }

unit uskew;

interface

uses
  utypes;

function Skewness(X : PVector; Lb, Ub : Integer; M, Sigma : Float) : Float;

function Kurtosis(X : PVector; Lb, Ub : Integer; M, Sigma : Float) : Float;

implementation

function Skewness(X : PVector; Lb, Ub : Integer; M, Sigma : Float) : Float;
  var
    S, T : Float;
    I    : Integer;
  begin
    S := 0.0;
    for I := Lb to Ub do
      begin
        T := (X^[I] - M) / Sigma;
        S := S + T * Sqr(T);
      end;
    Skewness := S / (Ub - Lb + 1);
  end;

function Kurtosis(X : PVector; Lb, Ub : Integer; M, Sigma : Float) : Float;
  var
    S, T : Float;
    I    : Integer;
  begin
    S := 0.0;
    for I := Lb to Ub do
      begin
        T := (X^[I] - M) / Sigma;
        S := S + Sqr(Sqr(T));
      end;
    Kurtosis := S / (Ub - Lb + 1) - 3.0;
  end;

end.
