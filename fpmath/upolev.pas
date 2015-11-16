{ ******************************************************************
  Polynomial evaluations for special functions.
  Translated from C code in Cephes library (http://www.moshier.net)
  ****************************************************************** }

unit upolev;

interface

uses
  utypes;

type
  TabCoef = array[0..9] of Float;

function PolEvl(var X : Float; Coef : TabCoef; N : Integer) : Float;

function P1Evl(var X : Float; Coef : TabCoef; N : Integer) : Float;

implementation

  function PolEvl(var X : Float; Coef : TabCoef; N : Integer) : Float;
{ ------------------------------------------------------------------
  Evaluates polynomial of degree N:

                        2          N
    y  =  C  + C x + C x  +...+ C x
           0    1     2          N

  Coefficients are stored in reverse order:

  Coef[0] = C  , ..., Coef[N] = C
             N                   0

  The function P1Evl() assumes that Coef[N] = 1.0 and is
  omitted from the array. Its calling arguments are
  otherwise the same as PolEvl().
  ------------------------------------------------------------------ }
  var
    Ans : Float;
    I : Integer;
  begin
    Ans := Coef[0];
    for I := 1 to N do
      Ans := Ans * X + Coef[I];
    PolEvl := Ans;
  end;

  function P1Evl(var X : Float; Coef : TabCoef; N : Integer) : Float;
{ ------------------------------------------------------------------
  Evaluate polynomial when coefficient of X is 1.0.
  Otherwise same as PolEvl.
  ------------------------------------------------------------------ }
  var
    Ans : Float;
    I : Integer;
  begin
    Ans := X + Coef[0];
    for I := 1 to N - 1 do
      Ans := Ans * X + Coef[I];
    P1Evl := Ans;
  end;

end.