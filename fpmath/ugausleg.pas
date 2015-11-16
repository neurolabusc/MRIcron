{ ******************************************************************
  Gauss-Legendre integration
  ****************************************************************** }

unit ugausleg;

interface

uses
  utypes;

function GausLeg(Func : TFunc; A, B : Float) : Float;
{ Integral from A to B }

function GausLeg0(Func : TFunc; B : Float) : Float;
{ Integral from 0 to B }

function Convol(Func1, Func2 : TFunc; T : Float) : Float;
{ Convolution product at time T }

implementation

const Npts = 8;  { Number of points / 2 }

const Root : array[1..Npts] of Float =
  (0.0950125098376370440185,
   0.281603550778258913230,
   0.458016777657227386342,
   0.617876244402643748447,
   0.755404408355003033895,
   0.865631202387831743880,
   0.944575023073232576078,
   0.989400934991649932596);

const Weight : array[1..Npts] of Float =
  (0.189450610455068496285,
   0.182603415044923588867,
   0.169156519395002538189,
   0.149595988816576732081,
   0.124628971255533872052,
   0.095158511682492784810,
   0.062253523938647892863,
   0.027152459411754094852);

function GausLeg(Func : TFunc; A, B : Float) : Float;
{ ------------------------------------------------------------------
  Computes the integral of function Func from A to B
  by the Gauss-Legendre method
  ------------------------------------------------------------------ }

var
  P, Q, Sum, X, Y : Float;
  I : Integer;

begin
  P := 0.5 * (B + A);
  Q := 0.5 * (B - A);

  Sum := 0.0;
  for I := 1 to Npts do
    begin
      X := Q * Root[I];
      Y := Func(P + X) + Func(P - X);
      Sum := Sum + Weight[I] * Y;
    end;

  GausLeg := Q * Sum;
end;

function GausLeg0(Func : TFunc; B : Float) : Float;
{ ------------------------------------------------------------------
  Computes the integral of function Func from 0 to B
  by the Gauss-Legendre method
  ------------------------------------------------------------------ }

var
  P, Sum, X, Y : Float;
  I : Integer;

begin
  P := 0.5 * B;

  Sum := 0;
  for I := 1 to Npts do
    begin
      X := P * Root[I];
      Y := Func(P + X) + Func(P - X);
      Sum := Sum + Weight[I] * Y;
    end;

  GausLeg0 := P * Sum;
end;

function Convol(Func1, Func2 : TFunc; T : Float) : Float;
{ ------------------------------------------------------------------
  Computes the convolution product of two functions Func1 and Func2
  at time T by the Gauss-Legendre method
  ------------------------------------------------------------------ }

var
  P, PpX, PmX, Sum, X, Y : Float;
  I : Integer;

begin
  P := 0.5 * T;

  Sum := 0.0;
  for I := 1 to Npts do
    begin
      X := P * Root[I];
      PpX := P + X;
      PmX := P - X;
      Y := Func1(T - PpX) * Func2(PpX) + Func1(T - PmX) * Func2(PmX);
      Sum := Sum + Weight[I] * Y;
    end;

  Convol := P * Sum;
end;

end.