{ ******************************************************************
  Inverses of incomplete Gamma function and Khi-2 distribution
  Translated from C code in Cephes library (http://www.moshier.net)
  ****************************************************************** }

unit uinvgam;

interface

uses
  utypes, ugamma, uigamma, uinvnorm;

function InvGamma(A, P : Float) : Float;
{ ------------------------------------------------------------------
  Given P, the function finds X such that

  IGamma(A, X) = P

  It is best valid in the right-hand tail of the distribution, P > 0.5
  ------------------------------------------------------------------ }

function InvKhi2(Nu : Integer; P : Float) : Float;
{ ------------------------------------------------------------------
  Inverse of Khi-2 distribution function

  Returns the argument, X, for which the area under the Khi-2
  probability density function (integrated from 0 to X)
  is equal to P.
  ------------------------------------------------------------------ }

implementation

function InvGamma(A, P : Float) : Float;

var
  x0, x1, x, Y, yl, yh, y1, d, lgm, dithresh : Float;
  i, ndir : Integer;

label
  ihalve, cont, cont1, done;

begin
  if P > 0.5 then SetErrCode(FPLoss) else SetErrCode(FOk);

  Y := 1.0 - P;

  { Bound the solution }
  x0 := MaxNum;
  yl := 0.0;
  x1 := 0.0;
  yh := 1.0;
  dithresh := 5 * MachEp;

  { Approximation to inverse function }
  d := 1.0 / (9.0 * a);
  y1 := 1.0 - d - InvNorm(Y) * sqrt(d);
  x := a * y1 * y1 * y1;

  lgm := LnGamma(a);

  for i := 0 to 9 do
  begin
    if (x > x0) or (x < x1) then goto ihalve;
    y1 := JGamma(a, x);
    if (y1 < yl) or (y1 > yh) then goto ihalve;
    if y1 < Y then
    begin
      x0 := x;
      yl := y1
    end
    else
    begin
      x1 := x;
      yh := y1
    end;

    { Compute the derivative of the function at this point }
    d := (a - 1) * Ln(x) - x - lgm;
    if d < MinLog then goto ihalve;
    d := -exp(d);

    { Compute the step to the next approximation of x }
    d := (y1 - Y) / d;
    if abs(d / x) < MachEp then goto done;
    x := x - d;
  end;

  { Resort to interval halving if Newton iteration did not converge }
ihalve:

  d := 0.0625;
  if x0 = MaxNum then
  begin
    if x <= 0 then x := 1;
    while x0 = MaxNum do
    begin
      x := (1 + d) * x;
      y1 := JGamma(a, x);
      if y1 < Y then
      begin
        x0 := x;
        yl := y1;
        goto cont
      end;
      d := d + d
    end
  end;

cont:
  d := 0.5;
  ndir := 0;

  for i := 0 to 399 do
  begin
    x := x1 + d * (x0 - x1);
    y1 := JGamma(a, x);
    lgm := (x0 - x1) / (x1 + x0);
    if abs(lgm) < dithresh then goto cont1;
    lgm := (y1 - Y) / Y;
    if abs(lgm) < dithresh then goto cont1;
    if x <= 0 then goto cont1;
    if y1 >= Y then
    begin
      x1 := x;
      yh := y1;
      if ndir < 0 then
      begin
        ndir := 0;
        d := 0.5
      end
      else if ndir > 1 then
        d := 0.5 * d + 0.5
      else
        d := (Y - yl) / (yh - yl);
      ndir := ndir + 1;
    end
    else
    begin
      x0 := x;
      yl := y1;
      if ndir > 0 then
      begin
        ndir := 0;
        d := 0.5
      end
      else if ndir < -1 then
        d := 0.5 * d
      else
        d := (Y - yl) / (yh - yl);
      ndir := ndir - 1;
    end;
  end;

cont1:
  if x = 0 then SetErrCode(FUnderflow);

done:
  InvGamma := x
end;

function InvKhi2(Nu : Integer; P : Float) : Float;
{ ------------------------------------------------------------------
  Inverse of Khi-2 distribution function

  Returns the argument, X, for which the area under the Khi-2
  probability density function (integrated from 0 to X)
  is equal to P.
  ------------------------------------------------------------------ }
begin
  if (P < 0.0) or (P > 1.0) or (Nu < 1) then
    InvKhi2 := DefaultVal(FDomain, 0.0)
  else
    InvKhi2 := 2.0 * InvGamma(0.5 * Nu, P);
end;

end.




