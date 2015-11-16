{ ******************************************************************
  Inverses of incomplete Beta function, Student and F-distributions
  Translated from C code in Cephes library (http://www.moshier.net)
  ****************************************************************** }

unit uinvbeta;

interface

uses
  utypes, ugamma, uibeta, uinvnorm;

function InvBeta(A, B, Y : Float) : Float;
{ ------------------------------------------------------------------
  Inverse of incomplete Beta function.
  Given P, the function finds X such that IBeta(A, B, X) = Y
  ------------------------------------------------------------------ }

function InvStudent(Nu : Integer; P : Float) : Float;
{ ------------------------------------------------------------------
  Inverse of Student's t-distribution function
  Given probability P, finds the argument X such that
  FStudent(Nu, X) = P
  ------------------------------------------------------------------ }

function InvSnedecor(Nu1, Nu2 : Integer; P : Float) : Float;
{ ------------------------------------------------------------------
  Inverse of Snedecor's F-distribution function
  Given probability P, finds the argument X such that
  FSnedecor(Nu1, Nu2, X) = P
  ------------------------------------------------------------------ }


implementation

function InvBeta(A, B, Y : Float) : Float;

var
  a1, b1, y0, y1, d, x, x0, x1      : Float;
  lgm, yp, di, dithresh, yl, yh, xt : Float;
  i, rflg, ndir, nflg               : Integer;

label
  ihalve, newt, under, noconv, done;

begin
  SetErrCode(FOk);

  if Y <= 0 then
    begin
      InvBeta := 0.0;
      Exit;
    end;

  if Y >= 1 then
    begin
      InvBeta := 1.0;
      Exit;
    end;

  x0 := 0.0;
  yl := 0.0;
  x1 := 1.0;
  yh := 1.0;
  nflg := 0;

  if (A <= 1) or (B <= 1) then
  begin
    dithresh := 1e-6;
    rflg := 0;
    a1 := A;
    b1 := B;
    y0 := Y;
    x := a1 / (a1 + b1);
    y1 := IBeta(a1, b1, x);
    goto ihalve
  end
  else
    dithresh := 1e-4;

{ approximation to inverse function }

  yp := - InvNorm(Y);

  if Y > 0.5 then
  begin
    rflg := 1;
    a1 := B;
    b1 := A;
    y0 := 1.0 - Y;
    yp := -yp;
  end
  else
  begin
    rflg := 0;
    a1 := A;
    b1 := B;
    y0 := Y;
  end;

  lgm := (yp * yp - 3.0) / 6.0;
  x := 2.0 / (1.0 / (2.0 * a1 - 1.0) + 1.0 / (2.0 * b1 - 1.0));
  d := yp * Sqrt(x + lgm) / x
        - (1.0 / (2.0 * b1 - 1.0) - 1.0 / (2.0 * a1 - 1.0))
        * (lgm + 5.0 / 6.0 - 2.0 / (3.0 * x));
  d := 2.0 * d;

  if d < MinLog then goto under;

  x  := a1 / (a1 + b1 * Exp(d));
  y1 := IBeta(a1, b1, x);
  yp := (y1 - y0) / y0;

  if Abs(yp) < 0.2 then goto newt;

{ Resort to interval halving if not close enough }

ihalve:

  ndir := 0;
  di := 0.5;

  for i := 0 to 99 do
  begin
    if i <> 0 then
    begin
      x := x0 + di * (x1 - x0);
      if x = 1.0 then x := 1.0 - MachEp;
      if x = 0.0 then
      begin
        di := 0.5;
        x := x0 + di * (x1 - x0);
        if x = 0.0 then goto under
      end;
      y1 := IBeta(a1, b1, x);
      yp := (x1 - x0) / (x1 + x0);
      if abs(yp) < dithresh then goto newt;
      yp := (y1 - y0) / y0;
      if abs(yp) < dithresh then goto newt;
    end;
    if y1 < y0 then
    begin
      x0 := x;
      yl := y1;
      if ndir < 0 then
      begin
        ndir := 0;
        di := 0.5;
      end
      else if ndir > 3 then
        di := 1.0 - Sqr(1.0 - di)
      else if ndir > 1 then
        di := 0.5 * di + 0.5
      else
        di := (y0 - y1) / (yh - yl);
      ndir := ndir + 1;
      if x0 > 0.75 then
      begin
        if rflg = 1 then
        begin
          rflg := 0;
          a1 := A;
          b1 := B;
          y0 := Y;
        end
        else
        begin
          rflg := 1;
          a1 := B;
          b1 := A;
          y0 := 1.0 - Y;
        end;
        x := 1.0 - x;
        y1 := IBeta(a1, b1, x);
        x0 := 0.0;
        yl := 0.0;
        x1 := 1.0;
        yh := 1.0;
        goto ihalve
      end
    end
    else
    begin
      x1 := x;
      if (rflg = 1) and (x1 < MachEp) then
        begin
          x := 0.0;
          goto done
        end;
      yh := y1;
      if ndir > 0 then
      begin
        ndir := 0;
        di := 0.5
      end
      else if ndir < -3 then
        di := di * di
      else if ndir < -1 then
        di := 0.5 * di
      else
        di := (y1 - y0) / (yh - yl);
      ndir := ndir - 1;
    end;
  end;

  SetErrCode(FPLoss);

  if x0 >= 1.0 then
    begin
      x := 1.0 - MachEp;
      goto done
    end;

  if x <= 0.0 then
  begin
under:
    SetErrCode(FUnderflow);
    x := 0.0;
    goto done;
  end;

newt:

  if nflg = 1 then goto done;

  nflg := 1;
  lgm := LnGamma(a1 + b1) - LnGamma(a1) - LnGamma(b1);

  for i := 0 to 7 do
  begin
    { Compute the function at this point }
    if i <> 0 then y1 := IBeta(a1, b1, x);
    if y1 < yl then
    begin
      x := x0;
      y1 := yl
    end
    else if y1 > yh then
    begin
      x := x1;
      y1 := yh
    end
    else if y1 < y0 then
    begin
      x0 := x;
      yl := y1
    end
    else
    begin
      x1 := x;
      yh := y1
    end;

    if (x = 1.0) or (x = 0.0) then goto noconv;

    { Compute the derivative of the function at this point }
    d := (a1 - 1.0) * Ln(x) + (b1 - 1.0) * Ln(1 - x) + lgm;
    if d < MinLog then goto done;
    if d > MaxLog then goto noconv;
    d := exp(d);

    { Compute the step to the next approximation of x }
    d := (y1 - y0) / d;
    xt := x - d;
    if xt <= x0 then
    begin
      y1 := (x - x0) / (x1 - x0);
      xt := x0 + 0.5 * y1 * (x - x0);
      if xt <= 0.0 then goto noconv;
    end;
    if xt >= x1 then
    begin
      y1 := (x1 - x) / (x1 - x0);
      xt := x1 - 0.5 * y1 * (x1 - x);
      if xt >= 1.0 then goto noconv;
    end;
    x := xt;
    if abs(d / x) < 128.0 * MachEp then goto done
  end;

noconv:
  { Did not converge }
  dithresh := 256.0 * MachEp;
  goto ihalve;

done:

  if rflg = 1 then
    if x <= MachEp then x := 1.0 - MachEp else x := 1.0 - x;

  InvBeta := x;
end;

function InvStudent(Nu : Integer; P : Float) : Float;
var
  t, rk, z : Float;
  rflg     : Integer;
begin
  if (Nu < 1) or (P < 0.0) or (P > 1.0) then
    begin
      InvStudent := DefaultVal(FDomain, 0.0);
      Exit;
    end;

  if P = 0.5 then
    begin
      SetErrCode(FOk);
      InvStudent := 0.0;
      Exit;
    end;

  rk := Nu;

  if (P > 0.25) and (P < 0.75) then
    begin
      z := 1.0 - 2.0 * P;
      z := InvBeta(0.5, 0.5 * rk, Abs(z));
      t := Sqrt(rk * z / (1 - z));
      if P < 0.5 then t := -t;
      InvStudent := t;
      Exit;
    end;

  if P < 0.5 then
    begin
      z := P;
      rflg := -1
    end
  else
    begin
      z := 1.0 - P;
      rflg := 1
    end;

  z := InvBeta(0.5 * rk, 0.5, 2 * z);

  if MaxNum * z < rk then
    begin
      InvStudent := rflg * MaxNum;
      Exit;
    end;

  t := Sqrt(rk / z - rk);
  InvStudent := rflg * t;
end;

function InvSnedecor(Nu1, Nu2 : Integer; P : Float) : Float;
var
  w : Float;
begin
  if (Nu1 < 1) or (Nu2 < 1) or (P < 0.0) or (P > 1.0) then
    begin
      InvSnedecor := DefaultVal(FDomain, 0.0);
      Exit;
    end;

  w := InvBeta(0.5 * Nu2, 0.5 * Nu1, 1.0 - P);
  InvSnedecor := (Nu2 - Nu2 * w) / (Nu1 * w);
end;

end.




