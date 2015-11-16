{ ******************************************************************
  Incomplete Gamma function and related functions.
  Translated from C code in Cephes library (http://www.moshier.net)
  ****************************************************************** }

unit uigamma;

interface

uses
  utypes, ugamma;

function IGamma(A, X : Float) : Float;
{ Incomplete Gamma function}

function JGamma(A, X : Float) : Float;
{ Complement of incomplete Gamma function }

function Erf(X : Float) : Float;
{ Error function }

function Erfc(X : Float) : Float;
{ Complement of error function }

implementation

  function IGamma(A, X : Float) : Float;
  var
    Ans, Ax, C, R : Float;
  begin
    SetErrCode(FOk);

    if (X <= 0.0) or (A <= 0.0) then
      begin
        IGamma := 0.0;
        Exit;
      end;

    if (X > 1.0) and (X > A) then
      begin
        IGamma := 1.0 - JGamma(A, X);
        Exit;
      end;

    Ax := A * Ln(X) - X - LnGamma(A);

    if Ax < MinLog then
      begin
        IGamma := DefaultVal(FUnderflow, 0.0);
        Exit;
      end;

    Ax := Exp(Ax);

    { Power series }
    R := A;
    C := 1.0;
    Ans := 1.0;

    repeat
      R := R + 1.0;
      C := C * X / R;
      Ans := Ans + C;
    until C / Ans <= MachEp;

    IGamma := Ans * Ax / A;
  end;

  function JGamma(A, X : Float) : Float;
  const
    Big = 1.0 / MachEp;
  var
    Ans, C, Yc, Ax, Y, Z, R, T,
    Pk, Pkm1, Pkm2, Qk, Qkm1, Qkm2 : Float;
  begin
    SetErrCode(FOk);

    if (X <= 0.0) or (A <= 0.0) then
      begin
        JGamma := 1.0;
        Exit;
      end;

    if (X < 1.0) or (X < A) then
      begin
        JGamma := 1.0 - IGamma(A, X);
        Exit;
      end;

    Ax := A * Ln(X) - X - LnGamma(A);

    if Ax < MinLog then
      begin
        JGamma := DefaultVal(FUnderflow, 0.0);
        Exit;
      end;

    Ax := Exp(Ax);

    { Continued fraction }
    Y := 1.0 - A;
    Z := X + Y + 1.0;
    C := 0.0;
    Pkm2 := 1.0;
    Qkm2 := X;
    Pkm1 := X + 1.0;
    Qkm1 := Z * X;
    Ans := Pkm1 / Qkm1;

    repeat
      C := C + 1.0;
      Y := Y + 1.0;
      Z := Z + 2.0;
      Yc := Y * C;
      Pk := Pkm1 * Z - Pkm2 * Yc;
      Qk := Qkm1 * Z - Qkm2 * Yc;
      if Qk <> 0.0 then
        begin
          R := Pk / Qk;
          T := Abs((Ans - R) / R);
          Ans := R;
        end
      else
        T := 1.0;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;
      if Abs(Pk) > Big then
        begin
          Pkm2 := Pkm2 * MachEp;
          Pkm1 := Pkm1 * MachEp;
          Qkm2 := Qkm2 * MachEp;
          Qkm1 := Qkm1 * MachEp;
        end;
    until T <= MachEp;

    JGamma := Ans * Ax;
  end;

  function Erf(X : Float) : Float;
  begin
    if X < 0.0 then
      Erf := - IGamma(0.5, Sqr(X))
    else
      Erf := IGamma(0.5, Sqr(X));
  end;

  function Erfc(X : Float) : Float;
  begin
    if X < 0.0 then
      Erfc := 1.0 + IGamma(0.5, Sqr(X))
    else
      Erfc := JGamma(0.5, Sqr(X));
  end;

end.