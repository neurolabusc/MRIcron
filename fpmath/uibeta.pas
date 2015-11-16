{ ******************************************************************
  Incomplete Beta function.
  Translated from C code in Cephes library (http://www.moshier.net)
  ****************************************************************** }

unit uibeta;

interface

uses
  utypes, umath, ugamma;

function IBeta(A, B, X : Float) : Float;
{ Incomplete Beta function}

implementation

const
  Big = 1.0 / MachEp;

  function PSeries(A, B, X : Float) : Float;
  { Power series for incomplete beta integral. Use when B*X is small }
  var
    S, T, U, V, T1, Z, Ai : Float;
    N : Integer;
  begin
    Ai := 1.0 / A;
    U := (1.0 - B) * X;
    V := U / (A + 1.0);
    T1 := V;
    T := U;
    N := 2;
    S := 0.0;
    Z := MachEp * Ai;
    while Abs(V) > Z do
      begin
        U := (N - B) * X / N;
        T := T * U;
        V := T / (A + N);
        S := S + V;
        N := N + 1;
      end;
    S := S + T1;
    S := S + Ai;

    U := A * Ln(X);
    if (A + B < MaxGam) and (Abs(U) < MaxLog) then
      begin
        T := Gamma(A + B) / (Gamma(A) * Gamma(B));
        S := S * T * Power(X, A);
      end
    else
      begin
        T := LnGamma(A + B) - LnGamma(A) - LnGamma(B) + U + Ln(S);
        if T < MinLog then
          S := 0.0
        else
          S := Exp(T);
      end;
    PSeries := S;
  end;

  function CFrac1(A, B, X : Float) : Float;
  { Continued fraction expansion #1 for incomplete beta integral }
  var
    Xk, Pk, Pkm1, Pkm2, Qk, Qkm1, Qkm2,
    K1, K2, K3, K4, K5, K6, K7, K8,
    R, T, Ans, Thresh : Float;
    N : Integer;
  label
    CDone;
  begin
    K1 := A;
    K2 := A + B;
    K3 := A;
    K4 := A + 1.0;
    K5 := 1.0;
    K6 := B - 1.0;
    K7 := K4;
    K8 := A + 2.0;

    Pkm2 := 0.0;
    Qkm2 := 1.0;
    Pkm1 := 1.0;
    Qkm1 := 1.0;
    Ans := 1.0;
    R := 1.0;
    N := 0;
    Thresh := 3.0 * MachEp;

    repeat
      Xk := - (X * K1 * K2) / (K3 * K4);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      Xk := (X * K5 * K6) / (K7 * K8);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      if Qk <> 0.0 then R := Pk / Qk;

      if R <> 0.0 then
        begin
          T := Abs((Ans - R) / R);
          Ans := R;
        end
      else
        T := 1.0;

      if T < Thresh then goto CDone;

      K1 := K1 + 1.0;
      K2 := K2 + 1.0;
      K3 := K3 + 2.0;
      K4 := K4 + 2.0;
      K5 := K5 + 1.0;
      K6 := K6 - 1.0;
      K7 := K7 + 2.0;
      K8 := K8 + 2.0;

      if Abs(Qk) + Abs(Pk) > Big then
        begin
          Pkm2 := Pkm2 * MachEp;
          Pkm1 := Pkm1 * MachEp;
          Qkm2 := Qkm2 * MachEp;
          Qkm1 := Qkm1 * MachEp;
        end;

      if (Abs(Qk) < MachEp) or (Abs(Pk) < MachEp) then
        begin
          Pkm2 := Pkm2 * Big;
          Pkm1 := Pkm1 * Big;
          Qkm2 := Qkm2 * Big;
          Qkm1 := Qkm1 * Big;
        end;
      N := N + 1;
    until N > 400;
    SetErrCode(FPLoss);

CDone:
    CFrac1 := Ans;
  end;

  function CFrac2(A, B, X : Float) : Float;
  { Continued fraction expansion #2 for incomplete beta integral }
  var
    Xk, Pk, Pkm1, Pkm2, Qk, Qkm1, Qkm2,
    K1, K2, K3, K4, K5, K6, K7, K8,
    R, T, Z, Ans, Thresh : Float;
    N : Integer;
  label
    CDone;
  begin
    K1 := A;
    K2 := B - 1.0;
    K3 := A;
    K4 := A + 1.0;
    K5 := 1.0;
    K6 := A + B;
    K7 := A + 1.0;
    K8 := A + 2.0;

    Pkm2 := 0.0;
    Qkm2 := 1.0;
    Pkm1 := 1.0;
    Qkm1 := 1.0;
    Z := X / (1.0 - X);
    Ans := 1.0;
    R := 1.0;
    N := 0;
    Thresh := 3.0 * MachEp;

    repeat
      Xk := - (Z * K1 * K2) / (K3 * K4);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      Xk := (Z * K5 * K6) / (K7 * K8);
      Pk := Pkm1 + Pkm2 * Xk;
      Qk := Qkm1 + Qkm2 * Xk;
      Pkm2 := Pkm1;
      Pkm1 := Pk;
      Qkm2 := Qkm1;
      Qkm1 := Qk;

      if Qk <> 0.0 then R := Pk / Qk;

      if R <> 0.0 then
        begin
          T := Abs((Ans - R) / R);
          Ans := R;
        end
      else
        T := 1.0;

      if T < Thresh then goto CDone;

      K1 := K1 + 1.0;
      K2 := K2 - 1.0;
      K3 := K3 + 2.0;
      K4 := K4 + 2.0;
      K5 := K5 + 1.0;
      K6 := K6 + 1.0;
      K7 := K7 + 2.0;
      K8 := K8 + 2.0;

      if Abs(Qk) + Abs(Pk) > Big then
        begin
          Pkm2 := Pkm2 * MachEp;
          Pkm1 := Pkm1 * MachEp;
          Qkm2 := Qkm2 * MachEp;
          Qkm1 := Qkm1 * MachEp;
        end;

      if (Abs(Qk) < MachEp) or (Abs(Pk) < MachEp) then
        begin
          Pkm2 := Pkm2 * Big;
          Pkm1 := Pkm1 * Big;
          Qkm2 := Qkm2 * Big;
          Qkm1 := Qkm1 * Big;
        end;
      N := N + 1;
    until N > 400;
    SetErrCode(FPLoss);

CDone:
    CFrac2 := Ans;
  end;

  function IBeta(A, B, X : Float) : Float;
  var
    A1, B1, X1, T, W, Xc, Y : Float;
    Flag : Boolean;
  label
    Done;
  begin
    SetErrCode(FOk);

    if (A <= 0.0) or (B <= 0.0) or (X < 0.0) then
      begin
        IBeta := DefaultVal(FDomain, 0.0);
        Exit;
      end;

    if X > 1.0 then
      begin
        IBeta := DefaultVal(FDomain, 1.0);
        Exit;
      end;

    if (X = 0.0) or (X = 1.0) then
      begin
        IBeta := X;
        Exit;
      end;

    Flag := False;
    if (B * X <= 1.0) and (X <= 0.95) then
      begin
        T := PSeries(A, B, X);
        goto Done;
      end;

    W := 1.0 - X;

    { Reverse a and b if x is greater than the mean. }
    if X > A / (A + B) then
      begin
        Flag := True;
        A1 := B;
        B1 := A;
        Xc := X;
        X1 := W;
      end
    else
      begin
        A1 := A;
        B1 := B;
        Xc := W;
        X1 := X;
      end;

    if Flag and (B1 * X1 <= 1.0) and (X1 <= 0.95) then
      begin
        T := PSeries(A1, B1, X1);
        goto Done;
      end;

    { Choose expansion for optimal convergence }
    Y := X1 * (A1 + B1 - 2.0) - (A1 - 1.0);
    if Y < 0.0 then
      W := CFrac1(A1, B1, X1)
    else
      W := CFrac2(A1, B1, X1) / Xc;

    { Multiply w by the factor
     a      b   _             _     _
    x  (1-x)   | (a+b) / ( a | (a) | (b) )    }

    Y := A1 * Ln(X1);
    T := B1 * Ln(Xc);
    if (A1 + B1 < MaxGam) and (Abs(Y) < MaxLog) and (Abs(T) < MaxLog) then
      begin
        T := Power(Xc, B1) ;
        T := T * Power(X1, A1);
        T := T / A1;
        T := T * W;
        T := T * Gamma(A1 + B1) / (Gamma(A1) * Gamma(B1));
      end
    else
      begin
        { Resort to logarithms }
        Y := Y + T + LnGamma(A1 + B1) - LnGamma(A1) - LnGamma(B1) + Ln(W / A1);
        if Y < MinLog then
          T := 0.0
        else
          T := Exp(Y);
      end;

Done:
    if Flag then
      if T <= MachEp then
        T := 1.0 - MachEp
      else
        T := 1.0 - T;

    IBeta := T;
  end;

end.