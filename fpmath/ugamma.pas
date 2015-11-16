{ ******************************************************************
  Gamma function and related functions.
  Translated from C code in Cephes library (http://www.moshier.net)
  ****************************************************************** }

unit ugamma;

interface

uses
  utypes, upolev;

function SgnGamma(X : Float) : Integer;
{ Sign of Gamma function }

function Stirling(X : Float) : Float;
{ Stirling's formula for the Gamma function }

function StirLog(X : Float) : Float;
{ Approximate Ln(Gamma) by Stirling's formula, for X >= 13 }

function Gamma(X : Float) : Float;
{ Gamma function }

function LnGamma(X : Float) : Float;
{ Logarithm of Gamma function }

implementation

  function SgnGamma(X : Float) : Integer;
  begin
    if X > 0.0 then
      SgnGamma := 1
    else if Odd(Trunc(Abs(X))) then
      SgnGamma := 1
    else
      SgnGamma := - 1;
  end;

  function Stirling(X : Float) : Float;
  { Stirling's formula for the gamma function
    Gamma(x) = Sqrt(2*Pi) x^(x-.5) exp(-x) (1 + 1/x P(1/x))
    where P(x) is a polynomial }
  const
    STIR : TabCoef = (
        7.147391378143610789273E-4,
      - 2.363848809501759061727E-5,
      - 5.950237554056330156018E-4,
        6.989332260623193171870E-5,
        7.840334842744753003862E-4,
      - 2.294719747873185405699E-4,
      - 2.681327161876304418288E-3,
        3.472222222230075327854E-3,
        8.333333333333331800504E-2,
        0);

  var
    W, P : Float;
  begin
    W := 1.0 / X;
    if X > 1024.0 then
      begin
        P := 6.97281375836585777429E-5 * W + 7.84039221720066627474E-4;
        P := P * W - 2.29472093621399176955E-4;
        P := P * W - 2.68132716049382716049E-3;
        P := P * W + 3.47222222222222222222E-3;
        P := P * W + 8.33333333333333333333E-2;
      end
    else
      P := PolEvl(W, STIR, 8);
    Stirling := Sqrt2Pi * Exp((X - 0.5) * Ln(X) - X) * (1.0 + W * P);
  end;

  function GamSmall(X1, Z : Float) : Float;
  { Gamma function for small values of the argument }
  const
    S : TabCoef = (
      - 1.193945051381510095614E-3,
        7.220599478036909672331E-3,
      - 9.622023360406271645744E-3,
      - 4.219773360705915470089E-2,
        1.665386113720805206758E-1,
      - 4.200263503403344054473E-2,
      - 6.558780715202540684668E-1,
        5.772156649015328608253E-1,
        1.000000000000000000000E0,
        0);

    SN : TabCoef = (
        1.133374167243894382010E-3,
        7.220837261893170325704E-3,
        9.621911155035976733706E-3,
      - 4.219773343731191721664E-2,
      - 1.665386113944413519335E-1,
      - 4.200263503402112910504E-2,
        6.558780715202536547116E-1,
        5.772156649015328608727E-1,
      - 1.000000000000000000000E0,
        0);

  var
    P : Float;
  begin
    if X1 = 0.0 then
      begin
        GamSmall := DefaultVal(FSing, MaxNum);
        Exit;
      end;
    if X1 < 0.0 then
      begin
        X1 := - X1;
        P := PolEvl(X1, SN, 8);
      end
    else
      P := PolEvl(X1, S, 8);
    GamSmall := Z / (X1 * P);
  end;

  function StirLog(X : Float) : Float;
  { Approximate Ln(Gamma) by Stirling's formula, for X >= 13 }
  const
    P : TabCoef = (
        4.885026142432270781165E-3,
      - 1.880801938119376907179E-3,
        8.412723297322498080632E-4,
      - 5.952345851765688514613E-4,
        7.936507795855070755671E-4,
      - 2.777777777750349603440E-3,
        8.333333333333331447505E-2,
        0, 0, 0);

  var
    Q, W : Float;
  begin
    Q := Ln(X) * (X - 0.5) - X;
    Q := Q + LnSqrt2Pi;
    if X > 1.0E+10 then
      StirLog := Q
    else
      begin
        W := 1.0 / Sqr(X);
        StirLog := Q + PolEvl(W, P, 6) / X;
      end;
  end;

  function Gamma(X : Float) : Float;
  const
    P : TabCoef = (
      4.212760487471622013093E-5,
      4.542931960608009155600E-4,
      4.092666828394035500949E-3,
      2.385363243461108252554E-2,
      1.113062816019361559013E-1,
      3.629515436640239168939E-1,
      8.378004301573126728826E-1,
      1.000000000000000000009E0,
      0, 0);

    Q : TabCoef = (
      - 1.397148517476170440917E-5,
        2.346584059160635244282E-4,
      - 1.237799246653152231188E-3,
      - 7.955933682494738320586E-4,
        2.773706565840072979165E-2,
      - 4.633887671244534213831E-2,
      - 2.243510905670329164562E-1,
        4.150160950588455434583E-1,
        9.999999999999999999908E-1,
        0);

  var
    SgnGam, N : Integer;
    A, X1, Z : Float;
  begin
    SetErrCode(FOk);
    SgnGam := SgnGamma(X);

    if (X = 0.0) or ((X < 0.0) and (Frac(X) = 0.0)) then
      begin
        Gamma := DefaultVal(FSing, SgnGam * MaxNum);
        Exit;
      end;

    if X > MaxGam then
      begin
        Gamma := DefaultVal(FOverflow, MaxNum);
        Exit;
      end;

    A := Abs(X);
    if A > 13.0 then
      begin
        if X < 0.0 then
          begin
            N := Trunc(A);
            Z := A - N;
            if Z > 0.5 then
              begin
                N := N + 1;
                Z := A - N;
              end;
            Z := Abs(A * Sin(Pi * Z)) * Stirling(A);
            if Z <= Pi / MaxNum then
              begin
                Gamma := DefaultVal(FOverflow, SgnGam * MaxNum);
                Exit;
              end;
            Z := PI / Z;
          end
        else
          Z := Stirling(X);
        Gamma := SgnGam * Z;
      end
    else
      begin
        Z := 1.0;
        X1 := X;
        while X1 >= 3.0 do
          begin
            X1 := X1 - 1.0;
            Z := Z * X1;
          end;
        while X1 < - 0.03125 do
          begin
            Z := Z / X1;
            X1 := X1 + 1.0;
          end;
        if X1 <= 0.03125 then
          Gamma := GamSmall(X1, Z)
        else
          begin
            while X1 < 2.0 do
              begin
                Z := Z / X1;
                X1 := X1 + 1.0;
              end;
            if (X1 = 2.0) or (X1 = 3.0) then
              Gamma := Z
            else
              begin
                X1 := X1 - 2.0;
                Gamma := Z * PolEvl(X1, P, 7) / PolEvl(X1, Q, 8);
              end;
          end;
      end;
  end;

  function LnGamma(X : Float) : Float;
  const
    P : TabCoef = (
      - 2.163690827643812857640E3,
      - 8.723871522843511459790E4,
      - 1.104326814691464261197E6,
      - 6.111225012005214299996E6,
      - 1.625568062543700591014E7,
      - 2.003937418103815175475E7,
      - 8.875666783650703802159E6,
        0, 0, 0);

    Q : TabCoef = (
      - 5.139481484435370143617E2,
      - 3.403570840534304670537E4,
      - 6.227441164066219501697E5,
      - 4.814940379411882186630E6,
      - 1.785433287045078156959E7,
      - 3.138646407656182662088E7,
      - 2.099336717757895876142E7,
        0, 0, 0);

  var
    N : Integer;
    A, X1, Z : Float;
  begin
    SetErrCode(FOk);

    if (X = 0.0) or ((X < 0.0) and (Frac(X) = 0.0)) then
      begin
        LnGamma := DefaultVal(FSing, MaxNum);
        Exit;
      end;

    if X > MaxLgm then
      begin
        LnGamma := DefaultVal(FOverflow, MaxNum);
        Exit;
      end;

    A := Abs(X);
    if A > 34.0 then
      begin
        if X < 0.0 then
          begin
            N := Trunc(A);
            Z := A - N;
            if Z > 0.5 then
              begin
                N := N + 1;
                Z := N - A;
              end;
            Z := A * Sin(Pi * Z);
            if Z = 0.0 then
              begin
                LnGamma := DefaultVal(FOverflow, MaxNum);
                Exit;
              end;
            Z := LnPi - Ln(Z) - StirLog(A);
          end
        else
          Z := StirLog(X);
        LnGamma := Z;
      end
    else if X < 13.0 then
      begin
        Z := 1.0;
        X1 := X;
        while X1 >= 3 do
          begin
            X1 := X1 - 1.0;
            Z := Z * X1;
          end;
        while X1 < 2.0 do
          begin
            if Abs(X1) <= 0.03125 then
              begin
                LnGamma := Ln(Abs(GamSmall(X1, Z)));
                Exit;
              end;
            Z := Z / X1;
            X1 := X1 + 1.0;
          end;
        if Z < 0.0 then Z := - Z;
        if X1 = 2.0 then
          LnGamma := Ln(Z)
        else
          begin
            X1 := X1 - 2.0;
            LnGamma := X1 * PolEvl(X1, P, 6) / P1Evl(X1, Q, 7) + Ln(Z);
          end;
      end
    else
      LnGamma := StirLog(X);
  end;

end.