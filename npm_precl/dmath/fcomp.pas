{ **********************************************************************
  *                            Unit FCOMP.PAS                          *
  *                              Version 1.1                           *
  *                       (c) J. Debord, July 2000                     *                                
  **********************************************************************
                        Complex functions for TPMATH
                     (Based on CMPLX.ZIP by E.F. Glynn)
  ********************************************************************** }

unit FComp;

interface

uses
  FMath;

{ **********************************************************************
  Complex type
  ********************************************************************** }

type
  ComplexForm = (Rec, Pol);  { Rectangular or Polar form }

  Complex = record
    case Form : ComplexForm of
      Rec : (X, Y : Float);
      Pol : (R, Theta : Float);
  end;

const
  C_infinity : Complex = (Form : Rec; X : MAXNUM; Y : 0.0);
  C_zero     : Complex = (Form : Rec; X : 0.0;    Y : 0.0);
  C_one      : Complex = (Form : Rec; X : 1.0;    Y : 0.0);
  C_i        : Complex = (Form : Rec; X : 0.0;    Y : 1.0);
  C_pi       : Complex = (Form : Rec; X : PI;     Y : 0.0);
  C_pi_div_2 : Complex = (Form : Rec; X : PIDIV2; Y : 0.0);
  
{ **********************************************************************
  Complex number initialization and conversion
  ********************************************************************** }

procedure CSet(var Z : Complex; A, B : Float; F : ComplexForm);
{ ----------------------------------------------------------------------
  Initializes a complex number according to the form specified by F
    F = Rec  ==>  Z = A + i * B
    F = Pol  ==>  Z = A * Exp(i * B)
  ---------------------------------------------------------------------- }

procedure CConvert(var Z : Complex; F : ComplexForm);
{ Converts the complex number Z to the form specified by F }

procedure CSwap(var X, Y : Complex);
{ Exchanges two complex numbers }
  
{ **********************************************************************
  Complex functions
  ********************************************************************** }

function CReal(Z : Complex) : Float;  { Re(Z) }
function CImag(Z : Complex) : Float;  { Im(Z) }
function CAbs(Z : Complex) : Float;   { |Z| }
function CArg(Z : Complex) : Float;   { Arg(Z) }
function CSgn(Z : Complex) : Integer; { Complex sign }

procedure CNeg(A : Complex; var Z : Complex);      { Z = -A }
procedure CConj(A : Complex; var Z : Complex);     { Z = A* }
procedure CAdd(A, B : Complex; var Z : Complex);   { Z = A + B }
procedure CSub(A, B : Complex; var Z : Complex);   { Z = A - B }
procedure CDiv(A, B : Complex; var Z : Complex);   { Z = A / B }
procedure CMult(A, B : Complex; var Z : Complex);  { Z = A * B }
procedure CLn(A : Complex; var Z : Complex);       { Z = Ln(A) }
procedure CExp(A : Complex; var Z : Complex);      { Z = Exp(A) }
procedure CPower(A, B : Complex; var Z : Complex); { Z = A^B }

procedure CIntPower(A : Complex; N : Integer; var Z : Complex);  { Z = A^N }
procedure CRealPower(A : Complex; X : Float; var Z : Complex);   { Z = A^X }
procedure CSqrt(A : Complex; var Z : Complex);                   { Z = Sqrt(A) }
procedure CRoot(A : Complex; K, N : Integer; var Z : Complex);   { Z = A^(1/N) }

procedure CSin(A : Complex; var Z : Complex);      { Z = Sin(A) }
procedure CCos(A : Complex; var Z : Complex);      { Z = Cos(A) }
procedure CTan(A : Complex; var Z : Complex);      { Z = Tan(A) }

procedure CArcSin(A : Complex; var Z : Complex);   { Z = ArcSin(A) }
procedure CArcCos(A : Complex; var Z : Complex);   { Z = ArcCos(A) }
procedure CArcTan(A : Complex; var Z : Complex);   { Z = ArcTan(A) }

procedure CSinh(A : Complex; var Z : Complex);     { Z = Sinh(A) }
procedure CCosh(A : Complex; var Z : Complex);     { Z = Cosh(A) }
procedure CTanh(A : Complex; var Z : Complex);     { Z = Tanh(A) }

procedure CArcSinh(A : Complex; var Z : Complex);  { Z = ArcSinh(A) }
procedure CArcCosh(A : Complex; var Z : Complex);  { Z = ArcCosh(A) }
procedure CArcTanh(A : Complex; var Z : Complex);  { Z = ArcTanh(A) }

procedure CLnGamma(A : Complex; var Z : Complex);  { Z = Ln(Gamma(A)) }

implementation

{$IFDEF CPU387}
  {$DEFINE USE_ASM}
{$ENDIF}

{$IFDEF CPUP2}
  {$DEFINE USE_ASM}
{$ENDIF}

  procedure CSet(var Z : Complex; A, B : Float; F : ComplexForm);
  begin
    Z.Form := F;
    if F = Pol then
      begin
        Z.R := A;
        Z.Theta := B;
      end
    else
      begin
        Z.X := A;
        Z.Y := B;
      end;
  end;

  function CAbs(Z : Complex) : Float;
  begin
    if Z.Form = Rec then
      CAbs := Pythag(Z.X, Z.Y)
    else
      CAbs := Z.R;
  end;

  function CArg(Z : Complex) : Float;
  begin
    if Z.Form = Rec then
      CArg := ArcTan2(Z.Y, Z.X)
    else
      CArg := Z.Theta;
  end;

  function CReal(Z : Complex) : Float;
  begin
    if Z.Form = Rec then
      CReal := Z.X
    else
      CReal := Z.R * {$IFDEF USE_ASM}fCos{$ELSE}Cos{$ENDIF}(Z.Theta);
  end;

  function CImag(Z : Complex) : Float;
  begin
    if Z.Form = Rec then
      CImag := Z.Y
    else
      CImag := Z.R * {$IFDEF USE_ASM}fSin{$ELSE}Sin{$ENDIF}(Z.Theta);
  end;

  function CSgn(Z : Complex) : Integer;
  var
    Re, Im : Float;
  begin
    Re := CReal(Z);
    if Re > 0.0 then
      CSgn := 1
    else if Re < 0.0 then
      CSgn := - 1
    else
      begin
        Im := CImag(Z);
        if Im > 0.0 then
          CSgn := 1
        else if Im < 0.0 then
          CSgn := - 1
        else
          CSgn := 0;
      end;
  end;

  procedure CConvert(var Z : Complex; F : ComplexForm);
  var
    A : Complex;
  begin
    if Z.Form = F then Exit;
    if Z.Form = Pol then
      begin               { Polar-to-rectangular conversion }
        A.Form := Rec;
        A.X := Z.R * {$IFDEF USE_ASM}fCos{$ELSE}Cos{$ENDIF}(Z.Theta);
        A.Y := Z.R * {$IFDEF USE_ASM}fSin{$ELSE}Sin{$ENDIF}(Z.Theta);
      end
    else
      begin               { Rectangular-to-polar conversion }
        A.Form := Pol;
        if Z.X = 0.0 then
          if Z.Y = 0.0 then
            A.R := 0.0
          else if Z.Y > 0.0 then
            A.R := Z.Y
          else
            A.R := - Z.Y
        else
          A.R := CAbs(Z);
        A.Theta := ArcTan2(Z.Y, Z.X);
      end;
    Z := A;
  end;

  procedure CSwap(var X, Y : Complex);
  var
    Temp : Complex;
  begin
    Temp := X;
    X := Y;
    Y := Temp;
  end;

  procedure CNeg(A : Complex; var Z : Complex);
  begin
    Z.Form := A.Form;
    if A.Form = Pol then
      begin
        Z.R := A.R;
        Z.Theta := FixAngle(A.Theta + PI)
      end
    else
      begin
        Z.X := - A.X;
        Z.Y := - A.Y
      end;
  end;

  procedure CConj(A : Complex; var Z : Complex);
  begin
    Z.Form := A.Form;
    if A.Form = Pol then
      begin
        Z.R := A.R;
        Z.Theta := FixAngle(- A.Theta)
      end
    else
      begin
        Z.X := A.X;
        Z.Y := - A.Y
      end
  end;

  procedure CAdd(A, B : Complex; var Z : Complex);
  begin
    CConvert(A, Rec);
    CConvert(B, Rec);
    Z.Form := Rec;
    Z.X := A.X + B.X;
    Z.Y := A.Y + B.Y;
  end;

  procedure CSub(A, B : Complex; var Z : Complex);
  begin
    CConvert(A, Rec);
    CConvert(B, Rec);
    Z.Form := Rec;
    Z.X := A.X - B.X;
    Z.Y := A.Y - B.Y;
  end;

  procedure CMult(A, B : Complex; var Z : Complex);
  begin
    CConvert(B, A.Form);  { arbitrarily convert one to type of other }
    Z.Form := A.Form;
    if A.Form = Pol then
      begin
        Z.R := A.R * B.R;
        Z.Theta := FixAngle(A.Theta + B.Theta)
      end
    else
      begin
        Z.X := A.X * B.X - A.Y * B.Y;
        Z.Y := A.X * B.Y + A.Y * B.X
      end;
  end;

  procedure CDiv(A, B : Complex; var Z : Complex);
  var
    Temp : Float;
  begin
    if ((B.Form = Rec) and (B.X = 0.0) and (B.Y = 0.0)) or
    ((B.Form = Pol) and (B.R = 0.0)) then
      begin
        MathErr := FN_OVERFLOW;
        Z := C_infinity;
        Exit;
      end;

    CConvert(B, A.Form);  { arbitrarily convert one to type of other }
    Z.Form := A.Form;
    if A.Form = Pol then
      begin
        Z.R := A.R / B.R;
        Z.Theta := FixAngle(A.Theta - B.Theta);
      end
    else
      begin
        Temp := Sqr(B.X) + Sqr(B.Y);
        Z.X := (A.X * B.X + A.Y * B.Y) / Temp;
        Z.Y := (A.Y * B.X - A.X * B.Y) / Temp;
      end;
  end;

  procedure CLn(A : Complex; var Z : Complex);
  var
    LnR : Float;
  begin
    CConvert(A, Pol);
    LnR := Log(A.R);
    if MathErr = FN_OK then
      CSet(Z, LnR, FixAngle(A.Theta), Rec)
    else
      CSet(Z, - MAXNUM, 0.0, Rec);
  end;

  procedure CExp(A : Complex; var Z : Complex);
  var
    ExpX, SinY, CosY : Float;
  begin
    CConvert(A, Rec);
    ExpX := Expo(A.X);
    if MathErr = FN_OK then
      begin
        SinY := {$IFDEF USE_ASM}fSin{$ELSE}Sin{$ENDIF}(A.Y);
        CosY := {$IFDEF USE_ASM}fCos{$ELSE}Cos{$ENDIF}(A.Y);
        CSet(Z, ExpX * CosY, ExpX * SinY, Rec);
      end
    else
      CSet(Z, ExpX, 0.0, Rec);
  end;

  procedure CPower(A, B : Complex; var Z : Complex);
  var
    BLnA, LnA : Complex;
  begin
    CConvert(A, Rec);
    CConvert(B, Rec);
    if (A.X = 0.0) and (A.Y = 0.0) then
      if (B.X = 0.0) and (B.Y = 0.0) then
        Z := C_one                         { lim a^a = 1 as a -> 0 }
      else
        Z := C_zero                        { 0^b = 0, b > 0 }
    else
      begin
        CLn(A, LnA);
        CMult(B, LnA, BLnA);
        CExp(BLnA, Z);
      end;
  end;

  procedure CIntPower(A : Complex; N : Integer; var Z : Complex);
  { CIntPower directly applies DeMoivre's theorem to calculate an integer
    power of a complex number. The formula holds for both positive and
    negative values of N }
  begin
    CConvert(A, Pol);
    if A.R = 0.0 then
      if N = 0 then
        Z := C_one
      else if N > 0 then
        Z := C_zero
      else
        begin
          MathErr := FN_SING;
          Z := C_infinity;
        end
    else
      CSet(Z, IntPower(A.R, N), FixAngle(N * A.Theta), Pol);
  end;

  procedure CRealPower(A : Complex; X : Float; var Z : Complex);
  begin
    CConvert(A, Pol);
    if A.R = 0.0 then
      if X = 0.0 then
        Z := C_one
      else if X > 0.0 then
        Z := C_zero
      else
        begin
          MathErr := FN_SING;
          Z := C_infinity;
        end
    else
      CSet(Z, Power(A.R, X), FixAngle(X * A.Theta), Pol);
  end;

  procedure CRoot(A : Complex; K, N : Integer; var Z : Complex);
  { CRoot can calculate all 'N' roots of 'A' by varying 'K' from 0..N-1 }
  { This is another application of DeMoivre's theorem. See CIntPower. }
  begin
    if (N <= 0) or (K < 0) or (K >= N) then
      begin
        MathErr := FN_DOMAIN;
        Z := C_zero;
        Exit;
      end;
    CConvert(A, Pol);
    if A.R = 0.0 then
      Z := C_zero
    else
      CSet(Z, Power(A.R, 1.0 / N), FixAngle((A.Theta + K * TWOPI) / N), Pol);
  end;

  procedure CSqrt(A : Complex; var Z : Complex);
  begin
    CConvert(A, Pol);
    if A.R = 0.0 then
      Z := C_zero
    else
      CSet(Z, Sqrt(A.R), FixAngle(0.5 * A.Theta), Pol);
  end;

  procedure CCos(A : Complex; var Z : Complex);
  var
    SinX, CosX, SinhY, CoshY : Float;
  begin
    CConvert(A, Rec);
    SinCos(A.X, SinX, CosX);
    SinhCosh(A.Y, SinhY, CoshY);  { Called here to set MathErr }
    CSet(Z, CosX * CoshY, - SinX * SinhY, Rec)
  end;

  procedure CSin(A : Complex; var Z : Complex);
  var
    SinX, CosX, SinhY, CoshY : Float;
  begin
    CConvert(A, Rec);
    SinCos(A.X, SinX, CosX);
    SinhCosh(A.Y, SinhY, CoshY);  { Called here to set MathErr }
    CSet(Z, SinX * CoshY, CosX * SinhY, Rec)
  end;

  procedure CTan(A : Complex; var Z : Complex);
  var
    X2, Y2, SinX2, CosX2, SinhY2, CoshY2, Temp : Float;
  begin
    CConvert(A, Rec);
    X2 := 2.0 * A.X;
    Y2 := 2.0 * A.Y;
    SinCos(X2, SinX2, CosX2);
    SinhCosh(Y2, SinhY2, CoshY2);
    if MathErr = FN_OK then
      Temp := CosX2 + CoshY2
    else
      Temp := CoshY2;
    if Temp <> 0.0 then
      CSet(Z, SinX2 / Temp, SinhY2 / Temp, Rec)
    else
      begin                  { A = Pi/2 + k*Pi }
        MathErr := FN_SING;
        CSet(Z, MAXNUM, 0.0, Rec);
      end;
  end;

  procedure CCosh(A : Complex; var Z : Complex);
  var
    SinhX, CoshX, SinY, CosY : Float;
  begin
    CConvert(A, Rec);
    SinCos(A.Y, SinY, CosY);
    SinhCosh(A.X, SinhX, CoshX);
    CSet(Z, CoshX * CosY, SinhX * SinY, Rec)
  end;

  procedure CSinh(A : Complex; var Z : Complex);
  var
    SinhX, CoshX, SinY, CosY : Float;
  begin
    CConvert(A, Rec);
    SinCos(A.Y, SinY, CosY);
    SinhCosh(A.X, SinhX, CoshX);
    CSet(Z, SinhX * CosY, CoshX * SinY, Rec)
  end;

  procedure CTanh(A : Complex; var Z : Complex);
  var
    X2, Y2, SinY2, CosY2, SinhX2, CoshX2, Temp : Float;
  begin
    CConvert(A, Rec);
    X2 := 2.0 * A.X;
    Y2 := 2.0 * A.Y;
    SinCos(Y2, SinY2, CosY2);
    SinhCosh(X2, SinhX2, CoshX2);
    if MathErr = FN_OK then
      Temp := CoshX2 + CosY2
    else
      Temp := CoshX2;
    if Temp <> 0.0 then
      CSet(Z, SinhX2 / Temp, SinY2 / Temp, Rec)
    else
      begin                  { A = i * (Pi/2 + k*Pi) }
        MathErr := FN_SING;
        CSet(Z, 0.0, MAXNUM, Rec);
      end;
  end;

  procedure CArcSin(A : Complex; var Z : Complex);
  var
    Rp, Rm, S, T, X2, XX, YY : Float;
    B : Complex;
  begin
    CConvert(A, Rec);
    CSet(B, A.Y, - A.X, Rec);  { Y - i*X }
    X2 := 2.0 * A.X;
    XX := Sqr(A.X);
    YY := Sqr(A.Y);
    S := XX + YY + 1.0;
    Rp := 0.5 * Sqrt(S + X2);
    Rm := 0.5 * Sqrt(S - X2);
    T := Rp + Rm;
    Z.Form := Rec;
    Z.X := ArcSin(Rp - Rm);
    Z.Y := CSgn(B) * Log(T + Sqrt(Sqr(T) - 1.0));
  end;

  procedure CArcCos(A : Complex; var Z : Complex);
  begin
    CArcSin(A, Z);
    CSub(C_pi_div_2, Z, Z);  { Pi/2 - ArcSin(Z) }
  end;

  procedure CArcTan(A : Complex; var Z : Complex);
  var
    XX, Yp1, Ym1 : Float;
  begin
    CConvert(A, Rec);
    if (A.X = 0.0) and (Abs(A.Y) = 1.0) then  { A = +/- i }
      begin
        MathErr := FN_SING;
        CSet(Z, 0.0, Sgn(A.Y) * MAXNUM, Rec);
        Exit;
      end;
    XX := Sqr(A.X);
    Yp1 := A.Y + 1.0;
    Ym1 := A.Y - 1.0;
    Z.Form := Rec;
    Z.X := 0.5 * (ArcTan2(A.X, - Ym1) - ArcTan2(- A.X, Yp1));
    Z.Y := 0.25 * Log((XX + Sqr(Yp1)) / (XX + Sqr(Ym1)));
  end;

  procedure CArcSinh(A : Complex; var Z : Complex);
  { ArcSinH(A) = -i*ArcSin(i*A) }
  begin
    CMult(C_i, A, Z);
    CArcSin(Z, Z);
    CMult(C_i, Z, Z);
    CNeg(Z, Z);
  end;

  procedure CArcCosh(A : Complex; var Z : Complex);
  { ArcCosH(A) = CSgn(Y + i(1-X))*i*ArcCos(A) where A = X+iY }
  var
    B : Complex;
  begin
    CArcCos(A, Z);
    CMult(C_i, Z, Z);
    CSet(B, A.Y, 1.0 - A.X, Rec);     { Y + i*(1-X) }
    if CSgn(B) = -1 then CNeg(Z, Z);
  end;

  procedure CArcTanh(A : Complex; var Z : Complex);
  { ArcTanH(A) = -i*ArcTan(i*A) }
  begin
    CConvert(A, Rec);
    if (Abs(A.X) = 1.0) and (A.Y = 0.0) then  { A = +/- 1 }
      begin
        MathErr := FN_SING;
        CSet(Z, Sgn(A.X) * MAXNUM, 0.0, Rec);
        Exit;
      end;
    CMult(C_i, A, Z);
    CArcTan(Z, Z);
    CMult(C_i, Z, Z);
    CNeg(Z, Z);
  end;

  procedure CApproxLnGamma(Z : Complex; var Sum : Complex);
  { This is the approximation used in the National Bureau of
    Standards "Table of the Gamma Function for Complex Arguments,"
    Applied Mathematics Series 34, 1954. The NBS table was created
    using this approximation over the area 9 < Re(z) < 10 and
    0 < Im(z) < 10. Other table values were computed using the
    relationship:
        _                   _
    ln | (z+1) = ln z + ln | (z) }

  const
    C : array[1..8] of Float =
    (8.33333333333333E-02, - 2.77777777777778E-03,
     7.93650793650794E-04, - 5.95238095238095E-04,
     8.41750841750842E-04, - 1.91752691752692E-03,
     6.41025641025641E-03, - 2.95506535947712E-02);
  var
    I : Integer;
    Powers : array[1..8] of Complex;
    Temp1, Temp2 : Complex;
  begin
    CConvert(Z, Rec);
    CLn(Z, Temp1);                       { Ln(Z) }
    CSet(Temp2, Z.X - 0.5, Z.Y, Rec);    { Z - 0.5 }
    CMult(Temp1, Temp2, Sum);            { (Z - 0.5)*Ln(Z) }
    CSub(Sum, Z, Sum);                   { (Z - 0.5)*ln(Z) - Z }
    Sum.X := Sum.X + LN2PIDIV2;
    Temp1 := C_one;
    CDiv(Temp1, Z, Powers[1]);           { Z^(-1) }
    CMult(Powers[1], Powers[1], Temp2);  { Z^(-2) }
    for I := 2 to 8 do
      CMult(Powers[I - 1], Temp2, Powers[I]);
    for I := 8 downto 1 do
      begin
        CSet(Temp1, C[I] * Powers[I].X, C[I] * Powers[I].Y, Rec);
        CAdd(Sum, Temp1, Sum);
      end
  end;

  procedure CLnGamma(A : Complex; var Z : Complex);
  var
    LnA, Temp : Complex;
  begin
    CConvert(A, Rec);
    if (A.X <= 0.0) and (A.Y = 0.0) then
      if (Int(A.X - 1E-8) - A.X) = 0.0 then  { Negative integer? }
        begin
          MathErr := FN_SING;
          Z := C_infinity;
          Exit
        end;
    if A.Y < 0.0 then    { 3rd or 4th quadrant? }
      begin
        CConj(A, A);
        CLnGamma(A, Z);  { Try again in 1st or 2nd quadrant }
        CConj(Z, Z)      { Left this out! 1/3/91 }
      end
    else
      begin
        if A.X < 9.0 then  { "left" of NBS table range }
          begin
            CLn(A, LnA);
            CSet(A, A.X + 1.0, A.Y, Rec);
            CLnGamma(A, Temp);
            CSub(Temp, LnA, Z)
          end
        else
          CApproxLnGamma(A, Z)  { NBS table range: 9 < Re(z) < 10 }
      end
  end;

end.
