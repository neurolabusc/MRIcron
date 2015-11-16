{ ******************************************************************
  Trigonometric functions
  ****************************************************************** }

unit utrigo;

interface

uses
  utypes, uminmax;

function Pythag(X, Y : Float) : Float;     { Sqrt(X^2 + Y^2) }
function FixAngle(Theta : Float) : Float;  { Set Theta in -Pi..Pi }
function Tan(X : Float) : Float;           { Tangent }
function ArcSin(X : Float) : Float;        { Arc sinus }
function ArcCos(X : Float) : Float;        { Arc cosinus }
function ArcTan2(Y, X : Float) : Float;    { Angle (Ox, OM) with M(X,Y) }

implementation

  function Pythag(X, Y : Float) : Float;
  { Computes Sqrt(X^2 + Y^2) without destructive underflow or overflow }
  var
    AbsX, AbsY : Float;
  begin
    SetErrCode(FOk);
    AbsX := Abs(X);
    AbsY := Abs(Y);
    if AbsX > AbsY then
      Pythag := AbsX * Sqrt(1.0 + Sqr(AbsY / AbsX))
    else if AbsY = 0.0 then
      Pythag := 0.0
    else
      Pythag := AbsY * Sqrt(1.0 + Sqr(AbsX / AbsY));
  end;

  function FixAngle(Theta : Float) : Float;
  begin
    SetErrCode(FOk);
    while Theta > Pi do
      Theta := Theta - TwoPi;
    while Theta <= - PI do
      Theta := Theta + TwoPi;
    FixAngle := Theta;
  end;

  function Tan(X : Float) : Float;
  var
    SinX, CosX : Float;
  begin
    SetErrCode(FOk);
    SinX := Sin(X);
    CosX := Cos(X);
    if CosX = 0.0 then
      Tan := DefaultVal(FSing, Sgn(SinX) * MaxNum)
    else
      Tan := SinX / CosX;
  end;

  function ArcSin(X : Float) : Float;
  begin
    SetErrCode(FOk);
    if (X < - 1.0) or (X > 1.0) then
      ArcSin := DefaultVal(FDomain, 0.0)
    else if X = 1.0 then
      ArcSin := PiDiv2
    else if X = - 1.0 then
      ArcSin := - PiDiv2
    else
      ArcSin := ArcTan(X / Sqrt(1.0 - Sqr(X)));
  end;

  function ArcCos(X : Float) : Float;
  begin
    SetErrCode(FOk);
    if (X < - 1.0) or (X > 1.0) then
      ArcCos := DefaultVal(FDomain, 0.0)
    else if X = 1.0 then
      ArcCos := 0.0
    else if X = - 1.0 then
      ArcCos := Pi
    else
      ArcCos := PiDiv2 - ArcTan(X / Sqrt(1.0 - Sqr(X)));
  end;

  function ArcTan2(Y, X : Float) : Float;
  var
    Theta : Float;
  begin
    SetErrCode(FOk);
    if X = 0.0 then
      if Y = 0.0 then
        ArcTan2 := 0.0
      else if Y > 0.0 then
        ArcTan2 := PiDiv2
      else
        ArcTan2 := - PiDiv2
    else
      begin
        { 4th/1st quadrant -Pi/2..Pi/2 }
        Theta := ArcTan(Y / X);

        { 2nd/3rd quadrants }
        if X < 0.0 then
          if Y >= 0.0 then
            Theta := Theta + Pi   { 2nd quadrant:  Pi/2..Pi }
          else
            Theta := Theta - Pi;  { 3rd quadrant: -Pi..-Pi/2 }
        ArcTan2 := Theta;
      end;
  end;

end.