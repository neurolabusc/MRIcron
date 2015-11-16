{ ******************************************************************
  Hyperbolic functions
  ****************************************************************** }

unit uhyper;

interface

uses
  utypes, uminmax;

function Sinh(X : Float) : Float;     { Hyperbolic sine }
function Cosh(X : Float) : Float;     { Hyperbolic cosine }
function Tanh(X : Float) : Float;     { Hyperbolic tangent }
function ArcSinh(X : Float) : Float;  { Inverse hyperbolic sine }
function ArcCosh(X : Float) : Float;  { Inverse hyperbolic cosine }
function ArcTanh(X : Float) : Float;  { Inverse hyperbolic tangent }

procedure SinhCosh(X : Float; var SinhX, CoshX : Float);  { Sinh & Cosh }

implementation

  function Sinh(X : Float) : Float;
  var
    ExpX : Float;
  begin
    if (X < MinLog) or (X > MaxLog) then
      Sinh := DefaultVal(FOverflow, Sgn(X) * MaxNum)
    else
      begin
        ExpX := Exp(X);
        Sinh := 0.5 * (ExpX - 1.0 / ExpX);
        SetErrCode(FOk);
      end;
  end;

  function Cosh(X : Float) : Float;
  var
    ExpX : Float;
  begin
    if (X < MinLog) or (X > MaxLog) then
      Cosh := DefaultVal(FOverflow, MaxNum)
    else
      begin
        ExpX := Exp(X);
        Cosh := 0.5 * (ExpX + 1.0 / ExpX);
        SetErrCode(FOk);
      end;
  end;

  procedure SinhCosh(X : Float; var SinhX, CoshX : Float);
  var
    ExpX, ExpMinusX : Float;
  begin
    if (X < MinLog) or (X > MaxLog) then
      begin
        CoshX := DefaultVal(FOverflow, MaxNum);
        SinhX := Sgn(X) * CoshX;
      end
    else
      begin
        ExpX := Exp(X);
        ExpMinusX := 1.0 / ExpX;
        SinhX := 0.5 * (ExpX - ExpMinusX);
        CoshX := 0.5 * (ExpX + ExpMinusX);
        SetErrCode(FOk);
      end;
  end;

  function Tanh(X : Float) : Float;
  var
    SinhX, CoshX : Float;
  begin
    SinhCosh(X, SinhX, CoshX);
    Tanh := SinhX / CoshX;
  end;

  function ArcSinh(X : Float) : Float;
  begin
    SetErrCode(FOk);
    ArcSinh := Ln(X + Sqrt(Sqr(X) + 1.0));
  end;

  function ArcCosh(X : Float) : Float;
  begin
    SetErrCode(FOk);
    if X < 1.0 then
      ArcCosh := DefaultVal(FDomain, 0.0)
    else
      ArcCosh := Ln(X + Sqrt(Sqr(X) - 1.0));
  end;

  function ArcTanh(X : Float) : Float;
  begin
    SetErrCode(FOk);
    if (X < - 1.0) or (X > 1.0) then
      ArcTanh := DefaultVal(FDomain, Sgn(X) * MaxNum)
    else if (X = - 1.0) or (X = 1.0) then
      ArcTanh := Sgn(X) * DefaultVal(FSing, Sgn(X) * MaxNum)
    else
      ArcTanh := 0.5 * Ln((1.0 + X) / (1.0 - X));
  end;

end.