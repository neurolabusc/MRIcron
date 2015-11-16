{ ******************************************************************
  Beta function
  ****************************************************************** }

unit ubeta;

interface

uses
  utypes, ugamma;

function Beta(X, Y : Float) : Float;

implementation

  function Beta(X, Y : Float) : Float;
  { Computes Beta(X, Y) = Gamma(X) * Gamma(Y) / Gamma(X + Y) }
  var
    Lx, Ly, Lxy : Float;
    SgnBeta : Integer;
  begin
    SetErrCode(FOk);

    SgnBeta := SgnGamma(X) * SgnGamma(Y) * SgnGamma(X + Y);

    Lxy := LnGamma(X + Y);
    if MathErr <> FOk then
      begin
        Beta := 0.0;
        Exit;
      end;

    Lx := LnGamma(X);
    if MathErr <> FOk then
      begin
        Beta := SgnBeta * MaxNum;
        Exit;
      end;

    Ly := LnGamma(Y);
    if MathErr <> FOk then
      begin
        Beta := SgnBeta * MaxNum;
        Exit;
      end;

    Beta := SgnBeta * Exp(Lx + Ly - Lxy);
  end;

end.