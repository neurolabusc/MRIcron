{ ******************************************************************
  Poisson distribution
  ****************************************************************** }

unit upoidist;

interface

uses
  utypes;

function PPoisson(Mu : Float; K : Integer) : Float;
{ Probability of Poisson distrib. }

implementation

function PPoisson(Mu : Float; K : Integer) : Float;
var
  P : Float;
  I : Integer;
begin
  if (Mu <= 0.0) or (K < 0) then
    PPoisson := DefaultVal(FDomain, 0.0)
  else if (- Mu) < MinLog then
    PPoisson := DefaultVal(FUnderflow, 0.0)
  else if K = 0 then
    PPoisson := DefaultVal(FOk, Exp(- Mu))
  else
    begin
      P := Mu;
      for I := 2 to K do  { P = Mu^K / K! }
        P := P * Mu / I;
      PPoisson := Exp(- Mu) * P;
      SetErrCode(FOk);
    end;
end;

end.