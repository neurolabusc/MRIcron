{ ******************************************************************
  Density of standard normal distribution
  ****************************************************************** }

unit unormal;

interface

uses
  utypes;

function DNorm(X : Float) : Float;
{ Density of standard normal distribution }

implementation

function DNorm(X : Float) : Float;
var
  Y : Float;
begin
  Y := - 0.5 * X * X;
  if Y < MinLog then
    DNorm := DefaultVal(FUnderflow, 0.0)
  else
    begin
      SetErrCode(FOk);
      DNorm := InvSqrt2Pi * Exp(Y);
    end;
end;

end.