{ ******************************************************************
  Exponential distribution
  ****************************************************************** }

unit uexpdist;

interface

uses
  utypes;

function DExpo(A, X : Float) : Float;
{ Density of exponential distribution with parameter A }

function FExpo(A, X : Float) : Float;
{ Cumulative probability function for exponential dist. with parameter A }

implementation

function DExpo(A, X : Float) : Float;
var
  Y : Float;
begin
  if (A <= 0.0) or (X < 0.0) then
    begin
      DExpo := DefaultVal(FDomain, 0.0);
      Exit;
    end;

  Y := - A * X;

  if Y < MinLog then
    begin
      DExpo := DefaultVal(FUnderflow, 0.0);
      Exit;
    end;

  SetErrCode(FOk);
  DExpo := A * Exp(Y);
end;

function FExpo(A, X : Float) : Float;
var
  Y : Float;
begin
  if (A <= 0.0) or (X < 0.0) then
    begin
      FExpo := DefaultVal(FDomain, 0.0);
      Exit;
    end;

  Y := - A * X;

  if Y < MinLog then
    begin
      FExpo := DefaultVal(FUnderflow, 1.0);
      Exit;
    end;

  SetErrCode(FOk);
  FExpo := 1.0 - Exp(Y);
end;

end.