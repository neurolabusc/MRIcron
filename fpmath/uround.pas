{ ******************************************************************
  Rounding functions
  Based on FreeBASIC version contributed by R. Keeling
  ****************************************************************** }

unit uround;

interface

uses
  utypes, uminmax, umath;

function RoundN(X : Float; N : Integer) : Float;
{ Rounds X to N decimal places }

function Ceil(X : Float) : Integer;
{ Ceiling function }

function Floor(X : Float) : Integer;
{ Floor function }

implementation

function RoundN (X : Float; N : Integer) : Float;
const
  MaxRoundPlaces = 18;
var
  ReturnAnswer, Dec_Place : Float;
  I : Integer;
begin
  if (N >= 0) and (N < MaxRoundPlaces) then I := N else I := 0;
  Dec_Place := Exp10(I);
  ReturnAnswer := Int((Abs(X) * Dec_Place) + 0.5);
  RoundN := Sgn(X) * ReturnAnswer / Dec_Place;
end;

function Ceil(X : Float) : Integer;
var
  ReturnAnswer : Integer;
begin
  ReturnAnswer := Trunc(X);
  if ReturnAnswer < X then ReturnAnswer := ReturnAnswer + 1;
  Ceil := ReturnAnswer;
end;

function Floor(X : Float) : Integer;
var
  ReturnAnswer : Integer;
begin
   ReturnAnswer := Trunc(X);
   if ReturnAnswer > X then ReturnAnswer := ReturnAnswer - 1;
   Floor := ReturnAnswer;
end;

end.