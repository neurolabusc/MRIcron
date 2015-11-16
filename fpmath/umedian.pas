{ ******************************************************************
  Median
  ****************************************************************** }

unit umedian;

interface

uses
  utypes, uqsort;

function Median(X : PVector; Lb, Ub : Integer; Sorted : Boolean) : Float;
{ ------------------------------------------------------------------
  Sorts vector X in ascending order (if it's not sorted already)
  and returns its median value
  ------------------------------------------------------------------ }

implementation

function Median(X : PVector; Lb, Ub : Integer; Sorted : Boolean) : Float;
  var
    N, N2 : Integer;
  begin
    N := Ub - Lb + 1;
    N2 := N div 2 + Lb - 1;

    if not Sorted then
      QSort(X, Lb, Ub);

    if Odd(N) then
      Median := X^[N2 + 1]
    else
      Median := 0.5 * (X^[N2] + X^[N2 + 1]);
  end;

end.

