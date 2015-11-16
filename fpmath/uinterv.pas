{ ******************************************************************
  Compute an appropriate interval for a set of values
  ****************************************************************** }

unit uinterv;

interface

uses
  utypes, umath;

procedure Interval(X1, X2             : Float;
                   MinDiv, MaxDiv     : Integer;
                   var Min, Max, Step : Float);
{ ------------------------------------------------------------------
  Determines an interval [Min, Max] including the values from X1
  to X2, and a subdivision Step of this interval
  ------------------------------------------------------------------
  Input parameters  : X1, X2 = min. & max. values to be included
                      MinDiv = minimum nb of subdivisions
                      MaxDiv = maximum nb of subdivisions
  ------------------------------------------------------------------
  Output parameters : Min, Max, Step
  ------------------------------------------------------------------ }

implementation

procedure Interval(X1, X2             : Float;
                   MinDiv, MaxDiv     : Integer;
                   var Min, Max, Step : Float);

  var
    H, R, K : Float;
  begin
    if X1 >= X2 then Exit;
    H := X2 - X1;
    R := Int(Log10(H));
    if H < 1.0 then R := R - 1.0;
    Step := Exp10(R);

    repeat
      K := Int(H / Step);
      if K < MinDiv then Step := 0.5 * Step;
      if K > MaxDiv then Step := 2.0 * Step;
    until (K >= MinDiv) and (K <= MaxDiv);

    Min := Step * Int(X1 / Step);
    Max := Step * Int(X2 / Step);
    while Min > X1 do Min := Min - Step;
    while Max < X2 do Max := Max + Step;
  end;

end.
