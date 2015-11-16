{ ******************************************************************
  Linear equation
  ****************************************************************** }

unit urtpol1;

interface

uses
  utypes;

function RootPol1(A, B : Float; var X : Float) : Integer;
{ ------------------------------------------------------------------
  Solves the linear equation A + B * X = 0
  Returns  1 if no error (B <> 0)
          -1 if X is undetermined (A = B = 0)
          -2 if no solution (A <> 0, B = 0)
  ------------------------------------------------------------------ }

implementation

function RootPol1(A, B : Float; var X : Float) : Integer;
begin
  X := 0.0;

  if B <> 0.0 then
    begin
      if A <> 0.0 then X := - A / B;
      RootPol1 := 1;
      Exit;
    end;

  if A = 0.0 then     { 0 + 0X = 0 }
    RootPol1 := - 1
  else                { A + 0X = 0 }
    RootPol1 := - 2;
end;

end.
