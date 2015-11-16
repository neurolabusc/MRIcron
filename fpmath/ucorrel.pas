{ ******************************************************************
  Correlation coefficient
  ****************************************************************** }

unit ucorrel;

interface

uses
  utypes;

function Correl(X, Y : PVector; Lb, Ub : Integer) : Float;
{ Correlation coefficient between samples X and Y }

implementation

function Correl(X, Y : PVector; Lb, Ub : Integer) : Float;
var
  SX, SY, Xbar, Ybar, DX, DY, SSX, SSY, SP : Float;
  N, I                                     : Integer;
begin
  N := Ub - Lb + 1;

  SX := 0.0;
  SY := 0.0;

  for I := Lb to Ub do
    begin
      SX := SX + X^[I];
      SY := SY + Y^[I];
    end;

  Xbar := SX / N;
  Ybar := SY / N;

  SSX := 0.0;
  SSY := 0.0;
  SP  := 0.0;

  for I := Lb to Ub do
    begin
      DX := X^[I] - Xbar;
      DY := Y^[I] - Ybar;

      SSX := SSX + DX * DX;
      SSY := SSY + DY * DY;
      SP  := SP  + DX * DY;
    end;

  Correl := SP / Sqrt(SSX * SSY);
end;

end.