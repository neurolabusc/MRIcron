{ ******************************************************************
  This program computes the area under an experimental curve
  by the trapezoidal rule. The result is compared with the
  exact value.

  Data are generated with the function exp(-x) for x = 0..1

  The exact integral is:

  (1
  |  exp(-x) dx = 1 - exp(-1) ~ 0.6321
  )0
  ****************************************************************** }

program Trap;

uses
  tpmath;

const
  N = 10;

var
  X, Y : PVector;
  I    : Integer;

begin
  DimVector(X, N);
  DimVector(Y, N);

  for I := 0 to N do
    begin
      X^[I] := 0.1 * I;
      Y^[I] := Exp(- X^[I]);
    end;

  WriteLn('     X         Y');
  WriteLn('--------------------');

  for I := 0 to N do
    WriteLn(X^[I]:10:4, Y^[I]:10:4);

  WriteLn('--------------------');

  WriteLn;
  WriteLn('Area under curve:');
  WriteLn;
  WriteLn('TrapInt: ', TrapInt(X, Y, N):10:4);
  WriteLn('Exact  : ', 1.0 - Exp(- 1.0):10:4);
end.
