{ ******************************************************************
  Trapezoidal integration
  ****************************************************************** }

unit utrapint;

interface

uses
  utypes;

function TrapInt(X, Y : PVector; N : Integer) : Float;
{ Integration by trapezoidal rule, from (X^[0], Y^[0]) to (X^[N], Y^[N]) }

implementation

function TrapInt(X, Y : PVector; N : Integer) : Float;
  var
    Sum  : Float;
    I, J : Integer;
  begin
    Sum := 0.0;
    for I := 0 to Pred(N) do
      begin
        J := Succ(I);
        Sum := Sum + 0.5 * (X^[J] - X^[I]) * (Y^[J] + Y^[I]);
      end;
    TrapInt := Sum;
  end;

end.