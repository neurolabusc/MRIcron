{ ******************************************************************
  Secant method for nonlinear equation
  ****************************************************************** }

unit usecant;

interface

uses
  utypes;

procedure Secant (Func     : TFunc;
                  var X, Y : Float;
                  MaxIter  : Integer;
                  Tol      : Float;
                  var F    : Float);

implementation

procedure Secant (Func     : TFunc;
                  var X, Y : Float;
                  MaxIter  : Integer;
                  Tol      : Float;
                  var F    : Float);

var
  Iter : Integer;
  G, Z : Float;

begin
  Iter := 0;
  SetErrCode(OptOk);

  repeat
    F := Func(X);

    if MaxIter < 1 then Exit;

    G := Func(Y);

    Iter := Iter + 1;

    if (F = G) or (Iter > MaxIter) then
      begin
        SetErrCode(OptNonConv);
        Exit;
      end;

    Z := (X * G - Y * F) / (G - F);

    X := Y;
    Y := Z;
  until Abs(X - Y) < Tol * (Abs(X) + Abs(Y));

  X := 0.5 * (X + Y);
  F := Func(X);
end;

end.