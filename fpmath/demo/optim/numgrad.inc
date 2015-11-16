{ ******************************************************************
  Numerical gradient
  ****************************************************************** }

procedure Gradient(X, G : PVector);

const
  Eta = 1.0E-4;     { Relative increment }

var
  I     : Integer;  { Loop variable }
  Temp  : Float;    { Temporary variable }
  Delta : Float;    { Increment }
  Xm    : Float;    { X - Delta }
  Xp    : Float;    { X + Delta }
  Fm    : Float;    { F(X - Delta) }
  Fp    : Float;    { F(X + Delta) }

begin
  for I := 1 to Nvar do
    begin
      if X^[I] <> 0.0 then
        Delta := Eta * Abs(X^[I])
      else
        Delta := Eta;

      Xp := X^[I] + Delta;
       Xm := X^[I] - Delta;

      Temp := X^[I];

      X^[I] := Xm;
      Fm := Func(X);

      X^[I] := Xp;
      Fp := Func(X);

      G^[I] := (Fp - Fm) / (2.0 * Delta);

      X^[I] := Temp
    end;
end;
