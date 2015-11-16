{ ******************************************************************
  Numerical hessian and gradient
  ****************************************************************** }

procedure HessGrad(X, G : PVector; H : PMatrix);

const
  Eta = 1.0E-6;     { Relative increment }

var
  Delta, Xminus, Xplus, Fminus, Fplus : PVector;
  Temp1, Temp2, F, F2plus             : Float;
  I, J                                : Integer;

begin
  DimVector(Delta, Nvar);   { Increments   }
  DimVector(Xminus, Nvar);  { X - Delta    }
  DimVector(Xplus, Nvar);   { X + Delta    }
  DimVector(Fminus, Nvar);  { F(X - Delta) }
  DimVector(Fplus, Nvar);   { F(X + Delta) }

  F := Func(X);

  for I := 1 to Nvar do
    begin
      if X^[I] <> 0.0 then
        Delta^[I] := Eta * Abs(X^[I])
      else
        Delta^[I] := Eta;
      Xplus^[I] := X^[I] + Delta^[I];
      Xminus^[I] := X^[I] - Delta^[I];
    end;

  for I := 1 to Nvar do
    begin
      Temp1 := X^[I];
      X^[I] := Xminus^[I];
      Fminus^[I] := Func(X);
      X^[I] := Xplus^[I];
      Fplus^[I] := Func(X);
      X^[I] := Temp1;
    end;

  for I := 1 to Nvar do
    begin
      G^[I] := (Fplus^[I] - Fminus^[I]) / (2.0 * Delta^[I]);
      H^[I]^[I] := (Fplus^[I] + Fminus^[I] - 2.0 * F) / Sqr(Delta^[I]);
    end;

  for I := 1 to Pred(Nvar) do
    begin
      Temp1 := X^[I];
      X^[I] := Xplus^[I];
      for J := Succ(I) to Nvar do
        begin
          Temp2 := X^[J];
          X^[J] := Xplus^[J];
          F2plus := Func(X);
          H^[I]^[J] := (F2plus - Fplus^[I] - Fplus^[J] + F) / (Delta^[I] * Delta^[J]);
          H^[J]^[I] := H^[I]^[J];
          X^[J] := Temp2;
        end;
      X^[I] := Temp1;
    end;

  DelVector(Delta, Nvar);
  DelVector(Xminus, Nvar);
  DelVector(Xplus, Nvar);
  DelVector(Fminus, Nvar);
  DelVector(Fplus, Nvar);
end;

