{ ******************************************************************
  This program solves a polynomial equation

  Analytical solutions are used up to degree 4, then the polynomial
  is solved by the method of the companion matrix.

  The example polynomial is:

  720 - 1764 * X + 1624 * X^2 - 735 * X^3 +
        175 * X^4 - 21 * X^5 + X^6

  The roots are: X = 1, 2 ... 6
  ****************************************************************** }

program polyroot;

uses
  tpmath;

var
 Coef              : PVector;
 Z                 : PCompVector;
 Deg, I, J, Nc, Nr : Integer;

begin

{ ------------------------------------------------------------------
  Define polynomial here, in the form:
  Coef(0) + Coef(1) * X + Coef(2) * X^2 + ... + Coef(Deg) * X^Deg
  ------------------------------------------------------------------ }

  Deg := 6;

  DimVector(Coef, Deg);
  DimCompVector(Z, Deg);

  Coef^[0] :=   720;
  Coef^[1] := -1764;
  Coef^[2] :=  1624;
  Coef^[3] :=  -735;
  Coef^[4] :=   175;
  Coef^[5] :=   -21;
  Coef^[6] :=     1;

{ ------------------------------------------------------------------ }

  Writeln;
  Writeln('Polynomial:');
  Writeln;

  for I := 0 to Deg do
    if Coef^[I] <> 0 then
      begin
        if Coef^[I] > 0 then Write(' + ');
        if Coef^[I] < 0 then Write(' - ');
        Write(Abs(Coef^[I]):12:6, ' ');
        if I > 0 then Write('X');
        if I > 1 then Write('^', I);
        Writeln;
      end;

  Writeln;
  Writeln;

  { Solve polynomial. Nr is the number of real roots }
  case Deg of
    1 : Nr := RootPol1(Coef^[0], Coef^[1], Z^[1].X);
    2 : Nr := RootPol2(Coef, Z);
    3 : Nr := RootPol3(Coef, Z);
    4 : Nr := RootPol4(Coef, Z);
  otherwise
        Nr := RootPol(Coef, Deg, Z);
  end;

  { Case when an error occurs }
  if Nr < 0 then
    begin
      Writeln('Error during root evaluation !');
      Halt;
    end;

  { Set the small imaginary parts to zero (optional) }
  Nr := SetRealRoots(Deg, Z, 1.0E-8);

  { Sort roots: first real roots, in ascending order,
    then complex roots (unordered)                    }
  SortRoots(Deg, Z);

  { Print real roots }
  if Nr > 0 then
    begin
      Writeln(Nr, ' real root(s):');
      Writeln;

      for I := 1 to Nr do
        Writeln('X[', I, '] = ', Z^[I].X:12:6);

      Writeln;
    end;

  { Print complex roots }
  Nc := Deg - Nr;
  if Nc > 0 then
    begin
      Writeln(Nc, ' complex roots:');
      Writeln;

      for I := 1 to Nc do
        begin
          J := I + Nr;
          Write('X[', J, '] = ', Z^[J].X:12:6);
          if Z^[J].Y > 0.0 then Write(' + ') else Write(' - ');
          Writeln(Abs(Z^[J].Y):12:6, ' * i');
        end;
    end;
end.
