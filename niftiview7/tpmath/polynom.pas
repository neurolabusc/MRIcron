{ **********************************************************************
  *                          Unit POLYNOM.PAS                          *
  *                            Version 1.3                             *
  *                    (c) J. Debord, January 1998                     *
  **********************************************************************
  This unit implements routines for polynomials and rational fractions.
  **********************************************************************
  Reference: 'Numerical Recipes' by Press et al.
  ********************************************************************** }

unit Polynom;

interface

uses
  FMath, Matrices, Eigen, Stat;

function Poly(X : Float; Coef : PVector; Deg : Integer) : Float;
{ ----------------------------------------------------------------------
  Evaluates the polynomial :
  P(X) = Coef[0] + Coef[1] * X + Coef[2] * X^2 +...+ Coef[Deg] * X^Deg
  ---------------------------------------------------------------------- }

function RRootPol(Coef : PVector; Deg : Integer; X : PVector) : Integer;
{ ----------------------------------------------------------------------
  Real roots of a polynomial. The roots are computed analytically if
  Deg <= 3, otherwise they are computed numerically from the eigenvalues
  of the companion matrix (function RootPol in EIGEN.PAS). The roots are
  returned in X (in increasing order). The function returns the number
  of real roots found.
  ---------------------------------------------------------------------- }

function CRootPol(Coef : PVector; Deg : Integer;
                  X_Re, X_Im : PVector) : Integer;
{ ----------------------------------------------------------------------
  Complex roots of a polynomial. The roots are computed numerically
  from the eigenvalues of the companion matrix (function RootPol in
  EIGEN.PAS). The real and imaginary parts of the roots are returned
  in X_Re and X_Im (in increasing order of the real parts). The function
  returns the number of roots found, which may be Deg or zero if the
  method did not converge.
  ---------------------------------------------------------------------- }

function RFrac(X : Float; Coef : PVector; Deg1, Deg2 : Integer) : Float;
{ ----------------------------------------------------------------------
  Evaluates the rational fraction :

           Coef[0] + Coef[1] * X + ... + Coef[Deg1] * X^Deg1
  F(X) = -----------------------------------------------------
         1 + Coef[Deg1+1] * X + ... + Coef[Deg1+Deg2] * X^Deg2
  ---------------------------------------------------------------------- }

implementation

const
  MAXDEG = 3;  { Maximal degree for analytical solution of polynomial }

  function Poly(X : Float; Coef : PVector; Deg : Integer) : Float;
  var
    I : Integer;
    Y : Float;
  begin
    Y := Coef^[Deg];
    for I := Pred(Deg) downto 0 do
      Y := Y * X + Coef^[I];
    Poly := Y;
  end;

  function RFrac(X : Float; Coef : PVector; Deg1, Deg2 : Integer) : Float;
  var
    I : Integer;
    Sum : Float;  { Denominator sum }
  begin
    Sum := 0.0;
    for I := (Deg1 + Deg2) downto Succ(Deg1) do
      Sum := (Sum + Coef^[I]) * X;
    RFrac := Poly(X, Coef, Deg1) / (1.0 + Sum);
  end;

  function RootPol3(Coef : PVector; Deg : Integer; X : PVector) : Integer;
  { Real roots of polynomial up to degree 3 (Analytical solution) }
  const
    PI2DIV3 = 2.0943951023931954923;  { 2*pi/3 }
  var
    NR : Integer;  { Number of roots }
    R, R2, Q, Q3, Delta, A0, A1, A2, A22, A3, AA, BB, Theta, Z : Float;
  begin
    if (Deg < 1) or (Deg > MAXDEG) then
      begin
        RootPol3 := 0;
        Exit;
      end;
    case Deg of
      1 : begin
            NR := 1;
            X^[1] := - Coef^[0] / Coef^[1];
          end;
      2 : begin
            Delta := Sqr(Coef^[1]) - 4.0 * Coef^[0] * Coef^[2];
            if Delta < 0 then
              NR := 0
            else
              begin
                NR := 2;
                if Coef^[1] >= 0 then
                  Q := - 0.5 * (Coef^[1] + Sqrt(Delta))
                else
                  Q := - 0.5 * (Coef^[1] - Sqrt(Delta));
                X^[1] := Q / Coef^[2];
                X^[2] := Coef^[0] / Q;
              end;
          end;
      3 : begin
            A0 := Coef^[0] / Coef^[3];
            A1 := Coef^[1] / Coef^[3];
            A2 := Coef^[2] / Coef^[3];
            A3 := A2 / 3.0;
            A22 := Sqr(A2);
            Q := (A22 - 3.0 * A1) / 9.0;
            R := (A2 * (2.0 * A22 - 9.0 * A1) + 27.0 * A0) / 54.0;
            R2 := R * R;
            Q3 := Q * Q * Q;
            Delta := Q3 - R2;
            if Delta < 0 then
              begin
                NR := 1;
                AA := Power(Abs(R) + Sqrt(- Delta), 0.333333333333333);
                if R >= 0 then AA := - AA;
                if AA <> 0 then BB := Q / AA else BB := 0.0;
                X^[1] := (AA + BB) - A3;
              end
            else
              begin
                NR := 3;
                Theta := ArcCos(R / Sqrt(Q3)) / 3.0;
                Z := - 2.0 * Sqrt(Q);
                X^[1] := Z * Cos(Theta) - A3;
                X^[2] := Z * Cos(Theta + PI2DIV3) - A3;
                X^[3] := Z * Cos(Theta - PI2DIV3) - A3;
              end;
          end;
    end;
    QSort(X, 1, Deg);
    RootPol3 := NR;
  end;

  function RRootPol(Coef : PVector; Deg : Integer; X : PVector) : Integer;
  var
    N : Integer;           { Number of real roots }
    X_Re, X_Im : PVector;  { Real and imaginary parts }
    ErrCode : Integer;     { Error code }
    I : Integer;           { Loop variable }
  begin
    DimVector(X_Re, Deg);
    DimVector(X_Im, Deg);

    if Deg <= MAXDEG then
      RRootPol := RootPol3(Coef, Deg, X)
    else
      begin
        ErrCode := RootPol(Coef, Deg, X_Re, X_Im);
        if ErrCode = MAT_OK then
          begin
            { Get real roots }
            N := 0;
            for I := 1 to Deg do
              if Abs(X_Im^[I]) <= MACHEP then
                begin
                  Inc(N);
                  X^[N] := X_Re^[I];
                end;
            { Set other roots to zero }
            for I := Succ(N) to Deg do
              X^[I] := 0.0;
            RRootPol := N;
          end
        else
          RRootPol := 0;
      end;

    DelVector(X_Re, Deg);
    DelVector(X_Im, Deg);
  end;

  function CRootPol(Coef : PVector; Deg : Integer;
                    X_Re, X_Im : PVector) : Integer;
  begin
    if RootPol(Coef, Deg, X_Re, X_Im) = MAT_OK then
      CRootPol := Deg
    else
      CRootPol := 0;
  end;

end.
