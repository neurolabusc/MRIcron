{ ******************************************************************
  Minimization of a sum of squared functions along a line
  (Used internally by equation solvers)
  ****************************************************************** }

unit ulinminq;

interface

uses
  utypes;

procedure LinMinEq(Equations    : TEquations;
                   X, DeltaX, F : PVector;
                   Lb, Ub       : Integer;
                   R            : Float;
                   MaxIter      : Integer;
                   Tol          : Float);
{ ------------------------------------------------------------------
  Minimizes a sum of squared functions from point X in the direction
  specified by DeltaX, using golden search as the minimization algo.
  ------------------------------------------------------------------
  Input parameters : SysFunc = system of functions
                     X       = starting point
                     DeltaX  = search direction
                     Lb, Ub  = bounds of X
                     R       = initial step, in fraction of |DeltaX|
                     MaxIter = maximum number of iterations
                     Tol     = required precision
  ------------------------------------------------------------------
  Output parameters: X = refined minimum coordinates
                     F = function values at minimum
                     R = step corresponding to the minimum
  ------------------------------------------------------------------
  Possible results : OptOk      = no error
                     OptNonConv = non-convergence
  ------------------------------------------------------------------ }

implementation

procedure LinMinEq(Equations    : TEquations;
                   X, DeltaX, F : PVector;
                   Lb, Ub       : Integer;
                   R            : Float;
                   MaxIter      : Integer;
                   Tol          : Float);

var
  I, Iter                : Integer;
  A, B, C, Fa, Fb, Fc    : Float;
  R0, R1, R2, R3, F1, F2 : Float;
  MinTol, Norm           : Float;
  P                      : PVector;

  procedure Swap2(var A, B, Fa, Fb : Float);
  { Exchanges A <--> B, Fa <--> Fb }
  var
    Temp : Float;
  begin
    Temp := A;
    A := B;
    B := Temp;
    Temp := Fa;
    Fa := Fb;
    Fb := Temp;
  end;

  function SumSqrFn : Float;
  { Computes the sum of squared functions F(i)^2 at point P }
  var
    Sum : Float;
    I   : Integer;
  begin
    Equations(P, F);

    Sum := 0.0;
    for I := Lb to Ub do
      Sum := Sum + Sqr(F^[I]);

    SumSqrFn := Sum;
  end;

begin
  DimVector(P, Ub);

  MinTol := Sqrt(MachEp);
  if Tol < MinTol then Tol := MinTol;
  if R <= 0.0 then R := 1.0;

  Norm := 0.0;
  for I := Lb to Ub do
    Norm := Norm + Sqr(DeltaX^[I]);
  Norm := Sqrt(Norm);

  { Bracket the minimum }

  A := 0.0; B := R * Norm;

  for I := Lb to Ub do
    P^[I] := X^[I];

  Fa := SumSqrFn;

  for I := Lb to Ub do
    P^[I] := X^[I] + B * DeltaX^[I];

  Fb := SumSqrFn;

  if Fb > Fa then Swap2(A, B, Fa, Fb);

  C := B + Gold * (B - A);

  for I := Lb to Ub do
    P^[I] := X^[I] + C * DeltaX^[I];

  Fc := SumSqrFn;

  while Fc < Fb do
    begin
      A := B;
      B := C;
      Fa := Fb;
      Fb := Fc;
      C := B + Gold * (B - A);

      for I := Lb to Ub do
        P^[I] := X^[I] + C * DeltaX^[I];

      Fc := SumSqrFn;
    end;

  if A > C then Swap2(A, C, Fa, Fc);

  { Refine the minimum }

  R0 := A; R3 := C;

  if (C - B) > (B - A) then
    begin
      R1 := B;
      R2 := B + CGold * (C - B);
      F1 := Fb;

      for I := Lb to Ub do
        P^[I] := X^[I] + R2 * DeltaX^[I];

      F2 := SumSqrFn;
    end
  else
    begin
      R1 := B - CGold * (B - A);
      R2 := B;

      for I := Lb to Ub do
        P^[I] := X^[I] + R1 * DeltaX^[I];

      F1 := SumSqrFn;
      F2 := Fb;
    end;

  Iter := 0;

  while (Iter <= MaxIter) and (Abs(R3 - R0) > Tol * (Abs(R1) + Abs(R2))) do
    begin
      if F2 < F1 then
        begin
          R0 := R1;
          R1 := R2;
          F1 := F2;
          R2 := R1 + CGold * (R3 - R1);

          for I := Lb to Ub do
            P^[I] := X^[I] + R2 * DeltaX^[I];

          F2 := SumSqrFn;
        end
      else
        begin
          R3 := R2;
          R2 := R1;
          F2 := F1;
          R1 := R2 - CGold * (R2 - R0);

          for I := Lb to Ub do
            P^[I] := X^[I] + R1 * DeltaX^[I];

          F1 := SumSqrFn
        end;

      Iter := Iter + 1;
    end;

  if F1 < F2 then R := R1 else R := R2;

  for I := Lb to Ub do
    X^[I] := X^[I] + R * DeltaX^[I];

  Equations(X, F);

  if Iter > MaxIter then
    SetErrCode(OptNonConv)
  else
    SetErrCode(OptOk);

  DelVector(P, Ub);
end;

end.

