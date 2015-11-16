{ ******************************************************************
  Minimization of a function of several variables along a line
  ****************************************************************** }

unit ulinmin;

interface

uses
  utypes, uminmax;

procedure LinMin(Func      : TFuncNVar;
                 X, DeltaX : PVector;
                 Lb, Ub    : Integer;
                 var R     : Float;
                 MaxIter   : Integer;
                 Tol       : Float;
                 var F_min : Float);
{ ------------------------------------------------------------------
  Minimizes function Func from point X in the direction specified by
  DeltaX
  ------------------------------------------------------------------
  Input parameters  : Func    = objective function
                      X       = initial minimum coordinates
                      DeltaX  = direction in which minimum is searched
                      Lb, Ub  = indices of first and last variables
                      R       = initial step, in fraction of |DeltaX|
                      MaxIter = maximum number of iterations
                      Tol     = required precision
  ------------------------------------------------------------------
  Output parameters : X       = refined minimum coordinates
                      R       = step corresponding to the minimum
                      F_min   = function value at minimum
  ------------------------------------------------------------------
  Possible results  : OptOk
                      OptNonConv
  ------------------------------------------------------------------ }

implementation

procedure LinMin(Func      : TFuncNVar;
                 X, DeltaX : PVector;
                 Lb, Ub    : Integer;
                 var R     : Float;
                 MaxIter   : Integer;
                 Tol       : Float;
                 var F_min : Float);

var
  A, B, C            : Float;
  Fa, Fb, Fc, F1, F2 : Float;
  MinTol, Norm       : Float;
  R0, R1, R2, R3     : Float;
  I, Iter            : Integer;
  P                  : PVector;

begin
  MinTol := Sqrt(MachEp);
  if Tol < MinTol then Tol := MinTol;

  if R < 0.0 then R := 1.0;

  Norm := 0.0;
  for I := Lb to Ub do
    Norm := Norm + Sqr(DeltaX^[I]);
  Norm := Sqrt(Norm);

  A := 0; B := R * Norm;

  DimVector(P, Ub);

  { Bracket the minimum (see procedure MinBrack in unit UMINBRAK) }

  for I := Lb to Ub do
    P^[I] := X^[I];

  Fa := Func(P);

  for I := Lb to Ub do
    P^[I] := X^[I] + B * DeltaX^[I];

  Fb := Func(P);

  if Fb > Fa then
    begin
      FSwap(A, B);
      FSwap(Fa, Fb);
    end;

  C := B + Gold * (B - A);

  for I := Lb to Ub do
    P^[I] := X^[I] + C * DeltaX^[I];

  Fc := Func(P);

  while Fc < Fb do
  begin
    A := B;
    B := C;
    Fa := Fb;
    Fb := Fc;
    C := B + Gold * (B - A);

    for I := Lb to Ub do
      P^[I] := X^[I] + C * DeltaX^[I];

    Fc := Func(P);
  end;

  if A > C then
    begin
      FSwap(A, C);
      FSwap(Fa, Fc);
    end;

  { Refine the minimum (see procedure GoldSearch in unit UGOLDSRC) }

  R0 := A; R3 := C;

  if (C - B) > (B - A) then
  begin
    R1 := B;
    R2 := B + CGold * (C - B);
    F1 := Fb;

    for I := Lb to Ub do
      P^[I] := X^[I] + R2 * DeltaX^[I];

    F2 := Func(P);
  end
  else
  begin
    R1 := B - CGold * (B - A);
    R2 := B;

    for I := Lb to Ub do
      P^[I] := X^[I] + R1 * DeltaX^[I];

    F1 := Func(P);
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

      F2 := Func(P);
    end
    else
    begin
      R3 := R2;
      R2 := R1;
      F2 := F1;
      R1 := R2 - CGold * (R2 - R0);

      for I := Lb to Ub do
        P^[I] := X^[I] + R1 * DeltaX^[I];

      F1 := Func(P);
    end;

    Iter := Iter + 1;
  end;

  if F1 < F2 then
  begin
    R := R1;
    F_min := F1;
  end
  else
  begin
    R := R2;
    F_min := F2;
  end;

  for I := Lb to Ub do
    X^[I] := X^[I] + R * DeltaX^[I];

  if Iter > MaxIter then
    SetErrCode(OptNonConv)
  else
    SetErrCode(OptOk);

  DelVector(P, Ub);
end;

end.
