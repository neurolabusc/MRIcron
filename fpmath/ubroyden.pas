{ ******************************************************************
  Broyden method for system of nonlinear equations
  ****************************************************************** }

unit ubroyden;

interface

uses
  utypes, ulinminq, ucompvec;

procedure Broyden(Equations : TEquations;
                  X, F      : PVector;
                  Lb, Ub    : Integer;
                  MaxIter   : Integer;
                  Tol       : Float);
{ ------------------------------------------------------------------
  Solves a system of nonlinear equations by Broyden's method
  ------------------------------------------------------------------
  Input parameters  : Equations = subroutine to compute equations
                      X         = initial roots
                      Lb, Ub    = bounds of X
                      MaxIter   = maximum number of iterations
                      Tol       = required precision
  ------------------------------------------------------------------
  Output parameters : X = refined roots
                      F = function values
  ------------------------------------------------------------------
  Possible results  : OptOk      = no error
                      OptNonConv = non-convergence
  ------------------------------------------------------------------ }

implementation

procedure Broyden(Equations : TEquations;
                  X, F      : PVector;
                  Lb, Ub    : Integer;
                  MaxIter   : Integer;
                  Tol       : Float);

var
  I, J, K, Iter                   : Integer;
  A, DeltaXmax, Fmax, P, Q, R, S  : Float;
  Conv                            : Boolean;
  OldX, DeltaX, dX, OldF, dF, DdF : PVector;
  Dinv                            : PMatrix;

  procedure Terminate(ErrCode : Integer);
  { Set error code and deallocate arrays }
  begin
    DelVector(OldX, Ub);
    DelVector(DeltaX, Ub);
    DelVector(dX, Ub);
    DelVector(OldF, Ub);
    DelVector(dF, Ub);
    DelVector(DdF, Ub);
    DelMatrix(Dinv, Ub, Ub);
    SetErrCode(ErrCode);
  end;

begin
  { Initialize function vector }
  Equations(X, F);

  { Quit if no iteration required }
  if MaxIter < 1 then
    begin
      SetErrCode(OptOk);
      Exit;
    end;

  { Dimension arrays }
  DimVector(OldX, Ub);
  DimVector(DeltaX, Ub);
  DimVector(dX, Ub);
  DimVector(OldF, Ub);
  DimVector(dF, Ub);
  DimVector(DdF, Ub);
  DimMatrix(Dinv, Ub, Ub);

  { Initialize inverse jacobian to unit matrix }
  for I := Lb to Ub do
    begin
      Dinv^[I]^[I] := 1.0;
      for J := I + 1 to Ub do
        begin
          Dinv^[I]^[J] := 0.0;
          Dinv^[J]^[I] := 0.0
        end;
    end;

  Iter := 0;

  { Compute max. function component }
  Fmax := Abs(F^[Lb]);
  for I := Lb + 1 to Ub do
    begin
      A := Abs(F^[I]);
      if A > Fmax then Fmax := A;
    end;

  { Quit if function vector is already small }
  if Fmax < MachEp then
    begin
      Terminate(OptOk);
      Exit;
    end;

  { Initialize search direction }
  for I := Lb to Ub do
    DeltaX^[I] := - F^[I];

  repeat
    { Prepare next iteration }
    Iter := Iter + 1;
    if Iter > MaxIter then
      begin
        Terminate(OptNonConv);
        Exit;
      end;

    { Normalize search direction to avoid excessive displacements }
    DeltaXmax := Abs(DeltaX^[Lb]);
    for I := Lb + 1 to Ub do
      begin
        A := Abs(DeltaX^[I]);
        if A > DeltaXmax then DeltaXmax := A;
      end;

    if DeltaXmax > 1.0 then
      for I := Lb to Ub do
        DeltaX^[I] := DeltaX^[I] / DeltaXmax;

    { Save old parameters and functions }
    for I := Lb to Ub do
      begin
        OldX^[I] := X^[I];
        OldF^[I] := F^[I];
      end;

    { Minimize along the direction specified by DeltaX,
      with initial step R = 1, and compute new function }
    R := 1.0;
    LinMinEq(Equations, X, DeltaX, F, Lb, Ub, R, 10, 0.01);
    Equations(X, F);

    { Compute differences between two successive
      estimations of parameter vector and function vector }
    for I := Lb to Ub do
      begin
        dX^[I] := X^[I] - OldX^[I];
        dF^[I] := F^[I] - OldF^[I];
      end;

    { Multiply by inverse jacobian }
    for I := Lb to Ub do
      begin
        DdF^[I] := 0.0;
        for J := Lb to Ub do
          DdF^[I] := DdF^[I] + Dinv^[I]^[J] * dF^[J];
      end;

    { Scalar product in denominator of Broyden formula }
    P := 0.0;
    for I := Lb to Ub do
      P := P + dX^[I] * DdF^[I];

    if P = 0.0 then Exit;

    { Inverse of scalar product }
    Q := 1.0 / P;

    { Update inverse jacobian }
    for I := Lb to Ub do
      begin
        A := (dX^[I] - DdF^[I]) * Q;
        for J := Lb to Ub do
          begin
            S := 0.0;
            for K := Lb to Ub do
              S := S + dX^[K] * Dinv^[K]^[J];
            Dinv^[I]^[J] := Dinv^[I]^[J] + A * S;
          end;
      end;

    { Update search direction }
    for I := Lb to Ub do
      begin
        DeltaX^[I] := 0.0;
        for J := Lb to Ub do
          DeltaX^[I] := DeltaX^[I] - Dinv^[I]^[J] * F^[J];
      end;

    { Test for convergence }
    Conv := CompVec(X, OldX, Lb, Ub, Tol);
  until Conv;

  Terminate(OptOk);
end;

end.