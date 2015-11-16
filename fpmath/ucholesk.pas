{ ******************************************************************
  Cholesky factorization of a positive definite symmetric matrix
  ****************************************************************** }

unit ucholesk;

interface

uses
  utypes;

procedure Cholesky(A, L : PMatrix; Lb, Ub : Integer);
{ ------------------------------------------------------------------
  Cholesky decomposition. Factors the symmetric positive definite
  matrix A as a product L * L' where L is a lower triangular matrix.
  This procedure may be used as a test of positive definiteness.
  ------------------------------------------------------------------
  Possible results : MatOk    : No error
                     MatNotPD : Matrix not positive definite
  ------------------------------------------------------------------ }

implementation

procedure Cholesky(A, L : PMatrix; Lb, Ub : Integer);
var
  I, J, K : Integer;
  Sum     : Float;
begin
  for K := Lb to Ub do
    begin
      Sum := A^[K]^[K];
      for J := Lb to K - 1 do
        Sum := Sum - Sqr(L^[K]^[J]);

      if Sum <= 0.0 then
        begin
          SetErrCode(MatNotPD);
          Exit
        end;

      L^[K]^[K] := Sqrt(Sum);
      for I := K + 1 to Ub do
        begin
          Sum := A^[I]^[K];
          for J := Lb to K - 1 do
            Sum := Sum - L^[I]^[J] * L^[K]^[J];
          L^[I]^[K] := Sum / L^[K]^[K];
          L^[K]^[I] := 0.0;
        end;
    end;

  SetErrCode(MatOk);
end;

end.
