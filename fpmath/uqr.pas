{ ******************************************************************
  QR decomposition

  Ref.: 'Matrix Computations' by Golub & Van Loan
         Pascal implementation contributed by Mark Vaughan
  ****************************************************************** }

unit uqr;

interface

uses
  utypes;

procedure QR_Decomp(A            : PMatrix;
                    Lb, Ub1, Ub2 : Integer;
                    R            : PMatrix);
{ ------------------------------------------------------------------
  QR decomposition. Factors the matrix A (n x m, with n >= m) as a
  product Q * R where Q is a (n x m) column-orthogonal matrix, and R
  a (m x m) upper triangular matrix. This routine is used in
  conjunction with QR_Solve to solve a system of equations.
  ------------------------------------------------------------------
  Input parameters : A   = matrix
                     Lb  = index of first matrix element
                     Ub1 = index of last matrix element in 1st dim.
                     Ub2 = index of last matrix element in 2nd dim.
  ------------------------------------------------------------------
  Output parameter : A   = contains the elements of Q
                     R   = upper triangular matrix
  ------------------------------------------------------------------
  Possible results : MatOk
                     MatErrDim
                     MatSing
  ------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ------------------------------------------------------------------ }

procedure QR_Solve(Q, R         : PMatrix;
                   B            : PVector;
                   Lb, Ub1, Ub2 : Integer;
                   X            : PVector);
{ ------------------------------------------------------------------
  Solves a system of equations by the QR decomposition,
  after the matrix has been transformed by QR_Decomp.
  ------------------------------------------------------------------
  Input parameters : Q, R         = matrices from QR_Decomp
                     B            = constant vector
                     Lb, Ub1, Ub2 = as in QR_Decomp
  ------------------------------------------------------------------
  Output parameter : X            = solution vector
  ------------------------------------------------------------------ }

implementation

procedure QR_Decomp(A            : PMatrix;
                    Lb, Ub1, Ub2 : Integer;
                    R            : PMatrix);
  var
    I, J, K : Integer;
    Sum     : Float;
  begin
    if Ub2 > Ub1 then
      begin
        SetErrCode(MatErrDim);
        Exit
      end;

    for K := Lb to Ub2 do
      begin
        { Compute the "k"th diagonal entry in R }
        Sum := 0.0;
        for I := Lb to Ub1 do
          Sum := Sum + Sqr(A^[I]^[K]);

        if Sum = 0.0 then
          begin
            SetErrCode(MatSing);
            Exit;
          end;

        R^[K]^[K] := Sqrt(Sum);

        { Divide the entries in the "k"th column of A by the computed "k"th }
        { diagonal element of R.  this begins the process of overwriting A  }
        { with Q . . .                                                      }
        for I := Lb to Ub1 do
          A^[I]^[K] := A^[I]^[K] / R^[K]^[K];

        for J := (K + 1) to Ub2 do
          begin
            { Complete the remainder of the row entries in R }
            Sum := 0.0;
            for I := Lb to Ub1 do
              Sum := Sum + A^[I]^[K] * A^[I]^[J];
            R^[K]^[J] := Sum;

            { Update the column entries of the Q/A matrix }
            for I := Lb to Ub1 do
              A^[I]^[J] := A^[I]^[J] - A^[I]^[K] * R^[K]^[J];
          end;
      end;

    SetErrCode(MatOk);
  end;

procedure QR_Solve(Q, R         : PMatrix;
                   B            : PVector;
                   Lb, Ub1, Ub2 : Integer;
                   X            : PVector);
  var
    I, J : Integer;
    Sum  : Float;
  begin
    { Form Q'B and store the result in X }
    for J := Lb to Ub2 do
      begin
        X^[J] := 0.0;
        for I := Lb to Ub1 do
          X^[J] := X^[J] + Q^[I]^[J] * B^[I];
      end;

    { Update X with the solution vector }
    X^[Ub2] := X^[Ub2] / R^[Ub2]^[Ub2];
    for I := (Ub2 - 1) downto Lb do
      begin
        Sum := 0.0;
        for J := (I + 1) to Ub2 do
          Sum := Sum + R^[I]^[J] * X^[J];
        X^[I] := (X^[I] - Sum) / R^[I]^[I];
      end;
  end;

end.