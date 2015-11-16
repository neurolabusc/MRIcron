{ ******************************************************************
  This program computes the eigenvalues and eigenvectors of a
  symmetric matrix by the iterative method of Jacobi. The method
  is demonstrated with Hilbert matrices. These matrices are
  ill-conditioned (i.e. the ratio of the lowest to the highest
  eigenvalue is very low).

  The Jacobi method applies a series of rotations to the original
  matrix in order to transform it into a diagonal matrix. The diagonal
  terms of this matrix are the eigenvalues. The product of the rotation
  matrices gives the eigenvectors. The original matrix is destroyed
  during the process.

  The parameter Tol defines the tolerance with which an off-diagonal
  element of the transformed matrix is considered zero (expressed as
  a fraction of the sum of squared diagonal terms). The parameter
  MaxIter defines the maximal number of iterations allowed. These two
  values are linked, i.e. decreasing Tol may need increasing MaxIter
  to avoid non-convergence of the Jacobi procedure.
  ****************************************************************** }

program eigensym;

uses
  tpmath;

const
  MaxIter = 1000;    { Maximum number of iterations }
  Tol     = 1.0E-8;  { Required precision }

var
  N      : Integer;  { Size of matrix }
  A      : PMatrix;  { Matrix }
  Lambda : PVector;  { Eigenvalues }
  V      : PMatrix;  { Eigenvectors }

  procedure Hilbert(A : PMatrix; N : Integer);
{ ------------------------------------------------------------------
  Generates the Hilbert matrix of order N

        ( 1      1/2     1/3     1/4     ... 1/N      )
        ( 1/2    1/3     1/4     1/5     ... 1/(N+1)  )
    A = ( 1/3    1/4     1/5     1/6     ... 1/(N+2)  )
        ( ........................................... )
        ( 1/N    1/(N+1) 1/(N+2) 1/(N+3) ... 1/(2N-1) )

  ------------------------------------------------------------------ }
  var
    I, J : Integer;
  begin
    { First row of matrix }
    A^[1]^[1] := 1.0;
    for J := 2 to N do
      A^[1]^[J] := 1.0 / J;

    for I := 2 to N do
      begin
        { Last column of matrix }
        A^[I]^[N] := 1.0 / (N + I - 1);
        { Fill matrix }
        for J := 1 to N - 1 do
          A^[I]^[J] := A^[I - 1]^[J + 1];
      end;
  end;

  procedure WriteResults(N : Integer; V : PMatrix; Lambda : PVector);
{ ------------------------------------------------------------------
  Outputs results to screen
  ------------------------------------------------------------------ }
  var
    I, J : Integer;
    Ch   : Char;
  begin
    WriteLn;
    WriteLn('Eigenvalues :');
    WriteLn;
    for I := 1 to N do
      WriteLn(Lambda^[I]:26);

    if N < 8 then
      begin
        WriteLn; WriteLn('Eigenvectors (columns) :'); WriteLn;
        for I := 1 to N do
          begin
            for J := 1 to N do
              Write(V^[I]^[J]:10:6);
            WriteLn;
          end;
      end;
  end;

begin
  repeat
    WriteLn;
    Write('Order of Hilbert matrix (1 to end) : ');
    ReadLn(N);

    if N > 1 then
      begin
        { Allocate vectors and matrices }
        DimMatrix(A, N, N);
        DimMatrix(V, N, N);
        DimVector(Lambda, N);

        { Generate Hilbert matrix of order N }
        Hilbert(A, N);

        { Compute eigenvalues and eigenvectors }
        Jacobi(A, 1, N, MaxIter, Tol, Lambda, V);

        case MathErr of
          MatOk      : WriteResults(N, V, Lambda);
          MatNonConv : WriteLn('Too many iterations!');
        end;

        { Deallocate vectors and matrices so that they
          may be redimensioned at the next iteration   }
        DelMatrix(A, N, N);
        DelMatrix(V, N, N);
        DelVector(Lambda, N);
      end;
  until N < 2;
end.
