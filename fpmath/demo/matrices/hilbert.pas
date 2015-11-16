{ ******************************************************************
  This program solves a system of linear equations, of the form
  AX = B, by the method of Gauss-Jordan. The method is demonstrated
  by solving a series of Hilbert systems of increasing order.
  Hilbert systems have ill-conditioned matrices (i.e. with
  determinants close to zero), so that the matrix is considered
  singular for an order which depends on the numerical precision
  of the software.

  The type of real numbers is defined by the compiler directives
  (see UTYPES.PAS for details). The constant MachEp (defined in
  UTYPES.PAS) sets the numerical precision which can be obtained
  with each type. When the determinant falls below this value the
  matrix is considered singular.
  ****************************************************************** }

program hilbert;

uses
  tpmath;

procedure HilbertSystem(N : Integer; var A : PMatrix);
{ ------------------------------------------------------------------
  Generates the Hilbert system of order N

  A[1..N, 1..N] = system matrix :

        ( 1      1/2     1/3     1/4     ... 1/N      )
        ( 1/2    1/3     1/4     1/5     ... 1/(N+1)  )
    A = ( 1/3    1/4     1/5     1/6     ... 1/(N+2)  )
        ( ........................................... )
        ( 1/N    1/(N+1) 1/(N+2) 1/(N+3) ... 1/(2N-1) )

  A[1..N, N+1] = vector of constant terms :

                 N
    A[i, N+1] = Sum A[i,j]
                j=1

  The solution vector is X = [1 1 1 ... 1]
  ------------------------------------------------------------------ }
var
  I, J, M : Integer;
  Sum     : Float;
begin
  { First row of matrix }
  A^[1]^[1] := 1.0;
  for J := 2 to N do
    A^[1]^[J] := 1.0 / J;

  for I := 2 to N do
    begin
      { N-th column of matrix }
      A^[I]^[N] := 1.0 / (N + I - 1);
      { Fill matrix }
      for J := 1 to N - 1 do
        A^[I]^[J] := A^[I - 1]^[J + 1];
    end;

  { Last column = Constant vector }
  M := N + 1;
  for I := 1 to N do
    begin
      Sum := 0.0;
      for J := 1 to N do
        Sum := Sum + A^[I]^[J];
      A^[I]^[M] := Sum;
    end;
end;

procedure WriteHilbertMatrix(N : Integer; A : PMatrix);
var
  I, J, M : Integer;
begin
  WriteLn('System matrix and constant vector :');
  WriteLn;
  M := N + 1;
  for I := 1 to N do
    begin
      for J := 1 to M do
        Write(A^[I]^[J]:10:6);
      WriteLn;
    end;
  WriteLn;
end;

procedure WriteSolution(ErrCode, N : Integer; A : PMatrix; D : Float);
var
  I, M : Integer;
begin
  if ErrCode = MatSing then
    WriteLn('Determinant <', D:10, ' ==> Quasi-Singular Matrix !')
  else
    begin
      WriteLn('Solution vector :');
      WriteLn;
      M := N + 1;
      for I := 1 to N do
        WriteLn(A^[I]^[M]:10:6);
      WriteLn;
      WriteLn('Determinant : ', D:10);
    end;
  WriteLn;
  Write('Press <Enter> ...');
  ReadLn;
end;

var
  N       : Integer;  { Order of the system }
  M       : Integer;  { N + 1 }
  ErrCode : Integer;  { Error code }
  A       : PMatrix;  { System matrix }
  D       : Float;    { Determinant }

begin
  { Initialize }
  N := 1;
  ErrCode := 0;

  { Main loop }
  while ErrCode = 0 do
    begin
      { Set system order }
      Inc(N);
      M := N + 1;

      { Allocate matrix }
      DimMatrix(A, N, M);

      { Generate Hilbert system of order N }
      HilbertSystem(N, A);
      WriteLn; WriteLn('HILBERT SYSTEM OF ORDER ', N); WriteLn;
      if N < 7 then WriteHilbertMatrix(N, A);

      { Solve Hilbert system }
      GaussJordan(A, 1, N, M, D);
      ErrCode := MathErr;

      { Write solution }
      WriteSolution(ErrCode, N, A, D);

      { Deallocate matrix so that it may be
        redimensioned at the next iteration }
      DelMatrix(A, N, M);
    end;
end.
