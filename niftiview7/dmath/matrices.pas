{ **********************************************************************
  *                          Unit MATRICES.PAS                         *
  *                             Version 2.0                            *
  *                       (c) J. Debord, May 2001                      *
  **********************************************************************
  This unit implements dynamic allocation of vectors and matrices in
  Pascal, together with various matrix operations.

  Dynamic allocation is allowed by declaring arrays as pointers. There
  are 8 types available :

  PVector,     PMatrix      for floating point arrays
  PIntVector,  PIntMatrix   for integer arrays
  PBoolVector, PBoolMatrix  for boolean arrays
  PStrVector,  PStrMatrix   for string arrays (255 char.)

  To use these arrays in your programs, you must :

  (1) Declare variables of the appropriate type, e.g.

         var
           V : PVector;
           A : PMatrix;

  (2) Allocate each array BEFORE using it :

         DimVector(V, N);         creates vector V[0..N]
         DimMatrix(A, N, M);      creates matrix A[0..N, 0..M]
                                  where N, M are two integer variables

         If the allocation succeeds, all array elements are initialized
         to zero (for numeric arrays), False (for boolean arrays), or
         the null string (for string arrays). Otherwise, the pointer is
         initialized to NIL.

  (3) Use arrays as in standard Turbo Pascal, with the following
      exceptions :

      (a) You must use the indirection operator (^) to reference any
          array element, i.e. write V^[I] and A^[I]^[J] instead of
          V[I] and A[I,J].

      (b) You cannot use the assignment operator (:=) to copy the
          contents of an array into another array. Writing B := A
          simply makes B point to the same memory block than A. You
          must use one of the provided Copy... procedures (see their
          documentation in the interface part of the unit).

      In addition, note that :

      (a) All arrays begin at index 0, so that the 0-indexed element
          is always present, even if you don't use it.

      (b) A matrix is declared as an array of vectors, so that A^[I]
          denotes the I-th vector of matrix A and may be used as any
          vector.

  (4) Deallocate arrays when you no longer need them. This will free
      the corresponding memory :

         DelVector(V, N);
         DelMatrix(A, N, M);

  For more information, read the comments of each routine in the
  interface part of the unit, and check the demo programs.
  **********************************************************************
  References :
  1) 'Basic Programs for Scientists and Engineers' by A.R. Miller :
     GaussJordan, InvMat
  2) Borland's Numerical Methods Toolbox : Det
  3) 'Numerical Recipes' by Press et al. : Cholesky, LU, SVD
  4) 'Matrix Computations' by Golub & Van Loan : QR_Decomp & QR_Solve
     (Pascal implementation contributed by Mark Vaughan)
  ********************************************************************** }

unit Matrices;

interface

uses
  FMath,dialogs,sysutils;

{ **********************************************************************
  This section defines some error codes.
  ********************************************************************** }

const
  MAT_OK       =   0;  { No error }
  MAT_SINGUL   = - 1;  { Singular matrix }
  MAT_NON_CONV = - 2;  { Non convergence of iterative procedure }
  MAT_NOT_PD   = - 3;  { Matrix not positive definite }

{ **********************************************************************
  This section defines the vector and matrix types. Maximal sizes are
  given for a 16-bit compiler (TP / BP / Delphi 1). Higher values may
  be used with the 32-bit compilers (Delphi 2-4, FPK, GPC).
  ********************************************************************** }

const
{$IFDEF EXTENDEDREAL}
  MAX_FLT = 6552;      { Max size of real vector }
{$ELSE}
{$IFDEF SINGLEREAL}
  MAX_FLT = 16382;
{$ELSE}
{$IFDEF PASCALREAL}
  MAX_FLT = 10921;
{$ELSE}
  {$DEFINE DOUBLEREAL}
  MAX_FLT = 8190;
{$ENDIF}
{$ENDIF}
{$ENDIF}

  MAX_INT  = 16382;  { Max size of integer vector }
  MAX_BOOL = 32766;  { Max size of boolean vector }
  MAX_STR  = 254;    { Max size of string vector }
  MAX_VEC  = 16382;  { Max number of vectors in a matrix }

type
  Str255= string[255];
  TVector     = array[0..MAX_FLT] of Float;
  TIntVector  = array[0..MAX_INT] of Integer;
  TBoolVector = array[0..MAX_BOOL] of Boolean;
  TStrVector  = array[0..MAX_STR] of Str255;

  PVector     = ^TVector;
  PIntVector  = ^TIntVector;
  PBoolVector = ^TBoolVector;
  PStrVector  = ^TStrVector;

  TMatrix     = array[0..MAX_VEC] of PVector;
  TIntMatrix  = array[0..MAX_VEC] of PIntVector;
  TBoolMatrix = array[0..MAX_VEC] of PBoolVector;
  TStrMatrix  = array[0..MAX_VEC] of PStrVector;

  PMatrix     = ^TMatrix;
  PIntMatrix  = ^TIntMatrix;
  PBoolMatrix = ^TBoolMatrix;
  PStrMatrix  = ^TStrMatrix;

{ **********************************************************************
  Memory allocation routines
  ********************************************************************** }

procedure DimVector(var V : PVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Creates floating point vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimIntVector(var V : PIntVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Creates integer vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimBoolVector(var V : PBoolVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Creates boolean vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimStrVector(var V : PStrVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Creates string vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimMatrix(var A : PMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Creates floating point matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DimIntMatrix(var A : PIntMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Creates integer matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DimBoolMatrix(var A : PBoolMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Creates boolean matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DimStrMatrix(var A : PStrMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Creates string matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Memory deallocation routines
  ********************************************************************** }

procedure DelVector(var V : PVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Deletes floating point vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DelIntVector(var V : PIntVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Deletes integer vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DelBoolVector(var V : PBoolVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Deletes boolean vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DelStrVector(var V : PStrVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Deletes string vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DelMatrix(var A : PMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Deletes floating point matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DelIntMatrix(var A : PIntMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Deletes integer matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DelBoolMatrix(var A : PBoolMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Deletes boolean matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DelStrMatrix(var A : PStrMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Deletes string matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Routines for copying vectors and matrices
  ----------------------------------------------------------------------
  Lbound, Ubound   : indices of first and last vector elements
  Lbound1, Lbound2 : indices of first matrix element in each dimension
  Ubound1, Ubound2 : indices of last matrix element in each dimension
  ********************************************************************** }

procedure SwapRows(I, K : Integer; A : PMatrix; Lbound, Ubound : Integer);
{ ----------------------------------------------------------------------
  Exchanges rows I and K of matrix A
  ---------------------------------------------------------------------- }

procedure SwapCols(J, K : Integer; A : PMatrix; Lbound, Ubound : Integer);
{ ----------------------------------------------------------------------
  Exchanges columns J and K of matrix A
  ---------------------------------------------------------------------- }

procedure CopyVector(Dest, Source : PVector; Lbound, Ubound : Integer);
{ ----------------------------------------------------------------------
  Copies vector Source into vector Dest
  ---------------------------------------------------------------------- }

procedure CopyMatrix(Dest, Source : PMatrix;
                     Lbound1, Lbound2, Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Copies matrix Source into matrix Dest
  ---------------------------------------------------------------------- }

procedure CopyRowFromVector(Dest : PMatrix; Source : PVector;
                            Lbound, Ubound, Row : Integer);
{ ----------------------------------------------------------------------
  Copies vector Source into line Row of matrix Dest
  ---------------------------------------------------------------------- }

procedure CopyColFromVector(Dest : PMatrix; Source : PVector;
                            Lbound, Ubound, Col : Integer);
{ ----------------------------------------------------------------------
  Copies vector Source into column Col of matrix Dest
  ---------------------------------------------------------------------- }

procedure CopyVectorFromRow(Dest : PVector; Source : PMatrix;
                            Lbound, Ubound, Row : Integer);
{ ----------------------------------------------------------------------
  Copies line Row of matrix Source into vector Dest
  ---------------------------------------------------------------------- }

procedure CopyVectorFromCol(Dest : PVector; Source : PMatrix;
                            Lbound, Ubound, Col : Integer);
{ ----------------------------------------------------------------------
  Copies column Col of matrix Source into vector Dest
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Vector and matrix functions
  ********************************************************************** }

function Min(X : PVector; Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Returns the lowest value of vector X
  ---------------------------------------------------------------------- }

function Max(X : PVector; Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Returns the highest value of vector X
  ---------------------------------------------------------------------- }

function IntMin(X : PIntVector; Lbound, Ubound : Integer) : Integer;
{ ----------------------------------------------------------------------
  Returns the lowest value of integer vector X
  ---------------------------------------------------------------------- }

function IntMax(X : PIntVector; Lbound, Ubound : Integer) : Integer;
{ ----------------------------------------------------------------------
  Returns the highest value of integer vector X
  ---------------------------------------------------------------------- }

procedure Transpose(A : PMatrix;
                    Lbound1, Lbound2, Ubound1, Ubound2 : Integer;
                    A_t : PMatrix);
{ ----------------------------------------------------------------------
  Transposes a matrix
  ----------------------------------------------------------------------
  Input parameters : A       = original matrix
                     Lbound1,
                     Lbound2 = indices of 1st matrix elem. in each dim.
                     Ubound1,
                     Ubound2 = indices of last matrix elem. in each dim.
  ----------------------------------------------------------------------
  Output parameter : A_t     = transposed matrix
  ---------------------------------------------------------------------- }

function GaussJordan(A : PMatrix; B : PVector; Lbound, Ubound : Integer;
                     A_inv : PMatrix; X : PVector) : Integer;
{ ----------------------------------------------------------------------
  Solves a system of linear equations by the Gauss-Jordan method
  ----------------------------------------------------------------------
  Input parameters  : A      = system matrix
                      B      = constant vector
                      Lbound = index of first matrix element
                      Ubound = index of last matrix element
  ----------------------------------------------------------------------
  Output parameters : A_inv  = inverse matrix
                      X      = solution vector
  ----------------------------------------------------------------------
  Possible results  : MAT_OK
                      MAT_SINGUL
  ---------------------------------------------------------------------- }

function InvMat(A : PMatrix; Lbound, Ubound : Integer;
                A_inv : PMatrix) : Integer;
{ ----------------------------------------------------------------------
  Computes the inverse of a square matrix by the Gauss-Jordan method
  ----------------------------------------------------------------------
  Parameters : as in Gauss-Jordan
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_SINGUL
  ---------------------------------------------------------------------- }

function Det(A : PMatrix; Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Computes the determinant of a square matrix
  ----------------------------------------------------------------------
  Parameters : as in Gauss-Jordan
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

function Cholesky(A : PMatrix; Lbound, Ubound : Integer;
                  L : PMatrix) : Integer;
{ ----------------------------------------------------------------------
  Cholesky decomposition. Factors the symmetric positive definite matrix
  A as a product L * L', where L is a lower triangular matrix. This
  procedure may be used as a test of positive definiteness.
  ----------------------------------------------------------------------
  Input parameters : A      = matrix
                     Lbound = index of first matrix element
                     Ubound = index of last matrix element
  ----------------------------------------------------------------------
  Output parameter : L      = Cholesky factor of matrix A
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_NOT_PD
  ---------------------------------------------------------------------- }

function LU_Decomp(A : PMatrix; Lbound, Ubound : Integer) : Integer;
{ ----------------------------------------------------------------------
  LU decomposition. Factors the square matrix A as a product L * U,
  where L is a lower triangular matrix (with unit diagonal terms) and U
  is an upper triangular matrix. This routine is used in conjunction
  with LU_Solve to solve a system of equations.
  ----------------------------------------------------------------------
  Input parameters : A      = matrix
                     Lbound = index of first matrix element
                     Ubound = index of last matrix element
  ----------------------------------------------------------------------
  Output parameter : A      = contains the elements of L and U
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_SINGUL
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

procedure LU_Solve(A : PMatrix; B : PVector; Lbound, Ubound : Integer;
                   X : PVector);
{ ----------------------------------------------------------------------
  Solves a system of equations whose matrix has been transformed by
  LU_Decomp
  ----------------------------------------------------------------------
  Input parameters : A      = result from LU_Decomp
                     B      = constant vector
                     Lbound,
                     Ubound = as in LU_Decomp
  ----------------------------------------------------------------------
  Output parameter : X      = solution vector
  ---------------------------------------------------------------------- }

function SV_Decomp(A : PMatrix; Lbound, Ubound1, Ubound2 : Integer;
                   S : PVector; V : PMatrix) : Integer;
{ ----------------------------------------------------------------------
  Singular value decomposition. Factors the matrix A (n x m, with n >= m)
  as a product U * S * V' where U is a (n x m) column-orthogonal matrix,
  S a (m x m) diagonal matrix with elements >= 0 (the singular values)
  and V a (m x m) orthogonal matrix. This routine is used in conjunction
  with SV_Solve to solve a system of equations.
  ----------------------------------------------------------------------
  Input parameters : A       = matrix
                     Lbound  = index of first matrix element
                     Ubound1 = index of last matrix element in 1st dim.
                     Ubound2 = index of last matrix element in 2nd dim.
  ----------------------------------------------------------------------
  Output parameter : A       = contains the elements of U
                     S       = vector of singular values
                     V       = orthogonal matrix
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_NON_CONV
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

procedure SV_SetZero(S : PVector; Lbound, Ubound : Integer; Tol : Float);
{ ----------------------------------------------------------------------
  Sets the singular values to zero if they are lower than a specified
  threshold.
  ----------------------------------------------------------------------
  Input parameters : S      = vector of singular values
                     Tol    = relative tolerance
                              Threshold value will be Tol * Max(S)
                     Lbound = index of first vector element
                     Ubound = index of last vector element
  ----------------------------------------------------------------------
  Output parameter : S      = modified singular values
  ---------------------------------------------------------------------- }

procedure SV_Solve(U : PMatrix; S : PVector; V : PMatrix; B : PVector;
                   Lbound, Ubound1, Ubound2 : Integer;
                   X : PVector);
{ ----------------------------------------------------------------------
  Solves a system of equations by singular value decomposition, after
  the matrix has been transformed by SV_Decomp, and the lowest singular
  values have been set to zero by SV_SetZero.
  ----------------------------------------------------------------------
  Input parameters : U, S, V = vector and matrices from SV_Decomp
                     B       = constant vector
                     Lbound,
                     Ubound1,
                     Ubound2 = as in SV_Decomp
  ----------------------------------------------------------------------
  Output parameter : X       = solution vector
                             = V * Diag(1/s(i)) * U' * B, for s(i) <> 0
  ---------------------------------------------------------------------- }

procedure SV_Approx(U : PMatrix; S : PVector; V : PMatrix;
                    Lbound, Ubound1, Ubound2 : Integer;
                    A : PMatrix);
{ ----------------------------------------------------------------------
  Approximates a matrix A by the product USV', after the lowest singular
  values have been set to zero by SV_SetZero.
  ----------------------------------------------------------------------
  Input parameters : U, S, V = vector and matrices from SV_Decomp
                     Lbound,
                     Ubound1,
                     Ubound2 = as in SV_Decomp
  ----------------------------------------------------------------------
  Output parameter : A       = approximated matrix
  ---------------------------------------------------------------------- }

function QR_Decomp(A : PMatrix; Lbound, Ubound1, Ubound2 : Integer;
                   R : PMatrix) : Integer;
{ ----------------------------------------------------------------------
  QR decomposition. Factors the matrix A (n x m, with n >= m) as a
  product Q * R where Q is a (n x m) column-orthogonal matrix, and R
  a (m x m) upper triangular matrix. This routine is used in conjunction
  with QR_Solve to solve a system of equations.
  ----------------------------------------------------------------------
  Input parameters : A       = matrix
                     Lbound  = index of first matrix element
                     Ubound1 = index of last matrix element in 1st dim.
                     Ubound2 = index of last matrix element in 2nd dim.
  ----------------------------------------------------------------------
  Output parameter : A       = contains the elements of Q
                     R       = upper triangular matrix
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_SING
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

procedure QR_Solve(Q, R : PMatrix; B : PVector;
                   Lbound, Ubound1, Ubound2 : Integer;
                   X : PVector);
{ ----------------------------------------------------------------------
  Solves a system of equations by the QR decomposition,
  after the matrix has been transformed by QR_Decomp.
  ----------------------------------------------------------------------
  Input parameters : Q, R    = matrices from QR_Decomp
                     B       = constant vector
                     Lbound,
                     Ubound1,
                     Ubound2 = as in QR_Decomp
  ----------------------------------------------------------------------
  Output parameter : X       = solution vector
  ---------------------------------------------------------------------- }

implementation

var
  { Used by LU procedures }
  LastDim : Integer = 1;     { Dimension of the last system solved }
  Index : PIntVector = nil;  { Records the row permutations }

  procedure DimVector(var V : PVector; Ubound : Integer);
  var
    I : Integer;
  begin
    { Check bounds }
    if (Ubound < 0) or (Ubound > MAX_FLT) then
      begin
        V := nil;
        Exit;
      end;

    { Allocate vector }
    GetMem(V, Succ(Ubound) * SizeOf(Float));
    if V = nil then Exit;

    { Initialize vector }
    for I := 0 to Ubound do
      V^[I] := 0.0;
  end;

  procedure DimIntVector(var V : PIntVector; Ubound : Integer);
  var
    I : Integer;
  begin
    { Check bounds }
    if (Ubound < 0) or (Ubound > MAX_INT) then
      begin
        V := nil;
        Exit;
      end;

    { Allocate vector }
    GetMem(V, Succ(Ubound) * SizeOf(Integer));
    if V = nil then Exit;

    { Initialize vector }
    for I := 0 to Ubound do
      V^[I] := 0;
  end;

  procedure DimBoolVector(var V : PBoolVector; Ubound : Integer);
  var
    I : Integer;
  begin
    { Check bounds }
    if (Ubound < 0) or (Ubound > MAX_BOOL) then
      begin
        V := nil;
        Exit;
      end;

    { Allocate vector }
    GetMem(V, Succ(Ubound) * SizeOf(Boolean));
    if V = nil then Exit;

    { Initialize vector }
    for I := 0 to Ubound do
      V^[I] := False;
  end;

  procedure DimStrVector(var V : PStrVector; Ubound : Integer);
  var
    I : Integer;
  begin
    { Check bounds }
    if (Ubound < 0) or (Ubound > MAX_STR) then
      begin
        showmessage('DIMstr error');
        V := nil;
        Exit;
      end;

    { Allocate vector }

    GetMem(V, Succ(Ubound) * sizeof(TStrVector) {256});

    if V = nil then Exit;
    { Initialize vector }

    for I := 0 to Ubound do
      V^[I] := '';
    //showmessage(inttostr(Ubound)+'b'+inttostr(MAX_STR));
  end;

  procedure DimMatrix(var A : PMatrix; Ubound1, Ubound2 : Integer);
  var
    I, J : Integer;
    RowSize : Word;
  begin
    if (Ubound1 < 0) or (Ubound1 > MAX_VEC) or
    (Ubound2 < 0) or (Ubound2 > MAX_FLT) then
      begin
        A := nil;
        Exit;
      end;

    { Allocate matrix }
    GetMem(A, Succ(Ubound1) * SizeOf(PVector));
    if A = nil then Exit;

    { Size of a row }
    RowSize := Succ(Ubound2) * SizeOf(Float);

    { Allocate each row }
    for I := 0 to Ubound1 do
      begin
        GetMem(A^[I], RowSize);
        if A^[I] = nil then
          begin
            A := nil;
            Exit;
          end;
      end;

    { Initialize matrix }
    for I := 0 to Ubound1 do
      for J := 0 to Ubound2 do
        A^[I]^[J] := 0.0;
  end;

  procedure DimIntMatrix(var A : PIntMatrix; Ubound1, Ubound2 : Integer);
  var
    I, J : Integer;
    RowSize : Word;
  begin
    { Check bounds }
    if (Ubound1 < 0) or (Ubound1 > MAX_VEC) or
    (Ubound2 < 0) or (Ubound2 > MAX_INT) then
      begin
        A := nil;
        Exit;
      end;

    { Allocate matrix }
    GetMem(A, Succ(Ubound1) * SizeOf(PIntVector));
    if A = nil then Exit;

    { Size of a row }
    RowSize := Succ(Ubound2) * SizeOf(Integer);

    { Allocate each row }
    for I := 0 to Ubound1 do
      begin
        GetMem(A^[I], RowSize);
        if A^[I] = nil then
          begin
            A := nil;
            Exit;
          end;
      end;

    { Initialize matrix }
    for I := 0 to Ubound1 do
      for J := 0 to Ubound2 do
        A^[I]^[J] := 0;
  end;

  procedure DimBoolMatrix(var A : PBoolMatrix; Ubound1, Ubound2 : Integer);
  var
    I, J : Integer;
    RowSize : Word;
  begin
    { Check bounds }
    if (Ubound1 < 0) or (Ubound1 > MAX_VEC) or
    (Ubound2 < 0) or (Ubound2 > MAX_BOOL) then
      begin
        A := nil;
        Exit;
      end;

    { Allocate matrix }
    GetMem(A, Succ(Ubound1) * SizeOf(PBoolVector));
    if A = nil then Exit;

    { Size of a row }
    RowSize := Succ(Ubound2) * SizeOf(Boolean);

    { Allocate each row }
    for I := 0 to Ubound1 do
      begin
        GetMem(A^[I], RowSize);
        if A^[I] = nil then
          begin
            A := nil;
            Exit;
          end;
      end;

    { Initialize matrix }
    for I := 0 to Ubound1 do
      for J := 0 to Ubound2 do
        A^[I]^[J] := False;
  end;

  procedure DimStrMatrix(var A : PStrMatrix; Ubound1, Ubound2 : Integer);
  var
    I, J : Integer;
    RowSize : Word;
  begin
    { Check bounds }
    if (Ubound1 < 0) or (Ubound1 > MAX_VEC) or
    (Ubound2 < 0) or (Ubound2 > MAX_STR) then
      begin
        A := nil;
        Exit;
      end;

    { Allocate matrix }
    GetMem(A, Succ(Ubound1) * SizeOf(PStrVector));
    if A = nil then Exit;

    { Size of a row }
    RowSize := Succ(Ubound2) * 256;

    { Allocate each row }
    for I := 0 to Ubound1 do
      begin
        GetMem(A^[I], RowSize);
        if A^[I] = nil then
          begin
            A := nil;
            Exit;
          end;
      end;

    { Initialize matrix }
    for I := 0 to Ubound1 do
      for J := 0 to Ubound2 do
        A^[I]^[J] := '';
  end;

  procedure DelVector(var V : PVector; Ubound : Integer);
  begin
    if V <> nil then
      begin
        FreeMem(V, Succ(Ubound) * SizeOf(Float));
        V := nil;
      end;
  end;

  procedure DelIntVector(var V : PIntVector; Ubound : Integer);
  begin
    if V <> nil then
      begin
        FreeMem(V, Succ(Ubound) * SizeOf(Integer));
        V := nil;
      end;
  end;

  procedure DelBoolVector(var V : PBoolVector; Ubound : Integer);
  begin
    if V <> nil then
      begin
        FreeMem(V, Succ(Ubound) * SizeOf(Boolean));
        V := nil;
      end;
  end;

  procedure DelStrVector(var V : PStrVector; Ubound : Integer);
  begin
    if V <> nil then
      begin
        FreeMem(V{, Succ(Ubound) * 256});
        V := nil;
      end;
  end;

  procedure DelMatrix(var A : PMatrix; Ubound1, Ubound2 : Integer);
  var
    I : Integer;
    RowSize : Word;
  begin
    if A <> nil then
      begin
        RowSize := Succ(Ubound2) * SizeOf(Float);
        for I := Ubound1 downto 0 do
          FreeMem(A^[I], RowSize);
        FreeMem(A, Succ(Ubound1) * SizeOf(PVector));
        A := nil;
      end;
  end;

  procedure DelIntMatrix(var A : PIntMatrix; Ubound1, Ubound2 : Integer);
  var
    I : Integer;
    RowSize : Word;
  begin
    if A <> nil then
      begin
        RowSize := Succ(Ubound2) * SizeOf(Integer);
        for I := Ubound1 downto 0 do
          FreeMem(A^[I], RowSize);
        FreeMem(A, Succ(Ubound1) * SizeOf(PIntVector));
        A := nil;
      end;
  end;

  procedure DelBoolMatrix(var A : PBoolMatrix; Ubound1, Ubound2 : Integer);
  var
    I : Integer;
    RowSize : Word;
  begin
    if A <> nil then
      begin
        RowSize := Succ(Ubound2) * SizeOf(Boolean);
        for I := Ubound1 downto 0 do
          FreeMem(A^[I], RowSize);
        FreeMem(A, Succ(Ubound1) * SizeOf(PBoolVector));
        A := nil;
      end;
  end;

  procedure DelStrMatrix(var A : PStrMatrix; Ubound1, Ubound2 : Integer);
  var
    I : Integer;
    RowSize : Word;
  begin
    if A <> nil then
      begin
        RowSize := Succ(Ubound2) * 256;
        for I := Ubound1 downto 0 do
          FreeMem(A^[I], RowSize);
        FreeMem(A, Succ(Ubound1) * SizeOf(PStrVector));
        A := nil;
      end;
  end;

  procedure SwapRows(I, K : Integer; A : PMatrix; Lbound, Ubound : Integer);
  var
    J : Integer;
  begin
    for J := Lbound to Ubound do
      FSwap(A^[I]^[J], A^[K]^[J]);
  end;

  procedure SwapCols(J, K : Integer; A : PMatrix; Lbound, Ubound : Integer);
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      FSwap(A^[I]^[J], A^[I]^[K]);
  end;

  procedure CopyVector(Dest, Source : PVector; Lbound, Ubound : Integer);
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      Dest^[I] := Source^[I];
  end;

  procedure CopyMatrix(Dest, Source : PMatrix;
                       Lbound1, Lbound2, Ubound1, Ubound2 : Integer);
  var
    I, J : Integer;
  begin
    for I := Lbound1 to Ubound1 do
      for J := Lbound2 to Ubound2 do
        Dest^[I]^[J] := Source^[I]^[J];
  end;

  procedure CopyRowFromVector(Dest : PMatrix; Source : PVector;
                              Lbound, Ubound, Row : Integer);
  var
    J : Integer;
  begin
    for J := Lbound to Ubound do
      Dest^[Row]^[J] := Source^[J];
  end;

  procedure CopyColFromVector(Dest : PMatrix; Source : PVector;
                              Lbound, Ubound, Col : Integer);
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      Dest^[I]^[Col] := Source^[I];
  end;

  procedure CopyVectorFromRow(Dest : PVector; Source : PMatrix;
                              Lbound, Ubound, Row : Integer);
  var
    J : Integer;
  begin
    for J := Lbound to Ubound do
      Dest^[J] := Source^[Row]^[J];
  end;

  procedure CopyVectorFromCol(Dest : PVector; Source : PMatrix;
                              Lbound, Ubound, Col : Integer);
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      Dest^[I] := Source^[I]^[Col];
  end;

  function Min(X : PVector; Lbound, Ubound : Integer) : Float;
  var
    Xmin : Float;
    I : Integer;
  begin
    Xmin := X^[Lbound];
    for I := Succ(Lbound) to Ubound do
      if X^[I] < Xmin then Xmin := X^[I];
    Min := Xmin;
  end;

  function Max(X : PVector; Lbound, Ubound : Integer) : Float;
  var
    Xmax : Float;
    I : Integer;
  begin
    Xmax := X^[Lbound];
    for I := Succ(Lbound) to Ubound do
      if X^[I] > Xmax then Xmax := X^[I];
    Max := Xmax;
  end;

  function IntMin(X : PIntVector; Lbound, Ubound : Integer) : Integer;
  var
    I, Xmin : Integer;
  begin
    Xmin := X^[Lbound];
    for I := Succ(Lbound) to Ubound do
      if X^[I] < Xmin then Xmin := X^[I];
    IntMin := Xmin;
  end;

  function IntMax(X : PIntVector; Lbound, Ubound : Integer) : Integer;
  var
    I, Xmax : Integer;
  begin
    Xmax := X^[Lbound];
    for I := Succ(Lbound) to Ubound do
      if X^[I] > Xmax then Xmax := X^[I];
    IntMax := Xmax;
  end;

  procedure Transpose(A : PMatrix;
                      Lbound1, Lbound2, Ubound1, Ubound2 : Integer;
                      A_t : PMatrix);
  var
    I, J : Integer;
  begin
    for I := Lbound1 to Ubound1 do
      for J := Lbound2 to Ubound2 do
        A_t^[J]^[I] := A^[I]^[J];
  end;

  function GaussJordan(A : PMatrix; B : PVector; Lbound, Ubound : Integer;
                       A_inv : PMatrix; X : PVector) : Integer;
  var
    I, J, K : Integer;
    Pvt, T : Float;
    PRow, PCol : PIntVector;  { Store line and column of pivot }
  begin
    DimIntVector(PRow, Ubound);
    DimIntVector(PCol, Ubound);

    { Copy A into A_inv and B into X }
    CopyMatrix(A_inv, A, Lbound, Lbound, Ubound, Ubound);
    CopyVector(X, B, Lbound, Ubound);

    K := Lbound;
    while K <= Ubound do
      begin
        { Search for largest pivot in submatrix A_inv[K..Ubound, K..Ubound] }
        Pvt := A_inv^[K]^[K];
        PRow^[K] := K;
        PCol^[K] := K;
        for I := K to Ubound do
          for J := K to Ubound do
            if Abs(A_inv^[I]^[J]) > Abs(Pvt) then
              begin
                Pvt := A_inv^[I]^[J];
                PRow^[K] := I;
                PCol^[K] := J;
              end;

        { Pivot too weak ==> quasi-singular matrix }
        if Abs(Pvt) < MACHEP then
          begin
            DelIntVector(PRow, Ubound);
            DelIntVector(PCol, Ubound);
            GaussJordan := MAT_SINGUL;
            Exit;
          end;

        { Exchange current row (K) with pivot row }
        if PRow^[K] <> K then
          begin
            SwapRows(PRow^[K], K, A_inv, Lbound, Ubound);
            FSwap(X^[PRow^[K]], X^[K]);
          end;

        { Exchange current column (K) with pivot column }
        if PCol^[K] <> K then
          SwapCols(PCol^[K], K, A_inv, Lbound, Ubound);

        { Transform pivot row }
        A_inv^[K]^[K] := 1.0;
        for J := Lbound to Ubound do
          A_inv^[K]^[J] := A_inv^[K]^[J] / Pvt;
        X^[K] := X^[K] / Pvt;

        { Transform other rows }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              T := A_inv^[I]^[K];
              A_inv^[I]^[K] := 0.0;
              for J := Lbound to Ubound do
                A_inv^[I]^[J] := A_inv^[I]^[J] - T * A_inv^[K]^[J];
              X^[I] := X^[I] - T * X^[K];
            end;
        Inc(K);
      end;

    { Rearrange inverse matrix }
    for I := Ubound downto Lbound do
      if PCol^[I] <> I then
        begin
          SwapRows(PCol^[I], I, A_inv, Lbound, Ubound);
          FSwap(X^[PCol^[I]], X^[I]);
        end;
    for J := Ubound downto Lbound do
      if PRow^[J] <> J then
        SwapCols(PRow^[J], J, A_inv, Lbound, Ubound);

    DelIntVector(PRow, Ubound);
    DelIntVector(PCol, Ubound);
    GaussJordan := MAT_OK;
  end;

  function InvMat(A : PMatrix; Lbound, Ubound : Integer;
                  A_inv : PMatrix) : Integer;
  var
    I, J, K : Integer;
    Pvt, T : Float;
    PRow, PCol : PIntVector;  { Store line and column of pivot }
  begin
    DimIntVector(PRow, Ubound);
    DimIntVector(PCol, Ubound);

    { Copy A into A_inv }
    CopyMatrix(A_inv, A, Lbound, Lbound, Ubound, Ubound);

    K := Lbound;
    while K <= Ubound do
      begin
        { Search for largest pivot in submatrix A_inv[K..Ubound, K..Ubound] }
        Pvt := A_inv^[K]^[K];
        PRow^[K] := K;
        PCol^[K] := K;
        for I := K to Ubound do
          for J := K to Ubound do
            if Abs(A_inv^[I]^[J]) > Abs(Pvt) then
              begin
                Pvt := A_inv^[I]^[J];
                PRow^[K] := I;
                PCol^[K] := J;
              end;

        { Pivot too weak ==> quasi-singular matrix }
        if Abs(Pvt) < MACHEP then
          begin
            DelIntVector(PRow, Ubound);
            DelIntVector(PCol, Ubound);
            InvMat := MAT_SINGUL;
            Exit;
          end;

        { Exchange current row (K) with pivot row }
        if PRow^[K] <> K then
          SwapRows(PRow^[K], K, A_inv, Lbound, Ubound);

        { Exchange current column (K) with pivot column }
        if PCol^[K] <> K then
          SwapCols(PCol^[K], K, A_inv, Lbound, Ubound);

        { Transform pivot row }
        A_inv^[K]^[K] := 1.0;
        for J := Lbound to Ubound do
          A_inv^[K]^[J] := A_inv^[K]^[J] / Pvt;

        { Transform other rows }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              T := A_inv^[I]^[K];
              A_inv^[I]^[K] := 0.0;
              for J := Lbound to Ubound do
                A_inv^[I]^[J] := A_inv^[I]^[J] - T * A_inv^[K]^[J];
            end;
        Inc(K);
      end;

    { Rearrange inverse matrix }
    for I := Ubound downto Lbound do
      if PCol^[I] <> I then
        SwapRows(PCol^[I], I, A_inv, Lbound, Ubound);
    for J := Ubound downto Lbound do
      if PRow^[J] <> J then
        SwapCols(PRow^[J], J, A_inv, Lbound, Ubound);

    DelIntVector(PRow, Ubound);
    DelIntVector(PCol, Ubound);
    InvMat := MAT_OK;
  end;

  function Det(A : PMatrix; Lbound, Ubound : Integer) : Float;
  var
    D, T : Float;       { Partial determinant & multiplier }
    I, J, K : Integer;  { Loop variables }
    ZeroDet : Boolean;  { Flags a null determinant }
  begin
    ZeroDet := False;
    D := 1.0;
    K := Lbound;

    { Make the matrix upper triangular }
    while not(ZeroDet) and (K < Ubound) do
      begin
        { If diagonal element is zero then switch rows }
        if Abs(A^[K]^[K]) < MACHEP then
          begin
            ZeroDet := True;
            I := K;

            { Try to find a row with a non-zero element in this column }
            while ZeroDet and (I < Ubound) do
              begin
                I := Succ(I);
                if Abs(A^[I]^[K]) > MACHEP then
                  begin
                    { Switch these two rows }
                    SwapRows(I, K, A, Lbound, Ubound);
                    ZeroDet := False;
                    { Switching rows changes the sign of the determinant }
                    D := - D;
                  end;
              end;
          end;

        if not(ZeroDet) then
          for I := Succ(K) to Ubound do
            if Abs(A^[I]^[K]) > MACHEP then
              begin
                { Make the K element of this row zero }
                T := - A^[I]^[K] / A^[K]^[K];
                for J := 1 to Ubound do
                  A^[I]^[J] := A^[I]^[J] + T * A^[K]^[J];
              end;

        D := D * A^[K]^[K];  { Multiply the diagonal term into D }
        Inc(K);
      end;

    if ZeroDet then
      Det := 0.0
    else
      Det := D * A^[Ubound]^[Ubound];
  end;

  function Cholesky(A : PMatrix; Lbound, Ubound : Integer;
                    L : PMatrix) : Integer;
  var
    I, J, K : Integer;
    Sum : Float;
  begin
    for K := Lbound to Ubound do
      begin
        Sum := A^[K]^[K];
        for J := Lbound to K - 1 do
          Sum := Sum - Sqr(L^[K]^[J]);

        if Sum <= 0.0 then
          begin
            Cholesky := MAT_NOT_PD;
            Exit;
          end;

        L^[K]^[K] := Sqrt(Sum);
        for I := K + 1 to Ubound do
          begin
            Sum := A^[I]^[K];
            for J := Lbound to K - 1 do
              Sum := Sum - L^[I]^[J] * L^[K]^[J];
            L^[I]^[K] := Sum / L^[K]^[K];
          end;
      end;
    Cholesky := MAT_OK;
  end;

  function LU_Decomp(A : PMatrix; Lbound, Ubound : Integer) : Integer;
  const
    TINY = 1.0E-20;
  var
    I, Imax, J, K : Integer;
    Pvt, T, Sum : Float;
    V : PVector;
  begin
    DimVector(V, Ubound);
    { Reallocate Index }
    if Index <> nil then
      DelIntVector(Index, LastDim);
    DimIntVector(Index, Ubound);
    LastDim := Ubound;

    for I := Lbound to Ubound do
      begin
        Pvt := 0.0;
        for J := Lbound to Ubound do
          if Abs(A^[I]^[J]) > Pvt then
            Pvt := Abs(A^[I]^[J]);
        if Pvt < MACHEP then
          begin
            DelVector(V, Ubound);
            LU_Decomp := MAT_SINGUL;
            Exit;
          end;
        V^[I] := 1.0 / Pvt;
      end;
    for J := Lbound to Ubound do
      begin
        for I := Lbound to Pred(J) do
          begin
            Sum := A^[I]^[J];
            for K := Lbound to Pred(I) do
              Sum := Sum - A^[I]^[K] * A^[K]^[J];
            A^[I]^[J] := Sum;
          end;
        Pvt := 0.0;
        for I := J to Ubound do
          begin
            Sum := A^[I]^[J];
            for K := Lbound to Pred(J) do
              Sum := Sum - A^[I]^[K] * A^[K]^[J];
            A^[I]^[J] := Sum;
            T := V^[I] * Abs(Sum);
            if T > Pvt then
              begin
                Pvt := T;
                Imax := I;
              end;
          end;
        if J <> Imax then
          begin
            SwapRows(Imax, J, A, Lbound, Ubound);
            V^[Imax] := V^[J];
          end;
        Index^[J] := Imax;
        if A^[J]^[J] = 0.0 then
          A^[J]^[J] := TINY;
        if J <> Ubound then
          begin
            T := 1.0 / A^[J]^[J];
            for I := Succ(J) to Ubound do
              A^[I]^[J] := A^[I]^[J] * T;
          end;
      end;
    DelVector(V, Ubound);
    LU_Decomp := MAT_OK;
  end;

  procedure LU_Solve(A : PMatrix; B : PVector; Lbound, Ubound : Integer;
                     X : PVector);
  var
    I, Ip, J, K : Integer;
    Sum : Float;
  begin
    K := Pred(Lbound);
    CopyVector(X, B, Lbound, Ubound);
    for I := Lbound to Ubound do
      begin
        Ip := Index^[I];
        Sum := X^[Ip];
        X^[Ip] := X^[I];
        if K >= Lbound then
          for J := K to Pred(I) do
            Sum := Sum - A^[I]^[J] * X^[J]
        else if Sum <> 0.0 then
          K := I;
        X^[I] := Sum;
      end;
    for I := Ubound downto Lbound do
      begin
        Sum := X^[I];
        if I < Ubound then
          for J := Succ(I) to Ubound do
            Sum := Sum - A^[I]^[J] * X^[J];
        X^[I] := Sum / A^[I]^[I];
      end;
  end;

  function SV_Decomp(A : PMatrix; Lbound, Ubound1, Ubound2 : Integer;
                     S : PVector; V : PMatrix) : Integer;
  label
    1, 2, 3;
  var
    I, Its, J, JJ, K, L, N : Integer;
    Anorm, C, F, G, H, Sum, Scale, T, X, Y, Z : Float;
    R : PVector;
  begin
    G := 0.0;
    Scale := 0.0;
    Anorm := 0.0;
    DimVector(R, Ubound2);
    for I := Lbound to Ubound2 do
      begin
        L := I + 1;
        R^[I] := Scale * G;
        G := 0.0;
        Sum := 0.0;
        Scale := 0.0;
        if I <= Ubound1 then
          begin
            for K := I to Ubound1 do
              Scale := Scale + Abs(A^[K]^[I]);
            if Scale <> 0.0 then
              begin
                for K := I to Ubound1 do
                  begin
                    A^[K]^[I] := A^[K]^[I] / Scale;
                    Sum := Sum + A^[K]^[I] * A^[K]^[I];
                  end;
                F := A^[I]^[I];
                G := - Sgn(F) * Sqrt(Sum);
                H := F * G - Sum;
                A^[I]^[I] := F - G;
                if I <> Ubound2 then
                  begin
                    for J := L to Ubound2 do
                      begin
                        Sum := 0.0;
                        for K := I to Ubound1 do
                          Sum := Sum + A^[K]^[I] * A^[K]^[J];
                        F := Sum / H;
                        for K := I to Ubound1 do
                          A^[K]^[J] := A^[K]^[J] + F * A^[K]^[I];
                      end;
                  end;
                for K := I to Ubound1 do
                  A^[K]^[I] := Scale * A^[K]^[I];
              end;
          end;
        S^[I] := Scale * G;
        G := 0.0;
        Sum := 0.0;
        Scale := 0.0;
        if (I <= Ubound1) and (I <> Ubound2) then
          begin
            for K := L to Ubound2 do
              Scale := Scale + Abs(A^[I]^[K]);
            if Scale <> 0.0 then
              begin
                for K := L to Ubound2 do
                  begin
                    A^[I]^[K] := A^[I]^[K] / Scale;
                    Sum := Sum + A^[I]^[K] * A^[I]^[K];
                  end;
                F := A^[I]^[L];
                G := - Sgn(F) * Sqrt(Sum);
                H := F * G - Sum;
                A^[I]^[L] := F - G;
                for K := L to Ubound2 do
                  R^[K] := A^[I]^[K] / H;
                if I <> Ubound1 then
                  for J := L to Ubound1 do
                    begin
                      Sum := 0.0;
                      for K := L to Ubound2 do
                        Sum := Sum + A^[J]^[K] * A^[I]^[K];
                      for K := L to Ubound2 do
                        A^[J]^[K] := A^[J]^[K] + Sum * R^[K];
                    end;
                for K := L to Ubound2 do
                  A^[I]^[K] := Scale * A^[I]^[K];
              end;
          end;
        Anorm := FMax(Anorm, Abs(S^[I]) + Abs(R^[I]));
      end;
    for I := Ubound2 downto Lbound do
      begin
        if I < Ubound2 then
          begin
            if G <> 0.0 then
              begin
                for J := L to Ubound2 do
                  V^[J]^[I] := (A^[I]^[J] / A^[I]^[L]) / G;
                for J := L to Ubound2 do
                  begin
                    Sum := 0.0;
                    for K := L to Ubound2 do
                      Sum := Sum + A^[I]^[K] * V^[K]^[J];
                    for K := L to Ubound2 do
                      V^[K]^[J] := V^[K]^[J] + Sum * V^[K]^[I];
                  end;
              end;
            for J := L to Ubound2 do
              begin
                V^[I]^[J] := 0.0;
                V^[J]^[I] := 0.0;
              end;
          end;
        V^[I]^[I] := 1.0;
        G := R^[I];
        L := I;
      end;
    for I := Ubound2 downto Lbound do
      begin
        L := I + 1;
        G := S^[I];
        if I < Ubound2 then
          for J := L to Ubound2 do
            A^[I]^[J] := 0.0;
        if G <> 0.0 then
          begin
            G := 1.0 / G;
            if I <> Ubound2 then
              for J := L to Ubound2 do
                begin
                  Sum := 0.0;
                  for K := L to Ubound1 do
                    Sum := Sum + A^[K]^[I] * A^[K]^[J];
                  F := (Sum / A^[I]^[I]) * G;
                  for K := I to Ubound1 do
                    A^[K]^[J] := A^[K]^[J] + F * A^[K]^[I];
                end;
            for J := I to Ubound1 do
              A^[J]^[I] := A^[J]^[I] * G;
          end
        else
          for J := I to Ubound1 do
            A^[J]^[I] := 0.0;
        A^[I]^[I] := A^[I]^[I] + 1.0;
      end;
    for K := Ubound2 downto Lbound do
      begin
        for Its := 1 to 30 do
          begin
            for L := K downto Lbound do
              begin
                N := L - 1;
                if (Abs(R^[L]) + Anorm) = Anorm then goto 2;
                if (Abs(S^[N]) + Anorm) = Anorm then goto 1;
              end;
1:          T := 1.0;
            for I := L to K do
              begin
                F := T * R^[I];
                if (Abs(F) + Anorm) <> Anorm then
                  begin
                    G := S^[I];
                    H := Pythag(F, G);
                    S^[I] := H;
                    H := 1.0 / H;
                    C := G * H;
                    T := - (F * H);
                    for J := Lbound to Ubound1 do
                      begin
                        Y := A^[J]^[N];
                        Z := A^[J]^[I];
                        A^[J]^[N] := (Y * C) + (Z * T);
                        A^[J]^[I] := - (Y * T) + (Z * C);
                      end;
                  end;
              end;
2:          Z := S^[K];
            if L = K then
              begin
                if Z < 0.0 then
                  begin
                    S^[K] := - Z;
                    for J := Lbound to Ubound2 do
                      V^[J]^[K] := - V^[J]^[K];
                  end;
                goto 3
              end;
            if Its = 30 then
              begin
                DelVector(R, Ubound2);
                SV_Decomp := MAT_NON_CONV;
                Exit;
              end;
            X := S^[L];
            N := K - 1;
            Y := S^[N];
            G := R^[N];
            H := R^[K];
            F := ((Y - Z) * (Y + Z) + (G - H) * (G + H)) / (2.0 * H * Y);
            G := Pythag(F, 1.0);
            F := ((X - Z) * (X + Z) + H * ((Y / (F + Sgn(F) * Abs(G))) - H)) / X;
            C := 1.0;
            T := 1.0;
            for J := L to N do
              begin
                I := J + 1;
                G := R^[I];
                Y := S^[I];
                H := T * G;
                G := C * G;
                Z := Pythag(F, H);
                R^[J] := Z;
                C := F / Z;
                T := H / Z;
                F := (X * C) + (G * T);
                G := - (X * T) + (G * C);
                H := Y * T;
                Y := Y * C;
                for JJ := Lbound to Ubound2 do
                  begin
                    X := V^[JJ]^[J];
                    Z := V^[JJ]^[I];
                    V^[JJ]^[J] := (X * C) + (Z * T);
                    V^[JJ]^[I] := - (X * T) + (Z * C);
                  end;
                Z := Pythag(F, H);
                S^[J] := Z;
                if Z <> 0.0 then
                  begin
                    Z := 1.0 / Z;
                    C := F * Z;
                    T := H * Z;
                  end;
                F := (C * G) + (T * Y);
                X := - (T * G) + (C * Y);
                for JJ := Lbound to Ubound1 do
                  begin
                    Y := A^[JJ]^[J];
                    Z := A^[JJ]^[I];
                    A^[JJ]^[J] := (Y * C) + (Z * T);
                    A^[JJ]^[I] := - (Y * T) + (Z * C);
                  end
              end;
            R^[L] := 0.0;
            R^[K] := F;
            S^[K] := X;
          end;
3:
      end;
    DelVector(R, Ubound2);
    SV_Decomp := MAT_OK;
  end;

  procedure SV_SetZero(S : PVector; Lbound, Ubound : Integer; Tol : Float);
  var
    Threshold : Float;
    I : Integer;
  begin
    Threshold := Tol * Max(S, Lbound, Ubound);
    for I := Lbound to Ubound do
      if S^[I] < Threshold then S^[I] := 0.0;
  end;

  procedure SV_Solve(U : PMatrix; S : PVector; V : PMatrix; B : PVector;
                     Lbound, Ubound1, Ubound2 : Integer;
                     X : PVector);
  var
    I, J, JJ : Integer;
    Sum : Float;
    Tmp : PVector;
  begin
    DimVector(Tmp, Ubound2);
    for J := Lbound to Ubound2 do
      begin
        Sum := 0.0;
        if S^[J] > 0.0 then
          begin
            for I := Lbound to Ubound1 do
              Sum := Sum + U^[I]^[J] * B^[I];
            Sum := Sum / S^[J];
          end;
        Tmp^[J] := Sum;
      end;
    for J := Lbound to Ubound2 do
      begin
        Sum := 0.0;
        for JJ := Lbound to Ubound2 do
          Sum := Sum + V^[J]^[JJ] * Tmp^[JJ];
        X^[J] := Sum;
      end;
    DelVector(Tmp, Ubound2);
  end;

  procedure SV_Approx(U : PMatrix; S : PVector; V : PMatrix;
                      Lbound, Ubound1, Ubound2 : Integer; A : PMatrix);
  var
    I, J, K : Integer;
  begin
    for I := Lbound to Ubound1 do
      for J := Lbound to Ubound2 do
        begin
          A^[I]^[J] := 0.0;
          for K := Lbound to Ubound2 do
            if S^[K] > 0.0 then
              A^[I]^[J] := A^[I]^[J] + U^[I]^[K] * V^[J]^[K];
        end;
  end;

  function QR_Decomp(A : PMatrix; Lbound, Ubound1, Ubound2 : Integer;
                     R : PMatrix) : Integer;
  var
    I, J, K : Integer;
    Sum : Float;
  begin
    for K := Lbound to Ubound2 do
      begin
        { Compute the "k"th diagonal entry in R }
        Sum := 0.0;
        for I := Lbound to Ubound1 do
          Sum := Sum + Sqr(A^[I]^[K]);

        if Sum = 0.0 then
          begin
            QR_Decomp := MAT_SINGUL;
            Exit;
          end;

        R^[K]^[K] := Sqrt(Sum);

        { Divide the entries in the "k"th column of A by the computed "k"th }
        { diagonal element of R.  this begins the process of overwriting A  }
        { with Q . . .                                                      }
        for I := Lbound to Ubound1 do
          A^[I]^[K] := A^[I]^[K] / R^[K]^[K];

        for J := (K + 1) to Ubound2 do
          begin
            { Complete the remainder of the row entries in R }
            Sum := 0.0;
            for I := Lbound to Ubound1 do
              Sum := Sum + A^[I]^[K] * A^[I]^[J];
            R^[K]^[J] := Sum;

            { Update the column entries of the Q/A matrix }
            for I := Lbound to Ubound1 do
              A^[I]^[J] := A^[I]^[J] - A^[I]^[K] * R^[K]^[J];
          end;
      end;

    QR_Decomp := MAT_OK;
  end;

  procedure QR_Solve(Q, R : PMatrix; B : PVector;
                     Lbound, Ubound1, Ubound2 : Integer;
                     X : PVector);
  var
    I, J : Integer;
    Sum : Float;
  begin
    { Form Q'B and store the result in X }
    for J := Lbound to Ubound2 do
      begin
        X^[J] := 0.0;
        for I := Lbound to Ubound1 do
          X^[J] := X^[J] + Q^[I]^[J] * B^[I];
      end;

    { Update X with the solution vector }
    X^[Ubound2] := X^[Ubound2] / R^[Ubound2]^[Ubound2];
    for I := (Ubound2 - 1) downto Lbound do
      begin
        Sum := 0.0;
        for J := (I + 1) to Ubound2 do
          Sum := Sum + R^[I]^[J] * X^[J];
        X^[I] := (X^[I] - Sum) / R^[I]^[I];
      end;
  end;

end.
