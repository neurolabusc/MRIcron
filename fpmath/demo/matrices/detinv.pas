{ ******************************************************************
  This program computes the determinant and inverse of a square
  matrix. The matrix is stored in a data file with the following
  structure :

    Line 1             : dimension of the matrix (N)
    Lines 2 to (N + 1) : matrix

  The file MATRIX1.DAT is an example data file with N = 4
  ****************************************************************** }

program detinv;

uses
  tpmath;

procedure WriteMatrix(Title : String;
                      A     : PMatrix;
                      N     : Integer);
{ ------------------------------------------------------------------
  Writes a matrix
  ------------------------------------------------------------------ }
var
  I, J : Integer;
begin
  WriteLn(Title, ' :');
  WriteLn;
  for I := 1 to N do
    begin
      for J := 1 to N do
        Write(A^[I]^[J]:12:6);
      WriteLn;
    end;
  WriteLn;
end;

procedure WriteMatrixDet(Title : String;
                         A     : PMatrix;
                         N     : Integer;
                         D     : Float);
{ ------------------------------------------------------------------
  Writes a matrix and its determinant
  ------------------------------------------------------------------ }
begin
  WriteLn('Determinant = ', D:12:6);
  WriteLn;
  WriteMatrix(Title, A, N);
end;

var
  F      : Text;     { Data file }
  N      : Integer;  { Size of matrix }
  A      : PMatrix;  { Matrix }
  D1, D2 : Float;    { Determinants }
  I, J   : Integer;  { Loop variable }

begin
  { Read matrix from file }
  Assign(F, 'matrix1.dat');
  Reset(F);
  Read(F, N);

  DimMatrix(A, N, N);

  for I := 1 to N do
    for J := 1 to N do
      Read(F, A^[I]^[J]);

  Close(F);

  { Write matrix }
  WriteMatrix('Original matrix', A, N);

  { Compute inverse matrix and determinant }
  GaussJordan(A, 1, N, N, D1);
  case MathErr of
    MatOk :
      begin
        WriteMatrixDet('Inverse matrix', A, N, D1);
        { Reinvert matrix. D2 = 1/D1 }
        GaussJordan(A, 1, N, N, D2);
        if MathErr = MatOk then
          WriteMatrixDet('Reinverted inverse matrix', A, N, D2);
        WriteLn('Product of determinants = ', (D1 * D2):12:6);
      end;
    MatSing :
      Write('Singular matrix!');
    MatErrDim :
      Write('Non-compatible dimensions!');
  end;
end.

