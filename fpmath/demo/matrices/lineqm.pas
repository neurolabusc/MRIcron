{ ******************************************************************
  This program solves a system of linear equations (A * X = B) with
  several constant vectors by the Gauss-Jordan method. The system is
  stored in a data file with the following structure :

    Line 1 : size of matrix (N) and number of constant vectors (P)

    Following lines : first N columns = matrix
                      other columns = constant vectors

  The file MATRIX2.DAT is an example data file with N = 4 and P = 5
  ****************************************************************** }

program lineqm;

uses
  tpmath;

procedure WriteMatrix(Title      : String;
                      A          : PMatrix;
                      N          : Integer;
                      Col1, Col2 : Integer);
{ ------------------------------------------------------------------
  Writes a matrix from Col1 to Col2
  ------------------------------------------------------------------ }
var
  I, J : Integer;
begin
  WriteLn(Title, ' :');
  WriteLn;
  for I := 1 to N do
    begin
      for J := Col1 to Col2 do
        Write(A^[I]^[J]:12:6);
      WriteLn;
    end;
  WriteLn;
end;

var
  A    : PMatrix;  { System matrix }
  N, M : Integer;  { Matrix dimensions }
  D    : Float;    { Determinant }
  F    : Text;     { Data file }
  I, J : Integer;  { Loop variables }

begin
  { Read matrix from file }
  Assign(F, 'matrix2.dat');
  Reset(F);
  Read(F, N, M);

  DimMatrix(A, N, M);

  for I := 1 to N do
    for J := 1 to M do
      Read(F, A^[I]^[J]);

  Close(F);

  { Read and display data }
  WriteMatrix('System matrix', A, N, 1, N);
  WriteMatrix('Constant vectors', A, N, N + 1, M);

  { Solve system }
  GaussJordan(A, 1, N, M, D);

  { Write results }
  case MathErr of
    MatOk     : begin
                  WriteMatrix('Inverse matrix', A, N, 1, N);
                  WriteMatrix('Solution vectors', A, N, N + 1, M);
                  WriteLn('Determinant = ', D:12:6);
                end;
    MatSing   : WriteLn('Singular matrix!');
    MatErrDim : WriteLn('Non-compatible dimensions!');
  end;
end.
