{ ******************************************************************
  This program solves a system of linear equations (A * X = B) with
  a single constant vector by singular value decomposition. The
  system is stored in a data file with the following structure :

    Line 1          : dimension of the matrix (N)
    Following lines : first N columns = matrix
                      last column = constant vector

  The file MATRIX3.DAT is an example data file with N = 4
  ****************************************************************** }

program test_svd;

uses
  tpmath;

procedure WriteMatrix(Title : String;
                      A     : PMatrix;
                      N     : Integer);
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

procedure WriteVector(Title : String;
                      V     : PVector;
                      N     : Integer);
var
  I : Integer;
begin
  WriteLn(Title, ' :');
  WriteLn;
  for I := 1 to N do
    WriteLn(V^[I]:12:6);
  WriteLn;
end;

var
  N    : Integer;  { Matrix dimension }
  A    : PMatrix;  { System matrix }
  B    : PVector;  { Constant vector }
  S    : PVector;  { Singular values }
  V    : PMatrix;  { Matrix from SVD }
  X    : PVector;  { Solution vector }
  F    : Text;     { Data file }
  I, J : Integer;  { Loop variables }

begin
  { Read matrix from file }
  Assign(F, 'matrix3.dat');
  Reset(F);
  Read(F, N);

  DimMatrix(A, N, N);
  DimVector(B, N);

  for I := 1 to N do
    begin
      for J := 1 to N do
        Read(F, A^[I]^[J]);
      Read(F, B^[I]);
    end;

  Close(F);

  { Read and display data }
  WriteMatrix('System matrix', A, N);
  WriteVector('Constant vector', B, N);

  { Perform SV decomposition of A. If successful, solve system }

  DimVector(S, N);
  DimMatrix(V, N, N);

  SV_Decomp(A, 1, N, N, S, V);

  if MathErr = MatNonConv then
    begin
      WriteLn('Non-convergence!');
      Halt;
    end;

  DimVector(X, N);

  SV_Solve(A, S, V, B, 1, N, N, X);

  WriteVector('Solution vector', X, N);
end.
