{ ******************************************************************
  This program solves a system of linear equations (A * X = B) with
  a single constant vector by the Gauss-Jordan method. The system is
  stored in a data file with the following structure :

    Line 1          : dimension of the matrix (N)
    Following lines : first N columns = matrix
                      last column = constant vector

  The file MATRIX3.DAT is an example data file with N = 4
  ****************************************************************** }

program lineq1;

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
  D    : Float;    { Determinant }
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

  { Solve system }
  LinEq(A, B, 1, N, D);

  { Write results }
  case MathErr of
    MatOk   : begin
                WriteMatrix('Inverse matrix', A, N);
                WriteVector('Solution vector', B, N);
                WriteLn('Determinant = ', D:12:6);
              end;
    MatSing : WriteLn('Singular matrix!');
  end;
end.
