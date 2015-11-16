{ ******************************************************************
  This program computes the eigenvalues of a general square matrix
  (see EIGENSYM.PAS for a symmetric matrix). The matrix is stored in
  a data file with the following structure :

    Line 1             : dimension of the matrix (N)
    Lines 2 to (N + 1) : matrix

  The file MATRIX1.DAT is an example data file with N = 4
  ****************************************************************** }

program eigenval;

uses
  tpmath;

var
  A       : PMatrix;      { Matrix }
  N       : Integer;      { Dimension of matrix }
  Lambda  : PCompVector;  { Eigenvalues }
  I       : Integer;      { Loop variable }
  ErrCode : Integer;      { Error code }

  procedure ReadMatrix(FileName : String; var A : PMatrix;
                       var N : Integer);
{ ----------------------------------------------------------------------
  Reads matrix from file. Note that A is passed as a VAR parameter
  because it is dimensioned inside the procedure.
  ---------------------------------------------------------------------- }
  var
    F    : Text;     { Data file }
    I, J : Integer;  { Loop variable }
  begin
    Assign(F, FileName);
    Reset(F);
    Read(F, N);
    DimMatrix(A, N, N);
    for I := 1 to N do
      for J := 1 to N do
        Read(F, A^[I]^[J]);
    Close(F);
  end;

  procedure WriteMatrix(Title : String; A : PMatrix; N : Integer);
{ ----------------------------------------------------------------------
  Writes matrix on screen
  ---------------------------------------------------------------------- }
  var
    I, J : Integer;
  begin
    WriteLn;
    WriteLn(Title, ':');
    WriteLn;
    for I := 1 to N do
      begin
        for J := 1 to N do
          Write(A^[I]^[J]:12:6);
        WriteLn;
      end;
    WriteLn;
  end;

  procedure WriteEigenValue(Lambda : PCompVector; I : Integer);
{ ----------------------------------------------------------------------
  Writes the I-th eigenvalue
  ---------------------------------------------------------------------- }
  begin
    if Lambda^[I].Y = 0.0 then
      WriteLn(Lambda^[I].X:12:6)
    else
      begin
        Write(Lambda^[I].X:12:6);
        if Lambda^[I].Y > 0.0 then Write(' + ') else Write(' - ');
        WriteLn(Abs(Lambda^[I].Y):12:6, ' * i');
      end;
  end;

begin
  { Read matrix from file }
  ReadMatrix('matrix1.dat', A, N);
  WriteMatrix('Original matrix', A, N);

  { Dimension the vector containing the eigenvalues }
  DimCompVector(Lambda, N);

  { Compute eigenvalues }
  EigenVals(A, 1, N, Lambda);

  ErrCode := MathErr;

  if ErrCode = 0 then
    begin
      WriteLn('Eigenvalues:');
      WriteLn;
      for I := 1 to N do
        WriteEigenValue(Lambda, I);
    end
  else
    begin
      WriteLn('Unable to find eigenvalues Lambda[1] to Lambda[', -ErrCode, ']');
      WriteLn('Eigenvalues Lambda[', 1 - ErrCode, '] to Lambda[', N, ']:');
      for I := 1 - ErrCode to N do
        WriteEigenValue(Lambda, I);
    end;
end.
