{ ******************************************************************
  Solution of a system of linear equations by Gauss-Jordan method
  ****************************************************************** }

unit ugausjor;

interface

uses
  utypes, uminmax;

procedure GaussJordan(A            : PMatrix;
                      Lb, Ub1, Ub2 : Integer;
                      var Det      : Float);
{ ------------------------------------------------------------------
  Transforms a matrix according to the Gauss-Jordan method
  ------------------------------------------------------------------
  Input parameters : A        = system matrix
                     Lb       = lower matrix bound in both dim.
                     Ub1, Ub2 = upper matrix bounds
  ------------------------------------------------------------------
  Output parameters: A   = transformed matrix
                     Det = determinant of A
  ------------------------------------------------------------------
  Possible results : MatOk     : No error
                     MatErrDim : Non-compatible dimensions
                     MatSing   : Singular matrix
  ------------------------------------------------------------------ }

implementation

procedure GaussJordan(A            : PMatrix;
                      Lb, Ub1, Ub2 : Integer;
                      var Det      : Float);
var
  Pvt        : Float;       { Pivot }
  Ik, Jk     : Integer;     { Pivot's row and column }
  I, J, K    : Integer;     { Loop variables }
  T          : Float;       { Temporary variable }
  PRow, PCol : PIntVector;  { Stores pivot's row and column }
  MCol       : PVector;     { Stores a column of matrix A }

  procedure Terminate(ErrCode : Integer);
  { Set error code and deallocate arrays }
  begin
    DelIntVector(PRow, Ub1);
    DelIntVector(PCol, Ub1);
    DelVector(MCol, Ub1);
    SetErrCode(ErrCode);
  end;

begin
  if Ub1 > Ub2 then
    begin
      SetErrCode(MatErrDim);
      Exit
    end;

  DimIntVector(PRow, Ub1);
  DimIntVector(PCol, Ub1);
  DimVector(MCol, Ub1);

  Det := 1.0;

  K := Lb;
  while K <= Ub1 do
    begin
      { Search for largest pivot in submatrix A[K..Ub1, K..Ub1] }
      Pvt := A^[K]^[K];
      Ik := K;
      Jk := K;
      for I := K to Ub1 do
        for J := K to Ub1 do
          if Abs(A^[I]^[J]) > Abs(Pvt) then
            begin
              Pvt := A^[I]^[J];
              Ik := I;
              Jk := J;
            end;

      { Store pivot's position }
      PRow^[K] := Ik;
      PCol^[K] := Jk;

      { Update determinant }
      Det := Det * Pvt;
      if Ik <> K then Det := - Det;
      if Jk <> K then Det := - Det;

      { Too weak pivot ==> quasi-singular matrix }
      if Abs(Pvt) < MachEp then
        begin
          Terminate(MatSing);
          Exit
        end;

      { Exchange current row (K) with pivot row (Ik) }
      if Ik <> K then
        for J := Lb to Ub2 do
          FSwap(A^[Ik]^[J], A^[K]^[J]);

      { Exchange current column (K) with pivot column (Jk) }
      if Jk <> K then
        for I := Lb to Ub1 do
          FSwap(A^[I]^[Jk], A^[I]^[K]);

      { Store column K of matrix A into MCol
        and set this column to zero }
      for I := Lb to Ub1 do
        if I <> K then
          begin
            MCol^[I] := A^[I]^[K];
            A^[I]^[K] := 0.0;
          end
        else
          begin
            MCol^[I] := 0.0;
            A^[I]^[K] := 1.0;
          end;

      { Transform pivot row }
      T := 1.0 / Pvt;
      for J := Lb to Ub2 do
        A^[K]^[J] := T * A^[K]^[J];

      { Transform other rows }
      for I := Lb to Ub1 do
        if I <> K then
          begin
            T := MCol^[I];
            for J := Lb to Ub2 do
              A^[I]^[J] := A^[I]^[J] - T * A^[K]^[J];
          end;

      Inc(K);
    end;

  { Exchange lines of inverse matrix }
  for I := Ub1 downto Lb do
    begin
      Ik := PCol^[I];
      if Ik <> I then
        for J := Lb to Ub2 do
          FSwap(A^[I]^[J], A^[Ik]^[J]);
    end;

  { Exchange columns of inverse matrix }
  for J := Ub1 downto Lb do
    begin
      Jk := PRow^[J];
      if Jk <> J then
        for I := Lb to Ub1 do
          FSwap(A^[I]^[J], A^[I]^[Jk]);
    end;

  Terminate(MatOk);
end;

end.
