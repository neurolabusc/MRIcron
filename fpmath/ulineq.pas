{ ******************************************************************
  Solution of a system of linear equations with a single
  constant vector by Gauss-Jordan method
  ****************************************************************** }

unit ulineq;

interface

uses
  utypes, uminmax;

procedure LinEq(A       : PMatrix;
                B       : PVector;
                Lb, Ub  : Integer;
                var Det : Float);
{ ------------------------------------------------------------------
  Solves a linear system according to the Gauss-Jordan method
  ------------------------------------------------------------------
  Input parameters  : A      = system matrix
                      B      = constant vector
                      Lb, Ub = lower and upper array bounds
  ------------------------------------------------------------------
  Output parameters : A   = inverse matrix
                      B   = solution vector
                      Det = determinant of A
  ------------------------------------------------------------------
  Possible results  : MatOk   : No error
                      MatSing : Singular matrix
  ------------------------------------------------------------------ }

implementation

procedure LinEq(A       : PMatrix;
                B       : PVector;
                Lb, Ub  : Integer;
                var Det : Float);
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
    DelIntVector(PRow, Ub);
    DelIntVector(PCol, Ub);
    DelVector(MCol, Ub);
    SetErrCode(ErrCode);
  end;

begin
  DimIntVector(PRow, Ub);
  DimIntVector(PCol, Ub);
  DimVector(MCol, Ub);

  Det := 1.0;

  K := Lb;
  while K <= Ub do
    begin
      { Search for largest pivot in submatrix
        A[K..Ub, K..Ub] }
      Pvt := A^[K]^[K];
      Ik := K;
      Jk := K;
      for I := K to Ub do
        for J := K to Ub do
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
        begin
          for J := Lb to Ub do
            FSwap(A^[Ik]^[J], A^[K]^[J]);
          FSwap(B^[Ik], B^[K]);
        end;

      { Exchange current column (K) with pivot column (Jk) }
      if Jk <> K then
        for I := Lb to Ub do
          FSwap(A^[I]^[Jk], A^[I]^[K]);

      { Store column K of matrix A into MCol
        and set this column to zero }
      for I := Lb to Ub do
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
      for J := Lb to Ub do
        A^[K]^[J] := T * A^[K]^[J];
      B^[K] := T * B^[K];

      { Transform other rows }
      for I := Lb to Ub do
        if I <> K then
          begin
            T := MCol^[I];
            for J := Lb to Ub do
              A^[I]^[J] := A^[I]^[J] - T * A^[K]^[J];
            B^[I] := B^[I] - T * B^[K];
          end;

      Inc(K);
    end;

  { Exchange lines of inverse matrix and solution vector }
  for I := Ub downto Lb do
    begin
      Ik := PCol^[I];
      if Ik <> I then
        for J := Lb to Ub do
          FSwap(A^[I]^[J], A^[Ik]^[J]);
      FSwap(B^[I], B^[Ik]);
    end;

  { Exchange columns of inverse matrix }
  for J := Ub downto Lb do
    begin
      Jk := PRow^[J];
      if Jk <> J then
        for I := Lb to Ub do
          FSwap(A^[I]^[J], A^[I]^[Jk]);
    end;

  Terminate(MatOk);
end;

end.
