{ ******************************************************************
  LU decomposition
  ****************************************************************** }

unit ulu;

interface

uses
  utypes, uminmax;

procedure LU_Decomp(A : PMatrix; Lb, Ub : Integer);
{ ----------------------------------------------------------------------
  LU decomposition. Factors the square matrix A as a product L * U,
  where L is a lower triangular matrix (with unit diagonal terms) and U
  is an upper triangular matrix. This routine is used in conjunction
  with LU_Solve to solve a system of equations.
  ----------------------------------------------------------------------
  Input parameters : A  = matrix
                     Lb = index of first matrix element
                     Ub = index of last matrix element
  ----------------------------------------------------------------------
  Output parameter : A  = contains the elements of L and U
  ----------------------------------------------------------------------
  Possible results : MatOk
                     MatSing
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

procedure LU_Solve(A : PMatrix; B : PVector; Lb, Ub : Integer;
                   X : PVector);
{ ----------------------------------------------------------------------
  Solves a system of equations whose matrix has been transformed by
  LU_Decomp
  ----------------------------------------------------------------------
  Input parameters : A      = result from LU_Decomp
                     B      = constant vector
                     Lb, Ub = as in LU_Decomp
  ----------------------------------------------------------------------
  Output parameter : X      = solution vector
  ---------------------------------------------------------------------- }

implementation

const
  InitDim : Integer    = 0;    { Initial vector size }
  Index   : PIntVector = nil;  { Records the row permutations }

procedure LU_Decomp(A : PMatrix; Lb, Ub : Integer);
  var
    I, Imax, J, K : Integer;
    Pvt, T, Sum   : Float;
    V             : PVector;
  begin
    { Reallocate Index if necessary}
    if Ub > InitDim then
      begin
        DelIntVector(Index, InitDim);
        DimIntVector(Index, Ub);
        InitDim := Ub;
      end;

    DimVector(V, Ub);

    for I := Lb to Ub do
      begin
        Pvt := 0.0;
        for J := Lb to Ub do
          if Abs(A^[I]^[J]) > Pvt then
            Pvt := Abs(A^[I]^[J]);
        if Pvt < MachEp then
          begin
            DelVector(V, Ub);
            SetErrCode(MatSing);
            Exit;
          end;
        V^[I] := 1.0 / Pvt;
      end;

    for J := Lb to Ub do
      begin
        for I := Lb to Pred(J) do
          begin
            Sum := A^[I]^[J];
            for K := Lb to Pred(I) do
              Sum := Sum - A^[I]^[K] * A^[K]^[J];
            A^[I]^[J] := Sum;
          end;
        Imax := 0;
        Pvt := 0.0;
        for I := J to Ub do
          begin
            Sum := A^[I]^[J];
            for K := Lb to Pred(J) do
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
            for K := Lb to Ub do
              FSwap(A^[Imax]^[K], A^[J]^[K]);
            V^[Imax] := V^[J];
          end;
        Index^[J] := Imax;
        if A^[J]^[J] = 0.0 then
          A^[J]^[J] := MachEp;
        if J <> Ub then
          begin
            T := 1.0 / A^[J]^[J];
            for I := Succ(J) to Ub do
              A^[I]^[J] := A^[I]^[J] * T;
          end;
      end;

    DelVector(V, Ub);
    SetErrCode(MatOk);
  end;

procedure LU_Solve(A : PMatrix; B : PVector; Lb, Ub : Integer;
                   X : PVector);
  var
    I, Ip, J, K : Integer;
    Sum         : Float;
  begin
    for I := Lb to Ub do
      X^[I] := B^[I];

    K := Pred(Lb);
    for I := Lb to Ub do
      begin
        Ip := Index^[I];
        Sum := X^[Ip];
        X^[Ip] := X^[I];
        if K >= Lb then
          for J := K to Pred(I) do
            Sum := Sum - A^[I]^[J] * X^[J]
        else if Sum <> 0.0 then
          K := I;
        X^[I] := Sum;
      end;

    for I := Ub downto Lb do
      begin
        Sum := X^[I];
        if I < Ub then
          for J := Succ(I) to Ub do
            Sum := Sum - A^[I]^[J] * X^[J];
        X^[I] := Sum / A^[I]^[I];
      end;
  end;

end.