{ **********************************************************************
  *                          Unit MATCOMP.PAS                          *
  *                             Version 1.3                            *
  *                     (c) J. Debord, August 2000                     *
  **********************************************************************
  Matrices with complex elements. See MATRICES.PAS for details
  concerning the dynamic allocation and use of matrices.
  **********************************************************************
  References:
  1) 'Basic Programs for Scientists and Engineers' by A.R. Miller
  2) 'Numerical Recipes' by Press et al.
  ********************************************************************** }

unit MatComp;

interface

uses
  FMath, FComp, Matrices;

{ **********************************************************************
  This section defines the vector and matrix types. Maximal sizes are
  given for a 16-bit compiler (TP/BP). Higher values may be used with
  a 32-bit compiler such as FPC.
  ********************************************************************** }

const
{$IFDEF DOUBLEREAL}
  MAX_COMP = 3854;   { Max size of complex vector }
{$ELSE}
{$IFDEF SINGLEREAL}
  MAX_COMP = 7280;
{$ELSE}
{$IFDEF PASCALREAL}
  MAX_COMP = 5040;
{$ELSE}
  {$DEFINE EXTENDEDREAL}
  MAX_COMP = 3119;
{$ENDIF}
{$ENDIF}
{$ENDIF}

type
  TCompVector = array[0..MAX_COMP] of Complex;
  PCompVector = ^TCompVector;

  TCompMatrix = array[0..MAX_VEC] of PCompVector;
  PCompMatrix = ^TCompMatrix;

{ **********************************************************************
  Memory allocation routines
  ********************************************************************** }

procedure DimCompVector(var V : PCompVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Creates complex vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimCompMatrix(var A : PCompMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Creates complex matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Memory deallocation routines
  ********************************************************************** }

procedure DelCompVector(V : PCompVector; Ubound : Integer);
{ ----------------------------------------------------------------------
  Deletes complex vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DelCompMatrix(A : PCompMatrix; Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Deletes complex matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Complex matrix functions
  ********************************************************************** }

function C_LU_Decomp(A : PCompMatrix; Lbound, Ubound : Integer) : Integer;
{ ----------------------------------------------------------------------
  LU decomposition
  ---------------------------------------------------------------------- }

procedure C_LU_Solve(A : PCompMatrix; B : PCompVector;
                     Lbound, Ubound : Integer; X : PCompVector);
{ ----------------------------------------------------------------------
  Solves a system of equations whose matrix has been transformed by
  C_LU_Decomp
  ---------------------------------------------------------------------- }

implementation

const
  { Used by LU procedures }
  LastDim : Integer = 1;     { Dimension of the last system solved }
  Index : PIntVector = nil;  { Records the row permutations }

  procedure DimCompVector(var V : PCompVector; Ubound : Integer);
  var
    I : Integer;
  begin
    { Check bounds }
    if (Ubound < 0) or (Ubound > MAX_COMP) then
      begin
        V := nil;
        Exit;
      end;

    { Allocate vector }
    GetMem(V, Succ(Ubound) * SizeOf(Complex));
    if V = nil then Exit;

    { Initialize vector }
    for I := 0 to Ubound do
      V^[I] := C_zero;
  end;

  procedure DimCompMatrix(var A : PCompMatrix; Ubound1, Ubound2 : Integer);
  var
    I, J : Integer;
    RowSize : Word;
  begin
    { Check bounds }
    if (Ubound1 < 0) or (Ubound1 > MAX_VEC) or
       (Ubound2 < 0) or (Ubound2 > MAX_COMP) then
         begin
           A := nil;
           Exit;
         end;

    { Size of a row }
    GetMem(A, Succ(Ubound1) * SizeOf(PCompVector));
    if A = nil then Exit;

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
        A^[I]^[J] := C_zero;
  end;

  procedure DelCompVector(V : PCompVector; Ubound : Integer);
  begin
    if V <> nil then
      begin
        FreeMem(V, Succ(Ubound) * SizeOf(Complex));
        V := nil;
      end;
  end;

  procedure DelCompMatrix(A : PCompMatrix; Ubound1, Ubound2 : Integer);
  var
    I : Integer;
    RowSize : Word;
  begin
    if A <> nil then
      begin
        RowSize := Succ(Ubound2) * SizeOf(Complex);
        for I := Ubound1 downto 0 do
          FreeMem(A^[I], RowSize);
        FreeMem(A, Succ(Ubound1) * SizeOf(PCompVector));
        A := nil;
      end;
  end;

  function C_LU_Decomp(A : PCompMatrix; Lbound, Ubound : Integer) : Integer;
  const
    TINY = 1.0E-20;
  var
    I, Imax, J, K : Integer;
    C, Pvt, T : Float;
    Sum, Z : Complex;
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
          begin
            C := CAbs(A^[I]^[J]);
            if C > Pvt then Pvt := C;
          end;
        if Pvt < MACHEP then
          begin
            DelVector(V, Ubound);
            C_LU_Decomp := MAT_SINGUL;
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
              begin
                { Sum := Sum - A^[I]^[K] * A^[K]^[J]; }
                CMult(A^[I]^[K], A^[K]^[J], Z);
                CSub(Sum, Z, Sum);
              end;
            A^[I]^[J] := Sum;
          end;
        Pvt := 0.0;
        for I := J to Ubound do
          begin
            Sum := A^[I]^[J];
            for K := Lbound to Pred(J) do
              begin
                { Sum := Sum - A^[I]^[K] * A^[K]^[J]; }
                CMult(A^[I]^[K], A^[K]^[J], Z);
                CSub(Sum, Z, Sum);
              end;
            A^[I]^[J] := Sum;
            T := V^[I] * CAbs(Sum);
            if T > Pvt then
              begin
                Pvt := T;
                Imax := I;
              end;
          end;
        if J <> Imax then
          begin
            { SwapRows(Imax, J, A, Lbound, Ubound); }
            for K := Lbound to Ubound do
              CSwap(A^[Imax]^[K], A^[J]^[K]);
            V^[Imax] := V^[J];
          end;
        Index^[J] := Imax;
        if CAbs(A^[J]^[J]) = 0.0 then
          CSet(A^[J]^[J], TINY, TINY, Rec);
        if J <> Ubound then
          for I := Succ(J) to Ubound do
            { A^[I]^[J] := A^[I]^[J] / A^[J]^[J]; }
            CDiv(A^[I]^[J], A^[J]^[J], A^[I]^[J]);
      end;
    DelVector(V, Ubound);
    C_LU_Decomp := MAT_OK;
  end;

  procedure C_LU_Solve(A : PCompMatrix; B : PCompVector;
                       Lbound, Ubound : Integer; X : PCompVector);
  var
    I, Ip, J, K : Integer;
    Sum, Z : Complex;
  begin
    K := Pred(Lbound);
    { CopyVector(X, B, Lbound, Ubound); }
    for I := Lbound to Ubound do
      X^[I] := B^[I];
    for I := Lbound to Ubound do
      begin
        Ip := Index^[I];
        Sum := X^[Ip];
        X^[Ip] := X^[I];
        if K >= Lbound then
          for J := K to Pred(I) do
            begin
              { Sum := Sum - A^[I]^[J] * X^[J] }
              CMult(A^[I]^[J], X^[J], Z);
              CSub(Sum, Z, Sum);
            end
        else if CAbs(Sum) <> 0.0 then
          K := I;
        X^[I] := Sum;
      end;
    for I := Ubound downto Lbound do
      begin
        Sum := X^[I];
        if I < Ubound then
          for J := Succ(I) to Ubound do
            begin
              { Sum := Sum - A^[I]^[J] * X^[J]; }
              CMult(A^[I]^[J], X^[J], Z);
              CSub(Sum, Z, Sum);
            end;
        { X^[I] := Sum / A^[I]^[I]; }
        CDiv(Sum, A^[I]^[I], X^[I]);
      end;
  end;

end.
