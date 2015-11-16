{ ******************************************************************
  Eigenvalues and eigenvectors of a symmetric matrix
  ****************************************************************** }

unit ujacobi;

interface

uses
  utypes, uminmax, utrigo;

procedure Jacobi(A               : PMatrix;
                 Lb, Ub, MaxIter : Integer;
                 Tol             : Float;
                 Lambda          : PVector;
                 V               : PMatrix);
{ ------------------------------------------------------------------
  Eigenvalues and eigenvectors of a symmetric matrix by the
  iterative method of Jacobi
  ------------------------------------------------------------------
  Input parameters  : A       = matrix
                      Lb      = index of first matrix element
                      Ub      = index of last matrix element
                      MaxIter = maximum number of iterations
                      Tol     = required precision
  ------------------------------------------------------------------
  Output parameters : Lambda  = eigenvalues in decreasing order
                      V       = matrix of eigenvectors (columns)
  ------------------------------------------------------------------
  Possible results  : MatOk
                      MatNonConv
  ------------------------------------------------------------------
  The eigenvectors are normalized, with their first component > 0
  This procedure destroys the original matrix A
  ------------------------------------------------------------------ }

implementation

procedure Jacobi(A               : PMatrix;
                 Lb, Ub, MaxIter : Integer;
                 Tol             : Float;
                 Lambda          : PVector;
                 V               : PMatrix);

  var
    I, J, K, Im, Jm, Iter               : Integer;
    B, C, C2, Na, Nd, P, Q, S, S2, R, T : Float;

  begin
    Iter := 0;
    Na := 0.0;
    Nd := 0.0;
    R := 0.0;

    for I := Lb to Ub do
      begin
        V^[I]^[I] := 1.0;
        Nd := Nd + Sqr(A^[I]^[I]);
        if I <> Ub then
          for J := Succ(I) to Ub do
            begin
              R := R + Sqr(A^[I]^[J]);
              V^[I]^[J] := 0.0;
              V^[J]^[I] := 0.0;
            end;
      end;

    Na := Nd + 2.0 * R;

    repeat
      R := 0.0;
      for I := Lb to Pred(Ub) do
        for J := Succ(I) to Ub do
          begin
            T := Abs(A^[I]^[J]);
            if T > R then
              begin
                R := T;
                Im := I;
                Jm := J;
              end;
          end;

      B := A^[Im]^[Im] - A^[Jm]^[Jm];

      if B = 0 then
        begin
          C := Sqrt2div2;
          S := C * Sgn(A^[Im]^[Jm]);
        end
      else
        begin
          P := 2.0 * A^[Im]^[Jm] * Sgn(B);
          Q := Abs(B);
          R := Pythag(P, Q);
          C := Sqrt(0.5 * (1.0 + Q / R));
          S := 0.5 * P / (R * C);
        end;

      for K := Lb to Ub do
        begin
          R := V^[K]^[Im];
          V^[K]^[Im] := C * R + S * V^[K]^[Jm];
          V^[K]^[Jm] := C * V^[K]^[Jm] - S * R;
        end;

      if Im <> Lb then
        for K := Lb to Pred(Im) do
          begin
            R := A^[K]^[Im];
            A^[K]^[Im] := C * R + S * A^[K]^[Jm];
            A^[K]^[Jm] := C * A^[K]^[Jm] - S * R;
          end;

      if Jm <> Succ(Im) then
        for K := Succ(Im) to Pred(Jm) do
          begin
            R := A^[Im]^[K];
            A^[Im]^[K] := C * R + S * A^[K]^[Jm];
            A^[K]^[Jm] := C * A^[K]^[Jm] - S * R;
          end;

      if Jm <> Ub then
        for K := Succ(Jm) to Ub do
          begin
            R := A^[Im]^[K];
            A^[Im]^[K] := C * R + S * A^[Jm]^[K];
            A^[Jm]^[K] := C * A^[Jm]^[K] - S * R;
          end;

      Nd := Nd + 2.0 * Sqr(A^[Im]^[Jm]);

      C2 := Sqr(C);
      S2 := Sqr(S);
      P := 2.0 * S * C * A^[Im]^[Jm];
      R := A^[Im]^[Im];
      A^[Im]^[Im] := C2 * R + S2 * A^[Jm]^[Jm] + P;
      A^[Jm]^[Jm] := S2 * R + C2 * A^[Jm]^[Jm] - P;
      A^[Im]^[Jm] := 0.0;

      Inc(Iter);
      if Iter > MaxIter then
        begin
          SetErrCode(MatNonConv);
          Exit;
        end;
    until Abs(1.0 - Na / Nd) < Tol;

    { The diagonal terms of the transformed matrix are the eigenvalues }
    for I := Lb to Ub do
      Lambda^[I] := A^[I]^[I];

    { Sort eigenvalues and eigenvectors }
    for I := Lb to Pred(Ub) do
      begin
        K := I;
        R := Lambda^[I];
        for J := Succ(I) to Ub do
          if Lambda^[J] > R then
            begin
              K := J;
              R := Lambda^[J];
            end;

        FSwap(Lambda^[I], Lambda^[K]);
        for J := Lb to Ub do
          FSwap(V^[J]^[I], V^[J]^[K]);
      end;

    { Make sure that the first component of each eigenvector is > 0 }
    for J := Lb to Ub do
      if V^[Lb]^[J] < 0.0 then
        for I := Lb to Ub do
          V^[I]^[J] := - V^[I]^[J];

    SetErrCode(MatOk);
  end;

end.
