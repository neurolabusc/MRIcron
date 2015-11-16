{ ******************************************************************
  Singular value decomposition
  ****************************************************************** }

unit usvd;

interface

uses
  utypes, uminmax, utrigo;

procedure SV_Decomp(A            : PMatrix;
                    Lb, Ub1, Ub2 : Integer;
                    S            : PVector;
                    V            : PMatrix);
{ ------------------------------------------------------------------
  Singular value decomposition. Factors the matrix A (n x m, with
  n >= m) as a product U * S * V' where U is a (n x m) column-
  orthogonal matrix, S a (m x m) diagonal matrix with elements >= 0
  (the singular values) and V a (m x m) orthogonal matrix. This
  routine is used in conjunction with SV_Solve to solve a system of
  equations.
  ------------------------------------------------------------------
  Input parameters : A   = matrix
                     Lb  = index of first matrix element
                     Ub1 = index of last matrix element in 1st dim.
                     Ub2 = index of last matrix element in 2nd dim.
  ------------------------------------------------------------------
  Output parameter : A   = contains the elements of U
                     S   = vector of singular values
                     V   = orthogonal matrix
  ------------------------------------------------------------------
  Possible results :
    MatOk          : No error
    MatNonConv     : Non-convergence
    MatErrDim      : Non-compatible dimensions (n < m)
  ------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ------------------------------------------------------------------ }

procedure SV_SetZero(S      : PVector;
                     Lb, Ub : Integer;
                     Tol    : Float);
{ ------------------------------------------------------------------
  Sets the singular values to zero if they are lower than a
  specified threshold.
  ------------------------------------------------------------------
  Input parameters : S   = vector of singular values
                     Tol = relative tolerance
                           Threshold value will be Tol * Max(S)
                     Lb  = index of first vector element
                     Ub  = index of last vector element
  ------------------------------------------------------------------
  Output parameter : S   = modified singular values
  ------------------------------------------------------------------ }

procedure SV_Solve(U            : PMatrix;
                   S            : PVector;
                   V            : PMatrix;
                   B            : PVector;
                   Lb, Ub1, Ub2 : Integer;
                   X            : PVector);
{ ------------------------------------------------------------------
  Solves a system of equations by singular value decomposition,
  after the matrix has been transformed by SV_Decomp, and the lowest
  singular values have been set to zero by SV_SetZero.
  ------------------------------------------------------------------
  Input parameters : U, S, V      = vector and matrices
                                    from SV_Decomp
                     B            = constant vector
                     Lb, Ub1, Ub2 = as in SV_Decomp
  ------------------------------------------------------------------
  Output parameter : X = solution vector
                       = V * Diag(1/s(i)) * U' * B, for s(i) <> 0
  ------------------------------------------------------------------ }

procedure SV_Approx(U            : PMatrix;
                    S            : PVector;
                    V            : PMatrix;
                    Lb, Ub1, Ub2 : Integer;
                    A            : PMatrix);
{ ------------------------------------------------------------------
  Approximates a matrix A by the product USV', after the lowest
  singular values have been set to zero by SV_SetZero.
  ------------------------------------------------------------------
  Input parameters : U, S, V      = vector and matrices
                                    from SV_Decomp
                     Lb, Ub1, Ub2 = as in SV_Decomp
  ------------------------------------------------------------------
  Output parameter : A = approximated matrix
  ------------------------------------------------------------------ }


implementation

procedure SV_Decomp(A            : PMatrix;
                    Lb, Ub1, Ub2 : Integer;
                    S            : PVector;
                    V            : PMatrix);
{ ----------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine SVD

  This procedure determines the singular value decomposition A = U.S.V'
  of a real M by N rectangular matrix. Householder bidiagonalization and
  a variant of the QR algorithm are used.
  ----------------------------------------------------------------------
  This is a crude translation. Many of the original goto's
  have been kept!
  ---------------------------------------------------------------------- }

  var
    I, J, K, L, I1, K1, L1, Mn, Its           : Integer;
    C, F, G, H, T, X, Y, Z, Tst1, Tst2, Scale : Float;
    R                                         : PVector;

  label
    190, 210, 270, 290, 360, 390, 430, 460,
    475, 490, 520, 540, 565, 580, 650, 700;

  begin
    if Ub2 > Ub1 then
      begin
        SetErrCode(MatErrDim);
        Exit
      end;

    DimVector(R, Ub2);

    Scale := 0.0;
    G := 0.0;
    X := 0.0;

    { Householder reduction to bidiagonal form }
    for I := Lb to Ub2 do
      begin
        L := I + 1;
        R^[I] := Scale * G;
        G := 0.0;
        T := 0.0;
        Scale := 0.0;
        if I > Ub1 then goto 210;

        for K := I to Ub1 do
          Scale := Scale + Abs(A^[K]^[I]);

        if Scale = 0.0 then goto 210;

        for K := I to Ub1 do
          begin
            A^[K]^[I] := A^[K]^[I] / Scale;
            T := T + Sqr(A^[K]^[I]);
          end;

        F := A^[I]^[I];
        G := - DSgn(Sqrt(T), F);
        H := F * G - T;
        A^[I]^[I] := F - G;
        if I = Ub2 then goto 190;

        for J := L to Ub2 do
          begin
            T := 0.0;
            for K := I to Ub1 do
              T := T + A^[K]^[I] * A^[K]^[J];
            F := T / H;
            for K := I to Ub1 do
              A^[K]^[J] := A^[K]^[J] + F * A^[K]^[I];
          end;

190:    for K := I to Ub1 do
          A^[K]^[I] := Scale * A^[K]^[I];

210:    S^[I] := Scale * G;
        G := 0.0;
        T := 0.0;
        Scale := 0.0;
        if (I > Ub1) or (I = Ub2) then goto 290;

        for K := L to Ub2 do
          Scale := Scale + Abs(A^[I]^[K]);

        if Scale = 0.0 then goto 290;

        for K := L to Ub2 do
          begin
            A^[I]^[K] := A^[I]^[K] / Scale;
            T := T + Sqr(A^[I]^[K]);
          end;

        F := A^[I]^[L];
        G := - DSgn(Sqrt(T), F);
        H := F * G - T;
        A^[I]^[L] := F - G;

        for K := L to Ub2 do
          R^[K] := A^[I]^[K] / H;

        if I = Ub1 then goto 270;

        for J := L to Ub1 do
          begin
            T := 0.0;
            for K := L to Ub2 do
              T := T + A^[J]^[K] * A^[I]^[K];
            for K := L to Ub2 do
              A^[J]^[K] := A^[J]^[K] + T * R^[K];
          end;

270:    for K := L to Ub2 do
          A^[I]^[K] := Scale * A^[I]^[K];

290:    X := FMax(X, Abs(S^[I]) + Abs(R^[I]));
      end;

    { Accumulation of right-hand transformations }
    for I := Ub2 downto Lb do
      begin
        if I = Ub2 then goto 390;
        if G = 0.0 then goto 360;

        for J := L to Ub2 do
          { Double division avoids possible underflow }
          V^[J]^[I] := (A^[I]^[J] / A^[I]^[L]) / G;

        for J := L to Ub2 do
          begin
            T := 0.0;
            for K := L to Ub2 do
              T := T + A^[I]^[K] * V^[K]^[J];
            for K := L to Ub2 do
              V^[K]^[J] := V^[K]^[J] + T * V^[K]^[I];
          end;

360:    for J := L to Ub2 do
          begin
            V^[I]^[J] := 0.0;
            V^[J]^[I] := 0.0;
          end;

390:    V^[I]^[I] := 1.0;
        G := R^[I];
        L := I;
      end;


    { Accumulation of left-hand transformations }
    Mn := IMin(Ub1, Ub2);

    for I := Mn downto Lb do
      begin
        L := I + 1;
        G := S^[I];
        if I = Ub2 then goto 430;

        for J := L to Ub2 do
          A^[I]^[J] := 0.0;

430:    if G = 0.0 then goto 475;
        if I = Mn then goto 460;

        for J := L to Ub2 do
          begin
            T := 0.0;

            for K := L to Ub1 do
              T := T + A^[K]^[I] * A^[K]^[J];

            { Double division avoids possible underflow }
            F := (T / A^[I]^[I]) / G;

            for K := I to Ub1 do
              A^[K]^[J] := A^[K]^[J] + F * A^[K]^[I];
          end;

460:    for J := I to Ub1 do
          A^[J]^[I] := A^[J]^[I] / G;

        goto 490;

475:    for J := I to Ub1 do
          A^[J]^[I] := 0.0;

490:    A^[I]^[I] := A^[I]^[I] + 1.0;
      end;

    { Diagonalization of the bidiagonal form }
    Tst1 := X;
    for K := Ub2 downto Lb do
      begin
        K1 := K - 1;
        Its := 0;

520:    { Test for splitting }
        for L := K downto Lb do
          begin
            L1 := L - 1;
            Tst2 := Tst1 + Abs(R^[L]);
            if Tst2 = Tst1 then goto 565;
            { R^[Lb] is always zero, so there is no exit
                through the bottom of the loop  }
            Tst2 := Tst1 + Abs(S^[L1]);
            if Tst2 = Tst1 then goto 540;
          end;

540:    { Cancellation of R^[L] if L greater than 1 }
        C := 0.0;
        T := 1.0;

        for I := L to K do
          begin
            F := T * R^[I];
            R^[I] := C * R^[I];
            Tst2 := Tst1 + Abs(F);
            if Tst2 = Tst1 then goto 565;
            G := S^[I];
            H := Pythag(F, G);
            S^[I] := H;
            C := G / H;
            T := - F / H;

            for J := Lb to Ub1 do
              begin
                Y := A^[J]^[L1];
                Z := A^[J]^[I];
                A^[J]^[L1] := Y * C + Z * T;
                A^[J]^[I] := - Y * T + Z * C;
              end;
           end;

565:    { Test for convergence }
        Z := S^[K];
        if L = K then goto 650;

        if Its = 30 then
          begin
            SetErrCode(MatNonConv);
            DelVector(R, Ub2);
            Exit;
          end;

        { Shift from bottom 2 by 2 minor }
        Its := Its + 1;
        X := S^[L];
        Y := S^[K1];
        G := R^[K1];
        H := R^[K];
        F := 0.5 * (((G + Z) / H) * ((G - Z) / Y) + Y / H - H / Y);
        G := Pythag(F, 1.0);
        F := X - (Z / X) * Z + (H / X) * (Y / (F + DSgn(G, F)) - H);

        { Next QR transformation }
        C := 1.0;
        T := 1.0;

        for I1 := L to K1 do
          begin
            I := I1 + 1;
            G := R^[I];
            Y := S^[I];
            H := T * G;
            G := C * G;
            Z := Pythag(F, H);
            R^[I1] := Z;
            C := F / Z;
            T := H / Z;
            F := X * C + G * T;
            G := - X * T + G * C;
            H := Y * T;
            Y := Y * C;

            for J := Lb to Ub2 do
              begin
                X := V^[J]^[I1];
                Z := V^[J]^[I];
                V^[J]^[I1] := X * C + Z * T;
                V^[J]^[I] := - X * T + Z * C;
              end;

            Z := Pythag(F, H);
            S^[I1] := Z;
            { Rotation can be arbitrary if Z is zero }
            if Z = 0.0 then goto 580;
            C := F / Z;
            T := H / Z;
580:        F := C * G + T * Y;
            X := - T * G + C * Y;

            for J := Lb to Ub1 do
              begin
                Y := A^[J]^[I1];
                Z := A^[J]^[I];
                A^[J]^[I1] := Y * C + Z * T;
                A^[J]^[I] := - Y * T + Z * C;
              end;
           end;

        R^[L] := 0.0;
        R^[K] := F;
        S^[K] := X;
        goto 520;

650:    { Convergence }
        if Z >= 0.0 then goto 700;

        { S^[K] is made non-negative }
        S^[K] := - Z;

        for J := Lb to Ub2 do
          V^[J]^[K] := - V^[J]^[K];
700:  end;

    DelVector(R, Ub2);
    SetErrCode(MatOk);
  end;

procedure SV_SetZero(S      : PVector;
                     Lb, Ub : Integer;
                     Tol    : Float);
  var
    Threshold : Float;
    I         : Integer;
  begin
    Threshold := S^[Lb];
    for I := Lb + 1 to Ub do
      if S^[I] > Threshold then Threshold := S^[I];
    Threshold := Tol * Threshold;
    for I := Lb to Ub do
      if S^[I] < Threshold then S^[I] := 0.0;
  end;

procedure SV_Solve(U            : PMatrix;
                   S            : PVector;
                   V            : PMatrix;
                   B            : PVector;
                   Lb, Ub1, Ub2 : Integer;
                   X            : PVector);
  var
    I, J, K : Integer;
    Sum     : Float;
    Tmp     : PVector;
  begin
    DimVector(Tmp, Ub2);

    for J := Lb to Ub2 do
      begin
        Sum := 0.0;
        if S^[J] > 0.0 then
          begin
            for I := Lb to Ub1 do
              Sum := Sum + U^[I]^[J] * B^[I];
            Sum := Sum / S^[J];
          end;
        Tmp^[J] := Sum;
      end;

    for J := Lb to Ub2 do
      begin
        Sum := 0.0;
        for K := Lb to Ub2 do
          Sum := Sum + V^[J]^[K] * Tmp^[K];
        X^[J] := Sum;
      end;

    DelVector(Tmp, Ub2);
  end;

procedure SV_Approx(U            : PMatrix;
                    S            : PVector;
                    V            : PMatrix;
                    Lb, Ub1, Ub2 : Integer;
                    A            : PMatrix);
  var
    I, J, K : Integer;
  begin
    for I := Lb to Ub1 do
      for J := Lb to Ub2 do
        begin
          A^[I]^[J] := 0.0;
          for K := Lb to Ub2 do
            if S^[K] > 0.0 then
              A^[I]^[J] := A^[I]^[J] + U^[I]^[K] * V^[J]^[K];
        end;
  end;

end.
