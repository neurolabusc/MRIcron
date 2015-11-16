{ ******************************************************************
  Eigenvalues of a real upper Hessenberg matrix by the QR method
  ****************************************************************** }

unit uhqr;

interface

uses
  utypes, uminmax;

procedure Hqr(H                    : PMatrix;
              Lb, Ub, I_low, I_igh : Integer;
              Lambda               : PCompVector);

implementation

procedure Hqr(H                    : PMatrix;
              Lb, Ub, I_low, I_igh : Integer;
              Lambda               : PCompVector);
{ ------------------------------------------------------------------
  This function is a translation of the EISPACK subroutine hqr.

  This function finds the eigenvalues of a real upper Hessenberg
  matrix by the QR method.

  On input:

    H contains the upper Hessenberg matrix.

    Lb, Ub are the lowest and highest indices
    of the elements of H

    I_low and I_igh are integers determined by the balancing subroutine
    Balance. If Balance has not been used, set I_low = Lb, I_igh = Ub

  On output:

    H has been destroyed.

    Wr and Wi contain the real and imaginary parts, respectively, of
    the eigenvalues. The eigenvalues are unordered except that complex
    conjugate pairs of values appear consecutively with the eigenvalue
    having the positive imaginary part first.

    The function returns an error code:
       zero       for normal return,
       -j         if the limit of 30*N iterations is exhausted
                  while the j-th eigenvalue is being sought.
                  (N being the size of the matrix). The eigenvalues
                  should be correct for indices j+1,...,Ub.
  ------------------------------------------------------------------
  Note: This is a crude translation. Many of the original goto's
  have been kept !
  ------------------------------------------------------------------ }

  var
    I, J, K, L, M, N, En, Na, Itn, Its, Mp2, Enm2 : Integer;
    P, Q, R, S, T, W, X, Y, Z, Norm, Tst1, Tst2   : Float;
    NotLas                                        : Boolean;

  label
    60, 70, 100, 130, 150, 170, 225, 260, 270, 280, 320, 330;

  begin
    { Store roots isolated by Balance and compute matrix norm }
    K := Lb;
    Norm := 0.0;
    for I := Lb to Ub do
      begin
        for J := K to Ub do
          Norm := Norm + Abs(H^[I]^[J]);
        K := I;
        if (I < I_low) or (I > I_igh) then
          begin
            Lambda^[I].X := H^[I]^[I];
            Lambda^[I].Y := 0.0;
          end;
      end;

    N := Ub - Lb + 1;
    Itn := 30 * N;
    En := I_igh;
    T := 0.0;

60: { Search for next eigenvalues }
    if En < I_low then
      begin
        SetErrCode(0);
        Exit;
      end;

    Its := 0;
    Na := En - 1;
    Enm2 := Na - 1;

70: { Look for single small sub-diagonal element }
    for L := En downto I_low do
      begin
        if L = I_low then goto 100;
        S := Abs(H^[L - 1]^[L - 1]) + Abs(H^[L]^[L]);
        if S = 0.0 then S := Norm;
        Tst1 := S;
        Tst2 := Tst1 + Abs(H^[L]^[L - 1]);
        if Tst2 = Tst1 then goto 100;
      end;

100: { Form shift }
    X := H^[En]^[En];
    if L = En then goto 270;
    Y := H^[Na]^[Na];
    W := H^[En]^[Na] * H^[Na]^[En];
    if L = Na then goto 280;

    if Itn = 0 then
      begin
        { Set error -- all eigenvalues have not
          converged after 30*N iterations }
        SetErrCode(- En);
        Exit;
      end;

    if (Its <> 10) and (Its <> 20) then goto 130;

    { Form exceptional shift }
    T := T + X;

    for I := I_low to En do
      H^[I]^[I] := H^[I]^[I] - X;

    S := Abs(H^[En]^[Na]) + Abs(H^[Na]^[Enm2]);
    X := 0.75 * S;
    Y := X;
    W := - 0.4375 * S * S;

130:
    Its := Its + 1;
    Itn := Itn - 1;

    { Look for two consecutive small sub-diagonal elements }
    for M := Enm2 downto L do
      begin
        Z := H^[M]^[M];
        R := X - Z;
        S := Y - Z;
        P := (R * S - W) / H^[M + 1]^[M] + H^[M]^[M + 1];
        Q := H^[M + 1]^[M + 1] - Z - R - S;
        R := H^[M + 2]^[M + 1];
        S := Abs(P) + Abs(Q) + Abs(R);
        P := P / S;
        Q := Q / S;
        R := R / S;
        if M = L then goto 150;
        Tst1 := Abs(P) * (Abs(H^[M - 1]^[M - 1]) + Abs(Z) + Abs(H^[M + 1]^[M + 1]));
        Tst2 := Tst1 + Abs(H^[M]^[M - 1]) * (Abs(Q) + Abs(R));
        if Tst2 = Tst1 then goto 150;
      end;

150:
    Mp2 := M + 2;

    for I := Mp2 to En do
      begin
        H^[I]^[I - 2] := 0.0;
        if I <> Mp2 then H^[I]^[I - 3] := 0.0;
      end;

    { Double QR step involving rows L to En and columns M to En }
    for K := M to Na do
      begin
        NotLas := (K <> Na);
        if (K = M) then goto 170;
        P := H^[K]^[K - 1];
        Q := H^[K + 1]^[K - 1];
        R := 0.0;
        if NotLas then R := H^[K + 2]^[K - 1];
        X := Abs(P) + Abs(Q) + Abs(R);
        if X = 0.0 then goto 260;
        P := P / X;
        Q := Q / X;
        R := R / X;
170:    S := DSgn(Sqrt(P * P + Q * Q + R * R), P);
        if K <> M then
          H^[K]^[K - 1] := - S * X
        else if L <> M then
          H^[K]^[K - 1] := - H^[K]^[K - 1];
        P := P + S;
        X := P / S;
        Y := Q / S;
        Z := R / S;
        Q := Q / P;
        R := R / P;
        if NotLas then goto 225;

        { Row modification }
        for J := K to En do
          begin
            P := H^[K]^[J] + Q * H^[K + 1]^[J];
            H^[K]^[J] := H^[K]^[J] - P * X;
            H^[K + 1]^[J] := H^[K + 1]^[J] - P * Y;
          end;

        J := Imin(En, K + 3);

        { Column modification }
        for I := L to J do
          begin
            P := X * H^[I]^[K] + Y * H^[I]^[K + 1];
            H^[I]^[K] := H^[I]^[K] - P;
            H^[I]^[K + 1] := H^[I]^[K + 1] - P * Q;
          end;
        goto 260;

225:
        { Row modification }
        for J := K to En do
          begin
            P := H^[K]^[J] + Q * H^[K + 1]^[J] + R * H^[K + 2]^[J];
            H^[K]^[J] := H^[K]^[J] - P * X;
            H^[K + 1]^[J] := H^[K + 1]^[J] - P * Y;
            H^[K + 2]^[J] := H^[K + 2]^[J] - P * Z;
          end;

        J := Imin(En, K + 3);

        { Column modification }
        for I := L to J do
          begin
            P := X * H^[I]^[K] + Y * H^[I]^[K + 1] + Z * H^[I]^[K + 2];
            H^[I]^[K] := H^[I]^[K] - P;
            H^[I]^[K + 1] := H^[I]^[K + 1] - P * Q;
            H^[I]^[K + 2] := H^[I]^[K + 2] - P * R;
          end;

260:  end;

    goto 70;

270: { One root found }
    Lambda^[En].X := X + T;
    Lambda^[En].Y := 0.0;
    En := Na;
    goto 60;

280: { Two roots found }
    P := 0.5 * (Y - X);
    Q := P * P + W;
    Z := Sqrt(Abs(Q));
    X := X + T;
    if Q < 0.0 then goto 320;

    { Real pair }
    Z := P + DSgn(Z, P);
    Lambda^[Na].X := X + Z;
    Lambda^[En].X := Lambda^[Na].X;
    if Z <> 0.0 then Lambda^[En].X := X - W / Z;
    Lambda^[Na].Y := 0.0;
    Lambda^[En].Y := 0.0;
    goto 330;

320: { Complex pair }
    Lambda^[Na].X := X + P;
    Lambda^[En].X := X + P;
    Lambda^[Na].Y := Z;
    Lambda^[En].Y := - Z;

330:
    En := Enm2;
    goto 60;
  end;

end.
