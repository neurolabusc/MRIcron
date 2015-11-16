{ ******************************************************************
  Eigenvalues and eigenvectors of a real upper Hessenberg matrix
  ****************************************************************** }

unit uhqr2;

interface

uses
  utypes, uminmax;

procedure Hqr2(H                    : PMatrix;
               Lb, Ub, I_low, I_igh : Integer;
               Lambda               : PCompVector;
               Z                    : PMatrix);

implementation

procedure Hqr2(H                    : PMatrix;
               Lb, Ub, I_low, I_igh : Integer;
               Lambda               : PCompVector;
               Z                    : PMatrix);
{ ------------------------------------------------------------------
  This function is a translation of the EISPACK subroutine hqr2

  This procedure finds the eigenvalues and eigenvectors of a real
  upper Hessenberg matrix by the QR method.

  On input:

    H contains the upper Hessenberg matrix.

    Lb, Ub are the lowest and highest indices
    of the elements of H

    I_low and I_igh are integers determined by the balancing subroutine
    Balance. If Balance has not been used, set I_low=Lb, I_igh=Ub

    Z contains the transformation matrix produced by Eltran after the
    reduction by Elmhes, or by Ortran after the reduction by Orthes, if
    performed. If the eigenvectors of the Hessenberg matrix are desired,
    Z must contain the identity matrix.

  On output:

    H has been destroyed.

    Wr and Wi contain the real and imaginary parts, respectively, of
    the eigenvalues. The eigenvalues are unordered except that complex
    conjugate pairs of values appear consecutively with the eigenvalue
    having the positive imaginary part first.

    Z contains the real and imaginary parts of the eigenvectors. If the
    i-th eigenvalue is real, the i-th column of Z contains its eigenvector.
    If the i-th eigenvalue is complex with positive imaginary part, the i-th
    and (i+1)-th columns of Z contain the real and imaginary parts of its
    eigenvector. The eigenvectors are unnormalized. If an error exit is made,
    none of the eigenvectors has been found.

    The function returns an error code:
       zero       for normal return,
       -j         if the limit of 30*N iterations is exhausted
                  while the j-th eigenvalue is being sought
                  (N being the size of the matrix). The eigenvalues
                  should be correct for indices j+1,...,Ub.
  ------------------------------------------------------------------
  Note: This is a crude translation. Many of the original goto's
  have been kept !
  ------------------------------------------------------------------ }

    procedure Cdiv(Ar, Ai, Br, Bi : Float; var Cr, Ci : Float);
    { Complex division, (Cr,Ci) = (Ar,Ai)/(Br,Bi) }
    var
      S, Ars, Ais, Brs, Bis : Float;
    begin
      S := Abs(Br) + Abs(Bi);
      Ars := Ar / S;
      Ais := Ai / S;
      Brs := Br / S;
      Bis := Bi / S;
      S := Sqr(Brs) + Sqr(Bis);
      Cr := (Ars * Brs + Ais * Bis) / S;
      Ci := (Ais * Brs - Ars * Bis) / S;
    end;

  var
    I, J, K, L, M, N, En, Na, Itn, Its, Mp2, Enm2                : Integer;
    P, Q, R, S, T, W, X, Y, Ra, Sa, Vi, Vr, Zz, Norm, Tst1, Tst2 : Float;
    NotLas                                                       : Boolean;

  label
    60, 70, 100, 130, 150, 170, 225, 260, 270, 280, 320, 330, 340,
    600, 630, 635, 640, 680, 700, 710, 770, 780, 790, 795, 800;

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
    if En < I_low then goto 340;
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
        Zz := H^[M]^[M];
        R := X - Zz;
        S := Y - Zz;
        P := (R * S - W) / H^[M + 1]^[M] + H^[M]^[M + 1];
        Q := H^[M + 1]^[M + 1] - Zz - R - S;
        R := H^[M + 2]^[M + 1];
        S := Abs(P) + Abs(Q) + Abs(R);
        P := P / S;
        Q := Q / S;
        R := R / S;
        if M = L then goto 150;
        Tst1 := Abs(P) * (Abs(H^[M - 1]^[M - 1]) + Abs(Zz) + Abs(H^[M + 1]^[M + 1]));
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
        Zz := R / S;
        Q := Q / P;
        R := R / P;
        if NotLas then goto 225;

        { Row modification }
        for J := K to Ub do
          begin
            P := H^[K]^[J] + Q * H^[K + 1]^[J];
            H^[K]^[J] := H^[K]^[J] - P * X;
            H^[K + 1]^[J] := H^[K + 1]^[J] - P * Y;
          end;

        J := Imin(En, K + 3);

        { Column modification }
        for I := Lb to J do
          begin
            P := X * H^[I]^[K] + Y * H^[I]^[K + 1];
            H^[I]^[K] := H^[I]^[K] - P;
            H^[I]^[K + 1] := H^[I]^[K + 1] - P * Q;
          end;

        { Accumulate transformations }
        for I := I_low to I_igh do
          begin
            P := X * Z^[I]^[K] + Y * Z^[I]^[K + 1];
            Z^[I]^[K] := Z^[I]^[K] - P;
            Z^[I]^[K + 1] := Z^[I]^[K + 1] - P * Q;
          end;
        goto 260;

225:
        { Row modification }
        for J := K to Ub do
          begin
            P := H^[K]^[J] + Q * H^[K + 1]^[J] + R * H^[K + 2]^[J];
            H^[K]^[J] := H^[K]^[J] - P * X;
            H^[K + 1]^[J] := H^[K + 1]^[J] - P * Y;
            H^[K + 2]^[J] := H^[K + 2]^[J] - P * Zz;
          end;

        J := Imin(En, K + 3);

        { Column modification }
        for I := Lb to J do
          begin
            P := X * H^[I]^[K] + Y * H^[I]^[K + 1] + Zz * H^[I]^[K + 2];
            H^[I]^[K] := H^[I]^[K] - P;
            H^[I]^[K + 1] := H^[I]^[K + 1] - P * Q;
            H^[I]^[K + 2] := H^[I]^[K + 2] - P * R;
          end;

        { Accumulate transformations }
        for I := I_low to I_igh do
          begin
            P := X * Z^[I]^[K] + Y * Z^[I]^[K + 1] + Zz * Z^[I]^[K + 2];
            Z^[I]^[K] := Z^[I]^[K] - P;
            Z^[I]^[K + 1] := Z^[I]^[K + 1] - P * Q;
            Z^[I]^[K + 2] := Z^[I]^[K + 2] - P * R;
          end;

260:  end;

    goto 70;

270: { One root found }
    H^[En]^[En] := X + T;
    Lambda^[En].X := H^[En]^[En];
    Lambda^[En].Y := 0.0;
    En := Na;
    goto 60;

280: { Two roots found }
    P := 0.5 * (Y - X);
    Q := P * P + W;
    Zz := Sqrt(Abs(Q));
    H^[En]^[En] := X + T;
    X := H^[En]^[En];
    H^[Na]^[Na] := Y + T;
    if Q < 0.0 then goto 320;

    { Real pair }
    Zz := P + DSgn(Zz, P);
    Lambda^[Na].X := X + Zz;
    Lambda^[En].X := Lambda^[Na].X;
    if Zz <> 0.0 then Lambda^[En].X := X - W / Zz;
    Lambda^[Na].Y := 0.0;
    Lambda^[En].Y := 0.0;
    X := H^[En]^[Na];
    S := Abs(X) + Abs(Zz);
    P := X / S;
    Q := Zz / S;
    R := Sqrt(P * P + Q * Q);
    P := P / R;
    Q := Q / R;

    { Row modification }
    for J := Na to Ub do
      begin
        Zz := H^[Na]^[J];
        H^[Na]^[J] := Q * Zz + P * H^[En]^[J];
        H^[En]^[J] := Q * H^[En]^[J] - P * Zz;
      end;

    { Column modification }
    for I := Lb to En do
      begin
        Zz := H^[I]^[Na];
        H^[I]^[Na] := Q * Zz + P * H^[I]^[En];
        H^[I]^[En] := Q * H^[I]^[En] - P * Zz;
      end;

    { Accumulate transformations }
    for I := I_low to I_igh do
      begin
        Zz := Z^[I]^[Na];
        Z^[I]^[Na] := Q * Zz + P * Z^[I]^[En];
        Z^[I]^[En] := Q * Z^[I]^[En] - P * Zz;
      end;

    goto 330;

320: { Complex pair }
    Lambda^[Na].X := X + P;
    Lambda^[En].X := Lambda^[Na].X;
    Lambda^[Na].Y := Zz;
    Lambda^[En].Y := - Zz;

330:
    En := Enm2;
    goto 60;

340:
    if Norm = 0.0 then Exit;

    { All roots found. Backsubstitute to find
      vectors of upper triangular form }
    for En := Ub downto Lb do
      begin
        P := Lambda^[En].X;
        Q := Lambda^[En].Y;
        Na := En - 1;
        if Q < 0.0 then
          goto 710
        else if Q = 0.0 then
          goto 600
        else
          goto 800;

600:    { Real vector }
        M := En;
        H^[En]^[En] := 1.0;
        if Na < Lb then goto 800;

        for I := Na downto Lb do
          begin
            W := H^[I]^[I] - P;
            R := 0.0;

            for J := M to En do
              R := R + H^[I]^[J] * H^[J]^[En];

            if Lambda^[I].Y >= 0.0 then goto 630;
            Zz := W;
            S := R;
            goto 700;
630:        M := I;
            if Lambda^[I].Y <> 0.0 then goto 640;
            T := W;
            if T <> 0.0 then goto 635;
            Tst1 := Norm;
            T := Tst1;
            repeat
              T := 0.01 * T;
              Tst2 := Norm + T;
            until Tst2 <= Tst1;
635:        H^[I]^[En] := - R / T;
            goto 680;

640:        { Solve real equations }
            X := H^[I]^[I + 1];
            Y := H^[I + 1]^[I];
            Q := Sqr(Lambda^[I].X - P) + Sqr(Lambda^[I].Y);
            T := (X * S - Zz * R) / Q;
            H^[I]^[En] := T;
            if Abs(X) > Abs(Zz) then
              H^[I + 1]^[En] := (- R - W * T) / X
            else
              H^[I + 1]^[En] := (- S - Y * T) / Zz;

680:        { Overflow control }
            T := Abs(H^[I]^[En]);
            if T = 0.0 then goto 700;
            Tst1 := T;
            Tst2 := Tst1 + 1.0 / Tst1;
            if Tst2 > Tst1 then goto 700;
            for J := I to En do
              H^[J]^[En] := H^[J]^[En] / T;
700:      end;
        { End real vector }
        goto 800;

        { Complex vector }
710:    M := Na;

        { Last vector component chosen imaginary so that
          eigenvector matrix is triangular }
        if Abs(H^[En]^[Na]) > Abs(H^[Na]^[En]) then
          begin
            H^[Na]^[Na] := Q / H^[En]^[Na];
            H^[Na]^[En] := - (H^[En]^[En] - P) / H^[En]^[Na];
          end
        else
          Cdiv(0.0, - H^[Na]^[En], H^[Na]^[Na] - P, Q, H^[Na]^[Na], H^[Na]^[En]);

        H^[En]^[Na] := 0.0;
        H^[En]^[En] := 1.0;
        Enm2 := Na - 1;
        if Enm2 < Lb then goto 800;

        for I := Enm2 downto Lb do
          begin
            W := H^[I]^[I] - P;
            Ra := 0.0;
            Sa := 0.0;

            for J := M to En do
              begin
                Ra := Ra + H^[I]^[J] * H^[J]^[Na];
                Sa := Sa + H^[I]^[J] * H^[J]^[En];
              end;

            if Lambda^[I].Y >= 0.0 then goto 770;
            Zz := W;
            R := Ra;
            S := Sa;
            goto 795;
770:        M := I;
            if Lambda^[I].Y <> 0.0 then goto 780;
            Cdiv(- Ra, - Sa, W, Q, H^[I]^[Na], H^[I]^[En]);
            goto 790;

            { Solve complex equations }
780:        X := H^[I]^[I + 1];
            Y := H^[I + 1]^[I];
            Vr := Sqr(Lambda^[I].X - P) + Sqr(Lambda^[I].Y) - Sqr(Q);
            Vi := (Lambda^[I].X - P) * 2.0 * Q;
            if (Vr = 0.0) and (Vi = 0.0) then
              begin
                Tst1 := Norm * (Abs(W) + Abs(Q) + Abs(X) + Abs(Y) + Abs(Zz));
                Vr := Tst1;
                repeat
                  Vr := 0.01 * Vr;
                  Tst2 := Tst1 + Vr;
                until Tst2 <= Tst1;
              end;
            Cdiv(X * R - Zz * Ra + Q * Sa, X * S - Zz * Sa - Q * Ra, Vr, Vi, H^[I]^[Na], H^[I]^[En]);
            if Abs(X) > Abs(Zz) + Abs(Q) then
              begin
                H^[I + 1]^[Na] := (- Ra - W * H^[I]^[Na] + Q * H^[I]^[En]) / X;
                H^[I + 1]^[En] := (- Sa - W * H^[I]^[En] - Q * H^[I]^[Na]) / X;
              end
            else
              Cdiv(- R - Y * H^[I]^[Na], - S - Y * H^[I]^[En], Zz, Q, H^[I + 1]^[Na], H^[I + 1]^[En]);

790:        { Overflow control }
            T := FMax(Abs(H^[I]^[Na]), Abs(H^[I]^[En]));
            if T = 0.0 then goto 795;
            Tst1 := T;
            Tst2 := Tst1 + 1.0 / Tst1;
            if Tst2 > Tst1 then goto 795;
            for J := I to En do
              begin
                H^[J]^[Na] := H^[J]^[Na] / T;
                H^[J]^[En] := H^[J]^[En] / T;
              end;

795:      end;
      { End complex vector }
800:  end;

    { End back substitution.
      Vectors of isolated roots }
    for I := Lb to Ub do
      if (I < I_low) or (I > I_igh) then
        for J := I to Ub do
          Z^[I]^[J] := H^[I]^[J];

    { Multiply by transformation matrix to give
      vectors of original full matrix. }
    for J := Ub downto I_low do
      begin
        M := Imin(J, I_igh);
        for I := I_low to I_igh do
          begin
            Zz := 0.0;
            for K := I_low to M do
              Zz := Zz + Z^[I]^[K] * H^[K]^[J];
            Z^[I]^[J] := Zz;
          end;
      end;

    SetErrCode(0);
  end;

end.
