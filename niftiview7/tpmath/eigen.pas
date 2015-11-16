{ **********************************************************************
  *                           Unit EIGEN.PAS                           *
  *                            Version 1.8                             *
  *                      (c) J. Debord, May 2001                       *
  **********************************************************************
          Procedures for computing eigenvalues and eigenvectors
  **********************************************************************
  References:
  1) Borland's Numerical Methods Toolbox : Jacobi
  2) 'Numerical Recipes' by Press et al. : EigenVals, RootPol
  ********************************************************************** }

unit Eigen;

interface

uses
  FMath, Matrices;

function Jacobi(A : PMatrix; Lbound, Ubound, MaxIter : Integer;
                Tol : Float; V : PMatrix; Lambda : PVector) : Integer;
{ ----------------------------------------------------------------------
  Eigenvalues and eigenvectors of a symmetric matrix by the iterative
  method of Jacobi
  ----------------------------------------------------------------------
  Input parameters  : A       = matrix
                      Lbound  = index of first matrix element
                      Ubound  = index of last matrix element
                      MaxIter = maximum number of iterations
                      Tol     = required precision
  ----------------------------------------------------------------------
  Output parameters : V      = matrix of eigenvectors (stored by lines)
                      Lambda = eigenvalues in decreasing order
  ----------------------------------------------------------------------
  Possible results  : MAT_OK
                      MAT_NON_CONV
  ----------------------------------------------------------------------
  NB : 1. The eigenvectors are normalized, with their first component > 0
       2. This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

function EigenVals(A : PMatrix; Lbound, Ubound : Integer;
                   Lambda_Re, Lambda_Im : PVector) : Integer;
{ ----------------------------------------------------------------------
  Eigenvalues of a general square matrix
  ----------------------------------------------------------------------
  Input parameters  : A      = matrix
                      Lbound = index of first matrix element
                      Ubound = index of last matrix element
  ----------------------------------------------------------------------
  Output parameters : Lambda_Re = real part of eigenvalues
                      Lambda_Im = imaginary part of eigenvalues
  ----------------------------------------------------------------------
  Possible results  : MAT_OK
                      MAT_NON_CONV
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

function EigenVect(A : PMatrix; Lbound, Ubound : Integer;
                   Lambda, Tol : Float; V : PVector) : Integer;
{ ----------------------------------------------------------------------
  Computes the eigenvector associated to a real eigenvalue
  ----------------------------------------------------------------------
  Input parameters  : A       = matrix
                      Lbound  = index of first matrix element
                      Ubound  = index of last matrix element
                      Lambda  = eigenvalue
                      Tol     = required precision
  ----------------------------------------------------------------------
  Output parameters : V       = eigenvector
  ----------------------------------------------------------------------
  Possible results  : MAT_OK
                      MAT_NON_CONV
  ----------------------------------------------------------------------
  NB : 1. The eigenvector is normalized, with its first component > 0
       2. The function returns only one eigenvector, even if the
          eigenvalue has a multiplicity greater than 1.
  ---------------------------------------------------------------------- }

procedure DivLargest(V : PVector; Lbound, Ubound : Integer;
                     var Largest : Float);
{ ----------------------------------------------------------------------
  Normalizes an eigenvector V by dividing by the element with the
  largest absolute value
  ---------------------------------------------------------------------- }

function RootPol(Coef : PVector; Deg : Integer;
                 X_Re, X_Im : PVector) : Integer;
{ ----------------------------------------------------------------------
  Real and complex roots of a real polynomial by the method of the
  companion matrix
  ----------------------------------------------------------------------
  Input parameters  : Coef = coefficients of polynomial
                      Deg  = degree of polynomial
  ----------------------------------------------------------------------
  Output parameters : X_Re = real parts of root (in increasing order)
                      X_Im = imaginary parts of root
  ----------------------------------------------------------------------
  Possible results  : MAT_OK
                      MAT_NON_CONV
  ---------------------------------------------------------------------- }

implementation

  function Jacobi(A : PMatrix; Lbound, Ubound, MaxIter : Integer;
                  Tol : Float; V : PMatrix; Lambda : PVector) : Integer;
  var
    SinTheta, CosTheta, TanTheta, Tan2Theta : Float;
    CosSqr, SinSqr, SinCos, SumSqrDiag : Float;
    AII, AJJ, AIJ, AIK, AJK, VIK, VJK, D : Float;
    I, J, K, Iter : Integer;
    Done : Boolean;
  begin
    Iter := 0;
    for I := Lbound to Ubound do
      for J := Lbound to Ubound do
        if I = J then
          V^[I]^[J] := 1.0
        else
          V^[I]^[J] := 0.0;

    repeat
      Iter := Succ(Iter);
      SumSqrDiag := 0.0;
      for I := Lbound to Ubound do
        SumSqrDiag := SumSqrDiag + Sqr(A^[I]^[I]);
      Done := True;

      for I := Lbound to Pred(Ubound) do
        for J := Succ(I) to Ubound do
          if Abs(A^[I]^[J]) > Tol * SumSqrDiag then
            begin
              Done := False;

              { Calculate rotation }
              D := A^[I]^[I] - A^[J]^[J];
              if Abs(D) > MACHEP then
                begin
                  Tan2Theta := D / (2.0 * A^[I]^[J]);
                  TanTheta := - Tan2Theta + Sgn(Tan2Theta) *
                  Sqrt(1.0 + Sqr(Tan2Theta));
                  CosTheta := 1.0 / Sqrt(1.0 + Sqr(TanTheta));
                  SinTheta := CosTheta * TanTheta;
                end
              else
                begin
                  CosTheta := SQRT2DIV2;                   { Sqrt(2)/2 }
                  SinTheta := Sgn(A^[I]^[J]) * SQRT2DIV2;
                end;

              { Rotate matrix }
              CosSqr := Sqr(CosTheta);
              SinSqr := Sqr(SinTheta);
              SinCos := SinTheta * CosTheta;
              AII := A^[I]^[I] * CosSqr + 2.0 * A^[I]^[J] * SinCos
              + A^[J]^[J] * SinSqr;
              AJJ := A^[I]^[I] * SinSqr - 2.0 * A^[I]^[J] * SinCos
              + A^[J]^[J] * CosSqr;
              AIJ := (A^[J]^[J] - A^[I]^[I]) * SinCos
              + A^[I]^[J] * (CosSqr - SinSqr);
              for K := Lbound to Ubound do
                if not(K in [I, J]) then
                  begin
                    AIK := A^[I]^[K] * CosTheta + A^[J]^[K] * SinTheta;
                    AJK := - A^[I]^[K] * SinTheta + A^[J]^[K] * CosTheta;
                    A^[I]^[K] := AIK;
                    A^[K]^[I] := AIK;
                    A^[J]^[K] := AJK;
                    A^[K]^[J] := AJK;
                  end;
              A^[I]^[I] := AII;
              A^[J]^[J] := AJJ;
              A^[I]^[J] := AIJ;
              A^[J]^[I] := AIJ;

              { Rotate eigenvectors }
              for K := Lbound to Ubound do
                begin
                  VIK := CosTheta * V^[I]^[K] + SinTheta * V^[J]^[K];
                  VJK := - SinTheta * V^[I]^[K] + CosTheta * V^[J]^[K];
                  V^[I]^[K] := VIK;
                  V^[J]^[K] := VJK;
                end;
            end;
    until Done or (Iter > MaxIter);

    { The diagonal terms of the transformed matrix are the eigenvalues }
    for I := Lbound to Ubound do
      Lambda^[I] := A^[I]^[I];

    if Iter > MaxIter then
      begin
        Jacobi := MAT_NON_CONV;
        Exit;
      end;

    { Sort eigenvalues and eigenvectors }
    for I := Lbound to Pred(Ubound) do
      begin
        K := I;
        D := Lambda^[I];
        for J := Succ(I) to Ubound do
          if Lambda^[J] > D then
            begin
              K := J;
              D := Lambda^[J];
            end;
        FSwap(Lambda^[I], Lambda^[K]);
        SwapRows(I, K, V, Lbound, Ubound);
      end;

    { Make sure that the first component of each eigenvector is > 0 }
    for I := Lbound to Ubound do
      if V^[I]^[Lbound] < 0.0 then
        for J := Lbound to Ubound do
          V^[I]^[J] := - V^[I]^[J];

    Jacobi := MAT_OK;
  end;

  procedure Balance(A : PMatrix; Lbound, Ubound : Integer);
  { Balances the matrix, i.e. reduces norm without affecting eigenvalues }
  const
    RADIX = 2;  { Base used for machine computations }
  var
    I, J, Last : Integer;
    C, F, G, R, S, Sqrdx : Float;
  begin
    Sqrdx := Sqr(RADIX);
    repeat
      Last := 1;
      for I := Lbound to Ubound do
        begin
          C := 0.0;
          R := 0.0;
          for J := Lbound to Ubound do
            if J <> I then
              begin
                C := C + Abs(A^[J]^[I]);
                R := R + Abs(A^[I]^[J]);
              end;
          if (C <> 0.0) and (R <> 0.0) then
            begin
              G := R / RADIX;
              F := 1.0;
              S := C + R;
              while C < G do
                begin
                  F := F * RADIX;
                  C := C * Sqrdx;
                end;
              G := R * RADIX;
              while C > G do
                begin
                  F := F / RADIX;
                  C := C / Sqrdx;
                end;
              if (C + R) / F < 0.95 * S then
                begin
                  Last := 0;
                  G := 1.0 / F;
                  for J := Lbound to Ubound do
                    A^[I]^[J] := A^[I]^[J] * G;
                  for J := Lbound to Ubound do
                    A^[J]^[I] := A^[J]^[I] * F;
                end;
            end;
        end;
    until Last <> 0;
  end;

  procedure ElmHes(A : PMatrix; Lbound, Ubound : Integer);
  { Reduces the matrix to upper Hessenberg form by elimination }
  var
    I, J, M : Integer;
    X, Y : Float;
  begin
    for M := Succ(Lbound) to Pred(Ubound) do
      begin
        X := 0.0;
        I := M;
        for J := M to Ubound do
          if Abs(A^[J]^[M - 1]) > Abs(X) then
            begin
              X := A^[J]^[M - 1];
              I := J;
            end;
        if I <> M then
          begin
            for J := Pred(M) to Ubound do
              FSwap(A^[I]^[J], A^[M]^[J]);
            for J := Lbound to Ubound do
              FSwap(A^[J]^[I], A^[J]^[M]);
          end;
        if X <> 0.0 then
          for I := Succ(M) to Ubound do
            begin
              Y := A^[I]^[M - 1];
              if Y <> 0.0 then
                begin
                  Y := Y / X;
                  A^[I]^[M - 1] := Y;
                  for J := M to Ubound do
                    A^[I]^[J] := A^[I]^[J] - Y * A^[M]^[J];
                  for J := Lbound to Ubound do
                    A^[J]^[M] := A^[J]^[M] + Y * A^[J]^[I];
                end;
            end;
      end;
    for I := (Lbound + 2) to Ubound do
      for J := Lbound to (I - 2) do
        A^[I]^[J] := 0.0;
  end;

  function Hqr(A : PMatrix; Lbound, Ubound : Integer;
               Lambda_Re, Lambda_Im : PVector) : Integer;
  { Finds the eigenvalues of an upper Hessenberg matrix }
  label 2, 3, 4;
  var
    I, Its, J, K, L, M, N : Integer;
    Anorm, P, Q, R, S, T, U, V, W, X, Y, Z : Float;

    function Sign(A, B : Float) : Float;
    begin
      if B < 0.0 then Sign := - Abs(A) else Sign := Abs(A)
    end;

  begin
    Anorm := Abs(A^[1]^[1]);
    for I := Succ(Lbound) to Ubound do
      for J := I - 1 to Ubound do
        Anorm := Anorm + Abs(A^[I]^[J]);
    N := Ubound;
    T := 0.0;
    while N >= Lbound do
      begin
        Its := 0;
2:      for L := N downto Succ(Lbound) do
          begin
            S := Abs(A^[L - 1]^[L - 1]) + Abs(A^[L]^[L]);
            if S = 0.0 then S := Anorm;
            if Abs(A^[L]^[L - 1]) <= MACHEP * S then goto 3
          end;
        L := Lbound;
3:      X := A^[N]^[N];
        if L = N then
          begin
            Lambda_Re^[N] := X + T;
            Lambda_Im^[N] := 0.0;
            N := N - 1
          end
        else
          begin
            Y := A^[N - 1]^[N - 1];
            W := A^[N]^[N - 1] * A^[N - 1]^[N];
            if L = N - 1 then
              begin
                P := 0.5 * (Y - X);
                Q := Sqr(P) + W;
                Z := Sqrt(Abs(Q));
                X := X + T;
                if Q >= 0.0 then
                  begin
                    Z := P + Sign(Z, P);
                    Lambda_Re^[N] := X + Z;
                    Lambda_Re^[N - 1] := Lambda_Re^[N];
                    if Z <> 0.0 then Lambda_Re^[N] := X - W / Z;
                    Lambda_Im^[N] := 0.0;
                    Lambda_Im^[N - 1] := 0.0
                  end
                else
                  begin
                    Lambda_Re^[N] := X + P;
                    Lambda_Re^[N - 1] := Lambda_Re^[N];
                    Lambda_Im^[N] := Z;
                    Lambda_Im^[N - 1] := - Z
                  end;
                N := N - 2
              end
            else
              begin
                if Its = 30 then
                  begin
                    Hqr := MAT_NON_CONV;
                    Exit;
                  end;
                if (Its = 10) or (Its = 20) then
                  begin
                    T := T + X;
                    for I := Lbound to N do
                      A^[I]^[I] := A^[I]^[I] - X;
                    S := Abs(A^[N]^[N - 1]) + Abs(A^[N - 1]^[N - 2]);
                    X := 0.75 * S;
                    Y := X;
                    W := - 0.4375 * Sqr(S)
                  end;
                Its := Its + 1;
                for M := N - 2 downto L do
                  begin
                    Z := A^[M]^[M];
                    R := X - Z;
                    S := Y - Z;
                    P := (R * S - W) / A^[M + 1]^[M] + A^[M]^[M + 1];
                    Q := A^[M + 1]^[M + 1] - Z - R - S;
                    R := A^[M + 2]^[M + 1];
                    S := Abs(P) + Abs(Q) + Abs(R);
                    P := P / S;
                    Q := Q / S;
                    R := R / S;
                    if M = L then goto 4;
                    U := Abs(A^[M]^[M - 1]) * (Abs(Q) + Abs(R));
                    V := Abs(P) * (Abs(A^[M - 1]^[M - 1]) + Abs(Z)
                                   + Abs(A^[M + 1]^[M + 1]));
                    if U <= MACHEP * V then goto 4
                  end;
4:              for I := M + 2 to N do
                  begin
                    A^[I]^[I - 2] := 0.0;
                    if I <> (M + 2) then A^[I]^[I - 3] := 0.0
                  end;
                for K := M to N - 1 do
                  begin
                    if K <> M then
                      begin
                        P := A^[K]^[K - 1];
                        Q := A^[K + 1]^[K - 1];
                        R := 0.0;
                        if K <> (N - 1) then
                          R := A^[K + 2]^[K - 1];
                        X := Abs(P) + Abs(Q) + Abs(R);
                        if X <> 0.0 then
                          begin
                            P := P / X;
                            Q := Q / X;
                            R := R / X
                          end
                      end;
                    S := Sign(Sqrt(Sqr(P) + Sqr(Q) + Sqr(R)), P);
                    if S <> 0.0 then
                      begin
                        if K = M then
                          begin
                            if L <> M then
                              A^[K]^[K - 1] := - A^[K]^[K - 1];
                          end
                        else
                          begin
                            A^[K]^[K - 1] := - S * X
                          end;
                        P := P + S;
                        X := P / S;
                        Y := Q / S;
                        Z := R / S;
                        Q := Q / P;
                        R := R / P;
                        for J := K to N do
                          begin
                            P := A^[K]^[J] + Q * A^[K + 1]^[J];
                            if K <> (N - 1) then
                              begin
                                P := P + R * A^[K + 2]^[J];
                                A^[K + 2]^[J] := A^[K + 2]^[J] - P * Z
                              end;
                            A^[K + 1]^[J] := A^[K + 1]^[J] - P * Y;
                            A^[K]^[J] := A^[K]^[J] - P * X
                          end;
                        for I := L to IMin(N, K + 3) do
                          begin
                            P := X * A^[I]^[K] + Y * A^[I]^[K + 1];
                            if K <> (N - 1) then
                              begin
                                P := P + Z * A^[I]^[K + 2];
                                A^[I]^[K + 2] := A^[I]^[K + 2] - P * R
                              end;
                            A^[I]^[K + 1] := A^[I]^[K + 1] - P * Q;
                            A^[I]^[K] := A^[I]^[K] - P
                          end
                      end
                  end;
                goto 2
              end
          end
      end;
    Hqr := MAT_OK;
  end;

  function EigenVals(A : PMatrix; Lbound, Ubound : Integer;
                     Lambda_Re, Lambda_Im : PVector) : Integer;
  begin
    Balance(A, Lbound, Ubound);
    ElmHes(A, Lbound, Ubound);
    EigenVals := Hqr(A, Lbound, Ubound, Lambda_Re, Lambda_Im);
  end;

  procedure DivLargest(V : PVector; Lbound, Ubound : Integer;
                       var Largest : Float);
  var
    I : Integer;
  begin
    Largest := V^[Lbound];
    for I := Succ(Lbound) to Ubound do
      if Abs(V^[I]) > Abs(Largest) then
        Largest := V^[I];
    for I := Lbound to Ubound do
      V^[I] := V^[I] / Largest;
  end;

  function EigenVect(A : PMatrix; Lbound, Ubound : Integer;
                     Lambda, Tol : Float; V : PVector) : Integer;

  procedure SetMatrix(A, A1 : PMatrix; Lbound, Ubound : Integer; Lambda : Float);
  { Form A1 = A - Lambda * I }
  var
    I : Integer;
  begin
    CopyMatrix(A1, A, Lbound, Lbound, Ubound, Ubound);
    for I := Lbound to Ubound do
      A1^[I]^[I] := A^[I]^[I] - Lambda;
  end;

  function Solve(A : PMatrix; Lbound, Ubound, N : Integer;
                 Tol : Float; V : PVector) : Integer;
  { Solve the system A*X = 0 after fixing the N-th unknown to 1 }
  var
    A1, W                          : PMatrix;
    B, S, X                        : PVector;
    ErrCode, I, I1, J, J1, Ubound1 : Integer;
  begin
    Ubound1 := Pred(Ubound);

    DimMatrix(A1, Ubound1, Ubound1);
    DimMatrix(W, Ubound1, Ubound1);
    DimVector(B, Ubound1);
    DimVector(S, Ubound1);
    DimVector(X, Ubound1);

    I1 := Pred(Lbound);
    for I := Lbound to Ubound do
      if I <> N then
        begin
          Inc(I1);
          J1 := 0;
          for J := Lbound to Ubound do
            if J <> N then
              begin
                Inc(J1);
                A1^[I1]^[J1] := A^[I]^[J];
              end
            else
              B^[I1] := - A^[I]^[J];
        end;

    ErrCode :=  SV_Decomp(A1, Lbound, Ubound1, Ubound1, S, W);

    if ErrCode = 0 then
      begin
        SV_SetZero(S, Lbound, Ubound1, Tol);
        SV_Solve(A1, S, W, B, Lbound, Ubound1, Ubound1, X);

        { Update eigenvector }
        I1 := 0;
        for I := Lbound to Ubound do
          if I = N then
            V^[I] := 1.0
          else
            begin
              Inc(I1);
              V^[I] := X^[I1];
            end;
      end;

    DelMatrix(A1, Ubound1, Ubound1);
    DelMatrix(W, Ubound1, Ubound1);
    DelVector(B, Ubound1);
    DelVector(S, Ubound1);
    DelVector(X, Ubound1);

    Solve := ErrCode;
  end;

  function ZeroVector(B : PVector; Lbound, Ubound : Integer; Tol : Float) : Boolean;
  { Check if vector B is zero }
  var
    I : Integer;
    Z : Boolean;
  begin
    Z := True;
    for I := Lbound to Ubound do
      Z := Z and (Abs(B^[I]) < Tol);
    ZeroVector := Z;
  end;

  function CheckEigenVector(A1 : PMatrix; V : PVector;
                            Lbound, Ubound : Integer; Tol : Float) : Boolean;
  { Check if the equation A1 * V = 0 holds }
  var
    I, K : Integer;
    B    : PVector;
  begin
    DimVector(B, Ubound);

    { Form B = A1 * V }
    for I := Lbound to Ubound do
      for K := Lbound to Ubound do
        B^[I] := B^[I] + A1^[I]^[K] * V^[K];

    { Check if B is zero }
    CheckEigenVector := ZeroVector(B, Lbound, Ubound, Tol);

    DelVector(B, Ubound);
  end;

  procedure Normalize(V : PVector; Lbound, Ubound : Integer);
  { Normalize eigenvector and make sure that the first component is >= 0 }
  var
    Sum, Norm : Float;
    I         : Integer;
  begin
    Sum := 0.0;
    for I := Lbound to Ubound do
      Sum := Sum + Sqr(V^[I]);
    Norm := Sqrt(Sum);
    for I := Lbound to Ubound do
      if V^[I] <> 0.0 then V^[I] := V^[I] / Norm;
    if V^[Lbound] < 0.0 then
      for I := Lbound to Ubound do
        if V^[I] <> 0.0 then V^[I] := - V^[I];
  end;

  var
    ErrCode, I : Integer;
    A1         : PMatrix;

  begin
    DimMatrix(A1, Ubound, Ubound);

    { Form A1 = A - Lambda * I }
    SetMatrix(A, A1, Lbound, Ubound, Lambda);

    { Try to solve the system A1*V=0 by eliminating 1 equation }
    I := Lbound;
    repeat
      if (Solve(A1, Lbound, Ubound, I, Tol, V) = 0) and
          CheckEigenVector(A1, V, Lbound, Ubound, Tol)
      then
        ErrCode := 0
      else
        ErrCode := - 1;
      Inc(I);
    until (ErrCode = 0) or (I > Ubound);

    if ErrCode = 0 then
      begin
        Normalize(V, Lbound, Ubound);
        EigenVect := MAT_OK;
      end
    else
      EigenVect := MAT_NON_CONV;

    DelMatrix(A1, Ubound, Ubound);
  end;

  function RootPol(Coef : PVector; Deg : Integer;
                   X_Re, X_Im : PVector) : Integer;
  var
    A : PMatrix;        { Companion matrix }
    N : Integer;        { Size of matrix }
    I, J, K : Integer;  { Loop variables }
    ErrCode : Integer;  { Error code }
    Temp : Float;
  begin
    N := Pred(Deg);
    DimMatrix(A, N, N);

    { Set up the companion matrix (to save space, begin at index 0) }
    for J := 0 to N do
      A^[0]^[J] := - Coef^[Deg - J - 1] / Coef^[Deg];
    for J := 0 to Pred(N) do
      A^[J + 1]^[J] := 1.0;

    { The roots of the polynomial are the eigenvalues of the companion matrix }
    Balance(A, 0, N);
    ErrCode := Hqr(A, 0, N, X_Re, X_Im);

    if ErrCode = MAT_OK then
      begin
        { Sort roots in increasing order of real parts }
        for I := 0 to N - 1 do
          begin
            K := I;
            Temp := X_Re^[I];
            for J := Succ(I) to N do
              if X_Re^[J] < Temp then
                begin
                  K := J;
                  Temp := X_Re^[J];
                end;
            FSwap(X_Re^[I], X_Re^[K]);
            FSwap(X_Im^[I], X_Im^[K]);
          end;

        { Transfer roots from 0..(Deg - 1) to 1..Deg }
        for J := N downto 0 do
          begin
            X_Re^[J + 1] := X_Re^[J];
            X_Im^[J + 1] := X_Im^[J];
          end;
      end;

    DelMatrix(A, N, N);
    RootPol := ErrCode;
  end;

end.
