{ ******************************************************************
  Balances a matrix and tries to isolate eigenvalues
  ****************************************************************** }

unit ubalance;

interface

uses
  utypes;

procedure Balance(    A            : PMatrix;
                      Lb, Ub       : Integer;
                  var I_low, I_igh : Integer;
                      Scale        : PVector);

implementation

procedure Balance(    A            : PMatrix;
                      Lb, Ub       : Integer;
                  var I_low, I_igh : Integer;
                      Scale        : PVector);
{ ------------------------------------------------------------------
  This procedure is a translation of the EISPACK procedure Balanc.

  This procedure balances a real matrix and isolates eigenvalues
  whenever possible.

  On input:

    A contains the input matrix to be balanced.

    Lb, Ub are the lowest and highest indices of the elements of A.

  On output:

    A contains the balanced matrix.

    I_low and I_igh are two integers such that A[i,j]
    is equal to zero if
      (1) i is greater than j and
      (2) j=Lb,...,I_low-1 or i=I_igh+1,...,Ub.

    Scale contains information determining the permutations
    and scaling factors used.

    Suppose that the principal submatrix in rows I_low through I_igh
    has been balanced, that P[j] denotes the index interchanged
    with j during the permutation step, and that the elements
    of the diagonal matrix used are denoted by D[i,j].  then
        Scale[j] = P[j],    for j = Lb,...,I_low-1
                 = D[j,j],      j = I_low,...,I_igh
                 = P[j]         j = I_igh+1,...,Ub.
    the order in which the interchanges are made is
    Ub to I_igh+1, then Lb to I_low-1.

    Note that Lb is returned for I_igh if I_igh is < Lb formally
  ------------------------------------------------------------------ }

  const
    RADIX = 2;  { Base used in floating number representation }

  var
    I, J, M           : Integer;
    C, F, G, R, S, B2 : Float;
    Flag, Found, Conv : Boolean;

    procedure Exchange;
    { Row and column exchange }
    var
      I : Integer;
    begin
      Scale^[M] := J;
      if J = M then Exit;

      for I := Lb to I_igh do
        begin
          F := A^[I]^[J];
          A^[I]^[J] := A^[I]^[M];
          A^[I]^[M] := F;
        end;

      for I := I_low to Ub do
        begin
          F := A^[J]^[I];
          A^[J]^[I] := A^[M]^[I];
          A^[M]^[I] := F;
        end;
    end;

  begin
    B2 := RADIX * RADIX;
    I_low := Lb;
    I_igh := Ub;

    { Search for rows isolating an eigenvalue and push them down }
    repeat
      J := I_igh;
      repeat
        I := Lb;
        repeat
          Flag := (I <> J) and (A^[J]^[I] <> 0.0);
          I := I + 1;
        until Flag or (I > I_igh);
        Found := not Flag;
        if Found then
          begin
            M := I_igh;
            Exchange;
            I_igh := I_igh - 1;
          end;
        J := J - 1;
      until Found or (J < Lb);
    until (not Found) or (I_igh < Lb);

    if I_igh < Lb then I_igh := Lb;
    if I_igh = Lb then Exit;

    { Search for columns isolating an eigenvalue and push them left }
    repeat
      J := I_low;
      repeat
        I := I_low;
        repeat
          Flag := (I <> J) and (A^[I]^[J] <> 0.0);
          I := I + 1;
        until Flag or (I > I_igh);
        Found := not Flag;
        if Found then
          begin
            M := I_low;
            Exchange;
            I_low := I_low + 1;
          end;
        J := J + 1;
      until Found or (J > I_igh);
    until (not Found);

    { Now balance the submatrix in rows I_low to I_igh }
    for I := I_low to I_igh do
      Scale^[I] := 1.0;

    { Iterative loop for norm reduction }
    repeat
      Conv := True;

      for I := I_low to I_igh do
        begin
          C := 0.0;
          R := 0.0;

          for J := I_low to I_igh do
            if J <> I then
              begin
                C := C + Abs(A^[J]^[I]);
                R := R + Abs(A^[I]^[J]);
              end;

          { Guard against zero C or R due to underflow }
          if (C <> 0.0) and (R <> 0.0) then
            begin
              G := R / RADIX;
              F := 1.0;
              S := C + R;

              while C < G do
                begin
                  F := F * RADIX;
                  C := C * B2;
                end;

              G := R * RADIX;

              while C >= G do
                begin
                  F := F / RADIX;
                  C := C / B2;
                end;

              { Now balance }
              if (C + R) / F < 0.95 * S then
                begin
                  G := 1.0 / F;
                  Scale^[I] := Scale^[I] * F;
                  Conv := False;
                  for J := I_low to Ub do A^[I]^[J] := A^[I]^[J] * G;
                  for J := Lb to I_igh do A^[J]^[I] := A^[J]^[I] * F;
                end;
            end;
        end;
    until Conv;
  end;

end.
