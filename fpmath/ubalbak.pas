{ ******************************************************************
  Back transformation of eigenvectors
  ****************************************************************** }

unit ubalbak;

interface

uses
  utypes;

procedure BalBak(Z                    : PMatrix;
                 Lb, Ub, I_low, I_igh : Integer;
                 Scale                : PVector;
                 M                    : Integer);

implementation

procedure BalBak(Z                    : PMatrix;
                 Lb, Ub, I_low, I_igh : Integer;
                 Scale                : PVector;
                 M                    : Integer);
{ ------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine Balbak

  This procedure forms the eigenvectors of a real general matrix
  by back transforming those of the corresponding balanced matrix
  determined by Balance.

  On input:

    Z contains the real and imaginary parts of the eigenvectors
    to be back transformed.

    Lb, Ub are the lowest and highest indices
    of the elements of Z

    I_low and I_igh are integers determined by Balance.

    Scale contains information determining the permutations
    and scaling factors used by Balance.

    M is the index of the latest column of Z to be back transformed.

  On output:

    Z contains the real and imaginary parts of the transformed
    eigenvectors in its columns Lb..M
  ------------------------------------------------------------------ }
  var
    I, J, K : Integer;
    S       : Float;
  begin
    if M < Lb then Exit;

    if I_igh <> I_low then
      for I := I_low to I_igh do
        begin
          S := Scale^[I];
          { Left hand eigenvectors are back transformed if the
            foregoing statement is replaced by S := 1.0 / Scale^[I] }
          for J := Lb to M do
            Z^[I]^[J] := Z^[I]^[J] * S;
        end;

    for I := (I_low - 1) downto Lb do
      begin
        K := Round(Scale^[I]);
        if K <> I then
          for J := Lb to M do
            begin
              S := Z^[I]^[J];
              Z^[I]^[J] := Z^[K]^[J];
              Z^[K]^[J] := S;
            end;
      end;

    for I := (I_igh + 1) to Ub do
      begin
        K := Round(Scale^[I]);
        if K <> I then
          for J := Lb to M do
            begin
              S := Z^[I]^[J];
              Z^[I]^[J] := Z^[K]^[J];
              Z^[K]^[J] := S;
            end;
      end;
  end;

end.
