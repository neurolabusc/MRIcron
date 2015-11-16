{ ******************************************************************
  Reduction of a square matrix to upper Hessenberg form
  ****************************************************************** }

unit uelmhes;

interface

uses
  utypes;

procedure ElmHes(A                    : PMatrix;
                 Lb, Ub, I_low, I_igh : Integer;
                 I_int                : PIntVector);

implementation

procedure ElmHes(A                    : PMatrix;
                 Lb, Ub, I_low, I_igh : Integer;
                 I_int                : PIntVector);
{ ------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine Elmhes

  Given a real general matrix, this procedure reduces a submatrix
  situated in rows and columns I_low through I_igh to upper
  Hessenberg form by stabilized elementary similarity transformations.

  On input:

    A contains the input matrix.

    Lb, Ub are the lowest and highest indices
    of the elements of A.

    I_low and I_igh are integers determined by the balancing procedure
    Balance. If Balance has not been used, set I_low = Lb, I_igh = Ub.

  On output:

    A contains the Hessenberg matrix. The multipliers which were used
    in the reduction are stored in the remaining triangle under the
    Hessenberg matrix.

    I_int contains information on the rows and columns interchanged
    in the reduction. Only elements I_low through I_igh are used.
  ------------------------------------------------------------------ }

  var
    I, J, M, La, Kp1, Mm1, Mp1 : Integer;
    X, Y                       : Float;

  begin
    La := I_igh - 1;
    Kp1 := I_low + 1;
    if La < Kp1 then Exit;

    for M := Kp1 to La do
      begin
        Mm1 := M - 1;
        X := 0.0;
        I := M;

        for J := M to I_igh do
          if Abs(A^[J]^[Mm1]) > Abs(X) then
            begin
              X := A^[J]^[Mm1];
              I := J;
            end;

        I_int^[M] := I;

        { Interchange rows and columns of A }
        if I <> M then
          begin
            for J := Mm1 to Ub do
              begin
                Y := A^[I]^[J];
                A^[I]^[J] := A^[M]^[J];
                A^[M]^[J] := Y;
              end;

            for J := Lb to I_igh do
              begin
                Y := A^[J]^[I];
                A^[J]^[I] := A^[J]^[M];
                A^[J]^[M] := Y;
              end;
          end;

        if X <> 0.0 then
          begin
            Mp1 := M + 1;
            for I := Mp1 to I_igh do
              begin
                Y := A^[I]^[Mm1];
                if Y <> 0.0 then
                  begin
                    Y := Y / X;
                    A^[I]^[Mm1] := Y;
                    for J := M to Ub do
                      A^[I]^[J] := A^[I]^[J] - Y * A^[M]^[J];
                    for J := Lb to I_igh do
                      A^[J]^[M] := A^[J]^[M] + Y * A^[J]^[I];
                  end;
              end;
          end;
      end;
  end;

end.
