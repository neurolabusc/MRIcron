{ ******************************************************************
  Save transformations used by ElmHes
  ****************************************************************** }

unit ueltran;

interface

uses
  utypes;

procedure Eltran(A                    : PMatrix;
                 Lb, Ub, I_low, I_igh : Integer;
                 I_int                : PIntVector;
                 Z                    : PMatrix);

implementation

procedure Eltran(A                    : PMatrix;
                 Lb, Ub, I_low, I_igh : Integer;
                 I_int                : PIntVector;
                 Z                    : PMatrix);
{ ------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine Eltran.

  This procedure accumulates the stabilized elementary similarity
  transformations used in the reduction of a real general matrix
  to upper Hessenberg form by Elmhes.

  On input:

    A contains the multipliers which were used in the reduction
    by Elmhes in its lower triangle below the subdiagonal.

    Lb, Ub are the lowest and highest indices
    of the elements of A

    I_low and I_igh are integers determined by the balancing procedure
    Balance. If Balance has not been used, set I_low=Lb, I_igh=Ub.

    I_int contains information on the rows and columns interchanged in
    the reduction by Elmhes. Only elements I_low through I_igh are used.

  On output:

    Z contains the transformation matrix produced in the reduction by
    Elmhes.
  ------------------------------------------------------------------ }

  var
    I, J, Mp, Mp1 : Integer;

  begin
    { Initialize Z to identity matrix }
    for I := Lb to Ub do
      for J := Lb to Ub do
        if I = J then Z^[I]^[J] := 1.0 else Z^[I]^[J] := 0.0;

    if I_igh < I_low then Exit;

    for Mp := I_igh - 1 downto I_low + 1 do
      begin
        Mp1 := Mp + 1;
        for I := Mp1 to I_igh do
          Z^[I]^[Mp] := A^[I]^[Mp - 1];
        I := I_int^[Mp];
        if I <> Mp then
          begin
            for J := Mp to I_igh do
              begin
                Z^[Mp]^[J] := Z^[I]^[J];
                Z^[I]^[J] := 0.0;
              end;
            Z^[I]^[Mp] := 1.0;
          end;
      end;
  end;

end.
