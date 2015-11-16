{ ******************************************************************
  Comparison of two vectors
  ****************************************************************** }

unit ucompvec;

interface

uses
  utypes;

function CompVec(X, Xref : PVector;
                 Lb, Ub  : Integer;
                 Tol     : Float) : Boolean;
{ ------------------------------------------------------------------
  Checks if each component of vector X is within a fraction Tol of
  the corresponding component of the reference vector Xref. In this
  case, the function returns True, otherwise it returns False
  ------------------------------------------------------------------ }

implementation

function CompVec(X, Xref : PVector;
                 Lb, Ub  : Integer;
                 Tol     : Float) : Boolean;
var
  I    : Integer;
  Ok   : Boolean;
  ITol : Float;

begin
  I := Lb;
  Ok := True;

  repeat
    ITol := Tol * Abs(Xref^[I]);
    if ITol < MachEp then ITol := MachEp;
    Ok := Ok and (Abs(X^[I] - Xref^[I]) < ITol);
    I := I + 1;
  until (not Ok) or (I > Ub);

  CompVec := Ok;
end;

end.