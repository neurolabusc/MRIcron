{ ******************************************************************
  Quick sort
  ****************************************************************** }

unit uqsort;

interface

uses
  utypes;

procedure QSort(X : PVector; Lb, Ub : Integer);
{ ------------------------------------------------------------------
  Sorts the elements of vector X in increasing order (quick sort)
  ------------------------------------------------------------------ }

procedure DQSort(X : PVector; Lb, Ub : Integer);
{ ------------------------------------------------------------------
  Sorts the elements of vector X in decreasing order (quick sort)
  ------------------------------------------------------------------ }

implementation

procedure QSort(X : PVector; Lb, Ub : Integer);
{ Quick sort in ascending order - Adapted from Borland's BP7 demo }

    procedure Sort(L, R : Integer);
    var
      I, J : Integer;
      U, V : Float;
    begin
      I := L;
      J := R;
      U := X^[(L + R) div 2];
      repeat
        while X^[I] < U do I := I + 1;
        while U < X^[J] do J := J - 1;
        if I <= J then
          begin
            V := X^[I]; X^[I] := X^[J]; X^[J] := V;
            I := I + 1; J := J - 1;
          end;
      until I > J;
      if L < J then Sort(L, J);
      if I < R then Sort(I, R);
    end;

begin
  Sort(Lb, Ub);
end;

procedure DQSort(X : PVector; Lb, Ub : Integer);
{ Quick sort in descending order - Adapted from Borland's BP7 demo }

    procedure Sort(L, R : Integer);
    var
      I, J : Integer;
      U, V : Float;
    begin
      I := L;
      J := R;
      U := X^[(L + R) div 2];
      repeat
        while X^[I] > U do I := I + 1;
        while U > X^[J] do J := J - 1;
        if I <= J then
          begin
            V := X^[I]; X^[I] := X^[J]; X^[J] := V;
            I := I + 1; J := J - 1;
          end;
      until I > J;
      if L < J then Sort(L, J);
      if I < R then Sort(I, R);
    end;

begin
  Sort(Lb, Ub);
end;

end.

