{ ******************************************************************
  Brackets a minimum of a function
  ****************************************************************** }

unit uminbrak;

interface

uses
  utypes, uminmax;

procedure MinBrack(Func : TFunc; var A, B, C, Fa, Fb, Fc : Float);

implementation

procedure MinBrack(Func : TFunc; var A, B, C, Fa, Fb, Fc : Float);
{ ------------------------------------------------------------------
  Given two points (A, B) this procedure finds a triplet (A, B, C)
  such that:

  1) A < B < C
  2) A, B, C are within the golden ratio
  3) Func(B) < Func(A) and Func(B) < Func(C).

  The corresponding function values are returned in Fa, Fb, Fc
  ------------------------------------------------------------------ }

  begin
    if A > B then
      FSwap(A, B);

    Fa := Func(A);
    Fb := Func(B);

    if Fb > Fa then
      begin
        FSwap(A, B);
        FSwap(Fa, Fb);
      end;

    C := B + GOLD * (B - A);
    Fc := Func(C);

    while Fc < Fb do
      begin
        A := B;
        B := C;
        Fa := Fb;
        Fb := Fc;
        C := B + GOLD * (B - A);
        Fc := Func(C);
      end;

    if A > C then
      begin
        FSwap(A, C);
        FSwap(Fa, Fc);
      end;
  end;

end.
