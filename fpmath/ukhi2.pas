{ ******************************************************************
  Khi-2 test
  ****************************************************************** }

unit ukhi2;

interface

uses
  utypes;

procedure Khi2_Conform(N_cls    : Integer;
                       N_estim  : Integer;
                       Obs      : PIntVector;
                       Calc     : PVector;
                       var Khi2 : Float;
                       var DoF  : Integer);
{ ------------------------------------------------------------------
  Khi-2 test for conformity
  ------------------------------------------------------------------ }

procedure Khi2_Indep(N_lin    : Integer;
                     N_col    : Integer;
                     Obs      : PIntMatrix;
                     var Khi2 : Float;
                     var DoF  : Integer);
{ ------------------------------------------------------------------
  Khi-2 test for independence
  ------------------------------------------------------------------ }

implementation

procedure Khi2_Conform(N_cls    : Integer;
                       N_estim  : Integer;
                       Obs      : PIntVector;
                       Calc     : PVector;
                       var Khi2 : Float;
                       var DoF  : Integer);

var
  I : Integer;

begin
  Khi2 := 0.0;

  for I := 1 to N_cls do
    Khi2 := Khi2 + Sqr(Obs^[I] - Calc^[I]) / Calc^[I];

  DoF := N_cls - N_estim - 1;
end;

procedure Khi2_Indep(N_lin    : Integer;
                     N_col    : Integer;
                     Obs      : PIntMatrix;
                     var Khi2 : Float;
                     var DoF  : Integer);

var
  SumLin, SumCol : PIntVector;
  Sum            : Integer;
  Prob, Calc     : Float;
  I, J           : Integer;

begin
  DimIntVector(SumLin, N_lin);
  DimIntVector(SumCol, N_col);

  for I := 1 to N_lin do
    for J := 1 to N_col do
      SumLin^[I] := SumLin^[I] + Obs^[I]^[J];

  for J := 1 to N_col do
    for I := 1 to N_lin do
      SumCol^[J] := SumCol^[J] + Obs^[I]^[J];

  Sum := 0;
  for I := 1 to N_lin do
    Sum := Sum + SumLin^[I];

  Khi2 := 0.0;
  for I := 1 to N_lin do
    begin
      Prob := SumLin^[I] / Sum;
      for J := 1 to N_col do
        begin
          Calc := SumCol^[J] * Prob;
          Khi2 := Khi2 + Sqr(Obs^[I]^[J] - Calc) / Calc;
        end;
    end;

  DoF := Pred(N_lin) * Pred(N_col);

  DelIntVector(SumLin, N_lin);
  DelIntVector(SumCol, N_col);
end;

end.