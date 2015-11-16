{ ******************************************************************
  Woolf test
  ****************************************************************** }

unit uwoolf;

interface

uses
  utypes;

procedure Woolf_Conform(N_cls   : Integer;
                        N_estim : Integer;
                        Obs     : PIntVector;
                        Calc    : PVector;
                        var G   : Float;
                        var DoF : Integer);
{ ------------------------------------------------------------------
  Woolf test for conformity
  ------------------------------------------------------------------ }

procedure Woolf_Indep(N_lin   : Integer;
                      N_col   : Integer;
                      Obs     : PIntMatrix;
                      var G   : Float;
                      var DoF : Integer);
{ ------------------------------------------------------------------
  Woolf test for independence
  ------------------------------------------------------------------ }

implementation

procedure Woolf_Conform(N_cls   : Integer;
                        N_estim : Integer;
                        Obs     : PIntVector;
                        Calc    : PVector;
                        var G   : Float;
                        var DoF : Integer);

var
  I : Integer;

begin
  for I := 1 to N_cls do
    if (Obs^[I] <= 0) or (Calc^[I] <= 0.0) then
      begin
        SetErrCode(FSing);
        Exit
      end;

  SetErrCode(FOk);

  G := 0.0;
  for I := 1 to N_cls do
    G := G + Obs^[I] * Ln(Obs^[I] / Calc^[I]);

  G := 2.0 * G;
  DoF := N_cls - N_estim - 1;
end;

procedure Woolf_Indep(N_lin   : Integer;
                      N_col   : Integer;
                      Obs     : PIntMatrix;
                      var G   : Float;
                      var DoF : Integer);

var
  SumLin, SumCol : PIntVector;
  Sum            : Integer;
  Prob, Calc     : Float;
  I, J           : Integer;

begin
  for I := 1 to N_lin do
    for J := 1 to N_col do
      if Obs^[I]^[J] <= 0 then
        begin
          SetErrCode(FSing);
          Exit
        end;

  SetErrCode(FOk);

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

  G := 0.0;
  for I := 1 to N_lin do
    begin
      Prob := SumLin^[I] / Sum;
      for J := 1 to N_col do
        begin
          Calc := SumCol^[J] * Prob;
          G := G + Obs^[I]^[J] * Ln(Obs^[I]^[J] / Calc);
        end;
    end;

  G := 2.0 * G;
  DoF := Pred(N_lin) * Pred(N_col);

  DelIntVector(SumLin, N_lin);
  DelIntVector(SumCol, N_col);
end;

end.