{ ******************************************************************
  Khi-2 and Woolf's tests
  ****************************************************************** }

program khi2;

uses
  tpmath;

const
  Alpha = 0.05;  { Significance level }

var
  K2, K2c, G : Float;
  DoF        : Integer;

{ ------------------------------------------------------------------
  Data for the conformity test
  ------------------------------------------------------------------ }

const
  N = 4;

  Obs  : array[1..N] of Integer = (104, 76, 18, 2);
  Calc : array[1..N] of Float   = ( 94, 86, 14, 6);

{ ------------------------------------------------------------------
  Data for the independence test
  ------------------------------------------------------------------ }

const
  Nr = 2; Nc = 3;  { Number of rows and columns }

  T : array[1..Nr, 1..Nc] of Integer =  { Contingency table }
  ((280, 210, 110),
   (220,  90,  90));

{ ------------------------------------------------------------------
  Procedures for reading data
  ------------------------------------------------------------------ }

procedure GetDataConf(O : PIntVector; C : PVector);
{ Get data for conformity test }
var
  I : Integer;
begin
  for I := 1 to N do
    begin
      O^[I] := Obs[I];
      C^[I] := Calc[I];
    end;
end;

procedure GetDataInd(A : PIntMatrix);
{ Get data for independence test }
var
  I, J : Integer;
begin
  for I := 1 to Nr do
    for J := 1 to Nc do
      A^[I]^[J] := T[I,J];
end;

{ ------------------------------------------------------------------
  Main program
  ------------------------------------------------------------------ }

var
  O : PIntVector;
  C : PVector;
  A : PIntMatrix;

begin
  DimIntVector(O, N);
  DimVector(C, N);
  DimIntMatrix(A, Nr, Nc);

  GetDataConf(O, C);
  GetDataInd(A);

  { Tests for conformity }

  Khi2_Conform(N, 0, O, C, K2, DoF);
  Woolf_Conform(N, 0, O, C, G, DoF);

  K2c := InvKhi2(DoF, 1.0 - Alpha);

  WriteLn('Comparison of two distributions:');

  WriteLn;

  WriteLn('Pearson''s Khi-2            = ', K2:10:4, '  (', DoF, ' DoF)');
  WriteLn('Woolf''s G                  = ', G:10:4, '  (', DoF, ' DoF)');
  WriteLn('Critical value (p = ', Alpha:5:3, ') = ', K2c:10:4);

  WriteLn;
  WriteLn;

 { Tests for independence }

  Khi2_Indep(Nr, Nc, A, K2, DoF);
  Woolf_Indep(Nr, Nc, A, G, DoF);

  K2c := InvKhi2(DoF, 1.0 - Alpha);

  WriteLn('Analysis of contingency table:');
  WriteLn;

  WriteLn('Pearson''s Khi-2            = ', K2:10:4, '  (', DoF, ' DoF)');
  WriteLn('Woolf''s G                  = ', G:10:4, '  (', DoF, ' DoF)');
  WriteLn('Critical value (p = ', Alpha:5:3, ') = ', K2c:10:4);
end.