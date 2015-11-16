{ ******************************************************************
  Non-parametric tests
  ****************************************************************** }

unit unonpar;

interface

uses
  utypes;

procedure Mann_Whitney(N1, N2     : Integer;
                       X1, X2     : PVector;
                       var U, Eps : Float);
{ ------------------------------------------------------------------
  Mann-Whitney test
  ------------------------------------------------------------------ }

procedure Wilcoxon(X, Y       : PVector;
                   Lb, Ub     : Integer;
                   var Ndiff  : Integer;
                   var T, Eps : Float);
{ ------------------------------------------------------------------
  Wilcoxon test
  ------------------------------------------------------------------ }

procedure Kruskal_Wallis(Ns      : Integer;
                         N       : PIntVector;
                         X       : PMatrix;
                         var H   : Float;
                         var DoF : Integer);
{ ------------------------------------------------------------------
  Kruskal-Wallis test
  ------------------------------------------------------------------ }

implementation

procedure Ranks(Ns       : Integer;
                N        : PIntVector;
                X        : PMatrix;
                Sr       : PVector;
                var Corr : Float);
{ ------------------------------------------------------------------
  Compute ranks for non-parametric tests
  ------------------------------------------------------------------
  Sr   = sum of ranks for each sample
  Corr = correction for ties = Sum k(k^2 - 1)  k = nb of ties
  ------------------------------------------------------------------ }

var
  I, J, I1, J1, NI, NE : Integer;
  Y, Z, R              : Float;

begin
  if Ns < 2 then
    begin
      SetErrCode(MatErrDim);
      Exit
    end;

  SetErrCode(MatOk);
  Corr := 0.0;

  for I := 1 to Ns do
    Sr^[I] := 0;

  for I := 1 to Ns do
    for J := 1 to N^[I] do
      begin
        Y := X^[J]^[I];
        NE := 0;                    { Nb of values = Y }
        NI := 0;                    { Nb of values < Y }
        for I1 := 1 to Ns do
          for J1 := 1 to N^[I1] do
            begin
              Z := X^[J1]^[I1];
              if Z < Y then Inc(NI) else if Z = Y then Inc(NE);
            end;
        R := NI + Succ(NE) / 2;     { Mean rank of Y }
        Sr^[I] := Sr^[I] + R;       { Sum of ranks for sample I }
        if NE > 1 then
          Corr := Corr + NE * (Sqr(NE) - 1);
      end;
end;

procedure Mann_Whitney(N1, N2     : Integer;
                       X1, X2     : PVector;
                       var U, Eps : Float);

var
  Nmax, I         : Integer;
  N               : PIntVector;
  X               : PMatrix;
  Sr              : PVector;
  Sum, Prod, Corr : Float;
  U1, U2, MU, VU  : Float;

begin
  if N1 > N2 then Nmax := N1 else Nmax := N2;

  DimIntVector(N, 2);
  DimVector(Sr, 2);
  DimMatrix(X, Nmax, 2);

  N^[1] := N1;
  N^[2] := N2;

  for I := 1 to N1 do     { Copy X1 into first column of X }
    X^[I]^[1] := X1^[I];

  for I := 1 to N2 do     { Copy X2 into second column of X }
    X^[I]^[2] := X2^[I];

  Ranks(2, N, X, Sr, Corr);

  Sum := N1 + N2;
  Prod := N1 * N2;

  U1 := Prod + N1 * (N1 + 1) / 2 - Sr^[1];
  U2 := Prod + N2 * (N2 + 1) / 2 - Sr^[2];

  if U1 > U2 then U := U2 else U := U1;

  MU := Prod / 2;
  VU := Prod * ((Sum + 1) - Corr / Sum / (Sum - 1)) / 12;

  Eps := (U - MU) / Sqrt(VU);

  DelIntVector(N, 2);
  DelVector(Sr, 2);
  DelMatrix(X, Nmax, 2);
end;

procedure Wilcoxon(X, Y       : PVector;
                   Lb, Ub     : Integer;
                   var Ndiff  : Integer;
                   var T, Eps : Float);

var
  J, J1, J2, N       : Integer;
  Diff, MT, VT, Corr : Float;
  D                  : PMatrix;
  ND                 : PIntVector;
  Sr                 : PVector;

begin
  N := Ub - Lb + 1;

  DimMatrix(D, N, 2);
  DimIntVector(ND, 2);
  DimVector(Sr, 2);

  J1 := 0; J2 := 0;
  for J := Lb to Ub do
    begin
      Diff := X^[J] - Y^[J];
      if Diff < 0 then
        begin
          Inc(J1);
          D^[J1]^[1] := Abs(Diff);  { Negative difference }
        end
      else if Diff > 0 then
        begin
          Inc(J2);
          D^[J2]^[2] := Diff;       { Positive difference }
        end;
    end;

  ND^[1] := J1;      { Nb of negative differences }
  ND^[2] := J2;      { Nb of positive differences }
  Ndiff := J1 + J2;  { Nb of non-null differences }

  Ranks(2, ND, D, Sr, Corr);

  if Sr^[1] > Sr^[2] then T := Sr^[2] else T := Sr^[1];

  MT := N * (N + 1) / 4;
  VT := MT * (2 * N + 1) / 6 - Corr / 48;
  Eps := (T - MT) / Sqrt(VT);

  DelMatrix(D, N, 2);
  DelIntVector(ND, 2);
  DelVector(Sr, 2);
end;

procedure Kruskal_Wallis(Ns      : Integer;
                         N       : PIntVector;
                         X       : PMatrix;
                         var H   : Float;
                         var DoF : Integer);

var
  I, NT   : Integer;
  S, Corr : Float;
  Sr      : PVector;

begin
  DimVector(Sr, Ns);

  Ranks(Ns, N, X, Sr, Corr);

  S := 0.0; NT := 0;
  for I := 1 to Ns do
    begin
      S := S + Sqr(Sr^[I]) / N^[I];
      NT := NT + N^[I];
    end;

  H := 12 * S / NT / (NT + 1) - 3 * (NT + 1);
  H := H / (1 - Corr / NT / (Sqr(NT) - 1));
  DoF := Pred(Ns);

  DelVector(Sr, Ns);
end;

end.