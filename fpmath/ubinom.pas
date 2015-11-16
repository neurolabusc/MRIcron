{ ******************************************************************
  Binomial distribution
  ****************************************************************** }

unit ubinom; 

interface

uses
  utypes, umath;

function Binomial(N, K : Integer) : Float;
{ Binomial coefficient C(N,K) }

function PBinom(N : Integer; P : Float; K : Integer) : Float;
{ Probability of binomial distribution }

implementation

  function Binomial(N, K : Integer) : Float;
  var
    I, N1 : Integer;
    Prod : Float;
  begin
    SetErrCode(FOk);
    if K < 0 then
      Binomial := 0.0
    else if (K = 0) or (K = N) then
      Binomial := 1.0
    else if (K = 1) or (K = N - 1) then
      Binomial := N
    else
      begin
        if K > N - K then K := N - K;
        N1 := Succ(N);
        Prod := N;
        for I := 2 to K do
          Prod := Prod * ((N1 - I) / I);
        Binomial := Int(0.5 + Prod);
      end;
  end;

  function PBinom(N : Integer; P : Float; K : Integer) : Float;
  begin
    SetErrCode(FOk);
    if (P < 0.0) or (P > 1.0) or (N <= 0) or (N < K) then
      PBinom := DefaultVal(FDomain, 0.0)
    else if K = 0 then
      PBinom := Power(1.0 - P, N)
    else if K = N then
      PBinom := Power(P, N)
    else
      PBinom := Binomial(N, K) * Power(P, K) * Power(1.0 - P, N - K);
  end;

end.