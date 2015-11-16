{ ******************************************************************
  This program computes the binomial distribution B(N,P).
  For each value of K (K = 0..N), the probability Prob(X = K)
  is computed from function PBinom. The cumulative probability
  Prob(X <= K) is computed either by direct summation or by a
  call to function FBinom.
  ****************************************************************** }

program Binom;

uses
  tpmath;

const
  N = 10;   { Number of repetitions }
  P = 0.4;  { Probability of success }

var
  K     : Integer;
  PK, S : Float;

begin
  WriteLn;
  WriteLn('Binomial distribution: N = ', N:3, ', P = ', P:6:4);
  WriteLn;
  WriteLn(' K    Prob(K)        Sum     FBinom');
  WriteLn('-----------------------------------');

  S := 0.0;
  for K := 0 to N do
    begin
      PK := PBinom(N, P, K);
      S := S + PK;
      WriteLn(K:2, PK:11:4, S:11:4, FBinom(N, P, K):11:4);
    end;
end.
