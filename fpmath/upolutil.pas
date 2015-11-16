{ ******************************************************************
  Utility functions to handle roots of polynomials
  ****************************************************************** }

unit upolutil;

interface

uses
  utypes, uminmax;

function SetRealRoots(Deg : Integer; Z : PCompVector; Tol : Float) : Integer;
{ ------------------------------------------------------------------
  Set the imaginary part of a root to zero if it is less than a
  fraction Tol of its real part. This root is therefore considered
  real. The function returns the total number of real roots.
  ------------------------------------------------------------------ }

procedure SortRoots(Deg : Integer; Z : PCompVector);
{ ------------------------------------------------------------------
  Sort roots so that:

  (1) The Nr real roots are stored in elements [1..Nr] of vector Z,
      in increasing order.

  (2) The complex roots are stored in elements [(Nr + 1)..Deg] of
      vector Z and are unordered.
  ------------------------------------------------------------------ }

implementation

function SetRealRoots(Deg : Integer; Z : PCompVector; Tol : Float) : Integer;
var
  I, N : Integer;
begin
  for I := 1 to Deg do
    if (Z^[I].Y <> 0.0) and (Abs(Z^[I].Y) < Tol * Abs(Z^[I].X)) then
      Z^[I].Y := 0.0;

  { Count real roots }
  N := 0;
  for I := 1 to Deg do
    if Z^[I].Y = 0.0 then
      Inc(N);

  SetRealRoots := N;
end;

procedure SortRoots(Deg : Integer; Z : PCompVector);
var
  I, J, K, Nr, Nc : Integer;
  R, X, Y         : PVector;

  procedure Sort(X : PVector; N : Integer);
  { Sort vector X (insertion sort) }
  var
    I, J, K : Integer;
    A       : Float;
  begin
    for I := 1 to Pred(N) do
      begin
        K := I;
        A := X^[I];
        for J := Succ(I) to N do
          if X^[J] < A then
            begin
              K := J;
              A := X^[J];
            end;
        FSwap(X^[I], X^[K]);
      end;
  end;

begin
  { Count real and complex roots }
  Nr := 0; Nc := 0;
  for I := 1 to Deg do
    if Z^[I].Y = 0.0 then Inc(Nr) else Inc(Nc);

  DimVector(R, Nr);
  DimVector(X, Nc);
  DimVector(Y, Nc);

  { Store real roots in R and complex roots in (X,Y) }
  J := 0; K := 0;
  for I := 1 to Deg do
    if Z^[I].Y = 0.0 then
      begin
        Inc(J);
        R^[J] := Z^[I].X;
      end
    else
      begin
        Inc(K);
        X^[K] := Z^[I].X;
        Y^[K] := Z^[I].Y;
      end;

  { Sort vector R (insertion sort) }
  if Nr > 0 then Sort(R, Nr);

  { Transfer real roots into elements 1..Nr }
  for I := 1 to Nr do
    begin
      Z^[I].X := R^[I];
      Z^[I].Y := 0.0;
    end;

  { Transfer complex roots into elements (Nr+1)..Deg }
  for I := 1 to Nc do
    begin
      J := I + Nr;
      Z^[J].X := X^[I];
      Z^[J].Y := Y^[I];
    end;

  DelVector(R, Nr);
  DelVector(X, Nc);
  DelVector(Y, Nc);
end;

end.
