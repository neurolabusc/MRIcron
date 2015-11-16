{ ******************************************************************
  Statistical distribution
  ****************************************************************** }

unit udistrib;

interface

uses
  utypes;

procedure DimStatClassVector(var C : PStatClassVector; Ub : Integer);
{ ------------------------------------------------------------------
  Allocates an array of statistical classes: C[0..Ub]
  ------------------------------------------------------------------ }

procedure DelStatClassVector(var C : PStatClassVector; Ub : Integer);
{ ------------------------------------------------------------------
  Deallocates an array of statistical classes: C[0..Ub]
  ------------------------------------------------------------------ }

procedure Distrib(X       : PVector;
                  Lb, Ub  : Integer;
                  A, B, H : Float;
                  C       : PStatClassVector);
{ ------------------------------------------------------------------
  Distributes the values of array X[Lb..Ub] into M classes with
  equal width H, according to the following scheme:

              C[1]    C[2]                    C[M]
           ]-------]-------].......]-------]-------]
           A      A+H     A+2H                     B

  such that B = A + M * H
  ------------------------------------------------------------------ }

implementation

procedure DimStatClassVector(var C : PStatClassVector; Ub : Integer);
var
  I : Integer;
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MAX_CLS) then
    begin
      C := nil;
      Exit;
    end;

  { Allocate vector }
  GetMem(C, (Ub + 1) * SizeOf(StatClass));
  if C = nil then Exit;

  { Initialize vector }
  for I := 0 to Ub do
    with C^[I] do
      begin
        Inf := 0.0;
        Sup := 0.0;
        N   := 0;
        F   := 0.0;
        D   := 0.0;
      end;
end;

procedure DelStatClassVector(var C : PStatClassVector; Ub : Integer);
begin
  if C <> nil then
    begin
      FreeMem(C, (Ub + 1) * SizeOf(StatClass));
      C := nil;
    end;
end;

function NumCls(X, A, H : Float) : Integer;
{ Returns the index of the class containing X
  A is the lower bound of the first class
  H is the class width }
var
  Y : Float;
  I : Integer;
begin
  Y := (X - A) / H;
  I := Trunc(Y);
  if Y <> I then Inc(I);
  NumCls := I;
end;

procedure Distrib(X       : PVector;
                  Lb, Ub  : Integer;
                  A, B, H : Float;
                  C       : PStatClassVector);
var
  I, K, M, Nt : Integer;
begin
  M := Round((B - A) / H);

  for K := 1 to M do
    C^[K].N := 0;

  for I := Lb to Ub do
    begin
      K := NumCls(X^[I], A, H);
      Inc(C^[K].N);
    end;

  Nt := Ub - Lb + 1;

  for K := 1 to M do
    with C^[K] do
      begin
        Inf := A + (K - 1) * H;
        Sup := Inf + H;
        F := N / Nt;
        D := F / H;
      end;
end;

end.