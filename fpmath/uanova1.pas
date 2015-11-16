{ ******************************************************************
  One-way analysis of variance
  ****************************************************************** }

unit uanova1;

interface

uses
  utypes;

procedure AnOVa1(Ns               : Integer;
                 N                : PIntVector;
                 M, S             : PVector;
                 var V_f, V_r, F  : Float;
                 var DoF_f, DoF_r : Integer);
{ ------------------------------------------------------------------
  Input parameters : Ns           = number of samples
                     N            = samples sizes
                     M            = samples means
                     S            = samples SD's (computed with StDev)
  Output parameters: V_f, V_r     = variances (factorial, residual)
                     F            = ratio Vf / Vr
                     DoF_f, DoF_r = degrees of freedom
  ------------------------------------------------------------------ }

implementation

procedure AnOVa1(Ns               : Integer;
                 N                : PIntVector;
                 M, S             : PVector;
                 var V_f, V_r, F  : Float;
                 var DoF_f, DoF_r : Integer);
var
  I, Nt    : Integer;
  Xbar     : Float;    { Global mean }
  SSf, SSr : Float;    { Sum of squares }
  D        : Float;    { Difference of means }

begin
  if Ns < 2 then
    begin
      SetErrCode(MatErrDim);
      Exit
    end;

  Nt := 0;
  for I := 1 to Ns do
    Nt := Nt + N^[I];

  if Nt <= Ns then
    begin
      SetErrCode(MatErrDim);
      Exit;
    end;

  SetErrCode(MatOk);

  Xbar := 0.0;
  for I := 1 to Ns do
    Xbar := Xbar + N^[I] * M^[I];

  Xbar := Xbar / Nt;

  SSf := 0.0;
  SSr := 0.0;
  for I := 1 to Ns do
    begin
      D := M^[I] - Xbar;
      SSf := SSf + N^[I] * Sqr(D);
      SSr := SSr + (N^[I] - 1) * Sqr(S^[I]);
    end;

  DoF_f := Ns - 1;
  DoF_r := Nt - Ns;
  V_f := SSf / DoF_f;
  V_r := SSr / DoF_r;
  F := V_f / V_r;
end;

end.