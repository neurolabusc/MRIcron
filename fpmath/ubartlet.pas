{ ******************************************************************
  Bartlett's test (comparison of several variances)
  ****************************************************************** }

unit ubartlet;

interface

uses
  utypes;

procedure Bartlett(Ns       : Integer;
                   N        : PIntVector;
                   S        : PVector;
                   var Khi2 : Float;
                   var DoF  : Integer);
{ ------------------------------------------------------------------
  Input parameters : Ns   = number of samples
                     N    = samples sizes
                     S    = samples SD's (computed with StDev)
  Output parameters: Khi2 = Bartlett's khi-2
                     DoF  = degrees of freedom
  ------------------------------------------------------------------ }

implementation

procedure Bartlett(Ns       : Integer;
                   N        : PIntVector;
                   S        : PVector;
                   var Khi2 : Float;
                   var DoF  : Integer);

var
  I, Nt, N1, DoF_r            : Integer;
  SSr, Vr, Vi, SumLog, SumInv : Float;

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

  SSr := 0.0;
  SumLog := 0.0;
  SumInv := 0.0;

  for I := 1 to Ns do
    begin
      N1 := N^[I] - 1;
      Vi := Sqr(S^[I]);
      SSr := SSr + N1 * Vi;
      SumLog := SumLog + N1 * Ln(Vi);
      SumInv := SumInv + 1 / N1;
    end;

  DoF := Ns - 1;
  DoF_r := Nt - Ns;
  Vr := SSr / DoF_r;
  Khi2 := (DoF_r * Ln(Vr) - SumLog) /
          (1.0 + (SumInv - 1.0 / DoF_r) / (3.0 * DoF));
end;

end.