{ ******************************************************************
  Student t-test for independent samples
  ****************************************************************** }

unit ustudind;

interface

uses
  utypes;

procedure StudIndep(N1, N2         : Integer;
                    M1, M2, S1, S2 : Float;
                    var T          : Float;
                    var DoF        : Integer);
{ ------------------------------------------------------------------
  Student t-test for independent samples
  ------------------------------------------------------------------
  Input parameters : N1, N2 = samples sizes
                     M1, M2 = samples means
                     S1, S2 = samples SD's (computed with StDev)
  Output parameters: T      = Student's t
                     DoF    = degrees of freedom
  ------------------------------------------------------------------ }

implementation

procedure StudIndep(N1, N2         : Integer;
                    M1, M2, S1, S2 : Float;
                    var T          : Float;
                    var DoF        : Integer);

var
  V1, V2 : Float;  { Sample variances }
  VarCom : Float;  { Estimate of common variance }

begin
  V1 := Sqr(S1);
  V2 := Sqr(S2);

  DoF := N1 + N2 - 2;

  if (N1 >= 30) and (N2 >= 30) then
    T := (M1 - M2) / Sqrt(V1 / N1 + V2 / N2)
  else
    begin
      VarCom := ((N1 - 1) * V1 + (N2 - 1) * V2) / DoF;
      T := (M1 - M2) / Sqrt(VarCom / N1 + VarCom / N2);
    end;
end;

end.