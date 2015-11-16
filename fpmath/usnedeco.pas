{ ******************************************************************
  Snedecor's F-test (comparison of two variances)
  ****************************************************************** }

unit usnedeco;

interface

uses
  utypes;

procedure Snedecor(N1, N2         : Integer;
                   S1, S2         : Float;
                   var F          : Float;
                   var DoF1, DoF2 : Integer);
{ ------------------------------------------------------------------
  Snedecor's F-test (comparison of two variances)
  ------------------------------------------------------------------
  Input parameters : N1, N2     = samples sizes
                     S1, S2     = samples SD's (computed with StDev)
  Output parameters: F          = Snedecor's F
                     DoF1, DoF2 = degrees of freedom
  ------------------------------------------------------------------ }

implementation

procedure Snedecor(N1, N2         : Integer;
                   S1, S2         : Float;
                   var F          : Float;
                   var DoF1, DoF2 : Integer);

var
  V1, V2 : Float;  { Sample variances }

begin
  V1 := Sqr(S1);
  V2 := Sqr(S2);

  if V1 > V2 then
    begin
      F := V1 / V2;
      DoF1 := N1 - 1;
      DoF2 := N2 - 1;
    end
  else
    begin
      F := V2 / V1;
      DoF1 := N2 - 1;
      DoF2 := N1 - 1;
    end;
end;

end.