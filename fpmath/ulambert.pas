{ ******************************************************************
  Lambert's function
  Translated from Fortran code by Barry et al.
  (http://www.netlib.org/toms/743)
  ****************************************************************** }

unit ulambert;

interface

uses
  utypes, umath;

function LambertW(X : Float; UBranch, Offset : Boolean) : Float;
{ ----------------------------------------------------------------------
  Lambert's W function: Y = W(X) ==> X = Y * Exp(Y)    X >= -1/e
  ----------------------------------------------------------------------
  X       = Argument
  UBranch = TRUE  for computing the upper branch (X >= -1/e, W(X) >= -1)
            FALSE for computing the lower branch (-1/e <= X < 0, W(X) <= -1)
  Offset  = TRUE  for computing W(X - 1/e), X >= 0
            FALSE for computing W(X)
  ---------------------------------------------------------------------- }

implementation

{$IFDEF SINGLEREAL}
const
  NBITS = 23;                       { MachEp = 2^(-NBITS) }
  X0    =  0.03507693900966790567;  { MachEp^(1/6) / 2 }
  X1    = -0.30212011943278473033;  { - Exp(-1) * (1 - 17 * MachEp^(2/7) }
{$ELSE}
{$IFDEF EXTENDEDREAL}
const
  NBITS = 63;
  X0    =  0.0003452669830012439084;
  X1    = -0.36785558424357094358;
{$ELSE}
const
  NBITS = 52;
  X0    =  0.001230391650287962075;
  X1    = -0.36766871970031223379;
{$ENDIF}
{$ENDIF}

const
  EM  = -0.36787944117144232160;    { - Exp(-1) }
  EM9 = -0.0001234098040866795495;  { - Exp(-9) }
  C13 = 1.0 / 3.0;
  C23 = 2.0 * C13;
  EM2 = 2.0 / EM;
  D12 = - EM2;
  AN3 = 8.0 / 3.0;
  AN4 = 135.0 / 83.0;
  AN5 = 166.0 / 39.0;
  AN6 = 3167.0 / 3549.0;
  S21 = 2.0 * Sqrt2 - 3.0;
  S22 = 4.0 - 3.0 * Sqrt2;
  S23 = Sqrt2 - 2.0;


  function LambertW(X : Float; UBranch, Offset : Boolean) : Float;
  var
    I, NITER : Integer;
    AN2, DELX, ETA, RETA, T, TEMP, TEMP2, TS, WAPR, XX, ZL, ZN : Float;

  begin
    SetErrCode(FOk);

    if Offset then
      begin
        DELX := X;
        if DELX < 0.0 then
          begin
            LambertW := DefaultVal(FDomain, 0.0);
            Exit;
          end;
        XX := X + EM;
      end
    else
      begin
        if X < EM then
          begin
            LambertW := DefaultVal(FDomain, 0.0);
            Exit;
          end;

        if X = EM then
          begin
            LambertW := - 1.0;
            Exit;
          end;

        XX := X;
        DELX := XX - EM;
      end;

    if UBranch then
      begin
        if Abs(XX) <= X0 then
          begin
            LambertW := XX / (1.0 + XX / (1.0 + XX / (2.0 + XX / (0.6 + 0.34 * XX))));
            Exit;
          end;

        if XX <= X1 then
          begin
            RETA := Sqrt(D12 * DELX);
            LambertW := RETA / (1.0 + RETA / (3.0 + RETA / (RETA / (AN4 +
                        RETA / (RETA * AN6 + AN5)) + AN3))) - 1.0;
            Exit;
          end;

        if XX <= 20.0 then
          begin
            RETA := Sqrt2 * Sqrt(1.0 - XX / EM);
            AN2 := 4.612634277343749 * Sqrt(Sqrt(RETA + 1.09556884765625));
            WAPR := RETA / (1.0 + RETA / (3.0 + (S21 * AN2 + S22) * RETA / (S23 * (AN2 + RETA)))) - 1.0;
          end
        else
          begin
            ZL := Ln(XX);
            WAPR := Ln(XX / Ln(XX / Power(ZL, Exp(- 1.124491989777808 / (0.4225028202459761 + ZL)))));
          end
      end
    else
      begin
        if XX >= 0.0 then
          begin
            LambertW := DefaultVal(FDomain, 0.0);
            Exit;
          end;

        if XX <= X1 then
          begin
            RETA := Sqrt(D12 * DELX);
            LambertW := RETA / (RETA / (3.0 + RETA / (RETA / (AN4 +
                        RETA / (RETA * AN6 - AN5)) - AN3)) - 1.0) - 1.0;
            Exit;
          end;

        ZL := Ln(- XX);

        if XX <= EM9 then
          begin
            T := - 1.0 - ZL;
            TS := Sqrt(T);
            WAPR := ZL - (2.0 * TS) / (SQRT2 + (C13 - T / (2.7E2 + TS * 127.0471381349219)) * TS);
          end
        else
          begin
            ETA := 2.0 - EM2 * XX;
            WAPR := Ln(XX / Ln(- XX / ((1.0 - 0.5043921323068457 * (ZL + 1.0)) * (Sqrt(ETA) + ETA / 3.0) + 1.0)));
          end
      end;

    if NBITS < 56 then NITER := 1 else NITER := 2;

    for I := 1 to NITER do
      begin
        ZN := Ln(XX / WAPR) - WAPR;
        TEMP := 1.0 + WAPR;
        TEMP2 := TEMP + C23 * ZN;
        TEMP2 := 2.0 * TEMP * TEMP2;
        WAPR := WAPR * (1.0 + (ZN / TEMP) * (TEMP2 - ZN) / (TEMP2 - 2.0 * ZN));
      end;

    LambertW := WAPR;
  end;

end.
