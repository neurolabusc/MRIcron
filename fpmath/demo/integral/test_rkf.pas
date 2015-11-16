{ ******************************************************************
   Integrate a System of Ordinary Differential Equations By the
   Runge-Kutta-Fehlberg method (double precision)
   -----------------------------------------------------------------
   REFERENCE:     H A Watts and L F Shampine,
                  Sandia Laboratories,
                  Albuquerque, New Mexico.
   -----------------------------------------------------------------

   Basic Release 1.1 By J-P Moreau, Paris.
   TPMath adaptation by J. Debord

   Release 1.1: added test #3.
  ****************************************************************** }

program test_rkf;

uses
  crt, tpmath;

procedure DiffEq1(T : Float; Y, Yp : PVector);
{ Differential equation for Test 1 }
begin
  Yp^[1] := 0.25 * Y^[1] * (1 - Y^[1] / 20);
end;

function Yexact1(T : Float) : Float;
{ Exact solution of the ODE for Test 1 }
begin
  Yexact1 := 20 / (1 + 19 * Exp(- 0.25 * T));
end;

procedure DiffEq2(T : Float; Y, Yp : PVector);
{ Differential equations for Test 2 }
begin
  Yp^[1] := Y^[2];
  Yp^[2] := - Y^[1];
end;

procedure Yexact2(T : Float; var Y1, Y2 : Float);
{ Exact solution of the ODE's for Test 2 }
begin
  Y1 := Cos(T);
  Y2 := - Sin(T);
end;

procedure DiffEq3(T : Float; Y, Yp : PVector);
{ Differential equations for Test 3 }
begin
  Yp^[1] := Y^[2];
  Yp^[2] := Y^[3];
  Yp^[3] := Y^[4];
  Yp^[4] := Y^[5];
  Yp^[5] := (45 * Y^[3] * Y^[4] * Y^[5] - 40 * Y^[4] * Sqr(Y^[4])) / (9 * Sqr(Y^[3]));
end;

var                          { Global variables used by all test procedures }
  Neqn           : Integer;  { Number of equations }
  Y, Yp          : PVector;  { Functions and derivatives }
  Tstart, Tstop  : Float;    { Integration interval }
  Nstep          : Integer;  { Number of steps }
  StepSize       : Float;    { Step size }
  AbsErr, RelErr : Float;    { Abs. and relative errors }
  Flag           : Integer;  { Error flag }
  T, Tout        : Float;    { Integration times }
  I              : Integer;  { Loop variable }

procedure Test1;
var
  Yc : Float;  { Exact solution }
begin
  Neqn := 1;

  DimVector(Y, Neqn);
  DimVector(Yp, Neqn);

  Y^[1] := 1;  { Initial condition }

  Tstart := 0;
  Tstop  := 20;
  Nstep  := 5;

  StepSize := (Tstop - Tstart) / Nstep;

  AbsErr := 1.0E-6;
  RelErr := 1.0E-6;

  Flag := 1;

  T := Tstart;

  WriteLn;
  WriteLn('TEST01');
  WriteLn('Solve a scalar equation:');
  WriteLn;
  WriteLn(' Y'' = 0.25 * Y * ( 1 - Y / 20 )');
  WriteLn;

  WriteLn(' T           Y            Y exact         Error');
  WriteLn;

  Yc := Yexact1(T);

  WriteLn(T:5:2, Y^[1]:14:4, Yc:14:4, (Y^[1] - Yc):14:4);

  for I := 1 to Nstep do
  begin
    Tout := T + StepSize;

    {$IFDEF FPC}
    RKF45(@DiffEq1, Neqn, Y, Yp, T, Tout, RelErr, AbsErr, Flag);
    {$ELSE}
    RKF45(DiffEq1, Neqn, Y, Yp, T, Tout, RelErr, AbsErr, Flag);
    {$ENDIF}

    Yc := Yexact1(Tout);

    WriteLn(T:5:2, Y^[1]:14:4, Yc:14:4, (Y^[1] - Yc):14:4);

    T := Tout;
  end;

  DelVector(Y, Neqn);
  DelVector(Yp, Neqn);
end;

procedure Test2;
var
  Yc1, Yc2 : Float;  { Exact solution }
begin
  Neqn := 2;

  DimVector(Y, Neqn);
  DimVector(Yp, Neqn);

  Y^[1] := 1;  { Initial conditions }
  Y^[2] := 0;

  Tstart := 0;
  Tstop  := 2 * Pi;
  Nstep  := 12;

  StepSize := (Tstop - Tstart) / Nstep;

  AbsErr := 1.0E-6;
  RelErr := 1.0E-6;

  Flag := 1;

  T := Tstart;

  WriteLn;
  WriteLn('TEST02');
  WriteLn('Solve a vector equation:');
  WriteLn;
  WriteLn(' Y''(1) =   Y(2)');
  WriteLn(' Y''(2) = - Y(1)');
  WriteLn;

  WriteLn(' T           Y1          Y1 exact        Y2          Y2 exact');
  WriteLn;

  Yexact2(T, Yc1, Yc2);

  WriteLn(T:5:2, Y^[1]:14:4, Yc1:14:4, Y^[2]:14:4, Yc2:14:4);

  for I := 1 to Nstep do
  begin
    Tout := T + StepSize;

    {$IFDEF FPC}
    RKF45(@DiffEq2, Neqn, Y, Yp, T, Tout, RelErr, AbsErr, Flag);
    {$ELSE}
    RKF45(DiffEq2, Neqn, Y, Yp, T, Tout, RelErr, AbsErr, Flag);
    {$ENDIF}

    Yexact2(Tout, Yc1, Yc2);

    WriteLn(T:5:2, Y^[1]:14:4, Yc1:14:4, Y^[2]:14:4, Yc2:14:4);

    T := Tout;
  end;

  DelVector(Y, Neqn);
  DelVector(Yp, Neqn);
end;

procedure Test3;
begin
  Neqn := 5;

  DimVector(Y, Neqn);
  DimVector(Yp, Neqn);

  Y^[1] := 1;  { Initial conditions }
  Y^[2] := 1;
  Y^[3] := 1;
  Y^[4] := 1;
  Y^[5] := 1;

  Tstart := 0;
  Tstop  := 1.5;
  Nstep  := 11;

  StepSize := (Tstop - Tstart) / Nstep;

  AbsErr := 1.0E-6;
  RelErr := 1.0E-6;

  Flag := 1;

  T := Tstart;

  WriteLn;
  WriteLn('TEST03');
  WriteLn('Solve a vector equation:');
  WriteLn;
  WriteLn(' Y''(1) = Y(2)');
  WriteLn(' Y''(2) = Y(3)');
  WriteLn(' Y''(3) = Y(4)');
  WriteLn(' Y''(4) = Y(5)');
  WriteLn(' Y''(5) = (45 * Y(3) * Y(4) * Y(5) - 40 * Y(4)^3) / (9 * Y(3)^2)');

  WriteLn;
  WriteLn(' T           Y1            Y2            Y3            Y4            Y5');
  WriteLn;

  WriteLn(T:5:2, Y^[1]:14:4, Y^[2]:14:4, Y^[3]:14:4, Y^[4]:14:4, Y^[5]:14:4);

  for I := 1 to Nstep do
  begin
    Tout := T + StepSize;

    {$IFDEF FPC}
    RKF45(@DiffEq3, Neqn, Y, Yp, T, Tout, RelErr, AbsErr, Flag);
    {$ELSE}
    RKF45(DiffEq3, Neqn, Y, Yp, T, Tout, RelErr, AbsErr, Flag);
    {$ENDIF}

    WriteLn(T:5:2, Y^[1]:14:4, Y^[2]:14:4, Y^[3]:14:4, Y^[4]:14:4, Y^[5]:14:4);

    T := Tout;
  end;

  DelVector(Y, Neqn);
  DelVector(Yp, Neqn);
end;

procedure Pause;
var
  Ch : Char;
begin
  Writeln;
  Write('Press a key to continue');
  Ch := ReadKey;
  Writeln;
  Writeln;
end;

begin
  WriteLn;
  WriteLn('PROGRAM TEST_RKF');
  WriteLn('Demonstrate the RKF45 ODE integrator.');

  Test1;
  Pause;

  Test2;
  Pause;

  Test3;
  Pause;
end.
