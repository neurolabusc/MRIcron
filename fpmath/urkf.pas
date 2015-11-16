{ ******************************************************************
  Numerical integration of a system of differential equations
  by the Runge-Kutta-Fehlberg (RKF) method.

  Adapted from a Fortran-90 program available at:
  http://www.csit.fsu.edu/~burkardt/f_src/rkf45/rkf45.f90
  ****************************************************************** }

unit urkf;

interface

uses
  utypes, uminmax;

procedure RKF45(F                    : TDiffEqs;
                Neqn                 : Integer;
                Y, Yp                : PVector;
                var T                : Float;
                Tout, RelErr, AbsErr : Float;
                var Flag             : Integer);

implementation

const
  maxeqn      : Integer =  0;
  flag_save   : Integer = -1000;
  init        : Integer = -1000;
  kflag       : Integer = -1000;
  kop         : Integer = -1;
  nfe         : Integer = -1;
  relerr_save : Float   = -1.0;
  abserr_save : Float   = -1.0;
  h           : Float   = -1.0;
  f1          : PVector = nil;
  f2          : PVector = nil;
  f3          : PVector = nil;
  f4          : PVector = nil;
  f5          : PVector = nil;

procedure Fehl(F                         : TDiffEqs;
               Neqn                      : Integer;
               Y                         : PVector;
               T, H                      : Float;
               Yp, F1, F2, F3, F4, F5, S : PVector);
{ ------------------------------------------------------------------

   Fehl takes one Fehlberg fourth-fifth order step (double precision).

   Discussion:

     This routine integrates a system of Neqn first order ordinary
     differential equations of the form
       dY(i)/dT = F(T,Y(1:Neqn))
     where the initial values Y and the initial derivatives
     YP are specified at the starting point T.

     The routine advances the solution over the fixed step H and returns
     the fifth order (sixth order accurate locally) solution
     approximation at T+H in array S.

     The formulas have been grouped to control loss of significance.
     The routine should be called with an H not smaller than 13 units of
     roundoff in T so that the various independent arguments can be
     distinguished.

   Modified:

     27 March 2004

   Author:

     H A Watts and L F Shampine,
     Sandia Laboratories,
     Albuquerque, New Mexico.

   Reference:

     E. Fehlberg,
     Low-order Classical Runge-Kutta Formulas with Stepsize Control,
     NASA Technical Report R-315.

     L F Shampine, H A Watts, S Davenport,
     Solving Non-stiff Ordinary Differential Equations - The State of the Art,
     SIAM Review,
     Volume 18, pages 376-411, 1976.

   Parameters:

     Input, external F, a user-supplied subroutine to evaluate the
     derivatives Y'(T), of the form:

       procedure(X : Float; Y, D : PVector);

     Input, Neqn, the number of equations to be integrated.

     Input, Y(Neqn), the current value of the
     dependent variable.

     Input, T, the current value of the independent
     variable.

     Input, H, the step size to take.

     Input, YP(Neqn), the current value of the
     derivative of the dependent variable.

     Output, F1(Neqn), F2(Neqn), F3(Neqn), F4(Neqn), F5(Neqn),
     derivative values needed for the computation.

     Output, S(Neqn), the estimate of the solution at T+H.
  ------------------------------------------------------------------ }

const
  C1 = 3.0 / 32.0;
  C2 = 3.0 / 8.0;
  C3 = 1.0 / 2197.0;
  C4 = 12.0 / 13.0;
  C5 = 1.0 / 4104.0;
  C6 = 1.0 / 20520.0;
  C7 = 1.0 / 7618050.0;

var
  ch : Float;
  i  : Integer;

begin
  ch := 0.25 * h;

  for i := 1 to neqn do
    f5^[i] := y^[i] + ch * yp^[i];

  f(t + ch, f5, f1);

  ch := C1 * h;

  for i := 1 to neqn do
    f5^[i] := y^[i] + ch * (yp^[i] + 3.0 * f1^[i]);

  f(t + C2 * h, f5, f2);

  ch := C3 * h;

  for i := 1 to neqn do
    f5^[i] := y^[i] + ch * (1932.0 * yp^[i] +
              (7296.0 * f2^[i] - 7200.0 * f1^[i]));

  f(t + C4 * h, f5, f3);

  ch := C5 * h;

  for i := 1 to neqn do
    f5^[i] := y^[i] + ch * ((8341.0 * yp^[i] - 845.0 * f3^[i])
              + (29440.0 * f2^[i] - 32832.0 * f1^[i]));

  f(t + h, f5, f4);

  ch := C6 * h;

  for i := 1 to neqn do
    f1^[i] := y^[i] + ch * ((-6080.0 * yp^[i]
                 + (9295.0 * f3^[i] - 5643.0 * f4^[i]))
                 + (41040.0 * f1^[i] - 28352.0 * f2^[i]));

  f(t + 0.5 * h, f1, f5);

{ Ready to compute the approximate solution at T+H. }

  ch := C7 * h;

  for i := 1 to neqn do
    s^[i] := y^[i] + ch * ((902880.0 * yp^[i]
                + (3855735.0 * f3^[i] - 1371249.0 * f4^[i]))
                + (3953664.0 * f2^[i] + 277020.0 * f5^[i]));

end;

procedure ReDim_Arrays(neqn : Integer);
{ Redimensions global arrays if necessary }
begin
  DelVector(f1, maxeqn);
  DelVector(f2, maxeqn);
  DelVector(f3, maxeqn);
  DelVector(f4, maxeqn);
  DelVector(f5, maxeqn);

  maxeqn := neqn;

  DimVector(f1, maxeqn);
  DimVector(f2, maxeqn);
  DimVector(f3, maxeqn);
  DimVector(f4, maxeqn);
  DimVector(f5, maxeqn);
end;

procedure RKF45(F                    : TDiffEqs;
                Neqn                 : Integer;
                Y, Yp                : PVector;
                var T                : Float;
                Tout, RelErr, AbsErr : Float;
                var Flag             : Integer);
{ ------------------------------------------------------------------

   RKF45 carries out the Runge-Kutta-Fehlberg method (double precision).


   Discussion:

     This routine is primarily designed to solve non-stiff and mildly stiff
     differential equations when derivative evaluations are inexpensive.
     It should generally not be used when the user is demanding
     high accuracy.

     This routine integrates a system of Neqn first-order ordinary
     differential equations of the form:

       dY(i)/dT = F(T,Y(1),Y(2),...,Y(Neqn))

     where the Y(1:Neqn) are given at T.

     Typically the subroutine is used to integrate from T to TOUT but it
     can be used as a one-step integrator to advance the solution a
     single step in the direction of TOUT.  On return, the parameters in
     the call list are set for continuing the integration.  The user has
     only to call again (and perhaps define a new value for TOUT).

     Before the first call, the user must

     * supply the subroutine F(T,Y,YP) to evaluate the right hand side;
       and declare F in an EXTERNAL statement;

     * initialize the parameters:
       Neqn, Y(1:Neqn), T, TOUT, RELERR, ABSERR, FLAG.
       In particular, T should initially be the starting point for integration,
       Y should be the value of the initial conditions, and FLAG should
       normally be +1.

     Normally, the user only sets the value of FLAG before the first call, and
     thereafter, the program manages the value.  On the first call, FLAG should
     normally be +1 (or -1 for single step mode.)  On normal return, FLAG will
     have been reset by the program to the value of 2 (or -2 in single
     step mode), and the user can continue to call the routine with that
     value of FLAG.

     (When the input magnitude of FLAG is 1, this indicates to the program
     that it is necessary to do some initialization work.  An input magnitude
     of 2 lets the program know that that initialization can be skipped,
     and that useful information was computed earlier.)

     The routine returns with all the information needed to continue
     the integration.  If the integration reached TOUT, the user need only
     define a new TOUT and call again.  In the one-step integrator
     mode, returning with FLAG = -2, the user must keep in mind that
     each step taken is in the direction of the current TOUT.  Upon
     reaching TOUT, indicated by the output value of FLAG switching to 2,
     the user must define a new TOUT and reset FLAG to -2 to continue
     in the one-step integrator mode.

     In some cases, an error or difficulty occurs during a call.  In that case,
     the output value of FLAG is used to indicate that there is a problem
     that the user must address.  These values include:

     * 3, integration was not completed because the input value of RELERR, the
       relative error tolerance, was too small.  RELERR has been increased
       appropriately for continuing.  If the user accepts the output value of
       RELERR, then simply reset FLAG to 2 and continue.

     * 4, integration was not completed because more than MAXNFE derivative
       evaluations were needed.  This is approximately (MAXNFE/6) steps.
       The user may continue by simply calling again.  The function counter
       will be reset to 0, and another MAXNFE function evaluations are allowed.

     * 5, integration was not completed because the solution vanished,
       making a pure relative error test impossible.  The user must use
       a non-zero ABSERR to continue.  Using the one-step integration mode
       for one step is a good way to proceed.

     * 6, integration was not completed because the requested accuracy
       could not be achieved, even using the smallest allowable stepsize.
       The user must increase the error tolerances ABSERR or RELERR before
       continuing.  It is also necessary to reset FLAG to 2 (or -2 when
       the one-step integration mode is being used).  The occurrence of
       FLAG = 6 indicates a trouble spot.  The solution is changing
       rapidly, or a singularity may be present.  It often is inadvisable
       to continue.

     * 7, it is likely that this routine is inefficient for solving
       this problem.  Too much output is restricting the natural stepsize
       choice.  The user should use the one-step integration mode with
       the stepsize determined by the code.  If the user insists upon
       continuing the integration, reset FLAG to 2 before calling
       again.  Otherwise, execution will be terminated.

     * 8, invalid input parameters, indicates one of the following:
       Neqn <= 0;
       T = TOUT and |FLAG| /= 1;
       RELERR < 0 or ABSERR < 0;
       FLAG == 0  or FLAG < -2 or 8 < FLAG.

   Modified:

     27 March 2004

   Author:

     H A Watts and L F Shampine,
     Sandia Laboratories,
     Albuquerque, New Mexico.

   Reference:

     E. Fehlberg,
     Low-order Classical Runge-Kutta Formulas with Stepsize Control,
     NASA Technical Report R-315.

     L F Shampine, H A Watts, S Davenport,
     Solving Non-stiff Ordinary Differential Equations - The State of the Art,
     SIAM Review,
     Volume 18, pages 376-411, 1976.

   Parameters:

     Input, external F, a user-supplied subroutine to evaluate the
     derivatives Y (T), of the form:

       sub f ( t as double, y() as double, yp() as double )

     Input, Neqn, the number of equations to be integrated.

     Input/output, Y(Neqn), the current solution vector at T.

     Input/output, YP(Neqn), the current value of the
     derivative of the dependent variable.  The user should not set or alter
     this information

     Input/output, T, the current value of the independent
     variable.

     Input, TOUT, the output point at which solution is
     desired.  TOUT = T is allowed on the first call only, in which case
     the routine returns with FLAG = 2 if continuation is possible.

     Input, RELERR, ABSERR, the relative and absolute
     error tolerances for the local error test.  At each step the code
     requires:
       abs ( local error ) <= RELERR * abs ( Y ) + ABSERR
     for each component of the local error and the solution vector Y.
     RELERR cannot be "too small".  If the routine believes RELERR has been
     set too small, it will reset RELERR to an acceptable value and return
     immediately for user action.

     Input/output, FLAG, indicator for status of integration.
     On the first call, set FLAG to +1 for normal use, or to -1 for single
     step mode.  On return, a value of 2 or -2 indicates normal progress,
     while any other value indicates a problem that should be addressed.
  ------------------------------------------------------------------ }

const
  remin  = 1.0E-12;
  maxnfe = 3000;

var
  k, mflag                                   : Integer;
  ae, dt, ee, eeoet, esttol, et              : Float;
  hmin, relerr_min, s, scale, tol, toln, ypk : Float;
  hfaild, outp                               : Boolean;

label
  Cont, Done;

begin
{ Check the input parameters. }

  if (neqn < 1) or (relerr < 0) or (abserr < 0) or
    ((flag = 0) or (flag > 8) or (flag < -2)) then
    begin
      flag := 8;
      exit;
    end;

  mflag := abs(flag);

{ Is this a continuation call? }

  if mflag <> 1 then
  begin
    if (t = tout) and (kflag <> 3) then
    begin
      flag := 8;
      exit;
    end;

    if mflag = 2 then
    begin
      if kflag = 3 then
      begin
        flag := flag_save;
        mflag := abs(flag)
      end
      else if init = 0 then
        flag := flag_save
      else if kflag = 4 then
        nfe := 0
      else if (kflag = 5) and (abserr = 0) then
        exit
      else if (kflag = 6) and (relerr <= relerr_save) and (abserr <= abserr_save) then
        exit;
    end
    else  { FLAG = 3, 4, 5, 6, 7 or 8. }
    begin
      if flag = 3 then
      begin
        flag := flag_save;
        if kflag = 3 then mflag := abs(flag)
      end
      else if flag = 4 then
      begin
        nfe := 0;
        flag := flag_save;
        if kflag = 3 then mflag := abs(flag)
      end
      else if (flag = 5) and (abserr > 0) then
      begin
        flag := flag_save;
        if kflag = 3 then mflag := abs(flag)
      end
      else    { Integration cannot be continued because the user did not }
        exit; { respond to the instructions pertaining to FLAG = 5,6,7,8 }
    end;
  end;

{ Save the input value of FLAG. }
{ Set the continuation flag KFLAG for subsequent input checking. }

  flag_save := flag;
  kflag := 0;

{ Save RELERR and ABSERR for checking input on subsequent calls. }

  relerr_save := relerr;
  abserr_save := abserr;

{ Restrict the relative error tolerance to be at least

     2 * EPS + REMIN

  to avoid limiting precision difficulties arising from impossible
  accuracy requests. }

  relerr_min := 2 * MachEp + remin;

{ Is the relative error tolerance too small? }

  if relerr < relerr_min then
  begin
    relerr := relerr_min;
    flag := 3;
    kflag := 3;
    exit
  end;

  dt := tout - t;

{ Initialization:

  Set the initialization completion indicator, INIT;
  set the indicator for too many output points, KOP;
  evaluate the initial derivatives;
  set the counter for function evaluations, NFE;
  estimate the starting stepsize. }

  if mflag = 1 then
  begin
    init := 0;
    kop := 0;
    f(t, y, yp);
    nfe := 1;
    if t = tout then
    begin
      flag := 2;
      exit;
    end;
  end;

  if init = 0 then
  begin
    init := 1;
    h := abs(dt);
    toln := 0;

    for k := 1 to neqn do
    begin
      tol := relerr * abs (y^[k]) + abserr;
      if tol > 0 then
      begin
        toln := tol;
        ypk := abs(yp^[k]);
        if tol < ypk * h * h * h * h * h then
          h := Exp(0.2 * Ln(tol / ypk));
      end
    end;

    if toln <= 0 then h := 0;

    h := FMax(h, 26 * MachEp * FMax(abs(t), abs(dt)));
    flag_save := sgn(flag) * 2
  end;

{ Set the stepsize for integration in the direction from T to TOUT. }

  h := sgn(dt) * abs(h);

{ Test to see if too may output points are being requested. }

  if 2 * abs(dt) <= abs(h) then kop := kop + 1;

{ Unnecessary frequency of output. }

  if kop = 100 then
  begin
    kop := 0;
    flag := 7;
    exit
  end;

{ If we are too close to the output point, then simply extrapolate and return. }

  if abs(dt) <= 26 * MachEp * abs(t) then
  begin
    t := tout;
    for k := 1 to neqn do
      y^[k] := y^[k] + dt * yp^[k];
    f(t, y, yp);
    nfe := nfe + 1;
    flag := 2;
    exit
  end;

{ Initialize the output point indicator. }

  outp := False;

{ To avoid premature underflow in the error tolerance function,
  scale the error tolerances. }

  scale := 2 / relerr;
  ae := scale * abserr;

{ Redimension global arrays if necessary }

  if neqn > maxeqn then ReDim_Arrays(neqn);

{ Step by step integration. }

  repeat

    hfaild := False;

{ Set the smallest allowable stepsize. }

    hmin := 26 * MachEp * abs(t);

{ Adjust the stepsize if necessary to hit the output point.

  Look ahead two steps to avoid drastic changes in the stepsize and
  thus lessen the impact of output points on the code. }

    dt := tout - t;

    if 2.0 * abs(h) > abs(dt) then
    begin

    { Will the next successful step complete the integration to the output point? }

      if abs(dt) <= abs(h) then
      begin
        outp := True;
        h := dt
      end
      else
        h := 0.5 * dt;
    end;

{ Here begins the core integrator for taking a single step.

  The tolerances have been scaled to avoid premature underflow in
  computing the error tolerance function ET.
  To avoid problems with zero crossings, relative error is measured
  using the average of the magnitudes of the solution at the
  beginning and end of a step.
  The error estimate formula has been grouped to control loss of
  significance.

  To distinguish the various arguments, H is not permitted
  to become smaller than 26 units of roundoff in T.
  Practical limits on the change in the stepsize are enforced to
  smooth the stepsize selection process and to avoid excessive
  chattering on problems having discontinuities.
  To prevent unnecessary failures, the code uses 9/10 the stepsize
  it estimates will succeed.

  After a step failure, the stepsize is not allowed to increase for
  the next attempted step.  This makes the code more efficient on
  problems having discontinuities and more effective in general
  since local extrapolation is being used and extra caution seems
  warranted.

  Test the number of derivative function evaluations.
  If okay, try to advance the integration from T to T+H. }

    repeat

{ Have we done too much work? }

      if maxnfe < nfe then
      begin
        flag := 4;
        kflag := 4;
        exit
      end;

{ Advance an approximate solution over one step of length H. }

      Fehl(f, neqn, y, t, h, yp, f1, f2, f3, f4, f5, f1);
      nfe := nfe + 5;

{ Compute and test allowable tolerances versus local error estimates
  and remove scaling of tolerances.  The relative error is
  measured with respect to the average of the magnitudes of the
  solution at the beginning and end of the step. }

      eeoet := 0;

      for k := 1 to neqn do
      begin
        et := abs(y^[k]) + abs(f1^[k]) + ae;

        if et <= 0 then
        begin
          flag := 5;
          exit
        end;

        ee := abs((-2090.0 * yp^[k] + (21970.0 * f3^[k] - 15048.0 * f4^[k]))
                 + (22528.0 * f2^[k] - 27360.0 * f5^[k]));

        eeoet := FMax(eeoet, ee / et);
      end;

      esttol := abs(h) * eeoet * scale / 752400.0;

      if esttol <= 1 then goto Cont;

{ Unsuccessful step. Reduce the stepsize, try again.
  The decrease is limited to a factor of 1/10. }

      hfaild := True;
      outp := False;

      if esttol < 59049.0 then
        s := 0.9 / Exp(0.2 * Ln(esttol))
      else
        s := 0.1;

      h := s * h;

      if abs(h) < hmin then
      begin
        flag := 6;
        kflag := 6;
        exit;
      end;

    until False;

{ We exited the loop because we took a successful step.
  Store the solution for T+H, and evaluate the derivative there. }

Cont:

    t := t + h;
    for k := 1 to neqn do
      y^[k] := f1^[k];
    f(t, y, yp);
    nfe := nfe + 1;

{ Choose the next stepsize.  The increase is limited to a factor of 5.
  If the step failed, the next stepsize is not allowed to increase. }

    if 0.0001889568 < esttol then
      s := 0.9 / Exp(0.2 * Ln(esttol))
    else
      s := 5.0;

    if hfaild then s := FMin(s, 1.0);

    h := sgn(h) * FMax(s * abs(h), hmin);

{ End of core integrator

  Should we take another step? }

    if outp then
    begin
      t := tout;
      flag := 2;
      exit
    end;

    if flag <= 0 then goto Done;

  until False;

{ One step integration mode. }

Done:

  flag := -2;

end;

end.

