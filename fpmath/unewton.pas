{ ******************************************************************
  Minimization of a function of several variables by the
  Newton-Raphson method
  ****************************************************************** }

unit unewton;

interface

uses
  utypes, ulineq, ulinmin, ucompvec;

procedure SaveNewton(FileName : string);
{ ------------------------------------------------------------------
  Save Newton-Raphson iterations in a file
  ------------------------------------------------------------------ }

procedure Newton(Func      : TFuncNVar;
                 HessGrad  : THessGrad;
                 X         : PVector;
                 Lb, Ub    : Integer;
                 MaxIter   : Integer;
                 Tol       : Float;
                 var F_min : Float;
                 G         : PVector;
                 H_inv     : PMatrix;
                 var Det   : Float);
{ ------------------------------------------------------------------
  Minimization of a function of several variables by the
  Newton-Raphson method
  ------------------------------------------------------------------
  Input parameters  : Func       = objective function
                      Gradient   = procedure to compute gradient
                      X          = initial minimum coordinates
                      Lb, Ub     = indices of first and last variables
                      MaxIter    = maximum number of iterations
                      Tol        = required precision
  ------------------------------------------------------------------
  Output parameters : X          = refined minimum coordinates
                      F_min      = function value at minimum
                      G          = gradient vector
                      H_inv      = inverse hessian matrix
                      Det        = determinant of hessian
  ------------------------------------------------------------------
  Possible results  : OptOk      = no error
                      OptNonConv = non-convergence
                      OptSing    = singular hessian matrix
  ---------------------------------------------------------------------- }

implementation

const
  WriteLogFile : Boolean = False;

var
  LogFile : Text;

procedure SaveNewton(FileName : string);
  begin
    Assign(LogFile, FileName);
    Rewrite(LogFile);
    WriteLogFile := True;
  end;

procedure Newton(Func      : TFuncNVar;
                 HessGrad  : THessGrad;
                 X         : PVector;
                 Lb, Ub    : Integer;
                 MaxIter   : Integer;
                 Tol       : Float;
                 var F_min : Float;
                 G         : PVector;
                 H_inv     : PMatrix;
                 var Det   : Float);

  var
    I, Iter      : Integer;
    R            : Float;
    OldX, DeltaX : PVector;

  procedure Init;
  { Initializes variables }
  var
    I : Integer;
  begin
    { Initialize function }
    F_min := Func(X);

    { Initialize gradient and hessian (stored in H_inv) }
    HessGrad(X, G, H_inv);

    { Initialize search direction }
    for I := Lb to Ub do
      DeltaX^[I] := - G^[I];

    { Solve system }
    LinEq(H_inv, DeltaX, Lb, Ub, Det);
  end;

  procedure Terminate(ErrCode : Integer);
  { Set error code and deallocate arrays }
  begin
    DelVector(OldX, Ub);
    DelVector(DeltaX, Ub);

    SetErrCode(ErrCode);

    if WriteLogFile then
      Close(LogFile);
  end;

  begin
    DimVector(OldX, Ub);
    DimVector(DeltaX, Ub);

    Init;

    if MathErr <> MatOk then
      begin
        Terminate(OptSing);
        Exit;
      end;

    if MaxIter < 1 then
      begin
        Terminate(OptOk);
        Exit;
      end;

    if WriteLogFile then
      begin
        WriteLn(LogFile, 'Newton-Raphson');
        WriteLn(LogFile, 'Iter         F');
      end;

    Iter := 0;

    repeat
      { Prepare next iteration }
      if WriteLogFile then
        WriteLn(LogFile, Iter:4, '   ', F_min:12);

      Iter := Iter + 1;
      if Iter > MaxIter then
        begin
          Terminate(OptNonConv);
          Exit;
        end;

      { Save old parameters }
      for I := Lb to Ub do
        OldX^[I] := X^[I];

      { Minimize along the direction specified by DeltaX }
      R := 0.1;
      LinMin(Func, X, DeltaX, Lb, Ub, R, 10, 0.01, F_min);

      { Compute new gradient and hessian }
      HessGrad(X, G, H_inv);

      { Initialize search direction }
      for I := Lb to Ub do
        DeltaX^[I] := - G^[I];

      { Solve system }
      LinEq(H_inv, DeltaX, Lb, Ub, Det);

      if MathErr <> MatOk then
        begin
          Terminate(OptSing);
          Exit;
        end;
    until CompVec(X, OldX, Lb, Ub, Tol);

    Terminate(OptOk);
  end;

end.

