{ ******************************************************************
  Minimization of a function of several variables by the
  Broyden-Fletcher-Goldfarb-Shanno (BFGS) method
  ****************************************************************** }

unit ubfgs;

interface

uses
  utypes, ulinmin, ucompvec;

procedure SaveBFGS(FileName : string);
{ ------------------------------------------------------------------
  Save BFGS iterations in a file
  ------------------------------------------------------------------ }

procedure BFGS(Func      : TFuncNVar;
               Gradient  : TGradient;
               X         : PVector;
               Lb, Ub    : Integer;
               MaxIter   : Integer;
               Tol       : Float;
               var F_min : Float;
               G         : PVector;
               H_inv     : PMatrix);
{ ------------------------------------------------------------------
  Minimization of a function of several variables by the
  Broyden-Fletcher-Goldfarb-Shanno method
  ------------------------------------------------------------------
  Input parameters  : Func     = objective function
                      Gradient = procedure to compute gradient
                      X        = initial minimum coordinates
                      Lb, Ub   = indices of first and last variables
                      MaxIter  = maximum number of iterations
                      Tol      = required precision
  ------------------------------------------------------------------
  Output parameters : X        = refined minimum coordinates
                      F_min    = function value at minimum
                      G        = gradient vector
                      H_inv    = inverse hessian matrix
  ------------------------------------------------------------------
  Possible results  : OptOk
                      OptNonConv
  ---------------------------------------------------------------------- }

implementation

const
  WriteLogFile : Boolean = False;

var
  LogFile : Text;

procedure SaveBFGS(FileName : string);
  begin
    Assign(LogFile, FileName);
    Rewrite(LogFile);
    WriteLogFile := True;
  end;

procedure BFGS(Func      : TFuncNVar;
               Gradient  : TGradient;
               X         : PVector;
               Lb, Ub    : Integer;
               MaxIter   : Integer;
               Tol       : Float;
               var F_min : Float;
               G         : PVector;
               H_inv     : PMatrix);
  var
    I, J, Iter                                           : Integer;
    A, DeltaXmax, Gmax, P1, P2, R, R1, R2                : Float;
    OldX, DeltaX, dX, OldG, dG, HdG, R1dX, R2HdG, U, P2U : PVector;

  procedure Init;
  { Initializes Function, Gradient and Inverse Hessian }
  var
    I, J : Integer;
  begin
    { Initialize function }
    F_min := Func(X);

    { Initialize gradient }
    Gradient(X, G);

    { Initialize inverse hessian to unit matrix }
    for I := Lb to Ub do
      begin
        H_inv^[I]^[I] := 1.0;
        for J := I + 1 to Ub do
          begin
            H_inv^[I]^[J] := 0.0;
            H_inv^[J]^[I] := 0.0;
          end;
      end;
  end;

  procedure Terminate(ErrCode : Integer);
  { Set error code and deallocate arrays }
  begin
    DelVector(OldX, Ub);
    DelVector(DeltaX, Ub);
    DelVector(dX, Ub);
    DelVector(OldG, Ub);
    DelVector(dG, Ub);
    DelVector(HdG, Ub);
    DelVector(R1dX, Ub);
    DelVector(R2HdG, Ub);
    DelVector(U, Ub);
    DelVector(P2U, Ub);

    SetErrCode(ErrCode);

    if WriteLogFile then
      Close(LogFile);
  end;

  begin
    DimVector(OldX, Ub);
    DimVector(DeltaX, Ub);
    DimVector(dX, Ub);
    DimVector(OldG, Ub);
    DimVector(dG, Ub);
    DimVector(HdG, Ub);
    DimVector(R1dX, Ub);
    DimVector(R2HdG, Ub);
    DimVector(U, Ub);
    DimVector(P2U, Ub);

    Init;

    if MaxIter < 1 then
      begin
        Terminate(OptOk);
        Exit;
      end;

    if WriteLogFile then
      begin
        WriteLn(LogFile, 'BFGS');
        WriteLn(LogFile, 'Iter         F');
      end;

    { Compute max. gradient component }
    Gmax := Abs(G^[Lb]);
    for I := Lb + 1 to Ub do
      begin
        A := Abs(G^[I]);
        if A > Gmax then Gmax := A;
      end;

    { Quit if gradient is already small }
    if Gmax < MachEp then
      begin
        Terminate(OptOk);
        Exit;
      end;

    { Initialize search direction }
    for I := Lb to Ub do
      DeltaX^[I] := - G^[I];

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

      { Normalize search direction to avoid excessive displacements }
      DeltaXmax := Abs(DeltaX^[Lb]);
      for I := Lb + 1 to Ub do
        begin
          A := Abs(DeltaX^[I]);
          if A > DeltaXmax then DeltaXmax := A;
        end;
      if DeltaXmax > 1.0 then
        for I := Lb to Ub do
          DeltaX^[I] := DeltaX^[I] / DeltaXmax;

      { Save old parameters and gradient }
      for I := Lb to Ub do
        begin
          OldX^[I] := X^[I];
          OldG^[I] := G^[I];
        end;

      { Minimize along the direction specified by DeltaX }
      R := 1.0;
      LinMin(Func, X, DeltaX, Lb, Ub, R, 10, 0.01, F_min);

      { Compute new gradient }
      Gradient(X, G);

      { Compute differences between two successive estimations
        of parameter vector and gradient vector }
      for I := Lb to Ub do
        begin
          dX^[I] := X^[I] - OldX^[I];
          dG^[I] := G^[I] - OldG^[I];
        end;

      { Multiply by inverse hessian }
      for I := Lb to Ub do
        begin
          HdG^[I] := 0.0;
          for J := Lb to Ub do
            HdG^[I] := HdG^[I] + H_inv^[I]^[J] * dG^[J];
        end;

      { Scalar products in denominator of BFGS formula }
      P1 := 0.0; P2 := 0.0;
      for I := Lb to Ub do
        begin
          P1 := P1 + dX^[I] * dG^[I];
          P2 := P2 + dG^[I] * HdG^[I];
        end;

      if (P1 = 0.0) or (P2 = 0.0) then Exit;

      { Inverses of scalar products }
      R1 := 1.0 / P1; R2 := 1.0 / P2;

      { Compute BFGS correction terms }
      for I := Lb to Ub do
        begin
          R1dX^[I] := R1 * dX^[I];
          R2HdG^[I] := R2 * HdG^[I];
          U^[I] := R1dX^[I] - R2HdG^[I];
          P2U^[I] := P2 * U^[I];
        end;

      { Update inverse hessian }
      for I := Lb to Ub do
        for J := Lb to Ub do
          H_inv^[I]^[J] := H_inv^[I]^[J] + R1dX^[I] * dX^[J]
                           - R2HdG^[I] * HdG^[J] + P2U^[I] * U^[J];

      { Update search direction }
      for I := Lb to Ub do
        begin
          DeltaX^[I] := 0.0;
          for J := Lb to Ub do
            DeltaX^[I] := DeltaX^[I] - H_inv^[I]^[J] * G^[J];
        end;
    until CompVec(X, OldX, Lb, Ub, Tol);

    Terminate(OptOk);
  end;

end.

