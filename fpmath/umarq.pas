{ ******************************************************************
  Minimization of a function of several variables by Marquardt's
  method
  ****************************************************************** }

unit umarq;

interface

uses
  utypes, ugausjor, ulinmin, ucompvec;

procedure SaveMarquardt(FileName : string);
{ ------------------------------------------------------------------
  Save Marquardt iterations in a file
  ------------------------------------------------------------------ }

procedure Marquardt(Func      : TFuncNVar;
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
  Minimization of a function of several variables by Marquardt's
  method
  ------------------------------------------------------------------
  Input parameters  : Func       = objective function
                      HessGrad   = procedure to compute hessian and gradient
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
  Possible results  : OptOk        = no error
                      OptNonConv   = non-convergence
                      OptSing      = singular hessian matrix
                      OptBigLambda = too high Marquardt parameter Lambda
  ---------------------------------------------------------------------- }

implementation

const
  WriteLogFile : Boolean = False;

var
  LogFile : Text;

procedure SaveMarquardt(FileName : string);
  begin
    Assign(LogFile, FileName);
    Rewrite(LogFile);
    WriteLogFile := True;
  end;

procedure Marquardt(Func      : TFuncNVar;
                    HessGrad  : THessGrad;
                    X         : PVector;
                    Lb, Ub    : Integer;
                    MaxIter   : Integer;
                    Tol       : Float;
                    var F_min : Float;
                    G         : PVector;
                    H_inv     : PMatrix;
                    var Det   : Float);

  const
    Lambda0   = 1.0E-2;   { Initial lambda value }
    LambdaMax = 1.0E+3;   { Highest lambda value }
    FTol      = 1.0E-10;  { Tolerance on function decrease }

  var
    Ub1, I, J, Iter : Integer;
    F1, R           : Float;
    OldX, DeltaX    : PVector;
    A, H            : PMatrix;
    Lambda          : Float;
    LambdaOk        : Boolean;

  procedure SolveSystem(Lambda : Float);
  { Solve the system of linear equations :

      H' * DeltaX = -G

    where H' is the modified hessian matrix (diagonal terms
    multiplied by (1 + Lambda)), and G is the gradient vector,
    for a given value of Marquardt's Lambda parameter.

    The whole system is stored in a matrix A = [H'|G]
    which is transformed by the Gauss-jordan method.
    The inverse hessian matrix H_inv is then retrieved
    from the transformed matrix. }

  var
    Lambda1 : Float;
    I, J    : Integer;
  begin
    if Lambda > 0.0 then
      begin
        Lambda1 := 1.0 + Lambda;
        for I := Lb to Ub do
          A^[I]^[I] := Lambda1 * H^[I]^[I];
      end;

    GaussJordan(A, Lb, Ub, Ub1, Det);

    if MathErr = MatOk then
      for I := Lb to Ub do
        for J := Lb to Ub do
          H_inv^[I]^[J] := A^[I]^[J];
  end;

  procedure Terminate(ErrCode : Integer);
  { Set error code and deallocate arrays }
  begin
    DelVector(OldX, Ub);
    DelVector(DeltaX, Ub);
    DelMatrix(A, Ub, Ub1);
    DelMatrix(H, Ub, Ub);

    SetErrCode(ErrCode);

    if WriteLogFile then
      Close(LogFile);
  end;

  begin
    Ub1 := Ub + 1;

    DimVector(OldX, Ub);
    DimVector(DeltaX, Ub);
    DimMatrix(A, Ub, Ub1);
    DimMatrix(H, Ub, Ub);

    if WriteLogFile then
      begin
        WriteLn(LogFile, 'Marquardt');
        WriteLn(LogFile, 'Iter         F            Lambda');
      end;

    Iter := 0;
    Lambda := Lambda0;
    F_min := Func(X);

    repeat
      if WriteLogFile then
        WriteLn(LogFile, Iter:4, '   ', F_min:12, '   ', Lambda:12);

      { Save old parameters }
      for I := Lb to Ub do
        OldX^[I] := X^[I];

      { Compute Gradient and Hessian }
      HessGrad(X, G, H);
      for I := Lb to Ub do
        begin
          for J := Lb to Ub do
            A^[I]^[J] := H^[I]^[J];
          A^[I]^[Ub1] := - G^[I];
        end;

      if MaxIter < 1 then
        begin
          SolveSystem(0.0);
          if MathErr = MatOk then
            Terminate(OptOk)
          else
            Terminate(OptSing);
          Exit;
        end;

      { Prepare next iteration }
      Iter := Iter + 1;
      if Iter > MaxIter then
        begin
          Terminate(OptNonConv);
          Exit;
        end;

      repeat
        SolveSystem(Lambda);

        if MathErr <> MatOk then
          begin
            Terminate(OptSing);
            Exit;
          end;

        { Initialize parameters and search direction }
        for I := Lb to Ub do
          begin
            X^[I] := OldX^[I];
            DeltaX^[I] := A^[I]^[Ub1];
          end;

        { Minimize along the direction specified by DeltaX }
        { using an initial step of 0.1 * |DeltaX|          }
        R := 0.1;
        LinMin(Func, X, DeltaX, Lb, Ub, R, 10, 0.01, F1);

        { Check that the function has decreased, otherwise }
        { increase Lambda, without exceeding LambdaMax     }
        LambdaOk := (F1 - F_min) < F_min * FTol;
        if not LambdaOk then Lambda := 10.0 * Lambda;
        if Lambda > LambdaMax then
          begin
            Terminate(OptBigLambda);
            Exit;
          end;
      until LambdaOk;

      Lambda := 0.1 * Lambda;
      F_min := F1;
    until CompVec(X, OldX, Lb, Ub, Tol);

    Terminate(OptOk);
  end;

end.

