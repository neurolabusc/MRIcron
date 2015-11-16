{ ******************************************************************
  Minimization of a function of one variable by Golden Search method
  ****************************************************************** }

unit ugoldsrc;

interface

uses
  utypes, uminbrak;

procedure GoldSearch(Func           : TFunc;
                     A, B           : Float;
                     MaxIter        : Integer;
                     Tol            : Float;
                     var Xmin, Ymin : Float);
{ ------------------------------------------------------------------
  Performs a golden search for the minimum of function Func
  ------------------------------------------------------------------
  Input parameters :
    Func    = objective function
    A, B    = two points near the minimum
    MaxIter = maximum number of iterations
    Tol     = required precision (should not be less than
                the square root of the machine precision)
  ------------------------------------------------------------------
  Output parameters : Xmin, Ymin = coordinates of minimum
  ------------------------------------------------------------------
  Possible results  : OptOk
                      OptNonConv
  ------------------------------------------------------------------ }

implementation

procedure GoldSearch(Func           : TFunc;
                     A, B           : Float;
                     MaxIter        : Integer;
                     Tol            : Float;
                     var Xmin, Ymin : Float);
  var
    C, Fa, Fb, Fc, F1, F2, MinTol, X0, X1, X2, X3 : Float;
    Iter                                          : Integer;

  begin
    MinTol := Sqrt(MachEp);
    if Tol < MinTol then Tol := MinTol;

    MinBrack(Func, A, B, C, Fa, Fb, Fc);

    X0 := A;
    X3 := C;

    if (C - B) > (B - A) then
      begin
        X1 := B;
        X2 := B + CGold * (C - B);
        F1 := Fb;
        F2 := Func(X2);
      end
    else
      begin
        X1 := B - CGold * (B - A);
        X2 := B;
        F1 := Func(X1);
        F2 := Fb;
      end;

    Iter := 0;

    while (Iter <= MaxIter) and (Abs(X3 - X0) > Tol * (Abs(X1) + Abs(X2))) do
      if F2 < F1 then
        begin
          X0 := X1;
          X1 := X2;
          F1 := F2;
          X2 := X1 + CGold * (X3 - X1);
          F2 := Func(X2);
          Inc(Iter);
        end
      else
        begin
          X3 := X2;
          X2 := X1;
          F2 := F1;
          X1 := X2 - CGold * (X2 - X0);
          F1 := Func(X1);
          Inc(Iter);
        end;

    if F1 < F2 then
      begin
        Xmin := X1;
        Ymin := F1;
      end
    else
      begin
        Xmin := X2;
        Ymin := F2;
      end;

    if Iter > MaxIter then
      SetErrCode(OptNonConv)
    else
      SetErrCode(OptOk);
  end;

end.
