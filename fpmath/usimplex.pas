{ ******************************************************************
  Function minimization by the simplex method
  ****************************************************************** }

unit usimplex;

interface

uses
  utypes;

procedure SaveSimplex(FileName : string);
{ ------------------------------------------------------------------
  Opens a file to save the Simplex iterations
  ------------------------------------------------------------------ }

procedure Simplex(Func      : TFuncNVar;
                  X         : PVector;
                  Lb, Ub    : Integer;
                  MaxIter   : Integer;
                  Tol       : Float;
                  var F_min : Float);
{ ------------------------------------------------------------------
  Minimization of a function of several variables by the
  simplex method of Nelder and Mead
  ------------------------------------------------------------------
  Input parameters  : Func    = objective function
                      X       = initial minimum coordinates
                      Lbound,
                      Ubound  = indices of first and last variables
                      MaxIter = maximum number of iterations
                      Tol     = required precision
  ------------------------------------------------------------------
  Output parameters : X       = refined minimum coordinates
                      F_min   = function value at minimum
  ------------------------------------------------------------------
  The function MathErr returns one of the following codes:

     OptOk      = no error
     OptNonConv = non-convergence
  ------------------------------------------------------------------ }

implementation

const
  WriteLogFile : Boolean = False;

var
  LogFile : Text;

procedure SaveSimplex(FileName : string);
begin
  Assign(LogFile, FileName);
  Rewrite(LogFile);
  WriteLogFile := True;
end;

procedure Simplex(Func      : TFuncNVar;
                  X         : PVector;
                  Lb, Ub    : Integer;
                  MaxIter   : Integer;
                  Tol       : Float;
                  var F_min : Float);
  const
    Step = 1.50;  { Step used to construct the initial simplex }
  var
    P             : PMatrix;  { Simplex coordinates }
    F             : PVector;  { Function values }
    Pbar          : PVector;  { Centroid coordinates }
    Pstar, P2star : PVector;  { New vertices }
    Ystar, Y2star : Float;    { New function values }
    F0            : Float;    { Function value at minimum }
    N             : Integer;  { Number of parameters }
    M             : Integer;  { Index of last vertex }
    L, H          : Integer;  { Vertices with lowest & highest F values }
    I, J          : Integer;  { Loop variables }
    Iter          : Integer;  { Iteration count }
    Corr, MaxCorr : Float;    { Corrections }
    Sum           : Float;
    Flag          : Boolean;


    procedure UpdateSimplex(Y : Float; Q : PVector);
    { Update "worst" vertex and function value }
    var
      J : Integer;
    begin
      F^[H] := Y;
      for J := Lb to Ub do
        P^[H]^[J] := Q^[J];
    end;

  begin
    { Quit if no iteration required }
    if MaxIter < 1 then
      begin
        F_min := Func(X);
        SetErrCode(OptOk);
        Exit;
      end;

    if WriteLogFile then
      begin
        WriteLn(LogFile, 'Simplex');
        WriteLn(LogFile, 'Iter         F');
      end;

    N := Ub - Lb + 1;
    M := Ub + 1;

    DimMatrix(P, M, Ub);
    DimVector(F, M);
    DimVector(Pbar, Ub);
    DimVector(Pstar, Ub);
    DimVector(P2star, Ub);

    Iter := 1;
    F0 := MaxNum;

    { Construct initial simplex }
    for I := Lb to M do
      for J := Lb to Ub do
        P^[I]^[J] := X^[J];
    for I := Lb to Ub do
      P^[I]^[I] := P^[I]^[I] * Step;

    { Evaluate function at each vertex }
    for I := Lb to M do
      F^[I] := Func(P^[I]);

    repeat
      { Find vertices (L,H) having the lowest and highest
        function values, i.e. "best" and "worst" vertices }
      L := Lb;
      H := Lb;
      for I := Lb + 1 to M do
        if F^[I] < F^[L] then
          L := I
        else if F^[I] > F^[H] then
          H := I;
      if F^[L] < F0 then
        F0 := F^[L];

      if WriteLogFile then
        WriteLn(LogFile, Iter:4, '   ', F0:12);

      { Find centroid of points other than P(H) }
      for J := Lb to Ub do
        begin
          Sum := 0.0;
          for I := Lb to M do
            if I <> H then Sum := Sum + P^[I]^[J];
          Pbar^[J] := Sum / N;
        end;

      { Reflect worst vertex through centroid }
      for J := Lb to Ub do
        Pstar^[J] := 2.0 * Pbar^[J] - P^[H]^[J];
      Ystar := Func(Pstar);

      { If reflection successful, try extension }
      if Ystar < F^[L] then
        begin
          for J := Lb to Ub do
            P2star^[J] := 3.0 * Pstar^[J] - 2.0 * Pbar^[J];
          Y2star := Func(P2star);

          { Retain extension or contraction }
          if Y2star < F^[L] then
            UpdateSimplex(Y2star, P2star)
          else
            UpdateSimplex(Ystar, Pstar);
        end
      else
        begin
          I := Lb;
          Flag := False;
          repeat
            if (I <> H) and (F^[I] > Ystar) then Flag := True;
            Inc(I);
          until Flag or (I > M);
          if Flag then
            UpdateSimplex(Ystar, Pstar)
          else
            begin
              { Contraction on the reflection side of the centroid }
              if Ystar <= F^[H] then
                UpdateSimplex(Ystar, Pstar);

              { Contraction on the opposite side of the centroid }
              for J := Lb to Ub do
                P2star^[J] := 0.5 * (P^[H]^[J] + Pbar^[J]);
              Y2star := Func(P2star);
              if Y2star <= F^[H] then
                UpdateSimplex(Y2star, P2star)
              else
                { Contract whole simplex }
                for I := Lb to M do
                  for J := Lb to Ub do
                    P^[I]^[J] := 0.5 * (P^[I]^[J] + P^[L]^[J]);
            end;
        end;

      { Test convergence }
      MaxCorr := 0.0;
      for J := Lb to Ub do
        begin
          Corr := Abs(P^[H]^[J] - P^[L]^[J]);
          if Corr > MaxCorr then MaxCorr := Corr;
        end;
      Inc(Iter);
    until (MaxCorr < Tol) or (Iter > MaxIter);

    for J := Lb to Ub do
      X^[J] := P^[L]^[J];
    F_min := F^[L];

    DelMatrix(P, M, Ub);
    DelVector(F, M);
    DelVector(Pbar, Ub);
    DelVector(Pstar, Ub);
    DelVector(P2star, Ub);

    if WriteLogFile then
      Close(LogFile);

    if Iter > MaxIter then
      SetErrCode(OptNonConv)
    else
      SetErrCode(OptOk);
  end;

end.

