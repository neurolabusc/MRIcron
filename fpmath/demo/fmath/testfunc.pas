{ ******************************************************************
  This program tests the accuracy of the elementary functions.
  For each function, 20 random arguments are picked, then the
  function is computed, the reciprocal function is applied to
  the result, and the relative error between this last result
  and the original argument is computed.
  ****************************************************************** }

program testfunc;

uses
  crt, tpmath;

const
  NARG  = 20;     { Number of arguments }
  BLANK = '   ';  { Separator }

type
  TVector = array[1..NARG] of Float;

var
  X, Y : TVector;  { Random arguments }

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

  procedure RanArgs(var X : TVector; Xmin, Xmax : Float);
  { Fills a table of random arguments between Xmin and Xmax,
    rounded to 4 decimal places and sorted in increasing order. }
  var
    I, J, K : integer;
    A : Float;
  begin
    for I := 1 to NARG do
      begin
        A := (Xmax - Xmin) * Random + Xmin;
        if Abs(A) < 1.0E-4 then A := Sgn(A) * 1.0E-4;
        X[I] := Int(10000 * A) / 10000;
      end;

    { Insertion sort }
    for I := 1 to Pred(NARG) do
      begin
        K := I;
        A := X[I];
        for J := Succ(I) to NARG do
          if X[J] < A then
            begin
              K := J;
              A := X[J];
            end;
        FSwap(X[I], X[K]);
      end;
  end;

  procedure Test_Exp;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, -10, 10);
    WriteLn('   X                      Y = Expo(X)                   X = Log(Y)    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Expo(X[I]);
        T := Log(Z);
        R := (X[I] - T) / X[I];
        WriteLn(X[I]:8:4, BLANK, Z:26, BLANK, T:26, BLANK, R:10);
      end;
    Pause;
  end;

  procedure Test_Exp10;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, -5, 5);
    WriteLn('   X                     Y = Exp10(X)                 X = Log10(Y)    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Exp10(X[I]);
        T := Log10(Z);
        R := (X[I] - T) / X[I];
        WriteLn(X[I]:8:4, BLANK, Z:26, BLANK, T:26, BLANK, R:10);
      end;
    Pause;
  end;

  procedure Test_Exp2;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, -15, 15);
    WriteLn('   X                      Y = Exp2(X)                  X = Log2(Y)    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Exp2(X[I]);
        T := Log2(Z);
        R := (X[I] - T) / X[I];
        WriteLn(X[I]:8:4, BLANK, Z:26, BLANK, T:26, BLANK, R:10);
      end;
    Pause;
  end;

  procedure Test_Power;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, 0, 10);
    RanArgs(Y, -5, 5);
    WriteLn('  X      Y                Z = Power(X, Y)              Y = Log(Z, X)  Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Power(X[I], Y[I]);   { X^Y }
        T := LogA(Z, X[I]);       { Log(X^Y, X) = Y }
        R := (Y[I] - T) / Y[I];
        WriteLn(X[I]:6:4, Y[I]:8:4, ' ', Z:26, ' ', T:26, ' ', R:10);
      end;
    Pause;
  end;

  procedure Test_Sin;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, -PiDiv2, PiDiv2);
    WriteLn('   X                       Y = Sin(X)                X = ArcSin(Y)    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Sin(X[I]);
        T := ArcSin(Z);
        R := (X[I] - T) / X[I];
        WriteLn(X[I]:8:4, BLANK, Z:26, BLANK, T:26, BLANK, R:10);
      end;
    Pause;
  end;

  procedure Test_Cos;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, 0, PI);
    WriteLn('   X                       Y = Cos(X)                X = ArcCos(Y)    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Cos(X[I]);
        T := ArcCos(Z);
        R := (X[I] - T) / X[I];
        WriteLn(X[I]:8:4, BLANK, Z:26, BLANK, T:26, BLANK, R:10);
      end;
    Pause;
  end;

  procedure Test_Tan;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, -PiDiv2, PiDiv2);
    WriteLn('   X                       Y = Tan(X)                X = ArcTan(Y)    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Tan(X[I]);
        T := ArcTan(Z);
        R := (X[I] - T) / X[I];
        WriteLn(X[I]:8:4, BLANK, Z:26, BLANK, T:26, BLANK, R:10);
      end;
    Pause;
  end;

  procedure Test_Sinh;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, 0, 5);
    WriteLn('   X                      Y = Sinh(X)               X = ArcSinh(Y)    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Sinh(X[I]);
        T := ArcSinh(Z);
        R := (X[I] - T) / X[I];
        WriteLn(X[I]:8:4, BLANK, Z:26, BLANK, T:26, BLANK, R:10);
      end;
    Pause;
  end;

  procedure Test_Cosh;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, 0, 5);
    WriteLn('   X                      Y = Cosh(X)               X = ArcCosh(Y)    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Cosh(X[I]);
        T := ArcCosh(Z);
        R := (X[I] - T) / X[I];
        WriteLn(X[I]:8:4, BLANK, Z:26, BLANK, T:26, BLANK, R:10);
      end;
    Pause;
  end;

  procedure Test_Tanh;
  var
    I : Integer;
    Z, T, R : Float;
  begin
    RanArgs(X, -5, 5);
    WriteLn('   X                      Y = Tanh(X)               X = ArcTanh(Y)    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to NARG do
      begin
        Z := Tanh(X[I]);
        T := ArcTanh(Z);
        R := (X[I] - T) / X[I];
        WriteLn(X[I]:8:4, BLANK, Z:26, BLANK, T:26, BLANK, R:10);
      end;
    Pause;
  end;

begin
  Test_Exp;
  Test_Exp10;
  Test_Exp2;
  Test_Power;
  Test_Sin;
  Test_Cos;
  Test_Tan;
  Test_Sinh;
  Test_Cosh;
  Test_Tanh;
end.
