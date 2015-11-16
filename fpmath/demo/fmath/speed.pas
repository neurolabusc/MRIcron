{ ******************************************************************
  This program measures the execution times of several standard
  mathematical functions, as well as some additional functions
  provided in TPMATH

  The results are stored in the output file SPEED.OUT

  The execution time of each function takes into account the
  computation of random arguments. This corresponds to the
  execution time of the function Random
  ****************************************************************** }

program Speed;

uses
  dos, tpmath;

const
  NFUNC = 29;        { Number of functions }
  NMAX  = 10000000;  { Number of evaluations for each function }

const
  FuncName : array[1..NFUNC] of String[8] =
  ('Ln      ',
   'Log10   ',
   'Log2    ',
   'Exp     ',
   'Exp10   ',
   'Exp2    ',
   'Power   ',
   'Sin     ',
   'Cos     ',
   'Tan     ',
   'ArcSin  ',
   'ArcCos  ',
   'ArcTan  ',
   'ArcTan2 ',
   'Sinh    ',
   'Cosh    ',
   'Tanh    ',
   'ArcSinh ',
   'ArcCosh ',
   'ArcTanh ',
   'Gamma   ',
   'DiGamma ',
   'TriGamma',
   'IGamma  ',
   'Beta    ',
   'IBeta   ',
   'Erf     ',
   'Erfc    ',
   'Random  ');

var
  F : Text;   { Output file }
  N : Byte;   { Function index }
  T : Float;  { Time }

function Time : Float;
{ Returns time in seconds }
var
  H, M, S, C : Word;
begin
  GetTime(H, M, S, C);
  Time := 3600.0 * H + 60.0 * M + S + 0.01 * C;
end;

function TimeToEval(N : Byte) : Float;
{ Returns time to evaluate NMAX functions }
var
  I : LongInt;
  T0, Y : Float;
begin
  T0 := Time;
  case N of
    1 : for I := 1 to NMAX do
          Y := Ln(Random);
    2 : for I := 1 to NMAX do
          Y := Log10(Random);
    3 : for I := 1 to NMAX do
          Y := Log2(Random);
    4 : for I := 1 to NMAX do
          Y := Exp(Random);
    5 : for I := 1 to NMAX do
          Y := Exp10(Random);
    6 : for I := 1 to NMAX do
          Y := Exp2(Random);
    7 : for I := 1 to NMAX do
          Y := Power(Random, 0.5);
    8 : for I := 1 to NMAX do
          Y := Sin(Random);
    9 : for I := 1 to NMAX do
          Y := Cos(Random);
   10 : for I := 1 to NMAX do
          Y := Tan(Random);
   11 : for I := 1 to NMAX do
          Y := ArcSin(Random);
   12 : for I := 1 to NMAX do
          Y := ArcCos(Random);
   13 : for I := 1 to NMAX do
          Y := ArcTan(Random);
   14 : for I := 1 to NMAX do
          Y := ArcTan2(Random, 0.5);
   15 : for I := 1 to NMAX do
          Y := Sinh(Random);
   16 : for I := 1 to NMAX do
          Y := Cosh(Random);
   17 : for I := 1 to NMAX do
          Y := Tanh(Random);
   18 : for I := 1 to NMAX do
          Y := ArcSinh(Random);
   19 : for I := 1 to NMAX do
          Y := ArcCosh(Random + 1.0);
   20 : for I := 1 to NMAX do
          Y := ArcTanh(Random);
   21 : for I := 1 to NMAX do
          Y := Gamma(Random);
   22 : for I := 1 to NMAX do
          Y := DiGamma(Random);
   23 : for I := 1 to NMAX do
          Y := TriGamma(Random);
   24 : for I := 1 to NMAX do
          Y := IGamma(Random, 0.5);
   25 : for I := 1 to NMAX do
          Y := Beta(Random, 0.5);
   26 : for I := 1 to NMAX do
          Y := IBeta(Random, 0.5, 0.5);
   27 : for I := 1 to NMAX do
          Y := Erf(Random);
   28 : for I := 1 to NMAX do
          Y := Erfc(Random);
   29 : for I := 1 to NMAX do
          Y := Random;
  end;
  TimeToEval := Time - T0;
end;

begin
  { Open output file }
  Assign(F, 'speed.out');
  Rewrite(F);

  Writeln;
  Writeln('Time in seconds to evaluate ', NMAX, ' functions');
  Writeln(F, 'Time in seconds to evaluate ', NMAX, ' functions');
  Writeln;
  Writeln(F);

  for N := 1 to NFUNC do
    begin
      T := TimeToEval(N);
      Writeln(FuncName[N], T:6:2);
      Writeln(F, FuncName[N], T:8:2);
    end;

  Close(F);
end.
