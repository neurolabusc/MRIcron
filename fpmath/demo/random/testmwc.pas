{ ******************************************************************
  This program picks 20000 random numbers and displays the next 6,
  together with the correct values obtained with the default
  initialization,
  ****************************************************************** }

program testmwc;

uses
  tpmath;

const
  Correct : array[1..6] of LongInt =
  (921625997, 1094293978, 115775252, 499820504, -1929018715, 2008943384);

var
  I, R : LongInt;

begin
  WriteLn;
  Writeln('  Test of Marsaglia random number generator');
  WriteLn('---------------------------------------------');
  WriteLn('       Correct           Actual');
  WriteLn('---------------------------------------------');

  SetRNG(RNG_MWC);

  { Pick 20000 random numbers }
  for I := 1 to 20000 do
    R := IRanGen;

  { Display 6 more numbers with correct values }
  for I := 1 to 6 do
    begin
      R := IRanGen;
      Write('  ', Correct[I]:12, '     ', R:12, '           ');
      if Correct[I] = R then WriteLn('OK') else WriteLn('BAD');
    end;
  WriteLn('---------------------------------------------');
end.

