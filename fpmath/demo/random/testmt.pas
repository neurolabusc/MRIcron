{ ******************************************************************
  Test of 'Mersenne Twister' random number generator

  This program prints 1000 random numbers, computed with the default
  initialization.

  The output of this program should be similar to file mt.txt
  ****************************************************************** }

program testmt;

uses
  tpmath;

var
  I : Word;
  R : LongInt;
  X : Float;

begin
  SetRNG(RNG_MT);

  Writeln('1000 outputs of IRanGen');
  for I := 1 to 1000 do
    begin
      R := IRanGen;
      Write(R:15);
      if i mod 5 = 0 then Writeln;
    end;

  Writeln;

  Writeln('1000 outputs of RanGen2');
  for I := 1 to 1000 do
    begin
      X := RanGen2;
      Write(X:15:8);
      if i mod 5 = 0 then Writeln;
    end;
end.
