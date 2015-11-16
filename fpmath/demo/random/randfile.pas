{ ******************************************************************
  This program generates a binary file of random numbers,
  to be used with Marsaglia's DIEHARD battery of tests
  (http://stat.fsu.edu/pub/diehard/)
  ****************************************************************** }

program randfile;

uses
  tpmath;

const
  N = 3000000;  { Generate N numbers }

var
  I, R : LongInt;
  F    : file of LongInt;

begin
  { Select a generator }

  SetRNG(RNG_MWC);  { or SetRNG(RNG_MT) or SetRNG(RNG_UVAG) }

  { Initialize the selected generator with the built-in generator }

  Randomize;

  InitGen(Trunc(Random * 2147483647));

  { Create file }

  Assign(F, 'random.dat');
  Rewrite(F);

  for I := 1 to N do
    begin
      R := IRanGen;
      Write(F, R);
    end;

  Close(F);
end.
