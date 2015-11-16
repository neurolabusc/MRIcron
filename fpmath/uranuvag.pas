{ ******************************************************************
  UVAG The Universal Virtual Array Generator
  by Alex Hay zenjew@hotmail.com
  Adapted to TPMath by Jean Debord
  ******************************************************************
  In practice, Cardinal (6-7 times the output of Word) is the
  IntType of choice, but to demonstrate UVAG's scalability here,
  IntType can be defined as any integer data type. IRanUVAG globally
  provides (as rndint) an effectively infinite sequence of IntTypes,
  uniformly distributed (0, 2^(8*sizeof(IntType))-1). Output (bps)
  is dependent solely on IntSize=sizeof(IntType) and CPU speed.  UVAG
  cycles at twice the speed of the 64-bit Mersenne Twister in a tenth
  the memory, tests well in DIEHARD, ENT and NIST and has a huge period.
  It is suitable for cryptographic purposes in that state(n) is not
  determinable from state(n+1).  Most attractive is that it uses integers
  of any size and requires an array of only 255 + sizeof(IntType) bytes.
  Thus it is easily adapted to 128 bits and beyond with negligible
  memory increase.  Lastly, seeding is easy.  From near zero entropy
  (s[]=0, rndint > 0), UVAG bootstraps itself to full entropy in under
  300 cycles.  Very robust, no bad seeds.
  ****************************************************************** }

unit uranuvag;

interface

type
  IntType = LongInt;

procedure InitUVAGbyString(KeyPhrase : string);
{ Initializes the generator with a string }

procedure InitUVAG(Seed : IntType);
{ Initializes the generator with an integer }

function IRanUVAG : IntType;
{ Returns a 32-bit random integer }

implementation

const
  IntSize = SizeOf(IntType);

type
  TByteArray = array[0..(255 + IntSize)] of Byte;

var
  s      : TByteArray;
  sp     : ^IntType;   { Pointer to random IntType somewhere in s }
  sindex : Byte;
  rndint : IntType;

procedure InitUVAGbyString(KeyPhrase : string);
var
  i, kindex, lk : Word;
  temp, tot     : Byte;
begin
  lk := Length(KeyPhrase);
  kindex := 1;
  tot := 0;

  { Initialize array }
  for i := 0 to 255 do
    s[i] := i;
  for i := 256 to (255 + IntSize) do
    s[i] := i - 256;

  { Shuffle array on keyphrase }
  for i := 0 to (255 + IntSize) do
    begin
      tot := tot + Ord(KeyPhrase[kindex]);
      temp := s[i];
      s[i] := s[tot];
      s[tot] := temp;
      kindex := kindex + 1;
      if kindex > lk then kindex := 1;  { wrap around key }
    end;

  sindex := s[0];
  rndint := 0
end;

procedure InitUVAG(Seed : IntType);
var
  S : string;
begin
  Str(Seed, S);
  InitUVAGbyString(S);
end;

function IRanUVAG : IntType;
begin
  sindex := sindex + 1;
  sp := @s[s[sindex]];
  sp^ := sp^ + rndint;
  rndint := rndint + sp^;
  IRanUVAG := rndint
end;

end.
