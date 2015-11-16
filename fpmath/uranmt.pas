{ ******************************************************************
    Mersenne Twister Random Number Generator
  ******************************************************************

    A C-program for MT19937, with initialization improved 2002/1/26.
    Coded by Takuji Nishimura and Makoto Matsumoto.

    Adapted for TPMath by Jean Debord - Feb. 2007

    Before using, initialize the state by using init_genrand(seed)
    or init_by_array(init_key, key_length) (respectively InitMT and
    InitMTbyArray in the TPMath version)

    Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

      1. Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.

      2. Redistributions in binary form must reproduce the above copyright
         notice, this list of conditions and the following disclaimer in the
         documentation and/or other materials provided with the distribution.

      3. The names of its contributors may not be used to endorse or promote
         products derived from this software without specific prior written
         permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    Any feedback is very welcome.
    http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
    email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
  ****************************************************************** }

unit uranmt;

interface

type
  MTKeyArray = array[0..623] of LongInt;

procedure InitMT(Seed : LongInt);
{ Initializes MT generator with a seed }

procedure InitMTbyArray(InitKey : MTKeyArray; KeyLength : Word);
{ Initialize MT generator with an array InitKey[0..(KeyLength - 1)] }

function IRanMT : LongInt;
{ Generates a Random number on [-2^31 .. 2^31 - 1] interval }

implementation

const
  N          = 624;
  M          = 397;
  MATRIX_A   = $9908b0df;  { constant vector a }
  UPPER_MASK = $80000000;  { most significant w-r bits }
  LOWER_MASK = $7fffffff;  { least significant r bits }

  mag01 : array[0..1] of LongInt = (0, MATRIX_A);

var
  mt  : MTKeyArray;  { the array for the state vector }
  mti : Word;        { mti == N+1 means mt[N] is not initialized }

procedure InitMT(Seed : LongInt);
var
  i : Word;
begin
  mt[0] := Seed and $ffffffff;
  for i := 1 to N-1 do
    begin
      mt[i] := (1812433253 * (mt[i-1] Xor (mt[i-1] shr 30)) + i);
        { See Knuth TAOCP Vol2. 3rd Ed. P.106 For multiplier.
          In the previous versions, MSBs of the seed affect
          only MSBs of the array mt[].
          2002/01/09 modified by Makoto Matsumoto }
      mt[i] := mt[i] and $ffffffff;
        { For >32 Bit machines }
    end;
  mti := N;
end;

procedure InitMTbyArray(InitKey : MTKeyArray; KeyLength : Word);
var
  i, j, k, k1 : Word;
begin
  InitMT(19650218);

  i := 1;
  j := 0;

  if N > KeyLength then k1 := N else k1 := KeyLength;

  for k := k1 downto 1 do
    begin
      mt[i] := (mt[i] Xor ((mt[i-1] Xor (mt[i-1] shr 30)) * 1664525)) + InitKey[j] + j; { non linear }
      mt[i] := mt[i] and $ffffffff; { for WORDSIZE > 32 machines }
      i := i + 1;
      j := j + 1;
      if i >= N then
        begin
          mt[0] := mt[N-1];
          i := 1;
        end;
      if j >= KeyLength then j := 0;
    end;

  for k := N-1 downto 1 do
    begin
      mt[i] := (mt[i] Xor ((mt[i-1] Xor (mt[i-1] shr 30)) * 1566083941)) - i; { non linear }
      mt[i] := mt[i] and $ffffffff; { for WORDSIZE > 32 machines }
      i := i + 1;
      if i >= N then
        begin
          mt[0] := mt[N-1];
          i := 1;
        end;
    end;

    mt[0] := $80000000; { MSB is 1; assuring non-zero initial array }
end;

function IRanMT : LongInt;
var
  y : LongInt;
  k : Word;
begin
  if mti >= N then  { generate N words at one Time }
    begin
      { If IRanMT() has not been called, a default initial seed is used }
      if mti = N + 1 then InitMT(5489);

      for k := 0 to (N-M)-1 do
        begin
          y := (mt[k] and UPPER_MASK) or (mt[k+1] and LOWER_MASK);
          mt[k] := mt[k+M] xor (y shr 1) xor mag01[y and $1];
        end;

      for k := (N-M) to (N-2) do
        begin
          y := (mt[k] and UPPER_MASK) or (mt[k+1] and LOWER_MASK);
          mt[k] := mt[k - (N - M)] xor (y shr 1) xor mag01[y and $1];
        end;

      y := (mt[N-1] and UPPER_MASK) or (mt[0] and LOWER_MASK);
      mt[N-1] := mt[M-1] xor (y shr 1) xor mag01[y and $1];

      mti := 0;
    end;

  y := mt[mti];
  mti := mti + 1;

  { Tempering }
  y := y xor (y shr 11);
  y := y xor ((y shl  7) and $9d2c5680);
  y := y xor ((y shl 15) and $efc60000);
  y := y xor (y shr 18);

  IRanMT := y
end;

end.
