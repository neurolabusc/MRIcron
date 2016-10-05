unit rng;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}
interface

type
TRNG = record
     test: boolean;
       c,cd,cm: double;
       u: array[0..96] of double;
       i97, j97: integer;
end;
function RandomInitialise(ij, kl: integer): TRNG;
function RandomGaussian(var rng: TRNG; mean, stddev: double): double;
function RandomDouble(var rng: TRNG; lower, upper: double): double;
function RandomInt(var rng: TRNG; lower, upper: integer): integer;
function RandomInt0(var rng: TRNG; upper: integer): integer;
function RandomUniform(var rng: TRNG): double;

(*
Chris Rorden's 2016 Pascal port of random number generator described at
 http://paulbourke.net/miscellaneous/random/
distributed under BSD license

Usage/Testing (Lazarus)
  procedure TForm1.TestRNG;
  var
     rng: TRNG = (test: FALSE);
     i: integer;
  begin
       rng := RandomInitialise(1802, 9373);
       for i := 1 to 20000 do
           RandomUniform(rng);
       for i := 1 to 6 do
           memo1.lines.Add(floattostr(RandomUniform(rng)*4096*4096));
  end;
this will result in:
     6533892 14220222 7275067 6172232 8354498 10633180
see also
  https://github.com/gabr42/GpDelphiUnits/blob/master/src/GpRandomGen.pas
 ---
   This Random Number Generator is based on the algorithm in a FORTRAN
   version published by George Marsaglia and Arif Zaman, Florida State
   University; ref.: see original comments below.
   At the fhw (Fachhochschule Wiesbaden, W.Germany), Dept. of Computer
   Science, we have written sources in further languages (C, Modula-2
   Turbo-Pascal(3.0, 5.0), Basic and Ada) to get exactly the same test
   results compared with the original FORTRAN version.
   April 1989
   Karl-L. Noell <NOELL@DWIFH1.BITNET>
      and  Helmut  Weber <WEBER@DWIFH1.BITNET>

   This random number generator originally appeared in "Toward a Universal
   Random Number Generator" by George Marsaglia and Arif Zaman.
   Florida State University Report: FSU-SCRI-87-50 (1987)
   It was later modified by F. James and published in "A Review of Pseudo-
   random Number Generators"
   THIS IS THE BEST KNOWN RANDOM NUMBER GENERATOR AVAILABLE.
   (However, a newly discovered technique can yield
   a period of 10^600. But that is still in the development stage.)
   It passes ALL of the tests for random number generators and has a period
   of 2^144, is completely portable (gives bit identical results on all
   machines with at least 24-bit mantissas in the floating point
   representation).
   The algorithm is a combination of a Fibonacci sequence (with lags of 97
   and 33, and operation "subtraction plus one, modulo one") and an
   "arithmetic sequence" (using subtraction).

   Use IJ = 1802 & KL = 9373 to test the random number generator. The
   subroutine RANMAR should be used to generate 20000 random numbers.
   Then display the next six random numbers generated multiplied by 4096*4096
   If the random number generator is working properly, the random numbers
   should be:
           6533892.0  14220222.0  7275067.0
           6172232.0  8354498.0   10633180.0
*)

implementation

function RandomInitialise(ij, kl: integer): TRNG;
var
  s,t: double;
  ii,i,j,k,l,jj,m: integer;
begin
  (*Handle the seed range errors
  First random number seed must be between 0 and 31328
  Second seed must have a value between 0 and 30081*)
  if (ij < 0) or (ij > 31328) or (kl < 0) or (kl > 30081) then begin
    ij := 1802;
    kl := 9373;
  end;
  i := (ij div 177) mod 177 + 2;
  j := ij mod 177 + 2;
  k := (kl div 169) mod 178 + 1;
  l := (kl mod 169);
  for ii:= 0 to 96 do begin
    s := 0.0;
    t := 0.5;
    for jj := 0 to 23 do begin
      m := (((i*j) mod 179)*k) mod 179;
      i := j;
      j := k;
      k := m;
      l := (53 * l + 1) mod 169;
      if ((l * m) mod 64 >= 32) then
         s := s + t;
      t := t * 0.5;
    end;
    result.u[ii] := s;
  end;

  result.c    := 362436.0 / 16777216.0;
  result.cd   := 7654321.0 / 16777216.0;
  result.cm   := 16777213.0 / 16777216.0;
  result.i97  := 97;
  result.j97  := 33;
  result.test := TRUE;
end;

function RandomUniform(var rng: TRNG): double;
var
  uni: double;
begin
   if (not rng.test) then //Make sure the initialisation routine has been called
      rng := RandomInitialise(1802,9373);
   uni := rng.u[rng.i97-1] - rng.u[rng.j97-1];
   if (uni <= 0.0) then
      uni := uni + 1;
   rng.u[rng.i97-1] := uni;
   dec(rng.i97);
   if (rng.i97 = 0) then
      rng.i97 := 97;
   dec(rng.j97);
   if (rng.j97 = 0) then
      rng.j97 := 97;
   rng.c := rng.c - rng.cd;
   if (rng.c < 0.0) then
      rng.c := rng.c + rng.cm;
   uni := uni - rng.c;
   if (uni < 0.0) then
      uni := uni + 1.0;
   result := uni;
end;

function RandomGaussian(var rng: TRNG; mean, stddev: double): double;
(*ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
  THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
  VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.
  The function returns a normally distributed pseudo-random number
  with a given mean and standard devaiation.  Calls are made to a
  function subprogram which must return independent random
  numbers uniform in the interval (0,1).
  The algorithm uses the ratio of uniforms method of A.J. Kinderman
  and J.F. Monahan augmented with quadratic bounding curves.*)
var
   q,u,v,x,y : double;
begin
   (*Generate P = (u,v) uniform in rect. enclosing acceptance region
      Make sure that any random numbers <= 0 are rejected, since
      gaussian() requires uniforms > 0, but RandomUniform() delivers >= 0*)
   repeat
      u := RandomUniform(rng);
      v := RandomUniform(rng);
      if (u <= 0.0) or (v <= 0.0) then begin
         u := 1.0;
         v := 1.0;
      end;
      v := 1.7156 * (v - 0.5);
      //  Evaluate the quadratic form
      x := u - 0.449871;
      y := abs(v) + 0.386595;
      q := x * x + y * (0.19600 * y - 0.25472 * x);
      // Accept P if inside inner ellipse
      if (q < 0.27597) then
         break;
      //  Reject P if outside outer ellipse, or outside acceptance region
    until ((q > 0.27846) or (v * v > -4.0 * Ln(u) * u * u));
    //  Return ratio of P's coordinates as the normal deviate
    result :=  (mean + stddev * v / u);
end;

function RandomInt(var rng: TRNG; lower, upper: integer): integer;
//Return random integer within a range, lower -> upper INCLUSIVE
begin
  //http://stackoverflow.com/questions/11128741/cast-variable-to-int-vs-round-function
  //Casting to int truncates a floating-point number, that is, it drops the fractional part.
  // return((int)(RandomUniform() * (upper - lower + 1)) + lower);
  result := trunc((RandomUniform(rng) * (upper - lower + 1)) + lower);
  //NOT
  // result := round((RandomUniform(rng) * (upper - lower + 1)) + lower);
end;

function RandomInt0(var rng: TRNG; upper: integer): integer;
//Return random integer within a range, 0 -> upper INCLUSIVE
begin
  //http://stackoverflow.com/questions/11128741/cast-variable-to-int-vs-round-function
  //Casting to int truncates a floating-point number, that is, it drops the fractional part.
  // return((int)(RandomUniform() * (upper - lower + 1)) + lower);
  result := trunc(RandomUniform(rng) * (upper + 1));
  //NOT
  // result := round((RandomUniform(rng) * (upper - lower + 1)) + lower);
end;

function RandomDouble(var rng: TRNG; lower, upper: double): double;
begin
   result := ((upper - lower) * RandomUniform(rng) + lower);
end;





end.

