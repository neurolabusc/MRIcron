{ Unit FFTs

  This unit provides a forward and inverse FFT pascal implementation
  for complex number series.

  The formal definition of the complex DFT is:
    y[k] = sum(x[m]*exp(-i*2*pi*k*m/n), m = 0..n-1), k = 0..n-1

  Copyright: Nils Haeck M.Sc. (email: n.haeck@simdesign.nl)
  For more information visit http://www.simdesign.nl
  Original date of publication: 10 Mar 2003

  This unit requires these other units:
  - Complexs: Complex number unit
  - Types:    Additional mathematical variable types
  - SysUtils: Delphi system utilities

  ****************************************************************

  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at:
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
}
unit FFTs;

interface

uses
  FFTComplexs, FFTTypes, SysUtils,Define_Types;

const

  cMaxPrimeFactor     = 1021;
  cMaxPrimeFactorDiv2 = (cMaxPrimeFactor + 1) div 2;
  cMaxFactorCount     = 20;

resourcestring

  sErrPrimeTooLarge = 'Prime factor for FFT length too large. Change value for cMaxPrimeFactor in FFTs unit';

{ ForwardFFT:
  Perform a complex FFT on the data in Source, put result in Dest. This routine
  works best for Count as a power of 2, but also works usually faster than DFT
  by factoring the series. Only in cases where Count is a prime number will this
  method be identical to regular complex DFT.

  The largest prime factor in Count should be less or equal to cMaxPrimeFactor.

  The remaining factors are handled by optimised partial FFT code, that can be
  found in the FFT_X procedures

  Inputs:
    Source: this can be any zero-based array type of TComplex
    Count: The number of elements in the array.

  Outputs:
    Dest: this can be any zero-based array type of TComplex, and will contain
      the FFT transformed data (frequency spectrum). Source may be equal to
      Dest. In this case, the original series will be overwritten with the new
      fourier-transformed series.
}
procedure ForwardFFT(const Source: array of TComplex; var Dest: array of TComplex; Count: integer);

{ Perform the inverse FFT on the Source data, and put result in Dest. This is based
  on the forward FFT with some additional customisation. The result of a forward
  FFT followed by an inverse FFT should yield the same data, except for rounding
  errors.
}
procedure InverseFFT(const Source: array of TComplex; var Dest: array of TComplex; Count: integer);

function FFTPower(var lDataIn,lDataOut: SingleP; lnIn: Integer): boolean;
//returns lDataOut with (lnIn div 2)-1 samples

implementation


function FFTPower(var lDataIn,lDataOut: SingleP; lnIn: Integer): boolean;
//returns lDataOut with (lnIn div 2)-1 samples
  var
    I,lnOut : Integer;
      Source, Dest : array of TComplex;
begin
   result := false;
   if lnIn < 6 then
      exit;
   SetLength(Source, lnIn);
   SetLength(Dest, lnIn);
     for I := 0 to (lnIn-1) do begin
         Source[I].Re := lDataIn[I+1]; //+1 as SingleP is indexed from 1
         Source[I].Im := 0;
     end;
     ForwardFFT(Source, Dest, lnIn);

     lnOut := ((lnIn) div 2)-1;
     //-1 because the first component of Dest, Dest[0], is simply the sum of the data, and can be removed
     for I := 1 to (lnOut) do
         lDataOut[I] :=  Sqrt(Sqr(Dest[I].Re)+Sqr(Dest[I].Im));//note we ignore [0].re and [0].im
   result := true;
end;

(*function FFTPower(var lDataIn,lDataOut: SingleP; lnIn: Integer): boolean;
//returns lDataOut with (lnIn div 2)-1 samples
  var
    I,lnOut : Integer;
      Source, Dest : array of TComplex;
begin
   result := false;
   if lnIn < 6 then
      exit;
   getmem(Source,lnIn* sizeof(TComplex));
   getmem(Dest,lnIn* sizeof(TComplex));
     for I := 0 to (lnIn-1) do begin
         Source[I].Re := lDataIn[I+1]; //+1 as SingleP is indexed from 1
         Source[I].Im := 0;
     end;
     ForwardFFT(Source, Dest, lnIn);
     lnOut := ((lnIn) div 2)-1;
     //-1 because the first component of Dest, Dest[0], is simply the sum of the data, and can be removed
     for I := 1 to (lnOut) do
         lDataOut[I] :=  Sqrt(Sqr(Dest[I].Re)+Sqr(Dest[I].Im));//note we ignore [0].re and [0].im
   freemem(Source);
   freemem(Dest);
   result := true;
end;*)

const
  // Some helper constants for the FFT optimisations
  c31: TFloat = -1.5000000000000E+00; //  cos(2*pi / 3) - 1;
  c32: TFloat =  8.6602540378444E-01; //  sin(2*pi / 3);

  u5:  TFloat =  1.2566370614359E+00; //  2*pi / 5;
  c51: TFloat = -1.2500000000000E+00; // (cos(u5) + cos(2*u5))/2 - 1;
  c52: TFloat =  5.5901699437495E-01; // (cos(u5) - cos(2*u5))/2;
  c53: TFloat = -9.5105651629515E-01; //- sin(u5);
  c54: TFloat = -1.5388417685876E+00; //-(sin(u5) + sin(2*u5));
  c55: TFloat =  3.6327126400268E-01; // (sin(u5) - sin(2*u5));
  c8:  TFloat =  7.0710678118655E-01; //  1 / sqrt(2);

type
  // Base 1 and Base 0 arrays
  TIdx0FactorArray = array[0..cMaxFactorCount] of integer;
  TIdx1FactorArray = array[1..cMaxFactorCount] of integer;

// Factorise the series with length Count into FactorCount factors, stored in Fact
procedure Factorize(Count: integer; var FactorCount: integer; var Fact: TIdx1FactorArray);
var
  i: integer;
  Factors: TIdx1FactorArray;
const
  // Define specific FFT lengths (radices) that we can process with optimised routines
  cRadixCount = 6;
  cRadices: array[1..6] of integer =
    (2, 3, 4, 5, 8, 10);
begin

  if Count = 1 then begin
    FactorCount := 1;
    Factors[1]  := 1;
  end else begin
    FactorCount := 0;
  end;

  // Factorise the original series length Count into known factors and rest value
  i := cRadixCount;
  while (Count > 1) AND (i > 0) do begin
    if Count mod cRadices[i] = 0 then begin
      Count := Count div cRadices[i];
      inc(FactorCount);
      Factors[FactorCount] := cRadices[i];
    end else
      dec(i);
  end;

  // substitute factors 2*8 with more optimal 4*4
  if Factors[FactorCount] = 2 then begin
    i := FactorCount - 1;
    while (i > 0) AND (Factors[i] <> 8) do
      dec(i);

    if i > 0 then begin
      Factors[FactorCount] := 4;
      Factors[i] := 4;
    end;
  end;

  // Analyse the rest value and see if it can be factored in primes
  if Count > 1 then begin
    for i := 2 to trunc(sqrt(Count)) do begin
      while Count mod i = 0 do begin
        Count := Count div i;
        inc(FactorCount);
        Factors[FactorCount] := i;
      end;
    end;

    if (Count > 1) then begin
      inc(FactorCount);
      Factors[FactorCount] := Count;
    end;
  end;

  // Reverse factors so that primes are first
  for i := 1 to FactorCount do
    Fact[i] := Factors[FactorCount - i + 1];

end;

{ Reorder the series in X to a permuted sequence in Y so that the later step can
  be done in place, and the final FFT result is in correct order.
  The series X and Y must be different series!
}
procedure ReorderSeries(Count: integer; var Factors: TIdx1FactorArray; var Remain: TIdx0FactorArray;
  const X: array of TComplex; var Y: array of TComplex);
var
  i, j, k: integer;
  Counts: TIdx1FactorArray;
begin
  FillChar(Counts, SizeOf(Counts), 0);

  k := 0;
  for i := 0 to Count - 2 do begin
    Y[i] := X[k];
    j := 1;
    k := k + Remain[j];
    Counts[1] := Counts[1] + 1;
    while Counts[j] >= Factors[j] do begin
      Counts[j] := 0;
      k := k - Remain[j - 1] + Remain[j + 1];
      inc(j);
      inc(Counts[j]);
    end;
  end;

  Y[Count - 1] := X[Count - 1];
end;

procedure FFT_2(var Z: array of TComplex);
var
  T1: TComplex;
begin
  T1   := ComplexAdd(Z[0], Z[1]);
  Z[1] := ComplexSub(Z[0], Z[1]);
  Z[0] := T1;
end;

procedure FFT_3(var Z: array of TComplex);
var
  T1, M1, M2, S1: TComplex;
begin
  T1   := ComplexAdd(Z[1], Z[2]);
  Z[0] := ComplexAdd(Z[0], T1);
  M1   := ComplexScl(c31, T1);
  M2.Re := c32 * (Z[1].Im - Z[2].Im);
  M2.Im := c32 * (Z[2].Re - Z[1].Re);
  S1   := ComplexAdd(Z[0], M1);
  Z[1] := ComplexAdd(S1, M2);
  Z[2] := ComplexSub(S1, M2);
end;

procedure FFT_4(var Z: array of TComplex);
var
  T1, T2, M2, M3: TComplex;
begin
  T1 := ComplexAdd(Z[0], Z[2]);
  T2 := ComplexAdd(Z[1], Z[3]);

  M2 := ComplexSub(Z[0], Z[2]);
  M3.Re := Z[1].Im - Z[3].Im;
  M3.Im := Z[3].Re - Z[1].Re;

  Z[0] := ComplexAdd(T1, T2);
  Z[2] := ComplexSub(T1, T2);
  Z[1] := ComplexAdd(M2, M3);
  Z[3] := ComplexSub(M2, M3);
end;

procedure FFT_5(var Z: array of TComplex);
var
  T1, T2, T3, T4, T5: TComplex;
  M1, M2, M3, M4, M5: TComplex;
  S1, S2, S3, S4, S5: TComplex;
begin
  T1 := ComplexAdd(Z[1], Z[4]);
  T2 := ComplexAdd(Z[2], Z[3]);
  T3 := ComplexSub(Z[1], Z[4]);
  T4 := ComplexSub(Z[3], Z[2]);

  T5   := ComplexAdd(T1, T2);
  Z[0] := ComplexAdd(Z[0], T5);
  M1   := ComplexScl(c51, T5);
  M2   := ComplexScl(c52, ComplexSub(T1, T2));

  M3.Re := -c53 * (T3.Im + T4.Im);
  M3.Im :=  c53 * (T3.Re + T4.Re);
  M4.Re := -c54 * T4.Im;
  M4.Im :=  c54 * T4.Re;
  M5.Re := -c55 * T3.Im;
  M5.Im :=  c55 * T3.Re;

  S3 := ComplexSub(M3, M4);
  S5 := ComplexAdd(M3, M5);;
  S1 := ComplexAdd(Z[0], M1);
  S2 := ComplexAdd(S1, M2);
  S4 := ComplexSub(S1, M2);

  Z[1] := ComplexAdd(S2, S3);
  Z[2] := ComplexAdd(S4, S5);
  Z[3] := ComplexSub(S4, S5);
  Z[4] := ComplexSub(S2, S3);
end;

procedure FFT_8(var Z: array of TComplex);
var
  A, B: array[0..3] of TComplex;
  Gem: TFloat;
begin
  A[0] := Z[0]; B[0] := Z[1];
  A[1] := Z[2]; B[1] := Z[3];
  A[2] := Z[4]; B[2] := Z[5];
  A[3] := Z[6]; B[3] := Z[7];

  FFT_4(A);
  FFT_4(B);

  Gem     := c8 * (B[1].Re + B[1].Im);
  B[1].Im := c8 * (B[1].Im - B[1].Re);
  B[1].Re := Gem;
  Gem     := B[2].Im;
  B[2].Im :=-B[2].Re;
  B[2].Re := Gem;
  Gem     := c8 * (B[3].Im - B[3].Re);
  B[3].Im :=-c8 * (B[3].Re + B[3].Im);
  B[3].Re := Gem;

  Z[0] := ComplexAdd(A[0], B[0]); Z[4] := ComplexSub(A[0], B[0]);
  Z[1] := ComplexAdd(A[1], B[1]); Z[5] := ComplexSub(A[1], B[1]);
  Z[2] := ComplexAdd(A[2], B[2]); Z[6] := ComplexSub(A[2], B[2]);
  Z[3] := ComplexAdd(A[3], B[3]); Z[7] := ComplexSub(A[3], B[3]);
end;

procedure FFT_10(var Z: array of TComplex);
var
  A, B: array[0..4] of TComplex;
begin
   A[0] := Z[0];  B[0] := Z[5];
   A[1] := Z[2];  B[1] := Z[7];
   A[2] := Z[4];  B[2] := Z[9];
   A[3] := Z[6];  B[3] := Z[1];
   A[4] := Z[8];  B[4] := Z[3];

   FFT_5(A);
   FFT_5(B);

   Z[0] := ComplexAdd(A[0], B[0]); Z[5] := ComplexSub(A[0], B[0]);
   Z[6] := ComplexAdd(A[1], B[1]); Z[1] := ComplexSub(A[1], B[1]);
   Z[2] := ComplexAdd(A[2], B[2]); Z[7] := ComplexSub(A[2], B[2]);
   Z[8] := ComplexAdd(A[3], B[3]); Z[3] := ComplexSub(A[3], B[3]);
   Z[4] := ComplexAdd(A[4], B[4]); Z[9] := ComplexSub(A[4], B[4]);
end;

{
  Synthesize the FFT by taking the even factors and the odd factors multiplied by
  complex sinusoid
}
procedure SynthesizeFFT(Sofar, Radix, Remain: integer; var Y: array of TComplex);
var
  GroupOffset, DataOffset, Position: integer;
  GroupNo, DataNo, BlockNo, SynthNo: integer;
  Omega: double;
  S, CosSin: TComplex;
  Synth, Trig, Z: array[0..cMaxPrimeFactor - 1] of TComplex;

  // Local function
  procedure InitializeTrigonomials(Radix: integer);
  // Initialize trigonomial coefficients
  var
    i: integer;
    W: double;
    X: TComplex;
  begin
    W := 2 * pi / Radix;
    Trig[0] := Complex(1.0, 0.0);
    X := Complex(cos(W), -sin(W));
    Trig[1] := X;
    for i := 2 to Radix - 1 do
      Trig[i] := ComplexMul(X, Trig[i - 1]);
  end;

  // Local Function
  procedure FFT_Prime(Radix: integer);
  // This is the general DFT, which can't be made any faster by factoring because
  // Radix is a prime number
  var
    i, j, k, N, AMax: integer;
    Re, Im: TComplex;
    V, W: array[0..cMaxPrimeFactorDiv2 - 1] of TComplex;
  begin
    N := Radix;
    AMax := (N + 1) div 2;
    for j := 1 to AMax - 1 do begin
      V[j].Re := Z[j].Re + Z[n-j].Re;
      V[j].Im := Z[j].Im - Z[n-j].Im;
      W[j].Re := Z[j].Re - Z[n-j].Re;
      W[j].Im := Z[j].Im + Z[n-j].Im;
    end;

    for j := 1 to AMax - 1 do begin
      Z[j]   := Z[0];
      Z[N-j] := Z[0];
      k := j;
      for i := 1 to AMax - 1 do begin
        Re.Re := Trig[k].Re * V[i].Re;
        Im.Im := Trig[k].Im * V[i].Im;
        Re.im := Trig[k].Re * W[i].Im;
        Im.Re := Trig[k].Im * W[i].Re;

        Z[N-j].Re := Z[N-j].Re + Re.Re + Im.Im;
        Z[N-j].Im := Z[N-j].Im + Re.Im - Im.Re;
        Z[j].Re   := Z[j].Re   + Re.Re - Im.Im;
        Z[j].Im   := Z[j].Im   + Re.Im + Im.Re;

        k := k + j;
        if k >= N then
          k := k - N;
      end;
    end;

    for j := 1 to AMax - 1 do begin
      Z[0].Re := Z[0].Re + V[j].Re;
      Z[0].Im := Z[0].Im + W[j].Im;
    end;
  end;

// main
begin
  // Initialize trigonomial coefficients
  InitializeTrigonomials(Radix);

  Omega       := 2 * pi / (Sofar * Radix);
  CosSin      := Complex(cos(Omega), -sin(Omega));
  S           := Complex(1.0, 0.0);
  DataOffset  := 0;
  GroupOffset := 0;
  Position    := 0;

  for DataNo := 0 to Sofar - 1 do begin

    if Sofar > 1 then begin

      Synth[0] := Complex(1.0, 0.0);
      Synth[1] := S;
      for SynthNo := 2 to Radix - 1 do
        Synth[SynthNo] := ComplexMul(S, Synth[SynthNo - 1]);
      S := ComplexMul(CosSin, S);

    end;

    for GroupNo := 0 to Remain - 1 do begin

      if (Sofar > 1) AND (DataNo > 0) then begin

        Z[0]    := Y[Position];
        BlockNo := 1;
        repeat
          inc(Position, Sofar);
          Z[BlockNo] := ComplexMul(Synth[BlockNo], Y[Position]);
          inc(BlockNo);
        until BlockNo >= Radix;

      end else begin

        for BlockNo := 0 to Radix - 1 do begin
          Z[BlockNo] := Y[Position];
          inc(Position, Sofar);
        end;

      end;

      case Radix of
      2:  FFT_2(Z);
      3:  FFT_3(Z);
      4:  FFT_4(Z);
      5:  FFT_5(Z);
      8:  FFT_8(Z);
      10: FFT_10(Z);
      else
        // Any larger prime number than 5 (so 7, 11, 13, etc, up to cMaxPrimeFactor)
        FFT_Prime(Radix);
      end; //case

      Position := GroupOffset;
      for BlockNo := 0 to Radix - 1 do begin
        Y[Position] := Z[blockNo];
        Inc(Position, Sofar);
      end;
      GroupOffset := GroupOffset + Sofar * Radix;
      Position    := GroupOffset;
    end;
    inc(DataOffset);
    GroupOffset := DataOffset;
    Position    := DataOffset;
  end;
end;

procedure ForwardFFT(const Source: array of TComplex; var Dest: array of TComplex; Count: integer);
// Perform a FFT on the data in Source, put result in Dest. This routine works best
// for Count as a power of 2, but also works usually faster than DFT by factoring
// the series. Only in cases where Count is a prime number will this method be
// identical to regular DFT.
type
  PComplexArray = ^TComplexArray;
  TComplexArray = array[0..0] of TComplex;
var
  i: integer;
  FactorCount: integer;
  SofarRadix:  TIdx1FactorArray;
  ActualRadix: TIdx1FactorArray;
  RemainRadix: TIdx0FactorArray;
  TmpDest: PComplexArray;
begin
  if Count = 0 then exit;

  // Decompose the series with length Count into FactorCount factors in ActualRadix
  Factorize(Count, FactorCount, ActualRadix);

  // Check if our biggest prime factor is not too large
  if (ActualRadix[1] > cMaxPrimeFactor) then
    raise EMathError.Create(sErrPrimeTooLarge);

  // Setup Sofar and Remain tables
  RemainRadix[0] := Count;
  SofarRadix[1]  := 1;
  RemainRadix[1] := Count div ActualRadix[1];
  for i := 2 to FactorCount do begin
    SofarRadix[i]  := SofarRadix[i-1] * ActualRadix[i-1];
    RemainRadix[i] := RemainRadix[i-1] div ActualRadix[i];
  end;

  // Make temp copy if dest = source (otherwise the permute procedure will completely
  // ruin the structure
  if @Dest = @Source then begin
    GetMem(TmpDest, SizeOf(TComplex) * Count);;
    Move(Dest, TmpDest^, SizeOf(TComplex) * Count);
  end else begin
    TmpDest := @Dest;
  end;

  // Reorder the series so that the elements are already in the right place for
  // synthesis
  ReorderSeries(Count{, FactorCount}, ActualRadix, RemainRadix, Source, TmpDest^);

  // Free the temporary copy (if any)
  if @Dest = @Source then begin
    Move(TmpDest^, Dest, SizeOf(TComplex) * Count);
    FreeMem(TmpDest);
  end;

  // Synthesize each of the FFT factored series
  for i := 1 to FactorCount do
    SynthesizeFFT(SofarRadix[i], ActualRadix[i], RemainRadix[i], Dest);

end;

procedure InverseFFT(const Source: array of TComplex; var Dest: array of TComplex; Count: integer);
// Perform the inverse FFT on the Source data, and put result in Dest. It performs
// the forward FFT and then divides elements by N
var
  i: integer;
  S: TFloat;
  TmpSource: array of TComplex;
begin
  if Count = 0 then exit;

  // Since TmpSource is local, we do not have to free it again,
  // it will be freed automatically when out of scope
  SetLength(TmpSource, Count);

  // Create a copy with inverted imaginary part.
  for i := 0 to Count - 1 do
    with Source[i] do
      TmpSource[i] := Complex(Re, -Im);
  ForwardFFT(TmpSource, Dest, Count);

  // Scale by 1/Count, and inverse the imaginary part
  S := 1.0 / Count;
  for i := 0 to Count - 1 do begin
    Dest[i].Re :=   S * Dest[i].Re;
    Dest[i].Im := - S * Dest[i].Im;
  end;
end;

end.
