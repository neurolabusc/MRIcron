(*==========================================================================

    fourier.pas  -  Don Cross <dcross@intersrv.com>

    Modified by Jean Debord <JDebord@compuserve.com> for use with TP Math.

    This is a Turbo Pascal Unit for calculating the Fast Fourier Transform
    (FFT) and the Inverse Fast Fourier Transform (IFFT).
    Visit the following URL for the latest version of this code.
    This page also has a C/C++ version, and a brief discussion of the
    theory behind the FFT algorithm.

       http://www.intersrv.com/~dcross/fft.html#pascal

    Revision history [most recent first]:

2007 December 10 [Jean Debord]
     Increased the theoretical number of points to about 2^26
     for a 32-bit compiler. The exact exponent is returned by
     function MaxPower.

2007 January 4 [Jean Debord]
     Modified for new TPMath version. Renamed as ufft.pas
     Now uses complex arrays.

1996 December 11 [Don Cross]
    Improved documentation of the procedure CalcFrequency.
    Fixed some messed up comments in procedure IFFT.

1996 December 6 [Don Cross]
    Made procedure 'fft_integer' more efficient when buffer size changes
    in successive calls:  the buffer is now only resized when the input
    has more samples, not a differing number of samples.
    Also changed the way 'fft_integer_cleanup' works so that it is
    more "bullet-proof".

1996 December 4 [Don Cross]
    Adding the procedure 'CalcFrequency', which calculates the FFT
    at a specific frequency index p=0..n-1, instead of the whole
    FFT.  This is O(n^2) instead of O(n*log(n)).

1996 November 30 [Don Cross]
    Adding a routine to allow FFT of an input array of integers.
    It is called 'fft_integer'.

1996 November 18 [Don Cross]
    Added some comments.

1996 November 17 [Don Cross]
    Wrote and debugged first version.

==========================================================================*)

unit ufft;

interface

uses
  utypes, umath;

(*---------------------------------------------------------------------------
  procedure FFT

  Calculates the Fast Fourier Transform of the array of complex
  numbers represented by 'InArray' to produce the output complex
  numbers in 'OutArray'.
---------------------------------------------------------------------------*)
procedure FFT(NumSamples        : LongInt;
              InArray, OutArray : PCompVector);

(*---------------------------------------------------------------------------
  procedure IFFT

  Calculates the Inverse Fast Fourier Transform of the array of
  complex numbers represented by 'InArray' to produce the output
  complex numbers in 'OutArray'.
---------------------------------------------------------------------------*)
procedure IFFT(NumSamples        : LongInt;
               InArray, OutArray : PCompVector);

(*---------------------------------------------------------------------------
  procedure FFT_Integer

  Same as procedure FFT, but uses Integer input arrays instead of
  double.  Make sure you call FFT_Integer_Cleanup after the last
  time you call FFT_Integer to free up memory it allocates.
---------------------------------------------------------------------------*)
procedure FFT_Integer(NumSamples     : LongInt;
                      RealIn, ImagIn : PIntVector;
                      OutArray       : PCompVector);

(*--------------------------------------------------------------------------
   procedure FFT_Integer_Cleanup

   If you call the procedure 'FFT_Integer', you must call
   'FFT_Integer_Cleanup' after the last time you call 'FFT_Integer'
   in order to free up dynamic memory.
--------------------------------------------------------------------------*)
procedure FFT_Integer_Cleanup;

(*--------------------------------------------------------------------------
   procedure CalcFrequency

   This procedure calculates the complex frequency sample at a given
   index directly.  Use this instead of 'FFT' when you only need one
   or two frequency samples, not the whole spectrum.

   It is also useful for calculating the Discrete Fourier Transform (DFT)
   of a number of data which is not an integer power of 2. For example,
   you could calculate the DFT of 100 points instead of rounding up to
   128 and padding the extra 28 array slots with zeroes.
--------------------------------------------------------------------------*)
procedure CalcFrequency(NumSamples, FrequencyIndex : LongInt;
                        InArray                    : PCompVector;
                        var FT                     : Complex);

implementation

const
  TempArraySize : Integer = 0;  { Flag that buffer Temp is not allocated }

var
  Temp : PCompVector;

  function MaxPower : LongInt;
  var
    M : Float;
  begin
    M := MAX_COMP;
    MaxPower := Trunc(Log2(M));
  end;

  function IsPowerOfTwo(X : LongInt) : Boolean;
  var
    I, Y : LongInt;
  begin
    Y := 2;
    for I := 1 to MaxPower do
      begin
        if X = Y then
          begin
            IsPowerOfTwo := True;
            Exit;
          end;
        Y := Y shl 1;
      end;
    IsPowerOfTwo := False;
  end;

  function NumberOfBitsNeeded(PowerOfTwo : LongInt) : Integer;
  var
    I : Integer;
  begin
    for I := 0 to MaxPower do
      begin
        if (PowerOfTwo and (1 shl I)) <> 0 then
          begin
            NumberOfBitsNeeded := I;
            Exit;
          end;
      end;
  end;

  function ReverseBits(Index, NumBits : LongInt) : Integer;
  var
    I, Rev : Integer;
  begin
    Rev := 0;
    for I := 0 to NumBits - 1 do
      begin
        Rev := (Rev shl 1) or (Index and 1);
        Index := Index shr 1;
      end;
    ReverseBits := Rev;
  end;

  procedure FourierTransform(AngleNumerator    : Float;
                             NumSamples        : LongInt;
                             InArray, OutArray : PCompVector);
  var
    NumBits, I, J, K, N, BlockSize, BlockEnd           : LongInt;
    Delta_angle, Delta_ar, Alpha, Beta, Tr, Ti, Ar, Ai : Float;
  begin
    if not IsPowerOfTwo(NumSamples) or (NumSamples < 2) then
      begin
        SetErrCode(-1);
        Exit;
      end;

    SetErrCode(0);

    NumBits := NumberOfBitsNeeded(NumSamples);
    for I := 0 to NumSamples - 1 do
      begin
        J := ReverseBits(I, NumBits);
        OutArray^[J].X := InArray^[I].X;
        OutArray^[J].Y := InArray^[I].Y;
      end;

    BlockEnd := 1;
    BlockSize := 2;
    while BlockSize <= NumSamples do
      begin
        Delta_angle := AngleNumerator / BlockSize;
        Alpha := Sin(0.5 * Delta_angle);
        Alpha := 2.0 * Alpha * Alpha;
        Beta := Sin(Delta_angle);

        I := 0;
        while I < NumSamples do
          begin
            Ar := 1.0;    (* cos(0) *)
            Ai := 0.0;    (* sin(0) *)

            J := I;
            for N := 0 to BlockEnd - 1 do
              begin
                K := J + BlockEnd;
                Tr := Ar * OutArray^[K].X - Ai * OutArray^[K].Y;
                Ti := Ar * OutArray^[K].Y + Ai * OutArray^[K].X;
                OutArray^[K].X := OutArray^[J].X - Tr;
                OutArray^[K].Y := OutArray^[J].Y - Ti;
                OutArray^[J].X := OutArray^[J].X + Tr;
                OutArray^[J].Y := OutArray^[J].Y + Ti;
                Delta_ar := Alpha * Ar + Beta * Ai;
                Ai := Ai - (Alpha * Ai - Beta * Ar);
                Ar := Ar - Delta_ar;
                Inc(J);
              end;

            I := I + BlockSize;
          end;

        BlockEnd := BlockSize;
        BlockSize := BlockSize shl 1;
      end;
  end;

  procedure FFT(NumSamples        : LongInt;
                InArray, OutArray : PCompVector);
  begin
    FourierTransform(2 * PI, NumSamples, InArray, OutArray);
  end;

  procedure IFFT(NumSamples        : LongInt;
                 InArray, OutArray : PCompVector);
  var
    I : Integer;
  begin
    FourierTransform(- 2 * PI, NumSamples, InArray, OutArray);
    if MathErr <> 0 then Exit;

    { Normalize the resulting time samples }
    for I := 0 to NumSamples - 1 do
      begin
        OutArray^[I].X := OutArray^[I].X / NumSamples;
        OutArray^[I].Y := OutArray^[I].Y / NumSamples;
      end;
  end;

  procedure FFT_Integer(NumSamples     : LongInt;
                        RealIn, ImagIn : PIntVector;
                        OutArray       : PCompVector);
  var
    I : Integer;
  begin
    if NumSamples > TempArraySize then
      begin
        FFT_Integer_Cleanup;  { free up memory in case we already have some }
        DimCompVector(Temp, NumSamples);
        TempArraySize := NumSamples;
      end;

    for I := 0 to NumSamples - 1 do
      begin
        Temp^[I].X := RealIn^[I];
        Temp^[I].Y := ImagIn^[I];
      end;

    FourierTransform(2 * PI, NumSamples, Temp, OutArray);
  end;

  procedure FFT_Integer_Cleanup;
  begin
    if TempArraySize > 0 then
      begin
        DelCompVector(Temp, TempArraySize);
        TempArraySize := 0;
      end;
  end;

  procedure CalcFrequency(NumSamples, FrequencyIndex : LongInt;
                          InArray                    : PCompVector;
                          var FT                     : Complex);
  var
    K                : Integer;
    Cos1, Cos2, Cos3 : Float;
    Sin1, Sin2, Sin3 : Float;
    Theta, Beta      : Float;
  begin
    FT.X := 0.0;
    FT.Y := 0.0;
    Theta := 2 * PI * FrequencyIndex / NumSamples;
    Sin1 := Sin(- 2 * Theta);
    Sin2 := Sin(- Theta);
    Cos1 := Cos(- 2 * Theta);
    Cos2 := Cos(- Theta);
    Beta := 2 * Cos2;
    for K := 0 to NumSamples - 1 do
      begin
        { Update trig values }
        Sin3 := Beta * Sin2 - Sin1;
        Sin1 := Sin2;
        Sin2 := Sin3;

        Cos3 := Beta * Cos2 - Cos1;
        Cos1 := Cos2;
        Cos2 := Cos3;

        FT.X := FT.X + InArray^[K].X * Cos3 - InArray^[K].Y * Sin3;
        FT.Y := FT.Y + InArray^[K].Y * Cos3 + InArray^[K].X * Sin3;
      end;
  end;

end.
