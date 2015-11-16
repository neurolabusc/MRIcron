{ ******************************************************************
  Fast Fourier Transform (modified from Pascal program by Don Cross)
  ******************************************************************
  The program generates a time signal consisting of a large 200 Hz 
  sine wave added to a small 2000 Hz cosine wave, which is graphed
  on the screen (Press a key after you are done viewing each graph)

  Next, it performs the FFT and graphs the resulting complex
  frequency samples.

  Then, it filters out all frequency components above 1000 Hz in
  the transformed data.

  Finally, it performs the inverse transform to get a filtered
  time signal back, and graphs the result.

  In addition, the program compares the FFT with the direct
  computation, on a set of random data.

  Results are stored in the output file fftout.txt
  ****************************************************************** }

program testfft;

uses
  tpmath, tpgraph;

const
  PathToBGI = 'c:\tp\bgi';  { Change as necessary }

  function F(T : Float) : Float;
  begin
    F := Sin(200 * 2 * PI * T) + 0.2 * Cos(2000 * 2 * PI * T);
  end;

const
  NumSamples   = 512;                        { Buffer size (power of 2) }
  SamplingRate = 22050;                      { Sampling rate (Hz) }
  MaxIndex     = NumSamples - 1;             { Max. array index }
  MidIndex     = NumSamples div 2;
  DT           = 1 / SamplingRate;           { Time unit (s) }
  DF           = SamplingRate / NumSamples;  { Frequency unit (Hz) }

var
  InArray, OutArray      : PCompVector;
  T, Freq                : PVector;
  OutputListingFile      : Text;
  I, FreqIndex, SymIndex : LongInt;

  procedure Test_CalcFrequency;
  var
    Z : Complex;
    I : Integer;
  begin
    { Fill input buffers with random data }
    for I := 0 to MaxIndex do
      begin
        InArray^[I].X := Random(10000);
        InArray^[I].Y := Random(10000);
      end;

    WriteLn(OutputListingFile);
    WriteLn(OutputListingFile, '*** Testing procedure CalcFrequency ***');
    WriteLn(OutputListingFile);

    FFT(NumSamples, InArray, OutArray);
    for I := 0 to MaxIndex do
      begin
        CalcFrequency(NumSamples, I, InArray, Z);
        WriteLn(OutputListingFile, I:4,
                OutArray^[I].X:15:6, Z.X:15:6,
                OutArray^[I].Y:20:6, Z.Y:15:6);
      end;
  end;

  procedure ListData(DataArray : PCompVector; Comment : String);
  var
    I : LongInt;
  begin
    WriteLn(OutputListingFile, '*** ', Comment, ' ***');
    WriteLn(OutputListingFile);
    WriteLn(OutputListingFile, 'index':20, 'real':20, 'imag':20);
    for I := 1 to NumSamples do
      begin
        WriteLn(OutputListingFile, I:20,
                DataArray^[I].X:20:5, DataArray^[I].Y:20:5);
      end;

    WriteLn(OutputListingFile);
    WriteLn(OutputListingFile, '------------------------------------------------------------------------');
    WriteLn(OutputListingFile);
  end;

procedure PlotData(T : PVector; Z : PCompVector; Title : String);
var
  X                 : PVector;  { Real part of Z }
  Xmin, Xmax, Xstep : Float;    { Ox scale }
  Ymin, Ymax, Ystep : Float;    { Oy scale }
  I                 : Integer;  { Loop variable }
begin
  if not InitGraphics(9, 2, PathToBGI) then
    begin
      Writeln('Unable to set graphic mode!');
      Exit;
    end;

  SetWindow(15, 85, 15, 85, True);

  DimVector(X, MaxIndex);

  for I := 0 to MaxIndex do
    X^[I] := Z^[I].X;

  AutoScale(T, 0, MaxIndex, LinScale, Xmin, Xmax, Xstep);
  AutoScale(X, 0, MaxIndex, LinScale, Ymin, Ymax, Ystep);

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetOxTitle('Time (s)');
  SetOyTitle('Amplitude');
  SetGraphTitle(Title);

  { Set point type to 0 so that only lines will be plotted }
  SetPointParam(1, 0, 1, 1);

  PlotOxAxis;
  PlotOyAxis;

  PlotGrid(BothGrid);

  WriteGraphTitle;

  PlotCurve(T, X, 0, MaxIndex, 1);

  ReadLn;

  LeaveGraphics;
end;

procedure PlotFFT(Freq : PVector; Z : PCompVector; Title : String);
var
  Fq : PVector;  { Frequency }
  X  : PVector;  { Real part of FFT }
  Y  : PVector;  { Imag. part of FFT }

  Xmin, Xmax, Xstep       : Float;    { Ox scale }
  Yr_min, Yr_max, Yr_step : Float;    { Oy scale (real part) }
  Yi_min, Yi_max, Yi_step : Float;    { Oy scale (imag. part) }
  Ymin, Ymax, Ystep       : Float;    { Oy scale (global) }
  I                       : Integer;  { Loop variable }
begin
  DimVector(Fq, MidIndex);  { Frequency }
  DimVector(X, MidIndex);   { Real part of FFT }
  DimVector(Y, MidIndex);   { Imag. part of FFT }

  if not InitGraphics(9, 2, PathToBGI) then
    begin
      Writeln('Unable to set graphic mode!');
      Exit;
    end;

  SetWindow(15, 80, 15, 85, True);

  for I := 0 TO MidIndex do
    begin
      Fq^[I] := Freq^[I];
      X^[I] := Z^[I].X;
      Y^[I] := Z^[I].Y;
    end;

  AutoScale(Fq, 0, MidIndex, LinScale, Xmin, Xmax, Xstep);
  AutoScale(X, 0, MidIndex, LinScale, Yr_min, Yr_max, Yr_step);
  AutoScale(Y, 0, MidIndex, LinScale, Yi_min, Yi_max, Yi_step);

  Ymin  := FMin(Yr_min, Yi_min);
  Ymax  := FMax(Yr_max, Yi_max);
  Ystep := FMin(Yr_step, Yi_step);

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetOxTitle('Frequency (Hz)');
  SetOyTitle('FFT');

  SetPointParam(1, 0, 1, 1);  { Set point type to 0 so that }
  SetPointParam(2, 0, 1, 1);  { only lines will be plotted. }

  SetCurvLegend(1, 'Real');
  SetCurvLegend(2, 'Imag.');

  PlotOxAxis;
  PlotOyAxis;

  PlotGrid(BothGrid);

  WriteLegend(2, False, True);

  PlotCurve(Fq, X, 0, MidIndex, 1);
  PlotCurve(Fq, Y, 0, MidIndex, 2);

  ReadLn;

  LeaveGraphics;
end;

begin
  DimCompVector(InArray, MaxIndex);
  DimCompVector(OutArray, MaxIndex);
  DimVector(T, MaxIndex);
  DimVector(Freq, MaxIndex);

  Assign(OutputListingFile, 'fftout.txt');
  Rewrite(OutputListingFile);

  for I := 0 to MaxIndex do
    begin
      T^[I] := I * DT;
      Freq^[I] := I * DF;
      InArray^[I].X := F(T^[I]);
      InArray^[I].Y := 0.0;
    end;

  ListData(InArray, 'Time domain data before transform');
  PlotData(T, InArray, 'Original signal');

  FFT(NumSamples, InArray, OutArray);
  PlotFFT(Freq, OutArray, 'Fourier Transform');

  ListData(OutArray, 'Frequency domain data after transform');

  { Filter out everything above 1000 Hz (low-pass) }
  FreqIndex := Trunc(1000.0 / DF);
  SymIndex := NumSamples - FreqIndex;

  for I := 0 to MaxIndex do
    begin
      if ((I > FreqIndex) and (I < MidIndex)) or
         ((I >= MidIndex) and (I < SymIndex)) then
        begin
          OutArray^[I].X := 0.0;
          OutArray^[I].Y := 0.0;
        end;
    end;

  IFFT(NumSamples, OutArray, InArray);

  ListData(InArray, 'Time domain data after inverse transform');
  PlotData(T, InArray, 'Filtered signal');

  Test_CalcFrequency;

  Close(OutputListingFile);
end.
