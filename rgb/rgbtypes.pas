{
 /***************************************************************************
                                  RGBTypes.pas


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author:  Tom Gregorovic (_tom_@centrum.cz)

  Abstract:
    TRGB32Pixel - TRGB32Bitmap picture element, contains red, green and blue
      component and is platform dependent!
    TRGBBitmapCore - universal RGB bitmap core.
    TRGB32BitmapCore - 32-bit core of TRGB32Bitmap.
}
unit RGBTypes;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

{$ifdef LCLwin32}
 {$define RGB}
{$endif}
{$ifdef LCLqt}
 {$define RGB}
{$endif}

interface

uses
  Classes, SysUtils, FPImage, IntfGraphics, Graphics, Math, LCLProc,
  RGBUtils;

type
  PRGBPixel = PByte;
  
  PRGB32Pixel = ^TRGB32Pixel;
  TRGB32Pixel = DWord;
  
  PRGB8Pixel = ^TRGB8Pixel;
  TRGB8Pixel = Byte;

  PRGB32PixelArray = ^TRGB32PixelArray;
  TRGB32PixelArray = packed array [0..0] of TRGB32Pixel;
  

  TPixelDifference = Word;
const
  MAXDIFFERENCE = 255 + 255 + 255;
type
  // integral float with 1/256 precision
  TIntensityFloat = Integer;
  TIntensityFloatTable = Array [0..255] of TIntensityFloat;

  { TRGBBitmapCore }

  TRGBBitmapCore = class(TPersistent)
  private
    FPixels: PRGBPixel;
    FSizeOfPixel: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FRowPixelStride: Integer;
    FDataOwner: Boolean;
    function GetSize: Integer;
  public
    constructor Create(AWidth, AHeight: Integer; ASizeOfPixel: Integer); virtual;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore; ASizeOfPixel: Integer); virtual;
    constructor CreateFromData(AData: Pointer; AWidth, AHeight: Integer; ASizeOfPixel: Integer; ADataOwner: Boolean = False); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); virtual;
  public
    function GetPixelPtrUnsafe(X, Y: Integer): PRGBPixel;
    function GetPixelPtr(X, Y: Integer): PRGBPixel;
    
    procedure Clear; virtual;
    procedure ClearWhite; virtual;
    procedure Invert; virtual;
    
    procedure FlipHorz; virtual;
    procedure FlipVert; virtual;
    procedure Rotate90; virtual;
    procedure Rotate180; virtual;
    procedure Rotate270; virtual;
  public
    property DataOwner: Boolean read FDataOwner;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Pixels: PRGBPixel read FPixels;
    property RowPixelStride: Integer read FRowPixelStride;
    property Size: Integer read GetSize;
    property SizeOfPixel: Integer read FSizeOfPixel;
  end;
  
  { TRGB8BitmapCore }

  TRGB8BitmapCore = class(TRGBBitmapCore)
  public
    constructor Create(AWidth, AHeight: Integer); virtual;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore); virtual;
    constructor CreateFromData(AData: Pointer; AWidth, AHeight: Integer; ADataOwner: Boolean = False); virtual;

    procedure LoadFromLazIntfImageAlpha(AImage: TLazIntfImage); virtual;
    procedure SaveToLazIntfImageAlpha(AImage: TLazIntfImage); virtual;
    procedure SaveToLazIntfImageAlpha(AImage: TLazIntfImage; const ARect: TRect); virtual;
    
    procedure Assign(Source: TPersistent); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); override;
  public
    function Get8PixelPtrUnsafe(X, Y: Integer): PRGB8Pixel;
    function Get8PixelPtr(X, Y: Integer): PRGB8Pixel;
    function Get8PixelUnsafe(X, Y: Integer): TRGB8Pixel;

    procedure Set8PixelUnsafe(X, Y: Integer; Value: TRGB8Pixel);
    procedure Set8Pixel(X, Y: Integer; Value: TRGB8Pixel);
  end;
  
  { TRGB32BitmapCore }

  TRGB32BitmapCore = class(TRGBBitmapCore)
  public
    constructor Create(AWidth, AHeight: Integer); virtual;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore); virtual;
    constructor CreateFromLazIntfImage(AImage: TLazIntfImage); virtual;
    constructor CreateFromData(AData: Pointer; AWidth, AHeight: Integer; ADataOwner: Boolean = False); virtual;

    procedure Assign(Source: TPersistent); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); override;
    procedure SaveToLazIntfImage(AImage: TLazIntfImage); virtual;
    procedure SaveToLazIntfImage(AImage: TLazIntfImage; const ARect: TRect); virtual;
  public
    function Get32PixelPtrUnsafe(X, Y: Integer): PRGB32Pixel;
    function Get32PixelPtr(X, Y: Integer): PRGB32Pixel;
    function Get32PixelUnsafe(X, Y: Integer): TRGB32Pixel;

    procedure Set32PixelUnsafe(X, Y: Integer; Value: TRGB32Pixel);
    procedure Set32Pixel(X, Y: Integer; Value: TRGB32Pixel);
  end;
  
  procedure FlipHorzRGBBitmap(Bitmap: TRGBBitmapCore);
  procedure FlipVertRGBBitmap(Bitmap: TRGBBitmapCore);

  // intensity tables
  function GetIntensityFloatTable(A, B: Single): TIntensityFloatTable;

  // rotate clockwise
  procedure Rotate90CWRGBBitmap(Bitmap: TRGBBitmapCore);
  procedure Rotate180CWRGBBitmap(Bitmap: TRGBBitmapCore);
  procedure Rotate270CWRGBBitmap(Bitmap: TRGBBitmapCore);

  procedure InvertRGBBitmap(Bitmap: TRGBBitmapCore);
  procedure GrayscaleRGB32Bitmap(Bitmap: TRGB32BitmapCore);
  procedure DisableRGB32Bitmap(Bitmap: TRGB32BitmapCore);

  function RGB32PixelToColor(P: TRGB32Pixel): TColor;
  function ColorToRGB32Pixel(C: TColor): TRGB32Pixel;

implementation

function GetRedInline(P: TRGB32Pixel): Byte; inline;
begin
  {$IFDEF RGB}
  Result := (P and $FF0000) shr 16;
  {$ELSE}
  Result := P and $FF;
  {$ENDIF}
end;

function GetGreenInline(P: TRGB32Pixel): Byte; inline;
begin
  {$IFDEF RGB}
  Result := (P and $FF00) shr 8;
  {$ELSE}
  Result := (P and $FF00) shr 8;
  {$ENDIF}
end;

function GetBlueInline(P: TRGB32Pixel): Byte; inline;
begin
  {$IFDEF RGB}
  Result := P and $FF;
  {$ELSE}
  Result := (P and $FF0000) shr 16;
  {$ENDIF}
end;

function RGBToRGB32PixelInline(R, G, B: Byte): TRGB32Pixel; inline;
begin
  {$IFDEF RGB}
  Result := B or (G shl 8) or (R shl 16);
  {$ELSE}
  Result := R or (G shl 8) or (B shl 16);
  {$ENDIF}
end;

function RGB32PixelToColorInline(P: TRGB32Pixel): TColor; inline;
begin
  {$IFDEF RGB}
  Result := ((P and $FF0000) shr 16) or (P and $FF00) or ((P and $FF) shl 16);
  {$ELSE}
  Result := P and $FFFFFF;
  {$ENDIF}
end;

function ColorToRGB32PixelInline(C: TColor): TRGB32Pixel; inline;
begin
  {$IFDEF RGB}
  Result := ((C and $FF0000) shr 16) or (C and $FF00) or ((C and $FF) shl 16);
  {$ELSE}
  Result := C and $FFFFFF;
  {$ENDIF}
end;

function FPColorToRGB32PixelInline(F: TFPColor): TRGB32Pixel; inline;
begin
  {$IFDEF RGB}
  Result := ((F.Blue shr 8) and $FF) or (F.Green and $FF00) or ((F.Red shl 8) and $FF0000);
  {$ELSE}
  Result := ((F.Red shr 8) and $FF) or (F.Green and $FF00) or ((F.Blue shl 8) and $FF0000);
  {$ENDIF}
end;

function RGB32PixelToFPColorInline(P: TRGB32Pixel): TFPColor; inline;
begin
  {$IFDEF RGB}
  Result.Red := (P shr 16) and $FF;
  Result.Red := Result.Red or (Result.Red shl 8);
  Result.Green := P and $FF00;
  Result.Green := Result.Green or (Result.Green shr 8);
  Result.Blue := P and $FF;
  Result.Blue := Result.Blue or (Result.Blue shl 8);
  {$ELSE}
  Result.Red := P and $FF;
  Result.Red := Result.Red or (Result.Red shl 8);
  Result.Green := P and $FF00;
  Result.Green := Result.Green or (Result.Green shr 8);
  Result.Blue := (P shr 16) and $FF;
  Result.Blue := Result.Blue or (Result.Blue shl 8);
  {$ENDIF}
end;

function RGB32PixelToColor(P: TRGB32Pixel): TColor;
begin
  Result := RGB32PixelToColorInline(P);
end;

function ColorToRGB32Pixel(C: TColor): TRGB32Pixel;
begin
  Result := ColorToRGB32PixelInline(C);
end;

procedure SwapRGBPixels(A, B: PRGBPixel; const Size: Integer); inline;
var
  T32: TRGB32Pixel;
  T8: TRGB8Pixel;
begin
  if Size = 4 then
  begin
    T32 := PRGB32Pixel(A)^;
    PRGB32Pixel(A)^ := PRGB32Pixel(B)^;
    PRGB32Pixel(B)^ := T32;
  end
  else
  begin
    T8 := PRGB8Pixel(A)^;
    PRGB8Pixel(A)^ := PRGB8Pixel(B)^;
    PRGB8Pixel(B)^ := T8;
  end;
end;

procedure CopyRGBPixels(Src, Dest: PRGBPixel; const Size: Integer); inline;
begin
  if Size = 4 then PRGB32Pixel(Dest)^ := PRGB32Pixel(Src)^
  else
  begin
    PRGB8Pixel(Dest)^ := PRGB8Pixel(Src)^;
  end;
end;

function GetRGBBitmapPixelPtr(const Bitmap: TRGBBitmapCore; X, Y: Integer): PRGBPixel; inline;
begin
  Result := Bitmap.FPixels;
  Inc(Result, Y * Bitmap.FRowPixelStride * Bitmap.FSizeOfPixel + X * Bitmap.FSizeOfPixel);
end;

function RoundIntensityFloatInline(V: TIntensityFloat): Byte; inline;
begin
  Result := Max(0, Min(255, (V + 128) shr 8));
end;

procedure FlipHorzRGBBitmap(Bitmap: TRGBBitmapCore);
var
  X, Y: Integer;
  PNew, POld: PRGBPixel;
begin
  for Y := 0 to Pred(Bitmap.Height) do
  begin
    PNew := Bitmap.GetPixelPtrUnsafe(0, Y);
    POld := Bitmap.GetPixelPtrUnsafe(Pred(Bitmap.Width), Y);
    for X := 0 to Pred(Bitmap.Width shr 1) do
    begin
      SwapRGBPixels(PNew, POld, Bitmap.SizeOfPixel);
      Inc(PNew, Bitmap.SizeOfPixel);
      Dec(POld, Bitmap.SizeOfPixel);
    end;
  end;
end;

procedure FlipVertRGBBitmap(Bitmap: TRGBBitmapCore);
var
  X, Y: Integer;
  PNew, POld: PRGBPixel;
begin
  for Y := 0 to Pred(Bitmap.Height shr 1) do
  begin
    PNew := Bitmap.GetPixelPtrUnsafe(0, Y);
    POld := Bitmap.GetPixelPtrUnsafe(0, Pred(Bitmap.Height) - Y);
    for X := 0 to Pred(Bitmap.Width) do
    begin
      SwapRGBPixels(PNew, POld, Bitmap.SizeOfPixel);
      Inc(PNew, Bitmap.SizeOfPixel);
      Inc(POld, Bitmap.SizeOfPixel);
    end;
  end;
end;

(*
  Creates look-up table T[I = 0..255] = A + I * B.
*)

function GetIntensityFloatTable(A, B: Single): TIntensityFloatTable;
var
  I: Integer;
  C: Single;
begin
  C := A;
  for I := 0 to High(Result) do
  begin
    Result[I] := Round(C * 256);
    C := C + B;
  end;
end;

procedure Rotate90CWRGBBitmap(Bitmap: TRGBBitmapCore);
var
  X, Y: Integer;
  PNew, POld: PRGBPixel;
  Result: TRGBBitmapCore;
begin
  Result := TRGBBitmapCore.Create(Bitmap.Height, Bitmap.Width, Bitmap.SizeOfPixel);
  try
    for Y := 0 to Pred(Bitmap.Height) do
    begin
      PNew := Result.GetPixelPtrUnsafe(Pred(Bitmap.Height) - Y, 0);
      POld := Bitmap.GetPixelPtrUnsafe(0, Y);
      for X := 0 to Pred(Bitmap.Width) do
      begin
        CopyRGBPixels(POld, PNew, Result.SizeOfPixel);
        Inc(PNew, Result.RowPixelStride * Result.SizeOfPixel);
        Inc(POld, Result.SizeOfPixel);
      end;
    end;
    Bitmap.SwapWith(Result);
  finally
    FreeAndNil(Result);
  end;
end;


procedure Rotate180CWRGBBitmap(Bitmap: TRGBBitmapCore);
var
  X, Y: Integer;
  PNew, POld: PRGBPixel;
begin
  for Y := 0 to Pred(Bitmap.Height shr 1) do
  begin
    PNew := Bitmap.GetPixelPtrUnsafe(0, Y);
    POld := Bitmap.GetPixelPtrUnsafe(Pred(Bitmap.Width), Pred(Bitmap.Height) - Y);
    for X := 0 to Pred(Bitmap.Width) do
    begin
      SwapRGBPixels(PNew, POld, Bitmap.SizeOfPixel);
      Inc(PNew, Bitmap.SizeOfPixel);
      Dec(POld, Bitmap.SizeOfPixel);
    end;
  end;
  if Odd(Bitmap.Height) then
  begin
    PNew := Bitmap.GetPixelPtrUnsafe(0, Bitmap.Height shr 1);
    POld := Bitmap.GetPixelPtrUnsafe(Pred(Bitmap.Width), Bitmap.Height shr 1);
    for X := 0 to Pred(Bitmap.Width shr 1) do
    begin
      SwapRGBPixels(PNew, POld, Bitmap.SizeOfPixel);
      Inc(PNew, Bitmap.SizeOfPixel);
      Dec(POld, Bitmap.SizeOfPixel);
    end;
  end;
end;

procedure Rotate270CWRGBBitmap(Bitmap: TRGBBitmapCore);
var
  X, Y: Integer;
  PNew, POld: PRGBPixel;
  Result: TRGBBitmapCore;
begin
  Result := TRGBBitmapCore.Create(Bitmap.Height, Bitmap.Width, Bitmap.SizeOfPixel);
  try
    for Y := 0 to Pred(Bitmap.Height) do
    begin
      PNew := Result.GetPixelPtrUnsafe(Y, Pred(Bitmap.Width));
      POld := Bitmap.GetPixelPtrUnsafe(0, Y);
      for X := 0 to Pred(Bitmap.Width) do
      begin
        CopyRGBPixels(POld, PNew, Result.SizeOfPixel);
        Dec(PNew, Result.RowPixelStride * Result.SizeOfPixel);
        Inc(POld, Result.SizeOfPixel);
      end;
    end;
    Bitmap.SwapWith(Result);
  finally
    FreeAndNil(Result);
  end;
end;

procedure InvertRGBBitmap(Bitmap: TRGBBitmapCore);
var
  I: Integer;
  P: PRGBPixel;
begin
  P := Bitmap.Pixels;
  for I := 0 to Pred(Bitmap.Height * Bitmap.RowPixelStride * Bitmap.SizeOfPixel) do
  begin
    P^ := $FF - P^;
    Inc(P);
  end;
end;

procedure GrayscaleRGB32Bitmap(Bitmap: TRGB32BitmapCore);
var
  X, Y: Integer;
  P: PRGB32Pixel;
  S: Byte;
  R, G, B: TIntensityFloatTable;
begin
  // R * 0.299 + G * 0.587 + B * 0.114
  R := GetIntensityFloatTable(0, 0.299);
  G := GetIntensityFloatTable(0, 0.587);
  B := GetIntensityFloatTable(0, 0.114);
  for Y := 0 to Pred(Bitmap.Height) do
  begin
    P := Bitmap.Get32PixelPtr(0, Y);
    for X := 0 to Pred(Bitmap.Width) do
    begin
      S := RoundIntensityFloatInline(R[GetRedInline(P^)] + G[GetGreenInline(P^)]
       + B[GetBlueInline(P^)]);
      P^ := RGBToRGB32PixelInline(S, S, S);
      Inc(P);
    end;
  end;
end;

procedure DisableRGB32Bitmap(Bitmap: TRGB32BitmapCore);
var
  X, Y: Integer;
  P: PRGB32Pixel;
  S: Byte;
  R, G, B: TIntensityFloatTable;
begin
  // 128 + R * 0.299 / 4 + G * 0.587 / 4 + B * 0.114 / 4
  R := GetIntensityFloatTable(128, 0.299 / 4);
  G := GetIntensityFloatTable(0, 0.587 / 4);
  B := GetIntensityFloatTable(0, 0.114 / 4);
  for Y := 0 to Pred(Bitmap.Height) do
  begin
    P := Bitmap.Get32PixelPtr(0, Y);
    for X := 0 to Pred(Bitmap.Width) do
    begin
      S := RoundIntensityFloatInline(R[GetRedInline(P^)] + G[GetGreenInline(P^)]
       + B[GetBlueInline(P^)]);
      P^ := RGBToRGB32PixelInline(S, S, S);
      Inc(P);
    end;
  end;
end;

{ TRGBBitmapCore }

function TRGBBitmapCore.GetSize: Integer;
begin
  Result := Height * RowPixelStride * SizeOfPixel;
end;

constructor TRGBBitmapCore.Create(AWidth, AHeight: Integer; ASizeOfPixel: Integer);
begin
  inherited Create;
  
  FWidth := AWidth;
  FHeight := AHeight;
  // TODO: check on 64-bit arch.
  // 32-bit alignment
  FRowPixelStride := (((AWidth * ASizeOfPixel + 3) shr 2) shl 2) div ASizeOfPixel;
  FSizeOfPixel := ASizeOfPixel;
  
  FDataOwner := True;

  GetMem(FPixels, FHeight * FRowPixelStride * FSizeOfPixel);
end;

constructor TRGBBitmapCore.CreateAsCopy(ABitmap: TRGBBitmapCore; ASizeOfPixel: Integer);
begin
  inherited Create;
  
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  FRowPixelStride := ABitmap.RowPixelStride;
  FSizeOfPixel := ASizeOfPixel;
  
  FDataOwner := True;

  GetMem(FPixels, FHeight * FRowPixelStride * SizeOfPixel);
  Move(ABitmap.Pixels^, FPixels^, FHeight * FRowPixelStride * SizeOfPixel);
end;

constructor TRGBBitmapCore.CreateFromData(AData: Pointer; AWidth, AHeight: Integer;
  ASizeOfPixel: Integer; ADataOwner: Boolean);
begin
  inherited Create;
  
  FWidth := AWidth;
  FHeight := AHeight;
  // TODO: check on 64-bit arch.
  // 32-bit alignment
  FRowPixelStride := (((AWidth * ASizeOfPixel + 3) shr 2) shl 2) div ASizeOfPixel;
  FSizeOfPixel := ASizeOfPixel;
  FPixels := AData;

  FDataOwner := ADataOwner;
end;

destructor TRGBBitmapCore.Destroy;
begin
  if FDataOwner then FreeMem(FPixels);
  inherited;
end;

procedure TRGBBitmapCore.Assign(Source: TPersistent);
begin
  if Source = nil then Exit;
  if Source = Self then Exit;
  if Source is TRGBBitmapCore then
  begin
    FreeMem(FPixels);

    FWidth := (Source as TRGBBitmapCore).Width;
    FHeight := (Source as TRGBBitmapCore).Height;
    FRowPixelStride := (Source as TRGBBitmapCore).RowPixelStride;
    FSizeOfPixel := (Source as TRGBBitmapCore).SizeOfPixel;

    GetMem(FPixels, FHeight * FRowPixelStride * FSizeOfPixel);
    Move((Source as TRGBBitmapCore).Pixels^, FPixels^,
      FHeight * FRowPixelStride * FSizeOfPixel);
  end
  else
    inherited Assign(Source);
end;

procedure TRGBBitmapCore.SwapWith(ABitmap: TRGBBitmapCore);
begin
  if ABitmap = nil then Exit;

  SwapPtr(FPixels, ABitmap.FPixels);
  SwapInt(FWidth, ABitmap.FWidth);
  SwapInt(FHeight, ABitmap.FHeight);
  SwapInt(FRowPixelStride, ABitmap.FRowPixelStride);
  SwapInt(FSizeOfPixel, ABitmap.FSizeOfPixel);
end;

function TRGBBitmapCore.GetPixelPtrUnsafe(X, Y: Integer): PRGBPixel;
begin
  Result := GetRGBBitmapPixelPtr(Self, X, Y);
end;

function TRGBBitmapCore.GetPixelPtr(X, Y: Integer): PRGBPixel;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
    Result := GetRGBBitmapPixelPtr(Self, X, Y)
  else
    Result := nil;
end;

procedure TRGBBitmapCore.Clear;
begin
  FillByte(Pixels^, Size, 0);
end;

procedure TRGBBitmapCore.ClearWhite;
begin
  FillByte(Pixels^, Size, $FF);
end;

procedure TRGBBitmapCore.Invert;
begin
  InvertRGBBitmap(Self);
end;

procedure TRGBBitmapCore.FlipHorz;
begin
  FlipHorzRGBBitmap(Self);
end;

procedure TRGBBitmapCore.FlipVert;
begin
  FlipVertRGBBitmap(Self);
end;

procedure TRGBBitmapCore.Rotate90;
begin
  Rotate90CWRGBBitmap(Self);
end;

procedure TRGBBitmapCore.Rotate180;
begin
  Rotate180CWRGBBitmap(Self);
end;

procedure TRGBBitmapCore.Rotate270;
begin
  Rotate270CWRGBBitmap(Self);
end;

{ TRGB32BitmapCore }

constructor TRGB32BitmapCore.Create(AWidth, AHeight: Integer);
begin
  inherited Create(AWidth, AHeight, SizeOf(TRGB32Pixel));
end;

constructor TRGB32BitmapCore.CreateAsCopy(ABitmap: TRGBBitmapCore);
begin
  if ABitmap.SizeOfPixel = SizeOf(TRGB32Pixel) then
    inherited CreateAsCopy(ABitmap, SizeOf(TRGB32Pixel));
end;

constructor TRGB32BitmapCore.CreateFromLazIntfImage(AImage: TLazIntfImage);
var
  I, J: Integer;
  P: PRGB32Pixel;
begin
  Create(AImage.Width, AImage.Height);

  for J := 0 to Pred(Height) do
  begin
    P := Get32PixelPtr(0, J);
    for I := 0 to Pred(Width) do
    begin
      if AImage.Colors[I, J].alpha < $FF00 then
        P^ := $FFFFFFFF
      else
        P^ := FPColorToRGB32PixelInline(AImage.Colors[I, J]);
      Inc(P);
    end;
  end;
end;

constructor TRGB32BitmapCore.CreateFromData(AData: Pointer; AWidth, AHeight: Integer;
  ADataOwner: Boolean);
begin
  inherited CreateFromData(AData, AWidth, AHeight, SizeOf(TRGB32Pixel), ADataOwner);
end;

procedure TRGB32BitmapCore.Assign(Source: TPersistent);
begin
  if (Source is TRGBBitmapCore) and ((Source as TRGBBitmapCore).SizeOfPixel = SizeOf(TRGB32Pixel)) then
    inherited Assign(Source);
end;

procedure TRGB32BitmapCore.SwapWith(ABitmap: TRGBBitmapCore);
begin
  if ABitmap.SizeOfPixel = SizeOf(TRGB32Pixel) then
    inherited SwapWith(ABitmap);
end;

procedure TRGB32BitmapCore.SaveToLazIntfImage(AImage: TLazIntfImage);
begin
  SaveToLazIntfImage(AImage, Bounds(0, 0, Width, Height));
end;

procedure TRGB32BitmapCore.SaveToLazIntfImage(AImage: TLazIntfImage; const ARect: TRect);
var
  I, J: Integer;
  P: PRGB32Pixel;
  W, H: Integer;
begin
  W := ARect.Right - ARect.Left;
  H := ARect.Bottom - ARect.Top;
  AImage.SetSize(W, H);
  try
    for J := 0 to Pred(H) do
    begin
      P := Get32PixelPtr(ARect.Left, J + ARect.Top);
      for I := 0 to Pred(W) do
      begin
        AImage.Colors[I, J] := RGB32PixelToFPColorInline(P^);
        Inc(P);
      end;
    end;
  except
    AImage.Free;
  end;
end;

function TRGB32BitmapCore.Get32PixelPtrUnsafe(X, Y: Integer
  ): PRGB32Pixel;
begin
  Result := PRGB32Pixel(GetRGBBitmapPixelPtr(Self, X, Y));
end;

function TRGB32BitmapCore.Get32PixelPtr(X, Y: Integer): PRGB32Pixel;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
    Result := PRGB32Pixel(GetRGBBitmapPixelPtr(Self, X, Y))
  else
    Result := nil;
end;

function TRGB32BitmapCore.Get32PixelUnsafe(X, Y: Integer): TRGB32Pixel;
begin
  Result := GetRGBBitmapPixelPtr(Self, X, Y)^;
end;

procedure TRGB32BitmapCore.Set32PixelUnsafe(X, Y: Integer;
  Value: TRGB32Pixel);
begin
  GetRGBBitmapPixelPtr(Self, X, Y)^ := Value;
end;

procedure TRGB32BitmapCore.Set32Pixel(X, Y: Integer; Value: TRGB32Pixel);
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
    PRGB32Pixel(GetRGBBitmapPixelPtr(Self, X, Y))^ := Value;
end;

{ TRGB8BitmapCore }

constructor TRGB8BitmapCore.Create(AWidth, AHeight: Integer);
begin
  inherited Create(AWidth, AHeight, SizeOf(TRGB8Pixel));
end;

constructor TRGB8BitmapCore.CreateAsCopy(ABitmap: TRGBBitmapCore);
begin
  if ABitmap.SizeOfPixel = SizeOf(TRGB8Pixel) then
    inherited CreateAsCopy(ABitmap, SizeOf(TRGB8Pixel));
end;

constructor TRGB8BitmapCore.CreateFromData(AData: Pointer; AWidth, AHeight: Integer;
  ADataOwner: Boolean);
begin
  inherited CreateFromData(AData, AWidth, AHeight, SizeOf(TRGB8Pixel), ADataOwner);
end;

procedure TRGB8BitmapCore.LoadFromLazIntfImageAlpha(AImage: TLazIntfImage);
var
  I, J: Integer;
  P: PRGB8Pixel;
begin
  for J := 0 to Pred(Height) do
  begin
    P := Get8PixelPtr(0, J);
    for I := 0 to Pred(Width) do
    begin
      P^ := (AImage.Colors[I, J].alpha shr 8) and $FF;
      Inc(P);
    end;
  end;
end;

procedure TRGB8BitmapCore.SaveToLazIntfImageAlpha(AImage: TLazIntfImage);
begin
  SaveToLazIntfImageAlpha(AImage, Bounds(0, 0, Width, Height));
end;

procedure TRGB8BitmapCore.SaveToLazIntfImageAlpha(AImage: TLazIntfImage;
  const ARect: TRect);
var
  I, J: Integer;
  P: PRGB8Pixel;
  F: TFPColor;
begin
  for J := 0 to Pred(AImage.Height) do
  begin
    P := Get8PixelPtr(ARect.Left, J + ARect.Top);
    for I := 0 to Pred(AImage.Width) do
    begin
      F := AImage.Colors[I, J];
      F.alpha := P^ shl 8;
      AImage.Colors[I, J] := F;

      Inc(P);
    end;
  end;
end;

procedure TRGB8BitmapCore.Assign(Source: TPersistent);
begin
  if (Source is TRGBBitmapCore) and ((Source as TRGBBitmapCore).SizeOfPixel = SizeOf(TRGB8Pixel)) then
    inherited Assign(Source);
end;

procedure TRGB8BitmapCore.SwapWith(ABitmap: TRGBBitmapCore);
begin
  if ABitmap.SizeOfPixel = SizeOf(TRGB8Pixel) then
    inherited SwapWith(ABitmap);
end;

function TRGB8BitmapCore.Get8PixelPtrUnsafe(X, Y: Integer): PRGB8Pixel;
begin
  Result := GetRGBBitmapPixelPtr(Self, X, Y);
end;

function TRGB8BitmapCore.Get8PixelPtr(X, Y: Integer): PRGB8Pixel;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
    Result := GetRGBBitmapPixelPtr(Self, X, Y)
  else
    Result := nil;
end;

function TRGB8BitmapCore.Get8PixelUnsafe(X, Y: Integer): TRGB8Pixel;
begin
  Result := GetRGBBitmapPixelPtr(Self, X, Y)^;
end;

procedure TRGB8BitmapCore.Set8PixelUnsafe(X, Y: Integer; Value: TRGB8Pixel);
begin
  GetRGBBitmapPixelPtr(Self, X, Y)^ := Value;
end;

procedure TRGB8BitmapCore.Set8Pixel(X, Y: Integer; Value: TRGB8Pixel);
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
    GetRGBBitmapPixelPtr(Self, X, Y)^ := Value;
end;


end.

