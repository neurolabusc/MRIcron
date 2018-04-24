{
 /***************************************************************************
                                  RGBGraphics.pas


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
    TRGB32Bitmap is a memory image which allows fast pixel manipulations.
    TRGB32Canvas is a TRGB32Bitmap canvas for drawing primitives and
      drawing bitmap image into TCanvas.
}
unit RGBGraphics;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, LCLIntf, FPWriteBMP,
  LCLType, LCLProc, FPImage, LResources, IntfGraphics,
  GraphType, Graphics, Forms, Math, Clipbrd,
  RGBTypes, RGBRoutines, RGBUtils;
  

type
  TEraseMode = (ermNone, ermErase, ermReplace);
  TDrawMode = (dmFillAndOutline, dmOutline, dmFill);

  TRandomDensity = Word;
const
  MAXRANDOMDENSITY = $FFFF;
  
type

  TMaskFillMode = (mfAdd, mfRemove, mfXOR);

  { TRGBMask }

  TRGBMask = class(TRGB8BitmapCore)
  private
    FBGPen: TPen;
    FFGPen: TPen;
    FFillMode: TMaskFillMode;
    FMaskedPixels: Integer;
  protected
    procedure AddPixel(X, Y: Integer);
    procedure RemovePixel(X, Y: Integer);
    procedure XORPixel(X, Y: Integer);
    procedure CreatePens; virtual;
    function GetFillProcedure: TDrawPixelProcedure; virtual;
    function GetMaskedPixelsCount: Integer;
  public
    constructor Create(AWidth, AHeight: Integer); override;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore); override;
    destructor Destroy; override;
    
    procedure LoadFromLazIntfImageAlpha(AImage: TLazIntfImage); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateMaskedPixels;
    
    procedure Draw(X, Y: Integer; AMask: TRGBMask);
  
    procedure DrawShapeTo(ACanvas: TCanvas; X, Y: Integer);
    procedure StretchDrawShapeTo(ACanvas: TCanvas; DstX, DstY, DstWidth, DstHeight: Integer);
    procedure StretchDrawShapePortionTo(ACanvas: TCanvas; DstX, DstY, DstWidth, DstHeight: Integer;
      DX, DY, DW, DH: Integer);
      
    procedure DrawTo(ACanvas: TCanvas; X, Y: Integer);
    procedure StretchTrunc(AWidth, AHeight: Integer); virtual;
      
    procedure Rectangle(X1, Y1, X2, Y2: Integer);
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    
    procedure Clear; override;
    procedure ClearWhite; override;
    procedure Invert; override;
  public
    function IsEmpty: Boolean;
    function GetMaskedRect: TRect;
    property BackgroundPen: TPen read FBGPen;
    property ForegroundPen: TPen read FFGPen;
    property FillMode: TMaskFillMode read FFillMode write FFillMode;
  end;

  { TRGB32Canvas }

  TRGB32Canvas = class
  private
    FDrawMode: TDrawMode;
    FEraseMode: TEraseMode;
    FFillColor: TRGB32Pixel;
    FFloodFillTolerance: TPixelDifference;
    FOutlineColor: TRGB32Pixel;
    FOwner: TRGB32BitmapCore;
    FPaperColor: TRGB32Pixel;
    FRandomDensity: TRandomDensity;
    FRandomEnabled: Boolean;
    FRectangleRoundness: Integer;
    function GetFillColor: TColor;
    function GetOutlineColor: TColor;
    function GetPaperColor: TColor;
    procedure SetFillColor(const AValue: TColor);
    procedure SetOutlineColor(const AValue: TColor);
    procedure SetPaperColor(const AValue: TColor);
  protected
    function PixelMasked(X, Y: Integer): Boolean;
    function SamePixelUnsafe(X, Y: Integer; Value: TRGB32Pixel): Boolean;
    function SamePixelUnmasked(X, Y: Integer; Value: TRGB32Pixel): Boolean;

    procedure DrawOutlinePixel(X, Y: Integer);
    procedure DrawFillPixel(X, Y: Integer);
    procedure DrawPaperPixel(X, Y: Integer);
    
    procedure DrawReplacePixel(X, Y: Integer);

    procedure DrawRandomOutlinePixel(X, Y: Integer);
    procedure DrawRandomFillPixel(X, Y: Integer);
    procedure DrawRandomPaperPixel(X, Y: Integer);
    
    procedure DrawEmptyPixel(X, Y: Integer);
    
    function GetOutlineProcedure: TDrawPixelProcedure; virtual;
    function GetFillProcedure: TDrawPixelProcedure; virtual;
  public
    constructor Create(AOwner: TRGB32BitmapCore);

    procedure SetColor(X, Y: Integer; Value: TColor);
    function GetColor(X, Y: Integer): TColor;
    
    procedure Fill(Color: TColor);
    procedure FillRect(X1, Y1, X2, Y2: Integer);
    procedure FillEllipse(X1, Y1, X2, Y2: Integer);
    procedure Line(X1, Y1, X2, Y2: Integer);
    procedure Rectangle(X1, Y1, X2, Y2: Integer);
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    procedure FloodFill(X, Y: Integer);
    procedure MaskFloodFill(X, Y: Integer);
  public
    procedure DrawTo(ACanvas: TCanvas; X, Y: Integer);
    procedure StretchDrawTo(ACanvas: TCanvas; DstX, DstY, DstWidth, DstHeight: Integer);
  public
    property EraseMode: TEraseMode read FEraseMode write FEraseMode;
    property DrawMode: TDrawMode read FDrawMode write FDrawMode;
    property FloodFillTolerance: TPixelDifference read FFloodFillTolerance
      write FFloodFillTolerance;
    
    property FillColor: TColor read GetFillColor write SetFillColor;
    property OutlineColor: TColor read GetOutlineColor write SetOutlineColor;
    property PaperColor: TColor read GetPaperColor write SetPaperColor;

    property RandomEnabled: Boolean read FRandomEnabled write FRandomEnabled;
    property RandomDensity: TRandomDensity read FRandomDensity write FRandomDensity;
    
    property RectangleRoundness: Integer read FRectangleRoundness write FRectangleRoundness;
  end;
  
  TSmoothMethod = (smAreaPixel, smBilinear, smBicubic);

  { TRGB32Bitmap }

  TRGB32Bitmap = class(TRGB32BitmapCore)
  private
    FCanvas: TRGB32Canvas;
    FMask: TRGBMask;
  protected
    function CreateDefaultLazIntfImage: TLazIntfImage;
  public
    constructor Create(AWidth, AHeight: Integer); override;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore); override;
    constructor CreateFromLazIntfImage(AImage: TLazIntfImage); override;
    
    constructor CreateFromFile(const FileName: String); virtual;
    constructor CreateFromBitmap(ABitmap: TRasterImage); virtual;
    destructor Destroy; override;
    
    procedure Assign(Source: TPersistent); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); override;
    procedure SaveToLazIntfImage(AImage: TLazIntfImage; const ARect: TRect); override;

    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream; AWriterClass: TFPCustomImageWriterClass); virtual;
    procedure SaveToStream(Stream: TStream; const ARect: TRect;
      AWriterClass: TFPCustomImageWriterClass); virtual;

    procedure SaveToFile(const FileName: String); virtual;
    procedure SaveToLazarusResource(const FileName, Name: String); virtual;
  public
    procedure Draw(X, Y: Integer; ABitmap: TRGB32Bitmap);
    
    procedure StretchTrunc(AWidth, AHeight: Integer); virtual;
    procedure StretchSmooth(AWidth, AHeight: Integer; Method: TSmoothMethod); virtual;

    procedure Grayscale; virtual;
    procedure Disable; virtual;
    
    procedure CutToClipboard; virtual;
    procedure CopyToClipboard; virtual;
    procedure Delete; virtual;
    
    procedure FlipHorz; override;
    procedure FlipVert; override;
    procedure Rotate90; override;
    procedure Rotate180; override;
    procedure Rotate270; override;
  public
    property Canvas: TRGB32Canvas read FCanvas;
    property Mask: TRGBMask read FMask write FMask;
  end;

implementation

function AbsByte(Src: Integer): Byte; inline;
begin
  if Src >= 0 then Result := Src
  else Result := -Src;
end;

function RGB32PixelDifference(A, B: TRGB32Pixel): TPixelDifference; inline;
begin
  Result := AbsByte(((A shr 16) and $FF) - ((B shr 16) and $FF))
    + AbsByte(((A shr 8) and $FF) - ((B shr 8) and $FF))
    + AbsByte((A and $FF) - (B and $FF));
end;

{ TRGB32Bitmap }

function TRGB32Bitmap.CreateDefaultLazIntfImage: TLazIntfImage;
var
  RID: TRawImageDescription;
  DC: HDC;
begin
  DC := GetDC(0);
  try
    RawImage_DescriptionFromDevice(DC, RID);
  finally
    ReleaseDC(0, DC);
  end;

  Result := TLazIntfImage.Create(0, 0);
  Result.DataDescription := RID;
end;

constructor TRGB32Bitmap.Create(AWidth, AHeight: Integer);
begin
  inherited;
  FCanvas := TRGB32Canvas.Create(Self);
  FMask := TRGBMask.Create(AWidth, AHeight);
end;

constructor TRGB32Bitmap.CreateAsCopy(ABitmap: TRGBBitmapCore);
begin
  inherited;
  FCanvas := TRGB32Canvas.Create(Self);
  if ABitmap is TRGB32Bitmap then
    FMask := TRGBMask.CreateAsCopy((ABitmap as TRGB32Bitmap).Mask)
  else
    FMask := TRGBMask.Create(ABitmap.Width, ABitmap.Height);
end;

constructor TRGB32Bitmap.CreateFromLazIntfImage(AImage: TLazIntfImage);
begin
  inherited CreateFromLazIntfImage(AImage);
  FMask.LoadFromLazIntfImageAlpha(AImage);
end;

constructor TRGB32Bitmap.CreateFromFile(const FileName: String);
var
  Image: TLazIntfImage;
begin
  Image := CreateDefaultLazIntfImage;
  try
    Image.LoadFromFile(FileName);
    CreateFromLazIntfImage(Image);
  finally
    Image.Free;
  end;
end;

constructor TRGB32Bitmap.CreateFromBitmap(ABitmap: TRasterImage);
var
  Image: TLazIntfImage;
begin
  Image := ABitmap.CreateIntfImage;
  try
    CreateFromLazIntfImage(Image);
  finally
    Image.Free;
  end;
end;

destructor TRGB32Bitmap.Destroy;
begin
  FCanvas.Free;
  FMask.Free;
  inherited;
end;

procedure TRGB32Bitmap.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TRGB32Bitmap then
  begin
    Mask.Assign((Source as TRGB32Bitmap).Mask);
  end;
end;

procedure TRGB32Bitmap.SwapWith(ABitmap: TRGBBitmapCore);
begin
  inherited SwapWith(ABitmap);
  if ABitmap is TRGB32Bitmap then
  begin
    Mask.SwapWith((ABitmap as TRGB32Bitmap).Mask);
  end;
end;

procedure TRGB32Bitmap.SaveToLazIntfImage(AImage: TLazIntfImage; const ARect: TRect);
begin
  inherited SaveToLazIntfImage(AImage, ARect);
  if not Mask.IsEmpty then FMask.SaveToLazIntfImageAlpha(AImage, ARect);
end;

procedure TRGB32Bitmap.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, Bounds(0, 0, Width, Height), TLazWriterXPM);
end;

procedure TRGB32Bitmap.SaveToStream(Stream: TStream;
  AWriterClass: TFPCustomImageWriterClass);
begin
  SaveToStream(Stream, Bounds(0, 0, Width, Height), AWriterClass);
end;

procedure TRGB32Bitmap.SaveToStream(Stream: TStream; const ARect: TRect;
  AWriterClass: TFPCustomImageWriterClass);
var
  Image: TLazIntfImage;
  Writer: TFPCustomImageWriter;
begin
  Image := CreateDefaultLazIntfImage;
  Writer := AWriterClass.Create;
  try
    SaveToLazIntfImage(Image, ARect);
    Image.SaveToStream(Stream, Writer);
  finally
    Writer.Free;
    Image.Free;
  end;
end;

procedure TRGB32Bitmap.SaveToFile(const FileName: String);
var
  Image: TLazIntfImage;
begin
  Image := CreateDefaultLazIntfImage;
  try
    inherited SaveToLazIntfImage(Image);
    Image.SaveToFile(FileName);
  finally
    Image.Free;
  end;
end;

procedure TRGB32Bitmap.SaveToLazarusResource(const FileName, Name: String);
var
  PixmapStream, ResourceStream: TMemoryStream;
  FileStream: TFileStream;
begin
  PixmapStream := TMemoryStream.Create;
  ResourceStream := TMemoryStream.Create;
  try
    SaveToStream(PixmapStream);
    PixmapStream.Position := 0;
    
    BinaryToLazarusResourceCode(PixmapStream, ResourceStream, Name, 'XPM');
    
    ResourceStream.Position := 0;
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.CopyFrom(ResourceStream, ResourceStream.Size);
    finally
      FileStream.Free;
    end;
  finally
    PixmapStream.Free;
    ResourceStream.Free;
  end;
end;

procedure TRGB32Bitmap.Draw(X, Y: Integer; ABitmap: TRGB32Bitmap);
begin
  DrawRGB32Bitmap(Self, X, Y, ABitmap);
end;

procedure TRGB32Bitmap.StretchTrunc(AWidth, AHeight: Integer);
var
  Result: TRGB32Bitmap;
begin
  if (AWidth = Width) and (AHeight = Height) then Exit;
  Result := TRGB32Bitmap.Create(AWidth, AHeight);
  try
    StretchRGB32BitmapTrunc(Result, Self);
    inherited SwapWith(Result);
    Mask.StretchTrunc(AWidth, AHeight);
  finally
    FreeAndNil(Result);
  end;
end;

procedure TRGB32Bitmap.StretchSmooth(AWidth, AHeight: Integer; Method: TSmoothMethod);
begin
  //
end;

procedure TRGB32Bitmap.Grayscale;
begin
  GrayscaleRGB32Bitmap(Self);
end;

procedure TRGB32Bitmap.Disable;
begin
  DisableRGB32Bitmap(Self);
end;

procedure TRGB32Bitmap.CutToClipboard;
begin
  CopyToClipboard;
  Delete;
end;

procedure TRGB32Bitmap.CopyToClipboard;
var
  PixmapStream, BitmapStream: TMemoryStream;
  PixmapWriter, BitmapWriter: TFPCustomImageWriter;
  Image: TLazIntfImage;
  R: TRect;
begin
  PixmapStream := TMemoryStream.Create;
  BitmapStream := TMemoryStream.Create;
  Image := CreateDefaultLazIntfImage;
  PixmapWriter := TLazWriterXPM.Create;
  BitmapWriter := TFPWriterBMP.Create;
  try
    R := Mask.GetMaskedRect;
    SaveToLazIntfImage(Image, R);
    
    Clipboard.Open;
    try
      Clipboard.Clear;
      
      Image.SaveToStream(PixmapStream, PixmapWriter);
      Clipboard.AddFormat(PredefinedClipboardFormat(pcfPixmap), PixmapStream);

      Image.SaveToStream(BitmapStream, BitmapWriter);
      Clipboard.AddFormat(PredefinedClipboardFormat(pcfBitmap), BitmapStream);
    finally
      Clipboard.Close;
    end;
  finally
    PixmapStream.Free;
    BitmapStream.Free;
    Image.Free;
    PixmapWriter.Free;
    BitmapWriter.Free;
  end;
end;

procedure TRGB32Bitmap.Delete;
begin
  Canvas.Fill(Canvas.PaperColor);
end;

procedure TRGB32Bitmap.FlipHorz;
begin
  inherited FlipHorz;
  Mask.FlipHorz;
end;

procedure TRGB32Bitmap.FlipVert;
begin
  inherited FlipVert;
  Mask.FlipVert;
end;

procedure TRGB32Bitmap.Rotate90;
begin
  inherited Rotate90;
  Mask.Rotate90;
end;

procedure TRGB32Bitmap.Rotate180;
begin
  inherited Rotate180;
  Mask.Rotate180;
end;

procedure TRGB32Bitmap.Rotate270;
begin
  inherited Rotate270;
  Mask.Rotate270;
end;

{ TRGB32Canvas }

constructor TRGB32Canvas.Create(AOwner: TRGB32BitmapCore);
begin
  inherited Create;
  
  FOwner := AOwner;
  FRandomDensity := MAXRANDOMDENSITY;
  FFloodFillTolerance := 0;
  FRectangleRoundness := 0;
end;

procedure TRGB32Canvas.SetColor(X, Y: Integer; Value: TColor);
begin
  FOwner.Set32Pixel(X, Y, ColorToRGB32Pixel(Value));
end;

function TRGB32Canvas.GetColor(X, Y: Integer): TColor;
var
  P: PRGB32Pixel;
begin
  P := FOwner.Get32PixelPtr(X, Y);
  if P <> nil then Result := RGB32PixelToColor(P^)
  else Result := clNone;
end;

function TRGB32Canvas.GetFillColor: TColor;
begin
  Result := RGB32PixelToColor(FFillColor);
end;

function TRGB32Canvas.GetOutlineColor: TColor;
begin
  Result := RGB32PixelToColor(FOutlineColor);
end;

function TRGB32Canvas.GetPaperColor: TColor;
begin
  Result := RGB32PixelToColor(FPaperColor);
end;

procedure TRGB32Canvas.SetFillColor(const AValue: TColor);
begin
  FFillColor := ColorToRGB32Pixel(AValue);
end;

procedure TRGB32Canvas.SetOutlineColor(const AValue: TColor);
begin
  FOutlineColor := ColorToRGB32Pixel(AValue);
end;

procedure TRGB32Canvas.SetPaperColor(const AValue: TColor);
begin
  FPaperColor := ColorToRGB32Pixel(AValue);
end;

function TRGB32Canvas.PixelMasked(X, Y: Integer): Boolean;
var
  P: PRGB8Pixel;
begin
  if not (FOwner is TRGB32Bitmap) then Result := True
  else
    if (FOwner as  TRGB32Bitmap).Mask.IsEmpty then Result := True
    else
    begin
      P := (FOwner as  TRGB32Bitmap).Mask.Get8PixelPtr(X, Y);
      Result := (P <> nil) and (P^ = $FF);
    end;
end;

function TRGB32Canvas.SamePixelUnsafe(X, Y: Integer; Value: TRGB32Pixel): Boolean;
begin
  Result := PixelMasked(X, Y) and (RGB32PixelDifference(FOwner.Get32PixelUnsafe(X, Y), Value)
      <= FFloodFillTolerance);
end;

function TRGB32Canvas.SamePixelUnmasked(X, Y: Integer; Value: TRGB32Pixel): Boolean;
begin
  Result := RGB32PixelDifference(FOwner.Get32PixelUnsafe(X, Y), Value)
      <= FFloodFillTolerance;
end;

procedure TRGB32Canvas.DrawOutlinePixel(X, Y: Integer);
begin
  if PixelMasked(X, Y) then FOwner.Set32Pixel(X, Y, FOutlineColor);
end;

procedure TRGB32Canvas.DrawFillPixel(X, Y: Integer);
begin
  if PixelMasked(X, Y) then FOwner.Set32Pixel(X, Y, FFillColor);
end;

procedure TRGB32Canvas.DrawPaperPixel(X, Y: Integer);
begin
  if PixelMasked(X, Y) then FOwner.Set32Pixel(X, Y, FPaperColor);
end;

procedure TRGB32Canvas.DrawReplacePixel(X, Y: Integer);
var
  P: PRGB32Pixel;
begin
  if not PixelMasked(X, Y) then Exit;
  P := FOwner.Get32PixelPtr(X, Y);
  if (P <> nil) and (P^ = FFillColor) then P^ := FPaperColor;
end;

procedure TRGB32Canvas.DrawRandomOutlinePixel(X, Y: Integer);
begin
  if PixelMasked(X, Y) and (Random(MAXRANDOMDENSITY) < FRandomDensity) then
    FOwner.Set32Pixel(X, Y, FOutlineColor);
end;

procedure TRGB32Canvas.DrawRandomFillPixel(X, Y: Integer);
begin
  if PixelMasked(X, Y) and (Random(MAXRANDOMDENSITY) < FRandomDensity) then
    FOwner.Set32Pixel(X, Y, FFillColor);
end;

procedure TRGB32Canvas.DrawRandomPaperPixel(X, Y: Integer);
begin
  if PixelMasked(X, Y) and (Random(MAXRANDOMDENSITY) < FRandomDensity) then
    FOwner.Set32Pixel(X, Y, FPaperColor);
end;

procedure TRGB32Canvas.DrawEmptyPixel(X, Y: Integer);
begin
  //
end;

function TRGB32Canvas.GetOutlineProcedure: TDrawPixelProcedure;
begin
  if not FRandomEnabled then
  begin
    case DrawMode of
    dmFillAndOutline, dmOutline:
    begin
      case EraseMode of
      ermNone: Result := @DrawOutlinePixel;
      ermErase: Result := @DrawPaperPixel;
      ermReplace: Result := @DrawReplacePixel;
      end;
    end;
    else
      Result := @DrawEmptyPixel;
    end;
  end
  else
  begin
    case EraseMode of
    ermNone: Result := @DrawRandomFillPixel;
    ermErase: Result := @DrawRandomPaperPixel;
    ermReplace: Result := @DrawRandomFillPixel;
    end;
  end;
end;

function TRGB32Canvas.GetFillProcedure: TDrawPixelProcedure;
begin
  if not FRandomEnabled then
  begin
    case DrawMode of
    dmFillAndOutline, dmFill:
    begin
      case EraseMode of
      ermNone: Result := @DrawFillPixel;
      ermErase: Result := @DrawPaperPixel;
      ermReplace: Result := @DrawReplacePixel;
      end;
    end;
    else
      Result := @DrawEmptyPixel;
    end;
  end
  else
  begin
    case EraseMode of
    ermNone: Result := @DrawRandomFillPixel;
    ermErase: Result := @DrawRandomPaperPixel;
    ermReplace: Result := @DrawRandomFillPixel;
    end;
  end;
end;

procedure TRGB32Canvas.Fill(Color: TColor);
var
  P: PRGB32Pixel;
  C: TRGB32Pixel;
  PM: PRGB8Pixel;
  I, J: Integer;
begin
  C := ColorToRGB32Pixel(Color);
  for J := 0 to Pred(FOwner.Height) do
  begin
    P := FOwner.Get32PixelPtr(0, J);
    if (FOwner is TRGB32Bitmap) and not (FOwner as TRGB32Bitmap).Mask.IsEmpty then
    begin
      PM := (FOwner as TRGB32Bitmap).Mask.Get8PixelPtr(0, J);
      for I := 0 to Pred(FOwner.Width) do
      begin
        if PM^ = $FF then P^ := C;
        Inc(P);
        Inc(PM);
      end;
    end
    else FillDWord(P^, FOwner.Width, C);
  end;
end;

procedure TRGB32Canvas.FillRect(X1, Y1, X2, Y2: Integer);
begin
  FillPixelRect(X1, Y1, X2, Y2, GetFillProcedure);
end;

procedure TRGB32Canvas.FillEllipse(X1, Y1, X2, Y2: Integer);
begin
  EllipticRectangle(X1, Y1, X2, Y2, 0, 0, GetFillProcedure, GetFillProcedure);
end;

procedure TRGB32Canvas.Line(X1, Y1, X2, Y2: Integer);
begin
  LineBresenham(X1, Y1, X2, Y2, GetOutlineProcedure);
end;

procedure TRGB32Canvas.Rectangle(X1, Y1, X2, Y2: Integer);
var
  R1, R2: Integer;
begin
  R1 := Max(0, Succ(Abs(X2 - X1)) - RectangleRoundness);
  R2 := Max(0, Succ(Abs(Y2 - Y1)) - RectangleRoundness);
  EllipticRectangle(X1, Y1, X2, Y2, R1, R2, GetOutlineProcedure, GetFillProcedure);
end;

procedure TRGB32Canvas.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  EllipticRectangle(X1, Y1, X2, Y2, 0, 0, GetOutlineProcedure, GetFillProcedure);
end;

procedure TRGB32Canvas.FloodFill(X, Y: Integer);
begin
  case EraseMode of
  ermNone: FloodFillScanLine(X, Y, FOwner.Width, FOwner.Height, @FOwner.Get32PixelUnsafe,
    @SamePixelUnsafe, @DrawFillPixel);
  ermErase: FloodFillScanLine(X, Y, FOwner.Width, FOwner.Height, @FOwner.Get32PixelUnsafe,
    @SamePixelUnsafe, @DrawPaperPixel);
  end;
end;

procedure TRGB32Canvas.MaskFloodFill(X, Y: Integer);
begin
  if not (FOwner is TRGB32Bitmap) then Exit;
  FloodFillScanLine(X, Y, FOwner.Width, FOwner.Height, @FOwner.Get32PixelUnsafe,
    @SamePixelUnmasked, (FOwner as TRGB32Bitmap).Mask.GetFillProcedure);;
end;

procedure TRGB32Canvas.DrawTo(ACanvas: TCanvas; X, Y: Integer);
begin
  if ACanvas <> nil then
    with FOwner do
      DrawRGB32Bitmap(ACanvas.Handle, X, Y, 0, 0, Width, Height, FOwner);
end;

procedure TRGB32Canvas.StretchDrawTo(ACanvas: TCanvas; DstX, DstY, DstWidth,
  DstHeight: Integer);
begin
  if ACanvas <> nil then
    with FOwner do
      StretchDrawRGB32Bitmap(ACanvas.Handle, DstX, DstY, DstWidth, DstHeight,
        0, 0, Width, Height, FOwner);
end;

{ TRGBMask }

procedure TRGBMask.AddPixel(X, Y: Integer);
var
  P: PRGB8Pixel;
begin
  P := Get8PixelPtr(X, Y);
  if P <> nil then
  begin
    if P^ <> $FF then Inc(FMaskedPixels);
    P^ := $FF;
  end;
end;

procedure TRGBMask.RemovePixel(X, Y: Integer);
var
  P: PRGB8Pixel;
begin
  P := Get8PixelPtr(X, Y);
  if P <> nil then
  begin
    if P^ = $FF then Dec(FMaskedPixels);
    P^ := 0;
  end;
end;

procedure TRGBMask.XORPixel(X, Y: Integer);
var
  P: PRGB8Pixel;
begin
  P := Get8PixelPtr(X, Y);
  if P <> nil then
  begin
    if P^ = 0 then Inc(FMaskedPixels);
    if P^ = $FF then Dec(FMaskedPixels);
    P^ := $FF - P^;
  end;
end;

procedure TRGBMask.CreatePens;
begin
  FBGPen := TPen.Create;
  FBGPen.Color := clYellow;
  
  FFGPen := TPen.Create;
  FFGPen.Color := clBlue;
  //FFGPen.Style := psDot;
end;

function TRGBMask.GetFillProcedure: TDrawPixelProcedure;
begin
  case FillMode of
  mfAdd: Result := @AddPixel;
  mfRemove: Result := @RemovePixel;
  mfXOR: Result := @XORPixel;
  end;
end;

function TRGBMask.GetMaskedPixelsCount: Integer;
var
  I, J: Integer;
  P: PRGB8Pixel;
begin
  Result := 0;
  
  for J := 0 to Pred(Height) do
  begin
    P := Get8PixelPtr(0, J);
    for I := 0 to Pred(Width) do
    begin
      if P^ = $FF then Inc(Result);
      Inc(P);
    end;
  end;
end;

constructor TRGBMask.Create(AWidth, AHeight: Integer);
begin
  inherited Create(AWidth, AHeight);
  Clear;
  
  CreatePens;
end;

constructor TRGBMask.CreateAsCopy(ABitmap: TRGBBitmapCore);
begin
  inherited CreateAsCopy(ABitmap);
  UpdateMaskedPixels;
  CreatePens;
end;

procedure TRGBMask.SwapWith(ABitmap: TRGBBitmapCore);
begin
  inherited SwapWith(ABitmap);
  UpdateMaskedPixels;
end;

procedure TRGBMask.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  UpdateMaskedPixels;
end;

procedure TRGBMask.UpdateMaskedPixels;
begin
  FMaskedPixels := GetMaskedPixelsCount;
end;

procedure TRGBMask.Draw(X, Y: Integer; AMask: TRGBMask);
begin
  DrawRGB8Bitmap(Self, X, Y, AMask);
  UpdateMaskedPixels;
end;

destructor TRGBMask.Destroy;
begin
  FBGPen.Free;
  FFGPen.Free;

  inherited Destroy;
end;

procedure TRGBMask.LoadFromLazIntfImageAlpha(AImage: TLazIntfImage);
begin
  inherited LoadFromLazIntfImageAlpha(AImage);
  UpdateMaskedPixels;
end;

procedure TRGBMask.DrawShapeTo(ACanvas: TCanvas; X, Y: Integer);
begin
  StretchDrawShapeTo(ACanvas, X, Y, Width, Height);
end;

procedure TRGBMask.StretchDrawShapeTo(ACanvas: TCanvas; DstX, DstY, DstWidth,
  DstHeight: Integer);
begin
  StretchDrawShapePortionTo(ACanvas, DstX, DstY, DstWidth, DstHeight,
    0, 0, Width, Height);
end;

procedure TRGBMask.StretchDrawShapePortionTo(ACanvas: TCanvas; DstX, DstY,
  DstWidth, DstHeight: Integer; DX, DY, DW, DH: Integer);
begin
  if ACanvas <> nil then
    StretchDrawRGBMaskShapePortion(ACanvas.Handle, DstX, DstY, DstWidth, DstHeight,
      Self, DX, DY, DW, DH, FBGPen.Reference.Handle, FFGPen.Reference.Handle);
end;

procedure TRGBMask.DrawTo(ACanvas: TCanvas; X, Y: Integer);
begin
  if ACanvas <> nil then
    DrawRGB8Bitmap(ACanvas.Handle, X, Y, 0, 0, Width, Height, Self);
end;

procedure TRGBMask.StretchTrunc(AWidth, AHeight: Integer);
var
  Result: TRGBMask;
begin
  if (AWidth = Width) and (AHeight = Height) then Exit;
  Result := TRGBMask.Create(AWidth, AHeight);
  try
    StretchRGB8BitmapTrunc(Result, Self);
    SwapWith(Result);
    FMaskedPixels := GetMaskedPixelsCount;
  finally
    FreeAndNil(Result);
  end;
end;

procedure TRGBMask.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  FillPixelRect(X1, Y1, X2, Y2, GetFillProcedure);
end;

procedure TRGBMask.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  EllipticRectangle(X1, Y1, X2, Y2, 0, 0, GetFillProcedure, GetFillProcedure);
end;

procedure TRGBMask.Clear;
begin
  inherited Clear;
  FMaskedPixels := 0;
end;

procedure TRGBMask.ClearWhite;
begin
  inherited ClearWhite;
  
  FMaskedPixels := Width * Height;
end;

procedure TRGBMask.Invert;
begin
  inherited Invert;
  
  FMaskedPixels := Width * Height - FMaskedPixels;
end;

function TRGBMask.IsEmpty: Boolean;
begin
  Result := FMaskedPixels = 0;
end;

function TRGBMask.GetMaskedRect: TRect;
var
  I, J: Integer;
  LineMasked: Boolean;
  P: PRGB8Pixel;
begin
  Result := Rect(Width, Height, 0, 0);
  for J := 0 to Pred(Height) do
  begin
    P := Get8PixelPtr(0, J);
    LineMasked := False;
    for I := 0 to Pred(Width) do
    begin
      if P^ = $FF then
      begin
        LineMasked := True;
        if I < Result.Left then Result.Left := I;
        if Succ(I) > Result.Right then Result.Right := Succ(I);
      end;
      Inc(P);
    end;
    if LineMasked then
    begin
      if J < Result.Top then Result.Top := J;
      if Succ(J) > Result.Bottom then Result.Bottom := Succ(J);
    end;
  end;
  
  SortRect(Result);
end;

end.

