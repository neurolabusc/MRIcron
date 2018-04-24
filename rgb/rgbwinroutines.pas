{
 /***************************************************************************
                                RGBWinRoutines.pas


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
    This unit contains routines for win32 interface.

}
unit RGBWinRoutines;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Windows, Classes,
  RGBTypes;
  
  procedure WidgetSetDrawRGB32Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
    Bitmap: TRGB32BitmapCore);
  procedure WidgetSetStretchDrawRGB32Bitmap(Dest: HDC; DstX, DstY, DstWidth, DstHeight: Integer;
    SrcX, SrcY, SrcWidth, SrcHeight: Integer; Bitmap: TRGB32BitmapCore);
    
  procedure WidgetSetDrawRGB8Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
    Bitmap: TRGB8BitmapCore);

implementation

procedure WidgetSetDrawRGB32Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth,
  SrcHeight: Integer; Bitmap: TRGB32BitmapCore);
var
  Info: BITMAPINFO;
begin
  with Info.bmiHeader do
  begin
    biSize := SizeOf(BITMAPINFOHEADER);
    biWidth := Bitmap.Width;
    biHeight := Bitmap.Height;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biClrImportant := 0;
  end;

  SetStretchBltMode(Dest, COLORONCOLOR);
  StretchDIBits(Dest, DstX, Pred(DstY + SrcHeight), SrcWidth, -SrcHeight,
    SrcX, SrcY, SrcWidth, SrcHeight, Bitmap.Pixels, Info, DIB_RGB_COLORS, SRCCOPY);
end;

procedure WidgetSetStretchDrawRGB32Bitmap(Dest: HDC; DstX, DstY, DstWidth,
  DstHeight: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
  Bitmap: TRGB32BitmapCore);
var
  Info: BITMAPINFO;
begin
  with Info.bmiHeader do
  begin
    biSize := SizeOf(BITMAPINFOHEADER);
    biWidth := Bitmap.Width;
    biHeight := Bitmap.Height;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biClrImportant := 0;
  end;

  SetStretchBltMode(Dest, COLORONCOLOR);
  StretchDIBits(Dest, DstX, Pred(DstY + DstHeight), DstWidth, -DstHeight, SrcX, SrcY,
    SrcWidth, SrcHeight, Bitmap.Pixels, Info, DIB_RGB_COLORS, SRCCOPY);
end;

procedure WidgetSetDrawRGB8Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY,
  SrcWidth, SrcHeight: Integer; Bitmap: TRGB8BitmapCore);
var
  Info: PBITMAPINFO;
  I: Byte;
  PColor: PRGBQUAD;
begin
  GetMem(Info, SizeOf(BITMAPINFO) + 256 * SizeOf(RGBQUAD));
  try
    with Info^.bmiHeader do
    begin
      biSize := SizeOf(BITMAPINFOHEADER);
      biWidth := Bitmap.Width;
      biHeight := Bitmap.Height;
      biPlanes := 1;
      biBitCount := 8;
      biCompression := BI_RGB;
      biSizeImage := 0;
      biClrUsed := 256;
      biClrImportant := 0;
    end;

    PColor := @(Info^.bmiColors[0]);
    for I := 0 to 255 do
    begin
      PColor^.rgbRed := I;
      PColor^.rgbGreen := I;
      PColor^.rgbBlue := I;
      Inc(PColor);
    end;

    SetStretchBltMode(Dest, COLORONCOLOR);
    StretchDIBits(Dest, DstX, Pred(DstY + SrcHeight), SrcWidth, -SrcHeight,
      SrcX, SrcY, SrcWidth, SrcHeight, Bitmap.Pixels, Info^, DIB_RGB_COLORS, SRCCOPY);
  finally
    FreeMem(Info);
  end;
end;


end.

