{
 /***************************************************************************
                                RGBQtRoutines.pas


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
    This unit contains routines for Qt interface.

}
unit RGBQtRoutines;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Types, LCLType, Qt4, QtObjects, Classes,
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
begin
  WidgetSetStretchDrawRGB32Bitmap(Dest, DstX, DstY, SrcWidth, SrcHeight, SrcX, SrcY, SrcWidth, SrcHeight, Bitmap);
end;

procedure WidgetSetStretchDrawRGB32Bitmap(Dest: HDC; DstX, DstY, DstWidth,
  DstHeight: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
  Bitmap: TRGB32BitmapCore);
var
  DstQDC: TQtDeviceContext absolute Dest;
  SrcRect, DstRect: TRect;
  Image: TQtImage;
begin
  DstRect := Bounds(DstX, DstY, DstWidth, DstHeight);
  SrcRect := Bounds(SrcX, SrcY, SrcWidth, SrcHeight);
  
  Image := TQtImage.Create(Bitmap.Pixels, Bitmap.Width, Bitmap.Height, QImageFormat_RGB32);
  try
    QPainter_drawImage(DstQDC.Widget, PRect(@DstRect), Image.Handle, @SrcRect, QtAutoColor);
  finally
    Image.Free;
  end;
end;

procedure WidgetSetDrawRGB8Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY,
  SrcWidth, SrcHeight: Integer; Bitmap: TRGB8BitmapCore);
var
  DstQDC: TQtDeviceContext absolute Dest;
  SrcRect, DstRect: TRect;
  Image: TQtImage;
  I: Integer;
begin
  DstRect := Bounds(DstX, DstY, SrcWidth, SrcHeight);
  SrcRect := Bounds(SrcX, SrcY, SrcWidth, SrcHeight);

  Image := TQtImage.Create(Bitmap.Pixels, Bitmap.Width, Bitmap.Height, QImageFormat_Indexed8);
  try
    // initialize palette
    for I := 0 to 255 do
      QImage_setColor(Image.Handle, I, I + I shl 8 + I shl 16 + $FF shl 24);
    
    QPainter_drawImage(DstQDC.Widget, PRect(@DstRect), Image.Handle, @SrcRect, QtAutoColor);
  finally
    Image.Free;
  end;
end;


end.

