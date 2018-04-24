{
 /***************************************************************************
                                RGBCarbonRoutines.pas


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
    This unit contains routines for Carbon interface.

}
unit RGBCarbonRoutines;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, LCLType,
  MacOSAll, CarbonProc, CarbonGDIObjects, CarbonCanvas,
  RGBTypes, RGBUtils;
  
  procedure WidgetSetDrawRGB32Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
    Bitmap: TRGB32BitmapCore);

  procedure WidgetSetDrawRGB8Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
    Bitmap: TRGB8BitmapCore);

implementation

procedure WidgetSetDrawRGB32Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth,
  SrcHeight: Integer; Bitmap: TRGB32BitmapCore);
var
  CGImage: CGImageRef;
  CarbonBitmap: TCarbonBitmap;
begin
  if not CheckDC(Dest, 'WidgetSetDrawRGB32Bitmap') then Exit;

  CarbonBitmap := TCarbonBitmap.Create(Bitmap.Width, Bitmap.Height, 24, 32, cbaDWord, cbtRGB, Bitmap.Pixels, False);
  try
    CGImage := CarbonBitmap.CreateSubImage(Bounds(SrcX, SrcY, SrcWidth, SrcHeight));

    TCarbonDeviceContext(Dest).DrawCGImage(DstX, DstY, SrcWidth, SrcHeight, CGImage);
    CGImageRelease(CGImage);
  finally
    CarbonBitmap.Free;
  end;
end;

procedure WidgetSetDrawRGB8Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY,
  SrcWidth, SrcHeight: Integer; Bitmap: TRGB8BitmapCore);
var
  CGImage: CGImageRef;
  CarbonBitmap: TCarbonBitmap;
begin
  if not CheckDC(Dest, 'WidgetSetDrawRGB8Bitmap') then Exit;

  CarbonBitmap := TCarbonBitmap.Create(Bitmap.Width, Bitmap.Height, 8, 8, cbaDWord, cbtGray, Bitmap.Pixels, False);
  try
    CGImage := CarbonBitmap.CreateSubImage(Bounds(SrcX, SrcY, SrcWidth, SrcHeight));

    TCarbonDeviceContext(Dest).DrawCGImage(DstX, DstY, SrcWidth, SrcHeight, CGImage);
    CGImageRelease(CGImage);
  finally
    CarbonBitmap.Free;
  end;
end;


end.

