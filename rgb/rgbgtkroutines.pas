{
 /***************************************************************************
                                RGBGTKRoutines.pas


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
    This unit contains routines for GTK interfaces.

}
unit RGBGTKRoutines;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, LCLType,
{$IFDEF LCLgtk2}
   gtkDef,     //<- uncomment for older versions of Lazarus
   glib2, gdk2,gtk2Def, gtk2Proc,
{$ELSE}  //LCLgtk1
  glib, gdk, gtkDef, gtkProc,
{$ENDIF}
  RGBTypes;

  
  procedure WidgetSetDrawRGB32Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
    Bitmap: TRGB32BitmapCore);

  procedure WidgetSetDrawRGB8Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
    Bitmap: TRGB8BitmapCore);

implementation



procedure WidgetSetDrawRGB32Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth,
  SrcHeight: Integer; Bitmap: TRGB32BitmapCore);
var
  P: TPoint;
begin
  P := TGtkDeviceContext(Dest).Offset; //<- if compiler stops here, uncomment gtkDef in Uses
  Inc(DstX, P.X);
  Inc(DstY, P.Y);
  gdk_draw_rgb_32_image(TGtkDeviceContext(Dest).Drawable, TGtkDeviceContext(Dest).GC,
    DstX, DstY, SrcWidth, SrcHeight, GDK_RGB_DITHER_NONE,
    Pguchar(Bitmap.GetPixelPtrUnsafe(SrcX, SrcY)), Bitmap.RowPixelStride shl 2);
end;

procedure WidgetSetDrawRGB8Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY,
  SrcWidth, SrcHeight: Integer; Bitmap: TRGB8BitmapCore);
var
  P: TPoint;
begin
  P := TGtkDeviceContext(Dest).Offset;
  Inc(DstX, P.X);
  Inc(DstY, P.Y);
  gdk_draw_gray_image(TGtkDeviceContext(Dest).Drawable, TGtkDeviceContext(Dest).GC,
    DstX, DstY, SrcWidth, SrcHeight, GDK_RGB_DITHER_NONE,
    Pguchar(Bitmap.Get8PixelPtrUnsafe(SrcX, SrcY)), Bitmap.RowPixelStride);
end;

initialization
  gdk_rgb_init;

end.

