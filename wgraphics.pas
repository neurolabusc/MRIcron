unit wgraphics;
//only for windows
{$mode objfpc}{$H+}

interface

uses
  Interfaces,

  SysUtils,  LCLType, LCLProc, InterfaceBase,  FPImage,
  IntfGraphics, Math,
  Windows,Classes,define_types;

procedure Draw32Bitmap(Dest: HDC; lWidth, lHeight: Integer; Bitmap: RGBQuadp);
procedure StretchDraw32Bitmap(Dest: HDC; DstWidth, DstHeight,SrcWidth, SrcHeight: Integer; Bitmap: RGBQuadp);

implementation



procedure StretchDraw32Bitmap(Dest: HDC; DstWidth, DstHeight,SrcWidth, SrcHeight: Integer; Bitmap: RGBQuadp);
var
  Clip: TRect;
  Info: BITMAPINFO;
  DstX, DstY,SrcX, SrcY: integer;
begin
  if (Bitmap = nil)  then Exit;
  if (SrcWidth <= 0) or (SrcHeight <= 0) then Exit;
  if (DstWidth <= 0) or (DstHeight <= 0) then Exit;
  DstX := 0; DstY := 0; SrcX := 0; SrcY := 0;
  Widgetset.GetClipBox(Dest, @Clip);
  if (DstX >= Clip.Right) or (DstY >= Clip.Bottom) or
     (DstX + DstWidth < Clip.Left) or (DstY + DstHeight < Clip.Top) then Exit;
  if (DstWidth = SrcWidth) and (DstHeight = SrcHeight) then  begin
    Draw32Bitmap(Dest, SrcWidth, SrcHeight, Bitmap);
    Exit;
  end;
  with Info.bmiHeader do begin
    biSize := SizeOf(BITMAPINFOHEADER);
    biWidth := SrcWidth;
    biHeight := SrcHeight;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biClrImportant := 0;
  end;
  SetStretchBltMode(Dest, COLORONCOLOR);
  StretchDIBits(Dest, DstX, Pred(DstY + DstHeight), DstWidth, -DstHeight, SrcX, SrcY,
    SrcWidth, SrcHeight, Bitmap, Info, DIB_RGB_COLORS, SRCCOPY);
end;

// ! SrcX < 0, SrcY < 0, SrcX + SrcWidth > Bitmap.Width, SrcY + SrcHeight > Bitmap.Height
// ! results in mash
procedure Draw32Bitmap(Dest: HDC; lWidth, lHeight: Integer; Bitmap: RGBQuadp);
var
  Clip: TRect;
  SrcX,SrcY,DstX,DstY:integer;
  Info: BITMAPINFO;
begin
  if (Bitmap = nil)  then Exit;
  if (lWidth <= 0) or (lHeight <= 0) then Exit;
  Widgetset.GetClipBox(Dest, @Clip);
  // clipping:
 SrcX := 0; SrcY := 0;DstX := 0; DstY := 0;
  //ClipDimension(Clip.Left, Clip.Right, DstX, SrcX, lWidth);
  //ClipDimension(Clip.Top, Clip.Bottom, DstY, SrcY, lHeight);
  with Info.bmiHeader do
  begin
    biSize := SizeOf(BITMAPINFOHEADER);
    biWidth := lWidth;
    biHeight := lHeight;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biClrImportant := 0;
  end;
  SetStretchBltMode(Dest, COLORONCOLOR);
  StretchDIBits(Dest, DstX, Pred(DstY + lHeight), lWidth, -lHeight,
    SrcX, SrcY, lWidth, lHeight, Bitmap, Info, DIB_RGB_COLORS, SRCCOPY);
end;

end.

