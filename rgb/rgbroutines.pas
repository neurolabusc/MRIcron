{
 /***************************************************************************
                                  RGBRoutines.pas


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
    This unit contains routines for manipulating rgb bitmaps (stretching,
    drawing on canvas...) and for drawing primitives (lines,
    ellipses...).

}
unit RGBRoutines;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Math, Forms, LCLIntf,
  LCLType, LCLProc, FPImage, IntfGraphics,
  Classes,
{$IFDEF LCLwin32}
  RGBWinRoutines,
{$ENDIF}
{$IFDEF LCLqt}
  RGBQtRoutines,
{$ENDIF}
{$IFDEF LCLgtk}
  {$DEFINE StretchRGB32}
  RGBGTKRoutines,
{$ENDIF}
{$IFDEF LCLgtk2}
  {$DEFINE StretchRGB32}
  RGBGTKRoutines,
{$ENDIF}
{$IFDEF LCLcarbon}
  {$DEFINE StretchRGB32}
  RGBCarbonRoutines,
{$ENDIF}
  RGBTypes, RGBUtils;
  
  procedure DrawRGB32Bitmap(Dst: TRGB32BitmapCore; X, Y: Integer; Src: TRGB32BitmapCore); overload;
  procedure DrawRGB8Bitmap(Dst: TRGB8BitmapCore; X, Y: Integer; Src: TRGB8BitmapCore); overload;
  procedure StretchRGB32BitmapTrunc(Dst, Src: TRGB32BitmapCore);
  procedure StretchRGB8BitmapTrunc(Dst, Src: TRGB8BitmapCore);
  
  procedure DrawRGB32Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
    Bitmap: TRGB32BitmapCore); overload;
  procedure StretchDrawRGB32Bitmap(Dest: HDC; DstX, DstY, DstWidth, DstHeight: Integer;
    SrcX, SrcY, SrcWidth, SrcHeight: Integer; Bitmap: TRGB32BitmapCore); overload;

  procedure StretchDrawRGBMaskShapePortion(Dest: HDC; DstX, DstY, DstWidth, DstHeight: Integer;
    Bitmap: TRGB8BitmapCore; DX, DY, DW, DH: Integer; BgPen: HPEN; FgPen: HPEN);
  procedure DrawRGB8Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
    Bitmap: TRGB8BitmapCore); overload;

type
  TDrawPixelProcedure = procedure (X, Y: Integer) of Object;
  TGetPixelFunction = function (X, Y: Integer): TRGB32Pixel of Object;
  TSamePixelFunction = function (X, Y: Integer; Value: TRGB32Pixel): Boolean of Object;

  procedure LineBresenham(X1, Y1, X2, Y2: Integer; DrawPixel: TDrawPixelProcedure);
  
  procedure FillPixelRect(X1, Y1, X2, Y2: Integer; DrawPixel: TDrawPixelProcedure);

  procedure NormalRectangle(X1, Y1, X2, Y2: Integer;
    DrawOutlinePixel, DrawFillPixel: TDrawPixelProcedure);
  procedure EllipticRectangle(X1, Y1, X2, Y2: Integer; LX, LY: Integer;
    DrawOutlinePixel, DrawFillPixel: TDrawPixelProcedure);
  procedure FloodFillScanLine(X, Y, W, H: Integer; GetPixel: TGetPixelFunction;
    SamePixel: TSamePixelFunction; DrawPixel: TDrawPixelProcedure);

implementation

function GetDCClipRect(Dest: HDC): TRect;
begin
  if GetClipBox(Dest, @Result) = ERROR then
  begin
    Result.TopLeft := Point(0, 0);
    if not GetDeviceSize(Dest, Result.BottomRight) then
      Result.BottomRight := Point(8000, 8000);
  end;
end;

procedure DrawRGB32Bitmap(Dst: TRGB32BitmapCore; X, Y: Integer; Src: TRGB32BitmapCore);
var
  SrcX, SrcWidth, SrcY, SrcHeight: Integer;
  I: Integer;
  PS, PD: PRGB32Pixel;
begin
  if (Dst = nil) or (Src = nil) then Exit;
  if (Dst.Width <= 0) or (Dst.Height <= 0) or (Src.Width <= 0) or (Src.Height <= 0) then Exit;
  if (X >= Dst.Width) or (Y >= Dst.Height) then Exit;
  if (X + Src.Width <= 0) or (Y + Src.Height <= 0) then Exit;

  SrcX := 0;
  SrcY := 0;
  SrcWidth := Src.Width;
  SrcHeight := Src.Height;

  if X < 0 then
  begin
    SrcX := -X;
    Inc(SrcWidth, X);
    X := 0;
  end;

  if Y < 0 then
  begin
    SrcY := -Y;
    Inc(SrcHeight, Y);
    Y := 0;
  end;

  if X + SrcWidth > Dst.Width then
    Dec(SrcWidth, X + SrcWidth - Dst.Width);
  if Y + SrcHeight > Dst.Height then
    Dec(SrcHeight, Y + SrcHeight - Dst.Height);

  for I := 0 to Pred(SrcHeight) do
  begin
    PS := Src.Get32PixelPtrUnsafe(SrcX, SrcY + I);
    PD := Dst.Get32PixelPtrUnsafe(X, Y + I);
    Move(PS^, PD^, SrcWidth shl 2);
  end;
end;

procedure DrawRGB8Bitmap(Dst: TRGB8BitmapCore; X, Y: Integer; Src: TRGB8BitmapCore);
var
  SrcX, SrcWidth, SrcY, SrcHeight: Integer;
  I: Integer;
  PS, PD: PRGB8Pixel;
begin
  if (Dst = nil) or (Src = nil) then Exit;
  if (Dst.Width <= 0) or (Dst.Height <= 0) or (Src.Width <= 0) or (Src.Height <= 0) then Exit;
  if (X >= Dst.Width) or (Y >= Dst.Height) then Exit;
  if (X + Src.Width <= 0) or (Y + Src.Height <= 0) then Exit;

  SrcX := 0;
  SrcY := 0;
  SrcWidth := Src.Width;
  SrcHeight := Src.Height;

  if X < 0 then
  begin
    SrcX := -X;
    Inc(SrcWidth, X);
    X := 0;
  end;

  if Y < 0 then
  begin
    SrcY := -Y;
    Inc(SrcHeight, Y);
    Y := 0;
  end;

  if X + SrcWidth > Dst.Width then
    Dec(SrcWidth, X + SrcWidth - Dst.Width);
  if Y + SrcHeight > Dst.Height then
    Dec(SrcHeight, Y + SrcHeight - Dst.Height);

  for I := 0 to Pred(SrcHeight) do
  begin
    PS := Src.Get8PixelPtrUnsafe(SrcX, SrcY + I);
    PD := Dst.Get8PixelPtrUnsafe(X, Y + I);
    Move(PS^, PD^, SrcWidth);
  end;
end;

procedure StretchRGB32BitmapTrunc(Dst, Src: TRGB32BitmapCore);
var
  Cols: TIntArray;
  Rows: TIntArray;
  X, Y, PX, TX, OX, PY, TY, OY: Integer;
  I: Integer;
  PD, PS, PDLine, PSLine: PRGB32Pixel;
  DstDataWidth, DstDataHeight: Integer;
  SrcDataWidth, SrcDataHeight: Integer;
  SrcRowPixelStride, DstRowPixelStride: Integer;
begin
  DstDataWidth := Dst.Width;
  DstDataHeight := Dst.Height;
  SrcDataWidth := Src.Width;
  SrcDataHeight := Src.Height;
  SrcRowPixelStride := Src.RowPixelStride;
  DstRowPixelStride := Dst.RowPixelStride;

  Cols := DivideTrunc(SrcDataWidth, DstDataWidth);
  Rows := DivideTrunc(SrcDataHeight, DstDataHeight);

  if DstDataWidth <= SrcDataWidth then
  begin
    PX := 0;
    OX := 0;
    for X := 0 to High(Cols) do
    begin
      TX := Cols[X];
      Cols[X] := (PX + TX shr 1) - OX;
      OX := (PX + TX shr 1);
      Inc(PX, TX);
    end;
  end;

  if DstDataHeight <= SrcDataHeight then
  begin
    PY := 0;
    OY := 0;
    for Y := 0 to High(Rows) do
    begin
      TY := Rows[Y];
      Rows[Y] := (PY + Rows[Y] shr 1) - OY;
      OY := (PY + TY shr 1);
      Inc(PY, TY);
    end;
  end;

  PD := PRGB32Pixel(Dst.Pixels);
  PS := PRGB32Pixel(Src.Pixels);

  for Y := 0 to High(Rows) do
  begin
    if DstDataHeight <= SrcDataHeight then
    begin
      Inc(PS, Rows[Y] * SrcRowPixelStride);
    end;

    PDLine := PD;
    PSLine := PS;

    if DstDataWidth > SrcDataWidth then
    begin
      for X := 0 to High(Cols) do
      begin
        if Cols[X] = 1 then
        begin
          PDLine^ := PSLine^;
          Inc(PDLine);
        end
        else
        begin
          FillDWord(PDLine^, Cols[X], PSLine^);
          Inc(PDLine, Cols[X]);
        end;
        Inc(PSLine);
      end;
    end
    else
    begin
      for X := 0 to High(Cols) do
      begin
        Inc(PSLine, Cols[X]);
        PDLine^ := PSLine^;
        Inc(PDLine);
      end;
    end;

    if DstDataHeight > SrcDataHeight then
    begin
      PDLine := PD;
      Inc(PD, DstRowPixelStride);
      for I := 2 to Rows[Y] do
      begin
        Move(PDLine^, PD^, DstDataWidth shl 2);
        Inc(PD, DstRowPixelStride);
      end;
      Inc(PS, SrcRowPixelStride);
    end
    else
    begin
      Inc(PD, DstRowPixelStride);
    end;
  end;
end;

procedure StretchRGB8BitmapTrunc(Dst, Src: TRGB8BitmapCore);
var
  Cols: TIntArray;
  Rows: TIntArray;
  X, Y, PX, TX, OX, PY, TY, OY: Integer;
  I: Integer;
  PD, PS, PDLine, PSLine: PRGB8Pixel;
  DstDataWidth, DstDataHeight: Integer;
  SrcDataWidth, SrcDataHeight: Integer;
  SrcRowPixelStride, DstRowPixelStride: Integer;
begin
  DstDataWidth := Dst.Width;
  DstDataHeight := Dst.Height;
  SrcDataWidth := Src.Width;
  SrcDataHeight := Src.Height;
  SrcRowPixelStride := Src.RowPixelStride;
  DstRowPixelStride := Dst.RowPixelStride;

  Cols := DivideTrunc(SrcDataWidth, DstDataWidth);
  Rows := DivideTrunc(SrcDataHeight, DstDataHeight);

  if DstDataWidth <= SrcDataWidth then
  begin
    PX := 0;
    OX := 0;
    for X := 0 to High(Cols) do
    begin
      TX := Cols[X];
      Cols[X] := (PX + TX shr 1) - OX;
      OX := (PX + TX shr 1);
      Inc(PX, TX);
    end;
  end;

  if DstDataHeight <= SrcDataHeight then
  begin
    PY := 0;
    OY := 0;
    for Y := 0 to High(Rows) do
    begin
      TY := Rows[Y];
      Rows[Y] := (PY + Rows[Y] shr 1) - OY;
      OY := (PY + TY shr 1);
      Inc(PY, TY);
    end;
  end;

  PD := PRGB8Pixel(Dst.Pixels);
  PS := PRGB8Pixel(Src.Pixels);

  for Y := 0 to High(Rows) do
  begin
    if DstDataHeight <= SrcDataHeight then
    begin
      Inc(PS, Rows[Y] * SrcRowPixelStride);
    end;

    PDLine := PD;
    PSLine := PS;

    if DstDataWidth > SrcDataWidth then
    begin
      for X := 0 to High(Cols) do
      begin
        if Cols[X] = 1 then
        begin
          PDLine^ := PSLine^;
          Inc(PDLine);
        end
        else
        begin
          FillByte(PDLine^, Cols[X], PSLine^);
          Inc(PDLine, Cols[X]);
        end;
        Inc(PSLine);
      end;
    end
    else
    begin
      for X := 0 to High(Cols) do
      begin
        Inc(PSLine, Cols[X]);
        PDLine^ := PSLine^;
        Inc(PDLine);
      end;
    end;

    if DstDataHeight > SrcDataHeight then
    begin
      PDLine := PD;
      Inc(PD, DstRowPixelStride);
      for I := 2 to Rows[Y] do
      begin
        Move(PDLine^, PD^, DstDataWidth);
        Inc(PD, DstRowPixelStride);
      end;
      Inc(PS, SrcRowPixelStride);
    end
    else
    begin
      Inc(PD, DstRowPixelStride);
    end;
  end;
end;

procedure StretchRGB32BitmapTrunc(Dst: TRGB32BitmapCore;
  DstX, DstY, DstWidth, DstHeight: Integer;
  SrcX, SrcY, SrcWidth, SrcHeight: Integer; Src: TRGB32BitmapCore);
var
  Cols: TIntArray;
  Rows: TIntArray;
  X, Y: Integer;
  SX, SY, DX, DY: Integer;
  I, J, C: Integer;
  PD, PS, PDLine, PSLine: PRGB32Pixel;
  DstDataWidth, DstDataHeight: Integer;
  SrcDataWidth, SrcDataHeight: Integer;
  SrcRowPixelStride, DstRowPixelStride: Integer;
begin
  DstDataWidth := Dst.Width;
  DstDataHeight := Dst.Height;
  SrcDataWidth := Src.Width;
  SrcDataHeight := Src.Height;
  SrcRowPixelStride := Src.RowPixelStride;
  DstRowPixelStride := Dst.RowPixelStride;

  Cols := DivideTrunc(SrcWidth, DstWidth);
  Rows := DivideTrunc(SrcHeight, DstHeight);

  if DstWidth <= SrcWidth then
    Cols := GetDifference(GetMidPoints(Cols));

  if DstHeight <= SrcHeight then
    Rows := GetDifference(GetMidPoints(Rows));

  PD := Dst.Get32PixelPtrUnsafe(DstX, DstY);
  PS := Src.Get32PixelPtrUnsafe(SrcX, SrcY);

  DY := DstY;
  SY := SrcY;

  for Y := 0 to High(Rows) do
  begin
    if DstHeight <= SrcHeight then
    begin
      Inc(PS, Rows[Y] * SrcRowPixelStride);
      Inc(SY, Rows[Y]);
    end;

    if DstHeight > SrcHeight then C := Rows[Y]
    else C := 1;

    for I := 1 to C do
    begin
      DX := DstX;
      SX := SrcX;
      PDLine := PD;
      PSLine := PS;

      if (SY >= 0) and (SY < SrcDataHeight) and (DY >= 0) and (DY < DstDataHeight) then
      begin
        if (DstWidth > SrcWidth) then
        begin
          for X := 0 to High(Cols) do
          begin
            if Cols[X] = 1 then
            begin
               if (SX >= 0) and (SX < SrcDataWidth) and (DX >= 0) and (DX < DstDataWidth) then
                PDLine^ := PSLine^;
              Inc(PDLine);
              Inc(DX);
            end
            else
            begin
              if (SX >= 0) and (SX < SrcDataWidth) then
              begin
                if (DX + Cols[X] <= 0) or (DX >= DstDataWidth) then
                begin
                  Inc(PDLine, Cols[X]);
                  Inc(DX, Cols[X]);
                end
                else
                  for J := 1 to Cols[X] do
                  begin
                    if (DX >= 0) and (DX < DstDataWidth) then
                      PDLine^ := PSLine^;
                    Inc(PDLine);
                    Inc(DX);
                  end;
              end
              else
              begin
                Inc(PDLine, Cols[X]);
                Inc(DX, Cols[X]);
              end;
            end;
            Inc(PSLine);
            Inc(SX);
          end;
        end
        else
        begin
          for X := 0 to High(Cols) do
          begin
            Inc(PSLine, Cols[X]);
            Inc(SX, Cols[X]);
            if (SX >= 0) and (SX < SrcDataWidth) and (DX >= 0) and (DX < DstDataWidth) then
              PDLine^ := PSLine^;
            Inc(PDLine);
            Inc(DX);
          end;
        end;
      end;

      if DstHeight > SrcHeight then
      begin
        Inc(PD, DstRowPixelStride);
        Inc(DY);
      end;
    end;

    if DstHeight > SrcHeight then
    begin
      Inc(PS, SrcRowPixelStride);
      Inc(SY);
    end
    else
    begin
      Inc(PD, DstRowPixelStride);
      Inc(DY);
    end;
  end;
end;

// ! SrcX < 0, SrcY < 0, SrcX + SrcWidth > Bitmap.Width, SrcY + SrcHeight > Bitmap.Height
// ! results in mash
procedure DrawRGB32Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
  Bitmap: TRGB32BitmapCore);
var
  Clip: TRect;
begin
  if (Bitmap = nil) or (Bitmap.Pixels = nil) then Exit;
  if (Bitmap.Width <= 0) or (Bitmap.Height <= 0) then Exit;
  if (SrcWidth <= 0) or (SrcHeight <= 0) then Exit;

  Clip := GetDCClipRect(Dest);

  if (DstX >= Clip.Right) or (DstY >= Clip.Bottom) or
     (DstX + SrcWidth < Clip.Left) or (DstY + SrcHeight < Clip.Top) then Exit;

  // clipping:

  ClipDimension(Clip.Left, Clip.Right, DstX, SrcX, SrcWidth);
  ClipDimension(Clip.Top, Clip.Bottom, DstY, SrcY, SrcHeight);

  WidgetSetDrawRGB32Bitmap(Dest, DstX, DstY, SrcX, SrcY, SrcWidth, SrcHeight, Bitmap);
end;

// ! SrcX < 0, SrcY < 0, SrcX + SrcWidth > Bitmap.Width, SrcY + SrcHeight > Bitmap.Height
// ! results in mash
procedure StretchDrawRGB32Bitmap(Dest: HDC; DstX, DstY, DstWidth, DstHeight: Integer;
  SrcX, SrcY, SrcWidth, SrcHeight: Integer; Bitmap: TRGB32BitmapCore);
var
  Clip: TRect;
{$IFDEF StretchRGB32}
  Temp: TRGB32BitmapCore;
  X, Y, W, H: Integer;
{$ENDIF}
begin
  if (Bitmap = nil) or (Bitmap.Pixels = nil) then Exit;
  if (Bitmap.Width <= 0) or (Bitmap.Height <= 0) then Exit;
  if (SrcWidth <= 0) or (SrcHeight <= 0) then Exit;
  if (DstWidth <= 0) or (DstHeight <= 0) then Exit;

  Clip := GetDCClipRect(Dest);

  if (DstX >= Clip.Right) or (DstY >= Clip.Bottom) or
     (DstX + DstWidth < Clip.Left) or (DstY + DstHeight < Clip.Top) then Exit;

  if (DstWidth = SrcWidth) and (DstHeight = SrcHeight) then
  begin
    DrawRGB32Bitmap(Dest, DstX, DstY, SrcX, SrcY, SrcWidth, SrcHeight, Bitmap);
    Exit;
  end;

  {$IFDEF StretchRGB32}
  X := Max(Clip.Left, DstX);
  Y := Max(Clip.Top, DstY);
  W := Min(Clip.Right, DstX + DstWidth) - X;
  H := Min(Clip.Bottom, DstY + DstHeight) - Y;

  Temp := TRGB32BitmapCore.Create(W, H);
  try
    StretchRGB32BitmapTrunc(Temp,
      DstX - X, DstY - Y, DstWidth, DstHeight,
      SrcX, SrcY, SrcWidth, SrcHeight, Bitmap);
    DrawRGB32Bitmap(Dest, X, Y, 0, 0, W, H, Temp);
  finally
    Temp.Free;
  end;
  {$ELSE}
    WidgetSetStretchDrawRGB32Bitmap(Dest, DstX, DstY, DstWidth, DstHeight,
      SrcX, SrcY, SrcWidth, SrcHeight, Bitmap);
  {$ENDIF}
end;

procedure StretchDrawRGBMaskShapePortion(Dest: HDC; DstX, DstY, DstWidth, DstHeight: Integer;
    Bitmap: TRGB8BitmapCore; DX, DY, DW, DH: Integer; BgPen: HPEN; FgPen: HPEN);
var
  ZoomX, ZoomY: Single;
  Clip: TRect;
  
  procedure DrawMask(SX, SY, EX, EY: Integer);
  var
    X, Y: Integer;
    P: PRGB8Pixel;
    V1, V2: TRGB8Pixel;
    A, B, C: Integer;
    Temp: HGDIOBJ;
  begin
    if EX >= Bitmap.Width then EX := Pred(Bitmap.Width);
    if EY >= Bitmap.Height then EY := Pred(Bitmap.Height);
    
    Temp := SelectObject(Dest, BgPen);
    try
      for Y := SY to EY do
      begin
        if Pred(SX) >= 0 then V2 := Bitmap.Get8PixelPtr(Pred(SX), Y)^
        else V2 := 0;
        P := Bitmap.Get8PixelPtr(SX, Y);
        for X := Pred(SX) to EX do
        begin
          V1 := V2;
          if X < Pred(Bitmap.Width) then V2 := P^
          else V2 := 0;

          if (V1 = $FF) and (V2 <> $FF) then
          begin
            A := DstX + Round(Succ(X) * ZoomX);
            B := DstY + Round(Y * ZoomY);
            C := DstY + Round(Succ(Y) * ZoomY);

            if ((X + Y) and $1) > 0 then
            begin
              SelectObject(Dest, BgPen);
              MoveToEx(Dest, A, B, nil);
              LineTo(Dest, A, C);
            end
            else
            begin
              SelectObject(Dest, FgPen);
              MoveToEx(Dest, A, B, nil);
              LineTo(Dest, A, C);
            end;
          end
          else
            if (V1 <> $FF) and (V2 = $FF) then
            begin
              A := DstX + Round(Succ(X) * ZoomX);
              B := DstY + Round(Y * ZoomY);
              C := DstY + Round(Succ(Y) * ZoomY);

              if ((X + Y) and $1) > 0 then
              begin
                SelectObject(Dest, BgPen);
                MoveToEx(Dest, A - 1, B, nil);
                LineTo(Dest, A - 1, C);
              end
              else
              begin
                SelectObject(Dest, FgPen);
                MoveToEx(Dest, A - 1, B, nil);
                LineTo(Dest, A - 1, C);
              end;
            end;

          Inc(P);
        end;
      end;

      for X := SX to EX do
      begin
        if Pred(SY) >= 0 then V2 := Bitmap.Get8PixelPtr(X, Pred(SY))^
        else V2 := 0;
        P := Bitmap.Get8PixelPtr(X, SY);
        for Y := Pred(SY) to EY do
        begin
          V1 := V2;
          if Y < Pred(Bitmap.Height) then V2 := P^
          else V2 := 0;

          if (V1 = $FF) and (V2 <> $FF) then
          begin
            A := DstX + Round(X * ZoomX);
            B := DstX + Round(Succ(X) * ZoomX);
            C := DstY + Round(Succ(Y) * ZoomY);

            if ((X + Y) and $1) > 0 then
            begin
              SelectObject(Dest, BgPen);
              MoveToEx(Dest, A, C, nil);
              LineTo(Dest, B, C);
            end
            else
            begin
              SelectObject(Dest, FgPen);
              MoveToEx(Dest, A, C, nil);
              LineTo(Dest, B, C);
            end;
          end
          else
            if (V1 <> $FF) and (V2 = $FF) then
            begin
              A := DstX + Round(X * ZoomX);
              B := DstX + Round(Succ(X) * ZoomX);
              C := DstY + Round(Succ(Y) * ZoomY);

              if ((X + Y) and $1) > 0 then
              begin
                SelectObject(Dest, BgPen);
                MoveToEx(Dest, A, C - 1, nil);
                LineTo(Dest, B, C - 1);
              end
              else
              begin
                SelectObject(Dest, FgPen);
                MoveToEx(Dest, A, C - 1, nil);
                LineTo(Dest, B, C - 1);
              end;
            end;

          Inc(P, Bitmap.RowPixelStride);
        end;
      end;
    finally
      SelectObject(Dest, Temp);
    end;
  end;
  
begin
  if (Bitmap = nil) or (Bitmap.Pixels = nil) then Exit;
  if (Bitmap.Width <= 0) or (Bitmap.Height <= 0) then Exit;
  if (DstWidth <= 0) or (DstHeight <= 0) then Exit;

  Clip := GetDCClipRect(Dest);
  
  ZoomX := DstWidth / Bitmap.Width;
  ZoomY := DstHeight / Bitmap.Height;
  
  if (Floor(DstX + DX * ZoomX) >= Clip.Right) or
     (Floor(DstY + DY * ZoomY) >= Clip.Bottom) or
     (Ceil(DstX + (DX + DW) * ZoomX) < Clip.Left) or
     (Ceil(DstY + (DY + DH) * ZoomY) < Clip.Top) then Exit;

  DrawMask(DX, DY, Pred(DX + DW), Pred(DY + DH));
end;

// ! SrcX < 0, SrcY < 0, SrcX + SrcWidth > Bitmap.Width, SrcY + SrcHeight > Bitmap.Height
// ! results in mash
procedure DrawRGB8Bitmap(Dest: HDC; DstX, DstY: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Integer;
  Bitmap: TRGB8BitmapCore);
var
  Clip: TRect;
begin
  if (Bitmap = nil) or (Bitmap.Pixels = nil) then Exit;
  if (Bitmap.Width <= 0) or (Bitmap.Height <= 0) then Exit;
  if (SrcWidth <= 0) or (SrcHeight <= 0) then Exit;

  Clip := GetDCClipRect(Dest);

  if (DstX >= Clip.Right) or (DstY >= Clip.Bottom) or
     (DstX + SrcWidth < Clip.Left) or (DstY + SrcHeight < Clip.Top) then Exit;

  // clipping:

  ClipDimension(Clip.Left, Clip.Right, DstX, SrcX, SrcWidth);
  ClipDimension(Clip.Top, Clip.Bottom, DstY, SrcY, SrcHeight);

  WidgetSetDrawRGB8Bitmap(Dest, DstX, DstY, SrcX, SrcY, SrcWidth, SrcHeight, Bitmap);
end;

(*
  LineBresenham - standard Bresenham's line plotting algorithm
  Note: Result depends on order of points.
*)
procedure LineBresenham(X1, Y1, X2, Y2: Integer; DrawPixel: TDrawPixelProcedure);
var
  Y, X: Integer;
  DX, DY, SX, SY, E: Integer;
begin
  DrawPixel(X1, Y1);

  if (Y1 = Y2) and (X1 = X2) then Exit;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  X := X1;
  Y := Y1;
  if DX > DY then
  begin
    E := DY - DX shr 1;

    while X <> X2 do
    begin
      if E >= 0 then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);
      Inc(E, DY);
      DrawPixel(X, Y);
    end;
  end
  else
  begin
    E := DX - DY shr 1;

    while Y <> Y2 do
    begin
      if E >= 0 then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);
      Inc(E, DX);
      DrawPixel(X, Y);
    end;
  end;
end;

procedure FillPixelRect(X1, Y1, X2, Y2: Integer; DrawPixel: TDrawPixelProcedure);
var
  X, Y: Integer;
begin
  SortRect(X1, Y1, X2, Y2);

  for Y := Y1 to Y2 do
    for X := X1 to X2 do DrawPixel(X, Y);
end;

procedure FillPixelRow(X1, X2, Y: Integer; DrawPixel: TDrawPixelProcedure); inline;
var
  X: Integer;
begin
  MinMax(X1, X2);
  for X := X1 to X2 do DrawPixel(X, Y);
end;

procedure NormalRectangle(X1, Y1, X2, Y2: Integer;
  DrawOutlinePixel, DrawFillPixel: TDrawPixelProcedure);
var
  X, Y: Integer;
begin
  SortRect(X1, Y1, X2, Y2);
  
  for X := X1 to X2 do DrawOutlinePixel(X, Y1);
  if Y1 < Y2 then
    for X := X1 to X2 do DrawOutlinePixel(X, Y2);

  for Y := Succ(Y1) to Pred(Y2) do
  begin
    DrawOutlinePixel(X1, Y);
    if X1 < X2 then DrawOutlinePixel(X2, Y);
    for X := Succ(X1) to Pred(X2) do
      DrawFillPixel(X, Y);
  end;
end;

(*
  EllipticRectangle - accurate elliptic rectangle plotting algorithm
  LX, LY - length of straight section of elliptic rectangle
  If both LX and LY are set to 0, the result is ellipse.
*)
procedure EllipticRectangle(X1, Y1, X2, Y2: Integer; LX, LY: Integer;
  DrawOutlinePixel, DrawFillPixel: TDrawPixelProcedure);
var
  CX, CY, CX1, CY1, A, B, NX, NY: Single;
  X, Y, EX, EY: Integer;
  LX1, LY1: Integer;
  LX2, LY2: Integer;
  DivSqrA, DivSqrB: Single;
  I, J, S: Integer;
  EdgeList: Array of TPoint;

  procedure AddEdge(X, Y: Integer);
  begin
    if (EdgeList[Y].X = -1) or (X < EdgeList[Y].X) then
      EdgeList[Y].X := X;
    if (EdgeList[Y].Y = -1) or (X > EdgeList[Y].Y) then
      EdgeList[Y].Y := X;
  end;
begin
  if (X1 = X2) and (Y1 = Y2) then
  begin
    DrawOutlinePixel(X1, Y1);
    Exit;
  end;

  SortRect(X1, Y1, X2, Y2);
  if (X2 - X1 = 1) or (Y2 - Y1 = 1) then
  begin
    FillPixelRect(X1, Y1, X2, Y2, DrawOutlinePixel);
    Exit;
  end;

  if (LX > X2 - X1) or (LY > Y2 - Y1) then
  begin
    NormalRectangle(X1, Y1, X2, Y2, DrawOutlinePixel, DrawFillPixel);
    Exit;
  end;

  SetLength(EdgeList, Ceil((Y2 - Y1 + 1) / 2));
  for I := 0 to Pred(High(EdgeList)) do EdgeList[I] := Point(-1, -1);
  EdgeList[High(EdgeList)] := Point(0, 0);

  A := (X2 - X1 + 1 - LX) / 2;
  B := (Y2 - Y1 + 1 - LY) / 2;
  CX := (X2 + X1 + 1) / 2;
  CY := (Y2 + Y1 + 1) / 2;

  CX1 := X2 + 1 - A - Floor(CX);
  CY1 := Y2 + 1 - B - Floor(CY);

  EX := Floor(Sqr(A) / Sqrt(Sqr(A) + Sqr(B)) + Frac(A));
  EY := Floor(Sqr(B) / Sqrt(Sqr(A) + Sqr(B)) + Frac(B));

  DivSqrA := 1 / Sqr(A);
  DivSqrB := 1 / Sqr(B);

  NY := B;
  AddEdge(Floor(CX1), Round(CY1 + B) - 1);
  for X := 1 to Pred(EX) do
  begin
    NY := B * Sqrt(1 - Sqr(X + 0.5 - Frac(A)) * DivSqrA);

    AddEdge(Floor(CX1) + X, Round(CY1 + NY) - 1);
  end;

  LX1 := Floor(CX1) + Pred(EX);
  LY1 := Round(CY1 + NY) - 1;

  NX := A;
  AddEdge(Round(CX1 + A) - 1, Floor(CY1));
  for Y := 1 to Pred(EY) do
  begin
    NX := A * Sqrt(1 - Sqr(Y + 0.5 - Frac(B)) * DivSqrB);

    AddEdge(Round(CX1 + NX) - 1, Floor(CY1) + Y);
  end;

  LX2 := Round(CX1 + NX) - 1;
  LY2 := Floor(CY1) + Pred(EY);

  if Abs(LX1 - LX2) > 1 then
  begin
    if Abs(LY1 - LY2) > 1 then AddEdge(LX1 + 1, LY1 - 1)
    else AddEdge(LX1 + 1, LY1);
  end
  else
    if Abs(LY1 - LY2) > 1 then AddEdge(LX2, LY1 - 1);

  for I := 0 to High(EdgeList) do
  begin
    if EdgeList[I].X = -1 then EdgeList[I] := Point(Round(CX1 + A) - 1, Round(CX1 + A) - 1)
    else Break;
  end;

  for J := 0 to High(EdgeList) do
  begin
    if (J = 0) and (Frac(CY) > 0) then
    begin
      for I := EdgeList[J].X to EdgeList[J].Y do
      begin
        DrawOutlinePixel(Floor(CX) + I, Floor(CY) + J);
        DrawOutlinePixel(Ceil(CX) - Succ(I), Floor(CY) + J);
      end;

      for I := Ceil(CX) - EdgeList[J].X to Floor(CX) + Pred(EdgeList[J].X) do
      begin
        DrawFillPixel(I, Floor(CY) + J);
      end;
    end
    else
      if (J = High(EdgeList)) then
      begin
        if Frac(CX) > 0 then S := -EdgeList[J].Y
        else S := -Succ(EdgeList[J].Y);

        for I := S to EdgeList[J].Y do
        begin
          DrawOutlinePixel(Floor(CX) + I, Floor(CY) + J);
          DrawOutlinePixel(Floor(CX) + I, Ceil(CY) - Succ(J));
        end;
      end
      else
      begin
        for I := EdgeList[J].X to EdgeList[J].Y do
        begin
          DrawOutlinePixel(Floor(CX) + I, Floor(CY) + J);
          DrawOutlinePixel(Floor(CX) + I, Ceil(CY) - Succ(J));
          if Floor(CX) + I <> Ceil(CX) - Succ(I) then
          begin
            DrawOutlinePixel(Ceil(CX) - Succ(I), Floor(CY) + J);
            DrawOutlinePixel(Ceil(CX) - Succ(I), Ceil(CY) - Succ(J));
          end;
        end;

        for I := Ceil(CX) - EdgeList[J].X to Floor(CX) + Pred(EdgeList[J].X) do
        begin
          DrawFillPixel(I, Floor(CY) + J);
          DrawFillPixel(I, Ceil(CY) - Succ(J));
        end;
      end;
  end;
end;

(*
  FloodFillScanLine - 4-directional scan line stack based flood fill algorithm
  with control of visited pixels.
*)
procedure FloodFillScanLine(X, Y, W, H: Integer; GetPixel: TGetPixelFunction;
  SamePixel: TSamePixelFunction; DrawPixel: TDrawPixelProcedure);
var
  S: TRGB32Pixel;
  SX, EX, I: Integer;
  Added: Boolean;
  Visited: Array of Byte;
  Stack: Array of Integer;
  StackCount: Integer;

  function CheckPixel(AX, AY: Integer): Boolean; inline;
  begin
    if Visited[AX + AY * W] = 1 then Result := False
    else
    begin
      Result := SamePixel(AX, AY, S);
    end;
  end;

  procedure Push(AX, AY: Integer); inline;
  begin
    if StackCount >= High(Stack) then SetLength(Stack, Length(Stack) shl 1);

    Stack[StackCount] := AX or (AY shl 16);
    Inc(StackCount);
  end;

  procedure Pop(var AX, AY: Integer); inline;
  begin
    Dec(StackCount);
    AX := Stack[StackCount] and $FFFF;
    AY := (Stack[StackCount] shr 16) and $FFFF;
  end;

begin
  if (X >= 0) and (X < W) and (Y >= 0) and (Y < H) then
  begin
    S := GetPixel(X, Y);

    SetLength(Stack, 1);
    StackCount := 0;

    SetLength(Visited, W * H);
    FillChar(Visited[0], Length(Visited), #0);

    Push(X, Y);
    repeat
      Pop(X, Y);
      if not CheckPixel(X, Y) then Continue;

      SX := X;
      while (SX > 0) and CheckPixel(Pred(SX), Y) do Dec(SX);
      EX := X;
      while (EX < Pred(W)) and CheckPixel(Succ(EX), Y) do Inc(EX);

      FillChar(Visited[SX + Y * W], Succ(EX - SX), #1);
      FillPixelRow(SX, EX, Y, DrawPixel);

      Added := False;
      if Y > 0 then
        for I := SX to EX do
          if CheckPixel(I, Pred(Y)) then
          begin
            if Added then Continue;
            Push(I, Pred(Y));
            Added := True;
          end
          else Added := False;

      Added := False;
      if Y < Pred(H) then
        for I := SX to EX do
          if CheckPixel(I, Succ(Y)) then
          begin
            if Added then Continue;
            Push(I, Succ(Y));
            Added := True;
          end
          else Added := False;

    until StackCount <= 0;
  end;
end;


end.

