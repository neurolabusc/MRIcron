// ***************************************************************
//  tiny madGraphics.pas           version:  1.0   ·  date: 2001-03-04
//  -------------------------------------------------------------
//  gray scaling, smooth stretching, alpha blending, ...
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2001 www.madshi.net, All Rights Reserved
// ***************************************************************

unit madGraphics;


{$R-}{$Q-}

interface

uses Windows, Graphics,define_types;



procedure StretchBitmap (srcBmp, dstBmp : TBitmap;
                         quality        : TStretchQuality = sqHigh);

implementation

uses SysUtils, Classes, Math, CommCtrl;



procedure StretchBitmap(srcBmp, dstBmp : TBitmap; quality        : TStretchQuality = sqHigh);
  procedure Bilinear32;  // 525 -> 305
  var ix, iy                   : integer;
	  x, y, xdif, ydif         : integer;
      xp1, xp2, yp             : integer;
      wy, wyi, wx              : integer;
      w11, w21, w12, w22       : integer;
      sbBits, sbLine1, sbLine2 : PByteArray;
      {smBits,} smLine1{, smLine2} : PByteArray;
	  dbLine                   : PByteArray;
      //dmLine                   : ^byte;
      sbLineDif, dbLineDif     : integer;
      //smLineDif, dmLineDif     : integer;
      w                        : integer;
begin
	xdif := (srcBmp.Width  shl 16) div (dstBmp.Width);//CR: +1 avoids slight scaling distortion
	ydif := (srcBmp.Height shl 16) div (dstBmp.Height);//CR: +1 avoids slight scaling distortion
	y := 0;
	sbBits := srcBmp.ScanLine[0];
    if srcBmp.Height > 1 then
         sbLineDif := integer(srcBmp.ScanLine[1]) - integer(sbBits)
    else sbLineDif := 0;
    dbLine := dstBmp.ScanLine[0];
	if dstBmp.Height > 1 then
		 dbLineDif := integer(dstBmp.ScanLine[1]) - integer(dbLine) - 4 * dstBmp.Width
    else dbLineDif := 0;
      //smBits    := nil;
      //smLineDif := 0;
      //dmLine    := nil;
      //dmLineDif := 0;

	w := srcBmp.Width - 1;
	for iy := 0 to dstBmp.Height - 1 do begin
	  yp := y shr 16;
	  integer(sbLine1) := integer(sbBits) + sbLineDif * yp;
	  integer(smLine1) := integer({smBits}nil) {+ smLineDif * yp};
	  if yp < srcBmp.Height - 1 then begin
		integer(sbLine2) := integer(sbLine1) + sbLineDif;
	  end else begin
		sbLine2 := sbLine1;
		//smLine2 := smLine1;
	  end;
	  x   := 0;
	  wy  :=      y  and $FFFF;
	  wyi := (not y) and $FFFF;
	  for ix := 0 to dstBmp.Width - 1 do begin
		xp1 := x shr 16;
		if xp1 < w then xp2 := xp1 + 1
		else            xp2 := xp1;
		wx  := x and $FFFF;
		w21 := (wyi * wx) shr 16; w11 := wyi - w21;
		w22 := (wy  * wx) shr 16; w12 := wy  - w22;
		{if smLine1 <> nil then begin
		  w11 := (w11 * (256 - smLine1^[xp1])) shr 8;
		  w21 := (w21 * (256 - smLine1^[xp2])) shr 8;
		  w12 := (w12 * (256 - smLine2^[xp1])) shr 8;
		  w22 := (w22 * (256 - smLine2^[xp2])) shr 8;
		  dmLine^ := 255 - byte((w11 + w21 + w12 + w22) shr 8);
		end;}
		xp1 := xp1 * 4;
		xp2 := xp2 * 4;
		{blue}dbLine^[0] := (sbLine1[xp1    ] * w11 + sbLine1[xp2    ] * w21 + sbLine2[xp1    ] * w12 + sbLine2[xp2    ] * w22) shr 16;
		{green}dbLine^[1] := (sbLine1[xp1 + 1] * w11 + sbLine1[xp2 + 1] * w21 + sbLine2[xp1 + 1] * w12 + sbLine2[xp2 + 1] * w22) shr 16;
		{red}dbLine^[2] := (sbLine1[xp1 + 2] * w11 + sbLine1[xp2 + 2] * w21 + sbLine2[xp1 + 2] * w12 + sbLine2[xp2 + 2] * w22) shr 16;
		inc(integer(dbLine), 4);
		//inc(dmLine);
		inc(x, xdif);
		//if ix = 0 then
		//	inc(x, Hlfxdif);
	  end;
	  inc(integer(dbLine), dbLineDif);
	  //inc(integer(dmLine), dmLineDif);
	  inc(y, ydif);
		//if iy = 0 then
		//	inc(y, Hlfydif);
	end;
end;
begin
  case quality of
    sqLow      : dstBmp.Canvas.StretchDraw(Rect(0, 0, dstBmp.Width, dstBmp.Height), srcBmp);
    sqHigh     : Bilinear32;
  end; //case
end;
end.
