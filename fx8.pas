unit fx8;
{$DEFINE VFLIP}
{$mode objfpc}{$H+}

interface

uses
  SysUtils,  define_types, Classes,dialogs{, Graphics, Controls, Forms, Dialogs,Menus,ComCtrls, ExtCtrls};
type
  Tfx8 =  RECORD
      
      Width,Height,X,Y,PenThick: integer;
      Img: Bytep;
  end;
procedure CreateFX8(var lFX8: Tfx8);
procedure DefineFX8(var lFX8: Tfx8; lWid,lHt: integer);
procedure DefineBuffFX8(var lFX8: Tfx8; lWid,lHt: integer; lBuff: ByteP);
procedure CopyFX8(var lFX8src, lFX8dest: Tfx8);
procedure RectangleFX8(var lFX8: Tfx8; lLin,lTin,lRin,lBin: integer; lClr: byte);
procedure FillRectFX8(var lFX8: Tfx8; lLin,lTin,lRin,lBin: integer; lClr: byte);
procedure EllipseFX8(var lFX8: Tfx8; lLin,lTin,lRin,lBin: integer; lClr: byte);
procedure FillEllipseFX8(var lFX8: Tfx8; lLin,lTin,lRin,lBin: integer; lClr: byte);
procedure MoveToFX8(var lFX8: Tfx8; lXin,lYin: integer);
procedure LineToFX8(var lFX8: Tfx8; lXin,lYin: integer; lClr: byte); overload;
procedure LineToFX8(var lFX8: Tfx8; lXin,lYin: integer; lClr, lLineThick: byte); overload;
procedure FloodFillFX8 (var lFX8: Tfx8;  lXin, lYin: Integer; lBoundClr,lWriteClr: byte; lfsSurface: boolean);
procedure FreeFX8(var lFX8: Tfx8);



implementation
uses nifti_img_view;

function FX8x( lXin: integer): integer;
begin
     result := lXin ;
end;

function FX8y(var lFX8: Tfx8;  lYin: integer): integer;
begin
 {$IFDEF VFLIP}
 result := lFX8.Height- lYin + 1;
 {$ELSE}
 result := lYin;
 {$ENDIF}
end;

procedure sortLTRB(var lXoutLow,lYOutLow,lXoutHi,lYOutHi: integer); //left<right, top<bottom
var lXin1,lYin1,lXin2,lYin2: integer;
begin
	lXin1 := lXoutLow;
	lYin1 := lYOutLow;
	lXin2 := lXoutHi;
	lYin2 := lYOutHi;
	if lXIn1 < lXin2 then begin
	   lXoutLow := lXIn1;
	   lXOutHi := lXIn2;
	end else begin
	   lXoutLow := lXIn2;
	   lXOutHi := lXIn1;
	end;
	if lYIn1 < lYin2 then begin
	   lYoutLow := lYIn1;
	   lYOutHi := lYIn2;
	end else begin
	   lYoutLow := lYIn2;
	   lYOutHi := lYIn1;
	end;
end; //sortLTRB

procedure Bound(var lX,lY: integer; var lFX8: TFX8);
begin
        if lX < 1 then
           lX := 1;
        if lX > lFX8.width then
           lX := lFX8.width;
        if lY < 1 then
           lY := 1;
        if lY > lFX8.height then
           lY := lFX8.height;

end;

procedure boundrect(var lL,lT,lR,lB: integer; var lFX8: TFX8);
begin
        sortLTRB(lL,lT,lR,lB);
        bound(lL,lT,lFX8);
        bound(lR,lB,lFX8);
end;

procedure MoveToFX8(var lFX8: Tfx8; lXin,lYin: integer);
var
   lX,lY: integer;
begin
     lX := FX8x(lXin); lY := FX8y(lFX8, lYin);
     bound(lX,lY,lFX8);
     lFX8.X := lX;
     lFX8.Y := lY;
end;

procedure HorLine(var lFX8: Tfx8; x1,x2,y: integer; lClr: byte);
var
   x,lStart: integer;
begin
  if lFX8.img = nil then exit; //not defined
  lStart := (y -1)* lFX8.Width;
  if x1 < x2 then begin
     for x := x1 to x2 do
         lFX8.Img^[lStart+x] := lClr;
  end else
     for x := x2 to x1 do
         lFX8.Img^[lStart+x] := lClr;
end;


function isOutOfBounds(var lFX8: Tfx8; var x,y:integer): boolean;
var
   iy: integer;
begin
  iy := y;
     x := FX8x(x); y := FX8y(lFX8, y);

     if (x < 0) or (y < 0) or (x > lFX8.Width) or (y > lFX8.Height) then begin
        imgform.StatusLabel.caption := inttostr(iy)+'pixel error '+inttostr(x)+'x'+inttostr(y)+'   '+inttostr(lFX8.Width)+'x'+inttostr(lFX8.Height);
        result := true;
        exit;

     end;
     result := false;
end;

function getpixel(var lFX8: Tfx8; x,y: integer): byte;
begin
  result := lFX8.Img^[(Y -1)* lFX8.Width+x];
end;

procedure putpixel(var lFX8: Tfx8; x,y: integer; lClr: byte);
begin
  if (x < 1) or (y < 1) or (x > lFX8.width) or (y > lFX8.height) then
     exit; //putwidepixel and puttallpixel can have x < 1, x > width, etc...
  lFX8.Img^[(Y -1)* lFX8.Width+x] := lClr;
end;

procedure putwidepixel(var lFX8: Tfx8; x,y: integer; lClr, lLineThick: byte);
var lBar: integer;
begin
     putpixel(lFX8,x,y,lClr);
     if lLineThick < 2 then exit;
     for lBar := 1 to ((lLineThick-1) div 2) do begin
         putpixel(lFX8,x-lBar,y,lClr);
         putpixel(lFX8,x+lBar,y,lClr);
     end;
end;

procedure puttallpixel(var lFX8: Tfx8; x,y: integer; lClr, lLineThick: byte);
var lBar: integer;
begin
     putpixel(lFX8,x,y,lClr);
     if lLineThick < 2 then exit;
     for lBar := 1 to ((lLineThick-1) div 2) do begin
         putpixel(lFX8,x,y-lBar,lClr);
         putpixel(lFX8,x,y+lBar,lClr);
     end;
end;

procedure LineToFX8(var lFX8: Tfx8; lXin,lYin: integer; lClr, lLineThick: byte) ; overload;
var
   lSlope: single;
   lX2,lY2,lX1,lY1,lP,lCol,lStart,lX,lY: integer;
begin
     lX1 := lFX8.X;
     lY1 := lFX8.Y;
     lX2 := FX8x(lXin); lY2 := FX8y(lFX8, lYin);
     Bound(lX2,lY2,lFX8);
     lFX8.X := lX2;
     lFX8.Y := lY2;
     //next: endpoints - required if no line
     lFX8.Img^[(lY1 -1)* lFX8.Width+lX1] := lClr;
     lFX8.Img^[(lY2 -1)* lFX8.Width+lX2] := lClr;
     if (lX1 = lX2) and (lY1 = lY2) then
        exit;
     if abs(lY1-lY2) > abs(lX1-lX2) then begin //mostly vertical
        if lY1 > lY2 then begin
             lSlope := (lX1-lX2) /(lY1-lY2);
             for lY := lY2 to lY1 do
                 putwidepixel(lFX8,lX2+round(lSlope*(lY-lY2)),lY, lClr, lLineThick);
                 //lFX8.Img^[((lY  -1)* lFX8.Width)+lX2+round(lSlope*(lY-lY2))] := lClr;
        end else begin
             lSlope := (lX2-lX1) /(lY2-lY1);
             for lY := lY1 to lY2 do
                 putwidepixel(lFX8,lX1+round(lSlope*(lY-lY1)),lY, lClr, lLineThick);
                 //lFX8.Img^[((lY  -1)* lFX8.Width)+lX1+round(lSlope*(lY-lY1))] := lClr;
        end;
     end else begin //mostly horizontal - primary change in X
        if lX1 > lX2 then begin
             lSlope := (lY1-lY2) /(lX1-lX2);
             for lX := lX2 to lX1 do
                 puttallpixel(lFX8,lX,lY2+round(lSlope*(lX-lX2) ) , lClr, lLineThick);
                 //lFX8.Img^[((lY2+round(lSlope*(lX-lX2) ) -1)* lFX8.Width)+lX] := lClr;
        end else begin
              lSlope := (lY2-lY1) /(lX2-lX1);
             for lX := lX1 to lX2 do
                 puttallpixel(lFX8,lX,lY1+round(lSlope*(lX-lX1) ) , lClr, lLineThick);
                 //lFX8.Img^[((lY1+round(lSlope*(lX-lX1) ) -1)* lFX8.Width)+lX] := lClr;
        end;
     end;
end;

procedure LineToFX8(var lFX8: Tfx8; lXin,lYin: integer; lClr: byte); overload;
//for speed: lSingle could use integer math
var
   lSlope: single;
   lX2,lY2,lX1,lY1,lX,lY: integer;
begin
     if lFX8.PenThick > 2 then begin
        LineToFX8(lFX8,lXin,lYin, lClr,lFX8.PenThick);
        exit;
     end;
     lX1 := lFX8.X;
     lY1 := lFX8.Y;
     lX2 := FX8x(lXin); lY2 := FX8y(lFX8, lYin);
     Bound(lX2,lY2,lFX8);
     lFX8.X := lX2;
     lFX8.Y := lY2;
     //next: endpoints - required if no line
     lFX8.Img^[(lY1 -1)* lFX8.Width+lX1] := lClr;
     lFX8.Img^[(lY2 -1)* lFX8.Width+lX2] := lClr;
     if (lX1 = lX2) and (lY1 = lY2) then
        exit;
     if abs(lY1-lY2) > abs(lX1-lX2) then begin //mostly vertical
        if lY1 > lY2 then begin
             lSlope := (lX1-lX2) /(lY1-lY2);
             for lY := lY2 to lY1 do
                 lFX8.Img^[((lY  -1)* lFX8.Width)+lX2+round(lSlope*(lY-lY2))] := lClr;
        end else begin
             lSlope := (lX2-lX1) /(lY2-lY1);
             for lY := lY1 to lY2 do
                 lFX8.Img^[((lY  -1)* lFX8.Width)+lX1+round(lSlope*(lY-lY1))] := lClr;
        end;
     end else begin //mostly horizontal - primary change in X
        if lX1 > lX2 then begin
             lSlope := (lY1-lY2) /(lX1-lX2);
             for lX := lX2 to lX1 do
                 lFX8.Img^[((lY2+round(lSlope*(lX-lX2) ) -1)* lFX8.Width)+lX] := lClr;
        end else begin
              lSlope := (lY2-lY1) /(lX2-lX1);
             for lX := lX1 to lX2 do
                 lFX8.Img^[((lY1+round(lSlope*(lX-lX1) ) -1)* lFX8.Width)+lX] := lClr;
        end;
     end;
end;

Procedure FillEllipseDefault(var lFX8: Tfx8; X,Y: smallint;XRadius: word;
    YRadius:word; lClr: byte);
   Const ConvFac = Pi/180.0;

   var
    j, Delta, DeltaEnd: single;
    NumOfPixels: longint;
    TempTerm: single;
    xtemp, ytemp, xp, yp, xm, ym, xnext, ynext,
      plxpyp, plxmyp, plxpym, plxmym: smallint;
    BackupColor, TmpAngle, OldLineWidth: word;
  Begin

   If xradius = 0 then inc(xradius);
   if yradius = 0 then inc(yradius);
   { check for an ellipse with negligable x and y radius }
   If (xradius <= 1) and (yradius <= 1) then begin
       putpixel(lFX8, x,y, lClr);
       exit;
   end;
   { approximate the number of pixels required by using the circumference }
   { equation of an ellipse.                                              }
   { Changed this formula a it (trial and error), but the net result is that }
   { less pixels have to be calculated now                                   }
   NumOfPixels:=Round(Sqrt(3)*sqrt(sqr(XRadius)+sqr(YRadius)));
   { Calculate the angle precision required }
   Delta := 90.0 / NumOfPixels;
   { for restoring after PatternLine }

   { removed from inner loop to make faster }
   { Always just go over the first 90 degrees. Could be optimized a   }
   { bit if StAngle and EndAngle lie in the same quadrant, left as an }
   { exercise for the reader :) (JM)                                  }
   j := 0;
   { calculate stop position, go 1 further than 90 because otherwise }
   { 1 pixel is sometimes not drawn (JM)                             }
   DeltaEnd := 91;
   { Calculate points }
   xnext := XRadius;
   ynext := 0;
   Repeat
     xtemp := xnext;
     ytemp := ynext;
     { this is used by both sin and cos }
     TempTerm := (j+Delta)*ConvFac;
     { Calculate points }
     xnext := round(XRadius*Cos(TempTerm));
     ynext := round(YRadius*Sin(TempTerm+Pi));
     xp := x + xtemp;
     xm := x - xtemp;
     yp := y + ytemp;
     ym := y - ytemp;
     plxpyp := maxsmallint;
     plxmyp := -maxsmallint-1;
     plxpym := maxsmallint;
     plxmym := -maxsmallint-1;
     plxpyp := xp;
     PutPixel(lFX8,xp,yp,lClr);
     plxmyp := xm;
     PutPixel(lFX8,xm,yp,lClr);
     plxmym := xm;
     PutPixel(lFX8,xm,ym,lClr);
     plxpym := xp;
     PutPixel(lFX8,xp,ym,lClr);
     If (ynext <> ytemp) and
        (xp - xm >= 1) then
       begin
         //CurrentColor := FillSettings.Color;
         HorLine(lFX8,plxmyp+1,plxpyp-1,yp,lClr);
         HorLine(lFX8,plxmym+1,plxpym-1,ym,lClr);
         //CurrentColor := BackupColor;*)
       end;
     j:=j+Delta;
   Until j > (DeltaEnd);
  end;

  Procedure EllipseDefault(var lFX8: Tfx8; X,Y: smallint;XRadius: word;
    YRadius:word; lClr: byte);
   Const ConvFac = Pi/180.0;

   var
    j, Delta, DeltaEnd: single;
    NumOfPixels: longint;
    TempTerm: single;
    xtemp, ytemp, xp, yp, xm, ym, xnext, ynext,
      plxpyp, plxmyp, plxpym, plxmym: smallint;
    BackupColor, TmpAngle, OldLineWidth: word;
  Begin

   If xradius = 0 then inc(xradius);
   if yradius = 0 then inc(yradius);
   { check for an ellipse with negligable x and y radius }
   If (xradius <= 1) and (yradius <= 1) then begin
       putpixel(lFX8, x,y, lClr);
       exit;
   end;
   { approximate the number of pixels required by using the circumference }
   { equation of an ellipse.                                              }
   { Changed this formula a it (trial and error), but the net result is that }
   { less pixels have to be calculated now                                   }
   NumOfPixels:=Round(Sqrt(3)*sqrt(sqr(XRadius)+sqr(YRadius)));
   { Calculate the angle precision required }
   Delta := 90.0 / NumOfPixels;
   { for restoring after PatternLine }

   { removed from inner loop to make faster }
   { Always just go over the first 90 degrees. Could be optimized a   }
   { bit if StAngle and EndAngle lie in the same quadrant, left as an }
   { exercise for the reader :) (JM)                                  }
   j := 0;
   { calculate stop position, go 1 further than 90 because otherwise }
   { 1 pixel is sometimes not drawn (JM)                             }
   DeltaEnd := 91;
   { Calculate points }
   xnext := XRadius;
   ynext := 0;
   Repeat
     xtemp := xnext;
     ytemp := ynext;
     { this is used by both sin and cos }
     TempTerm := (j+Delta)*ConvFac;
     { Calculate points }
     xnext := round(XRadius*Cos(TempTerm));
     ynext := round(YRadius*Sin(TempTerm+Pi));
     xp := x + xtemp;
     xm := x - xtemp;
     yp := y + ytemp;
     ym := y - ytemp;
     PutPixel(lFX8,xp,yp,lClr);
     PutPixel(lFX8,xm,yp,lClr);
     PutPixel(lFX8,xm,ym,lClr);
     PutPixel(lFX8,xp,ym,lClr);
     j:=j+Delta;
   Until j > (DeltaEnd);
  end;


procedure EllipseFX8(var lFX8: Tfx8; lLin,lTin,lRin,lBin: integer; lClr: byte);
var
	lL,lT,lR,lB,lP,lStart: integer;
begin
	if lFX8.img = nil then exit; //not defined
        lL := FX8x(lLin); lB := FX8y(lFX8, lBin);
        lR := FX8x(lRin); lT := FX8y(lFX8, lTin);

         BoundRect(lL,lT,lR,lB,lFX8);
        EllipseDefault(lFX8, (lL+lR) shr 1,(lT+lB) shr 1, (lR-lL) shr 1, (lB-lT) shr 1,lClr);
end;

procedure FillEllipseFX8(var lFX8: Tfx8; lLin,lTin,lRin,lBin: integer; lClr: byte);
var
	lL,lT,lR,lB,lP,lStart: integer;
begin
	if lFX8.img = nil then exit; //not defined
        lL := FX8x(lLin); lB := FX8y(lFX8, lBin);
        lR := FX8x(lRin); lT := FX8y(lFX8, lTin);

         BoundRect(lL,lT,lR,lB,lFX8);
        FillEllipseDefault(lFX8, (lL+lR) shr 1,(lT+lB) shr 1, (lR-lL) shr 1, (lB-lT) shr 1,lClr);
end;

procedure RectangleFX8(var lFX8: Tfx8; lLin,lTin,lRin,lBin: integer; lClr: byte);
var
	lL,lT,lR,lB,lP,lStart: integer;
begin
	if lFX8.img = nil then exit; //not defined
                lL := FX8x(lLin); lB := FX8y(lFX8, lBin);
        lR := FX8x(lRin); lT := FX8y(lFX8, lTin);

          BoundRect(lL,lT,lR,lB,lFX8);
         //top line
	lStart := (lT -1)* lFX8.Width;
	for lP := lL to lR do
		lFX8.Img^[lStart+lP] := lClr;	
	//bottom line
	lStart := (lB -1)* lFX8.Width;
	for lP := lL to lR do
		lFX8.Img^[lStart+lP] := lClr;
	//left and right lines
	lStart := (lT -1)* lFX8.Width;
	for lP := lT to lB do begin
		lFX8.Img^[lStart+lL] := lClr;
		lFX8.Img^[lStart+lR] := lClr;
		lStart := lStart + lFX8.Width; 
	end;
end;

procedure FillRectFX8(var lFX8: Tfx8; lLin,lTin,lRin,lBin: integer; lClr: byte);
var
	lL,lT,lR,lB,lRow,lCol,lStart: integer;
begin
	if lFX8.img = nil then exit; //not defined
        lL := FX8x(lLin); lB := FX8y(lFX8, lBin);
        lR := FX8x(lRin); lT := FX8y(lFX8, lTin);

         BoundRect(lL,lT,lR,lB,lFX8);
	lStart := (lT -1)* lFX8.Width;
	for lRow := lT to lB do begin
            for lCol := lL to lR do
  	        lFX8.Img^[lStart+lCol] := lClr;
	    lStart := lStart + lFX8.Width;
	end;
end;

procedure DefineFX8(var lFX8: Tfx8; lWid,lHt: integer);
begin
	if (lFX8.img = nil) or (lWid <> lFX8.Width) or (lHt <> lFX8.Height) then begin
		if lFX8.img <> nil then 
			freemem(lFX8.Img);
		Getmem(lFX8.img, lWid*lHt);
		lFX8.Height := lHt;
		lFX8.Width := lWid;
	end; 
        fillchar(lFX8.Img^,lWid*lHt,0);
	lFX8.X := 1;
	lFX8.Y := 1;
end;

procedure DefineBuffFX8(var lFX8: Tfx8; lWid,lHt: integer; lBuff: ByteP);
begin
     if lBuff = nil then exit;
     DefineFX8(lFX8, lWid,lHt);
     Move(lBuff^,lFX8.Img^,lWid*lHt);
end;

procedure CopyFX8(var lFX8src, lFX8dest: Tfx8);
begin
     if (lFX8src.Img = nil) then
        exit;
     DefineFX8(lFX8dest, lFX8src.Width,lFX8src.Height);
     Move(lFX8src.Img^,lFX8dest.Img^,lFX8src.Width*lFX8src.Height);
end;

procedure FloodFillFX8 (var lFX8: Tfx8;  lXin, lYin: Integer; lBoundClr,lWriteClr: byte; lfsSurface: boolean);
//Written by Chris Rorden
//A simple first-in-first-out circular buffer (the queue) for flood-filling contiguous voxels.
//This algorithm avoids stack problems associated simple recursive algorithms
//http://steve.hollasch.net/cgindex/polygons/floodfill.html
const
     kFill = 0; //pixels we will want to flood fill
     kFillable = 128; //voxels we might flood fill
     kUnfillable = 255; //voxels we can not flood fill
var
  lWid,lHt,lQSz,lQHead,lQTail: integer;
  lQRA: LongIntP;
  lMaskRA: ByteP;
procedure IncQra(var lVal, lQSz: integer);//nested inside FloodFill
begin
    inc(lVal);
    if lVal >= lQSz then
       lVal := 1;
end; //nested Proc IncQra
function Pos2XY (lPos: integer): TPoint;
begin
    result.X := ((lPos-1) mod lWid)+1; //horizontal position
    result.Y := ((lPos-1) div lWid)+1; //vertical position
end; //nested Proc Pos2XY
procedure TestPixel(lPos: integer);
begin
     if (lMaskRA^[lPos]=kFillable) then begin
        lMaskRA^[lPos] := kFill;
        lQra^[lQHead] := lPos;
        incQra(lQHead,lQSz);
     end;
end; //nested Proc TestPixel
procedure RetirePixel; //nested inside FloodFill
var
   lVal: integer;
   lXY : TPoint;
begin
   lVal := lQra^[lQTail];
   lXY := Pos2XY(lVal);
   if lXY.Y > 1 then
        TestPixel (lVal-lWid);//pixel above
   if lXY.Y < lHt then
      TestPixel (lVal+lWid);//pixel below
   if lXY.X > 1 then
        TestPixel (lVal-1); //pixel to left
   if lXY.X < lWid then
      TestPixel (lVal+1); //pixel to right
   incQra(lQTail,lQSz); //done with this pixel
end; //nested proc RetirePixel
const
     kIndex0or1 = 0;
var
   lTargetColorVal,lDefaultVal: byte;
   lX,lY,lPos,x,y: integer;
begin //FloodFill
 X := lXin; Y := lYin;
 if isOutOfBounds(lFX8, X,Y) then exit;
 //lX := FX8x(lXin); lY := FX8y(lFX8, lYin);
  //    imgform.StatusLabel.caption := 'pixel error '+inttostr(X)+'x'+inttostr(Y);
 //exit;
  if  lfsSurface then begin
     if getpixel(lFX8, x,y) <> lBoundClr then exit;
     lTargetColorVal := kFillable;
     lDefaultVal := kUnfillable;
  end else begin //fsBorder
      //fill non-target color with brush - bounded by target-color
     if getpixel(lFX8, x,y) = lBoundClr then exit;
     lTargetColorVal := kUnfillable;
     lDefaultVal := kFillable;
  end;
  lHt := lFX8.Height;
  lWid := lFX8.Width;
  lQSz := lHt * lWid;
  //Qsz should be more than the most possible simultaneously active pixels
  //Worst case scenario is a click at the center of a 3x3 image: all 9 pixels will be active simultaneously
  //for larger images, only a tiny fraction of pixels will be active at one instance.
  //perhaps lQSz = ((lHt*lWid) div 4) + 32; would be safe and more memory efficient
  if (lHt < 1) or (lWid < 1) then exit;
  getmem(lQra,lQSz*sizeof(longint)); //very wasteful -
  getmem(lMaskRA,lHt*lWid*sizeof(byte));
  for lPos := 1 to (lHt*lWid) do
      if lFX8.Img^[lPos] = lBoundClr then
         lMaskRA^[lPos] := lTargetColorVal //assume all voxels are non targets
        else
         lMaskRA^[lPos] := lDefaultVal; //assume all voxels are non targets

  lQHead := 2;
  lQTail := 1;
  lQra^[lQTail] := (((Y-1) * lWid)+X+kIndex0or1); //NOTE: both X and Y start from 0 not 1
  lMaskRA^[lQra^[lQTail]] := kFill;
 RetirePixel;
 {for lPos := 1 to 100 do
     RetirePixel;}
  while lQHead <> lQTail do
        RetirePixel;

  lPos := 0;

  for lY := 0 to (lHt-1) do
      for lX := 0 to (lWid-1) do begin
          lPos := lPos + 1;
          if lMaskRA^[lPos] = kFill then
             lFX8.Img^[lPos] := lWriteClr;
      end;
  freemem(lMaskRA);
  freemem(lQra);
end;// proc FloodFill


procedure CreateFX8(var lFX8: Tfx8);
begin
	lFX8.Img := nil;
end;

procedure FreeFX8(var lFX8: Tfx8);
begin
	if lFX8.Img <> nil then
		Freemem(lFX8.Img);
	lFX8.Img := nil;
end;


end.

