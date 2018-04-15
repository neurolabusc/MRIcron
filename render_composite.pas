unit render_composite;
interface
{$include isthreaded.inc}
uses
{$IFDEF Unix}
lclintf, //gettickcount
{$ELSE}
Windows,
{$ENDIF}
{$IFNDEF NoThreads}
 RenderThds,
{$ELSE}
rendernothreads,
{$ENDIF}
{$IFDEF FPC}
 LResources, //not sure if this is used...
 {$ENDIF}
 SysUtils, GraphicsMathLibrary,Classes, Graphics, Controls, Forms, Dialogs,ExtCtrls,Buttons,
 nifti_img, nifti_hdr,define_types,nifti_img_view,StdCtrls, Menus,ClipBrd,ReadInt,cutout,IniFiles,
 ComCtrls, nifti_types, nii_label;
type
   TRender =  record
           Zoom: single;
           Cutout,CutoutFrac: TCutout;
	 //BGNearClipFrac, BGNearClip,OverlayNearClipFrac,OverlayNearClip,
         ClipFrac,
         Azimuth,Elevation,cutoutLUTindex,ShadePct,
	 OverlayFromBGSurface,BGSurface,OverlaySurface,BGDepth,OverlayDepth,CutoutBias,Gain,Bias: integer;
	 SmoothBG,SmoothOverlay,Trilinear,ShowCutout,FlipLR: boolean;
  end;
  procedure VolumeRotateMatrix (var lBGImg: TBGImg; var lHdr: TMRIcroHdr; var lMatrixIn: TMatrix; lBilinearSmooth,lRenderCutout,lIsBG: boolean{;lNearSlicesClipIn: integer});
  procedure DrawRender;
  procedure SliceToFrac(var lBGImg: TBGImg);

var

  gRender:TRender;
const
	kBelow = 1;
	kInFront = 2;

implementation

uses math,render;

procedure MinMaxFilt (var lHdr: TMRIcroHdr; var lFiltMin8bit, lFiltMax8bit: integer);var lMin,lMax: single;
begin
ReturnMinMax (lHdr,lMin,lMax, lFiltMin8bit, lFiltMax8bit);
end;


procedure Smooth2DImage (lX,lY: integer; lInBuffer: ByteP);
var
	lSmoothBuffer: ByteP;
	lLine,lLineStart,lInc,lOutPixel,lV: integer;
begin
	 GetMem (lSmoothBuffer ,  lX*lY);
	 FillChar(lSmoothBuffer^,lX*lY, 0); //zero array
	  for lLine:= (lY-1) downto 2 do begin
		 lLineStart := ((lLine-1)*(lX));
		 for lInc := (lX-1) downto 2 do begin
			  lOutPixel := lLineStart+lInc;
				 lV := (lInBuffer^[lOutPixel] shl 3)
				   +(lInBuffer^[lOutPixel+1] shl 1)+(lInBuffer^[lOutPixel-1] shl 1)
				   +(lInBuffer^[lOutPixel+lX] shl 1)+(lInBuffer^[lOutPixel-lX] shl 1)
				   +(lInBuffer^[lOutPixel+lX+1])+(lInBuffer^[lOutPixel+lX-1])
				   +(lInBuffer^[lOutPixel-lX+1])+(lInBuffer^[lOutPixel-lX-1])
				   ;
				 lV := lV div 20;
				 lSmoothBuffer^[lOutPixel] := lV;//lV;
		 end; //for each column
	 end; //for each line (row)
  Move(lSmoothBuffer^,lInBuffer^,lX*lY);
  //Move(lSmoothBuffer^[1],lInBuffer[1]^,lX*lY);
	 FreeMem(lSmoothBuffer);
end; //proc Smooth2DImage

procedure CreateOverlayRenderInfrontNear(var lBGHdr,lHdr: TMRIcroHdr; var lX,lY,lZ,lInRenderSurface,lInRenderDepth: Integer; var lQuadP: RGBQuadp; Smooth2D: boolean);
//changes Aug2007 - make sure search depth is not MAxInt - we get wrap around
var
	lSrc,lOutBuffer: Bytep;
        lLow,lHigh,
	lIntensity,lDepth,lPixel,lSliceSz,lRenderSurface,lRenderDepth,lSamples: integer;
begin
  if gBGImg.RenderDepthBufferItems < 1 then exit;
  lSrc := lHdr.RenderBuffer;//lHdr.ScrnBuffer;
  lSliceSz := lX*lY;
  //lVolSz := lSliceSz * lZ;
  GetMem (lOutBuffer ,  lSliceSz);
  fillchar(lOutBuffer^,lSliceSz,0);
  lRenderSurface := lInRenderSurface;
  if (lHdr.IMgBufferItems > 0) {2/2008} and (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then
	lRenderSurface := 1;
  for lPixel := 1 to lSliceSz do begin
      if gBGImg.RenderDepthBuffer^[lPixel] <> 0 then begin //background surface at this voxel
         lIntensity := 0;
         lSamples := 0;
         if gBGImg.RenderDepthBuffer^[lPixel] < 0 then
             lRenderDepth := (abs(gBGImg.RenderDepthBuffer^[lPixel])-1)+1
         else
             lRenderDepth := (abs(gBGImg.RenderDepthBuffer^[lPixel])-1)+lInRenderDepth;
         if lRenderDepth >= lX then
            lRenderDepth := lX-1;
         lDepth := ((lPixel-1)* lX)+1;
         lRenderDepth := lDepth + lRenderDepth;
         while (lDepth < lRenderDepth)  do begin
               if (lSrc^[lDepth] > lRenderSurface)  then begin
                  lIntensity := lIntensity+lSrc^[lDepth];
                  inc(lSamples);
               end;

               inc(lDepth);
         end;
         if lSamples > 0 then
            lOutBuffer^[lPixel]:= lIntensity div lSamples;
  end; //for each pixel with a background image
end; //for each pixel
  (*for lPixel := 1 to lSliceSz do begin
	if gBGImg.RenderDepthBuffer^[lPixel] <> 0 then begin //background surface at this voxel
		lDepth := 0;
		lIntensity := 0;
                lSliceOffset := 0;
                lSamples := 0;
                lRenderDepth := (abs(gBGImg.RenderDepthBuffer^[lPixel])-1)+lInRenderDepth;
		while (lDepth < lRenderDepth) and (lSliceOffset < lVolSz) do begin
			if (lSrc^[lSliceOffset+lPixel] > lRenderSurface)  then begin
				lIntensity := lIntensity+lSrc^[lSliceOffset+lPixel];
                                inc(lSamples);
                        end;
			inc(lSliceOffset,lSliceSz);
			inc(lDepth);
			if gBGImg.RenderDepthBuffer^[lPixel] < 0 then
				lDepth := lRenderDepth; //only show surface for cutout
		end;
                if lSamples > 0 then
			lOutBuffer^[lPixel]:= lIntensity div lSamples;
	end ; //if background
  end;    *)
  //showmessage(inttostr(lHdr.NIFTIhdr.intent_code ));
  //if (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then showmessage('xx');
  if (Smooth2D) and (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then //do not smooth labels
	Smooth2DImage (lX,lY, lOutBuffer);
//Mar2007 start
if lHdr.LUTfromZero then begin
  MinMaxFilt(lHdr,lLow,lHigh);
  //fx(lLow,lHigh);
  if lLow > 0 then
    for lPixel := 1 to (lSliceSz) do
      if lOutBuffer^[lPixel] < lLow then
         lOutBuffer^[lPixel] := 0;
  if lHigh < 255 then
    for lPixel := 1 to (lSliceSz) do
      if lOutBuffer^[lPixel] < lHigh then
         lOutBuffer^[lPixel] := 0;
end;
  for lPixel := 1 to lSliceSz do
	lQuadP^[lPixel]:= lHdr.LUT[lOutBuffer^[lPixel]];
  Freemem(lOutBuffer);
end;
procedure CreateOverlayRenderBehind(var lBGHdr,lHdr: TMRIcroHdr; var lX,lY,lZ,lInRenderSurface,lInRenderDepth: Integer; var lQuadP: RGBQuadp; Smooth2D: boolean);
var
	lSrc,lOutBuffer: Bytep;
        lLow,lHigh,lQ,
	lSurfaceDepth,lIntensity,lDepth,lPixel,lSliceSz,lRenderSurface,lRenderDepth: integer;
begin
  if gBGImg.RenderDepthBufferItems < 1 then exit;
  lSrc := lHdr.RenderBuffer;//lHdr.ScrnBuffer;
  lSliceSz := lX*lY;
  //lVolSz := lSliceSz * lZ;
  GetMem (lOutBuffer ,  lSliceSz);
  fillchar(lOutBuffer^,lSliceSz,0);
  //lRenderDepth := lInRenderDepth;
  //if (lRenderDepth < 1) or (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then
  //	lRenderDepth := 1;
  lRenderSurface := lInRenderSurface;
  if (lHdr.IMgBufferItems > 0) {2/2008} and (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then
	lRenderSurface := 1;
   for lPixel := 1 to lSliceSz do begin
      lSurfaceDepth := abs(gBGImg.RenderDepthBuffer^[lPixel]);
      if (lSurfaceDepth > 0) and (lSurfaceDepth <= lX) then begin //background surface at this voxel
         lIntensity := 0;
         lRenderDepth := (lSurfaceDepth-1)+lInRenderDepth;
         if lRenderDepth >= lX then
            lRenderDepth := lX-1;
         lDepth := ((lPixel-1)* lX)+1;
         lRenderDepth := lDepth + lRenderDepth;
         lDepth := lDepth + lSurfaceDepth-1;
         lQ := 0;
         while (lDepth < lRenderDepth)  do begin
               if (lSrc^[lDepth] > lRenderSurface) and (lSrc^[lDepth] > lIntensity) then
                  lIntensity := lSrc^[lDepth];
               //if gBGImg.RenderDepthBuffer^[lPixel] < 0 then
               if (gBGImg.RenderDepthBuffer^[lPixel] < 0) and (lQ > 3) then
                  lDepth := lRenderDepth; //only show surface for cutout
               inc(lDepth);
               inc(lQ);
         end;

            lOutBuffer^[lPixel]:= lIntensity;
  end; //for each pixel with a background image
end; //for each pixel
//renderform.caption := inttostr(lQMax);

  (*for lPixel := 1 to lSliceSz do begin
	if gBGImg.RenderDepthBuffer^[lPixel] <> 0 then begin //background surface at this voxel
		lDepth := 0;
		lIntensity := 0;
		lSliceOffset := (abs(gBGImg.RenderDepthBuffer^[lPixel])-1)*lSliceSz; //start with nearest slice
		while (lDepth < lRenderDepth) and (lSliceOffset < lVolSz) do begin
			if (lSrc^[lSliceOffset+lPixel] > lRenderSurface) and (lSrc^[lSliceOffset+lPixel] > lIntensity) then
				lIntensity := lSrc^[lSliceOffset+lPixel];
			inc(lSliceOffset,lSliceSz);
			inc(lDepth);
			if gBGImg.RenderDepthBuffer^[lPixel] < 0 then
				lDepth := lRenderDepth; //only show surface for cutout
		end;
		lOutBuffer^[lPixel]:= lIntensity;
	end; //background surface at this voxel
  end; *)

  if (Smooth2D) and (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then //do not smooth labels
	Smooth2DImage (lX,lY, lOutBuffer);

//Mar2007 start
if lHdr.LUTfromZero then begin
  MinMaxFilt(lHdr,lLow,lHigh);
  //fx(lLow,lHigh);
  if lLow > 0 then
    for lPixel := 1 to (lSliceSz) do
      if lOutBuffer^[lPixel] < lLow then
         lOutBuffer^[lPixel] := 0;
  if lHigh < 255 then
    for lPixel := 1 to (lSliceSz) do
      if lOutBuffer^[lPixel] < lHigh then
         lOutBuffer^[lPixel] := 0;
end;
//Mar2007 end
  for lPixel := 1 to lSliceSz do
	lQuadP^[lPixel]:= lHdr.LUT[lOutBuffer^[lPixel]];
  Freemem(lOutBuffer);
end;


Function AziElevMatrix  : TMatrix;
var
	lLRFlipMatrix: TMatrix;
begin
   //	  gRender.Azimuth := RenderForm.AzimuthEdit.value;
  //gRender.Elevation := RenderForm.ElevationEdit.value;
	result := ViewTransformMatrix(
		   coordSpherical,
		   ToRadians(gRender.Azimuth),
		   ToRadians(gRender.Elevation),
		   3{Distance.Value},6{ScreenWidthHeight.Value},6{ScreenWidthHeight.Value},{ScreenToCamera.Value}3);
	  {The ViewTransformMatrix is all that is needed for other objects defined
	  in world coordinates.}
	if {RenderForm.FlipLRcheck.checked} gRender.FlipLR then begin
           lLRFlipMatrix := Matrix3D (-1,0,0,0,      // 3D "graphics" matrix
						   0,1,0,0,
						   0,0,1,0,
						   0,0,0,0);
           result := MultiplyMatrices(lLRFlipMatrix,Result);
        end;
end;

procedure ShadeCutoutCrease (var lRenderBuffer: bytep);
var
lZ,lY,lX: single;
  lXin,lYin,lZIn,lXm,lYm,lZm,lPixel,
 lOutDim,lOutPivot,lXPivotIn,lYPivotIn,lZPivotIn,
 lXlo,lXhi,lYlo,lYhi,lZlo,lZhi,lYOffset: integer;
 lClose,lScale: single;
 lMatrix: TMatrix;
begin
  lOutDim := gBGImg.RenderDim;//MaxDim(lBackgroundImg.ScrnDim[1],lBackgroundImg.ScrnDim[2],lBackgroundImg.ScrnDim[3]);
  if gRender.Zoom > 0 then
     lOutPivot := (round(gBGImg.RenderDim/gRender.Zoom)+1) shr 1
  else
      lOutPivot :=(gBGImg.RenderDim+1) shr 1;  //11/2007b
  //lOutPivot := (lOutDim+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lXPivotIn := (gBGImg.ScrnDim[1]+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lYPivotIn := (gBGImg.ScrnDim[2]+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lZPivotIn := (gBGImg.ScrnDim[3]+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lMatrix :=  InvertMatrix3D(AziElevMatrix);
	 //next: dilate borders by 1 pixel - draw crease INSIDE cutout
	 lXlo := gRender.CutOut.Lo[1]-1;
	 lXhi := gRender.CutOut.Hi[1]+1;
	 lYlo := gRender.CutOut.Lo[2]-1;
	 lYhi := gRender.CutOut.Hi[2]+1;
	 lZlo := gRender.CutOut.Lo[3]-1;
	 lZhi := gRender.CutOut.Hi[3]+1;
lScale := 1/gRender.Zoom; //11/2007

  for lYin := 1 to lOutDim do begin
	lYOffset := ((gBGImg.RenderDim-lYin)*gBGImg.RenderDim);
	for lXin := 1 to lOutDim do begin
		lPixel := lXin+ lYOffset;
		if gBGImg.RenderDepthBuffer^[lPixel]<0 then begin
			lZin := abs(gBGImg.RenderDepthBuffer^[lPixel]);
                        lX := (lXin *lScale)-lOutPivot ;
                        lY := lOutPivot -(lYin * lScale);
                        lZ := (lZin * lScale)-lOutPivot;
			lXm := round( (lX*lMatrix.matrix[1,1])+(lY * lMatrix.matrix[2,1])+(lZ*lMatrix.matrix[3,1]));
			lYm := round( (lX*(lMatrix.matrix[1,2]))+(lY * lMatrix.matrix[2,2])+(lZ*lMatrix.matrix[3,2]));
			lZm := round( (lX*(lMatrix.matrix[1,3]))+(lY * lMatrix.matrix[2,3])+(lZ*lMatrix.matrix[3,3]));
			lXm := (lXm+lXPivotIn);
			lYm := (lYm+lYPivotIn);
			lZm := (lZm+lZPivotIn);
			if abs(lXlo-lXm) < abs(lXhi-lXm)  then
				lXm := abs(lXlo-lXm)
			else
				lXm := abs(lXhi-lXm);
			if abs(lYlo-lYm) < abs(lYhi-lYm) then
				lYm := abs(lYlo-lYm)
			else
				lYm := abs(lYhi-lYm);
			if abs(lZlo-lZm) < abs(lZhi-lZm) then
				lZm := abs(lZlo-lZm)
			else
				lZm := abs(lZhi-lZm);
			if (lXm < lYm) and (lZm < lYm) then
				lYm := lZm //Y is furthest, replace with Z
			else if lZm < lXm then  //X is furthest, replace with Z
				lXm := lZm;
			lClose := sqrt((lXm*lXm) + (lYm*lYm));
			if  lClose < 8 then begin
				lClose := 1-sqr(1-(lClose/8));
				lRenderBuffer^[lPixel] := round(lRenderBuffer^[lPixel]*(0.33+(0.67*lClose)));
			end;
		end;
	end; //for lYin
  end; //for lXin
end;

procedure LUTbiasX (var lOutLUT : TLUT);
{http://dept-info.labri.fr/~schlick/DOC/gem2.html
http://dept-info.labri.fr/~schlick/publi.html
Fast Alternatives to Perlin's Bias and Gain Functions
Christophe Schlick
Graphics Gems IV, p379-382, April 1994  }
var
	lIndex: integer;
	lA,lT,lBias: single;
	lLUT: TLUT;
begin
	if gRender.CutoutBias = 4 then exit;
	lA := (gRender.CutoutBias+1)/10;

	 for lIndex := 1 to 254 do begin
		 lT := lIndex/255;
		 //lBias := 255*(lt/((1/la-2)*(1-lt)+1)) ;
		 lBias := 255*(lt/((1/la-2)*(1-lt)+1)) ;
		 lLUT[lIndex] := lOutLUT[round(lBias)];
		 {lHdr.LUT[lIndex].rgbRed := round(lBias*lHdr.LUT[lIndex].rgbRed);
		 lHdr.LUT[lIndex].rgbGreen := round(lBias*lHdr.LUT[lIndex].rgbGreen);
		 lHdr.LUT[lIndex].rgbBlue := round(lBias*lHdr.LUT[lIndex].rgbBlue);}
		 //lHdr.LUT[lIndex].rgbReserved := kLUTalpha;
	 end;
	 for lIndex := 1 to 254 do
		lOutLUT[lIndex] := lLUT[lIndex];
end;

procedure LUTgainX (var lOutLUT : TLUT; lBiasIn,lGainIn: integer {0..99});
{http://dept-info.labri.fr/~schlick/DOC/gem2.html
http://dept-info.labri.fr/~schlick/publi.html
Fast Alternatives to Perlin's Bias and Gain Functions
Christophe Schlick Graphics Gems IV, p379-382, April 1994  }
var
	lIndex,lV: integer;
	lA,lG,lT,lGain: single;
	lLUT: TLUT;
begin
 if (lGainIn = 50) and (lBiasIn = 50){gRender.CutoutBias = 4} then exit;
	lA := (lBiasIn)/100;
        if lA = 0 then
           lA := 0.000001;
	lG := (lGainIn)/100;
        if lG = 0 then
           lG := 0.00001;
        if lG = 1 then
           lG := 0.99999;
	for lIndex := 1 to 254 do begin
		 lT := lIndex/255;
                 //apply bias
		 lT := (lt/((1/la-2)*(1-lt)+1)) ;
                 //next apply gain
                 if lT < 0.5 then
                      lGain := (lT/((1/lG-2)*(1-2*lT)+1))
                 else
                     lGain := (( (1/lG-2)*(1-2*lT)-lT ) / ( (1/lG-2)*(1-2*lT)-1 ) );
                 lGain := lGain / lT;
                 lV := round(255*lT*lGain);
                 if lV > 255 then
                    lV := 255;
                 if lV < 0 then
                    lV := 0;
		 //lBias := 255*(lt/((1/la-2)*(1-lt)+1)) ;
		 lLUT[lIndex] := lOutLUT[lV];
	 end;
	 for lIndex := 1 to 254 do
		lOutLUT[lIndex] := lLUT[lIndex];
end;

function SmoothShading (lX,lY: integer;  lRenderDepthBuffer: SmallintP): boolean;
var
   kRenderInfiniteDepth,lPrevLineStart,lNextLineStart,lLineStart,lScanLines,
   lGap,lDepthSum,lWeightSum,lFar,lClose,lCenter,lInc,lXmG: integer;
   lRenderDepthBufferS: SmallIntP;
procedure AddPt (lI,lW: integer; var lSumI,lSumW: integer);
begin
    if lI = kRenderInfiniteDepth then exit;
    lSumI := lSumI + (lW*lI); //add scaled value
    lSumW := lSumW + lW;//add weight
end;
//problem - smoothing gives embossed look!
begin //func Smoothshading
  kRenderInfiniteDepth := 0;
  result := false;
  if (gRender.Zoom < 1) or (lY < 5) or (lX < 5) or (gBGImg.RenderDepthBufferItems <> (lX * lY)) then
     exit;
  lFar := 2;
  lClose := 3;
  lCenter := 5;
  lGap := trunc((gRender.Zoom-0.001)/1)+1; //must be at least 1!
  lXmG := lX-lGap;
  Getmem(lRenderDepthBufferS,lX*lY*sizeof(smallint));
  for lInc := 1 to (lX*lY) do
      lRenderDepthBufferS^[lInc] := lRenderDepthBuffer^[lInc];

  for lScanlines := (1+lGap) to (lY - lGap) do begin //can not compute angle for 1st and last scanline
      lLineStart := (lScanLines-1)*lX; //inc from 0
      lPrevLineStart := lLineStart-(lX*lGap); //inc from 0
      lNextLineStart := lLineStart+(lX*lGap); //inc from 0
      for lInc := (1+lGap) to (lXmG) do begin
          lWeightSum := 0;
          lDepthSum := 0;
          AddPt (lRenderDepthBuffer^[lPrevLineStart+lInc-1],lFar,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lPrevLineStart+lInc],lClose,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lPrevLineStart+lInc+1],lFar,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lLineStart+lInc-1],lClose,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lLineStart+lInc],lCenter,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lLineStart+lInc+1],lClose,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lNextLineStart+lInc-1],lFar,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lNextLineStart+lInc],lClose,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lNextLineStart+lInc+1],lFar,lDepthSum,lWeightSum);
          if lWeightSum > 0 then
              lRenderDepthBufferS^[lLineStart+lInc] := round(lDepthSum/lWeightSum);
      end; //columns
  end; //for scanlines: rows
  for lInc := 1 to (lX*lY) do
      lRenderDepthBuffer^[lInc] :=  lRenderDepthBufferS^[lInc];
  freemem(lRenderDepthBufferS);
  result := true;
end; //function SmoothShading


function IlluminationShading (lX,lY,lPct: integer; lImgBuffer: bytep;  lRenderDepthBuffer: SmallintP): boolean;
var
   kRenderInfiniteDepth,lXm1,lPrevLineStart,lNextLineStart,lLineStart,lScanLines,
   lIntensity,lInc,lGrayMin,lGrayMax: integer;
   lShadeFrac,lImgFrac,
   lPhongMagic,lMagic,lYVal,lXVal,lNormalPlane,lXLight,lYLight,lZLight,lLightVectorNormalise: single;
   lShadeBuffer: bytep;
begin //func illumination shading

  result := false;
  if (lPct < 1) or (lY < 5) or (lX < 5) or (gBGImg.RenderDepthBufferItems <> (lX * lY)) then
     exit;
       lMagic := 1;
  lPhongMagic := 1;
   kRenderInfiniteDepth := 0;
  lXLight := 0;//RenderForm.XL.value / 100;//lXLight / lLightVectorNormalise;
  lYLight := -0.5;//Renderform.YL.value / 100;//lYLight / lLightVectorNormalise;
  lZLight := -1;//RenderForm.ZL.value / 100;//lZLight / lLightVectorNormalise;
  lLightVectorNormalise := sqrt(sqr(lXLight)+sqr(lYLight)+sqr(lZLight));
  lXLight := lXLight / lLightVectorNormalise;
  lYLight := lYLight / lLightVectorNormalise;
  lZLight := lZLight / lLightVectorNormalise;
  lGrayMin := 0{64};
  lGrayMax := 255 - lGrayMin;
  lXm1 := lX-1;
  Getmem(lShadeBuffer,lX*lY*sizeof(byte));
  fillchar(lShadeBuffer^,lX*lY,0);

  for lScanlines := 2 to (lY - 1) do begin //can not compute angle for 1st and last scanline
      lLineStart := (lScanLines-1)*lX; //inc from 0
      lPrevLineStart := lLineStart-lX; //inc from 0
      lNextLineStart := lLineStart+lX; //inc from 0
      for lInc := 2 to (lXm1) do begin
        if  lImgBuffer^[lLineStart+lInc] <> 0 then begin //only shade non-zero intensities
         if ( lRenderDepthBuffer^[lPrevLineStart+lInc-1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lPrevLineStart+lInc]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lPrevLineStart+lInc+1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lLineStart+lInc-1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lLineStart+lInc]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lLineStart+lInc+1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lNextLineStart+lInc-1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lNextLineStart+lInc]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lNextLineStart+lInc+1]<>kRenderInfiniteDepth) then begin
              lYVal := lRenderDepthBuffer^[lPrevLineStart+lInc-1]+lRenderDepthBuffer^[lPrevLineStart+lInc]+lRenderDepthBuffer^[lPrevLineStart+lInc+1]
              -lRenderDepthBuffer^[lNextLineStart+lInc-1]-lRenderDepthBuffer^[lNextLineStart+lInc]-lRenderDepthBuffer^[lNextLineStart+lInc+1];
              lXVal := lRenderDepthBuffer^[lPrevLineStart+lInc-1]+lRenderDepthBuffer^[lLineStart+lInc-1]+lRenderDepthBuffer^[lNextLineStart+lInc-1]
              -lRenderDepthBuffer^[lPrevLineStart+lInc+1]-lRenderDepthBuffer^[lLineStart+lInc+1]-lRenderDepthBuffer^[lNextLineStart+lInc+1];
              lNormalPlane := sqrt(sqr(lXVal)+sqr(lYVal)+sqr(lMagic));
              if lNormalPlane <> 0 then begin
                lNormalPlane := -((-lXLight*lXVal)-(lYLight*lYVal)+lMagic*lZLight)/lNormalPlane;
                if {lImageAndShade} false then begin
                    lNormalPlane := Power(lNormalPlane,lPhongMagic);
                        //lIntensity := gProjBuffer[lLineStart+lInc];
                        //lIntensity := lPropShadingPivot+round((lPctImage*(lIntensity-lPropShadingPivot))+(lPctShade*(lNormalPlane-0.5)) );
                        if lIntensity > 254 then lIntensity := 254;
                        lShadeBuffer^[lLineStart+lInc] := lIntensity;
                end else begin //shading only
                  //if lAbbaRandom then //abba
                  lNormalPlane := (lNormalPlane+1) / 2;
                  if lNormalPlane > 0 then begin
                        lNormalPlane := Power(lNormalPlane,lPhongMagic);
                        //if lAbbaRandom then //abba
                        //if lNormalPlane < 0.5 then lNormalPlane := 1-lNormalPlane; //backlighting
                        lShadeBuffer^[lLineStart+lInc] := lGrayMin{64}+ round(lNormalPlane*(lGrayMax));
                  end else
                      lShadeBuffer^[lLineStart+lInc] :=  lGrayMin;
                end; //Shading vs ImageAndShading
              end; //NormalPlane = 0
         end else begin //samples for each pixel
             if {lImageAndShade}false then
               lShadeBuffer^[lLineStart+lInc] := 0//lPropShadingPivot+round((lPctImage*(gProjBuffer[lLineStart+lInc]-lPropShadingPivot))+(lPctShade*(-0.5)) )//1362
             else
                lShadeBuffer^[lLineStart+lInc] := lGrayMin;//1363;'# 20{64};
         end;
        end; //only shade non-zero intensities
      end; //columns
  end; //for scanlines: rows
  if lPct > 99 then begin
      for lInc := 1 to (lX*lY) do
          lImgBuffer^[lInc] := lShadeBuffer^[lInc];

  end else begin //partial shade
      lImgFrac := (100-lPct)/100;
      lShadeFrac := lPct/100;
      for lInc := 1 to (lX*lY) do
          lImgBuffer^[lInc] := round((lImgBuffer^[lInc]* lImgFrac) + (lShadeBuffer^[lInc]*lShadeFrac ));
  end;
  freemem(lShadeBuffer);
  result := true;
end; //function illuminationshading

procedure LUTLoad( lLUTindex: integer; var lLUT: TLUT);
var
   lHdr: TMRIcroHdr;
   lStr: string;
   lInc: integer;
begin
	 //gMRIcroOverlay[lLayer].LUTindex := LUTdrop.ItemIndex;
	 if lLUTindex < knAutoLUT then begin
		LoadMonochromeLUT(lLUTindex,gBGImg,lHdr);
	 end else begin //if B&W lut
	     lStr := gColorSchemeDir+pathdelim+ImgForm.LUTdrop.Items.Strings[lLUTindex]+'.lut';
	     if not FileExistsEX(lStr) then
		showmessage('Can not find '+lStr);
             LoadColorScheme(lStr, lHdr);
         end;
         for lInc := 0 to 255 do
		 lLUT[lInc] :=  lHdr.LUT[lInc];
end;


procedure CreateRender(var lBGHdr, lHdr: TMRIcroHdr; var lX,lY,lZ,lInRenderSurface,lInRenderDpeth: Integer; var lQuadP: RGBQuadp; Smooth2D, NormalizeIntensity,lCreateDepthBuffer: boolean;lUseDepthBuffer: integer);
var
   lLUT : array [0..255] of byte;
   lrgbLUT: TLUT;// array[0..255] of TRGBQuad;
   //lTime: DWord;
   lSrc,lOutBuffer: Bytep;
   lShade,lShadePrecise: boolean;
   lPreciseDepthBuffer: Smallintp;
   lMaxInten,lDepth,lPixel,lSamples,lSliceOffset,lIntensity,lSliceSz,lSliceEnd,lSliceStart,
   lVolSz,lRenderDepth,lRenderSurface,lTemp,lNear,lSubPixel,lClip: integer;
begin

  lShade := false;
  lShadePrecise := false;
  if {(gRender.BGNearClip<>0) or} (gRender.ShowCutout) then
     lMaxInten := 254
  else
      lMaxInten := 257;
  lRenderDepth := lInRenderDpeth;
  if (lHdr.IMgBufferItems > 0) {2/2008} and  (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then
	lRenderDepth := 1;
  lRenderSurface := lInRenderSurface;
  //if not lCreateDepthBuffer then

  if (lHdr.IMgBufferItems > 0) {2/2008} and (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then
	lRenderSurface := 0
  else begin
            //make sure at least some voxels are below air-surface threshold
	if (lHdr.WindowScaledMin <= (Raw2ScaledIntensity(lHdr,lHdr.GlMinUnscaledS) )) and (lHdr.WindowScaledMax <> 0 ) then begin
               lTemp := round( (Raw2ScaledIntensity(lHdr,lHdr.GlMinUnscaledS)-lHdr.WindowScaledMin)/(lHdr.WindowScaledMax)*255);
		//showmessage(inttostr(lTemp));
		if lTemp >= lRenderSurface then
			lRenderSurface := lTemp + 1;
	end;
  end;
  if (lUseDepthBuffer=kBelow)  then begin
	CreateOverlayRenderBehind(lBGHdr,lHdr, lX,lY,lZ,lRenderSurface,lRenderDepth, lQuadP, Smooth2D);
	exit;
  end;

  if (lUseDepthBuffer=kInFront) then begin
	CreateOverlayRenderInfrontNear(lBGHdr,lHdr, lX,lY,lZ,lRenderSurface,lRenderDepth, lQuadP, Smooth2D);
	exit;
  end;
  lSrc := lHdr.RenderBuffer;
  lSliceSz := lX*lY;
  lVolSz := lSliceSz * lZ;
  GetMem (lOutBuffer ,  lX*lY);
  //gRender.ClipFrac := kMaxFrac div 2;
  lClip := round(gRender.ClipFrac/kMaxFrac * lX);
  if lClip >= lX then
     lClip := 0;
  if lCreateDepthBuffer then begin
     if (gRender.ShadePct > 0) then begin
        lShade := true;
        if lRenderDepth > 0 then begin//not MIP
           lShadePrecise := true;
           getmem(lPreciseDepthBuffer,lSliceSz * sizeof(smallint));
           fillchar(lPreciseDepthBuffer^,lSliceSz* sizeof(smallint),0);
        end;
     end;
     if gBGImg.RenderDepthBufferItems <> lSliceSz then begin
	  if gBGImg.RenderDepthBufferItems > 0 then
		Freemem(gBGImg.RenderDepthBuffer);
	  gBGImg.RenderDepthBufferItems := lSliceSz;
	  GetMem(gBGImg.RenderDepthBuffer,lSliceSz*sizeof(smallint));
     end;
     fillchar(gBGImg.RenderDepthBuffer^,lSliceSz* sizeof(smallint),0);
     //lTime := gettickcount;
     if lRenderDepth < 1 then begin//MIP
       for lPixel := 1 to lSliceSz do begin
         lIntensity := 0;
        lSliceStart := ((lPixel-1)* lX)+1;
        lSliceOffset := lSliceStart+lClip; //start with nearest slice
         lSliceEnd := lSliceStart + lX;
         while (lSliceOffset < lSliceEnd) do begin
               if  (lSrc^[lSliceOffset] < lMaxInten) and (lSrc^[lSliceOffset] > lIntensity) then begin
                   lIntensity := lSrc^[lSliceOffset];
                   gBGImg.RenderDepthBuffer^[lPixel] := lSliceOffset - lSliceStart;
               end;
               inc(lSliceOffset,1);
         end; //while traversing front to back
         lOutBuffer^[lPixel]:= lIntensity;
     end; //for each pixel
  end else begin //if MIP else use opacity filter...
      for lPixel := 1 to lSliceSz do begin
	lDepth := 0;
	lSamples := 0;
	lIntensity := 0;
        lSliceStart := ((lPixel-1)* lX)+1;
	lSliceOffset := lSliceStart+lClip; //start with nearest slice
        lSliceEnd :=  (lPixel* lX);
	while (lDepth < lRenderDepth) and (lSliceOffset < lSliceEnd) do begin
		if  (lSrc^[lSliceOffset] < lMaxInten) and ((lDepth > 0) or (lSrc^[lSliceOffset] > lRenderSurface)) then begin
			inc(lDepth);
			if (lSrc^[lSliceOffset] > lRenderSurface) then begin
				lIntensity := lIntensity+ lSrc^[lSliceOffset];
				inc(lSamples);
			end;
			if (lDepth = 1) then begin
				gBGImg.RenderDepthBuffer^[lPixel] := lSliceOffset - lSliceStart;

				if (gBGImg.RenderDepthBuffer^[lPixel]=lCLip ) or ((gBGImg.RenderDepthBuffer^[lPixel] > 1) and (lSrc^[lSliceOffset-1]>=lMaxInten)) then begin //cutout
					if lSrc^[lSliceOffset-1]=lMaxInten-1 then
						lIntensity := 0;
					lDepth := lRenderDepth;
					gBGImg.RenderDepthBuffer^[lPixel] := -gBGImg.RenderDepthBuffer^[lPixel];  //negative: this is a cutout
				end;
                                if lShade then begin
                                   if (gBGImg.RenderDepthBuffer^[lPixel] > 1) then begin //estimate surface depth with sub-pixel accuracy
                                      lNear := lSrc^[lSliceOffset-1];
                                      lSubPixel := lIntensity-lNear; //delta
                                      lSubPixel := round(((lRenderSurface-lNear)/lSubPixel)*10);
                                      if lNear >= lMaxInten then //cutout
                                         lSubPixel := 0;
                                   end else
                                       lSubpixel := 0;
                                   lPreciseDepthBuffer^[lPixel] :=  (gBGImg.RenderDepthBuffer^[lPixel] * 10)+lSubPixel;
                                end;
   			end;
		end;
		inc(lSliceOffset,1);

	end; //while no voxel found
	if lDepth > 0 then
		lIntensity := lIntensity div lSamples;
		//lIntensity := lIntensity div lDepth; //mean of nDepth voxels
	lOutBuffer^[lPixel]:= lIntensity;
      end; //for each pixel 1..sliceSz
  if (Smooth2D) and (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then //do not smooth labels
	Smooth2DImage (lX,lY, lOutBuffer); //only smooth volume renderings - not MIPS (they looked embossed)
 end; //if not MIP
end else begin //do not create depth buffer
      for lPixel := 1 to lSliceSz do begin
	lDepth := 0;
	lSamples := 0;
	lIntensity := 0;
	lSliceOffset := ((lPixel-1)* lX)+1+lClip; //start with nearest slice
        lSliceEnd :=  (lPixel* lX);
	while (lDepth < lRenderDepth) and (lSliceOffset < lSliceEnd) do begin
		if  (lSrc^[lSliceOffset] < lMaxInten) and ((lDepth > 0) or (lSrc^[lSliceOffset] > lRenderSurface)) then begin
			inc(lDepth);
			if (lSrc^[lSliceOffset] > lRenderSurface) then begin
				lIntensity := lIntensity+ lSrc^[lSliceOffset];
				inc(lSamples);
			end;
		end;
		inc(lSliceOffset,1);
	end; //while no voxel found
	if lDepth > 0 then
		lIntensity := lIntensity div lSamples;
		//lIntensity := lIntensity div lDepth; //mean of nDepth voxels
	lOutBuffer^[lPixel]:= lIntensity;
  end; //for each pixel
end; //volume render without depth buffer
     //RenderForm.Caption := inttostr(gettickcount - lTime)+'  '+inttostr(lRenderDepth);
  if (NormalizeIntensity) and (lRenderSurface < 254) then begin //do BEFORE shading!
	for lPixel := 0 to 255  do
		lLUT[lPixel] := 0;
	for lPixel := lRenderSurface to 255 do
		lLUT[lPixel] := round(255*(lPixel-lRenderSurface)/(255-lRenderSurface));
	for lPixel := 1 to lSliceSz do
	   lOutBuffer^[lPixel] := lLUT[lOutBuffer^[lPixel]];
  end;

  if (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) and (lShade) then begin
      if lShadePrecise then begin
         SmoothShading (lX,lY,lPreciseDepthBuffer);
         IlluminationShading(lX,lY,gRender.ShadePct,lOutBuffer,lPreciseDepthBuffer{gBGImg.RenderDepthBuffer} );
         freemem(lPreciseDepthBuffer);
      end else
         IlluminationShading(lX,lY,gRender.ShadePct,lOutBuffer,gBGImg.RenderDepthBuffer);

  end;//shading
  //if (lHdr.UsesCustomPaletteRandomRainbow) then
  //      createLutLabel (lHdr.LUT, abs(lHdr.WindowScaledMax-lHdr.WindowScaledMin)/100);

  for lPixel := 0 to 255 do
      lrgbLUT[lPixel] := lHdr.LUT[lPixel];
  (*for lPixel := 1 to 255 do begin
      lrgbLUT[lPixel].rgbRed:= 255;
      //lrgbLUT[lPixel].rgbGreen:= lPixel;
      //lrgbLUT[lPixel].rgbBlue:= lPixel;
  end;  *)
  if (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then
   RenderForm.caption := '-'
  else
      RenderForm.caption := 'i';
  if (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then
      LUTGainX(lrgbLUT,gRender.Bias,gRender.Gain ); //Mar2007

  for lPixel := 1 to lSliceSz do
	lQuadP^[lPixel]:= lrgbLUT[lOutBuffer^[lPixel]];
  if  ((lClip >0) or (gRender.ShowCutout)) and (lCreateDepthBuffer)  then begin //make cutout grayscale, shade edges
	if gRender.ShowCutout then
		ShadeCutoutCrease(lOutBuffer);
        LUTLoad(gRender.cutoutLUTindex,lrgblut);//11/2007
	{for lPixel := 0 to 255 do begin
		lrgbLUT[lPixel].rgbRed := lPixel;
		lrgbLUT[lPixel].rgbGreen := lPixel;
		lrgbLUT[lPixel].rgbBlue := lPixel;
		lrgbLUT[lPixel].rgbReserved := kLUTalpha;

	end;}//create grayscale LUT
	LUTBiasX(lrgbLUT);
	for lPixel := 1 to lSliceSz do
		if gBGImg.RenderDepthBuffer^[lPixel]<0 then //cutout
			lQuadP^[lPixel]:= lrgbLUT[lOutBuffer^[lPixel]];
  end; //if BGimg with Cutout
  Freemem(lOutBuffer);
end;

function RenderDepth (lVal: integer): integer;//11/2007
begin
  if (lVal > 0) and (lVal < 16000) and (gBGImg.ScrnMM[1] > 0.1) and (gBGImg.ScrnMM[1] < 10) then begin
     result:= round (lVal / gBGImg.ScrnMM[1]);
     if result < 1 then
        result := 1;
  end else
      result := lVal;
result := round(result * gRender.Zoom);
end;

procedure DrawRender;
var
   lBGQuadP, lOverlayQuadP, l2ndOverlayQuadP: RGBQuadp;
   lUseBGSurface,lnOverlay,lOverlay, lX,lY,lZ,lSliceSz,lRenderSurface,lRenderDepth: longint;
   lBG0Clr,lOverlay0Clr: DWord;
   lSmooth : boolean;
begin
  lRenderSurface := gRender.BGSurface;
  //lRenderDepth:= gRender.BGDepth;
  lRenderDepth:= RenderDepth(gRender.BGDepth);//11/2007
  lSmooth := gRender.SmoothBG;
  lUseBGSurface := gRender.OverlayFromBGSurface ;
  lX := gMRIcroOverlay[kBGOverlayNum].RenderDim;
  lY := lX;
  lZ := lX;
  lSliceSz := (lX * lY);
  if (gMRIcroOverlay[kBGOverlayNum].RenderBufferItems=0)or (lX < 2) or (lY < 2) or (lZ < 2) or ((lX*lY*lZ) > gMRIcroOverlay[kBGOverlayNum].RenderBufferItems{ScrnBufferItems}) then
	 exit;
  GetMem ( lBGQuadP,  lSliceSz*4);
  CreateRender(gMRIcroOverlay[kBGOverlayNum],gMRIcroOverlay[kBGOverlayNum], lX,lY,lZ,lRenderSurface,lRenderDepth, lBGQuadP, lSmooth, true,true,0);
//next: overlays
  lSmooth := gRender.SmoothOverlay;
  lRenderSurface := gRender.OverlaySurface;
  //lRenderDepth:=  gRender.OverlayDepth;
  lRenderDepth:=  RenderDepth(gRender.OverlayDepth);//11/2007
lnOverlay := 0;
lBG0Clr:= TRGBQuad2DWord(gMRIcroOverlay[0].LUTinvisible);//just to avoid compiler warning hint - never used...
for lOverlay := knMaxOverlay downto 1 do begin
  if gMRIcroOverlay[lOverlay].RenderBufferItems{ScrnBufferItems} > 0 then begin
        if lOverlay = kVOIOverlayNum then //Aug2007
           lRenderSurface := 0
        else
            lRenderSurface := gRender.OverlaySurface;//
	inc(lnOverlay);
	if lnOverlay = 1 then begin //top overlay
		GetMem ( lOverlayQuadP ,  lSliceSz*4);
		lBG0Clr:= TRGBQuad2DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		CreateRender(gMRIcroOverlay[kBGOverlayNum],gMRIcroOverlay[lOverlay],lX,lY,lZ,lRenderSurface,lRenderDepth,lOverlayQuadP,lSmooth,false,false,lUseBGSurface);
	end else begin //2nd or lower overlay
		if lnOverlay = 2 then  //2nd overlay
			GetMem ( l2ndOverlayQuadP ,  lSliceSz*4);
		CreateRender(gMRIcroOverlay[kBGOverlayNum],gMRIcroOverlay[lOverlay], lX,lY,lZ,lRenderSurface,lRenderDepth,l2ndOverlayQuadP,lSmooth,false,false,lUseBGSurface);
		lOverlay0Clr:= TRGBQuad2DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		AlphaBlend32(lOverlayQuadP,l2ndOverlayQuadP, lBG0Clr,lOverlay0Clr, lSliceSz,gBGImg.OverlayTransPct);
	end; //2nd overlay or more
  end; //overlay loaded
end; //for knOverlay..1
//Finally: draw overlays on BG
if lnOverlay > 0 then begin
	lOverlay0Clr := lBG0Clr;
	//lBG0Clr := DWord(lHdr.LUTinvisible);
	lBG0Clr := 0;//0=impossible, no alpha DWord(lHdr.LUT[0]);
	if lnOverlay > 1 then
		FreeMem ( l2ndOverlayQuadP);
	AlphaBlend32(lBGQuadP,lOverlayQuadP, lBG0Clr,lOverlay0Clr, lSliceSz,gBGImg.BGTransPct);
	FreeMem ( lOverlayQuadP);
end;
//draw image
  SetDimension32(lY,lX,  lBGQuadP, gBGImg, RenderForm.RenderImage, RenderForm.RenderPanel);
  SetDimension32(lY,lX,  lBGQuadP, gBGImg, RenderForm.RenderImageBUP, RenderForm.RenderPanel);
  FreeMem ( lBGQuadP);
  if gBGImg.RenderDepthBufferItems > 0 then //negative depth was used for cutouts, now set to true depth
	for lX := 1 to gBGImg.RenderDepthBufferItems do
		gBGImg.RenderDepthBuffer^[lX] := abs(gBGImg.RenderDepthBuffer^[lX]);
end;

procedure SliceToFrac(var lBGImg: TBGImg);
var
   lInc: integer;
begin
            SortCutOut (gRender.CutOut);
	for lInc := 1 to 3 do begin
            if lBGImg.ScrnDim[lInc] < 1 then begin
               gRender.CutoutFrac.Lo[lInc] := round (0.5* kMaxFrac);
               gRender.CutoutFrac.Hi[lInc] := kMaxFrac;
            end else begin
                gRender.CutoutFrac.Lo[lInc] := round(kMaxFrac * gRender.Cutout.Lo[lInc]/lBGImg.ScrnDim[lInc]);
                gRender.CutoutFrac.Hi[lInc] := round(kMaxFrac * gRender.Cutout.Hi[lInc]/lBGImg.ScrnDim[lInc]);
            end;
	end;
end;       

procedure SetLimits(var lBGImg: TBGImg);
var lInc: integer;
lUpdateCutout: boolean;
lScale: single;
begin
        SortCutOut (gRender.CutOutFrac);
        if gRender.CutoutFrac.Lo[1] < 0 then
           SliceToFrac(lBGImg);
        lScale := 1/kMaxFrac;
        for lInc := 1 to 3 do begin
            gRender.Cutout.Lo[lInc] := round(gBGImg.ScrnDim[lInc] * lScale * gRender.CutoutFrac.Lo[lInc]);
            gRender.Cutout.Hi[lInc] := round(gBGImg.ScrnDim[lInc] * lScale * gRender.CutoutFrac.Hi[lInc]);
        end;
	lUpdateCutout := true;
	for lInc := 1 to 3 do
	  if gRender.Cutout.Lo[lInc] <> gRender.Cutout.Hi[lInc] then lUpdateCutout := false;
	if lUpdateCutout then
		for lInc := 1 to 3 do begin
			gRender.Cutout.Lo[lInc] := gBGImg.ScrnDim[lInc] div 2;
			gRender.Cutout.Hi[lInc] := gBGImg.ScrnDim[lInc];
		end;
	for lInc := 1 to 3 do begin
		if gRender.Cutout.Lo[lInc] < 1 then gRender.Cutout.Lo[lInc] := 1;
		if gRender.Cutout.Lo[lInc] > lBGImg.ScrnDim[lInc] then gRender.Cutout.Lo[lInc] := lBGImg.ScrnDim[lInc];
		if gRender.Cutout.Hi[lInc] < 1 then gRender.Cutout.Hi[lInc] := 1;
		if gRender.Cutout.Hi[lInc] > lBGImg.ScrnDim[lInc] then gRender.Cutout.Hi[lInc] := lBGImg.ScrnDim[lInc];
	end;
end;

procedure VolumeRotateMatrix (var lBGImg: TBGImg; var lHdr: TMRIcroHdr; var lMatrixIn: TMatrix; lBilinearSmooth,lRenderCutout,lIsBG: boolean {;lNearSlicesClipIn: integer});
label 345;
const
 kUgly2 = 10000;
 //kSh = 10; //bits to shift
 kUgly1 = (kUgly2 shl kSh) + (1 shl kSh);
var

 l:   TRotateVals;
 lZinc,lZ,lY,lX,lOutVolSz,
 lOutPos,lInVolSz,
 lYo,lZo,lnThreads: integer;
 lBuffIn,lSrcBuff,lBuffOut: Bytep;
 lXxp,lXyp,lXzp: Pointer;
 lStartTime: DWord;
 lM, lScale,lMatrix: TMatrix;
 lZoomRatio: Single;
 begin

 lMatrix := lMatrixIn;

 if (gRender.Zoom <> 0) and (gRender.Zoom <> 1 )then begin
      lZoomRatio := 1/gRender.Zoom;
      lScale := Matrix3D(lZoomRatio,0,0,0,   0,lZoomRatio,0,0,  0,0,lZoomRatio,0, 0,0,0,0);
      lMatrix := MultiplyMatrices(lMatrixIn,lScale);
  end else
      gRender.Zoom := 1;
  //lScale := Matrix3D(0,1,0,0,   1,0,0,0,  0,0,1,0, 0,0,0,0);
  //lScale := Matrix3D(0,1,0,0,   0,0,1,0,  1,0,0,0, 0,0,0,0);
  lScale := Matrix3D(0,1,0,0,   0,0,1,0,  1,0,0,0, 0,0,0,0);
  lMatrix := MultiplyMatrices(lMatrix,lScale);
  lStartTime := GetTickCount;
  l.XdimIn := lBGImg.ScrnDim[1];
  l.YdimIn := lBGImg.ScrnDim[2];
  l.ZdimIn := lBGImg.ScrnDim[3];;
  l.InSliceSz := l.XDimIn*l.YDimIn;
  lInVolSz := l.XdimIn*l.YdimIn*l.ZdimIn; //InVolSz!
  if  (lHdr.ScrnBufferItems < lInVolSz) then
	exit;
  lSrcBuff := lHdr.ScrnBuffer;
  l.OutDim := MaxDim(l.XDimIn,l.YDimIn,l.ZDimIn);
  l.OutDim := round(gRender.Zoom * l.OutDim); //11/2007
  (*lNearSlicesClip := lNearSlicesClipIn;//May07
  if lNearSlicesClip >= l.OutDim then //May07
     lNearSlicesClip := 0; //May07*)
  lBGImg.RenderDim := l.OutDim;
  lHdr.RenderDim := l.OutDim;
  //l.RenderCutout := false;
  if  (lRenderCutout) then begin
        //l.RenderCutout := true;

     SetLimits(lBGImg);
	 GetMem(lBuffIn, lInVolSz);
	 Move(lSrcBuff^,lBuffIn^,lInVolSz);
	 for lZ := 1 to lInVolSz do
		 if lBuffIn^[lZ] >= 254 then lBuffIn^[lZ] := 253;
   if	lRenderCutout then begin

        for lZ := gRender.Cutout.Lo[3] to gRender.Cutout.Hi[3] do begin
		 lZo := (lZ-1) * l.InSliceSz;
		 Application.ProcessMessages;
		 for lY := gRender.Cutout.Lo[2] to gRender.Cutout.Hi[2] do begin
			 lYo := (lY-1) * l.XdimIn;
			 for lX := gRender.Cutout.Lo[1] to gRender.Cutout.Hi[1] do
				 lBuffIn^[lX+lYo+lZo] := 255;
		 end; //for lY
	 end; //for lZ
   end;
  end else
   lBuffIn := lSrcBuff;
  l.OutPivot := (lHdr.RenderDim+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  l.XPivotIn := ((l.XdimIn+1) shr 1); //e.g. if DimMax=9, then pivot is 5
  l.YPivotIn := ((l.YdimIn+1) shr 1); //e.g. if DimMax=9, then pivot is 5
  l.ZPivotIn := ((l.ZdimIn+1) shr 1); //e.g. if DimMax=9, then pivot is 5
  l.YDimStart := -l.OutPivot+1; //e.g. if 9, start from -4
  l.ZDimStart := l.YDimStart ;

  l.YDimEnd := l.YDimStart+lHdr.RenderDim-1; //e.g. if 9, go to 4
  l.ZDimEnd := l.YDimEnd;
  if l.ZDimStart >= l.ZDimEnd then
	l.ZDImStart := l.ZDimStart;
  l.OutSliceSz :=  sqr(lHdr.RenderDim);
  lOutVolSz := lHdr.RenderDim*l.OutSliceSz;
  if lHdr.RenderBufferItems <> lOutVolSz then begin
	if lHdr.RenderBufferItems > 0 then
		Freemem(lHdr.RenderBuffer);
	lHdr.RenderBufferItems := lOutVolSz;
        try
	        GetMem(lHdr.RenderBuffer,lOutVolSz);
        except  //12/2007
           showmessage('Volume Rotate Error: System memory exhausted.');
           lHdr.RenderBufferItems := 0;
           exit;
        end;

  end;
  lBuffOut := lHdr.RenderBuffer;
  fillchar(lBuffOut^,lOutVolSz,0); //set all to zero

  //lMatrix :=  InvertMatrix3D(lMatrix);
  lZ := (sizeof(longint)* l.OutDim)+16;
  GetMem(lXxp, lZ);
  GetMem(lXyp, lZ);
  GetMem(lXzp, lZ);
//  if RenderForm.RenderRefreshTimer.enabled then goto 345;//abort
  {$IFNDEF FPC}
  l.XxRA := LongIntP($fffffff0 and (integer(lXxP)+15)); //data aligned to quad-word boundary
  l.XyRA := LongIntP($fffffff0 and (integer(lXyP)+15)); //quad-word boundary
  l.XzRA := LongIntP($fffffff0 and (integer(lXzP)+15)); //quad-word boundary}
  {$ELSE}
  l.XxRA := system.align(lXxP, 16); //data aligned to quad-word boundary
  l.XyRA := system.align(lXyP, 16); //quad-word boundary
  l.XzRA := system.align(lXzP, 16); //quad-word boundary
  {$ENDIF}
  for lX := 1 to  l.OutDim do begin
		   l.XxRA^[lX] := round((lX-l.OutPivot)*lMatrix.matrix[1,1]* (1 shl kSh)  )+kUgly1;
		   l.XyRA^[lX] := round((lX-l.OutPivot)*lMatrix.matrix[2,1]* (1 shl kSh)  )+kUgly1;
		   l.XzRA^[lX] := round((lX-l.OutPivot)*lMatrix.matrix[3,1]* (1 shl kSh)  )+kUgly1;
  end;
  l.XPivotInU2 := l.XPivotIn-kUgly2;
  l.YPivotInU2 := l.YPivotIn-kUgly2;
  l.ZPivotInU2 := l.ZPivotIn-kUgly2;

  {$IFNDEF NoThreads}
     lnThreads := gnCPUThreads;
   {$ELSE}
    lnThreads := 1;
   {$ENDIF}
   //if lIsBG then
   //TextForm.Memo1.Lines.Add( 'bg'+(inttostr(RenderForm.ThreadsRunning)+'  '+inttostr(lnThreads)))

   //else
   //TextForm.Memo1.Lines.Add( 'xx'+(inttostr(RenderForm.ThreadsRunning)+'  '+inttostr(lnThreads)));
     lZ := l.ZDimStart;
     lZo := l.ZDimEnd;
     lZinc := (l.ZDimEnd - l.ZDimStart) div lnThreads;
     l.ZDimEnd := l.ZDimStart + lZinc;
  //showmessage( inttostr(l.ZDimStart)+'..'+inttostr(l.ZDimEnd) +'  '+inttostr(lZo));
  if l.ZDimEnd > ImgForm.ProgressBar1.Min then begin //crashes if max < min, so write order important...
     ImgForm.ProgressBar1.Max := l.ZDimEnd+1;
     ImgForm.ProgressBar1.Min := l.ZDimStart;
  end else begin
     ImgForm.ProgressBar1.Min := l.ZDimStart;
     ImgForm.ProgressBar1.Max := l.ZDimEnd+1;

  end;
{$IFNDEF NoThreads}
        Application.processmessages;
     for lX := 1 to lnThreads do begin
         if lX = lnThreads then
            l.ZDimEnd := lZo; //avoid integer rounding error
         //TextForm.Memo1.Lines.Add('+'+inttostr(lX));
         if (lBilinearSmooth) and (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then
            TTriRender.Create(ImgForm.ProgressBar1,lX,l,lMatrix, lRenderCutout, lBuffIn,lBuffOut)
         else
           TNNRender.Create(ImgForm.ProgressBar1,lX,l,lMatrix, lRenderCutout, lBuffIn,lBuffOut);
         inc(ThreadsRunning);
         l.ZDimStart := l.ZDimEnd + 1;
         l.ZDimEnd := l.ZDimEnd + lZInc;

     end; //for each thread
     l.ZDimStart := lZ;

  repeat
        Application.processmessages;
  until ThreadsRunning = 0;
        Application.processmessages;
{$ELSE}//not threaded
            l.ZDimEnd := lZo; //avoid integer rounding error
         if (lBilinearSmooth) and (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then
            TriRotate(ImgForm.ProgressBar1,l,lMatrix, lRenderCutout, lBuffIn,lBuffOut)
         else
             NNRotate(ImgForm.ProgressBar1,l,lMatrix, lRenderCutout, lBuffIn,lBuffOut);
{$ENDIF}


	   FreeMem(lXxp);
	   FreeMem(lXyp);
	   FreeMem(lXzp);
  if (lRenderCutout)  then begin
	 FreeMem(lBuffIn);
  end;
  ImgForm.ProgressBar1.Position := l.ZDimStart;
  {$IFNDEF NoThreads}
    ImgForm.StatusLabel.caption :=('update(ms): '+inttostr(GetTickCount-lStartTime)+' threads: '+inttostr(lnThreads));
  {$ELSE}//not threaded
    ImgForm.StatusLabel.caption :=('update(ms): '+inttostr(GetTickCount-lStartTime));
  {$ENDIF}
end; //proceudre VolumeRotate;

end.
