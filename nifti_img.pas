unit nifti_img;
interface
uses
{$H+}

{$IFNDEF FPC}
RXSpin,capmenu,PNGImage,SSE,ShellAPI,Spin,
{$ENDIF}
{$IFNDEF Unix} Windows,
{$ELSE}
     {$IFDEF LINUX}
    // RGBGraphics,rgbroutines,
    {$ENDIF}
{$ENDIF}
{$DEFINE UINT16ASFLOAT}
nifti_types, bzip2stream,
SysUtils, Classes, Graphics, Controls, Forms, Dialogs, GraphType,
  Menus, ExtCtrls, NIFTI_hdr,nii_label,
Math,ClipBrd,define_types,
  GraphicsMathLibrary,Distr,Stat,ReadInt,gzio2;
const
  kMultiView = 0;
  kAxView0 = 1;
  kSagView0 = 2;
  kCoroView0 = 3;
  kAxViewOnly = -1;
  kSagViewOnly = -2;
  kCoroViewOnly = -3;
  kMaxLabel = 255;
  {$mode delphi}
Type

 TBGImg =  record //Next: analyze Format Header structure
   ScrnDim: array [1..3] of smallint;
   ScrnMM,ScrnOri: array [1..3] of single;
   XViewCenter,YViewCenter,ZViewCenter: single;
      SliceView,SPMDefaultsStatsFmriT,SPMDefaultsStatsFmriT0,
   MaxDim,LicenseID,XBarGap,XBarThick,VOIUndoSlice,VOIUndoOrient,VOIUndoVolItems,
   RenderDepthBufferItems,VOIInvZoom,ZoomPct,BGTransPct,OverlayTransPct, PlanarRGB,
   ImageSeparation,RenderDim,SigDig,LesionSmooth,LesionDilate,FontSize, SaveImgFilter, SaveVoiFilter: integer;
   //ResizeBeforeRescale - 0=intensity rescale, then resize;  1= nearest neighbor resize, then rescale;1=trilinear resize, then rescale;12:47 PM 7/13/2006
   UseReorientHdr,XBarVisible,ThinPen,Mirror,OverlaySmooth,VOIchanged,VOImirrored,
   SaveDefaultIni,KnownAlignment,Resliced, LoadUInt16asFloat32,
   FlipAx,FlipSag,SingleRow,ResliceOnLoad,Prompt4DVolume,OrthoReslice, ShowDraw, DarkMode: boolean;
   MinChar,MaxChar: array [1..3] of char; //May07
   StretchQuality : TStretchQuality;
   VOIClr,XBarClr: TColor;
   BackupLUT: TLUT;
   //LabelStr20 : Array[0..kMaxLabel] of kstr50;
      LabelRA: TStrRA;
   {FSLDIR,}FSLBASE,FSLOUTPUTTYPE{,FSLBETEXE}: kStr255;
      InvMat: TMatrix;
   ReorientHdr: TNIFTIHdr;
   //Cutout: TCutout;
   VOIUndoVol: bytep;
   RenderDepthBuffer: SmallIntp;
 end; //TNIFTIhdr Header Structure
 procedure CreateAnaRGB;
 function SlicesToImgPos(lX,lY,lZ: integer): integer;
 procedure  ImgPosToSlices(lPos: integer; var lX,lY,lZ: integer);
procedure DrawBMP( lx, ly: integer; var lBuff: RGBQuadp; var lImage: TImage);
procedure IntenBar (var lImage: TImage; var lHdr: TMRIcroHdr; lLTRB: integer {1=Left,2=Top,3=right,4=bottom}; lMin,lMax: single);
procedure Balance (var lHdr: TMRIcroHdr);
function ImgVaries ( var lHdr: TMRIcroHdr): boolean;
function OpenImg(var lBackgroundImg: TBGImg; var lImg2Load: TMRIcroHdr; lLoadBackground,lVOILoadAsBinary,lNoScaling8bit,lResliceIn,l4D: boolean): boolean;
procedure InitImgMemory(var lHdr: TMRIcroHdr);
procedure FreeImgMemory(var lHdr: TMRIcroHdr);
procedure SetDimension32(lInPGHt,lInPGWid:integer; lBuff: RGBQuadp; var lBackgroundImg: TBGImg; var lImage: TImage; lPanel: TScrollBox);
//procedure RescaleImgIntensity(var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr );
 procedure RescaleImgIntensity(var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr; lLayer: integer );
procedure LoadColorScheme(lStr: string; var lHdr: TMRIcroHdr);
procedure  LoadMonochromeLUT (var lLUT: integer; var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr); //lLUT: 0=gray,1=red,2=green,3=blue
procedure  FilterLUT (var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr; lMin, lMax: integer); //lLUT: 0=gray,1=red,2=green,3=blue
function Raw2ScaledIntensity (lHdr: TMRIcroHdr; lRaw: single): single;
function Scaled2RawIntensity (lHdr: TMRIcroHdr; lScaled: single): single;
procedure AlphaBlend32(lBGQuad,lOverlayQuad : RGBQuadp; lBG0Clr,lOverlay0Clr: DWord; lSlicePixels, lOverlayTransPct: integer);  // 630
procedure SetBGImgDefaults (var lBGImg: TBGImg);
function MaxDim (lX,lY,lZ: integer): integer; //returns largest of 3
procedure DrawHistogram (var lHdr: TMRIcroHdr; var lImage: TImage);
function MirrorImgBuffer(var lHdr: TMRIcroHdr ): boolean;
procedure MirrorScrnBuffer(var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr );
procedure SetSubmenuWithTag (var lRootMenu: TMenuItem; lTag: Integer);
procedure SaveAsVOIorNIFTIcore (var lFilename: string; var lImgBuffer: ByteP; lImgBufferItems, lImgBufferBPP,lnVol: integer; var lNiftiHdr: TNIFTIHdr);
procedure SaveAsVOIorNIFTI (var lImgBuffer: ByteP; lImgBufferItems, lImgBufferBPP,lnVol: integer; DefaultFormatVOI: boolean; var lNiftiHdr: TNIFTIHdr; lDefFilename: string);
function Scrn2ScaledIntensity (lHdr: TMRIcroHdr; lRaw: single): single;
procedure ScaleScrn2BMP (var lX, lY: integer;lImage: TImage);
procedure DrawXBar ( lHorPos, lVerPos: integer;var lImage: TImage);
function ImageZoomPct( var lImage: TImage): integer;
procedure ScaleBMP2Draw (var InvZoomShl10,lX, lY,lPanel: integer; lImage: TImage);
function ComputeInvZoomShl10(lSelectedImageNum: integer; var lImage: TImage): integer;
function ComputeZoomPct(lSelectedImageNum: integer; var lImage: TImage): integer;
function SelectedImageNum: Integer;
procedure EnsureVOIOpen;
procedure FreeUndoVol;
procedure CreateUndoVol;
procedure UndoVolVOI;
function IsVOIOpen: boolean;
//procedure SortCutout (var lCutout : TCutout); //ensure Lo < Hi
procedure SaveImgAsPNGBMP (lImage: TImage);
procedure RefreshImages;
procedure DrawAxial (lSlice,lMultiSlice: integer);
procedure DrawSag(lSlice,lMultiSlice: integer);
procedure DrawCor(lSlice,lMultiSlice: integer);
procedure DrawLabel(var lImage: TImage; lValue,lXCenterIn,lXWidthIn: integer);
procedure  ImgCoordToMM(var lX,lY,lZ: integer; var lXmm,lYmm,lZmm: single);
procedure  MMToImgCoord(out lX,lY,lZ: integer; var lXmm,lYmm,lZmm: single);
//function DimToMM (lIn, lDim: integer): integer;
function DimToMM (lX,lY,lZ, lDim: integer): integer;
function DimToMMx (lDim: integer): integer;
procedure  ImgPosToMM(lPos: integer; var lXmm,lYmm,lZmm: single);
procedure MakeStatHdr (var lBGHdr,lStatHdr: TMRIcroHdr; lMinIntensity,lMaxIntensity,lIntent_p1,lIntent_p2,lIntent_p3: single; lIntent_code: smallint;lIntentName: string);
function CenterOfMass (lOverlay: integer; var lX,lY,lZ: double): integer;
procedure TextReportHisto (var lHdr: TMRIcroHdr);
function TColor2TRGBQuad(lColor: TColor): TRGBQuad;
function TRGBQuad2DWord (lLUT: TRGBQuad): DWord;
procedure ReturnMinMax (var lHdr: TMRIcroHdr; var lMin,lMax: single; var lFiltMin8bit, lFiltMax8bit: integer);
function RawBGIntensity(lPos: integer): single;

//procedure FreeImgMemory(var lHdr: TMRIcroHdr);


const
gSelectedImageNum :integer= 1;
//gTripleZoom100: integer = 1;
//gImgSpacing: integer = 1;
implementation

uses nifti_img_view,MultiSlice,histoform,text, ortho_reorient, reslice_img;

function RawBGIntensity(lPos: integer): single;
var
	l16Buf : SmallIntP;
	l32Buf : SingleP;
begin
  result := 0;
  if (lPos > gMRIcroOverlay[kBGOverlayNum].ImgBufferItems) or (lPos < 1) then exit;
  if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 4) then begin
	l32Buf := SingleP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	result := l32Buf^[lPos];
  end else if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 2) then begin
	   l16Buf := SmallIntP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	result := l16Buf^[lPos];
  end else if gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 1 then
	 result := gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lPos]
  else begin
	showmessage('Unknown Background Buffer Bytes Per Pixel');
	exit;
  end;
end;

function TRGBQuad2DWord (lLUT: TRGBQuad): DWord;
var
  inguy : ^DWord;
begin
     inguy := @lLUT;
     result := inguy^;
end;

function TRGBQuad2TColor (lLUT: TRGBQuad): TColor;
begin
    result := (lLUT.rgbred)+(lLUT.rgbgreen shl 8)+(lLUT.rgbblue shl 16);
end;

function TColor2TRGBQuad(lColor: TColor): TRGBQuad;
begin
	result.rgbRed := (lColor and 255) ;
	result.rgbGreen := (lColor shr 8) and 255 ;// and 65280;
	result.rgbBlue := ((lColor shr 16) and 255) ;//and 16711680;
	result.rgbReserved := kLUTalpha;
end;

procedure InitImgMemory(var lHdr: TMRIcroHdr);
begin
	 with lHdr do begin
	  RenderBufferItems := 0;
	  ScrnBufferItems := 0;
	  ImgBufferItems := 0;
     end;
end;




function CenterOfMass (lOverlay: integer; var lX,lY,lZ: double): integer;
//result is volume in voxels - 0 = no volume or error
var
   lXpos,lYpos,lZpos,lInc: integer;

begin
     result := 0;
     lX := 0;
     lY := 0;
     lZ := 0;
     //fx((gMRIcroOverlay[lOverlay].NIFTIhdr.dim[1]*gMRIcroOverlay[lOverlay].NIFTIhdr.dim[2]* gMRIcroOverlay[lOverlay].NIFTIhdr.dim[3]),  gMRIcroOverlay[lOverlay].ScrnBufferItems);

     if (gMRIcroOverlay[lOverlay].NIFTIhdr.dim[1]*gMRIcroOverlay[lOverlay].NIFTIhdr.dim[2]* gMRIcroOverlay[lOverlay].NIFTIhdr.dim[3]) <>  gMRIcroOverlay[lOverlay].ScrnBufferItems then
        exit;
     //fx(999);
     lInc := 0;
     for lZpos := 1 to gMRIcroOverlay[lOverlay].NIFTIhdr.dim[3] do begin
         for lYpos := 1 to gMRIcroOverlay[lOverlay].NIFTIhdr.dim[2] do begin
             for lXpos := 1 to gMRIcroOverlay[lOverlay].NIFTIhdr.dim[1] do begin
                 inc(lInc);
                 if gMRIcroOverlay[lOverlay].ScrnBuffer^[lInc] > 0 then begin
                    inc(result);
                    lX := lX + lXpos;
                    lY := lY + lYpos;
                    lZ := lZ + lZpos;
                 end;
             end; //lX
         end;//Y
     end;//Z
     //fx(lX,lY,lZ);
     if result > 0 then begin
        lX := lX / result;
        lY := lY / result;
        lZ := lZ / result;
     end;
     //lARDistance := round(sqrt( sqr(lRX-lAX)+sqr(lRY-lAY)+sqr(lRZ-lAZ)));   //<- pythagorean theorem for dx
end;

procedure MakeStatHdr (var lBGHdr,lStatHdr: TMRIcroHdr; lMinIntensity,lMaxIntensity,lIntent_p1,lIntent_p2,lIntent_p3: single; lIntent_code: smallint;lIntentName: string);
//lIntent kNIFTI_INTENT_CHISQ  lIntent_p1 = DOF
//lIntent kNIFTI_INTENT_ZSCORE  no params
//lIntent kNIFTI_INTENT_TTEST lIntent_p1 = DOF
var lIntentNameLen,lPos: integer;
begin
	with lStatHdr do begin
		move(lBGHdr.niftiHdr,lStatHdr.niftiHdr,sizeof(TniftiHdr));
		ImgBufferBPP := 1;
		ImgBufferItems := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
		NIFTIhdr.scl_slope:= 1;
		NIFTIhdr.scl_inter:= 0;
		NIFTIhdr.glmin := round(lMinIntensity);
		NIFTIhdr.glmax := round(lMaxIntensity);
		AutoBalMinUnscaled := lMinIntensity;
		AutoBalMaxUnscaled := lMaxIntensity;
		WindowScaledMin := lMinIntensity;
		WindowScaledMax := lMaxIntensity;
		GlMinUnscaledS := lMinIntensity;
		GlMaxUnscaledS := lMaxIntensity;
		HdrFileName := extractfilepath(HdrFilename)+'stat.nii.gz';
		ImgFileName := HdrFileName;
		NIFTIhdr.intent_code := lIntent_Code;// kNIFTI_INTENT_ESTIMATE;
		NIFTIhdr.intent_p1 := lIntent_p1;
		NIFTIhdr.intent_p2 := lIntent_p2;
		NIFTIhdr.intent_p3 := lIntent_p3;
		lIntentNameLen := length(lIntentName);
		if lIntentNameLen > sizeof(NIFTIhdr.intent_name) then
			lIntentNameLen := sizeof(NIFTIhdr.intent_name);
		if lIntentNameLen > 0 then
			for lPos := 1 to lIntentNameLen do
				NIFTIhdr.intent_name[lPos] := lIntentName[lPos];
	end;
end;

procedure  MMToImgCoord(out lX,lY,lZ: integer; var lXmm,lYmm,lZmm: single);
var
  lXx,lYy,lZz: single;
begin
  if (not gBGImg.Resliced) and ( gMRIcroOverlay[kBGOverlayNum].NIfTItransform) then begin//vcx
    //mirror
    lxx := lXmm;
    lyy := lYmm;
    lzz := lZmm;
    mm2Voxel (lxx,lyy,lzz, gBGImg.InvMat);
    if gBGImg.Mirror then
      lXx := gBGImg.ScrnDim[1]-lXx;
    lX := round(lxx);
    ly := round(lyy);
    lz := round(lzz);
    exit;
  end;

  if gBGImg.Mirror then
    lX := round((gBGImg.ScrnDim[1]-gBGImg.ScrnOri[1]+1)-(lXmm/gBGImg.ScrnMM[1]))
  else
	  lX := round((lXmm/gBGImg.ScrnMM[1])+gBGImg.ScrnOri[1]);
	lY := round((lYmm/gBGImg.ScrnMM[2])+gBGImg.ScrnOri[2]);
	lZ := round((lZmm/gBGImg.ScrnMM[3])+gBGImg.ScrnOri[3]);
	if lX < 1 then lX := 1;
	if lY < 1 then lY := 1;
	if lZ < 1 then lZ := 1;
	if lX > gBGImg.ScrnDim[1] then lX := gBGImg.ScrnDim[1];
	if lY > gBGImg.ScrnDim[2] then lY := gBGImg.ScrnDim[2];
	if lZ > gBGImg.ScrnDim[3] then lZ := gBGImg.ScrnDim[3];
end;

(*2008
procedure  MMToImgCoord(var lX,lY,lZ: integer; var lXmm,lYmm,lZmm: single);
begin
	lX := round((lXmm/gBGImg.ScrnMM[1])+gBGImg.ScrnOri[1]);
	lY := round((lYmm/gBGImg.ScrnMM[2])+gBGImg.ScrnOri[2]);
	lZ := round((lZmm/gBGImg.ScrnMM[3])+gBGImg.ScrnOri[3]);
	if lX < 1 then lX := 1;
	if lY < 1 then lY := 1;
	if lZ < 1 then lZ := 1;
	if lX > gBGImg.ScrnDim[1] then lX := gBGImg.ScrnDim[1];
	if lY > gBGImg.ScrnDim[2] then lY := gBGImg.ScrnDim[2];
	if lZ > gBGImg.ScrnDim[3] then lZ := gBGImg.ScrnDim[3];
end;  *)

(*procedure  ImgCoordToMM(var lX,lY,lZ: integer; var lXmm,lYmm,lZmm: single);
begin
	lXmm := ((lX)-gBGImg.ScrnOri[1])*gBGImg.ScrnMM[1];
	lYmm := ((lY)-gBGImg.ScrnOri[2])*gBGImg.ScrnMM[2];
	lZmm := ((lZ)-gBGImg.ScrnOri[3])*gBGImg.ScrnMM[3];
end; *)

procedure  ImgCoordToMM(var lX,lY,lZ: integer; var lXmm,lYmm,lZmm: single);
begin
  if (not gBGImg.Resliced) and ( gMRIcroOverlay[kBGOverlayNum].NIfTItransform) then begin//vcx
    //mirror
    lXmm := lX;
    if gBGImg.Mirror then
      lXmm := gBGImg.ScrnDim[1]-lXmm;
    lYmm := lY;
    lZmm := lZ;
    Voxel2mm (lxmm,lymm,lzmm, gMRIcroOverlay[kBGOverlayNum].NIftiHdr);
    exit;
  end;
if gBGImg.Mirror then lXmm := ((gBGImg.ScrnDim[1]-lX+1)-gBGImg.ScrnOri[1])*gBGImg.ScrnMM[1] else

	lXmm := ((lX)-gBGImg.ScrnOri[1])*gBGImg.ScrnMM[1];
	lYmm := ((lY)-gBGImg.ScrnOri[2])*gBGImg.ScrnMM[2];
	lZmm := ((lZ)-gBGImg.ScrnOri[3])*gBGImg.ScrnMM[3];
end;

function XPos(lPos,XDim: integer): integer; //given 1D array return 3D column
begin
    result := lPos mod XDim;
    if result = 0 then
       result := XDim;
end;

function ZPos(lPos, XDimTimesYDim: integer): integer; //given 1D array return 3D slice
begin
    result := lPos div XDimTimesYDim;
    if (lPos mod XDimTimesYDim) <> 0 then
       inc(result);
end;

function YPos(lPos, XDim,YDim: integer): integer; //given 1D array return 3D row
var
   lSlicePos: integer;
begin
    //first - eliminate slice offset
    result := ZPos(lPos,XDim*YDim);
    lSlicePos := lPos - ((result-1)*(XDim*YDim));
    //now find row
    result :=lSlicePos div XDim;
    if (lSlicePos mod XDim) <> 0 then
       inc(result);
end;

(*function XPos(lPos,XDim: integer): integer; //given 1D array return 3D column
begin
    result := lPos mod XDim;
    if result = 0 then
       result := XDim;
end;

function ZPos(lPos, XDimTimesYDim: integer): integer; //given 1D array return 3D slice
begin
    result := lPos div XDimTimesYDim;
    if (lPos mod XDimTimesYDim) <> 0 then
       inc(result);
end;

function YPos(lPos, XDim,YDim: integer): integer; //given 1D array return 3D row
var
   lSlicePos: integer;
begin
    //first - eliminate slice offset
    result := ZPos(lPos,XDim*YDim);
    lSlicePos := lPos - ((result-1)*(XDim*YDim));
    //now find row
    result :=lSlicePos div XDim;
    if (lSlicePos mod XDim) <> 0 then
       inc(result);
end;  *)

function SlicesToImgPos(lX,lY,lZ: integer): integer;
begin
  result := lX + ((lY-1) * gBGImg.ScrnDim[1])+ ((lZ-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
end;
procedure  ImgPosToSlices(lPos: integer; var lX,lY,lZ: integer);
begin
     lX := XPos(lPos,gBGImg.ScrnDim[1]);
     lY := YPos(lPos,gBGImg.ScrnDim[1],gBGImg.ScrnDim[2]);
     lZ := ZPos(lPos,gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
end;


procedure  ImgPosToMM(lPos: integer; var lXmm,lYmm,lZmm: single);
var lX,lY,lZ: integer;
begin
     lX := XPos(lPos,gBGImg.ScrnDim[1]);
     lY := YPos(lPos,gBGImg.ScrnDim[1],gBGImg.ScrnDim[2]);
     lZ := ZPos(lPos,gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
     ImgCoordToMM(lX,lY,lZ, lXmm,lYmm,lZmm);
//xxx  lPos := lX + ((lY-1)*gBGImg.ScrnDim[1])+((lZ-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
end;

(*function DimToMM (lIn, lDim: integer): integer;
var
	lX,lY,lZ: integer;
	lXmm,lYmm,lZmm: single;
begin

	lX := lIn;
	lY := lIn;
	lZ := lIn;
      //if lDim = 2 then imgform.caption := inttostr(lY)+'-';
 ImgCoordToMM(lX,lY,lZ,lXmm,lYmm,lZmm);
	case lDim of
		3: result := round(lZmm);
		2: result := round(lYmm);
		else result := round(lXmm);
	end; //case
        //imgform.caption := floattostr(lYmm);
end; //DimToMM *)
function DimToMM (lX,lY,lZ, lDim: integer): integer;
//Sept2008 - X/Y/Z required for rotated images
var
  lXi,lYi,lZi: integer;
	lXmm,lYmm,lZmm: single;
begin
  lXi := lX;
  lYi := lY;
  lZi := lZ;
	ImgCoordToMM(lXi,lYi,lZi,lXmm,lYmm,lZmm);
  //imgform.Caption := floattostr(lxmm)+'  '+floattostr(lymm)+'  '+floattostr(lzmm)+'  666';
	case lDim of
		3: result := round(lZmm);
		2: result := round(lYmm);
		else result := round(lXmm);
	end //case
end; //DimToMM

function DimToMMx (lDim: integer): integer;
var
  lX,lY,lZ: integer;
begin
  lX := round(ImgForm.XViewEdit.value);
  lY := round(ImgForm.YViewEdit.value);
  lZ := round(ImgForm.ZViewEdit.value);
  result := DimToMM(lX,lY,lZ,lDim);
end; //DimToMM


procedure DrawTextLabel(var lImage: TImage; lOutStr: string; lXCenterIn,lXWidthIn: integer);
var
	lXWidth,lXCenter: integer;
begin
	lXWidth := lXWidthIn;
	lXCenter:= lXCenterIn;
	if lXWidth < 1 then begin
		lXWidth := lImage.Picture.Bitmap.Width;
	end;
	if gBGImg.XBarClr = TColor(gMRIcroOverlay[kBGOverlayNum].LUTinvisible) then
		lImage.canvas.font.Color := clBlack//clWhite;//gLUT[lClr].rgbRed+(gLUT[lClr].rgbGreen shl 8)+(gLUT[lClr].rgbBlue shl 16);
	 else
		lImage.canvas.font.Color := gBGImg.XBarClr;
	lImage.Canvas.Brush.Style := bsClear;
        {$IFDEF Darwin}
	lImage.Canvas.Font.Name := 'Helvetica';

        {$ELSE}
                            lImage.Canvas.Font.Name := 'Arial';
        {$ENDIF}
        lImage.Canvas.Font.Size := gBGImg.FontSize;
        (*if lXWidth < 100 then
		lImage.Canvas.Font.Size := 12
	else if lXWidth < 200 then
	   lImage.Canvas.Font.Size := 14
	else
		lImage.Canvas.Font.Size := 18;  *)
 //lImage.Canvas.Font.Size := 18;
 if lXCenterIn < 1 then
		lImage.canvas.TextOut(2,1,lOutStr)
	else if lXCenterIn = MaxInt then
		lImage.canvas.TextOut((lXWidth div 2)-(lImage.Canvas.TextWidth(lOutStr) div 2),1,lOutStr)
	else
		lImage.canvas.TextOut(lXCenter-(lImage.Canvas.TextWidth(lOutStr) div 2),1,lOutStr)
end;

procedure DrawLabel(var lImage: TImage; lValue,lXCenterIn,lXWidthIn: integer);
begin
	DrawTextLabel(lImage,inttostr(lValue),lXCenterIn,lXWidthIn);
end;

procedure DrawTextLabelV(var lImage: TImage; lOutStr: string);
var
	lYHt: integer;
begin
	lYHt := lImage.Picture.Bitmap.Height;
	if gBGImg.XBarClr = TColor(gMRIcroOverlay[kBGOverlayNum].LUTinvisible) then
		lImage.canvas.font.Color := clBlack//clWhite;//gLUT[lClr].rgbRed+(gLUT[lClr].rgbGreen shl 8)+(gLUT[lClr].rgbBlue shl 16);
	 else
		lImage.canvas.font.Color := gBGImg.XBarClr;
	lImage.Canvas.Brush.Style := bsClear;
	lImage.Canvas.Font.Name := 'Arial';
	lImage.canvas.TextOut(2,(lYHt div 2)-round(0.5*lImage.Canvas.TextHeight('X')),lOutStr)
end;

(*procedure DrawLabel(var lImage: TImage; lValue,lXCenterIn,lXWidthIn: integer);
var
	lOutStr: string;
	lXWidth,lXCenter: integer;
begin
	lXWidth := lXWidthIn;
	lXCenter:= lXCenterIn;
	if lXWidth < 1 then begin
		lXWidth := lImage.Picture.Bitmap.Width;
	end;
	if gBGImg.XBarClr = TColor(gMRIcroOverlay[kBGOverlayNum].LUTinvisible) then
		lImage.canvas.font.Color := clBlack//clWhite;//gLUT[lClr].rgbRed+(gLUT[lClr].rgbGreen shl 8)+(gLUT[lClr].rgbBlue shl 16);
	 else
		lImage.canvas.font.Color := gBGImg.XBarClr;
	lImage.Canvas.Brush.Style := bsClear;
	lImage.Canvas.Font.Name := 'Arial';
	if lXWidth < 100 then
		lImage.Canvas.Font.Size := 9
	else if lXWidth < 200 then
	   lImage.Canvas.Font.Size := 12
	else
		lImage.Canvas.Font.Size := 14;
	lOutStr := inttostr(lValue);
	if lXCenterIn < 1 then
		lImage.canvas.TextOut(2,1,lOutStr)
	else if lXCenterIn = MaxInt then
		lImage.canvas.TextOut((lXWidth div 2)-(lImage.Canvas.TextWidth(lOutStr) div 2),1,lOutStr)
	else
		lImage.canvas.TextOut(lXCenter-(lImage.Canvas.TextWidth(lOutStr) div 2),1,lOutStr)
end;*)


  {$IFNDEF FPC}
procedure PasteDimension32(lInPGHt,lInPGWid:integer; lBuff: RGBQuadp; var lImage: TImage; lXOffset: integer);
var
 sbBits : PByteArray;
 lPGWid,lPGHt,nBytesInImage: integer;
   lBMP: TBitmap;
   lSrcRect,lDestRect: TRect;
begin
  if lXOffset < 1 then begin
      showmessage('Error with paste dimension - XOffset is <1!');
      exit;
  end;
  lPGWid := lInPGWid;
	 lPGHt := lInPGHt;
	 lBMP := TBitmap.Create;
	 TRY
			 lBMP.PixelFormat := pf32bit;
			 lBMP.Width := lPGwid;
			 lBMP.Height := lPGHt;
			 sbBits := lBmp.ScanLine[lPGHt-1];
			 nBytesInImage := lPGWid*lPGHt * 4;
			 CopyMemory(Pointer(sbBits),Pointer(lBuff),nBytesInImage);
			 lImage.Canvas.CopyMode := cmSrcCopy;
			 lSrcRect := Rect(0,0,lBMP.Width,lBMP.Height);
			 lDestRect := Rect(lXOffset,0,lXOffset+lBMP.Width,lBMP.Height);
			 lImage.Canvas.CopyRect(lDestRect,lBMP.Canvas,lSrcRect);
	 FINALLY
			   lBMP.Free;
	 END; //try..finally
end; //proc PasteDimension32
  {$ELSE}
//PasteDimension32 FPC
procedure PasteDimension32(lInPGHt,lInPGWid:integer; lBuff: RGBQuadp; lXOffset: integer);
var
  lRowStart,x, y,lPos: Integer;
begin
    if lBuff = nil then exit;
    lPos := 0;
    for y:= (lInPGHt-1) downto 0  do begin
      lRowStart := (y * gMultiWid)+lXOffset;
      for x:=0 to lInPGWid-1 do begin
        //dec(lPos);
        inc(lPos);
        gMultiBuff^[lRowStart+x] := lBuff^[lPos];
      end;
    end;
end;
  {$ENDIF}

procedure CreateSag(var lHdr: TMRIcroHdr; lX,lXOffset,lY,lZ,lXYSliceSz: Integer; var lQuadP: RGBQuadp);
var
	lSrc: Bytep;
 //lLongBuff: LongIntp;
	lPixel,lYPos,lZPos,lZOffset,lYOffset: integer;
begin
  lSrc := lHdr.ScrnBuffer;
  lPixel := 0;
  //  lLongBuff := LongIntp(lQuadP);
  for lZPos := 1 to lZ do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
	  lYOffset := 0;
	  for lYPos := 1 to lY do begin
		  inc(lPixel);
    		  lQuadP^[lPixel] :=  lHdr.LUT[lSrc^[lZOffset+lYOffset+lXOffset]];
		  lYOffset := lYOffset+ lX;
	  end; //for each Y
  end; //for each Z
end; //CreateSag


procedure MirrorSlice (lY,lX: integer; lImage: RGBQuadp);
var
  lRowData: RGBQuadp;
  lXi,lYi,lHalfX,lRowBytes,lTop: integer;
begin
  if lX < 2 then exit;
  lRowBytes := lX * 4;
  getmem(lRowData,lRowBytes);
  lHalfX := lX div 2;
  lTop := 1;
  for lYi := 1 to lY do begin
    Move(lImage^[lTop],lRowData^[1],lRowBytes);
    for lXi := 1 to lX do
      lImage^[lTop+lXi-1] := lRowData^[lX - lXi + 1];
    lTop := lTop + lX;
  end;
  freemem(lRowData);
end;

procedure DrawSag (lSlice,lMultiSlice: integer);
var
   lBGQuadP, lOverlayQuadP, l2ndOverlayQuadP: RGBQuadp;
   lOverlay,lnOverlay,lXOffset, lX,lY,lZ,lXYSliceSz,lYZSliceSz: longint;
   lBG0Clr,lOverlay0Clr: DWord;
begin
  lX := round(gBGImg.ScrnDim[1]);
  lY := round(gBGImg.ScrnDim[2]);
  lZ := round(gBGImg.ScrnDim[3]);
  lXOffset := round(lSlice);
  lXYSliceSz := (lX*lY);
  lYZSliceSz := (lY*lZ);
  if (lXOffset > lX) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0)or (lXOffset < 1 {999+})  or (lXYSliceSz < 1) then
	 exit;
  if (lZ < 2)  then begin
	SetDimension32(1,1,  nil, gBGImg, ImgForm.PGImageSag, ImgForm.TriplePanel);
     exit;
  end;
  GetMem ( lBGQuadP ,  lYZSliceSz*4);
  CreateSag(gMRIcroOverlay[kBGOverlayNum], lX,lXOffset,lY,lZ,lXYSliceSz, lBGQuadP);
//next: overlays
lnOverlay := 0;
lBG0Clr:= TRGBQuad2DWord(gMRIcroOverlay[kBGOverlayNum].LUTinvisible);//just to avoid compiler warning hint - never used...
for lOverlay := knMaxOverlay downto 1 do begin
  if gMRIcroOverlay[lOverlay].ScrnBufferItems > 0 then begin
	inc(lnOverlay);
	if lnOverlay = 1 then begin //top overlay
		GetMem ( lOverlayQuadP ,  lYZSliceSz*4);
		lBG0Clr:= TRGBQuad2DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		CreateSag(gMRIcroOverlay[lOverlay], lX,lXOffset,lY,lZ,lXYSliceSz, lOverlayQuadP);
	end else begin //2nd or lower overlay
		if lnOverlay = 2 then  //2nd overlay
			GetMem ( l2ndOverlayQuadP ,  lYZSliceSz*4);
		CreateSag(gMRIcroOverlay[lOverlay], lX,lXOffset,lY,lZ,lXYSliceSz, l2ndOverlayQuadP);
		lOverlay0Clr:= TRGBQuad2DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		AlphaBlend32(lOverlayQuadP,l2ndOverlayQuadP, lBG0Clr,lOverlay0Clr, lYZSliceSz,gBGImg.OverlayTransPct);
	end; //2nd overlay or more
  end; //overlay loaded
end; //for knOverlay..1
//Finally: draw overlays on BG
if lnOverlay > 0 then begin
	lOverlay0Clr := lBG0Clr;
	lBG0Clr := 0;//0=impossible [no alpha] DWord(lHdr.LUTinvisible);
	if lnOverlay > 1 then
		FreeMem ( l2ndOverlayQuadP);
	AlphaBlend32(lBGQuadP,lOverlayQuadP, lBG0Clr,lOverlay0Clr, lYZSliceSz,gBGImg.BGTransPct);
	FreeMem ( lOverlayQuadP);
end;
//draw image
   if gBGImg.FlipSag then
    MirrorSlice (lZ,lY, lBGQuadP);
  if lMultiSlice >= 0 then
	PasteDimension32(lZ,lY,  lBGQuadP,lMultiSlice)//, MultiSliceForm.MultiImage,lMultiSlice)
  else begin
        //ImgForm.StatusLabel.Caption:=inttostr(lZ)+'x'+inttostr(lY)+' = '+inttostr(random(888));
        SetDimension32(lZ,lY,  lBGQuadP, gBGImg, ImgForm.PGImageSag, ImgForm.TriplePanel);
  	FreeMem ( lBGQuadP);
	if gBGImg.XBarVisible then begin

           if gBGImg.FlipSag then
              DrawXBar ( round(lY-gBGImg.YViewCenter), round(gBGImg.ZViewCenter),ImgForm.PGImageSag)
           else
    		DrawXBar ( round(gBGImg.YViewCenter), round({lZ-}gBGImg.ZViewCenter),ImgForm.PGImageSag);
		DrawLabel(ImgForm.PGImageSag, DimToMMx(1),-1,-1);
		if gBGImg.KnownAlignment then begin
			DrawTextLabel(ImgForm.PGImageSag,gBGImg.MaxChar[3]{'S'},MaxInt,-1);
       if gBGImg.FlipSag then
			  DrawTextLabelV(ImgForm.PGImageSag,gBGImg.MaxChar[2])
      else
			DrawTextLabelV(ImgForm.PGImageSag,gBGImg.MinChar[2]{'P'});
		end;
	end; //XBars
  end; //draw
end;

procedure CreateCor(var lHdr: TMRIcroHdr; lX,lYOffset,lZ,lXYSliceSz: Integer; var lQuadP: RGBQuadp);
var
	lSrc: Bytep;
	lPixel,lXPos,lZPos,lZOffset: integer;
begin
 lSrc := lHdr.ScrnBuffer;
 lPixel := 0;
 //fx(lYOffset);
 for lZPos := 1 to (lZ) do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
	  for lXPos := 1 to lX do begin
		  inc(lPixel);
                  lQuadP^[lPixel]:=lHdr.LUT[lSrc^[lZOffset+lYOffset+lXPos]];//+1 Mac???

	  end; //for each Y
  end; //for each Z
  {$IFDEF ENDIAN_BIG}
  lPixel := random(255);
  //fixes strange PPC compiler bug where lS value in DrawCor is corrupted
  //bug only seen in Lazarus IDE
  {$ENDIF}
end;

procedure DrawCor (lSlice,lMultiSlice: integer);
var
   lBGQuadP, lOverlayQuadP, l2ndOverlayQuadP: RGBQuadp;
   lOverlay,lnOverlay, lYOffset, lX,lY,lZ,lS,lXYSliceSz,lXZSliceSz: longint;
   lBG0Clr,lOverlay0Clr: DWord;
begin
  lX := round(gBGImg.ScrnDim[1]);
  lY := round(gBGImg.ScrnDim[2]);
  lZ := round(gBGImg.ScrnDim[3]);
  lS := round(lSlice);
  lXYSliceSz := (lX*lY);
  lXZSliceSz := (lX*lZ);
  lYOffset := (lX) * (lS-1);
  if (lS > lY) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0)or (lS < 1 {999+})  or (lXYSliceSz < 1) then
	 exit;
  if (lZ < 2)  then begin
	SetDimension32(1,1,  nil, gBGImg, ImgForm.PGImageSag, ImgForm.TriplePanel);
     //these do not work when image is stretched
     //ImgForm.PGImage3.Width := 1;
     //ImgForm.PGImage3.Height := 1;
     exit;
  end;

  GetMem ( lBGQuadP ,  (lXZSliceSz*4));
//imgform.caption := inttostr(lS)+'x666';
  CreateCor(gMRIcroOverlay[kBGOverlayNum], lX,lYOffset,lZ,lXYSliceSz, lBGQuadP);
//next: overlays
lnOverlay := 0;
//imgform.caption := inttostr(lS)+'x666';
lBG0Clr:= DWord(gMRIcroOverlay[1].LUTinvisible);//just to avoid compiler warning hint - never used...
for lOverlay := knMaxOverlay downto 1 do begin
  if gMRIcroOverlay[lOverlay].ScrnBufferItems > 0 then begin
	inc(lnOverlay);
	if lnOverlay = 1 then begin //top overlay
		GetMem ( lOverlayQuadP ,  lXZSliceSz*4);
		lBG0Clr:= DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		CreateCor(gMRIcroOverlay[lOverlay], lX,lYOffset,lZ,lXYSliceSz, lOverlayQuadP);
	end else begin //2nd or lower overlay
		if lnOverlay = 2 then  //2nd overlay
			GetMem ( l2ndOverlayQuadP ,  lXZSliceSz*4);
		CreateCor(gMRIcroOverlay[lOverlay], lX,lYOffset,lZ,lXYSliceSz, l2ndOverlayQuadP);
		lOverlay0Clr:= DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		AlphaBlend32(lOverlayQuadP,l2ndOverlayQuadP, lBG0Clr,lOverlay0Clr, lXZSliceSz,gBGImg.OverlayTransPct);
	end; //2nd overlay or more
  end; //overlay loaded
end; //for knOverlay..1
//Finally: draw overlays on BG
if lnOverlay > 0 then begin
	lOverlay0Clr := lBG0Clr;
	lBG0Clr := 0;//0=impossible, no alpha DWord(lHdr.LUTinvisible);
	if lnOverlay > 1 then
		FreeMem ( l2ndOverlayQuadP);
	AlphaBlend32(lBGQuadP,lOverlayQuadP, lBG0Clr,lOverlay0Clr, lXZSliceSz,gBGImg.BGTransPct);
	FreeMem ( lOverlayQuadP);
end;
//draw image
  if lMultiSlice >= 0 then
	PasteDimension32(lZ,lX,  lBGQuadP,lMultiSlice)// MultiSliceForm.MultiImage,lMultiSlice)
  else begin
	SetDimension32(lZ,lX,  lBGQuadP, gBGImg,ImgForm.PGImageCor, ImgForm.TriplePanel);
	if {ImgForm.XBarBtn.Down}gBGImg.XBarVisible then begin
		DrawXBar ( round(gBGImg.XViewCenter), round({lZ-}gBGImg.ZViewCenter),ImgForm.PGImageCor);
		DrawLabel(ImgForm.PGImageCor, DimToMMx(2),-1,-1);
		if gBGImg.KnownAlignment then begin
			DrawTextLabel(ImgForm.PGImageCor,gBGImg.MaxChar[3]{'S'},MaxInt,-1);
			if gBGImg.Mirror then
				DrawTextLabelV(ImgForm.PGImageCor,gBGImg.MaxChar[1]{'R'})
			else
				DrawTextLabelV(ImgForm.PGImageCor,gBGImg.MinChar[1]{'L'});
		end;

	end; //XBar
  end;
  FreeMem ( lBGQuadP);
end;

procedure CreateAxial(var lHdr: TMRIcroHdr; lStart,lSliceSz: Integer; var lQuadP: RGBQuadp);
var
	lSrc: Bytep;
	lPixel: integer;
begin
  lSrc := lHdr.ScrnBuffer;
  for lPixel := 1 to lSliceSz do
	  lQuadP^[lPixel]:=lHdr.LUT[lSrc^[lStart+lPixel]];
  //abba lQuadP^[200]:=lHdr.LUT[255];
end;

procedure FlipSlice (lY,lX: integer; lImage: RGBQuadp);
var
  lRowData: RGBQuadp;
  lYi,lHalfY,lRowBytes,lTop,lBottom: integer;
begin
  if lY < 2 then exit;
  lRowBytes := lX * 4;
  getmem(lRowData,lRowBytes);
  lHalfY := lY div 2;
  lTop := 1;
  lBottom := ((lY-1)*lX)+1;
  for lYi := 1 to lHalfY do begin
           Move(lImage^[lTop],lRowData^[1],lRowBytes);
           Move(lImage^[lBottom],lImage^[lTop],lRowBytes);
           Move(lRowData^[1],lImage^[lBottom],lRowBytes);
           lTop := lTop + lX;
           lBottom := lBottom - lX;
  end;
  freemem(lRowData);
end;


procedure DrawAxial (lSlice,lMultiSlice: integer);
var
   lBGQuadP, lOverlayQuadP, l2ndOverlayQuadP: RGBQuadp;
   lnOverlay,lOverlay, lX,lY,lS,lStart,lSliceSz: longint;
   lBG0Clr,lOverlay0Clr: DWord;
begin
  lX := round(gBGImg.ScrnDim[1]);
  lY := round(gBGImg.ScrnDim[2]);
  lS := round(lSlice{ImgForm.ZViewEdit.value});
  lSliceSz := (lX * lY{*lByte});
  lStart := lX*lY*(lS-1);
  if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0)or (lS < 0) or (lX < 2) or (lStart < 0) or (lSliceSz < 1) or ((lStart+lSliceSz-1) > gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems) then
	 exit;
  GetMem ( lBGQuadP,  lSliceSz*4);
  CreateAxial(gMRIcroOverlay[kBGOverlayNum], lStart,lSliceSz, lBGQuadP);
//next: overlays
lnOverlay := 0;
lBG0Clr:= DWord(gMRIcroOverlay[1].LUTinvisible);//just to avoid compiler warning hint - never used...
for lOverlay := knMaxOverlay downto 1 do begin
  if gMRIcroOverlay[lOverlay].ScrnBufferItems > 0 then begin
	inc(lnOverlay);
	if lnOverlay = 1 then begin //top overlay
		GetMem ( lOverlayQuadP ,  lSliceSz*4);
		lBG0Clr:= DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		CreateAxial(gMRIcroOverlay[lOverlay], lStart,lSliceSz,lOverlayQuadP);
	end else begin //2nd or lower overlay
		if lnOverlay = 2 then  //2nd overlay
			GetMem ( l2ndOverlayQuadP ,  lSliceSz*4);
		CreateAxial(gMRIcroOverlay[lOverlay], lStart,lSliceSz,l2ndOverlayQuadP);
		lOverlay0Clr:= DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		AlphaBlend32(lOverlayQuadP,l2ndOverlayQuadP, lBG0Clr,lOverlay0Clr, lSliceSz,gBGImg.OverlayTransPct);
	end; //2nd overlay or more
  end; //overlay loaded
end; //for knOverlay..1
//Finally: draw overlays on BG
if lnOverlay > 0 then begin
	lOverlay0Clr := lBG0Clr;
	lBG0Clr := 0;//0=impossible, no alpha DWord(lHdr.LUT[0]);
	if lnOverlay > 1 then
		FreeMem ( l2ndOverlayQuadP);
	AlphaBlend32(lBGQuadP,lOverlayQuadP, lBG0Clr,lOverlay0Clr, lSliceSz,gBGImg.BGTransPct);
	FreeMem ( lOverlayQuadP);
end;
//draw image
  if gBGImg.FlipAx then
    FlipSlice (lY,lX, lBGQuadP);
  if lMultiSlice >= 0 then
	PasteDimension32(lY,lX,  lBGQuadP, lMultislice)//MultiSliceForm.MultiImage,lMultiSlice)
  else begin
	SetDimension32(lY,lX,  lBGQuadP, gBGImg, ImgForm.PGImageAx, ImgForm.TriplePanel);
	if {ImgForm.XBarBtn.Down}gBGImg.XBarVisible then begin
    if gBGImg.FlipAx then
      lS := round(lY-gBGImg.YViewCenter)
    else
      lS := round(gBGImg.YViewCenter);
		DrawXBar ( round(gBGImg.XViewCenter), lS{round(gBGImg.YViewCenter)},ImgForm.PGImageAx);
		DrawLabel(ImgForm.PGImageAx, DimToMMx(3),-1,-1);
		if gBGImg.KnownAlignment then begin
			DrawTextLabel(ImgForm.PGImageAx,gBGImg.MaxChar[2]{'A'},MaxInt,-1);
			if gBGImg.Mirror then
				DrawTextLabelV(ImgForm.PGImageAx,gBGImg.MaxChar[1]{'R'})
			else
				DrawTextLabelV(ImgForm.PGImageAx,gBGImg.MinChar[1]{'L'});
		end;
	end; //XBar

  end;
  FreeMem ( lBGQuadP);
end; //DrawAxial

procedure DrawAxialCore (lSlice: integer; var lBGQuadP: RGBQuadp);
var
   lOverlayQuadP, l2ndOverlayQuadP: RGBQuadp;
   lnOverlay,lOverlay, lX,lY,lS,lStart,lSliceSz: longint;
   lBG0Clr,lOverlay0Clr: DWord;
begin
  lX := round(gBGImg.ScrnDim[1]);
  lY := round(gBGImg.ScrnDim[2]);
  lS := round(lSlice{ImgForm.ZViewEdit.value});
  lSliceSz := (lX * lY{*lByte});
  lStart := lX*lY*(lS-1);
  if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0)or (lS < 0) or (lX < 2) or (lStart < 0) or (lSliceSz < 1) or ((lStart+lSliceSz-1) > gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems) then
	 exit;
  CreateAxial(gMRIcroOverlay[kBGOverlayNum], lStart,lSliceSz, lBGQuadP);
  //next: overlays
  lnOverlay := 0;
  lBG0Clr:= DWord(gMRIcroOverlay[1].LUTinvisible);//just to avoid compiler warning hint - never used...
  for lOverlay := knMaxOverlay downto 1 do begin
      if gMRIcroOverlay[lOverlay].ScrnBufferItems > 0 then begin
	inc(lnOverlay);
	if lnOverlay = 1 then begin //top overlay
		GetMem ( lOverlayQuadP ,  lSliceSz*4);
		lBG0Clr:= DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		CreateAxial(gMRIcroOverlay[lOverlay], lStart,lSliceSz,lOverlayQuadP);
	end else begin //2nd or lower overlay
		if lnOverlay = 2 then  //2nd overlay
			GetMem ( l2ndOverlayQuadP ,  lSliceSz*4);
		CreateAxial(gMRIcroOverlay[lOverlay], lStart,lSliceSz,l2ndOverlayQuadP);
		lOverlay0Clr:= DWord(gMRIcroOverlay[lOverlay].LUTinvisible);
		AlphaBlend32(lOverlayQuadP,l2ndOverlayQuadP, lBG0Clr,lOverlay0Clr, lSliceSz,gBGImg.OverlayTransPct);
	end; //2nd overlay or more
      end; //overlay loaded
  end; //for knOverlay..1
  //Finally: draw overlays on BG
  if lnOverlay > 0 then begin
	lOverlay0Clr := lBG0Clr;
	lBG0Clr := 0;//0=impossible, no alpha DWord(lHdr.LUT[0]);
	if lnOverlay > 1 then
		FreeMem ( l2ndOverlayQuadP);
	AlphaBlend32(lBGQuadP,lOverlayQuadP, lBG0Clr,lOverlay0Clr, lSliceSz,gBGImg.BGTransPct);
	FreeMem ( lOverlayQuadP);
  end;
end; //DrawAxialCore

procedure SegmentRGBplanes (lSlice,lXVox,lYVox: integer; var lSliceQuadP: RGBQuadp; var lImg3: bytep; isPlanarRGB: boolean);
//analyze RGB saves data as red, green blue planes
var
   lLineOffset,lHalfX,lX,lY,lPos,lOutStart,lSliceVox: integer;
   lTempQuadP: TRGBQuad;

begin
     lSliceVox := lXVox*lYVox;
     if lSliceVox < 1 then exit;
     if (ImgForm.FlipLRmenu.checked) and (lXVox > 1) then begin
        lHalfX := lXVox div 2;
        lLineOffset := 0;
        for lY := 1 to lYVox do begin
            for lX := 1 to lHalfX do begin
              lTempQuadP := lSliceQuadP^[lX+lLineOffset];
              lSliceQuadP^[lX+lLineOffset] := lSliceQuadP^[1+lXVox-lX+lLineOffset];
              lSliceQuadP^[1+lXVox-lX+lLineOffset] := lTempQuadP;
            end; //for X
            lLineOffset := lLineOffset + lXVox;
        end;//lY

     end; //mirror
     if isPlanarRGB then begin
        //
       lOutStart := (lSlice-1)*lSliceVox*3;
       for lPos := 1 to lSliceVox do begin
         lImg3^[lPos+lOutStart] := lSliceQuadP^[lPos].rgbRed;
         lImg3^[lPos+lOutStart+lSliceVox] := lSliceQuadP^[lPos].rgbGreen;
         lImg3^[lPos+lOutStart+lSliceVox+lSliceVox] := lSliceQuadP^[lPos].rgbBlue;
       end;
     end else begin
       lOutStart := (lSlice-1)*lSliceVox*3;
       for lPos := 1 to lSliceVox do begin
              lOutStart := lOutStart + 1;
              lImg3^[lOutStart] := lSliceQuadP^[lPos].rgbRed;
              lOutStart := lOutStart + 1;
              lImg3^[lOutStart] := lSliceQuadP^[lPos].rgbGreen;
              lOutStart := lOutStart + 1;
              lImg3^[lOutStart] := lSliceQuadP^[lPos].rgbBlue;
       end;

     end;

end;

procedure CreateAnaRGB;
var
   lFilename: string;
   lImg3: bytep;
   lSliceQuadP: RGBQuadp;
   lVolVox,lX,lY,lZ,lI,lnSlice: integer;
   isPlanarRGB : boolean;
begin
     ImgForm.SaveDialog1.Filter := 'NIfTI compressed (.nii.gz)|*.nii.gz|NIfTI (.nii)|*.nii|NIfTI (.hdr/.img)|*.hdr|Volume of Interest(.voi)|*.voi|MRIcro (.roi)|*.roi';
     ImgForm.SaveDialog1.DefaultExt := '.hdr';
  ImgForm.SaveDialog1.Filename := ChangeFileExt(ImgForm.SaveDialog1.Filename, ImgForm.SaveDialog1.DefaultExt); //10102006
  if not ImgForm.SaveDialog1.Execute then exit;
  isPlanarRGB := false;
  case MessageDlg('Save as modern NIfTI style (RGBRGB..)? Press cancel for  Analyze style (RR..RGG..GBB..B)?', mtConfirmation,
  	[mbYes, mbCancel], 0) of
  	mrCancel: isPlanarRGB := true;
  end; //case

  lFilename := ImgForm.SaveDialog1.Filename;
     lX := round(gBGImg.ScrnDim[1]);
     lY := round(gBGImg.ScrnDim[2]);
     lZ := round(gBGImg.ScrnDim[3]);
     lVolVox := lX*lY*lZ ;
    if DiskFreeEx(lFilename) < (lVolVox*3) then begin
	case MessageDlg('Very little space on the selected drive. Attempt to save to this disk?', mtConfirmation,
		[mbYes, mbCancel], 0) of
		mrCancel: exit;
	end; //case
  end;

     getmem(lImg3, lVolVox* 3) ;
     //for Sag
     lnSlice := lZ;
     //fx(lX,lY,lZ);
     getmem(lSliceQuadP, lX*lY* sizeof(TRGBQuad)) ;
     for lI := 1 to lnSlice do begin       //[1+ ((lI-1)*lSliceBytes)]

         DrawAxialCore (lI,lSliceQuadP );
         //SegmentRGBplanes (lI,lX,lY,lSliceQuadP,lImg3, gGBImg.isPlanarRGB);
         SegmentRGBplanes (lI,lX,lY,lSliceQuadP,lImg3, isPlanarRGB);
     end;
     freemem(lSliceQuadP);
     //output data
    SaveAsVOIorNIFTIcore (lFilename, lImg3, lVolVox, 3,1, gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
     freemem(lImg3);
end;

procedure ComputeTripleZoom;
//computes axial, coronal and sagittal zoom
//values are SHL 10, so a 1% signal change will be 1024
//this preserves precision (though at the moment we round to nearest 1%)
label 543,641;
//const
// kSHval = 1 shl 10;
procedure SetPct(lAfrac,lCfrac,lSfrac: single);
begin
  ImgForm.PGImageAx.Tag := trunc(lAfrac*100);
  ImgForm.PGImageCor.Tag := trunc(lCfrac*100) ;
  ImgForm.PGImageSag.Tag := trunc(lSfrac*100) ;
end;
var
  lHpanel,lWpanel,lH,lW: integer;
  lPrimaryZoom,l2ndZoom,lZoomw,lZoomh: single;
begin
  SetPct(1,1,1);
  lHpanel := ImgForm.TriplePanel.ClientHeight-1;
  lWpanel := ImgForm.TriplePanel.ClientWidth-1;
   //gBGImg.ZoomPct := (ZoomDrop.ItemIndex-1)*100;
   if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then
    exit;
   if gBGImg.ZoomPct > 0 then begin
    SetPct(gBGImg.ZoomPct/100,gBGImg.ZoomPct/100,gBGImg.ZoomPct/100);
    lPrimaryZoom := ImgForm.PGImageAx.Tag/100;
    if abs(gBGImg.SliceView) <> kSagView0 then
      lW := gBGImg.ScrnDim[1] //Axial and Coronal width is X
    else
      lW := gBGImg.ScrnDim[2]; //Sagittal width is Y
    goto 543;
    exit;
   end;
   if (abs(gBGImg.SliceView) = kAxView0) or(abs(gBGImg.SliceView) = kCoroView0) or(abs(gBGImg.SliceView) = kSagView0) then begin //only show a single slice
    if abs(gBGImg.SliceView) <> kAxView0 then
      lH := gBGImg.ScrnDim[3] //Coronal and Sagitall height is Z
    else
      lH := gBGImg.ScrnDim[2]; //Axial height is Y

    if abs(gBGImg.SliceView) <> kSagView0 then
      lW := gBGImg.ScrnDim[1] //Axial and Coronal width is X
    else
      lW := gBGImg.ScrnDim[2]; //Sagittal width is Y
    lH := lH+1;
    lW := lW + 1;
   end else if gBGImg.SingleRow then begin //show 3 slices in row
    lW := gBGImg.ScrnDim[2]+gBGImg.ScrnDim[1]+gBGImg.ScrnDim[1];
    lWpanel := lWpanel-2- (2*gBGImg.ImageSeparation);
    if  gBGImg.ScrnDim[2]>gBGImg.ScrnDim[3] then
      lH := gBGImg.ScrnDim[2]+1
    else
      lH := gBGImg.ScrnDim[3]+1
   end else begin  //show three slices, 2 in top row, one in bottom
       //ImgForm.StatusLabel.Caption := inttostr( gBGImg.ImageSeparation);
    lW := gBGImg.ScrnDim[1]+gBGImg.ScrnDim[2]+4;
    lWpanel := lWpanel - 2 - gBGImg.ImageSeparation;
    lH := gBGImg.ScrnDim[3]+gBGImg.ScrnDim[2]+4;
    lHpanel := lHpanel - 2 - gBGImg.ImageSeparation;
   end;

   if (lW<1) or (lH < 1) or (lHpanel < 1) or (lWpanel < 1) then
    exit;
   lZoomw := lWpanel/ lW;
   lZoomh := lHpanel/ lH;
   if lZoomw < lZoomh then
    lPrimaryZoom := lZoomw
   else
    lPrimaryZoom := lZoomh;
   if (gBGImg.ZoomPct = 0) then begin//nearest integer
       lPrimaryZoom := trunc(lPrimaryZoom);
       if lPrimaryZoom < 1 then
        lPrimaryZoom := 1;
   end;
   //ImgForm.StatusLabel.Caption := floattostr( lPrimaryZoom);
   SetPct(lPrimaryZoom,lPrimaryZoom,lPrimaryZoom);
543: //for single slice views, set residual ...
   if gBGImg.SliceView = kMultiView then
    exit;//All orientations use primary zoom
   if gBGImg.SliceView < 0 then begin
    l2ndZoom := 0;
    goto 641;
   end;
   lWpanel := lWpanel-2- (2*gBGImg.ImageSeparation); //see if we can fit in two more images horizontally
   //note all images are currently set to primary zooom, so we will read PGImageAx
   lWpanel := lWPanel - round(lW*lPrimaryZoom);
   l2ndZoom := 0;
   if lWpanel < 3 then goto 641;
   if (abs(gBGImg.SliceView) = kAxView0) then
    lW := gBGImg.ScrnDim[1]+gBGImg.ScrnDim[2] //CorX + SagY
   else if (abs(gBGImg.SliceView) = kCoroView0) then
      lW := gBGImg.ScrnDim[1]+gBGImg.ScrnDim[2] //AxX + SagY
   else //(gBGImg.SliceView = kSagView)
      lW := gBGImg.ScrnDim[1]+gBGImg.ScrnDim[1];//AxX+CorX
   if lW < 1 then //avoid div0
    lZoomw := 0
   else
    lZoomw := lWpanel/ lW;
   if gBGImg.ScrnDim[2] > gBGImg.ScrnDim[3] then
    lH := gBGImg.ScrnDim[2]
   else
    lH := gBGImg.ScrnDim[3];
   if lH < 1 then //avoid div0
    lZoomh := 0
   else
    lZoomh := lHpanel/ lH;
   if lZoomw < lZoomh then
    l2ndZoom := lZoomw
   else
    l2ndZoom := lZoomh;
641:
   if (abs(gBGImg.SliceView) = kAxView0) then
    SetPct(lPrimaryZoom,l2ndZoom,l2ndZoom)
   else if (abs(gBGImg.SliceView) = kCoroView0) then
    SetPct(l2ndZoom,lPrimaryZoom,l2ndZoom)
   else //(gBGImg.SliceView = kSagView)
    SetPct(l2ndZoom,l2ndZoom,lPrimaryZoom);

end;

(*function ComputeTripleZoom : single;
var
  lHc,lWc,lH,lW: integer;
  lZw,lZh: single;
begin
   result := 1;
  lHc := ImgForm.TriplePanel.ClientHeight-1;
  lWc := ImgForm.TriplePanel.ClientWidth-1;
   //gBGImg.ZoomPct := (ZoomDrop.ItemIndex-1)*100;
   if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then
    exit;
   if gBGImg.ZoomPct > 0 then begin
    result := gBGImg.ZoomPct / 100;
    exit;
   end;
   if (gBGImg.SliceView = kAxView) or(gBGImg.SliceView = kCoroView) or(gBGImg.SliceView = kSagView) then begin //only show a single slice
    case gBGImg.SliceView of
      kSagView: lH := gBGImg.ScrnDim[3];
      kCoroView: lH := gBGImg.ScrnDim[3];
      else lH := gBGImg.ScrnDim[2];
    end;//case
    case gBGImg.SliceView of
      kSagView: lW := gBGImg.ScrnDim[2];
      kCoroView: lW := gBGImg.ScrnDim[1];
      else lW := gBGImg.ScrnDim[1];
    end;//case
    lH := lH+1;
    lW := lW + 1;

   end else if gBGImg.SingleRow then begin //show 3 slices in row
    lW := gBGImg.ScrnDim[2]+gBGImg.ScrnDim[1]+gBGImg.ScrnDim[1];
    lWc := lWc-2- (2*gBGImg.ImageSeparation);
    if  gBGImg.ScrnDim[2]>gBGImg.ScrnDim[3] then
      lH := gBGImg.ScrnDim[2]+1
    else
      lH := gBGImg.ScrnDim[3]+1
   end else begin  //show three slices, 2 in top row, one in bottom
    lW := gBGImg.ScrnDim[1]+gBGImg.ScrnDim[2]+4;
    lWc := lWc - 1 - gBGImg.ImageSeparation;
    lH := gBGImg.ScrnDim[3]+gBGImg.ScrnDim[2]+4;
    lHc := lHc - 1 - gBGImg.ImageSeparation;

   end;
   if (lW<1) or (lH < 1) or (lHc < 1) or (lWc < 1) then
    exit;
   lZw := lWc/ lW;
   lZh := lHc/ lH;
   if lZw < lZh then
    result := lZw
   else
    result := lZh;
   if (gBGImg.ZoomPct = 0) then begin//nearest integer
       result := trunc(result);
       if result < 1 then
        result := 1;
   end;
end;  *)

procedure ImageLT (lLScroll,lTScroll,lL,lT: integer; var lImage: TImage);
begin
  //if (lImage.Left = lL) and (lImage.Top = lT) then
  //  exit; ImgForm.Caption := 'a'+inttostr(lL)+'x'+inttostr(lT)+'debug'+inttostr(lImage.Left)+'x'+inttostr(lImage.Top);
  //if lImage.Left <> lL then
    lImage.Left := lL-lLScroll;
  //if lImage.Top <> lT then
    lImage.Top := lT-lTScroll;
end;

procedure RefreshImages;
var
  lL,lT: integer;
begin
  if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then begin
	  ImgForm.PGImageAx.Width := 0;
	  ImgForm.PGImageSag.Width := 0;
	  ImgForm.PGImageCor.Width := 0;
	  exit;
  end;
  {$IFDEF FPC}
  lL := 0;
  lT := 0;
  {$ELSE}
  lL := imgForm.Triplepanel.HorzScrollBar.Position;
  lT := imgForm.Triplepanel.VertScrollBar.Position;
  {$ENDIF}
    //imgform.Caption := inttostr(lL)+'x'+inttostr(lT);
  ComputeTripleZoom;
  ImgForm.PGImageAx.visible := ImgForm.PGImageAx.tag <> 0;
  ImgForm.PGImageCor.visible := ImgForm.PGImageCor.tag <> 0;
  ImgForm.PGImageSag.visible :=  ImgForm.PGImageSag.tag <> 0;
  if (gBGImg.SliceView = kMultiView) and (not gBGImg.SingleRow) then begin
      //Coronal is upper-left
      ImageLT(lL,lT,1,1,ImgForm.PGImageCor);
      //Axial is below Coronal
      ImageLT(lL,lT,1,round(gBGImg.ScrnDim[3]*ImgForm.PGImageCor.Tag/100)+gBGImg.ImageSeparation+1,ImgForm.PGImageAx);
      //Sag is to right of coronal
      ImageLT(lL,lT,round(gBGImg.ScrnDim[1]*ImgForm.PGImageCor.Tag/100)+gBGImg.ImageSeparation+1,1,ImgForm.PGImageSag);
   end else begin
      //Sag is left-most
      ImageLT(lL,lT,1,1,ImgForm.PGImageSag);
      //Next is coronal...
      ImageLT(lL,lT,round(gBGImg.ScrnDim[2]*ImgForm.PGImageSag.Tag/100)+gBGImg.ImageSeparation+1,1,ImgForm.PGImageCor);
      //Axial is rightmost
      ImageLT(lL,lT,round(gBGImg.ScrnDim[2]*ImgForm.PGImageSag.Tag/100)+round(gBGImg.ScrnDim[1]*ImgForm.PGImageCor.Tag/100)+gBGImg.ImageSeparation+gBGImg.ImageSeparation+1,1,ImgForm.PGImageAx);
   end;
(*         //Coronal is upper-left
      ImageLT(lL,lT,1,1,ImgForm.PGImageCor);
      //Axial is below Coronal
      ImageLT(lL,lT,1,round(gBGImg.ScrnDim[3]*ImgForm.PGImageCor.Tag/100)+gBGImg.ImageSeparation+1,ImgForm.PGImageAx);
      //Sag is to right of coronal
      ImageLT(lL,lT,round(gBGImg.ScrnDim[1]*ImgForm.PGImageCor.Tag/100)+gBGImg.ImageSeparation+1,1,ImgForm.PGImageCor);
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := true;
  end;

   if (gBGImg.SliceView = kAxView) or(gBGImg.SliceView = kCoroView) or(gBGImg.SliceView = kSagView) then begin //only show a single slice
    if (gBGImg.SliceView = kAxView) then begin
      ImageLT(lL,lT,1,1,ImgForm.PGImageAx);
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := false;
      ImgForm.PGImageSag.visible := false;
    end;
    if (gBGImg.SliceView = kCoroView) then begin
      ImageLT(lL,lT,1,1,ImgForm.PGImageCor);
      ImgForm.PGImageAx.visible := false;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := false;
    end;
    if (gBGImg.SliceView = kSagView) then begin
      ImageLT(lL,lT,1,1,ImgForm.PGImageSag);
      ImgForm.PGImageAx.visible := false;
      ImgForm.PGImageCor.visible := false;
      ImgForm.PGImageSag.visible := true;
    end;
  end else if gBGImg.SingleRow then begin
      //Sag is left-most
      ImageLT(lL,lT,1,1,ImgForm.PGImageSag);
      //Next is coronal...
      ImageLT(lL,lT,round(gBGImg.ScrnDim[2]*ImgForm.PGImageSag.Tag/100)+gBGImg.ImageSeparation+1,1,ImgForm.PGImageCor);
      //Axial is rightmost
      ImageLT(lL,lT,round(gBGImg.ScrnDim[2]*ImgForm.PGImageSag.Tag/100)+round(gBGImg.ScrnDim[1]*ImgForm.PGImageCor.Tag/100)+gBGImg.ImageSeparation+gBGImg.ImageSeparation+1,1,ImgForm.PGImageAx);
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := true;
  end else begin
      //Coronal is upper-left
      ImageLT(lL,lT,1,1,ImgForm.PGImageCor);
      //Axial is below Coronal
      ImageLT(lL,lT,1,round(gBGImg.ScrnDim[3]*ImgForm.PGImageCor.Tag/100)+gBGImg.ImageSeparation+1,ImgForm.PGImageAx);
      //Sag is to right of coronal
      ImageLT(lL,lT,round(gBGImg.ScrnDim[1]*ImgForm.PGImageCor.Tag/100)+gBGImg.ImageSeparation+1,1,ImgForm.PGImageCor);
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := true;
  end;                *)
  DrawAxial(round(gBGImg.ZViewCenter),-1);
  DrawSag (round(gBGImg.XViewCenter),-1);
  DrawCor (round(gBGImg.YViewCenter),-1);
end; //RefreshImages

(*procedure RefreshImages;
begin
  if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then begin
	  ImgForm.PGImageAx.Width := 0;
	  ImgForm.PGImageSag.Width := 0;
	  ImgForm.PGImageCor.Width := 0;
	  exit;
  end;
  gTripleZoom100 := trunc(100*ComputeTripleZoom);
  if gTripleZoom100 < 1 then
    gTripleZOom100 := 1;
   if (gBGImg.SliceView = kAxView) or(gBGImg.SliceView = kCoroView) or(gBGImg.SliceView = kSagView) then begin //only show a single slice
    if (gBGImg.SliceView = kAxView) then begin
      ImgForm.PGImageAx.Top := 1;
      ImgForm.PGImageAx.Left := 1;
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := false;
      ImgForm.PGImageSag.visible := false;
    end;
    if (gBGImg.SliceView = kCoroView) then begin
      ImgForm.PGImageCor.Top := 1;
      ImgForm.PGImageCor.Left := 1;
      ImgForm.PGImageAx.visible := false;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := false;
    end;
    if (gBGImg.SliceView = kSagView) then begin
      ImgForm.PGImageSag.Top := 1;
      ImgForm.PGImageSag.Left := 1;
      ImgForm.PGImageAx.visible := false;
      ImgForm.PGImageCor.visible := false;
      ImgForm.PGImageSag.visible := true;
    end;
  end else if gBGImg.SingleRow then begin
      ImgForm.PGImageCor.Left := round(gBGImg.ScrnDim[2]*gTripleZoom100/100)+gBGImg.ImageSeparation+1;
      ImgForm.PGImageCor.Top := 1;
      ImgForm.PGImageSag.Left := 1;
      ImgForm.PGImageSag.Top := 1;
      ImgForm.PGImageAx.Left := round(gBGImg.ScrnDim[1]*gTripleZoom100/100)+round(gBGImg.ScrnDim[2]*gTripleZoom100/100)+gBGImg.ImageSeparation+gBGImg.ImageSeparation+1;
      ImgForm.PGImageAx.Top :=  1;
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := true;
  end else begin
      ImgForm.PGImageCor.Left := 1;
      ImgForm.PGImageCor.Top := 1;
      ImgForm.PGImageSag.Left := round(gBGImg.ScrnDim[1]*gTripleZoom100/100)+gBGImg.ImageSeparation+1;
      ImgForm.PGImageSag.Top := 1;
      ImgForm.PGImageAx.Left := 1;
      ImgForm.PGImageAx.Top :=  round(gBGImg.ScrnDim[3]*gTripleZoom100/100)+gBGImg.ImageSeparation+1;
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := true;

  end;
  DrawAxial(round(gBGImg.ZViewCenter),-1);
  DrawSag (round(gBGImg.XViewCenter),-1);
  DrawCor (round(gBGImg.YViewCenter),-1);
end; //RefreshImages
*)
(*function ComputeTripleZoom : single;
var
  lHc,lWc,lH,lW: integer;
  lZw,lZh: single;
begin
   result := 1;
  lHc := ImgForm.TriplePanel.ClientHeight-1;
  lWc := ImgForm.TriplePanel.ClientWidth-1;
   //gBGImg.ZoomPct := (ZoomDrop.ItemIndex-1)*100;
   if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then
    exit;
   if gBGImg.ZoomPct > 0 then begin
    result := gBGImg.ZoomPct / 100;
    exit;
   end;
   if (gBGImg.SliceView = kAxView) or(gBGImg.SliceView = kCoroView) or(gBGImg.SliceView = kSagView) then begin //only show a single slice
    case gBGImg.SliceView of
      kSagView: lH := gBGImg.ScrnDim[3];
      kCoroView: lH := gBGImg.ScrnDim[3];
      else lH := gBGImg.ScrnDim[2];
    end;//case
    case gBGImg.SliceView of
      kSagView: lW := gBGImg.ScrnDim[2];
      kCoroView: lW := gBGImg.ScrnDim[1];
      else lW := gBGImg.ScrnDim[1];
    end;//case
    lH := lH+1;
    lW := lW + 1;

   end else if gBGImg.SingleRow then begin //show 3 slices in row
    lW := gBGImg.ScrnDim[2]+gBGImg.ScrnDim[1]+gBGImg.ScrnDim[1];
    lWc := lWc-4;
    if  gBGImg.ScrnDim[2]>gBGImg.ScrnDim[3] then
      lH := gBGImg.ScrnDim[2]+1
    else
      lH := gBGImg.ScrnDim[3]+1
   end else begin  //show three slices, 2 in top row, one in bottom
    lW := gBGImg.ScrnDim[1]+gBGImg.ScrnDim[2]+4;
    lWc := lWc - 2;
    lH := gBGImg.ScrnDim[3]+gBGImg.ScrnDim[2]+4;
    lHc := lHc - 2;

   end;
   if (lW<1) or (lH < 1) or (lHc < 1) or (lWc < 1) then
    exit;
   lZw := lWc/ lW;
   lZh := lHc/ lH;
   if lZw < lZh then
    result := lZw
   else
    result := lZh;
   if (gBGImg.ZoomPct = 0) then begin//nearest integer
       result := trunc(result);
       if result < 1 then
        result := 1;
   end;
end;

procedure RefreshImages;
//var
//  lZoom: single;
begin
  if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then begin
	  ImgForm.PGImageAx.Width := 0;
	  ImgForm.PGImageSag.Width := 0;
	  ImgForm.PGImageCor.Width := 0;
    //yui
	  exit;
  end;
  gTripleZoom100 := trunc(100*ComputeTripleZoom);
  if gTripleZoom100 < 1 then
    gTripleZOom100 := 1;
   if (gBGImg.SliceView = kAxView) or(gBGImg.SliceView = kCoroView) or(gBGImg.SliceView = kSagView) then begin //only show a single slice
    if (gBGImg.SliceView = kAxView) then begin
      ImgForm.PGImageAx.Top := 1;
      ImgForm.PGImageAx.Left := 1;
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := false;
      ImgForm.PGImageSag.visible := false;
    end;
    if (gBGImg.SliceView = kCoroView) then begin
      ImgForm.PGImageCor.Top := 1;
      ImgForm.PGImageCor.Left := 1;
      ImgForm.PGImageAx.visible := false;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := false;
    end;
    if (gBGImg.SliceView = kSagView) then begin
      ImgForm.PGImageSag.Top := 1;
      ImgForm.PGImageSag.Left := 1;
      ImgForm.PGImageAx.visible := false;
      ImgForm.PGImageCor.visible := false;
      ImgForm.PGImageSag.visible := true;
    end;
  end else if gBGImg.SingleRow then begin
      ImgForm.PGImageCor.Left := round(gBGImg.ScrnDim[2]*gTripleZoom100/100)+2;
      ImgForm.PGImageCor.Top := 1;
      ImgForm.PGImageSag.Left := 1;
      ImgForm.PGImageSag.Top := 1;
      ImgForm.PGImageAx.Left := round(gBGImg.ScrnDim[1]*gTripleZoom100/100)+round(gBGImg.ScrnDim[2]*gTripleZoom100/100)+3;
      ImgForm.PGImageAx.Top :=  1;
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := true;
  end else begin
      ImgForm.PGImageCor.Left := 1;
      ImgForm.PGImageCor.Top := 1;
      ImgForm.PGImageSag.Left := round(gBGImg.ScrnDim[1]*gTripleZoom100/100)+2;
      ImgForm.PGImageSag.Top := 1;
      ImgForm.PGImageAx.Left := 1;
      ImgForm.PGImageAx.Top :=  round(gBGImg.ScrnDim[3]*gTripleZoom100/100)+2;
      ImgForm.PGImageAx.visible := true;
      ImgForm.PGImageCor.visible := true;
      ImgForm.PGImageSag.visible := true;

  end;
  DrawAxial(round(gBGImg.ZViewCenter),-1);
  DrawSag (round(gBGImg.XViewCenter),-1);
  DrawCor (round(gBGImg.YViewCenter),-1);
end; //RefreshImages
  *)
  {$IFNDEF FPC}
function PNGFilterSize(lFilter: integer; lImage: TImage): integer;
var
   lStream: TMemoryStream;
    lPNGFilters : TEncodeFilterSet;
begin
	result := 0;
	if (lImage.Picture.Graphic = nil) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1) then begin
		Showmessage('You need to load an image before you can save it.');
		exit;
	end;
  lStream := TMemoryStream.Create;
  try
         with TPNGImage.Create do begin
           //gPNGSaveFilters := [];
           case lFilter of
                1: lPNGFilters := [efSub];
                2: lPNGFilters := [efUp];
                3: lPNGFilters := [efAverage];
                4: lPNGFilters := [efPaeth];//Include(SaveFilters, efPaeth);
                else lPNGFilters := [efNone];//[efNone,efSub,efUp,efAverage,efPaeth];
           end;
           Filter := lPNGFilters;
           //filters(efNone, efSub, efUp, efAverage, efPaeth);
           Assign(lImage.Picture.Graphic);
           SaveToStream(lStream);
           result := (lStream.Size);
         end;
  finally
         lStream.Free;
  end; //Stream TRY..FINALLY
end;

procedure SaveImgAsPNGBMP (lImage: TImage);
var
	lPNGFilter,lMinFilter,lMinFilterSz,lFilter,lSz: integer;
	 lPNGFilters : TEncodeFilterSet;
begin
	if (lImage.Picture.Graphic = nil) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1) then begin
		Showmessage('You need to load an image before you can save it.');
		exit;
	end;
	ImgForm.SaveDialog1.Filename := parsefilename(gMRIcroOverlay[kBGOverlayNum].HdrFilename);
	ImgForm.SaveDialog1.Filter := 'PNG bitmap|*.png';
	ImgForm.SaveDialog1.DefaultExt := '*.png';
	if not ImgForm.SaveDialog1.Execute then exit;
	lPNGFilter := 5;
		if lPNGFilter = 5 then begin //find PNG filter for smallest filesize
			 lMinFilter := 0;
			 lMinFilterSz := PNGFilterSize(0,lImage);
			 for lFilter := 1 to 4 do begin
				 Application.ProcessMessages;
				 lSz := PNGFilterSize(lFilter,lImage);
				 if lSz < lMinFilterSz then begin
					lMinFilter := lFilter;
					lMinFilterSz := lSz;
				 end;
			 end;  //Filter 1..4 try each filter
		end else
			lMinFilter := lPNGFilter;  //if look for smallest filter
		case lMinFilter of
				1: lPNGFilters := [efSub];
				2: lPNGFilters := [efUp];
				3: lPNGFilters := [efAverage];
				4: lPNGFilters := [efPaeth];//Include(SaveFilters, efPaeth);
				else lPNGFilters := [efNone];//[efNone,efSub,efUp,efAverage,efPaeth];
		end;
		with TPNGImage.Create do begin
		   //filters(efNone, efSub, efUp, efAverage, efPaeth);
		   Filter := lPNGFilters;
		   Assign(lImage.Picture.Bitmap);
		   SaveToFile(ChangeFileExt(ImgForm.SaveDialog1.FileName,'.png'));
		   free;
		end;
end;
  {$ELSE}
  procedure SaveImgAsPNGCore (lImage: TBitmap; lFilename: string);
  var
    PNG: TPortableNetworkGraphic;
    {$IFNDEF UNIX}
    BMP: TBitmap;
    {$ENDIF}
  begin
  	if (lImage = nil) then begin
  		Showmessage('No image found to save.');
  		exit;
  	end;
    PNG := TPortableNetworkGraphic.Create;
    try
      {$IFNDEF UNIX} //Windows has problems with pf32bit images, e.g. see multislice
      BMP := TBitmap.Create;
      Clipboard.Assign(lImage);
      BMP.Assign(Clipboard);
      PNG.Assign(BMP);    //Convert data into png
      BMP.Free;
      {$ELSE}
      PNG.Assign(lImage);    //Convert data into png
      {$ENDIF}
      PNG.SaveToFile(ChangeFileExt(lFilename,'.png'));
    finally
      PNG.Free;
    end
end;

procedure SaveImgAsPNGBMP (lImage: TImage);
begin
	if (lImage.Picture.Graphic = nil) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1) then begin
		Showmessage('You need to load an image before you can save it.');
		exit;
	end;
	ImgForm.SaveDialog1.Filename := parsefilename(gMRIcroOverlay[kBGOverlayNum].HdrFilename);
      {$IFDEF ENDIAN_BIG}
	ImgForm.SaveDialog1.Filter := 'PNG (*.png)|*.png;Bitmap|*.xpm';
	ImgForm.SaveDialog1.DefaultExt := '.png';
      {$ELSE}
	ImgForm.SaveDialog1.Filter := 'PNG (*.png)|*.png|Bitmap|*.bmp';
	ImgForm.SaveDialog1.DefaultExt := '.png';
      {$ENDIF}
	if not ImgForm.SaveDialog1.Execute then exit;
        //showmessage(ImgForm.SaveDialog1.FileName);
        if upcaseext(ImgForm.SaveDialog1.Filename)='.BMP' then
           lImage.Picture.Bitmap.SaveToFile(ImgForm.SaveDialog1.Filename)
        else
            SaveImgAsPNGCore(lImage.Picture.Bitmap,ImgForm.SaveDialog1.Filename);
end;
  {$ENDIF}

(*procedure SaveImgAsBMP (lImage: TImage);
begin
	if (lImage.Picture.Graphic = nil) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1) then begin
		Showmessage('You need to load an image before you can save it.');
		exit;
	end;
	 ImgForm.SaveDialog1.Filename := parsefilename(gMRIcroOverlay[kBGOverlayNum].HdrFilename);
	 ImgForm.SaveDialog1.Filter := 'Bitmap|*.bmp';
	 ImgForm.SaveDialog1.DefaultExt := '*.bmp';
	 if not ImgForm.SaveDialog1.Execute then exit;
	 lImage.Picture.Bitmap.SaveToFile(ImgForm.SaveDialog1.Filename);
end;*)


procedure UndoVolVOI;
var lTempBuf: ByteP;
begin
	if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems < 1 then exit;
	if gBGImg.VOIUndoVolItems <> gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems then exit;
	GetMem(lTempBuf,gBGImg.VOIUndoVolItems);
	Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,lTempBuf^,gBGImg.VOIUndoVolItems);
	Move(gBGImg.VOIUndoVol^,gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVolItems);
	Move(lTempBuf^,gBGImg.VOIUndoVol^,gBGImg.VOIUndoVolItems);
	FreeMem(lTempBuf);

end;

procedure FreeUndoVol;
begin
	if gBGImg.VOIUndoVolItems > 0 then
		freemem(gBGImg.VOIUndoVol);
	gBGImg.VOIUndoVolItems := 0;
	if gBGImg.RenderDepthBufferItems > 0 then
		freemem(gBGImg.RenderDepthBuffer);
	gBGImg.RenderDepthBufferItems := 0;
end;

procedure CreateUndoVol;
begin
	if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems < 1 then exit;
	gBGImg.VOIUndoSlice := 1;
	gBGImg.VOIUndoOrient := 4;
	if gBGImg.VOIUndoVolItems <> gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems then begin
		FreeUndoVol;
		gBGImg.VOIUndoVolItems := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
		getmem(gBGImg.VOIUndoVol,gBGImg.VOIUndoVolItems);
	end;
	Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVol^,gBGImg.VOIUndoVolItems);
end;

function IsVOIOpen: boolean;
begin
	result := false;
	if (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems = gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems)
		and (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0) then
			result := true;
end;

function isSameFloat (a,b: single): boolean;
const
     kEps = 0.0001;
begin
     result := abs(a-b) < kEps;
end;



function SameAsBG(var lBGImg: TBGImg;  var lHdr: TMRIcroHdr): boolean;
var
  lMatrixBG: TMatrix;
  i, j: Integer;
begin
  result := false;
  for i := 1 to 3 do //999
	if lHdr.NIFTIhdr.dim[i] <>lBGImg.ScrnDim[i] then //999
		exit; //999

lMatrixBG := Matrix3D ( lBGImg.Scrnmm[1],0,0,-lBGImg.Scrnmm[1]*(lBGImg.ScrnOri[1]-1),
            0,lBGImg.Scrnmm[2],0,-lBGImg.Scrnmm[2]*(lBGImg.ScrnOri[2]-1),
	    0,0,lBGImg.Scrnmm[3],-lBGImg.Scrnmm[3]*(lBGImg.ScrnOri[3]-1),
	    0,0,0,1);


  for i := 1 to 3 do
	for j := 1 to 4 do begin
		if not (isSameFloat(lMatrixBG.matrix[i,j], lHdr.Mat.matrix[i,j])) then begin
                       // showmessage(format('mat[%d,%d] %g - %g = %g ', [i,j, lMatrixBG.matrix[i,j], lHdr.Mat.matrix[i,j], lMatrixBG.matrix[i,j]-lHdr.Mat.matrix[i,j] ]) );
                       exit;

                end;
        end;
  //for i := 1 to 3 do if (lBGIMg.ScrnDim[i])<>lHdr.NIFTIhdr.dim[i] then exit;
  result := true;
end;

procedure EnsureVOIOpen;
var lMaxi: integer;
begin
	if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems = gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then exit;
	//showmessage(inttostr(gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems));
	if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0 then
		Freemem(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer);
        gMRIcroOverlay[kVOIOverlayNum].NIFTIhdr.dim[1] := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[1];
        gMRIcroOverlay[kVOIOverlayNum].NIFTIhdr.dim[2] := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[2];
        gMRIcroOverlay[kVOIOverlayNum].NIFTIhdr.dim[3] := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[3];
        gMRIcroOverlay[kVOIOverlayNum].NIFTIhdr.pixdim[1] := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.pixdim[1];
        gMRIcroOverlay[kVOIOverlayNum].NIFTIhdr.pixdim[2] := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.pixdim[2];
        gMRIcroOverlay[kVOIOverlayNum].NIFTIhdr.pixdim[3] := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.pixdim[3];
	gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;

	gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
	Getmem(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems);
	fillchar(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems,0);
	lMaxI := maxint;
	LoadMonochromeLUT(lMaxi,gBGImg,gMRIcroOverlay[kVOIOverlayNum]);
   if (gBGImg.Resliced) and (not SameAsBG(gBGImg,gMRIcroOverlay[kBGOverlayNum])) then //fv
    showmessage('Warning: you are about to draw a region of interest on an resliced image, which can problems with SPM and FSL. Solution: choose Help/Preferences and uncheck ''Reorient images when loading'', then reload your image.');
end;

function SelectedImageNum: Integer;
begin
 result := gSelectedImageNum;
 if (result < 1) or (result > 3) then
    result := 1;
{ with ImgForm do begin
  if TriplePanel.BorderStyle = bsSingle then
	 result := 3
  else if TriplePanel.BorderStyle = bsSingle then
	 result := 2
  else
	 result := 1
 end;} //with ImgForm
end;//SelectedImageNum


function ComputeInvZoomShl10(lSelectedImageNum: integer; var lImage: TImage): integer;
const
 kSHval = 1 shl 10;
var lPGWid,lImgWid: integer;
begin
	result := kSHval;//100%
	lPGWid := lImage.Picture.Bitmap.Width;
	if lImage.Tag > 0 then begin
		result := round((100/lImage.Tag)*kShVal);
		exit;
	end;
	if lSelectedImageNum = 2 then
		lImgWid := gBGImg.ScrnDim[2] //Sag - horizontal is Y
	else
		lImgWid := gBGImg.ScrnDim[1]; //cor and ax - horizontal is X
	If (lPGWid < 1) or (lImgWid < 1) then exit;
	result := round(lImgWid/lPGWid* kShVal);
end;

function ComputeZoomPct(lSelectedImageNum: integer; var lImage: TImage): integer;
var lPGWid,lImgWid: integer;
begin
	result := 100;//100%
	lPGWid := lImage.Picture.Bitmap.Width;
	if lImage.Tag > 0 then begin
		result := lImage.Tag;
		exit;
	end;
	if lSelectedImageNum = 2 then
		lImgWid := gBGImg.ScrnDim[2] //Sag - horizontal is Y
	else
		lImgWid := gBGImg.ScrnDim[1]; //cor and ax - horizontal is X
	If (lPGWid < 1) or (lImgWid < 1) then exit;
	result := round(lPGWid/lImgWid* 100);
end; //ComputeZoomPct

procedure ScaleScrn2BMP (var lX, lY: integer;lImage: TImage);
var lScale: single;
begin
	 if (lImage.Height = 0) or (lImage.Width = 0) then exit;
	 lScale := lImage.Picture.Bitmap.Height /lImage.Height;
	 lX := round(lX * lScale);
	 lY := round(lY * lScale);
end;

procedure ScaleBMP2Draw (var InvZoomShl10,lX, lY, lPanel: integer; lImage: TImage);
var
  pxHt, pxWid : integer;
begin
      {$IFDEF Darwin}
     //please check if next line required for this OS! 8/8/2014
  {$ENDIF}
  lX := lX + 1;
  lY := lY + 1;
  if lPanel = 2 then
    pxWid := gBGImg.ScrnDim[2]
  else
     pxWid := gBGImg.ScrnDim[1];
  if lPanel = 1 then
    pxHt := gBGImg.ScrnDim[2]
  else
     pxHt := gBGImg.ScrnDim[3];
  //imgForm.statusLabel.caption := inttostr(lPanel)+'  '+inttostr(lX)+' '+inttostr(ceil(lX * (pxWid/lImage.Width)));
  lX := ceil(lX * (pxWid/lImage.Width));
  lY := ceil(lY * (pxHt/lImage.Height));
  //if (gBGImg.FlipSag) and (lPanel = 2) then
  //   lX := (lImage.Width-lX) + 1;
  if (gBGImg.FlipAx) and (lPanel = 1) then
    lY := (lImage.Height-lY) + 1;
end;

(*procedure ScaleBMP2DrawOld (var InvZoomShl10,lX, lY, lPanel: integer; lImage: TImage);
var
  pxHt, pxWid : integer;
begin
	 //lScaleShl10 := ComputeInvZoomShl10(SelectedImageNum,lImage);
	 //ImgForm.StatusLabel.Caption := inttostr(InvZoomShl10);
  {$IFDEF Darwin}
     please check if next line required for this OS! 8/8/2014
  {$ENDIF}
  lX := lX + 1;
 //imgForm.statusLabel.caption := inttostr(lPanel)+'  '+inttostr(lX)+' '+inttostr(lY);
         if (gBGImg.FlipSag) and (lPanel = 2) then
		lX := ((lImage.Width-lX) * InvZoomShl10) shr 10
  	 else if (lX < 1) then
		lX := 0
	 else
		lX := (lX * InvZoomShl10) shr 10;
         if (gBGImg.FlipAx) and (lPanel = 1) then
		lY := ((lImage.Height-lY) * InvZoomShl10) shr 10
	 else if (lY < 1) then
		lY := 0
	 else
		lY := (lY * InvZoomShl10) shr 10;
end;  *)

function ImageZoomPct( var lImage: TImage): integer;
begin
	result := ComputeZoomPct(SelectedImageNum,lImage);
end;

procedure DrawXBar ( lHorPos, lVerPos: integer;var lImage: TImage);
var lL,lT,lW,lH,lZoomPct: integer;
lOffset: single;
begin
	 lZoomPct := ImageZoomPct(lImage);
	 //amx - must match XYscrn2Img and DrawXBar
	 lW := lImage.Width;// div 100;
	 lH := lImage.Height;// div 100;
	 //lL := lHorPos-1;
	 if lZoomPct > 100 then lOffset := 0.5 else
		lOffset := 0;
	 lL := ceil((lHorPos-lOffset) * lZoomPct/100)-1;// div 100;  //-1 as indexed from zero, 0.5 for middle of slice
	 lT := lH-ceil((lVerPos-lOffset) * lZoomPct/100.0);// div 100;
	 //ImgForm.Caption := inttostr(lZoomPct);
	 //lL := (lHorPos * lZoomPct) div 100;
	 //lT := (lVerPos * lZoomPct) div 100;

	 lImage.Canvas.Pen.Color:=gBGImg.XBarClr;
	 //lImage.Canvas.Pen.Color:=$03FF0000;
	 lImage.Canvas.Pen.Width := gBGImg.XBarThick;
	 //next horizontal lines
	 lImage.Canvas.MoveTo(0,lT);
	 lImage.Canvas.LineTo(lL-gBGImg.XBarGap,lT);
	 lImage.Canvas.MoveTo(lL+gBGImg.XBarGap,lT);
	 lImage.Canvas.LineTo(lW,lT);
	 //next vertical lines
	 lImage.Canvas.MoveTo(lL,0);
	 lImage.Canvas.LineTo(lL,lT-gBGImg.XBarGap);
	 lImage.Canvas.MoveTo(lL,lT+gBGImg.XBarGap);
	 lImage.Canvas.LineTo(lL,lH);
end; //Proc DrawXBar

function Scrn2ScaledIntensity (lHdr: TMRIcroHdr; lRaw: single): single;
var lRange,lMin,lMax: single;
begin
	lMin := lHdr.WindowScaledMin;
	lMax := lHdr.WindowScaledMax;
	if lMin > lMax then begin
		lRange := lMin;
		lMin := lMax;
		lMax := lRange;
	end;
	lRange := lMax - lMin;
	result := lMin+(lRaw/255*lRange);
end;

procedure SaveMRIcroROI (lFilename: string);
const
	 kMax12bit = 4095;
	 kMax16bit = (256*256)-1;
	 kMax20bit = (16*256*256)-1;
	 k20v16bit = kMax20bit - kMax16bit;
	kMaxRuns = 10000;
	kMaxFile =  65536;
var lFilePos,lZPos,lZ,lSliceSz,lSliceOffset,lPrevVoxel,lVoxel,lRun,lnRuns,lSlicePos: integer;
	lRunStartRA,lRunLengthRA : array [1..kMaxRuns] of longint;
	lOutputRA: array [1..kMaxFile] of word;
	lF: File;
	lBigFormat: boolean;
begin
	lSliceSz := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
	if lSliceSz > 65535 then
		lBigFormat := true
	else
		lBigFormat := false;
	if  gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems<> (lSLiceSz*lZ) then begin
		Showmessage('You need to create a VOI before you can save it.');
		exit;
	end;
	lSliceOffset := 0;
	lFilePos := 0;
	for lZPos := 1 to lZ do begin
		lnRuns := 0;
		lPrevVoxel := 0;
		for lSlicePos := 1 to lSliceSz do begin
			lVoxel := gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lSlicePos+lSliceOffset];
			if lVoxel > 1 then lVoxel := 1;
			if lVoxel <> lPrevVoxel then begin //start or end run
				lPrevVoxel := lVoxel;
				if lnRuns = kMaxRuns then
					Showmessage('Error: To many runs...')
				else if lVoxel = 1 then begin //start new run
					inc(lnRuns);
					lRunStartRA[lnRuns] := lSlicePos;

				end else begin
					lRunLengthRA[lnRuns] := lSlicePos-lRunStartRA[lnRuns];
				end;
			end; //if start or end
			if (lVoxel > 0) and ((lSlicePos-lRunStartRA[lnRuns])>4090) then begin //end this run, begin new
				lRunLengthRA[lnRuns] := lSlicePos-lRunStartRA[lnRuns]+1;
				lPrevVoxel := 0;
			end; //run >4090
		end; //for each voxel in slice
		if lPrevVoxel = 1 then
			lRunLengthRA[lnRuns] := lSliceSz-lRunStartRA[lnRuns]+1;
		lSliceOffset := lSliceOffset+lSliceSz;
		if lnRuns > 0 then begin
			inc(lFilePos);
			lOutputRA[lFilePos] := lZPos; //record slice number
			inc(lFilePos);
			lOutputRA[lFilePos] := 2*(lnRuns+1); //words to store this slice: 2 per run, plus 2 for slice number and size
			if lBigFormat then begin
				for lRun := 1 to lnRuns do begin
					inc(lFilePos);
					lOutputRA[lFilePos] := (lRunStartRA[lRun] and kMax16bit); //record slice number
					inc(lFilePos);
					lOutputRA[lFilePos] := (lRunLengthRA[lRun] and kMax12bit)+ ((lRunStartRA[lRun] and k20v16bit)shr 4) ; //record slice number
				end;
			end else begin
				for lRun := 1 to lnRuns do begin
					inc(lFilePos);
					lOutputRA[lFilePos] := lRunStartRA[lRun]; //record slice number
					inc(lFilePos);
					lOutputRA[lFilePos] := lRunLengthRA[lRun]; //record slice number
				end;//for each run
			end; //small format
		end; //if data on this slice
	end; //for lZ
	if lFilePos = 0 then begin
		Showmessage('No VOIs detected - create blank MRIcro ROI.');
		exit;
	end;
	if lBigFormat then
	   lOutputRA[1] := lOutputRA[1]+ 32768; //set MSB to 1 to denote this file uses 12/20 bytes
	Filemode := 1;
	AssignFile(lF, lFileName); {WIN}
	Rewrite(lF,lFilePos*2);
	BlockWrite(lF,lOutputRA, 1  {, NumWritten});
	CloseFile(lF);
	Filemode := 2;
end;

procedure SaveAsVOIorNIFTIinnercore (var lFilename: string; var lImgBuffer: ByteP; lImgBufferItems, lImgBufferBPP,lnVol: integer; var lNiftiHdr: TNIFTIHdr);
const
	kImgOffset = 352; //header is 348 bytes, but 352 is divisible by 8...
	kImgOffset2 = 480; //header is 348 bytes, but 352 is divisible by 8...
        lNII2 = false;
var
	lHdr: TNIFTIhdr;
	lBuff: ByteP;
	lF: File;
        lXmm,lYmm,lZmm: single;
	lUnCompressedFilename,lExt: string;
	lImgOffset,lC,lFSize: integer;
        lMat: TMatrix;
begin
  lExt := UpCaseExt(lFileName);
  move(lNiftiHdr,lHdr,sizeof(lHdr));
  if (lExt='.VOI') then begin
	    lHdr.intent_code := kNIFTI_INTENT_NONE;
	lHdr.intent_name[1] := 'B';//Binary
	lHdr.scl_slope := 1/kVOI8bit;
	lHdr.scl_inter := 0;
  end;
  if lnVol > 1 then begin
      lHdr.dim[0] := 4;//3D july2006
      lHdr.dim[4] := lnVol;//3D july2006
  end else begin
      lHdr.dim[0] := 3;//3D july2006
      lHdr.dim[4] := 1;//3D july2006
  end;
  //if not (lImgBufferItems = (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])) then begin //july2006
  //below - images are repositioned to match background
  if gBGImg.Resliced then begin
     lHdr.dim[1] := gBGImg.ScrnDim[1];
     lHdr.dim[2] := gBGImg.ScrnDim[2];
     lHdr.dim[3] := gBGImg.ScrnDim[3];
     lHdr.pixdim[1] := gBGImg.ScrnMM[1]; //Apr07
     lHdr.pixdim[2] := gBGImg.ScrnMM[2]; //Apr07
     lHdr.pixdim[3] := gBGImg.ScrnMM[3]; //Apr07
    lHdr.sform_code :=kNIFTI_XFORM_SCANNER_ANAT; //10102006
	  WriteNiftiMatrix ( lHdr,  //must match MAGMA in nifti_hdr
	gBGImg.ScrnMM[1],0,0,(gBGImg.ScrnOri[1]-1)*-gBGImg.ScrnMM[1],
	0,gBGImg.ScrnMM[2],0,(gBGImg.ScrnOri[2]-1)*-gBGImg.ScrnMM[2],
	0,0,gBGImg.ScrnMM[3],(gBGImg.ScrnOri[3]-1)*-gBGImg.ScrnMM[3]);

      lHdr.qform_code := kNIFTI_XFORM_SCANNER_ANAT; //May07
          lMat:= Matrix3D ( gBGImg.ScrnMM[1],0,0,(gBGImg.ScrnOri[1]-1)*-gBGImg.ScrnMM[1],
	0,gBGImg.ScrnMM[2],0,(gBGImg.ScrnOri[2]-1)*-gBGImg.ScrnMM[2],
	0,0,gBGImg.ScrnMM[3],(gBGImg.ScrnOri[3]-1)*-gBGImg.ScrnMM[3],
						  0,0,0,1);
  nifti_mat44_to_quatern( lMat,lHdr.quatern_b,lHdr.quatern_c,lHdr.quatern_d,
                          lHdr.qoffset_x,lHdr.qoffset_y,lHdr.qoffset_z,
                          lXmm,lYmm,lZmm,lHdr.pixdim[0]);
 end else begin
     //Apr07 - for unresliced data, use raw header for data
 end;
  case lImgBufferBPP of
	4: begin
                     {lSingleRA := SingleP(lImgBuffer);
                     for lPos := 1 to 4 do
                         fx(22,lSingleRA^[lPos]);}
                lHdr.bitpix := 32;
		lHdr.datatype := kDT_FLOAT;//note 32-bit integers saved internally as 32-bit float
	end;
        3: begin
           lHdr.bitpix := 24;
           lHdr.datatype := kDT_RGB;
        end;

	2: begin
		lHdr.bitpix := 16;
		lHdr.datatype := kDT_SIGNED_SHORT;
	end;
	1: begin
		lHdr.bitpix := 8;
		lHdr.datatype := kDT_UNSIGNED_CHAR;
		//lHdr.scl_inter := lHdr.WindowScaledMin;
		//lHdr.scl_slope := (lHdr.WindowScaledMax-lHdr.WindowScaledMin) /255;
	end;
	else begin
		showmessage('Error: Unsupported bytes per voxel: '+inttostr(lImgBufferBPP));
		exit;
	end;
  end;
  if (lExt='.IMG') or (lExt ='.HDR') then begin
	lHdr.magic := kNIFTI_MAGIC_SEPARATE_HDR;
	lHdr.vox_offset := 0;
	Filemode := 1;
	//next write header data as .hdr
	lFilename := changeFileExt(lFilename,'.hdr');
	AssignFile(lF, lFileName);
	Rewrite(lF,sizeof(TNIFTIhdr));
	BlockWrite(lF,lHdr, 1);
	CloseFile(lF);
	//next write image data as .img
	lFilename := changeFileExt(lFilename,'.img');
	AssignFile(lF, lFileName); {WIN}
	Rewrite(lF,lImgBufferItems*lImgBufferBPP);
	BlockWrite(lF,lImgBuffer^,1);
	CloseFile(lF);
	Filemode := 2;
	exit;
  end; //separate header

      lHdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
      lImgOffset := kImgOffset;
  lHdr.vox_offset := lImgOffset;//352 bytes
  lFSize := lImgOffset+(lImgBufferItems*lImgBufferBPP);
  getmem(lBuff,lFSize);

      move(lHdr,lBuff^,sizeof(lHdr));
  //Next: NIfTI 1.1 requires bytes 349..352 set to zero when no XML information

  lC := lImgOffset;
  lBuff^[lC-3] := 0;
  lBuff^[lC-2] := 0;
  lBuff^[lC-1] := 0;
  lBuff^[lC] := 0;
  lC := lImgOffset+1;
  //move(lImgBuffer^[1],lBuff[lC],lImgBufferItems*lImgBufferBPP);
  move(lImgBuffer^,lBuff^[lC],lImgBufferItems*lImgBufferBPP);
  if (lExt='.NII') then begin
	Filemode := 1;
	AssignFile(lF, lFileName);
	Rewrite(lF,lFSize);
	BlockWrite(lF,lBuff^,1);
	CloseFile(lF);
	Filemode := 2;
	exit;
  end; //uncompressed
  lUnCompressedFilename := changefileextx(lFilename,'.nii');
  GZipBuffer(lUnCompressedFilename,lFilename,lBuff,lFSize,false);
  freemem(lBuff);
end;

function gz2niigz (fnm: string): string; //img.gz -> img.nii.gz
var
  lExt: string;
begin
  lExt := uppercase(extractfileext(fnm));
  if (lExt = '.GZ') then
     result  := changefileextX(fnm,'.nii.gz')
  else
      result := fnm;
end;

procedure SaveAsVOIorNIFTIcoreOrtho (var lFilename: string; var lImgBuffer: ByteP; lImgBufferItems, lImgBufferBPP,lnVol: integer; var lNiftiHdr: TNIFTIHdr);
var
  lISize: integer;
  lTempHdr: TMRIcroHdr;
begin
  if not gBGImg.UseReorientHdr then
    exit;
  lFilename := gz2niigz(lFilename);
  lTempHdr.NIFTIhdr := lNIftIHdr;
  lISize := (lImgBufferItems*lImgBufferBPP);
  GetMem(lTempHdr.ImgBufferUnaligned ,lISize + 16);
     {$IFDEF FPC}
  lTempHdr.ImgBuffer := align(lTempHdr.ImgBufferUnaligned,16);
   {$ELSE}
  lTempHdr.ImgBuffer := ByteP($fffffff0 and (integer(lTempHdr.ImgBufferUnaligned)+15));
   {$ENDIF}
  lTempHdr.ImgBufferItems := lImgBufferItems;
  lTempHdr.ImgBufferBPP := lImgBufferBPP;
  move(lImgBuffer^,lTempHdr.ImgBuffer^,lISize);

  Reslice_Img_To_Unaligned (gBGImg.ReorientHdr, lTempHdr ,true);
  SaveAsVOIorNIFTIinnercore (lFilename, lTempHdr.ImgBuffer,lImgBufferItems, lImgBufferBPP,lnVol, lTempHdr.NIFTIhdr);
  //restore orientation
  //12/2010 remove this line - we changed TMPHdr lNiftiHdr := lTempHdr.NIFtiHdr;
  //reslease memory
  FreeMem(lTempHdr.ImgBufferUnaligned);
end;

procedure SaveAsVOIorNIFTIcore (var lFilename: string; var lImgBuffer: ByteP; lImgBufferItems, lImgBufferBPP,lnVol: integer; var lNiftiHdr: TNIFTIHdr);
const
	kImgOffset = 352; //header is 348 bytes, but 352 is divisible by 8...
begin
  //10/2007 - scl_slope;
  //lExt := UpCaseExt(lFileName);
  lFilename := gz2niigz(lFilename);
  if DiskFreeEx(lFilename) < (kImgOffset+(lImgBufferItems*lImgBufferBPP)) then begin
	case MessageDlg('Very little space on the selected drive. Attempt to save to this disk?', mtConfirmation,
		[mbYes, mbCancel], 0) of
		mrCancel: exit;
	end; //case
  end;
  if  FileExistsEX(lFileName) then begin
	  case MessageDlg('Overwrite the file named '+lFileName+'?', mtConfirmation,
		  [mbYes, mbCancel], 0) of
		mrCancel: exit;
	  end; //case
  end; //file exists
  if not gBGImg.UseReorientHdr then
    SaveAsVOIorNIFTIinnercore (lFilename, lImgBuffer,lImgBufferItems, lImgBufferBPP,lnVol, lNiftiHdr)
  else
    SaveAsVOIorNIFTIcoreOrtho (lFilename, lImgBuffer,lImgBufferItems, lImgBufferBPP,lnVol, lNiftiHdr);
end;

(*procedure SaveAsVOIorNIFTIcore (var lFilename: string; var lImgBuffer: ByteP; lImgBufferItems, lImgBufferBPP,lnVol: integer; var lNiftiHdr: TNIFTIHdr);
const
	kImgOffset = 352; //header is 348 bytes, but 352 is divisible by 8...
var
 lHdr: TNIFTIhdr;
	lBuff: ByteP;
 lMat: TMatrix;
 lXmm,lYmm,lZmm: single;
	lF: File;
	lUnCompressedFilename,lExt: string;
	lC,lFSize: integer;
begin
  lExt := UpCaseExt(lFileName);
  if DiskFreeEx(lFilename) < (kImgOffset+(lImgBufferItems*lImgBufferBPP)) then begin
	case MessageDlg('Very little space on the selected drive. Attempt to save to this disk?', mtConfirmation,
		[mbYes, mbCancel], 0) of
		mrCancel: exit;
	end; //case
  end;
  if  FileExistsEX(lFileName) then begin
	case MessageDlg('Overwrite the file named '+lFileName+'?', mtConfirmation,
		[mbYes, mbCancel], 0) of
		mrCancel: exit;
	end; //case
  end; //file exists
  move(lNiftiHdr,lHdr,sizeof(lHdr));
  if (lExt='.VOI') then begin
	    lHdr.intent_code := kNIFTI_INTENT_NONE;
	lHdr.intent_name[1] := 'B';//Binary
	lHdr.scl_slope := 1/kVOI8bit;
	lHdr.scl_inter := 0;
  end;
  if lnVol > 1 then begin
      lHdr.dim[0] := 4;//3D july2006
      lHdr.dim[4] := lnVol;//3D july2006
  end else begin
      lHdr.dim[0] := 3;//3D july2006
      lHdr.dim[4] := 1;//3D july2006
  end;
  //if not (lImgBufferItems = (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])) then begin //july2006
  //below - images are repositioned to match background
  if gBGImg.Resliced then begin
     lHdr.dim[1] := gBGImg.ScrnDim[1];
     lHdr.dim[2] := gBGImg.ScrnDim[2];
     lHdr.dim[3] := gBGImg.ScrnDim[3];
     lHdr.pixdim[1] := gBGImg.ScrnMM[1]; //Apr07
     lHdr.pixdim[2] := gBGImg.ScrnMM[2]; //Apr07
     lHdr.pixdim[3] := gBGImg.ScrnMM[3]; //Apr07
    lHdr.sform_code :=kNIFTI_XFORM_SCANNER_ANAT; //10102006
	  WriteNiftiMatrix ( lHdr,  //must match MAGMA in nifti_hdr
	gBGImg.ScrnMM[1],0,0,(gBGImg.ScrnOri[1]-1)*-gBGImg.ScrnMM[1],
	0,gBGImg.ScrnMM[2],0,(gBGImg.ScrnOri[2]-1)*-gBGImg.ScrnMM[2],
	0,0,gBGImg.ScrnMM[3],(gBGImg.ScrnOri[3]-1)*-gBGImg.ScrnMM[3]);

      lHdr.qform_code := kNIFTI_XFORM_SCANNER_ANAT; //May07
          lMat:= Matrix3D ( gBGImg.ScrnMM[1],0,0,(gBGImg.ScrnOri[1]-1)*-gBGImg.ScrnMM[1],
	0,gBGImg.ScrnMM[2],0,(gBGImg.ScrnOri[2]-1)*-gBGImg.ScrnMM[2],
	0,0,gBGImg.ScrnMM[3],(gBGImg.ScrnOri[3]-1)*-gBGImg.ScrnMM[3],
						  0,0,0,1);
  nifti_mat44_to_quatern( lMat,lHdr.quatern_b,lHdr.quatern_c,lHdr.quatern_d,
                          lHdr.qoffset_x,lHdr.qoffset_y,lHdr.qoffset_z,
                          lXmm,lYmm,lZmm,lHdr.pixdim[0]);
 end else begin
     //Apr07 - for unresliced data, use raw header for data
 end;
  case lImgBufferBPP of
	4: begin
                     {lSingleRA := SingleP(lImgBuffer);
                     for lPos := 1 to 4 do
                         fx(22,lSingleRA^[lPos]);}
                lHdr.bitpix := 32;
		lHdr.datatype := kDT_FLOAT;//note 32-bit integers saved internally as 32-bit float
	end;
        3: begin
           lHdr.bitpix := 24;
           lHdr.datatype := kDT_RGB;
        end;

	2: begin
		lHdr.bitpix := 16;
		lHdr.datatype := kDT_SIGNED_SHORT;
	end;
	1: begin
		lHdr.bitpix := 8;
		lHdr.datatype := kDT_UNSIGNED_CHAR;
		//lHdr.scl_inter := lHdr.WindowScaledMin;
		//lHdr.scl_slope := (lHdr.WindowScaledMax-lHdr.WindowScaledMin) /255;
	end;
	else begin
		showmessage('Error: Unsupported bytes per voxel: '+inttostr(lImgBufferBPP));
		exit;
	end;
  end;
  if (lExt='.IMG') or (lExt ='.HDR') then begin
	lHdr.magic := kNIFTI_MAGIC_SEPARATE_HDR;
	lHdr.vox_offset := 0;
	Filemode := 1;
	//next write header data as .hdr
	lFilename := changeFileExt(lFilename,'.hdr');
	AssignFile(lF, lFileName);
	Rewrite(lF,sizeof(TNIFTIhdr));
	BlockWrite(lF,lHdr, 1);
	CloseFile(lF);
	//next write image data as .img
	lFilename := changeFileExt(lFilename,'.img');
	AssignFile(lF, lFileName); {WIN}
	Rewrite(lF,lImgBufferItems*lImgBufferBPP);
	BlockWrite(lF,lImgBuffer^,1);
	CloseFile(lF);
	Filemode := 2;
	exit;
  end; //separate header
  lHdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
  lHdr.vox_offset := kImgOffset;//352 bytes
  lFSize := kImgOffset+(lImgBufferItems*lImgBufferBPP);
  getmem(lBuff,lFSize);
  move(lHdr,lBuff^,sizeof(lHdr));
  //Next: NIfTI 1.1 requires bytes 349..352 set to zero when no XML information
  lC := kImgOffset;
  lBuff^[lC-3] := 0;
  lBuff^[lC-2] := 0;
  lBuff^[lC-1] := 0;
  lBuff^[lC] := 0;
  lC := kImgOffset+1;
  //move(lImgBuffer^[1],lBuff[lC],lImgBufferItems*lImgBufferBPP);

  move(lImgBuffer^,lBuff^[lC],lImgBufferItems*lImgBufferBPP);
  if (lExt='.NII') then begin
	Filemode := 1;
	AssignFile(lF, lFileName);
	Rewrite(lF,lFSize);
	BlockWrite(lF,lBuff^,1);
	CloseFile(lF);
	Filemode := 2;
	exit;
  end; //uncompressed
  lUnCompressedFilename := changefileext(lFilename,'.nii');
  GZipBuffer(lUnCompressedFilename,lFilename,lBuff,lFSize,false);
  freemem(lBuff);
end;*)

{$IFDEF FPC}
// http://bugs.freepascal.org/view.php?id=7797
function GetExtensionFromFilterAtIndex(Filter: String; Index: Integer): String;
var
	p, pipe: Integer;
begin
  Result := '';
  if Index < 1 then Exit;
  p := 0;
  pipe := 0;
  while (p < Length(Filter)) do begin
    Inc(p);
    if Filter[p] = '|' then Inc(pipe);
    if (pipe = 2 * (Index - 1)) then break;
  end;
  if (p = length(Filter)) then exit;
  System.Delete(Filter,1,p);
  p := Pos('|',Filter);
  if (p = 0) then exit;
  System.Delete(Filter,1,p);
  Filter := Copy(Filter,1,MaxInt);
  p := Pos(';',Filter);
  pipe := Pos('|',Filter);
  if (pipe < p) or (p = 0) then p := pipe;
  if (p > 0) then System.Delete(Filter,p,Length(Filter) - p +1);
   Filter  := StringReplace(Filter, '*', '',[rfReplaceAll, rfIgnoreCase]);
  if (Pos('?',Filter) > 0) {or (Pos('*',Filter) > 0)} then exit;
  Result := Filter;
end;
{$ENDIF}

procedure SaveAsVOIorNIFTI (var lImgBuffer: ByteP; lImgBufferItems, lImgBufferBPP,lnVol: integer; DefaultFormatVOI: boolean; var lNiftiHdr: TNIFTIHdr; lDefFilename: string);
var
	lFileName,lExt: string;
begin
  if DefaultFormatVOI then begin
     ImgForm.SaveDialog1.Filter := 'Volume of Interest(.voi)|*.voi|NIfTI (.nii)|*.nii|NIfTI compressed (.nii.gz)|*.nii.gz|NIfTI (.hdr/.img)|*.hdr|MRIcro (.roi)|*.roi';
     ImgForm.SaveDialog1.FilterIndex:= gBGImg.SaveVoiFilter;    //+1since default added
     ImgForm.SaveDialog1.Filename := changefileext(ImgForm.SaveDialog1.Filename,'.voi');//10/10/06
     ImgForm.SaveDialog1.DefaultExt := '.voi';
  end else begin
      ImgForm.SaveDialog1.Filter := 'NIfTI compressed (.nii.gz)|*.nii.gz|NIfTI (.nii)|*.nii|NIfTI (.hdr/.img)|*.hdr|Volume of Interest(.voi)|*.voi|MRIcro (.roi)|*.roi';
      //ImgForm.SaveDialog1.Filename := changefileext(ImgForm.SaveDialog1.Filename,'.nii.gz');//10/10/06
      ImgForm.SaveDialog1.Filename := changefileext(ImgForm.SaveDialog1.Filename,'');//10/10/06
      ImgForm.SaveDialog1.FilterIndex:= gBGImg.SaveImgFilter; //8/8/2014 removed +1 new behavior with new lazarus 1.2+1since default added
      ImgForm.SaveDialog1.DefaultExt := '.nii.gz';
  end;
  ImgForm.SaveDialog1.filterIndex := 1;
  if lDefFilename <> '' then
	ImgForm.SaveDialog1.Filename := ParseFilename(lDefFilename);
  if not ImgForm.SaveDialog1.Execute then exit;
  if DefaultFormatVOI then
      gBGImg.SaveVoiFilter :=  ImgForm.SaveDialog1.FilterIndex
  else
      gBGImg.SaveImgFilter :=  ImgForm.SaveDialog1.FilterIndex;
  {$IFDEF Darwin}
  if (ImgForm.SaveDialog1.filterIndex = 1) and (not DefaultFormatVOI) and (not ExtGZ(ImgForm.SaveDialog1.Filename)) and(pos(ExtractFileExt(ImgForm.SaveDialog1.Filename), '.nii')=1) then begin
     ImgForm.SaveDialog1.Filename := ImgForm.SaveDialog1.Filename+'.gz';

  end;
  {$ENDIF}
  //showmessage(format('%s %d %s', [ExtractFileExt(ImgForm.SaveDialog1.Filename), ImgForm.SaveDialog1.filterIndex, ImgForm.SaveDialog1.Filename])); exit;
  lFileName := ImgForm.SaveDialog1.Filename;
  {$IFDEF XFPC} //recent versions of Lazarus (1.2) do handle this, but will put .gz not .nii.gz
  if ImgForm.SaveDialog1.filterIndex > 0 then begin
     {$IFNDEF Darwin}
        // check next line in each OS
     {$ENDIF}
     lExt := GetExtensionFromFilterAtIndex(ImgForm.SaveDialog1.Filter,ImgForm.SaveDialog1.FilterIndex); //8/8/2014 check on OSX 10.4
     lFilename := ChangeFileExtX(lFilename,lExt);
   end;
  {$ENDIF}
  lExt := UpCaseExt(lFileName);
  gBGImg.VOIchanged := false;
  if (lExt='.ROI') then begin
	Showmessage('Note that the MRIcro ROI format does not save image dimensions. You may want to save a  copy as VOI format.');
	SaveMRIcroROI (lFileName);
	exit;
  end;
  SaveAsVOIorNIFTIcore (lFilename,lImgBuffer, lImgBufferItems, lImgBufferBPP,lnVol,lNiftiHdr);
end;

procedure SetSubmenuWithTag (var lRootMenu: TMenuItem; lTag: Integer);
var
	lCount,lSubMenu: integer;
begin
	lCount := lRootMenu.Count;
	if lCount < 1 then exit;
	for lSubMenu := (lCount-1) downto 0 do
		if lRootmenu.Items[lSubmenu].Tag = lTag then begin
			lRootmenu.Items[lSubmenu].Checked := true;
			exit
		end;
	//will exit unless tag not found: default select 1st item
	lRootmenu.Items[0].Checked := true;
	//While Recent1.Count > 0 do Recent1.Items[0].Free;
end;

function MaxDim (lX,lY,lZ: integer): integer; //returns largest of 3
begin
  result := lX;
  if lY > result then
	result := lY;
  if lZ > result then
	 result := lZ;
end;
procedure SetBGImgDefaults (var lBGImg: TBGImg);
begin
     with lBGImg do begin
       FlipAx := false;
	FlipSag := false;
        SaveImgFilter := 0;
        SaveVoiFilter := 0;
	OverlayTransPct := -1;
        FontSize := 12;
	BGTransPct := 0;
	LicenseID := 0;
        DarkMode := false;
        ShowDraw := false;
	ResliceOnLoad := false;
	OrthoReslice := true;
	Prompt4DVolume := true;
	MaxDim := 384;
	XBarGap := 7;
	XBarThick := 3;
	XBarClr := clBlue;
	VOIClr := 255;//clRed;
	VOIInvZoom := 1 shl 10; //1024 = 100%
	LesionSmooth := 3;//3mm smoothing
        LesionDilate := 8;
	VOIUndoSlice := 0;
	VOIUndoOrient := 0;
	VOIChanged := false;
	VOImirrored := false;
	VOIUndoVolItems := 0;
	RenderDepthBufferItems := 0;
	SigDig := 5;
	ImageSeparation := 0;
	SliceView := 0;//multiple slices
	SPMDefaultsStatsFmriT := 16;
	SingleRow := false;
	SPMDefaultsStatsFmriT0 := 1;
	SaveDefaultIni := true;
	ThinPen := true;
        LoadUInt16asFloat32 := true;
        PlanarRGB := 2;
	XBarVisible := true;
	OverlaySmooth := true;
	//FSLDIR := 'FSLDIR=/usr/local/fsl';
        FSLBASE := '/usr/local/fsl';
        //FSLBETEXE := '/usr/local/fsl/bin/bet';
	FSLOUTPUTTYPE := 'FSLOUTPUTTYPE=NIFTI_GZ';
	//AutoFill := false;
	KnownAlignment := false;
	StretchQuality := sqHigh;
     end;
end;

procedure AlphaBlend32(lBGQuad,lOverlayQuad : RGBQuadp; lBG0Clr,lOverlay0Clr: DWord; lSlicePixels, lOverlayTransPct: integer);  // 630
var
	lBGwt,lOverlaywt,lPixel,lPos:integer;
	lBGp,lOverlayP: ByteP;
	lBGDWordp,lOverlayDWordp : DWordp;
begin
//note Here we blend the RGBA values - in fact we only need to blend RGB
//however, the position of Alpha varies between OSX ARGB and Linux/Windows RGBA
//this routine would be ~25% faster if we use a compiler-switch for the OS,
//but I do not want to do this until the Compiler code settles a bit more...
	lBGp := ByteP(lBGQuad);
        if (lOverlayTransPct = 100) then exit; //completely transparent: no influence
	lOverlayP := ByteP(lOverlayQuad);
	lOverlayDWordp := DWordp(lOverlayQuad);
	lBGDWordp := DWordp(lBGQuad);
	//next: transparency weighting
	lBGwt := round((lOverlayTransPct)/100 * 1024);
	lOverlaywt := round((100-lOverlayTransPct)/100 * 1024);
	//next redraw each pixel
	lPos := 1;
  if lOverlayTransPct > -1 then begin
	for lPixel := 1 to lSlicePixels do begin
	   if lOverlayDWordp^[lPixel] = lOverlay0Clr then
		inc(lPos,4)
	   else if lBGDWordp^[lPixel] = lBG0Clr then begin
		lBGDWordp^[lPixel] := lOverlayDWordp^[lPixel];
		inc(lPos,4);
	   end else begin
		lBGp^[lPos] := (lBGp^[lPos]*lBGwt+lOverlayP^[lPos]*lOverlaywt) shr 10;
		inc(lPos);
		lBGp^[lPos] := (lBGp^[lPos]*lBGwt+lOverlayP^[lPos]*lOverlaywt) shr 10;
		inc(lPos);
		lBGp^[lPos] := (lBGp^[lPos]*lBGwt+lOverlayP^[lPos]*lOverlaywt) shr 10;
		inc(lPos);
		lBGp^[lPos] := (lBGp^[lPos]*lBGwt+lOverlayP^[lPos]*lOverlaywt) shr 10;
		inc(lPos);
	   end;
	end;
  end else begin
	for lPixel := 1 to lSlicePixels do begin
	   if lOverlayDWordp^[lPixel] = lOverlay0Clr then
		inc(lPos,4)
	   else if lBGDWordp^[lPixel] = lBG0Clr then begin
		lBGDWordp^[lPixel] := lOverlayDWordp^[lPixel];
		inc(lPos,4);
	   end else begin
		if lOverlayP^[lPos] > lBGp^[lPos] then lBGp^[lPos] := lOverlayP^[lPos];
		inc(lPos);
		if lOverlayP^[lPos] > lBGp^[lPos] then lBGp^[lPos] := lOverlayP^[lPos];
		inc(lPos);
		if lOverlayP^[lPos] > lBGp^[lPos] then lBGp^[lPos] := lOverlayP^[lPos];
		inc(lPos);
		if lOverlayP^[lPos] > lBGp^[lPos] then lBGp^[lPos] := lOverlayP^[lPos];
		inc(lPos);
	   end;
	end;
  end;
end;

function Raw2ScaledIntensity (lHdr: TMRIcroHdr; lRaw: single): single;
begin
  if lHdr.NIFTIhdr.scl_slope = 0 then
	result := lRaw+lHdr.NIFTIhdr.scl_inter
  else
	result := (lRaw * lHdr.NIFTIhdr.scl_slope)+lHdr.NIFTIhdr.scl_inter;
end;

function Scaled2RawIntensity (lHdr: TMRIcroHdr; lScaled: single): single;
begin
  if lHdr.NIFTIhdr.scl_slope = 0 then
	result := (lScaled)-lHdr.NIFTIhdr.scl_inter
  else
	result := (lScaled-lHdr.NIFTIhdr.scl_inter) / lHdr.NIFTIhdr.scl_slope;
end;

procedure  FilterLUT (var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr; lMin, lMax: integer); //lLUT: 0=gray,1=red,2=green,3=blue
var
   lInc: integer;
   lRGB : TRGBQuad;
begin
	for lInc := 0 to 255 do
		lHdr.LUT[lInc] := lBackgroundImg.BackupLUT[lInc];
	if (lMin < 0) or (lMin > 255) or (lMax < 0) or (lMax > 255) then
		exit;
	if lMin > lMax then begin
		lInc := lMin;
		lMin := lMax;
		lMax := lInc;
	end; //swap lMin/lMax
	lRGB.rgbRed := (lBackgroundImg.XBarClr and 255) ;
	lRGB.rgbGreen := ((lBackgroundImg.XBarClr shr 8) and 255) ;// and 65280;
	lRGB.rgbBlue := ((lBackgroundImg.XBarClr shr 16) and 255) ;//and 16711680;
	lRGB.rgbReserved := kLUTalpha;
	for lInc := lMin to lMax do
		lHdr.LUT[lInc] := lRGB;
//z
end;

procedure LoadLabelsOld(var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr);
var lLUTname: string;
	lInc: integer;
	lTextFile: TextFile;
	lStr1: string;
	lCh: char;
begin
     SetLength(lBackgroundImg.LabelRA,kMaxLabel+1); //+1 as indexed from 0
     for lInc := 0 to High(lBackgroundImg.LabelRA) do
          lBackgroundImg.LabelRA[lInc] := inttostr(lInc);
     lLUTname := changefileext(lHdr.HdrFileName,'.txt');
     if not Fileexists(lLUTname) then begin
             lLUTname := ParseFileName(lHdr.HdrFileName)+'.txt'; //file.nii.gz -> file.txt
             if not Fileexists(lLUTname) then
                exit;
     end;
     assignfile(lTextFile,lLUTname);
     lHdr.UsesLabels := true;
     Filemode := 0;
	 reset(lTextFile);
	 while not EOF(lTextFile) do begin
        lStr1 := '';
		repeat
			read(lTextFile,lCh);
			if (lCh >= '0') and (lCh <= '9') then
				lStr1 := lStr1 + lCh;
		until (EOF(lTextFile)) or (lCh=kCR) or (lCh=UNIXeoln) or (((lCh=kTab)or (lCh=' ')) and (length(lStr1)>0));
		if (length(lStr1) > 0) and (not EOF(lTextFile)) then begin
		   linc := strtoint(lStr1);
		   if (lInc >= 0) and (lInc <= kMaxLabel) then begin
			  lStr1 := '';
			  repeat
					read(lTextFile,lCh);
					if (EOF(lTextFile)) or (lCh=kCR) or (lCh=UNIXeoln) {or (lCh=kTab) or (lCh=' ')} then
					else
					   lStr1 := lStr1 + lCh;
			  until (EOF(lTextFile)) or (lCh=kCR) or (lCh=UNIXeoln) {or (lCh=kTab)or (lCh=' ')};
                          //showmessage(inttostr(lInc)+'x'+lStr1);
			  lBackgroundImg.LabelRA[lInc] := lStr1;
		   end;

		end;
	 end;
	CloseFile(lTextFile);
	Filemode := 2;
end;

function LoadLabelLUT(var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr): boolean;
var lLUTname: string;
(*	lInc: integer;
	lTextFile: TextFile;
	lStr1: string;
	lCh: char; *)
begin
     result := false;
	lLUTname := changefileext(lHdr.HdrFileName,'.lut');
        if not Fileexists(lLUTname) then // .nii.gz -> .lut
           lLUTname := changefileextX(lHdr.HdrFileName,'.lut');
	if Fileexists(lLUTname) then begin
                result := true;
                lHdr.UsesCustomPalette := true;
		LoadColorScheme(lLUTname,lHdr);

	end;
        //if isBackground then begin
           LoadLabelsOld(lBackgroundImg,lHdr);
           lHdr.UsesLabels := true;
        //end;
end;

procedure  LoadMonochromeLUT (var lLUT: integer; var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr); //lLUT: 0=gray,1=red,2=green,3=blue
var
   lR,lG,lB,lInc: integer;
begin
     for lInc := 0 to 255 do
		 lHdr.LUT[lInc].rgbReserved := kLUTalpha;

	case lLUT of
		1:
		for lInc := 0 to 255 do begin
		 lHdr.LUT[lInc].rgbRed := lInc;
		 lHdr.LUT[lInc].rgbGreen := 0;
		 lHdr.LUT[lInc].rgbBlue := 0;
		end;//red
		2:
		for lInc := 0 to 255 do begin
		 lHdr.LUT[lInc].rgbRed := 0;
		 lHdr.LUT[lInc].rgbGreen := 0;
		 lHdr.LUT[lInc].rgbBlue := lInc;
		end;//blue
		3:
		for lInc := 0 to 255 do begin
		 lHdr.LUT[lInc].rgbRed := 0;
		 lHdr.LUT[lInc].rgbGreen := lInc;
		 lHdr.LUT[lInc].rgbBlue := 0;
		end;//green
		4:
		for lInc := 0 to 255 do begin
		 lHdr.LUT[lInc].rgbRed := lInc;
		 lHdr.LUT[lInc].rgbGreen := 0;
		 lHdr.LUT[lInc].rgbBlue := lInc;
		end;//r+b=violet
		5:
		for lInc := 0 to 255 do begin
		 lHdr.LUT[lInc].rgbRed := lInc;
		 lHdr.LUT[lInc].rgbGreen := lInc;
		 lHdr.LUT[lInc].rgbBlue := 0;
		end;//red + green = yellow
		6:
		for lInc := 0 to 255 do begin
		 lHdr.LUT[lInc].rgbRed := 0;
		 lHdr.LUT[lInc].rgbGreen := lInc;
		 lHdr.LUT[lInc].rgbBlue := lINc;
		end;//green+blue = cyan
		maxint: begin//
		 //showmessage(inttostr(lBackgroundImg.VOIClr)+' '+'r'+inttostr(lR)+'g'+inttostr(lG)+'b'+inttostr(lB));
		 lHdr.LUT[0].rgbRed := 0;
		 lHdr.LUT[0].rgbGreen := 0;
		 lHdr.LUT[0].rgbBlue := 0;

                 lR := (lBackgroundImg.VOIClr and 255) ;
		 lG := ((lBackgroundImg.VOIClr shr 8) and 255) ;// and 65280;
		 lB:= ((lBackgroundImg.VOIClr shr 16) and 255) ;//and 16711680;
		for lInc := 1 to kVOI8bit do begin
		 lHdr.LUT[lInc].rgbRed := round((lInc*lR) div kVOI8bit);
		 lHdr.LUT[lInc].rgbGreen := round((lInc*lG) div kVOI8bit);
		 lHdr.LUT[lInc].rgbBlue := round((lInc*lB) div kVOI8bit);
		end;//green+blue = cyan
		end;
		else begin
			lLUT := 0;
			for lInc := 0 to 255 do begin
				lHdr.LUT[lInc].rgbRed := lInc;
				lHdr.LUT[lInc].rgbGreen := lInc;
				lHdr.LUT[lInc].rgbBlue := lInc;
			end;//for gray
		end//else... gray
	end;


	lHdr.LUTinvisible := (lHdr.LUT[0]);
end;

procedure LUTbias (var lHdr: TMRIcroHdr);
{http://dept-info.labri.fr/~schlick/DOC/gem2.html
http://dept-info.labri.fr/~schlick/publi.html
Fast Alternatives to Perlin's Bias and Gain Functions
Christophe Schlick
Graphics Gems IV, p379-382, April 1994  }
var
	lIndex,lBias: integer;
	lA,lT: single;
	lLUT: TLUT;
begin
//if gBias = 0.5 then exit;
	lA := 0.2;
	 for lIndex := 1 to 254 do begin
		 lT := lIndex/255;
		 //lBias := 255*(lt/((1/la-2)*(1-lt)+1)) ;
		 lBias := round(255*(lt/((1/la-2)*(1-lt)+1)) );
		 lLUT[lIndex] := lHdr.LUT[(lBias)];
		 //lHdr.LUT[lIndex].rgbReserved := kLUTalpha;
	 end;
	 for lIndex := 1 to 254 do
	 	lHdr.LUT[lIndex] := lLUT[lIndex];
end;

procedure LoadColorScheme(lStr: string; var lHdr: TMRIcroHdr);
const UNIXeoln = chr(10);
var
   lF: textfile;
   lBuff: bytep0;
   lFData: file;
   lCh: char;
   lNumStr: String;
   lZ : integer;
   lByte,lIndex: byte;
   //lType,
   lIndx,lLong,lR,lG: boolean;
procedure ResetBools; //nested
begin
	//lType := false;
	lIndx := false;
	lR := false;
	lG := false;
	lNumStr := '';
end; //nested proc ResetBools
begin //proc LoadColorScheme
	 if not fileexistsex(lStr) then exit;
	 lZ := FSize(lStr);
	 if (lZ =768) or (lZ = 800) or (lZ=970) then begin
		//binary LUT
		assignfile(lFdata,lStr);
		Filemode := 0;
		reset(lFdata,1);
		seek(lFData,lZ-768);
		GetMem( lBuff, 768);
		BlockRead(lFdata, lBuff^, 768);
		for lZ := 0 to 255 do begin
			lHdr.LUT[lZ].rgbRed := lBuff^[lZ];
			lHdr.LUT[lZ].rgbGreen := lBuff^[lZ+256];
			lHdr.LUT[lZ].rgbBlue := lBuff^[lZ+512];
			lHdr.LUT[lZ].rgbReserved := kLUTalpha;
		end;
		closefile(lFdata);
		Filemode := 2;

		freemem(lBuff);
			 //LUTBIas (lHdr);
                lHdr.LUTinvisible := (lHdr.LUT[0]);
		exit;
	 end;
	 //Text LUT
	 assignfile(lF,lStr);
	 Filemode := 0;
	 reset(lF);
     lLong := false;
     lIndex := 0;
     ResetBools;
	 for lZ := 0 to 255 do begin
		 lHdr.LUT[lZ].rgbRed := 0;
		 lHdr.LUT[lZ].rgbGreen := 0;
		 lHdr.LUT[lZ].rgbBlue := 0;
		 lHdr.LUT[lZ].rgbReserved := kLUTalpha;
	 end;
     while not EOF(lF) do begin
         read(lF,lCh);
         if lCh = '*' then //comment character
            while (not EOF(lF)) and (lCh <> kCR) and (lCh <> UNIXeoln) do
                  read(lF,lCh);
         if (lCh = 'L') or (lCh = 'l') then begin
            //lType := true;
            lLong := true;
         end; //'l'
         if (lCh = 's') or (lCh = 'S') then begin
            //lType := true;
            lLong := false;
         end; //'s'
		 if lCh in ['0'..'9'] then
             lNumStr := lNumStr + lCh;
         if ((not(lCh in ['0'..'9'])) or (EOF(lF)) ) and (length(lNumStr) > 0) then begin //not a number = space??? try to read number string
              if not lIndx then begin
                 lIndex := strtoint(lNumStr);
                 lIndx := true;
              end else begin //not index
                  if lLong then
                     lByte := trunc(strtoint(lNumStr) / 256)
                  else
                      lByte := strtoint(lNumStr);
		  if not lR then begin
		     lHdr.LUT[lIndex].rgbRed := lByte;
                     lR := true;
                  end else if not lG then begin
                      lHdr.LUT[lIndex].rgbGreen := lByte;
                      lG := true;
                  end else {final value is blue} begin
                      lHdr.LUT[lIndex].rgbBlue := lByte;
                      ResetBools;
                  end;
              end;
              lNumStr := '';
         end;
     end; //not eof
     CloseFile(lF);
	 Filemode := 2;
	 //LUTBIas (lHdr);
	 lHdr.LUTinvisible := (lHdr.LUT[0]);
end; //Proc LoadColorScheme

procedure FreeImgMemory(var lHdr: TMRIcroHdr);
begin
	 with lHdr do begin
         if ScrnBufferItems > 0 then freemem(ScrnBuffer);
	  if ImgBufferItems > 0 then freemem(ImgBufferUnaligned);
	  if RenderBufferItems > 0 then freemem(RenderBuffer);
	  RenderBufferItems := 0;
	  ScrnBufferItems := 0;
	  ImgBufferItems := 0;
     end;
end;

procedure DrawFrame (var lImage: TImage; lL,lT,lR,lB: integer);
begin
lImage.Canvas.Brush.Style := bsSolid;
	 lImage.canvas.pen.color := clWhite;

 	 lImage.canvas.pen.color := clSilver;

	 lImage.Canvas.Rectangle(lL,lT,lR,lB);
	 lImage.canvas.pen.color := clBlack;
	 lImage.Canvas.Rectangle(lL+1,lT+1,lR-1,lB-1);
end;

procedure IntenLabel (var lImage: TImage; var lHdr: TMRIcroHdr; lLTRB: integer;lMinIn,lMaxIn: single);
//special: if lMin=lMax, assumes current window values
var
	lDesiredSteps,lPower,lTxtWid,lTxtTop,lPGWid,lPGHt,lBarTop,lBarLeft,lBarLength,lBarBorder,lBarThick: integer;
	lMin,lMax,l1stStep,lRange,lStepSize,lStepPos: single;
	lSteps,lStep,lDecimals,lStepPosScrn: integer;
begin
	 lMin := lMinIn;
	 lMax := lMaxIn;
	 lBarBorder := 6;
	 lBarThick := 10;
	lPGWid := lImage.Width;
	lPGHt := lImage.Height;
	if gBGImg.XBarClr = TColor(gMRIcroOverlay[kBGOverlayNum].LUTinvisible) then
		lImage.canvas.font.Color := clBlack//clWhite;//gLUT[lClr].rgbRed+(gLUT[lClr].rgbGreen shl 8)+(gLUT[lClr].rgbBlue shl 16);
	 else
		lImage.canvas.font.Color := gBGImg.XBarClr;
	//lImage.canvas.font.Color := clWhite;//gXBarClr;
	lImage.Canvas.Brush.Style := bsClear;
	lImage.Canvas.Font.Name := 'Arial';
	(*if lPGWid < 100 then
		lImage.Canvas.Font.Size := 9
	else if lPGWid < 200 then
	   lImage.Canvas.Font.Size := 12
	else
		lImage.Canvas.Font.Size := 14;*)
        lImage.Canvas.Font.Size := gBGImg.FontSize;
	lTxtTop := lPGHt - ( lBarBorder +(lImage.Canvas.TextHeight('X') div 2));
	//next: compute increment
	lDesiredSteps := 4;
	if lMin=lMax then begin

		lMin := lHdr.WindowScaledMin;
		lMax := lHdr.WindowScaledMax;
		SortSingle(lMin,lMax);
		if (lHdr.WindowScaledMin <= 0) and (lHdr.WindowScaledMax <= 0) then begin
			if (lHdr.LutFromZero) then
				lMax := 0;
			lStepPos := lMin;
			lMin := lMax;
			lMax := lStepPos;
		end else
			if (lHdr.LutFromZero) and (lMin > 0) then
				lMin := 0;
	end; //lMinIn=lMaxIn
   if lMin = lMax then exit;
   //showmessage(realtostr(lMin,4)+'  '+realtostr(lMax,4));
   lRange := abs(lMax - lMin);
   //if lRange = 0 then exit;
   if lRange < 0.000001 then exit;
   lStepSize := lRange / lDesiredSteps;
   lPower := 0;
   while lStepSize >= 10 do begin
	   lStepSize := lStepSize/10;
	   inc(lPower);
   end;
   while lStepSize < 1 do begin
	   lStepSize := lStepSize * 10;
	   dec(lPower);
   end;
   lStepSize := round(lStepSize) *Power(10,lPower);
   if lPower < 0 then
	lDecimals := abs(lPower)
   else
	lDecimals := 0;
   if lMin > lMax then begin // inverted
	l1stStep := trunc((lMax)  / lStepSize)*lStepSize;
	if l1stStep < (lMax) then l1stStep := l1stStep+lStepSize;
	lSteps := trunc( abs((lMin+0.0001)-l1stStep) / lStepSize)+1;
   end else begin
	l1stStep := trunc((lMin)  / lStepSize)*lStepSize;
	if l1stStep < (lMin) then l1stStep := l1stStep+lStepSize;
	lSteps := trunc( abs((lMax+0.0001)-l1stStep) / lStepSize)+1;
   end;
   if not odd(lLTRB) then begin //vertical
		 if lLTRB > 2 then //right
			  lBarLeft := lPGWid - (lBarThick+lBarBorder+3)
		 else //if right else LEFT
			lBarLeft := (lBarThick+lBarBorder+3);
		 lBarLength := lPGHt - (lBarBorder+lBarBorder+2);
		 for lStep := 1 to lSteps do begin
			lStepPos := l1stStep+((lStep-1)*lStepSize);
			lStepPosScrn := round( abs(lStepPos-lMin)/lRange*lBarLength);
			if lLTRB > 2 then //right - align text for width
				lImage.canvas.TextOut(lBarLeft-(lImage.Canvas.TextWidth(realtostr(lStepPos,lDecimals))),lTxtTop-lStepPosScrn,realtostr(lStepPos,lDecimals))
			else
				lImage.canvas.TextOut(lBarLeft,lTxtTop-lStepPosScrn,realtostr(lStepPos,lDecimals));
		end;
   end else begin //if vert else HORIZ
		 lBarLength := lPGWid - (lBarBorder+lBarBorder+2);
		 if lLTRB > 2 then //bottom
			lBarTop := lPGHt - (lBarThick+lBarBorder+lImage.Canvas.TextHeight('X')+1 )
		 else //top
			lBarTop := lBarThick+lBarBorder+1;
		 for lStep := 1 to lSteps do begin
			lStepPos := l1stStep+((lStep-1)*lStepSize);
			lStepPosScrn := round(abs(lStepPos-lMin)/lRange*lBarLength);
			//lStepPosScrn := 15*lStep;
			lTxtWid := lImage.Canvas.TextWidth(realtostr(lStepPos,lDecimals));
			lImage.canvas.TextOut(lBarBorder+lStepPosScrn-(lTxtWid div 2),lBarTop,realtostr(lStepPos,lDecimals));
		end;
   end;//if vert else HORIZ
end;

procedure IntenBar (var lImage: TImage; var lHdr: TMRIcroHdr; lLTRB: integer; lMin,lMax: single);
var lPGHt, lPGWid,lClr,lStripe,lBarBorder,lnStripes,lHorBarTop,lVerBarLeft,lBarThick: integer;
begin
	 //if lMin = lMax then
	 lBarBorder := 6;
	 lBarThick := 10;
	 lPGWid := lImage.Width;
	 lPGHt := lImage.Height;
	 lHorBarTop := lBarBorder;
	 lVerBarLeft := lBarBorder;
	 lImage.canvas.pen.width := 1;
	 if not odd(lLTRB) then begin
		 //vertical
		 if lLTRB > 2 then //right
			lVerBarLeft := lPGWid - (lBarThick+lBarBorder);
		 lnStripes := lPGHt - (lBarBorder+lBarBorder+2);

                 if lnStripes < 1 then exit;
                 {$IFNDEF FPC}
                 DrawFrame(lImage, lVerBarLeft-2, lBarBorder-2,lVerBarLeft+lBarThick+2, lBarBorder+lnStripes+3);
                 {$ELSE}
                 DrawFrame(lImage, lVerBarLeft-2, lBarBorder-2,lVerBarLeft+lBarThick+2, lBarBorder+lnStripes+2);
                 lBarBorder := lBarBorder; lBarThick  := lBarThick +1; {$ENDIF}
		 for lStripe := 0 to lnStripes do begin
			 lClr := round(((lnStripes- lStripe) / lnStripes)*255);
			 lImage.canvas.pen.color := TRGBQuad2TColor(lHdr.LUT[lClr]);
			 lImage.canvas.moveto(lVerBarLeft, lBarBorder+lStripe);
			 lImage.canvas.lineto(lVerBarLeft+lBarThick,lBarBorder+lStripe);
		 end; //draw each stripe
	 end else begin //LTRB
		 //Horizontal
		 if lLTRB > 2 then //bottom
			lHorBarTop := lPGHt - (lBarThick+lBarBorder)-1;
                 lnStripes := lPGWid - (lBarBorder+lBarBorder+1);
                 {$IFNDEF FPC}
                 DrawFrame(lImage,lBarBorder+1, lHorBarTop-2, lBarBorder+lnStripes+3,lHorBarTop+lBarThick+2);
                 {$ELSE}
		 DrawFrame(lImage,lBarBorder, lHorBarTop-2, lBarBorder+lnStripes+4,lHorBarTop+lBarThick+2);
                 lBarBorder := lBarBorder+2; lBarThick  := lBarThick +1; {$ENDIF}

		 if lnStripes < 1 then exit;
		 for lStripe := 0 to lnStripes do begin
			 lClr := round((lStripe / lnStripes)*255);

                 {$IFNDEF FPC}
lImage.canvas.pen.color := lHdr.LUT[lClr].rgbRed+(lHdr.LUT[lClr].rgbGreen shl 8)+(lHdr.LUT[lClr].rgbBlue shl 16);
                 {$ELSE}lImage.canvas.pen.Color := TRGBQuad2TColor(lHdr.LUT[lClr]);{$ENDIF}
			 lImage.canvas.moveto(lBarBorder+lStripe,lHorBarTop);
			 lImage.canvas.lineto(lBarBorder+lStripe,lHorBarTop+lBarThick);
		 end; //draw each stripe
	 end; //if horizontal
	 IntenLabel(lImage,lHdr,lLTRB,lMin,lMax);
end;

//procedure SetDimension32(lInPGHt,lInPGWid:integer; lBuff: RGBQuadp; var lBackgroundImg: TBGImg; var lImage: TImage; lPanel: TScrollBox);
(*procedure DrawBMPZoom( lx, ly, lZoomPct: integer; lBuff: RGBQuadp; var lImage: TImage);
var
  x, y,lYPos,lPos,lImgSz,lOutX,lOutY: Integer;
  lRatio,lRatioRecip: single;
  TempBitmap: TBitmap;
 lLongBuff: LongIntp;
  lXlut: LongIntp0;
begin
  if lZoomPct > 1 then
     lRatio := lZoomPct/100
  else
      lRatio := 1;
  lRatioRecip := 1/lRatio;//e.g. 200% -> ratio = 2, recip = 0.5
  lImgSz := lx * ly;
  TempBitmap := TBitmap.Create;
  lLongBuff := LongIntp(lBuff);
    lOutX := round(lx*lRatio);
    lOutY := round(ly*lRatio);
    TempBitmap.Width := lOutX;
    TempBitmap.Height := lOutY;
    //TempBitmap.PixelFormat := pf32bit ;
    TempBitmap.Transparent := False;
     if lBuff <> nil then begin
     getmem(lXlut,lOutX*sizeof(longint));
     for x:=0 to lOutx-1 do
        lXlut^[x] := trunc((x+lRatio)*lRatioRecip); //find col
     for y:= (lOutY-1) downto 0 do begin
      lYPos := trunc((lOutY-y-0.5)*lRatioRecip) * lx; //find row
      for x:=0 to lOutx-1 do begin
        lPos := lXlut^[x] + lYPos;
        if (lPos > 0) and (lPos <= lImgSz) then
           TempBitmap.Canvas.Pixels[x,y] := lLongBuff^[lPos];
      end; //for x
     end;//for y
        freemem(lXlut);
    end; //if Buff<> nil
    lImage.Picture.Bitmap := TempBitmap;
    TempBitmap.Free;
end;*)


 (*getmem(lTBuff,lInPGHt*lInPGWid*4);
        lLen := lInPGWid*4;
        lSrc := 1;
        lDest := ((lInPGHt-1)*lInPGWid)+1;
        for lY := 1 to lInPGHt do begin
           //Move(Pointer(lBuff^[lSrc]),Pointer(lTBuff^[lDest]),lLen);
           Move(lBuff^[lSrc],lTBuff^[lDest],lLen);
           lSrc := lSrc + lInPGWid;
           lDest := lDest - lInPGWid;
        end; *)
(*procedure DrawBMP( lx, ly: integer; var lBuff: RGBQuadp; var lImage: TImage);
var
  DestPtr: PInteger;
  row, i: integer;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.Width:=lx;
  bmp.Height:=ly;
  bmp.PixelFormat := pf32bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
  if lBuff = nil then exit;
  //lImage.Picture.Bitmap.BeginUpdate(False);
  DestPtr := PInteger(bmp.RawImage.Data);
  if bmp.RawImage.Description.BytesPerLine = (lx * 4) then
     Move(lBuff^, DestPtr^, lx * ly * 4)
  else begin
    i := 1;
    for row:=0 to ly-1 do begin
      Move(lBuff^[i], DestPtr^, lx * 4);
      Inc(PByte(DestPtr),bmp.RawImage.Description.BytesPerLine);
      inc(i, lx);
    end;
  end;
  //lImage.Picture.Bitmap.EndUpdate(False);
  lImage.Picture.Bitmap := bmp;
  bmp.free;
end; *)
(*UPSIDE DOWN!
procedure DrawBMP( lx, ly: integer; var lBuff: RGBQuadp; var lImage: TImage);
var
  DestPtr: PInteger;
  row, i: integer;
begin
  lImage.Picture.Bitmap.Width:=lx;
  lImage.Picture.Bitmap.Height:=ly;
  lImage.Picture.Bitmap.PixelFormat := pf32bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
  if lBuff = nil then exit;
  lImage.Picture.Bitmap.BeginUpdate(False);
  DestPtr := PInteger(lImage.Picture.Bitmap.RawImage.Data);
  if lImage.Picture.Bitmap.RawImage.Description.BytesPerLine = (lx * 4) then
     Move(lBuff^, DestPtr^, lx * ly * 4)
  else begin
    i := 1;
    for row:=0 to ly-1 do begin
      Move(lBuff^[i], DestPtr^, lx * 4);
      Inc(PByte(DestPtr), lImage.Picture.Bitmap.RawImage.Description.BytesPerLine);  //Bmp.RawImage.Description.BytesPerLine
      inc(i, lx);
    end;
  end;
  lImage.Picture.Bitmap.EndUpdate(False);
end; *)

{$IFDEF LINUX}
//DrawBMP16 draws graphics for 16-bit displays. There are two versions, the first has fewer dependencies, the second is thoroughly tested
(*
procedure DrawBMP16( lx, ly: integer; var lBuff: RGBQuadp; var lImage: TImage);
var
  DestPtr: PInteger;
  col, row, i, j: integer;
  clr: TRGBQuad;
  lTempBuff: Bytep;
begin
  lImage.Picture.Bitmap.Width:=lx;
  lImage.Picture.Bitmap.Height:=ly;
  if lBuff = nil then exit;
  lImage.Picture.Bitmap.BeginUpdate(False);
  i := 1;
  getmem(lTempBuff, lx * 2);
  for row:= ly-1 downto 0 do begin
    j := 1;
    for  col := 1 to lx do begin
       clr := lBuff^[i];
       lTempBuff^[j] := (((clr.rgbGreen shr 3) and 3)  shl 6) + clr.rgbBlue shr 3;
       lTempBuff^[j+1] :=  ((clr.rgbRed shr 3) shl 3) + (clr.rgbGreen shr 5)  ;//((clr.rgbRed shr 4) shl 4) + (clr.rgbGreen shr 4);//((clr.rgbRed shr 4) shl 4) + clr.rgbGreen shr 4;//255-63;//clr.rgbRed;
       i := i + 1;
       j := j + 2;
    end;
    DestPtr := PInteger(lImage.Picture.Bitmap.RawImage.GetLineStart(row));
    Move(lTempBuff^[1], DestPtr^, lx * 2);
  end;
  freemem(lTempBuff);
  lImage.Picture.Bitmap.EndUpdate(False);
end;*)

(*procedure DrawBMP16( lx, ly: integer; var lBuff: RGBQuadp; var lImage: TImage);
var
  TempBitmap: TBitmap;
  lRGBBitmap: TRGB32Bitmap;
  {$IFNDEF NOFLIP}
  i,j, row: integer;
  lBuffFlip: RGBQuadp;
  {$ENDIF}
begin
  TempBitmap := TBitmap.Create;
    TempBitmap.Width := lx;
    TempBitmap.Height := ly;
    if lBuff <> nil then begin
          {$IFDEF NOFLIP}
          lRGBBitmap := TRGB32Bitmap.CreateFromData(@lBuff[1],lx,ly);
          DrawRGB32Bitmap(TempBitmap.Canvas.Handle, 0, 0, 0, 0, lx, ly,lRGBBitmap {Self});
          {$ELSE}
          GetMem(lBuffFlip, lx * ly * 4);
          i := 1;
          j := (lx * (ly-1)) + 1;
          for row:= 1 to ly do begin
            Move(lBuff^[i], lBuffFlip^[j], lx * 4);
            inc(i, lx);
            dec(j,lx);
          end;
          lRGBBitmap := TRGB32Bitmap.CreateFromData(@lBuffFlip[1],lx,ly);
          DrawRGB32Bitmap(TempBitmap.Canvas.Handle, 0, 0, 0, 0, lx, ly,lRGBBitmap {Self});
          freemem(lBuffFlip);
          {$ENDIF}
    end;  //if lBuff=nil
    lImage.Picture.Bitmap := TempBitmap;
    TempBitmap.Free;
end;*)
{$ENDIF}
procedure DrawBMP( lx, ly: integer; var lBuff: RGBQuadp; var lImage: TImage);
var
  DestPtr: PInteger;
  row, i: integer;
begin
  lImage.Picture.Bitmap.Width:=lx;
  lImage.Picture.Bitmap.Height:=ly;
  (*if (lImage.Picture.Bitmap.PixelFormat = pf16bit) then begin
         {$IFDEF Linux}
         DrawBMP16( lx, ly, lBuff, lImage);
         {$ELSE}
          imgform.StatusLabel.Caption:='16-bit displays not supported: use an older version osf MRIcronn';
         {$ENDIF}
         exit;
  end;*)
  {$IFDEF UNIX}
    {$IFNDEF LCLCocoa}  //With Cocoa, saved bitmaps will not show lineto/textout if bitmap drawn as pf32bit
    lImage.Picture.Bitmap.PixelFormat := pf24bit; //gtk2, gtk3, qt5
    {$ELSE}
    lImage.Picture.Bitmap.PixelFormat := pf32bit; //Cocoa MacOS
    {$ENDIF}
  {$ELSE}
  lImage.Picture.Bitmap.PixelFormat := pf32bit; //Windows
  {$ENDIF}
  if lBuff = nil then exit;
  lImage.Picture.Bitmap.BeginUpdate(False);
  i := 1;
  for row:= ly-1 downto 0 do begin
    DestPtr := PInteger(lImage.Picture.Bitmap.RawImage.GetLineStart(row));
    Move(lBuff^[i], DestPtr^, lx * 4);
    inc(i, lx);
  end;
  lImage.Picture.Bitmap.EndUpdate(False);
end;

procedure DrawBMPZoomNN(lSrcHt,lSrcWid: integer; lZoomFrac: single; var lRGBBuff: RGBQuadp; var lImage: TImage);
//Nearest Neighbor Stretch
var
  lBuff,lInBuff,lXBuff: LongintP;
  lOutRGBBuff: RGBQuadp;
  lOutWid,lOutHt,lPos,x,y, lRowPos: integer;
  fp, z: single;
begin
  lOutwid := round(lSrcWid*lZoomFrac);
  lOutHt := round(lSrcHt*lZoomFrac);
  if (lOutwid < 2) or (lOutHt < 2) then exit;
  z := (1/lZoomFrac)-eps;
  lInBuff:= LongIntP(lRGBBuff);
  getmem(lBuff, lOutHt*lOutWid * 4);
  getmem(lXBuff, lOutWid*sizeof(longint));
  for x := 1 to lOutWid do begin
      fp := x * z;
      if fp < 1 then
         fp := 1;
      if fp > lSrcWid then
         fp := lSrcWid;
      lXBuff^[x] := ceil(fp);
  end;
  //imgForm.StatusLabel.Caption := floattostr(lZoomFrac);
  lPos := 1;
  for y:= 1 to lOutHt do begin
      fp := y * z;
      if fp < 1 then
         fp := 1;
      if fp > lSrcHt then
         fp := lSrcHt;
      lRowPos:= lSrcWid *  (ceil(fp)-1) ;

        for x := 1 to lOutWid do begin
            lBuff^[lPos] := lInBuff^[lRowPos+lXBuff^[x]];
            inc(lPos);
        end;

  end;//for each line
  freemem(lxBuff);
  lOutRGBBuff := RGBQuadp(@lBuff[1]);
  DrawBMP( lOutWid, lOutHt, lOutRGBBuff, lImage);
  freemem(lBuff);
end;

function mixColor(XloYlo, XloYhi, XhiYlo, XhiYhi: byte; Xfrac, Yfrac: integer): byte;
var
  XfracInv, YfracInv: integer;
begin
  YfracInv := 1024 - Yfrac;
  XfracInv := 1024 - Xfrac;
  result :=( (XloYlo*XfracInv*YfracInv) +
             (XloYhi*XfracInv*Yfrac) +
             (XhiYlo*Xfrac*YfracInv) +
             (XhiYhi*Xfrac*Yfrac)) shr 20;
(*  result :=( (XloYlo*Xfrac*Yfrac) +
             (XloYhi*Xfrac*YfracInv) +
             (XhiYlo*XfracInv*Yfrac) +
             (XhiYhi*XfracInv*YfracInv)) shr 10;  *)
end;

function mixRGB( XloYlo, XloYhi, XhiYlo, XhiYhi: TRGBquad; Xfrac, Yfrac: integer): TRGBquad;
begin
     result.rgbreserved:= kLUTalpha;
     result.rgbRed:=  mixColor(XloYlo.rgbRed, XloYhi.rgbRed, XhiYlo.rgbRed, XhiYhi.rgbRed, Xfrac, Yfrac);
     result.rgbGreen:= mixColor(XloYlo.rgbGreen, XloYhi.rgbGreen, XhiYlo.rgbGreen, XhiYhi.rgbGreen, Xfrac, Yfrac);
     result.rgbBlue:= mixColor(XloYlo.rgbBlue, XloYhi.rgbBlue, XhiYlo.rgbBlue, XhiYhi.rgbBlue, Xfrac, Yfrac);
end;

procedure DrawBMPZoomLin(lSrcHt,lSrcWid: integer; lZoomFrac: single; var lRGBBuff: RGBQuadp; var lImage: TImage);
//Stretch bitmap with bilinear interpolation
var
   lInBuff,lBuff: ByteP;
   lOutRGBBuff: RGBQuadp;
   lOutWid,lOutHt: integer;
  lSrcWidx4, lPos,xPmax, xP,yP,yP2,xP2,z, z2,iz2,w1,w2,w3,w4,lTopPos,lBotPos,
  lINSz,x,y, t: integer;
begin
  lInBuff:= ByteP(lRGBBuff);
  lOutwid := round(lSrcWid*lZoomFrac);
  lOutHt := round(lSrcHt*lZoomFrac);
  if (lOutwid < 2) or (lOutHt < 2) then exit;
  xP2:=((lSrcWid)shl 15)div (lOutWid );
  yP2:=((lSrcHt) shl 15)div (lOutHt);
  lPos := 1;
  getmem(lBuff, lOutHt*lOutWid*4);
  lInSz := lSrcWid *lSrcHt * 4; //32bytesperpixel
  lSrcWidx4 := lSrcWid * 4;
  yP:= -16384+ (yP2 shr 1);
  xPmax :=  ((lSrcWid - 1) * 32768)-1;
  for y:=0 to lOutHt-1 do begin
      xP:=  -16384+ (xP2 shr 1);  //16384, e.g. 0.5 voxel
      if yP <= 0 then begin
         lTopPos := 0;
         lBotPos := 0;
      end else begin
          lTopPos := lSrcWid * (yP shr 15) *4; //top row
          lBotPos := lTopPos+lSrcWidx4;
      end;
      if lBotPos >= lInSz then lBotPos := lBotPos - lSrcWidx4;
      if lTopPos >= lInSz then lTopPos := lTopPos - lSrcWidx4;
      z2:=yP and $7FFF;
      iz2:=$8000-z2;
      for x:=0 to lOutWid-1 do begin
        t:= ((xP shr 15) * 4);
        if (xP > xPmax) then begin
                lBuff^[lPos] :=lBuff^[lPos-4]; inc(lPos); //reds
                lBuff^[lPos] :=lBuff^[lPos-4]; inc(lPos);
                lBuff^[lPos] :=lBuff^[lPos-4]; inc(lPos);
                lBuff^[lPos] :=lBuff^[lPos-4]; inc(lPos);
        end else if (xP < 0) and  ((lBotPos+4) < lInSz) then begin
           {$IFNDEF Darwin}
           lBuff^[lPos] := (lInBuff^[lTopPos+1]*iz2+lInBuff^[lBotPos+1]*z2)shr 15; //red
           inc(lPos);
           lBuff^[lPos] := (lInBuff^[lTopPos+2]*iz2+lInBuff^[lBotPos+2]*z2)shr 15; //green
           inc(lPos);
           lBuff^[lPos] :=  (lInBuff^[lTopPos+3]*iz2+lInBuff^[lBotPos+3]*z2)shr 15; //blue
           inc(lPos);
           lBuff^[lPos] := kLUTalpha;
           inc(lPos);
           {$ELSE}
            {$IFDEF Darwin}
            lBuff^[lPos] :=kLUTalpha; inc(lPos); //reds
            lBuff^[lPos] :=(lInBuff^[lTopPos+2]*iz2+lInBuff^[lBotPos+2]*z2)shr 15; inc(lPos); //greens
            lBuff^[lPos] :=(lInBuff^[lTopPos+3]*iz2+lInBuff^[lBotPos+3]*z2)shr 15; inc(lPos); //greens
            lBuff^[lPos] :=(lInBuff^[lTopPos+4]*iz2+lInBuff^[lBotPos+4]*z2)shr 15; inc(lPos); //greens
            {$ELSE}
            lBuff^[lPos] :=  (lInBuff^[lTopPos+3]*iz2+lInBuff^[lBotPos+3]*z2)shr 15; //blue
            inc(lPos);
            lBuff^[lPos] := (lInBuff^[lTopPos+2]*iz2+lInBuff^[lBotPos+2]*z2)shr 15; //green
            inc(lPos);
            lBuff^[lPos] := (lInBuff^[lTopPos+1]*iz2+lInBuff^[lBotPos+1]*z2)shr 15; //red
            inc(lPos);
            lBuff^[lPos] := kLUTalpha;
            inc(lPos);
            {$ENDIF}
           {$ENDIF}
        end else begin
            z:=xP and $7FFF;
            w2:=(z*iz2)shr 15;
            w1:=iz2-w2;
            w4:=(z*z2)shr 15;
            w3:=z2-w4;
            {$IFNDEF DARWIN}
            lBuff^[lPos] := (lInBuff^[lTopPos+t+1]*w1+lInBuff^[lTopPos+t+5]*w2+lInBuff^[lBotPos+t+1]*w3+lInBuff^[lBotPos+t+5]*w4)shr 15;
            inc(lPos); //red
            lBuff^[lPos] := (lInBuff^[lTopPos+t+2]*w1+lInBuff^[lTopPos+t+6]*w2+lInBuff^[lBotPos+t+2]*w3+lInBuff^[lBotPos+t+6]*w4)shr 15;
            inc(lPos); //green
            lBuff^[lPos] := (lInBuff^[lTopPos+t+3]*w1+lInBuff^[lTopPos+t+7]*w2+lInBuff^[lBotPos+t+3]*w3+lInBuff^[lBotPos+t+7]*w4)shr 15;
            inc(lPos); //blue
            lBuff^[lPos] :=kLUTalpha;
            inc(lPos); //reserved   lPos := lPos + 4;
            {$ELSE}
            {$IFDEF Darwin}
                 //(lInBuff^[lTopPos+t+1]*w1+lInBuff^[lTopPos+t+5]*w2+lInBuff^[lBotPos+t+1]*w3+lInBuff^[lBotPos+t+5]*w4)shr 15;
                 //ALPHA
                 lBuff^[lPos] :=  kLUTalpha ;
                 inc(lPos);
                 //RED
                 lBuff^[lPos] := (lInBuff^[lTopPos+t+2]*w1+lInBuff^[lTopPos+t+6]*w2+lInBuff^[lBotPos+t+2]*w3+lInBuff^[lBotPos+t+6]*w4)shr 15; ;//
                 inc(lPos);
                 //GREEN
                 lBuff^[lPos] :=(lInBuff^[lTopPos+t+3]*w1+lInBuff^[lTopPos+t+7]*w2+lInBuff^[lBotPos+t+3]*w3+lInBuff^[lBotPos+t+7]*w4)shr 15;
                 inc(lPos);
                 //BLUE
                 lBuff^[lPos] :=(lInBuff^[lTopPos+t+4]*w1+lInBuff^[lTopPos+t+8]*w2+lInBuff^[lBotPos+t+4]*w3+lInBuff^[lBotPos+t+8]*w4)shr 15;
                 inc(lPos);
            {$ELSE}
            //Lines below tested on Linux
            lBuff^[lPos] :=(lInBuff^[lTopPos+t+3]*w1+lInBuff^[lTopPos+t+7]*w2+lInBuff^[lBotPos+t+3]*w3+lInBuff^[lBotPos+t+7]*w4)shr 15;
            inc(lPos); //red
            lBuff^[lPos] :=(lInBuff^[lTopPos+t+2]*w1+lInBuff^[lTopPos+t+6]*w2+lInBuff^[lBotPos+t+2]*w3+lInBuff^[lBotPos+t+6]*w4)shr 15;
            inc(lPos); //green
            lBuff^[lPos] :=(lInBuff^[lTopPos+t+1]*w1+lInBuff^[lTopPos+t+5]*w2+lInBuff^[lBotPos+t+1]*w3+lInBuff^[lBotPos+t+5]*w4)shr 15;
            inc(lPos); //blue
            lBuff^[lPos] :=kLUTalpha;
            inc(lPos); //reserved   lPos := lPos + 4;
            {$ENDIF}
           {$ENDIF}
        end;
        Inc(xP,xP2);
      end;   //inner loop
      Inc(yP,yP2);
    end;
  lOutRGBBuff := RGBQuadp(@lBuff[1]);
  DrawBMP( lOutWid, lOutHt, lOutRGBBuff, lImage);
  freemem(lBuff);
end;


procedure SetDimension32(lInPGHt,lInPGWid:integer; lBuff: RGBQuadp; var lBackgroundImg: TBGImg; var lImage: TImage; lPanel: TScrollBox);
var
   lZoom,lZoomY,lZoomX: integer;
  // lStartTime: DWord;  i: integer;
   lTBuff: RGBQuadp;
begin
       //first, compute zoom
       if (lPanel = nil) then
        lImage.Tag := 100
       else if (lPanel.Tag < 1) then begin//autosize
	         lZoomY := round(100*(lPanel.Height-8)/lInPGHt);
           lZoomX := round(100*(lPanel.Width-8)/lInPGWid);
           if lZoomX < lZoomY then
		        lZoom := lZoomX
           else
            lZoom := lZoomY;
           if lZoom < 1 then //nearest integer e.g. 100% or 200%, not 148%
		          lZoom := 100;
            lImage.Tag := lZoom;
        end;
       if (lImage.Tag < 1)  then
        lImage.Tag := 100 ;

       //next draw bitmap
     if lBuff = nil then begin
        getmem(lTBuff,lInPGHt*lInPGWid*4);
        Fillchar(lTBuff^,lInPGHt*lInPGWid*4,0); //set all to zero
         DrawBMP( lInPGWid, lInPGHt, lTBuff, lImage);
        freemem(lTBuff);
     end else if (lImage.Tag = 100) or (lPanel = nil)  then begin
        DrawBMP( lInPGWid, lInPGHt, lBuff, lImage);
     end else begin  //not 100%
        lZoom := lImage.Tag;
        if lZoom = 100 then
         DrawBMP( lInPGWid, lInPGHt, lBuff, lImage)
        else begin
            if lBackgroundImg.StretchQuality = sqHigh then //bilinear smoothed zoom
               DrawBMPZoomLin(lInPGHt,lInPGWid,lZoom/100,lBuff, lImage)
            else //nearest neighbor
               DrawBMPZoomNN(lInPGHt,lInPGWid,lZoom/100,lBuff, lImage);
        end;
        lImage.Tag := lZoom;
    end;
end;

procedure FindImgMinMax8 (var lHdr: TMRIcroHdr; var lMini,lMaxi: integer);
var
  lInc: integer;
begin
	 if (lHdr.ImgBufferBPP <> 1) or (lHdr.ImgBufferItems < 1) then exit;
	 lMini := lHdr.ImgBuffer^[1];
	 lMaxi := lHdr.ImgBuffer^[1];
	 for lInc :=  1 to lHdr.ImgBufferItems do begin
	 if lHdr.ImgBuffer^[lInc] > lMaxi then lMaxi := lHdr.ImgBuffer^[lInc];
	 if lHdr.ImgBuffer^[lInc] < lMini then lMini := lHdr.ImgBuffer^[lInc];
     end;
end; //FindImgMinMax8

procedure FindImgMinMax16 (var lHdr: TMRIcroHdr; var lMini,lMaxi: integer);
//very fast routine for finding brightest and darkest intensity...
var
  lImgSamples,lInc,lFinalVal: integer;
  l16Buf: SmallIntP;
begin
  if (lHdr.ImgBufferBPP <> 2) or (lHdr.ImgBufferItems < 1) then exit;
  lImgSamples := lHdr.ImgBufferItems;
  lInc:=1;
  l16Buf := SmallIntP(lHdr.ImgBuffer );
  lMaxI := l16Buf^[lImgSamples];
  lMinI := lMaxi;
  lFinalVal := lMaxi;
  l16Buf^[lImgSamples]:=32767;  // set last value to the maximum integer value
	while true do  // no check here at all now
	begin
	  while (lMaxI>l16Buf^[lInc]) and (l16Buf^[lInc] >= lMini) do   // stop for a >= value
	inc(lInc);
	  if lInc=lImgSamples then begin
	 l16Buf^[lImgSamples]:=lFinalVal;
	 exit;  // check to see if new max is actually end of data
	  end;
	  if l16Buf^[lInc] >lMaxi then
	 lMaxI:=l16Buf^[lInc];
      if l16Buf^[lInc] < lMini then
	 lMini:=l16Buf^[lInc];
	  inc(lInc);
    end;
end; //FindImgMinMax16

procedure FindImgMinMax32 (var lHdr: TMRIcroHdr; var lMin,lMax: single);
var
  lInc: integer;
  l32Buf : SingleP;
begin
	 if (lHdr.ImgBufferBPP <> 4) or (lHdr.ImgBufferItems < 2) then exit;
	 l32Buf := SingleP(lHdr.ImgBuffer );
	 //if specialsingle(lHdr.MRIcroHdr.gMultiBuf[1]) then lHdr.MRIcroHdr.gMultiBuf[1] := 0.0;
	 lMin := l32Buf^[1];
	 lMax := l32Buf^[1];
	 for lInc := 2 to lHdr.ImgBufferItems do begin
		if (l32Buf^[lInc] > lMax) then lMax := l32Buf^[lInc];
		if (l32Buf^[lInc] < lMin) then lMin := l32Buf^[lInc];
	 end;
         //showmessage(format('qx %g..%g',[lMin, lMax]));
end; //FindImgMinMax32

function ImgVaries ( var lHdr: TMRIcroHdr): boolean;
var
   lF: single;
   lI,lPos: integer;
   l32Buf : SingleP;
   l16Buf : SmallIntP;

begin
    result := false;
    if lHdr.ImgBufferItems = 2 then exit;
    result := true; //assume variance...
    if lHdr.ImgBufferBPP  = 4 then begin //32bit
       l32Buf := SingleP(lHdr.ImgBuffer );
       lF := l32Buf^[1];
       for lPos := 2 to lHdr.ImgBufferItems do
           if l32Buf^[lPos] <> lF then
              exit;
    end else if lHdr.ImgBufferBPP  = 2 then begin //if 16bit ints
        l16Buf := SmallIntP(lHdr.ImgBuffer );
       lI := l16Buf^[1];
       for lPos := 2 to lHdr.ImgBufferItems do
           if l16Buf^[lPos] <> lI then
              exit;
    end else if lHdr.ImgBufferBPP  = 1 then begin //if 16bit ints
       lI := lHdr.ImgBuffer^[1];
       for lPos := 2 to lHdr.ImgBufferItems do
           if lHdr.ImgBuffer^[lPos] <> lI then
              exit;
    end else
        showmessage('ImgVaries error: Unsupported format');
    result := false; //entire image has no variability...
end;

procedure CreateHisto (var lHdr: TMRIcroHdr; var lHisto: HistoRA);
var
   lModShl10,lMinI,lC: integer;
   lMod,lRng: double {was extended};
   l32Buf : SingleP;
   l16Buf : SmallIntP;
begin
	 if lHdr.ImgBufferItems = 0 then exit;
	 for lC := 0 to kHistoBins do
	 lHisto[lC] := 0;
	 if lHdr.ImgBufferBPP  = 4 then begin //32bit
		l32Buf := SingleP(lHdr.ImgBuffer );
		lRng := lHdr.GlMaxUnscaledS - lHdr.GlMinUnscaledS;
		if lRng > 0 then
			lMod := (kHistoBins)/lRng
		else
			lMod := 0;
		for lC := 1 to lHdr.ImgBufferItems do
			inc(lHisto[round((l32Buf^[lC]-lHdr.GlMinUnscaledS)*lMod)]);
	 end else {if lHdr.g16Sz >= lHdr.ScrnBufferSz then}begin //<>32bit.. integer
		   lMinI := round(lHdr.GlMinUnscaledS);
		   lRng := lHdr.GlMaxUnscaledS - lHdr.GlMinUnscaledS;
		   if lRng > 0 then
			  lMod := (kHistoBins)/lRng
		   else
			   lMod := 0;
		   lModShl10 := trunc(lMod * 1024);
			if lHdr.ImgBufferBPP  = 2 then begin //if 16bit ints
				l16Buf := SmallIntP(lHdr.ImgBuffer );
				for lC := 1 to lHdr.ImgBufferItems do
					inc(lHisto[((l16Buf^[lC]-lMinI)*lModShl10)shr 10])
			end else //else 8 bit data
			  for lC := 1 to lHdr.ImgBufferItems do
				  inc(lHisto[((lHdr.ImgBuffer^[lC]-lMinI)*lModShl10)shr 10]);
	 end; //not 32bit
end;

function BinCenter (lBin: integer; var lHdr: TMRIcroHdr): single;
begin
     result := (lHdr.GlMaxUnscaledS - lHdr.GlMinUnscaledS)/(kHistoBins-1); //range div bins
    result := (lBin * result)+ lHdr.GlMinUnscaledS+ (0.5*result);

end;

procedure TextReportHisto (var lHdr: TMRIcroHdr);
var
   lC: integer;
   var lHisto: HistoRA;
begin
     CreateHisto (lHdr, lHisto);
     TextForm.MemoT.Lines.Clear;
     TextForm.MemoT.Lines.add('#Histogram summary ~ Approximate Values');
     TextForm.MemoT.Lines.add('#Image intensity range: '+realtostr(lHdr.GlMinUnscaledS,3)+'..'+realtostr(lHdr.GlMaxUnscaledS,3));
     TextForm.MemoT.Lines.add('#BinNumber'+kTextSep+'BinCenter'+kTextSep+'BinCount');
     for lC := 0 to kHistoBins do
	    TextForm.MemoT.Lines.Add( inttostr(lC) + kTextSep +realtostr(BinCenter(lC,lHdr),3) +kTextSep+ inttostr(lHisto[lC]) );
     TextForm.Show;

end;

procedure DrawHistogram (var lHdr: TMRIcroHdr; var lImage: TImage);
var lPGHt, lPGWid,lIntenBarHt,lStripe,lBarBorder,lnStripes,lHorBarTop,lBarHt,
	l005Pct,ln005Pct,l02Pct,ln02Pct,l0005Pct,ln0005Pct,l001Pct,ln001Pct,l01Pct,ln01Pct,lMaxFreq,lMaxBarHt,lHistoPos,lPrevHistoPos,lFreq,lPos,lTotFreq: integer;
        lPct: double;
	lHisto: HistoRA;
begin
     lPGWid := lImage.Width;
     lPGHt := lImage.Height;
     SetDimension32(lPGHt,lPGWid,nil,gBGImg,lImage,nil);
     lImage.Canvas.Font.Name := 'Arial';
     (*if lPGWid < 100 then
		lImage.Canvas.Font.Size := 9
     else if lPGWid < 200 then
	   lImage.Canvas.Font.Size := 12
     else
         lImage.Canvas.Font.Size := 14;*)
     lImage.Canvas.Font.Size := gBGImg.FontSize;
     CreateHisto (lHdr, lHisto);
     lBarBorder := 6;
     lIntenBarHt := 14;
     DrawFrame(lImage, 0, 0,lPGWid,lPGHt);
     lHorBarTop := lPGHt - lBarBorder-lIntenBarHt-lImage.Canvas.TextHeight('X');
     lMaxBarHt := lHorBarTop - lBarBorder- lBarBorder- lBarBorder;
     lMaxFreq := 0;
     lnStripes := lPGWid - (lBarBorder+lBarBorder+1);
     if gBGImg.XBarClr = clWhite then
        lImage.canvas.pen.color := clBlack//clWhite;//gLUT[lClr].rgbRed+(gLUT[lClr].rgbGreen shl 8)+(gLUT[lClr].rgbBlue shl 16);
     else
         lImage.canvas.pen.color := gBGImg.XBarClr;//clWhite;//gLUT[lClr].rgbRed+(gLUT[lClr].rgbGreen shl 8)+(gLUT[lClr].rgbBlue shl 16);
     lImage.Canvas.Font.Color := lImage.canvas.pen.color;
     lImage.Canvas.Brush.Style := bsSolid;
     lImage.Canvas.Pen.Width := 1;
     lImage.Canvas.Pen.Style := psDot;
     lImage.canvas.moveto(lBarBorder,lHorBarTop-lMaxBarHt-1);
     lImage.canvas.lineto(lPGWid-lBarBorder,lHorBarTop-lMaxBarHt-1);
     lImage.Canvas.Brush.Style := bsClear;
     if (lnStripes < 1) then exit;
	 //Next: compute scale find freq in graph - not same as image, as with large graphs bars resampled
	 lPrevHistoPos := 0;
	 lTotFreq := 0;
	 for lStripe := 0 to lnStripes do begin
		lHistoPos := round(lStripe / lnStripes*kHistoBins);
		if lPrevHistoPos > lHistoPos then
			lPrevHistoPos := lHistoPos;
		for lPos := lPrevHistoPos to lHistoPos do
			lTotFreq := lTotFreq+lHisto[lPos];
                lPrevHistoPos := lHistoPos+1;
	 end;
         ln02Pct := 0;
	 ln01Pct := 0;
         ln005Pct := 0;
         ln001Pct := 0;
         ln0005Pct := 0;
	 l02Pct := round(lTotFreq/50);
	 l01Pct := round(lTotFreq/100);
         l005Pct := round(lTotFreq/200);
	 l001Pct := round(lTotFreq/1000);
	 l0005Pct := round(lTotFreq/2000);
	 lPrevHistoPos := 0;
	 for lStripe := 0 to lnStripes do begin
		lHistoPos := round(lStripe / lnStripes*kHistoBins);
		if lPrevHistoPos > lHistoPos then
			lPrevHistoPos := lHistoPos;
                lFreq := 0;
		for lPos := lPrevHistoPos to lHistoPos do
			lFreq := lFreq+lHisto[lPos];
		if lFreq > lMaxFreq then
			lMaxFreq := lFreq;
		if lFreq > l02Pct then
			inc(ln02Pct);
		if lFreq > l01Pct then
			inc(ln01Pct);
		if lFreq > l005Pct then
			inc(ln005Pct);
		if lFreq > l001Pct then
			inc(ln001Pct);
		if lFreq > l0005Pct then
			inc(ln0005Pct);
		//lTotFreq := lTotFreq + lFreq;
		lPrevHistoPos := lHistoPos+1;
	 end;
	 lImage.Canvas.Pen.Style := psSolid;
		if ln02Pct > 5 then
		   lPct := 5
                else if ln01Pct > 5 then
		   lPct := 2
		else if ln005Pct > 5 then
		   lPct := 1
                else if ln001Pct > 4 then
		   lPct := 0.5
                else if ln0005Pct > 4 then
		   lPct := 0.01
                else
		   lPct := 0.05;
         lMaxFreq :=round( lTotFreq * (lPct/100));
	 if (lMaxFreq = 0)  then exit;
     //Next: draw bars
     lImage.canvas.TextOut(lPGWid div 2,lHorBarTop-lMaxBarHt-1-6,' '+floattostr(lPct)+'% ');
     lImage.Canvas.Brush.Style := bsClear;
     lPrevHistoPos := 0;
     for lStripe := 0 to lnStripes do begin
		lHistoPos := round(lStripe / lnStripes*kHistoBins);
		if lPrevHistoPos > lHistoPos then
			lPrevHistoPos := lHistoPos;
		lFreq := 0;
		for lPos := lPrevHistoPos to lHistoPos do
			lFreq := lFreq+lHisto[lPos];
		if lFreq > lMaxFreq then begin
			lFreq := lMaxFreq;
			lImage.canvas.moveto(lBarBorder+lStripe,lHorBarTop-lMaxBarHt-8);
			lImage.canvas.lineto(lBarBorder+lStripe,lHorBarTop-lMaxBarHt-6);
			lImage.canvas.moveto(lBarBorder+lStripe,lHorBarTop-lMaxBarHt-4);
			lImage.canvas.lineto(lBarBorder+lStripe,lHorBarTop-lMaxBarHt-2);
		end;
		lBarHt := round(lFreq/lMaxFreq*lMaxBarHt);
		lImage.canvas.moveto(lBarBorder+lStripe,lHorBarTop);
		lImage.canvas.lineto(lBarBorder+lStripe,lHorBarTop-lBarHt);
		lPrevHistoPos := lHistoPos+1;
     end; //draw each stripe
     intenBar(lImage,lHdr,3,Raw2ScaledIntensity(lHdr,lHdr.GlMinUnScaledS),Raw2ScaledIntensity(lHdr,lHdr.GlMaxUnscaledS));
end;

procedure Balance (var lHdr: TMRIcroHdr);
var
   lPct,lNum,lC: integer;
   lHisto: HistoRA;
   lBlackAUtoBal,lWhiteAutoBal: integer;
begin //dsa
	 if lHdr.ImgBufferItems = 0 then exit;
	 CreateHisto (lHdr, lHisto);
	 lPct := (lHdr.ImgBufferItems *2) div 100;
	 lNum := 0;
	 lC := kHistoBins;
	 repeat
		   lNum := lNum + lHisto[lC];
		   dec(lC);
	 until (lC = 0) or (lNum >= lPct);
	 if (lNum >= lPct) and (lC > 0) then
		lWHiteAUtoBal:= lC
	 else begin
		  lC := kHistoBins;
		  repeat
				lNum := lHisto[lC];
				dec(lC);
		  until (lC = 0) or (lNum > 0);
		  if lC = 0 then
			 lWHiteAUtoBal := kHistoBins
		  else
			  lWHiteAUtoBal := lC;
	 end;
	 lNum := 0;
	 lC := 0;
	 repeat
		lNum := lNum + lHisto[lC];
		inc(lC);
	 until (lC >= kHistoBins) or (lNum >= lPct);
	 if (lNum >= lPct) and (lC < kHistoBins) and (lC >2)  then
			  lBlackAutoBal := lC
	 else
			   lBlackAutoBal := 2;
	 if (lWHiteAUtoBal-lBlackAutoBal) < (kHistoBins/20) then begin //5% of range..
			   lBlackAutoBal  := 2;
			   lWHiteAUtoBal := kHistoBins;
	 end;
         lHdr.AutoBalMaxUnscaled := ((lWhiteAutoBal/kHistoBins)*(lHdr.GlMaxUnscaledS-lHdr.GlMinUnscaledS))+lHdr.GlMinUnscaledS;
	 lHdr.AutoBalMinUnscaled := ((lBlackAutoBal/kHistoBins)*(lHdr.GlMaxUnscaledS-lHdr.GlMinUnscaledS))+lHdr.GlMinUnscaledS;
	 //only apply rounding if there is a large difference - e.g. if range is 0..1 then rounding will hurt
         if (lHdr.ImgBufferBPP  < 4) and ((lHdr.AutoBalMaxUnscaled-lHdr.AutoBalMinUnscaled) > 50) then begin //round integer values
			  lHdr.AutoBalMinUnscaled := round(lHdr.AutoBalMinUnscaled);
			  lHdr.AutoBalMaxUnscaled := round(lHdr.AutoBalMaxUnscaled);
	 end;
end; //proc Balance

procedure ReturnMinMax (var lHdr: TMRIcroHdr; var lMin,lMax: single; var lFiltMin8bit, lFiltMax8bit: integer);
var
	lSwap,lMinS,lMaxS {,lHalfBit}: single;
begin
	 lFiltMin8bit := 0;
	 lFiltMax8bit := 255;
	 lMinS := lHdr.WindowScaledMin;
	 lMaxS := lHdr.WindowScaledMax;
	 if lMinS > lMaxS then begin //swap
		lSwap := lMinS;
		lMinS := lMaxS;
		lMaxS := lSwap;
	 end;//swap
	 lMin := (Scaled2RawIntensity(lHdr, lMinS));
	 lMax := (Scaled2RawIntensity(lHdr, lMaxS));
	 //if lMin = lMax then exit;
	 if (lHdr.LutFromZero) then begin
		 if (lMinS > 0) and (lMaxS <> 0)  then begin
				//lMin := Scaled2RawIntensity(lHdr, 0);
				lFiltMin8bit := round(lMinS/lMaxS*255);
				//lMinS := - lHalfBit;//0;
				lHdr.Zero8Bit := 0;
		 end else if (lMaxS < 0) and (lMinS <> 0) then begin
				//lMax := Scaled2RawIntensity(lHdr, -0.000001);
				lFiltMax8bit := 255-round(lMaxS/lMinS*255);
				//lMaxS :=  lHalfBit; //0;
				//lFiltMax8bit := (Scaled2RawIntensity(lHdr, lHdr.WindowScaledMax));
		 end; //> 0
	 end; //LUTfrom Zero
	 lHdr.Zero8Bit := lMinS;
	 lHdr.Slope8bit := (lMaxS-lMinS)/255;
end; //ReturnMinMax


procedure FilterScrnImg (var lHdr: TMRIcroHdr);
var
	lInc,lItems,lFiltMin8bit,lFiltMax8bit: integer;
	lMinS,lMaxS,lScale: single;
begin
  ReturnMinMax(lHdr,lMinS,lMaxS,lFiltMin8bit,lFiltMax8bit);
  lItems :=lHdr.ScrnBufferItems;
  if lItems < 1 then exit;
if lFiltMax8Bit < 255 then begin
	lFiltMin8bit := 255-lFiltMax8bit;
	lFiltMax8Bit := 255;
end;
  lScale := (lFiltMax8bit-lFiltMin8bit)/255;
  if (lFiltMin8bit > 0) or (lFiltMax8bit < 255) then
	for lInc := 1 to lItems do
		if lHdr.ScrnBuffer^[lInc] <> 0 then
			lHdr.ScrnBuffer^[lInc] := lFiltMin8bit+round(lHdr.ScrnBuffer^[lInc]*lScale);
end; //FilterScrnImg

procedure RescaleImgIntensity8(var lHdr: TMRIcroHdr );
var lRng: single;
	lLUTra: array[0..255] of byte;
	lMax,lMin,lSwap,lMod: single;
	lFiltMin8bit,lFiltMax8bit,lInc: integer;
begin
     if (lHdr.ImgBufferItems < 2) or  (lHdr.ImgBufferBPP <> 1) then
		exit;
     if (lHdr.UsesCustomPaletteRandomRainbow) then begin
        createLutLabel (lHdr.LUT, abs(lHdr.WindowScaledMax-lHdr.WindowScaledMin)/100);
         for lInc := 1 to lHdr.ScrnBufferItems do
             lHdr.ScrnBuffer^[lInc] := lHdr.ImgBuffer^[lInc];
      (*    l16Buf := SmallIntP(lHdr.ImgBuffer );
          for lInc := 1 to lHdr.ScrnBufferItems do
             lHdr.ScrnBuffer^[lInc] := ((l16Buf^[lInc]-1) mod 100)+1;
      *)
      exit;
     end;

  ReturnMinMax (lHdr, lMin,lMax,lFiltMin8bit,lFiltMax8bit);

         lRng := (lMax - lMin);
	 if lRng <> 0 then
		lMod := abs({trunc}(((254)/lRng)))
	 else
		 lMod := 0;
	 if lMin > lMax then begin  //maw
		 lSwap := lMin;
		 lMin := lMax;
		 lMax := lSwap;
	 end;
	 for lInc := 0 to 255 do begin
		 if lInc < lMin then
			lLUTra[lInc] := 0
		 else if lInc >= lMax then
			lLUTra[lInc] := 255
		 else
			 lLUTra[lInc] := trunc(((lInc-lMin)*lMod)+1);
	 end; //fill LUT
	 if lRng < 0 then //inverted scale... e.g. negative scale factor
		for lInc := 0 to 255 do
			lLUTra[lInc] := 255-lLUTra[lInc];
	 for lInc := 1 to lHdr.ScrnBufferItems do
		 lHdr.ScrnBuffer^[lInc] := lLUTra[lHdr.ImgBuffer^[lInc]];
end;//proc RescaleImgIntensity8

procedure ReturnMinMaxInt (var lHdr: TMRIcroHdr; var lMin,lMax, lFiltMin8bit, lFiltMax8bit: integer);
var
	lMinS,lMaxS: single;
begin
	ReturnMinMax (lHdr, lMinS,lMaxS,lFiltMin8bit, lFiltMax8bit);
	lMin := round(lMinS);
	lMax := round(lMaxS);
end;

procedure RescaleImgIntensity16(var lHdr: TMRIcroHdr );
var lRng: single;
	lBuff: bytep0;
	l16Buf : SmallIntP;
	lFiltMin8bit,lFiltMax8bit,lRngi,lMin16Val,lMax,lMin,lSwap,lModShl10,lInc,lInt: integer;
begin
	 if (lHdr.ImgBufferBPP <> 2) or (lHdr.ImgBufferItems < 2) then exit;
         if (lHdr.UsesCustomPaletteRandomRainbow) then begin
          createLutLabel (lHdr.LUT, abs(lHdr.WindowScaledMax-lHdr.WindowScaledMin)/100);
          l16Buf := SmallIntP(lHdr.ImgBuffer );
          for lInc := 1 to lHdr.ScrnBufferItems do
                 lHdr.ScrnBuffer^[lInc] := ((l16Buf^[lInc]-1) mod 100)+1;
          exit;
         end;
         ReturnMinMaxInt (lHdr, lMin,lMax,lFiltMin8bit,lFiltMax8bit);
	 lRng := lMax - lMin;
	 if lRng <> 0 then
		lModShl10 := abs( trunc(((254)/lRng)* 1024))
	 else
		 lModShl10 := 0;
	 if lMin > lMax then begin
		 lSwap := lMin;
		 lMin := lMax;
		 lMax := lSwap;
	 end;
	 lMin16Val :=  trunc(lHdr.GlMinUnscaledS);
	 lRngi := (1+ trunc(lHdr.GlMaxUnscaledS))-lMin16Val;
	 getmem(lBuff, lRngi+1);  //+1 if the only values are 0,1,2 the range is 2, but there are 3 values!
	 for lInc := 0 to (lRngi) do begin //build lookup table
				   lInt := lInc+lMin16Val;
				   if lInt >= lMax then
					  lBuff^[lInc] := (255)
				   else if lInt < lMin then
						lBuff^[lInc] := 0
				   else
					  lBuff^[lInc] := (((lInt-lMin)*lModShl10) shr 10)+1 ;
					  //lBuff[lInc] := (((lInt-lMin)*lModShl10) shr 10) ;
	 end; //build lookup table
	 if lRng < 0 then //inverted scale... e.g. negative scale factor
		for lInc := 0 to lRngi do
			lBuff^[lInc] := 255-lBuff^[lInc];
	 l16Buf := SmallIntP(lHdr.ImgBuffer );
	 for lInc := 1 to lHdr.ImgBufferItems do
		 lHdr.ScrnBuffer^[lInc] := lBuff^[l16Buf^[lInc]-lMin16Val] ;
	 freemem(lBuff); //release lookup table
end;//proc RescaleImgIntensity16;

procedure RescaleImgIntensity32(var lHdr: TMRIcroHdr );
var lRng: double;
lMod,lMax,lMin,lSwap: single {was extended};
   lInc,lItems,lFiltMin8bit,lFiltMax8bit: integer;
   l32Buf : SingleP;
begin
	lItems := lHdr.ImgBufferItems ;
	//fx(lItems,777);
	if (lHdr.ImgBufferBPP <> 4) or (lItems< 2) then exit;
	l32Buf := SingleP(lHdr.ImgBuffer );
	//fx(lHdr.WindowScaledMin , lHdr.WindowScaledMax);
	ReturnMinMax (lHdr, lMin,lMax,lFiltMin8bit,lFiltMax8bit); //qaz
	lRng := (lMax - lMin);
	if lRng <> 0 then
		lMod := abs(254/lRng)
	 else begin //June 2007 - binary contrast
  		for lInc := 1 to lItems do begin
                    if l32Buf^[lInc] >= lMax then
                       lHdr.ScrnBuffer^[lInc] := 255
                    else //if l32Buf[lInc] < lMin then
                         lHdr.ScrnBuffer^[lInc] := 0;
                end;
		 exit;
         end;
	(*if lRng <> 0 then
		lMod := abs(254/lRng)
	 else
		 lMod := 0;*)
	 if lMin > lMax then begin
		 lSwap := lMin;
		 lMin := lMax;
		 lMax := lSwap;
	 end;
	 lMin := lMin - abs(lRng/255);//lMod;
	 //showmessage(realtostr(lMin,3)+'  '+realtostr(lMax,3));
	begin//not SSE
		for lInc := 1 to lItems do begin
		 if l32Buf^[lInc] > lMax then
					lHdr.ScrnBuffer^[lInc] := 255
		 else if l32Buf^[lInc] < lMin then
					  lHdr.ScrnBuffer^[lInc] := 0  //alfa
		 else begin
			 lHdr.ScrnBuffer^[lInc] :=  round ((l32Buf^[lInc]-lMin)*lMod);
		 end;
		end; //for each voxel
	 end; // SSE-vs-x87 choice
	 //next - flip intensity range OPTIONAL
	 if lRng < 0 then //inverted scale... e.g. negative scale factor
		for lInc := 1 to lItems do
			lHdr.ScrnBuffer^[lInc] := 255-lHdr.ScrnBuffer^[lInc];
end; //RescaleImgIntensity32

function MirrorImgBuffer(var lHdr: TMRIcroHdr ): boolean;
var
 lXPos,lYPos,lZPos,lX,lY,lZ,lHlfX,lLineOffset: integer;
 lTemp32: single;
 lTemp16: SmallInt;
 lTemp: byte;
 l32: SingleP;
 l16: SmallIntP;
begin
  result := false;
  lX := lHdr.NIFTIhdr.Dim[1];
  lY := lHdr.NIFTIhdr.Dim[2];
  lZ := lHdr.NIFTIhdr.Dim[3];
  if lHdr.NIFTIhdr.Dim[4] > 1 then begin
      Showmessage('Can not mirror 4D data : '+lHdr.HdrFileName);
      exit;
  end;
  if (lHdr.ImgBufferItems < (lX*lY*lZ)) or (lX < 2) then begin
      Showmessage('Unsupported filetype : '+lHdr.HdrFileName);
     exit;
  end;
  lHlfX := lX div 2;
  lLineOffset := 0;
  //for each datatype...
  if lHdr.ImgBufferBPP = 4 then begin
	l32 := SingleP(lHdr.ImgBuffer);
        for lZPos := 1 to lZ do begin
            for lYPos := 1 to lY do begin
                for lXPos := 1 to lHlfX do begin
                    lTemp32 := l32^[lXPos+lLineOffset];
                    l32^[lXPos+lLineOffset] := l32^[1+lX-lXPos+lLineOffset];
                    l32^[1+lX-lXPos+lLineOffset] := lTemp32;
                end; //for X
                lLineOffset := lLineOffset + lX;
            end; //for Y
        end; //for Z
  end else if lHdr.ImgBufferBPP = 2 then begin
	l16 := SmallIntP(lHdr.ImgBuffer);
        for lZPos := 1 to lZ do begin
            for lYPos := 1 to lY do begin
                for lXPos := 1 to lHlfX do begin
                    lTemp16 := l16^[lXPos+lLineOffset];
                    l16^[lXPos+lLineOffset] := l16^[1+lX-lXPos+lLineOffset];
                    l16^[1+lX-lXPos+lLineOffset] := lTemp16;
                end; //for X
                lLineOffset := lLineOffset + lX;
            end; //for Y
        end; //for Z
  end else if lHdr.ImgBufferBPP = 1 then begin
        for lZPos := 1 to lZ do begin
            for lYPos := 1 to lY do begin
                for lXPos := 1 to lHlfX do begin
                    lTemp := lHdr.ImgBuffer^[lXPos+lLineOffset];
                    lHdr.ImgBuffer^[lXPos+lLineOffset] := lHdr.ImgBuffer^[1+lX-lXPos+lLineOffset];
                    lHdr.ImgBuffer^[1+lX-lXPos+lLineOffset] := lTemp;
                end; //for X
                lLineOffset := lLineOffset + lX;
            end; //for Y
        end; //for Z
  end else //unsupported bits-per-pixel dataformat
	Showmessage('Unsupported BPP ='+inttostr(lHdr.ImgBufferBPP) );
  result := true;
end; //proc MirrorImgBuffer

procedure MirrorScrnBuffer(var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr );
var
 lXPos,lYPos,lZPos,lX,lY,lZ,lHlfX,lLineOffset: integer;
 lTemp: byte;
begin
  lX := lBackgroundImg.ScrnDim[1];
  lY := lBackgroundImg.ScrnDim[2];
  lZ := lBackgroundImg.ScrnDim[3];
  if (lHdr.ScrnBufferItems < (lX*lY*lZ)) or (lX < 2) then exit;
  lHlfX := lX div 2;
  lLineOffset := 0;
  for lZPos := 1 to lZ do begin
      for lYPos := 1 to lY do begin
          for lXPos := 1 to lHlfX do begin
              lTemp := lHdr.ScrnBuffer^[lXPos+lLineOffset];
			  lHdr.ScrnBuffer^[lXPos+lLineOffset] := lHdr.ScrnBuffer^[1+lX-lXPos+lLineOffset];
			  lHdr.ScrnBuffer^[1+lX-lXPos+lLineOffset] := lTemp;
		  end; //for X
		  lLineOffset := lLineOffset + lX;
	  end; //for Y
  end; //for Z
end; //proc MirrorImScrnBuffer

procedure FindMatrixPt (lX,lY,lZ: single; var lXout,lYOut,lZOut: single; var lMatrix: TMatrix);
begin
	lXOut := (lX*lMatrix.matrix[1,1])+(lY*lMatrix.matrix[1,2])+(lZ*lMatrix.matrix[1,3])+lMatrix.matrix[1,4];
	lYOut := (lX*lMatrix.matrix[2,1])+(lY*lMatrix.matrix[2,2])+(lZ*lMatrix.matrix[2,3])+lMatrix.matrix[2,4];
	lZOut := (lX*lMatrix.matrix[3,1])+(lY*lMatrix.matrix[3,2])+(lZ*lMatrix.matrix[3,3])+lMatrix.matrix[3,4];
end;

procedure CheckMaxMin(var lX,lY,lZ,lXMax,lYMax,lZMax,lXMin,lYMin,lZMin: single);
begin
	if lX > lXMax then lXMax := lX;
	if lY > lYMax then lYMax := lY;
	if lZ > lZMax then lZMax := lZ;
	if lX < lXMin then lXMin := lX;
	if lY < lYMin then lYMin := lY;
	if lZ < lZMin then lZMin := lZ;
end;

function FindOriMM (lX1,lY1,lZ1,lX2,lY2,lZ2: integer; var lMatrix: TMatrix): single;
var
   lXdx,lYdx,lZdx,lXmm1,lYmm1,lZmm1,lXmm2,lYmm2,lZmm2: single;
begin
  FindMatrixPt(lX1,lY1,lZ1,lXmm1,lYmm1,lZmm1,lMatrix);
  FindMatrixPt(lX2,lY2,lZ2,lXmm2,lYmm2,lZmm2,lMatrix);
  lXdx := abs(lXmm1-lXmm2);
  lYdx := abs(lYmm1-lYmm2);
  lZdx := abs(lZmm1-lZmm2);
  if (lXdx > lYdx) and (lXdx > lZdx) then begin //X greatest
     result := lXmm1;
  end else if (lYdx > lZdx) then begin //Y greatest
     result := lYmm1;
  end else begin //Z greatest
     result := lZmm1;
  end;
  result := -(result);
end;

procedure FindMatrixBounds (var lBGImg: TBGImg; var lHdr: TMRIcroHdr; lReslice: boolean);
label 121;
var
 lMatrix: TMatrix;
 lPos,lPass: integer;
 lXc,lYc,lZc,lXmin,lXMax,lYMin,lYMax,lZMin,lZMax,lX,lY,lZ,lmmMin,lDimMMMax: single;
begin
     if not lReslice then begin //Dec06
           lBGImg.ScrnDim[1] := lHdr.NIFTIhdr.Dim[1];//+0.5 Dec06
           lBGImg.ScrnDim[2] := lHdr.NIFTIhdr.Dim[2];//+0.5 Dec06
           lBGImg.ScrnDim[3] := lHdr.NIFTIhdr.Dim[3];//+0.5 Dec06
           lBGImg.ScrnMM[1] := lHdr.NIFTIhdr.pixdim[1];
           lBGImg.ScrnMM[2] := lHdr.NIFTIhdr.pixdim[2];
           lBGImg.ScrnMM[3] := lHdr.NIFTIhdr.pixdim[3];
           //Sept07 -estimate origin
              lBGImg.ScrnOri[1] := lBGImg.ScrnDim[1] div 2;
              lBGImg.ScrnOri[2] := lBGImg.ScrnDim[2] div 2;
              lBGImg.ScrnOri[3] := lBGImg.ScrnDim[3] div 2;
           if lHdr.NIfTItransform then begin
              lBGImg.ScrnOri[1] := 0;
              lBGImg.ScrnOri[2] := 0;
              lBGImg.ScrnOri[3] := 0;
              mm2Voxel (lBGImg.ScrnOri[1],lBGImg.ScrnOri[2],lBGImg.ScrnOri[3], lBGImg.invMat);//vcx
(*              lMatrix := lHdr.Mat;
              if lBGImg.ScrnMM[1] <> 0 then
                 lBGImg.ScrnOri[1] := 1+FindOriMM (0,0,0,lBGImg.ScrnDim[1]-1,0,0, lMatrix)/lBGImg.ScrnMM[1];
              if lBGImg.ScrnMM[2] <> 0 then
                 lBGImg.ScrnOri[2] := 1+FindOriMM (0,0,0,0,lBGImg.ScrnDim[2]-1,0, lMatrix)/lBGImg.ScrnMM[2];
              if lBGImg.ScrnMM[3] <> 0 then
                 lBGImg.ScrnOri[3] := 1+FindOriMM (0,0,0,0,0,lBGImg.ScrnDim[3]-1, lMatrix)/lBGImg.ScrnMM[3];
  *)
           end;
           //end estimate origin
           //fx(lBGImg.ScrnOri[1],lBGImg.ScrnMM[1],lBGImg.ScrnOri[3],1112);
           exit;
     end;
	lPass := 0;
     if (abs(lHdr.Mat.matrix[1,4]) > maxInt) or (abs(lHdr.Mat.matrix[2,4]) > MaxInt) or (abs(lHdr.Mat.matrix[3,4]) > maxint) then begin
        showmessage('Error: the origin is not plausible.');
        lHdr.Mat.matrix[1,4] := 0;
        lHdr.Mat.matrix[2,4] := 0;
        lHdr.Mat.matrix[3,4] := 0;

     end;
121:
  inc(lPass);
  lMatrix := lHdr.Mat;
  FindMatrixPt(0,0,0,lX,lY,lZ,lMatrix);
  lXMax := lX;
  lYMax := lY;
  lZMax := lZ;
  lXMin := lX;
  lYMin := lY;
  lZMin := lZ;
  for lPos := 1 to 7 do begin
	if odd(lPos) then
		lXc := lHdr.NIFTIhdr.Dim[1]-1
	else
		lXc := 0;
	if odd(lPos shr 1) then
		lYc := lHdr.NIFTIhdr.Dim[2]-1
	else
		lYc := 0;
	if odd(lPos shr 2) then
		lZc := lHdr.NIFTIhdr.Dim[3]-1
	else
		lZc := 0;
    	//showmessage(floattostr(lXc)+'  '+floattostr(lYc)+'  '+floattostr(lZc) );
	FindMatrixPt(lXc,lYc,lZc,lX,lY,lZ,lMatrix);
	CheckMaxMin(lX,lY,lZ,lXMax,lYMax,lZMax,lXMin,lYMin,lZMin);
  end;
  //fx(lXMax,lXMin,lZMax,lZMin);
  //next find min MM
  //fx(lZMin,lZMax);
  lmmMin := abs(lHdr.NIFTIhdr.pixdim[1]);
  if abs(lHdr.NIFTIhdr.pixdim[2]) < lmmMin then lmmMin := abs(lHdr.NIFTIhdr.pixdim[2]);
  if abs(lHdr.NIFTIhdr.pixdim[3]) < lmmMin then lmmMin := abs(lHdr.NIFTIhdr.pixdim[3]);
  if lmmMin = 0 then lmmMin := 1;
  //next find max Dim
  lDimMMMax := abs(lXMax-lXMin);
  if abs(lYMax-lYMin) > lDimMMMax then lDimMMMax := abs(lYMax-lYMin);
  if abs(lZMax-lZMin) > lDimMMMax then lDimMMMax := abs(lZMax-lZMin);
  if (1+trunc(lDimMMMax/lmmMin)) > gBGImg.MaxDim then begin
	  //image will be too large if isotropically scalled by smallest mm, try largest mm
	  lmmMin := lHdr.NIFTIhdr.pixdim[1];
	  if lHdr.NIFTIhdr.pixdim[2] > lmmMin then lmmMin := lHdr.NIFTIhdr.pixdim[2];
	  if lHdr.NIFTIhdr.pixdim[3] > lmmMin then lmmMin := lHdr.NIFTIhdr.pixdim[3];
	  if lmmMin = 0 then lmmMin := 1;
	  if (1+trunc(lDimMMMax/lmmMin)) > gBGImg.MaxDim then begin
		//image will be too large if isotropically scalled by largest mm, try isotropic 1mm
		lmmMin :=  1;
	  end;
	  if (1+trunc(lDimMMMax/lmmMin)) > gBGImg.MaxDim then begin
		//image will be too large if isotropically scaled by  1mm, find optimal scaling factor
		lmmMin :=  lDimMMMax/gBGImg.MaxDim;
                Showmessage('Maximum dimension is >'+inttostr(gBGImg.MaxDim)+' voxels. Therefore the image will resolution will be reduced. If you have a fast computer, you may consider increasing the ''MaxDim'' value saved in the mricron.ini file.');
	//showmessage('Warning: having to downsample this large image - you may wish to view this image with MRIcro.');
	  end;
	  //showmessage( floattostr(lmmMin));
	  //lmmMin := 3.5;//
  end;
  lBGImg.ScrnDim[1] := 1+trunc(0.5+((lXMax-lXMin)/lmmMin));//+0.5 May06
  lBGImg.ScrnDim[2] := 1+trunc(0.5+((lYMax-lYMin)/lmmMin));//+0.5 May06
  lBGImg.ScrnDim[3] := 1+trunc(0.5+((lZMax-lZMin)/lmmMin));//+0.5 May06
  //fx(lBGImg.ScrnDim[3],lmmMin);
  lBGImg.ScrnMM[1] := lmmMin;
  lBGImg.ScrnMM[2] := lmmMin;
  lBGImg.ScrnMM[3] := lmmMin;
  //fx(lBGImg.ScrnDim[1],lBGImg.ScrnDim[2],lBGImg.ScrnDim[3]);
  //showmessage(floattostr(lZMin)+'...'+floattostr(lZMax)+'   '+floattostr((lZMin)/lmmMin));
  lBGImg.ScrnOri[1] := -(((lXMin)/lmmMin))+1;
  lBGImg.ScrnOri[2] := -(((lYMin)/lmmMin))+1;
  lBGImg.ScrnOri[3] := -(((lZMin)/lmmMin))+1;

  //fx(lBGImg.ScrnOri[1],lBGImg.ScrnOri[2],lBGImg.ScrnOri[3]);
  if (lXMin > 0) and (lYMin > 0) and (lZMin > 0) and (lPass <= 2) then begin
	lHdr.Mat.matrix[1,4] := -lHdr.Mat.matrix[1,4];
	lHdr.Mat.matrix[2,4] := -lHdr.Mat.matrix[2,4];
	lHdr.Mat.matrix[3,4] := -lHdr.Mat.matrix[3,4];
	{lHdr.NIFTIhdr.srow_x[3] := -lHdr.NIFTIhdr.srow_x[3];
	lHdr.NIFTIhdr.srow_y[3] := -lHdr.NIFTIhdr.srow_y[3];
	lHdr.NIFTIhdr.srow_z[3] := -lHdr.NIFTIhdr.srow_z[3];}
	{lHdr.Mat.matrix[1,4] := 0;
	lHdr.Mat.matrix[2,4] := 0;
	lHdr.Mat.matrix[3,4] := 0; }
	if lPass = 1 then begin
		Showmessage('The origin is not in the image... check your transformation matrix - will attempt to invert offsets');
		goto 121;
	end else if lPass = 2 then begin
                lHdr.Mat.matrix[1,4] := 0;
	        lHdr.Mat.matrix[2,4] := 0;
	        lHdr.Mat.matrix[3,4] := 0;
		Showmessage('The origin is not in the image... check your transformation matrix - will attempt to zero offsets');
		goto 121;
        end else
		showmessage('The origin is not in the image... unable to correct.');
  end;
end;


function mat44_inverse(var R: Tmatrix ) : TMatrix;
var
	r11,r12,r13,r21,r22,r23,r31,r32,r33,v1,v2,v3 , deti : double;
	Q: TMatrix;
begin
   r11 := R.matrix[1,1]; r12 := R.matrix[1,2]; r13 := R.matrix[1,3];  //* [ r11 r12 r13 v1 ] */
   r21 := R.matrix[2,1]; r22 := R.matrix[2,2]; r23 := R.matrix[2,3];  //* [ r21 r22 r23 v2 ] */
   r31 := R.matrix[3,1]; r32 := R.matrix[3,2]; r33 := R.matrix[3,3];  //* [ r31 r32 r33 v3 ] */
   v1  := R.matrix[1,4]; v2  := R.matrix[2,4]; v3  := R.matrix[3,4];  //* [  0   0   0   1 ] */

   deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
		 +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;

   if( deti <> 0.0 ) then
	deti := 1.0 / deti ;

   Q.matrix[1,1] := deti*( r22*r33-r32*r23) ;
   Q.matrix[1,2] := deti*(-r12*r33+r32*r13) ;
   Q.matrix[1,3] := deti*( r12*r23-r22*r13) ;
   Q.matrix[1,4] := deti*(-r12*r23*v3+r12*v2*r33+r22*r13*v3
					 -r22*v1*r33-r32*r13*v2+r32*v1*r23) ;

   Q.matrix[2,1] := deti*(-r21*r33+r31*r23) ;
   Q.matrix[2,2] := deti*( r11*r33-r31*r13) ;
   Q.matrix[2,3] := deti*(-r11*r23+r21*r13) ;
   Q.matrix[2,4] := deti*( r11*r23*v3-r11*v2*r33-r21*r13*v3
					 +r21*v1*r33+r31*r13*v2-r31*v1*r23) ;

   Q.matrix[3,1] := deti*( r21*r32-r31*r22) ;
   Q.matrix[3,2] := deti*(-r11*r32+r31*r12) ;
   Q.matrix[3,3] := deti*( r11*r22-r21*r12) ;
   Q.matrix[3,4] := deti*(-r11*r22*v3+r11*r32*v2+r21*r12*v3
					 -r21*r32*v1-r31*r12*v2+r31*r22*v1) ;

   Q.matrix[4,1] := 0; Q.matrix[4,2] := 0; Q.matrix[4,3] := 0.0 ;
   Q.matrix[4,4] := 1;// (deti == 0.0l) ? 0.0l : 1.0l ; /* failure flag if deti == 0 */

   result :=  Q ;
end;

function TestSameOrtho(var lHdr: TMRIcroHdr): boolean;
var
	lRow,lCol: integer;
begin
	result := false;
	for lRow := 1 to 3 do
		for lCol := 1 to 3 do
                        if (lRow=lCol) then begin
				if lHdr.Mat.Matrix[lRow,lCol] <= 0 then
					exit;
			end else
				if lHdr.Mat.Matrix[lRow,lCol] <> 0 then
					exit;
	result := true;
end;

function OrthoReslice (var lBGImg: TBGImg; var lHdr: TMRIcroHdr): boolean;
label
	666;
Type
 TXImg =  record //Next: analyze Format Header structure
   rDim: array [1..3] of integer;
   rOri,rMM: array [1..3] of single;
   rSliceSz: integer;
 end; //TNIFTIhdr Header Structure
var
 //lStartTime,lEndTime: DWord;
 lIn,lOut: TXImg;
 lBuffIn,lBuffOut,lBuffOutUnaligned:  Bytep;
 lBuffIn16,lBuffOut16 : SmallIntP;
 lBuffIn32,lBuffOut32 : SingleP;
 lX,lY,lZ,lI,lPos,lOutVolItems,lInZPos,lInYPos,lOutZPos,lOutYPos,lInZPosHi,lInYPosHi: integer;
 lXmodLo,lXmodHi,lYmodLo,lYmodHi,lZmodLo,lZmodHi: single;
 lScale,lFloatPos:  single;
 lMin,lMax: array [1..3] of integer;
 lLUTra: array [1..3] of LongIntp;
 lLUTmodRA: array [1..3] of Singlep;
begin
  result := false;
  if not TestSameOrtho(lHdr) then exit;
  for lI := 1 to 3 do begin
	lIn.rDim[lI] := lHdr.NIFTIhdr.dim[lI];
	lIn.rMM[lI] := lHdr.NIFTIhdr.pixdim[lI];
	lIn.rOri[lI] := (abs(lHdr.Mat.Matrix[lI,4]))/abs(lHdr.NIFTIhdr.pixdim[lI])+1;//May07
  end;
  lIn.rSliceSz := lIn.rDim[1]*lIn.rDim[2];
  //Output to background size
  for lI := 1 to 3 do begin
	lOut.rDim[lI] := lBGImg.ScrnDim[lI];
	lOut.rMM[lI] := lBGImg.ScrnMM[lI];
	lOut.rOri[lI] := lBGImg.ScrnOri[lI];
//        fx(lOut.rDim[lI],lOut.rMM[lI],lOut.rOri[lI]);
  end;
  lOut.rSliceSz := lOut.rDim[1]*lOut.rDim[2];
  lOutVolItems := lOut.rSliceSz * lOut.rDim[3]; //InVolSz!
  //find bounding box for overlay, and create lookup tables
  for lI := 1 to 3 do begin
	lScale := lOut.rMM[lI] / lIn.rMM[lI];
	getmem(lLUTra[lI],lOut.rDim[lI]*4);
	getmem(lLUTmodra[lI],lOut.rDim[lI]*4);
	lMin[lI] := maxint;
	lMax[lI] := -1;
	for lPos := 1 to lOut.rDim[lI] do begin
		if lBGImg.OverlaySmooth then begin
			lFloatPos := ((lPos-lOut.rOri[lI]) *lScale)+lIn.rOri[lI] {-0.5};
			lLUTra[lI]^[lPos] := trunc ( lFloatPos  );
			lLUTmodra[lI]^[lPos] := ( frac (lFloatPos ));
		end else begin
			lLUTra[lI]^[lPos] := round ( ((lPos-lOut.rOri[lI]) *lScale)+lIn.rOri[lI]   );
			lLUTmodra[lI]^[lPos] :=0;//not used
		end;
		if (lLUTra[lI]^[lPos] > 0) and  (lMin[lI]=MaxInt) then
			lMin[lI] := lPos;
		if (lLUTra[lI]^[lPos] < lIn.rDim[lI])  {danger! <=} then
			lMax[lI] := lPos;
	end;
  end;

  //for lI := 1 to 3 do fx( lOut.rMM[lI],lIn.rMM[lI]);
  for lI := 1 to 3 do
	if lMin[lI] > lMax[lI] then begin
		showmessage ('Unusual rotation matrix - consider viewing with MRIcro.');//goto 345; //do after previous loop so we are sure all buffers used
		goto 666;
	end;
  lMax[1] := lMax[1] -1;{-1 as we do not want to sample past edge}
  ImgForm.ProgressBar1.Min := lMin[3];
  ImgForm.ProgressBar1.Max := lMax[3];
  //next - core

  if lHdr.ImgBufferBPP = 4 then begin  //next- 32 bit
        lBuffIn32 := SingleP(lHdr.ImgBuffer);
	GetMem(lBuffOutUnaligned,(lOutVolItems*sizeof(single))+16);
	//svn lBuffOut32 := SingleP($fffffff0 and (integer(lBuffOutUnaligned)+15));
        lBuffOut32 := Align(lBuffOutUnaligned, 16);
        for lX := 1 to lOutVolItems do
		lBuffOut32^[lX] := 0; //set all to zero
	//fx(lOutVolItems,lHdr.ImgBufferItems);
	for lZ := lMin[3] to lMax[3] do begin
			ImgForm.ProgressBar1.Position := lZ;
			lOutZPos := (lZ-1) * lOut.rSliceSz;
			lInZPos:= (lLUTra[3]^[lZ]-1) * lIn.rSliceSz;
			lInZPosHi := lInZPos + lIn.rSliceSz;
			lZmodHi := lLUTmodra[3]^[lZ];
			lZModLo := 1 - lZmodHi;
			for lY := lMin[2] to lMax[2] do begin
				lOutYPos := (lY-1) * lOut.rDim[1];
				lInYPos := (lLUTra[2]^[lY]-1) * lIn.rDim[1]; //number of lines
				lInYPosHi := lInYPos + lIn.rDim[1];
				lYmodHi := lLUTmodra[2]^[lY];
				lYModLo := 1 - lYmodHi;
				for lX := lMin[1] to lMax[1] do begin
					lXmodHi := lLUTmodra[1]^[lX];
					lXModLo := 1 - lXmodHi;
					lBuffOut32^[lOutZPos+lOutYPos+lX] := (
						lBuffIn32^[lInZPos+lInYPos+lLUTra[1]^[lX]]*lXModLo*lYModLo*lZModLo +
						lBuffIn32^[lInZPos+lInYPos+lLUTra[1]^[lX]+1]*lXModHi*lYModLo*lZModLo +
						lBuffIn32^[lInZPos+lInYPosHi+lLUTra[1]^[lX]]*lXModLo*lYModHi*lZModLo +
						lBuffIn32^[lInZPos+lInYPosHi+lLUTra[1]^[lX]+1]*lXModHi*lYModHi*lZModLo +
						lBuffIn32^[lInZPosHi+lInYPos+lLUTra[1]^[lX]]*lXModLo*lYModLo*lZModHi +
						lBuffIn32^[lInZPosHi+lInYPos+lLUTra[1]^[lX]+1]*lXModHi*lYModLo*lZModHi +
						lBuffIn32^[lInZPosHi+lInYPosHi+lLUTra[1]^[lX]]*lXModLo*lYModHi*lZModHi +
						lBuffIn32^[lInZPosHi+lInYPosHi+lLUTra[1]^[lX]+1]*lXModHi*lYModHi*lZModHi) ;
				end; //for X
			end; //for Y
	end; //for Z

	FreeMem(lHdr.ImgBufferUnaligned);
	GetMem(lHdr.ImgBufferUnaligned ,(lOutVolItems*sizeof(Single)) + 16);
	//svn lHdr.ImgBuffer := ByteP ($fffffff0 and (integer(lHdr.ImgBufferUnaligned )+15));
        lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16);
        lHdr.ImgBufferItems := lOutVolItems;
        Move(lBuffOut32^,lHdr.ImgBuffer^,lOutVolItems*sizeof(Single));//source/dest
        //678 winOnly-> CopyMemory(Pointer(lHdr.ImgBuffer),Pointer(lBuffOut32),(lOutVolItems*sizeof(Single)));
	FreeMem(lBuffOutUnaligned);
  end else if lHdr.ImgBufferBPP = 2 then begin  //next- 16 bit
	lBuffIn16 := SmallIntP(lHdr.ImgBuffer);
	GetMem(lBuffOutUnaligned,(lOutVolItems*sizeof(smallint))+16);
	//svn lBuffOut16 := SmallIntP($fffffff0 and (integer(lBuffOutUnaligned)+15));
        lBuffOut16 := align(lBuffOutUnaligned, 16);
        for lX := 1 to lOutVolItems do
		lBuffOut16^[lX] := 0; //set all to zero
	for lZ := lMin[3] to lMax[3] do begin
			ImgForm.ProgressBar1.Position := lZ;
			lOutZPos := (lZ-1) * lOut.rSliceSz;
			lInZPos:= (lLUTra[3]^[lZ]-1) * lIn.rSliceSz;
			lInZPosHi := lInZPos + lIn.rSliceSz;
			lZmodHi := lLUTmodra[3]^[lZ];
			lZModLo := 1 - lZmodHi;
			for lY := lMin[2] to lMax[2] do begin
				lOutYPos := (lY-1) * lOut.rDim[1];
				lInYPos := (lLUTra[2]^[lY]-1) * lIn.rDim[1]; //number of lines
				lInYPosHi := lInYPos + lIn.rDim[1];
				lYmodHi := lLUTmodra[2]^[lY];
				lYModLo := 1 - lYmodHi;
				for lX := lMin[1] to lMax[1] do begin
					lXmodHi := lLUTmodra[1]^[lX];
					lXModLo := 1 - lXmodHi;
					lBuffOut16^[lOutZPos+lOutYPos+lX] := round(
						lBuffIn16^[lInZPos+lInYPos+lLUTra[1]^[lX]]*lXModLo*lYModLo*lZModLo +
						lBuffIn16^[lInZPos+lInYPos+lLUTra[1]^[lX+1]]*lXModHi*lYModLo*lZModLo +
						lBuffIn16^[lInZPos+lInYPosHi+lLUTra[1]^[lX]]*lXModLo*lYModHi*lZModLo +
						lBuffIn16^[lInZPos+lInYPosHi+lLUTra[1]^[lX+1]]*lXModHi*lYModHi*lZModLo +
						lBuffIn16^[lInZPosHi+lInYPos+lLUTra[1]^[lX]]*lXModLo*lYModLo*lZModHi +
						lBuffIn16^[lInZPosHi+lInYPos+lLUTra[1]^[lX+1]]*lXModHi*lYModLo*lZModHi +
						lBuffIn16^[lInZPosHi+lInYPosHi+lLUTra[1]^[lX]]*lXModLo*lYModHi*lZModHi +
						lBuffIn16^[lInZPosHi+lInYPosHi+lLUTra[1]^[lX+1]]*lXModHi*lYModHi*lZModHi) ;
				end; //for X
			end; //for Y
	end; //for Z
	FreeMem(lHdr.ImgBufferUnaligned);
	GetMem(lHdr.ImgBufferUnaligned ,(lOutVolItems*sizeof(SmallInt)) + 16);
	//lHdr.ImgBuffer := ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
        lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16);
        lHdr.ImgBufferItems := lOutVolItems;
        Move((lBuffOut16^),(lHdr.ImgBuffer^),lOutVolItems*sizeof(SmallInt));//source/dest
	//678 winOnly-> CopyMemory(Pointer(lHdr.ImgBuffer),Pointer(lBuffOut16),(lOutVolItems*sizeof(SmallInt)));
	FreeMem(lBuffOutUnaligned);
  end else if lHdr.ImgBufferBPP = 1 then begin  //next- 8 bit
	lBuffIn := lHdr.ImgBuffer;
	GetMem(lBuffOut,lOutVolItems);
        Fillchar(lBuffOut^,lOutVolItems,0); //set all to zero
	//for lI := 1 to lOutVolItems do lBuffOut[lI] := 0; //set all to zero
	for lZ := lMin[3] to lMax[3] do begin
			ImgForm.ProgressBar1.Position := lZ;
			lOutZPos := (lZ-1) * lOut.rSliceSz;
			lInZPos:= (lLUTra[3]^[lZ]-1) * lIn.rSliceSz;
			lInZPosHi := lInZPos + lIn.rSliceSz;
			lZmodHi := lLUTmodra[3]^[lZ];
			lZModLo := 1 - lZmodHi;
			for lY := lMin[2] to lMax[2] do begin
				lOutYPos := (lY-1) * lOut.rDim[1];
				lInYPos := (lLUTra[2]^[lY]-1) * lIn.rDim[1]; //number of lines
				lInYPosHi := lInYPos + lIn.rDim[1];
				lYmodHi := lLUTmodra[2]^[lY];
				lYModLo := 1 - lYmodHi;
				for lX := lMin[1] to lMax[1] do begin
					lXmodHi := lLUTmodra[1]^[lX];
					lXModLo := 1 - lXmodHi;
					lBuffOut^[lOutZPos+lOutYPos+lX] := round(
						lBuffIn^[lInZPos+lInYPos+lLUTra[1]^[lX]]*lXModLo*lYModLo*lZModLo +
						lBuffIn^[lInZPos+lInYPos+lLUTra[1]^[lX+1]]*lXModHi*lYModLo*lZModLo +
						lBuffIn^[lInZPos+lInYPosHi+lLUTra[1]^[lX]]*lXModLo*lYModHi*lZModLo +
						lBuffIn^[lInZPos+lInYPosHi+lLUTra[1]^[lX+1]]*lXModHi*lYModHi*lZModLo +
						lBuffIn^[lInZPosHi+lInYPos+lLUTra[1]^[lX]]*lXModLo*lYModLo*lZModHi +
						lBuffIn^[lInZPosHi+lInYPos+lLUTra[1]^[lX+1]]*lXModHi*lYModLo*lZModHi +
						lBuffIn^[lInZPosHi+lInYPosHi+lLUTra[1]^[lX]]*lXModLo*lYModHi*lZModHi +
						lBuffIn^[lInZPosHi+lInYPosHi+lLUTra[1]^[lX+1]]*lXModHi*lYModHi*lZModHi);
				end; //for X
			end; //for Y
	end; //for Z
	FreeMem(lHdr.ImgBufferUnaligned);
	GetMem(lHdr.ImgBufferUnaligned ,lOutVolItems + 16);
	//svn lHdr.ImgBuffer := ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
        lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16);
        lHdr.ImgBufferItems := lOutVolItems;
        Move(lBuffOut^,lHdr.ImgBuffer^,lOutVolItems);//source/dest
        //678winonly-> CopyMemory((lHdr.ImgBuffer),(lBuffOut),lOutVolItems);
	FreeMem(lBuffOut);
  end else
	Showmessage('Unsupported BPP '+inttostr(lHdr.ImgBufferBPP));
  ImgForm.ProgressBar1.Position := lMin[3];
  result := true;

666:
 for lI := 1 to 3 do begin
	freemem(lLUTra[lI]);
	freemem(lLUTmodra[lI]);
 end;
  //Output dimensions: size of background image
  //lEndTime := GetTickCount;
  //ImgForm.Label1.caption :=('update(ms): '+inttostr(lEndTime-lStartTime));
end; //procedure OrthogonalResliceImg

procedure fSwap(var lX,lY: single);
var
	lSwap: single;
begin
	lSwap := lX;
	lX := lY;
	lY := lSwap;
end;

procedure ResliceScrnImg (var lBGImg: TBGImg; var lHdr: TMRIcroHdr; lTrilinearSmooth: boolean); var
 lOverlap: boolean;
 lMinY,lMinZ,lMaxY,lMaxZ: integer; //<- used by trilinear
 lXreal,lYreal,lZreal,lXrM1,lYrM1,lZrM1, //<- used by trilinear
 lZr,lYr,lXr,lZx,lZy,lZz,lYx,lYy,lYz: single;//lSwap
 lZ,lY,lX,lOutVolItems,lInVolItems,
 lXdimIn,lYDimIn,lZDimIn,lInSliceSz,
 lOutPos,lOutDimX,lOutDimY,lOutDimZ,lXo,lYo,lZo: integer;//lSrcPos
 lXxp,lXyp,lXzp: Pointer;
 lXxra,lXyra,lXzra : SingleP;
 lMatrix,lMatrixBG: TMatrix;
 lBuffIn,lBuffOut,lBuffOutUnaligned:  Bytep;
 lBuffIn16,lBuffOut16 : SmallIntP;//16bit
 lBuffIn32,lBuffOut32: SingleP;
 begin
  //x imgform.LabelY.caption :='-';
  if SameAsBG(lBGImg,lHdr) then exit;
  //x imgform.LabelY.caption :='+';
  if not lBGImg.Resliced then begin //2008
    Reslice_Img_To_Unaligned (gMRIcroOverlay[kBGOverlayNum].NIftiHdr, lHdr, lBGImg.OverlaySmooth);
    exit;
  end;
  if OrthoReslice(lBGImg,lHdr) then exit;
  lOverlap := false;
  lMatrix := lHdr.Mat;
  lMatrix := mat44_inverse(lMatrix);
  lMatrixBG := Matrix3D ( lBGImg.Scrnmm[1],0,0,0, 0,lBGImg.Scrnmm[2],0,0,  0,0,lBGImg.Scrnmm[3],0,  0,0,0,1);
  lMatrix.size := size3D;
  lMatrix := MultiplyMatrices(lMatrix,lMatrixBG);
  lXdimIn := lHdr.NiftiHdr.dim[1];
  lYdimIn := lHdr.NiftiHdr.dim[2];
  lZDimIn := lHdr.NiftiHdr.dim[3];
  lInSliceSz := lHdr.NiftiHdr.dim[1]*lHdr.NiftiHdr.dim[2];
  lInVolItems := lInSliceSz*lHdr.NiftiHdr.dim[3];
  if  (lHdr.ImgBufferItems < lInVolItems) then
	exit;
  lBuffIn := lHdr.ImgBuffer;
  lOutDimX := lBGImg.ScrnDim[1];
  lOutDimY := lBGImg.ScrnDim[2];
  lOutDimZ := lBGImg.ScrnDim[3];
  //lOutSliceSz :=  lOutDimX*lOutDimY;
  lOutVolItems := lBGImg.ScrnDim[1]*lBGImg.ScrnDim[2]*lBGImg.ScrnDim[3];
  lOutPos := 0;
  //start look up table...
  GetMem(lXxp, (sizeof(single)* lOutDimX)+16);
  GetMem(lXyp, (sizeof(single)* lOutDimX)+16);
  GetMem(lXzp, (sizeof(single)* lOutDimX)+16);
  lXxRA := align(lXxp, 16);  //SingleP($fffffff0 and (integer(lXxP)+15)); //data aligned to quad-word boundary
  lXyRA := align(lXyp, 16);//SingleP($fffffff0 and (integer(lXyP)+15)); //quad-word boundary
  lXzRA := align(lXzp, 16);//SingleP($fffffff0 and (integer(lXzP)+15)); //quad-word boundary
  for lX := 1 to  lOutDimX do begin
			lXr := lX-(lBGImg.ScrnOri[1]);//* lBGImg.ScrnMM[1]) ;
			//lXr := lX;
			lXxRA^[lX] :=  lXr*lMatrix.matrix[1,1]+1;
			lXyRA^[lX] :=  lXr*lMatrix.matrix[2,1]+1;
			lXzRA^[lX] :=  lXr*lMatrix.matrix[3,1]+1;
  end;
  //end look up table
if  lTrilinearSmooth then begin //smooth data
  if lHdr.ImgBufferBPP = 4 then begin
	lBuffIn32 := SingleP(lHdr.ImgBuffer);
	GetMem(lBuffOutUnaligned,(lOutVolItems*sizeof(single))+16);
	lBuffOut32 := align(lBuffOutUnaligned, 16);  //SingleP($fffffff0 and (integer(lBuffOutUnaligned)+15));
	for lX := 1 to lOutVolItems do
		lBuffOut32^[lX] := 0; //set all to zero
	for lZ := 1 to lOutDimZ do begin
	  lZr := lZ -(lBGImg.ScrnOri[3]);
	  lZx := lZr*lMatrix.matrix[1,3]+lMatrix.matrix[1,4];
	  lZy := lZr*lMatrix.matrix[2,3]+lMatrix.matrix[2,4];
	  lZz := lZr*lMatrix.matrix[3,3]+lMatrix.matrix[3,4];
	  for lY := 1  to lOutDimY do begin
		lYr := lY -(lBGImg.ScrnOri[2]);
		lYx := lYr*lMatrix.matrix[1,2];
		lYy := lYr*lMatrix.matrix[2,2];
		lYz := lYr*lMatrix.matrix[3,2];
		for lX := 1 to lOutDimX do begin
			inc(lOutPos);
			lXreal := lXxRA^[lX]+lYx+lZx;
			lYreal := lXyRA^[lX]+lYy+lZy;
			lZreal := lXzRA^[lX]+lYz+lZz;
			lXo := trunc(lXreal);
			lYo := trunc(lYreal);
			lZo := trunc(lZreal);
			if (lXo > 0) and (lXo < lXDimIn)
			  and (lYo > 0) and (lYo < lYDimIn) and
			  (lZo > 0) and (lZo < lZDimIn) then begin
			   lXreal := lXreal-lXo;
			   lYreal := lYreal-lYo;
			   lZreal := lZreal-lZo;
			   lXrM1 := 1-lXreal;
			   lYrM1 := 1-lYreal;
			   lZrM1 := 1-lZreal;
			   lMinY := ((lYo-1)*lXdimIn);
			   lMinZ := ((lZo-1)*lInSliceSz);
			   lMaxY := ((lYo)*lXdimIn);
			   lMaxZ := ((lZo)*lInSliceSz);
			   lOverlap := true;
			   lBuffOut32^[lOutPos] :=  (
			   {all min} ( (lXrM1*lYrM1*lZrM1)*lBuffIn32^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*lBuffIn32^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*lBuffIn32^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*lBuffIn32^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*lBuffIn32^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*lBuffIn32^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*lBuffIn32^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*lBuffIn32^[lXo+1+lMaxY+lMaxZ]) );
			  end; //values in range
			end; //for X
		end; //for OutY
	end; //for OutZ
	//core 32 end
	FreeMem(lHdr.ImgBufferUnaligned);
	GetMem(lHdr.ImgBufferUnaligned ,(lOutVolItems*sizeof(Single)) + 16);
	lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16);//ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
	lHdr.ImgBufferItems := lOutVolItems;
        Move(lBuffOut32^,lHdr.ImgBuffer^,lOutVolItems*sizeof(Single));//source/dest
	FreeMem(lBuffOutUnaligned);
  end else if lHdr.ImgBufferBPP = 2 then begin
	lBuffIn16 := SmallIntP(lHdr.ImgBuffer);
	GetMem(lBuffOutUnaligned,(lOutVolItems*sizeof(smallint))+16);
	lBuffOut16 := align(lBuffOutUnaligned, 16); //SmallIntP($fffffff0 and (integer(lBuffOutUnaligned)+15));
	for lX := 1 to lOutVolItems do
		lBuffOut16^[lX] := 0; //set all to zero
	//core 16 start
	for lZ := 1 to lOutDimZ do begin
	  lZr := lZ -(lBGImg.ScrnOri[3]);
	  lZx := lZr*lMatrix.matrix[1,3]+lMatrix.matrix[1,4];
	  lZy := lZr*lMatrix.matrix[2,3]+lMatrix.matrix[2,4];
	  lZz := lZr*lMatrix.matrix[3,3]+lMatrix.matrix[3,4];
	  for lY := 1  to lOutDimY do begin
		lYr := lY -(lBGImg.ScrnOri[2]);
		lYx := lYr*lMatrix.matrix[1,2];
		lYy := lYr*lMatrix.matrix[2,2];
		lYz := lYr*lMatrix.matrix[3,2];
		for lX := 1 to lOutDimX do begin
			inc(lOutPos);
			lXreal := lXxRA^[lX]+lYx+lZx;
			lYreal := lXyRA^[lX]+lYy+lZy;
			lZreal := lXzRA^[lX]+lYz+lZz;
			lXo := trunc(lXreal);
			lYo := trunc(lYreal);
			lZo := trunc(lZreal);
			if (lXo > 0) and (lXo < lXDimIn)
			  and (lYo > 0) and (lYo < lYDimIn) and
			  (lZo > 0) and (lZo < lZDimIn) then begin
			   lXreal := lXreal-lXo;
			   lXrM1 := 1-lXreal;
			   lYreal := lYreal-lYo;
			   lYrM1 := 1-lYreal;
			   lZreal := lZreal-lZo;
			   lZrM1 := 1-lZreal;
			   lMinY := ((lYo-1)*lXdimIn);
			   lMaxY := lMinY+lXdimIn;
			   lMinZ := ((lZo-1)*lInSliceSz);
			   lMaxZ := lMinZ+lInSliceSz;
			   lOverlap := true;
			   lBuffOut16^[lOutPos] := round (
			   {all min} ( (lXrM1*lYrM1*lZrM1)*lBuffIn16^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*lBuffIn16^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*lBuffIn16^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*lBuffIn16^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*lBuffIn16^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*lBuffIn16^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*lBuffIn16^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*lBuffIn16^[lXo+1+lMaxY+lMaxZ]) ); (**)
			  end; //values in range
			end; //for X
		end; //for OutY
	end; //for OutZ
	//core 16 end
	FreeMem(lHdr.ImgBufferUnaligned);
	GetMem(lHdr.ImgBufferUnaligned ,(lOutVolItems*sizeof(SmallInt)) + 16);
	lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16); //ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
	lHdr.ImgBufferItems := lOutVolItems;
        Move(lBuffOut16^,lHdr.ImgBuffer^,lOutVolItems*sizeof(SmallInt));//source/dest
	FreeMem(lBuffOutUnaligned);
  end else if lHdr.ImgBufferBPP = 1 then begin
	GetMem(lBuffOut,lOutVolItems);
	Fillchar(lBuffOut^,lOutVolItems,0); //set all to zero
	for lZ := 1 to lOutDimZ do begin
	  lZr := lZ -(lBGImg.ScrnOri[3]);
	  lZx := lZr*lMatrix.matrix[1,3]+lMatrix.matrix[1,4];
	  lZy := lZr*lMatrix.matrix[2,3]+lMatrix.matrix[2,4];
	  lZz := lZr*lMatrix.matrix[3,3]+lMatrix.matrix[3,4];
	  for lY := 1  to lOutDimY do begin
		lYr := lY -(lBGImg.ScrnOri[2]);
		lYx := lYr*lMatrix.matrix[1,2];
		lYy := lYr*lMatrix.matrix[2,2];
		lYz := lYr*lMatrix.matrix[3,2];
		for lX := 1 to lOutDimX do begin
			inc(lOutPos);
			lXreal := lXxRA^[lX]+lYx+lZx;
			lYreal := lXyRA^[lX]+lYy+lZy;
			lZreal := lXzRA^[lX]+lYz+lZz;
			lXo := trunc(lXreal);
			lYo := trunc(lYreal);
			lZo := trunc(lZreal);
			if (lXo > 0) and (lXo < lXDimIn)
			  and (lYo > 0) and (lYo < lYDimIn) and
			  (lZo > 0) and (lZo < lZDimIn) then begin
			   lXreal := lXreal-lXo;
			   lYreal := lYreal-lYo;
			   lZreal := lZreal-lZo;
			   lXrM1 := 1-lXreal;
			   lYrM1 := 1-lYreal;
			   lZrM1 := 1-lZreal;
			   lMinY := ((lYo-1)*lXdimIn);
			   lMinZ := ((lZo-1)*lInSliceSz);
			   lMaxY := ((lYo)*lXdimIn);
			   lMaxZ := ((lZo)*lInSliceSz);
			   lOverlap := true;
			   lBuffOut^[lOutPos] := round (
			   {all min} ( (lXrM1*lYrM1*lZrM1)*lBuffIn^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*lBuffIn^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*lBuffIn^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*lBuffIn^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*lBuffIn^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*lBuffIn^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*lBuffIn^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*lBuffIn^[lXo+1+lMaxY+lMaxZ]) );
			  end; //values in range
			end; //for X
		end; //for OutY
	end; //for OutZ
	FreeMem(lHdr.ImgBufferUnaligned);
	GetMem(lHdr.ImgBufferUnaligned ,lOutVolItems + 16);
	lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16); //ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
	lHdr.ImgBufferItems := lOutVolItems;
        Move(lBuffOut^,lHdr.ImgBuffer^,lOutVolItems);//source/dest
	FreeMem(lBuffOut);
 end else //unsupported bits-per-pixel dataformat
	Showmessage('Unsupported BPP ='+inttostr(lHdr.ImgBufferBPP) );
end else begin //not trilinear - use nearest neighbor
         //start nearest neighbor
  if lHdr.ImgBufferBPP = 4 then begin
	lBuffIn32 := SingleP(lHdr.ImgBuffer);
	GetMem(lBuffOutUnaligned,(lOutVolItems*sizeof(single))+16);
	lBuffOut32 := align(lBuffOutUnaligned, 16);//SingleP($fffffff0 and (integer(lBuffOutUnaligned)+15));
	for lX := 1 to lOutVolItems do
		lBuffOut32^[lX] := 0; //set all to zero
	//core 32 start
	for lZ := 1 to lOutDimZ do begin
	  lZr := lZ -(lBGImg.ScrnOri[3]);
	  lZx := lZr*lMatrix.matrix[1,3]+lMatrix.matrix[1,4];
	  lZy := lZr*lMatrix.matrix[2,3]+lMatrix.matrix[2,4];
	  lZz := lZr*lMatrix.matrix[3,3]+lMatrix.matrix[3,4];
	  for lY := 1  to lOutDimY do begin
		lYr := lY -(lBGImg.ScrnOri[2]);
		lYx := lYr*lMatrix.matrix[1,2];
		lYy := lYr*lMatrix.matrix[2,2];
		lYz := lYr*lMatrix.matrix[3,2];
		for lX := 1 to lOutDimX do begin
			inc(lOutPos);
			lXo := round(lXxRA^[lX]+lYx+lZx);
			lYo := round(lXyRA^[lX]+lYy+lZy);
			lZo := round(lXzRA^[lX]+lYz+lZz);
			if (lXo > 0) and (lXo < lXDimIn)
			  and (lYo > 0) and (lYo < lYDimIn) and
			  (lZo > 0) and (lZo < lZDimIn) then begin
         lOverlap := true;
			   lMinY := ((lYo-1)*lXdimIn);
			   lMinZ := ((lZo-1)*lInSliceSz);
			   lBuffOut32^[lOutPos] :=  lBuffIn32^[lXo+lMinY+lMinZ];
        end;
			end; //for X
		end; //for OutY
	end; //for OutZ
	//core 32 end
	FreeMem(lHdr.ImgBufferUnaligned);
	GetMem(lHdr.ImgBufferUnaligned ,(lOutVolItems*sizeof(Single)) + 16);
	lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16);//ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
	lHdr.ImgBufferItems := lOutVolItems;
        Move(lBuffOut32^,lHdr.ImgBuffer^,lOutVolItems*sizeof(Single));//source/dest
	FreeMem(lBuffOutUnaligned);
  end else if lHdr.ImgBufferBPP = 2 then begin
	lBuffIn16 := SmallIntP(lHdr.ImgBuffer);
	GetMem(lBuffOutUnaligned,(lOutVolItems*sizeof(smallint))+16);
	lBuffOut16 := align(lBuffOutUnaligned, 16);//SmallIntP($fffffff0 and (integer(lBuffOutUnaligned)+15));
	for lX := 1 to lOutVolItems do
		lBuffOut16^[lX] := 0; //set all to zero
	//core 16 start
	for lZ := 1 to lOutDimZ do begin
	  lZr := lZ -(lBGImg.ScrnOri[3]);
	  lZx := lZr*lMatrix.matrix[1,3]+lMatrix.matrix[1,4];
	  lZy := lZr*lMatrix.matrix[2,3]+lMatrix.matrix[2,4];
	  lZz := lZr*lMatrix.matrix[3,3]+lMatrix.matrix[3,4];
	  for lY := 1  to lOutDimY do begin
		lYr := lY -(lBGImg.ScrnOri[2]);
		lYx := lYr*lMatrix.matrix[1,2];
		lYy := lYr*lMatrix.matrix[2,2];
		lYz := lYr*lMatrix.matrix[3,2];
		for lX := 1 to lOutDimX do begin
			inc(lOutPos);
			lXo := round(lXxRA^[lX]+lYx+lZx);
			lYo := round(lXyRA^[lX]+lYy+lZy);
			lZo := round(lXzRA^[lX]+lYz+lZz);
			if (lXo > 0) and (lXo < lXDimIn)
			  and (lYo > 0) and (lYo < lYDimIn) and
			  (lZo > 0) and (lZo < lZDimIn) then begin
			   lOverlap := true;
			   lMinY := ((lYo-1)*lXdimIn);
			   lMinZ := ((lZo-1)*lInSliceSz);
			   lBuffOut16^[lOutPos] := lBuffIn16^[lXo+lMinY+lMinZ]//lBuffIn16[lXo+lYo+lZo]; xxxx

			  end; //values in range
			end; //for X
		end; //for OutY
	end; //for OutZ
	//core 16 end
	FreeMem(lHdr.ImgBufferUnaligned);
	GetMem(lHdr.ImgBufferUnaligned ,(lOutVolItems*sizeof(SmallInt)) + 16);
	lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16);// ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
	lHdr.ImgBufferItems := lOutVolItems;
        Move(lBuffOut16^,lHdr.ImgBuffer^,lOutVolItems*sizeof(SmallInt));//source/dest
	FreeMem(lBuffOutUnaligned);
  end else if lHdr.ImgBufferBPP = 1 then begin
	GetMem(lBuffOut,lOutVolItems);
	Fillchar(lBuffOut^,lOutVolItems,0); //set all to zero
	for lZ := 1 to lOutDimZ do begin
	  lZr := lZ -(lBGImg.ScrnOri[3]);
	  lZx := lZr*lMatrix.matrix[1,3]+lMatrix.matrix[1,4];
	  lZy := lZr*lMatrix.matrix[2,3]+lMatrix.matrix[2,4];
	  lZz := lZr*lMatrix.matrix[3,3]+lMatrix.matrix[3,4];
	  for lY := 1  to lOutDimY do begin
		lYr := lY -(lBGImg.ScrnOri[2]);
		lYx := lYr*lMatrix.matrix[1,2];
		lYy := lYr*lMatrix.matrix[2,2];
		lYz := lYr*lMatrix.matrix[3,2];
		for lX := 1 to lOutDimX do begin
			inc(lOutPos);
			lXo := round(lXxRA^[lX]+lYx+lZx);
			lYo := round(lXyRA^[lX]+lYy+lZy);
			lZo := round(lXzRA^[lX]+lYz+lZz);
			if (lXo > 0) and (lXo < lXDimIn)
			  and (lYo > 0) and (lYo < lYDimIn) and
			  (lZo > 0) and (lZo < lZDimIn) then begin
			   lMinY := ((lYo-1)*lXdimIn);
			   lMinZ := ((lZo-1)*lInSliceSz);
			   lOverlap := true;
			   lBuffOut^[lOutPos] := lBuffIn^[lXo+lMinY+lMinZ];
			  end; //values in range
			end; //for X
		end; //for OutY
	end; //for OutZ
	FreeMem(lHdr.ImgBufferUnaligned);
	GetMem(lHdr.ImgBufferUnaligned ,lOutVolItems + 16);
	lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16);//ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
	lHdr.ImgBufferItems := lOutVolItems;
        Move(lBuffOut^,lHdr.ImgBuffer^,lOutVolItems);//source/dest
	FreeMem(lBuffOut);
 end else //unsupported bits-per-pixel dataformat
	Showmessage('Unsupported BPP ='+inttostr(lHdr.ImgBufferBPP) ); //end nearest neighbor
end; //end if trilinear else nearest neighbor
if not lOverlap then
	showmessage('No overlap between image and background bounding box - check the transfomation matrices.');
  FreeMem(lXxp);
  FreeMem(lXyp);
  FreeMem(lXzp);
end; //ResliceScrnImg


procedure InvertScrnBuffer(var lHdr: TMRIcroHdr);
var lPos: integer;
begin
	 if lHdr.ScrnBufferItems < 1 then exit;
	 lHdr.Zero8Bit := lHdr.Zero8Bit+(255*lHdr.Slope8bit);
	 lHdr.Slope8bit := -lHdr.Slope8bit;
	 for lPos := 1 to lHdr.ScrnBufferItems  do
			lHdr.ScrnBuffer^[lPos] := 255- lHdr.ScrnBuffer^[lPos];
	 {lMin := 255;
	 for lPos := 1 to lHdr.ScrnBufferItems  do
			if lMin > lHdr.ScrnBuffer[lPos] then lMin := lHdr.ScrnBuffer[lPos];
	  }
	 //showmessage('inv'+inttostr(lMin));
end;

const
	kMin8bit = 1;

procedure RescaleImgIntensity(var lBackgroundImg: TBGImg; var lHdr: TMRIcroHdr; lLayer: integer );
var
   lImgSamples: integer;
   //lFiltMin8bit,lFiltMax8bit: integer;
   //lMin,lMax: single;
begin
  lImgSamples := round(ComputeImageDataBytes8bpp(lHdr));
  if (lHdr.ImgBufferItems = 0) and (lHdr.ScrnBufferItems > 0) then begin  //image buffer loaded - not VOIs have screen but not img buffers
	  if lBackgroundImg.VOImirrored  then
		MirrorScrnBuffer(lBackgroundImg,lHdr);
	  lBackgroundImg.VOImirrored := false;
	  exit;
  end;
  if lHdr.ImgBufferItems<>lHdr.ScrnBufferItems then begin
	if lHdr.ScrnBufferItems > 0 then
		freemem(lHdr.ScrnBuffer);
	lHdr.ScrnBufferItems := lHdr.ImgBufferItems;
	GetMem(lHdr.ScrnBuffer ,lHdr.ScrnBufferItems);
  end;
  if lHdr.ImgBufferItems = 0 then
    exit;  //2/2010
  if (lHdr.UsesCustomPalette) and (not lHdr.UsesCustomPaletteRandomRainbow) then begin  //2014
	  lHdr.WindowScaledMin := kMin8bit;
	  lHdr.WindowScaledMax := 255;
  end;

  if lImgSamples < 1 then
	exit;
  if (lHdr.ImgBufferBPP  = 4) then
	RescaleImgIntensity32(lHdr)
  else if (lHdr.ImgBufferBPP  = 2) then
	RescaleImgIntensity16(LHdr)
  else if lHdr.ImgBufferBPP  = 1 then
	 RescaleImgIntensity8(lHdr)
  else begin
	showmessage(inttostr(lHdr.ImgBufferItems)+'Unknown Image Buffer Bytes Per Pixel: '+inttostr(lHdr.ImgBufferBPP)+' : '+lHdr.HdrFileName);
	exit;
  end;

  //if not lHdr.SameDimsAsBG then OrthogonalResliceScrnImg (lBackgroundImg, lHdr);
  //ReturnRawMinMax (lHdr, lMin,lMax,lFiltMin8bit,lFiltMax8bit);
 if (lLayer <> kBGOverlayNum) and ((lHdr.WindowScaledMin <= 0) and (lHdr.WindowScaledMax <= 0)) then
		InvertScrnBuffer(lHdr);
  FilterScrnImg (lHdr);//,lFiltMin8bit,lFiltMax8bit);

  if lBackgroundImg.Mirror  then
		MirrorScrnBuffer(lBackgroundImg,lHdr);
end; //RescaleImgIntensity32

function PtoLog10 ( lIn: double): double; //in= pvalue <=1
begin
	//result := -log(abs(lIn),10)
	result := -log((lIn),10)
end;

function Log10toP (lIn: double): double;
begin
	 //result := log((lIn),10)
	 result := 1/power(10,lIn); //requires Math unit
end;

procedure ComputeFDR (var lInHdr: TMRIcroHdr; var lP05,lP01,lFWE05,lFWE01,lFDR05,lFDR01: single);
//(lImg2Load.NIFTIhdr.intent_code,round(lImg2Load.NIFTIhdr.intent_p1),lImg2Load.ImgBufferItems,lImg2Load.ImgBufferBPP,lImg2Load.ImgBuffer,lP05,lP01,lFWE05,lFWE01,lFDR05,lFDR01);
//procedure ComputeFDR(lStatIntent,lDF,lImgSamples,lImgBPP: integer; l32Buf:SingleP; var lP05,lP01,lFWE05,lFWE01,lFDR05,lFDR01: single);
//StatIntents in kNIFTI_INTENT_CHISQ, kNIFTI_INTENT_ZSCORE,kNIFTI_INTENT_TTEST
//Note DF meaningless for ZScore
label 555;
var
	lPs: SingleP; //array of tests
	lStr: string;
	lStatIntent,lImgSamples,lnTests,lInc,lDF: integer;
	lPrevP,lP,lFDR05p, lFDR01p,lnegFDR05p, lnegFDR01p,lnegFDR05, lnegFDR01 : double;
	l32Buf : SingleP;
begin

 lStatIntent := lInHdr.NIFTIhdr.intent_code;
	lDF := round(lInHdr.NIFTIhdr.intent_p1);
         if ((lStatIntent = kNIFTI_INTENT_CHISQ) or (lStatIntent = kNIFTI_INTENT_TTEST)) and (lDF <= 1) then  //May07
            lDF := ReadIntForm.GetInt('Please specify degrees of freedom for '+extractfilename(lInHdr.HdrFileName),1,16,32000);
	lImgSamples := lInHdr.ImgBufferItems;
	if (lImgSamples < 1) then exit;
	ImgForm.StatusLabel.Caption := 'Computing FDR rates...';
	ImgForm.refresh;
	//next: count number of tests [we could just rely on value lChiSamples to us, but perhaps value in intention is not correct
	lnTests := 0;

	l32Buf := SingleP(lInHdr.ImgBuffer );
	for lInc := 1 to lImgSamples do
		if l32Buf^[lInc] <> 0 then
			inc(lnTests);

	if lnTests < 1 then exit;
	GetMem(lPs,lnTests*sizeof(single));
	//for lInc := 1 to lnTests do lPs[lInc] := 1;
	//next - place Pvalues in array, as computing P is slow, we remember last Pvalue
	lPrevP := 0;
	lnTests :=  0;
	lP := 1; //never used
	//lStartTime := GetTickCount;
	for lInc := 1 to lImgSamples do
		if l32Buf^[lInc] <> 0 then begin
			inc(lnTests);
			if l32Buf^[lInc] <> lPrevP then
				case lStatIntent of
					kNIFTI_INTENT_TTEST: lP := pTdistr(lDF,l32Buf^[lInc]);//slow!! 110ms
					kNIFTI_INTENT_ZSCORE: lP := pNormal(l32Buf^[lInc]);//slow!! 94ms
					kNIFTI_INTENT_PVAL: lP := l32Buf^[lInc];
					NIFTI_INTENT_LOG10PVAL: lP := Log10toP(l32Buf^[lInc]);
					else {kNIFTI_INTENT_CHISQ:}begin
						if l32Buf^[lInc] < 0 then //MRIcro saves negative Chi
							lP := 0.6
						else
							lP := pChi2(lDF,l32Buf^[lInc]);//slow! 47ms
					end;
				end;
			lPs^[lnTests] := lP;
			lPrevP := l32Buf^[lInc];
		end;  //Chi <> 0
	//ImgForm.caption :=('update(ms): '+inttostr(GetTickCount-lStartTime));


	//EstimateFDR(lnTests, lPs, lFDR05p, lFDR01p);
        EstimateFDR2(lnTests, lPs, lFDR05p, lFDR01p,lnegFDR05p, lnegFDR01p);
	//lStartTime := GetTickCount;
	//next histogram!
	(*for lInc := 1 to lnTests do
		lPs^[lInc] := pNormalInvQuickApprox(lPs^[lInc]); //slow!!!!!!!!! >5100ms
	lHdr.ImgBufferBPP  := 4;
	lHdr.ImgBufferItems :=lnTests;
	lHdr.GlMaxUnscaledS :=lPs^[1];
	lHdr.GlMinUnscaledS := lPs^[lnTests];
	lHdr.ImgBuffer :=bytep(lPs);
	lHdr.NIFTIhdr.scl_slope := 1;
	lHdr.NIFTIhdr.scl_inter := 0;
	lInc := 0;//B&W
	LoadMonochromeLUT(lInc,gBGImg,lHdr);
	DrawHistogram(lHdr,HistogramForm.HistoImage);
	HistogramForm.Caption := 'Z Histogram'+realtostr(lHdr.GlMinUnscaledS,6)+'..'+realtostr(lHdr.GlMaxUnscaledS,6);
	HistogramForm.show;
	ImgForm.PGImageCor.refresh;
	//ImgForm.caption :=('update(ms): '+inttostr(GetTickCount-lStartTime));
	//showmessage('Z Histogram'+realtostr(lHdr.GlMinUnscaledS,6)+'..'+realtostr(lHdr.GlMaxUnscaledS,6));
	//end histogram *)
555:

	FreeMem(lPs);
	case lStatIntent of
		kNIFTI_INTENT_CHISQ:begin
			lP05:= pChi2Inv(lDF,0.05);
			lP01 := pChi2Inv(lDF,0.01);
			lFWE05 := pChi2Inv(lDF,0.05/lnTests);
			lFWE01 := pChi2Inv(lDF,0.01/lnTests);
			lFDR05 := pChi2Inv(lDF,lFDR05p);
			lFDR01 := pChi2Inv(lDF,lFDR01p);
			lnegFDR05 := pChi2Inv(lDF,lnegFDR05p);
			lnegFDR01 := pChi2Inv(lDF,lnegFDR01p);
			lStr := 'X DF='+inttostr(lDF);
				end;
		kNIFTI_INTENT_ZSCORE: begin
			lP05:= pNormalInv(0.05);
			lP01 := pNormalInv(0.01);
			lFWE05 := pNormalInv(0.05/lnTests);
			lFWE01 := pNormalInv(0.01/lnTests);
			lFDR05 := pNormalInv(lFDR05p);
			lFDR01 := pNormalInv(lFDR01p);
			lnegFDR05 := pNormalInv(lnegFDR05p);
			lnegFDR01 := pNormalInv(lnegFDR01p);
                        lStr := 'Z';
		end;
		kNIFTI_INTENT_TTEST: begin
			lP05:= pTdistrInv(lDF,0.05);
			lP01 := pTdistrInv(lDF,0.01);
			lFWE05 := pTdistrInv(lDF,0.05/lnTests);
			lFWE01 := pTdistrInv(lDF,0.01/lnTests);
			lFDR05 := pTdistrInv(lDF,lFDR05p);
			lFDR01 := pTdistrInv(lDF,lFDR01p);
			lnegFDR05 := pTdistrInv(lDF,lnegFDR05p);
			lnegFDR01 := pTdistrInv(lDF,lnegFDR01p);
			lStr := 't DF='+inttostr(lDF);

		end;
		kNIFTI_INTENT_PVAL:begin
			lP05:= (0.05);
			lP01 := (0.01);
			lFWE05 := (0.05/lnTests);
			lFWE01 := (0.01/lnTests);
			lFDR05 := (lFDR05p);
			lFDR01 := (lFDR01p);
			lnegFDR05 := (lnegFDR05p);
			lnegFDR01 := (lnegFDR01p);
                        lStr := 'p';
			end;
		NIFTI_INTENT_LOG10PVAL: begin
			lP05:= PtoLog10(0.05);
			lP01 := PtoLog10(0.01);
			lFWE05 := PtoLog10(0.05/lnTests);
			lFWE01 := PtoLog10(0.01/lnTests);
			lFDR05 := PtoLog10(lFDR05p);
			lFDR01 := PtoLog10(lFDR01p);
			lnegFDR05 := PtoLog10(lnegFDR05p);
			lnegFDR01 := PtoLog10(lnegFDR01p);

			lStr := 'log10p';
		end;
		else
			Showmessage('Error: unknown stats intent');
	end; //case
	if (lStatIntent = kNIFTI_INTENT_PVAL) then begin
		if (lFDR05 < lFWE05) then
			lFDR05 := lFWE05;
	end else if (lFDR05 > lFWE05) then
		lFDR05 := lFWE05;
	if (lStatIntent = kNIFTI_INTENT_PVAL) then begin
		if (lFDR01 < lFWE01) then
			lFDR01 := lFWE01;
	end else if (lFDR01 > lFWE01) then
		lFDR01 := lFWE01;

	if (lStatIntent = kNIFTI_INTENT_PVAL) then begin
		if (lnegFDR05 > -lFWE05) then
			lnegFDR05 := -lFWE05;
		if (lnegFDR01 > -lFWE01) then
			lnegFDR01 := -lFWE01;
	end else begin
            if (lnegFDR05 < -lFWE05) then
		lnegFDR05 := -lFWE05;
            if (lnegFDR01 < -lFWE01) then
		lnegFDR01 := -lFWE01;
        end;
	ImgForm.StatusLabel.Caption := lStr+' Tests='+inttostr(lnTests)+' p05='+realtostr(lP05,4)+ ' p01='+realtostr(lP01,4)+' fwe05='+realtostr(lFWE05,4)+ ' fwe01='+realtostr(lFWE01,4)
        +' fdr05='+realtostr(lFDR05,4)+' fdr01='+realtostr(lFDR01,4)
        +' -fdr05='+realtostr(lnegFDR05,4)+' -fdr01='+realtostr(lnegFDR01,4) ;
end;

function MakeSameOrtho(var lBGImg: TBGImg;  var lHdr: TMRIcroHdr):boolean;
//this function disables reslicing - images will be shown unrotated and unscaled...
var
	lRow: integer;
begin
        result := false;
        for lRow := 1 to 3 do begin
            //lHdr.NIFTIhdr.pixdim[lRow] := 1; //Apr07
	    if lHdr.NIFTIhdr.dim[lRow] <>lBGImg.ScrnDim[lRow] then
                exit;
        end;
        lHdr.Mat:= Matrix3D ( lBGImg.Scrnmm[1],0,0,-lBGImg.Scrnmm[1]*(lBGImg.ScrnOri[1]-1),
							0,lBGImg.Scrnmm[2],0,-lBGImg.Scrnmm[2]*(lBGImg.ScrnOri[2]-1),
							0,0,lBGImg.Scrnmm[3],-lBGImg.Scrnmm[3]*(lBGImg.ScrnOri[3]-1),
						  0,0,0,1);
        result := true;
end;

procedure FindAlignment (var lBGImg: TBGImg; var lHdr: TMRIcroHdr);
//identifies spatial position of low X,Y,Z voxels : A/P/L/R/S/I
var
   lDim: integer;
   lXMid,lYMid,lZMid,laX,laY,laZ,lX,lY,lZ,lX2,lY2,lZ2: single;
   lMatrix: TMatrix;
begin
     lBGImg.KnownAlignment := false;
     if not IsNifTiMagic (lHdr.NIFTIHdr) then
        exit; //Analyze format: spatial coordinates are amibguous
     if (lHdr.NIFTIhdr.sform_code <= 0) and (lHdr.NIFTIhdr.qform_code <= 0) then
        exit; //NIfTI format with unspecified coordinates
     lBGImg.KnownAlignment := true;
     if (lBGImg.Resliced) and (lHdr.NIFTIhdr.sform_code > 0)  then begin
        lBGImg.MinChar[1] := 'L';
        lBGImg.MaxChar[1] := 'R';
        lBGImg.MinChar[2] := 'P';
        lBGImg.MaxChar[2] := 'A';
        lBGImg.MinChar[3] := 'I';
        lBGImg.MaxChar[3] := 'S';
        exit;
     end;
     if (not gBGImg.OrthoReslice) then begin
         lBGImg.MinChar[1] := ' ';
        lBGImg.MaxChar[1] := ' ';
        lBGImg.MinChar[2] := ' ';
        lBGImg.MaxChar[2] := ' ';
        lBGImg.MinChar[3] := ' ';
        lBGImg.MaxChar[3] := ' ';
        exit;
     end;
     //there are two approaches to solve this - a more elegant solution is to find the nearest orthogonal aligment
     //the method below is simpler, but might give unusual results if the field of view in one dimension is much larger than another
     lMatrix := lHdr.Mat;
     lXMid := lHdr.NIFTIhdr.Dim[1] div 2;
     lYMid := lHdr.NIFTIhdr.Dim[2] div 2;
     lZMid := lHdr.NIFTIhdr.Dim[3] div 2;
     for lDim := 1 to 3 do begin
         if lDim = 1 then begin
            FindMatrixPt(0,lYMid,lZMid,lX,lY,lZ,lMatrix);
            FindMatrixPt(lXMid*2,lYMid,lZMid,lX2,lY2,lZ2,lMatrix);
         end else if lDim = 2 then begin
            FindMatrixPt(lXMid,0,lZMid,lX,lY,lZ,lMatrix);
            FindMatrixPt(lXMid,lYMid*2,lZMid,lX2,lY2,lZ2,lMatrix);
         end else begin //lDim=3
            FindMatrixPt(lXMid,lYMid,0,lX,lY,lZ,lMatrix);
            FindMatrixPt(lXMid,lYMid,lZMid*2,lX2,lY2,lZ2,lMatrix);
         end;
         lX := lX-lX2; laX := abs(lX);
         lY := lY-lY2; laY := abs(lY);
         lZ := lZ-lZ2; laZ := abs(lZ);
         if (laX > laY) and (laX > laZ) then begin
            if lX < 0 then begin
               lBGImg.MinChar[lDim] := 'L';
               lBGImg.MaxChar[lDim] := 'R';
            end else begin
                lBGImg.MinChar[lDim] := 'R';
                lBGImg.MaxChar[lDim] := 'L';
            end;
         end else if (laY > laZ) then begin
             if lY < 0 then begin
                lBGImg.MinChar[lDIm] := 'P';
                lBGImg.MaxChar[lDim] := 'A';
             end else begin
                 lBGImg.MinChar[lDim] := 'A';
                 lBGImg.MaxChar[lDim] := 'P';
             end;
         end else if (laZ > laX) then begin
             if lZ < 0 then begin
                lBGImg.MinChar[lDim] := 'I';
                lBGImg.MaxChar[lDim] := 'S';
             end else begin
                 lBGImg.MinChar[lDim] := 'S';
                 lBGImg.MaxChar[lDim] := 'I';
             end;
         end else begin //all dims are equal
                 lBGImg.MinChar[lDim] := '?';
                 lBGImg.MaxChar[lDim] := '?';
         end;
     end;//for each dim

end;  //proc FindAlignment

function DICOMMirrorImgBuffer(var lHdr: TMRIcroHdr ): boolean;
var
 lXPos,lYPos,lZPos,lX,lY,lZ,lHlfY,lLineOffset,lLineOffsetIn: integer;
 lTemp32: single;
 lTemp16: SmallInt;
 lTemp: byte;
 l32: SingleP;
 l16: SmallIntP;
begin
  result := false;
  lX := lHdr.NIFTIhdr.Dim[1];
  lY := lHdr.NIFTIhdr.Dim[2];
  lZ := lHdr.NIFTIhdr.Dim[3];
  if lHdr.NIFTIhdr.Dim[4] > 1 then begin
      Showmessage('Can not mirror 4D data : '+lHdr.HdrFileName);
      exit;
  end;
  if (lHdr.ImgBufferItems < (lX*lY*lZ)) or (lX < 2) then begin
      Showmessage('Unsupported filetype : '+lHdr.HdrFileName);
     exit;
  end;
  lHlfY := lY div 2;
  lLineOffset := 0;

  //for each datatype...
  if lHdr.ImgBufferBPP = 4 then begin
	l32 := SingleP(lHdr.ImgBuffer);
        for lZPos := 1 to lZ do begin
            lLineOffsetIn := lLineOffset + ((lY-1)*lX );
            for lYPos := 1 to lHlfY do begin
                for lXPos := 1 to lX do begin
                    lTemp32 := l32^[lXPos+lLineOffsetIn];
                    l32^[lXPos+lLineOffsetIn] := l32^[lXPos+lLineOffset];
                    l32^[lXPos+lLineOffset] := lTemp32;
                end; //for X
                lLineOffset := lLineOffset + lX;
                lLineOffsetIn := lLineOffsetIn - lX;
            end; //for Y
        end; //for Z

  end else if lHdr.ImgBufferBPP = 2 then begin
	l16 := SmallIntP(lHdr.ImgBuffer);
        for lZPos := 1 to lZ do begin
            lLineOffsetIn := lLineOffset + ((lY-1)*lX );
            for lYPos := 1 to lHlfY do begin
                for lXPos := 1 to lX do begin
                    lTemp16 := l16^[lXPos+lLineOffsetIn];
                    l16^[lXPos+lLineOffsetIn] := l16^[lXPos+lLineOffset];
                    l16^[lXPos+lLineOffset] := lTemp16;
                end; //for X
                lLineOffset := lLineOffset + lX;
                lLineOffsetIn := lLineOffsetIn - lX;
            end; //for Y
        end; //for Z
 end else if lHdr.ImgBufferBPP = 1 then begin
        for lZPos := 1 to lZ do begin
            lLineOffsetIn := lLineOffset + ((lY-1)*lX );
            for lYPos := 1 to lHlfY do begin
                for lXPos := 1 to lX do begin
                    lTemp := lHdr.ImgBuffer^[lXPos+lLineOffsetIn];
                    lHdr.ImgBuffer^[lXPos+lLineOffsetIn] := lHdr.ImgBuffer^[lXPos+lLineOffset];
                    lHdr.ImgBuffer^[lXPos+lLineOffset] := lTemp;
                end; //for X
                lLineOffset := lLineOffset + lX;
                lLineOffsetIn := lLineOffsetIn - lX;
            end; //for Y
        end; //for Z
  end else //unsupported bits-per-pixel dataformat
	Showmessage('Unsupported BPP ='+inttostr(lHdr.ImgBufferBPP) );
  result := true;
end; //proc DICOMMirrorImgBuffer

function isPlanarImg(  rawRGB: bytep;  lX, lY, lZ: integer): boolean ;
var
 pos, posEnd, incPlanar, incPacked, byteSlice: integer;
 dxPlanar, dxPacked: double;
begin
  //determine if RGB image is PACKED TRIPLETS (RGBRGBRGB...) or planar (RR..RGG..GBB..B)
  //assumes strong correlation between voxel and neighbor on next line
  result := false;
  if (lY < 2) then exit; //requires at least 2 rows of data
  incPlanar := lX; //increment next row of PLANAR image
  incPacked := lX * 3; //increment next row of PACKED image
  byteSlice := incPacked * lY; //bytes per 3D slice of RGB data
  dxPlanar := 0.0;//difference in PLANAR
  dxPacked := 0.0;//difference in PACKED
  pos := ((lZ div 2) * byteSlice)+1; //offset to middle slice for 3D data
  posEnd := pos + byteSlice - incPacked;
  while (pos <= posEnd) do begin
    dxPlanar := dxPlanar + abs(rawRGB^[pos]-rawRGB^[pos+incPlanar]);
    dxPacked := dxPacked + abs(rawRGB^[pos]-rawRGB^[pos+incPacked]);
    pos := pos + 1;
  end;
  result := (dxPlanar < dxPacked);
end;

function ParseRGB (var lHdr: TMRIcroHdr): boolean;//RGB
//red green blue saved as contiguous planes...
var
  lInSlice,lOutSlice,lZ,lSliceSz,lSliceVox,lInPos,lOutPos: integer;
  isPlanarRGB: boolean;
  lP: bytep;
begin
    result := false;
    lSliceSz := lHdr.NIFTIhdr.Dim[1]*lHdr.NIFTIhdr.Dim[2];
    lZ := lSliceSz * 3 * lHdr.NIFTIhdr.Dim[3];
    if lZ < 1 then exit;
    getmem( lP,lZ);
    Move(lHdr.ImgBuffer^,lP^,lZ);
    freemem(lHdr.ImgBufferUnaligned);
    lZ := lSliceSz  * lHdr.NIFTIhdr.Dim[3];
    GetMem(lHdr.ImgBufferUnaligned ,lZ+16);
    //lHdr.ImgBuffer := ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
    lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned,16);
    if gBGImg.PlanarRGB = 0 then
       isPlanarRGB := false
    else if gBGImg.PlanarRGB = 1 then
       isPlanarRGB := true
    else
        isPlanarRGB := isPlanarImg(lP, lHdr.NIFTIhdr.Dim[1], lHdr.NIFTIhdr.Dim[2], lHdr.NIFTIhdr.Dim[3]);
    if isPlanarRGB then begin
      if (lHdr.Index mod 3) = 1 then //green
        lInSlice := lSliceSz
      else if (lHdr.Index mod 3) = 2 then//blue
        lInSlice := lSliceSz+lSliceSz
      else
        lInSlice := 0;
      lOutSlice := 0;
      for lZ := 1 to lHdr.NIFTIhdr.Dim[3] do begin
        for lSliceVox := 1 to lSliceSz do begin
          lHdr.ImgBuffer^[lSliceVox+lOutSlice] := lP^[lSliceVox+lInSlice];
        end;
        inc(lOutSlice,lSliceSz);
        inc(lInSlice,lSliceSz+lSliceSz+lSliceSz);
      end;
    end else begin
      if (lHdr.Index mod 3) = 1 then //green
        lInPos := 2
      else if (lHdr.Index mod 3) = 2 then//blue
        lInPos := 3
      else
        lInPos := 1;
      for lOutPos := 1 to lZ do begin
          lHdr.ImgBuffer^[lOutPos] := lP^[lInPos];
          lInPos := lInPos + 3;
      end;
    end;
    freemem(lP);
    for lZ := 0 to 255 do begin
			lHdr.LUT[lZ].rgbRed := 0;
			lHdr.LUT[lZ].rgbGreen := 0;
			lHdr.LUT[lZ].rgbBlue := 0;
			lHdr.LUT[lZ].rgbReserved := kLUTalpha;
		end;
    if (lHdr.Index mod 3) = 1 then begin//green
      for lZ := 0 to 255 do
			  lHdr.LUT[lZ].rgbGreen := lZ;
    end else if (lHdr.Index mod 3) = 2 then begin //blue
      for lZ := 0 to 255 do
			  lHdr.LUT[lZ].rgbBlue := lZ;
    end else begin
      for lZ := 0 to 255 do
			  lHdr.LUT[lZ].rgbRed := lZ;
    end;
    result := true;
end;

(*function ParseRGB (var lHdr: TMRIcroHdr): boolean;//RGB
//red green blue saved as contiguous planes...
var
  lInSlice,lOutSlice,lZ,lSliceSz,lSliceVox: integer;
  lP: bytep;
begin
    result := false;
    lSliceSz := lHdr.NIFTIhdr.Dim[1]*lHdr.NIFTIhdr.Dim[2];
    lZ := lSliceSz * 3 * lHdr.NIFTIhdr.Dim[3];
    if lZ < 1 then exit;
    getmem( lP,lZ);
    Move(lHdr.ImgBuffer^,lP^,lZ);
    freemem(lHdr.ImgBufferUnaligned);
    lZ := lSliceSz  * lHdr.NIFTIhdr.Dim[3];
    GetMem(lHdr.ImgBufferUnaligned ,lZ+16);
       {$IFDEF FPC}
    lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned,16);
   {$ELSE}
    lHdr.ImgBuffer := ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
   {$ENDIF}
    if (lHdr.Index mod 3) = 1 then //green
      lInSlice := lSliceSz
    else if (lHdr.Index mod 3) = 2 then//blue
      lInSlice := lSliceSz+lSliceSz
    else
      lInSlice := 0;

    lOutSlice := 0;
    for lZ := 1 to lHdr.NIFTIhdr.Dim[3] do begin
      for lSliceVox := 1 to lSliceSz do begin
        lHdr.ImgBuffer^[lSliceVox+lOutSlice] := lP^[lSliceVox+lInSlice];
      end;
      inc(lOutSlice,lSliceSz);
      inc(lInSlice,lSliceSz+lSliceSz+lSliceSz);
    end;
    freemem(lP);
    if (lHdr.Index mod 3) = 1 then //green
       lZ := 3
    else if (lHdr.Index mod 3) = 2 then //blue
       lZ := 2
     else //red
       lZ := 1;

    LoadMonochromeLUT (lZ, gBGImg, lHdr) ;
    result := true;
end;      *)

procedure NonReslicedGB (var lBackgroundImg: TBGImg; var lImg2Load: TMRIcroHdr);//vcx
begin
  if lImg2Load.NIfTItransform then
    lBackgroundImg.InvMat := Hdr2InvMat (lImg2Load.NIftiHdr,lImg2Load.NIfTItransform );
  FindMatrixBounds(lBackgroundImg,lImg2Load,false);
  FindAlignment(lBackgroundImg,lImg2Load);
  MakeSameOrtho(lBackgroundImg,lImg2Load);
end;

procedure ReorientToNearestOrtho (var lBackgroundImg: TBGImg; var lImg2Load: TMRIcroHdr; lLoadBackground: boolean);
//only apply this to the background image - other routines will reorient overlays
begin
  lBackgroundImg.ReorientHdr := lImg2Load.NIFTIhdr;//vcx
  if not OrthoReorientCore(lImg2Load,false) then exit;//no change
  if not lLoadBackground then exit; //no change in bounding box
  lBackgroundImg.UseReorientHdr := true;
  NonReslicedGB(lBackgroundImg,lImg2Load);
end;

function UnBZip2 (var FileName: string; var lBuf: ByteP; offset,volBytes: int64): boolean;
label
     123;
var
  Decompressed: TDecompressBzip2Stream;
  InFile: TFileStream;
  i: int64;
begin
  result := false;
  InFile:=TFileStream.Create(FileName, fmOpenRead);
  Decompressed:=TDecompressBzip2Stream.Create(InFile);
  if (offset > 0) then begin
      i:=Decompressed.Read(lBuf^,volBytes);
      if i <> offset then begin
	      showmessage(format('BZip2 error: unable to skip header for %s', [FileName]));
	      goto 123;
      end;
  end;
  //i:=Decompressed.Read(Pointer(@rawData[0])^,volBytes);
  i:=Decompressed.Read(lBuf^,volBytes);
  result := (i = volBytes);
  if not result then
     showmessage(format('BZip2 error: read %d but expected %d bytes for %s', [i, volBytes, FileName]));
  123:
  Decompressed.Free;
  InFile.Free;
end;

function OpenImg(var lBackgroundImg: TBGImg; var lImg2Load: TMRIcroHdr; lLoadBackground,lVOILoadAsBinary,lNoScaling8bit,lResliceIn,l4D: boolean): boolean;
//lReslice: use orientation matrix to transform image -> do not use if l4D = true
//l4D: load all slices of a 4D volume
label
456;
var
  lReslice,lSwap: boolean;
  lWordX: word;
  lP05,lP01,lFWE05,lFWE01,lFDR05,lFDR01:single;
  lMinI,lMaxI,lInc: integer;
  lMultiImgSzOff,lMultiImgSz,lOffset,
  lVol,lnVol,lFileSz,lDataType,lFSz,lImgSamples: Int64;    //,lRow
   lP: Bytep;
   lW: Wordp;
  lFName,lParseName: String;
  F: file;
  l8Buf :  int8p;
  l16Buf : SmallIntP;
  l32Buf,l32TempBuf : SingleP;
  l64Buf : DoubleP;
begin
     lReslice := lResliceIn;
    if lLoadBackground then begin
       lBackgroundImg.LabelRA := nil;
      ImgForm.CloseImagesClick(nil);
    end;
    result := false;
    FreeImgMemory(lImg2Load);
    if not lImg2Load.DiskDataNativeEndian then
	  lSwap := true
    else
	   lSwap := false;
    if lLoadBackground then begin
          lBackgroundImg.UseReorientHdr := false;//vcx
          if(lImg2Load.NIFTIhdr.Dim[3] = 1) then
             lReslice := false;
          lBackgroundImg.Resliced := lReslice;
          if not lReslice then
            NonReslicedGB(lBackgroundImg,lImg2Load);
	  FindMatrixBounds(lBackgroundImg,lImg2Load,lReslice);
          if (gBGImg.ScrnDim[1] < 2) or (gBGImg.ScrnDim[2] < 2) or (gBGImg.ScrnDim[3] < 1) then begin
             Showmessage('Error: this does not appear to be a valid 2D or 3D image.');
             exit;
          end;
          if  (gBGImg.ScrnDim[3] = 1) then begin
             lBackgroundImg.Resliced := false;
             //showmessage('x');
          end;
          FindAlignment(lBackgroundImg,lImg2Load);
    end;

  if (not IsNifTiMagic(lImg2Load.niftiHdr)) or (lImg2Load.NIFTIhdr.sform_code < 1) or (lImg2Load.NIFTIhdr.sform_code > 10) then
	lBackgroundImg.KnownAlignment := false;
  if not lReslice then begin
    if lLoadBackground then begin
      //MakeSameOrtho(lBackgroundImg,lImg2Load);
      FindMatrixBounds(lBackgroundImg,lImg2Load,false);
      FindAlignment(lBackgroundImg,lImg2Load);
      MakeSameOrtho(lBackgroundImg,lImg2Load);
    end;
  end; //no reslice...
  lDataType := lImg2Load.NIFTIhdr.datatype;
   lFName := lImg2Load.ImgFileName;
  lMultiImgSz := ComputeImageDataBytes(lImg2Load);
  if (lMultiImgSz < 1) then begin
     Showmessage('Unable to load this image (too large or corrupt)');
  end;
  lOffset := round(lImg2Load.NIFTIhdr.vox_offset);
  lMultiImgSzOff := lMultiImgSz + abs(lOffset);
  if lImg2Load.NIFTIhdr.dim[4] < 1 then  //June2009 - prevent error if 3D image sets field  to zero instead of one
    lImg2Load.NIFTIhdr.dim[4] := 1;
  if lImg2Load.NIFTIhdr.dim[5] < 1 then //June2009 - prevent error if DTI image sets field  to zero instead of one
    lImg2Load.NIFTIhdr.dim[5] := 1;
  lnVol := lImg2Load.NIFTIhdr.dim[4]*lImg2Load.NIFTIhdr.dim[5];//June2009 - for DTI data where direction is 5th dimension
  if lMultiImgSz < 1 then exit;
  lFSz := FSize(lFName);


  if (lFSz = 0) then
	Showmessage('Unable to find the image file '+lFName);

  lVol := 1;
  if lnVol > 1 then begin
	 if lOffset < 0 then
		lFileSz := lMultiImgSzOff * lnVol
	 else
		 lFileSz := (lnVol * lMultiImgSz) + lOffset;
	 lVol := 1; //assume 1st volume
         if  lBackgroundImg.Prompt4DVolume then begin
	    lVol := ReadIntForm.GetInt('Multi-volume file, please select volume to view.',1,1,lnVol);
                 application.processmessages;
         end;
         if (lVol < 1) then lVol := 1;
         if (lVol > lnVol) then lVol := lnVol;
  end else
	  lFileSz := lMultiImgSzOff;
  if ((lFileSz) > lFSz) and (lImg2Load.gzBytesX = K_gzBytes_headerAndImageUncompressed) then begin
		ShowMessage('Error: This image file is smaller than described in header.'+
		' Expected: '+inttostr(lFileSz)+'  Selected:'+inttostr(lFSz)+ ' '+lFname);
		exit;
  end;
  {$I-}
  AssignFile(F, lFName);
  FileMode := 0;  { Set file access to read only }
  Reset(F, 1);
  if (lImg2Load.gzBytesX <> K_gzBytes_headerAndImageUncompressed) then begin  //deal with compressed data
     if (lImg2Load.gzBytesX = K_gzBytes_headerAndImageCompressed) then begin
       if lOffset < 0 then
           lOffset := abs(lOffset) + (lMultiImgSzOff *(lVol-1))
       else
         lOffset := lOffset + (lMultiImgSz *(lVol-1));
     end else
        lOffset := (lMultiImgSz *(lVol-1));//header UNCOMPRESSED!
  end else if lOffset < 0 then
	 Seek (F,abs(lOffset) + (lMultiImgSzOff *(lVol-1)) )
  else
	  Seek (F,lOffset + (lMultiImgSz *(lVol-1)) );

  case lDataType of
	  kDT_INT8,kDT_UNSIGNED_CHAR : lImg2Load.ImgBufferBPP := 1;
          kDT_SIGNED_SHORT,kDT_UINT16: lImg2Load.ImgBufferBPP := 2;
	  kDT_SIGNED_INT,kDT_FLOAT, kDT_UINT32:  lImg2Load.ImgBufferBPP := 4;
          kDT_DOUBLE: lImg2Load.ImgBufferBPP := 8;
	  kDT_RGB: lImg2Load.ImgBufferBPP := 1;//rgb
	  else begin
			  showmessage('Unable to read this image format '+inttostr(lDataType));
		  goto 456;
	  end;
  end;
  //Next get memory
  lImgSamples := round(ComputeImageDataBytes8bpp(lImg2Load));
  if lImgSamples < 1 then exit;
  lImg2Load.ImgBufferItems := lImgSamples;
  lMultiImgSz := (lImgSamples * lImg2Load.ImgBufferBPP);
  if lDataType = kDT_RGB then
    lMultiImgSz := lMultiImgSz * 3;//RGB
  if l4D then  begin
     lMultiImgSz := lMultiImgSz * lnVol;
     lImgSamples := lImgSamples * lnVol; //Apr07
  end;
  if lMultiImgSz > freeRam then begin
      Showmessage('Unable to load image: not enough RAM.');
      goto 456;
      //exit;
  end;
  try
     GetMem(lImg2Load.ImgBufferUnaligned ,lMultiImgSz+16);
  except
           showmessage('Load Image Error: System memory exhausted.');
           freemem(lImg2Load.ImgBufferUnaligned);
           //do goto 456
           exit;
  end;
  lImg2Load.ImgBuffer := align(lImg2Load.ImgBufferUnaligned, 16);
  //Next Load Image
    if (lImg2Load.gzBytesX <> K_gzBytes_headerAndImageUncompressed) then begin
	lP := ByteP(lImg2Load.ImgBuffer);
        if lImg2Load.gzBytesX = K_bz2Bytes_headerAndImageCompressed then
           UnBZip2(lFName,lP,lOffset,lMultiImgSz)
        else if lImg2Load.gzBytesX = K_gzBytes_headerAndImageCompressed then
           UnGZip(lFName,lP,lOffset,lMultiImgSz)
        else
             UnGZip2 (lFName,lP,lOffset,lMultiImgSz, round(lImg2Load.NIFTIhdr.vox_offset)); //unzip
  end else
	  BlockRead(F,lImg2Load.ImgBuffer^,lMultiImgSz);
  if IOResult <> 0 then
	 ShowMessage('Open image file error: '+inttostr(IOResult));
  //Next: prepare image : byte swap, check for special..
  case lDataType of
    kDT_RGB:  ParseRGB(lImg2Load);//RGB
    kDT_INT8: begin //int8 -> uint8
      l8Buf := int8p(lImg2Load.ImgBuffer );
      for lInc := 1 to lImgSamples do
          lImg2Load.ImgBuffer^[lInc] := l8Buf[lInc] + 128;;
      lImg2Load.NIFTIhdr.scl_inter := lImg2Load.NIFTIhdr.scl_inter - (128*lImg2Load.NIFTIhdr.scl_slope);
      lImg2Load.NIFTIhdr.datatype:= kDT_UINT8;
    end;
         {$IFDEF UINT16ASFLOAT}
        kDT_UINT16: begin
                    //showmessage(format('%d ', [12]));
                    if gBGImg.LoadUInt16asFloat32 then begin
                      l16Buf := SmallIntP(lImg2Load.ImgBuffer );
                      if lSwap then
			   for lInc := 1 to lImgSamples do
			    l16Buf^[lInc] := Swap2(l16Buf^[lInc]);
                       GetMem(lW , lImgSamples * sizeof(Word));
                       for lInc := 1 to lImgSamples do
			   lW^[lInc] := word(l16Buf^[lInc]);
                       FreeMem(lImg2Load.ImgBufferUnaligned );
                       //convert data type to float32
                       lImg2Load.NIFTIhdr.bitpix:= 32;
                       lImg2Load.NIFTIhdr.datatype := kDT_FLOAT;
                       lImg2Load.ImgBufferBPP := 4;
                       lMultiImgSz := (lImgSamples * lImg2Load.ImgBufferBPP);
                       GetMem(lImg2Load.ImgBufferUnaligned ,lMultiImgSz+16);
                       lImg2Load.ImgBuffer := align(lImg2Load.ImgBufferUnaligned, 16);
                       l32Buf := SingleP(lImg2Load.ImgBuffer );
                       for lInc := 1 to lImgSamples do
			   l32Buf^[lInc] := lW^[lInc];
                       FreeMem(lW);
                     end else begin
       		        l16Buf := SmallIntP(lImg2Load.ImgBuffer );
                        if lSwap then
       			   for lInc := 1 to lImgSamples do
       			       l16Buf^[lInc] := Swap2(l16Buf^[lInc]);
                        for lInc := 1 to lImgSamples do begin //downsample UINT16 to 15bit integer to fit into INT16
       			    lWordX := word(l16Buf^[lInc]);
       			    l16Buf^[lInc] := lWordX shr 1;
       			end; //for
                        lImg2Load.NIFTIhdr.scl_slope:= lImg2Load.NIFTIhdr.scl_slope * 2.0;
                        lImg2Load.NIFTIhdr.datatype:= kDT_SIGNED_SHORT;
       		end; //if kDT_UINT16                      end;
            end;//kDT_UINT16
        kDT_SIGNED_SHORT: begin //16-bit int
         {$ELSE}
	kDT_SIGNED_SHORT,kDT_UINT16: begin //16-bit int
        {$ENDIF}
		l16Buf := SmallIntP(lImg2Load.ImgBuffer );
                 if lSwap then
			 for lInc := 1 to lImgSamples do begin
			  l16Buf^[lInc] := Swap2(l16Buf^[lInc]);
			 end;

                  if (kDT_UINT16=lDataType ) then begin //avoid wrap around if read as signed value
		     for lInc := 1 to lImgSamples do begin
			 lWordX := word(l16Buf^[lInc]);
			 l16Buf^[lInc] := lWordX shr 1;
		     end; //for
                     lImg2Load.NIFTIhdr.scl_slope:= lImg2Load.NIFTIhdr.scl_slope * 2.0;
                     lImg2Load.NIFTIhdr.datatype:= kDT_SIGNED_SHORT;
		end; //if kDT_UINT16
		end; //16-bit
	  kDT_SIGNED_INT: begin
		l32Buf := SingleP(lImg2Load.ImgBuffer );
		if lSwap then //unswap and convert integer to float
			 for lInc := 1 to lImgSamples do
			  l32Buf^[lInc] := (Swap4r4i(l32Buf^[lInc]))
		else  //convert integer to float
			 for lInc := 1 to lImgSamples do
			  l32Buf^[lInc] := Conv4r4i(l32Buf^[lInc]);
		end; //32-bit int
          kDT_UINT32 : begin
		l32Buf := SingleP(lImg2Load.ImgBuffer );
		if lSwap then //unswap and convert integer to float
			 for lInc := 1 to lImgSamples do
			  l32Buf^[lInc] := (Swap4r4ui(l32Buf^[lInc]))
		else  //convert integer to float
			 for lInc := 1 to lImgSamples do
			  l32Buf^[lInc] := Conv4r4ui(l32Buf^[lInc]);
		end;
	  kDT_FLOAT: begin
		l32Buf := SingleP(lImg2Load.ImgBuffer );
		if lSwap then
                    for lInc := 1 to lImgSamples do
		        pswap4r(l32Buf^[lInc]);  //faster as procedure than function see www.optimalcode.com
                for lInc := 1 to lImgSamples do
			if specialsingle(l32Buf^[lInc]) then l32Buf^[lInc] := 0.0;
		end; //32-bit float
	  kDT_DOUBLE: begin
		l64Buf := DoubleP(lImg2Load.ImgBuffer );
                lImg2Load.ImgBufferBPP := 4; //we will save as 32-bit
                lMultiImgSz := (lImgSamples * lImg2Load.ImgBufferBPP);
                if l4D then  begin
                   lMultiImgSz := lMultiImgSz * lnVol;
                   lImgSamples := lImgSamples * lnVol; //Apr07
                end;
                try
                   GetMem(l32TempBuf ,lMultiImgSz+16);
                except
                      showmessage('64-bit Image Error: System memory exhausted.');
                      freemem(l32TempBuf);
                      freemem(lImg2Load.ImgBufferUnaligned);
                      exit;
                end;
                if lSwap then begin
                     for lInc := 1 to lImgSamples do begin
                         try
                            l32TempBuf^[lInc] := Swap64r(l64Buf^[lInc])
                         except
                            l32TempBuf^[lInc] := 0;
                         end; //except
                     end; //for
                  end else begin
                     for lInc := 1 to lImgSamples do begin
                         try
                            l32TempBuf^[lInc] := l64Buf^[lInc]
                         except
                            l32TempBuf^[lInc] := 0;
                         end; //except
                     end; //for
                  end; //not swap
                  //now copy from temp buffer to longer-term buffer
                  freemem(lImg2Load.ImgBufferUnaligned);
                  try
                     GetMem(lImg2Load.ImgBufferUnaligned ,lMultiImgSz+16);
                  except
                        showmessage('Load Image Error: System memory exhausted.');
                        freemem(lImg2Load.ImgBufferUnaligned);
                        exit;
                  end;
                  {$IFDEF FPC}
                  lImg2Load.ImgBuffer := Align(lImg2Load.ImgBufferUnaligned, 16);
                  {$ELSE}
                  lImg2Load.ImgBuffer := ByteP($fffffff0 and (integer(lImg2Load.ImgBufferUnaligned)+15));
                  {$ENDIF}
                  l32Buf := SingleP(lImg2Load.ImgBuffer );
                  Move(l32TempBuf^,l32Buf^,lMultiImgSz);
                  freemem(l32TempBuf);
		  for lInc := 1 to lImgSamples do
			if specialsingle(l32Buf^[lInc]) then l32Buf^[lInc] := 0.0;
		//for lInc := 1 to lImgSamples do
		//	if specialsingle(l32Buf^[lInc]) then l32Buf^[lInc] := 0.0;
		end; //64-bit float
	  kDT_UNSIGNED_CHAR : ;
	  //else will be aborted at previous case
  end;//case lDataType of
  if (lDataType = kDT_RGB) then
  	 //do not transform
  else if lImg2Load.NIFTIhdr.magic = kNIFTI_MAGIC_DCM then
     DICOMMirrorImgBuffer(lImg2Load)
  else if (lLoadBackground) and (not lReslice) and (lBackgroundImg.KnownAlignment) and (lBackgroundImg.OrthoReslice)  then
  	ReorientToNearestOrtho(lBackgroundImg,lImg2Load,lLoadBackground)
  else if (l4D) and (not lReslice) and (lBackgroundImg.KnownAlignment) and (lBackgroundImg.OrthoReslice)  then
          OrthoReorientCore(lImg2Load,true);
  //next correct image size
  if lImg2Load.NIFTIhdr.scl_slope = 0 then
	lImg2Load.NIFTIhdr.scl_slope := 1;


  if (lLoadBackground) and (not l4D) then begin
    ResliceScrnImg ( lBackgroundImg,lImg2Load,true);
  end
  else if not l4D then
          ResliceScrnImg ( lBackgroundImg,lImg2Load,lBackgroundImg.OverlaySmooth); //12 April 2009 - allow nearest neighbor
 //Next: find min/max - better after reslicing incase we have padded zeros at the edges and zero < min


   case lImg2Load.ImgBufferBPP of
	   1: begin
		FindImgMinMax8 (lImg2Load, lMini,lMaxi);
		lImg2Load.GlMaxUnscaledS := lMaxI;
		lImg2Load.GlMinUnscaledS := lMinI;
	   end;
	   2: begin
		FindImgMinMax16 (lImg2Load, lMini,lMaxi);
		lImg2Load.GlMaxUnscaledS := lMaxI;
		lImg2Load.GlMinUnscaledS := lMinI;
	   end;
	   4:
		FindImgMinMax32 (lImg2Load,lImg2Load.GlMinUnscaledS,lImg2Load.GlMaxUnscaledS);
	   else Showmessage('OpenImg and LoadImg error');
  end; //case ImgBufferBPP
    //showmessage(format('%g %g ', [lImg2Load.GlMinUnscaledS,lImg2Load.GlMaxUnscaledS]));
    balance(lImg2Load); //preparecontrast autobalance
	lImg2Load.WindowScaledMin := raw2ScaledIntensity(lImg2Load,lImg2Load.AutoBalMinUnscaled);
	lImg2Load.WindowScaledMax := raw2ScaledIntensity(lImg2Load,lImg2Load.AutoBalMaxUnscaled);
   if (lVOILoadAsBinary) then begin
	  lImg2Load.WindowScaledMin := kMin8bit;//MAW
	  lImg2Load.WindowScaledMax := kVOI8bit;
		  lImg2Load.NIFTIhdr.scl_slope := 1;
	  lImg2Load.NIFTIhdr.scl_inter := 0;
  end else if lDataType = kDT_RGB then begin//RGB
	  lImg2Load.UsesCustomPalette := true;
	  lImg2Load.NIFTIhdr.scl_slope := 1;
	  lImg2Load.NIFTIhdr.scl_inter := 0;
	  lImg2Load.WindowScaledMin := kMin8bit;
	  lImg2Load.WindowScaledMax := 255;
	  lImg2Load.AutoBalMinUnscaled := lImg2Load.WindowScaledMin;
	  lImg2Load.AutoBalMaxUnscaled := lImg2Load.WindowScaledMax;
  end else if (lNoScaling8bit) and (lImg2Load.ImgBufferBPP = 1) then begin
	lImg2Load.UsesCustomPalette := false;
	  lImg2Load.NIFTIhdr.scl_slope := 1;
	  lImg2Load.NIFTIhdr.scl_inter := 0;
	  lImg2Load.WindowScaledMin := kMin8bit;
	  lImg2Load.WindowScaledMax := 255;
	  lImg2Load.AutoBalMinUnscaled := lImg2Load.WindowScaledMin;
	  lImg2Load.AutoBalMaxUnscaled := lImg2Load.WindowScaledMax;
  end else if (lImg2Load.NIFTIhdr.intent_code = kNIFTI_INTENT_ESTIMATE) and (lImg2Load.NIFTIhdr.intent_name[1] = '%')  then begin
	  lImg2Load.WindowScaledMin := kMin8bit;
	  lImg2Load.WindowScaledMax := 100;//lImg2Load.GlMaxUnscaledS;
	  lImg2Load.LutFromZero := true;
	  lImg2Load.AutoBalMinUnscaled := lImg2Load.WindowScaledMin;
	  lImg2Load.AutoBalMaxUnscaled := lImg2Load.WindowScaledMax;
  end else if ( (lImg2Load.NIFTIhdr.intent_code = NIFTI_INTENT_LOG10PVAL) or (lImg2Load.NIFTIhdr.intent_code =kNIFTI_INTENT_PVAL) or (lImg2Load.NIFTIhdr.intent_code = kNIFTI_INTENT_ZSCORE) or ((lImg2Load.NIFTIhdr.intent_code = kNIFTI_INTENT_TTEST) or (lImg2Load.NIFTIhdr.intent_code = kNIFTI_INTENT_CHISQ))) and (lImg2Load.ImgBufferBPP = 4) and (not l4D)  then begin
	  //ComputeFDR(lImg2Load.NIFTIhdr.intent_code,round(lImg2Load.NIFTIhdr.intent_p1),lImg2Load.ImgBufferItems,lImg2Load.ImgBufferBPP,lImg2Load.ImgBuffer,lP05,lP01,lFWE05,lFWE01,lFDR05,lFDR01);
	  ComputeFDR(lImg2Load,lP05,lP01,lFWE05,lFWE01,lFDR05,lFDR01);

	  if (Raw2ScaledIntensity(lImg2Load,lImg2Load.GlMaxUnscaledS)> lFDR05) and (lFDR05 > 0) then begin
		lImg2Load.WindowScaledMin := lFDR05; //0.001      xxx
		if lFDR01 > 0 then
			lImg2Load.WindowScaledMax := lFDR01
		else
			lImg2Load.WindowScaledMax := 2*lFDR05; //0.000001
	  end else begin
		lImg2Load.WindowScaledMin := lP05; //0.001      xxx
		lImg2Load.WindowScaledMax := lP01; //0.000001
	  end;
	  if (lImg2Load.WindowScaledMax < 0.00001) and (lImg2Load.WindowScaledMin < 0.00001) then begin
		  lImg2Load.WindowScaledMax := 5;
		  lImg2Load.WindowScaledMin := 0;
	  end;
	  lImg2Load.LutFromZero := true;
	  lImg2Load.AutoBalMinUnscaled := lImg2Load.WindowScaledMin;
	  lImg2Load.AutoBalMaxUnscaled := lImg2Load.WindowScaledMax;
  end (*else if (lImg2Load.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) and (lImg2Load.ImgBufferBPP = 1) and (lImg2Load.NIFTIhdr.regular = char(98))  then begin
   //createLutLabel (lImg2Load, 1.0);
   LoadLabelLUT(lBackgroundImg,lImg2Load);
   if (( lImg2Load.NIFTIhdr.vox_offset- lImg2Load.NIFTIhdr.HdrSz) > 128) then
      LoadLabels(lImg2Load.HdrFileName,lBackgroundImg.LabelRA, lImg2Load.NIFTIhdr.HdrSz, round( lImg2Load.NIFTIhdr.vox_offset))
   else
       LoadLabelsTxt(lImg2Load.HdrFileName, lBackgroundImg.LabelRA);
   lImg2Load.NIFTIhdr.scl_slope := 1;
   lImg2Load.NIFTIhdr.scl_inter := 0;
   lImg2Load.WindowScaledMin := kMin8bit;
   lImg2Load.WindowScaledMax := 255;
   lImg2Load.UsesCustomPalette := true;
   lImg2Load.AutoBalMinUnscaled := lImg2Load.WindowScaledMin;
   lImg2Load.AutoBalMaxUnscaled := lImg2Load.WindowScaledMax;
  end else*) else if ( (lImg2Load.NIFTIhdr.regular = char(98)) or (lImg2Load.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL)) and ((lImg2Load.ImgBufferBPP = 1) or (lImg2Load.ImgBufferBPP = 2))  then begin
           if not LoadLabelLUT(lBackgroundImg,lImg2Load) then begin
              createLutLabel (lImg2Load.LUT, 1.0);
              lImg2Load.WindowScaledMin := kMin8bit;
             lImg2Load.WindowScaledMax := 255;
             lImg2Load.UsesCustomPaletteRandomRainbow := true;//must be done - see Inia template
              lImg2Load.WindowScaledMin := 0;//kMin8bit;
	      lImg2Load.WindowScaledMax := 100;//255;
              (*lImg2Load.UsesCustomPaletteRandomRainbow := true; *)
           end else begin
             lImg2Load.WindowScaledMin := kMin8bit;
             lImg2Load.WindowScaledMax := 255;
           end;
           lImg2Load.AutoBalMinUnscaled := lImg2Load.WindowScaledMin;
	   lImg2Load.AutoBalMaxUnscaled := lImg2Load.WindowScaledMax;
	  lImg2Load.NIFTIhdr.scl_slope := 1;
	  lImg2Load.NIFTIhdr.scl_inter := 0;
	  lImg2Load.UsesCustomPalette := true;
          if {lLoadBackground} true then begin
             if (( lImg2Load.NIFTIhdr.vox_offset- lImg2Load.NIFTIhdr.HdrSz) > 128) then
                LoadLabels(lImg2Load.HdrFileName,lBackgroundImg.LabelRA, lImg2Load.NIFTIhdr.HdrSz, round( lImg2Load.NIFTIhdr.vox_offset))
             else
                LoadLabelsTxt(lImg2Load.HdrFileName, lBackgroundImg.LabelRA);
             if  (High(lBackgroundImg.LabelRA) < 1) and (lImg2Load.ImgBufferBPP = 1) then
                 LoadLabelsOld(lBackgroundImg,lImg2Load);
             if High(lBackgroundImg.LabelRA) > 0 then
                lImg2Load.UsesLabels := true;
             //showmessage(inttostr(High(lBackgroundImg.LabelRA) )+'xxx');
          end
	  //ImgForm.Help1.caption := 'imaw'+realtostr(lImg2Load.WindowScaledMin,4);//maw
  end else  begin
    if (lImg2Load.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then begin//>only called when BPP <> 1
       LoadLabelLUT(lBackgroundImg,lImg2Load);
    end;
    lImg2Load.UsesCustomPalette := false;
	lImg2Load.WindowScaledMin := raw2ScaledIntensity(lImg2Load,lImg2Load.AutoBalMinUnscaled);
	lImg2Load.WindowScaledMax := raw2ScaledIntensity(lImg2Load,lImg2Load.AutoBalMaxUnscaled);

  end;
  lParseName := parsefilename(extractfilename(lImg2Load.HdrFileName));

  if (lImg2Load.NIFTIhdr.cal_min < lImg2Load.NIFTIhdr.cal_max) and (lImg2Load.NIFTIhdr.cal_min > lImg2Load.GlMinUnscaledS) and (lImg2Load.NIFTIhdr.cal_max < lImg2Load.GlMaxUnscaledS) then begin
	  lImg2Load.WindowScaledMin := lImg2Load.NIFTIhdr.cal_min;
	  lImg2Load.WindowScaledMax := lImg2Load.NIFTIhdr.cal_max;

	  lImg2Load.AutoBalMinUnscaled := lImg2Load.WindowScaledMin;
	  lImg2Load.AutoBalMaxUnscaled := lImg2Load.WindowScaledMax;
  end;

  if (lParsename = 'ch2bet') or (lParseName = 'ch2better') then begin
	  lImg2Load.WindowScaledMin := 45;
	  lImg2Load.WindowScaledMax := 120;

	  lImg2Load.AutoBalMinUnscaled := lImg2Load.WindowScaledMin;
	  lImg2Load.AutoBalMaxUnscaled := lImg2Load.WindowScaledMax;
  end;
  if lParseName = 'ch2' then begin
	  lImg2Load.WindowScaledMin := 30;
	  lImg2Load.WindowScaledMax := 120;
	  lImg2Load.AutoBalMinUnscaled := lImg2Load.WindowScaledMin;
	  lImg2Load.AutoBalMaxUnscaled := lImg2Load.WindowScaledMax;
  end;
  //Next: create screen buffer [scaled to background]
  //if not l4D then //12/2007: do not create screen buffer for 4D load! saves memory and time
  //   RescaleImgIntensity (lBackgroundImg,lImg2Load);
  if not l4D then begin//12/2007: do not create screen buffer for 4D load! saves memory and time
     if lLoadBackground then
      RescaleImgIntensity (lBackgroundImg,lImg2Load,kBGOverlayNum)
     else
      RescaleImgIntensity (lBackgroundImg,lImg2Load,kVOIOverlayNum);
  end;
  if (lVOILoadAsBinary) and (lImg2Load.ScrnBufferItems> 0) then begin
     if lImg2Load.NIFTIhdr.intent_name[1] = 'I' then //indexed
		showmessage('Indexed drawing - assuming drawing is binary. You may want to upgrade this software.');
	  gBGImg.VOIchanged := false;
	  for lInc := 1 to lImg2Load.ScrnBufferItems do
		if lImg2Load.ScrnBuffer^[lInc] > 1 then
			lImg2Load.ScrnBuffer^[lInc] := kVOI8bit;
	  lMaxI := maxint;
	  LoadMonochromeLUT(lMaxi,lBackgroundImg,lImg2Load);
	  if lImg2Load.ImgBufferItems > 1 then
		freemem(lImg2Load.ImgBufferUnaligned);
		lImg2Load.ImgBufferItems := 0;
  end else begin
	ImgForm.LayerDropSelect(nil);
	ImgForm.LUTdropSelect(nil);
  end;
  result := true;
456:
  CloseFile(F);
  {$I+}
  FileMode := 2;
end; //proc OpenImg

end.
