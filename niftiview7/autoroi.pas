unit autoroi;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, RXSpin,define_types, ExtCtrls,nifti_img,nifti_img_view,
  Mask;

type
  TAutoROIForm = class(TForm)
    OriginLabel: TLabel;
    OriginBtn: TSpeedButton;
    VarianceEdit: TRxSpinEdit;
    DiffLabel: TLabel;
    EdgeEdit: TRxSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    ErodeEdit: TRxSpinEdit;
    Label3: TLabel;
    AutoROIBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    Timer1: TTimer;
    Label4: TLabel;
    RadiusEdit: TRxSpinEdit;
    ExcludeBlackCheck: TCheckBox;
    ROIconstraint: TComboBox;
	procedure OriginBtnClick(Sender: TObject);
	procedure PreviewBtnClick(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure FormCreate(Sender: TObject);
	procedure FormHide(Sender: TObject);
	procedure AutoROIBtnClick(Sender: TObject);
	procedure CancelBtnClick(Sender: TObject);
	procedure AutoROIchange(Sender: TObject);
	procedure Timer1Timer(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
  private
	{ Private declarations }
  public
	{ Public declarations }
  end;

procedure ROICluster ({lInROIBuf: bytep;} lXdim, lYDim, lZDim,lXOriginIn,lYOrigin,lZOrigin: integer; lDeleteNotFill: boolean);
var
  AutoROIForm: TAutoROIForm;
  //gImageBackupSz,
  gOriginX,gOriginY,gOriginZ: integer;
  //gImageBackupBuffer: bytep;
implementation

//uses MRImain;

{$R *.DFM}

procedure TAutoROIForm.OriginBtnClick(Sender: TObject);
begin
 gOriginX := ImgForm.XViewEdit.asInteger;
 gOriginY := ImgForm.YViewEdit.asInteger;
 gOriginZ := ImgForm.ZViewEdit.asInteger;
 OriginLabel.Caption := 'Origin: '+inttostr(gOriginX)+'x'+inttostr(gOriginY)+'x'+inttostr(gOriginZ);
 PreviewBtnClick(sender);
end;

procedure TAutoROIForm.PreviewBtnClick(Sender: TObject);
var
   lXmm,lYmm,lZmm,lSqrRadius: single;
   lExcludeBlackIfZero,//lX,lY,lZ, //abba
   {lMaxROISz,}lEdge,lOriginPos,lROISz,lOriginIntensity,lVariance,lXdim, lYDim, lZDim: integer;
   lErodeCycles,lQTail,lQHead,lSliceSz,lQSz,lInc,lVolSz{,lX,lY,lZ}: integer;
   lROIConstrain,lReadFilteredData: boolean;
   lQra: LongIntP;
   lSourceBuffer,lBuff,lPreErodeBuff: ByteP;
const
	 kFillValue = -2;
Procedure IncQra(var lVal, lQSz: integer);
begin
	inc(lVal);
	if lVal >= lQSz then
	 lVal := 1;
end;
function UnsmoothedIntensity(lPixel: integer): integer; //1381
begin
	  if lReadFilteredData then
		 result := lBuff[lPixel]
	  else
		  Result :=lSourceBuffer[lPixel];
end;

 function MeanIntensity(lPixel: integer): integer;
 var lV: integer;
 begin
	  if lReadFilteredData then
		 result := lBuff[lPixel]
	  else if ((lPixel-lSliceSz) > 0) and ((lPixel+lSliceSz) <= lVolSz) then begin
		 lV :=lSourceBuffer[lPixel]+lSourceBuffer[lPixel+1]+lSourceBuffer[lPixel-1] //L/R
			+lSourceBuffer[lPixel+lXdim]+lSourceBuffer[lPixel-lXdim] //Anterior/Posterior
			+lSourceBuffer[lPixel+lSliceSz]+lSourceBuffer[lPixel-lSliceSz]; //Dorsal/Ventral
		 result := lV div 7;
	  end else result := lSourceBuffer[lPixel];//1401 gImageBackupBuffer[lPixel]
 end;
 procedure Check(lPixel,lIntensity: integer);
 var lSmoothInten :integer;
 begin
   //xxxxxxxxxxxxxx   lSmoothInten := MeanIntensity(lPixel);
   lSmoothInten := UnsmoothedIntensity(lPixel);
   if (lROIConstrain) and (gBGImg.VOIUndoVol[lPixel] > 0) then //1410
	 //constrain
   else if (lBuff[lPixel]<> 255) and (UnsmoothedIntensity(lPixel) > lExcludeBlackIfZero {1381}) and  (abs(lSmoothInten-lIntensity)<=lEdge) and(abs(lSmoothInten-lOriginIntensity)<=lVariance) {}then begin//add item
		incQra(lQHead,lQSz);
		inc(lROISz);
		lBuff[lPixel] := 255;
		lQra[lQHead] := lPixel;
   end;
 end;

PROCEDURE RetirePixel; //FIFO cleanup
function WithinRadius(lXs,lYs,lZs:integer): boolean;
begin
	 if (sqr((lXs-gOriginX)*lXmm)+sqr((lYs-gOriginY)*lYmm)+sqr((lZs-gOriginZ)*lZmm)) > lSqrRadius then
		result := false
	 else
		 result := true;
end;
VAR
   lVal,lXPos,lYPos,lZPos,lIntensity: integer;
BEGIN
   lVal := lQra[lQTail];
   lXpos := lVal mod lXdim;
   if lXpos = 0 then lXPos := lXdim;

   lYpos := (1+((lVal-1) div lXdim)) mod lYDim;
   if lYPos = 0 then lYPos := lYdim;

   lZpos := ((lVal-1) div lSliceSz)+1;
   if lReadFilteredData then
	  lIntensity := 128
   else
	   lIntensity := lSourceBuffer[lVal];//1401 gImageBackupBuffer[lVal];
   if (lXpos > 1) and WithinRadius(lXpos-1,lYpos,lZpos) then Check(lVal -1,lIntensity);//check to left
   if (lXPos < lXDim) and (WithinRadius(lXpos+1,lYpos,lZpos)) then Check(lVal + 1,lIntensity); //check to right
   if (lYpos > 1) and (WithinRadius(lXpos,lYpos-1,lZpos)) then Check(lVal -lXdim,lIntensity);//check previous line
   if (lYPos < lYDim) and (WithinRadius(lXpos,lYpos+1,lZpos)) then Check(lVal + lXdim,lIntensity); //check next line
   if (lZpos > 1) and (WithinRadius(lXpos,lYpos,lZpos-1)) then Check(lVal -lSliceSz,lIntensity);//check previous slice
   if (lZPos < lZDim) and (WithinRadius(lXpos,lYpos,lZpos+1)) then Check(lVal + lSliceSz,lIntensity); //check next slice
   incQra(lQTail,lQSz); //done with this pixel
END;

procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
var lI: integer;
begin
  for lI := 1 to lQsz do
	  lQra[lI] := 0;
  lQHead := 0;
  lQTail := 1;
  lROISz := 0;
  Check(lPt,lOriginIntensity);
  RetirePixel;
  while ((lQHead+1) <> lQTail) do begin//complete until all voxels in buffer have been tested
		RetirePixel;
		if (lQHead = lQSz) and (lQTail = 1) then
		   exit; //break condition: avoids possible infinite loop where QTail is being incremented but QHead is stuck at maximum value
  end;
end;

function ROIOnEdge (lVal: integer): boolean;
BEGIN
   result := false;
   if lBuff[lVal] <> 255 then exit; //not ROI - is not boundary
   //Find
   if ((lVal-lSliceSz) > 0) and ((lVal+lSliceSz) <= lVolSz) then begin
			if lBuff[lVal+1] = 0 then result := true;
			if lBuff[lVal-1] = 0 then result := true;
			if lBuff[lVal+lXdim] = 0 then result := true;
			if lBuff[lVal-lXdim] = 0 then result := true;
			if lBuff[lVal+lSliceSz] = 0 then result := true;
			if lBuff[lVal-lSliceSz] = 0 then result := true;
   end;
end;

function ZeroOnEdge (lVal: integer): boolean;
BEGIN
   result := false;
   if lBuff[lVal] <> 0 then exit; //not ROI - is not boundary
   //Find
   if ((lVal-lSliceSz) > 0) and ((lVal+lSliceSz) <= lVolSz) then begin
			if lBuff[lVal+1] = 255 then result := true;
			if lBuff[lVal-1] = 255 then result := true;
			if lBuff[lVal+lXdim] = 255 then result := true;
			if lBuff[lVal-lXdim] = 255 then result := true;
			if lBuff[lVal+lSliceSz] = 255 then result := true;
			if lBuff[lVal-lSliceSz] = 255 then result := true;
   end;
end;

begin                                                       //alfa666
	 if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems<1) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems<>gBGImg.VOIUndoVolItems) then exit;
	 //if gImageBackupSz <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then
	 //UpdateBackupBuffer;
	 lXdim := gBGImg.ScrnDim[1];
	 lYDim := gBGImg.ScrnDim[2];
	 lZDim := gBGImg.ScrnDim[3];
	 if (gBGImg.Scrnmm[1] = 0) or (gBGImg.Scrnmm[2]=0) or (gBGImg.Scrnmm[3]=0) then begin
		 lXmm := 1;
		 lYmm := 1;
		 lZmm := 1;
	 end else begin
		 lXmm := gBGImg.Scrnmm[1];
		 lYmm := gBGImg.Scrnmm[2];
		 lZmm := gBGImg.Scrnmm[3];
	 end;
	 lSliceSz := lXdim * lYdim;
	 lVolSz := lSliceSz*lZdim;
	 //lMaxROISz := round(PctImg.Value/100 * lVolSz);
	 lOriginPos := gOriginX + ((gOriginY-1)*lXdim) + ((gOriginZ-1)*lSliceSz);
	 if (lOriginPos < 1) or (lVolSz <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems) or (lOriginPos > lVolSz) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems <> gBGImg.VOIUndoVolItems) then
		exit;
	 lVariance := AutoROIForm.VarianceEdit.asinteger;
	 lEdge := AutoROIForm.EdgeEdit.asinteger;
	 lSqrRadius := sqr(AutoROIForm.RadiusEdit.asinteger);
	 if (lXDim < 4) or (lYDim < 4) or (lZDim < 4) or (lVolSz < 1)  then exit;
	 lSourceBuffer := gMRIcroOverlay[kBGOverlayNum].ScrnBuffer;//gBuffer;
	 //Next - START count cluster size
	 lQSz := (lVolSz div 4)+8;
	 GetMem(lQra,lQsz * sizeof(longint) );
	 //check positive clusters....
	 Getmem(lBuff,lVolSz);
	 FillChar(lBuff^,lVolSz, 0);
	 //Move(gImageBackupBuffer^,lBuff^,lVolSz);
	 if ExcludeBlackCheck.checked then //1381
	   lExcludeBlackIfZero := 0 //0
	 else
		 lExcludeBlackIfZero := -1;//impossible 8-bit value: do not use this feature
	 lOriginIntensity := lSourceBuffer[lOriginPos]; //1401 gImageBackupBuffer[lOriginPos];
	 lReadFilteredData := false;
	 //ROIconstrainCheck.enabled := (gROIBupSz > 1); //1410: next 3 lines
	 ROIconstraint.enabled := (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 1); //1410: next 3 lines
	 if (ROIconstraint.ItemIndex = 2) and (ROIconstraint.enabled) then
		lROIConstrain := true
	 else
		 lROIconstrain := false;
	 FillStart(lOriginPos);
	 lROIConstrain := false;//1410
	 //START: ERODE/DILATE CYCLES
	 lErodeCycles :=  AutoROIForm.ErodeEdit.asinteger;
	 if lErodeCycles > 0 then begin
		Getmem(lPreErodeBuff,lVolSz);
		Move(lBuff^,lPreErodeBuff^,lVolSz);
		for lQHead := 1 to lErodeCycles do begin//ERODE
			for lInc := 1 to lVolSz do
				if ROIonEdge(lInc) then
				   lBuff[lInc] :=254;
			for lInc := 1 to lVolSz do
				if lBuff[lInc]=254 then
				   lBuff[lInc] := 0; //erode
		end;//for ErodeCycles = ERODE
		//SET ALL VOXELS THAT HAVE SURVIVED EROSION TO 128, WE THEN GROW THE ORIGIN
		for lInc := 1 to lVolSz do
			if lBuff[lInc] =255 then lBuff[lInc] := 128;
		//NOW - ONLY PRESERVE STUFF CONNECTED TO ORIGIN
		lBuff[lOriginPos] := 128;
		lOriginIntensity := 128;
		lVariance := 2;
		lEdge := 2;
		lReadFilteredData := true;
		FillStart(lOriginPos);
		//SWITCH OFF ALL UNCONNECTED BLOBS
		for lInc := 1 to lVolSz do
			if lBuff[lInc] =128 then lBuff[lInc] := 0;
		//for lInc := 1 to lVolSz do
		//    if lBuff[lInc] > 0 then showmessage(inttostr(lBuff[lInc]));// := 0;

		for lQHead := 1 to lErodeCycles{GrowEdit.asinteger} do begin//DILATE
			for lInc := 1 to lVolSz do
				if (lPreErodeBuff[lInc] = 255) and (ZeroonEdge(lInc)) then
				   lBuff[lInc] :=254;
			for lInc := 1 to lVolSz do
				if lBuff[lInc]=254 then
				   lBuff[lInc] := 255; //erode
		end;//for ErodeCycles = DILATE
	  Freemem(lPreErodeBuff);
	  {}
	 end; //ERODE cycles > 0
	 //END: ERODE/DILATE
	 Freemem(lQra);
	 ROIconstraint.enabled := (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 1); //1410: next 3 lines
	 if (ROIconstraint.ItemIndex = 1) and (ROIconstraint.enabled) then begin //delete ROI
		for lInc := 1 to gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems do //gROIBup
			if (lBuff[lInc] = 255) then
			   lBuff[lInc] := 0
			else
			   lBuff[lInc] := gBGImg.VOIUndoVol[lInc];
	 end else (*if true {alfa (gDynSz > 1) and (gROIBupsz > 1) {and (gImageBackupSz = gDynSz){} then begin
		for lInc := 1 to gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems do
			if lBuff[lInc] = 255 then
			else if gImageBackupBuffer[lInc] = 255 then
				 lBuff[lInc] := 255//255;
			else lBuff[lInc] := lSourceBuffer[lInc];

	 end else *)
	   for lInc := 1 to lVolSz do
		 if lBuff[lInc] <> 255 then
			lBuff[lInc] := gBGImg.VOIUndoVol[lInc]
		 else
			lBuff[lInc] := kVOI8bit;//1401 gImageBackupBuffer[lInc];
	 Move(lBuff^,gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,lVolSz);
	 Freemem(lBuff);
	 //END check clusters
	 ImgForm.RefreshImagesTimer.Enabled := true;
end;

procedure TAutoROIForm.FormShow(Sender: TObject);
begin
EnsureVOIOpen;
CreateUndoVol;
	AutoROIForm.ModalResult := mrCancel;
	ROIconstraint.Enabled := (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 1);
	OriginBtn.OnClick(sender);
	 //DeleteCheck.enabled := (gROIBupSz > 1);
	 //ROIConstrainCheck.enabled := (gROIBupSz > 1);
end;

procedure TAutoROIForm.FormCreate(Sender: TObject);
begin
	 //gImageBackupSz := 0;
	 ROIconstraint.ItemIndex :=(0);//1410
end;

procedure TAutoROIForm.FormHide(Sender: TObject);
begin
//	 if (AutoROIForm.ModalResult = mrCancel) and (gBGImg.VOIUndoVolItems > 1) and (gBGImg.VOIUndoVolItems = gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems) then
//		Move(gImageBackupBuffer^,gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gImageBackupSz);
	 if (AutoROIForm.ModalResult = mrCancel) then
		UndoVolVOI;
	 if not (AutoROIForm.ModalResult = mrCancel) then
		gBGImg.VOIchanged := true;
	 //if gImageBackupSz <> 0 then Freemem(gImageBackupBuffer);
	 //gImageBackupSz := 0;
	 ImgForm.Fill3DBtn.Down := false;
	 ImgForm.RefreshImagesTimer.Enabled := true;
end;

//Previous: create 3D ROI
//Below fill bubbles in 3D ROIS
//ROIcluster Follows
(***********************************************************88
************************************************************
**********************************************************)
procedure ROICluster (lXdim, lYDim, lZDim,lXOriginIn,lYOrigin,lZOrigin: integer; lDeleteNotFill: boolean);
var
  lVariability,lOrigin,lClusterInputValue,lClusterOutputValue, lClusterSz,lQTail,
  lXOrigin,lQHead,lSliceSz,lQSz,lInc,lVolSz: integer;
  lXInc,lYInc,lZInc,lSlicePos,lYPos,
  lMinX,lMaxX,lMinY,lMaxY,lMinZ,lMaxZ,
  lMinXBound,lMaxXBound,lMinYBound,lMaxYBound,lMinZBound,lMaxZBound: integer;
  lAtEdge: boolean;
  lROIBuf: bytep;
  lQra: LongIntP;
const
     kFillValue = -2;
Procedure IncQra(var lVal, lQSz: integer);
begin
    inc(lVal);
    if lVal >= lQSz then
     lVal := 1;
end;

 procedure Check(lPixel: integer);
 begin
    if (abs(lROIBuf[lPixel] - lClusterInputValue)) <= lVariability then begin//add item
        incQra(lQHead,lQSz);
        inc(lClusterSz);
		lROIBuf[lPixel] := lClusterOutputValue;
        lQra[lQHead] := lPixel;
   end;
 end;

PROCEDURE RetirePixel; //FIFO cleanup
VAR
   lVal,lXPos,lYPos,lZPos: integer;
BEGIN
   lVal := lQra[lQTail];
   lXpos := lVal mod lXdim;
   if lXpos = 0 then lXPos := lXdim;

   lYpos := (1+((lVal-1) div lXdim)) mod lYDim;
   if lYPos = 0 then lYPos := lYdim;

   lZpos := ((lVal-1) div lSliceSz)+1;

   if lXPos < lMinX then lMinX := lXPos;
   if lXPos > lMaxX then lMaxX := lXPos;
   if lXpos > lMinXBound then Check(lVal -1);//check to left
   if lXPos < lMaxXBound then Check(lVal + 1); //check to right

   if lYPos < lMinY then lMinY := lYPos;
   if lYPos > lMaxY then lMaxY := lYPos;
   if lYpos > lMinYBound then Check(lVal -lXdim);//check previous line
   if lYPos < lMaxYBound then Check(lVal + lXdim); //check next line

   if lZPos < lMinZ then lMinZ := lZPos;
   if lZPos > lMaxZ then lMaxZ := lZPos;
   if lZpos > lMinZBound then Check(lVal -lSliceSz);//check previous slice
   if lZPos < lMaxZBound then Check(lVal + lSliceSz); //check next slice

   incQra(lQTail,lQSz); //done with this pixel
END;

procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
var lI: integer;
begin
  //1414 follows
  for lI := 1 to lQsz do
      lQra[lI] := 0;
  lQHead := 0;
  lQTail := 1;
  Check(lPt);
  RetirePixel;
  while ((lQHead+1) <> lQTail) do begin//complete until all voxels in buffer have been tested
        RetirePixel;
		if (lQHead = lQSz) and (lQTail = 1) then
           exit; //break condition: avoids possible infinite loop where QTail is being incremented but QHead is stuck at maximum value
  end;
end;

procedure SelectClusters (lInput,lOutput: integer);
begin
     lClusterSz := 0;
     lClusterInputValue := lInput;
     lClusterOutputValue := lOutput;
	 FillStart(lOrigin);
end;

function Lo (lVolumeEdge,lObjectEdge: integer): integer;
begin
    if lVolumeEdge > lObjectEdge then
       result := lObjectEdge
    else begin
        lAtEdge := true;
        result := lVolumeEdge;
    end;
end;

function Hi (lVolumeEdge,lObjectEdge: integer): integer;
begin
    if lVolumeEdge < lObjectEdge then
       result := lObjectEdge
    else begin
        lAtEdge := true;
        result := lVolumeEdge;
    end;
end;

begin
	 lXOrigin := lXOriginIn;
	 lVolSz := lXdim*lYdim*lZdim;
	 if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems <> lVolSz then begin
		 showmessage('You need to draw or load a VOI in order to use the 3D bubble tool.');
		 exit;
	 end;
	 CreateUndoVol;
     lSliceSz := lXdim * lYdim;
     lMinX:=lXOrigin;
     lMaxX:=lXOrigin;
     lMinY:=lYOrigin;
     lMaxY:=lYOrigin;
     lMinZ:=lZOrigin;
     lMaxZ:=lZOrigin;
   lMinXBound := 1;
   lMaxXBound := lXDim;
   lMinYBound := 1;
   lMaxYBound := lYDim;
   lMinZBound := 1;
   lMaxZBound := lZDim;
     lOrigin := lXOrigin + ((lYOrigin-1)*lXdim)+((lZOrigin-1)*lSliceSz);
	 if (lOrigin > lVolSz) or (lXDim < 4) or (lYDim < 4) or (lZDim < 4) or (lVolSz < 1) {or (gROIBupSz <> lVolSz )} then exit;
	 if (gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lOrigin] = 0) then begin
        showmessage('You must click directly on a ROI to select it. The 3D ROI bubble tool will not work unless you choose the ROI you wish to fill/delete.');
        exit;
	 end;
     GetMem(lROIBuf, lVolSz);
	 for lInc := 1 to lVolSz do
		 if gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lInc] > 0 then//ROI
            lROIBuf[lInc] := 1
         else
             lROIBuf[lInc] := 0;
     //BEGIN: define selected ROI contiguous cluster
     lQSz := (lVolSz div 4)+8;
     GetMem(lQra,lQsz * sizeof(longint) );
     lVariability := 0; //only convert images that are exactly 1
     SelectClusters(1,255); //selected 3D ROI is 255, other ROI = 1, nonROI 0
     //END: define selected roi
     //BEGIN: either delete selected ROI, _OR_ fill bubbles in selected ROI
     if lDeleteNotFill then begin
	   for lInc := 1 to lVolSz do
		 if lROIBuf[lInc] = 1 then    //alfa
			lROIBuf[lInc] := gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lInc] //a different ROI
		 else
			lROIBuf[lInc] := 0;//gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lInc]; //1402 selected ROI or non-ROI
	 end else begin //fill bubbles in selected ROI
		 //FindROIbounds;
   lMinXBound := Hi(1,lMinX-1);
   lMaxXBound := Lo(lXDim,lMaxX+1);
   lMinYBound := Hi(1,lMinY-1);
   lMaxYBound := Lo(lYDim,lMaxY+1);
   lMinZBound := Hi(1,lMinZ-1);
   lMaxZBound := Lo(lZDim,lMaxZ+1);
	 lOrigin := (lMinXBound) + ((lMinYBound-1)*lXdim)+((lMinZBound-1)*lSliceSz);
          lVariability := 2;//convert voxels that are either 0 or 1 to 1
          SelectClusters(1,128);
          //now bubbles trapped in volume are set to zero
          //we next need to distinguish bubbles from unmarked voxels outside the searched object boundary
          for lZInc := lMinZBound to lMaxZBound do begin
              lSlicePos := (lZInc-1) * lSliceSz;
              for lYInc := lMinYBound to lMaxYBound do begin
                  lYPos := (lYInc-1) * lXDim;
                  for lXInc := lMinXBound to lMaxXBound do begin
                      lInc :=  lXInc + lYPos + lSlicePos;
                      if lROIBuf[lInc] = 0 then lROIBuf[lInc] := 33;
                  end; //for X
			  end; //for Y
          end; //for Z

          for lInc := 1 to lVolSz do
			  if lROIBuf[lInc] = 33 then
				lROIBuf[lInc] := kVOI8bit //bubble in selected ROI
			  else
				lROIBuf[lInc] := gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lInc];
	 end;
     Freemem(lQra);
     //BEGIN: CREATE 3D UNDO BUFFER
	 (*if (gDynSz > 1) and (gDynSz = gImageBackupSz) then begin
        if (gUndoBufSz > 0) then freemem(gUndoBuffer);
        gUndoBufSz := gDynSz;
        getmem(gUndoBuffer,gDynSz);
        Move(gImageBackupBuffer^,gUndoBuffer^,gImageBackupSz);
        gSaveUndoBuf := true;
	 end;   (**)
     //END: CREATE 3D UNDO BUFFER
     //BEGIN: mopping up: prepare data for viewing, report ROI change
	 Move(lROIBuf^,gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,lVolSz);
	 Freemem(lROIBuf);  {}
	gBGImg.VOIchanged := true;
	 //END: mopping up
	ImgForm.RefreshImagesTimer.enabled := true;
end;  (**)

procedure TAutoROIForm.AutoROIBtnClick(Sender: TObject);
begin
	AutoROIForm.ModalResult := mrOK;
	AutoROIForm.close;
end;

procedure TAutoROIForm.CancelBtnClick(Sender: TObject);
begin
	 AutoROIForm.close;
end;

procedure TAutoROIForm.AutoROIchange(Sender: TObject);
begin
     if not AutoROIForm.visible then exit;
     Timer1.Enabled := true;
end;

procedure TAutoROIForm.Timer1Timer(Sender: TObject);
begin
Timer1.Enabled := false;
PreviewBtnClick(sender);
end;

procedure TAutoROIForm.FormDestroy(Sender: TObject);
begin
	 //if gImageBackupSz <> 0 then Freemem(gImageBackupBuffer);
     //gImageBackupSz := 0;
end;

end.
