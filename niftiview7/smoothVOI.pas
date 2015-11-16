unit smoothVOI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin, Buttons,define_types,nifti_img_view,nifti_img, Mask;

type
  TSmoothVOIForm = class(TForm)
    Label37: TLabel;
    Label38: TLabel;
    CancelBtn: TSpeedButton;
    OKBtn: TSpeedButton;
    SpeedButton5: TSpeedButton;
    XROIthresh: TRxSpinEdit;
    XROIfwhm: TRxSpinEdit;
    ScaleSides: TComboBox;
    xROIoutput: TComboBox;
    procedure SpeedButton5Click(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
	procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
	procedure SmoothOpenVOI(Sender: TObject);
        procedure SmoothVOI_SPM5masks;
  private
	{ Private declarations }
  public
	{ Public declarations }
  end;

var
  SmoothVOIForm: TSmoothVOIForm;

implementation

{$R *.DFM}

procedure TSmoothVOIForm.SpeedButton5Click(Sender: TObject);
begin
     Showmessage('The Full Width Half Maximum [FWHM] defines the width of the smoothing Gaussian. '+
     'The threshold defines a binary cutoff boundary - signals greater than the threshold will be included in the output. '+
     'A threshold of 0 will create an continuous 8-bit output (0..200 for signal 0..1)');
end;

procedure TSmoothVOIForm.CloseBtnClick(Sender: TObject);
begin
	 if (Sender as TSpeedButton).tag = 1 then
		SmoothOpenVOI(Sender);
	 SmoothVOIForm.Close;
end;

procedure TSmoothVOIForm.FormShow(Sender: TObject);
begin
	 SmoothVOIForm.ModalResult := mrCancel;

end;

procedure VOIinvert;
var
  lI,lImgSz: integer;
begin
  lImgSz := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
  if lImgSz < 1 then exit;
  CreateUndoVol;
  Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVol^,gBGImg.VOIUndoVolItems);
	for lI := 1 to lImgSz do
    if gBGImg.VOIUndoVol^[lI] = 0 then
      gBGImg.VOIUndoVol^[lI] := 1
    else
      gBGImg.VOIUndoVol^[lI] := 0;
		gBGImg.VOIchanged := true;
		ImgForm.Undo1Click(nil); //show smoothed buffer
end;

procedure TSmoothVOIForm.SmoothVOI_SPM5masks;
var
   lBGname,lmaskname,llesionname: string;
   lorigFWHM ,
   lorigThresh : single;
   lorigSS,lOrigOut: integer;
begin
   if not IsVOIOpen then begin
      ShowMessage('You have not created or opened a region of interest.');
	    exit;
   end;
   lBGname := gMRIcroOverlay[kBGOverlayNum].HdrFileName;
   if not gMRIcroOverlay[kBGOverlayNum].NIfTItransform then begin
       //need to save BG as NIfTI
       lBGName := ChangeFilePrefix(lBGname,'x');
      SaveAsVOIorNIFTIcore(lBGName,gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems,gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
   end;
   lmaskname := ChangeFilePrefix(lBGname,'m');
   lmaskname := changefileextx(lmaskname, '.nii');
   llesionname := ChangeFilePrefix(lBGname,'l');
   llesionname := changefileextx(llesionname, '.nii');
   if (fileexists(lmaskname)) or (fileexists(llesionname)) then begin
       showmessage ('Files already exist named '+lmaskname+'  '+llesionname);
       exit;
   end;
      //init
      lorigFWHM := XROIfwhm.value;
      lorigThresh := XROIthresh.value;
      lorigSS := SmoothVOIForm.ScaleSides.itemindex;
      lorigOut := xROIoutput.itemindex;
      //compute mask
      XROIfwhm.value := gBGImg.LesionDilate;
      XROIthresh.value := 0.001;
      ScaleSides.itemindex:=(1);
      xROIoutput.itemindex:=(1);
      if gBGImg.LesionDilate <= 0 then
        VOIinvert
      else
        SmoothOpenVOI(nil);
      if (gBGImg.VOIUndoSlice < 1) or (gBGImg.VOIUndoOrient <> 4) then begin //should be impossible - smoothVOI creates undovol
         showmessage('Serious error.');
         exit;
      end;
      ImgForm.StatusLabel.caption := 'Saving mask as '+lmaskname;
          gMRIcroOverlay[kVOIOverlayNum].HdrFileName := lmaskname;
          ImgForm.SaveVOIcore(false);//12/2010  //unmirrors image
      //xx SaveAsVOIorNIFTIcore (lmaskname, gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
      //compute lesion
      UndoVolVOI;

      XROIfwhm.value := gBGImg.LesionSmooth;
      XROIthresh.value := 0.5;
      //ScaleSides.setitemindex(0);
      xROIoutput.itemindex:=(0);
      SmoothOpenVOI(nil);
          gMRIcroOverlay[kVOIOverlayNum].HdrFileName := llesionname;
          ImgForm.SaveVOIcore(false);//12/2010  //unmirrors image
          gMRIcroOverlay[kVOIOverlayNum].HdrFileName := lmaskname;

      //SaveAsVOIorNIFTIcore (llesionname, gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);

      //re-init
      UndoVolVOI;

      XROIfwhm.value := lorigFWHM;
      XROIthresh.value := lorigThresh;
      ScaleSides.itemindex:=(lOrigSS);
      xROIoutput.itemindex:=(lOrigOut);
end;

procedure TSmoothVOIForm.SmoothOpenVOI(Sender: TObject);
//label
//	 123;
var
   lScaleXY,lOK,lResliceNotMask: boolean;
   lZXra,lYOutra,lROIrealRA: SingleP;
   lIncX,lSliceZ,
   lSlicePos,lMissing,lZPos,lYPos,lSliceSz,lXt,lYt,lZt,lX,lY,lZ,lXoffset,lYOffset,lZOffset,lI,lI2,lImgSz,lcutoffvoxx,lcutoffvoxy,lcutoffvoxz: integer;
   lScale,lThreshComp,lExpd,lThresh,lXVar,lSig,lXmm,lYmm,lZmm,lcumgauss: single;//double;
   lxra,lyra,lzra,lzraScaled,lxCutra,lyCutra,lzCutra:SingleP0;//x0P;
   lStartTime,lEndTime: DWord;
   lXDim,lYDim,lZDim,lPlanes,lMinX,lMaxX,lMinY,lMaxY,lMinZ,lMaxZ: integer;
begin
   lXDim := gBGImg.ScrnDim[1];
   lYDim := gBGImg.ScrnDim[2];
   lZDim := gBGImg.ScrnDim[3];
   lXmm := gBGImg.ScrnMM[1];
   lYmm := gBGImg.ScrnMM[2];
   lZmm := gBGImg.ScrnMM[3];
   lResliceNotMask := false;
   if not IsVOIOpen then begin
	 ShowMessage('You have not created or opened a region of interest.');
	 exit;
   end;
   if (gBGImg.ScrnMM[1] = 0) or (lXmm = 0) or (lYmm = 0) or (lZmm =0) or (SmoothVOIForm.XROIfwhm.value=0) then begin
	 ShowMessage('At least one of the images ''size [mm]'' settings or the ''FWHM [mm]'' is zero. Smoothing requires the image size to be specified.');
	 Exit;
   end;
   if SmoothVOIForm.ScaleSides.itemindex = 1 then
	lScaleXY := true
   else
	lScaleXY := false;
   lOK := true;
   if lScaleXY then begin
	 lsig  := (SmoothVOIForm.XROIfwhm.value / lXmm)/sqrt(8*ln(2));  //      % FWHM -> sigma
	 lcutoffvoxX  := round(6*lsig);
	 if (lcutoffvoxX *2) >= lXdim then lOK := false;
	 lsig  := (SmoothVOIForm.XROIfwhm.value / lYmm)/sqrt(8*ln(2));  //      % FWHM -> sigma
	 lcutoffvoxY  := round(6*lsig);
	 if (lcutoffvoxY *2) >= lYdim then lOK := false;
   end; {scaleXY}
   lsig  := (SmoothVOIForm.XROIfwhm.value / lZmm)/sqrt(8*ln(2));  //      % FWHM -> sigma
   lcutoffvoxZ  := round(6*lsig);
   if (lcutoffvoxZ *2) >= lZdim then lOK := false;
   if not lOK then begin
		   showmessage('Unable to smooth image: image dimensions are too small for such a broad smoothing. Reduce the FWHM');
		   exit;
   end;
   if SmoothVOIForm.xROIoutput.itemindex <> 1 then
	 lResliceNotMask := true;
   lImgSz := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
   if lImgSz < 1 then exit;
   CreateUndoVol;//create gBGImg.VOIUndoVol
   Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVol^,gBGImg.VOIUndoVolItems);
   if lResliceNotMask then begin //reslice
	for lI := 1 to lImgSz do
		if gBGImg.VOIUndoVol[lI] <> 0 then
			gBGImg.VOIUndoVol[lI] := 1;
   end else begin //create mask: invert
	for lI := 1 to lImgSz do
		if gBGImg.VOIUndoVol[lI] = 0 then
			gBGImg.VOIUndoVol[lI] := 1
		else
			gBGImg.VOIUndoVol[lI] := 0;
  end; //create mask
  lSliceSz := lXdim * lYdim;
  //lZXi := lZdim*lXdim; //for swizzle
  lPlanes := 0;
  ImgForm.ProgressBar1.Position := 0;
  ImgForm.ProgressBar1.Min := 0;
  ImgForm.ProgressBar1.Max := lZdim * 3;
  ImgForm.StatusLabel.caption := 'Smoothing slice data: X-plane';
  lStartTime := GetTickCount;
  lThresh := SmoothVOIForm.XRoiThresh.value;
  lsig  := (SmoothVOIForm.XROIfwhm.value / lXmm)/sqrt(8*ln(2));  //      % FWHM -> sigma
  if lsig = 0 then begin
		   Showmessage('Unable to compute gaussian with current FWHM');
		   exit;
  end;
  lcutoffvoxx  := round(6*lsig);       //    % highest / lowest voxel to go out to
  getmem(lxra,(lcutoffvoxx+1)*sizeof(double {was extended}));
  getmem(lxCutra,(lcutoffvoxx+1)*sizeof(double {was extended}));
  lexpd := 2*lsig*lsig;
  lCumGauss := 0;
  for lI := 0 to lcutoffvoxx do begin
			lxra[lI] := exp(-1*(lI*lI)/lexpd) ;
			lCumGauss := lCumGauss + lxra[lI];
  end;
  lCumGauss := 2*lCumGauss - lxra[0];
  if lCumGauss <> 0 then
		   for lI := 0 to lcutoffvoxx do begin
			lxra[lI] := lxra[lI]/lCumGauss;
		   end;
  for lI := 1 to lcutoffvoxX do begin
			lCumGauss := 0;
			for lI2 := (lcutoffvoxX - lI) downto -lcutoffvoxX  do
				lCumGauss := lCumGauss + lXra[abs(lI2)];
			if lCumGauss <> 0 then
			   lXCutra[lI] := 1/lCumGauss;
  end;
  lXCutra[0] := 1;
  lsig  := (SmoothVOIForm.XROIfwhm.value / lYmm)/sqrt(8*ln(2));  //      % FWHM -> sigma
  if lsig = 0 then begin
		   Showmessage('Unable to compute gaussian with current FWHM');
		   exit;
  end;
  lcutoffvoxY  := round(6*lsig);       //    % highest / lowest voxel to go out to
  getmem(lYra,(lcutoffvoxY+1)*sizeof(double {was extended}));
  getmem(lYCutra,(lcutoffvoxY+1)*sizeof(double {was extended}));
  lexpd := 2*lsig*lsig;
  lCumGauss := 0;
  for lI := 0 to lcutoffvoxY do begin
			lYra[lI] := exp(-1*(lI*lI)/lexpd) ;
			lCumGauss := lCumGauss + lYra[lI];
  end;
  lCumGauss := 2*lCumGauss - lYra[0];
  if lCumGauss <> 0 then
		   for lI := 0 to lcutoffvoxY do begin
			lYra[lI] := lYra[lI]/lCumGauss;
		   end;

  for lI := 1 to lcutoffvoxY do begin
			lCumGauss := 0;
			for lI2 := (lcutoffvoxY - lI) downto -lcutoffvoxY  do
				lCumGauss := lCumGauss + lYra[abs(lI2)];
			if lCumGauss <> 0 then
			   lYCutra[lI] := 1/lCumGauss;
  end;
  lYCutra[0] := 1;(**)
  lsig  := (SmoothVOIForm.XROIfwhm.value / lZmm)/sqrt(8*ln(2));  //      % FWHM -> sigma
  if lsig = 0 then begin
		   Showmessage('Unable to compute gaussian with current FWHM');
		   exit;
  end;
  lcutoffvoxZ  := round(6*lsig);       //    % highest / lowest voxel to go out to
  getmem(lZra,(lcutoffvoxZ+1)*sizeof(double {was extended}));
  getmem(lZraScaled,(lcutoffvoxZ+lcutoffvoxZ+1)*sizeof(double {was extended}));
  getmem(lZCutra,(lcutoffvoxZ+1)*sizeof(double {was extended}));
  lexpd := 2*lsig*lsig;
  lCumGauss := 0;
  for lI := 0 to lcutoffvoxZ do begin
			lZra[lI] := exp(-1*(lI*lI)/lexpd );
			lCumGauss := lCumGauss + lZra[lI];
  end;
  lCumGauss := 2*lCumGauss - lZra[0];
  if lCumGauss <> 0 then
		   for lI := 0 to lcutoffvoxZ do begin
			lZra[lI] := lZra[lI]/lCumGauss;
		   end;
  for lI := 1 to lcutoffvoxZ do begin
			lCumGauss := 0;
			for lI2 := (lcutoffvoxZ - lI) downto -lcutoffvoxZ  do
				lCumGauss := lCumGauss + lZra[abs(lI2)];
			if lCumGauss <> 0 then
			   lZCutra[lI] := 1/lCumGauss;
  end;
  lZCutra[0] := 1;(**)
  GetMem ( lROIrealRA ,  sizeof(single)*lImgSz);
  GetMem (lYOutRA, sizeof(single) * lYdim);
  if lResliceNotMask then
	for lI := 1 to lImgSz do
		lROIrealRA[lI] := 0
	else
		for lI := 1 to lImgSz do
		   lROIrealRA[lI] := 1;
  //X-direction
  for lZ := 1 to lZdim do begin
			  lZPos := (lZ-1)*lSliceSz;
			  for lY := 1 to lYdim do begin
				  lyPos := (lY-1)*lXdim;
				  for lX := 1 to lXdim do begin
					  lMinX := lX - lCutoffVoxX;
					  if lMinX < 1 then lMinX := 1;
					  lMaxX := lX + lCutoffVoxX;
					  if lMaxX > lXdim then lMaxX := lXdim;
					  lMissing := (2*lCutOffVoxX)-(lMaxX-lMinX);
					  if lScaleXY then
						 lScale := lXCutRA[lMissing]
					  else
						  lScale := lXCutRA[0];
					  lCumGauss := 0;
					  for lXt := lMinX to lMaxX do begin
						  //SSE optimization?
						  if (gBGImg.VOIUndoVol[lXt+lYPos+lZpos] <> 0) then
							 lCumGauss := lCumGauss + lScale*lXra[abs(lX-lXt)] (*{kSmoothImg}*(gROIEXport[lXt+lYPos+lZpos]/255)*);
					  end; {for each position}
					  lROIrealRA[lX+lYPos+lZpos] := lCumGauss;
				  end; {lX}
			  end; {lY}

		   Application.ProcessMessages;
		   inc(lPlanes);
		   ImgForm.ProgressBar1.Position := lPLanes;
	   end; {lZ loop for X-plane}
	   ImgForm.StatusLabel.caption := 'Smoothing slice data: Y-plane';
	   for lZ := 1 to lZdim do begin {Z loop for Y plane}
			  lZPos := (lZ-1)*lSliceSz;
			  for lX := 1 to lXdim do begin
				  for lY := 1 to lYdim do begin
					  lMinY := lY - lCutoffVoxY;
					  if lMinY < 1 then lMinY := 1;
					  lMaxY := lY + lCutoffVoxY;
					  if lMaxY > lYdim then lMaxY := lYdim;
					  lMissing := (2*lCutOffVoxY)-(lMaxY-lMinY);
					  if lScaleXY then
						 lScale := lYCutRA[lMissing]
					  else
						  lScale := lYCutRA[0];
					  lCumGauss := 0;
					  for lYt := lMinY to lMaxY do begin
						  //SSE optimization?
						  lCumGauss := lCumGauss+ lScale*(lROIrealRA[lX+((lYt-1)*lXdim)+lZpos])*lYra[abs(lY-lYt)];
					  end; {for each position}
					  lYOutRA[lY] := lCumGauss;
				  end; {lY}
				  for lY := 1 to lYdim do begin
					 //SSE optimization
					  lROIrealRA[lX+((lY-1)*lXdim)+lZpos] := lYOutRA[lY];
				  end;
			  end; {lX}
		   Application.ProcessMessages;
		   inc(lPlanes);
		   ImgForm.ProgressBar1.Position := lPlanes;
	   end; {Z loop for Y plane}
	   (*if (not lScaleXY)  then begin
		  //lOrigZPos := (lFirstEmptySlice-1)*lSliceSz;
		  for lZ := lFirstEmptySlice to lZi do begin
			  if (lROIonSliceRA[lZ]=0) then begin
				 lZPos := (lZ-1)*lSliceSz;
				 for lX := 1 to lSliceSz do
					 //SSE optimization?
					 lROIrealRA[lX+lZPos] := lROIrealRA[lX+lOrigZPos];
				 Application.ProcessMessages;
			  end; {no ROI on this slice}
		  end; {for n slices}
	   end; {not scaled}  (**)
	   lThreshComp := 1 - lThresh;
	   ImgForm.StatusLabel.caption := 'Smoothing slice data: Z-plane';
	   lI := 0;
	   for lZ := 1 to lZdim do begin
		   lMinZ := lZ - lCutoffVoxZ;
		   if lMinZ < 1 then lMinZ := 1;
		   lMaxZ := lZ + lCutoffVoxZ;
		   if lMaxZ > lZdim then lMaxZ := lZdim;
		  lScale := 1;
		  lMissing := (2*lCutOffVoxZ)-(lMaxZ-lMinZ);
		   if (lMissing >= 0) and (lMissing <= lCutOffVoxZ) then
			  lScale := lZCutRA[lMissing];
		   if lThreshComp <> 1 then begin
if lResliceNotMask then begin
		   for lIncX := 1 to lcutoffvoxZ do
			   lZraScaled[lcutoffvoxZ-lIncX] := lZra[lIncX]*lScale;
		   for lIncX := 0 to lcutoffvoxZ do
			lZraScaled[lcutoffvoxZ+lIncX] := lZra[lIncX]*lScale;
		   lZOffset := lcutoffvoxZ + lZ;
		   for lY := 1 to lYdim do begin
				  lyPos := (lY-1)*lXdim;
				  for lX := 1 to lXdim do begin
					  lCumGauss := 0;
					  lIncX := ((lMinZ-1)*lSliceSz)+lX+lYPos;
					  for lZt := lMinZ to lMaxZ do begin
						  lCumGauss := lCumGauss + lROIrealRA[lIncX]*lZraScaled[(lZoffset-lZt)];
						  lIncX := lIncX+ lSliceSz
						  //SSE optimization
						  //lCumGauss := lCumGauss + lROIrealRA[lX+lYPos+(lZt-1)*lSliceSz]*lZra[abs(lZ-lZt)]*lScale;
					  end;
					  inc(lI);
					  if (lCumGauss < (1-lThreshComp)) then
						 gBGImg.VOIUndoVol[lI] := 100
					  else
						  gBGImg.VOIUndoVol[lI] := 0;
				  end; {lX}
		   end; {lY}
end else begin //this is a mask -> unrolled loop means faster processing
		   for lY := 1 to lYdim do begin
				  lyPos := (lY-1)*lXdim;
				  for lX := 1 to lXdim do begin
					  lCumGauss := 0;
					  for lZt := lMinZ to lMaxZ do
						  lCumGauss := lCumGauss + lROIrealRA[lX+lYPos+(lZt-1)*lSliceSz]*lZra[abs(lZ-lZt)]*lScale;
					  inc(lI);
					  if lCumGauss > lThreshComp then
						 gBGImg.VOIUndoVol[lI] := 0
					  else
						  gBGImg.VOIUndoVol[lI] := 100;
				  end; {lX}
		   end; {lY}
end;
		   end else begin //threshcomp = 1 analogua output
			   for lY := 1 to lYdim do begin
				  lyPos := (lY-1)*lXdim;
				  for lX := 1 to lXdim do begin
					  lCumGauss := 0;
					  for lZt := lMinZ to lMaxZ do
						  //SSE optimization?
						  lCumGauss := lCumGauss + lROIrealRA[lX+lYPos+(lZt-1)*lSliceSz]*lZra[abs(lZ-lZt)]*lScale;
					  inc(lI);
					  gBGImg.VOIUndoVol[lI] := round(200 * lCumGauss);
				  end; {lX}
			   end; {lY}
		   end; //threshcomp=1 analogue output
		   Application.ProcessMessages;
		   inc(lPlanes);
		   ImgForm.ProgressBar1.Position := lPlanes;
	   end; {lZ loop}
	  lEndTime := GetTickCOunt;
	   ImgForm.StatusLabel.caption :=('Smoothing time(ms): '+inttostr(lEndTime-lStartTime));
	   FreeMem (lROIrealRA);
	   FreeMem (lYOutRA);
	   Freemem(lXra);
	   Freemem(lYra);
	   Freemem(lZra);
	   Freemem(lZraScaled);
	   Freemem(lXCutra);
	   Freemem(lYCutra);
	   Freemem(lZCutra);
	   if (lThreshComp = 1) then begin //analogue output
		   //gGlMaxUnscaledS := 200;
		   //Scale.value := 0.0050000;
		   for lI := 1 to lImgSz do
				  gBGImg.VOIUndoVol[lI] := 200 - gBGImg.VOIUndoVol[lI];
		end else begin //threshcomp <> 1
			//gGlMaxUnscaledS := 100;
			//Scale.value := 0.0100000;
			for lI := 1 to lImgSz do
				   if gBGImg.VOIUndoVol[lI] = 0 then
					  gBGImg.VOIUndoVol[lI] := kVOI8bit
				   else
					   gBGImg.VOIUndoVol[lI] := 0;
		end; //Threshcomp <> 1 so digital output
		lResliceNotMask := false;
		gBGImg.VOIchanged := true;
			ImgForm.ProgressBar1.Position := 0;
		ImgForm.Undo1Click(nil); //show smoothed buffer
  end;

procedure TSmoothVOIForm.FormCreate(Sender: TObject);
begin
	 ScaleSides.itemindex:=(0);
	 xROIoutput.itemindex:=(0);
  XROIthresh.value := 0.5;
  XROIfwhm.value := 8;
end;

end.
