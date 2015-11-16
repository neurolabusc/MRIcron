unit statclustertable;
//USED by stats to select only regions with a given number of connected/contiguous voxels
interface
{$H+}
uses define_types,dialogs,SysUtils,nifti_hdr,nifti_img, classes;

//procedure FindClustersText (var lHdr: TMRIcroHdr; lThreshClusterSz: integer; lThresh: double);
procedure BatchCluster;


implementation

uses text,nifti_img_view, nifti_hdr_view, readfloat, readint;



procedure FindClustersText (var lHdr: TMRIcroHdr; lThreshIn: single; lMinClusterSz: integer);
var
   lClusterMaxPos,lXdim,lYdim,lZdim,lScaledThresh,lClusterSz,lClusterFillVal,lQTail,lQHead,lSliceSz,lQSz,lInc,lVolSz: integer;
   lThresh,lClusterMax: single;
   lClusterBuffS: SingleP;
   lQra: LongIntP;
   lXcom,lYcom,lZcom,lBuffIn32 : SingleP;
   lBuffIn16 : SmallIntP;
   lCh: char;
procedure InitCenterOfMass;
begin
   getmem(lXcom, lXDim*sizeof(single));
   getmem(lYcom, lYDim*sizeof(single));
   getmem(lZcom, lZDim*sizeof(single));
end;

procedure FreeCenterOfMass;
begin
   freemem(lXcom);
   freemem(lYcom);
   freemem(lZcom);
end;

procedure ClearCenterOfMass;
var
  i: integer;
begin
  for i := 1 to lXDim do
    lXcom^[i] := 0;
  for i := 1 to lYDim do
    lYcom^[i] := 0;
  for i := 1 to lZDim do
    lZcom^[i] := 0;
end;

procedure AddCenterOfMass (lVox: integer; lInten: single);
var
  lXi,lYi,lZi: integer;
begin
//lukas
  ImgPosToSlices(lVox,lXi,lYi,lZi);
  lXcom^[lXi] := lXcom^[lXi] + lInten;
  lYcom^[lYi] := lYcom^[lYi] + lInten;
  lZcom^[lZi] := lZcom^[lZi] + lInten;
end;

function CenterOfMassPosition: integer;
var
  i : integer;
  lSum,lXs,lYs,lZs: double;
begin
  lSum := 0;
  lXs := 0;
  for i := 1 to lXDim do
    lSum := lSum +lXcom^[i];
  for i := 1 to lXDim do
    lXs := lXs +(i*lXcom^[i]);
  if lSum > 0 then
    lXs := lXs/lSum;
  //
  lSum := 0;
  lYs := 0;
  for i := 1 to lYDim do
    lSum := lSum +lYcom^[i];
  for i := 1 to lYDim do
    lYs := lYs +(i*lYcom^[i]);
  if lSum > 0 then
    lYs := lYs/lSum;
  //Z
  lSum := 0;
  lZs := 0;
  for i := 1 to lZDim do
    lSum := lSum +lZcom^[i];
  for i := 1 to lZDim do
    lZs := lZs +(i*lZcom^[i]);
  if lSum > 0 then
    lZs := lzs/lSum;
  result := SlicesToImgPos(round(lXs),round(lYs),round(lZs));
  //fx(result, lXs,lYs,lZs);
end;

function XYZstr (lPos: integer): string;
var lXmm,lYmm,lZmm: single;
begin
  ImgPosToMM(lPos, lXmm,lYmm,lZmm);
  result := inttostr(round(lXmm))+kTextSep+inttostr(round(lYmm))+kTextSep+inttostr(round(lZmm));
end;

procedure Report (lClusterMax: single; lClusterSz, lClusterMaxPos: integer);
var
  lTemplateLabel: string;
begin
    if lClusterSz < lMinClusterSz then
      exit;
    //lTemplateLabel := ImgForm.BGLabelString(lClusterMaxPos);
    //burger                                    ImgIntensityString
    lTemplateLabel := ImgForm.ImgIntensityString(gMRIcroOverlay[2], lClusterMaxPos);
    TextForm.MemoT.lines.add(XYZstr(lClusterMaxPos)+kTextSep+XYZstr(CenterOfMassPosition)+kTextSep+inttostr(lClusterSz)+kTextSep+lCh+floattostr(lClusterMax)+kTextSep+lTemplateLabel);
end;

procedure ReportLabel;
begin
    TextForm.MemoT.lines.add('# Data='+kTextSep+lHdr.HdrFileName +kTextSep+'Threshold='+kTextSep+floattostr(lThreshIn) +kTextSep+'MinCluster='+kTextSep+inttostr(lMinClusterSz));
    TextForm.MemoT.lines.add('#X'+kTextSep+'Y'+kTextSep+'Z'+kTextSep+'Xcom'+kTextSep+'Ycom'+kTextSep+'Zcom'+kTextSep+'ClusterSize[Vox]'+kTextSep+'Max'+kTextSep+'Template');

end;
Procedure IncQra(var lVal, lQSz: integer);
begin
    inc(lVal);
    if lVal >= lQSz then
     lVal := 1;
end;

procedure Check(lPixel: integer);
var
  lVal: single;
begin
    lVal := lClusterBuffS^[lPixel];
    if (lVal= 0) then
      exit;
    AddCenterOfMass(lPixel,lVal);
    if lVal > lClusterMax then begin
      lClusterMax := lVal;
      lClusterMaxPos := lPixel;
    end;
    incQra(lQHead,lQSz);
    inc(lClusterSz);
    lClusterBuffS^[lPixel] := 0;
    lQra^[lQHead] := lPixel;
end;

PROCEDURE RetirePixel; //FIFO cleanup , 1410: added 18-voxel check
VAR
   lVal,lValX,lXPos,lYPos,lZPos: integer;
BEGIN
   lVal := lQra^[lQTail];
   if lVal = 0 then begin
      incQra(lQTail,lQSz); //done with this pixel
      exit;
   end;
   lXpos := lVal mod lXdim;
   if lXpos = 0 then lXPos := lXdim;
   lYpos := (1+((lVal-1) div lXdim)) mod lYDim;
   if lYPos = 0 then lYPos := lYdim;
   lZpos := ((lVal-1) div lSliceSz)+1;
   if (lXPos <= 1) or (lXPos >= lXDim) or
    (lYPos <= 1) or (lYPos >= lYDim) or
    (lZPos <= 1) or (lZPos >= lZDim) then
     // retire and exiT
   else begin
    //lXDimM := lXDim;
    Check(lVal-1); //left
    Check(lVal+1); //right
    Check(lVal-lXDim); //up
    Check(lVal+lXDim); //down
    Check(lVal-lSliceSz); //up
    Check(lVal+lSliceSz); //down
    //check plane above
    lValX := lVal + lSLiceSz;
    Check(lValX-1); //left
    Check(lValX+1); //right
    Check(lValX-lXDim); //up
    Check(lValX+lXDim); //down
    //check plane below
    lValX := lVal - lSLiceSz;
    Check(lValX-1); //left
    Check(lValX+1); //right
    Check(lValX-lXDim); //up
    Check(lValX+lXDim); //down
    //check diagonals of current plane
    Check(lVal-lXDim-1); //up, left
    Check(lVal-lXDim+1); //up, right
    Check(lVal+lXDim-1); //down, left
    Check(lVal+lXDim+1); //down, right
  end; //not edge
  incQra(lQTail,lQSz); //done with this pixel
END;

procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
var lI: integer;
begin
  if (lClusterBuffS^[lPt]=0) then exit;
  for lI := 1 to lQsz do
      lQra^[lI] := 0;
  lQHead := 0;
  lQTail := 1;
  Check(lPt);
  RetirePixel;
  // check that there was anything in the cluster at all
  //showmessage('head'+inttostr(lQHead)+'.'+inttostr(lQTail));
  //if lQHead > 2 then begin
    // and do the recursion to get rid of it
  while ((lQHead+1) <> lQTail) do begin//complete until all voxels in buffer have been tested
        RetirePixel;
        if (lQHead = lQSz) and (lQTail = 1) then
           exit; //break condition: avoids possible infinite loop where QTail is being incremented but QHead is stuck at maximum value
  end;
end;

begin
     lCh := ' '; //assume positive values
     lXDim := lHdr.NIFTIhdr.dim[1];
     lYDim := lHdr.NIFTIhdr.dim[2];
     lZDim := lHdr.NIFTIhdr.dim[3];
     InitCenterOfMass;
     lVolSz := lXdim*lYdim*lZdim;
     lSliceSz := lXdim * lYdim;
     if (lXDim < 4) or (lYDim < 4) or (lZDim < 4) or (lVolSz < 1) or (lHdr.ImgBufferItems <> lVolSz)  then exit;
     GetMem(lClusterBuffS, lVolSz* sizeof(Single));
     ReportLabel;
     if lHdr.ImgBufferBPP = 4 then begin
	    lBuffIn32 := SingleP(lHdr.ImgBuffer);
      for lInc := 1 to lVolSz do
             lClusterBuffS^[lInc] := lBuffIn32^[lInc];
     end else if lHdr.ImgBufferBPP = 2 then begin  //not 32bit - if 16bit input
	     lBuffIn16 := SmallIntP(lHdr.ImgBuffer);
       for lInc := 1 to lVolSz do
             lClusterBuffS^[lInc] := lBuffIn16^[lInc];
     end else begin  //not 16 or 32 bit input
         for lInc := 1 to lVolSz do
            lClusterBuffS^[lInc] := lHdr.ImgBuffer^[lInc];
     end; //8-bit input
     //Next - apply scale and intercept
     if (lHdr.NIFTIhdr.scl_slope <> 0) and (lHdr.NIFTIhdr.scl_slope <> 1) then //if one then no effect - zero is meaningless
         for lInc := 1 to lVolSz do
            lClusterBuffS^[lInc] := lClusterBuffS^[lInc]*lHdr.NIFTIhdr.scl_slope;
     if (lHdr.NIFTIhdr.scl_inter <> 0) then  //if zero then no effect
         for lInc := 1 to lVolSz do
            lClusterBuffS^[lInc] := lClusterBuffS^[lInc]+lHdr.NIFTIhdr.scl_inter;
     lThresh := lThreshIn;
     if lThreshIn < 0 then begin //invert all values...
         for lInc := 1 to lVolSz do
            lClusterBuffS^[lInc] := -lClusterBuffS^[lInc];
         lThresh := -lThresh;
         lCh := '-';
     end;
     //Next - zero all voxels less than threshold
     for lInc := 1 to lVolSz do
            if (lClusterBuffS^[lInc]) < lThresh then
              lClusterBuffS^[lInc] := 0;
     //Next - get memory
     lQSz := (lVolSz div 4)+8;
     GetMem(lQra,lQsz * sizeof(longint) );
         //check positive clusters....
     ClearCenterOfMass;
     for lInc := 1 to lVolSz do begin
         if lClusterBuffS^[lInc] <> 0 then begin
            lClusterSz := 0;
            lClusterMax := 0;
            FillStart(lInc);
            // now fill the cluster with its size (=1 if the voxel was isolated)
            Report (lClusterMax,lClusterSz,lClusterMaxPos);
            ClearCenterOfMass;
         end;
     end;
     FreeCenterOfMass;
     Freemem(lQra);
     Freemem(lClusterBuffS);
end;

procedure BatchCluster;
var
	lInc,lNumberofFiles,lMinClusterSz: integer;
        lFilename,lTemplateName:string;
        lPref: boolean;
        lThresh: single;
begin
  for lInc := 1 to (knMaxOverlay-1) do
	    FreeImgMemory(gMRIcroOverlay[lInc]);
  ImgForm.UpdateLayerMenu;
  lMinClusterSz := ReadIntForm.GetInt('Minimum cluster size [in voxels]: ', 1,4,9999);
  lThresh := ReadFloatForm.GetFloat('Please enter statistical threshold. ', -9999,2.3,9999);
  lTemplateName := '';
  if  OpenDialogExecute(kImgFilter,'Select anatomical template (optional)',false) then begin
      lTemplateName := HdrForm.OpenHdrDlg.Filename;
  end;
  if not OpenDialogExecute(kImgFilter,'Select statistical maps',true) then exit;
	  lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
    if  lNumberofFiles < 1 then
		  exit;
    if not fileexists(lTemplateName) then
      lTemplateName := '';
    TextForm.MemoT.Lines.Clear;
    lPref := gBGImg.ResliceOnLoad;
    gBGImg.ResliceOnLoad := false;
    for lInc:= 1 to lNumberofFiles do begin
            lFilename := HdrForm.OpenHdrDlg.Files[lInc-1];

            ImgForm.OpenAndDisplayImg(lFilename,false);
            if lTemplateName <> '' then
              ImgForm.OverlayOpenCore ( lTemplateName, 2);
            FindClustersText(gMRIcroOverlay[kBGOverlayNum], lThresh,lMinClusterSz);
    end;//lLoop
    gBGImg.ResliceOnLoad := lPref;
    TextForm.Show;
end;

end.