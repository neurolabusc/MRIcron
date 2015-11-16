unit dilate;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin, Buttons,define_types,nifti_img_view,nifti_img, Mask, ReadInt;

procedure BatchDilate;
procedure MakeShells;
procedure DilateVOI(lDilateMM: single; var lVOI: bytep);
//function DilateOpenVOI(lDilateMM, lMaskDilateMM: single; lMaskWithBG: boolean): boolean;

implementation
uses text, nifti_hdr_view, readfloat;

procedure DilateVOI(lDilateMM: single; var lVOI: bytep);
var
   lBuff: bytep;
   lSqrDilateMM,lXmm,lYmm,lZmm: single;
   lPos,lZ,lY,lX,lCycle,lSliceSz,lI,lXDim,lYDim,lZDim,lPlanes,lImgSz: integer;
   lXVox,lYVox,lZVox: integer;
procedure Paint (lxx,lyy,lzz: integer);
//only VOI if voxel has a VOI and distance is less than lDilateMM from origin...
var
  lDx: single;
  lPos2: integer;
begin
  lPos2 := lxx + ((lyy-1)*lXDim)+((lzz-1)*lSliceSz);
  if lVOI^[lPos2] <> 0 then
    exit; //already painted
  lDx := (sqr((lxx-lx)*lXmm)+sqr((lyy-ly)*lYmm)+sqr((lzz-lz)*lZmm));
  if lDx > lSqrDilateMM then
    exit;
  lVOI^[lPos2] := kVOI8bit;
end;

procedure PaintNeighbors;
var
  i,lxx,lyy,lzz: integer;
  lBox: TCutout;
begin
  //range to check
  lBox.Lo[1] := lX - lXVox;
  lBox.Lo[2] := lY - lYVox;
  lBox.Lo[3] := lZ - lZVox;
  lBox.Hi[1] := lX + lXVox;
  lBox.Hi[2] := lY + lYVox;
  lBox.Hi[3] := lZ + lZVox;
  for i := 1 to 3 do
    if lBox.Lo[i] < 1 then
      lBox.Lo[i] := 1;
  if lBox.Hi[1] > lXDim then
    lBox.Hi[1] := lXDim;
  if lBox.Hi[2] > lYDim then
    lBox.Hi[2] := lYDim;
  if lBox.Hi[3] > lZDim then
    lBox.Hi[3] := lZDim;
  //we will check each voxel in sphere until we find a VOI...
  for lzz :=  lBox.Lo[3] to lBox.Hi[3] do
    for lyy :=  lBox.Lo[2] to lBox.Hi[2] do
      for lxx :=  lBox.Lo[1] to lBox.Hi[1] do
        Paint (lxx,lyy,lzz);
end;
begin
   if (lDilateMM< 1) then begin
	    //ShowMessage('Must dilate at least one voxel.');
	    Exit;
   end;
   lXDim := gBGImg.ScrnDim[1];
   lYDim := gBGImg.ScrnDim[2];
   lZDim := gBGImg.ScrnDim[3];
   lSliceSz := lXdim * lYdim;
   lXmm := gBGImg.Scrnmm[1];
   lYmm := gBGImg.Scrnmm[2];
   lZmm := gBGImg.Scrnmm[3];
   lSqrDilateMM := Sqr(lDilateMM);
   lXVox :=  abs(round(lDilateMM/lXmm));
   lYVox :=  abs(round(lDilateMM/lYmm));
   lZVox :=  abs(round(lDilateMM/lZmm));
   lImgSz := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
   if lImgSz < 1 then exit;
   getmem(lBuff,lImgSz);
   Move(lVOI^,lBuff^, lImgSz);
   lPos := 0;
   for lZ := 1 to lZDim do begin
        for lY := 1 to lYDim do begin
          for lX := 1 to lXDim do begin
            inc(lPos);
            if lBuff^[lPos] = kVOI8bit  then
              PaintNeighbors;
          end;//X
        end; //Y
      end;//Z
   //Move(lBuff^,lVOI^,lImgSz);
  freemem(lBuff);
end;

function DilateOpenVOI(lDilateMM, lMaskDilateMM: single; lMaskWithBG: boolean): boolean;
var
  lStartTime: DWord;
  lVOImask: bytep;
  lI,lImgSz: integer;
begin
   result := false;
   if not IsVOIOpen then begin
	  ShowMessage('You have not created or opened a region of interest.');
	  exit;
   end;
   if lMaskDilateMM > lDilateMM  then begin
	  ShowMessage('Error: mask-dilation can not be bigger than primary dilation.');
    exit;
   end;
   if lDilateMM <= 0 then begin
	  ShowMessage('You have not created or opened a region of interest.');
    exit;
   end;
   lStartTime := GetTickCount;
   lImgSz := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
   if lImgSz < 1 then exit;
   CreateUndoVol;//create gBGImg.VOIUndoVol
   Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVol^,gBGImg.VOIUndoVolItems);
   for lI := 1 to lImgSz do
      if gBGImg.VOIUndoVol[lI] <> 0 then
			  gBGImg.VOIUndoVol[lI] := kVOI8bit;
   //getmem(lVOI,lImgSz);
   //Move(gBGImg.VOIUndoVol^,lVOI^, lImgSz);
   DilateVOI(lDilateMM, gBGImg.VOIUndoVol);
   //freemem(lVOI);
   if lMaskDilateMM >= 0 then begin
      getmem(lVOImask,lImgSz);
      Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,lVOImask^, lImgSz);
      DilateVOI(lMaskDilateMM, lVOImask);
      for lI := 1 to lImgSz do
        if lVOImask[lI] <> 0 then
			    gBGImg.VOIUndoVol^[lI] := 0;
      freemem(lVOImask);
   end; //mask using dilated value...
   if lMaskWithBG then
    for lI := 1 to lImgSz do
      if gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lI] = 0 then
			  gBGImg.VOIUndoVol^[lI] := 0;
   ImgForm.StatusLabel.caption :=('Dilation time(ms): '+inttostr(GetTickCount-lStartTime));
   gBGImg.VOIchanged := true;
   //	ImgForm.ProgressBar1.Position := 0;
   ImgForm.Undo1Click(nil); //show smoothed buffer
   result := true;
end;

(*procedure DilateOpenVOI(lDilateMM: single; lMaskWithOriginal,lMaskWithBG: boolean);
var
   lBuff: bytep;
   lStartTime: DWord;
   lSqrDilateMM,lXmm,lYmm,lZmm: single;
   lPos,lZ,lY,lX,lCycle,lSliceSz,lI,lXDim,lYDim,lZDim,lPlanes,lImgSz: integer;
   lXVox,lYVox,lZVox: integer;
procedure Paint (lxx,lyy,lzz: integer);
//only VOI if voxel has a VOI and distance is less than lDilateMM from origin...
var
  lDx: single;
  lPos2: integer;
begin
  lPos2 := lxx + ((lyy-1)*lXDim)+((lzz-1)*lSliceSz);
  if gBGImg.VOIUndoVol[lPos2] <> 0 then
    exit; //already painted
  lDx := (sqr((lxx-lx)*lXmm)+sqr((lyy-ly)*lYmm)+sqr((lzz-lz)*lZmm));
  if lDx > lSqrDilateMM then
    exit;
  gBGImg.VOIUndoVol[lPos2] := kVOI8bit;
end;

procedure PaintNeighbors;
var
  i,lxx,lyy,lzz: integer;
  lBox: TCutout;
begin
  //range to check
  lBox.Lo[1] := lX - lXVox;
  lBox.Lo[2] := lY - lYVox;
  lBox.Lo[3] := lZ - lZVox;
  lBox.Hi[1] := lX + lXVox;
  lBox.Hi[2] := lY + lYVox;
  lBox.Hi[3] := lZ + lZVox;
  for i := 1 to 3 do
    if lBox.Lo[i] < 1 then
      lBox.Lo[i] := 1;
  if lBox.Hi[1] > lXDim then
    lBox.Hi[1] := lXDim;
  if lBox.Hi[2] > lYDim then
    lBox.Hi[2] := lYDim;
  if lBox.Hi[3] > lZDim then
    lBox.Hi[3] := lZDim;
  //we will check each voxel in sphere until we find a VOI...
  for lzz :=  lBox.Lo[3] to lBox.Hi[3] do
    for lyy :=  lBox.Lo[2] to lBox.Hi[2] do
      for lxx :=  lBox.Lo[1] to lBox.Hi[1] do
        Paint (lxx,lyy,lzz);
end;


begin
   lXDim := gBGImg.ScrnDim[1];
   lYDim := gBGImg.ScrnDim[2];
   lZDim := gBGImg.ScrnDim[3];
   lXmm := gBGImg.Scrnmm[1];
   lYmm := gBGImg.Scrnmm[2];
   lZmm := gBGImg.Scrnmm[3];
   lSqrDilateMM := Sqr(lDilateMM);
   lXVox :=  abs(round(lDilateMM/lXmm));
   lYVox :=  abs(round(lDilateMM/lYmm));
   lZVox :=  abs(round(lDilateMM/lZmm));
   if not IsVOIOpen then begin
	  ShowMessage('You have not created or opened a region of interest.');
	  exit;
   end;
   if (lDilateMM< 1) then begin
	 ShowMessage('Must dilate at least one voxel.');
	 Exit;
   end;
   lImgSz := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
   if lImgSz < 1 then exit;
   CreateUndoVol;//create gBGImg.VOIUndoVol
   Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVol^,gBGImg.VOIUndoVolItems);
   //set all voxels to 1 or zero
   for lI := 1 to lImgSz do
      if gBGImg.VOIUndoVol[lI] <> 0 then
			  gBGImg.VOIUndoVol[lI] := kVOI8bit;
   lStartTime := GetTickCOunt;
   lSliceSz := lXdim * lYdim;
   getmem(lBuff,lImgSz);
   Move(gBGImg.VOIUndoVol^,lBuff^, lImgSz);
   lPos := 0;
   for lZ := 1 to lZDim do begin
        for lY := 1 to lYDim do begin
          for lX := 1 to lXDim do begin
            inc(lPos);
            if lBuff[lPos] = kVOI8bit  then
              PaintNeighbors;
          end;//X
        end; //Y
      end;//Z
   if lMaskWithOriginal then
    for lI := 1 to lImgSz do
      if lBuff[lI] <> 0 then
			  gBGImg.VOIUndoVol[lI] := 0;
   freemem(lBuff);
   if lMaskWithBG then
    for lI := 1 to lImgSz do
      if gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lI] = 0 then
			  gBGImg.VOIUndoVol[lI] := 0;
   ImgForm.StatusLabel.caption :=('Dilation time(ms): '+inttostr(GetTickCount-lStartTime));
		gBGImg.VOIchanged := true;
		//	ImgForm.ProgressBar1.Position := 0;
		ImgForm.Undo1Click(nil); //show smoothed buffer
end;
  *)

(*procedure BatchDilate;
var
	lInc,lNumberofFiles: integer;
  lPrefix,lOutname,lFilename,lBGname:string;
  lMaskBG,lPref: boolean;
  lDilateMM,lMaskDilateMM: single;
begin
  for lInc := 1 to (knMaxOverlay-1) do
	    FreeImgMemory(gMRIcroOverlay[lInc]);
  ImgForm.UpdateLayerMenu;
  lDilateMM := ReadFloatForm.GetFloat('VOI dilation (mm). ', 0,8,9999);
  if lDilateMM <= 0 then begin
      showmessage('Error: dilation in mm must be positive');
      exit;
  end;
  lMaskDilateMM := ReadFloatForm.GetFloat('Dilated rim? mm, -1 for none ', -1,0,lDilateMM);
  lMaskBG := false;
	case MessageDlg('Mask output with background image?', mtConfirmation,
		[mbYes, mbNo], 0) of
		id_Yes: lMaskBG := true;
	end; //case
  if not OpenDialogExecute(kImgFilter,'Select background image (mask, e.g. gray matter mask)',false) then exit;
  lBGname:= HdrForm.OpenHdrDlg.Filename;
  if not OpenDialogExecute(kImgPlusVOIFilter,'Select VOIs',true) then exit;
  lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
    if  lNumberofFiles < 1 then
		  exit;
  lPref := gBGImg.ResliceOnLoad;
  gBGImg.ResliceOnLoad := false;
  ImgForm.OpenAndDisplayImg(lBGname,false);
  lPrefix := 'd'+inttostr(round(lDilateMM))+'m'+inttostr(round(lMaskDilateMM));
  if lMaskBG then
    lPrefix := lPrefix+'b';
  if not InputQuery('Output filename prefix','Enter prefix for filenames', lPrefix) then
           exit;
  for lInc:= 1 to lNumberofFiles do begin
	      FreeImgMemory(gMRIcroOverlay[kVOIOverlayNum]);
        ImgForm.UpdateLayerMenu;
        lFilename := HdrForm.OpenHdrDlg.Files[lInc-1];
        ImgForm.OpenVOICore(lFilename);
        DilateOpenVOI(lDilateMM,lMaskDilateMM,lMaskBG);
        lOutname := ChangeFilePrefix(lFilename,lPrefix{'m'});
        SaveAsVOIorNIFTIcore (lOutname, gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
        gBGImg.VOIchanged := false;
  end;//lLoop
  gBGImg.ResliceOnLoad := lPref;
end;  *)
function MeanInten (lOverlayNum: integer; var lVol: integer): double;
var
  lINc: integer;
  lSum: double;
begin
  result := 0;
  if gMRIcroOverlay[lOverlayNum].ScrnBufferItems < 2 then
    exit;
  lSum := 0;
  lVol := 0;
  for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
		if gMRIcroOverlay[lOverlayNum].ScrnBuffer[lInc] > 0 then begin
      inc(lVol);
			lSum := lSum+ RawBGIntensity(lInc);
		end; //if VOI voxel
	end; //for each voxel
  if lVol < 1 then
    exit;
  result := lSum/lVol;
  result := Raw2ScaledIntensity(gMRIcroOverlay[kBGOverlayNum],result);
end;

function MeanIntenGtrZero (lOverlayNum: integer; var lVol: integer): double;
var
  lINc: integer;
  lSum: double;
begin
  result := 0;
  if gMRIcroOverlay[lOverlayNum].ScrnBufferItems < 2 then
    exit;
  lSum := 0;
  lVol := 0;
  for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
		if (gMRIcroOverlay[lOverlayNum].ScrnBuffer[lInc] > 0) and (RawBGIntensity(lInc) > 0) then begin
      inc(lVol);
			lSum := lSum+ RawBGIntensity(lInc);
		end; //if VOI voxel
	end; //for each voxel
  if lVol < 1 then
    exit;
  result := lSum/lVol;
  result := Raw2ScaledIntensity(gMRIcroOverlay[kBGOverlayNum],result);
end;

procedure ExportInten (lOverlayNum: integer);
const
  kStatSep = kTab;
var
  lINc: integer;
  lStr: string;
begin
  if gMRIcroOverlay[lOverlayNum].ScrnBufferItems < 2 then
    exit;
   lStr := 'RawData';
  for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
		if gMRIcroOverlay[lOverlayNum].ScrnBuffer[lInc] > 0 then begin
      TextForm.MemoT.Lines.add( floattostr(Raw2ScaledIntensity(gMRIcroOverlay[kBGOverlayNum],RawBGIntensity(lInc)))+kStatSep);
		end; //if VOI voxel
	end; //for each voxel
end;

procedure BatchDilate;
label
  888;
const
  kMaxDilate = 12;
  kStatSep = kTab;
var
  lMean,lMeanGtrZero: double;
	lVol,lnDilate,lInc,lDilate: integer;
  lBasename,lPrefix,lOutname,lBGname,lVOIname,lPERFName:string;
  lMaskBG,lPref: boolean;
  lDilateMM: array [1..kMaxDilate] of single;
begin
  for lInc := 1 to (knMaxOverlay-1) do
	    FreeImgMemory(gMRIcroOverlay[lInc]);
  ImgForm.UpdateLayerMenu;
  lnDilate := ReadIntForm.GetInt('Number of dilation sizes ', 2,8,kMaxDilate);
  if (lnDilate < 2 ) or (lnDilate > kMaxDilate) then
    exit;
  for lInc := 1 to lnDilate do
    lDilateMM[lInc] := ReadFloatForm.GetFloat(inttostr(lInc)+ ' VOI dilation (mm). ', 0,lINc*3,9999);
  for lInc := 2 to lnDilate do begin
    if (lDilateMM[lInc-1] >= lDilateMM[lInc]) then begin
      Showmessage('Error: dilation sizes should specified be in ascending order');
      exit;
    end;
  end;
	case MessageDlg('Mask output with background image?', mtConfirmation,
		[mbYes, mbNo], 0) of
		id_Yes: lMaskBG := true;
	end; //case
  if lMaskBG then begin
    if not OpenDialogExecute(kImgFilter,'Select background image (mask, e.g. gray matter mask)',false) then exit;
    lBGname:= HdrForm.OpenHdrDlg.Filename;
  end else
    lBGName := '';
  lPref := gBGImg.ResliceOnLoad;
  gBGImg.ResliceOnLoad := false;
  TextForm.MemoT.Lines.clear;
repeat
  if not OpenDialogExecute(kImgPlusVOIFilter,'Select VOI',false) then goto 888;
  lVOIname := HdrForm.OpenHdrDlg.Filename;
  if not OpenDialogExecute(kImgFilter,'Select PERF image',false) then goto 888;
  lPerfName :=  HdrForm.OpenHdrDlg.Filename;
  if lMaskBG then
    lBaseName := lBGname
  else
     lBaseName := lPerfName;

  TextForm.MemoT.Lines.add( 'Mask'+kStatSep+'VOI'+kStatSep+'Perf'+kStatSep+'Outputname'+kStatSep+'MinDilate'+kStatSep+'MaxDilate'+kStatSep+'Volume[vox]'+kStatSep+'MeanIntensity'+kStatSep+'MeanIntensity>0');
  for lDilate := 1 to (lnDilate-1) do begin
    ImgForm.OpenAndDisplayImg(lBaseName,false);
    lPrefix := inttostr(lDilate);
    FreeImgMemory(gMRIcroOverlay[kVOIOverlayNum]);
    ImgForm.UpdateLayerMenu;
    ImgForm.OpenVOICore(lVOIname);
    DilateOpenVOI(lDilateMM[lDilate+1],lDilateMM[lDilate],lMaskBG);
    if gBGImg.Mirror then
        MirrorScrnBuffer(gBGImg, gMRIcroOverlay[kVOIOverlayNum]);   //April 2011

    lOutname := ChangeFilePrefix(lVOIname,lPrefix);
    SaveAsVOIorNIFTIcore (lOutname, gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
    gBGImg.VOIchanged := false;
    ImgForm.OpenAndDisplayImg(lPerfName,false);
    FreeImgMemory(gMRIcroOverlay[kVOIOverlayNum]);
    ImgForm.OpenVOICore(lOutname);
    lMean :=  MeanInten (kVOIOverlayNum,lVol);
    lMeanGtrZero :=  MeanIntenGtrZero (kVOIOverlayNum,lVol);
    TextForm.MemoT.Lines.add( lBGname+kStatSep+lVOIname+kStatSep+lPerfName+kStatSep+lOutname+kStatSep+floattostr(lDilateMM[lDilate])+kStatSep+floattostr(lDilateMM[lDilate+1])
    +kStatSep+inttostr(lVol)+kStatSep+floattostr(lMean)+kStatSep+floattostr(lMeanGtrZero));
    //  ExportInten(kVOIOverlayNum);
    //ImgForm.ShowDescriptive(kVOIOverlayNum,false);
  end;//lLoop
until false;
888:
  gBGImg.ResliceOnLoad := lPref;
  TextForm.Show;
end;
(*procedure BatchDilate;
label
  888;
const
  kMaxDilate = 12;
  kStatSep = kTab;
var
  lMean: double;
	lVol,lnDilate,lInc,lDilate: integer;
  lBasename,lPrefix,lOutname,lBGname,lVOIname,lPERFName:string;
  lMaskBG,lPref: boolean;
  lDilateMM: array [1..kMaxDilate] of single;
begin
  for lInc := 1 to (knMaxOverlay-1) do
	    FreeImgMemory(gMRIcroOverlay[lInc]);
  ImgForm.UpdateLayerMenu;
  lnDilate := ReadIntForm.GetInt('Number of dilation sizes ', 2,3,kMaxDilate);
  if (lnDilate < 2 ) or (lnDilate > kMaxDilate) then
    exit;
  for lInc := 1 to lnDilate do
    lDilateMM[lInc] := ReadFloatForm.GetFloat(inttostr(lInc)+ ' VOI dilation (mm). ', 0,lINc*3,9999);
  for lInc := 2 to lnDilate do begin
    if (lDilateMM[lInc-1] >= lDilateMM[lInc]) then begin
      Showmessage('Error: dilation sizes should specified be in ascending order');
      exit;
    end;
  end;
	case MessageDlg('Mask output with background image?', mtConfirmation,
		[mbYes, mbNo], 0) of
		id_Yes: lMaskBG := true;
	end; //case
  if lMaskBG then begin
    if not OpenDialogExecute(kImgFilter,'Select background image (mask, e.g. gray matter mask)',false) then exit;
    lBGname:= HdrForm.OpenHdrDlg.Filename;
  end else
    lBGName := '';
  lPref := gBGImg.ResliceOnLoad;
  gBGImg.ResliceOnLoad := false;
  TextForm.MemoT.Lines.clear;
repeat
  if not OpenDialogExecute(kImgPlusVOIFilter,'Select VOI',false) then goto 888;
  lVOIname := HdrForm.OpenHdrDlg.Filename;
  if not OpenDialogExecute(kImgFilter,'Select PERF image',false) then goto 888;
  lPerfName :=  HdrForm.OpenHdrDlg.Filename;
  if lMaskBG then
    lBaseName := lBGname
  else
     lBaseName := lPerfName;
  TextForm.MemoT.Lines.add( 'Mask'+kStatSep+'VOI'+kStatSep+'Perf'+kStatSep+'Outputname'+kStatSep+'MinDilate'+kStatSep+'MaxDilate'+kStatSep+'Volume[vox]'+kStatSep+'MeanIntensity');
  for lDilate := 1 to (lnDilate-1) do begin
    ImgForm.OpenAndDisplayImg(lBaseName,false);
    lPrefix := inttostr(lDilate);
    FreeImgMemory(gMRIcroOverlay[kVOIOverlayNum]);
    ImgForm.UpdateLayerMenu;
    ImgForm.OpenVOICore(lVOIname);
    DilateOpenVOI(lDilateMM[lDilate+1],lDilateMM[lDilate],lMaskBG);
    lOutname := ChangeFilePrefix(lVOIname,lPrefix);
    SaveAsVOIorNIFTIcore (lOutname, gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
    gBGImg.VOIchanged := false;
    ImgForm.OpenAndDisplayImg(lPerfName,false);
    FreeImgMemory(gMRIcroOverlay[kVOIOverlayNum]);
    ImgForm.OpenVOICore(lOutname);
    lMean :=  MeanInten (kVOIOverlayNum,lVol);
    TextForm.MemoT.Lines.add( lBGname+kStatSep+lVOIname+kStatSep+lPerfName+kStatSep+lOutname+kStatSep+floattostr(lDilateMM[lDilate])+kStatSep+floattostr(lDilateMM[lDilate+1])
    +kStatSep+inttostr(lVol)+kStatSep+floattostr(lMean));
    ExportInten(kVOIOverlayNum);

    //ImgForm.ShowDescriptive(kVOIOverlayNum,false);
  end;//lLoop
until false;
888:
  gBGImg.ResliceOnLoad := lPref;
  TextForm.Show;
end;  *)

{$DEFINE noTESTSHELL}
procedure MakeShells;
const
  kMaxDilate = 24;
  kStatSep = kTab;
var
  lFilename,lOutname : string;
  lV,lnDilate,lInc: integer;
  lDilateMM: array [0..kMaxDilate] of single;
begin
 {$IFDEF TESTSHELL}
  HdrForm.OpenHdrDlg.Files.Clear;
  HdrForm.OpenHdrDlg.Files.Add('c:\Voimskog3.voi');
  lnDilate := 2;
  lDilateMM[0] := 0;
  lDilateMM[1] := 6;
  lDilateMM[2] := 12;
  {$ELSE}
  if not OpenDialogExecute(kImgPlusVOIFilter,'Select VOI[s] to dilate',true) then
    exit;
  lnDilate := ReadIntForm.GetInt('Number of dilation shells ', 2,3,kMaxDilate);
  if (lnDilate < 2 ) or (lnDilate > kMaxDilate) then
    exit;
  lDilateMM[0] := ReadFloatForm.GetFloat(inttostr(lInc)+ 'Dilated shell 1s inner edge for dilated shell (mm). ', 0,1,9999);
  for lInc := 1 to lnDilate do
    lDilateMM[lInc] := ReadFloatForm.GetFloat(inttostr(lInc)+ 'Dilated shell '+inttostr(lInc)+'s outer edge (mm). ', 0,lDilateMM[lInc-1]+3,9999);

  {$ENDIF}
  if HdrForm.OpenHdrDlg.Files.Count < 1 then
    exit;

  for lV := 1 to HdrForm.OpenHdrDlg.Files.Count do begin //vcx
    lFilename := HdrForm.OpenHdrDlg.Files[lV-1];
    ImgForm.OpenAndDisplayImg(lFileName,false);

    for lInc := 1 to lnDilate do begin
      ImgForm.OpenVOICore(lFilename);
      DilateOpenVOI(lDilateMM[lInc], lDilateMM[lInc-1],false);
      //ImgForm.Undo1Click(nil); //show smoothed buffer
      lOutname := ChangeFilePrefix(lFilename,inttostr(lInc));
      //SaveAsVOIorNIFTI(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems,1,1,false,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,lOutname);
      if gBGImg.Mirror then
        MirrorScrnBuffer(gBGImg, gMRIcroOverlay[kVOIOverlayNum]);
      SaveAsVOIorNIFTIcore (lOutname, gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kVOIOverlayNum].NiftiHdr);
      gBGImg.VOIchanged := false;

    end;//for each dilate
  end; //for each voi
  ImgForm.OpenAndDisplayImg(lOutName,false);

end; //proc makeshells

end.
