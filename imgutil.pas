unit imgutil;
{$H+}
interface

function UnscaledMean (lOverlayNum: integer): double;
function ScaledMean (lOverlayNum: integer): double;
procedure BatchChangeInterceptSoVOIEqualsZero;


implementation
uses text,nifti_hdr,nifti_hdr_view,define_types,nifti_img, nifti_img_view, nifti_types;


function UnscaledMean (lOverlayNum: integer): double;
//kVOIOverlayNum
var
	lROIVol,lInc: integer;
  lROISum: double;
begin //proc ShowDescript
   result := 0;
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then
      exit;
		lROIVol := 0;
		lROISum := 0;
	for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
		if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then begin
		  lROISum := lROISum + RawBGIntensity(lInc);
      inc(lROIVol);
    end;
	end; //for each voxel
  if lROIVol > 0 then
    result := lROISum/lROIVol;
end;

function ScaledMean (lOverlayNum: integer): double;
begin
  result := UnscaledMean(lOverlayNum);
  result := Raw2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],result);
end;

procedure BatchChangeInterceptSoVOIEqualsZero;
var
	lInc,lNumberofFiles,lMinClusterSz: integer;
  lZeroHdr : TNIfTIHdr;
  lFilename,lVOIname:string;
  lPref: boolean;
  lMean: double;
begin
  for lInc := 1 to (knMaxOverlay-1) do
	    FreeImgMemory(gMRIcroOverlay[lInc]);
  ImgForm.UpdateLayerMenu;

  if not OpenDialogExecute(kImgPlusVOIFilter,'Select volume of interest',false) then exit;
  lVOIName := HdrForm.OpenHdrDlg.FileName;
  if not OpenDialogExecute(kImgFilter,'Select perfusion images',true) then exit;
  lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
  if  lNumberofFiles < 1 then
    exit;
  TextForm.MemoT.Lines.Clear;
  lPref := gBGImg.ResliceOnLoad;
  gBGImg.ResliceOnLoad := false;
  for lInc:= 1 to lNumberofFiles do begin
            lFilename := HdrForm.OpenHdrDlg.Files[lInc-1];
            ImgForm.OpenAndDisplayImg(lFilename,false);
            ImgForm.OverlayOpenCore ( lVOIname, kVOIOverlayNum);
            lMean := UnscaledMean(kVOIOverlayNum);
              lZeroHdr := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr;
            if lZeroHdr.scl_slope <> 1 then
              TextForm.MemoT.Lines.Add(lFilename+'  Scale slope is not 1, please contact Chris Rorden ')
            else if lMean <> 0 then begin
              TextForm.MemoT.Lines.Add(lFilename+kTextSep+realtostr(lMean,5));
              lZeroHdr.scl_inter := lZeroHdr.scl_inter - lMean;
		          lFilename := changefileprefix(lFilename,'z');
              SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems,gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,1,lZeroHdr)
            end else
              TextForm.MemoT.Lines.Add(lFilename+'  UNCHANGED (mean of VOI is already zero) ');

            //FindClustersText(gMRIcroOverlay[kBGOverlayNum], lThresh,lMinClusterSz);
  end;//lLoop
  gBGImg.ResliceOnLoad := lPref;
  TextForm.Show;
end;


end.