unit batch;
{$H+}
interface
uses
{$IFNDEF UNIX} Windows,
{$ELSE}
 lclintf,LCLType,LResources,BaseUnix,
{$ENDIF}
define_types;
procedure BatchVOI;

implementation

uses
  Forms,
  //lclintf,LResources,{$IFNDEF Unix}  Controls, {$ELSE}BaseUnix, LCLType,{$ENDIF}
  nifti_img, nifti_img_view, dialogs, nifti_hdr_view, text,sysutils,classes, fdr,batchstatselect;

(*function LesionFrac (lOverlayNum: integer): double;
var
   lLesionSum,lInten: double;
   lInc: integer;
begin
   result := 0;
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems < 1 then
    exit;
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then
      exit;
   lLesionSum := 0;
   for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
       lInten := RawBGIntensity(lInc);
       if gMRIcroOverlay[lOverlayNum].ScrnBuffer[lInc] > 0 then
          lLesionSum := lLesionSum + lInten;
   end; //for each voxel
   result := lLesionSum;
end;*)

function VOIVol (lOverlayNum: integer): integer;
var
   lInc,lVox: integer;
begin
   result := 0;
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems < 1 then
    exit;
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then
      exit;
   lVox := 0;
   for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
       if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then
          inc(lVox);
   end; //for each voxel
   result := lVox;
end; //VOIVol

function VOIMean (lOverlayNum: integer): double;
var
   lSum,lInten,lVol: double;
   lInc: integer;
begin
   result := 0;
   lVol := VOIVol(lOverlayNum);
   if lVol < 1 then
    exit;
   lSum := 0;
   for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
       lInten := RawBGIntensity(lInc);
       //Next line - only voxels that are part of VOI
       if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then
        lSum := lSum + lInten;
   end; //for each voxel
   result := lSum/lVol;
end; //VOIMean

function VOIMeanFrac10pct (lOverlayNum: integer; lMax: boolean): double;
//if lMax is true, return top 10pct, if false return bottom
var
   lSum: double;
   lVox,lInc,l10pct: integer;
   lRA: singlep;
begin //proc ShowDescript
   result := 0;
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems < 1 then
    exit;
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then
      exit;
   //first - count number of voxels in ROI
   lVox := 0;
   for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do
       if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then
          inc(lVox);
   //next - get memory
   if lVox < 1 then
    exit;
   getmem(lRA,lVox * sizeof(single));
   lVox := 0;
   for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do
       if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then begin
          inc(lVox);
          lRA^[lVox] := RawBGIntensity(lInc);
       end;
   qsort(1, lVox,lRA);
   l10pct := round(lVox / 10);
   if l10pct < 1 then
    l10pct := 1;
   lSum := 0;
   if not lMax then begin //lower 10pct
      for lInc := 1 to l10pct do
        lSum := lSum + lRA^[lInc]
   end else begin //top 10pct
      for lInc :=  (lVox-l10pct+1) to lVox do
        lSum := lSum + lRA^[lInc];
   end;
   result := lSum / l10pct;
   freemem(lRA);
end;

procedure BatchVOI;
var
	lNumberofP,lP,lInc,lNumberofFiles,lLoop: integer;
        lFilename,lStr:string;
        lBGStrings : TStrings;
begin

        for lInc := 1 to (knMaxOverlay-1) do
	    FreeImgMemory(gMRIcroOverlay[lInc]);
        ImgForm.UpdateLayerMenu;
        lBGStrings := TStringList.Create;
        if (ssShift in KeyDataToShiftState(vk_Shift)) then begin
                GetFilesInDir(ExtractFileDir(HdrForm.OpenHdrDlg.Filename),lBGStrings)
        end else begin
            if not OpenDialogExecute(kImgFilter,'Select background images (stat maps)',true) then
               exit;
            lBGStrings.AddStrings(HdrForm.OpenHdrDlg.Files);
        end;
        lNumberofP:= lBGStrings.Count;
        if  lNumberofP < 1 then begin
            lBGStrings.free;
            exit;

        end;

	if not OpenDialogExecute(kImgFilter,'Select overlay images (ROIs)',true) then exit;
	lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
        if  lNumberofFiles < 1 then
		exit;
    TextForm.MemoT.Lines.Clear;
    lStr := 'Function'+kTextSep+'VOIname'+kTextSep+'VOIvol';
    for lP := 1 to lNumberofP do
        lStr := lStr + kTextSep+(lBGStrings.Strings[lP-1]);
    TextForm.MemoT.lines.add(lStr);
    for lLoop := 1 to 3 do begin
        {if lLoop=3 then
          lStr := 'min10pct'+kSep+'Filename+'kSep+'Vol'
        else if lLoop=2 then
          lStr := 'max10pct'+Filename+'kSep+'Vol'
        else
          lStr := 'mean'+Filename+kSep'Vol';}

        for lInc:= 1 to lNumberofFiles do begin
            ImgForm.StatusLabel.Caption := inttostr(lInc)+'/'+inttostr(lNumberofFiles);
            IMgForm.refresh;
            if lLoop=3 then
               lStr := 'min10pct'
            else if lLoop=2 then
                 lStr := 'max10pct'
            else
                lStr := 'mean';
            lStr := lStr +kTextSep+ (HdrForm.OpenHdrDlg.Files[lInc-1]);
            for lP := 1 to lNumberofP do begin
                lFilename := lBGStrings.Strings[lP-1];
                ImgForm.OpenAndDisplayImg(lFilename,True);
                lFilename := HdrForm.OpenHdrDlg.Files[lInc-1];
	              ImgForm.OverlayOpenCore ( lFilename, 2);
                if lP = 1 then
                   lStr := lStr + kTextSep+ inttostr(VOIVol(2) );
                if lLoop = 3 then
                  lStr := lStr + kTextSep+ floattostr(VOIMeanFrac10Pct(2,false))
                else if lLoop = 2 then
                  lStr := lStr + kTextSep+ floattostr(VOIMeanFrac10Pct(2,true))
                else
                  lStr := lStr + kTextSep+ floattostr(VOIMean(2));
            end;
                TextForm.MemoT.lines.add(lStr );
        end;
    end;//lLoop
        FreeImgMemory(gMRIcroOverlay[2]);
        ImgForm.UpdateLayerMenu;
        //SaveDialog1.Filename := ExtractFileDirWithPathDelim(HdrForm.OpenHdrDlg.Files[0])+'desc.csv';
        lBGStrings.Free;
        //ImgForm.SaveDialog1.Filename := ExtractFileDirWithPathDelim(gMRIcroOverlay[lOverlayNum].HdrFileName)+'desc.csv';
        TextForm.Show;
end;

end.
 
