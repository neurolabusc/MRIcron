unit unpm;
{$IFDEF FPC}{$mode objfpc}{$H+}    {$ENDIF}
{$Include ..\common\isgui.inc}
interface

uses
  upower,math,utypes,
regmult,IniFiles,Classes, SysUtils , nifti_types, define_types,distr, statcr, StatThdsUtil, userdir, dialogsx, nifti_hdr, StatThds, lesion_pattern;
Type
  TNPMPrefs = record
         NULP,ROI,ttest,BMtest: boolean;
         TFCE,PlankMB,nPermute: integer;
  end;

  procedure ComputePlankSize (var lPlankMB: integer);
  procedure InitPermute (lnSubj, lnPermute: integer; var lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP; var  lRanOrderp: pointer; var lRanOrder: Doublep0);
  procedure NPMThreadDone;
  function reportBonferroni(lLabel: string; lnTests: integer): double; //returns 5% Z score
  function reportFDR (lLabel:string; lnVox, lnTests: integer; var lData: SingleP): double;
  function reportPermute (lLabel:string; lnPermute: integer; var lPermuteMaxZ, lPermuteMinZ: singleP): double;
  function ThreshMap(lThresh: single; lVolVox: integer;lOutImg: singleP): integer;
  procedure NPMMsgSave(lFilename: string);
  procedure NPMProgressBar(lPos: integer);
  procedure NPMmsg(str: string);
  procedure NPMMsgClear;
  function GetKVers: string;
  function ReportDescriptives (var RA: SingleP; n: integer): boolean;
  procedure FreePermute (lnPermute: integer; var lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;var  lRanOrderp: pointer);
  procedure ReadIniFile;
  procedure WriteIniFile;
  function Add2ndScans(var lImageNames: TStrings): boolean;
  function ChangeName (lInName: string): string;
  function NPMzscore (var lImages: TStrings; var lMnHdr,lStDevHdr: TMRIcroHdr): boolean;
  function NPMAnalyze (var lImages: TStrings; var lMaskname: string; lMaskVoxels,lnGroup1: integer; lNPMPrefs: TNPMPrefs; var lOutName: string): boolean;

  //function NPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lMaskVoxels,lnGroup1: integer; lNPMPrefs: TNPMPrefs; var lOutName: string): boolean;
   function ComputeLesionVolume (lImgName: string): integer;
   procedure Refresher;
   function MakeMean (lImages: TStrings; lBinarize,lVariance : boolean; lOutName: string): boolean;
    procedure NPMTitleMsg (S: string);
   //function MakeMean (var lImages: TStrings; lBinarize,lVariance : boolean; lOutName: string): boolean;
   function NPMAnalyzePaired (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lMaskVoxels: integer; lOutName: string): boolean;
   procedure NPMMultipleRegressClick(var lVALFilename, lMaskname,lOutname: string);
   procedure NPMSingleRegress (var lVALFilename, lMaskname,lOutname: string);
   function ComputeOverlap ( lROIname: string; var lLesionNames: TStrings; var lROIvol: double; lFracROIinjured: singlep): boolean;
   function Balance (var lImageName,lMaskName: String; {lInflection: boolean}lMethod: integer): boolean;
   function    ReadPairedFilenames(var lImageNames: TStrings): boolean;
  var
     gNPMPrefs: TNPMPrefs;


implementation
uses
    {$IFDEF GUI}npmform,Forms,{$ENDIF}       regression, valformat,
    firthThds,firth,hdr, prefs, nifti_img, tfce_clustering;
{$IFNDEF GUI}
var
   NPMmemo: TStringList;
{$ENDIF}

var
    gThreadsRunning: integer = 0;
procedure NPMTitleMsg (S: string);
begin
     MainForm.Caption := S;
end;


procedure NPMProgressBar(lPos: integer);
begin
     {$IFDEF GUI}
     MainForm.ProgressBar1.Position := lPos;
     MainForm.Refresh;
     {$ENDIF}
end;

procedure NPMMsgClear;
begin
    {$IFDEF GUI}
    MainForm.NPMMsgClearUI;
    {$ELSE}
     //shittt
    {$ENDIF}
end;

procedure NPMmsg(str: string);
begin
     {$IFDEF GUI}
     MainForm.NPMmsgUI(str);
     {$ELSE}
     writeln(str);
     //shittt
    {$ENDIF}
end;

procedure NPMMsgSave(lFilename: string);
var
       i: integer;
       f: textfile;
begin
     {$IFDEF GUI}
     MainForm.NPMmsgSaveUI(lFilename);
     {$ELSE}
     ShowMsg('SHITTTT');
     {$ENDIF}
end;



function GetKVers: string;
begin
     result :=  'Chris Rorden''s NPM '+kMRIcronVers+' :: '+inttostr(sizeof(integer)*8)+'-bit :: Threads used = '+inttostr(gnCPUThreads )+' :: plankSize = '+inttostr(gNPMPrefs.PlankMB)+'mb';
end;

procedure Refresher;
begin
     {$IFDEF GUI}
     Application.processmessages;
     MainForm.Refresh;
     {$ENDIF}
end;


function    ReadPairedFilenames(var lImageNames: TStrings): boolean;
var
   lLen,lPos: integer;
   lFilenames,lF1,lF2: string;
   lImageNames2:  TStrings;
   lF: TextFile;
begin
     result := false;
     ShowMsg('Please select a text file with the image names. '+kCR+
     'Each line of the file should specify the control and experimental filenames, separated by an *'+kCR+
       'C:\vbmdata\c1.nii.gz*C:\vbmdata\e1.nii.gz'+kCR +
       'C:\vbmdata\c2.nii.gz*C:\vbmdata\e2.nii.gz'+kCR+
       'C:\vbmdata\c3.nii.gz*C:\vbmdata\e3.nii.gz'+kCR+
       '...' );
(*SHITTT     if not MainForm.OpenDialogExecute('Select asterix separated filenames ',false,false,kTxtFilter) then
         exit;
     lImageNames2:= TStringList.Create; //not sure why TStrings.Create does not work???
     //xxx
     assignfile(lF,MainForm.OpenHdrDlg.FileName );    *)
  FileMode := 0;  //read only
     reset(lF);
     while not EOF(lF) do begin
           readln(lF,lFilenames);
           lLen := length(lFilenames);

           if lLen > 0 then begin
              lF1:= '';
              lF2 := '';
              lPos := 1;
              while (lPos <= lLen) and (lFilenames[lPos] <> '*') do  begin
                    lF1 := lF1 + lFilenames[lPos];
                    inc(lPos);
              end;
              inc(lPos);
              while (lPos <= lLen)  do  begin
                    lF2 := lF2 + lFilenames[lPos];
                    inc(lPos);
              end;
              if (length(lF1) > 0) and (length(lF2)>0) then begin
                 if Fileexists4D(lF1) then begin
                    if Fileexists4D(lF2) then begin
                       lImageNames.add(lF1);
                       lImageNames2.add(lF2);
                    end else //F2exists
                        ShowMsg('Can not find image '+lF2);
                 end else //F1 exists
                     ShowMsg('Can not find image '+lF1);
              end;
           end;//len>0
     end; //while not EOF
     closefile(lF);
       FileMode := 2;  //read/write
     if (lImageNames.count > 0) and (lImageNames2.count = lImageNames.count) then begin
        lImageNames.AddStrings(lImageNames2);

        result := true;
     end;
     lImageNames2.Free;
end;


function MinMax (var lImg: SingleP; var lVolVox: integer; var lMin, lMax: single): boolean;
var
   lC: integer;
begin
     result := false;
     if lVolVox < 1 then
        exit;
     lMax := lImg^[1];
     for lC := 1 to lVolVox do
         if lImg^[lC] > lMax then
            lMax := lImg^[lC];
            //lCx := lC;
     lMin := lImg^[1];
     for lC := 1 to lVolVox do
         if lImg^[lC] < lMin then
            lMin := lImg^[lC];
     result := true;
end;

function DetectMode (var lImg: SingleP; var lVolVox: integer; var lMin, lMax, lModeLo,lModeHi: single; lInflection: boolean): boolean;
const
     kHistoBins = 255;//numbers of bins for histogram/image balance
var
   lSmooth,lPrevSmooth,lModeWid,lC,lMinPos,lMode,lModePos,lMaxModePos,lMode2NotInflection: integer;
   lMod,lRng: single;
   lHisto : array [0..kHistoBins] of longint;
begin

     result := false;
     if (lVolVox < 1) or (lMax < lMin) then
        exit;
     //zero array
     for lC := 1 to kHistoBins do
         lHisto[lC] := 0;
     //find scaling
     lRng := abs(lMax-lMin);
     if lRng > 0 then
        lMod := (kHistoBins)/lRng
     else
         lMod := 0;
     //fill histogram
     for lC := 1 to lVolVox do
         if lImg^[lC] <> 0 then
         inc(lHisto[round((lImg^[lC]-lMin)*lMod)]);

     {for lC := 1 to lVolVox do
         inc(lHisto[round((lImg^[lC]-lMin)*lMod)]); }
     //smooth histogram
     lPrevSmooth := lHisto[1];
     for lC := 2 to (kHistoBins-1) do begin
         lSmooth := round( (lHisto[lC-1]+lHisto[lC]+lHisto[lC]+lHisto[lC+1])/4);
         lHisto[lC-1] := lPrevSmooth;
         lPrevSmooth := lSmooth;
     end;
     lHisto[kHistoBins-1] := lPrevSmooth;
     //find mode
     lMode := 0;
     lMinPos := 1;//indexed from zero
     //find highest peak
	 for lC := lMinPos to kHistoBins do begin
	     if lHisto[lC] > lMode then begin
                lModePos := lC;
                lMode := lHisto[lC];
             end;//if new mode
         end; //for each bin
         if lMode > 0 then
            lMaxModePos := lModePos
         else
             exit;
     //find 2nd highest peak
         //find 2nd highest peak
         lModeWid := 25;
         lModePos := lMinPos;
         lMode := lHisto[lMinPos];
         if (lMaxModePos - lModeWid) > lMinPos then begin
	   for lC := lMinPos to (lMaxModePos - lModeWid) do begin
	     if lHisto[lC] > lMode then begin
                lModePos := lC;
                lMode := lHisto[lC];
             end;//if new mode
           end; //for each bin
         end; //check below highest peak
         if (lMaxModePos + lModeWid) < kHistoBins then begin
	   for lC := (lMaxModePos + lModeWid) to kHistoBins do begin
	     if lHisto[lC] > lMode then begin
                lModePos := lC;
                lMode := lHisto[lC];
             end;//if new mode
           end; //for each bin
         end; //check above highest peak
         //fx(lModePos);
     //an alternative method to find mode is to look for inflection - less assumptions, more sensitive to noise
     if lInflection then begin
         lMode2NotInflection := lModePos;
         lModePos := lMinPos;

         lMode := 0;
         lC := lMaxModePos;
         while ((lC-1) > lMinPos) and (lHisto[lC] > lHisto[lC-1]) do
               dec(lC); //find inflection
         while ((lC-1) > lMinPos) do begin
             dec(lC);
	     if lHisto[lC] > lMode then begin
                lModePos := lC;
                lMode := lHisto[lC];
             end;//if new mode
         end; //look for mode

         lC := lMaxModePos;
         while ((lC+1) <= kHistoBins) and (lHisto[lC] > lHisto[lC+1]) do
               inc(lC); //find inflection
         while ((lC+1) <= kHistoBins) do begin
             inc(lC);
	     if lHisto[lC] > lMode then begin
                lModePos := lC;
                lMode := lHisto[lC];
             end;//if new mode
         end; //look for mode

         if abs(lMode2NotInflection-lModePos) > 3 then
             ShowMsg('Warning: inflection and windowed algorithms find different 2nd modes. Using inflection 2nd mode. inflection ='+inttostr(lModePos)+'  windowed: '+inttostr(lMode2NotInflection));

     end;
     //now, return scaled values...
     if lMod = 0 then exit;
     lModeLo := (lModePos/lMod)+lMin;
     lModeHi := (lMaxModePos/lMod)+lMin;
     if lModeLo > lModeHi then begin
         lMod := lModeLo;
         lModeLo := lModeHi;
         lModeHi := lMod;
     end;
     result := true;
end;

function DetectMeanStDev (var lImg: SingleP; var lVolVox: integer; var lMean,lStDev: double): boolean;
var
     lIncVox,lVox: integer;
     lSum,lSumSqr,lSE: double;
begin
     lMean := 0;
     lStDev := 0;
     result := false;
     if (lVolVox < 1)  then
        exit;
     lSum := 0;
     lSumSqr := 0;
     lIncVox := 0; //voxels included - e.g. not masked
     for lVox := 1 to lVolVox do begin
         if lImg^[lVox] <> 0 then begin //not in mask
            inc(lIncVox);
            lSum := lSum + lImg^[lVox];
            lSumSqr := lSumSqr + sqr(lImg^[lVox]);
         end;
     end;
     if lincVox < 3 then
        exit;
     Descriptive (lincVox, lSumSqr, lSum,lMean,lStDev,lSE);
     result := true;
end; //DetectMeanStDev



function Balance (var lImageName,lMaskName: String; {lInflection: boolean}lMethod: integer): boolean;
//0 =  masked peak
//1 = inflection
//2 = mean =1, stdev=1
var
   lImg,lMaskImg: SingleP;
   lHdr,lMaskHdr: TMRIcroHdr;
   lVolVox,lVox,lMasked: integer;
   lMaskedInten,lMean,lStDev: double;
   lMin,lMax: single;
   lModeLo,lModeHi,lIntercept,lSlope: single;
   lOutNameMod: string;
begin
	//lOutName := lMaskHdr.ImgFileName;
        result := false;
	//if not SaveHdrName ('Statistical Map', lOutName) then exit;
        if not NIFTIhdr_LoadHdr(lImageName,lHdr) then begin
	   ShowMsg('Error reading '+lImageName);
	   exit;
        end;
	lVolVox := lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]* lHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then exit;
	getmem(lImg,lVolVox*sizeof(single));
	if not LoadImg(lHdr.ImgFileName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load  ' +lHdr.ImgFileName);
		exit;
	end;
        if lMaskName <> '' then begin
           if not NIFTIhdr_LoadHdr(lMaskName,lMaskHdr) then begin
	      ShowMsg('Error reading '+lMaskName);
	      exit;
           end;
           if lVolVox <> (lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3]) then begin
	      ShowMsg('Mask and header must have identical dimensions '+lMaskName+ ' ' + lImageName);
	      exit;

           end;
           getmem(lMaskImg,lVolVox*sizeof(single));
	   if not LoadImg(lMaskHdr.ImgFileName, lMaskImg, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load mask ' +lMaskHdr.ImgFileName);
		exit;
           end;
           lMasked := 0;
           lMaskedInten := 0;
           for lVox := 1 to lVolVox do
               if lMaskImg^[lVox] = 0 then begin
                  lMaskedInten := lMaskedInten + lImg^[lVox];
                  lImg^[lVox] := 0;
                  inc(lMasked);
               end;
           if lMasked < 1 then
              NPMMsg('Warning: no voxels masked with image '+lMaskName)
           else
               NPMMsg('Mask='+ lMaskName+' Number of voxels masked= '+inttostr(lMasked)+'  Mean unscaled intensity of masked voxels= '+floattostr(lMaskedInten/lMasked));
           freemem(lMaskImg);
        end;//mask

        if not MinMax(lImg,lVolVox,lMin,lMax) then exit;
        NPMMsg(lImageName+'  -> '+lHdr.ImgFileName);
        NPMMsg('min..max ' +floattostr(lMin)+'..'+floattostr(lMax));
        if (lMethod = 0) or (lMethod = 1) then begin
           if not DetectMode(lImg,lVolVox,lMin,lMax,lModeLo,lModeHi, odd(lMethod)) then exit;
           if odd(lMethod) then
              NPMMsg('method for finding second mode: inflection')
           else
              NPMMsg('method for finding second mode: masked peak');
           NPMMsg('modes Lo Hi ' +floattostr(lModeLo)+'..'+floattostr(lModeHi));
           if lModeLo >= lModeHi then exit;
           lSlope := 1/abs(lModeHi-lModeLo);
           lIntercept := (abs(lModeHi-lModeLo)-(lModeLo))*lSlope ; //make mode lo = 1;
        end else begin
            DetectMeanStDev (lImg, lVolVox, lMean,lStDev);
            if lStDev <>0 then
               lSlope := 1/lStDev
            else begin
                NPMMsg('Warning: StDev = 0!!!!');
                lSlope := 1;
            end;
            lIntercept := (-lMean*lSlope)+2; //mean voxel has intensity of zero

            NPMMsg('method for intensity normalization: Mean = 2, StDev = 1');
            NPMMsg('raw_Mean = '+floattostr(lMean)+'  '+' raw_StDev = '+floattostr(lStDev));

        end;
        NPMMsg('Slope/Intercept ' +floattostr(lSlope)+'..'+floattostr(lIntercept));
        lHdr.NIFTIhdr.scl_slope := lSlope;
        lHdr.NIFTIhdr.scl_inter := lIntercept;
        //CopyFileEX(lHdr.HdrFilename,changefileext( lHdr.HdrFilename,'.hdx'));
        RenameFile(lHdr.HdrFilename,changefileext( lHdr.HdrFilename,'.hdx'));
        //optional - save input
        lOutNameMod :=  ChangeFilePrefixExt(lImageName,'i','.hdr');
        NIFTIhdr_SaveHdrImg(lOutNameMod,lHdr.NIFTIhdr,true,not IsNifTiMagic(lHdr.NIFTIhdr),true,lImg,1);
        //end optional
        NIFTIhdr_SaveHdr(lHdr.HdrFilename,lHdr.NIFTIhdr,true,not IsNifTiMagic(lHdr.NIFTIhdr));

	freemem(lImg);
end;

function ComputeOverlap ( lROIname: string; var lLesionNames: TStrings; var lROIvol: double; lFracROIinjured: singlep): boolean;
label 667;
var
	lName: string;
        lSum: double;
        lLesion,lnLesions,lVolVox,lVolVoxA,lVox: integer;
	lROIImg,lImgB: SingleP;
        lMaskHdr: TMRIcroHdr;
begin
   lROIvol := 0;
   result := false;
   lnLesions := lLesionNames.count;
   if lnLesions < 1 then begin
      ShowMsg('Error: no lesion names');
      exit;
   end;
   for lLesion := 1 to lnLesions do
       lFracROIinjured^[lLesion] := 0;
     //read A
   if not NIFTIhdr_LoadHdr(lROIname,lMaskHdr) then begin
	   ShowMsg('Error reading ROI - '+lROIname);
	   exit;
   end;
   lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
   if (lVolVox < 1) then begin
      ShowMsg('Error with Mask voxels ' + inttostr(lVolVox));
      exit;
   end;
   if not CheckVoxelsGroupX(lLesionNames, lMaskHdr) then begin
      ShowMsg('Error image dimensions vary.');
      exit;
   end;
   getmem(lROIImg,lVolVox*sizeof(single));
   getmem(lImgB,lVolVox*sizeof(single));
   if not LoadImg(lROIname, lROIImg, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMmsg('Unable to load lesion ' +lMaskHdr.ImgFileName);
		goto 667;
   end;
   lVolVoxA := lVolVox;
   for lVox := 1 to lVolVox do
       if  (lROIImg^[lVox] > 0) then
           lROIvol := lROIvol +lROIImg^[lVox];
   //read Lesion
   if lROIvol < 1 then begin
      		NPMmsg('ROI volume < 1');
		goto 667;
   end;
   //for each lesion
   //NPMmsg('Compute overlap started '+inttostr(lnLesions)+'  '+floattostr(lROIvol)+'  '+inttostr(lVolVoxA));
   NPMProgressBar( 0);
   for lLesion := 1 to lnLesions do begin
         NPMProgressBar(round((lLesion/lnLesions)*100)) ;
         lSum := 0;
         lName := lLesionNames.Strings[lLesion-1];
         if not NIFTIhdr_LoadHdr(lName,lMaskHdr) then begin
	   ShowMsg('Error reading lesion - '+lName);
	   exit;
         end;
         lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
         if (lVolVoxA <> lVolVox) or (lVolVox < 1) then begin
            NPMmsg('Volume does not have expected number of voxels ' +lMaskHdr.ImgFileName +'<>'+lROIname+ ' found ' +inttostr(lVolVox)+'  expected '+inttostr(lVolVoxA));
            goto 667;
         end;
	if not LoadImg(lName, lImgB, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMmsg('Unable to load mask ' +lMaskHdr.ImgFileName);
		goto 667;
	end;
        for lVox := 1 to lVolVox do begin
            //if {(lImgB^[lVox] <> 0) and} (lROIImg^[lVox] <> 0) then fx(lROIImg^[lVox]);

            if  (lROIImg^[lVox] > 0) and (lImgB^[lVox] <> 0) then
               lSum := lSum + lROIImg^[lVox];
        end;
       lFracROIinjured^[lLesion] := lSum/lROIvol;
   end;//for each lesion
   result := true;
   NPMProgressBar( 0);

   (*for lLesion := 1 to lnLesions do begin
       if lFracROIinjured^[lLesion] > 0 then
          fx( lFracROIinjured^[lLesion], lLesion);
   end;    *)

   667:

   freemem(lImgB);
   freemem(lROIImg);
end;

procedure NPMSingleRegress (var lVALFilename, lMaskname,lOutname: string);
label
	666;
var
	lnSubj1,lnFactors,lnSubj,lMaskVoxels,lRow,lCol: integer;
	lImageNames,lImageNames1:  TStrings;
        lPredictorList,lPredictorList1: TStringList;
	lTemp4D: string;
	lMaskHdr: TMRIcroHdr;
        X,X1 : PMatrix;
begin

  lTemp4D := '';
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  lPredictorList := TStringList.Create;
  lPredictorList1 := TStringList.Create;
  if not GetValReg(lVALFilename,lnSubj,lnFactors,X,lImageNames,lPredictorList) then
     goto 666;
  if (length(lMaskname) < 1) then
     lMaskName := lImageNames[0];
  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
        ShowMsg('Error reading mask image.');
        exit;
  end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
  if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,-1)){make sure there is uncompressed .img file}  then begin
	   ShowMsg('Mask file size too small.');
	   goto 666;
   end;
   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   ShowMsg('File dimensions differ from mask.');
	   goto 666;
   end;
   lTemp4D := CreateDecompressed4D(lImageNames);
   lImageNames1:= TStringList.Create;
   for lCol := 1 to lnFactors do begin
       lPredictorList1.Clear;
       lPredictorList1.Add(lPredictorList[lCol-1]);
       lImageNames1.clear;
       for lRow := 1 to lnSubj do
           if X^[lCol]^[lRow] <> kNaN then
              lImageNames1.Add(lImageNames[lRow-1]);
       DimMatrix(X1, 1, lImageNames1.Count);
       lnSubj1 := 0;

       for lRow := 1 to lnSubj do
           if X^[lCol]^[lRow] <> kNaN then begin
              inc(lnSubj1);
              X1^[1]^[lnSubj1] := X^[lCol]^[lRow];
           end;
       if lnSubj1 <>  lImageNames1.Count then //should be impossible
          ShowMsg('NPMSingleRegress: serious error - number of participants does not match');
       NPMMsgClear;
       NPMMsg(GetKVers);
       NPMMsg('Single Linear Regression [Weighted Least Squares]');
       NPMMsg('Mask = '+lMaskname);
       NPMMsg('Total voxels = '+inttostr(lMaskVoxels));
       NPMMsg('Number of observations = '+inttostr(lnSubj1));
       NPMMsg('Image,'+ lPredictorList1.Strings[0]);
       for lRow := 1 to lnSubj1 do
           NPMMsg(lImageNames1[lRow-1]+','+floattostr(X1^[1]^[lRow])  ) ;

       ARegressNPMAnalyze(lImageNames1,lMaskHdr,X1,1,lPredictorList1,lOutName, gNPMPrefs.nPermute,gNPMPrefs.TFCE);
       //PermuteRegressNPMAnalyze (lImageNames1,lMaskHdr,X1,1,lPredictorList1,lOutName);
       DelMatrix(X1, 1, lnSubj1);
   end;
   lImageNames1.Free;
   DelMatrix(X, lnFactors, lnSubj);
    666:
DeleteDecompressed4D(lTemp4D);
        lImageNames.Free;
        lPredictorList.Free;
        lPredictorList1.Free;
end;

procedure NPMMultipleRegressClick(var lVALFilename, lMaskname,lOutname: string);
label
	666;
var
	lnFactors,lnSubj,lMaskVoxels,lRow,lCol: integer;
	lImageNames:  TStrings;
        lPredictorList: TStringList;
	lTemp4D,lStr: string;
	lMaskHdr: TMRIcroHdr;
        X : PMatrix;
begin
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  lPredictorList := TStringList.Create;
  NPMMsgClear;
  NPMMsg(GetKVers);
  NPMMsg('Multiple Linear Regression [Weighted Least Squares]');
  if not GetValReg(lVALFilename,lnSubj,lnFactors,X,lImageNames,lPredictorList) then
     goto 666;
    lTemp4D := CreateDecompressed4D(lImageNames);

   //lMaskname := lImageNames[0];

  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   ShowMsg('Error reading 1st image.');
	   goto 666;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,-1)){make sure there is uncompressed .img file}  then begin
	   ShowMsg('Mask file size too small.');
	   goto 666;
   end;
   NPMMsg('Mask = '+lMaskname);
   NPMMsg('Total voxels = '+inttostr(lMaskVoxels));
   NPMMsg('Number of observations = '+inttostr(lnSubj));
   if not CheckVoxelsGroupX(lImageNames,lMaskHdr {lMaskVoxels}) then begin
	   ShowMsg('File dimensions differ from mask.');
	   goto 666;
   end;
   //show matrix
   lStr := 'Image,';
   for lCol := 1 to lnFactors do
            lStr := lStr + lPredictorList.Strings[lCol-1]+', ';
       NPMmsg(lStr);
   for lRow := 1 to lnSubj do begin
       lStr := lImageNames[lRow-1]+',';
       for lCol := 1 to lnFactors do
            lStr := lStr + floattostr(X^[lCol]^[lRow])+', ';
       NPMmsg(lStr);
   end;

   ARegressNPMAnalyze(lImageNames,lMaskHdr,X,lnFactors,lPredictorList,lOutName,0,0);

   DelMatrix(X, lnFactors, lnSubj);
    666:
        lImageNames.Free;
        lPredictorList.Free;
        DeleteDecompressed4D(lTemp4D);
end;

function NPMAnalyzePaired (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lMaskVoxels: integer; lOutName: string): boolean;
label
	667;
var
	//lOutName,
        lOutNameMod: string;
	lMaskImg,lPlankImg,lOutImgMn,lOutImgT,lDummy,lDummy2: SingleP;
        lTotalMemory: double; //not integer - limit for 32bit int is 2Gb
	lPlank,lVolVox,lPos,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lThreadStart,lThreadEnd,lThreadInc: integer;
	lT,  lSum, lMn: double;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lThread,lnPermute: integer;
        lPermuteMaxT, lPermuteMinT: singleP;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
begin
        //lnPermute := ReadPermute;
        lnPermute := 0;//not yet
	NPMmsg('Permutations = ' +IntToStr(lnPermute));

	NPMmsg('Analysis began = ' +TimeToStr(Now));
	lTotalMemory := 0;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
	//load mask
	getmem(lMaskImg,lVolVox*sizeof(single));
	if not LoadImg(lMaskHdr.ImgFileName, lMaskImg, 1, lVolVox,round(gOffsetRA[0]),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMmsg('Unable to load mask ' +lMaskHdr.ImgFileName);
		goto 667;
	end;
	//next find start and end of mask
	lPos := 0;
	repeat
		inc(lPos);
	until (lMaskImg^[lPos] > 0) or (lPos = lVolVox);
	lMinMask := lPos;
	lPos := lVolVox+1;
	repeat
		dec(lPos);
	until (lMaskImg^[lPos] > 0) or (lPos = 1);
	lMaxMask := lPos;
	if lMaxMask = 1 then begin
		NPMmsg('Mask appears empty' +lMaskHdr.ImgFileName);
		goto 667;
	end;
	NPMmsg('Mask has voxels from '+inttostr(lMinMask)+'..'+inttostr(lMaxMask));
	lVoxPerPlank :=  kPlankSz div lImages.Count div sizeof(single) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
	NPMmsg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	NPMmsg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
	getmem(lPlankImg,kPlankSz);
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
        for lPos := 1 to lImages.Count do
		if gScaleRA[lPos] = 0 then
			gScaleRA[lPos] := 1;
        getmem(lOutImgMn,lVolVox* sizeof(single));
	getmem(lOutImgT,lVolVox* sizeof(single));
        //not yet InitPermute (lImages.Count, lnPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do begin
                lOutImgMn^[lPos] := 0;
		lOutImgT^[lPos] := 0;
	end;
        ClearThreadData(gnCPUThreads,lnPermute);
	for lPlank := 1 to lnPlanks do begin
		NPMmsg('Computing plank = ' +Inttostr(lPlank));
                Refresher;
                lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lMaxMask then begin
			lVoxPerPlank := lVoxPerPlank - (lEndVox-lMaxMask);
			lEndVox := lMaxMask;
		end;
		lPlankImgPos := 1;
		for lPos := 1 to lImages.Count do begin
			if not LoadImg(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
				goto 667;
			lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
                //threading start
                (**SHITTTT
                lThreadStart := 1;
                lThreadInc := lVoxPerPlank  div gnCPUThreads;
                lThreadEnd := lThreadInc;
                Application.processmessages;
                for lThread := 1 to gnCPUThreads do begin
                    if lThread = gnCPUThreads then
                       lThreadEnd := lVoxPerPlank; //avoid integer rounding error
                    with TPairedTStat.Create (ProgressBar1,false,false,0, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,666, lMaskImg,lPlankImg,lOutImgMn,lDummy2,lOutImgT,lDummy) do
                         {$IFDEF FPC} OnTerminate := @ThreadDone; {$ELSE}OnTerminate := ThreadDone;{$ENDIF}
                    inc(gThreadsRunning);
                    lThreadStart := lThreadEnd + 1;
                    lThreadEnd :=lThreadEnd + lThreadInc;
                end; //for each thread
                repeat
                      Application.processmessages;
                until gThreadsRunning = 0;
                Application.processmessages;
                *)
                //threading end
		lStartVox := lEndVox + 1;
	end;
        lnVoxTested := SumThreadDataLite(gnCPUThreads);//not yet SumThreadData(gnCPUThreads,lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM);
        //next report findings
	NPMmsg('Voxels tested = ' +Inttostr(lnVoxTested));
        reportBonferroni('Std',lnVoxTested);
        //next: save data
(*savedata *)
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save mean
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Mean','.hdr');
 if lnVoxTested > 1 then

        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgMn,1);
        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );

if (lnVoxTestED > 1 ) then begin //save Ttest
        //next: convert t-scores to z scores
        for lPos := 1 to lVolVox do
            lOutImgT^[lPos] := TtoZ (lOutImgT^[lPos],(lImages.Count div 2)-1);
        for lPos := 1 to lnPermute do begin
            lPermuteMaxT^[lPos] := TtoZ (lPermuteMaxT^[lPos],lImages.Count-2);
            lPermuteMinT^[lPos] := TtoZ (lPermuteMinT^[lPos],lImages.Count-2);
        end;

        reportFDR ('ttest', lVolVox, lnVoxTested, lOutImgT);
        reportPermute('ttest',lnPermute,lPermuteMaxT, lPermuteMinT);
	lOutNameMod := ChangeFilePostfixExt(lOutName,'ttest','.hdr');
	NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgT,1);
end;
//next: close images
        //not yet FreePermute (lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,  lRanOrderp);
	freemem(lOutImgT);
        freemem(lOutImgMn);
	//freemem(lObsp);
	freemem(lMaskImg);
	freemem(lPlankImg);
	NPMmsg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes','.txt');
        NPMMsgSave(lOutNameMod);
        NPMProgressBar(0);
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lVolVox > 1 then freemem(lMaskImg);
	if lTotalMemory > 1 then freemem(lPlankImg);
	NPMmsg('Unable to complete analysis.');
        NPMProgressBar(0);
end;


(*function ApplyTFCE (lImageName: string): boolean;
var
   lImg: SingleP;
   lHdr: TMRIcroHdr;
   lVolVox: integer;
   maxTFCE, maxNegTFCE: single;
   lOutNameMod: string;
begin
	result := false;
	if not NIFTIhdr_LoadHdr(lImageName,lHdr) then begin
	   ShowMsg('Error reading '+lImageName);
	   exit;
  end;
	lVolVox := lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]* lHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then exit;
	getmem(lImg,lVolVox*sizeof(single));
	if not LoadImg(lHdr.ImgFileName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load  ' +lHdr.ImgFileName);
		exit;
	end;
  //lHdr.NIFTIhdr.scl_slope := 1; lHdr.NIFTIhdr.scl_inter := 0;
  doTFCEbothPolarities (lHdr.NIFTIhdr,  lImg, 6 {NumConn}, 2.0 {H}, 0.5 { E}, 0, 0,0,0 ,maxTFCE, maxNegTFCE);

  lOutNameMod :=  ChangeFilePrefixExt(lImageName,'i','.hdr');
  NPMMsg('Creating  ' +lOutNameMod);
  NIFTIhdr_SaveHdrImg(lOutNameMod,lHdr.NIFTIhdr,true,not IsNifTiMagic(lHdr.NIFTIhdr),true,lImg,1);
	freemem(lImg);
end;*)


function MakeMean ( lImages: TStrings; lBinarize,lVariance : boolean; lOutName: string): boolean;
label
	667;
var
        lMaskname,lOutNameMod: string;
        lMaskHdr: TMRIcroHdr;
	lCountRA,lOutImgMn,lOutStDev,lPlankImg: SingleP;
        lTotalMemory: double;
	lMaskVoxels,lPlank,lVolVox,lPos,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lPosPct: integer;
        lStDev: boolean;
	lT,  lSum,lSumSqr,lSD, lMn,lTotalSum,lTotalN: double;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
begin

     if lImages.count < 2 then begin
        ShowMsg('Error: you must select at least two images');
        exit;
     end;
     lMaskname := lImages[0];
     if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   ShowMsg('Error reading '+lMaskName);
	  exit;
     end;
     lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
     if not CheckVoxelsGroupX(lImages,lMaskHdr {lMaskVoxels}) then begin
	   ShowMsg('File dimensions differ from mask.');
	   exit;
     end;


        result := false;
        NPMMsgClear;
        NPMMsg(GetKVers);

        if (not lVariance) and (not lBinarize) then
           lStDev := true
        else
            lStDev := false;
	NPMMsg('Analysis began = ' +TimeToStr(Now));
	lTotalMemory := 0;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
        NPMMsg('Voxels = '+inttostr(lMaskVoxels)+'  '+inttostr(kPlankSz));
	if (lVolVox < 1) then goto 667;
	lMinMask := 1;
	lMaxMask := lVolVox;
	lVoxPerPlank :=  kPlankSz div lImages.Count div sizeof(single) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
	NPMMsg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	NPMMsg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
       // fx(kPlankSz,8888);
	getmem(lPlankImg,kPlankSz);
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
        NPMMsg('Number of scans = '+inttostr(lImages.count));
        NPMMsg(' Index,Filename,Intercept,Slope');
        if lBinarize then begin
           getmem(lCountRA,lImages.Count*sizeof(single));
           for lPos := 1 to lImages.Count do begin
            gInterceptRA[lPos] := 0;
            gScaleRA[lPos] := 1;
            lCountRA^[lPos] := 0;
           end;
        end else begin
            for lPos := 1 to lImages.Count do begin
                NPMMsg('  '+inttostr(lPos)+','+lImages[lPos-1]+','+realtostr(gInterceptRA[lPos],4)+','+realtostr(gScaleRA[lPos],4));
                if gScaleRA[lPos] = 0 then
			gScaleRA[lPos] := 1;
            end;
        end;

        lTotalSum := 0;
        lTotalN := 0;
	//createArray64(lObsp,lObs,lImages.Count);
        getmem(lOutImgMn,lVolVox* sizeof(single));
        if lStDev then
           getmem(lOutStDev,lVolVox* sizeof(single));
	for lPlank := 1 to lnPlanks do begin
		NPMMsg('Computing plank = ' +Inttostr(lPlank));
                Refresher;
		lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lMaxMask then begin
			lVoxPerPlank := lVoxPerPlank - (lEndVox-lMaxMask);
			lEndVox := lMaxMask;
		end;
		lPlankImgPos := 1;
		for lPos := 1 to lImages.Count do begin
			if not LoadImg(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
				goto 667;
			lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
                lPosPct := lVoxPerPlank div 100;
		for lPos2 := 1 to lVoxPerPlank do begin
                        if (lPos2 mod lPosPct) = 0 then begin
                           NPMProgressBar(round((lPos2/lVoxPerPlank)*100));
                           refresher;
                        end;
			lPos2Offset := lPos2+lStartVox-1;
                        lSum := 0;
                        if lVariance then begin
                          lSum := sqr(lPlankImg^[lPos2]-lPlankImg^[lVoxPerPlank+lPos2]);//actually variance...
                          //% signal
                          //if lPlankImg[lVoxPerPlank+lPos2] <> 0 then

                          //   lSum := lPlankImg[lPos2]/lPlankImg[lVoxPerPlank+lPos2]
                          //else
                          //    lSum := 0;//pct signal...
                          //end % signal
                          lOutImgMn^[lPos2Offset] := lSum;
                           lTotalSum := lTotalSum + lOutImgMn^[lPos2Offset];
                           lTotalN := lTotalN + 1;
                        end else begin //not variance

                            if lBinarize then begin
                               for lPos := 1 to lImages.Count do
                                   if lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2] <> 0 then begin
                                      lSum := lSum+1;
                                      lCountRA^[lPos] := lCountRA^[lPos] + 1;
                                   end;
                            end else
                                for lPos := 1 to lImages.Count do
                                    lSum := lSum +(gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos];
                              // fx(lPos, gScaleRA[lPos],gInterceptRA[lPos]);
                           lOutImgMn^[lPos2Offset] := lSum/lImages.Count;
                           if lStDev then begin
                              //lSum := 0;
                              //for lPos := 1 to lImages.Count do
                              //      lSum := lSum +  (gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos];
                              lSumSqr := 0;
                              for lPos := 1 to lImages.Count do
                                    lSumSqr := lSumSqr +  Sqr((gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos]);
                              lSD := (lSumSqr - ((Sqr(lSum))/lImages.Count));
			      if  (lSD > 0) then
				lSD :=  Sqrt ( lSD/(lImages.Count-1))
                              else begin
				lSD := 0;

                              end;
                              lOutStDev^[lPos2Offset] := lSD;
                           end;
                         end; //not variance
                         if lSum > 0 then begin
                           lTotalSum := lTotalSum + lOutImgMn^[lPos2Offset];
                           lTotalN := lTotalN + 1;
                         end;

		end;
		lStartVox := lEndVox + 1;
	end;
        if lBinarize then begin
           for lPos := 1 to lImages.Count do begin
                NPMMsg('  '+inttostr(lPos)+','+lImages[lPos-1]+','+inttostr(round(lCountRA^[lPos]))  );

            lCountRA^[lPos] := 0;
           end;
           freemem(lCountRA);
        end; //if binar
        //next: save data
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save mean


if lVariance then
        lOutNameMod := ChangeFilePostfixExt(lOutName,'var','.hdr')
else
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Mean','.hdr');
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgMn,1);
        freemem(lOutImgMn);
        if lStDev then begin
           lOutNameMod := ChangeFilePostfixExt(lOutName,'StDev','.hdr');
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutStDev,1);
           freemem(lOutStDev);
        end;

	//freemem(lObsp);
	freemem(lPlankImg);
	NPMMsg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes','.txt');
        NPMMsgSave(lOutNameMod);
        if (lTotalN > 0) then
	   NPMMsg('num voxels >0 = ' +inttostr(round(lTotalN))+'  mean value for voxels >0: '+floattostr(lTotalSum/lTotalN));

        NPMProgressBar(0);
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lTotalMemory > 1 then freemem(lPlankImg);
	NPMMsg('Unable to complete analysis.');
        NPMProgressBar(0);
end;
function ComputeLesionVolume (lImgName: string): integer;
var
   lHdr: TMRIcroHdr;
   lImg: byteP;
   lVolVox,lVox:integer;
begin
     result := -1; //error
     NIFTIhdr_LoadHdr(lImgName,lHdr);
     lVolVox := lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]* lHdr.NIFTIhdr.dim[3];
     getmem(lImg,lVolVox*sizeof(byte));
     if not LoadImg8(lImgName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load  ' +lHdr.ImgFileName);
                freemem(lImg);
		exit;
     end;
     result := 0;
     for lVox := 1 to lVolVox do
         if (lImg^[lVox] <> 0) then
            inc(result);
     freemem(lImg);
end;

function NPMAnalyze (var lImages: TStrings; var lMaskname: string; lMaskVoxels,lnGroup1: integer; lNPMPrefs: TNPMPrefs; var lOutName: string): boolean;

label
	667;
var
	//lOutName,
        lOutNameMod: string;
        lMaskHdr: TMRIcroHdr;
	lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lDummy: SingleP;
        lTotalMemory: double; //not integer - limit for 32bit int is 2Gb
	lPlank,lVolVox,lPos,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lThreadStart,lThreadEnd,lThreadInc: integer;
	lT,  lSum, lMn: double;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lThread: integer;
        lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
begin
     result := false;
     NPMMsgClear;
  NPMMsg(GetKVers);
  NPMMsg('Threads: '+inttostr(gnCPUThreads));

   NPMMsg('Mask name = '+ lMaskname);
   NPMMsg('Total voxels = '+inttostr(lMaskVoxels));
   NPMMsg('Scans in Group 1 = '+inttostr(lnGroup1));
   NPMMsg('Scans in Group 2 = '+inttostr(lImages.count-lnGroup1));
   if (lnGroup1 < 1) or ((lImages.count-lnGroup1) < 1) then begin
      ShowMsg('Error: group size(s) too small');
      exit;
   end;

  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showMsg('Error reading mask.');
	   exit;
  end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   ShowMsg('Mask file size too small.');
	   exit;
   end;
   if not CheckVoxelsGroupX(lImages,lMaskHdr ) then begin
	   ShowMsg('File dimensions differ from mask.');
	   exit;
   end;


	NPMmsg('Permutations = ' +IntToStr(lNPMPrefs.nPermute));
	NPMmsg('Analysis began = ' +TimeToStr(Now));
	lTotalMemory := 0;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
	//load mask
	getmem(lMaskImg,lVolVox*sizeof(single));
	if not LoadImg(lMaskHdr.ImgFileName, lMaskImg, 1, lVolVox,round(gOffsetRA[0]),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMmsg('Unable to load mask ' +lMaskHdr.ImgFileName);
		goto 667;
	end;
	//next find start and end of mask
	lPos := 0;
	repeat
		inc(lPos);
	until (lMaskImg^[lPos] > 0) or (lPos = lVolVox);
	lMinMask := lPos;
	lPos := lVolVox+1;
	repeat
		dec(lPos);
	until (lMaskImg^[lPos] > 0) or (lPos = 1);
	lMaxMask := lPos;
	if lMaxMask = 1 then begin
		NPMmsg('Mask appears empty' +lMaskHdr.ImgFileName);
		goto 667;
	end;
	NPMmsg('Mask has voxels from '+inttostr(lMinMask)+'..'+inttostr(lMaxMask));
	lVoxPerPlank :=  kPlankSz div lImages.Count div sizeof(single) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
	NPMmsg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	NPMmsg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
	getmem(lPlankImg,kPlankSz);
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
        for lPos := 1 to lImages.Count do
		if gScaleRA[lPos] = 0 then
			gScaleRA[lPos] := 1;

        getmem(lOutImgMn,lVolVox* sizeof(single));
	getmem(lOutImgBM,lVolVox* sizeof(single));
	getmem(lOutImgT,lVolVox* sizeof(single));
        InitPermute (lImages.Count, lNPMPrefs.nPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do begin
                lOutImgMn^[lPos] := 0;
		lOutImgBM^[lPos] := 0;
		lOutImgT^[lPos] := 0;
	end;
        ClearThreadData(gnCPUThreads,lNPMPrefs.nPermute);

	for lPlank := 1 to lnPlanks do begin
		NPMmsg('Computing plank = ' +Inttostr(lPlank));
    Refresher;
		lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lMaxMask then begin
			lVoxPerPlank := lVoxPerPlank - (lEndVox-lMaxMask);
			lEndVox := lMaxMask;
		end;
		lPlankImgPos := 1;
    for lPos := 1 to lImages.Count do begin
			if not LoadImg(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
				goto 667;
      lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
                //threading start
                lThreadStart := 1;
                lThreadInc := lVoxPerPlank  div gnCPUThreads;
                lThreadEnd := lThreadInc;
      (*SHITTTT          Application.processmessages;
                for lThread := 1 to gnCPUThreads do begin
                    if lThread = gnCPUThreads then
                       lThreadEnd := lVoxPerPlank; //avoid integer rounding error
                    with TNNStat.Create (ProgressBar1,lttest,lBM,0, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lnGroup1, lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lDummy) do
                         {$IFDEF FPC} OnTerminate := @ThreadDone; {$ELSE}OnTerminate := ThreadDone;{$ENDIF}
                    inc(gThreadsRunning);
                    lThreadStart := lThreadEnd + 1;
                    lThreadEnd :=lThreadEnd + lThreadInc;
                end; //for each thread
                repeat
                      Application.processmessages;
                until gThreadsRunning = 0;
                Application.processmessages;
                *)
                //threading end
		lStartVox := lEndVox + 1;
	end;
        lnVoxTested := SumThreadData(gnCPUThreads,lNPMPrefs.nPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM);
        //next report findings
	NPMmsg('Voxels tested = ' +Inttostr(lnVoxTested));
        reportBonferroni('Std',lnVoxTested);
        //next: save data
(*savedata*)
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save mean
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Mean','.hdr');
 if lnVoxTested > 1 then

        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgMn,1);
        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );

if (lNPMPrefs.ttest) and (lnVoxTestED > 1 ) then begin //save Ttest
        //reportRange ('ttest', lVolVox, lnVoxTested, lOutImgT);
        //next: convert t-scores to z scores
        for lPos := 1 to lVolVox do
            lOutImgT^[lPos] := TtoZ (lOutImgT^[lPos],lImages.Count-2);
        for lPos := 1 to lNPMPrefs.nPermute do begin
            lPermuteMaxT^[lPos] := TtoZ (lPermuteMaxT^[lPos],lImages.Count-2);
            lPermuteMinT^[lPos] := TtoZ (lPermuteMinT^[lPos],lImages.Count-2);
        end;

        reportFDR ('ttest', lVolVox, lnVoxTested, lOutImgT);
        reportPermute('ttest',lNPMPrefs.nPermute,lPermuteMaxT, lPermuteMinT);
	lOutNameMod := ChangeFilePostfixExt(lOutName,'ttest','.hdr');
	NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgT,1);
end;
if (lNPMPrefs.BMtest) and (lnVoxTested > 1 ) then begin //save Brunner Munzel
       reportFDR ('BM', lVolVox, lnVoxTested, lOutImgBM);
        reportPermute('BM',lNPMPrefs.nPermute,lPermuteMaxBM, lPermuteMinBM);
        lOutNameMod := ChangeFilePostfixExt(lOutName,'BM','.hdr');

       {reportFDR ('absT', lVolVox, lnVoxTested, lOutImgBM);
        reportPermute('absT',lnPermute,lPermuteMaxBM, lPermuteMinBM);
        lOutNameMod := ChangeFilePostfixExt(lOutName,'absT','.hdr');
         }
 	//NIFTIhdr_SaveHdr(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr));
	lOutNameMod := changefileext(lOutNameMod,'.img');
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgBM,1);
end;(**)
//next: close images
        FreePermute (lNPMPrefs.nPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,  lRanOrderp);
	freemem(lOutImgT);
	freemem(lOutImgBM);
        freemem(lOutImgMn);
	//freemem(lObsp);
	freemem(lMaskImg);
	freemem(lPlankImg);
	NPMmsg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes','.txt');
        NPMMsgSave(lOutNameMod);
        NPMProgressBar(0);
        result := true;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lVolVox > 1 then freemem(lMaskImg);
	if lTotalMemory > 1 then freemem(lPlankImg);
	NPMmsg('Unable to complete analysis.');
        NPMProgressBar(0);
end;

(*procedure MakeStatHdr (var lBGHdr,lStatHdr: TniftiHdr; lMinIntensity,lMaxIntensity,lIntent_p1,lIntent_p2,lIntent_p3: single; lIntent_code: smallint;lIntentName: string);
var lIntentNameLen,lPos: integer;
    lStr: string;
begin
    move(lBGHdr,lStatHdr,sizeof(TniftiHdr));
	with lStatHdr do begin
		magic :=kNIFTI_MAGIC_SEPARATE_HDR;
		bitpix := 32; //32-bit real data
		datatype := kDT_FLOAT;
		scl_slope:= 1;
		scl_inter:= 0;
		glmin := round(lMinIntensity);
		glmax := round(lMaxIntensity);
		intent_code := lIntent_Code;// kNIFTI_INTENT_ESTIMATE;
		intent_p1 := lIntent_p1;
		intent_p2 := lIntent_p2;
		intent_p3 := lIntent_p3;
		lIntentNameLen := length(lIntentName);
                descrip[1] := 'N';
                descrip[2] := 'P';
                descrip[3] := 'M';
                if lIntent_code=kNIFTI_INTENT_TTEST then begin
                    descrip[4] := 't' ;
                    lStr := inttostr(trunc(lIntent_p1));
                    for lPos := 1 to length (lStr) do
                        descrip[4+lPos] := lStr[lPos] ;
                end else
                   descrip[4] := 'z';
		if lIntentNameLen > sizeof(intent_name) then
			lIntentNameLen := sizeof(intent_name);
		if lIntentNameLen > 0 then
			for lPos := 1 to lIntentNameLen do
				intent_name[lPos] := lIntentName[lPos];
	end;
end;  *)

function NPMzscore (var lImages: TStrings; var lMnHdr,lStDevHdr: TMRIcroHdr): boolean;
label
	667;
var
	lOutNameMod: string;
	lMnImg,lStDevImg,lSubjImg,lOutImg: SingleP;
        lVal: single;
	lSubj,lPos,lVolVox: integer;
	lStatHdr: TNIfTIhdr;
begin
        result := false;
	NPMMsg('Analysis began = ' +TimeToStr(Now));
	lVolVox := lMnHdr.NIFTIhdr.dim[1]*lMnHdr.NIFTIhdr.dim[2]* lMnHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
	//load mask
        for lPos := 0 to lImages.Count do
            if gScaleRA[lPos] = 0 then
               gScaleRA[lPos] := 1;
        if gScaleRA[kMaxImages] = 0 then
           gScaleRA[kMaxImages] := 1;

	getmem(lMnImg,lVolVox*sizeof(single));
	if not LoadImg(lMnHdr.ImgFileName, lMnImg, 1, lVolVox,round(gOffsetRA[0]),1,lMnHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load mean ' +lMnHdr.ImgFileName);
		goto 667;
	end;
	//load StDev
	getmem(lStDevImg,lVolVox*sizeof(single));
	if not LoadImg(lStDevHdr.ImgFileName, lStDevImg, 1, lVolVox,round(gOffsetRA[kMaxImages]),1,lStDevHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load StDev ' +lStDevHdr.ImgFileName);
		goto 667;
	end;
        getmem(lOutImg,lVolVox* sizeof(single));
        for lPos := 1 to lVolVox do begin
            lMnImg^[lPos] := (gScaleRA[0]*lMnImg^[lPos])+gInterceptRA[0];
            lStDevImg^[lPos] := (gScaleRA[kMaxImages]*lStDevImg^[lPos])+gInterceptRA[kMaxImages];
            if lStDevImg^[lPos] = 0 then
               lOutImg^[lPos] := 0;
        end;
	getmem(lSubjImg,lVolVox* sizeof(single));
        for lSubj := 1 to lImages.Count do begin
                NPMProgressBar(round((lSubj/lImages.Count)*100));
                NPMMsg( lImages.Strings[lSubj-1]);
                ShowMsg(inttostr(round(gOffsetRA[lSubj])));
                LoadImg(lImages.Strings[lSubj-1], lSubjImg, 1, lVolVox,round(gOffsetRA[lSubj]),1,gDataTypeRA[lSubj],lVolVox);
                for lPos := 1 to lVolVox do begin
                    if lStDevImg^[lPos] <> 0 then begin
                       lVal := (gScaleRA[lSubj]*lSubjImg^[lPos])+gInterceptRA[lSubj];
                       lOutImg^[lPos] := (lVal-lMnImg^[lPos])/lStDevImg^[lPos];
                    end; //for each voxel with variance
                end; //for each voxel
                lOutNameMod := ChangeFilePostfixExt(lImages.Strings[lSubj-1],'Z','.hdr');
                MakeStatHdr (lMnHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lVolVox,kNIFTI_INTENT_ZSCORE,inttostr(lVolVox) );
                NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMnHdr.NIFTIhdr),true,lOutImg,1);
        end; //for each subj
	freemem(lSubjImg);
        freemem(lOutImg);
	freemem(lMnImg);
	freemem(lStDevImg);
	NPMMsg('Analysis finished = ' +TimeToStr(Now));
        NPMProgressBar(0);
        result := true;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lVolVox > 1 then freemem(lMnImg);
	NPMMsg('Unable to complete analysis.');
        NPMProgressBar(0);
end;

function ChangeName (lInName: string): string;
var
    lPath,lName,lExt: string;
begin
    FilenameParts (lInName, lPath,lName,lExt);
    if length(lName) > 0 then
       lName[1] := 'e'
    else
        lName := 'Unable to convert '+lInName;
    result := lPath+lName+lExt;
end;

function Add2ndScans(var lImageNames: TStrings): boolean;
var
   lnSubj,lSubj: integer;
   lFilename: string;
begin
     result := false;
     lnSubj :=lImageNames.Count;
     if lnSubj < 1 then
        exit;
     for lSubj := 1 to lnSubj do begin
         lFilename := ChangeName(lImageNames[lSubj-1]);
         if not (fileexists4D(lFilename)) then begin
             ShowMsg('Unable to find a file named '+ lFilename);
             exit;
         end;
         lImageNames.add(lFilename);
     end;
     result := true;
end;

procedure ComputePlankSize (var lPlankMB: integer);
begin
     if lPlankMB < 128 then
        lPlankMB := 128;
     {$IFDEF CPU32}
     if lPlankMB > 1536 then
        lPlankMB := 1536; //we use signed 32-bit pointers, so we can not exceed 2Gb
     {$ELSE}
     if lPlankMB > 8000 then
        lPlankMB := 8000; //64-bit pointers, perhaps 8Gb is reasonable limit
     {$ENDIF}
     kPlankSz :=1024 {bytes/kb} * 1024 {bytes/mb} * lPlankMB;
     //kVers := GetKVers + ' CacheMB = '+inttostr(kPlankMB);
end;

procedure DefaultPrefs( var lNPMPrefs: TNPMPrefs);
begin
     lNPMPrefs.NULP := true;
     lNPMPrefs.ROI := true;
     lNPMPrefs.TFCE := 0;
     lNPMPrefs.ttest := true;
     lNPMPrefs.BMtest := true;
     lNPMPrefs.PlankMB := 512;
     lNPMPrefs.nPermute := 0;
     ComputePlankSize(lNPMPrefs.PlankMB);
end;

procedure ReadIniFile;
var
  lFilename: string;
  lThreads: integer;
  lIniFile: TIniFile;
begin
     DefaultPrefs(gNPMprefs);
     lFilename := IniName;
     if not FileexistsEx(lFilename) then
        exit;

     lIniFile := TIniFile.Create(lFilename);
     //ttestmenu.checked := IniBool(lIniFile,'computettest',true);
     //BMmenu.checked := IniBool(lIniFile,'computebm',false);
     gNPMprefs.ttest := IniBool(lIniFile,'computettest',gNPMprefs.ttest);
     gNPMprefs.BMtest := IniBool(lIniFile,'computebm',gNPMprefs.BMtest);
     gNPMPrefs.NULP := IniBool(lIniFile,'countlesionpatterns',gNPMPrefs.NULP);
     gNPMPrefs.ROI := IniBool(lIniFile,'ROI',gNPMPrefs.ROI);
     gNPMPrefs.TFCE := IniInt(lIniFile,'TFCE',gNPMPrefs.TFCE);
     gNPMPrefs.PlankMB := IniInt(lIniFile,'CacheMB',gNPMPrefs.PlankMB);
     ComputePlankSize(gNPMPrefs.PlankMB);
     gNPMPrefs.nPermute  := IniInt(lIniFile,'nPermute',gNPMPrefs.nPermute);
     //WritePermute(IniInt(lIniFile,'nPermute',0));
     lThreads := IniInt(lIniFile,'nThread', gnCPUThreads );
     if lThreads > gnCPUThreads then
        lThreads := gnCPUThreads;
     gnCPUThreads := lThreads;
     ComputePlankSize(gNPMPrefs.PlankMB);
  lIniFile.Free;
end; //ReadIniFile

procedure WriteIniFile;
var
  lIniName: string;
  lIniFile: TIniFile;
begin
     lIniName := IniName;
  if (DiskFreeEx(lIniName) < 1)  then
	exit;
  lIniFile := TIniFile.Create(lIniName);
  lIniFile.WriteString('BOOL', 'computettest',Bool2Char(gNPMprefs.ttest));
    lIniFile.WriteString('BOOL', 'computebm',Bool2Char(gNPMprefs.BMtest));
  lIniFile.WriteString('BOOL', 'countlesionpatterns',Bool2Char(gNPMPrefs.NULP));
  lIniFile.WriteString('BOOL', 'ROI',Bool2Char(gNPMPrefs.ROI));
  lIniFile.WriteString('INT', 'TFCE',inttostr(gNPMPrefs.TFCE));
  lIniFile.WriteString('INT', 'CacheMB',inttostr(gNPMPrefs.PlankMB));
  lIniFile.WriteString('INT', 'nPermute',inttostr(gNPMPrefs.nPermute));
  lIniFile.WriteString('INT', 'nThread',inttostr(gnCPUThreads));
  lIniFile.Free;
end;

procedure FreePermute (lnPermute: integer; var lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;var  lRanOrderp: pointer);
begin
     if (lnPermute < 2) then
        exit;
    Freemem(lRanOrderp);
    Freemem(lPermuteMaxT);
    Freemem(lPermuteMinT);
    Freemem(lPermuteMaxBM);
    Freemem(lPermuteMinBM);
end;

function ReportDescriptives (var RA: SingleP; n: integer): boolean;
var lMn,lSD,lSE,lSkew,lZSkew: double;
begin
     SuperDescriptive (RA, n, lMn,lSD,lSE,lSkew,lZSkew);
     NPMMsg('mean='+floattostr(lMn)+',StDev='+floattostr(lSD)+',StEr='+floattostr(lSE)+',Skew='+floattostr(lSkew)+',ZSkew='+floattostr(lZSkew));
end;



function ThreshMap(lThresh: single; lVolVox: integer;lOutImg: singleP): integer;
var
   lVox: integer;
begin
     result := 0;
     for lVox := 1 to lVolVox do
         if lOutImg^[lVox] >= lThresh then
            inc(result);

     for lVox := 1 to lVolVox do
         if lOutImg^[lVox] >= lThresh then
            lOutImg^[lVox] := 1
         else
             lOutImg^[lVox] := 0;
end;

procedure sort (lo, up: integer; var r:SingleP);
//62ms Shell Sort http://www.dcc.uchile.cl/~rbaeza/handbook/algs/4/414.sort.p.html
label     999;
var  d, i, j : integer;
          tempr : single;
begin
     d := up-lo+1;
     while d>1 do begin
          if d<5 then
             d := 1
          else
              d := trunc( 0.45454*d );
          //Do linear insertion sort in steps size d
          for i:=up-d downto lo do begin
               tempr := r^[i];
               j := i+d;
               while j <= up do
                    if tempr > r^[j] then begin
                         r^[j-d] := r^[j];
                         j := j+d
                         end
                    else goto 999;  //break
               999:
               r^[j-d] := tempr
          end //for
     end //while
end; //proc Sort

function IndexPct(lnPermute: integer; lPct: single; lTop: boolean): integer;
begin
    result := round(lnPermute * lPct);
    if lTop then
       result := (lnPermute - result)+1;
    if (result < 1)  then
       result := 1;
    if (result > lnPermute) then
       result := lnPermute;
end;

function ReportThresh (lLabel: string; lnPermute: integer; var lRankedData: singleP;lTop:boolean): double;
begin
     result := lRankedData^[IndexPct(lnPermute,0.050,lTop)];
     NPMmsg(lLabel+': permutationFWE '+
       //'0.500='+realtostr(lRankedData[IndexPct(lnPermute,0.500,lTop)],3)+
       ', 0.050='+realtostr({lRankedData^[IndexPct(lnPermute,0.050,lTop)]} result,8)+
       ', 0.025='+realtostr(lRankedData^[IndexPct(lnPermute,0.025,lTop)],8)+
       ', 0.01='+realtostr(lRankedData^[IndexPct(lnPermute,0.010,lTop)],8)+
       ' ');
end;

function reportPermute (lLabel:string; lnPermute: integer; var lPermuteMaxZ, lPermuteMinZ: singleP): double;
begin
     result := 0;
     if (lnPermute < 2) then
        exit;
    sort (1, lnPermute,lPermuteMaxZ);
    result := ReportThresh(lLabel+'+',lnPermute,lPermuteMaxZ,true);
    sort (1, lnPermute,lPermuteMinZ);
    ReportThresh(lLabel+'-',lnPermute,lPermuteMinZ,false);
    //for lPos := 1 to lnPermute do
    //    msg(inttostr(lPos)+', '+realtostr(lPermuteMinZ[lPos],4));

end;

function reportFDR (lLabel:string; lnVox, lnTests: integer; var lData: SingleP): double;
var
   lC,lN: integer;
   lPs: SingleP;
   lFDR05r, lFDR01r,lFDR05p, lFDR01p,lMin,lMax : double;
begin
    result := 10000;
    if (lnTests < 1) or (lnVox < 1) then
       exit;
    GetMem(lPs,lnTests*sizeof(single));
    for lC := 1 to lnTests do
        lPs^[lC] := 0;
    lN := 0;
    lMin := 0;
    lMax := 0;
    for lC := 1 to lnVox do begin
        if lData^[lC] <> 0 then begin
           inc(lN);
           if lData^[lC] > lMax then lMax := lData^[lC]
           else if lData^[lC] < lMin then lMin := lData^[lC];
           if lN <= lnTests then
              lPs^[lN] := pNormal(lData^[lC]);
        end;
    end;
    EstimateFDR2(lnTests, lPs, lFDR05p, lFDR01p,lFDR05r, lFDR01r);
    NPMmsg(lLabel+' Range '
       +realtostr(lMin,3)+
       '...'+realtostr(lMax,3));
    {Msg(lLabel+' Range '
       +realtostr(pNormalInv(lPs[lnTests]),3)+
       '...'+realtostr(pNormalInv(lPs[1]),3)+
       ' '); } //we could use this and save time computing lmin/lmax, but loss in precision
    NPMmsg(lLabel+' +FDR Z '+
       '0.050='+realtostr(pNormalInv(lFDR05p),8)+
       ', 0.01='+realtostr(pNormalInv(lFDR01p),8)+
       ' ');
    NPMmsg(lLabel+' -FDR Z '+
       '0.050='+realtostr(pNormalInv(1-lFDR05r),8)+
       ', 0.01='+realtostr(pNormalInv(1-lFDR01r),8)+
       ' ');
    result := pNormalInv(lFDR01p);
end;


function reportBonferroni(lLabel: string; lnTests: integer): double; //returns 5% Z score
begin
     if lnTests < 1 then exit;
     result := pNormalInv(0.05/lnTests);
     NPMmsg(inttostr(lnTests)+' test '+lLabel+' Bonferroni FWE Z '+
       '0.050='+realtostr(result,3)+
       ', 0.025='+realtostr(pNormalInv(0.025/lnTests),3)+
       ', 0.01='+realtostr(pNormalInv(0.01/lnTests),3));
end;


procedure NPMThreadDone;
begin
     Dec(gThreadsRunning);
end;


procedure InitRA (lnPermute: integer; var lRA: singleP);
var
   lInc: integer;
begin
     getmem(lRA,lnPermute* sizeof(single));
     for lInc := 1 to lnPermute do
        lRA^[lInc] := 0;
end;

procedure InitPermute (lnSubj, lnPermute: integer; var lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP; var  lRanOrderp: pointer; var lRanOrder: Doublep0);
begin
     if (lnPermute < 2) then
        exit;
     InitRA(lnPermute,lPermuteMaxT);
     InitRA(lnPermute,lPermuteMinT);
     InitRA(lnPermute,lPermuteMaxBM);
     InitRA(lnPermute,lPermuteMinBM);
     createArray64(lRanOrderp,lRanOrder,lnSubj);
end; //init permute

initialization
 {$IFNDEF GUI}
 NPMmemo:= TStringList.Create;
 {$ENDIF}

finalization
{$IFNDEF GUI}
 NPMmemo.Free;
{$ENDIF}
end.

