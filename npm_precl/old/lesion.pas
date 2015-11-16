unit lesion;
interface
{$H+}
uses
  define_types,SysUtils,
part,StatThds,statcr,StatThdsUtil,Brunner,DISTR,nifti_img, hdr,
   Messages,  Classes, Graphics, Controls, Forms, Dialogs,
StdCtrls,  ComCtrls,ExtCtrls,Menus, overlap,ReadInt,lesion_pattern,stats,LesionStatThds,nifti_hdr,

{$IFDEF FPC} LResources,gzio2,
{$ELSE} gziod,associate,{$ENDIF}   //must be in search path, e.g. C:\pas\mricron\npm\math
{$IFNDEF UNIX} Windows, {$ENDIF}
upower,firthThds,firth,IniFiles,cpucount,userdir,math,
regmult,utypes;


function LesionNPMAnalyze2 (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lnCrit,lRun,lnPermute: integer; var lSymptomRA: SingleP;var lFactname,lOutName: string; lttest,lBM: boolean): boolean;
function LesionNPMAnalyzeBinomial2 (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lnCrit,lnPermute: integer; var lSymptomRA: SingleP; var lFactname,lOutName: string): boolean;
var
   gNULP,gROI: boolean;
implementation

uses npmform;

{$DEFINE NOTmedianfx}
function LesionNPMAnalyze2 (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lnCrit,lRun,lnPermute: integer; var lSymptomRA: SingleP;var lFactname,lOutName: string; lttest,lBM: boolean): boolean;
label
	123,667;
var
	lOutNameMod: string;
	lPlankImg: byteP;
        lOutImgSum,lOutImgBM,lOutImgT,lOutImgAUC,
        lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;
        lPos,lPlank,lThread: integer;
	lVolVox,lMinMask,lMaxMask,lTotalMemory,lnPlanks,lVoxPerPlank,
        lThreadStart,lThreadEnd,lThreadInc,lnLesion,//,lnPermute,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lPosPct: int64;
	lT,lBMz,  lSum,lThresh,lThreshPermute,lThreshBonf,lThreshNULP :double;
	lObsp: pointer;
	lObs: Doublep0;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
        lPlankAllocated: boolean;
        //lttest,lBM: boolean;
        {$IFDEF medianfx}
        lmedianFX,lmeanFX,lsummean,lsummedian: double;
        lmediancount: integer;
        {$ENDIF}
begin
        //lttest:= ttestmenu.checked;
        //lBM := BMmenu.checked;
        lPlankAllocated := false;
        //lnPermute := MainForm.ReadPermute;
        MainForm.NPMmsg('Permutations = ' +IntToStr(lnPermute));
	MainForm.NPMmsg('Analysis began = ' +TimeToStr(Now));
	lTotalMemory := 0;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
	lMinMask := 1;
        lMaxMask := lVolVox;
	lVoxPerPlank :=  kPlankSz div lImages.Count div sizeof(byte) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
	MainForm.NPMmsg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	MainForm.NPMmsg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
        if (lnPlanks = 1) then
            getmem(lPlankImg,lTotalMemory) //assumes 1bpp
        else
	    getmem(lPlankImg,kPlankSz);
        lPlankAllocated := true;
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
        {$IFDEF medianfx}
        lsummean := 0;
        lsummedian:= 0;
        lmediancount := 0;
        {$ENDIF}
        for lPos := 1 to lImages.Count do
		if gScaleRA[lPos] = 0 then
			gScaleRA[lPos] := 1;
	createArray64(lObsp,lObs,lImages.Count);
        getmem(lOutImgSum,lVolVox* sizeof(single));
	getmem(lOutImgBM,lVolVox* sizeof(single));
	getmem(lOutImgT,lVolVox* sizeof(single));
	getmem(lOutImgAUC,lVolVox* sizeof(single));
        MainForm.InitPermute (lImages.Count, lnPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do begin
                lOutImgSum^[lPos] := 0;
		lOutImgBM^[lPos] := 0;
		lOutImgT^[lPos] := 0;
		lOutImgAUC^[lPos] := 0;
	end;
        //next create permuted BM bounds
        if lBM then begin
           MainForm.NPMmsg('Generating BM permutation thresholds');
           MainForm.Refresh;
           for lPos := 1 to lImages.Count do
               lObs^[lPos-1] := lSymptomRA^[lPos];
           genBMsim (lImages.Count, lObs);
        end;
         ClearThreadData(gnCPUThreads,lnPermute) ;
	for lPlank := 1 to lnPlanks do begin
		MainForm.NPMmsg('Computing plank = ' +Inttostr(lPlank));
        MainForm.Refresh;
        Application.processmessages;
		lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lMaxMask then begin
			lVoxPerPlank := lVoxPerPlank - (lEndVox-lMaxMask);
			lEndVox := lMaxMask;
		end;
		lPlankImgPos := 1;
		for lPos := 1 to lImages.Count do begin
			if not LoadImg8(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
				goto 667;
			lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
                //threading start
                lThreadStart := 1;
                lThreadInc := lVoxPerPlank  div gnCPUThreads;
                lThreadEnd := lThreadInc;
                Application.processmessages;
                for lThread := 1 to gnCPUThreads do begin
                    if lThread = gnCPUThreads then
                       lThreadEnd := lVoxPerPlank; //avoid integer rounding error
                    with TLesionContinuous.Create (MainForm.ProgressBar1,lttest,lBM,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,0,lPlankImg,lOutImgSum,lOutImgBM,lOutImgT,lOutImgAUC,lSymptomRA) do
                    //with TLesionContinuous.Create (MainForm.ProgressBar1,lttest,lBM,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lPlankImg,lOutImgSum,lOutImgBM,lOutImgT,lSymptomRA) do
                         {$IFDEF FPC} OnTerminate := @MainForm.ThreadDone; {$ELSE}OnTerminate := MainForm.ThreadDone;{$ENDIF}
                    inc(gThreadsRunning);
                    lThreadStart := lThreadEnd + 1;
                    lThreadEnd :=lThreadEnd + lThreadInc;
                end; //for each thread
                repeat
                      Application.processmessages;
                until gThreadsRunning = 0;
                Application.processmessages;
                //threading end
		lStartVox := lEndVox + 1;
	end;
        //freemem(lPlankImg);
        //lPlankAllocated := false;
        lThreshPermute := 0;
        lnVoxTested := SumThreadData(gnCPUThreads,lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM);
        //next report findings
        if lnVoxTested < 1 then begin
	   MainForm.NPMmsg('**Error: no voxels tested: no regions lesioned in at least '+inttostr(lnCrit)+' patients**');
           goto 123;
        end;
	MainForm.NPMmsg('Voxels tested = ' +Inttostr(lnVoxTested));
        {$IFDEF medianfx}
	MainForm.NPMmsg('Average MEAN effect size = ' +realtostr((lsummean/lmediancount),3));
        MainForm.NPMmsg('Average MEDIAN effect size = ' +realtostr((lsummedian/lmediancount),3));
        {$ENDIF}
        MainForm.NPMmsg('Only tested voxels with more than '+inttostr(lnCrit)+' lesions');
        //Next: save results from permutation thresholding....
        lThreshBonf := MainForm.reportBonferroni('Std',lnVoxTested);

        //next: save data
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save sum map
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Sum'+lFactName,'.hdr');
        if lRun < 1 then
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
        //save Area Under Curve
        lOutNameMod := ChangeFilePostfixExt(lOutName,'rocAUC'+lFactName,'.hdr');
        if lRun < 1 then
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgAUC,1);
//create new header - subsequent images will use Z-scores
        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
        if (lRun < 1) and (Sum2PowerCont(lOutImgSum,lVolVox,lImages.Count)) then begin
           lOutNameMod := ChangeFilePostfixExt(lOutName,'Power'+lFactName,'.hdr');
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
        end;
        if lRun > 0 then //terrible place to do this - RAM problems, but need value to threshold maps
           lThreshNULP := MainForm.reportBonferroni('Unique overlap',CountOverlap2 (lImages, lnCrit,lnVoxTested,lPlankImg));

        //MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
if lttest then begin //save Ttest
        //next: convert t-scores to z scores
        for lPos := 1 to lVolVox do
            lOutImgT^[lPos] := TtoZ (lOutImgT^[lPos],lImages.Count-2);
        for lPos := 1 to lnPermute do begin
            lPermuteMaxT^[lPos] := TtoZ (lPermuteMaxT^[lPos],lImages.Count-2);
            lPermuteMinT^[lPos] := TtoZ (lPermuteMinT^[lPos],lImages.Count-2);
        end;
        lThresh := MainForm.reportFDR ('ttest', lVolVox, lnVoxTested, lOutImgT);
        lThreshPermute := MainForm.reportPermute('ttest',lnPermute,lPermuteMaxT, lPermuteMinT);
	lOutNameMod := ChangeFilePostfixExt(lOutName,'ttest'+lFactName,'.hdr');
        if lRun > 0 then
           MainForm.NPMmsgAppend('threshtt,'+inttostr(lRun)+','+inttostr(MainForm.ThreshMap(lThreshNULP,lVolVox,lOutImgT))+','+realtostr(lThreshNULP,3)+','+realtostr(lThreshPermute,3)+','+realtostr(lThreshBonf,3));
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgT,1);

end;
if lBM then begin //save Brunner Munzel
        lThresh :=  MainForm.reportFDR ('BM', lVolVox, lnVoxTested, lOutImgBM);
        lThreshPermute := MainForm.reportPermute('BM',lnPermute,lPermuteMaxBM, lPermuteMinBM);
        lOutNameMod := ChangeFilePostfixExt(lOutName,'BM'+lFactName,'.hdr');
        if lRun > 0 then
           MainForm.NPMmsgAppend('threshbm,'+inttostr(lRun)+','+inttostr(MainForm.ThreshMap(lThreshNULP,lVolVox,lOutImgBM))+','+realtostr(lThreshNULP,3)+','+realtostr(lThreshPermute,3)+','+realtostr(lThreshBonf,3));
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgBM,1);
end;
//next: free dynamic memory
123:
        MainForm.FreePermute (lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,  lRanOrderp);
	freemem(lOutImgT);
        freemem(lOutImgAUC);
	freemem(lOutImgBM);
        freemem(lOutImgSum);
	freemem(lObsp);
        if lPlankAllocated then
	   freemem(lPlankImg);
        //Next: NULPS - do this after closing all memory - this is a memory hog
        if gNULP then
           lThreshNULP := MainForm.reportBonferroni('Unique overlap',CountOverlap (lImages, lnCrit,lnVoxTested));
	MainForm.NPMmsg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes'+lFactName,'.txt');
        MainForm.MsgSave(lOutNameMod);
        MainForm.ProgressBar1.Position := 0;
        //if lRun > 0 then
        //   AX(freeram,freeram,freeram,freeram,freeram,freeram);
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lTotalMemory > 1 then freemem(lPlankImg);
	MainForm.NPMmsg('Unable to complete analysis.');
        MainForm.ProgressBar1.Position := 0;
end; //LesionNPMAnalyze

function LesionNPMAnalyzeBinomial2 (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lnCrit,lnPermute: integer; var lSymptomRA: SingleP; var lFactname,lOutName: string): boolean;
label
     123,667;
var
   lVal: single;
	lOutNameMod: string;
	lPlankImg: byteP;
        lOutImgSum,lOutImgL,lOutImgAUC,lDummyImg,
        lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;
        lPos,lPlank,lThread,lnDeficit: integer;
        lTotalMemory,lVolVox,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
        lThreadStart,lThreadInc,lThreadEnd, lnLesion,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lPosPct: int64;
	lT,  lSum: double;
	lObsp: pointer;
	lObs: Doublep0;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
begin
        MainForm.NPMmsg('Permutations = ' +IntToStr(lnPermute));
	//lOutName := lMaskHdr.ImgFileName;
	//if not SaveHdrName ('Statistical Map', lOutName) then exit;
	MainForm.NPMmsg('Analysis began = ' +TimeToStr(Now));
	lTotalMemory := 0;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
	lMinMask := 1;
        lMaxMask := lVolVox;
	lVoxPerPlank :=  kPlankSz div lImages.Count div sizeof(byte) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
	MainForm.NPMmsg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	MainForm.NPMmsg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
    if (lnPlanks = 1) then
            getmem(lPlankImg,lTotalMemory) //assumes 1bp
    else
	    getmem(lPlankImg,kPlankSz);
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
        for lPos := 1 to lImages.Count do
		if gScaleRA[lPos] = 0 then
			gScaleRA[lPos] := 1;
	createArray64(lObsp,lObs,lImages.Count);
        getmem(lOutImgSum,lVolVox* sizeof(single));
	getmem(lOutImgL,lVolVox* sizeof(single));
	getmem(lOutImgAUC,lVolVox* sizeof(single));

        MainForm.InitPermute (lImages.Count, lnPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do begin
                lOutImgSum^[lPos] := 0;
		lOutImgL^[lPos] := 0;
		lOutImgAUC^[lPos] := 0;
	end;
        ClearThreadDataPvals(gnCPUThreads,lnPermute) ;
	for lPlank := 1 to lnPlanks do begin
            MainForm.ProgressBar1.Position := 1;
		MainForm.NPMmsg('Computing plank = ' +Inttostr(lPlank));
                MainForm.Refresh;
                Application.processmessages;
		lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lMaxMask then begin
			lVoxPerPlank := lVoxPerPlank - (lEndVox-lMaxMask);
			lEndVox := lMaxMask;
		end;
		lPlankImgPos := 1;
		for lPos := 1 to lImages.Count do begin
			if not LoadImg8(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
				goto 667;
			lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
                  //threading start
                lThreadStart := 1;
                lThreadInc := lVoxPerPlank  div gnCPUThreads;
                lThreadEnd := lThreadInc;
                Application.processmessages;
                for lThread := 1 to gnCPUThreads do begin
                    if lThread = gnCPUThreads then
                       lThreadEnd := lVoxPerPlank; //avoid integer rounding error
                    //with TLesionBinomial.Create (ProgressBar1,false,true,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,666, lDummyImg,lPlankImg,lOutImgSum,lOutImgL,lDummyImg,lSymptomRA) do
                    with TLesionBinom.Create (MainForm.ProgressBar1,false,true,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,0,lPlankImg,lOutImgSum,lOutImgL,lDummyImg,lOutImgAUC,lSymptomRA) do
                         {$IFDEF FPC} OnTerminate := @MainForm.ThreadDone; {$ELSE}OnTerminate := MainForm.ThreadDone;{$ENDIF}
                    inc(gThreadsRunning);
                    MainForm.NPMmsg('Thread ' +Inttostr(gThreadsRunning)+' = '+inttostr(lThreadStart)+'..'+inttostr(lThreadEnd));
                    lThreadStart := lThreadEnd + 1;
                    lThreadEnd :=lThreadEnd + lThreadInc;
                end; //for each thread
                repeat
                      Application.processmessages;
                until gThreadsRunning = 0;
                Application.processmessages;
                //threading end
		lStartVox := lEndVox + 1;
	end;
        lnVoxTested := SumThreadData(gnCPUThreads,lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM);
        for lPos := 1 to lnPermute do begin
            if (lPermuteMinT^[lPos] > 1.1) or (lPermuteMinT^[lPos] < -1.1) then
               lPermuteMinT^[lPos] := 0.5;
            if (lPermuteMaxT^[lPos] > 1.1) or (lPermuteMaxT^[lPos] < -1.1) then
               lPermuteMaxT^[lPos] := 0.5;
            lVal := lPermuteMaxT^[lPos];
            lPermuteMaxT^[lPos] := lPermuteMinT^[lPos];
            lPermuteMinT^[lPos] := lVal;
            if lPermuteMaxT^[lPos] < 0 then
			lPermuteMaxT^[lPos] := -pNormalInv(abs(lPermuteMaxT^[lPos]))
            else
			lPermuteMaxT^[lPos] := pNormalInv(lPermuteMaxT^[lPos]);
            if lPermuteMinT^[lPos] < 0 then
			lPermuteMinT^[lPos] := -pNormalInv(abs(lPermuteMinT^[lPos]))
            else
			lPermuteMinT^[lPos] := pNormalInv(lPermuteMinT^[lPos]);
        end;



        if lnVoxTested < 1 then begin
	   MainForm.NPMmsg('**Error: no voxels tested: no regions lesioned in at least '+inttostr(lnCrit)+' patients**');
           goto 123;
        end;
        //next report findings
	MainForm.NPMmsg('Voxels tested = ' +Inttostr(lnVoxTested));
        MainForm.NPMmsg('Only tested voxels with more than '+inttostr(lnCrit)+' lesions');
        //Next: save results from permutation thresholding....
        MainForm.reportBonferroni('Std',lnVoxTested);

        //next: save data
//savedata
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save sum map
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Sum'+lFactName,'.hdr');
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
//save Area Under Curve
           lOutNameMod := ChangeFilePostfixExt(lOutName,'rocAUC'+lFactName,'.hdr');
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgAUC,1);

//future images will store Z-scores...
        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
//save power map
        lnDeficit := 0;
        for lPos := 1 to lImages.Count do
            if lSymptomRA^[lPos] = 0 then
               inc(lnDeficit);
        if Sum2PowerBinom(lOutImgSum,lVolVox,lImages.Count,lnDeficit) then begin
           lOutNameMod := ChangeFilePostfixExt(lOutName,'Power'+lFactName,'.hdr');
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
        end;
        //save Liebermeister

        lOutNameMod := ChangeFilePostfixExt(lOutName,'L'+lFactName,'.hdr');
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgL,1);
        //save end
       MainForm.reportFDR ('L', lVolVox, lnVoxTested, lOutImgL);
       MainForm.reportPermute('L',lnPermute,lPermuteMaxT, lPermuteMinT);

123:
//next: free dynamic memory
        MainForm.FreePermute (lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,  lRanOrderp);
	freemem(lOutImgL);
	freemem(lOutImgAUC);
        freemem(lOutImgSum);
	freemem(lObsp);
	freemem(lPlankImg);
        //Next: NULPS  - do this at the end, it is a memory hog!
        if gNULP then
           MainForm.reportBonferroni('Unique overlap',CountOverlap (lImages, lnCrit,lnVoxTested));

	MainForm.NPMmsg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes'+lFactName,'.txt');
        MainForm.MsgSave(lOutNameMod);

        MainForm.ProgressBar1.Position := 0;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lTotalMemory > 1 then freemem(lPlankImg);
	MainForm.NPMmsg('Unable to complete analysis.');
        MainForm.ProgressBar1.Position := 0;
end;





end.
