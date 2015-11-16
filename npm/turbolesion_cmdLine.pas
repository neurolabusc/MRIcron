unit turbolesion;
interface
{$H+}
{$Include ..\common\isgui.inc}
uses
  //  Messages,  Graphics, Controls, Forms, Dialogs,StdCtrls,  ComCtrls,ExtCtrls,Menus,
  {$IFDEF GUI} ComCtrls,ReadInt,Forms, {$ENDIF}
  Classes,dialogsx, define_types,SysUtils,unpm,
part,StatThds,statcr,StatThdsUtil,Brunner,DISTR,nifti_img, hdr,

overlap,lesion_pattern,stats,LesionStatThds,nifti_hdr,

{$IFDEF FPC} {$IFDEF GUI} LResources,{$ENDIF}  gzio2,
{$ELSE} gziod,associate,{$ENDIF}   //must be in search path, e.g. C:\pas\mricron\npm\math
{$IFNDEF UNIX} Windows, {$ENDIF}
upower,firthThds,firth,IniFiles,cpucount,userdir,math,
regmult,utypes;
Type
  TLDMPrefs = record
         NULP,BMtest,Ttest,Ltest: boolean;
         CritPct,nCrit,nPermute,Run: integer;
         ValFilename, OutName, ExplicitMaskName: string;
  end;
function TurboLDM (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; var lPrefs: TLDMPrefs ; var lSymptomRA: SingleP;var lFactname,lOutName: string): boolean;



implementation
{$IFDEF GUI}
        uses npmform;
{$ELSE}
      // uses npmcl;
{$ENDIF}

(*procedure Debog (var lSumImg: Smallintp; lVox: integer);
var
   lInName : string;
   lFData: file;
begin
         lInName := 'c:\16.img';
	 assignfile(lFdata,lInName);
	 filemode := 2;
	 Rewrite(lFdata,lVox*sizeof(smallint));
	 BlockWrite(lFdata,lSumImg^, 1  {, NumWritten});
	 closefile(lFdata);
end;*)

function MakeSum (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; var lSumImg: Smallintp): boolean;
//if successful, you MUST freemem(lSumImg)...
label
	667;
var
	lVolVox,lVox,lImg,lPosPct: integer;
        lVolImg: byteP;

begin
        result := false;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then exit;
        getmem(lVolImg,lVolVox* sizeof(byte));
        getmem(lSumImg,lVolVox* sizeof(smallint));
        for lVox := 1 to lVolVox do //June 2009 init array
               lSumImg^[lVox] := 0;
(*        for lVox := 1 to lVolVox do
            if lVolImg^[lVox] <> 0 then
               lSumImg^[lVox] := lSumImg^[lVox]+1;*)
        for lImg := 1 to lImages.Count do begin
                lPosPct := round(100*(lImg / lImages.Count));
                NPMProgressBar(lPosPct);
                if not LoadImg8(lImages[lImg-1], lVolImg, 1, lVolVox,round(gOffsetRA[lImg]),1,gDataTypeRA[lImg],lVolVox) then
                   goto 667;
                for lVox := 1 to lVolVox do
                    if lVolImg^[lVox] <> 0 then
                       lSumImg^[lVox] := lSumImg^[lVox]+1;
	end;//for each image
	NPMmsg('Sum image finished = ' +TimeToStr(Now));
        NPMProgressBar( 0);
        //Debog(lSumImg, lVolVox);
        freemem(lVolImg);
        result := true;
	exit;
667: //you only get here if you aborted ... free memory and report error
           freemem(lVolImg);
           freemem(lSumImg);
	NPMMsg('Unable to complete analysis.');
        NPMProgressBar( 0 );
end;


function ThreshSumImg (var lSumImg: Smallintp; lVolVox,lThresh: integer): integer;
//sets all voxels with values < lThresh to zero, returns number of voxels to survive threshold.
var
   lPos: integer;
begin
     result := 0;
     if lVolVox < 1 then
        exit;
     for lPos := 1 to lVolVox do
         if lSumImg^[lPos] < lThresh then
            lSumImg^[lPos] := 0
         else
             inc(result);
end;

function ExplicitMaskSumImg (lMaskName: string; var lSumImg: Smallintp; lVolVox: integer): integer;
//Any voxels in MaskImg that are 0 are zeroed in the SumImg
var
   lOK: boolean;
   lPos: integer;
   lMaskHdr: TMRIcroHdr;
   lMaskData: bytep;
label
  666;
begin
     result := 0;
     if (lVolVox < 1) or (not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr)) then begin
	      NPMmsg('Error: unable to load explicit mask named '+lMaskName);
	      exit;
     end;
     if lVolVox <> (lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3]) then begin
	      NPMmsg('Error: data and explicit mask have different sizes '+lMaskName);
	      exit;
     end;

     getmem(lMaskData,lVolVox* sizeof(byte));
     lOK := LoadImg8(lMaskName, lMaskData, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.DataType,lVolVox);
     if not lOK then goto 666;
     if lVolVox < 1 then
        exit;
     for lPos := 1 to lVolVox do
         if lMaskData^[lPos] < 1 then
            lSumImg^[lPos] := 0
         else
             inc(result);

     666:
     freemem(lMaskData);
end;

function LoadImg8Masked(lInName: string; lImgData: bytep; lMaskData: SmallIntP; lStartMaskPos, lEndMaskPos,linvox_offset,lRApos,lDataType,lVolVox: integer): boolean;
label
     111;
var
   lFullImgData: bytep;
   lMaskPos,lPos: integer;
begin
     result := false;
     if (lVolVox < 1) or (lEndMaskPos < lStartMaskPos) then
        exit;
     getmem(lFullImgData,lVolVox* sizeof(byte));
     result := LoadImg8(lInName, lFullImgData, 1, lVolVox,linvox_offset,1,lDataType,lVolVox);
     if result then begin
        lMaskPos := 0;
        for lPos := 1 to lVolVox do begin
            if lMaskData^[lPos] <> 0 then begin
                inc(lMaskPos);
                if (lMaskPos >=lStartMaskPos) then
                   lImgData^[lRApos+lMaskPos-1] := lFullImgData^[lPos];
                if lMaskPos = lEndMaskPos then goto 111;

            end;//voxel in mask
        end; //for each voxel in image

     end;//if LoadImg8 success
111:
     freemem(lFullImgData);
end;

function reformat(var lStatImg: singlep; lMaskImg: smallintp; lVolVox: integer): boolean;
var
   lPos,lStatPos,lMaskItems: integer;
begin
     result := false;
     if lVolVox < 1 then
        exit;
     lMaskItems := 0;
     for lPos := 1 to lVolVox do
         if lMaskImg^[lPos] <> 0 then
            inc(lMaskItems);
     result := true;
     if (lMaskItems < 1) or (lMaskItems >= lVolVox) then
        exit;//no need to reformat
     //note that we do this in descending order, so we do not overwrite...
     lStatPos := lMaskItems;
     for lPos := lVolVox downto 1 do
         if lMaskImg^[lPos] <> 0 then begin
            lStatImg^[lPos] := lStatImg^[lStatPos];
            dec(lStatPos);
         end else
             lStatImg^[lPos] := 0;
end;//reformat


function NULPcount (lPlankImg: bytep; lVoxPerPlank,lImagesCount: integer; var lUniqueOrders: integer; var lOverlapRA: Overlapp): boolean;
procedure CheckOrder(var lObservedOrder: TLesionPattern);
var
   lInc: integer;
begin
     if lUniqueOrders > 0 then begin //see if this is unique
          for lInc := 1 to lUniqueOrders do
              if SameOrder(lObservedOrder,lOverlapRA^[lInc],lImagesCount) then
                 exit; //not unique
     end; //UniqueOrders > 0
     //if we have not exited yet, we have found a new ordering!
     lUniqueOrders := lUniqueOrders + 1;
     lOverlapRA^[lUniqueOrders] := lObservedOrder;
end;

var
   lVox,lPlankImgPos,lPos: integer;
   lOrder,lPrevOrder: TLesionPattern;
begin
     result := false;
     lPrevOrder := EmptyOrder;//impossible: forces first voxel of each order to be checked
     for lVox := 1 to lVoxPerPlank do begin
         (*if (lVox mod lVoxPerPlankDiv10) = 0 then begin
                       MainForm.ProgressBar1.Position := (lVox div lVoxPerPlankDiv10)*10;
                       MainForm.Refresh;
                       Application.processmessages;
                    end;*)
         lOrder := EmptyOrder;
         lPlankImgPos := 0;
         //lnDeficits := 0;
         for lPos := 1 to lImagesCount do begin
             if (lPlankImg^[lPlankImgPos + lVox] > 0) then begin
                //inc(lnDeficits);
                SetBit(lPos,lOrder);
             end;
             lPlankImgPos := lPlankImgPos + lVoxPerPlank;
         end;
         //if  (lnDeficits >= lminDeficits) then begin //this is different from the last voxel: perhaps this is a new ordering
             if (not SameOrder(lOrder,lPrevOrder,lImagesCount)) then
                CheckOrder(lOrder);
             //inc(lnVoxels);
         //end;//nDeficies
         lPrevOrder := lOrder;
     end;//for lVox
     result := true;
end;

procedure PtoZpermute (lnPermute: integer; lPermuteMaxT, lPermuteMinT: singlep);
var
   lPos: integer;
   lVal : single;
begin
     if lPos < 1 then exit;
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
end;


function TurboLDM (var lImages: TStrings; var lMaskHdr: TMRIcroHdr;var lPrefs: TLDMPrefs ; var lSymptomRA: SingleP;var lFactname,lOutName: string): boolean;
label
	123,667;
var
	lOutNameMod: string;
	lStatHdr: TNIfTIhdr;
        lThreshFDR,lThreshPermute,lThreshBonf,lThreshNULP :double;
	lObsp: pointer;
	lObs: Doublep0;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
        lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,lOutImgSum,lOutImgBM,lOutImgT,lOutImgAUC: singlep;
        lSumImg: Smallintp;
        lPlankImg: byteP;
        lVoxPerPlank,lnPlanks,lTotalMemory,lnVoxTested,lVolVox: int64;
        lUniqueOrders,lThread,lThreadStart,lThreadInc,lThreadEnd,
        lPos2,lPosPct,lPos,lPlankImgPos,lPlank,lStartVox,lEndVox: integer;
        lOverlapRA: Overlapp;

     {$IFNDEF FPC} lStartTime :DWord;{$ENDIF}
begin
     {$IFNDEF FPC} lStartTime := GetTickCount;{$ENDIF}
     result := false;
     lSumImg := nil;
     lPlankImg := nil;
     lOutImgSum := nil;
     lOutImgBM := nil;
     lOutImgT := nil;
     lOutImgAUC := nil;
     lOverlapRA := nil;
     lUniqueOrders := 0;
     if lPrefs.Ltest then begin
        lPrefs.Ttest := false;
        lPrefs.BMtest := false;
     end else if (not lPrefs.Ttest) and (not lPrefs.BMtest) then begin//not binomial
        NPMmsg('Error no tests specified');
        exit;
     end;
        NPMmsg('Permutations = ' +IntToStr(lPrefs.nPermute));
	NPMmsg('Analysis began = ' +TimeToStr(Now));
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
  if not MakeSum( lImages, lMaskHdr, lSumImg) then goto 667;
  lnVoxTested := ThreshSumImg(lSumImg,lVolVox,lPrefs.nCrit);
	NPMmsg('Voxels damaged in at least '+inttostr(lPrefs.nCrit)+' individuals = ' +Floattostr(lnVoxTested));
  if lnVoxTested < 1 then begin
	   NPMmsg('Error: no voxels damaged in at least '+inttostr(lPrefs.nCrit)+' individuals.');
     goto 667;
  end;
  if (lPrefs.ExplicitMaskName <> '') then begin
    lnVoxTested := ExplicitMaskSumImg (lPrefs.ExplicitMaskName, lSumImg, lVolVox);
	  NPMmsg('Voxels also non-zero in mask '+lPrefs.ExplicitMaskName+' = ' +Floattostr(lnVoxTested));
    if lnVoxTested < 1 then begin
	    NPMmsg('Error: no remaing voxels also non-zero in mask '+lPrefs.ExplicitMaskName);
      goto 667;
    end;
  end;

        //compute planks and acquire memory
	lTotalMemory := lnVoxTested * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/kPlankSz ) + 1;
	NPMmsg('Memory planks = ' +Floattostr(lTotalMemory/kPlankSz));
        if (lnPlanks = 1) then begin
            lVoxPerPlank := lnVoxTested; //we can do this in a single pass
            getmem(lPlankImg,lTotalMemory)
        end else begin
	    getmem(lPlankImg,kPlankSz);
            lVoxPerPlank :=  kPlankSz div lImages.Count;
        end;
        //spatial maps for results
        getmem(lOutImgSum,lVolVox*sizeof(single));
        getmem(lOutImgBM,lVolVox*sizeof(single));
        getmem(lOutImgT,lVolVox*sizeof(single));
        getmem(lOutImgAUC,lVolVox*sizeof(single));
        //initialize memory
        InitPermute (lImages.Count, lPrefs.nPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do begin
                lOutImgSum^[lPos] := 0;
		lOutImgBM^[lPos] := 0;
		lOutImgT^[lPos] := 0;
		lOutImgAUC^[lPos] := 0;
	end;
        //next create permuted BM bounds
        if lPrefs.BMtest then begin
           NPMmsg('Generating BM permutation thresholds');
           createArray64(lObsp,lObs,lImages.Count);
           for lPos := 1 to lImages.Count do
               lObs^[lPos-1] := lSymptomRA^[lPos];
           genBMsim (lImages.Count, lObs);
           freemem(lObsp);
        end;
        if lPrefs.NULP then
           getmem(lOverlapRA,lnVoxTested* sizeof(TLesionPattern));
        if lPrefs.Ltest then
           ClearThreadDataPvals(gnCPUThreads,lPrefs.nPermute)
        else
            ClearThreadData(gnCPUThreads,lPrefs.nPermute) ;
        //load and process data
	lStartVox := 1;
	lEndVox := 0;
	for lPlank := 1 to lnPlanks do begin
		NPMmsg('Computing plank = ' +Inttostr(lPlank)+' of '+inttostr(lnPlanks));
		lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lnVoxTested then begin
			lVoxPerPlank := lnVoxTested-lStartVox+1{lVoxPerPlank - (lEndVox-lVolVox)};
			lEndVox := lnVoxTested;
		end;
		lPlankImgPos := 1;
		for lPos := 1 to lImages.Count do begin
			if not LoadImg8Masked(lImages[lPos-1], lPlankImg,lSumImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
				goto 667;
			lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
                lThreadStart := 1;
                lThreadInc := lVoxPerPlank  div gnCPUThreads;
                lThreadEnd := lThreadInc;
                  NPMmsg('starting threads '+inttostr(gnCPUThreads));
                {$IFDEF aaaaa}
                for lThread := 1 to gnCPUThreads do begin
                    if lThread = gnCPUThreads then
                       lThreadEnd := lVoxPerPlank; //avoid integer rounding error
                    ThreadArray[lThread]:= TLesionContinuous.Create (MainForm.ProgressBar1,lPrefs.ttest,lPrefs.BMtest,lPrefs.nCrit, lPrefs.nPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,0,lPlankImg,lOutImgSum,lOutImgBM,lOutImgT,lOutImgAUC,lSymptomRA) ;
                    inc(gThreadsRunning);

                    lThreadStart := lThreadEnd + 1;
                    lThreadEnd :=lThreadEnd + lThreadInc;
                 end;

                  NPMmsg('started threads '+inttostr(gnCPUThreads));
                  for lThread := 1 to gnCPUThreads do if not ThreadArray[lThread].Terminated then Sleep(100);
                  NPMmsg('done threads '+inttostr(gnCPUThreads));
                {$ELSE}
                  for lThread := 1 to gnCPUThreads do begin
                    if lThread = gnCPUThreads then
                       lThreadEnd := lVoxPerPlank; //avoid integer rounding error
                    if lPrefs.Ltest then begin
                       with TLesionBinom.Create (MainForm.ProgressBar1,false,true,lPrefs.nCrit, lPrefs.nPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,0,lPlankImg,lOutImgSum,lOutImgBM,lOutIMgT{not used},lOutImgAUC,lSymptomRA) do
                        {$IFDEF GUI}
                                {$IFDEF FPC} OnTerminate := @MainForm.ThreadDone; {$ELSE}OnTerminate := MainForm.ThreadDone;{$ENDIF}
                        {$ELSE}
                          NPMmsg(inttostr(gThreadsRunning));
                        //OnTerminate := @NPMThreadDone;
                        {$ENDIF}
                    end else begin
                        with TLesionContinuous.Create (MainForm.ProgressBar1,lPrefs.ttest,lPrefs.BMtest,lPrefs.nCrit, lPrefs.nPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,0,lPlankImg,lOutImgSum,lOutImgBM,lOutImgT,lOutImgAUC,lSymptomRA) do
                        //with TLesionContinuous.Create (MainForm.ProgressBar1,lttest,lBM,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lPlankImg,lOutImgSum,lOutImgBM,lOutImgT,lSymptomRA) do
                        {$IFDEF GUI}
                         {$IFDEF FPC} OnTerminate := @MainForm.ThreadDone; {$ELSE}OnTerminate := MainForm.ThreadDone;{$ENDIF}

                         {$ELSE}

                         OnTerminate := @Application.ThreadDone;
                         NPMmsg('starting '+inttostr(gThreadsRunning));
                        {$ENDIF}
                    end;
                    inc(gThreadsRunning);
                    lThreadStart := lThreadEnd + 1;
                    lThreadEnd :=lThreadEnd + lThreadInc;
                end; //for each thread
                repeat
                  Sleep(100);
                  refresher;
                until gThreadsRunning = 0;
                refresher;
                 {$ENDIF}
                //end of threading

                if lPrefs.NULP then
                   NULPcount (lPlankImg, lVoxPerPlank,lImages.Count, lUniqueOrders, lOverlapRA);

		lStartVox := lEndVox + 1;
	end;
        //calculate max per thread
        SumThreadData(gnCPUThreads,lPrefs.nPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM);

        //data in maps is stored in voxels 1..lnVoxTested - put in spatial order
        reformat(lOutImgSum,lSumImg,lVolVox);
        reformat(lOutImgBM,lSumImg,lVolVox);
        reformat(lOutImgT,lSumImg,lVolVox);
        reformat(lOutImgAUC,lSumImg,lVolVox);
        lThreshBonf := reportBonferroni('Std',lnVoxTested);
        if lPrefs.NULP then
           lThreshBonf := reportBonferroni('Number of Unique Lesion Patterns',lUniqueOrders);


        //next: save data
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
        //save sum map
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Sum'+lFactName,'.hdr');
        if lPrefs.Run < 1 then
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
        //save Area Under Curve
        lOutNameMod := ChangeFilePostfixExt(lOutName,'rocAUC'+lFactName,'.hdr');
        if lPrefs.Run < 1 then
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgAUC,1);
        //create new header - subsequent images will use Z-scores
        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
        if (lPrefs.Run < 1) and (Sum2Power(lOutImgSum,lVolVox,lImages.Count,lPrefs.nCrit, lPrefs.LTest)) then begin
           lOutNameMod := ChangeFilePostfixExt(lOutName,'Power'+lFactName,'.hdr');
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
        end;
        //if lPrefs.Run > 0 then //terrible place to do this - RAM problems, but need value to threshold maps
        //   lThreshNULP := MainForm.reportBonferroni('Unique overlap',CountOverlap2 (lImages, lPrefs.nCrit,lnVoxTested,lPlankImg));
        if lPrefs.ttest then begin //save Ttest
           //next: convert t-scores to z scores
           for lPos := 1 to lVolVox do
               lOutImgT^[lPos] := TtoZ (lOutImgT^[lPos],lImages.Count-2);
           for lPos := 1 to lPrefs.nPermute do begin
               lPermuteMaxT^[lPos] := TtoZ (lPermuteMaxT^[lPos],lImages.Count-2);
               lPermuteMinT^[lPos] := TtoZ (lPermuteMinT^[lPos],lImages.Count-2);
           end;
           lThreshFDR := reportFDR ('ttest', lVolVox, lnVoxTested, lOutImgT);
           lThreshPermute := reportPermute('ttest',lPrefs.nPermute,lPermuteMaxT, lPermuteMinT);
	   lOutNameMod := ChangeFilePostfixExt(lOutName,'ttest'+lFactName,'.hdr');
           {$IFNDEF FPC}
           if lPrefs.Run > 0 then begin
              MainForm.NPMmsgAppend('threshtt,'+inttostr(lPrefs.Run)+','+inttostr(MainForm.ThreshMap(lThreshBonf,lVolVox,lOutImgT))+','+realtostr(lThreshNULP,3)+','+realtostr(lThreshPermute,3)+','+realtostr(lThreshBonf,3)+','+inttostr(round((GetTickCount - lStartTime)/1000)));
           end;
           {$ENDIF}
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgT,1);
        end;
        if lPrefs.LTest then begin
           PtoZpermute (lPrefs.nPermute, lPermuteMaxT, lPermuteMinT);
           lOutNameMod := ChangeFilePostfixExt(lOutName,'L'+lFactName,'.hdr');
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgBM,1);
           reportFDR ('L', lVolVox, lnVoxTested, lOutImgBM);
           reportPermute('L',lPrefs.nPermute,lPermuteMaxT, lPermuteMinT);
        end;//Liebermeister
        if lPrefs.BMtest then begin //save Brunner Munzel
           lThreshFDR :=  reportFDR ('BM', lVolVox, lnVoxTested, lOutImgBM);
           lThreshPermute := reportPermute('BM',lPrefs.nPermute,lPermuteMaxBM, lPermuteMinBM);
           lOutNameMod := ChangeFilePostfixExt(lOutName,'BM'+lFactName,'.hdr');
           if lPrefs.Run > 0 then
              NPMmsg('threshbm,'+inttostr(lPrefs.Run)+','+inttostr(ThreshMap(lThreshBonf,lVolVox,lOutImgBM))+','+realtostr(lThreshNULP,3)+','+realtostr(lThreshPermute,3)+','+realtostr(lThreshBonf,3));
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgBM,1);
        end;
	NPMmsg('Analysis finished = ' +TimeToStr(Now));
     {$IFNDEF FPC} MainForm.NPMmsg('Processing Time = ' +inttostr(round((GetTickCount - lStartTime)/1000)));{$ENDIF}

        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes'+lFactName,'.txt');
        NPMMsgSave(lOutNameMod);
        //all done
        result := true;//all done without aborting
667: // free memory and report error
        if lPlankImg <> nil then freemem(lPlankImg);
        if lSumImg <> nil then freemem(lSumImg);
        if lOutImgSum <> nil then freemem(lOutImgSum);
        if lOutImgBM <> nil then freemem(lOutImgBM);
        if lOutImgT <> nil then freemem(lOutImgT);
        if lOutImgAUC <> nil then freemem(lOutImgAUC);
        if lOverlapRA <> nil then freemem(lOverlapRA);

        if not result then
	   NPMmsg('Unable to complete analysis.');
       NPMProgressBar( 0);
end; //TurboLDM

end.