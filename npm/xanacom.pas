unit anacom;
interface
{$H+}
uses
  define_types,SysUtils,part,StatThds,statcr,StatThdsUtil,Brunner,
  DISTR,nifti_img, hdr,filename,Messages,  Classes, Graphics,
  Controls, Forms, Dialogs,StdCtrls,ComCtrls,ExtCtrls,Menus, overlap,
  ReadInt,lesion_pattern,stats,LesionStatThds,nifti_hdr,
  upower,firthThds,firth,IniFiles,cpucount,userdir,math,
  {$IFDEF FPC} LResources,gzio2,
  {$ELSE} gziod,associate,{$ENDIF}   //must be in search path, e.g. C:\pas\mricron\npm\math
  {$IFNDEF UNIX} Windows, {$ENDIF}
  regmult,utypes;

  function AnacomLesionNPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lnCrit,lRun,lnControl: integer; var lSymptomRA,lControlSymptomRA: SingleP;var lFactname,lOutName: string; lttestIn,lBMIn: boolean): boolean;
  procedure DoAnaCOM;
  function readTxt (lFilename: string; var lnObservations : integer; var ldataRA1: singlep): boolean;


implementation

uses npmform;

{$DEFINE NOTmedianfx}
function AnacomLesionNPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lnCrit,lRun,lnControl: integer; var lSymptomRA,lControlSymptomRA: SingleP;var lFactname,lOutName: string; lttestIn,lBMIn: boolean): boolean;
label
	123,667;
var
	lOutNameMod: string;
	lPlankImg: byteP;
        lOutImgSum,lOutImgBM,lOutImgT,
        lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,lCombinedSymptomRA: singleP;
        lPos,lPlank,lThread,lnControlsPlusPatients: integer;
	lVolVox,lMinMask,lMaxMask,lTotalMemory,lnPlanks,lVoxPerPlank,
        lThreadStart,lThreadEnd,lThreadInc,lnLesion,lnPermute,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lPosPct: int64;
	lT,lBMz,  lSum,lThresh,lThreshBonf,lThreshPermute,lThreshNULP :double;
	lObsp: pointer;
	lObs: Doublep0;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
        lSave,lBM,lttest,lLtest: boolean;
        lnControlNeg: integer;

        {$IFDEF medianfx}
        lmedianFX,lmeanFX,lsummean,lsummedian: double;
        lmediancount: integer;
        {$ENDIF}
begin
        lSave := true;
        lnControlNeg := lnControl; //negative for binomial test
        lttest := lttestin;
        lbm := lbmin;
        if (not (lttest)) and (not (lbm)) then begin
           lLtest := true;
           lBM := true;
           lnControlNeg := -lnControl;
        end;
        //lttest:= ttestmenu.checked;
        //lBM := BMmenu.checked;
        if lnControl < 1 then begin
           MainForm.NPMmsg('AnaCOM aborted - need data from at least 1 control individual');
           exit;
        end;
        lnPermute := 0;//MainForm.ReadPermute;
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
        lnControlsPlusPatients := lImages.Count+lnControl;
	createArray64(lObsp,lObs,lnControlsPlusPatients);
        getmem(lOutImgSum,lVolVox* sizeof(single));
	getmem(lOutImgBM,lVolVox* sizeof(single));
	getmem(lOutImgT,lVolVox* sizeof(single));
        MainForm.InitPermute (lImages.Count, lnPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do begin
                lOutImgSum^[lPos] := 0;
		lOutImgBM^[lPos] := 0;
		lOutImgT^[lPos] := 0;
	end;
        //sumptom array for lesions AND controls
        for lPos := 1 to lImages.Count do
            lObs^[lPos-1] := lSymptomRA^[lPos];
        for lPos := 1 to lnControl do
            lObs^[lPos-1+lImages.Count] :=  lControlSymptomRA^[lPos];
        getmem(lCombinedSymptomRA,lnControlsPlusPatients* sizeof(single));
        for lPos := 1 to lnControlsPlusPatients do
            lCombinedSymptomRA^[lPos] := lObs^[lPos-1];
        //next create permuted BM bounds
        if lBM then begin
           MainForm.NPMmsg('Generating BM permutation thresholds');
           MainForm.Refresh;
           //for lPos := 1 to lImages.Count do
           //    lObs^[lPos-1] := lSymptomRA^[lPos];
           genBMsim (lnControlsPlusPatients, lObs);
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

                        with TLesionContinuous.Create (MainForm.ProgressBar1,lttest,lBM,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lnControlNeg,lPlankImg,lOutImgSum,lOutImgBM,lOutImgT,nil,lCombinedSymptomRA) do
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
        //Next: save results from permutation thresholding....
        lThreshBonf := MainForm.reportBonferroni('Std',lnVoxTested);
        //Next: NULPS
        if lRun > 0 then //terrible place to do this - RAM problems, but need value to threshold maps
           lThreshNULP := MainForm.reportBonferroni('Unique overlap',CountOverlap2 (lImages, lnCrit,lnVoxTested,lPlankImg));

        //lThreshNULP := MainForm.reportBonferroni('Unique overlap',CountOverlap (lImages, lnCrit));
        //next: save data
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save sum map
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Sum'+lFactName,'.hdr');
        if (lSave) and (lRun < 1) then
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
//create new header - subsequent images will use Z-scores
        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
        if (lSave) and (lRun < 1) and (Sum2PowerCont(lOutImgSum,lVolVox,lImages.Count)) then begin
           lOutNameMod := ChangeFilePostfixExt(lOutName,'Power'+lFactName,'.hdr');
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
        end;

        //MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
if lttest then begin //save Ttest
        //next: convert t-scores to z scores

        if lnControl < 1 then //do not convert t-scores for anaCOM - numbers vary from voxel to voxel...
           for lPos := 1 to lVolVox do
               lOutImgT^[lPos] := TtoZ (lOutImgT^[lPos],lImages.Count-2);
        for lPos := 1 to lnPermute do begin
            lPermuteMaxT^[lPos] := TtoZ (lPermuteMaxT^[lPos],lImages.Count-2);
            lPermuteMinT^[lPos] := TtoZ (lPermuteMinT^[lPos],lImages.Count-2);
        end;
        lThresh := MainForm.reportFDR ('ttest', lVolVox, lnVoxTested, lOutImgT);
        lThreshPermute := MainForm.reportPermute('attest',lnPermute,lPermuteMaxT, lPermuteMinT);
	lOutNameMod := ChangeFilePostfixExt(lOutName,'attest'+lFactName,'.hdr');
        if lRun > 0 then
           MainForm.NPMmsgAppend('AnaComthreshtt,'+inttostr(lRun)+','+inttostr(MainForm.ThreshMap(lThreshNULP,lVolVox,lOutImgT))+','+realtostr(lThreshNULP,3)+','+realtostr(lThreshPermute,3)+','+realtostr(lThreshBonf,3));
        if lSave then
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgT,1);

end;
if lBM then begin //save Mann Whitney
        lThresh :=  MainForm.reportFDR ('BM', lVolVox, lnVoxTested, lOutImgBM);
        lThreshPermute := MainForm.reportPermute('aBM',lnPermute,lPermuteMaxBM, lPermuteMinBM);
        lOutNameMod := ChangeFilePostfixExt(lOutName,'aBM'+lFactName,'.hdr');
        if lRun > 0 then
           MainForm.NPMmsgAppend('AnaCOMthreshbm,'+inttostr(lRun)+','+inttostr(MainForm.ThreshMap(lThreshNULP,lVolVox,lOutImgBM))+','+realtostr(lThreshNULP,3)+','+realtostr(lThreshPermute,3)+','+realtostr(lThreshBonf,3));
        if lSave then
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgBM,1);
end;
//next: free dynamic memory
123:
        MainForm.FreePermute (lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,  lRanOrderp);
	freemem(lOutImgT);
	freemem(lOutImgBM);
        freemem(lOutImgSum);
	freemem(lObsp);
	freemem(lPlankImg);
        freemem(lCombinedSymptomRA);
	MainForm.NPMmsg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes'+lFactName,'.txt');
        if lSave then
           MainForm.MsgSave(lOutNameMod);
        MainForm.ProgressBar1.Position := 0;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lTotalMemory > 1 then freemem(lPlankImg);
	MainForm.NPMmsg('Unable to complete analysis.');
        MainForm.ProgressBar1.Position := 0;
end; //LesionNPMAnalyze



(*function readCSV2 (lFilename: string; lCol1,lCol2: integer;  var lnObservations : integer; var ldataRA1,ldataRA2: singlep): boolean;
const
     kHdrRow = 0;//1;
     kHdrCol = 0;//1;
var
   lNumStr: string;
   F: TextFile;
   lTempFloat: double;
   lCh: char;
   lnFactors,MaxC,R,C:integer;
   lError: boolean;

begin
     lError := false;
     result := false;
	 if not fileexists(lFilename) then begin
            showmessage('Can not find '+lFilename);
            exit;
         end;
	 AssignFile(F, lFilename);
	 FileMode := 0;  //Set file access to read only
	 //First pass: determine column height/width
	 Reset(F);
	 C := 0;
	 MaxC := 0;
	 R := 0;
	 while not Eof(F) do begin
		//read next line
		//read next line
		Read(F, lCh);
		if lCh = '#' then
		   while not (lCh in [#10,#13]) do
			   Read(F, lCh)
		else if not (lCh in [#10,#13,#9,',']) then begin
		   lNumStr := lNumStr + lCh;
		end else if lNumStr <> '' then begin
			lNumStr := '';
			inc(C);
			if C > MaxC then
				MaxC := C;
			if (lCh in [#10,#13]) then begin
				C := 0;
			   inc(R);

			end; //eoln
		end; //if lNumStr <> '' and not tab
	 end;
	 if lNumStr <> '' then  //july06- read data immediately prior to EOF
        inc(R);

     if (R <= (kHdrRow+1)) or (MaxC < (kHdrCol+lCol1)) or (MaxC < (kHdrCol+lCol2)) then begin
         showmessage('problems reading CSV - not enough columns/rows '+inttostr(lCol1)+'  '+inttostr(lCol2));
         exit;
     end;

     lnObservations := R -kHdrRow ; //-1: first row is header....
     lnFactors := MaxC-1;// -1: first column is Y values
     //fx(lnObservations,lnFactors);

     //exit;
     getmem(ldataRA1,lnObservations*sizeof(single));
     getmem(ldataRA2,lnObservations*sizeof(single));

     //second pass
	 Reset(F);
	 C := 1;
	 MaxC := 0;
	 R := 1;
	 lNumStr := '';
     lTempfloat := 0;
	 while not Eof(F) do begin
		//read next line
		Read(F, lCh);
		if lCh = '#' then
		   while not (lCh in [#10,#13]) do
			   Read(F, lCh)
		else if not (lCh in [#10,#13,#9,',']) then begin
		   lNumStr := lNumStr + lCh;
		end else if lNumStr <> '' then begin
            if (R > kHdrRow) and (C > kHdrCol) then begin
             if ((C-kHdrCol) = lCol1) or ((C-kHdrCol) = lCol2) then begin
               if lNumStr = '-' then begin
                  lTempFloat := 0;
               end else begin //number
                try
                   lTempFloat := strtofloat(lNumStr);
                except
                                    on EConvertError do begin
                                       if not lError then
                                          showmessage('Empty cells? Error reading CSV file row:'+inttostr(R)+' col:'+inttostr(C)+' - Unable to convert the string '+lNumStr+' to a number');
                                       lError := true;
                                       lTempFloat := nan;
                                    end;
                end;//except
                //showmessage(lNumStr);
                if (C-kHdrCol) = lCol1 then
                   ldataRA1^[R-kHdrRow] := lTempFloat
                else if (C-kHdrCol) = lCol2 then
                    ldataRA2^[R-kHdrRow] := lTempFloat;
               end; //number
             end; //col1 or col2
            end;// else //R > 1

			lNumStr := '';
			inc(C);
			if C > MaxC then
				MaxC := C;
			if (lCh in [#10,#13]) then begin
				C := 1;
			   inc(R);
			end; //eoln
		end; //if lNumStr <> '' and not tab
	 end;
     if (lNumStr <> '') and (C = lnFactors) then begin  //unterminated string
        try
           lTempFloat := strtofloat(lNumStr);
        except
              on EConvertError do begin
                 if not lError then
                    showmessage('Empty cells? Error reading CSV file row:'+inttostr(R)+' col:'+inttostr(C)+' - Unable to convert the string '+lNumStr+' to a number');
                 lError := true;
                 lTempFloat := nan;
              end;
        end;//except
        ldataRA2^[R-1] := lTempFloat;
     end;//unterminated string
     //read finel item
	 CloseFile(F);
	 FileMode := 2;  //Set file access to read/write
     result := true;
end;  *)

function readTxt (lFilename: string; var lnObservations : integer; var ldataRA1: singlep): boolean;
const
     kHdrRow = 0;//1;
     kHdrCol = 0;//1;
var
   lCol1: integer;
   lNumStr: string;
   F: TextFile;
   lTempFloat: double;
   lCh: char;
   lnFactors,MaxC,R,C:integer;
   lError: boolean;
begin
     lCol1:= 1;
     lError := false;
     result := false;
	 if not fileexists(lFilename) then begin
            showmessage('Can not find '+lFilename);
            exit;
         end;
	 AssignFile(F, lFilename);
	 FileMode := 0;  //Set file access to read only
	 //First pass: determine column height/width
	 Reset(F);
	 C := 0;
	 MaxC := 0;
	 R := 0;
	 while not Eof(F) do begin
		//read next line
		//read next line
		Read(F, lCh);
		if lCh = '#' then
		   while not (lCh in [#10,#13]) do
			   Read(F, lCh)
		else if not (lCh in [#10,#13,#9,',']) then begin
		   lNumStr := lNumStr + lCh;
		end else if lNumStr <> '' then begin
			lNumStr := '';
			inc(C);
			if C > MaxC then
				MaxC := C;
			if (lCh in [#10,#13]) then begin
				C := 0;
			   inc(R);

			end; //eoln
		end; //if lNumStr <> '' and not tab
	 end;
	 if lNumStr <> '' then  //july06- read data immediately prior to EOF
        inc(R);

     if (R <= (kHdrRow+1)) or (MaxC < (kHdrCol+lCol1)) then begin
         showmessage('problems reading CSV - not enough columns/rows ');
         exit;
     end;

     lnObservations := R -kHdrRow ; //-1: first row is header....
     lnFactors := kHdrCol+lCol1;// -1: first column is Y values
     //fx(lnObservations,lnFactors);

     //exit;
     getmem(ldataRA1,lnObservations*sizeof(single));

     //second pass
	 Reset(F);
	 C := 1;
	 MaxC := 0;
	 R := 1;
	 lNumStr := '';
     lTempfloat := 0;
	 while not Eof(F) do begin
		//read next line
		Read(F, lCh);
		if lCh = '#' then
		   while not (lCh in [#10,#13]) do
			   Read(F, lCh)
		else if not (lCh in [#10,#13,#9,',']) then begin
		   lNumStr := lNumStr + lCh;
		end else if lNumStr <> '' then begin
            if (R > kHdrRow) and (C > kHdrCol) then begin
             if ((C-kHdrCol) = lCol1) {or ((C-kHdrCol) = lCol2)} then begin
               if lNumStr = '-' then begin
                  lTempFloat := 0;
               end else begin //number
                try
                   lTempFloat := strtofloat(lNumStr);
                except
                                    on EConvertError do begin
                                       if not lError then
                                          showmessage('Empty cells? Error reading CSV file row:'+inttostr(R)+' col:'+inttostr(C)+' - Unable to convert the string '+lNumStr+' to a number');
                                       lError := true;
                                       lTempFloat := nan;
                                    end;
                end;//except
                //showmessage(lNumStr);
                if (C-kHdrCol) = lCol1 then begin
                    //showmessage(lNumStr);
                   ldataRA1^[R-kHdrRow] := lTempFloat;
                end;
                {else if (C-kHdrCol) = lCol2 then
                    ldataRA2^[R-kHdrRow] := lTempFloat;}
               end; //number
             end; //col1 or col2
            end;// else //R > 1

			lNumStr := '';
			inc(C);
			if C > MaxC then
				MaxC := C;
			if (lCh in [#10,#13]) then begin
				C := 1;
			   inc(R);
			end; //eoln
		end; //if lNumStr <> '' and not tab
	 end;
     //showmessage(lNumStr+'  '+inttostr(lnFactors)+'  '+inttostr(C));
     if (lNumStr <> '') and (C = lnFactors) then begin  //unterminated string

        try
           lTempFloat := strtofloat(lNumStr);
        except
              on EConvertError do begin
                 if not lError then
                    showmessage('Empty cells? Error reading CSV file row:'+inttostr(R)+' col:'+inttostr(C)+' - Unable to convert the string '+lNumStr+' to a number');
                 lError := true;
                 lTempFloat := nan;
              end;
        end;//except
         //showmessage(inttostr(R)+'  '+floattostr(lTempFLoat));
        ldataRA1^[R] := lTempFloat;
     end;//unterminated string
     //read finel item
	 CloseFile(F);
	 FileMode := 2;  //Set file access to read/write
     result := not lError;
end;

procedure DoAnaCOM;
label
	666;
var
   lControlFilename: string;
   lI, lnControlObservations : integer;
   lControldata: singlep;
        //lBinomial: boolean;
	lFact,lnFactors,lSubj,lnSubj,lnSubjAll,lMaskVoxels,lnCrit: integer;
	lImageNames,lImageNamesAll:  TStrings;
        lPredictorList: TStringList;
	lTemp4D,lMaskname,lOutName,lFactname: string;
	lMaskHdr: TMRIcroHdr;
        lMultiSymptomRA,lSymptomRA: singleP;
begin
     npmform.MainForm.memo1.lines.clear;
     npmform.MainForm.memo1.lines.add('AnaCOM analysis requires TXT/CSV format text file.');
     npmform.MainForm.memo1.lines.add('One row per control participant.');
     npmform.MainForm.memo1.lines.add('First column is performance of that participant.');
     npmform.MainForm.memo1.lines.add('Example file:');
     npmform.MainForm.memo1.lines.add('11');
     npmform.MainForm.memo1.lines.add('19');
     npmform.MainForm.memo1.lines.add('2');
     npmform.MainForm.memo1.lines.add('22');
     npmform.MainForm.memo1.lines.add('19');
     npmform.MainForm.memo1.lines.add('6');
     if not MainForm.OpenDialogExecute('Select text file',false,false,'Text file (*.txt)|*.txt;*.csv') then begin
	   showmessage('AnaCOM aborted: Control data file selection failed.');
	   exit;
     end; //if not selected
     lControlFilename := MainForm.OpenHdrDlg.Filename;
     if (not readTxt (lControlFilename, lnControlObservations,lControldata)) or (lnControlObservations < 1) then begin
      showmessage('Error reading file '+lControlFilename);
      exit;
     end;
     npmform.MainForm.memo1.lines.add('Control (n='+inttostr(lnControlObservations)+')performance   ['+lControlFilename+']');
     for lI := 1 to lnControlObservations do
        npmform.MainForm.memo1.lines.add(inttostr(lI)+' '+floattostr(lControldata^[lI]));
     //begin - copy
     lImageNamesAll:= TStringList.Create; //not sure why TStrings.Create does not work???
     lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
     //next, get 1st group
     if not MainForm.GetValX(lnSubjAll,lnFactors,lMultiSymptomRA,lImageNamesAll,lnCrit,{,binom}lPredictorList) then begin
        showmessage('Error with VAL file');
        goto 666;
     end;
     lTemp4D := CreateDecompressed4D(lImageNamesAll);
     if (lnSubjAll < 1) or (lnFactors < 1) then begin
        Showmessage('AnaCOM error: not enough patients ('+inttostr(lnSubjAll)+') or factors ('+inttostr(lnFactors)+').');
        goto 666;
     end;
     lMaskname := lImageNamesAll[0];
     if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading 1st file: '+lMaskName);
	   goto 666;
     end;
     lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
     if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mask file size too small.');
	   goto 666;
     end;
     lOutName := ExtractFileDirWithPathDelim(lMaskName)+'results';
     MainForm.SaveHdrDlg.Filename := loutname;
     lOutName := lOutName+'.nii.gz';
     if not MainForm.SaveHdrName ('Base Statistical Map', lOutName) then exit;
     for lFact := 1 to lnFactors do begin
         lImageNames.clear;
         for lSubj := 1 to lnSubjAll do
           {$IFNDEF FPC}if lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)] <> NaN then {$ENDIF}
              lImageNames.Add(lImageNamesAll[lSubj-1]);
         lnSubj := lImageNames.Count;
         if lnSubj > 1 then begin
            getmem(lSymptomRA,lnSubj * sizeof(single));
            lnSubj := 0;
            for lSubj := 1 to lnSubjAll do
              {$IFNDEF FPC}if lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)] <> NaN then begin
              {$ELSE} begin{$ENDIF}
                 inc(lnSubj);
                 lSymptomRA^[lnSubj] := lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)];
              end;
            MainForm.NPMmsgClear;
            MainForm.NPMMsg(MainForm.GetKVers);
            MainForm.NPMMsg('Threads: '+inttostr(gnCPUThreads));
            npmform.MainForm.memo1.lines.add('Control (n='+inttostr(lnControlObservations)+')performance   ['+lControlFilename+']');
            for lI := 1 to lnControlObservations do
                npmform.MainForm.memo1.lines.add(inttostr(lI)+' '+floattostr(lControldata^[lI]));
            lFactName := lPredictorList.Strings[lFact-1];
            lFactName := LegitFilename(lFactName,lFact);
            MainForm.NPMMsg('Patient performance, (n= '+inttostr(lnSubj)+') Factor = '+lFactname);
            For lSubj := 1 to lnSubj do
                MainForm.NPMMsg (lImageNames.Strings[lSubj-1] + ' = '+realtostr(lSymptomRA^[lSubj],2) );
            MainForm.NPMMsg('Total voxels = '+inttostr(lMaskVoxels));
            MainForm.NPMMsg('Only testing voxels damaged in at least '+inttostr(lnCrit)+' individual[s]');
            MainForm.NPMMsg('Number of Lesion maps = '+inttostr(lnSubj));
            if not CheckVoxelsGroup(lImageNames,lMaskVoxels) then begin
               showmessage('File dimensions differ from mask.');
	       goto 666;
            end;
            MainForm.ReportDescriptives(lSymptomRA,lnSubj);
            AnacomLesionNPMAnalyze(lImageNames,lMaskHdr,lnCrit,-1,lnControlObservations,lSymptomRA,lControldata,lFactName,lOutname,true {ttest},false{BM});
            Freemem(lSymptomRA);
         end; //lnsubj > 1
     end; //for each factor
     if lnSubjAll > 0 then
       Freemem(lMultiSymptomRA);
     666:
     lImageNames.Free;
     lImageNamesAll.Free;
     lPredictorList.Free;
     DeleteDecompressed4D(lTemp4D);
     freemem(lControldata);
end;

end.
