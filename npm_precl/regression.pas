unit regression;
//only for Delphi - not Freepascal
//Unit for running multiple regression
interface
uses
{$H+}
{$IFNDEF UNIX} Windows, {$ENDIF}
{$IFDEF FPC} utypes,regmult,{$ELSE}
utypes,regmult,
{$ENDIF}define_types,Classes,nifti_hdr,sysutils,nifti_img,
     StatThdsUtil,Forms,Distr,Dialogs,npmform, tfce_clustering;

function GetValReg (var lnSubj,lnFactors: integer; var X : PMatrix; var lImageNames:  TStrings; var lPredictorList: TStringList): boolean;
function ARegressNPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; var X: PMatrix; lnFactors: integer; var lPredictorList: TStringList; lOutname: string; lnPermute, TFCEconn: integer): boolean;
function Regress2NPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lOutname: string; var lXadditional: PMatrix; lnAdditionalFactors: integer ): boolean;
function TtoR(t,df: double): double;


implementation
uses valformat,hdr,math;

(*function readCSV (lFilename: string;  var lnObservations,lnFactors : integer; var X : PMatrix; var Y: PVector): boolean;
var
   lNumStr: string;
   F: TextFile;
   lTempFloat: double;
   lCh: char;
   lPos,MaxC,R,C:integer;
   lError: boolean;

begin
     result := false;
	 if not fileexists(lFilename) then exit;
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

     if (R < 2) or (MaxC < 5) then begin
         showmessage('problems reading CSV');
         exit;
     end;
     lnObservations := MaxC;
     lnFactors := R -1;
     DimVector(Y, lnObservations);
     DimMatrix(X, lnFactors, lnObservations);
     //second pass
	 Reset(F);
	 C := 0;
	 MaxC := 0;
	 R := 1;
	 lNumStr := '';
	 while not Eof(F) do begin
		//read next line
		Read(F, lCh);
		if lCh = '#' then
		   while not (lCh in [#10,#13]) do
			   Read(F, lCh)
		else if not (lCh in [#10,#13,#9,',']) then begin
		   lNumStr := lNumStr + lCh;
		end else if lNumStr <> '' then begin
            if lNumStr = '-' then begin
               lTempFloat := 0;
            end else begin //number
                try
                   lTempFloat := strtofloat(lNumStr);
                except
                                    on EConvertError do begin
                                       if not lError then
                                          showmessage('Empty cells? Error reading VAL file row:'+inttostr(R)+' col:'+inttostr(C)+' - Unable to convert the string '+lNumStr+' to a number');
                                       lError := true;
                                       lTempFloat := nan;
                                    end;
                end;
                if R = 1 then
                   Y^[C+1] := lTempFloat
                else
                    X^[R-1]^[C+1] := lTempFloat;
				//xxx := lTempFloat;//DataGrid.Cells[ C, kMaxFactors+R-1 ] := (lNumStr) ;
            end;
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
	 CloseFile(F);
	 FileMode := 2;  //Set file access to read/write
         result := true;
end;

function TestMultReg: boolean;
var
i,lnFactors, lnObservations: integer;
X : PMatrix;
Y: PVector;
lOutT,lOutSlope: DoubleP0;
lStart: dword;
begin
     if not readCSV('C:\xio.csv',lnObservations,lnFactors,X,Y ) then exit;
     //showmessage('alpha');
     getmem(lOutT, (lnFactors+1)* sizeof(double));
     getmem(lOutSlope, (lnFactors+1)* sizeof(double));

     lStart := gettickcount;
     for i := 1 to 10000 do
         MultipleRegressionVec (lnObservations,lnFactors, X, Y, lOutT,lOutSlope);
     fx(gettickcount-lstart);


     if MultipleRegressionVec (lnObservations,lnFactors, X, Y, lOutT,lOutSlope) then begin
        for i := 0 to lnFactors do
            fx(lOutT^[i],lOutSlope^[i]);

     end;
     freemem(lOutT);
     freemem(lOutSlope);
     DelMatrix(X, lnFactors, lnObservations);
     DelVector(Y, lnObservations);
end;     *)

(*procedure rx(lnObs,lnFactors: integer; X: PMatrix; lObs: Doublep0);
var
   n,f: integer;
   str: string;
begin
     for n := 1 to lnObs do begin
         str := floattostr(lObs^[n-1]);
         for f := 1 to lnFactors do
             str := str+','+floattostr(X^[f]^[n]);

         MainForm.NPMmsg(str);
     end;//each obs
     str := '----------';
end;//proc RX*)

function Sign(value: double): double;
begin
    if value > 0 then
       result := 1
    else if value < 0 then
         result := -1
    else
        result := 0;
end;

function TtoR(t,df: double): double;
CONST
   eps=3.0e-7;
begin
    result := 0;
    if (t = 0) or (df = 0) then
       exit;
    result := sign(t)/ sqrt( (df/(t*t+eps)) +1  );
end;

{$DEFINE SaveT} //if  SaveT then t-score map will be saved
{$DEFINE SaveRnotZ} //if  SaveRnotZ then r-value map will be saved, but not Z-score map
function Regress2NPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lOutname: string; var lXadditional: PMatrix; lnAdditionalFactors: integer ): boolean;
//lImages is list 1..N of 1st images followed by 1..N of corresponding control images
//example c1.img, c2.img,c3.img,e1.img,e2.img,e3.img
//lImages.Count must be even
label
	667;
const
     kMaxFact = 80;
var
	lOutNameMod,lFactName,lRunName: string;
	lMaskImg,lPlankImg,lOutImgMn: SingleP;
        lOutImgR: array [1..kMaxFact] of SingleP;
        lTotalMemory: int64;
	lnFactors,lnObservations,lnObservationsDiv2,lPlank,lVolVox,lPos,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
	lDF,lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lPosPct,lFact,lnStatFact: integer;
	l1st,  lSum, lMn: double;
        lVar: boolean;
	lObsp: pointer;
	lObs: Doublep0;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lnPermute: integer;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
        lZP: Pointer;
	lZra : DoubleP0;
        X : PMatrix;
begin
        lnFactors := 1+lnAdditionalFactors;
        lnPermute := MainForm.ReadPermute;
        if odd(lImages.Count) then begin
            showmessage('Regress2NPMAnalyze must be passed an even number of images: the first half of the list is the experimental images, followed by corresponding control images.');
            exit;
        end;
        lnObservations := lImages.Count;
       lnObservationsDiv2 := lImages.Count div 2;
       lDF := lnObservationsDiv2-lnFactors-1;
       if lDF < 1 then begin
            showmessage('Regress2NPMAnalyze: DF must be >0 (DF=[Num-Factors-1]) Num='+inttostr(lnObservationsDiv2)+' Factors='+inttostr(lnFactors) );
            exit;
       end;
       DimMatrix(X, lnFactors, lnObservationsDiv2);
        //fx(lnAdditionalFactors);

     if lnAdditionalFactors > 0 then begin
        for lPos2 := 1 to lnAdditionalFactors do begin
            for lPos := 1 to lnObservationsDiv2 do begin
                X^[lPos2+1]^[lPos] := lXadditional^[lPos2]^[lPos];
                //fx(lPos2+1,lPos, X^[lPos2+1]^[lPos]);
            end;
        end; //pos 2
     end; //additional factros
	//Memo1.Lines.Add('Permutations = ' +IntToStr(lnPermute));
	MainForm.Memo1.Lines.Add('Analysis began = ' +TimeToStr(Now));
	lTotalMemory := 0;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
        lnStatFact := lnFactors + 1; //factors + overall model
        if lnStatFact > (kMaxFact-1) then begin //-1 because factors + model
	   MainForm.Memo1.Lines.Add('ERROR: Can not analyze more than = ' +inttostr(kMaxFact-1)+' factors');
           goto 667;
        end;
	//load mask
	getmem(lMaskImg,lVolVox*sizeof(single));
	if not LoadImg(lMaskHdr.ImgFileName, lMaskImg, 1, lVolVox,round(gOffsetRA[0]),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		MainForm.Memo1.Lines.Add('Unable to load mask ' +lMaskHdr.ImgFileName);
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
		MainForm.Memo1.Lines.Add('Mask appears empty' +lMaskHdr.ImgFileName);
		goto 667;
	end;
	MainForm.Memo1.Lines.Add('Mask has voxels from '+inttostr(lMinMask)+'..'+inttostr(lMaxMask));
	lVoxPerPlank :=  kPlankSz div lnObservations div sizeof(single) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lnObservations;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lnObservations) ) + 1;
	MainForm.Memo1.Lines.Add('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lnObservations)));
	MainForm.Memo1.Lines.Add('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
        if (lnPlanks = 1) then
            getmem(lPlankImg,lTotalMemory*sizeof(single)) //assumes 4bpp
        else
	    getmem(lPlankImg,kPlankSz);
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
	lnVoxTested := 0;
        for lPos := 1 to lnObservations do
		if gScaleRA[lPos] = 0 then
			gScaleRA[lPos] := 1;
	createArray64(lObsp,lObs,lnObservations);
        getmem(lOutImgMn,lVolVox* sizeof(single));
	for lPos := 1 to lVolVox do
                lOutImgMn^[lPos] := 0;
        for lFact := 1 to (lnStatFact) do begin //+1 as we include full model
	    getmem(lOutImgR[lFact],lVolVox* sizeof(single));
	    for lPos := 1 to lVolVox do
		lOutImgR[lFact]^[lPos] := 0;

        end;
        createArray64(lZp,lZra,lnFactors+1); //+1 as we include full model
        //InitPermute (lImages.Count, lnPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxTW, lPermuteMinTW,lPermuteMaxWMW, lPermuteMinWMW, lRanOrderp, lRanOrder);
	for lPlank := 1 to lnPlanks do begin
		MainForm.Memo1.Lines.Add('Computing plank = ' +Inttostr(lPlank));
                MainForm.Refresh;
		lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lMaxMask then begin
			lVoxPerPlank := lVoxPerPlank - (lEndVox-lMaxMask);
			lEndVox := lMaxMask;
		end;
		lPlankImgPos := 1;
		for lPos := 1 to lnObservations do begin
			if not LoadImg(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
				goto 667;
			lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
                lPosPct := lVoxPerPlank div 100;
		for lPos2 := 1 to lVoxPerPlank do begin
                        if (lPos2 mod lPosPct) = 0 then begin
                           MainForm.ProgressBar1.Position := round((lPos2/lVoxPerPlank)*100);
                           Application.Processmessages;
                        end;
			lPos2Offset := lPos2+lStartVox-1;
			if lMaskImg^[lPos2Offset] <> 0 then begin
				inc(lnVoxTested);
                                lSum := 0;
                                //check for variance
                                lVar := false;
                                lPos := 1;
                                l1st := (gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos];
				for lPos := 1 to lnObservations do
					lObs^[lPos-1] := (gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos];
                                for lPos := 1 to lnObservationsDiv2 do begin
                                        lSum := lSum +  lObs^[lPos-1];
                                        if (not lVar) and (lObs^[lPos-1]<>l1st) then
                                           lVar := true;
                                        //lSumOfSqrs := lSumOfSqrs + sqr(lObs[lPos-1]);
                                        X^[1]^[lPos] := lObs^[lnObservationsDiv2+lPos-1];
                                end;
                                lOutImgMn^[lPos2Offset] := lSum/lnObservationsDiv2;
                                if lVar then begin
                                   MultipleRegression (lnObservationsDiv2,lnFactors, X, lObs,  lZra);
                                   //if lPos2Offset = 359948 then   rx(lnObservationsDiv2,lnFactors,X,lObs);
                                   for lFact := 1 to lnStatFact do
				                       lOutImgR[lFact]^[lPos2Offset] := lZra^[lFact-1];
                                end;
                                //StatPermute (lttest,lwelch,lWMW,lImages.Count, lnGroup1,lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxTW, lPermuteMinTW,lPermuteMaxWMW, lPermuteMinWMW, lObs,lRanOrder);
			end; //in brain mask - compute
		end;
		lStartVox := lEndVox + 1;
	end;
        //next report findings
	MainForm.Memo1.Lines.Add('Voxels tested = ' +Inttostr(lnVoxTested));
        MainForm.reportBonferroni('Std',lnVoxTested);
        //next: save data
        if lnFactors = 1 then
           lRunName := 'reg'
        else
            lRunName := '';
//savedata
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save mean
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Mn'+lRunName,'.hdr');
        if not FileExistsEX(lOutNameMod) then
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgMn,1);
//save regression
        for lFact := 1 to (lnStatFact) do begin
            if (lFact > lnFactors) and (lnFactors = 1) then
                lFactName := 'intercept'+'reg' //for analysis of multiple single regressions
            else if (lFact > lnFactors) then
                   lFactName := 'intercept'
            else
                lFactName := 'reg'+inttostr(lFact);
            MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
            {$IFDEF SaveT} //if  SaveTRnotZ then t-score and r-score maps will be created, but no Z-score maps
            //the next bit is optional - save data as T-values instead of Z-scores
            //  this allows direct comparison with SPM...
            MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,lDF,0,lnVoxTested,kNIFTI_INTENT_TTEST,inttostr(lnVoxTested) );
            lOutNameMod := ChangeFilePostfixExt(lOutName, 'wlsT'+lFactName,'.hdr');
	    NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgR[lFact],1);
            {$ENDIF}
            {$IFDEF SaveRnotZ}
            MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,lDF,0,lnVoxTested,kNIFTI_INTENT_CORREL,inttostr(lnVoxTested) );
            for lPos := 1 to lVolVox do
                lOutImgR[lFact]^[lPos] := TtoR (lOutImgR[lFact]^[lPos],lDF);
            lOutNameMod := ChangeFilePostfixExt(lOutName, 'wlsR'+lFactName,'.hdr');
	    NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgR[lFact],1);
            {$ELSE}
            //next - save Zscores
            MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,lDF,0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
            //{ DoF = Nb points - Nb parameters }
            for lPos := 1 to lVolVox do
                lOutImgR[lFact]^[lPos] := TtoZ (lOutImgR[lFact]^[lPos],lDF);
            MainForm.reportFDR ('wls'+lFactName, lVolVox, lnVoxTested, lOutImgR[lFact]);
            lOutNameMod := ChangeFilePostfixExt(lOutName, 'wls'+lFactName,'.hdr');
	    NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgR[lFact],1);
            {$ENDIF}
	    freemem(lOutImgR[lFact]);
        end;
        //next: close images
        Freemem(lZp);
        freemem(lOutImgMn);
	freemem(lObsp);
	freemem(lMaskImg);
	freemem(lPlankImg);
	MainForm.Memo1.Lines.Add('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes'+lRunName,'.txt');
        MainForm.Memo1.Lines.SaveToFile(lOutNameMod);
        MainForm.ProgressBar1.Position := 0;
        DelMatrix(X, lnFactors, lnObservationsDiv2);
	exit;
667: //you only get here if you aborted ... free memory and report error
        DelMatrix(X, 1, lnObservationsDiv2);
	if lVolVox > 1 then freemem(lMaskImg);
	if lTotalMemory > 1 then freemem(lPlankImg);
	MainForm.Memo1.Lines.Add('Unable to complete analysis.');
        MainForm.ProgressBar1.Position := 0;
end;



{$DEFINE NoThread}
function InnerARegressNPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; var X: PMatrix; lnFactors: integer; var lPredictorList: TStringList; lOutname: string; lSaveData: boolean; var lMinZ,lMaxZ: double; var lMaxNegTFCEZ, lMaxTFCEZ:single; TFCEconn: integer): boolean;
//TFCEmode 0 = no TFCE, 1 = only report min/maxTFCE, 2 = save TFCE map to disk
{$IFNDEF Thread}
const
     kMaxFact = 80;
{$ENDIF}
label
	667;
var
	lOutNameMod,lFactName,lRunName: string;
	lMaskImg,lPlankImg,lOutImgMn: SingleP;
    {$IFDEF Thread}
    lOutImgR: TRegRA;
    {$ELSE}
    lOutImgR: array [1..kMaxFact] of SingleP;
    {$ENDIF}
    lTotalMemory: int64;
	lnObservations,lPlank,lVolVox,lPos,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
	//lPos2,lPos2Offset,
    lDF,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lFact,lnStatFact: integer;
	//l1st,  lSum, lMn: double;
    //lVar: boolean;
	//lObsp: pointer;lObs: Doublep0;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
    {$IFDEF Thread}
    lThread,lThreadStart,lThreadEnd,lThreadInc: integer;
    {$ELSE}
   lObsP,lZP: Pointer;
	lObs,lZra : DoubleP0;
    lSum,l1st: double;
    lVar: boolean;
    lPos2,lPosPct,lPos2Offset: integer;

    {$ENDIF}
begin


     lnObservations := lImages.Count;
     lDF := lnObservations-lnFactors-1;
       if lDF < 1 then begin
            showmessage('Regress2NPMAnalyze: DF must be >0 (DF=[Num-Factors-1]) Num='+inttostr(lnObservations)+' Factors='+inttostr(lnFactors) );
            exit;
       end;
	 if (lSaveData) then MainForm.NPMmsg('Analysis began = ' +TimeToStr(Now));
	 lTotalMemory := 0;
     lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	 if (lVolVox < 1) then goto 667;
     lnStatFact := lnFactors + 1; //factors + overall model
     if lnStatFact > (kMaxFact-1) then begin //-1 because factors + model
        MainForm.NPMmsg('ERROR: Can not analyze more than = ' +inttostr(kMaxFact-1)+' factors');
        goto 667;
     end;
	//load mask
	getmem(lMaskImg,lVolVox*sizeof(single));
	if not LoadImg(lMaskHdr.ImgFileName, lMaskImg, 1, lVolVox,round(gOffsetRA[0]),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		MainForm.NPMmsg('Unable to load mask ' +lMaskHdr.ImgFileName);
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
		MainForm.NPMmsg('Mask appears empty' +lMaskHdr.ImgFileName);
		goto 667;
	end;
	if (lSaveData) then MainForm.NPMmsg('Mask has voxels from '+inttostr(lMinMask)+'..'+inttostr(lMaxMask));
	lVoxPerPlank :=  kPlankSz div lnObservations div sizeof(single) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lnObservations;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lnObservations) ) + 1;
	if (lSaveData) then MainForm.NPMmsg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lnObservations)));
	if (lSaveData) then MainForm.NPMmsg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
    if (lnPlanks = 1) then
       getmem(lPlankImg,lTotalMemory* sizeof(single)) //assumes 4bpp
    else
	    getmem(lPlankImg,kPlankSz);
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
	//lnVoxTested := 0;
    for lPos := 1 to lnObservations do
		if gScaleRA[lPos] = 0 then
			gScaleRA[lPos] := 1;
	//createArray64(lObsp,lObs,lnObservations);
    getmem(lOutImgMn,lVolVox* sizeof(single));
	for lPos := 1 to lVolVox do
        lOutImgMn^[lPos] := 0;
    for lFact := 1 to (lnStatFact) do begin //+1 as we include full model
	    getmem(lOutImgR[lFact],lVolVox* sizeof(single));
	    for lPos := 1 to lVolVox do
		    lOutImgR[lFact]^[lPos] := 0;
    end;
    //createArray64(lZp,lZra,lnFactors+1); //+1 as we include full model
    {$IFDEF Thread}
    
    ClearThreadDataPvals(gnCPUThreads,0) ;
    {$ELSE}
    lnVoxTested := 0;
    {$ENDIF}
	for lPlank := 1 to lnPlanks do begin
		if (lSaveData) then MainForm.NPMmsg('Computing plank = ' +Inttostr(lPlank));
        MainForm.Refresh;
		lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lMaxMask then begin
			lVoxPerPlank := lVoxPerPlank - (lEndVox-lMaxMask);
			lEndVox := lMaxMask;
		end;
		lPlankImgPos := 1;
		for lPos := 1 to lnObservations do begin
			if not LoadImg(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
				goto 667;
			lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
        {$IFDEF Thread}
        lThreadStart := 1;
        lThreadInc := lVoxPerPlank  div gnCPUThreads;
        lThreadEnd := lThreadInc;
        Application.processmessages;
        for lThread := 1 to gnCPUThreads do begin
            if lThread = gnCPUThreads then
               lThreadEnd := lVoxPerPlank; //avoid integer rounding error
            with TLinThreadStat.Create (X,ProgressBar1, lnFactors,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lnObservations, lMaskImg,lPlankImg,lOutImgMn,lOutImgR) do
                    {$IFDEF FPC} OnTerminate := @ThreadDone; {$ELSE}OnTerminate := ThreadDone;{$ENDIF}
               inc(gThreadsRunning);
               Msg('Thread ' +Inttostr(gThreadsRunning)+' = '+inttostr(lThreadStart)+'..'+inttostr(lThreadEnd));
               lThreadStart := lThreadEnd + 1;
               lThreadEnd :=lThreadEnd + lThreadInc;
        end; //for each thread
        repeat
              Application.processmessages;
        until gThreadsRunning = 0;
        Application.processmessages;
        {$ELSE} //not threaded
          createArray64(lZp,lZra,lnFactors+1); //+1 as we include full model
          createArray64(lObsp,lObs,lnObservations);
        lPosPct := lVoxPerPlank div 100;
		for lPos2 := 1 to lVoxPerPlank do begin
            if (lPos2 mod lPosPct) = 0 then begin
               MainForm.ProgressBar1.Position := round((lPos2/lVoxPerPlank)*100);
               Application.Processmessages;
            end;
			lPos2Offset := lPos2+lStartVox-1;
			if lMaskImg^[lPos2Offset] <> 0 then begin
				inc(lnVoxTested);
                lSum := 0;
                //check for variance
                lVar := false;
                lPos := 1;
                l1st := (gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos];
				for lPos := 1 to lnObservations do begin
					lObs^[lPos-1] := (gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos];
                    lSum := lSum +  lObs^[lPos-1];
                    if (not lVar) and (lObs^[lPos-1]<>l1st) then
                       lVar := true;
                end;
                lOutImgMn^[lPos2Offset] := lSum/lnObservations;
                if lVar then begin
                   MultipleRegression (lnObservations,lnFactors, X, lObs,  lZra);
                   //if {lZra^[0] < -5.548} lPos2Offset = 762287 then
                   //   ReportRegression (lPos2Offset,lnObservations,lnFactors, X, lObs,  lZra );
                   for lFact := 1 to lnStatFact do
                       lOutImgR[lFact]^[lPos2Offset] := lZra^[lFact-1];
                end;
			end; //in brain mask - compute
		end; //for each voxel
          Freemem(lZp);
          Freemem(lObsp);
        {$ENDIF} //if threaded else not threaded
		lStartVox := lEndVox + 1;
	end; //for each plank
    {$IFDEF Thread}
    lnVoxTested :=  SumThreadDataLite(gnCPUThreads);
    {$ENDIF}
    //FACTOR 1 MinMax
    lFact := 1;
    lMinZ := lOutImgR[lFact]^[1];
    for lPos := 1 to lVolVox do
                if (lOutImgR[lFact]^[lPos] < lMinZ) then lMinZ :=lOutImgR[lFact]^[lPos];
	lMinZ := TtoZ (lMinZ,lDF);
    lMaxZ := lOutImgR[lFact]^[1];
    for lPos := 1 to lVolVox do
                if (lOutImgR[lFact]^[lPos] > lMaxZ) then lMaxZ :=lOutImgR[lFact]^[lPos];
	lMaxZ := TtoZ (lMaxZ,lDF);
    //MainForm.NPMmsg('Factor1MinMax ' +floattostr(lMinZ)+' '+floattostr(lMaxZ));

    if (lSaveData) then begin
    //next report findings
	MainForm.NPMmsg('Voxels tested = ' +Inttostr(lnVoxTested));
    MainForm.reportBonferroni('Std',lnVoxTested);
    //next: save data
    if lnFactors = 1 then
       lRunName := lPredictorList[0]
    else
        lRunName := '';

    //savedata
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);



    //save mean
    lOutNameMod := ChangeFilePostfixExt(lOutName,'Mean'+lRunName,'.hdr');
    if not FileExistsEX(lOutNameMod) then
       NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgMn,1);
    //save regression
    for lFact := 1 to (lnStatFact) do begin
         if (lFact > lnFactors) and (lnFactors = 1) then begin
            //nothing
         end else begin
            if (lFact > lnFactors) and (lnFactors = 1) then
                lFactName := 'intercept'+lPredictorList[0] //for analysis of multiple single regressions
            else if (lFact > lnFactors) then
                   lFactName := 'model'
            else
                lFactName := lPredictorList[lFact-1];
            MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
            //NEXT : optional save t-maps
            //MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,lDF,0,lnVoxTested,kNIFTI_INTENT_TTEST,inttostr(lnVoxTested) );
            //lOutNameMod := ChangeFilePostfixExt(lOutName, 'wlsT'+lFactName,'.hdr');
            //NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgR[lFact],1);
            //END: t-maps
            //next - Z scores
            MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,lDF,0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
            //{ DoF = Nb points - Nb parameters }
            for lPos := 1 to lVolVox do
                lOutImgR[lFact]^[lPos] := TtoZ (lOutImgR[lFact]^[lPos],lDF);
            MainForm.reportFDR ('wls'+lFactName, lVolVox, lnVoxTested, lOutImgR[lFact]);
            lOutNameMod := ChangeFilePostfixExt(lOutName, 'wls'+lFactName,'.hdr');
	    NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgR[lFact],1);
            if (lFact = 1) and (TFCEconn > 0) then begin //TFCE
               //lMinZ := lOutImgR[lFact]^[1];

               MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,lDF,0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
               doTFCEbothPolarities (lStatHdr, lOutImgR[lFact], TFCEconn {NumConn}, 2.0{H}, 0.5 {E}, 0, lMaxZ/100, 0, lMinZ/100, lMaxTFCEZ, lMaxNegTFCEZ);
               lOutNameMod := ChangeFilePostfixExt(lOutName, 'tfce'+lFactName,'.hdr');
	       NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgR[lFact],1);
            end; //TFCE

         end;//if..else intercept and lnFactors = 1
    end;//for each statfactor
    end; //if lSaveData


    if (not (lSaveData)) and (TFCEconn > 0) and ((lMaxTFCEZ <> 0) or (lMaxNegTFCEZ <> 0)) then begin
       //lMinZ := lOutImgR[lFact]^[1];
       lFact := 1;
       MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,lDF,0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
       doTFCEbothPolarities (lStatHdr, lOutImgR[lFact], TFCEconn {NumConn}, 2.0{H}, 0.5 {E}, 0, lMaxTFCEZ, 0, lMaxNegTFCEZ, lMaxTFCEZ, lMaxNegTFCEZ)

    end; //xxx
    //next: close images
    for lFact := 1 to (lnStatFact) do
    	freemem(lOutImgR[lFact]);

    //Freemem(lZp);
    freemem(lOutImgMn);
	//freemem(lObsp);
	freemem(lMaskImg);
	freemem(lPlankImg);

    //lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes'+lRunName,'.txt');
    //MainForm.MsgSave(lOutNameMod);
    MainForm.ProgressBar1.Position := 0;
exit;
667: //you only get here if you aborted ... free memory and report error
	if lVolVox > 1 then freemem(lMaskImg);
	if lTotalMemory > 1 then freemem(lPlankImg);
	MainForm.NPMmsg('Unable to complete analysis.');
    MainForm.ProgressBar1.Position := 0;
end;

procedure PermuteMatrix(var Src, Dest: PMatrix; lnSubj: integer); //assumes only one column/factor!!!
var
    lRow,lPos: integer;
    lSwap: double;
begin
    for lRow := 1 to lnSubj do
         Dest^[1]^[lRow] := Src^[1]^[lRow];
    for lRow := lnSubj downto 1 do begin
     lPos := random(lRow)+1;
     lSwap := Dest^[1]^[lRow];
     Dest^[1]^[lRow] := Dest^[1]^[lPos];
     Dest^[1]^[lPos] := lSwap;
    end;

end;

function ARegressNPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; var X: PMatrix; lnFactors: integer; var lPredictorList: TStringList; lOutname: string; lnPermute, TFCEconn: integer ): boolean;
label
    777;
var
     //SaveData: boolean; var
          lMaxTFCEZ, lMaxNegTFCEZ: single;
          lMinZ,lMaxZ,lTFCEdh,lNegTFCEdh:double;
          Xp : PMatrix;
          lp,lnSubj,lRow : integer;
          lPermuteMaxZ, lPermuteMinZ,lPermuteMaxTFCEZ, lPermuteMinTFCEZ: singleP;
begin
    InnerARegressNPMAnalyze (lImages, lMaskHdr, X, lnFactors, lPredictorList, lOutname, TRUE,lMinZ,lMaxZ, lMaxNegTFCEZ, lMaxTFCEZ, TFCEconn );
    if lnFactors > 1 then goto 777;
    if (lnPermute < 1) then goto 777;
    //MainForm.NPMmsg('0 ObservedzMinMax ' +floattostr(lMinZ)+' '+floattostr(lMaxZ));
    MainForm.NPMmsg('OBSERVED Factor1 zMin zMax zMinTFCE zMaxTFCE ' +floattostr(lMinZ)+' '+floattostr(lMaxZ) +' ' +floattostr(lMaxNegTFCEZ)+' '+floattostr(lMaxTFCEZ));

    lnSubj := lImages.Count;
    DimMatrix(Xp, lnFactors, lnSubj);
    randomize;
    getmem(lPermuteMaxZ,lnPermute* sizeof(single));
    getmem(lPermuteMinZ,lnPermute* sizeof(single));
    getmem(lPermuteMaxTFCEZ,lnPermute* sizeof(single));
    getmem(lPermuteMinTFCEZ,lnPermute* sizeof(single));
    lTFCEdh := lMaxZ / 100;
    lNegTFCEdh := abs(lMinZ) / 100;
    for lp := 1 to lnPermute do begin
        //for lRow := 1 to lnSubj do
        //     Xp^[1]^[lRow] := X^[1]^[lRow];
        lMaxNegTFCEZ := lNegTFCEdh;
         lMaxTFCEZ := lTFCEdh;
     	 PermuteMatrix(X,Xp,lnSubj);

     	InnerARegressNPMAnalyze (lImages, lMaskHdr, Xp, lnFactors, lPredictorList, lOutname, FALSE,lMinZ,lMaxZ,lMaxNegTFCEZ, lMaxTFCEZ, TFCEconn);
    	MainForm.NPMmsg(inttostr(lp)+' Factor1 zMin zMax zMinTFCE zMaxTFCE ' +floattostr(lMinZ)+' '+floattostr(lMaxZ) +' ' +floattostr(lMaxNegTFCEZ)+' '+floattostr(lMaxTFCEZ));
        lPermuteMaxZ^[lp] := lMaxZ;
        lPermuteMinZ^[lp] := lMinZ;
        lPermuteMaxTFCEZ^[lp] := lMaxTFCEZ;
        lPermuteMinTFCEZ^[lp] := lMaxNegTFCEZ;
    end;
    DelMatrix(Xp, lnFactors, lnSubj);
    MainForm.reportPermute ('Permutation', lnPermute, lPermuteMaxZ, lPermuteMinZ);
    MainForm.reportPermute ('TFCEPermutation', lnPermute, lPermuteMaxTFCEZ, lPermuteMinTFCEZ);
    Freemem(lPermuteMaxZ);
    Freemem(lPermuteMinZ);
    Freemem(lPermuteMaxTFCEZ);
    Freemem(lPermuteMinTFCEZ);
    777:
    MainForm.NPMmsg('Analysis finished = ' +TimeToStr(Now));
    MainForm.MsgSave( ChangeFilePostfixExt(lOutName,'Notes','.txt'));

end;



function GetValReg (var lnSubj,lnFactors: integer; var X : PMatrix; var lImageNames:  TStrings; var lPredictorList: TStringList): boolean;
var
   lVALFilename,lTemplateName: string;
   lnRow,lnColWObs,lnCritPct,lInc,lRow,lCol: integer;
   lDesignUnspecified : boolean;
   lFileList:TStringList;
   lInRA: DoubleP0;
   lInP: Pointer;
begin
     result := false;

     lnSubj := 0;
     if not MainForm.OpenDialogExecute('Select MRIcron VAL file',false,false,'MRIcron VAL (*.val)|*.val') then begin
	   showmessage('NPM aborted: VAL file selection failed.');
	   exit;
     end; //if not selected
     lVALFilename := MainForm.OpenHdrDlg.Filename;
     MainForm.Memo1.Lines.Add( 'VAL filename: '+lVALFilename);

     lFileList := TStringList.Create;
     if not OpenValFile (lVALFilename,lTemplateName, lnRow,lnFactors,lnColWObs,lnCritPct,
            lDesignUnspecified,lPredictorList,lFileList, lInP) then
        exit;
     if lnRow > 1 then begin
        lnSubj := lnRow -1; //top row is predictor
        {$IFDEF FPC}
        lInRA := align(lInP,16);
        {$ELSE}
        lInRA := DoubleP0($fffffff0 and (integer(lInP)+15));
        //lInRA := DoubleP0((integer(lInP) and $FFFFFFF0)+16);
        {$ENDIF}
        DimMatrix(X, lnFactors, lnSubj);
        for lCol := 1 to lnFactors do begin
            for lRow := 1 to lnSubj do begin
             //MainForm.Memo1.Lines.Add(inttostr( (lRow*lnColWObsAndCovary)-4+lCol  ));
             X^[lCol]^[lRow] := lInRA^[(lRow*lnColWObs)-lnColWObs-1+lCol];
            end;
        end;
        MainForm.Memo1.Lines.Add(inttostr(lnFactors)+'  '+inttostr(lnSubj));
        for lInc := 1 to lnSubj do
            lImageNames.add(ExtractFileDirWithPathDelim(lVALFilename)+lFileList.Strings[lInc-1]);
        result := true;
     end else
         result := false;
     lFileList.free;
     Freemem(lInP);
end;


end.