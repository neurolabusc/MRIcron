unit prefs;

{$H+}
interface
uses
  inifiles, define_types,SysUtils,classes;

type
  TPrefs = record
         UnusedBool: boolean;
         Test, Permutations,CritPct: integer;
  end;
const
     knotest = 0; //no test specified
     kltest = 1;//binomial Liebermeister test
     kttest = 2; //t-test
     kbmtest = 4;//Bruneer-Mnuzel test
     klrtest = 8; //logisitic regression test

//procedure ReadIni(var lIniName: string; var lPrefs: TPrefs);
procedure SetDefaultPrefs (var lPrefs: TPrefs);
//procedure SaveIni (var lIniName: string; var lPrefs: TPrefs);
//procedure CorrectPrefs (var lPrefs: TPrefs); //ensures only usable file types are created
procedure ReadParamStr;

implementation

uses nifti_img, hdr,nifti_hdr;

procedure Msg(lStr: string);
begin
   //
end;

procedure SetDefaultPrefs (var lPrefs: TPrefs);
begin
  lPrefs.unusedbool := true;
  lPrefs.Test := knotest;
  lPrefs.Permutations := 0;
  lPrefs.CritPct := 0;
end;
function CheckBool (lPref, lFlag: integer): boolean;
//check if Flag is ni lPref. For example, if Flag is 1 then returns true for all odd lPrefs
begin
    result := (lPref and lFlag) = lFlag; 
end;

function DoLesion (lPrefs: TPrefs): boolean;
label
	666;
const
     kSimSampleSize = 64;
     knSim = 100;
     kCrit = 3;
var
        //lBinomial: boolean;
	lSim,lFact,lnFactors,lSubj,lnSubj,lnSubjAll,lMaskVoxels,lnCrit,lnControlObservations: integer;
	lPartImageNames,lImageNames,lImageNamesAll:  TStrings;
        lPredictorList: TStringList;
	lTemp4D,lMaskname,lOutName,lFactname,lOutNameSim: string;
	lMaskHdr: TMRIcroHdr;
        lMultiSymptomRA,lSymptomRA,lPartSymptomRA,lControlSymptomRA: singleP;
begin
  result := false;
  //lBinomial := not odd( (Sender as tMenuItem).tag);
  if (not CheckBool(lPrefs.test ,kltest)) and (not CheckBool(lPrefs.test, kttest))  and (not CheckBool(lPrefs.test, kbmtest)) then begin
      Msg('Error: you need to compute at least on test [options/test menu]');
      exit;
  end;
  lImageNamesAll:= TStringList.Create; //not sure why TStrings.Create does not work???
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  lPartImageNames := TStringList.Create;
  getmem(lPartSymptomRA,kSimSampleSize*sizeof(single));
  lnControlObservations := 20;
  getmem(lControlSymptomRA,lnControlObservations*sizeof(single));
  for lSim := 1 to lnControlObservations do
      lControlSymptomRA[lSim] := 5;
   //next, get 1st group
  if not MainForm.GetVal(lnSubjAll,lnFactors,lMultiSymptomRA,lImageNamesAll,lnCrit,lBinomial,lPredictorList) then begin
     showmessage('Error with VAL file');
     goto 666;
  end;
  lTemp4D := MainForm.CreateDecompressed4D(lImageNamesAll);
  if (lnSubjAll < 1) or (lnFactors < 1) then begin
     Showmessage('Not enough subjects ('+inttostr(lnSubjAll)+') or factors ('+inttostr(lnFactors)+').');
     goto 666;
  end;
  lMaskname := lImageNamesAll[0];
  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading 1st mask.');
	   goto 666;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not MainForm.CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mask file size too small.');
	   goto 666;
   end;
   lOutName := ExtractFileDirWithPathDelim(lMaskName)+'results';
   MainForm.SaveHdrDlg.Filename := loutname;
   lOutName := lOutName+'.nii.gz';
   if not MainForm.SaveHdrName ('Base Statistical Map', lOutName) then goto 666;

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
        //randomization loop....
        for lSim := 1 to knSim do begin
            RandomGroup(kSimSampleSize, lImageNames,lSymptomRA, lPartImageNames, lPartSymptomRA);
            lOutNameSim := AddIndexToFilename(lOutName,lSim);
            lnCrit := kCrit;
            MainForm.NPMMsgClear;
            //Msg(GetKVers);
            MainForm.NPMMsg('Threads: '+inttostr(gnCPUThreads));
        lFactName := lPredictorList.Strings[lFact-1];
          lFactName := MainForm.LegitFilename(lFactName,lFact);
          MainForm.NPMMsg('Factor = '+lFactname);
          For lSubj := 1 to kSimSampleSize do
            MainForm.NPMMsg (lPartImageNames.Strings[lSubj-1] + ' = '+realtostr(lPartSymptomRA^[lSubj],2) );
           MainForm.NPMMsg('Total voxels = '+inttostr(lMaskVoxels));
          MainForm.NPMMsg('Only testing voxels damaged in at least '+inttostr(lnCrit)+' individual[s]');
          MainForm.NPMMsg('Number of Lesion maps = '+inttostr(kSimSampleSize));
          if not MainForm.CheckVoxelsGroup(lPartImageNames,lMaskVoxels) then begin
             showmessage('File dimensions differ from mask.');
	     goto 666;
          end;
          if lBinomial then
             MainForm.LesionNPMAnalyzeBinomial(lPartImageNames,lMaskHdr,lnCrit,lPartSymptomRA,lFactname,lOutNameSim)
          else begin
              MainForm.ReportDescriptives(lPartSymptomRA,lnSubj);
              //LesionNPMAnalyze2(lImageNames,lMaskHdr,lnCrit,-1,lSymptomRA,lFactName,lOutname);
              LesionNPMAnalyze2(lPartImageNames,lMaskHdr,lnCrit,lSim{-1},MainForm.ReadPermute,lPartSymptomRA,lFactName,lOutNameSim,lTTest,lBM);
          end;
        end; //for each simulation...
          Freemem(lSymptomRA);
       end; //lnsubj > 1
          end; //for each factor
    if lnSubjAll > 0 then begin
       Freemem(lMultiSymptomRA);
    end;
    result := true;
    666:
    lPartImageNames.free;
    lImageNames.Free;
    lImageNamesAll.Free;
    lPredictorList.Free;
    freemem(lPartSymptomRA);
    MainForm.DeleteDecompressed4D(lTemp4D);
end;


procedure ReadParamStr;
var
   lStr: String;
   I,lError: integer;

   //lResult,lHelpShown : boolean;
   lCommandChar: Char;
   //I,lError: integer;
   lSingle: single;
   //lOrigWinWid,lOrigWinCen: Integer;*)
   lPrefs: TPrefs;
begin
     SetDefaultPrefs(lPrefs);
  lStr := paramstr(0);
  lStr := extractfilename(lStr);
  lStr := string(StrUpper(PChar(lStr))) ;
  {$IFDEF PNG}
  if (lStr = 'DCM2PNG.EXE') then
     gOutputFormat := kPNG;
  {$ENDIF}

  if (ParamCount > 0) then begin
    I := 0;
    repeat
     lStr := '';
     repeat
        inc(I);
        if I = 1 then
            lStr := ParamStr(I)
        else begin
            if lStr <> '' then
               lStr := lStr +' '+ ParamStr(I)
            else
                lStr := ParamStr(I);
        end;
        if (length(lStr)>1) and (lStr[1] = '-') and (ParamCount > I) then begin //special command
           //-z= zoom, -f= format [png,jpeg,bmp], -o= output directory
           lCommandChar := UpCase(lStr[2]);
           inc(I);
           lStr := ParamStr(I);
           lStr := string(StrUpper(PChar(lStr))) ;
           case lCommandChar of
                'C','P','T': begin //CritPct
                     Val(lStr,lSingle,lError);
                     if lError = 0 then begin
                        if lCommandChar = 'C' then
                           lPrefs.CritPct := round(lSingle)
                        else if lCOmmandChar = 'P' then
                             lPrefs.Permutations := round(lSingle)
                        else if lCOmmandChar = 'T' then
                             lPrefs.Test := round(lSingle);
                     end; //not lError
                end; //C= CritPct

           end; //case lStr[2]
           lStr := '';
        end; //special command
     until (I=ParamCount) or (fileexists(lStr)) {or (gAbort)};
     if fileexists(lStr) then begin
        //lStr :=  GetLongFileName(lStr);
        xxx
     end else if  not (gSilent) then begin
        MyWriteln('0 dcm2jpg ERROR: unable to find '+lStr);
        if lHelpShown then
          MyReadln
        else
            Showhelp;
        lHelpShown := true;
     end;
    until I >= ParamCount;
  end else begin
    //begin test routines....
    (*
      lStr := 'D:\yuv2.dcm';
        ResetDCMvalues;
        lOrigWinWid := gWinWid;
        lOrigWinCen := gWinCen;
        LoadData(lStr);
        gWinWid := lOrigWinWid;
        gWinCen := lOrigWinCen;
    //...end test routines(**)
   ShowHelp;
  end;{param count > 0}
end;

end.
 