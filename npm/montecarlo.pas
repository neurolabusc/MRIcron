unit montecarlo;
interface
{$H+}
{$DEFINE anacom}
uses
  define_types,SysUtils,
part,StatThds,statcr,StatThdsUtil,Brunner,DISTR,nifti_img, hdr,
   Messages,  Classes, Graphics, Controls, Forms, Dialogsx,
StdCtrls,  ComCtrls,ExtCtrls,Menus, overlap,ReadInt,lesion_pattern,stats,LesionStatThds,nifti_hdr,

{$IFDEF FPC} LResources,gzio2,
{$ELSE} gziod,associate,{$ENDIF}   //must be in search path, e.g. C:\pas\mricron\npm\math
{$IFNDEF UNIX} Windows, {$ENDIF}
upower,firthThds,firth,IniFiles,cpucount,userdir,math,
regmult,utypes{$IFDEF anacom} ,anacom{$ENDIF};

procedure LesionMonteCarlo (lBinomial,lTTest,lBM: boolean);

implementation

uses npmform,filename,turbolesion;

procedure RandomGroup(kSamplesPerTest: integer;lImageNames: TStrings;lSymptomRA: SingleP;var lPartImageNames: TStrings; var lPartSymptomRA: SingleP);
var
   lTotal,lInc,lRand,lSwap: integer;
   lRanOrder: longintP;
begin
    lPartImageNames.Clear;
    lTotal := lImageNames.Count;
    if kSamplesPerTest > lTotal then begin
        showmessage('Monte carlo error: population must be larger than sample size.');
        exit;
    end;
    Getmem(lRanOrder,lTotal*sizeof(longint));
    for lInc := 1 to lTotal do
        lRanOrder^[lInc] := lInc;
    for lInc := lTotal downto 2 do begin
         lRand := Random(lInc)+1;
         lSwap := lRanOrder^[lRand];
         lRanOrder^[lRand] := lRanOrder^[lInc];
         lRanOrder^[lInc] := lSwap;
     end;
     for lInc := 1 to kSamplesPerTest do begin
         lPartImageNames.Add(lImageNames.Strings[lRanOrder^[lInc]-1]);//indexed from 0
         lPartSymptomRA^[lInc] := lSymptomRA^[lRanOrder^[lInc]];
     end;
     Freemem(lRanOrder);
end;


procedure LesionMonteCarlo (lBinomial,lTTest,lBM: boolean);
label
	666;
const
     kSimSampleSize = 64;
     knSim = 2;
     kCrit = 3;
        {$IFDEF anacom}
        knControls = 64;
        {$ENDIF}
var
        lPrefs: TLDMPrefs ;
	lSim,lFact,lnFactors,lSubj,lnSubj,lnSubjAll,lMaskVoxels,lnCrit: integer;
	lPartImageNames,lImageNames,lImageNamesAll:  TStrings;
        lPredictorList: TStringList;
	lTemp4D,lMaskname,lOutName,lFactname,lOutNameSim: string;
	lMaskHdr: TMRIcroHdr;
        lMultiSymptomRA,lSymptomRA,lPartSymptomRA: singleP;
        {$IFDEF anacom}
        lnControlObservations: integer;
        lControlSymptomRA: singleP;
        {$ENDIF}
begin
  //lBinomial := not odd( (Sender as tMenuItem).tag);
          lPrefs.NULP := true{gNULP false};
          if not lBinomial then begin
             lPrefs.BMtest :=   lbm;//BMmenu.checked;
             lPrefs.Ttest := lttest;//ttestmenu.checked;
             if (not lPrefs.BMtest) and (not lPrefs.ttest) then
                lPrefs.ttest := true;
             lPrefs.Ltest:= false;
          end else begin
             lPrefs.BMtest := false;
             lPrefs.Ttest := false;
             lPrefs.Ltest:= true;
          end;
          lPrefs.nCrit := kCrit;
          lPrefs.nPermute := 0;//MainForm.ReadPermute;;
          lPrefs.Run := 0;{0 except for montecarlo}
  if (not lBinomial) and (not lTTest)  and (not lBM) then begin
      Showmessage('Error: you need to compute at least on test [options/test menu]');
      exit;
  end;
  lImageNamesAll:= TStringList.Create; //not sure why TStrings.Create does not work???
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  lPartImageNames := TStringList.Create;
  getmem(lPartSymptomRA,kSimSampleSize*sizeof(single));
  {$IFDEF anacom}
  lnControlObservations := knControls;
  getmem(lControlSymptomRA,lnControlObservations*sizeof(single));
  for lSim := 1 to lnControlObservations do
      lControlSymptomRA^[lSim] := 1000;
   {$ENDIF}
   //next, get 1st group
  if not MainForm.GetValX(lnSubjAll,lnFactors,lMultiSymptomRA,lImageNamesAll,lnCrit{,binom},lPredictorList) then begin
     showmessage('Error with VAL file');
     goto 666;
  end;
  lTemp4D := CreateDecompressed4D(lImageNamesAll);
  if (lnSubjAll < 1) or (lnFactors < 1) or (lnSubjAll < kSimSampleSize) then begin
     Showmessage('Not enough subjects ('+inttostr(lnSubjAll)+') [sample size is '+inttostr(kSimSampleSize)+']or factors ('+inttostr(lnFactors)+').');
     goto 666;
  end;
  lMaskname := lImageNamesAll[0];
  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading 1st mask.');
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
          lFactName := LegitFilename(lFactName,lFact);
          MainForm.NPMMsg('Factor = '+lFactname);
          For lSubj := 1 to kSimSampleSize do
            MainForm.NPMMsg (lPartImageNames.Strings[lSubj-1] + ' = '+realtostr(lPartSymptomRA^[lSubj],2) );
           MainForm.NPMMsg('Total voxels = '+inttostr(lMaskVoxels));
          MainForm.NPMMsg('Only testing voxels damaged in at least '+inttostr(lnCrit)+' individual[s]');
          MainForm.NPMMsg('Number of Lesion maps = '+inttostr(kSimSampleSize));
          if not CheckVoxelsGroup(lPartImageNames,lMaskVoxels) then begin
             showmessage('File dimensions differ from mask.');
	     goto 666;
          end;
          lPrefs.Run := lSim;
          if lBinomial then
              TurboLDM (lPartImageNames, lMaskHdr, lPrefs, lPartSymptomRA, lFactname,lOutNameSim)
          else begin
              MainForm.ReportDescriptives(lPartSymptomRA,lnSubj);
              TurboLDM (lPartImageNames, lMaskHdr, lPrefs, lPartSymptomRA, lFactname,lOutNameSim);
              {$IFDEF anacom}
              AnacomLesionNPMAnalyze (lPartImageNames, lMaskHdr, lnCrit,lSim,lnControlObservations, lPartSymptomRA,lControlSymptomRA, lFactname,lOutNameSim,true,false);
              {$ENDIF}
          end;
        end; //for each simulation...
          Freemem(lSymptomRA);
       end; //lnsubj > 1
          end; //for each factor
    if lnSubjAll > 0 then begin
       Freemem(lMultiSymptomRA);
    end;
    666:
    lPartImageNames.free;
    lImageNames.Free;
    lImageNamesAll.Free;
    lPredictorList.Free;
    freemem(lPartSymptomRA);
    {$IFDEF anacom}
    freemem(lControlSymptomRA);
    {$ENDIF}
    DeleteDecompressed4D(lTemp4D);
end;



end.


