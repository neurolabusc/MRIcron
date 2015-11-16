unit npmform;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface
{$I options.inc}
uses
  define_types,SysUtils,
part,StatThds,statcr,StatThdsUtil,Brunner,DISTR,nifti_img,
   Messages,  Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, ExtCtrls, StdCtrls,
overlap,ReadInt,lesion_pattern,stats,LesionStatThds,nifti_hdr,tfce_clustering,

{$IFDEF FPC} LResources,gzio2,
{$ELSE} gziod,associate,{$ENDIF}   //must be in search path, e.g. C:\pas\mricron\npm\math
{$IFNDEF UNIX} Windows,
  {$ELSE}
  LCLType,
  {$ENDIF}
upower,firthThds,firth,IniFiles,cpucount,userdir,math,
regmult,utypes,turbolesion
{$IFDEF compileANACOM}, anacom{$ENDIF}

{$IFDEF benchmark}, montecarlo{$ENDIF}
;
//regmultdelphi,matrices;
type

  { TMainForm }

  TMainForm = class(TForm)
    Binaryimagescontinuousgroupsfast1: TMenuItem;
    Memo1: TMemo;

    Design1: TMenuItem;
    FCE1: TMenuItem;
    MultipleRegress: TMenuItem;
    SaveText1: TMenuItem;
    ROIanalysis1: TMenuItem;
    OpenHdrDlg: TOpenDialog;
    SaveHdrDlg: TSaveDialog;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    MainMenu1: TMainMenu;
    About1: TMenuItem;
    AssociatevalfileswithNPM1: TMenuItem;
    Balance1: TMenuItem;
    BinomialAnalysislesions1: TMenuItem;
    BMmenu: TMenuItem;
    ContinuousanalysisVBM1: TMenuItem;
    Copy1: TMenuItem;
    Countlesionoverlaps1: TMenuItem;
    Edit1: TMenuItem;
    Exit1: TMenuItem;
    File1: TMenuItem;
    Help1: TMenuItem;
    IntensitynormalizationA1: TMenuItem;
    Makemeanimage1: TMenuItem;
    Makemeanimage2: TMenuItem;
    N0: TMenuItem;
    N1000: TMenuItem;
    N2000: TMenuItem;
    N3000: TMenuItem;
    N4000: TMenuItem;
    niiniigz1: TMenuItem;
    Options1: TMenuItem;
    PairedTMenu: TMenuItem;
    PenalizedLogisticRegerssion1: TMenuItem;
    Permutations1: TMenuItem;
    PhysiologicalArtifactCorrection1: TMenuItem;
    SingleRegress: TMenuItem;
    SingleSubjectZScores1: TMenuItem;
    T1: TMenuItem;
    T15: TMenuItem;
    T16: TMenuItem;
    T2: TMenuItem;
    T3: TMenuItem;
    T4: TMenuItem;
    T7: TMenuItem;
    T8: TMenuItem;
    Tests1: TMenuItem;
    Threads1: TMenuItem;
    ttestmenu: TMenuItem;
    Utilities1: TMenuItem;
    Variance1: TMenuItem;
    VBM1: TMenuItem;
    VLSM1: TMenuItem;
    ComputeIntersectionandUnion1: TMenuItem;
    Intensitynormalization1: TMenuItem;
    Masked1: TMenuItem;
    MaskedintensitynormalizationA1: TMenuItem;
    MaskedintensitynormalizationB1: TMenuItem;
    //Binaryimagescontinuousgroupsfast1: TMenuItem;
    Binarizeimages1: TMenuItem;
    Resliceimagetoneworientationandboundingbox1: TMenuItem;
    Setnonseroto1001: TMenuItem;
    AnaCOMmenu: TMenuItem;
    MonteCarloSimulation1: TMenuItem;
    Subtract1: TMenuItem;
    LogPtoZ1: TMenuItem;
    //FCE1: TMenuItem;
    function GetKVers: string;
    function GetValX (var lnSubj, lnFactors: integer; var lSymptomRA: singleP; var lImageNames:  TStrings; var lCrit: integer; {lBinomial : boolean;} var lPredictorList: TStringList):boolean;
    function ThreshMap(lThresh: single; lVolVox: integer;lOutImg: singleP): integer;
    function FirthNPMAnalyze (var lImages: TStrings; var lPredictorList: TStringList; var lMaskHdr: TMRIcroHdr; lnCond,lnCrit: integer; var lSymptomRA: SingleP; var lOutName: string): boolean;
    procedure InitPermute (lnSubj, lnPermute: integer; var lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP; var  lRanOrderp: pointer; var lRanOrder: Doublep0);
    function reportPermute (lLabel:string; lnPermute: integer; var lPermuteMaxZ, lPermuteMinZ: singleP): double;
    procedure FreePermute (lnPermute: integer; var lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;var  lRanOrderp: pointer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    function SaveHdrName (lCaption: string; var lFilename: string): boolean;
    procedure StrToMemo (lStr: string);
    procedure NPMclick(Sender: TObject);
    function OpenDialogExecute (lCaption: string;lAllowMultiSelect,lForceMultiSelect: boolean; lFilter: string): boolean;//; lAllowMultiSelect: boolean): boolean;
    function NPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lMaskVoxels,lnGroup1: integer): boolean;
    function NPMAnalyzePaired (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lMaskVoxels: integer): boolean;
    function NPMzscore (var lImages: TStrings; var lMnHdr,lStDevHdr: TMRIcroHdr): boolean;
    procedure FormCreate(Sender: TObject);
    function ReportDescriptives (var RA: SingleP; n: integer): boolean;
    function MakeSubtract (lPosName,lNegName: string): boolean;
    function MakeMean (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lBinarize,lVariance: boolean): boolean;
    function Balance (var lImageName,lMaskName: String; lMethod: integer{lInflection: boolean}): boolean;
    procedure LesionBtnClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure testmenuclick(Sender: TObject);
    procedure radiomenuclick(Sender: TObject);
    procedure ReadIniFile;
    procedure WriteIniFile;
    function reportBonferroni(lLabel: string; lnTests: integer): double;
    function reportFDR (lLabel:string; lnVox, lnTests: integer; var lData: SingleP): double;
    procedure Makemeanimage1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Balance1Click(Sender: TObject);
    procedure niiniigz1Click(Sender: TObject);
    procedure Variance1Click(Sender: TObject);
    procedure ZtoP1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Design1Click(Sender: TObject);
    procedure PhysiologicalArtifactCorrection1Click(Sender: TObject);
    procedure DualImageCorrelation1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PairedTMenuClick(Sender: TObject);
    procedure SingleSubjectZScores1Click(Sender: TObject);
    procedure MultipleRegressClick(Sender: TObject);
    function ReadPermute: integer;
    procedure NPMmsg( lStr: string);
    procedure NPMmsgClear;
    procedure MsgSave(lFilename: string);
    procedure SingleRegressClick(Sender: TObject);
    procedure AssociatevalfileswithNPM1Click(Sender: TObject);
    procedure threadChange(Sender: TObject);
    procedure Countlesionoverlaps1Click(Sender: TObject);
    procedure PenalizedLogisticRegerssion1Click(Sender: TObject);
    procedure ComputeIntersectionandUnion1Click(Sender: TObject);
    procedure ROCbinomialdeficit1Click(Sender: TObject);
    procedure ROCcontinuousdeficit1Click(Sender: TObject);
    procedure ThreadDone(Sender: TObject);
    procedure NPMmsgAppend( lStr: string);
    procedure ROIanalysis1Click(Sender: TObject);
    procedure Masked1Click(Sender: TObject);
    procedure Binarizeimages1Click(Sender: TObject);
    procedure Resliceimagetoneworientationandboundingbox1Click(
      Sender: TObject);
    procedure Setnonseroto1001Click(Sender: TObject);
    procedure Savetext1Click(Sender: TObject);
    procedure AnaCOMmenuClick(Sender: TObject);
    procedure MonteCarloSimulation1Click(Sender: TObject);
    procedure Subtract1Click(Sender: TObject);
    procedure LogPtoZ1Click(Sender: TObject);
    procedure FCE1Click(Sender: TObject);
  private
  { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
implementation

uses filename,prefs,roc,hdr,regression,valformat {$IFDEF SPREADSHEET}  ,design,spread{$ENDIF}
{$IFNDEF UNIX},ActiveX {$ENDIF};
{$IFNDEF FPC}
{$R *.DFM}

  {$ENDIF}
const
        kVers : string = 'Chris Rorden''s NPM  :: '+kMRIcronVers;
var
gNULP: boolean = true;
gROI : boolean = false;
gTFCE: integer;
function TMainForm.GetKVers: string;
begin
     result := kVers +'; Threads used = '+inttostr(gnCPUThreads );
end;


procedure TMainForm.NPMmsgAppend( lStr: string);
var
  lOutname: string;
  f: TextFile;
begin
  MainForm.Memo1.Lines.add(lStr);
  lOutname:='c:\dx.txt';
  if fileexists(lOutname) then begin                    { open a text file }
    AssignFile(f, lOutname);
    Append(f);
    Writeln(f, lStr);
    Flush(f);  { ensures that the text was actually written to file }
    { insert code here that would require a Flush before closing the file }
    CloseFile(f);
  end;
end;

procedure TMainForm.NPMmsg( lStr: string);
begin
    MainForm.Memo1.Lines.add(lStr);
end;

procedure Msg(lStr: string);
begin
     MainForm.NPMmsg(lStr);
end;

procedure MsgClear;
begin
    MainForm.Memo1.Lines.Clear;
end;

procedure TMainForm.NPMmsgClear;
begin
    MsgClear;
end;


procedure TMainForm.MsgSave(lFilename: string);
var
       i: integer;
       f: textfile;
begin
  if (Memo1.Lines.Count < 1) then exit;
  if fileexists(lFilename) then begin
     AssignFile(f, lFilename);
     {$I-}
     append(f);
     {$I+}
     if IOResult= 0 then
        for i:= 0 to Memo1.Lines.Count- 1 do
            WriteLn(f, Memo1.Lines[i]);
     CloseFile(f);
  end else
      MainForm.Memo1.Lines.SaveToFile(lFilename);
end;

procedure TMainForm.ThreadDone(Sender: TObject);
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

procedure TMainForm.InitPermute (lnSubj, lnPermute: integer; var lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP; var  lRanOrderp: pointer; var lRanOrder: Doublep0);
begin
     if (lnPermute < 2) then
        exit;
     InitRA(lnPermute,lPermuteMaxT);
     InitRA(lnPermute,lPermuteMinT);
     InitRA(lnPermute,lPermuteMaxBM);
     InitRA(lnPermute,lPermuteMinBM);
     createArray64(lRanOrderp,lRanOrder,lnSubj);
end; //init permute

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

function TMainForm.reportBonferroni(lLabel: string; lnTests: integer): double; //returns 5% Z score
begin
     if lnTests < 1 then exit;
     result := pNormalInv(0.05/lnTests);
     msg(inttostr(lnTests)+' test '+lLabel+' Bonferroni FWE Z '+
       '0.050='+realtostr(result,3)+
       ', 0.025='+realtostr(pNormalInv(0.025/lnTests),3)+
       ', 0.01='+realtostr(pNormalInv(0.01/lnTests),3));
end;

function TMainForm.reportFDR (lLabel:string; lnVox, lnTests: integer; var lData: SingleP): double;
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
    msg(lLabel+' Range '
       +realtostr(lMin,3)+
       '...'+realtostr(lMax,3));
    {Msg(lLabel+' Range '
       +realtostr(pNormalInv(lPs[lnTests]),3)+
       '...'+realtostr(pNormalInv(lPs[1]),3)+
       ' '); } //we could use this and save time computing lmin/lmax, but loss in precision
    msg(lLabel+' +FDR Z '+
       '0.050='+realtostr(pNormalInv(lFDR05p),8)+
       ', 0.01='+realtostr(pNormalInv(lFDR01p),8)+
       ' ');
    msg(lLabel+' -FDR Z '+
       '0.050='+realtostr(pNormalInv(1-lFDR05r),8)+
       ', 0.01='+realtostr(pNormalInv(1-lFDR01r),8)+
       ' ');
    result := pNormalInv(lFDR01p);
end;

function ReportThresh (lLabel: string; lnPermute: integer; var lRankedData: singleP;lTop:boolean): double;
begin
     result := lRankedData^[IndexPct(lnPermute,0.050,lTop)];
     msg(lLabel+': permutationFWE '+
       //'0.500='+realtostr(lRankedData[IndexPct(lnPermute,0.500,lTop)],3)+
       ', 0.050='+realtostr({lRankedData^[IndexPct(lnPermute,0.050,lTop)]} result,8)+
       ', 0.025='+realtostr(lRankedData^[IndexPct(lnPermute,0.025,lTop)],8)+
       ', 0.01='+realtostr(lRankedData^[IndexPct(lnPermute,0.010,lTop)],8)+
       ' ');
end;

function TMainForm.reportPermute (lLabel:string; lnPermute: integer; var lPermuteMaxZ, lPermuteMinZ: singleP): double;
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

procedure TMainForm.FreePermute (lnPermute: integer; var lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;var  lRanOrderp: pointer);
begin
     if (lnPermute < 2) then
        exit;
    Freemem(lRanOrderp);
    Freemem(lPermuteMaxT);
    Freemem(lPermuteMinT);
    Freemem(lPermuteMaxBM);
    Freemem(lPermuteMinBM);
end;

function TMainForm.SaveHdrName (lCaption: string; var lFilename: string): boolean;
begin
	 result := false;
	 SaveHdrDlg.InitialDir := lFilename;
	 SaveHdrDlg.Title := lCaption;
	 SaveHdrDlg.Filter := kAnaHdrFilter;
	 if not SaveHdrDlg.Execute then exit;
	 lFilename := SaveHdrDlg.Filename;
	 result := true;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     //showmessage('zz');
        WriteIniFile;
end;

procedure MakeStatHdr (var lBGHdr,lStatHdr: TniftiHdr; lMinIntensity,lMaxIntensity,lIntent_p1,lIntent_p2,lIntent_p3: single; lIntent_code: smallint;lIntentName: string);
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
end;

procedure WriteThread( lnThread: integer);
begin
    case lnThread of
         2: MainForm.T2.checked := true;
         3: MainForm.T3.checked := true;
         4: MainForm.T4.checked := true;
         7: MainForm.T7.checked := true;
         8: MainForm.T8.checked := true;
         15: MainForm.T15.checked := true;
         16: MainForm.T16.checked := true;
         else MainForm.T1.checked := true;
    end;
    gnCPUThreads := lnThread;
end;

function ReadThread: integer;
begin
    if MainForm.T16.checked then result := 16
    else if MainForm.T15.checked then result := 15
    else if MainForm.T8.checked then result := 8
    else if MainForm.T7.checked then result := 7
    else if MainForm.T4.checked then result := 4
    else if MainForm.T3.checked then result := 3
    else if MainForm.T2.checked then result := 2
    else result := 1;
    gnCPUThreads := result;
end;


procedure WritePermute( lnPermute: integer);
begin
    case lnPermute of
         4000: MainForm.N4000.checked := true;
         3000: MainForm.N3000.checked := true;
         2000: MainForm.N2000.checked := true;
         1000: MainForm.N1000.checked := true;
         else MainForm.N0.checked := true;
    end;
end;

function TMainForm.ReadPermute: integer;
begin
    if MainForm.N4000.checked then result := 4000
    else if MainForm.N3000.checked then result := 3000
    else if MainForm.N2000.checked then result := 2000
    else if MainForm.N1000.checked then result := 1000
    else result := 0;
end;

(*function LoadImgX(lInName: string; lImgData: SingleP; lStart, lEnd,linvox_offset,lRApos,lDataType,lVolVox: integer): boolean;
var
   lInc: integer;
begin
     //LoadImgX(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox);
     for lInc := 1 to ((lEnd{+1})-lStart) do
                lImgData^[lRApos+lInc-1] := 123;
     msg(inttostr(lRApos+1-1)+'  '+inttostr(lRApos+((lEnd+1)-lStart)-1)  );
end;*)

function TMainForm.NPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lMaskVoxels,lnGroup1: integer): boolean;

label
	667;
var
	lOutName,lOutNameMod: string;
	lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lDummy: SingleP;
        lTotalMemory: double; //not integer - limit for 32bit int is 2Gb
	lPlank,lVolVox,lPos,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lThreadStart,lThreadEnd,lThreadInc: integer;
	lT,  lSum, lMn: double;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lThread,lnPermute: integer;
        lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
        lttest,lBM: boolean;
begin

     result := false;
     lttest:= ttestmenu.checked;
     lBM := BMmenu.checked;
        lnPermute := ReadPermute;
        //lnPermute := 100;
	msg('Permutations = ' +IntToStr(lnPermute));
	lOutName := lMaskHdr.ImgFileName;
	if not SaveHdrName ('Statistical Map', lOutName) then exit;
	msg('Analysis began = ' +TimeToStr(Now));
	lTotalMemory := 0;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
	//load mask
	getmem(lMaskImg,lVolVox*sizeof(single));
	if not LoadImg(lMaskHdr.ImgFileName, lMaskImg, 1, lVolVox,round(gOffsetRA[0]),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		msg('Unable to load mask ' +lMaskHdr.ImgFileName);
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
		msg('Mask appears empty' +lMaskHdr.ImgFileName);
		goto 667;
	end;
	msg('Mask has voxels from '+inttostr(lMinMask)+'..'+inttostr(lMaxMask));
	lVoxPerPlank :=  kPlankSz div lImages.Count div sizeof(single) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
	msg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	msg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
	getmem(lPlankImg,kPlankSz);
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
        for lPos := 1 to lImages.Count do
		if gScaleRA[lPos] = 0 then
			gScaleRA[lPos] := 1;

        getmem(lOutImgMn,lVolVox* sizeof(single));
	getmem(lOutImgBM,lVolVox* sizeof(single));
	getmem(lOutImgT,lVolVox* sizeof(single));
        InitPermute (lImages.Count, lnPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do begin
                lOutImgMn^[lPos] := 0;
		lOutImgBM^[lPos] := 0;
		lOutImgT^[lPos] := 0;
	end;
        ClearThreadData(gnCPUThreads,lnPermute);

	for lPlank := 1 to lnPlanks do begin
		msg('Computing plank = ' +Inttostr(lPlank));
    Refresh;
    Application.processmessages;
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
                //showmessage('stop');
                //threading start
                lThreadStart := 1;
                lThreadInc := lVoxPerPlank  div gnCPUThreads;
                lThreadEnd := lThreadInc;
                Application.processmessages;
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
                //threading end
		lStartVox := lEndVox + 1;
	end;
        lnVoxTested := SumThreadData(gnCPUThreads,lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM);
        //next report findings
	msg('Voxels tested = ' +Inttostr(lnVoxTested));
        reportBonferroni('Std',lnVoxTested);
        //next: save data
(*savedata*) 
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save mean
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Mean','.hdr');
 if lnVoxTested > 1 then

        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgMn,1);
        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );

if (lttest) and (lnVoxTestED > 1 ) then begin //save Ttest
        //reportRange ('ttest', lVolVox, lnVoxTested, lOutImgT);
        //next: convert t-scores to z scores
        for lPos := 1 to lVolVox do
            lOutImgT^[lPos] := TtoZ (lOutImgT^[lPos],lImages.Count-2);
        for lPos := 1 to lnPermute do begin
            lPermuteMaxT^[lPos] := TtoZ (lPermuteMaxT^[lPos],lImages.Count-2);
            lPermuteMinT^[lPos] := TtoZ (lPermuteMinT^[lPos],lImages.Count-2);
        end;

        reportFDR ('ttest', lVolVox, lnVoxTested, lOutImgT);
        reportPermute('ttest',lnPermute,lPermuteMaxT, lPermuteMinT);
	lOutNameMod := ChangeFilePostfixExt(lOutName,'ttest','.hdr');
	NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgT,1);
end;
if (lBM) and (lnVoxTested > 1 ) then begin //save Brunner Munzel
       reportFDR ('BM', lVolVox, lnVoxTested, lOutImgBM);
        reportPermute('BM',lnPermute,lPermuteMaxBM, lPermuteMinBM);
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
        FreePermute (lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,  lRanOrderp);
	freemem(lOutImgT);
	freemem(lOutImgBM);
        freemem(lOutImgMn);
	//freemem(lObsp);
	freemem(lMaskImg);
	freemem(lPlankImg);
	msg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes','.txt');
        MsgSave(lOutNameMod);
        ProgressBar1.Position := 0;
        result := true;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lVolVox > 1 then freemem(lMaskImg);
	if lTotalMemory > 1 then freemem(lPlankImg);
	msg('Unable to complete analysis.');
        ProgressBar1.Position := 0;
end;

function TMainForm.NPMAnalyzePaired (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lMaskVoxels: integer): boolean;
label
	667;
var
	lOutName,lOutNameMod: string;
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
	msg('Permutations = ' +IntToStr(lnPermute));
	lOutName := lMaskHdr.ImgFileName;
	if not SaveHdrName ('Statistical Map', lOutName) then exit;
	msg('Analysis began = ' +TimeToStr(Now));
	lTotalMemory := 0;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
	//load mask
	getmem(lMaskImg,lVolVox*sizeof(single));
	if not LoadImg(lMaskHdr.ImgFileName, lMaskImg, 1, lVolVox,round(gOffsetRA[0]),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		msg('Unable to load mask ' +lMaskHdr.ImgFileName);
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
		Msg('Mask appears empty' +lMaskHdr.ImgFileName);
		goto 667;
	end;
	Msg('Mask has voxels from '+inttostr(lMinMask)+'..'+inttostr(lMaxMask));
	lVoxPerPlank :=  kPlankSz div lImages.Count div sizeof(single) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
	Msg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	Msg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
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
		Msg('Computing plank = ' +Inttostr(lPlank));
                Refresh;
                Application.processmessages;
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
                //threading end
		lStartVox := lEndVox + 1;
	end;
        lnVoxTested := SumThreadDataLite(gnCPUThreads);//not yet SumThreadData(gnCPUThreads,lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM);
        //next report findings
	Msg('Voxels tested = ' +Inttostr(lnVoxTested));
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
	Msg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes','.txt');
        MsgSave(lOutNameMod);
        ProgressBar1.Position := 0;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lVolVox > 1 then freemem(lMaskImg);
	if lTotalMemory > 1 then freemem(lPlankImg);
	Msg('Unable to complete analysis.');
        ProgressBar1.Position := 0;
end;



function TMainForm.OpenDialogExecute (lCaption: string;lAllowMultiSelect,lForceMultiSelect: boolean; lFilter: string): boolean;//; lAllowMultiSelect: boolean): boolean;
var
   lNumberofFiles: integer;
begin
	OpenHdrDlg.Filter := lFilter;//kAnaHdrFilter;//lFilter;
	OpenHdrDlg.FilterIndex := 1;
	OpenHdrDlg.Title := lCaption;
	if lAllowMultiSelect then
		OpenHdrDlg.Options := [ofAllowMultiSelect,ofFileMustExist]
	else
		OpenHdrDlg.Options := [ofFileMustExist];
	result := OpenHdrDlg.Execute;
	if not result then exit;
	if lForceMultiSelect then begin
		lNumberofFiles:= OpenHdrDlg.Files.Count;
		if  lNumberofFiles < 2 then begin
			Showmessage('Error: This function is designed to overlay MULTIPLE images. You selected less than two images.');
			result := false;
		end;
	end;
end;



procedure TMainForm.NPMclick(Sender: TObject);
label
	666;
var
	lnGroup1,lMaskVoxels: integer;
	lG:  TStrings;
	lMaskname: string;
	lMaskHdr: TMRIcroHdr;
begin
  if (not ttestmenu.checked)  and (not BMmenu.checked) then begin
      Showmessage('Error: you need to compute at least on test [options/test menu]');
      exit;
  end;
	MsgClear;
        Msg(GetKVers);
        Msg('Threads: '+inttostr(gnCPUThreads));
   if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
	   showmessage('NPM aborted: mask selection failed.');
	   exit;
   end; //if not selected
   lMaskname := OpenHdrDlg.Filename;
   if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading mask.');
	   exit;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mask file size too small.');
	   exit;
   end;
   Msg('Mask name = '+ lMaskname);
   Msg('Total voxels = '+inttostr(lMaskVoxels));
   //next, get 1st group
   if not OpenDialogExecute('Select postive group (Z scores positive if this group is brighter)',true,true,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
   end; //if not selected
   lG:= TStringList.Create; //not sure why TStrings.Create does not work???
   lG.addstrings(OpenHdrDlg.Files);
   lnGroup1 :=OpenHdrDlg.Files.Count;
   Msg('Scans in Group 1 = '+inttostr(lnGroup1));
   //next, get 2nd group
   if not OpenDialogExecute('Select negative group (Z scores negative if this group is brighter)',true,true,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   goto 666;
   end; //if not selected
   lG.addstrings(OpenHdrDlg.Files);
   if not CheckVoxelsGroupX(lG,lMaskHdr {lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;
   Msg('Scans in Group 2 = '+inttostr(lG.count-lnGroup1));
     NPMAnalyze(lG,lMaskHdr,lMaskVoxels,lnGroup1);
   666:
   lG.Free;
end;

function TMainForm.ThreshMap(lThresh: single; lVolVox: integer;lOutImg: singleP): integer;
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

{x$DEFINE NOTmedianfx}
(*function TMainForm.LesionNPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lnCrit,lRun: integer; var lSymptomRA: SingleP;var lFactname,lOutName: string): boolean;
label
	123,667;
var
	lOutNameMod: string;
	lPlankImg: byteP;
        lOutImgSum,lOutImgBM,lOutImgT,
        lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;
        lPos,lPlank,lThread: integer;
	lVolVox,lMinMask,lMaxMask,lTotalMemory,lnPlanks,lVoxPerPlank,
        lThreadStart,lThreadEnd,lThreadInc,lnLesion,lnPermute,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lPosPct: int64;
	lT,lBMz,  lSum,lThresh :double;
	lObsp: pointer;
	lObs: Doublep0;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
        lttest,lBM: boolean;
        {$IFDEF medianfx}
        lmedianFX,lmeanFX,lsummean,lsummedian: double;
        lmediancount: integer;
        {$ENDIF}
begin
        lttest:= ttestmenu.checked;
        lBM := BMmenu.checked;
        lnPermute := ReadPermute;
        Msg('Permutations = ' +IntToStr(lnPermute));
	Msg('Analysis began = ' +TimeToStr(Now));
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
	Msg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	Msg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
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
	createArray64(lObsp,lObs,lImages.Count);
        getmem(lOutImgSum,lVolVox* sizeof(single));
	getmem(lOutImgBM,lVolVox* sizeof(single));
	getmem(lOutImgT,lVolVox* sizeof(single));
        InitPermute (lImages.Count, lnPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do begin
                lOutImgSum^[lPos] := 0;
		lOutImgBM^[lPos] := 0;
		lOutImgT^[lPos] := 0;
	end;
        //next create permuted BM bounds
        if lBM then begin
           Msg('Generating BM permutation thresholds');
           Refresh;
           for lPos := 1 to lImages.Count do
               lObs^[lPos-1] := lSymptomRA^[lPos];
           genBMsim (lImages.Count, lObs);
        end;
         ClearThreadData(gnCPUThreads,lnPermute) ;
	for lPlank := 1 to lnPlanks do begin
		Msg('Computing plank = ' +Inttostr(lPlank));
        Refresh;
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
                    with TLesionContinuous.Create (ProgressBar1,lttest,lBM,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,0,lPlankImg,lOutImgSum,lOutImgBM,lOutImgT,nil,lSymptomRA) do
                         {$IFDEF FPC} OnTerminate := @ThreadDone; {$ELSE}OnTerminate := ThreadDone;{$ENDIF}
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
        lnVoxTested := SumThreadData(gnCPUThreads,lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM);
        //next report findings
        if lnVoxTested < 1 then begin
	   Msg('**Error: no voxels tested: no regions lesioned in at least '+inttostr(lnCrit)+' patients**');
           goto 123;
        end;

	Msg('Voxels tested = ' +Inttostr(lnVoxTested));
        {$IFDEF medianfx}
	Msg('Average MEAN effect size = ' +realtostr((lsummean/lmediancount),3));
        Msg('Average MEDIAN effect size = ' +realtostr((lsummedian/lmediancount),3));
        {$ENDIF}
        Msg('Only tested voxels with more than '+inttostr(lnCrit)+' lesions');
        //Next: save results from permutation thresholding....
        reportBonferroni('Std',lnVoxTested);
        //next: save data
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save sum map
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Sum'+lFactName,'.hdr');
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
//create new header - subsequent images will use Z-scores
        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
        if Sum2PowerCont(lOutImgSum,lVolVox,lImages.Count) then begin
           lOutNameMod := ChangeFilePostfixExt(lOutName,'Power'+lFactName,'.hdr');
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
        end;

        //MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
if lttest then begin //save Ttest
        //next: convert t-scores to z scores
        for lPos := 1 to lVolVox do
            lOutImgT^[lPos] := TtoZ (lOutImgT^[lPos],lImages.Count-2);
        for lPos := 1 to lnPermute do begin
            lPermuteMaxT^[lPos] := TtoZ (lPermuteMaxT^[lPos],lImages.Count-2);
            lPermuteMinT^[lPos] := TtoZ (lPermuteMinT^[lPos],lImages.Count-2);
        end;
        lThresh := reportFDR ('ttest', lVolVox, lnVoxTested, lOutImgT);
        reportPermute('ttest',lnPermute,lPermuteMaxT, lPermuteMinT);
	lOutNameMod := ChangeFilePostfixExt(lOutName,'ttest'+lFactName,'.hdr');
        if lRun > 0 then
           Msg('threshtt,'+inttostr(lRun)+','+inttostr(ThreshMap(lThresh,lVolVox,lOutImgT))+','+realtostr(lThresh,3));
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgT,1);

end;
if lBM then begin //save Mann Whitney
        lThresh :=  reportFDR ('BM', lVolVox, lnVoxTested, lOutImgBM);
        reportPermute('BM',lnPermute,lPermuteMaxBM, lPermuteMinBM);
        lOutNameMod := ChangeFilePostfixExt(lOutName,'BM'+lFactName,'.hdr');
        if lRun > 0 then
           Msg('threshbm,'+inttostr(lRun)+','+inttostr(ThreshMap(lThresh,lVolVox,lOutImgBM))+','+realtostr(lThresh,3));
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgBM,1);

end;
//next: free dynamic memory
123:
        FreePermute (lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,  lRanOrderp);
	freemem(lOutImgT);
	freemem(lOutImgBM);
        freemem(lOutImgSum);
	freemem(lObsp);
	freemem(lPlankImg);
	Msg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes'+lFactName,'.txt');
        MsgSave(lOutNameMod);
        ProgressBar1.Position := 0;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lTotalMemory > 1 then freemem(lPlankImg);
	Msg('Unable to complete analysis.');
        ProgressBar1.Position := 0;
end; //LesionNPMAnalyze    *)


(*function TMainForm.LesionNPMAnalyzeBinomial (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lnCrit: integer; var lSymptomRA: SingleP; var lFactname,lOutName: string): boolean;
label
     123,667;
var
   lVal: single;
	lOutNameMod: string;
	lPlankImg: byteP;
        lOutImgSum,lOutImgL,lDummyImg,
        lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP;
        lPos,lPlank,lThread,lnDeficit: integer;
        lTotalMemory,lVolVox,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
        lThreadStart,lThreadInc,lThreadEnd, lnLesion,lnPermute,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lPosPct: int64;
	lT,  lSum: double;
	lObsp: pointer;
	lObs: Doublep0;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
begin
        lnPermute := ReadPermute;
        Msg('Permutations = ' +IntToStr(lnPermute));
	//lOutName := lMaskHdr.ImgFileName;
	//if not SaveHdrName ('Statistical Map', lOutName) then exit;
	Msg('Analysis began = ' +TimeToStr(Now));
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
	Msg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	Msg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
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
        InitPermute (lImages.Count, lnPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do begin
                lOutImgSum^[lPos] := 0;
		lOutImgL^[lPos] := 0;
	end;
        ClearThreadDataPvals(gnCPUThreads,lnPermute) ;
	for lPlank := 1 to lnPlanks do begin
        ProgressBar1.Position := 1;
		Msg('Computing plank = ' +Inttostr(lPlank));
                Refresh;
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
                    with TLesionBinom.Create (ProgressBar1,false,true,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,0,lPlankImg,lOutImgSum,lOutImgL,lDummyImg,nil,lSymptomRA) do
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
	   Msg('**Error: no voxels tested: no regions lesioned in at least '+inttostr(lnCrit)+' patients**');
           goto 123;
        end;
        //next report findings
	Msg('Voxels tested = ' +Inttostr(lnVoxTested));
        Msg('Only tested voxels with more than '+inttostr(lnCrit)+' lesions');
        //Next: save results from permutation thresholding....
        reportBonferroni('Std',lnVoxTested);
        //next: save data
//savedata
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save sum map
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Sum'+lFactName,'.hdr');
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
//future images will store Z-scores...
        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
//save power map
        lnDeficit := 0;
        for lPos := 1 to lImages.Count do
            if lSymptomRA^[lPos] = 0 then
               inc(lnDeficit);
        if Sum2Power(lOutImgSum,lVolVox,lImages.Count,lnDeficit) then begin
           lOutNameMod := ChangeFilePostfixExt(lOutName,'Power'+lFactName,'.hdr');
           NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);
        end;
        //        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
        //save Liebermeister

        lOutNameMod := ChangeFilePostfixExt(lOutName,'L'+lFactName,'.hdr');
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgL,1);
        //save end
       reportFDR ('L', lVolVox, lnVoxTested, lOutImgL);
        reportPermute('L',lnPermute,lPermuteMaxT, lPermuteMinT);

123:
//next: free dynamic memory
        FreePermute (lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,  lRanOrderp);
	freemem(lOutImgL);
        freemem(lOutImgSum);
	freemem(lObsp);
	freemem(lPlankImg);
	Msg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes'+lFactName,'.txt');
        MsgSave(lOutNameMod);

        ProgressBar1.Position := 0;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lTotalMemory > 1 then freemem(lPlankImg);
	Msg('Unable to complete analysis.');
        ProgressBar1.Position := 0;
end;    *)


function TMainForm.GetValX (var lnSubj, lnFactors: integer; var lSymptomRA: singleP; var lImageNames:  TStrings; var lCrit: integer; {lBinomial : boolean;} var lPredictorList: TStringList):boolean;
//warning: you MUST free lPredictorList
var
   lVALFilename {,lTemplateName}: string;
   lCritPct: integer;
begin
     lPredictorList := TStringList.Create;
     result := false;
     lnSubj := 0;
     if not MainForm.OpenDialogExecute('Select MRIcron VAL file',false,false,'MRIcron VAL (*.val)|*.val') then begin
	   showmessage('NPM aborted: VAL file selection failed.');
	   exit;
     end; //if not selected
     lVALFilename := MainForm.OpenHdrDlg.Filename;
     result := GetValCore ( lVALFilename, lnSubj, lnFactors, lSymptomRA, lImageNames, lCrit,lCritPct{,binom},lPredictorList);
end;


function TMainForm.ReportDescriptives (var RA: SingleP; n: integer): boolean;
var lMn,lSD,lSE,lSkew,lZSkew: double;
begin
     SuperDescriptive (RA, n, lMn,lSD,lSE,lSkew,lZSkew);
     Msg('mean='+floattostr(lMn)+',StDev='+floattostr(lSD)+',StEr='+floattostr(lSE)+',Skew='+floattostr(lSkew)+',ZSkew='+floattostr(lZSkew));
end;

(*function noVariance (lRA: singlep; lnSubj: integer): boolean;
var
   lI : integer;
begin
     result := false;
     if lnSubj < 2 then exit;
     for lI := 2 to lnSubj do
         if lRA^[1] <> lRA^[lI] then
            exit;
     result := true;
end;    *)

(*procedure TMainForm.LesionBtnClick(Sender: TObject);
label
	666;
var
        lBinomial: boolean;
	lFact,lnFactors,lSubj,lnSubj,lnSubjAll,lMaskVoxels,lnCrit: integer;
	lImageNames,lImageNamesAll:  TStrings;
        lPredictorList: TStringList;
	lTemp4D,lMaskname,lOutName,lFactname: string;
	lMaskHdr: TMRIcroHdr;
        lMultiSymptomRA,lSymptomRA: singleP;
begin
  lBinomial := not odd( (Sender as tMenuItem).tag);
  if (not lBinomial) and (not ttestmenu.checked)  and (not BMmenu.checked) then begin
      Showmessage('Error: you need to compute at least on test [options/test menu]');
      exit;
  end;
  lImageNamesAll:= TStringList.Create; //not sure why TStrings.Create does not work???
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
   //next, get 1st group
  if not GetVal(lnSubjAll,lnFactors,lMultiSymptomRA,lImageNamesAll,lnCrit{,binom},lPredictorList) then begin
     showmessage('Error with VAL file');
     goto 666;
  end;
  lTemp4D := CreateDecompressed4D(lImageNamesAll);
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
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mask file size too small.');
	   goto 666;
   end;
   if not CheckVoxelsGroup(lImageNamesAll,lMaskVoxels) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;
   lOutName := ExtractFileDirWithPathDelim(lMaskName)+'results';
   SaveHdrDlg.Filename := loutname;
   lOutName := lOutName+'.nii.gz';
   if not SaveHdrName ('Base Statistical Map', lOutName) then goto 666;
   for lFact := 1 to lnFactors do begin
          MsgClear;
          Msg(GetKVers);
      lImageNames.clear;
       for lSubj := 1 to lnSubjAll do
           if (not lBinomial) or (lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)] = 0) OR (lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)] = 1) THEN begin
           {$IFNDEF FPC}if lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)] <> NaN then {$ENDIF}
              lImageNames.Add(lImageNamesAll[lSubj-1]);
           end else begin
               Msg('Data rejected: behavior must be zero or one for binomial test '+lImageNamesAll.Strings[lSubj-1]);
           end;
       lnSubj := lImageNames.Count;
       if lnSubj > 1 then begin
          getmem(lSymptomRA,lnSubj * sizeof(single));
          lnSubj := 0;
          for lSubj := 1 to lnSubjAll do
           if (not lBinomial) or (lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)] = 0) OR (lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)] = 1) THEN
              {$IFNDEF FPC}if lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)] <> NaN then begin
              {$ELSE} begin{$ENDIF}
                 inc(lnSubj);
                 lSymptomRA^[lnSubj] := lMultiSymptomRA^[lSubj+((lFact-1)*lnSubjAll)];
              end;
        Msg('Threads: '+inttostr(gnCPUThreads));
        lFactName := lPredictorList.Strings[lFact-1];
          lFactName := LegitFilename(lFactName,lFact);
          Msg('Factor = '+lFactname);
          For lSubj := 1 to lnSubj do
            Msg (lImageNames.Strings[lSubj-1] + ' = '+realtostr(lSymptomRA^[lSubj],2) );
           Msg('Total voxels = '+inttostr(lMaskVoxels));
          Msg('Only testing voxels damaged in at least '+inttostr(lnCrit)+' individual[s]');
          Msg('Number of Lesion maps = '+inttostr(lnSubj));
          if not CheckVoxelsGroup(lImageNames,lMaskVoxels) then begin
             showmessage('File dimensions differ from mask.');
	     goto 666;
          end;
          if noVariance (lSymptomRA,lnSubj) then
             Msg('Error no variability in behavioral data ')
          else if lBinomial then
             LesionNPMAnalyzeBinomial2(lImageNames,lMaskHdr,lnCrit,MainForm.ReadPermute,lSymptomRA,lFactname,lOutName)
          else begin
              ReportDescriptives(lSymptomRA,lnSubj);
              LesionNPMAnalyze2(lImageNames,lMaskHdr,lnCrit,-1,MainForm.ReadPermute,lSymptomRA,lFactName,lOutname,ttestmenu.checked,BMmenu.checked);
          end;
          Freemem(lSymptomRA);
       end; //lnsubj > 1
          end; //for each factor
    if lnSubjAll > 0 then begin
       Freemem(lMultiSymptomRA);
    end;
    666:
    lImageNames.Free;
    lImageNamesAll.Free;
    lPredictorList.Free;
    DeleteDecompressed4D(lTemp4D);
end;   *)

procedure TMainForm.Copy1Click(Sender: TObject);
begin
  	Memo1.SelectAll;
	Memo1.CopyToClipboard;

end;

procedure TMainForm.testmenuclick(Sender: TObject);
begin
     (sender as TMenuItem).checked := not  (sender as TMenuItem).checked;
end;

procedure TMainForm.radiomenuclick(Sender: TObject);
begin
     (sender as tmenuitem).checked := true;
end;

procedure ComputePlankSize;
begin
     if kPlankMB < 128 then
        kPlankMB := 128;
     if kPlankMB > 2000 then
        kPlankMB := 2000; //we use signed 32-bit pointers, so we can not exceed 2Gb
     kPlankSz :=1024 {bytes/kb} * 1024 {bytes/mb} * kPlankMB;
     kVers := kVers + ' CacheMB = '+inttostr(kPlankMB);
end;

procedure TMainForm.ReadIniFile;
var
  lFilename: string;
  lThreads: integer;
  lIniFile: TIniFile;
begin
     lFilename := IniName;
     if not FileexistsEx(lFilename) then
		exit;
     lIniFile := TIniFile.Create(lFilename);
     ttestmenu.checked := IniBool(lIniFile,'computettest',true);
     //welchmenu.checked := IniBool(lIniFile,'computewelch',true);
     BMmenu.checked := IniBool(lIniFile,'computebm',false);
     gNULP := IniBool(lIniFile,'countlesionpatterns',false);
     gROI := IniBool(lIniFile,'ROI',false);
     gTFCE := IniInt(lIniFile,'TFCE',0);
     kPlankMB := IniInt(lIniFile,'CacheMB',512);
     
     WritePermute(IniInt(lIniFile,'nPermute',0));
     lThreads := IniInt(lIniFile,'nThread', gnCPUThreads );
     if lThreads > gnCPUThreads then
        lThreads := gnCPUThreads;
     gnCPUThreads := lThreads;
  lIniFile.Free;
end; //ReadIniFile

procedure TMainForm.WriteIniFile;
var
  lIniName: string;
  lIniFile: TIniFile;
begin
//showmessage('aaa');
     lIniName := IniName;
  if (DiskFreeEx(lIniName) < 1)  then
	exit;
  lIniFile := TIniFile.Create(lIniName);
  lIniFile.WriteString('BOOL', 'computettest',Bool2Char(ttestmenu.checked));
  lIniFile.WriteString('BOOL', 'countlesionpatterns',Bool2Char(gNULP));
  lIniFile.WriteString('BOOL', 'ROI',Bool2Char(gROI));

  //lIniFile.WriteString('BOOL', 'computewelch',Bool2Char(welchmenu.checked));
  lIniFile.WriteString('BOOL', 'computebm',Bool2Char(BMmenu.checked));
  lIniFile.WriteString('INT', 'TFCE',inttostr(gTFCE));
  lIniFile.WriteString('INT', 'CacheMB',inttostr(kPlankMB));
  lIniFile.WriteString('INT', 'nPermute',inttostr(ReadPermute));
  lIniFile.WriteString('INT', 'nThread',inttostr(ReadThread));
  lIniFile.Free;
end;



procedure TMainForm.FormCreate(Sender: TObject);
begin
    {$IFDEF Darwin}
     File1.visible := false;//for OSX, exit is in the application's menu
     //Edit1.visible := false;//clipboard note yet working for OSX
    {$ENDIF}
    {$IFDEF FPC}
    Application.ShowButtonGlyphs :=  sbgNever;
    {$ENDIF}
        {$IFDEF Darwin}
    {$IFNDEF LCLgtk} //only for Carbon compile
            Copy1.ShortCut := ShortCut(Word('C'), [ssMeta]);
            BinomialAnalysislesions1.ShortCut := ShortCut(Word('B'), [ssMeta]);
            Binaryimagescontinuousgroupsfast1.ShortCut := ShortCut(Word('L'), [ssMeta]);
            Design1.ShortCut := ShortCut(Word('D'), [ssMeta]);
            ContinuousanalysisVBM1.ShortCut := ShortCut(Word('V'), [ssMeta]);
            MultipleRegress.ShortCut := ShortCut(Word('R'), [ssMeta]);
            Makemeanimage1.ShortCut := ShortCut(Word('M'), [ssMeta]);
            About1.ShortCut := ShortCut(Word('A'), [ssMeta]);
    {$ENDIF}//Carbon
    {$ENDIF}//Darwin
    gnCPUThreads := GetLogicalCpuCount;
   (*if (ssShift in KeyDataToShiftState(vk_Shift))  then begin
    	case MessageDlg('Shift key down during launch: do you want to reset the default preferences?', mtConfirmation,
				[mbYes, mbNo], 0) of	{ produce the message dialog box }
				mrNo: ReadIniFile;
	    end; //case
   end else *)
   if not ResetDefaults then
	  ReadIniFile;
   WriteThread(gnCPUThreads);
   ComputePlankSize;
   // ROIanalysis1.visible := gROI;
    {$IFDEF compileANACOM}
    AnaCOMmenu.visible := gROI;
    {$ENDIF}
end;




function TMainForm.MakeMean (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lBinarize,lVariance : boolean): boolean;
label
	667;
var
	lOutName,lOutNameMod: string;
	lCountRA,lOutImgMn,lOutStDev,lPlankImg: SingleP;
        lTotalMemory: double;
	lPlank,lVolVox,lPos,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lPosPct: integer;
        lStDev: boolean;
	lT,  lSum,lSumSqr,lSD, lMn,lTotalSum,lTotalN: double;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
begin
        result := false;
	if not SaveHdrName ('Output image', lOutName) then exit;
        if (not lVariance) and (not lBinarize) then
           lStDev := true
        else
            lStDev := false;
        if lStDev then
           lStDev := OKMsg('Create a standard deviation image as well as a mean image?');
	Msg('Analysis began = ' +TimeToStr(Now));
	lTotalMemory := 0;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
	lMinMask := 1;
	lMaxMask := lVolVox;
	lVoxPerPlank :=  kPlankSz div lImages.Count div sizeof(single) ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := ((lMaxMask+1)-lMinMask) * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
	Msg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	Msg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
       // fx(kPlankSz,8888);
	getmem(lPlankImg,kPlankSz);
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
        Msg('Number of scans = '+inttostr(lImages.count));
        Msg(' Index,Filename,Intercept,Slope');
        if lBinarize then begin
           getmem(lCountRA,lImages.Count*sizeof(single));
           for lPos := 1 to lImages.Count do begin
            gInterceptRA[lPos] := 0;
            gScaleRA[lPos] := 1;
            lCountRA^[lPos] := 0;
           end;
        end else begin
            for lPos := 1 to lImages.Count do begin
                Msg('  '+inttostr(lPos)+','+lImages[lPos-1]+','+realtostr(gInterceptRA[lPos],4)+','+realtostr(gScaleRA[lPos],4));
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
		Msg('Computing plank = ' +Inttostr(lPlank));
                Refresh;
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
                           ProgressBar1.Position := round((lPos2/lVoxPerPlank)*100);
                           Application.Processmessages;
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
                                (*if l1stError then begin
                                   for lPos := 1 to lImages.Count do
                                    Msg(floattostr( (gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos]));
                                   msg('---');
                                   msg(floattostr(lSum));
                                   msg(floattostr(lSumSqr));

                                   l1stError := false;
                                end;*)
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
                Msg('  '+inttostr(lPos)+','+lImages[lPos-1]+','+inttostr(round(lCountRA^[lPos]))  );

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
	Msg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes','.txt');
        MsgSave(lOutNameMod);
        if (lTotalN > 0) then
	   Msg('num voxels >0 = ' +inttostr(round(lTotalN))+'  mean value for voxels >0: '+floattostr(lTotalSum/lTotalN));

        ProgressBar1.Position := 0;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lTotalMemory > 1 then freemem(lPlankImg);
	Msg('Unable to complete analysis.');
        ProgressBar1.Position := 0;
end;

function ApplyTFCE (lImageName: string): boolean;
var
   lImg: SingleP;
   lHdr: TMRIcroHdr;
   lVolVox: integer;
   maxTFCE, maxNegTFCE: single;
   lOutNameMod: string;
begin
	result := false;
	if not NIFTIhdr_LoadHdr(lImageName,lHdr) then begin
	   showmessage('Error reading '+lImageName);
	   exit;
  end;
	lVolVox := lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]* lHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then exit;
	getmem(lImg,lVolVox*sizeof(single));
	if not LoadImg(lHdr.ImgFileName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		Msg('Unable to load  ' +lHdr.ImgFileName);
		exit;
	end;
  //lHdr.NIFTIhdr.scl_slope := 1; lHdr.NIFTIhdr.scl_inter := 0;
  doTFCEbothPolarities (lHdr.NIFTIhdr,  lImg, 6 {NumConn}, 2.0 {H}, 0.5 { E}, 0, 0,0,0 ,maxTFCE, maxNegTFCE);

  lOutNameMod :=  ChangeFilePrefixExt(lImageName,'i','.hdr');
  Msg('Creating  ' +lOutNameMod);
  NIFTIhdr_SaveHdrImg(lOutNameMod,lHdr.NIFTIhdr,true,not IsNifTiMagic(lHdr.NIFTIhdr),true,lImg,1);

	freemem(lImg);


end;

procedure TMainForm.FCE1Click(Sender: TObject);
var
        lFilename: string;
	lPos:  Integer;
  lHdr: TMRIcroHdr;
begin
     MsgClear;
     Msg(GetKVers);
     if not OpenDialogExecute('Select images for TFCE',true,false,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if OpenHdrDlg.Files.Count < 1 then
        exit;
     for lPos := 1 to OpenHdrDlg.Files.Count do begin
         lFilename := OpenHdrDlg.Files[lPos-1];
         //TFCE(lFilename,1,false);
         //ClusterTFCE (lHdr, 666, 2);
         ApplyTFCE(lFilename);
         //Binarize (var lImageName:String; lNonZeroVal: integer; lZeroThresh: boolean): boolean;
     end;
     Msg('Done');
end;

procedure TMainForm.Makemeanimage1Click(Sender: TObject);
label
	666;
var
	lMaskVoxels: integer;
	lG:  TStrings;
	lMaskname: string;
        lMaskHdr: TMRIcroHdr;
begin
     MsgClear;
     Msg(GetKVers);
     if not OpenDialogExecute('Select images to average',true,true,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     lG:= TStringList.Create;
     lG.addstrings(OpenHdrDlg.Files);
     lMaskname := lG[0];
     if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading '+lMaskName);
	   goto 666;
     end;
     lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
     if not CheckVoxelsGroupX(lG,lMaskHdr {lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
     end;
     Msg('Voxels = '+inttostr(lMaskVoxels));
     MakeMean(lG,lMaskHdr, odd((Sender as TMenuItem).tag),false);
     666:
     lG.Free;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
     Close;
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
             Showmessage('Warning: inflection and windowed algorithms find different 2nd modes. Using inflection 2nd mode. inflection ='+inttostr(lModePos)+'  windowed: '+inttostr(lMode2NotInflection));

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


procedure CopyFileEXoverwrite (lInName,lOutName: string);
var lFSize: Integer;
   lBuff: bytep0;
   lFData: file;
begin
	 lFSize := FSize(lInName);
	 if (lFSize < 1)  then exit;
	 assignfile(lFdata,lInName);
	 filemode := 0;
	 reset(lFdata,lFSize{1});
	 GetMem( lBuff, lFSize);
	 BlockRead(lFdata, lBuff^, 1{lFSize});
	 closefile(lFdata);
	 assignfile(lFdata,lOutName);
	 filemode := 2;
	 Rewrite(lFdata,lFSize);
	 BlockWrite(lFdata,lBuff^, 1  {, NumWritten});
	 closefile(lFdata);
	 freemem(lBuff);
end;

procedure CopyFileEX (lInName,lOutName: string);
var lFSize: Integer;
begin
	 lFSize := FSize(lInName);
	 if (lFSize < 1) or (fileexistsEX(lOutName)) then exit;
	CopyFileEXoverwrite (lInName,lOutName);
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
     //zero array



function TMainForm.Balance (var lImageName,lMaskName: String; {lInflection: boolean}lMethod: integer): boolean;
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
	   showmessage('Error reading '+lImageName);
	   exit;
        end;
	lVolVox := lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]* lHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then exit;
	getmem(lImg,lVolVox*sizeof(single));
	if not LoadImg(lHdr.ImgFileName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		Msg('Unable to load  ' +lHdr.ImgFileName);
		exit;
	end;
        if lMaskName <> '' then begin
           if not NIFTIhdr_LoadHdr(lMaskName,lMaskHdr) then begin
	      showmessage('Error reading '+lMaskName);
	      exit;
           end;
           if lVolVox <> (lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3]) then begin
	      showmessage('Mask and header must have identical dimensions '+lMaskName+ ' ' + lImageName);
	      exit;

           end;
           getmem(lMaskImg,lVolVox*sizeof(single));
	   if not LoadImg(lMaskHdr.ImgFileName, lMaskImg, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		Msg('Unable to load mask ' +lMaskHdr.ImgFileName);
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
              Msg('Warning: no voxels masked with image '+lMaskName)
           else
               Msg('Mask='+ lMaskName+' Number of voxels masked= '+inttostr(lMasked)+'  Mean unscaled intensity of masked voxels= '+floattostr(lMaskedInten/lMasked));
           freemem(lMaskImg);
        end;//mask

        if not MinMax(lImg,lVolVox,lMin,lMax) then exit;
        Msg(lImageName+'  -> '+lHdr.ImgFileName);
        Msg('min..max ' +floattostr(lMin)+'..'+floattostr(lMax));
        if (lMethod = 0) or (lMethod = 1) then begin
           if not DetectMode(lImg,lVolVox,lMin,lMax,lModeLo,lModeHi, odd(lMethod)) then exit;
           if odd(lMethod) then
              Msg('method for finding second mode: inflection')
           else
              Msg('method for finding second mode: masked peak');
           Msg('modes Lo Hi ' +floattostr(lModeLo)+'..'+floattostr(lModeHi));
           if lModeLo >= lModeHi then exit;
           lSlope := 1/abs(lModeHi-lModeLo);
           lIntercept := (abs(lModeHi-lModeLo)-(lModeLo))*lSlope ; //make mode lo = 1;
        end else begin
            DetectMeanStDev (lImg, lVolVox, lMean,lStDev);
            if lStDev <>0 then
               lSlope := 1/lStDev
            else begin
                Msg('Warning: StDev = 0!!!!');
                lSlope := 1;
            end;
            lIntercept := (-lMean*lSlope)+2; //mean voxel has intensity of zero

            Msg('method for intensity normalization: Mean = 2, StDev = 1');
            Msg('raw_Mean = '+floattostr(lMean)+'  '+' raw_StDev = '+floattostr(lStDev));

        end;
        Msg('Slope/Intercept ' +floattostr(lSlope)+'..'+floattostr(lIntercept));
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

procedure TMainForm.Balance1Click(Sender: TObject);
var
        lFilename,lMaskName: string;
	lPos:  Integer;
begin
     MsgClear;
     Msg(GetKVers);
     if not OpenDialogExecute('Select images for intensity normalization',true,false,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if OpenHdrDlg.Files.Count < 1 then
        exit;
     lMaskName := '';
     for lPos := 1 to OpenHdrDlg.Files.Count do begin
         lFilename := OpenHdrDlg.Files[lPos-1];
         balance(lFilename,lMaskname,(Sender as TMenuItem).tag);
     end;
end;

procedure TMainForm.niiniigz1Click(Sender: TObject);
var
        lFilename,lOutname,lPath,lName,lExt: string;
	lPos:  Integer;
begin
     MsgClear;
     Msg(GetKVers);
     if not OpenDialogExecute('Select images',true,false,kNIIFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if OpenHdrDlg.Files.Count < 1 then
        exit;

     for lPos := 1 to OpenHdrDlg.Files.Count do begin
         lFilename := OpenHdrDlg.Files[lPos-1];
         FilenameParts(lFilename,lPath,lName,lExt);
         lOutname := lPath+lName+'.nii.gz';
         msg('Compressing '+ lFilename+'  -> '+lOutname);
         GZipFile(lFilename, lOutname,false);
     end;
     msg('Compression completed');
end;

procedure TMainForm.Variance1Click(Sender: TObject);
label
	666;
var
	lMaskVoxels: integer;
	lG:  TStrings;
	lMaskname: string;
        lMaskHdr: TMRIcroHdr;
begin
     MsgClear;
     Msg(GetKVers);
     if not OpenDialogExecute('Select 2 images)',true,true,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     lG:= TStringList.Create;
     lG.addstrings(OpenHdrDlg.Files);
     if lG.count <> 2 then begin
         showmessage('You must select exactly two image.');
         goto 666;
     end;
     lMaskname := lG[0];
     if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading mask.');
	   goto 666;
     end;
     lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
     if not CheckVoxelsGroupX(lG,lMaskHdr{lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
     end;
     Msg('Voxels = '+inttostr(lMaskVoxels));
     MakeMean(lG,lMaskHdr, odd((Sender as TMenuItem).tag),true);
     666:
     lG.Free;
end;

procedure BMX;
const
     kN = 53;
     knNoLesion = 48;
     kSymptomRA: array[1..kn] of single =
(4,4.5,2.5,5,4,3.25,0.75,4.5,4.5,0.5,1.625,0,3.5,3,4,2,4.5,5,1.5,5,2.5,5,4,0,2,
1.5,1.75,2.5,5,0,3.25,4.375,0,3.75,0.25,0,2,5,0,0.5,0,2.25,0,2.25,2,0,0.25,0,0,0,0,0,0);
var
   lObs: doublep0;
   lI: integer;
   lBMz,lBMzs,lDF: double;

begin
     getmem(lObs,kN * sizeof(double));
     for lI := 1 to kN do
         lObs^[lI-1] := kSymptomRA[lI];

     tBM (kN, knNoLesion, lObs,lBMz,lDF);
     //simulate
     MainForm.NPMmsg('Generating BM permutation thresholds');
     MainForm.Refresh;
     genBMsim (kN, lObs);

     lBMzs := BMzVal (kN, knNoLesion,lBMz,lDF);
     //end simulate
     MainForm.NPMmsg('BMsim= '+floattostr(lBMzs)+'  '+'BM= '+floattostr(lBMz)+'  '+floattostr(lDF) );
     freemem(lObs);
end;

(*procedure SX;
var
   lVALFilename {,lTemplateName}: string;
   lCritPct,lnSubj, lnFactors: integer;
   var lSymptomRA: singleP;
   var lImageNames:  TStrings;
   var lCrit: integer; {lBinomial : boolean;}
   var lPredictorList: TStringList;

begin
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
     lVALFilename := 'c:\RT_norm.val';//MainForm.OpenHdrDlg.Filename;
     lPredictorList := TStringList.Create;
     GetValCore ( lVALFilename, lnSubj, lnFactors, lSymptomRA, lImageNames, lCrit,lCritPct{,binom},lPredictorList);
    lImageNames.Free;
    lPredictorList.Free;
end;*)

(*procedure ComputeR;
var
   lStr: string;
   lT,lDF: double;
begin
    inputquery('Enter value','Enter T score',lStr);
    lT := strtofloat(lStr);
    inputquery('Enter value','Enter DF',lStr);
    lDF := strtofloat(lStr);
    showmessage('The coresponding correlation Z score for t('+floattostr(lDF)+')='+floattostr(lT) +' is '+floattostr(TtoZ(lT,lDF) )  );
    //showmessage('The coresponding correlation R score for t('+floattostr(lDF)+')='+floattostr(lT) +' is '+floattostr(TtoR(lT,lDF) )  );
end;

function Log10x (lLogP: double): double;
begin
     result := -Log10(lLogP);
     fx(result);
end;


procedure LogPtoZ (lLogP: double);
var
   lD,lZ: double;
begin
     ///lD := Log10(lLogp);
     lD := Power(10,-lLogP);
     lZ := pNormalInv(lD);
     fx(lD,lZ);
end;     *)

procedure TMainForm.About1Click(Sender: TObject);
begin
//Masked1Click(nil); exit;
//LogPtoZ (Log10x(0.02));
     //LogPtoZ(1.699);
     //ComputeR;
     showmessage(GetkVers );
end;

procedure TMainForm.Design1Click(Sender: TObject);
begin
{$IFDEF SPREADSHEET} SpreadForm.Show; {$ELSE} Showmessage('Spreadsheet not yet supported on the Operating System');{$ENDIF}
end;

procedure TMainForm.StrToMemo(lStr: String);
var
   lLen,lPos: integer;
   lOutStr: string;
begin
     lLen := length(lStr);
     if lLen < 1 then exit;
     lOutStr := '';
     for lPos := 1 to lLen do begin
         if lStr[lPos] = kCR then begin
            Msg(lOutStr);
            lOutStr := '';
         end else
             lOutStr := lOutStr + lStr[lPos];
     end;
     if lOutStr <> '' then
        Msg(lOutStr);
end;


procedure TMainForm.PhysiologicalArtifactCorrection1Click(Sender: TObject);
var
   lInImgName,lPulsFile,lRespFile,lOutImgName,lStr: string;
   l4Ddata: singlep;
   lHdr: TMRIcroHdr;
   lDim,lImgVox: integer;
   lOutHdr: TniftiHdr;
begin
     if not OpenDialogExecute('Select file with pulse onsets',false,false,'Siemens physio |*.puls|3-column text |*.txt') then
              exit;
     lPulsFile := OpenHdrDlg.Filename;
     if UpCaseExt(lPulsFile) = '.PULS' then
        lRespFile := changefileext(lPulsFile,'.resp')
     else begin //text input
          if not OpenDialogExecute('Select file with respiration onsets',false,false,'3-column text |*.txt') then
              lRespFile := ''
          else
              lRespFile :=  OpenHdrDlg.Filename;
     end;
     if not OpenDialogExecute('Select 4D motion corrected data ',false,false,kImgFilter) then
        exit;
     lInImgName := OpenHdrDlg.Filename;
     if not NIFTIhdr_LoadHdr(lInImgName,lHdr) then begin
	   showmessage('Error reading image header.');
	   exit;
     end;
     for lDim := 1 to 4 do
         if lHdr.NIFTIhdr.Dim[lDim] < 4 then begin
             Showmessage('You need to select a 4D image with at least 4 voxels/images in each dimension.');
             exit;
         end;
     lImgVox := lHdr.NIFTIhdr.Dim[1]*lHdr.NIFTIhdr.Dim[2]*lHdr.NIFTIhdr.Dim[3];
     lDim := lImgVox*lHdr.NIFTIhdr.Dim[4];
     MsgClear;
     Msg(kVers);
     Msg('Physiological Artifact Removal Tool started = ' +TimeToStr(Now));
     Msg('Assuming continuous fMRI ascending acquisition with TR = '+realtostr(lHdr.NIFTIhdr.PixDim[4],4)+'sec');
     MainForm.refresh;
     getmem(l4Ddata,lDim*sizeof(single));
     if not LoadImg(lHdr.ImgFileName, l4Ddata, 1, lDim,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lImgVox) then begin
        Showmessage('Unable to load data');
        freemem(l4Ddata);
        exit;
     end;
     lStr := ApplyPART( lPulsFile,l4Ddata,40, lImgVox,lHdr.NIFTIhdr.Dim[3], lHdr.NIFTIhdr.Dim[4], lHdr.NIFTIhdr.PixDim[4]);
     if lStr = '' then begin
         Showmessage('Unable to apply physio file. Physiological correction is being aborted.');
         exit;
     end;
     StrToMemo (lStr);
     if (lRespFile <> '') and (fileexists(lRespFile)) then begin
        lStr := ApplyPART( lRespFile,l4Ddata,20, lImgVox,lHdr.NIFTIhdr.Dim[3], lHdr.NIFTIhdr.Dim[4], lHdr.NIFTIhdr.PixDim[4]);
        StrToMemo (lStr);
        if lStr = '' then begin
           Showmessage('Unable to read Respiration file. Hysiological correction is being aborted.');
           exit;
        end;

     end;

     MakeHdr (lHdr.NIFTIhdr,lOutHdr);
     Msg('Input = ' +lInImgName);
     lOutImgName := ChangeFilePrefixExt(lInImgName,'i','.hdr');
     NIFTIhdr_SaveHdrImg(lOutImgName,lOutHdr,true,not IsNifTiMagic(lHdr.NIFTIhdr),true,l4Ddata,lHdr.NIFTIhdr.Dim[4]);
     Msg('Output = ' +lOutImgName);
     Msg('Physiological Artifact Removal Tool finished = ' +TimeToStr(Now));
     lOutImgName := ChangeFilePostfixExt(lOutImgName,'Notes','.txt');
     MsgSave(lOutImgName);
     freemem(l4Ddata);
end;

function ChangeName (lInName: string): string;
var
    lPath,lName,lExt: string;
begin
    //lInName:= 'c:\vbm\ds\123';
    FilenameParts (lInName, lPath,lName,lExt);
    //showmessage(lPath+'*'+lName+'*'+lExt);
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
             showmessage('Unable to find a file named '+ lFilename);
             exit;
         end;
         lImageNames.add(lFilename);
     end;
     result := true;
end;

function    ReadPairedFilenames(var lImageNames: TStrings): boolean;
var
   lLen,lPos: integer;
   lFilenames,lF1,lF2: string;
   lImageNames2:  TStrings;
   lF: TextFile;
begin
     result := false;
     Showmessage('Please select a text file with the image names. '+kCR+
     'Each line of the file should specify the control and experimental filenames, separated by an *'+kCR+
       'C:\vbmdata\c1.nii.gz*C:\vbmdata\e1.nii.gz'+kCR +
       'C:\vbmdata\c2.nii.gz*C:\vbmdata\e2.nii.gz'+kCR+
       'C:\vbmdata\c3.nii.gz*C:\vbmdata\e3.nii.gz'+kCR+
       '...' );
     if not MainForm.OpenDialogExecute('Select asterix separated filenames ',false,false,kTxtFilter) then
         exit;
     lImageNames2:= TStringList.Create; //not sure why TStrings.Create does not work???
     //xxx
     assignfile(lF,MainForm.OpenHdrDlg.FileName );
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
                        showmessage('Can not find image '+lF2);
                 end else //F1 exists
                     showmessage('Can not find image '+lF1);
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

function  AddNumStr(var X : PMatrix; var lNumStr: string; lRow,lCol: integer):boolean;
var
   lTempFloat: double;
begin

    result := false;
    if (lNumStr = '') or (lRow < 1) or (lCol < 1) then exit;
    try
       lTempFloat := strtofloat(lNumStr);
    except
          on EConvertError do begin
                showmessage('Empty cells? Error reading TXT file row:'+inttostr(lRow)+' col:'+inttostr(lCol)+' - Unable to convert the string '+lNumStr+' to a number');
             exit;
          end;
    end;
    //fx(lRow,lCol,lTempFloat);
    X^[lCol]^[lRow] := lTempFloat;
    lNumStr := '';
    result := true;
end;

{$DEFINE notRTEST}
function    ReadPairedFilenamesReg(var lImageNames: TStrings; var X : PMatrix; var  lnAdditionalFactors: integer): boolean;
var
   lLen,lPos,lSep,lMaxSep,lLine: integer;
   lFilenames,lF1,lF2,lNumStr: string;
   lImageNames2:  TStrings;
   lF: TextFile;
begin
     result := false;
     {$IFDEF RTEST}
      MainForm.OpenHdrDlg.FileName := 'c:\twins\dataplus.txt';
     {$ELSE}
     Showmessage('Please select a text file with the image names. '+kCR+
     'Each line of the file should specify the control and experimental filenames, separated by an *'+kCR+
       'C:\vbmdata\c1.nii.gz*C:\vbmdata\e1.nii.gz'+kCR +
       'C:\vbmdata\c2.nii.gz*C:\vbmdata\e2.nii.gz'+kCR+
       'C:\vbmdata\c3.nii.gz*C:\vbmdata\e3.nii.gz'+kCR+
       '...' );
     if not MainForm.OpenDialogExecute('Select asterix separated filenames ',false,false,kTxtFilter) then
         exit;
     {$ENDIF}

     lImageNames2:= TStringList.Create; //not sure why TStrings.Create does not work???
     //xxx
     assignfile(lF,MainForm.OpenHdrDlg.FileName );
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
              while (lPos <= lLen) and (lFilenames[lPos] <> '*') do  begin
                    lF2 := lF2 + lFilenames[lPos];
                    inc(lPos);
              end;
              if (length(lF1) > 0) and (length(lF2)>0) then begin
                 if Fileexists4D(lF1) then begin
                    if Fileexists4D(lF2) then begin
                       lImageNames.add(lF1);
                       lImageNames2.add(lF2);
                    end else //F2exists
                        showmessage('Can not find image '+lF2);
                 end else //F1 exists
                     showmessage('Can not find image '+lF1);
              end;
           end;//len>0
     end; //while not EOF

     //fx(lImageNames.count);
     //next - count additional factors
     lnAdditionalFactors := 0;
     reset(lF);
     lMaxSep := 0;
     while not EOF(lF) do begin
           readln(lF,lFilenames);
           lLen := length(lFilenames);
           lSep := 0;
           if lLen > 0 then begin
              for lPos := 1 to lLen do
                  if lFilenames[lPos] = '*' then
                    inc(lSep)
           end;//len>0
           if lSep > lMaxSep then
              lMaxSep := lSep;
     end; //while not EOF
     if (lMaxSep > 1) and (lImageNames2.count > 1) then begin //additional factors present
        //final pas - load additional factors
        lnAdditionalFactors := lMaxSep - 1;

        DimMatrix(X, lnAdditionalFactors, lImageNames2.count);
        reset(lF);
        lLine := 0;
        while not EOF(lF) do begin
           readln(lF,lFilenames);
           lLen := length(lFilenames);
           lSep := 0;

           if lLen > 0 then begin
              inc(lLine);
              lPos := 1;
              lNumStr := '';
              while lPos <= lLen do begin
                  if (lFilenames[lPos] = '*') then begin
                    AddNumStr(X,lNumStr,lLine,lSep-1);
                    inc(lSep);
                  end else if (lSep >= 2) and (not (lFilenames[lPos] in [#10,#13,#9]) ) then begin
                        lNumStr := lNumStr+lFilenames[lPos];
                        //showmessage(lNumStr);
                  end;
                  inc(lPos);
              end; //while not EOLN
              AddNumStr(X,lNumStr,lLine,lSep-1);
           end;//len>0
        end; //while not EOF
        //next - read final line of unterminated string...
     end;//maxsepa > 1


     //2nd pass vals
     closefile(lF);
       FileMode := 2;  //read/write
     if (lImageNames.count > 0) and (lImageNames2.count = lImageNames.count) then begin
        lImageNames.AddStrings(lImageNames2);

        result := true;
     end;
     lImageNames2.Free;
     result := true;
end;

procedure TMainForm.DualImageCorrelation1Click(Sender: TObject);
label
	666;
var
	lnSubj,lSubj,lMaskVoxels,lnAdditionalFactors,lI: integer;
	lImageNames:  TStrings;
    X: PMatrix;
	lMaskname,lStr,lOutName: string;
	lMaskHdr: TMRIcroHdr;
begin
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  MsgClear;
  Msg(kVers);
  {$IFDEF RTEST}
   OpenHdrDlg.FileName := 'c:\twins\aameanMean.hdr';
  {$ELSE}
  Msg('Dual-image Linear Regression [Weighted Least Squares]');
  if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
	   showmessage('NPM aborted: mask selection failed.');
	   goto 666;
   end; //if not selected
   {$ENDIF}

   lMaskname := OpenHdrDlg.Filename;

  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading Mask image.');
	   goto 666;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mask file size too small.');
	   goto 666;
   end;
   if not ReadPairedFilenamesReg(lImageNames,X,lnAdditionalFactors) then exit;
   lnSubj :=lImageNames.Count div 2;

   //fx(lnAdditionalFactors);
   //show matrix
   //MsgStrings (lImageNames);
   Msg ('n Subjects = '+inttostr(lnSubj));
   for lSubj := 0 to (lnSubj-1) do begin
       lStr := lImageNames[lSubj]+' <-> '+lImageNames[lSubj+lnSubj];
       if lnAdditionalFactors > 0 then
          for lI := 1 to lnAdditionalFactors do
              lStr := lStr+','+floattostr(X^[lI]^[lSubj+1]);


            Msg(lStr);
   end;
   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;


   Msg('Mask = '+lMaskname);
   Msg('Total voxels = '+inttostr(lMaskVoxels));
   Msg('Number of observations = '+inttostr(lnSubj));
   (*if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;*)

   if lnSubj < 5 then begin
      showmessage('Paired regression error: Requires at least 5 images per group.');
      goto 666;
   end;
   lOutName := lMaskName;
   if not SaveHdrName ('Base Statistical Map', lOutName) then exit;
   //showmessage('Unimplemented Regress');//
   Regress2NPMAnalyze (lImageNames, lMaskHdr, lOutname,X,lnAdditionalFactors);
   if lnAdditionalFactors > 1 then
      DelMatrix(X, lnAdditionalFactors, lnSubj);
    666:
        lImageNames.Free;
end;

procedure TMainForm.LesionBtnClick(Sender: TObject);
  label
	666;
var
        lPrefs: TLDMPrefs ;
begin
     lPrefs.NULP := gNULP;
     if (1= (Sender as tMenuItem).tag) then begin //continuous
             lPrefs.BMtest :=   BMmenu.checked;
             lPrefs.Ttest := ttestmenu.checked;
             if (not lPrefs.BMtest) and (not lPrefs.ttest) then
                lPrefs.ttest := true;
             lPrefs.Ltest:= false;
     end else begin //binomial
             lPrefs.BMtest := false;
             lPrefs.Ttest := false;
             lPrefs.Ltest:= true;
     end;
     lPrefs.CritPct := -1;
     lPrefs.nPermute := ReadPermute;
     lPrefs.Run := 0;{0 except for montecarlo}
     {if (not lPrefs.Ltest) and (not lPrefs.Ttest)  and (not lPrefs.BMtest) then begin
        Showmessage('Error: you need to compute at least on test [options/test menu]');
        exit;
     end; code above defaults to t-test}
     if not MainForm.OpenDialogExecute('Select MRIcron VAL file',false,false,'MRIcron VAL (*.val)|*.val') then begin
	      showmessage('NPM aborted: VAL file selection failed.');
	      exit;
     end; //if not selected
     lPrefs.VALFilename := MainForm.OpenHdrDlg.Filename;
   lPrefs.OutName := ExtractFileDirWithPathDelim(lPrefs.VALFilename)+'results';
   lPrefs.OutName := lPrefs.OutName+'.nii.gz';
   SaveHdrDlg.Filename := lPrefs.Outname;
   if not SaveHdrName ('Base Statistical Map', lPrefs.OutName) then exit;
   //Explicit mask
   if not OpenDialogExecute('Select explicit mask [optional]',false,false,kImgPlusVOIFilter) then
    lPrefs.ExplicitMaskName := ''
   else
     lPrefs.ExplicitMaskName := OpenHdrDlg.FileName;

   DoLesion (lPrefs); //Prefs.pas
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
     MsgClear;
     Msg(GetkVers);
     {$IFNDEF UNIX} {GUILaunch;}{$ENDIF}
     LongTimeFormat := 'YYYY-MMM-DD hh:nn:ss';  //delphi TimeToStr
     ShortTimeFormat := 'YYYY-MMM-DD hh:nn:ss'; //freepascal TimeToStr
     //stax;
     //MakeGZ;
     //{x$IFNDEF UNIX} Threads1.visible := false;{x$ENDIF}//Windows can read actual CPU count
     //TestMultReg;
     //SingleRegressClick(nil);
     //DualImageCorrelation1Click(nil);
     //AnaCOM1Click(nil);
     //msg(floattostr(pNormalInv(1/20000)));
     //msg(floattostr(pNormalInv(0.01667)));
     //msg(floattostr(pNormalInv(0.05/51700)));
     //msg(floattostr(0.05/pNormal(4.76 )));
     {$IFNDEF UNIX} ReadParamStr; {$ENDIF}
     //FX(rocA(0.2,0.4),AUC(0.7,0.4),rocA(0.4,0.7),AUC(0.4,0.7) );
     //LesionX;
     {$IFDEF benchmark}
     MonteCarloSimulation1.visible := true;
     //        LesionMonteCarlo (false,true,true);
     {$ENDIF}
end;


procedure TMainForm.PairedTMenuClick(Sender: TObject);
label
	666;
var
	lnSubj,lSubj,lMaskVoxels: integer;
	lImageNames:  TStrings;
	lMaskname,lStr,lOutName: string;
	lMaskHdr: TMRIcroHdr;
begin
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  MsgClear;
  Msg(kVers);
  Msg('Paired T-test [Repeated Measures]');
  if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
	   showmessage('NPM aborted: mask selection failed.');
	   goto 666;
   end; //if not selected
   //OpenHdrDlg.FileName := 'c:\vbmdata\mask50.nii.gz';
   lMaskname := OpenHdrDlg.Filename;

  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading Mask image.');
	   goto 666;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mask file size too small.');
	   goto 666;
   end;
   if not ReadPairedFilenames(lImageNames) then exit;
   lnSubj :=lImageNames.Count div 2;

   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;


   Msg('Mask = '+lMaskname);
   Msg('Total voxels = '+inttostr(lMaskVoxels));
   Msg('Number of observations = '+inttostr(lnSubj));
   Msg('Degrees of Freedom = '+inttostr(lnSubj-1));

   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;
   //show matrix
   //MsgStrings (lImageNames);
   Msg ('n Subjects = '+inttostr(lnSubj));
   lStr := 'Image,';
   for lSubj := 0 to (lnSubj-1) do
            Msg(lImageNames[lSubj]+' <-> '+lImageNames[lSubj+lnSubj]);
   if lnSubj < 4 then begin
      showmessage('Paired t-test error: Requires at least 4 images per group.');
      goto 666;
   end;
   lOutName := lMaskName;
   //if not SaveHdrName ('Base Statistical Map', lOutName) then exit;
   NPMAnalyzePaired (lImageNames, lMaskHdr, lMaskVoxels);
   //Regress2NPMAnalyze (lImageNames, lMaskHdr, lOutname);
    666:
        lImageNames.Free;
end;

function TMainForm.NPMzscore (var lImages: TStrings; var lMnHdr,lStDevHdr: TMRIcroHdr): boolean;
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
	//lOutName := lMnHdr.ImgFileName;
	//if not SaveHdrName ('Statistical Map', lOutName) then exit;
	Msg('Analysis began = ' +TimeToStr(Now));
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
		Msg('Unable to load mean ' +lMnHdr.ImgFileName);
		goto 667;
	end;
	//load StDev
	getmem(lStDevImg,lVolVox*sizeof(single));
	if not LoadImg(lStDevHdr.ImgFileName, lStDevImg, 1, lVolVox,round(gOffsetRA[kMaxImages]),1,lStDevHdr.NIFTIhdr.datatype,lVolVox) then begin
		Msg('Unable to load StDev ' +lStDevHdr.ImgFileName);
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
                ProgressBar1.Position := round((lSubj/lImages.Count)*100);
                Msg( lImages.Strings[lSubj-1]);
                showmessage(inttostr(round(gOffsetRA[lSubj])));
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
	Msg('Analysis finished = ' +TimeToStr(Now));
        ProgressBar1.Position := 0;
        result := true;
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lVolVox > 1 then freemem(lMnImg);
	Msg('Unable to complete analysis.');
        ProgressBar1.Position := 0;
end;


procedure TMainForm.SingleSubjectZScores1Click(Sender: TObject);
label
	666;
var
	lnSubj,lMnVoxels: integer;
	lG:  TStrings;
	lMn,lStDev: string;
	lMnHdr,lStDevHdr: TMRIcroHdr;
begin
  if (not ttestmenu.checked)  and (not BMmenu.checked) then begin
      Showmessage('Error: you need to compute at least on test [options/test menu]');
      exit;
  end;
	MsgClear;
        Msg(kVers);
        Msg('Threads: '+inttostr(gnCPUThreads));
   if not OpenDialogExecute('Select mean image ',false,false,kImgFilter) then begin
	   showmessage('NPM aborted: mean selection failed.');
	   exit;
   end; //if not selected
   lMn := OpenHdrDlg.Filename;
   if not NIFTIhdr_LoadHdr(lMn,lMnHdr) then begin
	   showmessage('Error reading mask.');
	   exit;
   end;
   lMnVoxels := ComputeImageDataBytes8bpp(lMnHdr);
   if (lMnVoxels < 2) or (not CheckVoxels(lMn,lMnVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mean file size too small.');
	   exit;
   end;

   if not OpenDialogExecute('Select StDev image ',false,false,kImgFilter) then begin
	   showmessage('NPM aborted: StDev selection failed.');
	   exit;
   end; //if not selected
   lStDev := OpenHdrDlg.Filename;
   if not NIFTIhdr_LoadHdr(lStDev,lStDevHdr) then begin
	   showmessage('Error reading StDev.');
	   exit;
   end;
   if not  CheckVoxels(lStDev, lMnVoxels,kMaxImages) then begin
	   showmessage('Error Mean and StDev must have same size.');
	   exit;
   end;
   Msg('Mean name = '+ lMn);
   Msg('Total voxels = '+inttostr(lMnVoxels));
   //next, get 1st group
   if not OpenDialogExecute('Select postive group (Z scores positive if this group is brighter)',true,false,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
   end; //if not selected
   lG:= TStringList.Create; //not sure why TStrings.Create does not work???
   lG.addstrings(OpenHdrDlg.Files);
   lnSubj :=OpenHdrDlg.Files.Count;
   Msg('Subjects= '+inttostr(lnSubj));
   if not CheckVoxelsGroupX(lG,lMnHdr {lMnVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;
   NPMzscore (lG, lMnHdr,lStDevHdr);
   //NPMAnalyze(lG,lMnHdr,lMnVoxels,lnSubj);
   666:
   lG.Free;
end;

procedure TMainForm.MultipleRegressClick(Sender: TObject);
label
	666;
var
	lnFactors,lnSubj,lMaskVoxels,lRow,lCol: integer;
	lImageNames:  TStrings;
        lPredictorList: TStringList;
	lTemp4D,lMaskname,lStr,lOutName: string;
	lMaskHdr: TMRIcroHdr;
        X : PMatrix;
begin


     {$IFDEF FPC}
     showmessage('Regression routines not extensively tested: you may want to use the Windows compilation.');
     {$ENDIF}
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  lPredictorList := TStringList.Create;
  Memo1.Lines.Clear;
  Memo1.Lines.Add(kVers);
  Memo1.Lines.Add('Multiple Linear Regression [Weighted Least Squares]');
  if not GetValReg(lnSubj,lnFactors,X,lImageNames,lPredictorList) then
     goto 666;
    lTemp4D := CreateDecompressed4D(lImageNames);
   if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
	   showmessage('NPM aborted: mask selection failed.');
	   goto 666;
   end; //if not selected
   lMaskname := OpenHdrDlg.Filename;
   //lMaskname := lImageNames[0];
  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading 1st image.');
	   goto 666;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mask file size too small.');
	   goto 666;
   end;
   Memo1.Lines.Add('Mask = '+lMaskname);
   Memo1.Lines.Add('Total voxels = '+inttostr(lMaskVoxels));
   Memo1.Lines.Add('Number of observations = '+inttostr(lnSubj));
   if not CheckVoxelsGroupX(lImageNames,lMaskHdr {lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;
   //show matrix
   lStr := 'Image,';
   for lCol := 1 to lnFactors do
            lStr := lStr + lPredictorList.Strings[lCol-1]+', ';
       MainForm.Memo1.Lines.Add(lStr);
   for lRow := 1 to lnSubj do begin
       lStr := lImageNames[lRow-1]+',';
       for lCol := 1 to lnFactors do
            lStr := lStr + floattostr(X^[lCol]^[lRow])+', ';
       MainForm.Memo1.Lines.Add(lStr);
   end;
   lOutName := lMaskName;
   if not SaveHdrName ('Base Statistical Map', lOutName) then exit;
   ARegressNPMAnalyze(lImageNames,lMaskHdr,X,lnFactors,lPredictorList,lOutName,0,0);

   DelMatrix(X, lnFactors, lnSubj);
    666:
        lImageNames.Free;
        lPredictorList.Free;
        DeleteDecompressed4D(lTemp4D);
end;

{$DEFINE notRegTest}
procedure TMainForm.SingleRegressClick(Sender: TObject);
label
	666;
var
	lnSubj1,lnFactors,lnSubj,lMaskVoxels,lRow,lCol: integer;

	lImageNames,lImageNames1:  TStrings;
        lPredictorList,lPredictorList1: TStringList;
	lTemp4D,lMaskname,lOutName: string;
	lMaskHdr: TMRIcroHdr;
        X,X1 : PMatrix;
begin

     {$IFDEF FPC}
     showmessage('Regression routines not extensively tested: you may want to use the Windows compilation.');
     {$ENDIF}
  lTemp4D := '';
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  lPredictorList := TStringList.Create;
  lPredictorList1 := TStringList.Create;
  if not GetValReg(lnSubj,lnFactors,X,lImageNames,lPredictorList) then
     goto 666;
  {$IFDEF regtest}
  lMaskname := 'C:\Documents and Settings\Chris Rorden\Desktop\npmdata\npmdata\amask50.nii.gz';
  {$ELSE}
  if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
	   showmessage('NPM aborted: mask selection failed.');
	   goto 666;
   end; //if not selected
   lMaskname := OpenHdrDlg.Filename;
  {$ENDIF}
  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading 1st image.');
	   goto 666;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mask file size too small.');
	   goto 666;
   end;
   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;
   lOutName := lMaskName;
   {$IFNDEF regtest}

   if not SaveHdrName ('Base Statistical Map', lOutName) then goto 666;
   {$ENDIF}
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
          showmessage('serious error');
       Memo1.Lines.Clear;
       Memo1.Lines.Add(kVers);
       Memo1.Lines.Add('Single Linear Regression [Weighted Least Squares]');
       Memo1.Lines.Add('Mask = '+lMaskname);
       Memo1.Lines.Add('Total voxels = '+inttostr(lMaskVoxels));
       Memo1.Lines.Add('Number of observations = '+inttostr(lnSubj1));
       MainForm.Memo1.Lines.Add('Image,'+ lPredictorList1.Strings[0]);
       for lRow := 1 to lnSubj1 do
           MainForm.Memo1.Lines.Add(lImageNames1[lRow-1]+','+floattostr(X1^[1]^[lRow])  ) ;
       ARegressNPMAnalyze(lImageNames1,lMaskHdr,X1,1,lPredictorList1,lOutName, ReadPermute,gTFCE);
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

procedure TMainForm.AssociatevalfileswithNPM1Click(Sender: TObject);
begin
{$IFDEF FPC}
    //unsupported by FreePascal
{$ELSE}
  case MessageDlg('NPM installation:'+kCR+'Do you want .val fiels to automatically open NPM when you double click on their icons?', mtConfirmation,
     [mbYes, mbNo], 0) of	{ produce the message dialog box }
     id_No: exit;
  end;
     registerfiletype(kVALNativeExt,'NPM'{key},'NPM',Application.ExeName+',1');
{$ENDIF}

end;

procedure TMainForm.threadChange(Sender: TObject);
begin
 (sender as tmenuitem).checked := true;
 ReadThread;
end;

procedure TMainForm.Countlesionoverlaps1Click(Sender: TObject);
label
	666;
var
	lReps,lMax,lInc,lMaskVoxels,lDefault,lTotal,lPct: integer;
	lG:  TStrings;
	lMaskname: string;
        lMaskHdr: TMRIcroHdr;
begin
     MsgClear;
     Msg(kVers);
     if not OpenDialogExecute('Select images to overlap',true,false,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if  MainForm.OpenHdrDlg.Files.Count < 2 then begin
         lTotal := NIFTIhdr_HdrVolumes(MainForm.OpenHdrDlg.Filename);
         if lTotal < 2 then begin
            Showmessage('Error: This function is designed to overlay MULTIPLE volumes. You selected less than two images.');
            exit;
         end;
         lG:= TStringList.Create;
         for lReps := 1 to lTotal do
             lG.Add(MainForm.OpenHdrDlg.Filename+':'+inttostr(lReps) );
     end else begin
         lG:= TStringList.Create;
         lG.addstrings(OpenHdrDlg.Files);
     end;
     lMaskname := lG[0];
     if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading mask.');
	   goto 666;
     end;
     lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
     if not CheckVoxelsGroupX(lG,lMaskHdr{lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
     end;
     lTotal := lG.Count;
     if lTotal > kMaxObs then
        lTotal := kMaxObs; //this implemmentation uses 126 bits per voxel - we can not test more than this!
     if lTotal > 100 then
        lDefault := 100
     else
         lDefault := lTotal;
     lMax := ReadIntForm.GetInt('Enter maximum number of overlaps to test ', 3,lDefault,lTotal);
     lDefault := lMax div 10;
     if lDefault < 1 then
        lDefault := 1;
     lInc := ReadIntForm.GetInt('Enter overlap increment (e.g. if 5; then 5, 10, 15...) ', 1,lDefault,lMax);
     lReps := ReadIntForm.GetInt('Enter number of times each increment is tested ', 1,10,100);
     lPct := ReadIntForm.GetInt('Only include voxels damaged in N% of patients ', 0,5,100);

     Msg('Voxels = '+inttostr(lMaskVoxels));
     Msg('Scans to permute = '+inttostr(lG.count));
     EvaluatePower (lG,lInc,lMax,lReps,lPct);

     //MakeMean(lG,lMaskHdr, odd((Sender as TMenuItem).tag),false);
     666:
     lG.Free;
end;

{$DEFINE SINGLETHREAD}
{$DEFINE NOTHREAD}

function TMainForm.FirthNPMAnalyze (var lImages: TStrings; var lPredictorList: TStringList; var lMaskHdr: TMRIcroHdr; lnCond,lnCrit: integer; var lSymptomRA: SingleP; var lOutName: string): boolean;
label
     123,667;
var
	lOutNameMod: string;
	lPlankImg: bytep;
        lOutImgSum : singleP;
	lOutImg: SingleRAp;
        {$IFDEF SINGLETHREAD}lnCPUThreads,{$ENDIF}
        lCond,lPos,lPlank,lThread,lnDeficit: integer;
        lTotalMemory,lVolVox,lMinMask,lMaxMask,lnPlanks,lVoxPerPlank,
        lThreadStart,lThreadInc,lThreadEnd, lnLesion,lnPermute,
	lPos2,lPos2Offset,lStartVox,lEndVox,lPlankImgPos,lnTests,lnVoxTested,lPosPct: int64;
	lT,  lSum: double;
	lObsp: pointer;
	lObs: Doublep0;
	lStatHdr: TNIfTIhdr;
	lFdata: file;
        lRanOrderp: pointer;
        lRanOrder: Doublep0;
begin
        if lnCond < 1 then
           exit;
        lnPermute := ReadPermute;
        if lnPermute > 1 then begin
            Msg('NPM does not (yet) support permutation thresholding with Logisitic Regression.');
            lnPermute := 0;
        end;
        {$IFDEF SINGLETHREAD}
        lnCPUThreads := gnCPUThreads;
        if gnCPUThreads > 1 then
           Msg('July 2007 logistic regression will only use 1 thread. You may want to check for a software update');
        gnCPUThreads := 1;
        {$ENDIF}
        Msg('Permutations = ' +IntToStr(lnPermute));
	Msg('Logisitic Regression began = ' +TimeToStr(Now));
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
	Msg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	Msg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
        if (lnPlanks = 1) then
            getmem(lPlankImg,lTotalMemory)
        else
	    getmem(lPlankImg,kPlankSz);
	lStartVox := lMinMask;
	lEndVox := lMinMask-1;
        for lPos := 1 to lImages.Count do
		if gScaleRA[lPos] = 0 then
			gScaleRA[lPos] := 1;
	createArray64(lObsp,lObs,lImages.Count);
        getmem(lOutImgSum,lVolVox* sizeof(single));
	//getmem(lOutImgL,lVolVox* sizeof(single));
        getmem(lOutImg,lnCond*sizeof(Singlep));
        for lCond := 1 to lnCond do begin
            getmem(lOutImg^[lCond],lVolVox* sizeof(single));
            for lPos := 1 to lVolVox do
                lOutImg^[lCond]^[lPos] := 0;
        end;
        //InitPermute (lImages.Count, lnPermute, lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM, lRanOrderp, lRanOrder);
	for lPos := 1 to lVolVox do
                lOutImgSum^[lPos] := 0;
        ClearThreadData(gnCPUThreads,lnPermute) ;
	for lPlank := 1 to lnPlanks do begin
                ProgressBar1.Position := 1;
		Msg('Computing plank = ' +Inttostr(lPlank));
                Refresh;
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
                    {$IFDEF NOTHREAD}
                      FirthAnalyzeNoThread (lnCond, lnCrit,lnPermute,1,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lPlankImg,lOutImgSum,lSymptomRA,lOutImg);
                    //FirthAnalyzeNoThread (lnCond,lnCrit, lnPermute,1,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lPlankImg,lOutImgSum,lSymptomRA,lOutImg);
                    {$ELSE}
                for lThread := 1 to gnCPUThreads do begin
                    if lThread = gnCPUThreads then
                       lThreadEnd := lVoxPerPlank; //avoid integer rounding error
                    with TFirthThreadStat.Create (ProgressBar1,lnCond,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lPlankImg,lOutImgSum,lSymptomRA,lOutImg) do
                         {$IFDEF FPC} OnTerminate := @ThreadDone; {$ELSE}OnTerminate := ThreadDone;{$ENDIF}
                    inc(gThreadsRunning);
                    Msg('Thread ' +Inttostr(gThreadsRunning)+' = '+inttostr(lThreadStart)+'..'+inttostr(lThreadEnd));
                    lThreadStart := lThreadEnd + 1;
                    lThreadEnd :=lThreadEnd + lThreadInc;
                end; //for each thread

                repeat
                      Application.processmessages;
                until gThreadsRunning = 0;
                {$ENDIF} //THREADED
                Application.processmessages;
                //showmessage('Threads done');
                //threading end
		lStartVox := lEndVox + 1;
	end;
        lnVoxTested :=  SumThreadDataLite(gnCPUThreads); //not yet lnVoxTested := SumThreadData(gnCPUThreads,lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM);
        if lnVoxTested < 1 then begin
	   Msg('**Error: no voxels tested: no regions lesioned in at least '+inttostr(lnCrit)+' patients**');
           goto 123;
        end;
        //next report findings
	Msg('Voxels tested = ' +Inttostr(lnVoxTested));
        Msg('Only tested voxels with more than '+inttostr(lnCrit)+' lesions');
        //Next: save results from permutation thresholding....
        reportBonferroni('Std',lnVoxTested);
        //next: save data
(*savedata *)
	MakeHdr (lMaskHdr.NIFTIhdr,lStatHdr);
//save sum map
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Sum','.hdr');
        NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImgSum,1);

        MakeStatHdr (lMaskHdr.NIFTIhdr,lStatHdr,-6, 6,1{df},0,lnVoxTested,kNIFTI_INTENT_ZSCORE,inttostr(lnVoxTested) );
        for lCond := 1 to lnCond do begin
             reportFDR (lPredictorList[lCond-1]+inttostr(lCond), lVolVox, lnVoxTested, lOutImg^[lCond]);
             //reportPermute('L',lnPermute,lPermuteMaxBM, lPermuteMinBM);
             lOutNameMod := ChangeFilePostfixExt(lOutName,lPredictorList[lCond-1]+inttostr(lCond),'.hdr');
             NIFTIhdr_SaveHdrImg(lOutNameMod,lStatHdr,true,not IsNifTiMagic(lMaskHdr.NIFTIhdr),true,lOutImg^[lCond],1);
         end;
123:
//next: free dynamic memory
        //FreePermute (lnPermute,lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM,  lRanOrderp);
        for lCond := 1 to lnCond do
            freemem(lOutImg^[lCond]);
	freemem(lOutImg);

        freemem(lOutImgSum);
	freemem(lObsp);
	freemem(lPlankImg);
	Msg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes','.txt');
        MsgSave(lOutNameMod);

        ProgressBar1.Position := 0;
        {$IFDEF SINGLETHREAD}
        gnCPUThreads := lnCPUThreads;
        {$ENDIF}
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lTotalMemory > 1 then freemem(lPlankImg);
	Msg('Unable to complete analysis.');
        ProgressBar1.Position := 0;
        {$IFDEF SINGLETHREAD}
        gnCPUThreads := lnCPUThreads;
        {$ENDIF}
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
		Msg('Unable to load  ' +lHdr.ImgFileName);
                freemem(lImg);
		exit;
     end;
     result := 0;
     for lVox := 1 to lVolVox do
         if (lImg^[lVox] <> 0) then
            inc(result);
     freemem(lImg);
end;


procedure TMainForm.PenalizedLogisticRegerssion1Click(Sender: TObject);
label
	666;
var
	lVol,lMin,lMax,lI,lFact,lnFactors,lSubj,lnSubj,lMaskVoxels,lnCrit: integer;
	lImageNames:  TStrings;
        lPredictorList: TStringList;
	lTemp4D,lMaskname,lOutName,lStr: string;
	lMaskHdr: TMRIcroHdr;
        lMultiSymptomRA,lTempRA: singleP;
        //lBinomial: boolean;
begin
    // lBinomial := false;
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
   //next, get 1st group
  if not GetValX(lnSubj,lnFactors,lMultiSymptomRA,lImageNames,lnCrit{,binom},lPredictorList) then
     goto 666;
  lTemp4D := CreateDecompressed4D(lImageNames);
  if (lnSubj < 2) or (lnFactors < 1) then begin
     showmessage('This analysis requires at least 2 participants and one factor');
     goto 666;
  end;
  lMaskname := lImageNames[0];
  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading 1st image: '+lMaskname);
	   goto 666;
  end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;
  case MessageDlg('Do you want to add lesion volume as a regressor?', mtConfirmation,
     [mbYes, mbNo], 0) of	{ produce the message dialog box }
     mrYes: begin
             //add a new condition called lesionvolume - create a new larger array for data
             Msg('Computing lesion volumes...');
             lPredictorList.Add('LesionVolume');
             GetMem(lTempRA,lnSubj*lnFactors*sizeof(single));
             for  lI := 1 to (lnSubj*lnFactors) do
                  lTempRA^[lI] := lMultiSymptomRA^[lI];
             Freemem(lMultiSymptomRA);
             GetMem(lMultiSymptomRA,lnSubj*(lnFactors+1)*sizeof(single));
             for  lI := 1 to (lnSubj*lnFactors) do
                  lMultiSymptomRA^[lI] := lTempRA^[lI];
             Freemem(lTempRA);
             //now create the new factor
             lI := lnSubj*lnFactors;
             for lSubj := 1 to lnSubj do
                   lMultiSymptomRA^[lI+lSubj] := ComputeLesionVolume(lImageNames[lSubj-1]);
             //ensure there is variability in this regressor
             lMin := round(lMultiSymptomRA^[lI+1]);
             lMax := round(lMultiSymptomRA^[lI+1]);
             for lSubj := 1 to lnSubj do begin
                 lVol := round(lMultiSymptomRA^[lI+lSubj]);
                 if lVol < lMin then lMin := lVol;
                 if lVol > lMax then lMax := lVol;
             end;
             if (lMin < 0) then begin
	        showmessage('Regression aborted: Error computing lesion volumes.');
	        goto 666;
             end;
             if (lMin = lMax) then begin
	        showmessage('Regression aborted: no variability in lesion volume.');
	        goto 666;
             end;
             inc(lnFactors);
     end; //if user decides to include lesion volume
  end; //case

   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Mask file size too small.');
	   goto 666;
   end;
   lOutName := ExtractFileDirWithPathDelim(lMaskName)+'results';
    SaveHdrDlg.Filename := loutname;
   MsgClear;
   Msg(kVers);
   Msg('Firth Penalized regression is still beta software...');
   Msg('Number of participants: '+inttostr(lnSubj));
   Msg('Number of factors: '+inttostr(lnFactors));
   Msg('Threads: '+inttostr(gnCPUThreads));
   //next - header shows factor names
   lStr :='imagename';
   for lFact := 1 to lnFactors do
       lStr := lStr+','+lPredictorList[lFact-1];
   Msg(lStr);
   For lSubj := 1 to lnSubj do begin
       lStr :='';
       for lFact := 1 to lnFactors do begin
           lStr := lStr+','+realtostr(lMultiSymptomRA^[lSubj+ ((lFact-1)*lnSubj)],2);
       end;
       Msg (lImageNames.Strings[lSubj-1] + ' = '+lStr );
   end;
   Msg('Total voxels = '+inttostr(lMaskVoxels));
   Msg('Only testing voxels damaged in at least '+inttostr(lnCrit)+' individual[s]');
   Msg('Number of Lesion maps = '+inttostr(lnSubj));
   lOutName := lOutName+'.nii.gz';
   if not SaveHdrName ('Base Statistical Map', lOutName) then goto 666;

   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
             showmessage('File dimensions differ from mask.');
	     goto 666;
   end;
   FirthNPMAnalyze (lImageNames,lPredictorList,lMaskHdr,lnFactors,lnCrit, lMultiSymptomRA, lOutName);
    666:
    lImageNames.Free;
    lPredictorList.Free;
    DeleteDecompressed4D(lTemp4D);
end;

function ComputeIntersection ( lAname,lBname: string; var lUnion,lIntersection,lAnotB,lBnotA: integer): boolean;
label 667;
var
	lOutName,lOutNameMod: string;
        lVolVox,lVolVoxA,lVox: integer;
	lImgA,lImgB: SingleP;

        lMaskHdr: TMRIcroHdr;
        lA,lB: boolean;
begin
   lUnion:= 0;
   lIntersection := 0;
   lAnotB := 0;
   lBnotA := 0;
   result := false;
     //read A
   if not NIFTIhdr_LoadHdr(lAname,lMaskHdr) then begin
	   showmessage('Error reading image A - '+lAname);
	   exit;
   end;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then goto 667;
	getmem(lImgA,lVolVox*sizeof(single));
	if not LoadImg(lAname, lImgA, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		msg('Unable to load mask ' +lMaskHdr.ImgFileName);
		goto 667;
	end;
        lVolVoxA := lVolVox;
     //read B
   if not NIFTIhdr_LoadHdr(lBname,lMaskHdr) then begin
	   showmessage('Error reading image B - '+lBname);
	   exit;
   end;
   //fx(666,round(gOffsetRA[0]));
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVoxA <> lVolVox) or (lVolVox < 1) then goto 667;
	getmem(lImgB,lVolVox*sizeof(single));
	if not LoadImg(lBname, lImgB, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		msg('Unable to load mask ' +lMaskHdr.ImgFileName);
		goto 667;
	end;
        for lVox := 1 to lVolVox do begin
            lA := (lImgA^[lVox] <> 0);
            lB := (lImgB^[lVox] <> 0);
            if lA and lB then begin
               //fx(lVox,lImgA^[lVox],lImgB^[lVox]);
               inc(lIntersection);
            end;
            if lA or lB then
               inc(lUnion);
            if lA and not lB then
               inc(lAnotB);
            if lB and not lA then
               inc(lBnotA);

        end;
        freemem(lImgA);
        freemem(lImgB);
     result := true;
     667:
end;

procedure TMainForm.ZtoP1Click(Sender: TObject);
var
lAname,lBname: string; var lUnion,lIntersection,lAnotB,lBnotA: integer;
begin
//removed
           lAName := 'C:\mri\roc\p2.nii.gz';
           lBName := 'C:\mri\roc\RBD35.voi';
           if not ComputeIntersection ( lAName,lBName,lUnion,lIntersection,lAnotB,lBnotA) then
              Msg('Error');
           Msg( lAName+'  '+lBName+' I'+inttostr(lIntersection)+' U'+inttostr(lUnion)+' AnotB'+inttostr(lAnotB)+' BnotA'+inttostr(lBnotA));

end;


procedure TMainForm.ComputeIntersectionandUnion1Click(Sender: TObject);
label
	666;
var
        lUnion,lIntersection,lAnotB,lBnotA,
	lnSubj,lSubj,lMaskVoxels,lnAdditionalFactors: integer;
	lImageNames:  TStrings;
	lMaskname,
        lStr,lOutName: string;
	lMaskHdr: TMRIcroHdr;
    X: PMatrix;
begin
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  MsgClear;
  Msg(kVers);
  Msg('Compute intersection [A and B] and union [A or B] for a series of images');


   if not ReadPairedFilenamesReg(lImageNames,X,lnAdditionalFactors) then exit;
   lnSubj :=lImageNames.Count div 2;
      if lnAdditionalFactors > 1 then
      DelMatrix(X, lnAdditionalFactors, lnSubj);

  lMaskname :=lImageNames[0];
  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showmessage('Error reading first image.');
	   goto 666;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   showmessage('Image file size too small.');
	   goto 666;
   end;

   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   showmessage('File dimensions differ from first image.');
	   goto 666;
   end;


   Msg ('n Subjects = '+inttostr(lnSubj));
   for lSubj := 0 to (lnSubj-1) do begin
       lStr := 'A=,'+lImageNames[lSubj]+',B=,'+lImageNames[lSubj+lnSubj];
       ComputeIntersection ( lImageNames[lSubj],lImageNames[lSubj+lnSubj],lUnion,lIntersection,lAnotB,lBnotA);
       lStr := lStr + ',A and B=,'+inttostr(lIntersection);
       lStr := lStr + ',A or B=,'+inttostr(lUnion);
       lStr := lStr + ',A not B=,'+inttostr(lAnotB);
       lStr := lStr + ',B not A=,'+inttostr(lBnotA);




            Msg(lStr);
   end;

   //Msg('Mask = '+lMaskname);
   //Msg('Total voxels = '+inttostr(lMaskVoxels));
   Msg('Number of observations = '+inttostr(lnSubj));
    666:
        lImageNames.Free;
end; //compute intersection and union


procedure TMainForm.ROCbinomialdeficit1Click(Sender: TObject);
begin
        testROC;
end;

procedure TMainForm.ROCcontinuousdeficit1Click(Sender: TObject);
begin
   testROC2;
end;

function isBinom ( lRA:  singleP; lnObs: integer): boolean;
var
   lI: integer;
begin
     result := false;
     if lnObs < 1 then exit;
     for lI := 1 to lnObs do
         if (lRA^[lI] <> 0) and (lRA^[lI] <> 1) then
            exit;
     result := true;
end;

procedure Means ( lBinomRA,lContRA:  singleP; lnObs: integer);
var
   lI,ln0: integer;
   lMeans0, lMeans1: double;
begin
     lMeans0 := 0;
     lMeans1 := 0;
     ln0 := 0;
     if lnObs < 1 then exit;
     for lI := 1 to lnObs do begin
         if (lBinomRA^[lI] = 0) then begin
            inc(ln0);
            lMeans0 := lMeans0 + lContRA^[lI];
         end else
            lMeans1 := lMeans1 + lContRA^[lI];
     end;
     if ln0 > 0 then
        lMeans0 := lMeans0 / ln0;
     if ln0 < lnObs then
        lMeans1 := lMeans1 / (lnObs-ln0);
     npmform.MainForm.memo1.lines.add('mean volume for '+inttostr(ln0)+' people who scored 0 is = '+floattostr(lmeans0));
     npmform.MainForm.memo1.lines.add('mean volume for '+inttostr(lnObs-ln0)+' people who scored 1 is = '+floattostr(lmeans1));

end;

function AUCbinomcontT (lBinomdataRA,lContdataRA: singlep; lnSubj :integer; var lT: double): double;
var
   lIn : DoubleP0;
   lnGroup0,lnGroup1,lI: integer;
begin
   result := 0.5;
   if lnSubj < 1 then
      exit;
   Getmem(lIn,lnSubj*sizeof(double));
   lnGroup0 := 0;
   lnGroup1 := 0;
   for lI := 1 to lnSubj do begin
       if lBinomdataRA^[lI] = 0 then begin
          lIn^[lnGroup0] := lContdataRA^[lI];
          inc (lnGroup0);
       end else begin
          inc (lnGroup1);
          lIn^[lnSubj-lnGroup1] := lContdataRA^[lI];

       end;
   end;
   result := continROC (lnSubj, lnGroup0, lIn);
   TStat2 (lnSubj, lnGroup0, lIn,lT);
   freemem(lIn);
end;


procedure Contrast(lBehavName,lROIname: string;  lBehavRA,lLesionVolRA: singleP; lnSubj: integer);
var
   lDF: integer;
   lROC,lT,lP: double;
begin
     if isBinom (lBehavRA,lnSubj) then begin
        lROC :=  AUCbinomcontT (lBehavRA,lLesionVolRA, lnSubj,lT);
        lDF := lnSubj-2;
        lP := pTdistr(lDF,lT);
        Means ( lBehavRA,lLesionVolRA, lnSubj);

        npmform.MainForm.memo1.lines.add('ROI=,'+lROIname+',Behav=,'+lBehavName+', Area Under Curve=,'+floattostr(lROC)+', T('+inttostr(lDF)+')=,'+floattostr(lT)+',p<,'+floattostr(lp));
     end else begin
        lROC :=  AUCcontcont (lBehavRA,lLesionVolRA, lnSubj);
        npmform.MainForm.memo1.lines.add('ROI=,'+lROIname+',Behav=,'+lBehavName+', Area Under Curve = '+floattostr(lROC));
     end;
    //xxx
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
      showmessage('Error: no lesion names');
      exit;
   end;
   for lLesion := 1 to lnLesions do
       lFracROIinjured^[lLesion] := 0;
     //read A
   if not NIFTIhdr_LoadHdr(lROIname,lMaskHdr) then begin
	   showmessage('Error reading ROI - '+lROIname);
	   exit;
   end;
   lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
   if (lVolVox < 1) then begin
      showmessage('Error with Mask voxels ' + inttostr(lVolVox));
      exit;
   end;
   if not CheckVoxelsGroupX(lLesionNames, lMaskHdr) then begin
      showmessage('Error image dimensions vary.');
      exit;
   end;
   getmem(lROIImg,lVolVox*sizeof(single));
   getmem(lImgB,lVolVox*sizeof(single));
   if not LoadImg(lROIname, lROIImg, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		MainForm.NPMmsg('Unable to load lesion ' +lMaskHdr.ImgFileName);
		goto 667;
   end;
   lVolVoxA := lVolVox;
   for lVox := 1 to lVolVox do
       if  (lROIImg^[lVox] > 0) then
           lROIvol := lROIvol +lROIImg^[lVox];
   //read Lesion
   if lROIvol < 1 then begin
      		MainForm.NPMmsg('ROI volume < 1');
		goto 667;
   end;
   //for each lesion
   //MainForm.NPMmsg('Compute overlap started '+inttostr(lnLesions)+'  '+floattostr(lROIvol)+'  '+inttostr(lVolVoxA));
   MainForm.ProgressBar1.Position := 0;
   for lLesion := 1 to lnLesions do begin
         MainForm.ProgressBar1.Position := round((lLesion/lnLesions)*100) ;
         lSum := 0;
         lName := lLesionNames.Strings[lLesion-1];
         if not NIFTIhdr_LoadHdr(lName,lMaskHdr) then begin
	   showmessage('Error reading lesion - '+lName);
	   exit;
         end;
         lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
         if (lVolVoxA <> lVolVox) or (lVolVox < 1) then begin
            MainForm.NPMmsg('Volume does not have expected number of voxels ' +lMaskHdr.ImgFileName +'<>'+lROIname+ ' found ' +inttostr(lVolVox)+'  expected '+inttostr(lVolVoxA));
            goto 667;
         end;
	if not LoadImg(lName, lImgB, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		MainForm.NPMmsg('Unable to load mask ' +lMaskHdr.ImgFileName);
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
   MainForm.ProgressBar1.Position := 0;

   (*for lLesion := 1 to lnLesions do begin
       if lFracROIinjured^[lLesion] > 0 then
          fx( lFracROIinjured^[lLesion], lLesion);
   end;    *)

   667:

   freemem(lImgB);
   freemem(lROIImg);
end;


procedure TMainForm.ROIanalysis1Click(Sender: TObject);
label
	666;
var
        lROI,lnROI,lVol,lMin,lMax,lI,lFact,lnFactors,lSubj,lnSubj,lMaskVoxels,lnCrit: integer;
	lROInames,lImageNames:  TStrings;
        lPredictorList: TStringList;
	lVolStr,lTemp4D,lOutName,lStr: string;
        lBehav: single;
        lROIvolRA: doubleP;
        lMultiSymptomRA,lLesionVolRA,lBehavRA: singleP;
        lError: boolean;
begin
     if not OpenDialogExecute('Select regions of interest',true,false,kImgPlusVOIFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     lROInames:= TStringList.Create;
     lROInames.addstrings(OpenHdrDlg.Files);
     lnROI := lROINames.Count;
     if lnROI < 1 then begin
         showmessage('You need to select at least one ROI.');
         exit;
     end;
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
  if not GetValX(lnSubj,lnFactors,lMultiSymptomRA,lImageNames,lnCrit,lPredictorList) then
     goto 666;
  lTemp4D := CreateDecompressed4D(lImageNames);
  if (lnSubj < 1) or (lnFactors < 1) then begin
     showmessage('This analysis requires at least 1 participant and one factor');
     goto 666;
  end;
   MsgClear;
   Msg(kVers);
	MainForm.NPMmsg('Analysis began = ' +TimeToStr(Now));
   Msg('VAL file name: '+MainForm.OpenHdrDlg.Filename);
   Msg('Number of participants: '+inttostr(lnSubj));
   Msg('Number of factors: '+inttostr(lnFactors));
   Msg('Number of Lesion maps = '+inttostr(lnSubj));
   //next - header shows factor names
   lStr :='imagename';
   for lFact := 1 to lnFactors do
       lStr := lStr+','+lPredictorList[lFact-1];
   for lROI := 1 to lnROI do
       lStr := lStr+','+lROInames[lROI-1];
   Msg(lStr+',LesionVolume');
   lError := false;
   Getmem(lROIVolRA, lnSubj*lnROI*sizeof(double));
   Getmem(lLesionVolRA, lnSubj*lnROI*sizeof(single));
   Getmem(lBehavRA, lnSubj*lnFactors*sizeof(single));
   for lROI := 1 to lnROI do begin
       //if not ComputeIntersection ( lImageNames.Strings[lSubj-1],lROInames[lROI-1],lUnion,lIntersection,lAnotB,lBnotA) then
       if not ComputeOverlap (lROInames[lROI-1],lImageNames, lROIvolRA^[lROI], singlep(@lLesionVolRA^[((lROI-1)*lnSubj)+1])) then begin
          MainForm.NPMmsg('Error computing overlap');
          goto 666;
       end;
   end;
   For lSubj := 1 to lnSubj do begin
       lStr :='';
       for lFact := 1 to lnFactors do begin
           lBehav := lMultiSymptomRA^[lSubj+ ((lFact-1)*lnSubj)];
           lStr := lStr+','+realtostr(lBehav,2);
           lBehavRA^[((lFact-1)*lnSubj) +lSubj] := lBehav;
       end;
       for lROI := 1 to lnROI do
              lStr := lStr+','+floattostr(lLesionVolRA^[((lROI-1)*lnSubj) +lSubj]);
       lVolStr := floattostr(ComputeLesionVolume(lImageNames.Strings[lSubj-1]));
       Msg (lImageNames.Strings[lSubj-1] + ' = '+lStr +','+lVolStr );
   end;
   (*   For lSubj := 1 to lnSubj do begin
       lStr :='';
       for lFact := 1 to lnFactors do begin
           lBehav := lMultiSymptomRA^[lSubj+ ((lFact-1)*lnSubj)];
           lStr := lStr+','+realtostr(lBehav,2);
           lBehavRA^[((lFact-1)*lnSubj) +lSubj] := lBehav;
       end;
       for lROI := 1 to lnROI do begin
           if ComputeIntersection ( lImageNames.Strings[lSubj-1],lROInames[lROI-1],lUnion,lIntersection,lAnotB,lBnotA) then begin
              lStr := lStr+','+inttostr(lIntersection);
              lLesionVolRA^[((lROI-1)*lnSubj) +lSubj] := lIntersection;
           end else begin
               lError:= true;
               lStr := lStr+',error';
           end;
           //Msg( lImageNames.Strings[lSubj-1]+'  '+lROInames[lROI-1]+' I'+inttostr(lIntersection)+' U'+inttostr(lUnion)+' AnotB'+inttostr(lAnotB)+' BnotA'+inttostr(lBnotA));

       end;
       Msg (lImageNames.Strings[lSubj-1] + ' = '+lStr );
   end;*)
   for lROI := 1 to lnROI do begin
       for lFact := 1 to lnFactors do begin
           Contrast(lPredictorList[lFact-1],lROInames[lROI-1],singlep(@lBehavRA^[((lFact-1)*lnSubj)+1]),singlep(@lLesionVolRA^[((lROI-1)*lnSubj)+1]),lnSubj);//,((lFact-1)*lnSubj),((lROI-1)*lnSubj));
       end; //for each factor
   end; //for each ROI
   for lROI := 1 to lnROI do begin
       Msg( lROInames[lROI-1] +' volume = '+floattostr(lROIvolRA^[lROI]) )
   end; //for each ROI
   Freemem(lLesionVolRA);
   Freemem(lBehavRA);
   Freemem(lROIvolRA);

666:
    lROInames.free;
    lImageNames.Free;
    lPredictorList.Free;
    DeleteDecompressed4D(lTemp4D);
  	MainForm.NPMmsg('Analysis finished = ' +TimeToStr(Now));
end;
                    

procedure TMainForm.Masked1Click(Sender: TObject);
var
        lFilename,lMaskname: string;
	lPos:  Integer;
begin
     MsgClear;
     Msg(GetKVers);
     if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
	   showmessage('NPM aborted: mask selection failed.');
	   exit;
     end; //if not selected
     lMaskname := OpenHdrDlg.Filename;
     if not OpenDialogExecute('Select images for intensity normalization',true,false,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if OpenHdrDlg.Files.Count < 1 then
        exit;

     for lPos := 1 to OpenHdrDlg.Files.Count do begin
         lFilename := OpenHdrDlg.Files[lPos-1];
         balance(lFilename,lMaskname,(Sender as TMenuItem).tag);
     end;
end;

function Binarize (var lImageName:String; lNonZeroVal: integer; lZeroThresh: boolean): boolean;
var
   lImg8: ByteP;
   lImg: SingleP;
   lHdr: TMRIcroHdr;
   lVolVox,lVox: integer;
   lMin,lMax: single;
   lModeLo,lModeHi,lIntercept,lSlope: single;
   lOutNameMod: string;
begin
	//lOutName := lMaskHdr.ImgFileName;
        result := false;
	//if not SaveHdrName ('Statistical Map', lOutName) then exit;
        if not NIFTIhdr_LoadHdr(lImageName,lHdr) then begin
	   showmessage('Error reading '+lImageName);
	   exit;
        end;
	lVolVox := lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]* lHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then exit;
	getmem(lImg,lVolVox*sizeof(single));
	getmem(lImg8,lVolVox*sizeof(byte));
	if not LoadImg(lHdr.ImgFileName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		Msg('Unable to load  ' +lHdr.ImgFileName);
		exit;
	end;
        lHdr.NIFTIhdr.scl_slope := 1;
        lHdr.NIFTIhdr.scl_inter := 0;
if lZeroThresh then begin
        lOutNameMod :=  ChangeFilePrefixExt(lImageName,'i','.nii');

           lMin := 0;
           lMax := 0
end else begin
        lOutNameMod :=  ChangeFilePrefixExt(lImageName,'i','.voi');

        lMin := lIMg^[1];
        for lVox := 1 to lVolVox do
            if lImg^[lVox] < lMin then lMin := lIMg^[lVox];

        lMax := lIMg^[1];
        for lVox := 1 to lVolVox do
            if lImg^[lVox] > lMax then lMax := lIMg^[lVox];
        for lVox := 1 to lVolVox do
            lImg8^[lVox] := 0;
        lMax := ((lMax-lMin) / 2)+lMin;
end;
        for lVox := 1 to lVolVox do
            if lImg^[lVox] > lMax then
                        lImg8^[lVox] := lNonZeroVal;
        Msg('Creating  ' +lOutNameMod+' Threshold = '+floattostr(lMax));
        NIFTIhdr_SaveHdrImg8(lOutNameMod,lHdr.NIFTIhdr,true,not IsNifTiMagic(lHdr.NIFTIhdr),true,lImg8,1);
        freemem(lIMg8);
	freemem(lImg);

end;


procedure TMainForm.Binarizeimages1Click(Sender: TObject);
var
        lFilename: string;
	lPos:  Integer;
begin
     MsgClear;
     Msg(GetKVers);
     if not OpenDialogExecute('Select images for intensity normalization',true,false,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if OpenHdrDlg.Files.Count < 1 then
        exit;
     for lPos := 1 to OpenHdrDlg.Files.Count do begin
         lFilename := OpenHdrDlg.Files[lPos-1];
         Binarize(lFilename,1,false);
         //Binarize (var lImageName:String; lNonZeroVal: integer; lZeroThresh: boolean): boolean;
     end;
     Msg('Done');
end;

procedure TMainForm.Resliceimagetoneworientationandboundingbox1Click(
  Sender: TObject);
begin
(*  var
   lSourcename,lTargetName: string;
   lPos: integer;
begin
     MsgClear;
     Msg(GetKVers);
     Msg('This function will transform a source image to match a target image.');
     Msg(' The resliced image will have the voxel size, orientation and bounding box of the target.');
     Msg('You will be prompted to select a target image as well as source images.');
     Msg(' The resliced image will have the prefix ''r'', e.g. c:\source.nii -> c:\rsource.nii.');
     if not OpenDialogExecute('Select target image (source images will be resliced to match target)',false,false,kImgFilter) then begin
	   showmessage('reslice aborted: target selection failed.');
	   exit;
     end; //if not selected
     lTargetName := OpenHdrDlg.Filename;
     if not OpenDialogExecute('Select source images for reslicing',true,false,kImgFilter) then begin
	   showmessage('reslice aborted: source selection failed.');
	   exit;
     end; //if not selected
     if OpenHdrDlg.Files.Count < 1 then
        exit;
     for lPos := 1 to OpenHdrDlg.Files.Count do begin
         lSourcename := OpenHdrDlg.Files[lPos-1];

         xxBinarize(lFilename);
     end;
     Msg('Done');
        *)
end;


procedure TMainForm.Setnonseroto1001Click(Sender: TObject);
var
        lFilename: string;
	lPos:  Integer;
begin
     MsgClear;
     Msg(GetKVers);
     if not OpenDialogExecute('Select images for intensity normalization',true,false,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if OpenHdrDlg.Files.Count < 1 then
        exit;
     for lPos := 1 to OpenHdrDlg.Files.Count do begin
         lFilename := OpenHdrDlg.Files[lPos-1];
         Binarize(lFilename,100,true);
         //Binarize (var lImageName:String; lNonZeroVal: integer; lZeroThresh: boolean): boolean;
     end;        
end;

procedure TMainForm.Savetext1Click(Sender: TObject);
begin
	 SaveHdrDlg.Title := 'Save file as comma separated values (to open with Excel)';
	 SaveHdrDlg.Filter := 'Comma Separated (*.csv)|*.csv|Text (*.txt)|*.txt';
         SaveHdrDlg.DefaultExt := '*.csv';
	 if not SaveHdrDlg.Execute then exit;
	 Memo1.Lines.SaveToFile(SaveHdrDlg.Filename);
end;

procedure TMainForm.AnaCOMmenuClick(Sender: TObject);
begin
{$IFDEF compileANACOM}
     DoAnaCOM;
{$ENDIF}
end;

procedure TMainForm.MonteCarloSimulation1Click(Sender: TObject);
begin
{$IFDEF benchmark}
        LesionMonteCarlo (false,true,true);
{$ENDIF}
end;

function TMainForm.MakeSubtract (lPosName,lNegName: string): boolean;
var
   lNegImg,lImg,lOutImg: SingleP;
   lHdr,lNegHdr: TMRIcroHdr;
   lVolVox,lVox: integer;
   lOutNameMod: string;
begin
        result := false;
        if not NIFTIhdr_LoadHdr(lPosName,lHdr) then begin
	   showmessage('Error reading '+lPosName);
	   exit;
        end;
	lVolVox := lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]* lHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then exit;
	getmem(lImg,lVolVox*sizeof(single));
	if not LoadImg(lHdr.ImgFileName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		Msg('Unable to load  ' +lHdr.ImgFileName);
		exit;
	end;

        if not NIFTIhdr_LoadHdr(lNegName,lNegHdr) then begin
	   showmessage('Error reading '+lNegName);
	   exit;
        end;
	if lVolVox <> (lNegHdr.NIFTIhdr.dim[1]*lNegHdr.NIFTIhdr.dim[2]* lNegHdr.NIFTIhdr.dim[3]) then begin
           showmessage('Volumes differ');
           exit;

        end;
	getmem(lImg,lVolVox*sizeof(single));
	if not LoadImg(lHdr.ImgFileName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		Msg('Unable to load  ' +lHdr.ImgFileName);
		exit;
	end;
	getmem(lNegImg,lVolVox*sizeof(single));
	if not LoadImg(lNegHdr.ImgFileName, lNegImg, 1, lVolVox,round(lNegHdr.NIFTIhdr.vox_offset),1,lNegHdr.NIFTIhdr.datatype,lVolVox) then begin
		Msg('Unable to load  ' +lNegHdr.ImgFileName);
		exit;
	end;
        getmem(lOutImg,lVolVox*sizeof(single));
        for lVox := 1 to lVolVox do
            lOutImg^[lVox] := lImg^[lVox] - lNegImg^[lVox];


        lHdr.NIFTIhdr.scl_slope := 1;
        lHdr.NIFTIhdr.scl_inter := 0;
        lOutNameMod :=  ChangeFilePrefixExt(lPosName,'subtract_','.hdr');
        Msg(lPosName+' - ' + lNegName+ '  = '+lOutNameMod);
        NIFTIhdr_SaveHdrImg(lOutNameMod,lHdr.NIFTIhdr,true,not IsNifTiMagic(lHdr.NIFTIhdr),true,lOutImg,1);
        //end optional
        //NIFTIhdr_SaveHdr(lHdr.HdrFilename,lHdr.NIFTIhdr,true,not IsNifTiMagic(lHdr.NIFTIhdr));

	freemem(lImg);
	freemem(lOutImg);
	freemem(lNegImg);
end;//makesubtract

procedure TMainForm.Subtract1Click(Sender: TObject);
var
   lPosName,lNegName: string;
begin
     if not OpenDialogExecute('Select positive',false,false,kImgPlusVOIFilter) then
	   exit;
     lPosName := OpenHdrDlg.FileName;
     if not OpenDialogExecute('Select negative',false,false,kImgPlusVOIFilter) then
	   exit;
     lNegName := OpenHdrDlg.FileName;
     MakeSubtract (lPosName,lNegName);

end;






procedure TMainForm.LogPtoZ1Click(Sender: TObject);
var
        lFilename: string;
	lPos:  Integer;
begin
     MsgClear;
     Msg(GetKVers);
     if not OpenDialogExecute('Select images for intensity normalization',true,false,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if OpenHdrDlg.Files.Count < 1 then
        exit;
     for lPos := 1 to OpenHdrDlg.Files.Count do begin
         lFilename := OpenHdrDlg.Files[lPos-1];
         //LogPToZ(lFilename,1,false);

     end;
     Msg('Done');
end;

  {$IFDEF UNIX}


initialization
  {$I npmform.lrs}
{$ELSE} //not unix: windows
initialization
{$IFDEF FPC}
  {$I npmform.lrs}
 {$ENDIF}//FPC
  OleInitialize(nil);

finalization
  OleUninitialize
{$ENDIF} //Windows

end.
