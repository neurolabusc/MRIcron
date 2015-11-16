unit npmform;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
{$DEFINE SINGLETHREAD}
//{$DEFINE FIRTHNOTHREAD}
interface
{$I options.inc}
uses
 define_types,SysUtils,
part,StatThds,statcr,StatThdsUtil,Brunner,DISTR,nifti_img,
   Messages,         userDir,
  Classes, Graphics, Controls, Forms, DialogsX,Dialogs, nifti_types ,
  Menus, ComCtrls, ExtCtrls, StdCtrls,
overlap,ReadInt,lesion_pattern,stats,LesionStatThds,nifti_hdr,

{$IFDEF FPC} LResources,gzio2,
{$ELSE} gziod,associate,{$ENDIF}   //must be in search path, e.g. C:\pas\mricron\npm\math
{$IFNDEF UNIX} Windows,
  {$ELSE}
  LCLType,
  {$ENDIF}
upower,firthThds,firth,IniFiles,cpucount,math,
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
    //PlankSzMenuItem1: TMenuItem;
    DualImageCorrelation1: TMenuItem;
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
    Options1: TMenuItem;
    PairedTMenu: TMenuItem;
    PenalizedLogisticRegerssion1: TMenuItem;
    Permutations1: TMenuItem;
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
    //StartTimer: TTimer;
    ttestmenu: TMenuItem;
    Utilities1: TMenuItem;
    Variance1: TMenuItem;
    VBM1: TMenuItem;
    VLSM1: TMenuItem;
    Intensitynormalization1: TMenuItem;
    Masked1: TMenuItem;
    MaskedintensitynormalizationA1: TMenuItem;
    MaskedintensitynormalizationB1: TMenuItem;
    Binarizeimages1: TMenuItem;
    PlankSzMenuItem1: TMenuItem;
    //Setnonseroto1001: TMenuItem;
    //AnaCOMmenu: TMenuItem;
    //MonteCarloSimulation1: TMenuItem;
    //Subtract1: TMenuItem;
    //LogPtoZ1: TMenuItem;
    procedure PlankSzMenuItem1Click(Sender: TObject);
    procedure NPMmsgUI( lStr: string);
    procedure NPMmsgClearUI;
    procedure NPMmsgSaveUI(lFilename: string);
    //procedure ProcessParamStr;
    function GetValX (var lnSubj, lnFactors: integer; var lSymptomRA: singleP; var lImageNames:  TStrings; var lCrit: integer; {lBinomial : boolean;} var lPredictorList: TStringList):boolean;
    function FirthNPMAnalyze (var lImages: TStrings; var lPredictorList: TStringList; var lMaskHdr: TMRIcroHdr; lnCond,lnCrit: integer; var lSymptomRA: SingleP; var lOutName: string): boolean;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    function SaveHdrName (lCaption: string; var lFilename: string): boolean;
    procedure NPMclick(Sender: TObject);
    function OpenDialogExecute (lCaption: string;lAllowMultiSelect,lForceMultiSelect: boolean; lFilter: string): boolean;//; lAllowMultiSelect: boolean): boolean;
    //function NPMAnalyze (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lMaskVoxels,lnGroup1: integer): boolean;
    //function NPMAnalyzePaired (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lMaskVoxels: integer): boolean;
    procedure FormCreate(Sender: TObject);
    //function MakeSubtract (lPosName,lNegName: string): boolean;
    //function MakeMean (var lImages: TStrings; var lMaskHdr: TMRIcroHdr; lBinarize,lVariance: boolean): boolean;
    //function Balance (var lImageName,lMaskName: String; lMethod: integer{lInflection: boolean}): boolean;
    procedure LesionBtnClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    //procedure StartTimerTimer(Sender: TObject);
    procedure testmenuclick(Sender: TObject);
    procedure radiomenuclick(Sender: TObject);
    procedure Makemeanimage1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Balance1Click(Sender: TObject);

    procedure Variance1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Design1Click(Sender: TObject);
    procedure DualImageCorrelation1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PairedTMenuClick(Sender: TObject);
    procedure SingleSubjectZScores1Click(Sender: TObject);
    procedure MultipleRegressClick(Sender: TObject);
    function ReadPermute: integer;
    procedure SingleRegressClick(Sender: TObject);
    procedure AssociatevalfileswithNPM1Click(Sender: TObject);
    procedure threadChange(Sender: TObject);
    //procedure Countlesionoverlaps1Click(Sender: TObject);
    procedure PenalizedLogisticRegerssion1Click(Sender: TObject);
    //procedure ROCbinomialdeficit1Click(Sender: TObject);
    //procedure ROCcontinuousdeficit1Click(Sender: TObject);
    procedure ThreadDone(Sender: TObject);
    procedure ROIanalysis1Click(Sender: TObject);
    procedure Masked1Click(Sender: TObject);
    procedure Binarizeimages1Click(Sender: TObject);
    procedure Setnonseroto1001Click(Sender: TObject);
    procedure Savetext1Click(Sender: TObject);
    //procedure Subtract1Click(Sender: TObject);
  private
  { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
implementation

uses unpm, filename,prefs,hdr,roc,regression,valformat {$IFDEF SPREADSHEET}  ,design,spread{$ENDIF}
{$IFNDEF UNIX},ActiveX {$ENDIF};
{$IFNDEF FPC}
{$R *.DFM}
  {$ENDIF}

(*function WarnIfLowNCrit(lnSubj,lnCrit: integer): boolean;
//returns true if warning generated
begin
     result := (round(lnSubj * 0.15) ) > lnCrit; //15%
     if result then
        Showmessage('Warning: low statistical power as tests computed for voxels damaged in at least '+inttostr(lnCrit) +' people. Solution: change Design value "Ignore voxels damaged in less than N%".');

end; *)

procedure TMainForm.NPMmsgUI( lStr: string);
begin
    Memo1.Lines.add(lStr);
end;

procedure TMainForm.PlankSzMenuItem1Click(Sender: TObject);
var
  str : string;
  v,max: integer;
begin
  {$IFDEF CPU32}
  max := 1536;
  {$ELSE}
  max := 8000;
  {$ENDIF}

  str := inttostr(gNPMPrefs.PlankMB);
  if not InputQuery('Specify cache size', 'Mb for computation (256..'+inttostr(max)+')', str) then exit;
  try
     v := StrToInt(str);    // Trailing blanks are not supported
   except
     on Exception : EConvertError do begin
       ShowMessage(Exception.Message);
       exit;
     end;
   end;
   if (v < 256) then
      v := 256;
   if v > max then
      v := max;
   gNPMPrefs.PlankMB := v;
   NPMMsgClear;
   NPMMsg(GetKVers);
   ComputePlankSize(gNPMPrefs.PlankMB);
end;

procedure TMainForm.NPMmsgClearUI;
begin
    Memo1.Lines.Clear;
end;


procedure TMainForm.NPMMsgSaveUI(lFilename: string);
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
        WriteIniFile;
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
			ShowMsg('Error: This function is designed to overlay MULTIPLE images. You selected less than two images.');
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
	lMaskname, lOutName: string;
	lMaskHdr: TMRIcroHdr;
begin
  if (not ttestmenu.checked)  and (not BMmenu.checked) then begin
      ShowMsg('Error: you need to compute at least on test [options/test menu]');
      exit;
  end;

  if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
	   ShowMsg('NPM aborted: mask selection failed.');
	   exit;
  end; //if not selected
  lMaskname := OpenHdrDlg.Filename;
  (*if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   showMsg('Error reading mask.');
	   exit;
  end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   ShowMsg('Mask file size too small.');
	   exit;
   end; *)

   //next, get 1st group
   if not OpenDialogExecute('Select postive group (Z scores positive if this group is brighter)',true,true,kImgFilter) then begin
	   ShowMsg('NPM aborted: file selection failed.');
	   exit;
   end; //if not selected
   lG:= TStringList.Create; //not sure why TStrings.Create does not work???
   lG.addstrings(OpenHdrDlg.Files);
   lnGroup1 :=OpenHdrDlg.Files.Count;

   //next, get 2nd group
   if not OpenDialogExecute('Select negative group (Z scores negative if this group is brighter)',true,true,kImgFilter) then begin
	   ShowMsg('NPM aborted: file selection failed.');
	   goto 666;
   end; //if not selected
   lG.addstrings(OpenHdrDlg.Files);
   if not CheckVoxelsGroupX(lG,lMaskHdr {lMaskVoxels}) then begin
	   ShowMsg('File dimensions differ from mask.');
	   goto 666;
   end;
   lOutName := lMaskHdr.ImgFileName;
   if not SaveHdrName ('Statistical Map', lOutName) then exit;
   NPMAnalyze(lG,lMaskName,lMaskVoxels,lnGroup1,gNPMPrefs,lOutName);
   666:
   lG.Free;
end;

function TMainForm.GetValX (var lnSubj, lnFactors: integer; var lSymptomRA: singleP; var lImageNames:  TStrings; var lCrit: integer;  var lPredictorList: TStringList):boolean;
//warning: you MUST free lPredictorList
var
   lVALFilename: string;
   lCritPct: integer;
begin
     lPredictorList := TStringList.Create;
     result := false;
     lnSubj := 0;
     if not MainForm.OpenDialogExecute('Select MRIcron VAL file',false,false,'MRIcron VAL (*.val)|*.val') then begin
	   ShowMsg('NPM aborted: VAL file selection failed.');
	   exit;
     end; //if not selected
     lVALFilename := MainForm.OpenHdrDlg.Filename;
     result := GetValCore ( lVALFilename, lnSubj, lnFactors, lSymptomRA, lImageNames, lCrit,lCritPct{,binom},lPredictorList);
end;

procedure TMainForm.Copy1Click(Sender: TObject);
begin
  	Memo1.SelectAll;
	Memo1.CopyToClipboard;

end;

(*procedure TMainForm.StartTimerTimer(Sender: TObject);
begin
     if StartTimer.Tag < 2 then begin
        StartTimer.tag := StartTimer.tag + 1;
         exit;

     end;

     StartTimer.Enabled := false;
     //if (ParamCount > 0) then  ProcessParamStr;

end; *)

procedure TMainForm.testmenuclick(Sender: TObject);
begin
     (sender as TMenuItem).checked := not  (sender as TMenuItem).checked;
     gNPMprefs.BMtest := BMmenu.Checked;
     gNPMprefs.ttest := TTestmenu.Checked;
end;

procedure TMainForm.radiomenuclick(Sender: TObject);
begin
     (sender as tmenuitem).checked := true;
     gNPMprefs.nPermute:= readPermute;
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

   ttestmenu.checked := gNPMprefs.ttest;
   bmmenu.Checked:= gNPMprefs.BMtest;
   WritePermute(gNPMprefs.nPermute);
   WriteThread(gnCPUThreads);
end;

(*procedure TMainForm.Makemeanimage1Click(Sender: TObject);
label
	666;
var
	lG:  TStrings;
	 loutname: string;
begin

     if not OpenDialogExecute('Select images to average',true,true,kImgFilter) then begin
	   ShowMsg('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if not SaveHdrName ('Output image', lOutName) then exit;
     lG:= TStringList.Create;
     lG.addstrings(OpenHdrDlg.Files);


     MakeMean(lG,odd((Sender as TMenuItem).tag),false,loutname);
     666:
     lG.Free;
end; *)
procedure TMainForm.Makemeanimage1Click(Sender: TObject);
var
	 loutname: string;
begin

     if not OpenDialogExecute('Select images to average666',true,true,kImgFilter) then begin
	   ShowMsg('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     if not SaveHdrName ('Output image', lOutName) then exit;
     MakeMean(OpenHdrDlg.Files,odd((Sender as TMenuItem).tag),false,loutname);
end;


procedure TMainForm.Exit1Click(Sender: TObject);
begin
     Close;
end;

(*procedure CopyFileEXoverwrite (lInName,lOutName: string);
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
end;*)

procedure TMainForm.Balance1Click(Sender: TObject);
var
        lFilename,lMaskName: string;
	lPos:  Integer;
begin
     NPMMsgClear;
     NPMMsg(GetKVers);
     if not OpenDialogExecute('Select images for intensity normalization',true,false,kImgFilter) then begin
	   ShowMsg('NPM aborted: file selection failed.');
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


procedure TMainForm.Variance1Click(Sender: TObject);
label
	666;
var
	lMaskVoxels: integer;
	lG:  TStrings;
	lMaskname,loutname: string;
        lMaskHdr: TMRIcroHdr;
begin
     NPMMsgClear;
     NPMMsg(GetKVers);
     if not OpenDialogExecute('Select 2 images)',true,true,kImgFilter) then begin
	   ShowMsg('NPM aborted: file selection failed.');
	   exit;
     end; //if not selected
     lG:= TStringList.Create;
     lG.addstrings(OpenHdrDlg.Files);
     if lG.count <> 2 then begin
         ShowMsg('You must select exactly two image.');
         goto 666;
     end;
     if not SaveHdrName ('Output image', lOutName) then exit;
     MakeMean(lG, odd((Sender as TMenuItem).tag),true,loutname);
     666:
     lG.Free;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
     ShowMsg(GetkVers );
end;

procedure TMainForm.Design1Click(Sender: TObject);
begin
{$IFDEF SPREADSHEET} SpreadForm.Show; {$ELSE} ShowMsg('Spreadsheet not yet supported on the Operating System');{$ENDIF}
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
                ShowMsg('Empty cells? Error reading TXT file row:'+inttostr(lRow)+' col:'+inttostr(lCol)+' - Unable to convert the string '+lNumStr+' to a number');
             exit;
          end;
    end;
    //fx(lRow,lCol,lTempFloat);
    X^[lCol]^[lRow] := lTempFloat;
    lNumStr := '';
    result := true;
end;

function    ReadPairedFilenamesReg(var lImageNames: TStrings; var X : PMatrix; var  lnAdditionalFactors: integer): boolean;
var
   lLen,lPos,lSep,lMaxSep,lLine: integer;
   lFilenames,lF1,lF2,lNumStr: string;
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
                        ShowMsg('Can not find image '+lF2);
                 end else //F1 exists
                     ShowMsg('Can not find image '+lF1);
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
                        //ShowMsg(lNumStr);
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
  NPMMsgClear;
  NPMMsg(GetKVers);

  NPMMsg('Dual-image Linear Regression [Weighted Least Squares]');
  if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
	   ShowMsg('NPM aborted: mask selection failed.');
	   goto 666;
   end; //if not selected
   lMaskname := OpenHdrDlg.Filename;

  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   ShowMsg('Error reading Mask image.');
	   goto 666;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   ShowMsg('Mask file size too small.');
	   goto 666;
   end;
   if not ReadPairedFilenamesReg(lImageNames,X,lnAdditionalFactors) then exit;
   lnSubj :=lImageNames.Count div 2;

   //fx(lnAdditionalFactors);
   //show matrix
   //MsgStrings (lImageNames);
   NPMMsg ('n Subjects = '+inttostr(lnSubj));
   for lSubj := 0 to (lnSubj-1) do begin
       lStr := lImageNames[lSubj]+' <-> '+lImageNames[lSubj+lnSubj];
       if lnAdditionalFactors > 0 then
          for lI := 1 to lnAdditionalFactors do
              lStr := lStr+','+floattostr(X^[lI]^[lSubj+1]);


            NPMMsg(lStr);
   end;
   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   ShowMsg('File dimensions differ from mask.');
	   goto 666;
   end;


   NPMMsg('Mask = '+lMaskname);
   NPMMsg('Total voxels = '+inttostr(lMaskVoxels));
   NPMMsg('Number of observations = '+inttostr(lnSubj));

   if lnSubj < 5 then begin
      ShowMsg('Paired regression error: Requires at least 5 images per group.');
      goto 666;
   end;
   lOutName := lMaskName;
   if not SaveHdrName ('Base Statistical Map', lOutName) then exit;
   //ShowMsg('Unimplemented Regress');//
   Regress2NPMAnalyze (lImageNames, lMaskHdr, lOutname,X,lnAdditionalFactors,gNPMprefs.nPermute);
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
     lPrefs.NULP := gNPMPrefs.NULP;
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
        ShowMsg('Error: you need to compute at least on test [options/test menu]');
        exit;
     end; code above defaults to t-test}
     if not MainForm.OpenDialogExecute('Select MRIcron VAL file',false,false,'MRIcron VAL (*.val)|*.val') then begin
	      ShowMsg('NPM aborted: VAL file selection failed.');
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

function HasOption(const S: string):Boolean;
var
        i: integer;
begin
   result := false;
   if (ParamCount < 1) then exit;
   for i := 1 to ParamCount do
       if ParamStr(i) = ('-'+S) then result := true;
end;

procedure msg (s: string);
begin
   writeln(s);
end;

procedure ShowOptions (lTestInt: integer; lMaskFilename,lOutFilename: string);
begin
  msg(' -c : CPU threads, Default : '+inttostr(gnCPUThreads));
  msg(' -m : mask name. Default "' +lMaskFilename+'"');
  msg(' -n : neighbors for TFCE, 0 for none. Default ' +inttostr(gNPMprefs.TFCE));
  msg(' -o : output name. Default "' +lOutFilename+'"');
  msg(' -p : Permutations, 0 for none. Default '+inttostr(gNPMprefs.nPermute));
  msg(' -r : RAM for processing (Mb). Default '+inttostr(gNPMPrefs.PlankMB));
  msg(' -t : test (0=continuous,1=binomial,2=regress,3=multiregress). Default '+inttostr(lTestInt));

end;

procedure WriteHelp ;
begin
     msg(GetKVers);
     msg(' usage: '+ExtractFileName(ParseFileName(paramstr(0)))+' [options] [-t test] [valfilename]' );
     msg('Examples:');
     msg(' '+ ExtractFileName(ParseFileName(paramstr(0)))+' -t 0 test.val');
     msg(' '+ ExtractFileName(ParseFileName(paramstr(0)))+' -r 1024 -p 1000 -m mymask.nii -t 0 test.val');
     msg('Options:');
     msg(' -h : Help displayed');
end;

function GetOptionValue(const S: string):string;
var
        i: integer;
begin
   result := '';
   if (ParamCount < 2) then exit;
   for i := 1 to (ParamCount-1) do
       if ParamStr(i) = ('-'+S) then begin
          result := ParamStr(i+1);
          exit;

       end;
end;

function GetOptionValueInt(lCmd: string; lDefault: integer): integer;
var
  lResp : string;
begin
     lResp := GetOptionValue(lCmd);
     if length(lResp) < 1 then result := lDefault;
     try
        result := strtoint(lResp);
     except
       Writeln('Error '+(lResp)+' is not a valid integer.');
       result := lDefault;
    end;
end;

procedure doVLSM(lBinomial: boolean; VALFilename, lMaskFilename,lOutFilename: string);
 var
        lPrefs: TLDMPrefs ;
begin
     lPrefs.NULP := gNPMPrefs.NULP;
     if (not lBinomial) then begin //continuous
             lPrefs.BMtest :=   true;
             lPrefs.Ttest := true;
             lPrefs.Ltest:= false;
     end else begin //binomial
             lPrefs.BMtest := false;
             lPrefs.Ttest := false;
             lPrefs.Ltest:= true;
     end;
     lPrefs.CritPct := -1;
     lPrefs.nPermute := gNPMprefs.nPermute;
     lPrefs.Run := 0;{0 except for montecarlo}
     lPrefs.VALFilename := VALFilename;
     lPrefs.OutName := lOutFilename;
    lPrefs.ExplicitMaskName := lMaskFilename;
    DoLesion (lPrefs);
end;

(*procedure TMainForm.ProcessParamStr;
label
  666;
var
   lTestInt: integer;
   lMaskFilename : string;
   lValFilename : string;
   lOutFilename : string;
begin
  lTestInt := 0;
     lMaskFilename := '';
   lValFilename := '';
   lOutFilename := '';
  gnCPUThreads := GetLogicalCpuCount;
  ReadIniFile;
  // parse parameters
  if (HasOption('h')) or (ParamCount = 0) then begin
    WriteHelp;
    ShowOptions(lTestInt, lMaskFilename, lOutFilename);
    goto 666;
  end;
  if (HasOption('c')) then gnCPUThreads := GetOptionValueInt('c', gnCPUThreads);
  if (HasOption('m')) then begin
     lMaskFilename := GetOptionValue('m');
     if (not FileExistsEX(lMaskFilename)) then begin
        WriteHelp ;
        writeln('Can not find masking image '+ lMaskFilename);

          ShowOptions(lTestInt,lMaskFilename,lOutFilename);
        goto 666;
     end;
  end;
  if (HasOption('n')) then gnCPUThreads := GetOptionValueInt('n', gNPMprefs.TFCE);
  if (HasOption('o')) then begin
     lOutFilename := GetOptionValue('o');
  end;
  if (HasOption('p')) then gNPMprefs.nPermute := GetOptionValueInt('p', gNPMprefs.nPermute);
  if (HasOption('r')) then begin
     gNPMPrefs.PlankMB := GetOptionValueInt('r', gNPMPrefs.PlankMB);
     ComputePlankSize(gNPMPrefs.PlankMB);
  end;
  if (HasOption('t')) then lTestInt := GetOptionValueInt('t', lTestInt);


  lValFilename := (paramstr(ParamCount));
  if (UpCaseExt(lValFilename) <> '.VAL') or (not FileExistsEX(lValFilename)) then begin
     Writeln('Error: final option should be an existing file with the .val extension');
     WriteHelp ;
     ShowOptions(lTestInt,lMaskFilename,lOutFilename);
     goto 666;
  end;

  if (lOutFilename = '') then begin
     lOutFilename := ChangeFileExtX( lValFilename,'res.nii');
  end;
  //show settings
  ShowOptions(lTestInt,lMaskFilename,lOutFilename);
  Writeln('VAL File: '+lValFilename);
  if (lTestInt > 1) and (lMaskFilename = '') then begin
     Writeln('Error: this test require you to specify a mask image');
     goto 666;
  end;
  //run test
  Application.ProcessMessages;
  case lTestInt of
       0: doVLSM(false, lVALFilename, lMaskFilename,lOutFilename);//continuous : t-test
       1: doVLSM(true, lVALFilename, lMaskFilename,lOutFilename);//binomial: Liebermeister
       2: NPMSingleRegress ( lVALFilename, lMaskFilename,lOutFilename);
       3: NPMMultipleRegressClick( lVALFilename, lMaskFilename,lOutFilename);

  end;
  Writeln('Goodbye');
  Application.ProcessMessages;


  //WriteIniFile;
  // stop program loop
  666:
  Close;
end;   *)

(*function TestT: string;
var
   T: double;
   l1,l0,lN: integer;

   lIn: DoubleP0;
   lInp: pointer;
    //TStat2 (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutT: double);
begin
  T := 666;
  l1 := 16;
  l0 := 8;
  lN := l0+l1;
  createArray64(lInp,lIn,lN);
  lIn^[0] := 44 ;
  lIn^[1] := 23 ;
  lIn^[2] := 41 ;
  lIn^[3] := 32 ;
  lIn^[4] := 60 ;
  lIn^[5] := 58 ;
  lIn^[6] := 57 ;
  lIn^[7] := 57 ;
  lIn^[8] := 55 ;
  lIn^[9] := 56 ;
  lIn^[10] := 60;
  lIn^[11] := 59;
  lIn^[12] := 57;
  lIn^[13] := 58;
  lIn^[14] := 56;
  lIn^[15] := 57;
  lIn^[16] := 2 ;
  lIn^[17] := 22;
  lIn^[18] := 24;
  lIn^[19] := 22;
  lIn^[20] := 18;
  lIn^[21] := 12;
  lIn^[22] := 15 ;
  lIn^[23] := 22;

   TStat2 (lN, l1, lIn,  T);
  result := floattostr(T);
  freemem(lInp);

end;   *)

procedure TMainForm.FormShow(Sender: TObject);
begin
  NPMMsgClear;
     NPMMsg(GetkVers);
     {$IFNDEF UNIX} {GUILaunch;}{$ENDIF}
     LongTimeFormat := 'YYYY-MMM-DD hh:nn:ss';  //delphi TimeToStr
     ShortTimeFormat := 'YYYY-MMM-DD hh:nn:ss'; //freepascal TimeToStr
     {$IFDEF FPC}{$IFNDEF UNIX} ReadParamStr; {$ENDIF} {$ENDIF}
     {$IFDEF benchmark}
     //MonteCarloSimulation1.visible := true;
     {$ENDIF}
     //StartTimer.enabled := true;
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
  NPMMsgClear;
  NPMMsg(GetKVers);
  NPMMsg('Paired T-test [Repeated Measures]');
  if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
	   ShowMsg('NPM aborted: mask selection failed.');
	   goto 666;
   end; //if not selected
   //OpenHdrDlg.FileName := 'c:\vbmdata\mask50.nii.gz';
   lMaskname := OpenHdrDlg.Filename;

  if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	   ShowMsg('Error reading Mask image.');
	   goto 666;
   end;
   lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
   if (lMaskVoxels < 2) or (not CheckVoxels(lMaskname,lMaskVoxels,0)){make sure there is uncompressed .img file}  then begin
	   ShowMsg('Mask file size too small.');
	   goto 666;
   end;
   if not ReadPairedFilenames(lImageNames) then exit;
   lnSubj :=lImageNames.Count div 2;

   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   ShowMsg('File dimensions differ from mask.');
	   goto 666;
   end;
   NPMMsg('Mask = '+lMaskname);
   NPMMsg('Total voxels = '+inttostr(lMaskVoxels));
   NPMMsg('Number of observations = '+inttostr(lnSubj));
   NPMMsg('Degrees of Freedom = '+inttostr(lnSubj-1));

   if not CheckVoxelsGroupX(lImageNames,lMaskHdr{lMaskVoxels}) then begin
	   ShowMsg('File dimensions differ from mask.');
	   goto 666;
   end;
   //show matrix
   //MsgStrings (lImageNames);
   NPMMsg ('n Subjects = '+inttostr(lnSubj));
   lStr := 'Image,';
   for lSubj := 0 to (lnSubj-1) do
            NPMMsg(lImageNames[lSubj]+' <-> '+lImageNames[lSubj+lnSubj]);
   if lnSubj < 4 then begin
      ShowMsg('Paired t-test error: Requires at least 4 images per group.');
      goto 666;
   end;
   lOutName := lMaskName;
   if not SaveHdrName ('Statistical Map', lOutName) then exit;
   //if not SaveHdrName ('Base Statistical Map', lOutName) then exit;
   NPMAnalyzePaired (lImageNames, lMaskHdr, lMaskVoxels,lOutName);
   //Regress2NPMAnalyze (lImageNames, lMaskHdr, lOutname);
    666:
        lImageNames.Free;
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
      ShowMsg('Error: you need to compute at least on test [options/test menu]');
      exit;
  end;
	NPMMsgClear;
        NPMMsg(GetKVers);
        NPMMsg('Threads: '+inttostr(gnCPUThreads));
   if not OpenDialogExecute('Select mean image ',false,false,kImgFilter) then begin
	   ShowMsg('NPM aborted: mean selection failed.');
	   exit;
   end; //if not selected
   lMn := OpenHdrDlg.Filename;
   if not NIFTIhdr_LoadHdr(lMn,lMnHdr) then begin
	   ShowMsg('Error reading mask.');
	   exit;
   end;
   lMnVoxels := ComputeImageDataBytes8bpp(lMnHdr);
   if (lMnVoxels < 2) or (not CheckVoxels(lMn,lMnVoxels,0)){make sure there is uncompressed .img file}  then begin
	   ShowMsg('Mean file size too small.');
	   exit;
   end;

   if not OpenDialogExecute('Select StDev image ',false,false,kImgFilter) then begin
	   ShowMsg('NPM aborted: StDev selection failed.');
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
   NPMMsg('Mean name = '+ lMn);
   NPMMsg('Total voxels = '+inttostr(lMnVoxels));
   //next, get 1st group
   if not OpenDialogExecute('Select postive group (Z scores positive if this group is brighter)',true,false,kImgFilter) then begin
	   showmessage('NPM aborted: file selection failed.');
	   exit;
   end; //if not selected
   lG:= TStringList.Create; //not sure why TStrings.Create does not work???
   lG.addstrings(OpenHdrDlg.Files);
   lnSubj :=OpenHdrDlg.Files.Count;
   NPMMsg('Subjects= '+inttostr(lnSubj));
   if not CheckVoxelsGroupX(lG,lMnHdr {lMnVoxels}) then begin
	   showmessage('File dimensions differ from mask.');
	   goto 666;
   end;
   NPMzscore (lG, lMnHdr,lStDevHdr);
   666:
   lG.Free;
end;

procedure TMainForm.MultipleRegressClick(Sender: TObject);
var lVALFilename, lMaskname,lOutname: string;
begin
  Showmessage('This function has been superceded by nii_stat');
  exit;
  
   if not MainForm.OpenDialogExecute('Select MRIcron VAL file',false,false,'MRIcron VAL (*.val)|*.val') then begin
         ShowMsg('NPM aborted: VAL file selection failed.');
         exit;
   end; //if not selected
   lVALFilename := MainForm.OpenHdrDlg.Filename;
   if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then begin
         showmessage('NPM aborted: mask selection failed.');
         exit;
   end; //if not selected
   lMaskname := OpenHdrDlg.Filename;
   lOutName := lMaskName;
   if not SaveHdrName ('Base Statistical Map', lOutName) then exit;
   NPMMultipleRegressClick(lVALFilename, lMaskname,lOutname);
end;

procedure TMainForm.SingleRegressClick(Sender: TObject);
var lVALFilename, lMaskname,lOutname: string;
begin
  showmessage('This function has been superceded with nii_stat');
  exit;
   if not MainForm.OpenDialogExecute('Select MRIcron VAL file',false,false,'MRIcron VAL (*.val)|*.val') then exit;
   lVALFilename := MainForm.OpenHdrDlg.Filename;
   if not OpenDialogExecute('Select brain mask ',false,false,kImgFilter) then exit;
   lMaskname := OpenHdrDlg.Filename;
   lOutname := lVALFilename;
   NPMSingleRegress (lVALFilename, lMaskname,lOutname);
end;

procedure TMainForm.AssociatevalfileswithNPM1Click(Sender: TObject);
begin
{$IFNDEF FPC}//unsupported by FreePascal
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

(*procedure TMainForm.Countlesionoverlaps1Click(Sender: TObject);
label
	666;
var
	lReps,lMax,lInc,lMaskVoxels,lDefault,lTotal,lPct: integer;
	lG:  TStrings;
	lMaskname: string;
        lMaskHdr: TMRIcroHdr;
begin
     NPMMsgClear;
     NPMMsg(GetKVers);
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

     NPMMsg('Voxels = '+inttostr(lMaskVoxels));
     NPMMsg('Scans to permute = '+inttostr(lG.count));
     EvaluatePower (lG,lInc,lMax,lReps,lPct);

     //MakeMean(lG,lMaskHdr, odd((Sender as TMenuItem).tag),false);
     666:
     lG.Free;
end;     *)



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
            NPMMsg('NPM does not (yet) support permutation thresholding with Logisitic Regression.');
            lnPermute := 0;
        end;
        {$IFDEF SINGLETHREAD}
        lnCPUThreads := gnCPUThreads;
        if gnCPUThreads > 1 then
           NPMMsg('July 2007 logistic regression will only use 1 thread. You may want to check for a software update');
        gnCPUThreads := 1;
        {$ENDIF}
        NPMMsg('Permutations = ' +IntToStr(lnPermute));
	NPMMsg('Logisitic Regression began = ' +TimeToStr(Now));
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
	NPMMsg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	NPMMsg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
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
		NPMMsg('Computing plank = ' +Inttostr(lPlank));
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
                    {$IFDEF FIRTHNOTHREAD}
                      FirthAnalyzeNoThread (lnCond, lnCrit,lnPermute,1,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lPlankImg,lOutImgSum,lSymptomRA,lOutImg);
                    {$ELSE}
                for lThread := 1 to gnCPUThreads do begin
                    if lThread = gnCPUThreads then
                       lThreadEnd := lVoxPerPlank; //avoid integer rounding error
                    with TFirthThreadStat.Create ((lThread = ((gnCPUThreads+1) div 2)),MainForm, ProgressBar1,lnCond,lnCrit, lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lPlankImg,lOutImgSum,lSymptomRA,lOutImg) do
                         {$IFDEF FPC} OnTerminate := @ThreadDone; {$ELSE}OnTerminate := ThreadDone;{$ENDIF}
                    inc(gThreadsRunning);
                    NPMMsg('Thread ' +Inttostr(gThreadsRunning)+' = '+inttostr(lThreadStart)+'..'+inttostr(lThreadEnd));
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
	   NPMMsg('**Error: no voxels tested: no regions lesioned in at least '+inttostr(lnCrit)+' patients**');
           goto 123;
        end;
        //next report findings
	NPMMsg('Voxels tested = ' +Inttostr(lnVoxTested));
        NPMMsg('Only tested voxels with more than '+inttostr(lnCrit)+' lesions');
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
	NPMMsg('Analysis finished = ' +TimeToStr(Now));
        lOutNameMod := ChangeFilePostfixExt(lOutName,'Notes','.txt');
        NPMMsgSave(lOutNameMod);

        ProgressBar1.Position := 0;
        {$IFDEF SINGLETHREAD}
        gnCPUThreads := lnCPUThreads;
        {$ENDIF}
	exit;
667: //you only get here if you aborted ... free memory and report error
	if lTotalMemory > 1 then freemem(lPlankImg);
	NPMMsg('Unable to complete analysis.');
        ProgressBar1.Position := 0;
        {$IFDEF SINGLETHREAD}
        gnCPUThreads := lnCPUThreads;
        {$ENDIF}
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
  Showmessage('This function has been superceded by nii_stat');
  exit;
    // lBinomial := false;
  lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
   //next, get 1st group
  if not GetValX(lnSubj,lnFactors,lMultiSymptomRA,lImageNames,lnCrit{,binom},lPredictorList) then
     goto 666;
  if (lnSubj < 2) or (lnFactors < 1) then begin
     showmessage('This analysis requires at least 2 participants and one factor');
     goto 666;
  end;
  WarnIfLowNCrit(lnSubj,lnCrit);
  lTemp4D := CreateDecompressed4D(lImageNames);
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
             NPMMsg('Computing lesion volumes...');
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
   NPMMsgClear;
   NPMMsg(GetKVers);
   NPMMsg('Firth Penalized regression is still beta software...');
   NPMMsg('Number of participants: '+inttostr(lnSubj));
   NPMMsg('Number of factors: '+inttostr(lnFactors));
   NPMMsg('Threads: '+inttostr(gnCPUThreads));
   //next - header shows factor names
   lStr :='imagename';
   for lFact := 1 to lnFactors do
       lStr := lStr+','+lPredictorList[lFact-1];
   NPMMsg(lStr);
   For lSubj := 1 to lnSubj do begin
       lStr :='';
       for lFact := 1 to lnFactors do begin
           lStr := lStr+','+realtostr(lMultiSymptomRA^[lSubj+ ((lFact-1)*lnSubj)],2);
       end;
       NPMMsg (lImageNames.Strings[lSubj-1] + ' = '+lStr );
   end;
   NPMMsg('Total voxels = '+inttostr(lMaskVoxels));
   NPMMsg('Only testing voxels damaged in at least '+inttostr(lnCrit)+' individual[s]');
   NPMMsg('Number of Lesion maps = '+inttostr(lnSubj));
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

(*function ComputeIntersection ( lAname,lBname: string; var lUnion,lIntersection,lAnotB,lBnotA: integer): boolean;
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
		NPMMsg('Unable to load mask ' +lMaskHdr.ImgFileName);
		goto 667;
	end;
        lVolVoxA := lVolVox;
     //read B
   if not NIFTIhdr_LoadHdr(lBname,lMaskHdr) then begin
	   showmessage('Error reading image B - '+lBname);
	   exit;
   end;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
	if (lVolVoxA <> lVolVox) or (lVolVox < 1) then goto 667;
	getmem(lImgB,lVolVox*sizeof(single));
	if not LoadImg(lBname, lImgB, 1, lVolVox,round(lMaskHdr.NIFTIhdr.vox_offset),1,lMaskHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load mask ' +lMaskHdr.ImgFileName);
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
              NPMMsg('Error');
           NPMMsg( lAName+'  '+lBName+' I'+inttostr(lIntersection)+' U'+inttostr(lUnion)+' AnotB'+inttostr(lAnotB)+' BnotA'+inttostr(lBnotA));

end; *)


(*procedure TMainForm.ComputeIntersectionandUnion1Click(Sender: TObject);
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
  NPMMsgClear;
  NPMMsg(GetKVers);
  NPMMsg('Compute intersection [A and B] and union [A or B] for a series of images');


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


   NPMMsg ('n Subjects = '+inttostr(lnSubj));
   for lSubj := 0 to (lnSubj-1) do begin
       lStr := 'A=,'+lImageNames[lSubj]+',B=,'+lImageNames[lSubj+lnSubj];
       ComputeIntersection ( lImageNames[lSubj],lImageNames[lSubj+lnSubj],lUnion,lIntersection,lAnotB,lBnotA);
       lStr := lStr + ',A and B=,'+inttostr(lIntersection);
       lStr := lStr + ',A or B=,'+inttostr(lUnion);
       lStr := lStr + ',A not B=,'+inttostr(lAnotB);
       lStr := lStr + ',B not A=,'+inttostr(lBnotA);
       NPMMsg(lStr);
   end;

   //Msg('Mask = '+lMaskname);
   //Msg('Total voxels = '+inttostr(lMaskVoxels));
   NPMMsg('Number of observations = '+inttostr(lnSubj));
    666:
        lImageNames.Free;
end; //compute intersection and union
             *)

(*procedure TMainForm.ROCbinomialdeficit1Click(Sender: TObject);
begin
        testROC;
end;

procedure TMainForm.ROCcontinuousdeficit1Click(Sender: TObject);
begin
   testROC2;
end; *)

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
       (*
procedure ROIanalysis(var lROInames,lImageNames:  TStrings; var lVALFilename: string);
label
	666;
var
        lROI,lnROI,lVol,lMin,lMax,lI,lFact,lnFactors,lSubj,lnSubj,lMaskVoxels,lnCrit: integer;
	//lROInames,lImageNames:  TStrings;
        lPredictorList: TStringList;
	lVolStr,lTemp4D,lOutName,lStr: string;
        lBehav: single;
        lROIvolRA: doubleP;
        lMultiSymptomRA,lLesionVolRA,lBehavRA: singleP;
        lError: boolean;
begin
     lnROI := lROINames.Count;
     if lnROI < 1 then begin
         showmessage('You need to select at least one ROI.');
         goto 666;
     end;
  //lImageNames:= TStringList.Create; //not sure why TStrings.Create does not work???
      if not GetValCore ( lVALFilename,lnSubj,lnFactors,lMultiSymptomRA,lImageNames,lnCrit,lPredictorList) then
     goto 666;
  lTemp4D := CreateDecompressed4D(lImageNames);
  if (lnSubj < 1) or (lnFactors < 1) then begin
     showmessage('This analysis requires at least 1 participant and one factor');
     goto 666;
  end;
   NPMMsgClear;
   NPMMsg(GetKVers);
	NPMmsg('Analysis began = ' +TimeToStr(Now));
   NPMMsg('VAL file name: '+MainForm.OpenHdrDlg.Filename);
   NPMMsg('Number of participants: '+inttostr(lnSubj));
   NPMMsg('Number of factors: '+inttostr(lnFactors));
   NPMMsg('Number of Lesion maps = '+inttostr(lnSubj));
   //next - header shows factor names
   lStr :='imagename';
   for lFact := 1 to lnFactors do
       lStr := lStr+','+lPredictorList[lFact-1];
   for lROI := 1 to lnROI do
       lStr := lStr+','+lROInames[lROI-1];
   NPMMsg(lStr+',LesionVolume');
   lError := false;
   Getmem(lROIVolRA, lnSubj*lnROI*sizeof(double));
   Getmem(lLesionVolRA, lnSubj*lnROI*sizeof(single));
   Getmem(lBehavRA, lnSubj*lnFactors*sizeof(single));
   for lROI := 1 to lnROI do begin
       //if not ComputeIntersection ( lImageNames.Strings[lSubj-1],lROInames[lROI-1],lUnion,lIntersection,lAnotB,lBnotA) then
       if not ComputeOverlap (lROInames[lROI-1],lImageNames, lROIvolRA^[lROI], singlep(@lLesionVolRA^[((lROI-1)*lnSubj)+1])) then begin
          NPMmsg('Error computing overlap');
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
       NPMMsg (lImageNames.Strings[lSubj-1] + ' = '+lStr +','+lVolStr );
   end;
   for lROI := 1 to lnROI do begin
       for lFact := 1 to lnFactors do begin
           Contrast(lPredictorList[lFact-1],lROInames[lROI-1],singlep(@lBehavRA^[((lFact-1)*lnSubj)+1]),singlep(@lLesionVolRA^[((lROI-1)*lnSubj)+1]),lnSubj);//,((lFact-1)*lnSubj),((lROI-1)*lnSubj));
       end; //for each factor
   end; //for each ROI
   for lROI := 1 to lnROI do begin
       NPMMsg( lROInames[lROI-1] +' volume = '+floattostr(lROIvolRA^[lROI]) )
   end; //for each ROI
   Freemem(lLesionVolRA);
   Freemem(lBehavRA);
   Freemem(lROIvolRA);
666:
    lROInames.free;
    lImageNames.Free;
    lPredictorList.Free;
    DeleteDecompressed4D(lTemp4D);
  	NPMmsg('Analysis finished = ' +TimeToStr(Now));
end;       *)


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
   NPMMsgClear;
   NPMMsg(GetKVers);
	NPMmsg('Analysis began = ' +TimeToStr(Now));
   NPMMsg('VAL file name: '+MainForm.OpenHdrDlg.Filename);
   NPMMsg('Number of participants: '+inttostr(lnSubj));
   NPMMsg('Number of factors: '+inttostr(lnFactors));
   NPMMsg('Number of Lesion maps = '+inttostr(lnSubj));
   //next - header shows factor names
   lStr :='imagename';
   for lFact := 1 to lnFactors do
       lStr := lStr+','+lPredictorList[lFact-1];
   for lROI := 1 to lnROI do
       lStr := lStr+','+lROInames[lROI-1];
   NPMMsg(lStr+',LesionVolume');
   lError := false;
   Getmem(lROIVolRA, lnSubj*lnROI*sizeof(double));
   Getmem(lLesionVolRA, lnSubj*lnROI*sizeof(single));
   Getmem(lBehavRA, lnSubj*lnFactors*sizeof(single));
   for lROI := 1 to lnROI do begin
       //if not ComputeIntersection ( lImageNames.Strings[lSubj-1],lROInames[lROI-1],lUnion,lIntersection,lAnotB,lBnotA) then
       if not ComputeOverlap (lROInames[lROI-1],lImageNames, lROIvolRA^[lROI], singlep(@lLesionVolRA^[((lROI-1)*lnSubj)+1])) then begin
          NPMmsg('Error computing overlap');
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
       NPMMsg (lImageNames.Strings[lSubj-1] + ' = '+lStr +','+lVolStr );
   end;
   for lROI := 1 to lnROI do begin
       for lFact := 1 to lnFactors do begin
           Contrast(lPredictorList[lFact-1],lROInames[lROI-1],singlep(@lBehavRA^[((lFact-1)*lnSubj)+1]),singlep(@lLesionVolRA^[((lROI-1)*lnSubj)+1]),lnSubj);//,((lFact-1)*lnSubj),((lROI-1)*lnSubj));
       end; //for each factor
   end; //for each ROI
   for lROI := 1 to lnROI do begin
       NPMMsg( lROInames[lROI-1] +' volume = '+floattostr(lROIvolRA^[lROI]) )
   end; //for each ROI
   Freemem(lLesionVolRA);
   Freemem(lBehavRA);
   Freemem(lROIvolRA);
666:
    lROInames.free;
    lImageNames.Free;
    lPredictorList.Free;
    DeleteDecompressed4D(lTemp4D);
  	NPMmsg('Analysis finished = ' +TimeToStr(Now));
end;
                    

procedure TMainForm.Masked1Click(Sender: TObject);
var
        lFilename,lMaskname: string;
	lPos:  Integer;
begin
     NPMMsgClear;
     NPMMsg(GetKVers);
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
		NPMMsg('Unable to load  ' +lHdr.ImgFileName);
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
        NPMMsg('Creating  ' +lOutNameMod+' Threshold = '+floattostr(lMax));
        NIFTIhdr_SaveHdrImg8(lOutNameMod,lHdr.NIFTIhdr,true,not IsNifTiMagic(lHdr.NIFTIhdr),true,lImg8,1);
        freemem(lIMg8);
	freemem(lImg);
end;


procedure TMainForm.Binarizeimages1Click(Sender: TObject);
var
        lFilename: string;
	lPos:  Integer;
begin
     NPMMsgClear;
     NPMMsg(GetKVers);
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
     NPMMsg('Done');
end;



procedure TMainForm.Setnonseroto1001Click(Sender: TObject);
var
        lFilename: string;
	lPos:  Integer;
begin
     NPMMsgClear;
     NPMMsg(GetKVers);
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

(*
function TMainForm.MakeSubtract (lPosName,lNegName: string): boolean;
var
   lNegImg,lImg,lOutImg: SingleP;
   lHdr,lNegHdr: TMRIcroHdr;
   lVolVox,lVox: integer;
   lOutNameMod: string;
begin
        result := false;
        if not NIFTIhdr_LoadHdr(lPosName,lHdr) then begin
	   ShowMsg('Error reading '+lPosName);
	   exit;
        end;
	lVolVox := lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]* lHdr.NIFTIhdr.dim[3];
	if (lVolVox < 1) then exit;
	getmem(lImg,lVolVox*sizeof(single));
	if not LoadImg(lHdr.ImgFileName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load  ' +lHdr.ImgFileName);
		exit;
	end;

        if not NIFTIhdr_LoadHdr(lNegName,lNegHdr) then begin
	   showmessage('Error reading '+lNegName);
	   exit;
        end;
	if lVolVox <> (lNegHdr.NIFTIhdr.dim[1]*lNegHdr.NIFTIhdr.dim[2]* lNegHdr.NIFTIhdr.dim[3]) then begin
           ShowMsg('Volumes differ');
           exit;

        end;
	getmem(lImg,lVolVox*sizeof(single));
	if not LoadImg(lHdr.ImgFileName, lImg, 1, lVolVox,round(lHdr.NIFTIhdr.vox_offset),1,lHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load  ' +lHdr.ImgFileName);
		exit;
	end;
	getmem(lNegImg,lVolVox*sizeof(single));
	if not LoadImg(lNegHdr.ImgFileName, lNegImg, 1, lVolVox,round(lNegHdr.NIFTIhdr.vox_offset),1,lNegHdr.NIFTIhdr.datatype,lVolVox) then begin
		NPMMsg('Unable to load  ' +lNegHdr.ImgFileName);
		exit;
	end;
        getmem(lOutImg,lVolVox*sizeof(single));
        for lVox := 1 to lVolVox do
            lOutImg^[lVox] := lImg^[lVox] - lNegImg^[lVox];


        lHdr.NIFTIhdr.scl_slope := 1;
        lHdr.NIFTIhdr.scl_inter := 0;
        lOutNameMod :=  ChangeFilePrefixExt(lPosName,'subtract_','.hdr');
        NPMMsg(lPosName+' - ' + lNegName+ '  = '+lOutNameMod);
        NIFTIhdr_SaveHdrImg(lOutNameMod,lHdr.NIFTIhdr,true,not IsNifTiMagic(lHdr.NIFTIhdr),true,lOutImg,1);


	freemem(lImg);
	freemem(lOutImg);
	freemem(lNegImg);
end;//makesubtract
*)

(*procedure TMainForm.Subtract1Click(Sender: TObject);
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

end;  *)





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
