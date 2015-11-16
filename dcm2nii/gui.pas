unit gui;
{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
interface
uses

{$IFDEF FPC}LResources,LCLIntf, {$ELSE} Messages,{$ENDIF}
{$IFNDEF UNIX}  Windows,ShellAPI,ShlObj,
{$ELSE}
//BaseUnix,
LCLType,
{$ENDIF}
//Messages,
SysUtils, Classes, Graphics, Controls, Forms, Dialogs,StdCtrls,
//ToolWin,
//ComCtrls,
ExtCtrls, nifti_types,
//IniFiles,
define_types,sortdicom,//dicom,
parconvert,
//filename,convert, nifti_hdr,ConvertSimple,
userdir,  paramstrs,nii_math,dicomtypes,nii_crop,
nii_orient, nii_4dto3d,nii_asl,nii_reslice, Menus,nii_3dto4d,prefs,
GraphicsMathLibrary;
{$IFDEF FPC}
type
   { TMainForm }
   TMainForm = class(TForm)
     AppleMenu: TMenuItem;
     ApplePrefs: TMenuItem;
    SelectDirectoryDialog1: TSelectDirectoryDialog;//<-Lazarus only - does not exist in Delphi 4
    Label1: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Copy1: TMenuItem;
    DICOMtoNIfTI1: TMenuItem;
    AnonymizeDICOM1: TMenuItem;
    Exit1: TMenuItem;
    ExtractDICOMdims1: TMenuItem;
    ExtractDICOMhdr1: TMenuItem;
    ExtractNIfTIhdrs1: TMenuItem;
    SumTPM1: TMenuItem;
    MirrorXdimension1: TMenuItem;
    UntestedMenu: TMenuItem;
    NIfTI3D4D1: TMenuItem;
    ModifyNIfTI1: TMenuItem;
    Preferences1: TMenuItem;
    Memo1: TMemo;
    OpenHdrDlg: TOpenDialog;
    Panel1: TPanel;
    TypeCombo: TComboBox;
    ResliceNIfTI1: TMenuItem;
    Deletenondcm1: TMenuItem;
    HalveMenu1: TMenuItem;
    procedure SavePrefs;
    procedure ExtractDICOMdims1Click(Sender: TObject);
    procedure ExtractDICOMhdr1Click(Sender: TObject);
    procedure ExtractNIfTIhdrs1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var vAction: TCloseAction);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure HalveMenu1Click(Sender: TObject);
    function OpenDialogExecute (lCaption: string;lAllowMultiSelect,lForceMultiSelect: boolean; lFilter: string): boolean;
    procedure CheckPrefs (var lPrefs: TPrefs; lWrite: boolean);
    function ConvertDCM2NII (lFilename: string; var lPrefs: TPrefs): boolean;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SumTPM1Click(Sender: TObject);
    procedure TypeComboChange(Sender: TObject);
    procedure AnonymizeDICOM1Click(Sender: TObject);
    procedure ModifyNIfTI1Click(Sender: TObject);
    procedure NIfTI3D4D1Click(Sender: TObject);
    procedure ResliceNIfTI1Click(Sender: TObject);
    procedure Deletenondcm1Click(Sender: TObject);
    procedure dcm2niiBtnClick(Sender: TObject);
    procedure MirrorXdimension1Click(Sender: TObject);
    function BrowseDialog(const Title: string): string;
  end;
{$ELSE}
type
   TMainForm = class(TForm)
    Label1: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Copy1: TMenuItem;
    DICOMtoNIfTI1: TMenuItem;
    AnonymizeDICOM1: TMenuItem;
    Exit1: TMenuItem;
    ExtractDICOMdims1: TMenuItem;
    ExtractDICOMhdr1: TMenuItem;
    ExtractNIfTIhdrs1: TMenuItem;
    SumTPM1: TMenuItem;
    MirrorXdimension1: TMenuItem;
    UntestedMenu: TMenuItem;
    NIfTI3D4D1: TMenuItem;
    ModifyNIfTI1: TMenuItem;
    Preferences1: TMenuItem;
    Memo1: TMemo;
    OpenHdrDlg: TOpenDialog;
    Panel1: TPanel;
    TypeCombo: TComboBox;
    ResliceNIfTI1: TMenuItem;
    Deletenondcm1: TMenuItem;
    HalveMenu1: TMenuItem;
        procedure SavePrefs;
    procedure ExtractDICOMdims1Click(Sender: TObject);
    procedure ExtractDICOMhdr1Click(Sender: TObject);
    procedure ExtractNIfTIhdrs1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var vAction: TCloseAction);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure HalveMenu1Click(Sender: TObject);
    function OpenDialogExecute (lCaption: string;lAllowMultiSelect,lForceMultiSelect: boolean; lFilter: string): boolean;
    procedure CheckPrefs (var lPrefs: TPrefs; lWrite: boolean);
    function ConvertDCM2NII (lFilename: string; var lPrefs: TPrefs): boolean;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SumTPM1Click(Sender: TObject);
    procedure TypeComboChange(Sender: TObject);
    procedure AnonymizeDICOM1Click(Sender: TObject);
    procedure ModifyNIfTI1Click(Sender: TObject);
    procedure NIfTI3D4D1Click(Sender: TObject);
    procedure ResliceNIfTI1Click(Sender: TObject);
    procedure Deletenondcm1Click(Sender: TObject);
    procedure dcm2niiBtnClick(Sender: TObject);
    procedure MirrorXdimension1Click(Sender: TObject);
    function BrowseDialog(const Title: string): string;
  private
         procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES; //<-Delphi only - does not exist in Lazarus
  end;
{$ENDIF}
var
  MainForm: TMainForm;

implementation

uses untar,pref_form, nifti_form,niftiutil{$IFNDEF UNIX},ActiveX {$ENDIF};
{$IFNDEF FPC}
{$R *.DFM}
{$R windowsxp.res}
{$ENDIF}

procedure MsgX (lStr: string);
begin
         MainForm.Memo1.Lines.Add(lStr);
end;

function is4D (var lHdr: TNIFTIhdr): boolean;
begin
     if lHdr.dim[4] > 1 then
        result := true
     else
         result := false;
end;
function SelectProcessNIFTI (var lHdr: TNIFTIhdr; var lFilename: string): integer;
begin
     result := -1; //returns -1 if error
     if is4D(lHdr) then begin
        NIfTIForm.Combo3D.visible := false;
        NIfTIForm.Combo4D.visible := true;
     end else begin
         //NIfTIForm.Combo3D.itemIndex := 2;
         NIfTIForm.Combo3D.visible := true;
         NIfTIForm.Combo4D.visible := false;
     end;
     NiftiForm.Combo4DChange(nil);
     NIftiForm.caption := extractfilename(lFilename);
     //next - let user specify task
     NiftiForm.showmodal;
     if (NiftiForm.ModalResult = mrCancel) then
        exit;
     if is4D(lHdr) then
        result := NiftiForm.Combo4D.ItemIndex
     else
         result := NiftiForm.Combo3D.ItemIndex;
end;

procedure ProcessNIfTI(lFilenames : TStrings; lPrefs: TPrefs);
var
   l4D, lPrev4D, lByteSwap: boolean;
   lINc,lProcess: integer;
   lExt,lFilename,lOutname: string;
   lHdr: TNIFTIhdr;
   lO: TNIIOpts;
begin
     if lFilenames.Count < 1 then exit;
     lPrev4D := false; //ignored in if statement - set only to avoid compiler warning
     lProcess := 0; //always set in if statement - set only to avoid compiler warning
     for lInc := 1 to lFilenames.Count do begin
         lFilename := lFilenames.Strings[lInc-1];
         lExt := UpCaseExt(lFilename);
         if lExt ='.IMG' then
            lFilename :=  changefileext(lFilename,'.hdr');
         if not NIFTIhdr_LoadHdr (lFilename, lHdr, lO) then begin
            MsgX('Unable to read as NifTI/Analyze' + lFilename);
            exit;
         end;
         l4D := is4D(lHdr);
         //choose process
         //fx( lFilenames.Count,777);
         if (lInc = 1) or (l4D <> lPrev4D) then begin
            lProcess := SelectProcessNIFTI(lHdr,lFilename);
            if lProcess < 0 then exit;
            lPrev4D := l4D;
         end;
         //next - convert image as specified
         SetOutputFormat(NIfTIForm.TypeCombo.ItemIndex,lPrefs);
         if l4D then begin
         case lProcess of
              0: ChangeNIfTISubformat(lFilename,lHdr,lPrefs);
              1: Reorder4D(lFilename, lHdr, lByteSwap,lPrefs);
              2: Clip4D(lFilename, lHdr, false,lPrefs,NiftiForm.StartEdit.value, NiftiForm.EndEdit.value);
              3: Float32NIfTI(lFilename, lPrefs);
               4: FormulaNIfTI(lFilename,lPrefs, NiftiForm.ScaleEdit.value, NiftiForm.PowerEdit.value);
              5: ASL_subtract(lFilename,false,{subtract} (NiftiForm.AsLCombo.itemIndex ),lPrefs);
              6: CropNIfTIX(lFilename, lPrefs, NiftiForm.EndEdit.value, NiftiForm.StartEdit.value, 0,0,0,0 );

              else showmessage('Unknown function');
         end; //case combo
     end else begin //if 4d else 3d
         //Int16LogPtoZNIfTI32Z(lFilename, lPrefs);
         case lProcess of
              0: ChangeNIfTISubformat(lFilename,lHdr,lPrefs);
              1: Reorient(lFilename,lHdr, lPrefs,false,false);
              2: begin
                   lOutname := Reorient(lFilename,lHdr, lPrefs,false,false);
                   if lOutname <> '' then
                    CropNIfTI(lOutname,lPrefs);
                 end;//2
              3: CropNIfTIX(lFilename, lPrefs, NiftiForm.EndEdit.value, NiftiForm.StartEdit.value, 0,0,0,0 );
              4: SiemensPhase2RadiansNIfTI(lFilename, lPrefs);
              else showmessage('Unknown function');

         end; //case 3d
     end; //if 4d else 3d end
 end; //for each image
end;

procedure PromptOutput (var lPrefs: TPrefs);
begin
     if (lPrefs.OutDirMode = kOutDirModePrompt) then
        lPrefs.OutDir := GetDirPrompt(lPrefs.OutDir);
     //GetDirPrompt(lPrefs.OutDir);
end;

function TMainForm.ConvertDCM2NII (lFilename: string; var lPrefs: TPrefs): boolean;
//returns true if files treated as DICOM or PAR/REC - these will search entire folder
var
   lOutDir,lExt: String;
   lStartTime: DWord;
   lStrings : TStrings;
begin
 {$IFDEF FPC}
 DefaultFormatSettings.DecimalSeparator  := '.';
 {$ELSE}
 DecimalSeparator := '.';
 {$ENDIF}
     result := false;
     if (not Fileexists(lFilename)) and (not DirExists(lFilename)) then
        exit;
     PromptOutput ( lPrefs);
     result := true;
     //3/2011... do not clear here, so we can look across images... Memo1.lines.clear;
     MsgX(kVers);
     refresh;
     Memo1.lines.add('Converting '+lFilename);
     lOutDir := extractfiledir(lFilename);
     lStartTime := GetTickCount;
     if DirExists(lFilename) then begin
        RecursiveFolderSearch(lFilename,lFilename,lPrefs,0);
        lPrefs.NameAppend := '';
     end else begin
          lExt := UpCaseExt(lFilename);
          {if (lExt = '.FDF') then
             ConvertSimple2NII(lFilename,lOutDir,lPrefs)
          else}
          if (lExt = '.REC') or (lExt = '.PAR') then begin
             LoadFileListPARREC(lFilename,lOutDir,lPrefs)
          end else if (lExt = '.TGZ')  then
                DeTGZ(lFilename,lPrefs)
          else if (IsNiftiExt (lFilename)) or (IsVOIExt (lFilename)) then begin
              result := false;
              lStrings := TStringList.Create;
              lStrings.add(lFilename);
              ProcessNIfTI(lStrings,lPrefs);
              lStrings.Free;
          end else begin
              if (DirExists(lOutDir)) and (not lPrefs.Verbose)  then
                 RecursiveFolderSearch(lOutDir,lOutDir,lPrefs,0)
              else
                  LoadFileList(lFilename,lOutDir,lPrefs);
              lPrefs.NameAppend := '';
          end;
     end;
     Memo1.lines.add('Conversion completed in '+inttostr(GetTickCount-lStartTime)+' ms');
end;

function ShowHeader (lFilename: string): boolean;
var
   lPrefs: TPrefs;
begin
     PrefsForm.ReadPrefs(lPrefs);
     lPrefs.verbose := true;
     MainForm.Memo1.lines.add('Location '+lFilename);
     result := MainForm.ConvertDCM2NII( lFilename,lPrefs);

end;

procedure ProcessFilenames(lFilenames : TStrings; lPrefs: TPrefs);
var
   i: integer;
   lAllNII: boolean;
begin
     if lFilenames.Count < 1 then exit;
     MainForm.Memo1.lines.clear;
     lAllNii := true;
     for i := 0 to (lFilenames.Count-1) do
           if (not (IsNiftiExt (lFilenames.Strings[i]))) and (not (IsVOIExt (lFilenames.Strings[i]))) then
              lAllNii := false;
     if lAllNii then begin
        ProcessNiFTI (lFilenames,lPrefs);
        exit;
     end;
     if ssCtrl in KeyDataToShiftState(vk_Shift) then begin
        for i := 0 to (lFilenames.Count-1) do
             ShowHeader (lFilenames.Strings[i])
     end else
             MainForm.ConvertDCM2NII( lFilenames.Strings[0],lPrefs);
end;

{$IFNDEF FPC}//if delphi

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  CFileName: array[0..MAX_PATH] of Char;

  lInc: integer;
  lPrefs: TPrefs;
  lStrings: TStrings;
begin
     CheckPrefs(lPrefs,False);
  //lDone := false;
  lInc := 0;
  try
   lStrings := TStringList.Create;
   while (DragQueryFile(Msg.Drop, lInc, CFileName, MAX_PATH) > 0) {and (not lDone)} do begin
          lStrings.add(CFilename);

         Msg.Result := 0;
         inc(lInc);
    end; //while
    ProcessFilenames(lStrings,lPrefs);
    lStrings.Free;
  finally
    DragFinish(Msg.Drop);
  end;
end;

function TMainForm.BrowseDialog(const Title: string): string;
var
  iFlag: integer;
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of char;
  TempPath : array[0..MAX_PATH] of char;
begin
  iFlag :=  BIF_RETURNONLYFSDIRS;
    //iFlag :=  BIF_BROWSEINCLUDEFILES;
    //iFlag :=  BIF_BROWSEFORCOMPUTER;
    //iFlag :=  BIF_BROWSEFORPRINTER;
  Result:='';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
	hwndOwner := Application.Handle;
	pszDisplayName := @DisplayName;
	lpszTitle := PChar(Title);
	ulFlags := iFlag;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
	SHGetPathFromIDList(lpItemID, TempPath);
	Result := TempPath;
	GlobalFreePtr(lpItemID);
  end;     
end;
{$ELSE}
function TMainForm.BrowseDialog(const Title: string): string;
begin
    result := '';
    SelectDirectoryDialog1.title := Title;
    if not SelectDirectoryDialog1.execute then exit;
    result := SelectDirectoryDialog1.Filename;
end;
{$ENDIF}
procedure TMainForm.dcm2niiBtnClick(Sender: TObject);
var
   sTitle,lDirName: string;
   lPrefs: TPrefs;
begin
     CheckPrefs(lPrefs,False);
//    {$IFNDEF UNIX}
      sTitle:='Choose a folder with DICOM images';
      lDirName := BrowseDialog(sTitle);
//      {$ELSE}
//      if not OpenDialogExecute('Select DICOM images you wish to convert)',true,false,kAnyFilter) then
//         exit;
//         lDirName := extractfiledir( OpenHdrDlg.Filename);
//      {$ENDIF}
      ConvertDCM2NII(lDirName,lPrefs);
end;



procedure TMainForm.CheckPrefs (var lPrefs: TPrefs; lWrite: boolean);
begin
    if lWrite then begin
       //showmessage('w');
       //options if writing
      TypeCombo.ItemIndex :=  DefaultOutputFormat (lPrefs);

      (*  if lPrefs.SPM2 then
          TypeCombo.ItemIndex := 0 //SPM2 3D hdr/img analyze
       else if not lPrefs.FourD  then begin
            if not (lPrefs.SingleNIIFile) then
               TypeCombo.ItemIndex := 1 //SPM5 3D hdr/img
            else
                TypeCombo.ItemIndex := 2; //SPM8 3D nii
       end else if not lPrefs.SingleNIIFile then
            TypeCombo.ItemIndex := 3 //?? 4D hdr/img
       else if not lPrefs.GZip then
            TypeCombo.ItemIndex := 4 //FSL 4D nii
       else
           TypeCombo.ItemIndex := 5; //FSL 4D nii.gz *)
       exit;
    end;
  SetDefaultPrefs (lPrefs);
  PrefsForm.ReadPrefs(lPrefs);
  SetOutputFormat(TypeCombo.ItemIndex,lPrefs);
  lPrefs.AnonymizeSourceDICOM := false;
end;

(*procedure Fz;
var
   lPrefs: TPrefs;
   lByteSwap: boolean;
   lExt,lFilename,lOutname,lNameWOExt: string;
   lHdr: TNIFTIhdr;
begin
  lFilename := 'C:\dti64\rapid\fz3.nii';
  lFilename := 'C:\t1\mx.nii';
     if not NIFTIhdr_LoadHdr (lFilename, lHdr, lByteSwap) then begin
        MsgX('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
     MainForm.CheckPrefs(lPrefs,False);
  Reorient(lFilename, lHdr,lPrefs,false);
end;     *)

(*procedure Fz;
var
   lPrefs: TPrefs;
   lF: string;
begin
     lF := 'C:\iceland\temp';
      SetDefaultPrefs (lPrefs);
       lPrefs.AnonymizeSourceDICOM := true;
     MainForm.ConvertDCM2NII(lF,lPrefs);
end;*)

procedure TMainForm.FormCreate(Sender: TObject);
begin
    {$IFDEF Darwin}
      Exit1.visible := false;
    {$ENDIF}
      {$IFNDEF UNIX}DragAcceptFiles(Handle, True);{$ENDIF}
      {$IFDEF FPC}
      DefaultFormatSettings.DecimalSeparator  := '.';
      {$ELSE}
      DecimalSeparator := '.';
      {$ENDIF}
    Application.HintHidePause := 30000;
    {$IFDEF Darwin}
    AppleMenu.Visible := true;
    {$IFNDEF LCLgtk} //for Carbon and Cocoa
            DICOMtoNIfTI1.ShortCut := ShortCut(Word('D'), [ssMeta]);
            Copy1.ShortCut := ShortCut(Word('C'), [ssMeta]);
            Preferences1.ShortCut := ShortCut(Word('P'), [ssMeta]);
            About1.ShortCut := ShortCut(Word('A'), [ssMeta]);
    {$ENDIF}//Carbon
    {$ENDIF}//Darwin
end;          



procedure TMainForm.Exit1Click(Sender: TObject);
begin
     Close;
end;

procedure TMainForm.Copy1Click(Sender: TObject);
begin
	Memo1.SelectAll;
	Memo1.CopyToClipboard;
end;

(*procedure testpermissions;
var
   p,n,x,s: string;

begin
    s:= '/usr/lib64/lazarus/cr/';
    inputquery('cap','name',s);
     FilenameParts (s,p,n,x);
     if DirWritePermission(p) then
        showmessage('+'+p+'*'+n+'*'+x)
     else
         showmessage('-'+p+'*'+n+'*'+x);
end; *)

(*procedure testpermissions;
var
   p,n,x,s: string;

begin
    s:= '/usr/lib64/lazarus/test/dcm2niigui.ini';
    if fpAccess (s,R_OK)=0 then //ensure user has read-access to prefs file...
       showmessage('dcm = 0');
    s:= '/usr/lib64/lazarus/test/dcx.ini';
    if fpAccess (s,R_OK)=0 then //ensure user has read-access to prefs file...
       showmessage('dcx = 0');

end; *)
(*procedure Force32;
var
   lPrefs: TPrefs;
   lI: integer;
begin
     PrefsForm.ReadPrefs(lPrefs);
     for lI := 1 to 6 do
         NII_force32 ('C:\walker\vois\i'+inttostr(lI)+'.nii','C:\walker\vois\ri'+inttostr(lI)+'.nii',lPrefs);
end;*)

(*procedure Force32;
var
   lPrefs: TPrefs;
begin
     PrefsForm.ReadPrefs(lPrefs);
         Rescale_4Dtissuemaps ('C:\walker\vois\4Dsri1.nii','C:\walker\vois\TPMQ.nii',lPrefs);
end;*)

(*procedure Force32;
var
   lPrefs: TPrefs;
const
     kDir = 'C:\walker\i3\';
     kTemp = kDir + 'TPM3.nii';
     kTempSym = kDir + 'TPM3sym.nii';
begin
//exit;
     PrefsForm.ReadPrefs(lPrefs);
         //scale_4Dtissuemaps ('C:\walker\vois\4Dsri1.nii','C:\walker\TPMLo.nii',lPrefs);
         //rge4DFiles ('C:\walker\TPMLo.nii','C:\walker\TPMHi.nii','C:\walker\TPMEX.nii',78,lPrefs);
     Insert3Din4D (kDir+'m1.nii.gz',kTemp,kTemp,1, lPrefs);
     Insert3Din4D (kDir+'m2.nii.gz',kTemp,kTemp,2, lPrefs);
     Insert3Din4D (kDir+'m3.nii.gz',kTemp,kTemp,3, lPrefs);
     Insert3Din4D (kDir+'m4.nii.gz',kTemp,kTemp,4, lPrefs);
     Insert3Din4D (kDir+'m5.nii.gz',kTemp,kTemp,5, lPrefs);
     Insert3Din4D (kDir+'m6.nii.gz',kTemp,kTemp,6, lPrefs);
     Rescale_4Dtissuemaps(kTemp,kTempSym,lPrefs,true);
end;  *)

(*procedure Force32;
var
   lPrefs: TPrefs;
const
     kDir = 'C:\walker\i4\';
     kTemp = kDir + 'TPM4.nii';
     kTempSym = kDir + 'TPM4sym.nii';
begin
//exit;
     PrefsForm.ReadPrefs(lPrefs);
         //scale_4Dtissuemaps ('C:\walker\vois\4Dsri1.nii','C:\walker\TPMLo.nii',lPrefs);
         //rge4DFiles ('C:\walker\TPMLo.nii','C:\walker\TPMHi.nii','C:\walker\TPMEX.nii',78,lPrefs);
     Insert3Din4D (kDir+'sm1.nii',kTemp,kTemp,1, lPrefs);
     Insert3Din4D (kDir+'sm2.nii',kTemp,kTemp,2, lPrefs);
     Insert3Din4D (kDir+'sm3.nii',kTemp,kTemp,3, lPrefs);
     Insert3Din4D (kDir+'sm4.nii',kTemp,kTemp,4, lPrefs);
     Insert3Din4D (kDir+'sm5.nii',kTemp,kTemp,5, lPrefs);
     Insert3Din4D (kDir+'sm6.nii',kTemp,kTemp,6, lPrefs);
     Rescale_4Dtissuemaps(kTemp,kTempSym,lPrefs,true);
end;  *)
(*procedure Force32;
var
   lPrefs: TPrefs;
   lMaskName: string;
   lHdr: TNIfTIHdr;
   lByteSwap, lSaveThresh3D: boolean;
   lV: integer;
begin
//exit;
     PrefsForm.ReadPrefs(lPrefs);
     if not MainForm.OpenDialogExecute('Select the mask image',false,false,kImgFilter) then
        exit;
     lMaskName := MainForm.OpenHdrDlg.Filename;
     if not NIFTIhdr_LoadHdr (lMaskName, lHdr, lByteSwap) then
        exit;
     if (lHdr.Dim[4] < 1) then
        exit;
     lSaveThresh3D := (MessageDlg('Save thresholded images for each individual?',mtCustom,[mbYes,mbNo], 0)=mrYes);

     for lV := 1 to lHdr.Dim[4] do
         if MainForm.OpenDialogExecute('Select NIfTI images you wish to mask with volume '+inttostr(lV),true,false,kImgFilter) then
            MaskImages(lMaskName, MainForm.OpenHdrDlg.Files,lPrefs,lV, lSaveThresh3D);
end;   *)

(*procedure Force32;
var
   lPrefs: TPrefs;
   lI: integer;
   //lMaskName: string;
begin
//exit;
     PrefsForm.ReadPrefs(lPrefs);
     if not MainForm.OpenDialogExecute('Select all the c1 (gray matter) images to binarize. The c2 (gray matter),c3,c4,c5,c6 images should be in th same folder.',true,false,kImgFilter) then
        exit;
     //lMaskName := ('C:\Documents and Settings\chris\Desktop\walkerseg\zero\wc120100128_102305t1saghiress002a1001.nii');
     //Binarize(lMaskName,lPrefs);
     if MainForm.OpenHdrDlg.Files.count < 1 then exit;
     for lI := 0 to (MainForm.OpenHdrDlg.Files.count-1) do
         Binarize(MainForm.OpenHdrDlg.Files[lI],lPrefs);
end;    *)

{$IFNDEF FPC}
procedure MaskVBM;
var
   lPrefs: TPrefs;
   lI: integer;
   lMaskName: string;
begin
     PrefsForm.ReadPrefs(lPrefs);
     if not MainForm.OpenDialogExecute('Select all TEMPLATE c1 (gray matter) image.',false,false,kImgFilter) then
        exit;
     lMaskName := MainForm.OpenHdrDlg.Filename;
     if not MainForm.OpenDialogExecute('Select all the c1 (gray matter) images to binarize. The c2 (gray matter),c3,c4,c5,c6 images should be in th same folder.',true,false,kImgFilter) then
        exit;
     if MainForm.OpenHdrDlg.Files.count < 1 then exit;
     for lI := 0 to (MainForm.OpenHdrDlg.Files.count-1) do
         MaskImgs(lMaskName, MainForm.OpenHdrDlg.Files[lI],lPrefs, 0.02);
end;
{$ENDIF}
{$IFNDEF FPC}
procedure Mask;
var
   lPrefs: TPrefs;
   lMaskName: string;
   lHdr: TNIfTIHdr;
   lO: TNIIOpts;
   lI,lV: integer;
begin
     PrefsForm.ReadPrefs(lPrefs);
     if not MainForm.OpenDialogExecute('Select the mask image',false,false,kImgFilter) then
        exit;
     lMaskName := MainForm.OpenHdrDlg.Filename;
     if not NIFTIhdr_LoadHdr (lMaskName, lHdr, lO) then
        exit;
     lV := 1;
     //lSaveThresh3D := (MessageDlg('Save thresholded images for each individual?',mtCustom,[mbYes,mbNo], 0)=mrYes);
     //for lV := 1 to lHdr.Dim[4] do
     if not MainForm.OpenDialogExecute('Select NIfTI images you wish to mask with volume '+inttostr(lV),true,false,kImgFilter) then
        exit;
     if MainForm.OpenHdrDlg.Files.count < 1 then exit;
     for lI := 0 to (MainForm.OpenHdrDlg.Files.count-1) do
         MaskImg(lMaskName, MainForm.OpenHdrDlg.Files[lI], lPrefs, 1);
end;
{$ENDIF}


function ExtNIIorIMG(lStr: string): boolean;
var
   lExt: string;
begin
     result := false;
     lExt := UpCaseExt(lStr);
     if (lExt = '.NII') or (lExt = '.NII.GZ') then
        result := true;
     if (lExt = '.IMG') {and (FSize(ChangeFileExt(lStr,'.hdr'))> 0)} then
        result := true;
end;

procedure NIIbatch (lDir,lS: string);
begin
     with mainform.Memo1.lines do begin
          add('subjx = strvcat'+lS+';');
          add('subj = cellstr(subjx);');
          add('dir = '''+lDir+''';');
          add('tic');
          add('for i=1:length(subj)');
          add('  filename = [dir,filesep,subj{i}];');
          add('  nii_16bit(filename);');
          add('end;');
          add('toc');
     end;//with
end;//proc NIIbatch
procedure NII2Mat;
var
   str,pre,sTitle,lDirName: string;
   lSearchRec: TSearchRec;
begin
        {$IFNDEF FPC}
        sTitle:='Choose a folder with DICOM images';
      lDirName := MainForm.BrowseDialog(sTitle);
      {$ELSE}
      if not MainForm.OpenDialogExecute('Select DICOM images you wish to inspect)',true,false,kAnyFilter) then
         exit;
      lDirName := extractfiledir( MainForm.OpenHdrDlg.Filename);
      {$ENDIF}
      str := '(';
      pre := '';

{$IFDEF UNIX}
 if FindFirst(lDirName+pathdelim+'*',faAnyFile-faSysFile,lSearchRec) = 0 then begin
{$ELSE}
 if FindFirst(lDirName+pathdelim+'*.*',faAnyFile-faSysFile,lSearchRec) = 0 then begin
{$ENDIF}
    //lFilename := '';
    repeat
      //lNewName := lNewDir+lSearchRec.Name;
      if  (lSearchRec.Name = '.') or (lSearchRec.Name = '..') then begin
            //
      end else if (lSearchRec.Name <> '') and (ExtNIIorIMG(lSearchRec.Name)) and (not DirExists(lSearchRec.Name)) then begin
	      	str := str +pre+ ''''+extractfilename(lSearchRec.Name)+'''';
                pre:=','
      end;
      //mainform.Memo1.lines.add(lSearchRec.Name);
    until (FindNext(lSearchRec) <> 0);

 end;
 FindClose(lSearchRec);
 str := str + ')';
 if length(str) > 2 then
    NIIbatch (lDirName,str)//mainform.Memo1.lines.add(str)
 else
    mainform.Memo1.lines.add('No NIfTI images found in '+lDirName)

end;

(*procedure NII2Mat(lExt: string);
var
   str,pre,sTitle,lDirName: string;
   lSearchRec: TSearchRec;
begin
        {$IFNDEF FPC}
        sTitle:='Choose a folder with DICOM images';
      lDirName := BrowseDialog(sTitle);
      {$ELSE}
      if not OpenDialogExecute('Select DICOM images you wish to inspect)',true,false,kAnyFilter) then
         exit;
      lDirName := extractfiledir( OpenHdrDlg.Filename);
      {$ENDIF}
      str := '(';
      pre := '';

{$IFDEF UNIX}
 if FindFirst(lDirName+pathdelim+'*.img',faAnyFile-faSysFile,lSearchRec) = 0 then begin
{$ELSE}
 if FindFirst(lDirName+pathdelim+'*.img',faAnyFile-faSysFile,lSearchRec) = 0 then begin
{$ENDIF}
    //lFilename := '';
    repeat
      //lNewName := lNewDir+lSearchRec.Name;
      if  (lSearchRec.Name = '.') or (lSearchRec.Name = '..') then begin
            //
      end else if (lSearchRec.Name <> '') and (not DirExists(lSearchRec.Name)) then begin
	      	str := str +pre+ ''''+extractfilename(lSearchRec.Name)+'''';
                pre:=','
      end;
      //mainform.Memo1.lines.add(lSearchRec.Name);
    until (FindNext(lSearchRec) <> 0);

 end;
 FindClose(lSearchRec);
 str := str + ')';
 mainform.Memo1.lines.add(str);
end;      *)


(*procedure BenchMarkDicom;
var
    lC: Integer;
    lS: TDateTime;
    var lDICOMdata: DICOMdata;
         lHdrOK, lImageFormatOK: boolean;
         lDynStr: string;var lFileName: string;
    var lPrefs: TPrefs ;
begin
 SetDefaultPrefs (lPrefs);
lS := Now;
lFilename := '/Users/rorden/philips/T1_IM_0007';
for lC := 1 to 100 do
    read_dicom_data(true,false,false,false,false,false,false, lDICOMdata, lHdrOK, lImageFormatOK, lDynStr, lFileName, lPrefs);
Showmessage('Milliseconds elapsed '+  FormatDateTime('z', Now-lS) );
end; *)

procedure TMainForm.About1Click(Sender: TObject);
//var value: int64;
begin
//fx(VBversion('MR B13 4VB13A')); exit;
//NII2Mat;exit;
 //BenchMarkDicom;
{$IFNDEF FPC}
  if (ssCtrl in KeyDataToShiftState(vk_Shift))  then begin
    Mask;
    exit;
  end;
  if (ssShift in KeyDataToShiftState(vk_Shift))  then begin
    MaskVBM;
    exit;
  end;
{$ENDIF}
     //force32;
    //showmessage(ExtractFileDirWithPathDelim('c:\pas'));

    //testpermissions;
    Showmessage(kVers+ kCR+'Fallback ini file: '+ changefileext(paramstr(0),'.ini'));
end;

procedure TMainForm.Preferences1Click(Sender: TObject);
var
   lPrefs: TPrefs;
begin
     PrefsForm.ReadPrefs(lPrefs);
     PrefsForm.Showmodal;
     if (PrefsForm.ModalResult = mrCancel) then
        PrefsForm.WritePrefs(lPrefs);
end;

(*procedure ShowDICOM (var lPrefs: TPrefs);
var
lDICOMdata: DICOMdata;
lHdrOK,lImgOK: boolean;
lDynStr,lFilename: string;

begin
     lFilename := 'c:\i185386.MRDC.94';
     read_dicom_data(true,true{not verbose},true,true,true,true,false, lDICOMdata, lHdrOK, lImgOK, lDynStr,lFileName,lPrefs );
     msgX(lDynStr);
end;*)
procedure TMainForm.FormShow(Sender: TObject);
var
   lPrefs: TPrefs;
   lIniName: string;
begin
    MsgX(kVers);

    SetDefaultPrefs(lPrefs);
    lIniName := IniName;//changefileext(paramstr(0),'.ini');
    //showmessage(changefileext(paramstr(0),'.ini'));
    (*lReadPrefs := true;
    if (ssShift in KeyDataToShiftState(vk_Shift))  then
    	case MessageDlg('Shift key down during launch: do you want to reset the default preferences?', mtConfirmation,
				[mbYes, mbNo], 0) of	{ produce the message dialog box }
				mrYes: lReadPrefs := false;
        end; //case *)
    if not ResetDefaults {lReadPrefs} then begin
    {$IFNDEF UNIX}
     if (ParamCount > 0) then
        ProcessParamStrs
     else if fileexists (lIniName) then
          IniFile(True,lIniName, lPrefs)
     else
         IniFile(True,changefileext(paramstr(0),'.ini'), lPrefs); //this allows an administrator to create default startup
         //IniFile(True,lIniName, lPrefs);
     {$ELSE}
     if fileexists (lIniName) then
          IniFile(True,lIniName, lPrefs)
     else
         IniFile(True,changefileext(paramstr(0),'.ini'), lPrefs); //this allows an administrator to create default startup
     {$ENDIF}
   end; //lReadPrefs
    CheckPrefs(lPrefs,True);
     PrefsForm.WritePrefs(lPrefs);
     NIfTIForm.TypeCombo.ItemIndex := TypeCombo.ItemIndex;
    UntestedMenu.visible := lPrefs.UntestedFeatures;
     //ConvertDCM2NII('c:\b17\b17\b17.IMA',lPrefs);
end;



procedure TMainForm.TypeComboChange(Sender: TObject);
begin
     NIfTIForm.TypeCombo.ItemIndex := TypeCombo.ItemIndex;
end;

procedure TMainForm.AnonymizeDICOM1Click(Sender: TObject);
var
   sTitle,lDirName: string;
   lPrefs: TPrefs;
begin
  CheckPrefs(lPrefs,False);
  lPrefs.AnonymizeSourceDICOM := true;
  sTitle:='Choose a folder with DICOM images';
    lDirName := BrowseDialog(sTitle);
    ConvertDCM2NII(lDirName,lPrefs);
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

procedure TMainForm.SavePrefs;
var
   lPrefs: TPrefs;
   lIniName: string;
begin
    lIniName := IniName;//changefileext(paramstr(0),'.ini');
    CheckPrefs(lPrefs,False);
    if lPrefs.WritePrefsOnQuit then
       IniFile(False,lIniName, lPrefs);
end;

procedure TMainForm.FormClose(Sender: TObject; var vAction: TCloseAction);
begin
  SavePrefs;
end;


procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
   lI,lN: integer;
  lPrefs: TPrefs;
  lStrings: TStrings;//lFilename: string;
begin
     //lDone := false;
     CheckPrefs(lPrefs,False);
     lN := length(FileNames);
     if lN < 1 then
        exit;
      lStrings := TStringList.Create;
      for lI := 0 to (lN-1) do
         lStrings.add(Filenames[lI]);
     ProcessFilenames(lStrings,lPrefs);
    lStrings.Free;

end;

procedure TMainForm.HalveMenu1Click(Sender: TObject);
var
   lPrefs: TPrefs;
   lI: integer;
begin
     PrefsForm.ReadPrefs(lPrefs);
     if not MainForm.OpenDialogExecute('Select image(s) you wish to LR flip',true,false,kImgFilter) then
        exit;
     if MainForm.OpenHdrDlg.Files.count < 1 then exit;
     for lI := 0 to (MainForm.OpenHdrDlg.Files.count-1) do
         ShrinkNII(MainForm.OpenHdrDlg.Files[lI], lPrefs);
end;

procedure TMainForm.ModifyNIfTI1Click(Sender: TObject);
var
   lPrefs: TPrefs;
begin
 if not OpenDialogExecute('Select NIfTI images you wish to modify)',true,false,kImgFilter) then
    exit;
 CheckPrefs(lPrefs,False);
 ProcessNIfTI(OpenHdrDlg.Files,lPrefs);
end; //ModifyNIfTI1Click

procedure TMainForm.NIfTI3D4D1Click(Sender: TObject);
var lStrings: TStringList;
   lPrefs: TPrefs;
begin
 if not OpenDialogExecute('Select the 3D NIfTI images you wish to stack)',true,false,kImgFilter) then
    exit;
 lStrings := TStringList.Create;
 lStrings.addstrings(OpenHdrDlg.Files);
 CheckPrefs(lPrefs,False);
 Stack3Dto4D(lStrings, False, lPrefs);
 lStrings.Free;
end;

procedure TMainForm.ResliceNIfTI1Click(Sender: TObject);
var
   lDestName,lSourceName,lTargetName: string;
   lPos: integer;
   lPrefs: TPrefs;
begin
         CheckPrefs(lPrefs,False);
     Memo1.lines.clear;
     refresh;
     MsgX(kVers);
     MsgX('This function reslices source images to match the dimensions of a target image.');
     MsgX(' Images are assumed to be coregistered.');
     MsgX(' The resulting images will have the orientation, voxel size and bounding box of the target image.');
     MsgX(' Resliced images will be given the prefix ''r''.');
     MsgX(' This function uses trilinear interpolation - there may be some loss of precision.');
     if not OpenDialogExecute('Select target image',true,false,kImgFilter) then
        exit;
     lTargetName := OpenHdrDlg.Filename;
     if not OpenDialogExecute('Select images you wish to reslice to match target)',true,false,kImgFilter) then
        exit;
     for lPos := 1 to OpenHdrDlg.Files.Count do begin
         lSourceName := OpenHdrDlg.Files[lPos-1];
         lDestName := ChangeFilePrefix (lSourceName,'r');
         MsgX('Reslicing '+lSourceName +' to match dimensions of '+lTargetname+' resliced image = '+lDestName);
         Reslice2Targ (lSourceName,lTargetName,lDestName, lPrefs );

     end;

end;


procedure DelRecursiveFolderSearch (lFolderName: string; lMaxDepth, lDepth: integer);
var
 lNewDir,lNewName,lFilename,lExt: String;
 lSearchRec: TSearchRec;
begin
 lNewDir := lFolderName+PathDelim;
{$IFDEF UNIX}
 if FindFirst(lNewDir+'*',faAnyFile-faSysFile,lSearchRec) = 0 then begin
{$ELSE}
 if FindFirst(lNewDir+'*.*',faAnyFile-faSysFile,lSearchRec) = 0 then begin
{$ENDIF}
    lFilename := '';
    repeat
      lNewName := lNewDir+lSearchRec.Name;
      if  (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') then begin
            if DirExists(lNewName) then begin
               if lDepth < lMaxDepth then begin
                  DelRecursiveFolderSearch(lNewName,lMaxDepth,lDepth+1);
               end;
               //exit;//4/4/2008
            end else
                lFilename := lNewname;
      end;
      if (lFilename <> '') and (not DirExists(lNewName)) then begin
	      	lExt := UpCaseExt(lFilename);
                if (lExt <> '.DCM') then begin
                   msgx('del '+lFilename);
                   DeleteFile(lFilename);
                end;
      end;
    until (FindNext(lSearchRec) <> 0);

 end;
 FindClose(lSearchRec);
end;


procedure TMainForm.Deletenondcm1Click(Sender: TObject);
var
   sTitle,lDirName: string;
begin
  Showmessage('Warning: this command will delete all files that do not have the extension .dcm') ;
  sTitle:='Choose a folder with DICOM images';
    lDirName := BrowseDialog(sTitle);
    DelRecursiveFolderSearch(lDirName,32,1);
end;

procedure TMainForm.MirrorXdimension1Click(Sender: TObject);
//UntestedFeatures
var
   lPrefs: TPrefs;
   lI: integer;
begin
     PrefsForm.ReadPrefs(lPrefs);
     if not MainForm.OpenDialogExecute('Select image(s) you wish to LR flip',true,false,kImgFilter) then
        exit;
     if MainForm.OpenHdrDlg.Files.count < 1 then exit;
     for lI := 0 to (MainForm.OpenHdrDlg.Files.count-1) do
         LRFlip(MainForm.OpenHdrDlg.Files[lI], lPrefs);
end;

procedure TMainForm.SumTPM1Click(Sender: TObject);
var
   lPrefs: TPrefs;
   lI: integer;
begin
     PrefsForm.ReadPrefs(lPrefs);
     if not OpenDialogExecute('Select TPM to sum)',true,false,kAnyFilter) then
          exit;
     for lI := 1 to 5 do
         SumTPM(OpenHdrDlg.Filename,ChangeFilePrefix (OpenHdrDlg.Filename,'sum'+inttostr(lI)) ,lPrefs,lI);
end;


procedure TMainForm.ExtractDICOMdims1Click(Sender: TObject);
var
   {$IFNDEF FPC}sTitle,{$ENDIF}
     lDirName: string;
   lPrefs: TPrefs;
begin
        CheckPrefs(lPrefs,False);
        lPrefs.DebugMode2 := true;
        {$IFNDEF FPC}
        sTitle:='Choose a folder with DICOM images';
      lDirName := BrowseDialog(sTitle);
      {$ELSE}
      if not OpenDialogExecute('Select DICOM images you wish to inspect)',true,false,kAnyFilter) then
         exit;
      lDirName := extractfiledir( OpenHdrDlg.Filename);
      {$ENDIF}
      Memo1.lines.Clear;
      ConvertDCM2NII(lDirName,lPrefs);
end;

procedure TMainForm.ExtractDICOMhdr1Click(Sender: TObject);
var
   lnVol,lVol: integer;
   //lHdrName: string;
begin
 if not OpenDialogExecute('Select the 3D NIfTI images to inspect)',true,false,kAnyFilter) then
    exit;                                
 lnVol := OpenHdrDlg.Files.count;
   Memo1.lines.clear;
   for lVol := 1 to lnVol do
      ShowHeader (OpenHdrDlg.Files[lVol-1]);
end;

procedure TMainForm.ExtractNIfTIhdrs1Click(Sender: TObject);
var
          lStrings: TStringList;
begin
 if not OpenDialogExecute('Select the 3D NIfTI images to inspect)',true,false,kImgFilter) then
    exit;
 Memo1.lines.clear;
 lStrings := TStringList.Create;
 lStrings.addstrings(OpenHdrDlg.Files);
 ExtractNIFTIHdrs(lStrings);
 lStrings.Free;
end;

{$IFDEF UNIX}
initialization
  {$I gui.lrs}
{$ELSE} //not unix: windows
initialization
{$IFDEF FPC}
  {$I gui.lrs}
 {$ENDIF}
  OleInitialize(nil);

finalization
  OleUninitialize
{$ENDIF}
end.


