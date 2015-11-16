unit prefs;
{$H+}
{$Include ..\common\isgui.inc}
interface
uses
  {$IFDEF FPC}
  {$IFDEF UNIX} BaseUnix,{$ENDIF}
  {$IFDEF GUI}LResources, {$ENDIF}
  
  {$ELSE}
   SelectFolder,
{$ENDIF}
  inifiles, define_types,SysUtils, userdir, dialogsx, dialogs_msg;

type
  TPrefs = record
         WritePrefsOnQuit,OrthoFlipXDim,RecursiveUseNameAppend,AnonymizeSourceDICOM, ManualNIfTIConv,Anonymize,
          SingleNIIFile,Gzip,SPM2,VOI,enablereorient,createoutputfolder,
         AppendDate,AppendAcqSeries,AppendProtocolName,AppendPatientName,AppendFilename,
         everyfile,fourD,Swizzle4D,Stack3DImagesWithSameAcqNum,customRename,
         CollapseFolders,AutoCrop, UseGE_0021_104F, PhilipsPrecise,Verbose, PlanarRGB,
         DebugMode,DebugMode2,UntestedFeatures,UINT16toFLOAT32, TxtReport: boolean;

         BeginClip, LastClip,SiemensDTIUse0019If00181020atleast,
         SiemensDTINoAngulationCorrectionIf00181020atleast,
         SiemensDTIStackIf00181020atleast,
         OutDirMode, MinReorientMatrix,MaxReorientMatrix,RecursiveFolderDepth,usePigz
         : integer;
         OutDir, BackupDir,NameAppend: string;
  end;
const
     kOutDirModeInput = 0;//save output files to source folder
     kOutDirModePrompt = 1;//prompt user to specify location of output dir
     kOutDirModeOutDir = 2;//save output to lPrefs.OutDir
     kMinReorientMatrix = 200; //reorient images with matrices > this value
procedure SetOutputFormat (lItemIndex: integer; var lPrefs: TPrefs);
procedure SetDefaultPrefs (var lPrefs: TPrefs);
procedure CorrectPrefs (var lPrefs: TPrefs); //ensures only usable file types are created
function IniFile(lRead: boolean; lFilename: string; var lPrefs: TPrefs): boolean;
function DefaultOutputFormat (lPrefs: TPrefs): integer;

implementation

function DefaultOutputFormat (lPrefs: TPrefs): integer;
begin
     if lPrefs.SPM2 then
          result := 0 //SPM2 3D hdr/img analyze
     else if not lPrefs.FourD  then begin
            if not (lPrefs.SingleNIIFile) then
               result := 1 //SPM5 3D hdr/img
            else
                result:= 2; //SPM8 3D nii
     end else if not lPrefs.SingleNIIFile then
            result := 3 //?? 4D hdr/img
     else if not lPrefs.GZip then
            result := 4 //FSL 4D nii
     else
           result := 5; //FSL 4D nii.gz
end;

procedure SetOutputFormat (lItemIndex: integer; var lPrefs: TPrefs);
//SetOutputFormat(n,lPrefs) : 0=SPM2,1=SPM5,2=spm8,3=4D hdr/img,4=fsl(default),5=fsl.gz, 6=.voi
begin
    //next: options for reading;
    lPrefs.VOI := false;
    lPrefs.SPM2 := false;
    lPrefs.fourD := true;
    lPrefs.SingleNIIFile := true;
    lPrefs.GZip := false;
    case lItemIndex of
         0: begin//spm2
                lPrefs.SPM2 := true;
                lPrefs.fourD := false;
                lPrefs.SingleNIIFile := false;
                end;
         1: begin//spm5
                lPrefs.fourD := false;
                lPrefs.SingleNIIFile := false;
                end;
         2: begin//spm8
                lPrefs.fourD := false;
                end;
         3: lPrefs.SingleNIIFile := false;//4D Hdr/Img
         5: lPrefs.GZip := true;//FSL compressed
         6: begin //VOI
            lPrefs.GZip := true;//FSL compressed
            lPrefs.VOI := true;
         end;
    end;//case
end;

procedure CorrectPrefs (var lPrefs: TPrefs); //ensures only usable file types are created
begin
  if lPrefs.SingleNIIFile then
     lPrefs.SPM2 := false; //SPM2 only reads .hdr/.img - loses NIfTI information
  if not lPrefs.SingleNIIFile then
     lPrefs.Gzip := false; //nii.gz is OK, but img.gz is not
end;

procedure SetDefaultPrefs (var lPrefs: TPrefs);
begin
 with lPrefs do begin
  OutDirMode := kOutDirModeInput;
  SiemensDTIUse0019If00181020atleast := 15;
  SiemensDTINoAngulationCorrectionIf00181020atleast := 1000;
  SiemensDTIStackIf00181020atleast := 15;
  //IgnoreDTIRotationsIf_0002_0013_atleast := 15;
  VOI := false;
  OutDir := UserDataFolder;
  PhilipsPrecise := false;
  UseGE_0021_104F := false;
  CollapseFolders := true;
  AutoCrop := false; //for dcm2nii - reorient and crop 3D nifti input images...
  CustomRename := false;
  createoutputfolder := false;
  Stack3DImagesWithSameAcqNum := false;
  RecursiveUseNameAppend := false;//changes paramstrs.pas recursive search to add top level folder name
  DebugMode := false;
  DebugMode2 := false;
  UntestedFeatures := false;
  TxtReport := false;
  PlanarRGB := false;
  Verbose := false;
  SingleNIIFile := true;
  Gzip := true;
  OrthoFlipXDim := false;
  SPM2 := false;
  Anonymize := true;
  AppendDate := true;
  AppendAcqSeries := true;
  AppendProtocolName := true;
  AppendPatientName := false;
  AppendFilename := false;
  EveryFile:=true;
  FourD := true;
  enablereorient := true;
  ManualNIfTIConv := true;
  AnonymizeSourceDICOM := false;
  Swizzle4D := true;
  RecursiveFolderDepth := 5;
  MinReorientMatrix := kMinReorientMatrix;
  MaxReorientMatrix := 1023;
  NameAppend := '';
  BackupDir := '';
  WritePrefsOnQuit := true;
  UINT16toFLOAT32 := true;
  BeginClip := 0;
  LastClip := 0;
  usePigz := 0;
 end;
end;

procedure IniInt(lRead: boolean; lIniFile: TCustomIniFile; lIdent: string;  var lValue: integer);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('INT',lIdent,IntToStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('INT',lIdent, '');
	if length(lStr) > 0 then
		lValue := StrToInt(lStr);
end; //IniInt

procedure IniBool(lRead: boolean; lIniFile: TCustomIniFile; lIdent: string;  var lValue: boolean);
//read or write a boolean value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('BOOL',lIdent,Bool2Char(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('BOOL',lIdent, '');
	if length(lStr) > 0 then
		lValue := Char2Bool(lStr[1]);
end; //IniBool

procedure IniStr(lRead: boolean; lIniFile: TCustomIniFile; lIdent: string; var lValue: string);
//read or write a string value to the initialization file
begin
  if not lRead then begin
    lIniFile.WriteString('STR',lIdent,lValue);
    exit;
  end;
	lValue := lIniFile.ReadString('STR',lIdent, '');
end; //IniStr

function IniFile(lRead: boolean; lFilename: string; var lPrefs: TPrefs): boolean;
//Read or write initialization variables to disk
var
  lIniFile: TMemIniFile;
begin
  result := false;
  if (lRead) and (not Fileexists(lFilename)) then
        exit;
  {$IFDEF UNIX}  //Uses BaseUnix;
  if (lRead) and (fpAccess (lFilename,R_OK)<>0) then begin//ensure user has read-access to prefs file...
    dcmMsg('Unable to load preferences: no write access for '+lFilename);
    exit;
  end;
  {$ENDIF}
  if (lRead) then begin
     Filemode := 0; //Readonly
     dcmMsg('reading preferences file '+lFilename);
  end else
      Filemode := 2; //Read-Write
  //lIniFile := TIniFile.Create(lFilename);
  lIniFile := TMemIniFile.Create(lFilename);
  IniBool(lRead,lIniFile,'DebugMode',lPrefs.DebugMode);
  IniBool(lRead,lIniFile,'UntestedFeatures',lPrefs.UntestedFeatures);
  IniBool(lRead,lIniFile,'TxtReport',lPrefs.TxtReport);
  IniBool(lRead,lIniFile,'UINT16toFLOAT32',lPrefs.UINT16toFLOAT32);
  IniBool(lRead,lIniFile,'PlanarRGB',lPrefs.PlanarRGB);
  IniBool(lRead,lIniFile,'Verbose',lPrefs.Verbose);
  IniBool(lRead,lIniFile,'Anonymize',lPrefs.Anonymize);
  IniBool(lRead,lIniFile, 'AnonymizeSourceDICOM',lPrefs.AnonymizeSourceDICOM);
  IniBool(lRead,lIniFile,'AppendAcqSeries',lPrefs.AppendAcqSeries);
  IniBool(lRead,lIniFile,'AppendDate',lPrefs.AppendDate);
  IniBool(lRead,lIniFile,'AppendFilename',lPrefs.AppendFilename);
  IniBool(lRead,lIniFile,'AppendPatientName',lPrefs.AppendPatientName);
  IniBool(lRead,lIniFile,'AppendProtocolName',lPrefs.AppendProtocolName);
  IniBool(lRead,lIniFile,'AutoCrop',lPrefs.AutoCrop);
  IniBool(lRead,lIniFile,'CollapseFolders',lPrefs.CollapseFolders);
  IniBool(lRead,lIniFile,'createoutputfolder',lPrefs.createoutputfolder);
  IniBool(lRead,lIniFile,'CustomRename',lPrefs.CustomRename);
  IniBool(lRead,lIniFile,'enablereorient',lPrefs.enablereorient);
  IniBool(lRead,lIniFile,'OrthoFlipXDim',lPrefs.OrthoFlipXDim);
  IniBool(lRead,lIniFile,'EveryFile',lPrefs.EveryFile);
  IniBool(lRead,lIniFile,'fourD',lPrefs.fourD);
  IniBool(lRead,lIniFile,'Gzip',lPrefs.Gzip);
  IniBool(lRead,lIniFile,'ManualNIfTIConv',lPrefs.ManualNIfTIConv);
  IniBool(lRead,lIniFile,'PhilipsPrecise',lPrefs.PhilipsPrecise);
  IniBool(lRead,lIniFile,'RecursiveUseNameAppend',lPrefs.RecursiveUseNameAppend);
  IniBool(lRead,lIniFile,'SingleNIIFile',lPrefs.SingleNIIFile);
  IniBool(lRead,lIniFile,'SPM2',lPrefs.SPM2);
  IniBool(lRead,lIniFile,'Stack3DImagesWithSameAcqNum',lPrefs.Stack3DImagesWithSameAcqNum);
  IniBool(lRead,lIniFile,'Swizzle4D',lPrefs.Swizzle4D);
  IniBool(lRead,lIniFile,'UseGE_0021_104F',lPrefs.UseGE_0021_104F);

  IniInt(lRead,lIniFile,'BeginClip',lPrefs.BeginClip);
  IniInt(lRead,lIniFile,'LastClip',lPrefs.LastClip);
  IniInt(lRead,lIniFile,'usePigz',lPrefs.usePigz);
	IniInt(lRead,lIniFile,'MaxReorientMatrix',lPrefs.MaxReorientMatrix);
	IniInt(lRead,lIniFile,'MinReorientMatrix',lPrefs.MinReorientMatrix);
	IniInt(lRead,lIniFile,'RecursiveFolderDepth',lPrefs.RecursiveFolderDepth);
	IniInt(lRead,lIniFile,'OutDirMode',lPrefs.OutDirMode);

  IniInt(lRead,lIniFile,'SiemensDTIUse0019If00181020atleast',lPrefs.SiemensDTIUse0019If00181020atleast);
  IniInt(lRead,lIniFile,'SiemensDTINoAngulationCorrectionIf00181020atleast',lPrefs.SiemensDTINoAngulationCorrectionIf00181020atleast);
  IniInt(lRead,lIniFile,'SiemensDTIStackIf00181020atleast',lPrefs.SiemensDTIStackIf00181020atleast);
  lPrefs.BackupDir := lIniFile.ReadString('STR','BackupDir',lPrefs.BackupDir);
  IniStr(lRead,lIniFile,'OutDir',lPrefs.OutDir);
  if (lPrefs.OutDirMode < kOutDirModeInput) or (lPrefs.OutDirMode > kOutDirModeOutDir) then
           lPrefs.OutDirMode := kOutDirModeOutDir;
  if (lRead) and (not(DirExists(lPrefs.OutDir))) then
     lPrefs.OutDir := UserDataFolder;
  if not lRead then
    lIniFile.UpdateFile;
  lIniFile.Free;
  if (lRead) then
     Filemode := 2; //Read-write
end;

end.



