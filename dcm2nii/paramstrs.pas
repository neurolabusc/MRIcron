unit paramstrs;
   {$H+}
interface
uses prefs,define_types,dialogs_msg;
const
kVers = 'Chris Rorden''s dcm2nii: '+kMRIcronvers+' (obsolete use dcm2niix)';
{$Include ..\common\isgui.inc}
procedure ProcessParamStrs;
// procedure Testdcm2nii;
procedure RecursiveFolderSearch (lFolderName,lOutDir: string; var lPrefs: TPrefs; lDepth: integer);
implementation

uses
{$IFDEF GUI}gui, {$ELSE} nii_4dto3d,{$ENDIF}

{$IFNDEF UNIX}
    Windows,{$ENDIF} Classes,
  SysUtils,sortdicom,dicom,parconvert,filename,dicomtypes,userdir;

(*procedure RecursiveFolderSearch (lFolderName,lOutDir: string; var lPrefs: TPrefs; lDepth: integer);
var
 lNewDir,lNewName,lFilename,lExt: String;
 lSearchRec: TSearchRec;
begin
 if (lPrefs.CollapseFolders) then begin //Convert all folders in single step...
    LoadFileList(lFolderName,lOutDir,lPrefs);
    exit;
 end;
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
               if lDepth < lPrefs.RecursiveFolderDepth then begin
                  if (lDepth = 0) and (lPrefs.RecursiveUseNameAppend) then begin
                     lPrefs.NameAppend := extractfilename(lNewName)+'_';
                     Msg('recursive base folder '+lPrefs.NameAppend);
                  end;
                  RecursiveFolderSearch(lNewName,lOutDir,lPrefs,lDepth+1);
               end;
               //exit;//4/4/2008
            end else
                lFilename := lNewname;
      end;
    until (FindNext(lSearchRec) <> 0);
    if lFilename <> '' then begin
	      	lExt := UpCaseExt(lFilename);
                if (lExt = '.REC') or (lExt = '.PAR') then
			LoadFileListPARREC(lFilename,lOutDir,lPrefs)
		else begin
			Msg('recursive conversion '+lFilename);
                        LoadFileList(lFilename,lOutDir,lPrefs);
                end;
     end;
 end;
 FindClose(lSearchRec);
end;  *)

function IsDicom (lFilename: string): boolean;
var
   lDICOMdata: DICOMData;
   lDynStr: string;
   lHdrOK,lImgOK: boolean;
   lPrefs: TPrefs;
   lDTIra: TDTIRA;
begin
     result := false;
     read_dicom_data(true,false{not verbose},true,true,true,true,false, lDICOMdata, lDTIra, lHdrOK, lImgOK, lDynStr,lFileName,lPrefs );
     if (lHdrOK) and (lImgOK) then
        result := true;
end;

procedure RecursiveFolderSearch (lFolderName,lOutDir: string; var lPrefs: TPrefs; lDepth: integer);
var
 lNewDir,lFilename,lExt: String;
 lDirStrings: TStringList;
 lSearchRec: TSearchRec;
 lI: integer;
begin
 if (lPrefs.CollapseFolders) then begin //Convert all folders in single step...
    LoadFileList(lFolderName,lOutDir,lPrefs);
    exit;
 end;
 lNewDir := lFolderName+PathDelim;
 //first check for deeper folders...
 {$IFDEF DEBUG}Msg('---Recursively searching '+lfoldername+' depth = '+inttostr(lDepth)); {$ENDIF}
 if lDepth < lPrefs.RecursiveFolderDepth then begin
    lDirStrings := TStringList.Create;
    {$IFDEF UNIX}
    if FindFirst(lNewDir+'*',faDirectory,lSearchRec) = 0 then begin
    {$ELSE}
    if FindFirst(lNewDir+'*.*',faDirectory,lSearchRec) = 0 then begin
    {$ENDIF}
      lFilename := '';
      repeat
        if  (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') then begin
          lFileName := lNewDir+lSearchRec.Name;
          if DirExists(lFileName) then begin
             (*if (lDepth = 0) and (lPrefs.RecursiveUseNameAppend) then begin
                     lPrefs.NameAppend := extractfilename(lSearchRec.Name)+'_';
                     Msg('recursive base folder '+lPrefs.NameAppend);
             end;  *)
             lDirStrings.Add(lNewDir+lSearchRec.Name);
          end;
        end;
      until (FindNext(lSearchRec) <> 0);
    end; //findfirst
    FindClose(lSearchRec);
    lDirStrings.Sort;
    if lDirStrings.Count > 0 then
     for lI := 0 to (lDirStrings.Count-1) do begin
               if (lDepth = 0) and (lPrefs.RecursiveUseNameAppend) then begin
                       lPrefs.NameAppend := extractfilename(lDirStrings[lI])+'_';
                       dcmMsg('recursive base folder '+lPrefs.NameAppend);
               end;
               RecursiveFolderSearch(lDirStrings[lI],lOutDir,lPrefs,lDepth+1);
     end;
    lDirStrings.free;
 end; //lDepth < lPrefs.RecursiveFolderDepth
 //next check for files in current folder...
 {$IFDEF UNIX}
 if FindFirst(lNewDir+'*',faAnyFile-faSysFile-faHidden,lSearchRec) = 0 then begin
{$ELSE}
 if FindFirst(lNewDir+'*.*',faAnyFile-faSysFile-faHidden,lSearchRec) = 0 then begin
{$ENDIF}
        repeat
          lFileName := lNewDir+lSearchRec.Name;
          lExt := UpCaseExt(lFilename);
          if (lFilename <> '') and (FileExists(lFileName)) and (not DirExists(lFileName)) and (not IsNiftiExt(lExt)) then begin
                if (lExt = '.REC') or (lExt = '.PAR') then
			LoadFileListPARREC(lFilename,lOutDir,lPrefs)
		else if IsDicom (lFilename) then begin
			dcmMsg('Looking for DICOM files in folder with '+lFilename);
                        LoadFileList(lFilename,lOutDir,lPrefs);
                end else
                    lFilename := '';
          end else
              lFilename :='';
        until (FindNext(lSearchRec) <> 0) or (lFilename <> '');
  end; //findfirst
  FindClose(lSearchRec);
end; //RecursiveFolderSearch

function Bool2YN (lBool: boolean): char;
begin
	if lBool then
		result := 'Y'
	else
		result := 'N';
end;

procedure CharBool (lCh: char; var lBool: boolean);
begin
	if lCh = 'Y' then
		lBool := true;
	if lCh = 'N' then
		lBool := false;
end;

procedure ShowHelp (var lIniName: string; lPrefs: TPrefs);
begin
	 dcmMsg('Either drag and drop or specify command line options:');
	 dcmMsg('  '+FilenameWOExt(paramstr(0))+' <options> <sourcenames>');
	 dcmMsg('OPTIONS:');
         dcmMsg('-4 Create 4D volumes, else DTI/fMRI saved as many 3D volumes: Y,N = '+Bool2YN(lPrefs.FourD));
         dcmMsg('-3 Create planar RGB images: Y,N = '+Bool2YN(lPrefs.PlanarRGB));
         dcmMsg('-a Anonymize [remove identifying information]: Y,N = '+Bool2YN(lPrefs.Anonymize));
	 dcmMsg('-b Load settings from specified inifile, e.g. ''-b C:\set\t1.ini''  ');
	 dcmMsg('-c Collapse input folders: Y,N = '+Bool2YN(lPrefs.CollapseFolders));
	 dcmMsg('-d Date in filename [filename.dcm -> 20061230122032.nii]: Y,N = '+Bool2YN(lPrefs.AppendDate));
	 dcmMsg('-e Events (series/acq) in filename [filename.dcm -> s002a003.nii]: Y,N = '+Bool2YN(lPrefs.AppendAcqSeries));
	 dcmMsg('-f Source filename [e.g. filename.par -> filename.nii]: Y,N = '+Bool2YN(lPrefs.AppendFilename));
	 dcmMsg('-g Gzip output, filename.nii.gz [ignored if ''-n n'']: Y,N = '+Bool2YN(lPrefs.Gzip));
	 dcmMsg('-i ID  in filename [filename.dcm -> johndoe.nii]: Y,N = '+Bool2YN(lPrefs.AppendPatientName));
         dcmMsg('-k sKip initial n volumes in fMRI, e.g. ''-k 2'':  = '+inttostr(lPrefs.BeginClip));
         dcmMsg('-l pLanar RGB (Y=old Analyze; N=new VTK NIfTI): Y,N = '+Bool2YN(lPrefs.PlanarRGB));
         dcmMsg('-m Manually prompt user to specify output format [NIfTI input only]: Y,N = '+Bool2YN(lPrefs.ManualNIfTIConv));
	 dcmMsg('-n Output .nii file [if no, create .hdr/.img pair]: Y,N = '+Bool2YN(lPrefs.SingleNIIFile));
	 dcmMsg('-o Output Directory, e.g. ''C:\TEMP'' (if unspecified, source directory is used)');
	 dcmMsg('-p Protocol in filename [filename.dcm -> TFE_T1.nii]: Y,N = '+Bool2YN(lPrefs.AppendProtocolName));
	 dcmMsg('-r Reorient image to nearest orthogonal: Y,N ');
	 dcmMsg('-s SPM2/Analyze not SPM5/NIfTI [ignored if ''-n y'']: Y,N = '+Bool2YN(lPrefs.SPM2));
         dcmMsg('-t Text report (patient and scan details): Y,N = '+Bool2YN(lPrefs.txtReport));
   dcmMsg('-v Convert every image in the directory: Y,N = '+Bool2YN(lPrefs.EveryFile));
   dcmMsg('-x Reorient and crop 3D NIfTI images: Y,N = '+Bool2YN(lPrefs.Autocrop));
	 dcmMsg('  You can also set defaults by editing '+lIniName);
{$IFDEF UNIX}
	 dcmMsg('EXAMPLE: '+FilenameWOExt(paramstr(0))+' -a y /Users/Joe/Documents/dcm/IM_0116');
{$ELSE}
	 dcmMsg('EXAMPLE: '+FilenameWOExt(paramstr(0))+' -a y -o C:\TEMP C:\DICOM\input1.par C:\input2.par');
	 dcmMsg('Hit <Enter> to exit.');

     {$IFNDEF GUI}{$IFNDEF UNIX}if IsConsole then ReadLn;{$ENDIF}{$ENDIF}

{$ENDIF}
end; //proc ShowHelp

 (*procedure Testdcm2nii;
 var
   lIniName : string;
   lPrefs: TPrefs;
 begin
  lIniName := IniName;//DefaultsDir('')+ParseFileName(ExtractFilename(paramstr(0) ) )+'.ini';
     IniFile(True,lIniName, lPrefs);

     ModifyAnalyze('C:\4d\4d.nii', lPrefs)
 end; *)

function CustomIni: boolean; //returns true if user specifies a custom ini file
var
  i: integer;
  lStr: string;
begin
  result := false;

  if (ParamCount < 1) then exit;
  for i := 1 to ParamCount do begin
    lStr := UpcaseStr(ParamStr(I));
		if (length(lStr)>1) and (lStr[1] = '-') and (lStr[2] = 'B') then
      result := true;
  end;
end;



procedure ProcessParamStrs;
var
   lDir,lStr,lOutDir,lExt: String;
   {$IFNDEF UNIX}lStartTime: DWord;{$ENDIF}
   lHelpShown,lAbort,lSilent: boolean;
   lCommandChar: Char;
   lPrefs: TPrefs;
   P,I: integer;
  lIniName : string;
begin
  if (ParamCount > 0) then
       ExitCode := 1;//assume error ... will be set to 0 on successful processing of any files...
  SetDefaultPrefs (lPrefs);
  {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator  := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$ENDIF}
  lHelpShown := false;
  lAbort := false;
  lSilent := false;
  if not CustomIni then begin //if the user specifies a custom ini file, do not load the default file....
    lIniName := IniName;//DefaultsDir('')+ParseFileName(ExtractFilename(paramstr(0) ) )+'.ini';
    if fileexists (lIniName) then
     IniFile(True,lIniName, lPrefs)
    else
      IniFile(True,changefileext(paramstr(0),'.init'), lPrefs); //this allows an administrator to create default startup
  end;
  lOutDir := '';
  //dcm2nii will save nii as default, dcm2niiz will default to gzip, dcm2nii3d will make 3d files..
  lStr := UpcaseStr(FilenameWOExt(paramstr(0)));
  I := length(lStr);
  if I > 1 then begin
	lCommandChar := lStr[I];
	if (lCommandChar = 'G') or (lCommandChar = 'R') then
		lPrefs.SingleNIIFile := false
	else if (lCommandChar = 'Z') then
		lPrefs.Gzip := true;
	for P := 1 to I do
		if lStr[P] in ['0'..'9'] then
			lCommandChar := lStr[P];
	if (lCommandChar = '3')  then
		lPrefs.FourD := false
  end;
  //now read filename
  lStr := paramstr(0);
  lStr := extractfilename(lStr);
  lStr := string(StrUpper(PChar(lStr))) ;
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
		   lCommandChar := UpCase(lStr[2]);
           inc(I);
           lStr := ParamStr(I);
		   {$IFDEF UNIX}
		   if (lCommandChar <> 'O') and (lCommandChar <> 'B') then begin
                      lStr := string(StrUpper(PChar(lStr))) ;  //do not upcase paths...
		   end;
		   {$ELSE}
		   lStr := string(StrUpper(PChar(lStr))) ;
		   {$ENDIF}
		   case lCommandChar of
				'4': CharBool(lStr[1],lPrefs.FourD);
				'A': CharBool(lStr[1],lPrefs.Anonymize);
                                'C': CharBool(lStr[1],lPrefs.CollapseFolders);
				'D': CharBool(lStr[1],lPrefs.AppendDate);
				'E': CharBool(lStr[1],lPrefs.AppendAcqSeries);
				'F': CharBool(lStr[1],lPrefs.AppendFilename);
				'G': CharBool(lStr[1],lPrefs.Gzip);
				'I': CharBool(lStr[1],lPrefs.AppendPatientName);
                                'K': lPrefs.BeginClip:= strtoint(lStr);
                                'L': CharBool(lStr[1],lPrefs.PlanarRGB);
				'M': CharBool(lStr[1],lPrefs.ManualNIfTIConv);
				'N': CharBool(lStr[1],lPrefs.SingleNIIFile);
				'P': CharBool(lStr[1],lPrefs.AppendProtocolName);
                                'R': CharBool(lStr[1],lPrefs.enablereorient);
				'S': CharBool(lStr[1],lPrefs.SPM2);
                                'T': CharBool(lStr[1],lPrefs.txtReport);
				'V': CharBool(lStr[1],lPrefs.EveryFile);
                                'X': CharBool(lStr[1],lPrefs.Autocrop);
				'B': begin //load INI file
					  lIniName := lStr;
					  if fileexists(lIniName) then begin
                                                 IniFile(True,lIniName, lPrefs);
					 end else
		                             dcmMsg('0 ERROR: unable to find '+lIniName);
                                     end;
				'O': begin //output directory
					  lOutDir := '';
					  if direxists(lStr) then begin
						 lOutDir := lStr;
						 if lOutDir[length(lOutDir)] <> pathdelim then
							lOutDir := lOutDir + pathdelim;
					  end;
					 end;
		   end; //case lStr[2]
		   lStr := '';
		end; //special command
	 until (I=ParamCount) or (fileexists(lStr)) or (lAbort);
	 if (not lPrefs.AppendPatientName) and (not lPrefs.AppendProtocolName) and (not lPrefs.AppendAcqSeries) and (not lPrefs.AppendDate) and (not lPrefs.AppendFilename) then begin
		 lPrefs.AppendPatientName := true;
		 lPrefs.AppendProtocolName :=  true;
	       	 lPrefs.AppendDate := true;
		 lPrefs.AppendAcqSeries := true;
	 end;
         if direxists(lStr) then begin
               if  (lStr[length(lStr)] = PathDelim) and (length(lStr) > 1) then //and
                  delete(lStr, length(lStr), 1);   //delete trialing separator
            RecursiveFolderSearch(lStr,lOutDir,lPrefs,0);
            lPrefs.NameAppend := '';
	 end else if fileexists(lStr) then begin
                lDir := ExtractFileDir(lStr);
                if lDir = '' then begin //since fileexists, file is in working directory
                  lDir := GetCurrentDir;
                  dcmMsg('0 files in working directory '+lDir);
                  EnsureDirEndsWithPathDelim(lDir);
                  if fileexists(lDir + lStr) then
                     lStr := lDir+ lStr;
                end;
		lExt := UpCaseExt(lStr);
                if IsNiftiExt(lStr) then begin
{$IFDEF GUI}
        MainForm.ConvertDCM2NII(lStr,lPrefs);
        //dcmMsg('Please drag and drop NIfTI images onto dcm2niigui to convert them')
{$ELSE}
  ModifyAnalyze(lStr,lPrefs);
{$ENDIF}
		end else if (lExt = '.REC') or (lExt = '.PAR') then begin
                    LoadFileListPARREC(lStr,lOutDir,lPrefs);
                    if lPrefs.everyfile then
                       exit;
		end else begin
                    {$IFNDEF UNIX}lStartTime := GetTickCount; {$ENDIF}
                    if  lPrefs.everyfile then
                        LoadFileList(lStr,lOutDir,lPrefs)
                    else
                        LoadParamFileList(lStr,lOutDir,lPrefs,I);
                    {$IFNDEF UNIX}dcmMsg('Time elapsed '+inttostr( GetTickCount-lStartTime)+'ms'); {$ENDIF}
                    exit; //only process a single file
                end;
	 end else if  not (lSilent) then begin
		dcmMsg('0 '+paramstr(0)+' ERROR: unable to find '+lStr);
		if not lHelpShown then
			Showhelp(lIniName, lPrefs);
		lHelpShown := true;
	 end;
	until I >= ParamCount;
  end else begin //no parameters passed - show help
	  ShowHelp(lIniName, lPrefs);
          IniFile(False,lIniName, lPrefs);//ensure latest version of preferences file is created...
  end;//param count > 0
end;

end.

