unit untar;

interface
{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}
uses
{$IFDEF FPC}
gzio2,
{$ELSE}
gziod,
{$ENDIF}
define_types,SysUtils,LibTar,  dialogs_msg,
gzio,dialogsx,prefs,sortdicom,classes;

function DeTGZ (lFilename: string; lPrefs: TPrefs): boolean;
function isTGZ (var lStr: string): boolean;

implementation

function isTGZ (var lStr: string): boolean;
var lExt: string;
begin
   lExt := extractfileext(lStr);
   lExt := UpperCase(lExt);
   if (lExt='.TGZ') then
      Result := true
   else
       Result := false;
end;

(*procedure Extract (var lTarFile: string; lOverwrite: boolean); //extract target
VAR
  TA        : TTarArchive;
  DirRec    : TTarDirRec;
  lPos,lLen,lnumFilesTotal,lnumFilesCompleted,lPct: longint;
  lStr,lOutDir,lLocalDir,lFileName,lNewDir,lTarName : String;
begin
            lOutDir := extractfiledir(lTarFile);
            //next Count files for progress bar....
            lnumFilesTotal := 0;
            TA := TTarArchive.Create (lTarFile);
            TRY
               TA.Reset;
               TA.SetFilePos (0);
               TA.FindNext (DirRec);
               repeat
                     inc(lnumFilesTotal);
               until not TA.FindNext (DirRec);

            FINALLY
                   TA.Free;
            END;
            //finished counting files
            //next: extract files...
            lnumFilesCompleted := 0;
            //FProgress := 0;
            TA := TTarArchive.Create (lTarFile);
            TRY
               TA.Reset;
               TA.SetFilePos (0);
               TA.FindNext (DirRec);
            repeat
              inc(lNumFilesCompleted);
              {lPct := round(lNumFilesCOmpleted/lNumfilesTotal*100);
              if lPct > FProgress then begin //only update progress bar 100 times: do not waste time updating screen
                 FProgress := lPct;
                 DoOnProgress;
              end;}
              if DirRec.Name <> '' then begin
               //Screen.Cursor := crHourGlass;
               TRY
                  //filename change '/' to '\'
                  lTarName := '';
                  lLen := length(DirRec.name);
                  for lPos := 1 to lLen do begin
                     if (DirRec.Name[lPos]='/') or (DirRec.Name[lPos]='\') then
                        lTarName := lTarName + pathdelim//'\'
                     else if (DirRec.Name[lPos]=':') then

                     else
                         lTarName := lTarName + DirRec.Name[lPos];
                  end;
                  lFilename := lOutDir+pathdelim+lTarName;
                  lLocalDir := extractfiledir(lFileName);
                  if (DirExists(lLocalDir)) then begin
                    {lProceed := mrYes;
                    if Fileexists(lFileName) then begin
                       if (gmrOverwrite = mrYes) or (gmrOverwrite = mrNo) then begin
                          OverwriteForm.Label1.caption := 'Warning: the file '+lFilename+' already exists.';
                          gmrOverwrite := OverwriteForm.Showmodal;
                       end;
                       lProceed := gmrOverwrite;
                    end;  }
                    if lOverwrite{(lProceed = mrYes) or (lProceed = mrYesToAll)} then begin
                     if (length(lFilename)>2) and ((lFilename[length(lFilename)] = '\') or (lFilename[length(lFilename)] = '/'))  then begin
                        lLen := length(lFilename)-1;
                        lStr := lFilename;
                        lFilename := '';
                        for lPos := 1 to lLen do
                            lFilename := lFilename+lStr[lPos];
                        if not direxists(lFilename) then begin
                           mkdir (lFilename);
                        end;
                     end else
                         TA.ReadFile (lFileName);
                    end; //proceed
                  end else begin
                     lLen := length(lTarName);
                     lPos := 1;
                     if (lLen >= 1) and ((lTarName[1] = '\') or (lTarName[1] = '/')) then inc(lPos);
                     lNewDir := lOutDir+pathdelim;
                     while lPos <= lLen do begin
                          if (lTarName[lPos] = '\') or (lTarName[lPos] = '/') then begin
                             //showmessage('creating directory:'+lNewDir);
                             if not direxists(lNewDir) then
                                mkdir(lNewDir);
                             lNewDir := lNewDir + pathdelim;
                          end else
                           lNewDir := lNewDir + lTarName[lPos];
                          inc(lPos);
                     end;
                     if (lFileName[length(lFileName)] <> '/') and(lFileName[length(lFileName)] <> '\') and  (DirExists(lLocalDir)) and (not Fileexists(lFileName)) then begin
                        TA.ReadFile (lFileName)
                     end;
                  end;
               FINALLY
                      //Screen.Cursor := crDefault;
               END;
              end;
            until not TA.FindNext (DirRec);
            FINALLY
                   TA.Free;
            END;
end;*)

procedure Extract (var lTarFile: string; lOverwrite: boolean); //extract target
VAR
  TA        : TTarArchive;
  DirRec    : TTarDirRec;
  lPos,lLen,lnumFilesTotal,lnumFilesCompleted,lPct: longint;
  lStr,lOutDir,lLocalDir,lFileName,lNewDir,lTarName : String;
begin
            lOutDir := extractfiledir(lTarFile);
            //next Count files for progress bar....
            lnumFilesTotal := 0;
            TA := TTarArchive.Create (lTarFile);
            TRY
               TA.Reset;
               TA.SetFilePos (0);
               TA.FindNext (DirRec);
               repeat
                     inc(lnumFilesTotal);
               until not TA.FindNext (DirRec);

            FINALLY
                   TA.Free;
            END;
            //finished counting files
            //next: extract files...
            lnumFilesCompleted := 0;
            //FProgress := 0;
            TA := TTarArchive.Create (lTarFile);
            TRY
               TA.Reset;
               TA.SetFilePos (0);
               TA.FindNext (DirRec);
            repeat
              inc(lNumFilesCompleted);
              {lPct := round(lNumFilesCOmpleted/lNumfilesTotal*100);
              if lPct > FProgress then begin //only update progress bar 100 times: do not waste time updating screen
                 FProgress := lPct;
                 DoOnProgress;
              end;}
              if DirRec.Name <> '' then begin
               //Screen.Cursor := crHourGlass;
               TRY
                  //filename change '/' to '\'
                  lTarName := '';
                  lLen := length(DirRec.name);
                  for lPos := 1 to lLen do begin
                     if (DirRec.Name[lPos]='/') or (DirRec.Name[lPos]='\') then
                        lTarName := lTarName + pathdelim//'\'
                     else if (DirRec.Name[lPos]=':') then

                     else
                         lTarName := lTarName + DirRec.Name[lPos];
                  end;
                  lFilename := lOutDir+pathdelim+lTarName;
                  lLocalDir := extractfiledir(lFileName);
                  if (DirExists(lLocalDir)) then begin
                    (*lProceed := mrYes;
                    if Fileexists(lFileName) then begin
                       if (gmrOverwrite = mrYes) or (gmrOverwrite = mrNo) then begin
                          OverwriteForm.Label1.caption := 'Warning: the file '+lFilename+' already exists.';
                          gmrOverwrite := OverwriteForm.Showmodal;
                       end;
                       lProceed := gmrOverwrite;
                    end;  *)
                    if lOverwrite{(lProceed = mrYes) or (lProceed = mrYesToAll)} then begin
                     if (length(lFilename)>2) and (lFilename[length(lFilename)] = pathdelim)   then begin
                        lLen := length(lFilename)-1;
                        lStr := lFilename;
                        lFilename := '';
                        for lPos := 1 to lLen do
                            lFilename := lFilename+lStr[lPos];
                        if not direxists(lFilename) then begin
                           mkdir (lFilename);
                        end;
                     end else
                         TA.ReadFile (lFileName);
                    end; //proceed
                  end else begin
                     lLen := length(lTarName);
                     lPos := 1;
                     if (lLen >= 1) and (lTarName[1] = pathdelim) then inc(lPos);
                     lNewDir := lOutDir+pathdelim;
                     while lPos <= lLen do begin
                          if (lTarName[lPos] = pathdelim) then begin
                             //showmessage('creating directory:'+lNewDir);
                             if not direxists(lNewDir) then
                                mkdir(lNewDir);
                             lNewDir := lNewDir + pathdelim;
                          end else
                           lNewDir := lNewDir + lTarName[lPos];
                          inc(lPos);
                     end;
                     if (lFileName[length(lFileName)] <> pathdelim) and  (DirExists(lLocalDir)) and (not Fileexists(lFileName)) then begin
                        TA.ReadFile (lFileName)
                     end;
                  end;
               FINALLY
                      //Screen.Cursor := crDefault;
               END;
              end;
            until not TA.FindNext (DirRec);
            FINALLY
                   TA.Free;
            END;
end;


function FindFile (lDir: String): string;
//lDir should include pathdelim, e.g. c:\folder\
var lSearchRec: TSearchRec;
begin
  result := '';
  if FindFirst(lDir+'*', faAnyFile-faSysFile-faDirectory, lSearchRec) = 0 then
  	result := lDir +lSearchRec.Name;
  FindClose(lSearchRec);
end;

procedure DeleteTreeX (const Path : string; recursive : boolean);
var
  Result    : integer;
  SearchRec : TSearchRec;
begin
  Result := FindFirst(Path + '*', faAnyFile-faDirectory , SearchRec);
  while Result = 0 do
  begin
    if not DeleteFile (Path + SearchRec.name) then
    begin
      FileSetAttr (Path + SearchRec.name, 0); { reset all flags }
      DeleteFile (Path + SearchRec.name);
    end;
    Result := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  if not recursive then
    exit;
  Result := FindFirst(Path + '*.*', faDirectory, SearchRec);
  while Result = 0 do
  begin
    if (SearchRec.name <> '.') and (SearchRec.name <> '..') then
    begin
      FileSetAttr (Path + SearchRec.name, faDirectory);
      DeleteTreeX (Path + SearchRec.name + '\', TRUE);
      RmDir (Path + SearchRec.name);
    end;
    Result := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

Procedure copyfile( Const sourcefilename, targetfilename: String );
Var 
  S, T: TFileStream; 
Begin 
  S := TFileStream.Create( sourcefilename, fmOpenRead ); 
  try 
    T := TFileStream.Create( targetfilename, fmOpenWrite or fmCreate ); 
    try 
      T.CopyFrom(S, S.Size ) ; 
    finally 
      T.Free; 
    end; 
  finally 
    S.Free; 
  end; 
End;

procedure CopyDir ( lSourceDir,lDestDir: string);
var
  Result    : integer;
  SearchRec : TSearchRec;
  lSrc,lDest: string;
begin
  Result := FindFirst(lSourceDir + '*', faAnyFile-faDirectory , SearchRec);
  while Result = 0 do  begin
      lSrc := lSourceDir + SearchRec.name;
      lDest := lDestDir + SearchRec.Name;
      copyfile(lSrc,lDest);
    Result := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function DeTGZ (lFilename: string; lPrefs: TPrefs): boolean;
var
 lPath,lName,lExt,lOutPath,lTempDir,lTarName,lDicomName: string;
begin
     result := false;
     if (not fileexists(lFilename)) or (not isTGZ(lFilename)) then
        exit;
     FilenameParts (lFilename , lPath,lName,lExt);

     lOutPath := lPath+lName;
     if direxists(lOutPath) then begin
         dcmMsg('Unable to extract TGZ file - folder exists '+lOutpath);
         exit;
     end;
     MkDir(lOutPath);
     lTempDir := lOutPath+pathdelim+'temp';
     MkDir(lTempDir);
     lTarName := lTempDir+Pathdelim+lName+'.tar';
     UnGZipFile (lFilename,lTarName); //unzip
     Extract (lTarName,true);
     deletefile(lTarName);
     //now convert files to NIFTI
     lDICOMName := FindFile(lTempDir+Pathdelim);
     if (lDICOMName = '') or (not fileexists(lDICOMname)) then
        exit;
     LoadFileList(lDICOMName,lOutPath,lPrefs);

     DeleteTreeX(lTempDir+Pathdelim, true);
     RmDir(lTempDir);
     if (lPrefs.BackupDir <> '') and (DirExists(lPrefs.BackupDir)) then begin
        lTempDir := lPrefs.BackupDir;
        if lTempDir[length(lTempDir)] <> pathdelim then
           lTempDir := lTempDir + pathdelim;
        dcmMsg('Copying to backup folder named '+lTempDir);
        CopyDir(lOutPath+pathdelim, lTempDir);
        dcmMsg('Backup completed');
     end;
     //Extract (lFilename,true);
     result := true;
end;

end.