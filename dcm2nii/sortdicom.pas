unit sortdicom;
{$H+}
{$Include ..\common\isgui.inc}

interface
uses
  SysUtils,define_types,classes,dicom,dicomtypes,convert,dicomfast,prefs,userdir,dialogs_msg;


function LoadFileList (var lInFilename, lOutDirname: string; var lPrefs: TPrefs):boolean;
function LoadParamFileList (var lInFilename, lOutDirname: string; var lPrefs: TPrefs; lParamNum: integer): boolean;

implementation

uses dialogsx;

function IsRepeat (var lD1,lD2: DicomData) : boolean;
begin
     if (lD1.ImageNum = lD2.ImageNum) and
        (lD1.AcquNum = lD2.AcquNum) and
        (lD1.SeriesNum = lD2.SeriesNum) and
        (lD1.DateTime = lD2.DateTime)  then
          result := true
     else
         result := false;
end;

function IsEqualDT (lI1,lI2: TDateTime; var l1LessThan2: boolean): boolean;
begin
     if lI1 = lI2 then
        result := true
     else
         result := false;
     if lI1 < lI2 then
        l1LessThan2 := true
     else
         l1LessThan2 := false;
end;

function IsEqual (lI1,lI2: integer; var l1LessThan2: boolean): boolean;
begin
     if lI1 = lI2 then
        result := true
     else
         result := false;
     if lI1 < lI2 then
        l1LessThan2 := true
     else
         l1LessThan2 := false;
end;

function D1LessThanD2 (var lD1,lD2: DicomData) : boolean;
begin
     if not IsEqualDT (lD1.DateTime, lD2.DateTime, result) then
        exit;
     //only get here if lD1.DataTime = lD2.DateTime
     if not IsEqual (lD1.SeriesNum, lD2.SeriesNum, result) then
        exit;
     //only get here if lD1.SeriesNum = lD2.SeriesNum
     if not IsEqual (lD1.AcquNum, lD2.AcquNum, result) then
        exit;
     //only get here if lD1.AcquNum = lD2.AcquNum
     if not IsEqual (lD1.ImageNum, lD2.ImageNum, result) then
        exit;
     //only get here if lD1.ImageNum = lD2.ImageNum
end;



procedure ReportError (l,i: integer; var lDICOMra: TDICOMrap);
begin
//Msg('Error: these files have the same index '+ lDICOMra^[lPositionRA^[l]].Filename+' = '+lDICOMra^[lPositionRA^[i]].Filename);
              dcmMsg('Error: these files have the same index '+ DICOMstr(l,lDICOMra)+' = '+DICOMstr(i,lDICOMra));
end;

procedure ShellSortDCM (var Items: integer; var lDICOMra: TDICOMrap; var lRepeatedValues: boolean);
//Shell sort /- see 'Numerical Recipes in C' for similar sorts: less memory intensive than recursive quicksort
label
     555;
const
     tiny = 1.0e-5;
     aln2i = 1.442695022;
var
   inputItems,n,t, nn, m, lognb2, l, k, j, i: longint;
 lPositionRA,lPositionRA2: LongintP;
 lTempDICOMra: TDICOMrap;

begin
     inputItems := Items;
     lRepeatedValues := false;
     if Items < 2 then
        exit;
     Getmem(lPositionRA,Items*sizeof(LongInt));
     for i := 1 to items do
         lPositionRA^[i] := i;
     n := (Items );
     lognb2 := trunc(ln(n) * aln2i + tiny);
     m := Items;
     for nn := 1 to lognb2 do
         begin
              m := m div 2;
              k := Items - m;
              for j := 1 to k do begin
                  i := j;
                  555:
                  l := i + m;

                  if //identical refs
                    {(lDICOMra^[lPositionRA^[l]].ImageNum = lDICOMra^[lPositionRA^[i]].ImageNum) and
                    (lDICOMra^[lPositionRA^[l]].AcquNum = lDICOMra^[lPositionRA^[i]].AcquNum) and
                    (lDICOMra^[lPositionRA^[l]].SeriesNum = lDICOMra^[lPositionRA^[i]].SeriesNum) and
                    (lDICOMra^[lPositionRA^[l]].DateTime = lDICOMra^[lPositionRA^[i]].DateTime)}
IsRepeat(lDICOMra^[lPositionRA^[l]], lDICOMra^[lPositionRA^[i]])
                    then begin
                      lRepeatedValues := true;
                      ReportError(lPositionRA^[l],lPositionRA^[i],lDICOMra);
                      //Msg('Error: these files have the same index '+ lDICOMra^[lPositionRA^[l]].Filename+' = '+lDICOMra^[lPositionRA^[i]].Filename);

                  end else
                  if D1LessThanD2 (lDICOMra^[lPositionRA^[l]],lDICOMra^[lPositionRA^[i]])
                   then begin
                     //swap values for i and l
                     t := lPositionRA^[i];
                     lPositionRA^[i] := lPositionRA^[l];
                     lPositionRA^[l] := t;
                     i := i - m;
                     if (i >= 1) then
                        goto 555;
                  end
              end
         end;
         //next - remove any repeated values

     if lRepeatedValues then begin
        Getmem(lPositionRA2,Items*sizeof(LongInt));
            k := 1;
            lPositionRA2^[1] := lPositionRA^[1];
            for i := 2 to Items do begin
                  if not IsRepeat(lDICOMra^[lPositionRA^[i-1]],lDICOMra^[lPositionRA^[i]]) then begin
                     inc(k);
                     lPositionRA2^[k] := lPositionRA^[i];
                  end;
            end;
            Items := k;
            for i := 1 to Items do
                lPositionRA^[i] := lPositionRA2^[i];
            Freemem(lPositionRA2);
         end;
         //Next - created sorted lists based on pointers...
          //... a  quicker way would be to return the pointers, but this is still pretty fast...
          //... a lower memory solution would be to swap items inside lDICOMra
          Getmem(lTempDICOMra,InputItems*sizeof(DicomData));
          for I := 1 to InputItems do
              lTempDICOMra^[I] := lDICOMra^[I];
          if InputItems <> Items then begin
             Freemem(lDICOMra);
             Getmem(lDICOMra,Items*sizeof({TDICOM}DicomData));
          end;
          for I := 1 to Items do
              lDICOMra^[I] := lTempDICOMra^[lPositionRA^[I]];
          Freemem(lTempDICOMra);
         //finally, cleanup
         Freemem(lPositionRA);
end; //ShellSortDCM

const
kTolerance = 0.0000095; //assume files are from different series if their orientation differs by more than this value
//unfortunately, GE images have a rounding error, so nearby slices often have different values...

function SameIDSeriesAcqXYZ( var ld1,ld2: DicomData{TDICOM};var lPrefs: TPrefs): boolean;
var
   lStack: boolean;
   lI: integer;
begin
     result := false;
     if (ld1.file4D)  then //if previous file is a 4D image, we should convert it separately
        exit;
     if (ld1.DateTime = ld2.DateTime) and(ld1.SeriesNum = ld2.SeriesNum) {and (ld1.acquNum = ld2.acquNum)}
       and(ld1.XYZdim[1] = ld2.XYZdim[1]) and(ld1.XYZdim[2] = ld2.XYZdim[2]) and(ld1.XYZdim[3] = ld2.XYZdim[3]) then
        //result := true
     else
         exit;
     lStack := lPrefs.Stack3DImagesWithSameAcqNum;
     if (ld1.Vers0018_1020 >= lPrefs.SiemensDTIStackIf00181020atleast) then
        lStack := true; //recent Siemens scanners will have different NEx saved as different images and different directions saved as different images
     if   (not lStack) and (ld1.acquNum <> ld2.acquNum) then begin
         dcmMsg('Images not stacked because acquisition number changes. If you want to stack these images set Stack3DImagesWithSameAcqNum=1 in your ini file.');
         exit;
     end;
     (*if (ld1.PatientIDInt = ld2.PatientIDInt) and(ld1.SeriesNum = ld2.SeriesNum) and (ld1.acquNum = ld2.acquNum)
       and(ld1.XYZdim[1] = ld2.XYZdim[1]) and(ld1.XYZdim[2] = ld2.XYZdim[2]) and(ld1.XYZdim[3] = ld2.XYZdim[3]) then
        //result := true
     else
         exit;*)
     for lI := 1 to 6 do begin
         //if (ld1.orient[lI] <> ld2.orient[lI]) then
         if abs (ld1.orient[lI] - ld2.orient[lI]) > kTolerance then
            exit;
     end;
     (*if (ld1.IntenScale <> ld2.IntenScale) or (ld1.IntenIntercept <> ld2.IntenIntercept)  then begin//if previous file is a 4D image, we should convert it separately
        msg('Warning: unable to stack images because intensity scaling varies. Names: '+ld1.Filename+' '+ld2.filename+' Slopes: '+floattostr(ld1.IntenScale)+' '+floattostr(ld2.IntenScale)+' Intercepts: '+floattostr(ld1.IntenIntercept)+' '+floattostr(ld2.IntenIntercept));
        exit;
     end;  *)
     result := true;
end;

function ProcessSingleFolderDCM (var lInFilename: string; var lStringList : TStringList): boolean;
//assumes lStringList is already created and will be freed later...
var
  lSearchRec: TSearchRec;
  lPrev,lFilename,lFilepath,lMaskExt,lExt: string;
begin
     result := false;
     lFilePath := ExtractFileDirWithPathDelim(lInFilename);
     lExt := string(StrUpper(PChar(ExtractFileExt(lInFilename)))); //.head
     if (lExt = '.PAR') or (lExt = '.REC') {or (lExt = '.HDR') or (lExt = '.IMG')or (lExt = '.HEAD') or (lExt = '.BRIK')} then
     {$IFDEF Unix}
        lMaskExt := '*'+ExtractFileExt(lInFilename) //Linux is case sensitive, these extensions are used by paired files: only read one of pair
     else
         lMaskExt := '*';
     {$ELSE}
        lMaskExt := '*'+lExt //these extensions are used by paired files: only read one of pair
     else
         lMaskExt := '*.*';
     {$ENDIF}
    //Msg('yyy'+lFilePath+'::'+lMaskExt);
    lPrev := '.';
    Filemode := 0; //readonly
     if FindFirst(lFilePath{+PathDelim}+lMaskExt, faAnyFile-faSysFile-faDirectory, lSearchRec) = 0 then begin
        repeat
              if (length(lSearchRec.Name) < 1) then
                 //do nothing
                 lFilename := ''
              {$IFDEF Unix}
//next two lines would not recognize filename that starts with dor, e.g. \home\cr\.filename.ima
//              else if  (lSearchRec.Name[1] = '.')  then
//                 lFilename := ''
              else if  (lSearchRec.Name = '..')  then
                 lFilename := ''
              else if  (lSearchRec.Name = '.')  then
                 lFilename := ''
              {$ENDIF}
              else begin
                   lFilename := lFilePath+lSearchRec.Name;

                   if (lFilename = '') or (length (lFilename) > 255) then begin
                      dcmMsg('Unable to convert images where the file path and name exceed 255 characters.');
                      dcmMsg('Solution: put images in a folder with a shorter path.');
                      dcmMsg(lFilename);
                   end else if (lFilename <> lPrev) then begin
                       lStringList.Add(lFileName);
                        //if lFilename = lPrev then
                        //   msg(lPrev);
                        lPrev := lFilename;
                        //  msg(lFilePath+lMaskExt+' ->'+lSearchRec.Name+'z'+inttostr(lStringList.count));

                   end;
              end;
        until (FindNext(lSearchRec) <> 0);
     end; //some files found
     // msg('xxxx'+inttostr( lStringList.Count));

     SysUtils.FindClose(lSearchRec);
     Filemode := 2; //readonly
     result := true;
end; //ProcessSingleFolder

procedure ProcessRecursiveFolder (var lFolderNameIn: string; var lStringList : TStringList; lDepth: integer; var lPrefs: TPrefs);
var
 len: integer;
 lFolderName,lNewDir,lNewName,lFilename,lExt: String;
 lSearchRec: TSearchRec;
begin
 lFolderName := lFolderNameIn;
 if not DirExists (lFolderName) then begin
     lFolderName := ExtractFileDir(lFolderName);
 end;
 if (length(lFolderName) > 1) and (lFolderName[length(lFolderName)] <> PathDelim) then
    lNewDir := lFolderName+PathDelim;
 if DirExists (lNewDir) then begin
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
                  ProcessRecursiveFolder (lNewName, lStringList, lDepth+1, lPrefs);
               end;
               //exit;//4/4/2008
            end else
                lFilename := lNewname;
      end;
    until (FindNext(lSearchRec) <> 0);
   end else begin //if directory exists... else we were passed a filename
       lFilename := lFolderName;
   end;
   if lFilename = '' then
      exit;
   //Msg('xxxx '+ lFilename);
    if lFilename <> '' then begin
       ProcessSingleFolderDCM (lFilename, lStringList);
     end;
 end;
 FindClose(lSearchRec);

end;


function LoadFileListInner (var lOutDirname: string; var lPrefs: TPrefs; var lStringList : TStringList): boolean;
var
  lPrevDICOM,lDicomData: DicomData;
  lDICOMra, lDICOMra4D: TDICOMrap;
  lnumEchos,lRepeatLocations,lStartImg,lValidItems, lItems,lInc: integer;
  lError,lRepeatedValues,lHdrOK,lImgOK, l4dDTI: boolean;
  lDTIra: TDTIRA;
  lFilename,lDynStr: string;
begin
    result := false;
     lItems := lStringList.Count;
     if lItems < 1 then begin
        lStringList.Free;
        exit;
     end;
     Filemode := 2;
    dcmMsg('Validating '+inttostr(lItems)+' potential DICOM images.');
     l4dDTI := false;
     //START ANON
     if lPrefs.AnonymizeSourceDICOM then begin
        for lInc := 1 to lItems do begin
         lFilename := lStringList.Strings[lInc-1];
         fast_read_dicom_data(lDICOMdata, 128, lFileName); //x3 faster!
        end;
        lStringList.Free;
        result := true;
        Exit;
     end; //if anonymizeSourceDICOM
     //END ANON
     getmem(lDICOMra,lItems*sizeof(DicomData));
     lValidItems := 0;
     for lInc := 1 to lItems do begin
         lFilename := lStringList.Strings[lInc-1];

         read_dicom_data(true,false{not verbose},true,true,true,true,false, lDICOMdata, lDTIra, lHdrOK, lImgOK, lDynStr,lFileName,lPrefs );
         if (lHdrOK) and (lImgOK) then begin //valid file
            if lDICOMdata.nDTIdir > 1  then begin
               //showmsg('oh dear'+inttostr(lDICOMdata.nDTIdir));
                l4dDTI := true;
                dcmMsg('Converting 4D ');
                getmem(lDICOMra4D,sizeof(DicomData));
                lDICOMra4D^[1] := lDicomData;
                result := Dicom2NII(lDICOMra4D,lDTIra, 1,1,lOutDirname,lPrefs,0);
                freemem(lDICOMra4D);
            end else begin
                inc(lValidItems);
                lDICOMra^[lValidItems] := lDicomData;
            end
         end; //if image is OK
     end; //for each item
     lStringList.Free;
     if lValidItems = 0 then begin
        if not l4dDTI then  //do not generate warning if we processed 4D data...
           dcmMsg('Unable to find any DICOM files in the path '+lFileName);
        freemem(lDICOMra);
        exit;
     end;
     dcmMsg('Found '+inttostr(lValidItems)+' DICOM images.');

     ShellSortDCM (lValidItems,lDICOMra,lRepeatedValues);

     if lRepeatedValues then begin //separate into series
        dcmMsg('Warning: repeated image indexes in the path '+lFileName);
        //freemem(lDICOMra);
        //exit;
     end;
     if lPrefs.DebugMode then begin
        for lInc := 1 to lValidItems do
            dcmMsg( DICOMstr(lInc,lDICOMra));
         exit;
     end;
     lStartImg := 1;
     lRepeatLocations := 0;
     lnumEchos := 1;
     lPrevDICOM := lDICOMra^[1];
     for lInc := 1 to lValidItems do begin
         //msg(lDICOMra^[lInc].Filename+','+floattostr(lDICOMra^[lInc].PatientPosX)+','+floattostr(lDICOMra^[lInc].PatientPosY)+','+floattostr(lDICOMra^[lInc].PatientPosZ)  );

         if  (lInc > 1) and (not SameIDSeriesAcqXYZ (lPrevDICOM ,lDICOMra^[lInc],lPrefs))   then begin
                    //SameIDSeriesAcqXYZ2 (lPrevDICOM ,lDICOMra^[lInc]);
                    if (lRepeatLocations < 2) and (lnumEchos > 1) then
                       lRepeatLocations := lnumEchos;
                    dcmMsg('Converting '+inttostr(lInc-1)+'/'+inttostr(lValidItems)+'  volumes: '+inttostr(lRepeatLocations));
                    result := Dicom2NII(lDICOMra,lDTIra,lStartImg,lInc-1,lOutDirname,lPrefs,lRepeatLocations);
                    if not result then
                       lError := true;
                    lPrevDICOM := lDICOMra^[lInc];
                    lStartImg := lInc;
                    lRepeatLocations := 1;
         end else if //(lDICOMra^[lInc].location = lPrevDICOM.Location) and
              (lDICOMra^[lInc].PatientPosX = lPrevDICOM.PatientPosX) and
             (lDICOMra^[lInc].PatientPosY = lPrevDICOM.PatientPosY) and
             (lDICOMra^[lInc].PatientPosZ = lPrevDICOM.PatientPosZ) then begin
             //fx(lDICOMra^[lInc].PatientPosZ , lPrevDICOM.PatientPosZ );
             //fx(666,lRepeatLocations,lDICOMra^[lInc].location, lPrevDICOM.Location);
             inc(lRepeatLocations);
         end else if (lInc > 1) and ( (lDICOMra^[lInc].AcquNum) > (lDICOMra^[lInc-1].AcquNum+999)) then //we increment acquisition number by 1000 to denote new echo
             inc(lnumEchos);
     end; //for each valid
     if (lRepeatLocations < 2) and (lnumEchos > 1) then
        lRepeatLocations := lnumEchos;
     //fx(lPrevDICOM.PatientPosX, lPrevDICOM.PatientPosY, lPrevDICOM.PatientPosZ );
     //Msg( inttostr(lValidItems-lStartImg+1)+'  '+ inttostr(lRepeatLocations));
     //fx(lRepeatLocations);
     if (((lValidItems-lStartImg+1) mod lRepeatLocations) <> 0) then begin
        dcmMsg('*Warning: Number of images in series ('+inttostr(lValidItems-lStartImg+1)+') not divisible by number of volumes ('+inttostr(lRepeatLocations)+')');
        dcmMsg('*  Perhaps the selected folder only has some of the images');
         PartialAcquisitionError;

     end;
     if (lPrevDICOM.SlicesPer3DVol > 0) and (not lPrevDICOM.file4D) and ((lValidItems div lRepeatLocations) <> lPrevDICOM.SlicesPer3DVol) then begin
        dcmMsg('Warning: Number of slices per volume ('+inttostr((lValidItems div lRepeatLocations))+') appears different than reported in DICOM header ('+inttostr(lPrevDICOM.SlicesPer3DVol)+')');
        dcmMsg('  Perhaps the selected folder only has some of the images');
     end;

     dcmMsg('Converting '+inttostr(lValidItems)+'/'+inttostr(lValidItems)+'  volumes: '+inttostr(lRepeatLocations));
     Dicom2NII(lDICOMra,lDTIra, lStartImg,lValidItems,lOutDirname,lPrefs,lRepeatLocations);
     if lError then
        result := false //at least one error
     else
       result := true;
     freemem(lDICOMra);
end;

function ReportDICOMHeader (var lInFilename: string; var lPrefs: TPRefs): boolean;
var
  lDicomData: DicomData;
  lDynStr: string;
  lHdrOK,lImgOK: boolean;
  lDTIra: TDTIRA;
begin
        read_dicom_data(false,true,false,false,false,false,false, lDICOMdata, lDTIra, lHdrOK, lImgOK, lDynStr,lInFileName,lPrefs );
        result := lHdrOK;
end;



function LoadFileList (var lInFilename, lOutDirname: string; var lPrefs: TPrefs): boolean;
var
  lStringList : TStringList;
begin
     result := false;
     if lPrefs.Verbose then begin
        //msg(lInFilename);
        result :=  ReportDICOMHeader (lInFilename, lPrefs);
        exit;
     end;
     lStringList := TStringList.Create;
     if (lPrefs.OutDirMode <> kOutDirModeInput) and (DirExists(lPrefs.OutDir)) then begin
        //For kOutDirModePrompt one should set OutDir before getting here
        //This is required so recursive searches do not repetitively prompt the user...
        lOutDirName := lPrefs.OutDir;
     end; //1/2010
     if lOutDirName = '' then begin
        if DirExists (lInFilename) then
           lOutDirName := lInFilename
        else
            lOutDirName := extractfiledir(lInFilename);
     end;
     if not(DirExists(lOutDirName)) then
        lOutDirName := UserDataFolder;
     if lPrefs.CollapseFolders then begin
         dcmMsg('Data will be exported to '+lOutDirname);
         ProcessRecursiveFolder (lInFilename, lStringList, 0, lPrefs);
         result := true;
     end else
         result :=  ProcessSingleFolderDCM (lInFilename, lStringList);
     if (not result) or (lStringList.Count  < 1) then begin
        dcmMsg('+Unable to find any images in the path '+lInFilename);
        lStringList.Free;
     end else
        result :=  LoadFileListInner (lOutDirName, lPrefs,lStringList)
end;


function LoadParamFileList (var lInFilename, lOutDirname: string; var lPrefs: TPrefs; lParamNum: integer): boolean;
var
  lStringList : TStringList;
  lI : integer;
begin
     result := false;
     if lPrefs.Verbose then begin
        result :=  ReportDICOMHeader (lInFilename, lPrefs);
        exit;
     end;
     lStringList := TStringList.Create;
     lStringList.Add(lInFilename);
     if ((lParamNum) < ParamCount) then
        for lI := (lParamNum+1) to ParamCount do
            lStringList.Add(Paramstr(lI));
     dcmMsg('Only converting files explicitly specified');
     result :=  LoadFileListInner (lOutDirName, lPrefs,lStringList)
end;

end.
