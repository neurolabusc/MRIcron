unit nii_4dto3d;
{$H+}


interface

uses
{$IFDEF FPC}gzio2,{$ENDIF}
  SysUtils,define_types,dicomtypes,niftiutil,prefs,nii_orient,nii_crop, nifti_types;

//function Convert4Dto3D(var lHdrName: string; var lHdr: TNIFTIhdr; lByteSwap,lSPM2in,lSingleNIIFile,lGZ :boolean ): boolean;
//function Clip4D(var lHdrName: string; var lHdr: TNIFTIhdr; lByteSwap,lSPM2in,lSingleNIIFile,lGZ,lOverwrite: boolean; lStartIn,lEndIn: integer ): string;
function ModifyAnalyze(lFilename: string; lPrefs: TPrefs): boolean;
function Clip4D(var lHdrName: string; var lHdr: TNIFTIhdr;lOverwrite: boolean; lPrefs: TPrefs; lStartIn,lEndIn: integer): string;
//function Reorder4D(var lHdrName: string; var lHdr: TNIFTIhdr; lByteSwap,lSPM2in,lSingleNIIFile,lGZ,lOverwrite: boolean): boolean;
function Reorder4D(var lHdrName: string; var lHdr: TNIFTIhdr; lOverwrite: boolean; lPrefs: TPrefs): boolean;

implementation
uses dialogsx,dialogs_msg;




function ModifyAnalyze(lFilename: string; lPrefs: TPrefs): boolean;
var
   lExt,lOutname: string;
   lHdr: TNIFTIhdr;
   lFormat,lStartIn,lEndIn, lMinStartIn: integer;
   lReorder: boolean;
   lPref: TPrefs;
   lO: TNIIOpts;
begin
    lPref := lPrefs;
     result := false;
            lStartIn := 0;
            lEndIn := 0;
            lReorder := false;
     lExt := UpCaseExt(lFilename);
     if not NIFTIhdr_LoadHdr (lFilename, lHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
     if lPrefs.AutoCrop then begin
        dcmMsg('Autocrop NIfTI/Analyze image '+lFileName);
        lOutname := Reorient(lFilename,lHdr, lPrefs,false,false);
        if lOutname <> '' then
           CropNIfTI(lOutname,lPrefs);
         exit;
     end;
     dcmMsg('Adjusting NIfTI/Analyze image '+lFileName);
     if (lHdr.dim[4] > 1) then begin //if 4D input
      if (lPrefs.BeginClip > 0) and (lPrefs.BeginClip < lHdr.dim[4]) then begin
        lStartIn := lPrefs.BeginClip;
        dcmMsg('Warning: removing first '+inttostr(lStartIn) + ' volumes (preference: BeginClip)');
      end;
      if (lStartIn <> lPrefs.BeginClip) then
        dcmMsg('Warning preference BeginClip is being ignored (not enough volumes)');
      if (lPrefs.LastClip > 0)  and ( (lPrefs.LastClip+ lPrefs.BeginClip) < lHdr.dim[4]) then begin
        lEndIn := lPrefs.LastClip;
        dcmMsg('Warning: removing final '+inttostr(lEndIn) + ' volumes (preference: LastClip)');
      end;
      if (lEndIn <> lPrefs.LastClip) then
        dcmMsg('Warning preference LastClip is being ignored (not enough volumes)');
     end;//if 4D input
     //next - determine output format
     if not lPref.ManualNiFtiConv then begin
        lReorder := false;
     end else begin //manually specify conversion parameters
        lFormat := GetInt('Output: 0=spm2,1=spm5,2=spm8,3=hdr4D,4=fsl,5=fsl.gz ', 0,DefaultOutputFormat (lPrefs),5);
        SetOutputFormat(lFormat,lPref); //: 0=SPM2,1=SPM5,2=spm8,3=4D hdr/img,4=fsl(default),5=fsl.gz


(*        if (lFormat <= 0) then
           lPref.SPM2 := true
        else
            lPref.SPM2 := false;
        if (lFormat <= 1) then //0,1 = hdr/img pairs
           lPref.singleNIIfile := false
        else //>1 = .nii
             lPref.singleNIIfile := true;
        if (lFormat <= 2) then //0,1,2 = 3D output
           lPref.fourD := false
        else //>2 = 4D
             lPref.fourD  := true;
        if (lFormat >= 4) then
           lPref.GZip := true
        else
            lPref.GZip := false; *)
        //next - 4D images: clip ends or flip order
        if lHdr.dim[4] > 1 then begin //4D file
           if (lHdr.dim[4] > 1) and (lHdr.dim[3] > 1) then begin
              dcmMsg('  Enter a value of -1 to flip 3rd and 4th dimensions.');
              lMinStartIn := -1;
           end else
               lMinStartIn := 0;
           lStartIn := GetInt('Enter volumes to remove from start ', lMinStartIn,lStartIn,lHdr.dim[4]);
           if lStartIn >= 0 then
              lEndIn := GetInt('Enter volumes to remove from end ' ,0,lEndIn,lHdr.dim[4]);
           if ((lStartIn < 0) or (lEndIn < 0)) and (lHdr.dim[4] > 1) and (lHdr.dim[3] > 1) then
              lReorder := true
           else
               lReorder := false;
           if lHdr.dim[4] <= (lStartIn+lEndIn) then begin
              dcmMsg('Clip Analyze aborted: unable to remove this many volumes.');
              exit;
           end;
        end;(* else begin //not 4D file
            l4Dto3D := false;
            lStartIn := 0;
            lEndIn := 0;
            lReorder := false;
        end;//if 4D else    *)
     end; //manual specification of conversion
     //

(*     if lExt = '.NII.GZ' then begin
        //lTempName := lFilename;//ChangeFilePrefixExt (lFileName,'x');
        ExtractFileParts (lFileName, lNameWOExt,lExt);
        lTempName := lNameWOExt+'.nii';
        Gunzip(lFileName,lTempName);
        //dcmMsg('Unzip '+lFilename+'->'+lTempName);
        lFilename := lTempName;
     end else //not gzip
         lTempName := '';   *)
     //Next create reordered or trimmed image in the correct format

     if lReorder then begin
       if not Reorder4D(lFileName, lHdr, false,lPref) then exit;
       //if not Reorder4D(lFileName, lHdr, lByteSwap,lSPM2,lSingleFile,lGZ, false) then exit;
     end else if (lStartIn=0) and (lEndIn= 0) then begin
         if  not ChangeNIfTISubformat(lFileName, lHdr,lPref) then begin
            dcmMsg('Error changing format!');
            exit;
         end;
     end else begin
       if  Clip4D(lFileName, lHdr, false,lPref,lStartIn,lEndIn)='' then exit;
     end;
     result := true;

end;

function Clip4D(var lHdrName: string; var lHdr: TNIFTIhdr;lOverwrite: boolean; lPrefs: TPrefs; lStartIn,lEndIn: integer): string;
var
   lImgBuffer: byteP;
   lImgOffset: integer;
   lOutImgName: string;
   lO: TNIIOpts;
begin
   result := '';
   if not NIFTIhdr_LoadImg (lHdrName, lHdr, lImgBuffer, lImgOffset,lO) then  exit;
   dcmMsg('4D Clipping '+lHdrName);
   lOutImgName := ChangeFilePrefix (lHdrName,'f');
   result := SaveNIfTICoreCrop (lOutImgName, lImgBuffer, lImgOffset+1,lStartIn,lEndIn, lHdr, lPrefs);
   Freemem(lImgBuffer);

end;

function Reorder4D(var lHdrName: string; var lHdr: TNIFTIhdr; lOverwrite: boolean; lPrefs: TPrefs): boolean;
var
   lInBuffer,lOutBuffer: byteP;
   lImgOffset,lSliceBytes,lIn3DBytes,l4DBytes,lVol,lInPos,lOutPos,lSlice: integer;
   lOutImgName: string;
   lOutHdr : TNIFTIhdr;
   lO: TNIIOpts;
begin
   result := false;
   if not NIFTIhdr_LoadImg (lHdrName, lHdr, lInBuffer, lImgOffset,lO) then  exit;
   if (lHdr.dim[4] < 2) or (lHdr.dim[3] < 2) then
      exit;
   if lOverwrite then
       lOutImgName := lHdrName
   else
       lOutImgName := ChangeFilePrefix (lHdrName,'x');
   lOutHdr := lHdr;
   lOutHdr.dim[3] := lHdr.dim[4];
   lOutHdr.dim[4] := lHdr.dim[3];
   lSliceBytes := lHdr.dim[1]*lHdr.dim[2]*(lHdr.bitpix div 8);
   lIn3DBytes := lSliceBytes*lHdr.dim[3];
   l4DBytes := lIn3DBytes*lHdr.dim[4];
   dcmMsg('Changing order of dimensions 3 and 4 of '+lHdrName);
   GetMem(lOutBuffer,l4DBytes+kNIIImgOffset);
   lOutPos := kNIIImgOffset + 1;
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lInPos := ((lVol-1)*lSliceBytes) + lImgOffset+1;
       for lSlice := 1 to lOutHdr.dim[3] do begin
           Move(lInBuffer^[lInPos],lOutBuffer^[lOutPos],lSliceBytes);
           lInPos := lInPos + lIn3DBytes;
           lOutPos := lOutPos + lSliceBytes;
       end;//for lslice
   end; //for lvol
   dcmMsg(lOutImgName);
   if  SaveNIfTICore (lOutImgName, lOutBuffer, kNIIImgOffset+1, lOutHdr, lPrefs) = '' then begin
      dcmMsg('Reorder Error');
      Freemem(lInBuffer);
      Freemem(lOutBuffer);
      exit;
   end;
   result := true;
end;

(*function Reorder4D(var lHdrName: string; var lHdr: TNIFTIhdr; lByteSwap,lSPM2in,lSingleNIIFile,lGZ,lOverwrite: boolean): boolean;
var
   lOutHdr: TNIFTIhdr;
   lInName,lImgName: string;
   lPos,lSlice,lVol,lInVolBytes,lSliceBytes: integer;
   lBuffer: bytep;
   lGZi,lSPM2: boolean;
   lOutF,lInF: File;
begin
   result := false;
   lGZi := lGZ;
   lSPM2 := lSPM2in;
   if lSingleNIIFIle then
      lSPM2 := false;
   if (lHdr.dim[4] < 2) or (lHdr.dim[3] < 2) then
      exit;
   lOutHdr := lHdr;
   lOutHdr.dim[4] := lHdr.dim[3];
   lOutHdr.dim[3] := lHdr.dim[4];
   lSliceBytes := lHdr.dim[1]*lHdr.dim[2]*(lHdr.bitpix div 8);
   lInVolBytes := lSliceBytes*lHdr.dim[3];
   GetMem(lBuffer,lSliceBytes);
   if UpCaseExt(lHdrName) ='.HDR' then begin
       if lOverwrite then
          deletefile(lHdrName);
       lInName :=  changefileext(lHdrName,'.img')
   end else begin
       lOutHdr.vox_offset := 352;
       lInName := lHdrName;
   end;
   if not fileexists(lInName) then begin
       dcmMsg('4Dclip Error: Unable to find '+lInName);
       exit;
   end;
   if FSize (lInName) < ( (lInVolBytes*lHdr.dim[4])+round(lHdr.vox_offset)) then begin
       dcmMsg('4Dclip Error: File smaller than expected (can not convert compressed) '+lInName);
       exit;
   end;
   dcmMsg('Reordering image');
   if not lSingleNiiFile then begin
       lHdrName :=  changefileext(lHdrName,'.hdr');
       lImgName :=  changefileext(lHdrName,'.img');
       lGZi := false;
   end else begin
       lHdrName :=  changefileext(lHdrName,'.nii');
       lImgName :=  changefileext(lHdrName,'.nii');
   end;
   if lOverwrite then begin
      renamefile(lInName,changefileext(lInName,'.tmp'));
      lInName := changefileext(lInName,'.tmp');
   end else begin
       lHdrName := ChangeFilePrefixExt (lHdrName,'x');
       lImgName := ChangeFilePrefixExt (lImgName,'x');
       dcmMsg('saving as '+lHdrName);
   end;
   AssignFile(lInF, lInName);
   Reset(lInF,1);
   Seek(lInF,round(lHdr.vox_offset));
   SaveHdr (lHdrName,lOutHdr,lByteSwap{ false},lSPM2);
   AssignFile(lOutF, lImgName);
   if lSingleNIIFile then begin
      Reset(lOutF,1);
      Seek(lOutF,352);
   end else
       Rewrite(lOutF,1);
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lPos := ((lVol-1)*lSliceBytes) + round(lHdr.vox_offset);
       for lSlice := 1 to lOutHdr.dim[3] do begin
           Filemode := 0;  //ReadONly
           seek(lInF,lPos);
           BlockRead(lInF, lBuffer^, lSliceBytes);
           Filemode := 2;
           BlockWrite(lOutF, lBuffer^, lSliceBytes);
           lPos := lPos + lInVolBytes;
       end;//for lslice
   end; //for lvol
   CloseFile(lInF);
   CloseFile(lOutF);
   Freemem(lBuffer);
   if lOverwrite then
      DeleteFile(lInName);
   if lGZi then
      GZipFile(lImgName,lImgName+'.gz',true);
   result := true;
end;      *)

(*function Clip4D(var lHdrName: string; var lHdr: TNIFTIhdr;  lByteSwap,lSPM2in, lSingleNIIFile,lGZ,lOverwrite: boolean; lStartIn,lEndIn: integer ): string;
var
   lOutHdr: TNIFTIhdr;
   lInName,lImgName: string;
   lVol,lVolBytes,lStart,lEnd: integer;
    lBuffer: bytep;
    lGZi,lSPM2 : boolean;
    lOutF,lInF: File;
begin
   result := '';
   lGZi := lGZ;
   lSPM2 := lSPM2in;
   if lSingleNIIFIle then
      lSPM2 := false;
   lStart := lStartIn;
   if lStart < 0 then
      lStart := 0;
   lEnd := lEndIn;
   if lEnd < 0 then
      lEnd := 0;
   lOutHdr := lHdr;
   lOutHdr.dim[4] := lOutHdr.dim[4]-lStart-lEnd;
   if lOutHdr.dim[4] < 1 then
      exit;
   lVolBytes := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3]*(lOutHdr.bitpix div 8);
   GetMem(lBuffer,lVolBytes);
   if UpCaseExt(lHdrName) ='.HDR' then begin
       if lOverwrite then
          deletefile(lHdrName);
       lInName :=  changefileext(lHdrName,'.img')
   end else begin
       lOutHdr.vox_offset := 352;
       lInName := lHdrName;
   end;
   if not fileexists(lInName) then begin
       dcmMsg('4Dclip Error: Unable to find '+lInName);
       exit;
   end;
   if FSize (lInName) < ( (lVolBytes*lHdr.dim[4])+round(lHdr.vox_offset)) then begin
       dcmMsg('4Dclip Error: File smaller than expected (can not convert compressed) '+lInName);
       exit;
   end;
   if (lStart > 0) or (lEnd > 0) then
      dcmMsg('4D clip - removing first '+inttostr(lStart)+' and last '+inttostr(lEnd) +' volumes')
   else
       dcmMsg('Formatting image');
   if not lSingleNiiFile then begin
       lGZi := false;
       lHdrName :=  changefileext(lHdrName,'.hdr');
       lImgName :=  changefileext(lHdrName,'.img');
   end else begin
       lHdrName :=  changefileext(lHdrName,'.nii');
       lImgName :=  changefileext(lHdrName,'.nii');
   end;
   if lOverwrite then begin
      renamefile(lInName,changefileext(lInName,'.tmp'));
      lInName := changefileext(lInName,'.tmp');
   end else begin
       lHdrName := ChangeFilePrefixExt (lHdrName,'x');
       lImgName := ChangeFilePrefixExt (lImgName,'x');
       dcmMsg('Saving clipped as '+lHdrName);
   end;
   AssignFile(lInF, lInName);
   Reset(lInF,1);
   Seek(lInF,round(lHdr.vox_offset));
   SaveHdr (lHdrName,lOutHdr, lByteSwap{false},lSPM2);
   AssignFile(lOutF, lImgName);
   if lSingleNIIFile then begin
      Reset(lOutF,1);
      Seek(lOutF,352);
   end else
       Rewrite(lOutF,1);
   for lVol := 1 to (lHdr.dim[4]-lEnd) do begin
       //1st - save header
       Filemode := 0;  //ReadONly
       BlockRead(lInF, lBuffer^, lVolBytes);
       if (lVol > lStart) then begin
          Filemode := 2;
          BlockWrite(lOutF, lBuffer^, lVolBytes);
       end;
   end;
   CloseFile(lInF);
   CloseFile(lOutF);
   Freemem(lBuffer);
   if lOverwrite then
      DeleteFile(lInName);
   if lGZi then begin
      lHdrName := lImgName+'.gz';
      GZipFile(lImgName,lHdrName,true);
   end;
   result := lHdrName;
end;

function Convert4Dto3D(var lHdrName: string; var lHdr: TNIFTIhdr; lByteSwap, lSPM2in,lSingleNIIFile,lGZ: boolean ): boolean;
var
   lOutHdr: TNIFTIhdr;
   lOutName,lImgName: string;
   lVol,lVolBytes: integer;
    lBuffer: bytep;
    lSPM2,lGZi: boolean;
     lOutF,lInF: File;
begin
   result := false;
   lSPM2 := lSPM2in;
   if lSingleNIIFIle then
      lSPM2 := false;
   lGZi := lGZ;
   if lHdr.dim[4] < 2 then
      exit;
   lOutHdr := lHdr;
   lOutHdr.dim[0] := 3;//3D
   lOutHdr.dim[4] := 1;
   lVolBytes := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3]*(lOutHdr.bitpix div 8);
   GetMem(lBuffer,lVolBytes);
   //lSingleNIIFile := true;
   if UpCaseExt(lHdrName) ='.HDR' then begin
       //lSingleNIIFile := false;
       lImgName :=  changefileext(lHdrName,'.img')
   end else
       lImgName := lHdrName;
   if not fileexists(lImgName) then begin
       dcmMsg('4D->3D Error: Unable to find '+lImgName);
       exit;
   end;
   if FSize (lImgName) < ( (lVolBytes*lHdr.dim[4])+round(lHdr.vox_offset)) then begin
       dcmMsg('4D->3D Error: File smaller than expected (can not convert compressed) '+lImgName);
       exit;
   end;
   //dcmMsg(inttostr(round(lHdr.vox_offset)));
   AssignFile(lInF, lImgName);
   Reset(lInF,1);
   Seek(lInF,round(lHdr.vox_offset));
   if not lSingleNiiFile then begin
       lGZi := false;
       lHdrName :=  changefileext(lHdrName,'.hdr');
       lImgName :=  changefileext(lHdrName,'.img');
   end else begin
       lHdrName :=  changefileext(lHdrName,'.nii');
       lImgName :=  changefileext(lHdrName,'.nii');
   end;
   for lVol := 1 to lHdr.dim[4] do begin
       //1st - save header
       lOutName := AddFileNum(lVol,lHdr.dim[4],lHdrName);
       SaveHdr (lOutName,lOutHdr,lByteSwap {false},lSPM2);
       Filemode := 0;  //ReadONly
       BlockRead(lInF, lBuffer^, lVolBytes);

       lOutName := AddFileNum(lVol,lHdr.dim[4],lImgName);
       Filemode := 2;
       AssignFile(lOutF, lOutName);
       if (lSingleNIIFile) and (not lSPM2) then begin
		Reset(lOutF,1);
		Seek(lOutF,352);
       end else
		Rewrite(lOutF,1);
       BlockWrite(lOutF, lBuffer^, lVolBytes);
       CloseFile(lOutF);
       if lGZi then begin
          GZipFile(lOutName,lOutName+'.gz',true);
          //DeleteFile(lOutName);
       end;
   end;
   CloseFile(lInF);
   Freemem(lBuffer);
   //if lDeleteOrig then begin
      DeleteFile(lHdrName);
      if not lSingleNIIFile then
         DeleteFile(lImgName);
   //end;
end;       *)


end.