unit nii_3dto4d;
{$H+}


interface

uses
{$IFDEF FPC}gzio2,{$ENDIF}
  SysUtils,define_types,dicomtypes,niftiutil,prefs,classes,dialogs_msg, nifti_types;

function Stack3Dto4D(var lStr: TStringList; lOverwrite: boolean; lPrefs: TPrefs): boolean;
function ExtractNIFTIHdrs(var lStr: TStringList): boolean;

implementation
uses dialogsx;

function LeadingZeroFilename (lInX: string): string;
var
   lIn: string;
   lC,lnPad,lPos,lnDec,lExtPos,lLen: integer;
begin
     {$IFDEF Unix}
     lIn := lInX;
     {$ELSE}
     lIn := Lowercase(lInX);
     {$ENDIF}
     lnPad := 8;
     lLen := length(lIn);
     result := lIn;
     if lLen < 1 then exit;
     lExtPos := 1;
     while (lExtPos <= lLen) and (lIn[lExtPos] <> '.') do
           inc(lExtPos);
     if lExtPos <= 1 then
        exit;
     //lnDec := 0;
     lPos := lExtPos -1;
     while (lPos > 0) and ( lIn[lPos] in ['0'..'9']) do
           dec(lPos);
     lnDec := (lExtPos-lPos)-1;
     if (lnDec = 0) or (lnDec >= lnPad) then
        exit;
     result := '';
     if lPos > 0 then
        for lC := 1 to lPos do
            result := result + lIn[lC];
     for lC := 1 to (lnPad-lnDec) do
         result := result + '0';
     for lC := (lPos+1) to lLen do
         result := result+lIn[lC];
end;

procedure SortStrPadded (var lStr: TStringList);
//file1,file2...file10  not file1,file10..file2
//may be slow: not a great sorting algorithm
//may be inefficient: not sure if strings are exchanged or only pointers...
var counter, look:integer; temp:Tstrings;
begin
   if lStr.Count < 2 then exit;
   temp := TStringList.Create;
   for counter:=0 to lStr.Count-1 do
          temp.Append(LeadingZeroFilename{LowerCase}(lStr[counter]));
          for counter:=0 to temp.Count-1 do
               for look:=counter+1 to temp.Count-1 do
               if temp[look]<temp[counter] then begin
               lStr.Exchange(look, counter);
               temp.Exchange(look,counter);
     end;
     temp.Free;
end;

function PasStr (lStr: string): string; //removes nulls
var
   i: integer;
   t: string;
begin
     result := '';
     for i := 1 to length(lStr) do begin
         if (lStr[i] <> kDel) and(lStr[i] <> kTab) and (lStr[i] <> kEsc) and (lStr[i] <> chr(10)) and (lStr[i] <> chr (13)) and (ord(lStr[i]) <> 0) then
            result := result + lStr[i];
     end;
     result := '"'+result +'"'
end;

function NIIstr (lFileName: string; lHdr : TNIFTIhdr): string;
begin
     result := lFileName
     +kTab+'XYZT'+kTab+inttostr(lHdr.Dim[1])+kTab+inttostr(lHdr.Dim[2])+kTab+inttostr(lHdr.Dim[3])+kTab+inttostr(lHdr.Dim[4])
     +kTab+'XYZTmm'+kTab+floattostr(lHdr.PixDim[1])+kTab+floattostr(lHdr.PixDim[2])+kTab+floattostr(lHdr.PixDim[3])+kTab+floattostr(lHdr.PixDim[4])
     +kTab+'Description'+kTab+PasStr(lHdr.descrip)
     +kTab+'Data_Type'+kTab+PasStr(lHdr.Data_Type)
     +kTab+'db_name'+kTab+PasStr(lHdr.db_name)
     +kTab+'aux_file'+kTab+PasStr(lHdr.aux_file)
     +kTab+'intent_name'+kTab+PasStr(lHdr.intent_name)

     ;
end;

function ExtractNIFTIHdrs(var lStr: TStringList): boolean;
var
   lHdrName: string;
   lHdr : TNIFTIhdr;
   lByteSwap: boolean;
   lVol,lnVol : integer;
   lO: TNIIOpts;
begin
   result := false;
   lnVol := lStr.Count;
   if lnVol < 1 then
      exit;
   SortStrPadded(lStr);
   for lVol := 1 to lnVol do begin
       lHdrName := lStr[lVol-1];
       if not NIFTIhdr_LoadHdr (lHdrName, lHdr,lO) then
          dcmMsg('Unable to find '+lHdrName)
       else
           dcmMsg(NIIstr(lHdrName,lHdr));

       //dcmMsg( inttostr(lVol)+': '+lHdrName);
   end;
end;


function Stack3Dto4D(var lStr: TStringList; lOverwrite: boolean; lPrefs: TPrefs): boolean;
//function Reorder4D(var lHdrName: string; var lHdr: TNIFTIhdr; lOverwrite: boolean; lPrefs: TPrefs): boolean;
label 123;
var
   lOutBuffer,lIBuffer: byteP;
   lInOffset,lnVol,l4DVolBytes,l3DVolBytes,lIn3DBytes,l4DBytes,lVol,lInPos,lOutPos,lSlice: integer;
   lHdrName,lOutImgName: string;
   lHdr1,lHdr2,lOutHdr : TNIFTIhdr;
   lO: TNIIOpts;
   lPrefs4D: TPrefs;
begin
   result := false;
   lnVol := lStr.Count;
   if lnVol < 2 then begin
      dcmMsg('Stack 3D to 4D requires >1 volume');
      exit;
   end;
   SortStrPadded(lStr);
   lHdrName := lStr[0];
   NIFTIhdr_LoadHdr (lHdrName, lHdr1,lO);
   for lVol := 1 to lnVol do begin
       lHdrName := lStr[lVol-1];
       if not NIFTIhdr_LoadHdr (lHdrName, lHdr2,lO) then begin
          dcmMsg('Stack 3D to 4D unable to find '+lHdrName);
          exit;
       end;
       if (lHdr1.dim[4] > 1) then begin
          dcmMsg('Stack 3D to 4D aborted, image is already 4D: '+lHdrName );
          exit;
       end;
       if (lHdr1.dim[1] <> lHdr2.dim[1]) or (lHdr1.dim[2] <> lHdr2.dim[2]) or
          (lHdr1.dim[3] <> lHdr2.dim[3]) or (lHdr1.datatype <> lHdr2.datatype) then begin
          dcmMsg('Stack 3D to 4D aborted, image dimensions/datatype vary '+lHdrName + ' <> '+lStr[0]);
          exit;
       end;
       //dcmMsg( inttostr(lVol)+': '+lHdrName);
   end;
   lOutHdr := lHdr1;
   lOutHdr.dim[4] := lnVol;
   l3DVolBytes := lHdr1.dim[1]*lHdr1.dim[2]*lHdr1.dim[3]*(lHdr1.bitpix div 8);
   l4DVolBytes := l3DVolBytes * lnVol;
   GetMem(lOutBuffer,l4DVolBytes+kNIIImgOffset);

   dcmMsg('Order in output file:');
   lOutImgName := ChangeFilePrefix (lStr[0],'4D');
   lOutPos := kNIIImgOffset + 1;
   for lVol := 1 to lnVol do begin
       lHdrName := lStr[lVol-1];
       dcmMsg( inttostr(lVol)+': '+lHdrName);
       if not NIFTIhdr_LoadImg (lHdrName, lHdr2, lIBuffer, lInOffset,lO) then begin
          dcmMsg('3D -> 4D error loading image '+lHdrName);
          goto 123;
       end;
       Move(lIBuffer^[lInOffset+1],lOutBuffer^[lOutPos],l3DVolBytes);
       freemem(lIBuffer);
       lOutPos := lOutPos + l3DVolBytes;
   end;
   lPrefs4D := lPrefs;
   lPrefs4D.fourD := true;
   dcmMsg('4D image '+lOutImgName);
   if  SaveNIfTICore (lOutImgName, lOutBuffer, kNIIImgOffset+1, lOutHdr, lPrefs4D) = '' then begin
      dcmMsg('3D -> 4D Error');
      goto 123;
   end;

    freemem(lOutBuffer);
  result := true;
  exit;
123:
    freemem(lOutBuffer);
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