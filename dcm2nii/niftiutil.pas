unit niftiutil;
 {$Include ..\common\isgui.inc}
interface
uses
{$IFDEF FPC}
 {$IFDEF GUI}FileUtil, {$ENDIF} //FileUtil requires LResources that requires extra environment variables with tools like matlab
gzio2, process,  //FileUtil,
{$ELSE}
gziod, ShellAPI,Windows,Forms,
{$ENDIF}
  SysUtils,Classes,define_types,filename,dicomtypes,prefs,dialogs_msg, nifti_foreign, nifti_types;
{$H+}
type
  TNIIopts =  RECORD //peristimulus plot
    bs: boolean;
    gzBytes: Int64;  // K_gzBytes_headerAndImageCompressed, K_gzBytes_onlyImageCompressed, K_gzBytes_headerAndImageUncompressed= 0;
    ImgName: string;
  end;

const
	kNIIImgOffset = 352; //header is 348 bytes, but 352 is divisible by 8...
function MaskImgs(lC1template, lC1source: string; lPrefs: TPrefs ; lThresh: single): string;
function MaskImg(ltemplate, lsource: string; lPrefs: TPrefs; lThresh: single ): string;
function Binarize(lC1Name: string; lPrefs: TPrefs ): string;
function SameHdrDim (lAHdr,lBHdr: TNIFTIhdr; lCheck4D, lCheckDataType: boolean): boolean;
procedure NIFTIhdr_ClearHdr (var lHdr: TNIFTIhdr ); //put sensible default values into header
procedure DICOM2AnzHdr (var lBHdr: TNIFTIhdr; lAnonymize: boolean; var lFilename: string; var lDICOMdata: DicomData);
procedure CustomFilename (var lFilename: string);
function SumTPM (lSrcName,lDestName: string; lPrefs: TPrefs; lTissueTypes2Average: integer):string;
//function SameHdrDim (lAHdr,lBHdr: TNIFTIhdr): boolean;
function SaveHdr (var lFilename: ANSIstring; var lInHdr: TNIFTIhdr ; lSwap,lSPM2:boolean): boolean;
function NIFTIhdr_LoadHdr (var lFilename: string; var lHdr: TNIFTIHdr; var lOpts: TNIIopts): boolean;

procedure NIFTIhdr_SlicesToCoord (var lHdr: TNIFTIhdr; lXslice,lYslice,lZslice: integer; var lXmm,lYmm,lZmm: single);
function ChangeNIfTISubformat(lHdrName: string; var lHdr: TNIFTIhdr; lPrefs: TPrefs): boolean;
procedure SaveHdrRAM (var lFilename: ANSIstring; var lInHdr,lOutHdr: TNIFTIhdr ; lSwap,lSPM2:boolean);
function SaveNIfTICore (var lOutImgName: string; var lvBuffer: bytep;  lVolOffset: integer; var lInHdr: TNIFTIhdr; var lPrefs: TPrefs): string;
function SaveNIfTICoreCrop (var lOutImgName: string; var lvBuffer: bytep;  lVolOffset,lStartClip,lEndClip: integer; var lInHdr: TNIFTIhdr; var lPrefs: TPrefs): string;
function NIFTIhdr_LoadImg (var lFilename: string; var lHdr: TNIFTIHdr; var lImgBuffer: byteP; var lImgOffset: integer; var lOpts: TNIIopts): boolean;
//procedure NIFTIhdr_UnswapImg (var lHdr: TNIFTIHdr; var lImgBuffer: byteP; var lImgOffset: integer; var lByteSwap: boolean); //ensures image data is in native space
function NIFTIhdr_LoadImgRaw (LoadHdr: boolean; var lFilename: string; var lHdr: TNIFTIHdr; var lImgBuffer: byteP; var lImgOffset: integer; var lOpts: TNIIopts): boolean;
function NII_force32 (lSrcName,lDestName: string; lPrefs: TPrefs):string;
function Rescale_4Dtissuemaps (lSrcName,lDestName: string; lPrefs: TPrefs; lMakeSym: boolean):string;
function Merge4DFiles (lLowSliceName,lHighSliceName,lDestName: string; lNumberofLowSlicesToCopy: integer; lPrefs: TPrefs):string;
function Insert3Din4D (l3DSliceName,l4DSliceName,lDestName: string; lVol2Copy: integer; lPrefs: TPrefs):string;
function MaskImages(lMaskName: string; lFiles: TStrings; lPrefs: TPrefs; lVol: integer; lSaveThresh: boolean): string;
function NonspatialDimensionsNII (lA: TNIFTIhdr): integer;
implementation
uses dialogsx;



function AddFileNum(lVol,lnVol: integer; var lInName: string): string;
var
lNameWOExt,lExt: string;
begin
    ExtractFileParts (lInName, lNameWOExt,lExt);
    result := lNameWOExt+'_'+PadStr(lVol,length(inttostr(lnVol))) +lExt;
end;


procedure NIFTIhdr_SwapBytes (var lAHdr: TNIFTIhdr ); //Swap Byte order for the Analyze type
var
   lInc: integer;
begin
    with lAHdr do begin
         swap4(hdrsz);
         swap4(extents);
         session_error := swap(session_error);
         for lInc := 0 to 7 do
             dim[lInc] := swap(dim[lInc]);
         Xswap4r(intent_p1);
         Xswap4r(intent_p2);
         Xswap4r(intent_p3);
         intent_code:= swap(intent_code);
         datatype:= swap(datatype);
         bitpix := swap(bitpix);
         slice_start:= swap(slice_start);
         for lInc := 0 to 7 do
             Xswap4r(pixdim[linc]);
         Xswap4r(vox_offset);
         Xswap4r(scl_slope);
         Xswap4r(scl_inter);
         slice_end := swap(slice_end);
         Xswap4r(cal_max);
         Xswap4r(cal_min);
         Xswap4r(slice_duration);
         Xswap4r(toffset);
         swap4(glmax);
         swap4(glmin);
         qform_code := swap(qform_code);
         sform_code:= swap(sform_code);
         Xswap4r(quatern_b);
         Xswap4r(quatern_c);
         Xswap4r(quatern_d);
         Xswap4r(qoffset_x);
         Xswap4r(qoffset_y);
         Xswap4r(qoffset_z);
         for lInc := 0 to 3 do begin
             Xswap4r(srow_x[lInc]);
             Xswap4r(srow_y[lInc]);
             Xswap4r(srow_z[lInc]);
         end;
    end; //with NIFTIhdr
end; //proc NIFTIhdr_Swa

(*procedure TestUINT16 (lval: integer);
//this procedure demonstrates that words and smallints are identical for values 0..32767, so no need to swap if values are in this range
var
l16ui : WordP;
l16i: SmallIntP;
begin
  getmem(l16ui,1*sizeof(word));
  l16ui^[1] := lval;
  l16i := SmallIntP(@l16ui^[1]);
  fx(l16i^[1],l16ui^[1]);
  freemem(l16ui);
end;*)

procedure Uint16 (var lvBuffer: bytep;  lVolOffset: integer; var lInHdr: TNIFTIhdr;var lPrefs: TPrefs; var lByteSwap: boolean);
//kDT_UINT16 saves data range 0..65535, but this is an atypical NIfTI format (not included in earlier Analyze format)
// this procedure saves the data as kDT_SIGNED_SHORT 0..36767
//   if data range is <32767 then saved unchanged, if range is >32767, saved as 15-bit (Least Significant bit clipped).
var
    lmax,lv,lnv: integer;
    lTempB: ByteP;
    l16ui : WordP;
    //l16i: SmallIntP;
    l32f: SingleP;
begin
  lnv := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*NonspatialDimensionsNII(lInHdr);
  if (lInHdr.datatype <> kDT_UINT16) or (lnv < 1) then
    exit;
  l16ui := WordP(@lvBuffer^[lVolOffset]);
  if lByteSwap then begin
    lmax := swap(l16ui^[1]);
    for lv := 1 to lnv do
      if swap(l16ui^[lv]) > lmax then
        lmax := swap(l16ui^[lv]);

  end else begin
    lmax := l16ui^[1];
    for lv := 1 to lnv do
      if l16ui^[lv] > lmax then
        lmax := l16ui^[lv];
  end;
  if lmax < 32768 then begin //lossless: unsigned range <32768 (15 bits), so can be stored in signed 16bit
    lInHdr.datatype := kDT_SIGNED_SHORT;
    dcmMsg(' brightest voxel was '+inttostr(lmax)+': data will be saved as 16-bit signed integer.');
    (*next lines not required, as range 0..32767 is stored identically for WORDS and SMALLINTS, see TestUINT16
    l16i := SmallIntP(@lvBuffer^[lVolOffset]);
    for lv := 1 to lnv do
      l16i^[lv] := l16ui^[lv];  *)
  end else if not lPrefs.UINT16toFLOAT32 then begin
     dcmMsg('Warning: unusual NIFTI format UINT16, range: '+inttostr(lMax) );
     dcmMsg(' If you prefer compatibility, edit your preference named UINT16toFLOAT32');
  end else begin
    dcmMsg('Warning: for compatibility, converting UINT16->FLOAT32, range: '+inttostr(lMax) );
    dcmMsg(' If you prefer filesize over compatibility, edit your preference named UINT16toFLOAT32');
    lInHdr.datatype := kDT_Float;
    lInHdr.bitpix := 32;
    lmax := lVolOffset+ (lnv*sizeof(Word));
    GetMem(lTempB,lmax);
    if lByteSwap then begin
      dcmMsg(' Swapping data to native byte order (Big vs Little Endian)');
      for lv := 1 to lnv do
        l16ui^[lv] := swap(l16ui^[lv]);
    end;
    for lv := 1 to lmax do
      lTempB^[lv] := lvBuffer^[lv];
    freemem(lvBuffer);
    GetMem(lvBuffer,lVolOffset+ (lnv*sizeof(single)));
    for lv := 1 to lVolOffset do //copy header
      lvBuffer^[lv] := lTempB^[lv];
    l16ui := WordP(@lTempB^[lVolOffset]);
    l32f := SingleP(@lvBuffer^[lVolOffset]);
    for lv := 1 to lnv do
     l32f^[lv] :=l16ui^[lv];
    Freemem(lTempB);
  end;// if range requires conversion to 32-bit float
end; //Uint16


function getPigzNameWithPath: string;
//returns path to pigz executable, e.g. '/Users/rorden/downloads/pigz-master/pigz';
var
  i: integer;
  exename: string;
  {$IFDEF DARWIN}  temp: string;{$ENDIF}
begin
  {$IFDEF ENDIAN_BIG}
  Msg('pigz not available with PowerPC computers');
  result := '';
  exit;
  {$ENDIF}
  for i := 1 to 2 do begin
  {$IFDEF UNIX}
    if i = 1 then
      exename := 'pigz'
    else
      exename := 'pigz_mricron';
    {$IFDEF GUI}
      result := FindDefaultExecutablePath(exename);  // "which pigz"
      if length(result) > 0 then
         exit;
    {$ELSE}
       result := exename;
       if fileexists(result) then exit;
       result := ExtractFilePath(  paramstr(0))+exename;
       if fileexists(result) then exit;
       result := '/usr/bin/'+exename;
       if fileexists(result) then exit;
       result := '/usr/local/bin/'+exename;
       if fileexists(result) then exit;
    {$ENDIF}
  {$ELSE}
    if i = 1 then
      exename := 'pigz.exe'
    else
      exename := 'pigz_mricron.exe';
  {$ENDIF}
  result := ExtractFilePath(  paramstr(0))+exename;
  if fileexists(result) then exit;
  {$IFDEF DARWIN}
  temp := result;
  result := ExtractFilePath(paramstr(0));
  result := LeftStr(result, Pos((ExtractFileName(paramstr(0))+'.app'), result)-1)+exename;
  if fileexists(result) then exit;
  {$ENDIF}
  end; //for i:= 1 to 2


  {$IFDEF DARWIN}
     {$IFDEF GUI}
     dcmMsg('File compression error: pigz does not exist in you path or '+result+' or '+temp);
     {$ELSE}
      dcmMsg('File compression error: to use "pigz" place it in the same folder as '+ paramstr(0));
     {$ENDIF}
  {$ELSE}
         {$IFDEF GUI}
         dcmMsg('File compression error: pigz does not exist in you path or '+result);
         {$ELSE}
         dcmMsg('File compression error: to use "pigz" place it in the same folder as '+ paramstr(0));
         {$ENDIF}
  {$ENDIF}
  result := '';
end;

{$IFDEF FPC} //Freepascal has handy 'Process' for calling console applications
function runPigz(var lImgName : string; processes: integer): boolean;
// abs(processes): 1= default (as many as available, 2..n: use this many processors
//  if processes is a NEGATIVE value, application does not wait for Pigz to complete...
var
   AProcess: TProcess;
   Acmd: string;
   AResponse: TStringList;
   i: integer;
begin
  Acmd := getPigzNameWithPath;  //+' -k' //<- to KEEP original
  if length(Acmd) < 1 then exit;
  //Acmd := Acmd +' -v -k';//verbose, keep files
  if abs(processes) > 1 then
     Acmd := Acmd + ' -p '+inttostr( abs(processes) );
  Acmd := Acmd +' '+lImgName;
  dcmMsg('External compression: '+Acmd);
  AProcess := TProcess.Create(nil);
  //AProcess.Environment.Add('FSLDIR='/usr/local/fsl/);   //optional
  AProcess.CommandLine := Acmd;
  if (processes > 0) then //wait for pgzip to complete...
    AProcess.Options := AProcess.Options + [poWaitOnExit, poStderrToOutPut, poUsePipes]
  else  //do not wait for pigz
    AProcess.Options := AProcess.Options + [poStderrToOutPut, poUsePipes];
  AProcess.Execute;
  if (processes > 0) then begin//wait for pgzip to complete...
    AResponse := TStringList.Create;
    AResponse.LoadFromStream(AProcess.Output);
    if AResponse.Count > 0 then
     for i := 1 to AResponse.Count do
         dcmMsg(' '+AResponse.Strings[i-1]);
    AResponse.Free;
  end;
  AProcess.Free;
end;
{$ELSE} //Delphi does not have 'Process' for calling console applications

procedure ExecNewProcess(AppName, ACmd : String; WaitUntilDone: boolean);
var
  StartInfo  : TStartupInfo;
  ProcInfo   : TProcessInformation;
  CreateOK   : Boolean;
begin
  { fill with known state }
  FillChar(StartInfo,SizeOf(TStartupInfo),#0);
  FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
  StartInfo.cb := SizeOf(TStartupInfo);
  CreateOK := CreateProcess(PChar(AppName),Pchar(Acmd), nil, nil,False,
              CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS, 
              nil, nil, StartInfo, ProcInfo);
  { check to see if successful }
  if (CreateOK) and (WaitUntilDone) then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
end;

function runPigz(var lImgName : string; processes: integer): boolean;
// abs(processes): 1= default (as many as available, 2..n: use this many processors
//  if processes is a NEGATIVE value, application does not wait for Pigz to complete...
var
   AppName, Acmd: string;
begin
  AppName := getPigzNameWithPath;  //+' -k' //<- to KEEP original
  if length(AppName) < 1 then exit;
  Acmd := '';
  //Acmd := Acmd +' -v -k';//verbose, keep files
  if abs(processes) > 1 then
     Acmd := Acmd + ' -p '+inttostr( abs(processes) );
  Acmd := Acmd +' "'+lImgName+'"';
  dcmMsg('External compression: '+AppName+' '+Acmd);
  Acmd := AppName+' '+Acmd;
  ExecNewProcess(AppName, ACmd, (processes > 0));
end;
{$ENDIF}


function SaveNIfTICore (var lOutImgName: string; var lvBuffer: bytep;  lVolOffset: integer; var lInHdr: TNIFTIhdr; var lPrefs: TPrefs): string;
//image data should start at lVolOffset - this should be AT LEAST kNIIImgOffset (=352) bytes for creating .nii.gz files
//important note - when converting 4D to 3D to .nii format the lvBuffer is changed :: must correct this
var
   lPref : TPrefs;
   lVol,lVolStart,lVolBytes: integer;
   lOutF: File;
   lNoGZName,lHdrName,lImgName: string;
   l3dHdr,lOutHdr : TNIFTIHdr;
   lHdrBupRA: bytep;
   lByteSwap: boolean;
begin
  lByteSwap := false;
     lNoGZName := (lOutImgName);
     StripGZExt(lNoGZName); //we want to convert filename.nii.gz -> filename.hdr not -> filename.nii.hdr
     StripNIIVOIExt(lNoGZName);//we want to convert filename.nii.voi to filename.hdr
     lPref := lPrefs;
     CorrectPrefs(lPref);
     result := '';
     Uint16 (lvBuffer,lVolOffset, lInHdr,lPrefs,lByteSwap);
     if (not lPref.FourD) and (lInHdr.dim[4] > 1) then begin //4D -> 3D
        lVolBytes := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*trunc(((lInHdr.bitpix)+7)/8);
        lVolStart := lVolOffset;
        l3dHdr := lInHdr;
        l3dHdr.dim[4] := 1;
        for lVol := 1 to lInHdr.dim[4] do begin
            //1st - save header
            lHdrName := AddFileNum(lVol,lInHdr.dim[4],lNoGZName);
            result := SaveNIfTICore (lHdrName, lvBuffer, lVolStart, l3dHdr, lPref);
            lVolStart := lVolStart + lVolBytes;
            //SaveNiftiCore new filename, new offset
        end; //for each vol
        exit;
     end; //l4Dto3D
     Filemode := 2;
     lVolBytes := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*lInHdr.dim[4]*trunc(((lInHdr.bitpix)+7)/8);
     if ((kNIIImgOffset+lVolBytes)> DiskFreeEx(lNoGZName)) then begin
		    dcmMsg('There is not enough free space on the destination disk to save the data. '+kCR+
		    lNoGZName+ kCR+' Bytes Required: '+inttostr(lVolBytes) );
		    exit;
     end;

     if (lPref.SingleNIIFile) then begin
        lVolStart := lVolOffset-kNIIImgOffset;
        lVolBytes := lVolBytes + kNIIImgOffset;
        if lVolStart < 1 then begin
           dcmMsg('SaveNIfTICore Error: '+inttostr(lVolStart));
           exit;
        end;

        getmem(lHdrBupRA,kNIIImgOffset);
        Move(lvBuffer^[lVolStart],lHdrBupRA^[1],kNIIImgOffset);
        //bytes 349,350,351,352 should be set to zero
        lVol := 0;
        lvBuffer^[kNIIImgOffset-3+lVol] := 0;
        lvBuffer^[kNIIImgOffset-2+lVol] := 0;
        lvBuffer^[kNIIImgOffset-1+lVol] := 0;
        lvBuffer^[kNIIImgOffset+lVol] := 0;
        //next - create [potentially byte swapped] header and load into buffer
        lImgName :=  changefileext(lNoGZName,'.nii');

        SaveHdrRAM (lImgName,lInHdr,lOutHdr, lByteSwap,lPrefs.SPM2);
        Move(lOutHdr,lvBuffer^[lVolStart],sizeof(lOutHdr)); //move 348 byte header in place
        //finally - write buffer to disk
        if (lPrefs.Gzip) and (lPrefs.usePigz <> 0) and (length(getPigzNameWithPath) > 0) then begin
            AssignFile(lOutF, lImgName);
            Rewrite(lOutF,1);
            BlockWrite(lOutF, lvBuffer^[lVolStart], lVolBytes);
            CloseFile(lOutF);
            runPigz(lImgName, lPrefs.usePigz);
           //DeleteFile(lImgName);
        end else if lPrefs.Gzip then begin
            if lPrefs.VOI then
                lImgName :=  changefileext(lNoGZName,'.voi')
            else
                lImgName :=  changefileext(lNoGZName,'.nii.gz');
            dcmMsg('GZip...' + extractfilename(lImgName));
            GZipBuffer(lImgName, @lvBuffer^[lVolStart],lVolBytes,true);


        end else begin //not .nii.gz -> .nii
            dcmMsg('Saving '+lImgName);
            AssignFile(lOutF, lImgName);
            Rewrite(lOutF,1);
            BlockWrite(lOutF, lvBuffer^[lVolStart], lVolBytes);
            CloseFile(lOutF);
        end; //else no GZip
        Move(lHdrBupRA^[1],lvBuffer^[lVolStart],kNIIImgOffset); //replace data overwritten by header - otherwise 4D->3D corrupts lvBuffer
        freemem(lHdrBupRA);
     end else begin  //not .nii -> hdr and img
         lHdrName :=  changefileext(lNoGZName,'.hdr');
         lImgName :=  changefileext(lNoGZName,'.img');
        //next - create [potentially byte swapped] header and save to disk
        if not SaveHdr (lHdrName,lInHdr, lByteSwap,lPrefs.SPM2) then
           exit;
        //finally - write buffer to disk
        AssignFile(lOutF, lImgName);
        Rewrite(lOutF,1);
        BlockWrite(lOutF, lvBuffer^[lVolOffset], lVolBytes);
        CloseFile(lOutF);
     end; //else hdr+img

     result := lImgName;
end;

function SaveNIfTICoreCrop (var lOutImgName: string; var lvBuffer: bytep;  lVolOffset,lStartClip,lEndClip: integer; var lInHdr: TNIFTIhdr; var lPrefs: TPrefs): string;
var
   lVolStart,lVolBytes: integer;
   lClipName: string;
   lClipHdr : TNIFTIHdr;
begin
     result := '';
     if (lStartClip < 0) or (lEndClip < 0) then
        exit; //no negative values
     if (lStartClip <= 0) and (lEndClip <= 0) then
        exit; //no change
     if (lStartClip+lEndClip) >= lInHdr.dim[4] then
        exit; //can not remove this many volumes
     lVolBytes := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*trunc(((lInHdr.bitpix)+7)/8);
     lClipHdr := lInHdr;
     lClipHdr.dim[4] := lInHdr.dim[4]-lStartClip-lEndClip;
     lVolStart := lVolOffset + (lStartClip*lVolBytes);
     lClipName := ChangeFilePrefix (lOutImgName,'x');
     result := SaveNIfTICore (lClipName, lvBuffer, lVolStart, lClipHdr, lPrefs);
end;

function SubBound (lVal,lMin: integer): integer;
begin
    result := lVal;
    if result < lMin then
       result := lMin;
end;

function NonspatialDimensionsNII (lA: TNIFTIhdr): integer;
//returns sum of 4th, 5th, 6th and 7th dimension...
begin
     result := SubBound(lA.dim[4],1)*SubBound(lA.dim[5],1)*SubBound(lA.dim[6],1)*SubBound(lA.dim[7],1);
end;

procedure NIFTIhdr_UnswapImgX (var lHdr: TNIFTIHdr; var lImgBuffer: byteP; var lImgOffset: integer; var lByteSwap: boolean); //ensures image data is in native space
//returns data in native endian
//sets 'ByteSwap' flag to false. E.G. a big-endian image will be saved as little-endian on little endian machines
var
   lInc,lImgSamples : integer;
   //2f : SingleP;
   l32i : LongIntP;
   l16i : SmallIntP;
begin
     if not lByteSwap then exit;
     case lHdr.datatype of
          kDT_UNSIGNED_CHAR : begin
              lByteSwap := false; //single byte data - no need to byte swap...
              exit;
            end;
	  kDT_SIGNED_SHORT,kDT_SIGNED_INT,kDT_FLOAT: ;//supported format
         else begin
             dcmMsg('niftiutil UnSwapImg error: datatype not supported.');
             exit;
         end;
     end; //case
     lImgSamples := lHdr.Dim[1] *lHdr.Dim[2]*lHdr.Dim[3]*NonspatialDimensionsNII(lHdr);
     if lImgSamples < 1 then
        exit;
     case lHdr.datatype of
	  kDT_SIGNED_SHORT: begin
             l16i := SmallIntP(@lImgBuffer^[lImgOffset+1]);
             for lInc := 1 to lImgSamples do
                 l16i^[lInc] := Swap(l16i^[lInc]);
          end; //l16i
          kDT_SIGNED_INT,kDT_FLOAT: begin
             //note: for the purposes of byte swapping, floats and long ints are the same
             l32i := LongIntP(@lImgBuffer^[lImgOffset+1]);
             for lInc := 1 to lImgSamples do
                 Swap4(l32i^[lInc]);
          end;//32i
          (*kDT_FLOAT: begin
             l32f := SingleP(@lImgBuffer^[lImgOffset+1]);
             for lInc := 1 to lImgSamples do
                 pswap4r(l32f^[lInc]);  //faster as procedure than function see www.optimalcode.com
          end; //32f*)
     end; //case
     lByteSwap := false;
end;

function NIFTIhdr_LoadImgRaw (LoadHdr: boolean; var lFilename: string; var lHdr: TNIFTIHdr; var lImgBuffer: byteP; var lImgOffset: integer; var lOpts: TNIIopts ): boolean;
//ImgBuffer always offset by kNIIImgOffset- this allows rapid nii.gz creation
//loads img to byteP - if this returns successfully you must freemem(lImgBuffer)
var
   lVol,lFileBytes,lImgBytes: integer;
   lBuf: ByteP;
   lInF: File;
begin
    result := false;
    if loadHdr then begin
       if not NIFTIhdr_LoadHdr (lFilename, lHdr, lOpts) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
       end;
    end;//if we load the header from disk...
   lImgOffset := kNIIImgOffset;// (=352) bytes for creating .nii.gz files
   lVol := NonspatialDimensionsNII(lHdr);//lHdr.dim[4];
   if lVol < 1 then
      lVol := 1;
   lImgBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*lVol*(lHdr.bitpix div 8);
   if not fileexists(lOpts.ImgName) then begin
       dcmMsg('LoadImg Error: Unable to find '+lOpts.ImgName);
       exit;
   end;
   if (lOpts.gzBytes = K_gzBytes_headerAndImageUncompressed) and (FSize (lOpts.ImgName) < ( lImgBytes+round(lHdr.vox_offset))) then begin
       dcmMsg('LoadImg Error: File smaller than expected  '+lOpts.ImgName);
       exit;
   end;
   lFileBytes := lImgBytes+ lImgOffset;
   GetMem(lImgBuffer,lFileBytes);
   if (lOpts.gzBytes <> K_gzBytes_headerAndImageUncompressed) then begin
      lBuf := @lImgBuffer^[lImgOffset+1];

      if lOpts.gzBytes = K_gzBytes_headerAndImageCompressed then
           UnGZip (lOpts.ImgName,lBuf, round(lHdr.vox_offset),lImgBytes)
        else
          UnGZip2 (lOpts.ImgName,lBuf, 0,lImgBytes, round(lHdr.vox_offset));
   end else begin
       AssignFile(lInF, lOpts.ImgName);
       Reset(lInF,1);
       Seek(lInF,round(lHdr.vox_offset));
       Filemode := 0;  //ReadONly
       BlockRead(lInF, lImgBuffer^[lImgOffset+1],lImgBytes);
       CloseFile(lInF);
   end;
   Filemode := 2;  //Read/Write
   NIFTIhdr_UnswapImgX(lHdr, lImgBuffer, lImgOffset,lOpts.bs);
   result := true;
end; //NIFTIhdr_LoadImgRaw

function NIFTIhdr_LoadImg (var lFilename: string; var lHdr: TNIFTIHdr; var lImgBuffer: byteP; var lImgOffset: integer; var lOpts: TNIIOpts): boolean;
begin
    result := NIFTIhdr_LoadImgRaw (true, lFilename, lHdr, lImgBuffer, lImgOffset, lOpts);
end;
(*function NIFTIhdr_LoadImg (var lFilename: string; var lHdr: TNIFTIHdr; var lImgBuffer: byteP; var lImgOffset: integer; var lByteSwap: boolean): boolean;
//ImgBuffer always offset by kNIIImgOffset- this allows rapid nii.gz creation
//loads img to byteP - if this returns successfully you must freemem(lImgBuffer)
var
   lExt,lImgName: string;
   lVol,lFileBytes,lImgBytes: integer;
   lBuf: ByteP;
   lGZin: boolean;
   lInF: File;
begin
    result := false;
    if not NIFTIhdr_LoadHdr (lFilename, lHdr, lByteSwap) then begin
        Msg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
    end;
    lExt := UpCaseExt(lFilename);
    lGZin := ExtGZ(lFilename);
    if lExt = '.VOI' then
       lGZin := true;
   lImgOffset := kNIIImgOffset;// (=352) bytes for creating .nii.gz files
   lVol := lHdr.dim[4];
   if lVol < 1 then
      lVol := 1;
   lImgBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*lVol*(lHdr.bitpix div 8);
   if lExt ='.HDR' then
       lImgName :=  changefileext(lFilename,'.img')
   else
       lImgName := lFilename;
   if not fileexists(lImgName) then begin
       Msg('LoadImg Error: Unable to find '+lImgName);
       exit;
   end;
   if (not lGZin) and (FSize (lImgName) < ( lImgBytes)) then begin
       Msg('LoadImg Error: File smaller than expected  '+lImgName);
       exit;
   end;
   lFileBytes := lImgBytes+ lImgOffset;
   GetMem(lImgBuffer,lFileBytes);
   if lGZin then begin
      lBuf := @lImgBuffer^[lImgOffset+1];
      UnGZip (lImgName,lBuf, round(lHdr.vox_offset),lImgBytes);
   end else begin
       AssignFile(lInF, lImgName);
       Reset(lInF,1);
       Seek(lInF,round(lHdr.vox_offset));
       Filemode := 0;  //ReadONly
       BlockRead(lInF, lImgBuffer^[lImgOffset+1],lImgBytes);
       CloseFile(lInF);
   end;
   Filemode := 2;  //Read/Write
   result := true;
end; *)

procedure CustomFilename (var lFilename: string);
var
   lNew,lPath,lName,lExt: string;
begin
     if not FilenameParts (lFilename, lPath,lName,lExt) then exit;
     lNew := GetStr('Rename '+lName);
     if lNew = '' then exit;
     lFilename := lPath + lNew + lExt;
end;

function ChangeNIfTISubformat(lHdrName: string; var lHdr: TNIFTIhdr;  lPrefs: TPrefs): boolean;
var
   lImgBuffer: byteP;
   lImgOffset: integer;
   lOutImgName: string;
   lOpts: TNIIOpts;
begin
   result := false;
   if not NIFTIhdr_LoadImg (lHdrName, lHdr, lImgBuffer, lImgOffset,lOpts) then  exit;
   dcmMsg('Changing subformat of '+lHdrName);
   lOutImgName := ChangeFilePrefix (lHdrName,'f');
   if lPrefs.CustomRename then
      CustomFilename(lOutImgName);
   if  SaveNIfTICore (lOutImgName, lImgBuffer, lImgOffset+1, lHdr, lPrefs) ='' then exit;
   Freemem(lImgBuffer);
   result := true; //11/2007
   ExitCode := 0;
end;


procedure NIFTIhdr_SlicesToCoord (var lHdr: TNIFTIhdr; lXslice,lYslice,lZslice: integer; var lXmm,lYmm,lZmm: single);
//ignores origin offset
begin
    lXmm := (lHdr.srow_x[0]*lXslice)+ (lHdr.srow_x[1]*lYslice)+(lHdr.srow_x[2]*lzslice);
    lYmm := (lHdr.srow_y[0]*lXslice)+ (lHdr.srow_y[1]*lYslice)+(lHdr.srow_y[2]*lzslice);
    lZmm := (lHdr.srow_z[0]*lXslice)+ (lHdr.srow_z[1]*lYslice)+(lHdr.srow_z[2]*lzslice);
end;

function NIFTIhdr_LoadHdr (var lFilename: string;  var lHdr: TNIFTIHdr; var lOpts: TNIIOpts): boolean;
var
  lHdrFile: file;  {lOri: array [1..3] of single;}
  lBuff: Bytep;
  lReportedSz, lSwappedReportedSz,lHdrSz,lFileSz: Longint;
  lForeignSwapEndian: boolean;
  lExt: string; //1494
begin
  Result := false; //assume error
  lOpts.gzBytes := K_gzBytes_headerAndImageUncompressed;
  lForeignSwapEndian := false;
  if lFilename = '' then exit;
  lOpts.Imgname := lFilename;
  lExt := UpCaseExt(lFilename);
  if lExt = '.IMG' then
	  lFilename := changeFileExt(lFilename,'.hdr');
  if (lExt = '.BRIK') or (lExt = '.BRIK.GZ') then
	  lFilename := changeFileExt(lFilename,'.HEAD');
  lExt := UpCaseExt(lFilename);
  if lExt = '.HDR' then
	  lOpts.Imgname := changeFileExt(lFilename,'.img');
  lHdrSz := sizeof(TniftiHdr);
  lFileSz := FSize (lFilename);
  if lFileSz = 0 then begin
	  dcmMsg('Unable to find NIFTI header named '+lFilename);
	  exit;
  end;
  if lFileSz < lHdrSz then begin
	  dcmMsg('Error in reading NIFTI header: NIfTI headers need to be at least '+inttostr(lHdrSz)+ ' bytes: '+lFilename);
	  exit;
  end;
  FileMode := 0;  { Set file access to read only }
  if (lExt = '.MGH') or (lExt = '.MGZ') or (lExt = '.MHD') or (lExt = '.MHA') or (lExt = '.NRRD') or (lExt = '.NHDR') or (lExt = '.HEAD') then begin
    lOpts.Imgname := lFilename; //will change header name to image name if required
    result := readForeignHeader( lOpts.Imgname, lHdr,lOpts.gzBytes, lForeignSwapEndian);  //we currently ignore result!
  end else begin //native NIfTI
    if (lExt = '.NII.GZ') or (lExt = '.VOI') then begin//1388
	    lBuff := @lHdr;
	    UnGZip(lFileName,lBuff,0,lHdrSz); //1388
      lOpts.gzBytes := K_gzBytes_headerAndImageCompressed;
    end else begin //if gzip
	   {$I-}
	   AssignFile(lHdrFile, lFileName);
	   FileMode := 0;  { Set file access to read only }
	   Reset(lHdrFile, 1);
	   {$I+}
	   if ioresult <> 0 then begin
		  dcmMsg('Error in reading NIFTI header.'+inttostr(IOResult));
		  FileMode := 2;
		  exit;
	   end;
	   BlockRead(lHdrFile, lHdr, lHdrSz);
	   CloseFile(lHdrFile);
   end;
  end;//nifti
  FileMode := 2;
  if (IOResult <> 0) then exit;
  lReportedSz := lHdr.HdrSz;
  lSwappedReportedSz := lReportedSz;
  swap4(lSwappedReportedSz);
  if lReportedSz = lHdrSz then begin
	 lOpts.bs := false;
  end else if lSwappedReportedSz = lHdrSz then begin
	  lOpts.bs := true;
	  NIFTIhdr_SwapBytes (lHdr);
  end else begin
	  dcmMsg('Warning: the header file is not in NIfTi format [the first 4 bytes do not have the value 348]. Assuming big-endian data.');
	  exit;
  end;
  if (lHdr.dim[0] > 7) or (lHdr.dim[0] < 1) then begin //only 1..7 dims, so this
	  dcmMsg('Illegal NIfTI Format Header: this header does not specify 1..7 dimensions.');
	  exit;
  end;
  if lHdr.Dim[4] < 1 then
        lHdr.Dim[4] := 1;
  if lForeignSwapEndian then
    lOpts.bs := true;

  result := true;
end; //func Analyzehdr_LoadHdr

function SaveHdr (var lFilename: ANSIstring; var lInHdr: TNIFTIhdr ; lSwap,lSPM2:boolean): boolean;
var
	lOutHdr: TNIFTIhdr;
	lExt: string;
	lF: File;
	lLong: LongINt;
begin
	 result := false;
	 if ((sizeof(TNIFTIhdr ))> DiskFreeEx(lFilename)) then begin
		dcmMsg('There is not enough free space on the destination disk to save the header. '+kCR+
		lFileName+ kCR+' Bytes Required: '+inttostr(sizeof(TNIFTIhdr )) );
		exit;
	 end;
  if lInHdr.dim[4] > 1 then begin
      lInHdr.dim[0] := 4;
  end else begin
      lInHdr.dim[0] := 3;//3D july2006
      lInHdr.dim[4] := 1;//3D july2006
  end;
	 {if Fileexists(lFileName) then begin
		 Msg('Error: the file '+lFileName+' already exists.');
		 exit;
	 end; }
	 result := true;
	 move(lInHdr, lOutHdr, sizeof(lOutHdr)) ;
	lExt := UpCaseExt(lFileName);
	if (lExt='.IMG') or (lExt ='.HDR') then begin
 {$IFDEF obsoleteENDIAN_BIG} //OSX PPC
		lOutHdr.magic := kswapNIFTI_MAGIC_SEPARATE_HDR;
  {$ELSE}
		lOutHdr.magic := kNIFTI_MAGIC_SEPARATE_HDR;
  {$ENDIF}
		lOutHdr.vox_offset := 0;
                  if lSPM2 then begin //SPM2 does not recognize NIfTI - origin values will be wrong
                   lOutHdr.magic := 0;
                   lOutHdr.qform_code := 0;
                   lOutHdr.sform_code:= 0;
                   lOutHdr.quatern_b := 0;
                   lOutHdr.quatern_c := 0;
                   lOutHdr.quatern_d := 0;
                   lOutHdr.qoffset_x  := 0;
                   lOutHdr.qoffset_y := 0;
                   lOutHdr.qoffset_z := 0;
                end;
	end else begin
 {$IFDEF obsoleteENDIAN_BIG} //OSX PPC
		lOutHdr.magic := kswapNIFTI_MAGIC_EMBEDDED_HDR;
  {$ELSE}
		lOutHdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
  {$ENDIF}
	lOutHdr.vox_offset := kNIIImgOffset;//352 bytes
	end;
 {$IFDEF obsoleteENDIAN_BIG} //OSX PPC
	 if not lSwap  then
  {$ELSE}
	 if lSwap  then
  {$ENDIF}
        NIFTIhdr_SwapBytes (lOutHdr);{swap to sun format}
     Filemode := 1; //1366
	 AssignFile(lF, lFileName); {WIN}
	 if fileexists(lFilename) then
		Reset(lF,1)
	 else
	 Rewrite(lF,1);
	 BlockWrite(lF,lOutHdr, sizeof(TNIFTIhdr ));
	 if (lExt='.IMG') or (lExt ='.HDR') then begin
	 end else begin
		lLong := 0;
		BlockWrite(lF,lLong, 4);
	 end;
	 CloseFile(lF);
     Filemode := 2; //1366
end;

procedure SaveHdrRAM (var lFilename: ANSIstring; var lInHdr,lOutHdr: TNIFTIhdr ; lSwap,lSPM2:boolean);
var
	lExt: string;
begin
  if lInHdr.dim[4] > 1 then begin
      lInHdr.dim[0] := 4;
  end else begin
      lInHdr.dim[0] := 3;//3D july2006
      lInHdr.dim[4] := 1;//3D july2006
  end;
	 move(lInHdr, lOutHdr, sizeof(lOutHdr)) ;
	lExt := UpCaseExt(lFileName);
	if (lExt='.IMG') or (lExt ='.HDR') then begin
 {$IFDEF obsoleteENDIAN_BIG} //OSX PPC
		lOutHdr.magic := kswapNIFTI_MAGIC_SEPARATE_HDR;
  {$ELSE}
		lOutHdr.magic := kNIFTI_MAGIC_SEPARATE_HDR;
  {$ENDIF}
		lOutHdr.vox_offset := 0;
                  if lSPM2 then begin //SPM2 does not recognize NIfTI - origin values will be wrong
                   lOutHdr.magic := 0;
                   lOutHdr.qform_code := 0;
                   lOutHdr.sform_code:= 0;
                   lOutHdr.quatern_b := 0;
                   lOutHdr.quatern_c := 0;
                   lOutHdr.quatern_d := 0;
                   lOutHdr.qoffset_x  := 0;
                   lOutHdr.qoffset_y := 0;
                   lOutHdr.qoffset_z := 0;
                end;
	end else begin
 {$IFDEF obsoleteENDIAN_BIG} //OSX PPC
		lOutHdr.magic := kswapNIFTI_MAGIC_EMBEDDED_HDR;
  {$ELSE}
		lOutHdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
  {$ENDIF}
	lOutHdr.vox_offset := kNIIImgOffset;//352 bytes

	end;
 {$IFDEF obsoleteENDIAN_BIG} //OSX PPC
	 if not lSwap  then
  {$ELSE}
	 if lSwap  then
  {$ENDIF}
        NIFTIhdr_SwapBytes (lOutHdr);{swap to sun format}
end;

procedure NIFTIhdr_SetIdentityMatrixx (var lHdr: TNIFTIHdr); //create neutral rotation matrix
var lInc: integer;
begin
	with lHdr do begin
		 for lInc := 0 to 3 do
			 srow_x[lInc] := 0;
		 for lInc := 0 to 3 do
             srow_y[lInc] := 0;
         for lInc := 0 to 3 do
             srow_z[lInc] := 0;
         for lInc := 1 to 16 do
             intent_name[lInc] := chr(0);
         //next: create identity matrix: if code is switched on there will not be a problem
		 srow_x[0] := 1;
         srow_y[1] := 1;
         srow_z[2] := 1;
    end;
end; //proc NIFTIhdr_IdentityMatrix

procedure NIFTIhdr_ClearHdr (var lHdr: TNIFTIhdr ); //put sensible default values into header
var lInc: byte;
begin
  with lHdr do begin
         {set to 0}
         HdrSz := sizeof(TNIFTIhdr);
         for lInc := 1 to 10 do
             Data_Type[lInc] := chr(0);
         for lInc := 1 to 18 do
             db_name[lInc] := chr(0);
         extents:=0;
         session_error:= 0;
         regular:='r';
		 dim_info:=(0);
         dim[0] := 4;
         for lInc := 1 to 7 do
             dim[lInc] := 0;
         intent_p1 := 0;
         intent_p2 := 0;
         intent_p3 := 0;
         intent_code:=0;
         datatype:=0 ;
         bitpix:=0;
         slice_start:=0;
         for lInc := 1 to 7 do
             pixdim[linc]:= 1.0;
         vox_offset:= 0.0;
         scl_slope := 1.0;
         scl_inter:= 0.0;
         slice_end:= 0;
         slice_code := 0;
		 xyzt_units := 10;
         cal_max:= 0.0;
         cal_min:= 0.0;
         slice_duration:=0;
         toffset:= 0;
         glmax:= 0;
         glmin:= 0;
         for lInc := 1 to 80 do
             descrip[lInc] := chr(0);{80 spaces}
         for lInc := 1 to 24 do
             aux_file[lInc] := chr(0);{80 spaces}
         {below are standard settings which are not 0}
         bitpix := 16;//vc16; {8bits per pixel, e.g. unsigned char 136}
         DataType := 4;//vc4;{2=unsigned char, 4=16bit int 136}
         Dim[0] := 3;
         Dim[1] := 256;
         Dim[2] := 256;
         Dim[3] := 128;
         Dim[4] := 1; {n vols}
         Dim[5] := 1;
         Dim[6] := 1;
         Dim[7] := 1;
         glMin := 0;
         glMax := 255;
         qform_code := kNIFTI_XFORM_UNKNOWN;
         sform_code:= kNIFTI_XFORM_UNKNOWN;
         quatern_b := 0;
         quatern_c := 0;
         quatern_d := 0;
         qoffset_x := 0;
         qoffset_y := 0;
         qoffset_z := 0;
         NIFTIhdr_SetIdentityMatrixx(lHdr);
         magic := kNIFTI_MAGIC_SEPARATE_HDR;
    end; //with the NIfTI header...
end; //proc NIFTIhdr_ClearHdr

procedure DICOM2AnzHdr (var lBHdr: TNIFTIhdr; lAnonymize: boolean; var lFilename: string; var lDICOMdata: DicomData);
var lInc,lLen: integer;
   lStr: string;
begin
  NIFTIhdr_ClearHdr(lBHdr);
if not lAnonymize then begin
  //next: put PatientID into patient_ID array
   lLen := length(lDICOMdata.ProtocolName);
   if lLen > 23 then lLen := 23; //24=size of aux_file
   if lLen > 0 then begin
     lBHdr.aux_file[1] :='!';
      for lInc := 1 to lLen do
        lBHdr.aux_file[lInc+1] := lDICOMdata.ProtocolName[lInc];
   end;
 (*  lLen := length(lDicomData.PatientID);
  if lLen > 10 then lLen := 10; //10=size of patient_ID array
  if lLen > 0 then
     for lInc := 1 to lLen do
      lBHdr.patient_id[lInc] := lDicomData.PatientID[lInc];   *)

  //next: put PatientName into Descrip array
  (*lLen := length(lDicomData.PatientName);
  if lLen > 80 then lLen := 80; //80=size of descrip array
  if lLen > 0 then
     for lInc := 1 to lLen do
      lBHdr.descrip[lInc] := lDicomData.PatientName[lInc];*)
  //next: put StudyDate into exp_date array
 (* lLen := length(lDicomData.StudyDate);
  if lLen > 10 then lLen := 10; //10=size of exp_date array
  if lLen > 0 then
     for lInc := 1 to lLen do
      lBHdr.exp_date[lInc] := lDicomData.StudyDate[lInc]; *)
  //next: put AcqTime into exp_time array
  (*lLen := length(lDicomData.AcqTime);
  if lLen > 10 then lLen := 10; //10=size of exp_time array
  if lLen > 0 then
     for lInc := 1 to lLen do
      lBHdr.exp_time[lInc] := lDicomData.AcqTime[lInc];  *)
  //next: put Modality into generated array
  (*lLen := length(lDicomData.modality);
  if lLen > 10 then lLen := 10; //10=size of generated array
  if lLen > 0 then
     for lInc := 1 to lLen do
      lBHdr.generated[lInc] := lDicomData.modality[lInc];*)
end; //Not anonymized
  //next: put TR into db_Name array
  lStr := 'TE='+floattostr(lDicomData.TE) +';sec='+realtostr(lDicomData.SecSinceMidnight,4);
  if not lAnonymize then
     lStr := lStr+';name='+lDicomData.PatientName[lInc] ;
  lLen := length(lStr);
  if lLen > 80 then lLen := 80;
   for lInc := 1 to lLen do
             lBHdr.descrip[lInc] := lStr[lInc];{80 spaces}
   lStr := lDicomData.ImageComments;
  lLen := length(lStr);
  if lLen > 24 then lLen := 24; //10=size of generated array
   for lInc := 1 to lLen do
             lBHdr.aux_file[lInc] := lStr[lInc];//up to 24

  if lDICOMdata.XYZdim[4] > 1 then
  	lBHdr.Dim[0] := 4 //4D Data June 2006
  else
  	lBHdr.Dim[0] := 3;
  lBHdr.Dim[1] := lDICOMdata.XYZdim[1];
  lBHdr.Dim[2] := lDICOMdata.XYZdim[2];
  lBHdr.Dim[3] := lDICOMdata.XYZdim[3];
  lBHdr.Dim[4] := lDICOMdata.XYZdim[4];
  lBHdr.pixdim[1]:= lDICOMdata.XYZmm[1];
  lBHdr.pixdim[2]:= lDICOMdata.XYZmm[2];
  lBHdr.pixdim[3]:= lDICOMdata.XYZmm[3];
  lBHdr.pixdim[4] := lDicomData.TR/1000; //convert MS to second = assumes xyzt = 10

  lBHdr.pixdim[7] := lDICOMdata.SecSinceMidnight;
  if lDICOMdata.IntenScale <> 0 then
	 lBHdr.scl_slope := lDICOMdata.IntenScale
  else
      lBHdr.scl_slope := 1;
  if not specialsingle(lDICOMdata.IntenIntercept) then
	 lBHdr.scl_inter := lDICOMdata.IntenIntercept //1406
  else lBHdr.scl_inter := 0;
  lBHdr.bitpix := 8; //1360
  lBHdr.datatype := 2; //1360
  if lDicomData.Allocbits_per_pixel <> 8 then begin
     if lDicomData.Allocbits_per_pixel = 32 then begin
        lBHdr.bitpix := 32;
        if lDicomData.FloatData then
           lBHdr.datatype := 16
        else
            lBHdr.datatype := 8;
     end else if lDicomData.Allocbits_per_pixel = 64 then begin
        lBHdr.bitpix := 64;
        lBHdr.datatype := 64;
     end else begin //16bits per pixel
         lBHdr.bitpix := 16;
         lBHdr.datatype := kDT_SIGNED_SHORT;
         if (not lDicomData.SignedData) and (lDicomData.Allocbits_per_pixel = 16) then begin
           lBHdr.datatype :=kDT_UINT16;
           //Msg('NII convert warning: unusual 16-bit UNsigned data format - may not be correctly recognized by all software.');
         end;
     end;
  end;
end;  //proc DICOM2AnzHdr

function NII_force32 (lSrcName,lDestName: string; lPrefs: TPrefs):string;
var
   lPOs,lSrcOffset,lVol,lVox: integer;
  l32f : SingleP;
   l32is : LongIntP;
   l16is : SmallIntP;
   l8is,lSrcBuffer,lBuffUnaligned,lBuffAligned: bytep;
   lSrcHdr,lDestHdr: TNIFTIhdr;
   lOpts: TNIIOpts;
begin
     result := '';
     if not NIFTIhdr_LoadHdr (lSrcname, lSrcHdr, lOpts) then exit;
     case lSrcHdr.datatype of
           kDT_UNSIGNED_CHAR : ;
	  kDT_SIGNED_SHORT: ;
          kDT_SIGNED_INT: ;
	  kDT_FLOAT: begin
          dcmMsg('NII convert to 32-bit float error: datatype already 32-bit float.');
             exit;
            end;
         else begin
             dcmMsg('NII convert to 32-bit float error: datatype not supported.');
             exit;
         end;
     end; //case
     lDestHdr := lSrcHdr; //destination has the comments and voxel BPP of source
     //lDestHdr.dim[4] := 1;
     lDestHdr.datatype := kDT_FLOAT;
     lDestHdr.bitpix := 32;
     lVol := lDestHdr.Dim[4];
     lVox := lDestHdr.Dim[1]*lDestHdr.Dim[2]*lDestHdr.Dim[3]*lVol;
     //load dataset
     if not NIFTIhdr_LoadImg (lSrcName, lSrcHdr, lSrcBuffer, lSrcOffset,lOpts) then  exit;
     l8is := (@lSrcBuffer^[lSrcOffset+1]);
     GetMem(lBuffUnaligned ,(4*lVox) + 16+kNIIImgOffset);
     {$IFDEF FPC}
     lBuffAligned := Align(lBuffUnaligned,16); // not commented - check this
     {$ELSE}
     lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
     {$ENDIF}
     lPos := 1;
     l32f := SingleP(@lBuffAligned^[kNIIImgOffset+lPos] );
     case lSrcHdr.datatype of
	  kDT_SIGNED_SHORT: l16is := SmallIntP(l8is );
          kDT_SIGNED_INT:l32is := LongIntP(l8is );
	  //kDT_FLOAT: l32fs := SingleP(l8is );
     end; //case
     if lSrcHdr.datatype = kDT_UNSIGNED_CHAR then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l8is^[lPos];
     end else if lSrcHdr.datatype = kDT_SIGNED_SHORT then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l16is^[lPos];
     end else if lSrcHdr.datatype = kDT_SIGNED_INT then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l32is^[lPos];
     end;
     result :=  SaveNIfTICore (lDestName, lBuffAligned, kNIIImgOffset+1, lDestHdr, lPrefs);
     Freemem(lBuffUnaligned);
     Freemem(lSrcBuffer);
end;
procedure MakeSym (var l32f: SingleP; var lHdr: TNIFTIhdr);
var
   lHalf,lL,lLines,lH,lX,lOffset: integer;
   lV : single;
begin
     lX := lHdr.Dim[1];
     lHalf := lX div 2; //we will not touch middle voxel of odd data...
     lLines := lHdr.Dim[2]*lHdr.Dim[3]*lHdr.Dim[4];
     if (lHalf < 1) or (lLines < 1) then
        exit;

     lOffset := 0;
     for lL := 1 to lLines do begin
         lOffset := lOffset + lX;
         for lH := 1 to lHalf do begin
             lV := (l32f^[lOffset+lH] + l32f^[lOffset+lX-lH+1]) / 2;
             l32f^[lOffset+lH] := lV;
             l32f^[lOffset+lX-lH+1] := lV;
         end;
     end;
end;

function Rescale_4Dtissuemaps (lSrcName,lDestName: string; lPrefs: TPrefs; lMakeSym: boolean):string;
//takes 4D image where each volume is 8-bit tissue map, saves as 32-bit float, ensures that no voxel has more than kmax or less than kmin intensity
const
     kMaxAllTissues = 0.99;
     kMinAllTissues = 0.50; //set to 0 to ignore
     //kMax= 0.85;
     //kMin = 0.000;
var
   lScale,lSum: double;
   lV,lPOs,lSrcOffset,lVol,lVox: integer;
   l32fs,l32f : SingleP;
   l32is : LongIntP;
   l16is : SmallIntP;
   l8is,lSrcBuffer,lBuffUnaligned,lBuffAligned: bytep;
   lSrcHdr,lDestHdr: TNIFTIhdr;
   lOpts: TNIIOpts;
   lSumName: string;
begin
     result := '';
     if not NIFTIhdr_LoadHdr (lSrcname, lSrcHdr, lOpts) then exit;
     case lSrcHdr.datatype of
           kDT_UNSIGNED_CHAR : ;
	  kDT_SIGNED_SHORT: ;
          kDT_SIGNED_INT: ;
	  kDT_FLOAT: ;
         else begin
             dcmMsg('NII convert to 32-bit float error: datatype not supported.');
             exit;
         end;
     end; //case
     lDestHdr := lSrcHdr; //destination has the comments and voxel BPP of source
     //lDestHdr.dim[4] := 1;
     lDestHdr.datatype := kDT_FLOAT;
     lDestHdr.bitpix := 32;
     lVol := lDestHdr.Dim[4];
     lVox := lDestHdr.Dim[1]*lDestHdr.Dim[2]*lDestHdr.Dim[3]*lVol;
     //load dataset
     if not NIFTIhdr_LoadImg (lSrcName, lSrcHdr, lSrcBuffer, lSrcOffset,lOPts) then  exit;
     l8is := (@lSrcBuffer^[lSrcOffset+1]);
     GetMem(lBuffUnaligned ,(4*lVox) + 16+kNIIImgOffset);
     {$IFDEF FPC}
     lBuffAligned := Align(lBuffUnaligned,16); // not commented - check this
     {$ELSE}
     lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
     {$ENDIF}
     lPos := 1;
     l32f := SingleP(@lBuffAligned^[kNIIImgOffset+lPos] );
     case lSrcHdr.datatype of
	  kDT_SIGNED_SHORT: l16is := SmallIntP(l8is );
          kDT_SIGNED_INT:l32is := LongIntP(l8is );
	  kDT_FLOAT: l32fs := SingleP(l8is );
     end; //case
     if lSrcHdr.datatype = kDT_UNSIGNED_CHAR then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l8is^[lPos];
     end else if lSrcHdr.datatype = kDT_SIGNED_SHORT then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l16is^[lPos];
     end else if lSrcHdr.datatype = kDT_SIGNED_INT then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l32is^[lPos];
     end else if lSrcHdr.datatype = kDT_FLOAT then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l32fs^[lPos];
     end;
     if lMakeSym then
        MakeSym (l32f,lDestHdr);

     //next - ensure that no voxel has sum probability more than kMaxAllTissues
     if lVol > 1 then begin  //for 4D data...
        lVox := lDestHdr.Dim[1]*lDestHdr.Dim[2]*lDestHdr.Dim[3]; //this will be done in 3D, not 4D
        for lPos := 1 to lVox do begin
            lSum := 0;
            for lV := 1 to lVol do
             lSum := lSum+ l32f^[lPos + ((lV-1)*lVox )]; //lookup table could speed this up
             if (lSum < kMinAllTissues) and (kMinAllTissues > 0) then begin
               lScale := kMinAllTissues-lSum;
               //add to 5th volume (soft tissue - non-brain
               l32f^[lPos + ((5-1)*lVox )] := lScale + l32f^[lPos + ((5-1)*lVox )]; //lookup table could speed this up
            end else if lSum > kMaxAllTissues then begin
               lScale := (kMaxAllTissues/lSum);
               for lV := 1 to lVol do
                   l32f^[lPos + ((lV-1)*lVox )] := lScale * l32f^[lPos + ((lV-1)*lVox )]; //lookup table could speed this up
            end;
        end; //each voxel
     end; //4D

     (*lVox := lDestHdr.Dim[1]*lDestHdr.Dim[2]*lDestHdr.Dim[3]*lVol;
     //next - ensure no voxel is more than kmax
     for lPos := 1 to lVox do
         if l32f^[lPos] > kMax then
             l32f^[lPos] := kMax;
     //next - ensure that no voxel is less than kmin
     for lPos := 1 to lVox do
         if l32f^[lPos] < kMin then
             l32f^[lPos] := kMin;    *)
     result :=  SaveNIfTICore (lDestName, lBuffAligned, kNIIImgOffset+1, lDestHdr, lPrefs);

     //optional ... SumMap
     if lVol > 1 then begin  //for 4D data...
        lVox := lDestHdr.Dim[1]*lDestHdr.Dim[2]*lDestHdr.Dim[3]; //this will be done in 3D, not 4D
        for lPos := 1 to lVox do begin
            lSum := 0;
            for lV := 1 to lVol do
                lSum := lSum+ l32f^[lPos + ((lV-1)*lVox )]; //lookup table could speed this up
            l32f^[lPos ] := lSum; //lookup table could speed this up
        end; //each voxel
     end; //4D
     lDestHdr.Dim[4] := 1;
     lSumName :=  ChangeFilePrefix (lDestName,'sum');
     result :=  SaveNIfTICore (lSumName, lBuffAligned, kNIIImgOffset+1, lDestHdr, lPrefs);
     //... end SumMap
     Freemem(lBuffUnaligned);
     Freemem(lSrcBuffer);
end;


function SumTPM (lSrcName,lDestName: string; lPrefs: TPrefs; lTissueTypes2Average: integer):string;
//Sum of first three tissue types (GM, WM, CSF
var
   //lScale,lSum: double;
   lPOs,lSrcOffset,lVol,lVox,lnVol: integer;
   l32fs,l32f : SingleP;
   l32is : LongIntP;
   l16is : SmallIntP;
   l8is,lSrcBuffer,lBuffUnaligned,lBuffAligned: bytep;
   lSrcHdr,lDestHdr: TNIFTIhdr;
   lOpts: TNIIOpts;
   //lSumName: string;
begin
     result := '';
     if not NIFTIhdr_LoadHdr (lSrcname, lSrcHdr, lOpts) then exit;
     case lSrcHdr.datatype of
           kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT,kDT_SIGNED_INT,kDT_FLOAT: ;
         else begin
             dcmMsg('SumTPM error: datatype not supported.');
             exit;
         end;
     end; //case
     lDestHdr := lSrcHdr; //destination has the comments and voxel BPP of source
     lDestHdr.datatype := kDT_FLOAT;
     lDestHdr.bitpix := 32;
     lDestHdr.Dim[4] := 1;
     lVox := lDestHdr.Dim[1]*lDestHdr.Dim[2]*lDestHdr.Dim[3];
     //load dataset
     if not NIFTIhdr_LoadImg (lSrcName, lSrcHdr, lSrcBuffer, lSrcOffset,lOpts) then  exit;
     l8is := (@lSrcBuffer^[lSrcOffset+1]);
     lnVol := NonspatialDimensionsNII(lSrcHdr);
     if lnVol > lTissueTypes2Average then
        lnVol := lTissueTypes2Average;
     if lnVol < 1 then
        exit;
     GetMem(lBuffUnaligned ,(4*lVox) + 16+kNIIImgOffset);
     {$IFDEF FPC}
     lBuffAligned := Align(lBuffUnaligned,16); // not commented - check this
     {$ELSE}
     lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
     {$ENDIF}
     lPos := 1;
     l32f := SingleP(@lBuffAligned^[kNIIImgOffset+lPos] );
     for lPos := 1 to lVox do
                l32f^[lPos] := 0;
     if lSrcHdr.datatype = kDT_UNSIGNED_CHAR then begin
        for lVol := 0 to lnVol -1 do
            for lPos := 1 to lVox do
                l32f^[lPos] := l32f^[lPos]+l8is^[lPos+(lVol*lVox)];
     end else if lSrcHdr.datatype = kDT_SIGNED_SHORT then begin
         l16is := SmallIntP(l8is );
        for lVol := 0 to lnVol -1 do
            for lPos := 1 to lVox do
                l32f^[lPos] := l32f^[lPos]+l16is^[lPos+(lVol*lVox)];
     end else if lSrcHdr.datatype = kDT_SIGNED_INT then begin
        l32is := LongIntP(l8is );
        for lVol := 0 to lnVol -1 do
            for lPos := 1 to lVox do
                l32f^[lPos] := l32f^[lPos]+l32is^[lPos+(lVol*lVox)];
     end else if lSrcHdr.datatype = kDT_FLOAT then begin
         l32fs := SingleP(l8is );
         for lVol := 0 to lnVol -1 do
            for lPos := 1 to lVox do
                l32f^[lPos] := l32f^[lPos]+l32fs^[lPos+(lVol*lVox)];
     end;
     result :=  SaveNIfTICore (lDestName, lBuffAligned, kNIIImgOffset+1, lDestHdr, lPrefs);
     lDestHdr.Dim[4] := 1;

     //result :=  SaveNIfTICore (lSumName, lBuffAligned, kNIIImgOffset+1, lDestHdr, lPrefs,lByteSwap);
     //... end SumMap
     Freemem(lBuffUnaligned);
     Freemem(lSrcBuffer);
end;


function SameHdrDim (lAHdr,lBHdr: TNIFTIhdr; lCheck4D, lCheckDataType: boolean): boolean;
var
   i: integer;
begin
     result := true;
     if (lCheckDataType) and (lAHdr.datatype <> lBHdr.datatype) then
        result := false;
     for i := 1 to 3 do
         if (lAHdr.Dim[i] <> lBHdr.Dim[i]) then
            result := false;
     for i := 1 to 3 do
         if (lAHdr.pixdim[i] <> lBHdr.pixdim[i]) then
            result := false;
     if lCheck4D then
         if (lAHdr.Dim[4] <> lBHdr.Dim[4]) then
            result := false;
     if not result then begin
        //fx(1211);
        dcmMsg('Image dimensions or datatype differ');
     end;
end;

function Merge4DFiles (lLowSliceName,lHighSliceName,lDestName: string; lNumberofLowSlicesToCopy: integer; lPrefs: TPrefs):string;
//takes 4D image where each volume is 8-bit tissue map, saves as 32-bit float, ensures that no voxel has more than kmax or less than kmin intensity
var
   lVolOffset,lSliceBytes,lV,lLoOffset,lHiOffset,lVol,lVox: integer;
   l8iHi,l8iLo,lLoBuffer,lHiBuffer {,lBuffUnaligned,lBuffAligned}: bytep;
   lLoHdr,lHiHdr: TNIFTIhdr;
   lOptsLo,lOptsHi: TNIIOpts;
   lBPP: integer;
begin
     result := '';
     if not NIFTIhdr_LoadHdr (lLowSliceName, lLoHdr, lOptsLo) then exit;
     if not NIFTIhdr_LoadHdr (lHighSliceName, lHiHdr, lOptsHi) then exit;
     if lNumberofLowSlicesToCopy < 1 then
        exit;
     if not SameHdrDim(lLoHdr, lHiHdr,true,true) then
        exit;
     case lLoHdr.datatype of
          kDT_UNSIGNED_CHAR : lBPP := 1;
	        kDT_SIGNED_SHORT: lBPP := 2;
          kDT_SIGNED_INT:lBPP := 4;
	        kDT_FLOAT: lBPP := 4;
         else begin
             dcmMsg('Merge4DFiles error: datatype not supported.');
             exit;
         end;
     end; //case
    //lDestHdr.dim[4] := 1;
     lVol := lHiHdr.Dim[4];
     lSliceBytes:= lHiHdr.Dim[1]*lHiHdr.Dim[2] * lBPP;
     //load dataset
     if not NIFTIhdr_LoadImg (lLowSliceName, lLoHdr, lLoBuffer, lLoOffset,lOptsLo) then  exit;
     if not NIFTIhdr_loadImg (lHighSliceName, lhiHdr, lhiBuffer, lhiOffset,lOptsHi) then  exit;
     l8iLo := (@lLoBuffer^[lLoOffset+1]);
     l8iHi := (@lHiBuffer^[lHiOffset+1]);

     for lV := 1 to lVol do begin
         lVolOffset := (lV - 1) * (lSliceBytes*lHiHdr.Dim[3]);
         for lVox := 1 to (lSliceBytes*lNumberofLowSlicesToCopy) do
             l8iHi^[lVox+lVolOffset] := l8iLo^[lVox+lVolOffset];
     end;
     result :=  SaveNIfTICore (lDestName, lhiBuffer, kNIIImgOffset+1, lHiHdr, lPrefs);
     Freemem(lhiBuffer);
     Freemem(lloBuffer);
end;

function Insert3Din4D (l3DSliceName,l4DSliceName,lDestName: string; lVol2Copy: integer; lPrefs: TPrefs):string;
//takes 4D image where each volume is 8-bit tissue map, saves as 32-bit float, ensures that no voxel has more than kmax or less than kmin intensity
var
   lVolOffset,lVolBytes,l3DOffset,l4DOffset,lVox: integer;
   l8i4D,l8i3D,l3DBuffer,l4DBuffer {,lBuffUnaligned,lBuffAligned}: bytep;
   l3DHdr,l4DHdr: TNIFTIhdr;
   lOpts3D,lOpts4D: TNIIOpts;
   lBPP: integer;
begin
     result := '';
     if not NIFTIhdr_LoadHdr (l3DSliceName, l3DHdr, lOpts3D) then exit;
     if not NIFTIhdr_LoadHdr (l4DSliceName, l4DHdr, lOpts4D) then exit;
     if lVol2Copy < 1 then
        exit;
     if not SameHdrDim(l3DHdr, l4DHdr,false,true) then
        exit;
     case l3DHdr.datatype of
           kDT_UNSIGNED_CHAR : lBPP := 1;
	  kDT_SIGNED_SHORT: lBPP := 2;
          kDT_SIGNED_INT:lBPP := 4;
	  kDT_FLOAT: lBPP := 4;
         else begin
             dcmMsg('Merge4DFiles error: datatype not supported.');
             exit;
         end;
     end; //case
    //lDestHdr.dim[4] := 1;
    // lVol := l4DHdr.Dim[4];
     lVolBytes:= l4DHdr.Dim[1]*l4DHdr.Dim[2]*l4DHdr.Dim[3]* lBPP;
     //load dataset
     if not NIFTIhdr_LoadImg (l3DSliceName, l3DHdr, l3DBuffer, l3DOffset,lOpts3D) then  exit;
     if not NIFTIhdr_loadImg (l4DSliceName, l4DHdr, l4DBuffer, l4DOffset,lOpts4D) then  exit;
     l8i3D := (@l3DBuffer^[l3DOffset+1]);
     l8i4D := (@l4DBuffer^[l4DOffset+1]);
     lVolOffset := (lVol2Copy - 1) * (lVolBytes);
     for lVox := 1 to (lVolBytes) do
             l8i4D^[lVox+lVolOffset] := l8i3D^[lVox];
     result :=  SaveNIfTICore (lDestName, l4DBuffer, kNIIImgOffset+1, l4DHdr, lPrefs);
     Freemem(l4DBuffer);
     Freemem(l3DBuffer);
end;


function NIFTIhdr_LoadImg8bit (var lSrcName: string; var lSrcHdr: TNIFTIHdr; var lSrcBuffer: bytep; var lSrcOffset: integer; var lOPts: TNIIOPts): boolean;

begin
     result := false;
     if not NIFTIhdr_LoadImg (lSrcName, lSrcHdr, lSrcBuffer, lSrcOffset,lOpts) then  exit;
     if lSrcHdr.datatype <> kDT_UNSIGNED_CHAR then begin
        dcmMsg('Only able to read 8-bit data.');
        exit;
     end;
     //NIFTIhdr_UnswapImg(lSrcHdr, lSrcBuffer, lSrcOffset,lByteSwap);//interpolation requires data is in native endian
     result := true;
end;

function MaskImages(lMaskName: string; lFiles: TStrings; lPrefs: TPrefs; lVol: integer; lSaveThresh: boolean): string;
var
   lFileOffset,lMaskOffset,lInc,lVox,lPos,lOK: integer;
   lMaskHdr,lFileHdr: TNIFTIHdr;
   lMaskOpts,lFileOpts: TNIIOpts;
   lFilename: string;
   lMaskBuffer,lFileBuffer,l8if: bytep;
   l32fm,l32fmean, l32fmeanpre: singlep;
begin
     result := '';
     if not NIFTIhdr_LoadHdr (lMaskName, lMaskHdr, lMaskOpts) then exit;
     if lMaskHdr.datatype <> kDT_FLOAT then begin
         dcmMsg('This function only works with 32-bit float data.');
         exit;
     end;
     lVox := lMaskHdr.Dim[1]*lMaskHdr.Dim[2]*lMaskHdr.Dim[3];
     if lFiles.Count < 1 then exit;
     if not NIFTIhdr_LoadImg (lMaskName, lMaskHdr, lMaskBuffer, lMaskOffset,lMaskOpts) then  exit;
     l32fm := SingleP(@lMaskBuffer^[lMaskOffset+1+ ((lVol-1)* (lVox*4) )]);
     //l32fo := SingleP(@lBuffAligned^[kNIIImgOffset+lPos] );
     GetMem(l32fmean ,(4*lVox));
     for lPos := 1 to lVox do
         l32fmean^[lPos] := 0;
     GetMem(l32fmeanpre ,(4*lVox));
     for lPos := 1 to lVox do
         l32fmeanpre^[lPos] := 0;
     lOK := 0;
     for lInc := 1 to lFiles.Count do begin
         lFilename := lFiles.Strings[lInc-1];
         if not NIFTIhdr_LoadImg8bit (lFileName, lFileHdr, lFilebuffer, lFileOffset,lFileOpts) then begin
            dcmMsg('Serious error reading '+lFilename);
            exit;
         end;
         if not (SameHdrDim(lMaskHdr,lFileHdr,false,false)) then
            //fx(666)
            //msg('This function only works with data with identical dimensions.')
         else begin
              l8if := (@lFilebuffer^[lFileOffset+1]);
              for lPos := 1 to lVox do begin
                  l32fmeanpre^[lPos] := l32fmeanpre^[lPos] + l8if^[lPos];
                  if l32fm^[lPos] = 0 then
                     l8if^[lPos] := 0
                  else
                      l32fmean^[lPos] := l32fmean^[lPos] + l8if^[lPos];
              end;
              lFilename :=  ChangeFilePrefix (lFilename,'z');
              if lSaveThresh then
                 result :=  SaveNIfTICore (lFilename, lFileBuffer, lFileOffset+1, lFileHdr, lPrefs);
              inc(lOK);
         end;
         Freemem(lFilebuffer);
     end;
     if lOK > 1 then begin
        lMaskHdr.dim[4] := 1; //save only one volume
        lMaskHdr.scl_slope := lFileHdr.scl_slope;
        lMaskHdr.scl_inter := lFileHdr.scl_inter;

        l32fm := SingleP(@lMaskBuffer^[lMaskOffset+1]);
        for lPos := 1 to lVox do
            l32fm^[lPos] := l32fmean^[lPos]/lOK;
        lFilename :=  ChangeFilePrefix (lMaskName,'mean'+inttostr(lVol));
        result :=  SaveNIfTICore (lFilename, lMaskBuffer, lMaskOffset+1, lMaskHdr, lPrefs);
        for lPos := 1 to lVox do
            l32fm^[lPos] := l32fmeanpre^[lPos]/lOK;
        lFilename :=  ChangeFilePrefix (lMaskName,'meanpre'+inttostr(lVol));
        result :=  SaveNIfTICore (lFilename, lMaskBuffer, lMaskOffset+1, lMaskHdr, lPrefs);
     end;
     Freemem(l32fmean);
     Freemem(lMaskBuffer);
end;

function lDigitChar (lString: string): integer;
//returns position of first number in filename, e.g. c:\x1xx.nii would return 5, since '1' is 5th char
var
   lP, lLen: integer;
begin
     result := 0;
     lLen := length(lString);
     if lLen < 1 then
        exit;
     for lP := lLen downto 1 do begin
         if lString[lP] in ['0'..'9'] then
            result := lP;
         if lString[lP] in ['/','\'] then
            exit;
     end;

end;

function Binarize(lC1Name: string; lPrefs: TPrefs ): string;
const
     kMaps = 5;
     kInten: array [1.. kMaps] of integer = ({graymatter}4,{whitematter}5,{csf}3,{bone}2,{soft tissue}1);
var
   //lFileOffset,lMaskOffset,lInc,lVox,lPos,lOK: integer;
   lHname : array [1..kMaps] of string;
   lH: array [1..kMaps] of TNIFTIHdr;
   lMax,lMaxV,lV,lVox,lMap,lCharPos: integer;
   lHOpts: TNIIOpts;
   //lFilename: string;
   lHBuffer,lH8i: array [1..kMaps] of bytep;
   lHOffset: array [1..kMaps] of integer;
   //l32fm,l32fmean, l32fmeanpre: singlep;
begin
     result := '';
     lCharPos :=  lDigitChar (lC1Name);
     if lCharPos < 1 then begin
        dcmMsg('Error: number should be in filename.');
        exit;
     end;
     for lMap := 1 to kMaps do begin
         lHname[lMap] := lC1Name;
         lHname[lMap][lCharPos] := inttostr(lMap)[1];
         if not fileexists(lHname[lMap]) then begin
            dcmMsg('Can not find '+lHname[lMap]);
            exit;
         end;
     end;

     for lMap := 1 to kMaps do begin
         if not NIFTIhdr_LoadImg8bit (lHname[lMap], lH[lMap], lHBuffer[lMap], lHOffset[lMap],lHOpts) then begin
            dcmMsg('Serious error reading '+lHname[lMap]);
            exit;
         end;
         lH8i[lMap] := (@lHBuffer[lMap]^[lHOffset[lMap]+1]);
     end;
     lVox := lH[1].Dim[1]*lH[1].Dim[2]*lH[1].Dim[3];
     (*for lV := 1 to lVox do begin
         lMax := lH8i[4]^[lV] * 2;
         if lMax > 1 {255} then lMax := 255;
         lH8i[4]^[lV] := 255;
     end;     *)
     for lV := 1 to lVox do begin
         lMax := kMaps;
         lMaxV := lH8i[kMaps]^[lV];
         for lMap := (kMaps-1) downto 1 do begin
             if lH8i[lMap]^[lV] > lMaxV then begin
                lMax := lMap;
                lMaxV := lH8i[lMap]^[lV];
             end;
         end;
         lMax := kInten[lMax];
         //if lMax = kMaps then lMax := 0;
         if lMaxV < 25 then lMax := 0;
         lH8i[1]^[lV] := lMax;
     end;
     lH[1].dim[4] := 1; //save only one volume
     lH[1].scl_slope := 1;
     lH[1].scl_inter := 0;
     lHname[1][lCharPos] := 'b';
     result :=  SaveNIfTICore (lHname[1], lHBuffer[1], lHOffset[1]+1, lH[1], lPrefs);
     for lMap := 1 to kMaps do
         Freemem(lHBuffer[lMap]);
end;

(*function NIFTIhdr_LoadImgAs32float (var lSrcName: string; var lSrcHdr: TNIFTIHdr; var lSrcBuffer: bytep; var l32f: singlep; var lSrcOffset: integer; var lByteSwap: boolean): boolean;
//function NIFTIhdr_LoadImgAs32float (var lSrcName: string; var lSrcHdr: TNIFTIHdr; {var lSrcBuffer: bytep;} var l32f: singlep; var lSrcOffset: integer; var lByteSwap: boolean): boolean;
var
   l32is : LongIntP;
   l16is : SmallIntP;
   lImgBuffer,l8is: bytep;
   lPos,lVox: integer;
   //lSrcBuffer: bytep;
begin
     result := false;
     if not NIFTIhdr_LoadImg (lSrcName, lSrcHdr, lImgBuffer, lSrcOffset,lByteSwap) then  exit;
     NIFTIhdr_UnswapImg(lSrcHdr, lImgBuffer, lSrcOffset,lByteSwap);//interpolation requires data is in native endian
     result := true;
     l8is := (@lImgBuffer^[lSrcOffset+1]);

     //GetMem(l32f ,4*lVox );
     case lSrcHdr.datatype of
	  kDT_SIGNED_SHORT: l16is := SmallIntP(l8is );
          kDT_SIGNED_INT:l32is := LongIntP(l8is );
	  kDT_FLOAT: begin l32f := SingleP(l8is ); lSrcBuffer := lImgBuffer; {<- not sure if this works!} exit; end;
     end; //case
     lVox := lSrcHdr.Dim[1]*lSrcHdr.Dim[2]*lSrcHdr.Dim[3];
     GetMem(lSrcBuffer ,(4*lVox) +lSrcOffset);
     l32f := SingleP(@lSrcBuffer^[lSrcOffset+1]);

     if lSrcHdr.datatype = kDT_UNSIGNED_CHAR then
        for lPos := 1 to lVox do
            l32f^[lPos] := l8is^[lPos]
     else if lSrcHdr.datatype = kDT_SIGNED_SHORT then
        for lPos := 1 to lVox do
            l32f^[lPos] := l16is^[lPos]
     else if lSrcHdr.datatype = kDT_SIGNED_INT then
        for lPos := 1 to lVox do
            l32f^[lPos] := l32is^[lPos];
     result := true;
     freemem(lImgBuffer);
     lSrcHdr.datatype := kDT_FLOAT;
end;   *)


function As32 (lSrcName: string; var lSrcHdr:TNIFTIhdr; var l32f: singlep):boolean;
//takes 4D image where each volume is 8-bit tissue map, saves as 32-bit float, ensures that no voxel has more than kmax or less than kmin intensity
var
(*   lScale,lSum: double;
   lV,lPOs,lSrcOffset,lVol,lVox: integer;
   l32fs,l32f : SingleP;
   l32is : LongIntP;
   l16is : SmallIntP;
   l8is,lSrcBuffer,lBuffUnaligned,lBuffAligned: bytep;
   lSrcHdr,lDestHdr: TNIFTIhdr;  *)
   l32is : LongIntP;
   l32fs : SingleP;
   l16is : SmallIntP;
   l8is,lSrcBuffer: bytep;
   lSrcOffset,lVox,lPos: integer;
   lOpts: TNIIOpts;
begin
     result := false;
     if not NIFTIhdr_LoadHdr (lSrcname, lSrcHdr, lOpts) then exit;
     case lSrcHdr.datatype of
           kDT_UNSIGNED_CHAR : ;
	  kDT_SIGNED_SHORT: ;
          kDT_SIGNED_INT: ;
	  kDT_FLOAT: ;
         else begin
             dcmMsg('NII convert to 32-bit float error: datatype not supported.');
             exit;
         end;
     end; //case
     lVox := lSrcHdr.Dim[1]*lSrcHdr.Dim[2]*lSrcHdr.Dim[3];
     //load dataset
     if not NIFTIhdr_LoadImg (lSrcName, lSrcHdr, lSrcBuffer, lSrcOffset,lOpts) then  exit;
     l8is := (@lSrcBuffer^[lSrcOffset+1]);
     GetMem(l32f ,lVox * sizeof(single));
     //lPos := 1;
     case lSrcHdr.datatype of
	  kDT_SIGNED_SHORT: l16is := SmallIntP(l8is );
          kDT_SIGNED_INT:l32is := LongIntP(l8is );
	  kDT_FLOAT: l32fs := SingleP(l8is );
     end; //case
     if lSrcHdr.datatype = kDT_UNSIGNED_CHAR then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l8is^[lPos];
     end else if lSrcHdr.datatype = kDT_SIGNED_SHORT then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l16is^[lPos];
     end else if lSrcHdr.datatype = kDT_SIGNED_INT then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l32is^[lPos];
     end else if lSrcHdr.datatype = kDT_FLOAT then begin
        for lPos := 1 to lVox do
            l32f^[lPos] := l32fs^[lPos];
        for lPos := 1 to lVox do
           if specialsingle(l32f^[lPos]) then
              l32f^[lPos] := 0;
     end;


     freemem(lSrcBuffer);
     result := true;

end;
//function MaskImgs(lC1template, lC1source: string; lPrefs: TPrefs ; lThresh: integer): string;
function MaskImg(ltemplate, lsource: string; lPrefs: TPrefs; lThresh: single ): string;
label
     666;
var
   lH,lT:  TNIFTIHdr;
   lV,lVox,lHOffset: integer;
   lHOpts: TNIIOpts;
  l32fs : SingleP;
  l8is,lHBuffer:  bytep;
   lOutname: string;
begin
     result := '';
     if (not fileexists(lsource)) then begin
        dcmMsg('Can not find '+ lsource);
        exit;
     end;

     if  (not fileexists(ltemplate)) then begin
        dcmMsg('Can not find '+ ltemplate);
        exit;
     end;
     //if not NIFTIhdr_LoadImgAs32float (lsource, lH, lHBuffer, lHOffset,lHSwap) then begin
     if not NIFTIhdr_LoadImg8bit (lsource, lH, lHBuffer, lHOffset,lHOpts) then begin
        dcmMsg('Serious error reading '+lsource);
        exit;
     end;
     //function NIFTIhdr_LoadImgAs32float (var lSrcName: string; var lSrcHdr: TNIFTIHdr; var lSrcBuffer: bytep; var l32f: singlep; var lSrcOffset: integer; var lByteSwap: boolean): boolean;
     if not As32 (ltemplate, lT, l32fs) then  begin
        dcmMsg('Serious error reading '+ltemplate);
        exit;
     end;
     lVox := lH.Dim[1]*lH.Dim[2]*lH.Dim[3];
     if not SameHdrDim (lH,lT, false, false) then begin
        dcmMsg('Image dimensions do not match: '+ltemplate+' <> '+lsource);
        goto 666;
     end;
     l8is := (@lHBuffer^[lHOffset+1]);
     for lV := 1 to lVox do
          if (l32fs^[lV] < lThresh) then
             l8is^[lV] := 0;
     lH.dim[4] := 1; //save only one volume
     lH.scl_slope := 1;
     lH.scl_inter := 0;
     lOutname := ChangeFilePrefix(lsource,'m');
     dcmMsg(lsource +' masked with '+ltemplate +' = '+lOutname);
     result :=  SaveNIfTICore (loutname, lHBuffer, lHOffset+1, lH, lPrefs);
666:
    Freemem(l32fs);
    Freemem(lHBuffer);
end;



function MaskImgs(lC1template, lC1source: string; lPrefs: TPrefs ; lThresh: single): string;
const
     kMaps = 5;
var
   lTName,lSName: string;
   lMap,lSPos,lTPos: integer;
begin
     result := '';
     lSPos :=  lDigitChar (lC1source);
     lTPos :=  lDigitChar (lC1template);
     if (lSPos < 1) or (lTPos < 1)  then begin
        dcmMsg('Error: number should be in filenames: '+lC1template+'  '+ lC1source);
        exit;
     end;
     lSname:= lC1source;
     lTname:= lC1template;

     for lMap := 1 to kMaps do begin
         lSname[lSPos] := inttostr(lMap)[1];
         lTname[lTPos] := inttostr(lMap)[1];
         //msg(lTName+'  '+ lSName);
         if ( fileexists(lTName)) and ( fileexists(lSName)) then
            result := MaskImg(lTName, lSName, lPrefs, lThresh) ;
     end;
     dcmMsg('Masking completed');
end;




end.

