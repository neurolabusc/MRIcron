unit hdr;
interface
{$H+}
{$Include ..\common\isgui.inc}
uses nifti_hdr,define_types,classes, unpm, nifti_types;

procedure MakeStatHdr (var lBGHdr,lStatHdr: TniftiHdr; lMinIntensity,lMaxIntensity,lIntent_p1,lIntent_p2,lIntent_p3: single; lIntent_code: smallint;lIntentName: string);
procedure MakeHdr (var lBGHdr,lStatHdr: TniftiHdr);
function NIFTIhdr_SaveHdrImg (var lFilename: string; var lHdr: TNIFTIHdr; lAllowOverwrite,lSPM2,lSingleFile: boolean;var lImg: SingleP; lnVol: integer): boolean;
function NIFTIhdr_SaveHdrImg8 (var lFilename: string; var lHdr: TNIFTIHdr; lAllowOverwrite,lSPM2,lSingleFile: boolean;var lImg: ByteP; lnVol: integer): boolean;

function Files4D (lFilename: string): boolean;
function Vol4D (lFilename: string): integer;
function FileExists4D (lFilename: string): boolean;
function Filename4D(lFilename: string): string;
function FilenameVol4D (lFilename: string; var lBaseName: string; var lVol: integer): boolean;
function NIFTIhdr_HdrVolumes (lFilenameIn: string): integer;
function BPP (lDataType: integer): integer;
function CreateDecompressed4D(var lImageNames: TStrings): string;
function CheckVoxels(var lHdrNameIn : string; lMaskVoxels, lImageNumber: integer):boolean;
//function CheckVoxelsGroupX(var lG: TStrings; lMaskVoxels: integer):boolean;
function CheckVoxelsGroupX(var lG: TStrings; lMaskHdr: TMRIcroHdr): boolean;
//function CheckVoxelsGroupY(var lG: TStrings):boolean;

procedure DeleteDecompressed4D(lDecomName: string);
implementation
uses
{$IFDEF FPC} gzio2,Controls,
{$ELSE} {gzio,ZLib,}DiskSpaceKludge,gziod,{$ENDIF}
{$IFNDEF UNIX}Windows, {$ENDIF}
{$IFDEF GUI}Dialogs,{$ENDIF}
 Dialogsx ,SysUtils,StatThdsUtil;
//define_types,GraphicsMathLibrary;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}


procedure DeleteDecompressed4D(lDecomName: string);
begin
     if lDecomName = '' then
        exit;
     if not fileexists(lDecomName) then
        exit;
     sysutils.deletefile(lDecomName);
end;

function CheckVoxels(var lHdrNameIn : string; lMaskVoxels, lImageNumber: integer):boolean;
var
	lHdr: TMRIcroHdr;
        lHdrName: string;
        lVox: integer;
begin
	 result := false;
         lHdrName := Filename4D(lHdrNameIn);
	 if not NIFTIhdr_LoadHdr(lHdrName,lHdr) then begin
                 NPMmsg('Unable to load image '+lHdrName);
            exit;
         end;
         lVox := ComputeImageDataBytes8bpp(lHdr);
	 if lVox <> lMaskVoxels then begin
            NPMmsg('Voxels differ for '+lHdrName+' expected '+inttostr(lMaskVoxels)+' described '+inttostr(lVox));
            exit;
         end;
         if UpCaseExt(lHdrName) = '.HDR' then
	    lHdrName := changefileext(lHdrName,'.img');
	 if (not GzExt(lHdrName) ) and  (FSize(lHdrName) < lMaskVoxels)  then begin
		 ShowMsg('The uncompressed image data should be at least '+inttostr(lMaskVoxels)+' bytes. '+lHdrName);
		 exit;
	 end;
	 result := true;

         if (lImageNumber < 0) or (lImageNumber > kMaxImages) then exit;
         gDataTypeRA[lImageNumber] := lHdr.NIFTIhdr.datatype;
	 gOffsetRA[lImageNumber] := lHdr.NIFTIhdr.vox_offset;
	 gScaleRA[lImageNumber] := lHdr.NIFTIhdr.scl_slope;
	 gInterceptRA[lImageNumber] := lHdr.NIFTIhdr.scl_inter;
end;

(*function CheckVoxelsGroup(var lG: TStrings; lMaskVoxels: integer):boolean;
var
	lC: integer;
	lHdrName : string;
begin
	 result := false;
	 if lG.count < 1 then exit;
	 for lC := 1 to lG.count do begin
		lHdrName:= lG[lC-1];
		result := CheckVoxels(lHdrName, lMaskVoxels,lC);
	 end;
end;*)
(*function CheckVoxelsGroup(var lG: TStrings; lMaskVoxels: integer):boolean;
var
	lC: integer;
	lHdrName : string;
begin
	 result := false;
	 if lG.count < 1 then exit;

	 for lC := 1 to lG.count do begin
		lHdrName:= lG[lC-1];
		if not CheckVoxels(lHdrName, lMaskVoxels,lC) then begin
                    if not fileexists (lHdrName) then
                     MainForm.NPMmsg('File not found "'+lHdrName+'"')

                    else
                     MainForm.NPMmsg('Problem with "'+lHdrName+'" expected '+inttostr(lMaskVoxels));
                    exit;
                end;
	 end;
         result := true;
end;*)
function SameTransform (A,B:TNIFTIhdr): boolean;
var
   lDim: integer;
begin
   result := false;
   for lDim := 0 to 3 do begin
       if A.srow_x[lDim] <> B.srow_x[lDim] then
          exit;
       if A.srow_y[lDim] <> B.srow_y[lDim] then
          exit;
       if A.srow_z[lDim] <> B.srow_z[lDim] then
          exit;
   end;
   result := true;
end;

function TransformTxt (A:TNIFTIhdr): string;
var
   lDim: integer;
begin
     result := '[';
     for lDim := 0 to 3 do
       result := result + ' '+floattostr(A.srow_x[lDim]);
     result := result + ';';
     for lDim := 0 to 3 do
       result := result + ' '+floattostr(A.srow_y[lDim]);
     result := result + ';';
     for lDim := 0 to 3 do
       result := result + ' '+floattostr(A.srow_z[lDim]);
     result := result + ']';
end;

function CheckVoxelsX(var lHdrNameIn : string; lMaskHdr: TMRIcroHdr; lImageNumber: integer):boolean;
var
	lHdr: TMRIcroHdr;
        lHdrName: string;
        lDim: integer;
begin
	 result := false;
         lHdrName := Filename4D(lHdrNameIn);
	 if not NIFTIhdr_LoadHdr(lHdrName,lHdr) then begin
                 NPMmsg('Unable to load image '+lHdrName+' Possible solution: make sure VAL file and images are in the same folder');
            exit;
         end;
         (*lVox := ComputeImageDataBytes8bpp(lHdr);
         lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
	 if lVox <> lMaskVoxels then begin
            NPMmsg('Voxels differ for '+lHdrName+' expected '+inttostr(lMaskVoxels)+' described '+inttostr(lVox));
            exit;
         end; *)
         for lDim := 1 to 3 do begin
            if (lHdr.NIFTIhdr.dim[lDim] <> lMaskHdr.NIFTIhdr.dim[lDim]) then begin
               NPMmsg('Dimension '+inttostr(lDim)+' of '+lHdrName+' does not match '+lMaskHdr.HdrFileName);
               exit;
            end;
         end;
         if (not lHdr.NIfTItransform) then
             NPMmsg('Warning: no spatial transform for '+lHdrName+' (Analyze not NIfTI). Please ensure images are coregistered.')
         else if (not lMaskHdr.NIfTItransform) then
             NPMmsg('Warning: no spatial transform for '+lMaskHdr.HdrFileName+' (Analyze not NIfTI). Please ensure images are coregistered.')
         else begin
              if not SameTransform (lHdr.NIFTIhdr, lMaskHdr.NIFTIhdr) then begin
                 NPMmsg('Warning: spatial transforms differ for '+lHdrName+' and '+lMaskHdr.HdrFileName);
                 NPMmsg(TransformTxt(lHdr.NIFTIhdr)+' <> '+ TransformTxt(lMaskHdr.NIFTIhdr));
              end;
         end;
	 (*if (lHdr.NIFTIhdr.bitpix <> 8) and (lHdr.NIFTIhdr.datatype <> kDT_FLOAT) and (lHdr.NIFTIhdr.datatype <> kDT_SIGNED_INT) then begin
		 showmessage('Error: This software can only read uncompressed images that are either 8-bit integer or 32-bit real precision.');
		 exit;
	 end; //beta  *)
         if UpCaseExt(lHdrName) = '.HDR' then
	    lHdrName := changefileext(lHdrName,'.img');
	 if (not GzExt(lHdrName) ) and  (FSize(lHdrName) < (lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]*lHdr.NIFTIhdr.dim[3]))  then begin
		 ShowMsg('The file size appears too small '+lHdrName);
		 exit;
	 end;
	 result := true;
          //gBitPixRA[lImageNumber] := lHdr.NIFTIhdr.bitpix;
          gDataTypeRA[lImageNumber] := lHdr.NIFTIhdr.datatype;
	 gOffsetRA[lImageNumber] := lHdr.NIFTIhdr.vox_offset;
	 gScaleRA[lImageNumber] := lHdr.NIFTIhdr.scl_slope;
	 gInterceptRA[lImageNumber] := lHdr.NIFTIhdr.scl_inter;
end;

function CheckVoxelsGroupX(var lG: TStrings; lMaskHdr: TMRIcroHdr):boolean;
var
	lC: integer;
	lHdrName : string;
begin
	 result := false;
	 if lG.count < 1 then exit;

	 for lC := 1 to lG.count do begin
		lHdrName:= lG[lC-1];
		if not CheckVoxelsX(lHdrName, lMaskHdr,lC) then begin
                    if not fileexists (lHdrName) then
                      NPMmsg('File not found "'+lHdrName+'". Possible solution: make sure VAL file and images are in the same folder')

                    else
                     NPMmsg('Problem with "'+lHdrName);
                    exit;
                end;
	 end;
         result := true;
end;

(*function CheckVoxelsGroupY(var lG: TStrings):boolean;
var
   lMaskHdr: TMRIcroHdr;
   lS: string;
begin
    result := false;
    if lG.count < 1 then exit;
    lS := lG[0];
    if not NIFTIhdr_LoadHdr(lS,lMaskHdr) then begin
       NPMmsg('Unable to load image '+lS);
       exit;
    end;
    result := CheckVoxelsGroupX(lG,lMaskHdr);
end; *)

function BPP (lDataType: integer): integer;
begin
     result := 0;
     case lDataType of
          kDT_UNSIGNED_CHAR: result := 1;
          kDT_SIGNED_SHORT: result := 2;    // signed short (16 bits/voxel)
          kDT_SIGNED_INT : result := 4; // signed int (32 bits/voxel)
          kDT_FLOAT  : result := 4; // float (32 bits/voxel)
          kDT_COMPLEX : result := 8; // complex (64 bits/voxel)
     end;
end;


function NIFTIhdr_HdrVolumes (lFilenameIn: string): integer;
var
   lFilename: string;
   lHdr: TMRIcroHdr;
begin
     result := 0;
     lFilename := lFilenameIn;
     if not NIFTIhdr_LoadHdr (lFilename, lHdr)then exit;
     result := lHdr.niftiHdr.dim[4];
end;

function FileExists4D (lFilename: string): boolean;
var
lBaseName: string; var lVol: integer;
begin
   FilenameVol4D (lFilename, lBasename,lVol);
   result := fileexists(lBasename);
end;

function FilenameVol4D (lFilename: string; var lBaseName: string; var lVol: integer): boolean;
//4D files end with the image index number c:\dir\filename:1
//returns true if 4D file (with lVol = volume), otherwise returns false with lvol = 1
var
   lLen,lP: integer;
   lNumStr: string;
begin
    lVol := 1;
    lBasename := lFilename;
    result := false;
    lLen := length(lFilename);
    if lLen < 1 then exit;
    lP := lLen;
    lNumStr := '';
    while (lP > 0) and (lFilename[lP] in ['0'..'9']) do begin
          lNumStr := lFilename[lP]+lNumStr;
          dec(lP);
    end;
    //showmessage(lNumStr + '*'+lFilename[lP]);
    if (lNumStr = '') or (lP < 2) or (lFilename[lP] <> ':')  then exit;
    lVol := strtoint(lNumStr);
    lLen := lP -1;
    lBasename := '';
    for lP := 1 to lLen do
        lBasename := lBasename + lFilename[lP];
    result := true;
end;

function Filename4D(lFilename: string): string;
var lVol: integer;
begin
   FilenameVol4D (lFilename, result,lVol);
end;

function Vol4D (lFilename: string): integer;
var
lBaseName: string;
begin
   FilenameVol4D (lFilename, lBasename,result);
end;

function Files4D (lFilename: string): boolean;
var
lBaseName: string; var lVol: integer;
begin
   result := FilenameVol4D (lFilename, lBasename,lVol);
end;

function CreateDecompressed4D(var lImageNames: TStrings): string;
//returns temp filename if all imagenames are a single compressed 4D datafile
//this means that a nii.gz file is only decompressed once, instead of once per volume*plank
var
   lP: integer;
   lFilename : string;
begin
     result := '';
     if lImageNames.Count < 2 then
        exit;
     if not Files4D(lImageNames.Strings[0]) then exit;
     lFilename := Filename4D(lImageNames.Strings[0]);
     if not Fileexists(lFilename) then
        exit;
     if not GzExt(lFilename) then
        exit; //not a decompressed file
     //see if single 4D image
     for lP := 2 to lImageNames.Count do
         if not Files4D(lImageNames.Strings[lP-1]) then
            exit;
     for lP := 2 to lImageNames.Count do
         if lFilename <> Filename4D(lImageNames.Strings[lP-1]) then
            exit;
     //find unique filename for extracted file
     result := lFilename +'.nii';
     while fileexists(result) do //make sure we do not overwrite anything
           result := lFilename +inttostr(random(9999))+'.nii';
     //unzip
     Gunzip(lFilename,result);
     //set image names to point to uncompressed volume
     for lP := 1 to lImageNames.Count do
         lImageNames.Strings[lP-1] := result +':'+inttostr(Vol4D(lImageNames.Strings[lP-1]) );
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



procedure SaveAsVOIorNIFTIcore (var lFilename: string;  var lNiftiHdr: TNIFTIHdr; var lImg: SingleP; lnVolIn,lImgBufferBPP: integer);
const
	kImgOffset = 352; //header is 348 bytes, but 352 is divisible by 8...
var
	lHdr: TNIFTIhdr;
	lBuff: ByteP;
	lF: File;
	lCompressedFilename,lExt: string;
	lnVol,lC,lFSize: integer;
        lImgBuffer: ByteP; lImgBufferItems{, lImgBufferBPP}: integer;
begin
  lnVol := lnVolIn;
  move(lNiftiHdr,lHdr,sizeof(lHdr));
  lImgBufferItems := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3];
  //lImgBufferBPP:= 4;
  lImgBuffer := ByteP(lImg);
  lExt := UpCaseExt(lFileName);
  if DiskFreeEx(lFilename) < (kImgOffset+(lImgBufferItems*lImgBufferBPP*lnVol)) then begin
        {$IFDEF GUI}

        case MsgDlg('Very little space on the selected drive. Attempt to save to this disk?', mtConfirmation,[mbYes, mbCancel], 0) of
            {$IFDEF FPC}mrCancel: exit; {$ELSE} id_Cancel: exit;{$ENDIF}
	end; //case
        {$ELSE}
        ShowMsg('Very little space on the selected drive. Data may be lost.');
        {$ENDIF}
  end;
  if  FileExistsEX(lFileName) then begin
      {$IFDEF GUI}
       if (ParamCount < 1) then begin
          case MsgDlg('Overwrite the file named '+lFileName+'?', mtConfirmation,[mbYes, mbCancel], 0) of   //MessageDlg
                {$IFDEF FPC}mrCancel: exit; {$ELSE} id_Cancel: exit;{$ENDIF}  //requires Uses Controls
	  end; //case
       end else begin
          NPMMsg('Warning: overwriting '+lFilename);

       end;
        {$ELSE}
        ShowMsg('Warning: overwriting '+lFilename);
        {$ENDIF}
        DeleteFile(lFilename);
  end; //file exists
  if (lExt='.VOI') then begin
	lHdr.intent_name[1] := 'B';//Binary
	lHdr.scl_slope := 1/kVOI8bit;
	lHdr.scl_inter := 0;
  end;
  if lnVol < 2 then begin
      lHdr.dim[0] := 3;//3D july2006
      lHdr.dim[4] := 1;//3D Aug 2007
      lnVol := 1;
  end else begin
      lHdr.dim[0] := 4;//3D july2006
      lHdr.dim[4] := lnVol;//3D july2006
  end;
  (*if not (lImgBufferItems = (lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3])) then begin //july2006
    lHdr.sform_code := 1;
	  WriteNiftiMatrix ( lHdr,  //must match MAGMA in nifti_hdr
	gBGImg.ScrnMM[1],0,0,(gBGImg.ScrnOri[1]-1)*-gBGImg.ScrnMM[1],
	0,gBGImg.ScrnMM[2],0,(gBGImg.ScrnOri[2]-1)*-gBGImg.ScrnMM[2],
	0,0,gBGImg.ScrnMM[3],(gBGImg.ScrnOri[3]-1)*-gBGImg.ScrnMM[3]);
  end;*)
  if not IsNifTiMagic(lHdr) then begin
    {lHdr.sform_code := 1;
	  WriteNiftiMatrix ( lHdr,  //must match MAGMA in nifti_hdr
	gBGImg.ScrnMM[1],0,0,(gBGImg.ScrnOri[1]-1)*-gBGImg.ScrnMM[1],
	0,gBGImg.ScrnMM[2],0,(gBGImg.ScrnOri[2]-1)*-gBGImg.ScrnMM[2],
	0,0,gBGImg.ScrnMM[3],(gBGImg.ScrnOri[3]-1)*-gBGImg.ScrnMM[3]);
   }
  end;
  case lImgBufferBPP of
	4: begin
		lHdr.bitpix := 32;
		lHdr.datatype := kDT_FLOAT;//note 32-bit integers saved internally as 32-bit float
	end;
	2: begin
		lHdr.bitpix := 16;
		lHdr.datatype := kDT_SIGNED_SHORT;
	end;
	1: begin
		lHdr.bitpix := 8;
		lHdr.datatype := kDT_UNSIGNED_CHAR;
		//lHdr.scl_inter := lHdr.WindowScaledMin;
		//lHdr.scl_slope := (lHdr.WindowScaledMax-lHdr.WindowScaledMin) /255;
	end;
	else begin
		showmsg('Error: Unsupported bytes per voxel: '+inttostr(lImgBufferBPP));
		exit;
	end;
  end;
  if (lExt='.IMG') or (lExt ='.HDR') then begin
	//done previously lHdr.magic := kNIFTI_MAGIC_SEPARATE_HDR;
	lHdr.vox_offset := 0;
	Filemode := 1;
	//next write header data as .hdr
	lFilename := changeFileExt(lFilename,'.hdr');
	AssignFile(lF, lFileName);
	Rewrite(lF,sizeof(TNIFTIhdr));
	BlockWrite(lF,lHdr, 1);
	CloseFile(lF);
	//next write image data as .img
	lFilename := changeFileExt(lFilename,'.img');
	AssignFile(lF, lFileName); {WIN}
	Rewrite(lF,lImgBufferItems*lImgBufferBPP*lnVol);
	BlockWrite(lF,lImgBuffer^,1);
	CloseFile(lF);
	Filemode := 2;
	exit;
  end; //separate header
  lHdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
  lHdr.vox_offset := kImgOffset;//352 bytes
  lFSize := kImgOffset+(lImgBufferItems*lImgBufferBPP*lnVol);
  getmem(lBuff,lFSize);
  move(lHdr,lBuff^,sizeof(lHdr));
  //Next: NIfTI 1.1 requires bytes 349..352 set to zero when no XML information
  lC := kImgOffset;
  lBuff^[lC-3] := 0;
  lBuff^[lC-2] := 0;
  lBuff^[lC-1] := 0;
  lBuff^[lC] := 0;
  lC := kImgOffset+1;
  move(lImgBuffer^[1],lBuff^[lC],lImgBufferItems*lImgBufferBPP*lnVol);
   if (lExt='.NII') then begin
	Filemode := 1;
	AssignFile(lF, lFileName);
	Rewrite(lF,lFSize);
	BlockWrite(lF,lBuff^,1);
	CloseFile(lF);
	Filemode := 2;
	exit;
  end; //uncompressed
  if (lExt<>'.VOI') then
     lCompressedFilename := changefileextX(lFilename,'.nii.gz')
  else
      lCompressedFilename := lFilename;


  GZipBuffer(lCompressedFilename,lBuff,lFSize,false);
  freemem(lBuff);
end;


procedure MakeHdr (var lBGHdr,lStatHdr: TniftiHdr);
//lIntent kNIFTI_INTENT_CHISQ  lIntent_p1 = DOF
//lIntent kNIFTI_INTENT_ZSCORE  no params
//lIntent kNIFTI_INTENT_TTEST lIntent_p1 = DOF
begin
    move(lBGHdr,lStatHdr,sizeof(TniftiHdr));
	with lStatHdr do begin
		magic :=kNIFTI_MAGIC_SEPARATE_HDR;
		bitpix := 32; //32-bit real data
		datatype := kDT_FLOAT;
		scl_slope:= 1;
		scl_inter:= 0;
                descrip[1] := 'X';//can not be npm
	end;
end;

function NIFTIhdr_SaveHdrImg (var lFilename: string; var lHdr: TNIFTIHdr; lAllowOverwrite,lSPM2,lSingleFile: boolean;var lImg: SingleP; lnVol: integer): boolean;
var
   lOutNameMod: string;
   lSPM2output: boolean;
begin
     lOutNameMod := lFilename;
     lOutNameMod := changefileextX(lOutNameMod,'.hdr');
     lSPM2output := lSPM2;
     //fx(lHdr.srow_x[3],lHdr.srow_y[3],lHdr.srow_z[3]);
     (*if not IsNifTiMagic(lHdr) then
        lSPM2output := true;*)
     if (lSingleFile) and (not lSPM2output) then begin
        lHdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
        lOutNameMod := changefileextX(lOutNameMod,'.nii.gz');
        //HACK lOutNameMod := changefileextX(lOutNameMod,'.nii');

     end else if (not lSPM2output) then
         lHdr.magic :=  kNIFTI_MAGIC_SEPARATE_HDR
     else //the nifti_hdr reader converts the Analyze to NIfTI, so we need to save as NIfTI with NPM
         lHdr.magic :=  kNIFTI_MAGIC_SEPARATE_HDR;
        //lHdr.magic :=  1984;
     SaveAsVOIorNIFTIcore (lOutNameMod, lHdr, lImg,lnVol,4);
end;

function NIFTIhdr_SaveHdrImg8 (var lFilename: string; var lHdr: TNIFTIHdr; lAllowOverwrite,lSPM2,lSingleFile: boolean;var lImg: ByteP; lnVol: integer): boolean;
var
   lOutNameMod: string;
begin
     lOutNameMod := lFilename;
     if  IsVOIExt (lOutNameMod) then begin
           lHdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
     end else begin
        lOutNameMod := changefileextX(lOutNameMod,'.hdr');
        if (lSingleFile) and (not lSPM2) then begin
           lHdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
           lOutNameMod := changefileextX(lOutNameMod,'.nii.gz');
        end else if (not lSPM2) then
            lHdr.magic :=  kNIFTI_MAGIC_SEPARATE_HDR
        else
            lHdr.magic :=  1984;
     end;
     SaveAsVOIorNIFTIcore (lOutNameMod, lHdr, SingleP(lImg),lnVol,1);
end;

end.