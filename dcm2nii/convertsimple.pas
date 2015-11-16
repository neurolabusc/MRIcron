unit convertsimple;
{$H+}
interface
uses
{$IFDEF FPC}gzio2, {$ENDIF}
define_types,SysUtils,dicomtypes,filename,nii_4dto3d,niftiutil,nii_orient, nii_crop,GraphicsMathLibrary,prefs;
//function ConvertSimple2NII  ({var} lInFilename, lOutDir: string; var lPrefs: TPrefs): boolean;
function FDF( lFName: string; var lDcm: DicomData {; var lByteSwap: boolean}): boolean;

implementation
uses dialogsx,dialogs_msg;

procedure ReadlnX (var F: TextFile; var lResult: string);
//Replicates Readln, but works for Unix files... Delphi 4's readln fails for non-MSDOS EOLs
var
   lCh: char;
begin
     lResult := '';
     while not Eof(F) do begin
           Read(F, lCh);
           if (lCh in [#10,#13]) then begin
              if lResult <> '' then begin
                 //Showmessage(lResult);
                 exit;
              end;
           end else
               lResult := lResult + lCh;
     end;
end; //ReadlnX

function ParseStr(lPattern: string; var lSample: string): integer;
begin

     result := Pos(lPattern,lSample);

     if result < 1 then
        exit;
     //Msg('*'+lPattern+'*'+inttostr(result)+'*'+lSample+'*');
     result := result+length(lPattern);//end of pattern
     lSample := trim(copy(lSample,result,length(lSample)+1-result));
end;

procedure ExtractFloats(lSample: string; var V1,V2,V3: double);
var
   lCh: char;
   lStr: string;
   lP,lL,lN: integer;
procedure RetireStr;
begin
     if lStr = '' then exit;
     inc(lN);
     case lN of
                     1: V1 := strtofloat(lStr);
                     2: V2 := strtofloat(lStr);
                     3: V3 := strtofloat(lStr);
                     else exit;
     end;
     lStr := '';
end;
begin
     V1:= 1; V2 := 1; V3 := 1;
     lL := length(lSample);
     if lL < 1 then
        exit;
     decimalseparator := '.';
     lP := 1;
     lN:= 0;
     lStr := '';
     while lP <= lL do begin
           lCh := lSample[lP];
           if lCh in ['-','.','0'..'9'] then
                lStr := lStr + lCh
           else
               retireStr;
           inc(lP);
     end;
     RetireStr;
end;

procedure ExtractFloats10x(lSample: string; var V1,V2,V3: double);
//cm to mm
begin
     ExtractFloats(lSample, V1,V2,V3);
     V1 := V1 * 10;
     V2 := V2 * 10;
     V3 := V3 * 10;
end;

procedure ExtractInts(lSample: string; var V1,V2,V3: integer);
var
   F1,F2,F3: double;
begin
     //Msg('*'+lSample+'*');
     ExtractFloats(lSample, F1,F2,F3);
     V1 := round(F1);
     V2 := round(F2);
     V3 := round(F3);
     //dcmMsg(inttostr(V1)+','+inttostr(V2)+','+inttostr(V3));

end;

procedure ReportHeader (lFormatName: string; var lDicomData: dicomdata);
const
     kCR = '; ';
begin
      dcmMsg (lFormatName
              +kCR+ ' Slice Number:'+inttostr(lDicomData.ImageNum)
              +kCR+ 'XYZ dim: ' +inttostr(lDicomData.XYZdim[1])+'/'+inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
              +kCR+ 'XYZ position: ' +floattostr(lDicomData.PatientPosX)+'/'+floattostr(lDicomData.PatientPosY)+'/'+floattostr(lDicomData.PatientPosZ)
              +kCR+'Data offset: ' +inttostr(lDicomData.ImageStart)
              +kCR+'XYZ mm: '+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
			  +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2)
              +kCR+'TR: '+floattostrf(lDicomData.TR,ffFixed,8,2)  );
end;

function CMFOV2MM( lCMFOV,lMatrix: double): double;
begin
    if lMatrix > 0 then
       result := lCMFOV*10 / lMatrix;
end;
(*Reads Varian FDF images (FDF = Flexible Data Format).
Varian MRI FDF reader http://www.mathworks.com/matlabcentral/fileexchange/7449
Multi_FDF_Opener.java - http://rsbweb.nih.gov/ij/plugins/multi-opener.html
 char  *spatial_rank = "2dfov";
	if 3dfov then matrix will have three elements
	if 3dfov then span will have three elements
 int    bigendian
 float  matrix[] = {512, 512};
 float  bits = 32;
 float  span[] = {4.000000, 4.000000};
 byte offset of image data is = file.length - xdim*ydim*zdim*bits/8;
 Appears only 32-bit float data... *)


function Deblank (lInStr: string): string;
var
   lLen,lPos: integer;
begin
     result := '';
     lLen := length(lInStr);
     if lLen < 1 then
        exit;
     for lPos := 1 to lLen do
         if lInStr[lPos] in ['=',',','[',']','+','-','.','\','~','/', '0'..'9','a'..'z','A'..'Z'] then
            result := result +lINStr[lPos];
end;

function FDF( lFName: string; var lDcm: DicomData): boolean;
//Note - some Varian scanners put in more white space between text than others...
// "float bits=" versus "float  bits="
//Therefore we deblank the data text to remove whitespace
VAR
   ltextfile: textfile;
   lLine: string;
   lFileSz,num: integer;
   junk1f,junk2f,junk3f: double;
   junk1,junk2: integer;
   lDone,lSliceNum: boolean;
begin
    result := false;
    lFileSz := FSize(lFName);
    if lFileSz < 1{not fileexists (lFName)} then begin
        dcmMsg('Can not find file '+lFName);
        exit;
    end;
    FileMode := 0;  { Set file access to read only }
    AssignFile(ltextfile, lFName);
    Reset(ltextfile);
    lDone := false;
    num := 1;
    lSliceNum := false;
    repeat
          readlnx(ltextfile,lLine);
          //lLine := trim(lLine);//remove leading/following characters
          lLine := deblank(lLine);//remove leading/following characters
          //dcmdcmdcmdcmMsg(lLine);
          if ParseStr('floatbits=', lLine) > 0 then
             ExtractInts(lLine, lDcm.Allocbits_per_pixel ,junk1,junk2);
          if ParseStr('intslice_no', lLine) > 0 then begin
             lSliceNum := true;
             ExtractInts(lLine, lDcm.ImageNum  ,junk1,junk2);
          end;
          if ParseStr('floatlocation[]=', lLine) > 0 then
             ExtractFloats10x(lLine, lDcm.PatientPosX,lDcm.PatientPosY,lDcm.PatientPosZ);
          if ParseStr('floatmatrix[]=', lLine) > 0 then
             ExtractInts(lLine, lDcm.XYZdim[1],lDcm.XYZdim[2],lDcm.XYZdim[3]);
          if ParseStr('floatspan[]=', lLine) > 0 then
             ExtractFloats(lLine, lDCm.XYZmm[1],lDcm.XYZmm[2],lDcm.XYZmm[3]);
          if ParseStr('floatTR=', lLine) > 0 then begin
             ExtractFloats(lLine, junk1f,junk2f,junk3f);
             lDcm.TR := junk1f/1000; //convert TR in msec to time in sec
          end;
          {if strmatch('intbigendian', line) then
             machineformat = 'ieee-le'; % New Linux-based}
          num := num + 1;
          if num > 41 then
             lDone := true;
    until lDone;
    if lDcm.Allocbits_per_pixel = 32 then begin
       result := true;
       lDcm.FloatData := true;
    end else begin
        dcmMsg('Unsupported datatype: '+inttostr( lDcm.Allocbits_per_pixel)+ 'bits per pixel '+lFName);
    end;
    if not lSliceNum then begin
       ExtractInts(lFName, lDcm.ImageNum ,junk1,junk2);
    end;
    //next - convert field of view in cm to voxel size in mm...
    for num := 1 to 3 do
        lDCm.XYZmm[num] := CMFOV2MM( lDCm.XYZmm[num],lDcm.XYZdim[num]);
    lDcm.Little_Endian := 0;
    lDcm.ImageStart :=  lFileSz - (lDcm.XYZdim[1]*lDcm.XYZdim[2]*lDcm.XYZdim[3]*(round(lDcm.Allocbits_per_pixel/8)));
    if lDcm.ImageStart < 10 then
       result := false;
    if result then
       ReportHeader('Varian FDF',lDcm);
    CloseFile(ltextfile);
    FileMode := 2;
end;


(*function Raw2NIfTI(lHdrName: string; var lHdr: TNIFTIhdr;  lPrefs: TPrefs; lByteSwap: boolean): boolean;
//like ChangeNIfTISubformat except we use the passed lHdrdata...
var
   lImgBuffer: byteP;
   lImgOffset: integer;
   lOutImgName: string;
   //lByteSwap: boolean;
begin
   result := false;
   if not NIFTIhdr_LoadImgRaw  (false,lHdrName, lHdr, lImgBuffer, lImgOffset,lByteSwap) then  exit;
   dcmMsg('Converting to NIfTI '+lHdrName);
   lOutImgName := ChangeFilePrefix (lHdrName,'c');
   if lPrefs.CustomRename then
      CustomFilename(lOutImgName);
   if  SaveNIfTICore (lOutImgName, lImgBuffer, lImgOffset+1, lHdr, lPrefs,lByteSwap) ='' then exit;
   Freemem(lImgBuffer);
   result := true; //11/2007
   ExitCode := 0;
end;


function ConvertSimple2NII  ({var} lInFilename , lOutDir: string; var lPrefs: TPrefs): boolean;
//this is simple, but does not stack 2d slices into 3d volumes...
var
   lDcm: DicomData;
   lHdr: TNIFTIhdr;
    lBigEndian: boolean;
begin
    result := false;
     NIFTIhdr_ClearHdr(lHdr);
    dcmMsg('WARNING: no image orientation matrix for '+lInFilename);
    if not FDF (lInFilename,lDcm) then exit;
    DICOM2AnzHdr (lHdr, false,lInFilename,lDcm);
    //Raw2NIfTI(lInFilename,lHdr,lPrefs,true);
end; *)

end.