unit dicomhdr;
{$H+}
//Simple dicom to nifti translator
interface
uses
{$IFNDEF FPC}Controls, {$ENDIF}
   SysUtils,define_types,classes,nifti_hdr,GraphicsMathLibrary, nifti_types;
function NIFTIhdr_LoadDCM (var lFilename: string; var lHdr: TMRIcroHdr): boolean;

type
    kDICOMStr = String[32];
    DICOMdata = record
    XYZdim: array [1..4] of integer;
    XYZori: array [1..3] of integer;
    XYZmm: array [1..3] of double;
    Orient: array [1..6] of double;
   Float,file4D: boolean;
   PatientPosX,PatientPosY,PatientPosZ,AngulationAP,AngulationFH,AngulationRL: double;
   TE, TR,IntenScale,IntenIntercept,location{,DTIv1,DTIv2,DTIv3}: single;
   {Bval,}SlicesPer3DVol,SiemensInterleaved {0=no,1=yes,2=not defined},SiemensSlices,SiemensMosaicX,SiemensMosaicY,
   nDTIdir,AcquNum,ImageNum,SeriesNum,ImageStart,little_endian,Allocbits_per_pixel,SamplesPerPixel,
   PatientIDint,CSAImageHeaderInfoPos,CSAImageHeaderInfoSz,ManufacturerID: integer;
   PatientPos,PatientName,ProtocolName,StudyDate,StudyTime,PhilipsSliceOrient,PhaseEncoding: kDICOMStr;
   Filename: string[255];
end;

implementation

uses dialogsx;

procedure Msg(lStr: string);
begin
     ShowMsg(lStr);
end;

procedure clear_dicom_data (var lDicomdata:Dicomdata);
var
	lI: integer;
begin
  with lDicomData do begin
           lDicomData.CSAImageHeaderInfoPos  := 0;
           lDicomData.CSAImageHeaderInfoSz := 0;
	   for lI := 1 to 6 do
		Orient[lI] := 0;
	   PatientIDInt := 0;
       ManufacturerID := 0;
       AngulationFH := 0;
       AngulationRL := 0;
       AngulationAP := 0;
       nDTIdir := 0;
       PhilipsSliceOrient := 'NA';
       PhaseEncoding := 'NA';
       PatientPos := 'NA';

       file4D := false;
       PatientName := 'NO NAME';
       StudyDate := '';
       StudyTime := '';
       TR := 0;
       TE := 0;
        Float := false;
		ImageNum := 0;
		SlicesPer3DVol := 0;
		SiemensInterleaved := 2; //0=no,1=yes,2=undefined
        SiemensSlices := 0;
        SiemensMosaicX := 1;
        SiemensMosaicY := 1;
        IntenScale := 1;
        IntenIntercept := 0;
        SeriesNum := 1;
        AcquNum := 0;
        ImageNum := 1;
        SamplesPerPixel := 1;
        XYZmm[1] := 1;
        XYZmm[2] := 1;
        XYZmm[3] := 1;
        XYZdim[1] := 1;
        XYZdim[2] := 1;
        XYZdim[3] := 1;
        XYZdim[4] := 1;
        lDicomData.XYZori[1] := 0;
        lDicomData.XYZori[2] := 0;
        lDicomData.XYZori[3] := 0;
        ImageStart := 0;
        Little_Endian := 0;
        Allocbits_per_pixel := 16;//bits
        Location:=0;
        PatientPosX := 0;//1392
        PatientPosY := 0;//1392
        PatientPosZ := 0;//1392
  end;
end;

function NIFTIhdr_LoadDCM (var lFilename: string; var lHdr: TMRIcroHdr): boolean;
var
   lDICOMdata: DICOMdata;
const
     kMaxBuf = (256*256)-1; //bytes
     kMax16bit = (256*256)-1;
     kImageType = $0008+($0008 shl 16 );
     kStudyDate = $0008+($0020 shl 16 );
     kStudyTime = $0008+($0030 shl 16 );
     kPatientName = $0010+($0010 shl 16 );
     kSeq = $0018+($0020 shl 16 );
     kZThick =  $0018+($0050 shl 16 );
     kTR =  $0018+($0080 shl 16 );
     kTE =  $0018+($0081 shl 16 );
     kEchoNum =  $0018+($0086 shl 16 );
     kZSpacing =  $0018+($0088 shl 16 );
     kProtocolName =  $0018+($1030shl 16 );
     kPatientPos = $0018+($5100 shl 16 );
     kSeriesNum = $0020+($0011 shl 16 );
     kAcquNum = $0020+($0012 shl 16 );
     kImageNum = $0020+($0013 shl 16 );
     kOrientation = $0020+($0037 shl 16 );
     kLocation = $0020+($1041 shl 16 );
     kDim3 = $0028+($0008 shl 16 );
     kDim2 = $0028+($0010 shl 16 );
     kDim1 = $0028+($0011 shl 16 );
     kXYSpacing =  $0028+($0030 shl 16 );
     kPosition = $0020+($0032 shl 16 );
     knVol =  $0020+($0105 shl 16 );
     kAlloc = $0028+($0100 shl 16 );
     kIntercept = $0028+($1052 shl 16 );
     kSlope = $0028+($1053 shl 16 );
     kCSAImageHeaderInfo =  $0029+($1010 shl 16 );
     kSlicesPer3DVol =  $2001+($1018 shl 16 );
     kTransferSyntax = $0002+($0010 shl 16);
     kImageStart = $7FE0+($0010 shl 16 );
     kMaxFloats = 6;
var
  vr : array [1..2] of Char;
   lByteRA: Bytep;
   lFloatRA: array [1..kMaxFloats] of double;
   lBufferSz,lPos,lFileSz,lBuffStart: integer;
   lInFile: file;
   lBufferError, lDoEndianSwap: boolean;
procedure Str2FloatNum ( lStr: string; lnFloats: integer);
var
    lFStr: string;
    lP,lnF: integer;
begin
     if (length(lStr) < 1) or (lnFloats < 1) or (lnFloats > kMaxFloats) then
        exit;
     for lnF := 1 to lnFloats do
         lFloatRA[lnF] := 1;
     lStr := lStr + ' '; //terminator
     lFStr := '';
     lP := 1;
     lnF:= 0;
     while lP <= length(lStr) do begin
           if lStr[lP]  in ['+','-','0'..'9','.','e','E'] then
              lFStr := lFStr + lStr[lP]
           else if (lFStr <> '') then begin
                inc(lnF);
                try
                   lFloatRA[lnF] := strtofloat(lFStr);
                except on EConvertError do
                       lFloatRA[lnF] := 1;
                end;//except
                if lnF = lnFloats then exit;
                lFStr := '';
           end;
           inc(lP);
     end;
end; //function Str2Float

function GetByte (lFilePos: integer): byte;
var
   lBufPos: integer;
begin
    //the following error checking slows down reads a lot!
    //a simpler alternative would be to make the buffer size the same size as the entire image...
    //the current strategy saves memory and is faster for large images with small headers
    if lFilepos > lFileSz then begin
        lBufferError := true;
        result := 0;
        exit;
    end;
    lBufPos := lFilepos - lBuffStart+1;
    if (lBufPos > lBufferSz) or (lBufPos < 1) then begin //reload buffer
       if lFilePos+kMaxBuf > lFileSz then
          lBufferSz := lFileSz - (lFilePos)
       else
           lBufferSz := kMaxBuf; //read remaining
       AssignFile(lInFile, lFileName);
       FileMode := 0;  //Set file access to read only
       Reset(lInFile, 1);
       seek(lInFile,lFilePos);
       BlockRead(lInFile, lByteRA^[1], lBufferSz);
       CloseFile(lInFile);
       FileMode := 2;
       lBuffStart := lFilePos;
       lBufPos := 1;
    end;
    result := lByteRA^[lBufPos];
end;

function ReadInt2x2: integer;
begin
    if lDicomData.little_endian = 0 then
        result := GetByte(lPos+1)+(GetByte(lPos) shl 8)+(GetByte(lPos+3) shl 16)+(GetByte(lPos+2) shl 24)
    else
        result := GetByte(lPos)+(GetByte(lPos+1) shl 8)+(GetByte(lPos+2) shl 16)+(GetByte(lPos+3) shl 24);
    inc(lPos,4);
end; //function Read4

function ReadInt4: integer;
begin
    if lDicomData.little_endian = 0 then
        result := GetByte(lPos+3)+(GetByte(lPos+2) shl 8)+(GetByte(lPos+1) shl 16)+(GetByte(lPos) shl 24)
    else
        result := GetByte(lPos)+(GetByte(lPos+1) shl 8)+(GetByte(lPos+2) shl 16)+(GetByte(lPos+3) shl 24);
    inc(lPos,4);
end; //function Read4

procedure ReadGroupElementLength(var lGroupElement,lLength: integer);
begin
     lGroupElement := ReadInt2x2;
     vr[1] := chr(GetByte(lPos));
     vr[2] := chr(GetByte(lPos+1));
    if (lDoEndianSwap) and ((lGroupElement and $FFFF) <> 0002) then begin
       //msg('SWAPPING');
       lDoEndianSwap := false;
       lDicomData.little_endian := 0;
    end;
    if vr[2] < 'A' then begin //implicit vr with 32-bit length
        lLength := ReadInt4;
        exit;
     end;
     if (vr = 'UN') {2/2008} or (vr = 'OB') or (vr = 'OW') or (vr = 'SQ') then begin  {explicit VR with 32-bit length}
          lPos := lPos + 4;  {skip 2 byte string and 2 reserved bytes = 4 bytes = 2 words}
          lLength := ReadInt4;//Ord4(buf[lPos]) + $100 * (buf[lPos+1] + $100 * (buf[lPos+2] + $100 * buf[lPos+3]))
     end else begin {explicit VR with 16-bit length}
         if lDicomData.little_endian = 0  then
             lLength := (GetByte(lPos+3))+(GetByte(lPos+2) shl 8)
         else
             lLength := (GetByte(lPos+2))+(GetByte(lPos+3) shl 8);//GetLength := Ord4(buf[i+2]) + $100 * (buf[i+3]);
         lPos := lPos + 4;  {skip 2 byte string and 2 length bytes = 4 bytes = 2 words}

     end;

end; //procedure ReadGroupElementLength

function DCMStr(lBytes: integer): string;
var
   lC: integer;
begin
    result := '';
    if lBytes < 1 then
       exit;
    for lC := lPos to (lPos+(lBytes-1)) do
        result := result + char(GetByte(lC));
    for lC := 1 to lBytes do
        if result[lC]  in ['+','-','/','\',' ','0'..'9','a'..'z','A'..'Z','.'] then
        else
           result[lC] := ' ';
end; //function DCMStr

function DCMStr2Int (lBytes: integer): integer;
var lErr: integer;
    lStr: string;
begin
     lStr := DCMStr(lBytes);
     Val(lStr,result,lErr);
end; //function DCMStr2Int

procedure DCMStr2FloatNum (lBytes,lnFloats: integer);
begin
     Str2FloatNum (DCMStr(lBytes), lnFloats);
end; //function DCMStr2Float

function DCMStr2Float (lBytes: integer): single;
begin
     DCMStr2FloatNum (lBytes,1);
     result := lFloatRA[1];
end; //function DCMStr2Float

procedure DCMStr2Float2 (lBytes: integer; var lF1,lF2: double);
begin
     DCMStr2FloatNum (lBytes,3);
     lF1 := lFloatRA[1];
     lF2 := lFloatRA[2];
end; //function DCMStr2Float2

procedure DCMStr2Float3 (lBytes: integer; var lF1,lF2,lF3: double);
begin
     DCMStr2FloatNum (lBytes,3);
     lF1 := lFloatRA[1];
     lF2 := lFloatRA[2];
     lF3 := lFloatRA[3];
end; //function DCMStr2Float3

procedure DCMStr2Float6 (lBytes: integer; var lF1,lF2,lF3,lF4,lF5,lF6: double);
begin
     DCMStr2FloatNum (lBytes,6);
     lF1 := lFloatRA[1];
     lF2 := lFloatRA[2];
     lF3 := lFloatRA[3];
     lF4 := lFloatRA[4];
     lF5 := lFloatRA[5];
     lF6 := lFloatRA[6];
end; //function DCMStr2Float6

function DCMint (lBytes: integer): integer; //read 16 bit short integer
begin
  if (lDicomData.little_endian = 0) then begin
     if lBytes <= 2 then
       result := GetByte(lPos+1)+(GetByte(lPos) shl 8) //shortint vs word?
    else
        result := GetByte(lPos+3)+(GetByte(lPos+2) shl 8)+(GetByte(lPos+1) shl 16)+(GetByte(lPos) shl 24);; //byte order??

     exit;
  end;
     if lBytes <= 2 then
        result := GetByte(lPos)+(GetByte(lPos+1) shl 8) //shortint vs word?
     else
         result := GetByte(lPos)+(GetByte(lPos+1) shl 8)+(GetByte(lPos+2) shl 16)+(GetByte(lPos+3) shl 24);; //byte order??
end; //function DCMint
var
   lTempStr,lStr: string;
   lOffset,lTemp,lGroupElement,lLength,lEchoNum,lnVol: integer;
   lResearchMode: boolean;
   lThick: double;
begin //function fast_read_dicom_data
     lOffset := 128;
     lnVol := 1;
     lEchoNum := 1;
     lDoEndianSwap := false;
     lThick := 0;
     clear_dicom_data(lDicomData);
     lDicomData.little_endian := 1;
     result := false;
     lResearchMode := false;
     lBufferError := false;
     lFileSz := FSize(lFilename);
     lBufferSz := lFileSz-lOffset;
     if lBufferSz < 512 then begin
        //showmessage('Error: File too small '+lFilename);
        exit;
     end;
     if lBufferSz > kMaxBuf then
        lBufferSz := kMaxBuf;
     GetMem(lByteRA,kMaxBuf);
     lBufferSz := lBufferSz;
     AssignFile(lInFile, lFileName);
     FileMode := 0;  //Set file access to read only
     Reset(lInFile, 1);
     seek(lInFile,lOffset);
     BlockRead(lInFile, lByteRA^[1], lBufferSz);
     CloseFile(lInFile);
     FileMode := 2;
     lBuffStart := lOffset;
     lPos := lOffset;
     if lOffset = 128 then begin //DICOM files start with DICM at 128, Siemens shadow headers do not
        if DCMStr(4) <> 'DICM' then begin
           //Msg(DCMStr(4)+ ' <> DICM');
           FreeMem(lByteRA);
           exit;
        end;
        lPos := lOffset + 4;//DICM read
     end;//Offset = 128
     //next check VR
     if not( chr(GetByte(lPos+4)) in ['A'..'Z']) or not( chr(GetByte(lPos+5)) in ['A'..'Z']) then
        Msg('implicit VR untested');
     //next check Endian
     lTemp := lPos;
     ReadGroupElementLength(lGroupElement,lLength);
     //if lLength > kMax16bit then
     //   Msg('ByteSwapped');
     lPos := lTemp;
     //end VR check

     while (lDICOMData.imagestart = 0) and (not lBufferError) do begin
         ReadGroupElementLength(lGroupElement,lLength);
         //if (lGroupElement and $FF) > $18 then
         //   msg(VR+' '+inttohex(lGroupElement and $FFFF,4)+' '+inttohex((lGroupElement shr 16) and $FFFF,4)+'  '+inttostr(lLength));
         case lGroupElement of
                kTransferSyntax: begin
                     lTempStr := (DCMStr(lLength));
                     if (length(lTempStr) >= 19) and (lTempStr[19] = '2') then begin
                        //lDicomData.little_endian := 0;
                        lDoEndianSwap := true;
                        //msg('Terror');
                     end;
                 end;
                 kImageType : begin
                            lTempStr := DCMStr(lLength);
                            //read last word - ver\mosaic -> MOSAIC
                            lStr := '';
                            lTemp := length(lTempStr);
                            while (lTemp > 0) and (lTempStr[lTemp] in ['a'..'z','A'..'Z']) do begin
                                  lStr := upcase(lTempStr[lTemp])+lStr;
                                  dec(lTemp);
                            end;
                            if lStr = 'MOSAIC' then
                               lDicomData.SiemensMosaicX := 2; //we need to read numaris for details...
                 end;
                 kStudyDate: lDicomData.StudyDate := DCMStr(lLength);
                 kStudyTime : lDicomData.StudyTime := DCMStr(lLength);
                 kPatientName : lDicomData.PatientName := DCMStr(lLength);
                 kProtocolName : lDicomData.ProtocolName :=DCMStr(lLength);
                 kPatientPos : lDicomData.PatientPos :=DCMStr(lLength); //should be HFS for Siemens = Head First Supine
                 kSeriesNum : lDicomData.SeriesNum := DCMStr2Int(lLength);
                 kAcquNum : lDicomData.AcquNum := DCMStr2Int(lLength);
                 kSeq: begin
                     if DCMStr(lLength) = 'RM' then
                        lResearchMode := True;
                 end;
                 kImageNum : lDicomData.ImageNum := DCMStr2Int(lLength);
                 kDim3 :lDicomData.XYZdim[3] := DCMStr2Int(lLength);
                 kDim2 : lDicomData.XYZdim[2] := DCMint (lLength);
                 kDim1 : lDicomData.XYZdim[1] := DCMint (lLength);
                 kLocation : lDICOMData.Location := DCMStr2Float(lLength);
                 kAlloc: lDicomData.Allocbits_per_pixel := DCMint (lLength);
                 kTR : lDicomData.TR := DCMStr2Float(lLength);
                 kTE: lDicomData.TE := DCMStr2Float(lLength);
                 kEchoNum: lEchoNum := round (DCMStr2Float(lLength));
                 kSlope : lDICOMData.IntenScale := DCMStr2Float(lLength);
                 kIntercept : lDICOMData.IntenIntercept := DCMStr2Float(lLength);
                 kOrientation : DCMStr2Float6(lLength, lDicomData.Orient[1], lDicomData.Orient[2],lDicomData.Orient[3],lDicomData.Orient[4], lDicomData.Orient[5],lDicomData.Orient[6]);
                 kPosition : DCMStr2Float3 (lLength,lDicomData.PatientPosX, lDicomData.PatientPosY,lDicomData.PatientPosZ);
                 knVol: lnVol := round (DCMStr2Float(lLength));
                 kZThick: begin lThick := DCMStr2Float(lLength); lDICOMData.XYZmm[3] := lThick; end;//used differently by manufacturers
                 kZSpacing: begin lDICOMData.XYZmm[3] := DCMStr2Float(lLength);
                                 if  (lThick/2) > lDICOMdata.XYZmm[3]  then
                                     lDICOMdata.XYZmm[3] := lDICOMdata.XYZmm[3] + lThick
                           end; //used different by different manufacturers
                 kXYSpacing: DCMStr2Float2 (lLength,  lDICOMdata.XYZmm[2],  lDICOMdata.XYZmm[1]);
                 (*kCSAImageHeaderInfo: begin //order ICE,Acq,Num,Vector
                   lDICOMdata.CSAImageHeaderInfoPos := lPos;
                   lDICOMdata.CSAImageHeaderInfoSz :=  lLength;
                                       end;  *)
                 kSlicesPer3DVol: lDICOMData.SlicesPer3DVol := DCMint (lLength);
                 kImageStart: lDICOMData.ImageStart := lPos ; //-1 as indexed from 0.. not 1..

         end; //Case lGroupElement
         //Msg(VR+inttohex(lGroupElement and kMax16bit,4) +':'+inttohex( lGroupElement shr 16,4)+'  '+inttostr(lLength)+'@'+inttostr(lPos) );
         lPos := lPos + (lLength);
     end; //while imagestart=0 and not error

     //clean up
     if (lDicomData.SiemensMosaicX > 1) then
  	lDicomData.AcquNum := 1;
     if (lEchoNum > 1) and (lEchoNum < 16) then
        lDicomData.AcquNum := lDicomData.AcquNum + (100*lEchoNum);
     if lResearchMode then
        lDicomData.SeriesNum   := lDicomData.SeriesNum + 100;
     if (lDICOMData.SlicesPer3DVol > 0) and (lnVol > 1) and (lDicomdata.XYZdim[3] > 1) and (lDicomData.SlicesPer3DVol > 0)and ((lDicomdata.XYZdim[3] mod lDicomData.SlicesPer3DVol) = 0)  then
        lDICOMdata.File4D := true;
     if not lBufferError then
        result := true;
     FreeMem(lByteRA);
     if result then begin
          lHdr.HdrFileName:= lFilename;
	  lHdr.ImgFileName:= lFilename;
          lHdr.NIfTItransform := false;//Analyze
          case lDicomData.Allocbits_per_pixel of
               8: lHdr.NiftiHdr.datatype := kDT_UNSIGNED_CHAR;
               16: lHdr.NiftiHdr.datatype := kDT_SIGNED_SHORT;
               32: begin
                   if lDicomdata.Float then
                      lHdr.NiftiHdr.datatype := kDT_SIGNED_INT
                   else
                      lHdr.NiftiHdr.datatype :=  kDT_FLOAT;     // float (32 bits/voxel)
                   end;
               else begin
                   Msg('Unsupported DICOM bit-depth : '+inttostr(lDicomData.Allocbits_per_pixel) );
                   result := false;
               end;
         end;
          lHdr.NIFTIhdr.vox_offset := lDicomData.ImageStart;
          lHdr.NIFTIhdr.bitpix := lDicomData.Allocbits_per_pixel;
          lHdr.NIFTIhdr.pixdim[1] := lDicomdata.XYZmm[1];
          lHdr.NIFTIhdr.pixdim[2] := lDicomdata.XYZmm[2];
          lHdr.NIFTIhdr.pixdim[3] := lDicomdata.XYZmm[3];
	  NIFTIhdr_SetIdentityMatrix(lHdr);
          lHdr.NIFTIhdr.dim[1] := lDicomdata.XYZdim[1];
          lHdr.NIFTIhdr.dim[2] := lDicomdata.XYZdim[2];
          lHdr.NIFTIhdr.dim[3] := lDicomdata.XYZdim[3];
          lHdr.NIFTIhdr.dim[4] := lDicomdata.XYZdim[4];
          if lHdr.NIFTIhdr.dim[4] < 2 then
             lHdr.NIFTIhdr.dim[0] := 3
          else
              lHdr.NIFTIhdr.dim[0] := 4;
	  lHdr.NIFTIhdr.qform_code := kNIFTI_XFORM_UNKNOWN;
	  lHdr.NIFTIhdr.sform_code := kNIFTI_XFORM_UNKNOWN;
          //test - input estimated orientation matrix
          lHdr.NIFTIhdr.sform_code := kNIFTI_XFORM_SCANNER_ANAT ;
          lHdr.NIFTIhdr.srow_x[0] := lHdr.NIFTIhdr.pixdim[1];
          lHdr.NIFTIhdr.srow_y[1] := lHdr.NIFTIhdr.pixdim[2];
          lHdr.NIFTIhdr.srow_z[2] := lHdr.NIFTIhdr.pixdim[3];
			lHdr.NIFTIhdr.srow_x[3] := (lHdr.NIFTIhdr.dim[1] /2)*-lHdr.NIFTIhdr.pixdim[1];
			lHdr.NIFTIhdr.srow_y[3] := (lHdr.NIFTIhdr.dim[2] /2)*-lHdr.NIFTIhdr.pixdim[2];
			lHdr.NIFTIhdr.srow_z[3] := (lHdr.NIFTIhdr.dim[3] /2)*-lHdr.NIFTIhdr.pixdim[3];
	lHdr.Mat:= Matrix3D(
		lHdr.NIFTIhdr.srow_x[0],lHdr.NIFTIhdr.srow_x[1],lHdr.NIFTIhdr.srow_x[2],lHdr.NIFTIhdr.srow_x[3],      // 3D "graphics" matrix
		lHdr.NIFTIhdr.srow_y[0],lHdr.NIFTIhdr.srow_y[1],lHdr.NIFTIhdr.srow_y[2],lHdr.NIFTIhdr.srow_y[3],      // 3D "graphics" matrix
		lHdr.NIFTIhdr.srow_z[0],lHdr.NIFTIhdr.srow_z[1],lHdr.NIFTIhdr.srow_z[2],lHdr.NIFTIhdr.srow_z[3],      // 3D "graphics" matrix
		0,0,0,1);	  //Warning: some of the NIFTI float values that do exist as integer values in Analyze may have bizarre values like +INF, -INF, NaN
	  lHdr.NIFTIhdr.toffset := 0;
	  lHdr.NIFTIhdr.intent_code := kNIFTI_INTENT_NONE;
	  lHdr.NIFTIhdr.dim_info := kNIFTI_SLICE_SEQ_UNKNOWN + (kNIFTI_SLICE_SEQ_UNKNOWN shl 2) + (kNIFTI_SLICE_SEQ_UNKNOWN shl 4); //Freq, Phase and Slie all unknown
	  lHdr.NIFTIhdr.xyzt_units := kNIFTI_UNITS_UNKNOWN;
	  lHdr.NIFTIhdr.slice_duration := 0; //avoid +inf/-inf, NaN
	  lHdr.NIFTIhdr.intent_p1 := 0;  //avoid +inf/-inf, NaN
	  lHdr.NIFTIhdr.intent_p2 := 0;  //avoid +inf/-inf, NaN
	  lHdr.NIFTIhdr.intent_p3 := 0;  //avoid +inf/-inf, NaN
	  lHdr.NIFTIhdr.pixdim[0] := 1; //QFactor should be 1 or -1
          lHdr.DiskDataNativeEndian := odd(lDicomData.little_endian);
          lHdr.NIFTIHdr.magic := kNIFTI_MAGIC_DCM;
     end;
end; //function NIFTIhdr_LoadDCM


end.
