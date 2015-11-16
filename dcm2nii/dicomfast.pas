unit dicomfast;
{$H+}
//this is x3 faster than the dicomcompat routines, but only works with well-behaved Explicit Little-Endian DICOM
interface
//0008,0070,ManufacturerID
//Next values for Philips
//Philips - must set 2001,105F length to 8 - allows reading of stack sequence
//  2005,1071. Philips AP angulation
//  2005,1072. Philips RL angulation
//  2005,1073. Philips FH angulation
//2001,100B Philips slice orientation (TRANSVERSAL, AXIAL, SAGITTAL)
// Next values for GE
//0019,10bb (or 0019,a0bb)= X diffusion direction
//0019,10bc (or 0019,a0bc)= Y diffusion direction
//0019,10bd (or 0019,a0bd)= Z diffusion direction
//0018,1312 = phase encoding.


uses
{$IFNDEF FPC}Controls, {$ENDIF}
   SysUtils,define_types,classes,dicomtypes,dialogs_msg;

function fast_read_dicom_data(var lDICOMdata: DICOMdata; lOffset: integer;  var lFileName: string): boolean; //x3 faster!


implementation
uses dialogsx;
{$DEFINE ANON}

function fast_read_dicom_data(var lDICOMdata: DICOMdata; lOffset: integer; var lFileName: string): boolean;
label
999;
const
     kMaxBuf = (256*256)-1; //bytes
     kMax16bit = (256*256)-1;
     kImageType = $0008+($0008 shl 16 );
     kStudyDate = $0008+($0020 shl 16 );
     kSeriesDate = $0008+($0021 shl 16 );
     kAcqDate = $0008+($0022 shl 16 );
     kCreateDate =  $0008+($0012 shl 16 );
     kStudyTime = $0008+($0030 shl 16 );
     kPatientName = $0010+($0010 shl 16 );
     {$IFDEF ANON} //position and lengths of tags to anonymize
     kPatientID = $0010+($0020 shl 16 );
     kPatientDOB = $0010+($0030 shl 16 );
     kPatientSex = $0010+($0040 shl 16 );
     kPatientAge = $0010+($1010 shl 16 );
     kPatientWt = $0010+($1030 shl 16 );
     {$ENDIF}
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
  lFailure: boolean;
   lByteRA: Bytep;
   lFloatRA: array [1..kMaxFloats] of double;
   lBufferSz,lPos,lFileSz,lBuffStart: integer;
   lInFile: file;
   lBufferError: boolean;
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
                //if lnFloats = 6 then showmessage(lFStr);
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
       if (lFilePos < 0) or (lFilePos > lFileSz) then begin
          lBuffStart := lFilePos;
          lBufPos := 1;
          dcmMsg(lFileName);
          lFailure := true;
          dcmMsg('Error: buffer overrun in DICOM read.');
          exit;
       end;
       if lFilePos+kMaxBuf > lFileSz then
          lBufferSz := lFileSz - (lFilePos)
       else
           lBufferSz := kMaxBuf; //read remaining
       //fx(lFilepos,kMaxBuf);
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
     lGroupElement := ReadInt4;
     vr[1] := chr(GetByte(lPos));
     vr[2] := chr(GetByte(lPos+1));
     //msg('   '+InttoHex(lGroupElement,8)+'   '+inttostr(lLength)+'   '+VR+ inttostr(GetByte(lPos+2)));

     if (vr[2] < 'A')  then begin //implicit vr with 32-bit length
        lLength := ReadInt4;
        exit;
     end;
     //if vr = 'UN' then
     if (vr = 'OB') or (vr = 'OW') or (vr = 'SQ') or (vr = 'UN') then begin  {explicit VR with 32-bit length}
          lPos := lPos + 4;  {skip 2 byte string and 2 reserved bytes = 4 bytes = 2 words}
          lLength := ReadInt4;//Ord4(buf[lPos]) + $100 * (buf[lPos+1] + $100 * (buf[lPos+2] + $100 * buf[lPos+3]))
     end else begin {explicit VR with 16-bit length}
         if lDicomData.little_endian = 0  then
             lLength := (GetByte(lPos+3))+(GetByte(lPos+2) shl 8)
         else
             lLength := (GetByte(lPos+2))+(GetByte(lPos+3) shl 8);//GetLength := Ord4(buf[i+2]) + $100 * (buf[i+3]);
         lPos := lPos + 4;  {skip 2 byte string and 2 length bytes = 4 bytes = 2 words}
     end;
     // msg(InttoHex(lGroupElement,8)+'   '+inttostr(lLength)+'   '+VR+ inttostr(GetByte(lPos+2)) + 'x'+inttostr(GetByte(lPos+3)) );

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
     if lBytes <= 2 then
        result := GetByte(lPos)+(GetByte(lPos+1) shl 8) //shortint vs word?
     else
         result := GetByte(lPos)+(GetByte(lPos+1) shl 8)+(GetByte(lPos+2) shl 16)+(GetByte(lPos+3) shl 24);; //byte order??
end; //function DCMint

{$IFDEF ANON}
Type
  TPosLen =  RECORD //peristimulus plot
    Pos,Len: integer;
  end;
procedure InitPosLen(var lPL: TPOsLen);
begin
    lPL.Pos := 0;
    lPL.Len := 0;
end;
procedure SetPosLen(var lPL: TPosLen; lP,lL: integer);
begin
    lPL.Pos := lP;
    lPL.Len := lL;
end;

procedure Anonymize(lStr: string; lPL: TPOsLen{lPos,lLen: integer});
var
   lDCMstr: string;
   lP,lL: integer;
begin
     if ((lPL.Pos+lPL.Len) > lBufferSz) or (lPL.Len < 1) or (lPL.Pos < 1) then
        exit;
     lDCMStr := '';
     lL := length(lStr);
     if lL > lPL.Len then
        lL := lPL.Len;
     lP := 1;
     while lP <= lL do begin
           lDCMStr := lDCMStr + lStr[lP];
           inc(lP);
     end;
     while lP <= lPL.Len do begin //pad string
           lDCMStr := lDCMStr + ' ';
           inc(lP);
     end;
     for lP := 1 to lPL.Len do
          lByteRA^[lPL.Pos+lP] := Ord(lDCMstr[lP]);

end;

(*procedure AnonymizeDate(lStr: string; lPos,lLen: integer);
var
   lDCMstr: string;
   lP,lL,lDateLen: integer;
begin
     Anonymize(lStr, lPos,lLen);
     //put 19730316 into date field -Lauterbur paper published in Nature
     lDCMstr := '19730316';
     lDateLen := length(lDCMStr);
     if ((lPos+lLen) > lBufferSz) or (lLen < lDateLen) or (lPos < 1) then
        exit;
     for lP := 1 to lDateLen do
          lByteRA^[lPos+lP] := Ord(lDCMstr[lP]);

end;*)

{$ENDIF}

var
   lTempStr,lStr: string;
   lTemp,lGroupElement,lLength,lEchoNum,lnVol: integer;
   lResearchMode: boolean;
   lThick: double;
   {$IFDEF ANON} //position and lengths of tags to anonymize
   lCreateDate,lName,lID,lDOB,lSex,lAge,lWt,lStudyDate,lSeriesDate,lAcqDate: TPosLen;
   {$ENDIF}
begin //function fast_read_dicom_data
   {$IFDEF ANON} //position and lengths of tags to anonymize
   InitPosLen(lName);
   InitPosLen(lID);
   InitPosLen(lDOB);
   InitPosLen(lSex);
   InitPosLen(lAge);
   InitPosLen(lWt);
   InitPosLen(lStudyDate);
   InitPosLen(lSeriesDate);
   InitPosLen(lAcqDate);
   InitPosLen(lCreateDate);
   {$ENDIF}
   lFailure := false;
     lnVol := 1;
     lEchoNum := 1;
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
           dcmMsg(DCMStr(4)+ ' <> DICM');
           FreeMem(lByteRA);
           exit;
        end;
        lPos := lOffset + 4;//DICM read
     end;//Offset = 128
     //next check VR
     if not( chr(GetByte(lPos+4)) in ['A'..'Z']) or not( chr(GetByte(lPos+5)) in ['A'..'Z']) then
        dcmMsg('implicit VR untested');
     //next check Endian
     lTemp := lPos;
     ReadGroupElementLength(lGroupElement,lLength);
     if lLength > kMax16bit then
        dcmMsg('ByteSwapped');
     lPos := lTemp;
     //end VR check
     while (lDICOMData.imagestart = 0) and (not lBufferError) do begin
         ReadGroupElementLength(lGroupElement,lLength);
         if lFailure then goto 999;
         case lGroupElement of
                kTransferSyntax: begin
                     lTempStr := (DCMStr(lLength));
                     if (length(lTempStr) >= 19) and (lTempStr[19] = '2') then
                        lDicomData.little_endian := 0;
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

                 kStudyTime : lDicomData.StudyTime := DCMStr(lLength);
                 {$IFDEF ANON} //position and lengths of tags to anonymize
                kCreateDate: begin SetPosLen(lCreateDate,lPos,lLength) end;
                 kSeriesDate: begin SetPosLen(lSeriesDate,lPos,lLength) end;
                 kAcqDate: begin SetPosLen(lAcqDate,lPos,lLength) end;
                 kStudyDate: begin SetPosLen(lStudyDate,lPos,lLength) end;
                 kPatientName : begin SetPosLen(lName,lPos,lLength) end;
                 kPatientID : begin SetPosLen(lID,lPos,lLength)  end;
                 kPatientDOB : begin SetPosLen(lDOB,lPos,lLength)  end;
                 kPatientSex : begin SetPosLen(lSex,lPos,lLength)  end;
                 kPatientAge : begin SetPosLen(lAge,lPos,lLength)  end;
                 kPatientWt : begin SetPosLen(lWt,lPos,lLength)  end;
                 {$ELSE}
                 kPatientName : lDicomData.PatientName := DCMStr(lLength);
                 kStudyDate: lDicomData.StudyDate := DCMStr(lLength);

                 {$ENDIF}
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
                 kCSAImageHeaderInfo: begin //order ICE,Acq,Num,Vector
                   lDICOMdata.CSAImageHeaderInfoPos := lPos;
                   lDICOMdata.CSAImageHeaderInfoSz :=  lLength;
                                       end;
                 kSlicesPer3DVol: lDICOMData.SlicesPer3DVol := DCMint (lLength);
                 kImageStart: lDICOMData.ImageStart := lPos ; //-1 as indexed from 0.. not 1..
         end; //Case lGroupElement
         //Msg(VR+inttohex(lGroupElement and kMax16bit,4) +':'+inttohex( lGroupElement shr 16,4)+'  '+inttostr(lLength)+'@'+inttostr(lPos) );
         lPos := lPos + (lLength);
     end; //while imagestart=0 and not error
     //clean up
     lDicomData.DateTime := StudyDateTime(lDicomData.StudyDate,lDicomData.StudyTime);
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
     //Remaining portions only if anonymizing
     {$IFDEF ANON}
     dcmMsg('Anonymizing DICOM '+extractfilename(lFilename));
     lBufferSz := lFileSz;
     GetMem(lByteRA,lBufferSz);
     //read original
     AssignFile(lInFile, lFileName);
     FileMode := 0;  //Set file access to read only
     Reset(lInFile, 1);
     //seek(lInFile,1);
     BlockRead(lInFile, lByteRA^[1], lBufferSz);
     CloseFile(lInFile);
     //anonymize...
     Anonymize ('ANONYMIZED',lName);
     Anonymize ('ANONYMIZED',lID);
     Anonymize('19890323',lDOB); //Cold Fusion YYYYMMDD
     Anonymize('19890323',lStudyDate); //Cold Fusion YYYYMMDD
     Anonymize('19890323',lAcqDate); //Cold Fusion YYYYMMDD
     Anonymize('19890323',lSeriesDate); //Cold Fusion YYYYMMDD
     Anonymize('19890323',lCreateDate); //Cold Fusion YYYYMMDD

     Anonymize('M',lSex);
     Anonymize('18',lAge);
     Anonymize('100',lWt);
     //write anonymized data...
     assignfile(lInFile, lFileName+'.dcm');
     Filemode := 2; //read&write
     Rewrite(lInFile,1);
     BlockWrite(lInFile, lByteRA^[1],lBufferSz);
     CloseFile(lInFile);
     //clean up...
   999:
     FreeMem(lByteRA);
     {$ENDIF}
end; //function fast_read_dicom_data


end.