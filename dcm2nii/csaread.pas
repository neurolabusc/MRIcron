unit csaread;
interface
// Extract vital details from the Siemens CSA header that is contained within a DICOM file.
//   This is DICOM group:element (0029:1010) [CSA Image Header Info]
//   These values are crucial converting 2D mosaics to 3D images and computing DTI vectors
//see http://nipy.sourceforge.net/nibabel/dicom/siemens_csa.html
//    This is a port of John Ashburners' spm_dicom_headers.m Matlab code
uses SysUtils, dialogsx, define_types,dialogs_msg, nifti_hdr, nifti_types;
{DEFINE verbose}
{$H+}
type
  TBytearray =  array of byte;
  TSliceTimes =  array of single;
  TCSA = record
     PhaseDirectionPositive: integer; //0 or 1, -1 for unknown
     Slices,MosaicX,MosaicY: longword;
     BandwidthPerPixelPhaseEncode,
     Bvalue,DTIv1,DTIv2,DTIv3, SliceNormV1, SliceNormV2,SliceNormV3: double;
     CustomerSeq: string;
     //SliceTimes :  array of single;
   end;


function DecodeCSA2 (lFilename: string; lCSAImageHeaderInfoPos, lCSAImageHeaderInfoSz: integer; var lCSA: TCSA; var lSliceTimes: TSliceTimes; var lFlippedMosaic: boolean): boolean;

function GetCSASeriesHeaderInfo (lFilename: string; lStart, lLength, lnSlices: integer; var lSliceOrder: integer): string;

function GetCSAImageHeaderInfoDTI (lFilename: string; lStart,lLength: integer; var lBval: integer; var ldti1,ldti2,ldti3: double): boolean;
function GetCSAImageHeaderInfo (lFilename: string; lStart,lLength: integer; var lMosaicSlices,lMosaicX,lMosaicY: integer; var lv1,lv2,lv3: double): boolean;


implementation

function GetCSAImageHeaderInfoDTI (lFilename: string; lStart,lLength: integer; var lBval: integer; var ldti1,ldti2,ldti3: double): boolean;
var
   lCSA: TCSA;
   lFlippedMosaic: boolean;
   lSliceTimes: TSliceTimes ;
begin
     //lBval := -1;//imposibble - read error
     result := DecodeCSA2 (lFilename, lStart,lLength, lCSA, lSliceTimes, lFlippedMosaic);
     lSliceTimes := nil; //free
     if not result then exit;
     lBval := round(lCSA.bvalue);
     ldti1 := lCSA.DTIv1;
     ldti2 := lCSA.DTIv2;
     ldti3 := lCSA.DTIv3;
     if (abs(ldti1) > 0.9) and (abs(ldti2) > 0.9) and (abs(ldti3) > 0.9)  then
       lBval := 0; //syngo MR 2004A 4VA25A CSA header reports Bval=1000 for B0 images, use vectors to detect if this is a B0 image

end;

function GetCSAImageHeaderInfo (lFilename: string; lStart,lLength: integer; var lMosaicSlices,lMosaicX,lMosaicY: integer; var lv1,lv2,lv3: double): boolean;
var
   lCSA: TCSA;
   lSliceTimes: TSliceTimes;
      lFlippedMosaic: boolean;
begin
     //lMosaicSlices := -1;//imposibble - read error
     result := DecodeCSA2 (lFilename, lStart,lLength, lCSA,lSliceTimes, lFlippedMosaic);
     lSliceTimes := nil; //free
     if not result then exit;
     lMosaicSlices := lCSA.Slices;
     lMosaicX := lCSA.MosaicX;
     lMosaicY := lCSA.MosaicY;
     lv1 := lCSA.SliceNormV1;
     lv2 := lCSA.SliceNormV2;
     lv3 := lCSA.SliceNormV3;  //5/5/2013
end;

function DecodeCSA2 (lFilename: string; lCSAImageHeaderInfoPos, lCSAImageHeaderInfoSz: integer; var lCSA: TCSA; var lSliceTimes: TSliceTimes; var lFlippedMosaic: boolean): boolean;
//provided with DICOM file as well as location and size of CSA header, this code returns the Siemens CSA header information

const
  kMaxItem = 1024; // if you only need first 3 values, set to 4 so if an item has 6 values the final ones will overwrite 4th item
type
  TCSAtag = record
     name : string[64];
     vm: longint;
     vr123: string[3];
     vr4: string[1];
     syngodt ,nitems,xx : longint;
   end;
  TCSAitem = record
     xx1, xx2_Len, xx3_77, xx4: longint; // [ x L 77 x] L is length
     value: string;
   end;
var
    lFile    : File;
    lVers: string;
    lData : array of byte;
    lnTag,lPos,lI,lT,lIbound: integer;
    lTag :  TCSAtag;
    lItem : array [1..kMaxItem] of TCSAitem;
function SafeStr2Num (lStr: string): boolean; //for some reason, many fMRI images have bvalue = 'X1_01_001
var
  lP,lL: integer;
begin
  result := false;
  lL := length(lStr);
  if lL < 1 then exit;
  for lP := 1 to lL do
    if not(lStr[lP] in ['+','-','0'..'9','.','e','E']) then
      exit;
  result := true;
end;//nested func SafeStr2Num
function RightStr2Num (lStr: string): integer; //e.g. Siemens AcquisitionMatrixText "104p*96" -> "96"
var
  lL: integer;
  lS: string;
  lDone: boolean;
begin
  result := -1;
  lS := '';
  lL := length(lStr);
  if lL < 1 then exit;
  lDone := false;
  while (lL >= 1) and (not lDone) do begin
    if (lStr[lL] in ['+','-','0'..'9','.','e','E']) then
      lS := lStr[lL]+lS
    else if lS <> '' then
      lDone := true;
    dec(lL);
  end;
  if lS = '' then exit;
  result := strtoint(lS);
end; //nested func RightStr2Num
function LeftStr2Num (lStr: string): integer; //e.g. Siemens AcquisitionMatrixText "104p*96" -> "104"
var
  lP,lL: integer;
  lS: string;
  lDone: boolean;
begin
  result := -1;
  lS := '';
  lL := length(lStr);
  if lL < 1 then exit;
  lP := 1;
  lDone := false;
  while (lP <= lL) and (not lDone) do begin
    if (lStr[lP] in ['+','-','0'..'9','.','e','E']) then
      lS := lS + lStr[lP]
    else if lS <> '' then
      lDone := true;
    inc(lP);
  end;
  if lS = '' then exit;
  result := strtoint(lS);
end; //nested func LeftStr2Num
function freadStr(len: integer): string;
var
  i: integer;
begin
  if (len+lPos) >= lCSAImageHeaderInfoSz then
    Raise Exception.CreateFmt('csaread: corrupt file ', [lFilename]);
  result := '';
  i := 0;
  while (i < len) and (lData[i+lPos] <> 0) and (lData[i+lPos] <> $20)  do begin
      result := result + chr(lData[i+lPos]);
      inc(i);
  end;
  lPos := lPos + len;
end; //nested func freadStr
function freaduint32: longword; overload; //uint32
begin
  if (4+lPos) >= lCSAImageHeaderInfoSz then
    Raise Exception.CreateFmt('csaread: corrupt file ', [lFilename]);
  result := (lData[lPos+3] shl 24)+(lData[lPos+2] shl 16)+(lData[lPos+1] shl 8)+lData[lPos];
  lPos := lPos + 4;
end;//nested func freaduint32
function freadint32: longint; overload; //uint32
begin
  if (4+lPos) >= lCSAImageHeaderInfoSz then
    Raise Exception.CreateFmt('csaread: corrupt file ', [lFilename]);
  result := (lData[lPos+3] shl 24)+(lData[lPos+2] shl 16)+(lData[lPos+1] shl 8)+lData[lPos];
  lPos := lPos + 4;
end;//nested func freadint32
function freadTag: TCSAtag;
begin
    result.name := freadStr(64);
    result.vm:= freadint32;
    result.vr123:= freadStr(3);
    result.vr4:= freadStr(1);
    result.syngodt := freadint32;
    result.nitems := freadint32;
    result.xx := freadint32;
end;//nested func freadTag
function freadItem: TCSAitem;
begin
  result.xx1:= freadint32;
  result.xx2_Len:= freadint32;
  result.xx3_77:= freadint32;
  result.xx4:= freadint32;
  result.value := freadStr(result.xx2_len);
  lPos := lPos + ((4-(result.xx2_len) mod 4  )mod 4) ;
end;//nested func freadItem
begin  //main function DecodeCSA2
  lFlippedMosaic := false;
   result := false;
   lSliceTimes := nil; //clear
   lCSA.CustomerSeq := ''; //clear
   lCSA.Bvalue := -1;
   lCSA.PhaseDirectionPositive := -1;
   if (lCSAImageHeaderInfoSz < 1) then
    exit;
   if FSize(lFilename) <= (lCSAImageHeaderInfoPos+lCSAImageHeaderInfoSz) then
        exit;
   if lCSAImageHeaderInfoSz < 118 then
    exit; //Too short to be a CSA header - Perhaps Philips or GE is using this tag
   setlength(lData, lCSAImageHeaderInfoSz);
   lPos := 0;
   FileMode := fmOpenRead;
   AssignFile(lFile, lFilename);
   Reset(lFile, 1);   // Now we define one record as 1 byte
   Seek(lFile, lCSAImageHeaderInfoPos);    // Records start from 0
   BlockRead(lFile, lData[0], lCSAImageHeaderInfoSz);
   CloseFile(lFile);
   lVers := freadStr(4);
   if lVers = 'SV10' then begin
      //read header
      lPos := lPos + 4; //skip 8 bytes of data, spm_dicom_headers refers to these as unused1 and unused2
      lnTag := freaduint32;
      if (lnTag < 1) or (lnTag > 1024) then begin
        dcmMsg('Error reading CSA header');
        exit;
      end;
      if (lData[lPos] <> 77) then showmsg('warning: strange CSA2 header');
      lPos := lPos + 4; // skip the four bytes 77 00 00 00
      //read tags
      for lT := 1 to lnTag do begin
        lTag := freadTag;
        if lTag.nitems > 0 then begin
          for lI := 1 to lTag.nitems do begin //read items
            if lI > kMaxItem then
              lIbound := kMaxItem //out of range
            else
              lIbound := lI;
            lItem[lIbound] := freadItem;
          end; //for each item
          if (lTag.name = 'NumberOfImagesInMosaic') then
            lCSA.Slices := round(strtofloat(lItem[1].value))
          else if (lTag.name = 'AcquisitionMatrixText') then begin  //'96p*96 -> X= 96 and Y= 96
            lCSA.MosaicX := LeftStr2Num(lItem[1].value);
            lCSA.MosaicY := RightStr2Num(lItem[1].value);
          end else if (lTag.name = 'B_value') and (SafeStr2Num(lItem[1].value)) then  begin
            lCSA.Bvalue := strtofloat(lItem[1].value);
            //dcmmsg('Bvalue = '+floattostr(lCSA.Bvalue));
          end
          else if (lTag.name = 'DiffusionGradientDirection') and (SafeStr2Num(lItem[1].value)) and (SafeStr2Num(lItem[2].value)) and (SafeStr2Num(lItem[3].value))  then begin
            lCSA.DTIv1 := strtofloat(lItem[1].value);
            lCSA.DTIv2 := strtofloat(lItem[2].value);
            lCSA.DTIv3 := strtofloat(lItem[3].value);
          end else if (lTag.name = 'SliceNormalVector') and (SafeStr2Num(lItem[1].value)) and (SafeStr2Num(lItem[2].value)) and (SafeStr2Num(lItem[3].value))  then begin
            lCSA.SliceNormV1 := strtofloat(lItem[1].value);
            lCSA.SliceNormV2 := strtofloat(lItem[2].value);
            lCSA.SliceNormV3 := strtofloat(lItem[3].value);
            //fx( lCSA.SliceNormV3,1234);
          end else if (lTag.name = 'SliceMeasurementDuration') and (SafeStr2Num(lItem[1].value)) then begin
            lCSA.BandwidthPerPixelPhaseEncode := strtofloat(lItem[1].value);
            {$IFDEF verbose} msg('SliceMeasurementDuration: '+floattostr(lCSA.BandwidthPerPixelPhaseEncode));  {$ENDIF}
          end else if (lTag.name = 'BandwidthPerPixelPhaseEncode') and (SafeStr2Num(lItem[1].value)) then begin
            lCSA.BandwidthPerPixelPhaseEncode := strtofloat(lItem[1].value);
            {$IFDEF verbose} msg('BandwidthPerPixelPhaseEncode: '+floattostr(lCSA.BandwidthPerPixelPhaseEncode));  {$ENDIF}
          end else if (lTag.name = 'MosaicRefAcqTimes') and (lTag.nitems > 1) and (SafeStr2Num(lItem[1].value))  then begin
            //showmsg(lTag.name+ '-> '+inttostr(lTag.nitems));
            setlength(lSliceTimes,lTag.nitems);
            for lI := 1 to lTag.nitems do
                if  SafeStr2Num(lItem[lI].value) then
                    lSliceTimes[lI-1] := strtofloat(lItem[lI].value);
          end else if (lTag.name = 'ProtocolSliceNumber')  and (lTag.nitems > 0) and (SafeStr2Num(lItem[1].value))  then begin
             if  SafeStr2Num(lItem[1].value) and (strtofloat(lItem[1].value) > 0) then lFlippedMosaic := true;
          end else if (lTag.name = 'PhaseEncodingDirectionPositive')  and (lTag.nitems > 0) and (SafeStr2Num(lItem[1].value))  then begin
             lCSA.PhaseDirectionPositive := round(strtofloat(lItem[1].value));
          end;



          // else dcmmsg(lTag.name+ '-> '+inttostr(lTag.nitems));
         //  dcmmsg(lTag.name+ '-> '+inttostr(lTag.nitems));

             (*if (lTag.name = 'EchoLinePosition') and (SafeStr2Num(lItem[1].value)) then begin
            lCSA.BandwidthPerPixelPhaseEncode := strtofloat(lItem[1].value);
            msg('EchoLinePosition: '+floattostr(lCSA.BandwidthPerPixelPhaseEncode));
          end;*)
           if (SafeStr2Num(lItem[1].value)) then begin
            lCSA.BandwidthPerPixelPhaseEncode := strtofloat(lItem[1].value);
            {$IFDEF verbose} msg(lTag.name+'  '+inttostr(lTag.nitems)+'  '+floattostr(lCSA.BandwidthPerPixelPhaseEncode));  {$ENDIF}
           end;
          //if true then begin //(lTag.name = 'sSliceArray.ucMode') then begin
          //    showmsg(lTag.name+'xxxxxxxxxx'+lItem[1].value);

          //  end;  ;

        end;//at least one item
      end; //for each tag
      result := true;
      //showmsg('Success DecodeCSA2');
   end else begin
      dcmMsg('CSAread Warning: '+ lFilename +' at byte '+inttostr(lCSAImageHeaderInfoPos)+' reports version "'+lVers+'": only "SV10" format is supported: image is corrupted, very old or new. See if a new version of this software is available.');
   end;
   //Showmsg('CSA done, final tag '+lTag.name+' CSA started at '+inttostr(lCSAImageHeaderInfoPos)+' CSA length of '+inttostr(lCSAImageHeaderInfoSz)+' formal CSA ended at @ '+inttostr(lPos));
  lData := nil;
end;// func DecodeCSA2

function GetCSASeriesHeaderInfo (lFilename: string; lStart,lLength, lnSlices: integer; var lSliceOrder: integer): string;
var
  lData: array of byte;
      lFile    : File;
      asciiStart: integer;
function str2str (lStr: string): string;
var
      lPos,lOK, lStrLen,lDataLen: integer;
      lS: string;
begin
     result := ''; //is failure
     lStrLen := length(lStr);
     lDataLen := lLength-lStrLen-1; //-1 since data must be at least 1 byte
     lPos := 1;
     lOK := 0;
     while lPos < lDataLen do begin
           lOK := 0;
           while (chr(lData[lPos+lOK]) = lStr[lOK+1]) do begin
                 inc(lOK);
                 if (lOK = lStrLen) then begin
                   lS := '0';
                   lPos := lPos + lOK + 1;
                   while (lPos <= lLength) and (lData[lPos] <> 10)  and (lData[lPos] <> ord('"')) do begin // end of line is 0x0A = 10
                         if (lData[lPos] <> ord('\')) and  (lData[lPos] <> ord('/')) then
                            result := result + chr(lData[lPos]);
                         inc(lPos);
                   end; //while not end of line
                   exit;
                 end; //if whole string matches
           end; //while character matches
           lPos:= lPos + lOK + 1;
     end; //while pos in less than data length
end;

function strValue (lStr: string; var lPos: integer): integer;
//returns 4 for "sSliceArray.ucMode  = 0x4" (0A ends line)
var
      lOK, lStrLen,lDataLen: integer;
      lS: string;
begin
     result := 0; //is failure
     lStrLen := length(lStr);
     lDataLen := lLength-lStrLen-1; //-1 since data must be at least 1 byte
     //lPos := 1;
     lOK := 0;
     while lPos < lDataLen do begin
           lOK := 0;
           while (chr(lData[lPos+lOK]) = lStr[lOK+1]) do begin
                 inc(lOK);
                 if (lOK = lStrLen) then begin
                   lS := '0';
                   lPos := lPos + lOK + 1;
                   while (lPos <= lLength) and (lData[lPos] <> 10) do begin // end of line is 0x0A = 10
                         if chr(lData[lPos]) in ['0'..'9'] then
                           lS := lS + chr(lData[lPos]);
                         inc(lPos);
                   end; //while not end of line
                   result := strtointDef(lS,0);
                   //https://wiki.cimec.unitn.it/tiki-index.php?page=MRIBOLDfMRI
                   //http://cbs.fas.harvard.edu/node/559#slice_order
                   case result of
                        1: result := kNIFTI_SLICE_SEQ_INC;
                        2 : result :=  kNIFTI_SLICE_SEQ_DEC;
                        4: begin
                              if odd(lnSlices) then
                                result := kNIFTI_SLICE_ALT_INC
                              else begin
                                dcmMsg(' Warning: Siemens interleaved acquisition with an even number of slices. Assume even slices acquired PRIOR to odd slices. https://wiki.cimec.unitn.it/tiki-index.php?page=MRIBOLDfMRI');
                                result := kNIFTI_SLICE_ALT_INC2;
                              end;
                             end;
                        else begin
                              dcmMsg('Warning: Unknown Siemens slice order '+inttostr(result));
                              result := 0;
                        end;
                   end; //case
                   exit;
                 end; //if whole string matches
           end; //while character matches
           lPos:= lPos + lOK + 1;
     end; //while pos in less than data length
end;

begin
  result := '';
  lSliceOrder := 0;
  if (lLength < 0) then exit;
  SetLength(lData,lLength);
  FileMode := fmOpenRead;
  AssignFile(lFile, lFilename);
  Reset(lFile, 1);   // Now we define one record as 1 byte
  Seek(lFile, lStart);    // Records start from 0
  BlockRead(lFile, lData[0], lLength);
  CloseFile(lFile);
  asciiStart := 1;
  strValue('ASCCONV BEGIN', asciiStart);
  if (asciiStart > 0 ) then begin
     lSliceOrder := strValue('sSliceArray.ucMode', asciiStart); // Prisma data from Tel Aviv: "sSliceArray.ucMode" can be in both main and ASCII header: we want the one from ASCII only!
     result := str2str ('CustomerSeq');
  end;
  lData := nil;
end;

end.
 
