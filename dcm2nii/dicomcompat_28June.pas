unit dicomcompat;
interface
uses
{$Define NoTroubleshoot}
{$IFDEF FPC}
gzio2,
{$ELSE}
gziod,
{$ENDIF}

  SysUtils,Classes,define_types,filename,dicomtypes,dicomfastread,prefs,convertsimple, csaread;
{$H+}
var
kUseDateTimeForID: boolean = false;
procedure read_afni_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string; var lRotation1,lRotation2,lRotation3: integer);
procedure read_ecat_data(var lDICOMdata: DICOMdata;lVerboseRead,lReadECAToffsetTables:boolean; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
procedure read_siemens_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
procedure read_ge_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
procedure read_interfile_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string);
procedure read_voxbo_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string);
procedure read_VFF_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string);
procedure read_picker_data(lVerboseRead: boolean; var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
procedure read_tiff_data(var lDICOMdata: DICOMdata; var lReadOffsets,lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
procedure read_dicom_data_compat(lReadJPEGtables,lVerboseRead,lAutoDECAT7,lReadECAToffsetTables,lAutodetectInterfile,lAutoDetectGenesis,lReadColorTables: boolean; var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string; var lPrefs: TPrefs);

var
  gSizeMMWarningShown : boolean = false;
  gECATJPEG_table_entries: integer = 0;
  gECATJPEG_pos_table,gECATJPEG_size_table : LongIntP;
  red_table_size : Integer = 0;
  green_table_size : Integer = 0;
  blue_table_size : Integer = 0;
  red_table   : ByteP;
  green_table : ByteP;
  blue_table  : ByteP;
implementation

uses dialogsx;

function SecSinceMidnightFloat (lStr: string): double;
var
  lNumStr: string;
  sec: double;
  i,len,dec: integer;
begin
  result := 0;//error

  if lStr = '' then
    exit;
  len := length(lStr);
  lNumStr := '';
  for i := 1 to len do begin
    if (lStr[i] = '.') or (lStr[i] = ',') then
      lStr[i] := DecimalSeparator; //make native format, e.g. in Germany 10,123 whereas in USA 10.123
    if lStr[i] in ['0'..'9',DecimalSeparator] then
      lNumStr := lNumStr + lStr[i];
  end;
  if lNumStr = '' then
    exit;

  //make sure 6 characters before decimal, in case HHMMSS is written HMMSS
  dec := length(lNumStr) + 1;
  for i := length(lNumStr) downto 1 do
    if lNumStr[i] = DecimalSeparator then
      dec := i;
  if dec > 7 then
    exit; //HHMMSS.??? can only have 6 digits before decimal
  while dec < 7 do begin
    lNumStr := '0'+lNumStr;
    inc(dec);
  end;
  //now in HHMMSS.????? format
  len := length(lNumStr);
  lStr := lNumStr[1]+lNumStr[2]; //HH
  sec :=  60 * 60 * strtoint(lStr); //60m/h, 60s/m
  lStr := lNumStr[3]+lNumStr[4]; //MM
  sec := sec + ( 60 * strtoint(lStr)); //60s/m 1000ms/s
  lStr := '';
  for i := 5 to len do //SS.SSSS
    lStr := lStr + lNumStr[i];
  sec := sec + (  strtofloat(lStr)); //60s/m 1000ms/s
  result := sec;
end;

function AddIndent(lIndent: integer): string;
var
  i: integer;
begin
  result := '';
  if lIndent < 1 then
    exit;
  //for i := 1 to lIndent do
    result := result +'|';
end;

procedure read_ecat_data(var lDICOMdata: DICOMdata;lVerboseRead,lReadECAToffsetTables:boolean; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
label
  121,539;
const
     kMaxnSLices = 6000;
     kStrSz = 40;
var
   lLongRA: LongIntp;
   lECAT7sigUpcase,lECAT7sig  : array [0..6] of Char;
  lParse,lSPos,lFPos{,lScomplement},lF,lS,lYear,lFrames,lVox,lHlfVox,lJ,lPass,lVolume,lNextDirectory,lSlice,lSliceSz,lVoxelType,lPos,lEntry,
  lSlicePos,lLongRApos,lLongRAsz,{lSingleRApos,lSingleRAsz,}{lMatri,}lX,lY,lZ,lCacheSz,lImgSz,lSubHeadStart,lMatrixStart,lMatrixEnd,lInt,lInt2,lInt3,lINt4,n,filesz: LongInt;
  lPlanes,lGates,lAqcType,lFileType: word;
  lXmm,lYmm,lZmm,lCalibrationFactor, lQuantScale: real;
  FP: file;
  lCreateTable,lSwapBytes,lMR,lECAT6: boolean;
function xWord(lPos: longint): word;
var
s: word;
begin
     seek(fp,lPos);
     BlockRead(fp, s, 2, n);
     if lSwapBytes then
        result := swap(s)
     else result := s; //assign address of s to inguy
end;

function swap32i(lPos: longint): Longint;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
   s : LongInt;
  inguy:swaptypep;
  outguy:swaptype;
begin
     seek(fp,lPos);
  BlockRead(fp, s, 4, n);
  inguy := @s; //assign address of s to inguy
  if not lSwapBytes then begin
      result := inguy^.long;
      exit;
  end;
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  swap32i:=outguy.Long;
end;
function StrRead (lPos, lSz: longint) : string;
var
   I: integer;
   tx  : array [1..kStrSz] of Char;
begin
  result := '';
  if lSz > kStrSz then exit;
  seek(fp, lPos{-1});
  BlockRead(fp, tx, lSz*SizeOf(Char), n);
  for I := 1 to (lSz-1) do begin
      if tx[I] in [' ','[',']','+','-','.','\','~','/', '0'..'9','a'..'z','A'..'Z'] then
      {if (tx[I] <> kCR) and (tx[I] <> UNIXeoln) then}
      result := result + tx[I];
  end;
end;
function fswap4r (lPos: longint): single;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(float:single);
  end;
  swaptypep = ^swaptype;
var
   s:single;
  inguy:swaptypep;
  outguy:swaptype;
begin
     seek(fp,lPos);
     if not lSwapBytes then begin
        BlockRead(fp, result, 4, n);
        exit;
     end;
  BlockRead(fp, s, 4, n);
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  fswap4r:=outguy.float;
end;
function fvax4r (lPos: longint): single;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(float:single);
  end;
  swaptypep = ^swaptype;
var
   s:single;
   lT1,lT2 : word;
  inguy:swaptypep;
begin
     seek(fp,lPos);
     BlockRead(fp, s, 4, n);
     inguy := @s;
     if (inguy^.Word1 =0) and (inguy^.Word2 = 0) then begin
        result := 0;
        exit;
     end;
     lT1 := inguy^.Word1 and $80FF;
     lT2 := ((inguy^.Word1 and $7F00) +$FF00) and $7F00;
     inguy^.Word1 := inguy^.Word2;
     inguy^.Word2 := (lt1+lT2);
     fvax4r:=inguy^.float;
end;
begin
  Clear_Dicom_Data(lDicomData);
  if gECATJPEG_table_entries <> 0 then begin
     freemem (gECATJPEG_pos_table);
     freemem (gECATJPEG_size_table);
     gECATJPEG_table_entries := 0;
  end;
  lHdrOK:= false;
  lQuantScale:= 1;
  lCalibrationFactor := 1;
  lLongRASz := 0;
  lLongRAPos := 0;
  lImageFormatOK := false;
  lVolume := 1;
  if not fileexists(lFileName) then begin
     Msg('Unable to find the image '+lFileName);
     exit;
  end;
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FileSz := FileSize(fp);
     if filesz < (2048) then begin
        Msg('This file is to small to be a ECAT format image.');
        goto 539;
     end;
  seek(fp, 0);
  BlockRead(fp, lECAT7Sig, 6*SizeOf(Char){, n});
  for lInt4 := 0 to (5) do begin
      if lECAT7Sig[lInt4] in ['a'..'z','A'..'Z'] then
         lECAT7SigUpCase[lInt4] := upcase(lECAT7Sig[lInt4])
      else
          lECAT7SigUpCase[lInt4] := ' ';
  end;
  if (lECAT7SigUpCase[0]='M') and (lECAT7SigUpCase[1]='A') and (lECAT7SigUpCase[2]='T') and (lECAT7SigUpCase[3]='R') and
  (lECAT7SigUpCase[4]='I') and (lECAT7SigUpCase[5]='X') then
    lECAT6 := false
  else
      lECAT6 := true;
   if lEcat6 then begin
      lSwapBytes := false;
      lFileType := xWord(27*2);
      if lFileType > 255 then lSwapBytes := not lSwapBytes;
      lFileType := xWord(27*2);
      lAqcType := xWord(175*2);
      lPlanes := xWord(188*2);
      lFrames := xword(189*2);
      lGates := xWord(190*2);
      lYear := xWord(70);
      if (lPlanes < 1) or (lFrames < 1) or (lGates < 1) then begin
         case MsgDlg('Warning: one of the planes/frames/gates values is less than 1 ['+inttostr(lPlanes)+'/'+inttostr(lFrames)+'/'+inttostr(lGates)+']. Is this file really ECAT 6 format? Press abort to cancel conversion. ',
             mterror,[mbOK,mbAbort], 0) of
             mrAbort: goto 539;
         end; //case
      end else if (lYear < 1940) or (lYear > 3000) then begin
        case MsgDlg('Warning: the year value appears invalid ['+inttostr(lYear)+']. Is this file really ECAT 6 format? Press abort to cancel conversion. ',
             mterror,[mbOK,mbAbort], 0) of
             mrAbort: goto 539;
        end; //case
     end;
     if lVerboseRead then begin
        lDynStr :='ECAT6 data';
        lDynStr :=lDynStr+kCR+('Patient Name:'+StrRead(190,32));
        lDynStr :=lDynStr+kCR+('Patient ID:'+StrRead(174,16));
        lDynStr :=lDynStr+kCR+('Study Desc:'+StrRead(318,32));
        lDynStr := lDynStr+kCR+('Facility: '+StrRead(356,20));
        lDynStr := lDynStr+kCR+('Planes: '+inttostr(lPlanes));
        lDynStr := lDynStr+kCR+('Frames: '+inttostr(lFrames));
        lDynStr := lDynStr+kCR+('Gates: '+inttostr(lGates));
        lDynStr := lDynStr+kCR+('Date DD/MM/YY: '+ inttostr(xWord(66))+'/'+inttostr(xWord(68))+'/'+inttostr(lYear));
     end; {show summary}
   end else begin //NOT ECAT6
       lSwapBytes := true;
     lFileType := xWord(50);
     if lFileType > 255 then lSwapBytes := not lSwapBytes;
     lFileType := xWord(50);
     lAqcType := xWord(328);
     lPlanes := xWord(352);
     lFrames := xWord(354);
     lGates := xWord(356);
     lCalibrationFactor := fswap4r(144);
     if {(true) or} (lPlanes < 1) or (lFrames < 1) or (lGates < 1) then begin
        case MsgDlg('Warning: on of the planes/frames/gates values is less than 1 ['+inttostr(lPlanes)+'/'+inttostr(lFrames)+'/'+inttostr(lGates)+']. Is this file really ECAT 7 format? Press abort to cancel conversion. ',
             mterror,[mbOK,mbAbort], 0) of
             mrAbort: goto 539;
        end; //case
     end; //error
     if lVerboseRead then begin
          lDynStr := 'ECAT 7 format';
          lDynStr := lDynStr+kCR+('Serial Number:'+StrRead(52,10));
          lDynStr := lDynStr+kCR+('Patient Name:'+StrRead(182,32));
          lDynStr := lDynStr+kCR+('Patient ID:'+StrRead(166,16));
          lDynStr := lDynStr+kCR+('Study Desc:'+StrRead(296,32));
          lDynStr := lDynStr+kCR+('Facility: '+StrRead(332,20));
          lDynStr := lDynStr+kCR+('Scanner: '+inttostr(xWord(48)));
          lDynStr := lDynStr+kCR+('Planes: '+inttostr(lPlanes));
          lDynStr := lDynStr+kCR+('Frames: '+inttostr(lFrames));
          lDynStr := lDynStr+kCR+('Gates: '+inttostr(lGates));
          lDynStr := lDynStr+kCR+'Calibration: '+floattostr(lCalibrationFactor);
     end; {lShow Summary}
   end; //lECAT7
if lFiletype = 9 then lFiletype := 7;  //1364: treat projections as Volume16's 
if not (lFileType in [1,2,3,4,7]) then begin
   Msg('This software does not recognize the ECAT file type. Selected filetype: '+inttostr(lFileType));
   goto 539;
end;
lVoxelType := 2;
if lFileType = 3 then lVoxelType := 4;
if lVerboseRead then begin
  case lFileType of
    1: lDynStr := lDynStr+kCR+('File type: Scan File');
    2: lDynStr := lDynStr+kCR+('File type: Image File'); //x
    3: lDynStr := lDynStr+kCR+('File type: Attn File');
    4: lDynStr := lDynStr+kCR+('File type: Norm File');
    7: lDynStr := lDynStr+kCR+('File type: Volume 16'); //x
  end; //lfiletye case
  case lAqcType of
     1:lDynStr := lDynStr+kCR+('Acquisition type: Blank');
     2:lDynStr := lDynStr+kCR+('Acquisition type: Transmission');
     3:lDynStr := lDynStr+kCR+('Acquisition type: Static Emission');
     4:lDynStr := lDynStr+kCR+('Acquisition type: Dynamic Emission');
     5:lDynStr := lDynStr+kCR+('Acquisition type: Gated Emission');
     6:lDynStr := lDynStr+kCR+('Acquisition type: Transmission Rect');
     7:lDynStr := lDynStr+kCR+('Acquisition type: Emission Rect');
     8:lDynStr := lDynStr+kCR+('Acquisition type: Whole Body Transm');
     9:lDynStr := lDynStr+kCR+('Acquisition type: Whole Body Static');
     else lDynStr := lDynStr+kCR+('Acquisition type: Undefined');
  end; //case AqcType
end; //verbose read
if ((lECAT6) and (lFiletype =2)) or ({(not lECAT6) and} (lFileType=7)) then  //Kludge
else begin
     Msg('Unusual ECAT filetype. Please contact the author.');
     goto 539;
end;
lHdrOK:= true;
lImageFormatOK := true;
lLongRASz := kMaxnSlices * sizeof(longint);
getmem(lLongRA,lLongRAsz);
lPos := 512;
//lSingleRASz := kMaxnSlices * sizeof(single);
//getmem(lSingleRA,lSingleRAsz);
//lMatri := 0;
lVolume := 1;
lPass := 0;
121:
     lEntry := 1;
     lInt := swap32i(lPos);
     lInt2 := swap32i(lPos+4);
   lNextDirectory := lInt2;
   while true do begin
      inc(lEntry);
     lPos := lPos + 16;
     lInt := swap32i(lPos);
     lInt2 := swap32i(lPos+4);
     lInt3 := swap32i(lPos+8);
     lInt4 := swap32i(lPos+12);
     lInt2 := lInt2 - 1;
     lSubHeadStart := lINt2 *512;
     lMatrixStart := ((lInt2) * 512)+512 {add subhead sz};
     lMatrixEnd := lInt3 * 512;
     if  (lInt4 = 1) and (lMatrixStart < FileSz) and (lMatrixEnd <= FileSz) then begin
        if (lFileType= 7) {or (lFileType = 4) } or (lFileType = 2) then begin //Volume of 16-bit integers
           if lEcat6 then begin
               lX := xWord(lSubHeadStart+(66*2));
               lY := xWord(lSubHeadStart+(67*2));
               lZ := 1;//uxWord(lSubHeadStart+8);
               lXmm := 10*fvax4r(lSubHeadStart+(92*2));// fswap4r(lSubHeadStart+(92*2));
               lYmm := lXmm;//read32r(lSubHeadStart+(94*2));
               lZmm := 10 * fvax4r(lSubHeadStart+(94*2));
               lCalibrationFactor :=  fvax4r(lSubHeadStart+(194*2));
               lQuantScale := fvax4r(lSubHeadStart+(86*2));
               if lVerboseRead then
                  lDynStr := lDynStr+kCR+'Plane '+inttostr(lPass+1)+' Calibration/Scale Factor: '+floattostr(lCalibrationFactor)+'/'+floattostr(lQuantScale);
           end else begin
           //02 or 07
               lX := xWord(lSubHeadStart+4);
               lY := xWord(lSubHeadStart+6);
               lZ := xWord(lSubHeadStart+8);
               //if lFileType <> 4 then begin
               lXmm := 10*fswap4r(lSubHeadStart+34);
               lYmm := 10*fswap4r(lSubHeadStart+38);
               lZmm := 10*fswap4r(lSubHeadStart+42);
               lQuantScale := fswap4r(lSubHeadStart+26);
               if lVerboseRead then
                  lDynStr := lDynStr+kCR+'Volume: '+inttostr(lPass+1)+' Scale Factor: '+floattostr(lQuantScale);
               //end; //filetype <> 4
           end;  //ecat7
           if true then begin
           //FileMode := 2; //set to read/write
           inc(lPass);
           lImgSz := lX * lY * lZ * lVoxelType; {2 bytes per voxel}
           lSliceSz := lX * lY * lVoxelType;
           if lZ < 1 then begin
              lHdrOK := false;
              goto 539;
           end;
           lSlicePos := lMatrixStart;
           if ((lECAT6) and (lPass = 1)) or ( (not lECAT6)) then begin
             lDICOMdata.XYZdim[1] := lX;
             lDICOMdata.XYZdim[2] := lY;
             lDICOMdata.XYZdim[3] := lZ;
             lDICOMdata.XYZmm[1] := lXmm;
             lDICOMdata.XYZmm[2] := lYmm;
             lDICOMdata.XYZmm[3] := lZmm;
             case lVoxelType of
                  1: begin
                     Msg('Error: 8-bit data not supported [yet]. Please contact the author.');
                     lDicomData.Allocbits_per_pixel := 8;
                     lHdrOK := false;
                     goto 539;
                  end;
                  4: begin
                     Msg('Error: 32-bit data not supported [yet]. Please contact the author.');
                     lHdrOK := false;
                     goto 539;
                  end;
                  else begin //16-bit integers
                     lDicomData.Allocbits_per_pixel := 16;
                  end;
             end; {case lVoxelType}
           end else begin //if lECAT6
               if (lDICOMdata.XYZdim[1] <> lX) or (lDICOMdata.XYZdim[2] <> lY) or (lDICOMdata.XYZdim[3] <> lZ) then begin
                  Msg('Error: different slices in this volume have different slice sizes. Please contact the author.');
                  lHdrOK := false;
                  goto 539;
               end; //dimensions have changed
               //lSlicePos :=((lMatri-1)*lImgSz);
           end; //ECAT6
           lVox := lSliceSz div 2;
           lHlfVox := lSliceSz div 4;
           for lSlice := 1 to lZ do begin
              if (not lECAT6) then
                 lSlicePos := ((lSlice-1)*lSliceSz)+lMatrixStart;
               if lLongRAPos >= kMaxnSLices then begin
                  lHdrOK := false;
                  goto 539;
               end;
               inc(lLongRAPos);
               lLongRA^[lLongRAPos] := lSlicePos;
               {inc(lSingleRAPos);
               if lCalibTableType = 1 then
                  lSingleRA[lSingleRAPos] := lQuantScale
               else
                  lSingleRA[lSingleRAPos] := lCalibrationFactor *lQuantScale;}

           end; //slice 1..lZ
           if not lECAT6 then inc(lVolume);
          end; //fileexistsex
        end; //correct filetype
     end; //matrix start/end within filesz
     if (lMatrixStart > FileSz) or (lMatrixEnd >= FileSz) then goto 539;
     if ((lEntry mod 32) = 0) then begin
        if ((lNextDirectory-1)*512) <= lPos then goto 539; //no more directories
        lPos := (lNextDirectory-1)*512;
        goto 121;
     end;  //entry 32
     end ;  //while true
539:
  CloseFile(fp);
  FileMode := 2; //set to read/write
  lDicomData.XYZdim[3] := lLongRApos;
  if not lECAT6 then dec(lVolume); //ECAT7 increments immediately before exiting loop - once too often
  lDicomData.XYZdim[4] :=(lVolume);
  if lSwapBytes then
     lDicomData.little_endian := 0
  else
      lDicomData.little_endian := 1;
  if (lLongRApos > 0) and (lHdrOK) then begin
     lDicomData.ImageStart := lLongRA^[1];
     lCreateTable := false;
     if (lLongRApos > 1) then begin
        lFPos := lDICOMdata.ImageStart;
        for lS := 2 to lLongRApos do begin
            lFPos := lFPos + lSliceSz;
            if lFPos <> lLongRA^[lS] then lCreateTable := true;
        end;
        if (lCreateTable) and (lReadECAToffsetTables) then begin
           gECATJPEG_table_entries := lLongRApos;
           getmem (gECATJPEG_pos_table, gECATJPEG_table_entries*sizeof(longint));
           getmem (gECATJPEG_size_table, gECATJPEG_table_entries*sizeof(longint));
           for lS := 1 to gECATJPEG_table_entries do
               gECATJPEG_pos_table^[lS] := lLongRA^[lS]
        end else if (lCreateTable) then
            lImageFormatOK := false;  //slices are offset within this file
     end;
     if (lVerboseRead) and (lHdrOK) then begin
        lDynStr :=lDynStr+kCR+('XYZdim:'+inttostr(lX)+'/'+inttostr(lY)+'/'+inttostr(gECATJPEG_table_entries));
        lDynStr :=lDynStr+kCR+('XYZmm: '+floattostrf(lDicomData.XYZmm[1],ffFixed,7,7)+'/'+floattostrf(lDicomData.XYZmm[2],ffFixed,7,7)
        +'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,7,7));
        //xlDynStr :=lDynStr+kCR+('Bits per voxel: '+inttostr(lDicomData.Storedbits_per_pixel));
        lDynStr :=lDynStr+kCR+('Image Start: '+inttostr(lDicomData.ImageStart));
        if lCreateTable then
           lDynStr :=lDynStr+kCR+('Note: staggered slice offsets');
     end
  end;
  //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
  if lLongRASz > 0 then
     freemem(lLongRA);
  (*if (lSingleRApos > 0) and (lHdrOK) and (lCalibTableType <> 0) then begin
           gECAT_scalefactor_entries := lSingleRApos;
           getmem (gECAT_scalefactor_table, gECAT_scalefactor_entries*sizeof(single));
           for lS := 1 to gECAT_scalefactor_entries do
               gECAT_scalefactor_table[lS] := lSingleRA[lS];
  end;
  if lSingleRASz > 0 then
     freemem(lSingleRA);*)
end;

(*procedure write_slc (lFileName: string; var pDICOMdata: DICOMdata;var lSz: integer; lDICOM3: boolean);
const kMaxRA = 41;
     lXra: array [1..kMaxRA] of byte = (7,8,9,21,22,26,27,
     35,36,44,45,
     50,62,66,78,
     81,95,
     97,103,104,105,106,111,
     113,123,127,
     129,139,142,
     146,147,148,149,155,156,157,
     166,167,168,169,170);
var
   fp: file;
   lX,lClr,lPos,lRApos: integer;
   lP: bytep;
procedure WriteString(lStr: string; lCR: boolean);
var
     n,lStrLen      : Integer;
begin
     lStrLen := length(lStr);
     for n := 1 to lstrlen do begin
            lPos := lPos + 1;
            lP[lPos] := ord(lStr[n]);
     end;
     if lCR then begin
        lPos := lPos + 1;
        lP[lPos] := ord(kCR);
     end;
end;

begin
  lSz := 0;
  getmem(lP,2048);
  lPos := 0;
  WriteString('11111',true);
  WriteString(inttostr(pDicomData.XYZdim[1])+' '+inttostr(pDicomData.XYZdim[2])+' '+inttostr(pDicomData.XYZdim[3])+' 8',true);
  WriteString(floattostrf(pDicomData.XYZmm[1],ffFixed,7,7)+' '+floattostrf(pDicomData.XYZmm[2],ffFixed,7,7)+' '+floattostrf(pDicomData.XYZmm[3],ffFixed,7,7),true);
  WriteString('1 1 0 0',true); //mmunits,MR,original,nocompress
  WriteString('16 12 X',false); //icon is 8x8 grid, so 64 bytes for red,green blue
  for lClr := 1 to 3 do begin
    lRApos := 1;
    for lX := 1 to 192 do begin
      inc(lPos);
      if (lRApos <= kMaxRA) and (lX = lXra[lRApos]) then begin
         inc(lRApos);
         lP[lPos] := 200;
      end else
          lP[lPos] := 0;
    end; {icongrid 1..192}
  end; {RGB}
  if lFileName <> '' then begin
     AssignFile(fp, lFileName);
     Rewrite(fp, 1);
     blockwrite(fp,lP^,lPos);
     close(fp);
  end;
  freemem(lP);
  lSz := lPos;
end;*)
procedure read_interfile_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string);
label 333;
const UNIXeoln = chr(10);
var lTmpStr,
lInStr,lUpCaseStr: string;
lHdrEnd,lFloat,lUnsigned: boolean;
lPos,lLen,FileSz,linPos: integer;
fp: file;
lCharRA: bytep;
function readInterFloat:real;
var lStr: string;
begin
  lStr := '';
  While (lPos <= lLen) and (lInStr[lPos] <> ';') do begin
        if lInStr[lPos] in ['+','-','e','E','.','0'..'9'] then
           lStr := lStr+(linStr[lPos]);
        inc(lPos);
  end;
    try
       result := strtofloat(lStr);
    except
          on EConvertError do begin
             Msg('Unable to convert the string '+lStr+' to a number');
             result := 1;
             exit;
          end;
    end; {except}
  end;
function readInterStr:string;
var lStr: string;
begin
  lStr := '';
  While (lPos <= lLen) and (lInStr[lPos] = ' ') do begin
        inc(lPos);
  end;
  While (lPos <= lLen) and (lInStr[lPos] <> ';') do begin
		if lInStr[lPos] <> ' ' then //1.39 build 6
			lStr := lStr+upcase(linStr[lPos]); //zebra upcase
        inc(lPos);
  end;
  result := lStr;
end; //interstr func
begin
  lHdrOK := false;
  lFloat := false;
  lUnsigned := false;
  lImageFormatOK := true;
  Clear_Dicom_Data(lDicomData);
  lDynStr := '';
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FileSz := FileSize(fp);
  lHdrEnd := false;
  //lDicomData.ImageStart := FileSz;
  GetMem( lCharRA, FileSz+1 );
  BlockRead(fp, lCharRA^, FileSz, linpos);
  if lInPos <> FileSz then Msg('Disk error: Unable to read full input file.');
  linPos := 1;
  CloseFile(fp);
  FileMode := 2; //set to read/write
repeat
  linstr := '';
  while (linPos < FileSz) and (lCharRA^[linPos] <> ord(kCR)) and (lCharRA^[linPos] <> ord(UNIXeoln)) do begin
      lInStr := lInstr + chr(lCharRA^[linPos]);
      inc(linPos);
  end;
  inc(lInPos);  //read EOLN
  lLen := length(lInStr);
  lPos := 1;
  lUpcaseStr := '';
  While (lPos <= lLen) and (lInStr[lPos] <> ';') and (lInStr[lPos] <> '=') and (lUpCaseStr <>'INTERFILE') do begin
        if lInStr[lPos] in ['[',']','(',')','/','+','-',{' ',} '0'..'9','a'..'z','A'..'Z'] then
           lUpCaseStr := lUpCaseStr+upcase(linStr[lPos]);
        inc(lPos);
  end;
  inc(lPos); {read equal sign in := statement}
  if lUpCaseStr ='INTERFILE' then begin
     lHdrOK := true;
     lDicomData.little_endian := 0;
     end;
  if lUpCaseStr ='DATASTARTINGBLOCK'then lDicomData.ImageStart := 2048 * round(readInterFloat);
  if lUpCaseStr ='DATAOFFSETINBYTES'then lDicomData.ImageStart := round(readInterFloat);
  if (lUpCaseStr ='MATRIXSIZE[1]') or (lUpCaseStr ='MATRIXSIZE[X]') then lDicomData.XYZdim[1] :=  round(readInterFloat);
  if (lUpCaseStr ='MATRIXSIZE[2]')or (lUpCaseStr ='MATRIXSIZE[Y]')then lDicomData.XYZdim[2] :=  round(readInterFloat);
  if (lUpCaseStr ='MATRIXSIZE[3]')or (lUpCaseStr ='MATRIXSIZE[Z]') or (lUpCaseStr ='NUMBEROFSLICES') or (lUpCaseStr ='TOTALNUMBEROFIMAGES') then begin
     lDicomData.XYZdim[3] :=  round(readInterFloat);
  end;
  if lUpCaseStr ='IMAGEDATABYTEORDER' then begin
     if readInterStr = 'LITTLEENDIAN' then lDicomData.little_endian := 1;
  end;
  if lUpCaseStr ='NUMBERFORMAT' then begin
      lTmpStr := readInterStr;
      if (lTmpStr = 'ASCII') or (lTmpStr='BIT') then begin
         lHdrOK := false;
         Msg('This software can not convert '+lTmpStr+' data type.');
         goto 333;
      end;
      if lTmpStr = 'UNSIGNEDINTEGER' then lUnsigned := true;
      if (lTmpStr='FLOAT') or (lTmpStr='SHORTFLOAT') or (lTmpStr='LONGFLOAT') then begin //1395
         lFloat := true;
	  end;
  end;
  if lUpCaseStr ='NAMEOFDATAFILE' then lFileName := ExtractFilePath(lFileName)+readInterStr;
  if lUpCaseStr ='NUMBEROFBYTESPERPIXEL' then
     lDicomData.Allocbits_per_pixel :=  round(readInterFloat)*8;
  if (lUpCaseStr ='SCALINGFACTOR(MM/PIXEL)[1]') or (lUpCaseStr ='SCALINGFACTOR(MM/PIXEL)[X]') then
     lDicomData.XYZmm[1] :=  (readInterFloat);
  if (lUpCaseStr ='SCALINGFACTOR(MM/PIXEL)[2]') or (lUpCaseStr ='SCALINGFACTOR(MM/PIXEL)[Y]')then lDicomData.XYZmm[2] :=  (readInterFloat);
  if (lUpCaseStr ='SCALINGFACTOR(MM/PIXEL)[3]')or (lUpCaseStr ='SCALINGFACTOR(MM/PIXEL)[Z]')or (lUpCaseStr ='SLICETHICKNESS')then lDicomData.XYZmm[3] :=  (readInterFloat);
  if (lUpCaseStr ='ENDOFINTERFILE') then lHdrEnd := true;
  if not lHdrOK then goto 333;
  if lInStr <> '' then
     lDynStr := lDynStr + lInStr+kCr;
  lHdrOK := true;
until (linPos >= FileSz) or (lHdrEnd){EOF(fp)};
//xlDicomData.Storedbits_per_pixel := lDicomData.Allocbits_per_pixel;
lImageFormatOK := true;
if (not lFLoat) and (lUnsigned) and ((lDicomData.Allocbits_per_pixel = 16)) then begin
   Msg('Warning: this Interfile image uses UNSIGNED 16-bit data [values 0..65535]. Analyze specifies SIGNED 16-bit data [-32768..32767]. Some images may not transfer well. [Future versions of MRIcro should fix this].');
   lImageFormatOK := false;
end else if (not lFLoat) and (lDicomData.Allocbits_per_pixel > 16) then begin
   Msg('WARNING: The image '+lFileName+' is a '+inttostr(lDicomData.Allocbits_per_pixel)+'-bit integer data type. This software may display this as SIGNED data. Bits per voxel: '+inttostr(lDicomData.Allocbits_per_pixel));
   lImageFormatOK := false;
end else if (lFloat) then begin //zebra change float check
   //Msg('WARNING: The image '+lFileName+' uses floating point [real] numbers. The current software can only read integer data type Interfile images.');
   lDicomData.FloatData := true;
   //lImageFormatOK := false;
end;
333:
FreeMem( lCharRA);
end; //interfile



//afni start
function ParseFileName (lFilewExt:String): string;
var
   lLen,lInc: integer;
   lName: String;
begin
	lName := '';
     lLen := length(lFilewExt);
	lInc := lLen+1;
     if  lLen > 0 then
	   repeat
              dec(lInc);
        until (lFileWExt[lInc] = '.') or (lInc = 1);
     if lInc > 1 then
        for lLen := 1 to (lInc - 1) do
            lName := lName + lFileWExt[lLen]
     else
         lName := lFilewExt; //no extension
        ParseFileName := lName;
end;

procedure read_afni_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string; var lRotation1,lRotation2,lRotation3: integer);
//label 333;
const UNIXeoln = chr(10);
kTab = ord(chr(9));
kSpace = ord(' ');
var lTmpStr,lInStr,lUpCaseStr: string;
lHdrEnd: boolean;
lMSBch: char;
lOri : array [1..4] of single;
lTmpInt,lPos,lLen,FileSz,linPos: integer;
fp: file;
lCharRA: bytep;
procedure readAFNIeoln;
begin
  while (linPos < FileSz) and (lCharRA^[linPos] <> ord(kCR)) and (lCharRA^[linPos] <> ord(UNIXeoln)) do
      inc(linPos);
  inc(lInPos);  //read EOLN
end;
function readAFNIFloat:real;
var lStr: string;
lCh:char;
begin
  lStr := '';
  while (linPos < FileSz) and ((lStr='') or ((lCharRA^[lInPos] <> kTab) and (lCharRA^[lInPos] <> kSpace))) do begin
        lCh:= chr(lCharRA^[linPos]);
        if lCh in ['+','-','e','E','.','0'..'9'] then
           lStr := lStr+lCh;
      inc(linPos);
  end;
  if lStr = '' then exit;
    try
       result := strtofloat(lStr);
    except
          on EConvertError do begin
             Msg('Unable to convert the string '+lStr+' to a number');
             result := 1;
             exit;
          end;
    end; {except}
  end;
begin
  lHdrOK := false;
  lImageFormatOK := true;
  Clear_Dicom_Data(lDicomData);
  lDynStr := '';
  lTmpStr := string(StrUpper(PChar(ExtractFileExt(lFileName))));
  if lTmpStr <> '.HEAD' then exit;
  for lInPos := 1 to 3 do
      lOri[lInPos] := -6666;
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FileSz := FileSize(fp);
  lHdrEnd := false;
  //lDicomData.ImageStart := FileSz;
  GetMem( lCharRA, FileSz+1 );
  BlockRead(fp, lCharRA^, FileSz, linpos);
  if lInPos <> FileSz then Msg('Disk error: Unable to read full input file.');
  linPos := 1;
  CloseFile(fp);
  FileMode := 2; //set to read/write
repeat
  linstr := '';
  while (linPos < FileSz) and (lCharRA^[linPos] <> ord(kCR)) and (lCharRA^[linPos] <> ord(UNIXeoln)) do begin
      lInStr := lInstr + chr(lCharRA^[linPos]);
      inc(linPos);
  end;
  inc(lInPos);  //read EOLN
  lLen := length(lInStr);
  lPos := 1;
  lUpcaseStr := '';
  While (lPos <= lLen) do begin
        if lInStr[lPos] in ['_','[',']','(',')','/','+','-','=',{' ',} '0'..'9','a'..'z','A'..'Z'] then
           lUpCaseStr := lUpCaseStr+upcase(linStr[lPos]);
        inc(lPos);
  end;
  inc(lPos); {read equal sign in := statement}
  if lUpCaseStr ='NAME=DATASET_DIMENSIONS'then begin
     lImageFormatOK := true;
     lHdrOK := true;
     lFileName := parsefilename(lFilename)+'.BRIK'; //always UPPERcase
     readAFNIeoln;
     lDICOMdata.XYZdim[1] := round(readAFNIFloat);
     lDICOMdata.XYZdim[2] := round(readAFNIFloat);
     lDICOMdata.XYZdim[3] := round(readAFNIFloat);
     //lDicomData.ImageStart := 2048 * round(readInterFloat);
  end;
  if lUpCaseStr ='NAME=BRICK_FLOAT_FACS'then begin
     readAFNIeoln;
     lDICOMdata.IntenScale :=  readAFNIFloat; //1380 read slope of intensity
  end;
  if lUpCaseStr ='NAME=DATASET_RANK'then begin
     readAFNIeoln;
     //2nd value is number of volumes
     readAFNIFloat;
     lDICOMdata.XYZdim[4] := round(readAFNIFloat);
  end;
  if lUpCaseStr ='NAME=BRICK_TYPES'then begin
     readAFNIeoln;
     lTmpInt := round(readAFNIFloat);
     case lTmpInt of
          0:lDicomData.Allocbits_per_pixel := 8;
          1:begin
                 lDicomData.Allocbits_per_pixel := 16;
                 //lDicomData.MaxIntensity := 65535; //Old AFNI were UNSIGNED, new ones are SIGNED???
          end;
          3:begin
                 lDicomData.Allocbits_per_pixel := 32;
                 lDicomData.FloatData := true;
          end;
          else begin
              lHdrEnd := true;
              Msg('Unsupported AFNI BRICK_TYPES: '+inttostr(lTmpInt));
          end;

     end; //case
     {datatype
     0 = byte    (unsigned char; 1 byte)
                1 = short   (2 bytes, signed)
                3 = float   (4 bytes, assumed to be IEEE format)
                5 = complex (8 bytes: real+imaginary parts)}
  end;
  if lUpCaseStr ='NAME=BYTEORDER_STRING'then begin
     readAFNIeoln;
     if ((linPos+2) < FileSz) then begin
      lMSBch := chr(lCharRA^[linPos+1]);
      if lMSBCh = 'L' then lDicomData.Little_Endian := 1;
      if lMSBCh = 'M' then begin
         lDicomData.Little_Endian := 0;
      end;
      linPos := lInPos + 2;
     end;
     //littleendian
  end;
  if lUpCaseStr ='NAME=ORIGIN'then begin
     readAFNIeoln;
     lOri[1] := (abs(readAFNIFloat));
     lOri[2] := (abs(readAFNIFloat));
     lOri[3] := (abs(readAFNIFloat));
     //Xori,YOri,ZOri
  end;
  if lUpCaseStr ='NAME=DELTA'then begin
     readAFNIeoln;
     lDICOMdata.XYZmm[1] := abs(readAFNIFloat);
     lDICOMdata.XYZmm[2] := abs(readAFNIFloat);
     lDICOMdata.XYZmm[3] := abs(readAFNIFloat);

     //Xmm,Ymm,Zmm
  end;
  if lUpCaseStr ='NAME=ORIENT_SPECIFIC'then begin
     readAFNIeoln;
     lRotation1 := round(readAFNIFloat);
     lRotation2 := round(readAFNIFloat);
     lRotation3 := round(readAFNIFloat);
  end; //ORIENT_SPECIFIC rotation details
  if lInStr <> '' then
     lDynStr := lDynStr + lInStr+kCr;
until (linPos >= FileSz) or (lHdrEnd){EOF(fp)};
//xlDicomData.Storedbits_per_pixel := lDicomData.Allocbits_per_pixel;
for lInPos := 1 to 3 do begin
    if lOri[lInPos] < -6666 then //value not set
       lDICOMdata.XYZori[lInPos] := round((1.0+lDICOMdata.XYZdim[lInPos])/2)
    else if lDICOMdata.XYZmm[lInPos] <> 0 then
       lDICOMdata.XYZori[lInPos] := round(1.5+lOri[lINPos] / lDICOMdata.XYZmm[lInPos]);
end;
//   lDicomData.Float := true;
FreeMem( lCharRA);
end; //interfile
//afni end
//voxbo start
procedure read_voxbo_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string);
label 333;
const UNIXeoln = chr(10);
     kTab = chr(9);
var lTmpStr,lInStr,lUpCaseStr: string;
lFileTypeKnown,lHdrEnd,lFloat: boolean;
lStartPos,lPos,lLen,FileSz,linPos: integer;
fp: file;
lCharRA: bytep;
procedure readVBfloats (var lF1,lF2,lF3: double);
//  While (lPos <= lLen) and ((lInStr[lPos] = kTab) or (lInStr[lPos] = ' ')) do begin
//        inc(lPos);
var  //lDigit : boolean;
   n,lItemIndex: integer;
   lStr,lfStr: string;
begin
    lf1 := 1;
    lf2 := 1;
    lf3 := 1;
 n := 0;
 for lItemIndex := 1 to 3 do begin
    inc(n);
    While (lPos <= lLen) and ((lInStr[lPos] = kTab) or (lInStr[lPos] = ' ')) do
        inc(lPos);
    if lPos > lLen then
       exit;
    lStr := '';
    repeat
        lStr := lStr+upcase(linStr[lPos]);
        inc(lPos);
    until (lPos > lLen) or (lInStr[lPos] = kTab) or (lInStr[lPos] = ' ');
    if lStr <> '' then begin //string to convert
       try
          case n of
               1: lF1 := strtofloat(lStr);
               2: lF2 := strtofloat(lStr);
               3: lF3 := strtofloat(lStr);
          end;
       except
          on EConvertError do begin
             Msg('Unable to convert the string '+lfStr+' to a real number');
             exit;
          end;
       end; {except}
    end; //if string to convert
 end;
end;

procedure readVBints (var lI1,lI2,lI3: integer);
var lF1,lF2,lF3: double;
begin
     readVBfloats (lF1,lF2,lF3);
     lI1 := round(lF1);
     lI2 := round(lF2);
     lI3 := round(lF3);
end;
function readVBStr:string;
var lStr: string;
begin
  lStr := '';
  While (lPos <= lLen) and ((lInStr[lPos] = kTab) or (lInStr[lPos] = ' ')) do begin
        inc(lPos);
  end;
  While (lPos <= lLen) {and (lInStr[lPos] <> ';')} do begin
        lStr := lStr+upcase(linStr[lPos]); //zebra upcase
        inc(lPos);
  end;
  result := lStr;
end; //interstr func
begin
  lHdrOK := false;
  lFloat := false;
  lImageFormatOK := true;
  Clear_Dicom_Data(lDicomData);
  lDynStr := '';
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FileSz := FileSize(fp);
  lHdrEnd := false;
  //lDicomData.ImageStart := FileSz;
  GetMem( lCharRA, FileSz+1 );
  BlockRead(fp, lCharRA^, FileSz, linpos);
  if lInPos <> FileSz then Msg('Disk error: Unable to read full input file.');
  linPos := 1;
  CloseFile(fp);
  FileMode := 2; //set to read/write
  lFileTypeKnown := false;
repeat
  linstr := '';

  while (linPos < FileSz) and (lCharRA^[linPos] <> ord(kCR)) and (lCharRA^[linPos] <> ord(UNIXeoln)) do begin
      lInStr := lInstr + chr(lCharRA^[linPos]);
      inc(linPos);
  end;
  inc(lInPos);  //read EOLN
  lLen := length(lInStr);
  lPos := 1;
  lUpcaseStr := '';
  While (lPos <= lLen) and (lInStr[lPos] <> ':') do begin
        if lInStr[lPos] in ['[',']','(',')','/','+','-', '0'..'9','a'..'z','A'..'Z'] then
           lUpCaseStr := lUpCaseStr+upcase(linStr[lPos]);
        inc(lPos);
  end;
  inc(lPos); {read equal sign in := statement}
  if (lHdrOK) and (not lFileTypeKnown) and (lUpCaseStr = 'CUB1') then
     lFileTypeKnown := true;
  if (lHdrOK) and (not lFileTypeKnown) then begin
     Msg('This software can not read this kind of VoxBo image. (Type:"'+lUpCaseStr+'")');
     lHdrEnd := true;
     lHdrOK := false;
  end;
  if (not lHdrOK) and (lUpCaseStr ='VB98') then begin
     lDicomData.little_endian := 0;//all VoxBo files are Big Endian!
     lStartPos := linPos;
     lFileTypeKnown := true; //test for While Loop
     while (linPos < FileSz) and lFileTypeKnown do begin
           if (lCharRA^[linPos-1] = $0C) and (lCharRA^[linPos] = $0A) then begin
              lFileTypeKnown := false;
              lDicomData.ImageStart := linPos;
              FileSz := linPos;  //size of VoxBo header
           end;
           inc(linPos);
     end;
     if lFileTypeKnown then begin //end of file character not found: abort!
           Msg('Unable to find the end of the VoxBo header.');
           lHdrEnd := true
     end else
           lHdrOK := true;
     linPos := lStartPos; //now that we have found the header size, we can start from the beginning of the header
  end;
  if not lHdrOK then lHdrEnd := true;
  if (lUpCaseStr ='BYTEORDER') and (readVBStr = 'LSBFIRST') then
     lDicomData.little_endian := 1;
  if lUpCaseStr ='DATATYPE'then begin
     lTmpStr := readVBStr;
     if lTmpStr = 'Byte' then
        lDicomData.Allocbits_per_pixel := 8
     else if (lTmpStr = 'INTEGER') or (lTmpStr = 'INT16')  then
        lDicomData.Allocbits_per_pixel := 16
     else if (lTmpStr = 'LONG') or (lTmpStr = 'INT32')  then
        lDicomData.Allocbits_per_pixel := 32
     else if (lTmpStr = 'FLOAT')  then begin
        lFloat := true;
        lDicomData.Allocbits_per_pixel := 32;
     end else if (lTmpStr = 'DOUBLE')  then begin
        lFloat := true;
        lDicomData.Allocbits_per_pixel := 64;
     end else begin
         Msg('Unknown VoxBo data format: '+lTmpStr);
     end;
  end;
  if lUpCaseStr ='VOXDIMS(XYZ)'then readVBints(lDicomData.XYZdim[1],lDicomData.XYZdim[2],lDicomData.XYZdim[3]);
  if (lUpCaseStr ='VOXSIZES(XYZ)') then readVBfloats(lDicomData.XYZmm[1],lDicomData.XYZmm[2],lDicomData.XYZmm[3]);
  if (lUpCaseStr ='ORIGIN(XYZ)')then begin
     readVBints(lDicomData.XYZori[1],lDicomData.XYZori[2],lDicomData.XYZori[3]);
     inc(lDicomData.XYZori[1]);//1393
     inc(lDicomData.XYZori[2]);//1393
     inc(lDicomData.XYZori[3]);//1393
  end;
  if not lHdrOK then goto 333;
  if lInStr <> '' then
     lDynStr := lDynStr + lInStr+kCr;
until (linPos >= FileSz) or (lHdrEnd){EOF(fp)};
//xlDicomData.Storedbits_per_pixel := lDicomData.Allocbits_per_pixel;
//xlDicomData.Rotate180deg := true;
lImageFormatOK := true;
if (lFloat) then begin //zebra change float check
   lDicomData.FloatData := true;
   //lImageFormatOK := false;
end;
333:
FreeMem( lCharRA);
end;
//voxbo end

procedure read_vff_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string);
label 333;
const UNIXeoln = chr(10);
var lInStr,lUpCaseStr: string;
//lHdrEnd: boolean;
lPos,lLen,FileSz,linPos: integer;
lDummy1,lDummy2,lDummy3 : double;
fp: file;
lCharRA: bytep;
procedure readVFFvals (var lFloat1,lFloat2,lFloat3: double);
var lStr: string;
    lDouble: DOuble;
    lInc: integer;
begin
 for lInc := 1 to 3 do begin
  lStr := '';
  While (lPos <= lLen) and (lInStr[lPos] = ' ') do begin
        inc(lPos);
  end;
  While (lPos <= lLen) and (lInStr[lPos] <> ';') and (lInStr[lPos] <> ' ') do begin
        if lInStr[lPos] in ['+','-','e','E','.','0'..'9'] then
           lStr := lStr+(linStr[lPos]);
        inc(lPos);
  end;
  if lStr <> '' then begin
    try
       lDouble := strtofloat(lStr);
    except
          on EConvertError do begin
             Msg('Unable to convert the string '+lStr+' to a number');
             exit;
          end;
    end; {except}
    case lInc of
         2: lFloat2 := lDouble;
         3: lFloat3 := lDouble;
         else lFloat1 := lDouble;
    end;
  end; //lStr <> ''
 end; //lInc 1..3
end; //interstr func
begin
  lHdrOK := false;
  lImageFormatOK := true;
  Clear_Dicom_Data(lDicomData);
  lDicomData.little_endian := 0; //big-endian
  lDynStr := '';
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FileSz := FileSize(fp);
  if FileSz > 2047 then FileSz := 2047;
  GetMem( lCharRA, FileSz+1 );
  BlockRead(fp, lCharRA^, FileSz, linpos);
  if lInPos <> FileSz then Msg('Disk error: Unable to read full input file.');
  lInPos := 1;
  while (lCharRA^[lInPos] <> 12) and (lInPos < FileSz) do begin
      inc(lInPos);
  end;
  inc(lInPos);
  if (lInPos >= FileSz) or (lInPos < 12) then goto 333; //unable to find
  lDynStr := lDynStr + 'Sun VFF Volume File Format'+kCr;
  lDicomData.ImageStart := lInPos;
  FileSz := lInPos-1;
  linPos := 1;
  CloseFile(fp);
  FileMode := 2; //set to read/write
repeat
  linstr := '';
  while (linPos < FileSz) and (lCharRA^[linPos] <> ord(kCR)) and (lCharRA^[linPos] <> ord(UNIXeoln)) do begin
      lInStr := lInstr + chr(lCharRA^[linPos]);
      inc(linPos);
  end;
  inc(lInPos);  //read EOLN
  lLen := length(lInStr);
  lPos := 1;
  lUpcaseStr := '';
  While (lPos <= lLen) and (lInStr[lPos] <> ';') and (lInStr[lPos] <> '=') and (lUpCaseStr <>'NCAA') do begin
        if lInStr[lPos] in ['[',']','(',')','/','+','-',{' ',} '0'..'9','a'..'z','A'..'Z'] then
           lUpCaseStr := lUpCaseStr+upcase(linStr[lPos]);
        inc(lPos);
  end;
  inc(lPos); {read equal sign in := statement}
  if lUpCaseStr ='NCAA' then begin
     lHdrOK := true;
     end;
  if lUpCaseStr ='BITS' then begin
      lDummy1 := 8;
      readVFFvals(lDummy1,lDummy2,lDummy3);
      lDicomData.Allocbits_per_pixel := round(lDummy1);
  end;
  if lUpCaseStr ='SIZE' then begin
     lDummy1 := 1; lDummy2 := 1; lDummy3 := 1;
     readVFFvals(lDummy1,lDummy2,lDummy3);
     lDicomData.XYZdim[1] := round(lDummy1);
     lDicomData.XYZdim[2] := round(lDummy2);
     lDicomData.XYZdim[3] := round(lDummy3);
  end;
  if lUpCaseStr ='ASPECT' then begin
     lDummy1 := 1; lDummy2 := 1; lDummy3 := 1;
     readVFFvals(lDummy1,lDummy2,lDummy3);
     lDicomData.XYZmm[1] := (lDummy1);
     lDicomData.XYZmm[2] := (lDummy2);
     lDicomData.XYZmm[3] := (lDummy3);
  end;
  if not lHdrOK then goto 333;
  if lInStr <> '' then
     lDynStr := lDynStr + lInStr+kCr;
  //lHdrOK := true;
until (linPos >= FileSz);
//xlDicomData.Storedbits_per_pixel := lDicomData.Allocbits_per_pixel;
lImageFormatOK := true;
333:
FreeMem( lCharRA);
end;
//********************************************************************
(*procedure ShellSortItems (first, last: integer; var lPositionRA, lIndexRA: LongintP; var lRepeatedValues: boolean);
{Shell sort chuck uses this- see 'Numerical Recipes in C' for similar sorts.}
label
     555;
const
     tiny = 1.0e-5;
     aln2i = 1.442695022;
var
   n,t, nn, m, lognb2, l, k, j, i: longint;
begin
     lRepeatedValues := false;
     n := abs(last - first + 1);
     lognb2 := trunc(ln(n) * aln2i + tiny);
     m := last;
     for nn := 1 to lognb2 do
         begin
              m := m div 2;
              k := last - m;
              for j := 1 to k do begin
                  i := j;
                  555: {<- LABEL}
                  l := i + m;
                  if  (lIndexRA^[lPositionRA^[l]] = lIndexRA^[lPositionRA^[i]]) then begin
                      lRepeatedValues := true;
                      exit;
                  end;
                  if (lIndexRA^[lPositionRA^[l]] < lIndexRA^[lPositionRA^[i]]) then begin
                     //swap values for i and l
                     t := lPositionRA^[i];
                     lPositionRA^[i] := lPositionRA^[l];
                     lPositionRA^[l] := t;
                     i := i - m;
                     if (i >= 1) then
                        goto 555;
                  end
              end
         end
end; //shellsort is fast and requires less memory than quicksort *)


(*procedure PAR2DICOMstudyDate(var lDicomData: DICOMdata);
{input: lDicomData.StudyDate =  2002.12.29 / 19:48:58.0000
output: StudyDate = YYYYMMDD StudyTime= hhmmss }
var
 I: integer;
	lStr: string;
begin
	if length(lDicomData.StudyDate) < 14 then exit;
	lStr := '';
	for I := 1 to length(lDicomData.StudyDate) do
		if lDicomData.StudyDate[I] in ['0'..'9'] then
			lStr := lStr+ lDicomData.StudyDate[I];
	if length(lStr) < 14 then exit;
	lDicomData.StudyDate := '';
	for I := 1 to 8 do
		lDicomData.StudyDate := lDicomData.StudyDate+lStr[I];
	lDicomData.StudyTime := '';
	for I := 9 to 14 do
		lDicomData.StudyTime := lDicomData.StudyTime+lStr[I];
	lDicomData.PatientIDInt := StudySecSince2K(lDicomData.StudyDate,lDicomData.StudyTime);
end;
type tRange = record
     Min,Val,Max: double; //some vals are ints, others floats
end;

procedure read_PAR_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK:boolean;  var lDynStr: string;var lFileName: string; lReadOffsetTables: boolean; var lOffset_pos_table: LongIntp; var lOffsetTableEntries: integer; lReadVaryingScaleFactors: boolean; var lVaryingScaleFactors_table,lVaryingIntercept_table: Singlep; var lVaryingScaleFactorsTableEntries,lnum4Ddatasets: integer);
label 333; //1384 now reads up to 8 dimensional data....
const UNIXeoln = chr(10);
     kMaxnSLices = 32000;
     kXdim = 1;
     kYdim = 2;
     kBitsPerVoxel = 3;
     kSliceThick = 4;
     kSliceGap = 5;
     kXmm = 6;
     kYmm = 7;
     kSlope = 8;
     kIntercept = 9;
     kCalibratedSlope = 10; //1393 - attempt to use calibrated values
     kDynTime = 11;
     kSlice = 12;
     kEcho = 13;
     kDyn = 14;
     kCardiac = 15;
     kType = 16;
     kSequence = 17;
     kIndex = 18;
     lIsParVers3x: boolean = true;
     lRepeatedValues : boolean = false;
     lSlicesNotInSequence: boolean = false;
     lMaxSlice : integer = 0;
var
   lErrorStr,lInStr,lUpCaseStr,lReportedTRStr: string;
   lSliceSequenceRA,lSortedSliceSequence: LongintP;
   lSliceIndexRA: array [1..kMaxnSlices] of Longint;
   lSliceSlopeRA,lSliceInterceptRA,lCalibratedSliceSlopeRA: array [1..kMaxnSlices] of single;
   lSliceHeaderRA: array [1..32] of double;
   lRangeRA: array [kXdim..kIndex] of tRange;
   lMaxIndex,lSliceSz,lSliceInfoCount,lPos,lLen,lFileSz,lHdrPos,linPos,lInc: LongInt;
   fp: file;
   lCharRA: bytep;

procedure MinMaxTRange (var lDimension: tRange;  lNewVal: double); //nested
begin
     lDimension.Val := lNewVal;
     if lSliceInfoCount < 2 then begin
        lDimension.Min := lDimension.Val;
        lDimension.Max := lDimension.Val;
     end;
     if lNewVal < lDimension.Min then lDimension.Min := lNewVal;
     if lNewVal > lDimension.Max then lDimension.Max := lNewVal;
end; //nested InitTRange proc

function readParStr:string;//nested
var lStr: string;
begin
  lStr := '';
  While (lPos <= lLen) do begin
        if (lStr <> '') or (linStr[lPos]<>' ') then //strip leading spaces
           lStr := lStr+(linStr[lPos]);
        inc(lPos);
  end; //while lPOs < lLen
   result := lStr;
end; //nested func ReadParStr
function readParFloat:double;//nested
var lStr: string;
begin
  lStr := '';
  result := 1;
  While (lPos <= lLen) and ((lStr='')  or(lInStr[lPos] <> ' ')) do begin
        if lInStr[lPos] in ['+','-','e','E','.','0'..'9'] then
           lStr := lStr+(linStr[lPos]);
        inc(lPos);
  end;
  if lStr = '' then exit;
    try
       result := strtofloat(lStr);
    except
          on EConvertError do begin
             Msg('read_PAR_data: Unable to convert the string '+lStr+' to a number');
             result := 1;
             exit;
          end;
    end; {except}
end; //nested func ReadParFloat
begin
  //Initialize parameters
  lnum4Ddatasets := 1;
  lSliceInfoCount := 0;
  for lInc := kXdim to kIndex do //initialize all values: important as PAR3 will not explicitly report all
      MinMaxTRange(lRangeRA[lInc],0);
  lHdrOK := false;
  lImageFormatOK := false;
  lIsParVers3x := true;
  lOffsetTableEntries := 0;
  lVaryingScaleFactorsTableEntries := 0;
  Clear_Dicom_Data(lDicomData);
  lDynStr := '';
  //Read text header to buffer (lCharRA)
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  lFileSz := FileSize(fp);
  GetMem( lCharRA, lFileSz+1 ); //note: must free dynamic memory: goto 333 if any error
  GetMem (lSliceSequenceRA, kMaxnSLices*sizeof(longint));  //note: must free dynamic memory: goto 333 if any error
  BlockRead(fp, lCharRA^, lFileSz, lInpos);
  if lInPos <> lFileSz then begin
     Msg('read_PAR_data: Disk error, unable to read full input file.');
     goto 333;
  end;
  linPos := 1;
  CloseFile(fp);
  FileMode := 2; //set to read/write
  //Next: read each line of header file...
  repeat //for each line in file....
    linstr := '';
    while (linPos < lFileSz) and (lCharRA^[linPos] <> ord(kCR)) and (lCharRA^[linPos] <> ord(UNIXeoln)) do begin
      lInStr := lInstr + chr(lCharRA^[linPos]);
      inc(linPos);
    end;
    inc(lInPos);  //read EOLN
    lLen := length(lInStr);
    lPos := 1;
    lUpcaseStr := '';
    if lLen < 1 then
       //ignore blank lines
    else if (lInStr[1] = '*') and (not lHdrOK) then  //# -> comment
         //ignore comment lines prior to start of header
    else if (lInStr[1] = '#') and (lHdrOK) then  //# -> comment
         //ignore comment lines
    else if (lInStr[1] = '.') or (not lHdrOK) then  begin  //  GENERAL_INFORMATION section (line starts with '.')
      //Note we also read in lines that do not have '.' if we have HdrOK=false, this allows us to detect the DATADESCRIPTIONFILE signature
      While (lPos <= lLen) and (lInStr[lPos] <> ':') and ((not lHdrOK) or (lInStr[lPos] <> '#')) do begin
        if lInStr[lPos] in ['[',']','(',')','/','+','-',{' ',} '0'..'9','a'..'z','A'..'Z'] then
           lUpCaseStr := lUpCaseStr+upcase(linStr[lPos]);
        inc(lPos);
      end; //while reading line
      inc(lPos); {read equal sign in := statement}
               lDynStr := lDynStr + lInStr+kCR;
      if (not lHdrOK) and (lUpcaseStr = ('DATADESCRIPTIONFILE')) then begin //1389 PAR file
            lHdrOK := true;
            lDicomData.little_endian := 1;
      end;



      if (lUpCaseStr ='REPETITIONTIME[MSEC]') then
         lDicomData.TR :=  round(readParFloat);
      if (lUpCaseStr ='MAXNUMBEROFSLICES/LOCATIONS') then
         lDicomData.XYZdim[3] :=  round(readParFloat);
      if (lUpCaseStr ='SLICETHICKNESS[MM]') then
         MinMaxTRange(lRangeRA[kSliceThick],readParFloat);
      if (lUpCaseStr ='SLICEGAP[MM]') then
         MinMaxTRange(lRangeRA[kSliceGap],readParFloat);
      if lUpCaseStr = 'RECONRESOLUTION(XY)' then begin
         MinMaxTRange(lRangeRA[kXdim],readParFloat);
         MinMaxTRange(lRangeRA[kYdim],readParFloat);
	  end;
      if lUpCaseStr = 'RECONSTRUCTIONNR' then
         lDicomData.AcquNum :=  round(readParFloat);
      if lUpCaseStr = 'ACQUISITIONNR' then
         lDicomData.SeriesNum :=  round(readParFloat);
      if lUpCaseStr = 'MAXNUMBEROFDYNAMICS' then begin
         lDicomData.XYZdim[4] :=  round(readParFloat);
      end;
	  if lUpCaseStr = 'EXAMINATIONDATE/TIME' then begin
		 lDicomData.StudyDate := readParStr;
		 PAR2DICOMstudyDate(lDicomData);
	  end;
      //if lUpCaseStr = 'PROTOCOLNAME' then
      //   lDicomData.modality := readParStr;
      if lUpCaseStr = 'PATIENTNAME' then
         lDicomData.PatientName := readParStr;
      if lUpCaseStr ='IMAGEPIXELSIZE[8OR16BITS]' then begin
         MinMaxTRange(lRangeRA[kBitsPerVoxel],readParFloat);
      end;
      if not lHdrOK then  begin
         Msg('read_PAR_data: Error reading header');
         goto 333;
      end;
    end else begin  //SliceInfo: IMAGE_INFORMATION (line does NOT start with '.' or '#')
         inc(lSliceInfoCount);
         if (lSliceInfoCount < 2) and (lRangeRA[kBitsPerVoxel].val < 1) then //PARvers3 has imagedepth in general header, only in image header for later versions
            lIsParVers3x := false;
         for lHdrPos := 1 to 26 do
             lSliceHeaderRA[lHdrPos] := readparfloat;
         //The next few values are in the same location for both PAR3 and PAR4
         MinMaxTRange(lRangeRA[kSlice], round(lSliceHeaderRA[1]));
         MinMaxTRange(lRangeRA[kEcho], round(lSliceHeaderRA[2]));
         MinMaxTRange(lRangeRA[kDyn], round(lSliceHeaderRA[3]));
         MinMaxTRange(lRangeRA[kCardiac], round(lSliceHeaderRA[4]));
         MinMaxTRange(lRangeRA[kType], round(lSliceHeaderRA[5]));
         MinMaxTRange(lRangeRA[kSequence], round(lSliceHeaderRA[6]));
         MinMaxTRange(lRangeRA[kIndex], round(lSliceHeaderRA[7]));
         if lIsParVers3x then begin //Read PAR3 data
            MinMaxTRange(lRangeRA[kIntercept], lSliceHeaderRA[8]);; //8=intercept in PAR3
            MinMaxTRange(lRangeRA[kSlope],lSliceHeaderRA[9]); //9=slope in PAR3
            MinMaxTRange(lRangeRA[kCalibratedSlope],lSliceHeaderRA[10]);  //10=lcalibrated slope in PAR3 1393 - attempt to use calibrated values
            MinMaxTRange(lRangeRA[kXmm],lSliceHeaderRA[23]); //23 PIXEL SPACING X  in PAR3
            MinMaxTRange(lRangeRA[kYmm],lSliceHeaderRA[24]); //24 PIXEL SPACING Y IN PAR3
            MinMaxTRange(lRangeRA[kDynTime],(lSliceHeaderRA[26]));  //26= dyn_scan_begin_time in PAR3
         end else begin  //not PAR: assume PAR4
            for lHdrPos := 27 to 32 do
                lSliceHeaderRA[lHdrPos] := readparfloat;
            MinMaxTRange(lRangeRA[kBitsPerVoxel],lSliceHeaderRA[8]);//8 BITS in PAR4
            MinMaxTRange(lRangeRA[kXdim], lSliceHeaderRA[10]); //10 XDim in PAR4
            MinMaxTRange(lRangeRA[kYdim], lSliceHeaderRA[11]); //11 YDim in PAR4
            MinMaxTRange(lRangeRA[kIntercept],lSliceHeaderRA[12]); //12=intercept in PAR4
            MinMaxTRange(lRangeRA[kSlope],lSliceHeaderRA[13]); //13=lslope in PAR4
            MinMaxTRange(lRangeRA[kCalibratedSlope],lSliceHeaderRA[14]);  //14=lcalibrated slope in PAR4 1393 - attempt to use calibrated values
            MinMaxTRange(lRangeRA[kSliceThick],lSliceHeaderRA[23]);//23 SLICE THICK in PAR4
            MinMaxTRange(lRangeRA[kSliceGap], lSliceHeaderRA[24]); //24 SLICE GAP in PAR4
            MinMaxTRange(lRangeRA[kXmm],lSliceHeaderRA[29]); //29 PIXEL SPACING X  in PAR4
            MinMaxTRange(lRangeRA[kYmm],lSliceHeaderRA[30]); //30 PIXEL SPACING Y in PAR4
            MinMaxTRange(lRangeRA[kDynTime],(lSliceHeaderRA[32]));//32= dyn_scan_begin_time in PAR4
         end; //PAR4
         if lSliceInfoCount < kMaxnSlices then begin
            lSliceSequenceRA^[lSliceInfoCount] := ( (round(lRangeRA[kSequence].val)+round(lRangeRA[kType].val)+round(lRangeRA[kCardiac].val+lRangeRA[kEcho].val)) shl 24)+(round(lRangeRA[kDyn].val) shl 10)+round(lRangeRA[kSlice].val);
            lSliceSlopeRA [lSliceInfoCount] := lRangeRA[kSlope].Val;
            lCalibratedSliceSlopeRA [lSliceInfoCount] := lRangeRA[kCalibratedSlope].Val;
            lSliceInterceptRA [lSliceInfoCount] := lRangeRA[kIntercept].val;
            lSliceIndexRA[lSliceInfoCount]:= round(lRangeRA[kIndex].val);
         end;
    end; //SliceInfo Line
  until (linPos >= lFileSz);//until done reading entire file...
  //describe generic DICOM parameters
  lDicomData.XYZdim[1] := round(lRangeRA[kXdim].Val);
  lDicomData.XYZdim[2] := round(lRangeRA[kYdim].Val);
  lDicomData.XYZdim[3] := 1+round(lRangeRA[kSlice].Max-lRangeRA[kSlice].Min);
  if (lSliceInfoCount mod lDicomData.XYZdim[3]) <> 0 then
     Msg('read_PAR_data: Total number of slices not divisible by number of slices per volume. Reconstruction error?');
  if lDicomData.XYZdim[3] > 0 then
     lDicomData.XYZdim[4] := lSliceInfoCount div lDicomData.XYZdim[3] //nVolumes = nSlices/nSlicePerVol
  else
      lDicomData.XYZdim[4] := 1;

  lDicomData.XYZmm[1] := lRangeRA[kXmm].Val;
  lDicomData.XYZmm[2] := lRangeRA[kYmm].Val;
  lDicomData.XYZmm[3] := lRangeRA[kSliceThick].Val+lRangeRA[kSliceGap].Val;
  lDicomData.Allocbits_per_pixel :=  round(lRangeRA[kBitsPerVoxel].Val);
  lDicomData.IntenScale := lRangeRA[kSlope].Val;
  lDicomData.IntenIntercept := lRangeRA[kIntercept].Val;
if gPARprecise then begin
  if (lDicomData.IntenIntercept <> 0) or (lRangeRA[kCalibratedSlope].val = 0) then
     Msg('Warning: Unable to save calibrated Philips image intensity (non-zero scaling intercept). Turn off Etc/Options/CalibratedScaling to hide warning.');
  if (lRangeRA[kSlope].min  = lRangeRA[kSlope].max)
     and (lRangeRA[kIntercept].min = lRangeRA[kIntercept].max)
     and (lRangeRA[kCalibratedSlope].min = lRangeRA[kCalibratedSlope].max)
     and (lDicomData.IntenIntercept = 0) and (lRangeRA[kCalibratedSlope].val <> 0) then
      lDicomData.IntenScale := 1 / lRangeRA[kCalibratedSlope].val;
end; //if PARprecise
  //Next: report number of Dynamic scans, this allows people to parse DynScans from Type/Cardiac/Echo/Sequence 4D files
  lnum4Ddatasets := (round(lRangeRA[kDyn].Max - lRangeRA[kDyn].Min)+1)*lDicomData.XYZdim[3]; //slices in each dynamic session
  if ((lSliceInfoCount mod lnum4Ddatasets) = 0) and ((lSliceInfoCount div lnum4Ddatasets) > 1) then
    lnum4Ddatasets := (lSliceInfoCount div lnum4Ddatasets) //infer multiple Type/Cardiac/Echo/Sequence
  else
      lnum4Ddatasets := 1;
  //next: Determine actual interscan interval
  if (lDicomData.XYZdim[4] > 1) and ((lRangeRA[kDynTime].max-lRangeRA[kDynTime].min)> 0)  {1384} then begin
        lReportedTRStr := 'Reported TR: '+floattostrf(lDicomData.TR,ffFixed,8,2)+kCR;
        lDicomData.TR := (lRangeRA[kDynTime].max-lRangeRA[kDynTime].min)  /(lDicomData.XYZdim[4] - 1)*1000; //infer TR in ms
  end else
         lReportedTRStr :='';
  //next: report header details
  lDynStr := 'Philips PAR/REC Format' //'PAR/REC Format'
              +kCR+ 'Patient name:'+lDicomData.PatientName
              +kCR+ 'XYZ dim: ' +inttostr(lDicomData.XYZdim[1])+'/'+inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
              +kCR+'Volumes: ' +inttostr(lDicomData.XYZdim[4])
              +kCR+'XYZ mm: '+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
              +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2)
              +kCR+'TR: '+floattostrf(lDicomData.TR,ffFixed,8,2)
              +kCR+lReportedTRStr+kCR+lDynStr;
  //if we get here, the header is fine, next steps will see if image format is readable...
  lHdrOK := true;
  if lSliceInfoCount < 1 then
     goto 333;
  //next: see if slices are in sequence
  lSlicesNotInSequence := false;
  if lSliceInfoCount > 1 then begin
     lMaxSlice :=  lSliceSequenceRA^[1];
     lMaxIndex := lSliceIndexRA[1];
     lInc := 1;
     repeat
        inc(lInc);
        if lSliceSequenceRA^[lInc] < lMaxSlice then //not in sequence if image has lower slice order than previous image
           lSlicesNotInSequence := true
        else
           lMaxSlice := lSliceSequenceRA^[lInc];
        if lSliceIndexRA[lInc] < lMaxIndex then //not in sequence if image has lower slice index than previous image
           lSlicesNotInSequence := true
        else
           lMaxIndex := lSliceIndexRA[lInc];
     until (lInc = lSliceInfoCount) or (lSlicesNotInSequence);
  end; //at least 2 slices
  //Next: report any errors
  lErrorStr := '';
  if (lSlicesNotInSequence) and (not lReadOffsetTables) then
     lErrorStr := lErrorStr + ' Slices not saved sequentially [using MRIcro''s ''Philips PAR to Analyze'' command may solve this]'+kCR;
  if lSliceInfoCount > kMaxnSlices then
     lErrorStr := lErrorStr + ' Too many slices: >'+inttostr(kMaxnSlices)+kCR;
  if (not lReadVaryingScaleFactors) and (  (lRangeRA[kSlope].min <> lRangeRA[kSlope].max)
    or (lRangeRA[kIntercept].min <> lRangeRA[kIntercept].max)) then
     lErrorStr := lErrorStr + ' Differing intensity slope/intercept [using MRIcro''s ''Philips PAR to Analyze'' command may solve this]'+kCR;
  if (lRangeRA[kBitsPerVoxel].min <> lRangeRA[kBitsPerVoxel].max) then  //5D file space+time+cardiac
     lErrorStr := lErrorStr + ' Differing bits per voxel'+kCR;
  //if (lRangeRA^[kCardiac].min <> lRangeRA^[kCardiac].max) then  //5D file space+time+cardiac
  //   lErrorStr := lErrorStr + 'Multiple cardiac timepoints'+kCR;
  //if (lRangeRA^[kEcho].min <> lRangeRA^[kEcho].max) then  //5D file space+time+echo
  //   lErrorStr := lErrorStr + 'Multiple echo timepoints'+kCR;
  if (lRangeRA[kSliceThick].min <> lRangeRA[kSliceThick].max) or (lRangeRA[kSliceGap].min <> lRangeRA[kSliceGap].max)
    or (lRangeRA[kXdim].min <> lRangeRA[kXdim].max) or (lRangeRA[kYDim].min <> lRangeRA[kYDim].max)
    or (lRangeRA[kXmm].min <> lRangeRA[kXmm].max) or (lRangeRA[kYmm].min <> lRangeRA[kYmm].max) then
     lErrorStr := lErrorStr + ' Multiple/varying slice dimensions'+kCR;
  //if any errors were encountered, report them....
  if lErrorStr <> '' then begin
      Msg('read_PAR_data: This software can not convert this Philips data:'+kCR+lErrorStr);
      goto 333;
  end;
  //Next sort image indexes here...
  if (lSliceInfoCount > 1) and(lSlicesNotInSequence) and ( lReadOffsetTables) then begin //sort image order...
     //ShellSort (first, last: integer; var lPositionRA, lIndexLoRA,lIndexHiRA: LongintP; var lRepeatedValues: boolean)
     GetMem (lOffset_pos_table, lSliceInfoCount*sizeof(longint));
     for lInc := 1 to  lSliceInfoCount do
         lOffset_pos_table^[lInc] := lInc;
     ShellSortItems (1, lSliceInfoCount,lOffset_pos_table,lSliceSequenceRA, lRepeatedValues);
     if lRepeatedValues then begin
         Msg('read_PAR_data: fatal error, slices do not appear to have unique indexes [multiple copies of same slice]');
         FreeMem (lOffset_pos_table);
         goto 333;
     end;
     lOffsetTableEntries := lSliceInfoCount;
  end; //sort image order...
  //Next, generate list of scale slope
  if  (lSliceInfoCount > 1) and (lReadVaryingScaleFactors) and ( (lRangeRA[kSlope].min <> lRangeRA[kSlope].max)
    or (lRangeRA[kIntercept].min <> lRangeRA[kIntercept].max))  then begin {create offset LUT}
      lVaryingScaleFactorsTableEntries := lSliceInfoCount;
      getmem (lVaryingScaleFactors_table, lVaryingScaleFactorsTableEntries*sizeof(single));
      getmem (lVaryingIntercept_table, lVaryingScaleFactorsTableEntries*sizeof(single));
      if  lOffsetTableEntries = lSliceInfoCount then begin //need to sort slices

          for lInc := 1 to lSliceInfoCount do begin
              lVaryingScaleFactors_table^[lInc] := lSliceSlopeRA[lOffset_pos_table^[lInc]];
              lVaryingIntercept_table^[lInc] := lSliceInterceptRA[lOffset_pos_table^[lInc]];
if gPARprecise then begin
  if (lVaryingIntercept_table^[lInc] <> 0) or (lCalibratedSliceSlopeRA[lOffset_pos_table^[lInc]]=0) then
     Msg('Warning: Unable to save calibrated Philips image intensity (non-zero scaling intercept). Turn off Etc/Options/CalibratedScaling to hide warning.')
  else begin
      lVaryingScaleFactors_table^[lInc] := 1 / lCalibratedSliceSlopeRA[lOffset_pos_table^[lInc]];
  end;
end; //if PARprecise

          end;
      end else begin //if sorted, else unsorted

          for lInc := 1 to lSliceInfoCount do begin
              lVaryingScaleFactors_table^[lInc] := lSliceSlopeRA[lInc];
              lVaryingIntercept_table^[lInc] := lSliceInterceptRA[lInc];
if gPARprecise then begin
  if (lVaryingIntercept_table^[lInc] <> 0) or (lCalibratedSliceSlopeRA[lInc]=0) then
     Msg('Warning: Unable to save calibrated Philips image intensity (non-zero scaling intercept). Turn off Etc/Options/CalibratedScaling to hide warning.')
  else
      lVaryingScaleFactors_table^[lInc] := 1 / lCalibratedSliceSlopeRA[lInc];
end; //if PARprecise

          end;
      end; //slices sorted
  end;//read scale factors
  //Next: now adjust Offsets to point to byte offset instead of slice number
  lSliceSz := lDicomData.XYZdim[1]*lDicomData.XYZdim[2]*(lDicomData.Allocbits_per_pixel div 8);
  if lOffsetTableEntries = lSliceInfoCount then
          for lInc := 1 to lSliceInfoCount do
              lOffset_pos_table^[lInc] := lSliceSz * (lSliceIndexRA[lOffset_pos_table^[lInc]]);
  //report if 5D/6D/7D file is being saved as 4D
  if (lRangeRA[kCardiac].min <> lRangeRA[kCardiac].max)
    or (lRangeRA[kEcho].min <> lRangeRA[kEcho].max)   //5D file space+time+echo
    or (lRangeRA[kType].min <> lRangeRA[kType].max)   //5D file space+time+echo
    or (lRangeRA[kSequence].min <> lRangeRA[kSequence].max) then  //5D file space+time+echo
      Msg('Warning: note that this image has more than 4 dimensions (multiple Cardiac/Echo/Type/Sequence)');
  //if we get here, the Image Format is OK
  lImageFormatOK := true;
  lFileName := changefileextX(lFilename,'.rec'); //for Linux: case sensitive extension search '.rec' <> '.REC'
 333: //abort clause: skips lHdrOK and lImageFormatOK
 //next: free dynamically allocated memory
 FreeMem( lCharRA);
 FreeMem (lSliceSequenceRA);
end;   *)

procedure read_ge_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
label
  539;
var
  lGap,lSliceThick,lTempFloat: single;
  lTemp16,lI: word;
  lSeriesOffset,lTemp32,lExamHdr,lImgHdr,lDATFormatOffset,lHdrOffset,lCompress,linitialoffset,n,filesz: LongInt;
  tx     : array [0..36] of Char;
  FP: file;
  lGEodd,lGEFlag,{lSpecial,}lMR: boolean;
function GEflag: boolean;
begin
     if (tx[0] = 'I') AND (tx[1]= 'M') AND (tx[2] = 'G')AND (tx[3]= 'F') then
        result := true
     else
         result := false;
end;
function swap16i(lPos: longint): word;
var
   w : Word;
begin
  seek(fp,lPos-2);
  BlockRead(fp, W, 2);
  result := swap(W);
end;

function swap32i(lPos: longint): Longint;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
   s : LongInt;
  inguy:swaptypep;
  outguy:swaptype;
begin
     seek(fp,lPos);
  BlockRead(fp, s, 4, n);
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  swap32i:=outguy.Long;
end;
function fswap4r (lPos: longint): single;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(float:single);
  end;
  swaptypep = ^swaptype;
var
   s:single;
  inguy:swaptypep;
  outguy:swaptype;
begin
     seek(fp,lPos);
  BlockRead(fp, s, 4, n);
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  fswap4r:=outguy.float;
end;
begin
  lImageFormatOK := true;
  lSeriesOffset := 0;
  lSLiceThick := 0;
  lGap := 0;
  lHdrOK := false;
  lHdrOffset := 0;
  if not fileexists(lFileName) then begin
     lImageFormatOK := false;
     exit;
  end;
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FIleSz := FileSize(fp);
  lDATFormatOffset := 0;
  Clear_Dicom_Data(lDicomData);
     if filesz < (3240) then begin
        Msg('This file is too small to be a Genesis DAT format image.');
        goto 539;
     end;
     lDynStr:= '';
     //lGEFlag := false;
     lInitialOffset := 3228;//3240;
     seek(fp, lInitialOffset);
     BlockRead(fp, tx, 4*SizeOf(Char), n);
     lGEflag := GEFlag;
     if not lGEFlag then begin
        lInitialOffset := 3240;
        seek(fp, lInitialOffset);
        BlockRead(fp, tx, 4*SizeOf(Char), n);
        lGEflag := GEFlag;
     end;
     lGEodd := lGEFlag;
     if not lGEFlag then begin
        lInitialOffset := 0;
        seek(fp, lInitialOffset);
        BlockRead(fp, tx, 4*SizeOf(Char), n);
        if not GEflag then begin {DAT format}
           lDynStr := lDynStr+'GE Genesis Signa DAT tape format'+kCR;
           seek(fp,114);
           BlockRead(fp, tx, 4*SizeOf(Char), n);
           lDynStr := lDynStr + 'Suite: ';
           for lI := 0 to 3 do
            lDynStr := lDynStr + tx[lI];
           lDynStr := lDynStr + kCR;

           seek(fp,114+97);
           BlockRead(fp, tx, 25*SizeOf(Char), n);
           lDynStr := lDynStr + 'Patient Name: ';
           for lI := 0 to 24 do
            lDynStr := lDynStr + tx[lI];
           lDynStr := lDynStr + kCR;
           seek(fp,114+84);
           BlockRead(fp, tx, 13*SizeOf(Char), n);
           lDynStr := lDynStr + 'Patient ID: ';
           for lI := 0 to 12 do
               lDynStr := lDynStr + tx[lI];
           lDynStr := lDynStr + kCR;
           seek(fp, 114+305);
           BlockRead(fp, tx, 3*SizeOf(Char), n);
           if (tx[0]='M') and (tx[1] = 'R') then
              lMR := true
           else if (tx[0] = 'C') and(tx[1] = 'T') then
             lMR := false
           else begin
                Msg('Is this a Genesis DAT image? The modality is '+tx[0]+tx[1]+tx[3]
              +'. Expected ''MR'' or ''CT''.');
              goto 539;
           end;
           if lMR then
              lInitialOffset := 3180
           else
               lInitialOffset := 3178;
           seek(fp, lInitialOffset);
           BlockRead(fp, tx, 4*SizeOf(Char), n);
           if (tx[0] <> 'I') OR (tx[1] <> 'M') OR (tx[2] <> 'G') OR (tx[3] <> 'F') then begin
              Msg('This image does not have the required label ''IMGF''. This is not a Genesis DAT image.');
              goto 539;
           end else
        lDicomData.ImageNum := swap16i(2158+12);
        lDicomData.XYZmm[3] := fswap4r (2158+26);// slice thickness mm
        lDicomData.XYZmm[1] := fswap4r (2158+50);// pixel size- X
        lDicomData.XYZmm[2] := fswap4r (2158+54);//pixel size - Y
        lSliceThick := lDicomData.XYZmm[3];
        lGap :=  fswap4r (lHdrOffset+118);//1410 gap thickness mm
        if lGap > 0 then
                  lDicomData.XYZmm[3] := lDicomData.XYZmm[3] + lGap;
        lDATFormatOffset := 4;
        if lMR then begin
          lTemp32 := swap32i(2158+194);
           lDynStr := lDynStr +'TR[usec]: '+inttostr(lTemp32) + kCR;
           lTemp32 := swap32i(2158+198);
           lDynStr := lDynStr +'TInvert[usec]: '+inttostr(lTemp32) + kCR;
           lTemp32 := swap32i(2158+202);
           lDynStr := lDynStr +'TE[usec]: '+inttostr(lTemp32) + kCR;
           lTemp16 := swap16i(2158+210);
           lDynStr := lDynStr +'Number of echoes: '+inttostr(lTemp16) + kCR;
           lTemp16 := swap16i(2158+212);
           lDynStr := lDynStr +'Echo: '+inttostr(lTemp16) + kCR;

           lTempFloat  := fswap4r (2158+50); //not sure why I changed this to 50... 218 in Clunie's Description
           lDynStr := lDynStr +'NEX: '+floattostr(lTempFloat) + kCR;

           seek(fp,2158+308);
           BlockRead(fp, tx, 33*SizeOf(Char), n);
           lDynStr := lDynStr + 'Sequence: ';
           for lI := 0 to 32 do
               lDynStr := lDynStr + tx[lI];
           lDynStr := lDynStr + kCR;


           seek(fp,2158+362);
           BlockRead(fp, tx, 17*SizeOf(Char), n);
           lDynStr := lDynStr + 'Coil: ';
           for lI := 0 to 16 do
               lDynStr := lDynStr + tx[lI];
           lDynStr := lDynStr + kCR;


        end;

     end; {DAT format}
end;
     lDicomData.ImageStart := lDATFormatOffset+linitialoffset + swap32i(linitialoffset+4);//byte displacement to image data
     lDicomData.XYZdim[1] := swap32i(linitialoffset+8); //width
     lDicomData.XYZdim[2] := swap32i(linitialoffset+12);//height
     lDicomData.Allocbits_per_pixel := swap32i(linitialoffset+16);//bits
     //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
     lCompress := swap32i(linitialoffset+20); //compression
     lExamHdr :=  swap32i(linitialoffset+136);
     lImgHdr :=  swap32i(linitialoffset+152);
     if (lImgHdr = 0) and (lDicomData.ImageStart = 8432) then begin
        lDicomData.ImageNum := swap16i(2310+12);
		lDicomData.XYZmm[3] := fswap4r (2310+26);// slice thickness mm
		lDicomData.XYZmm[1] := fswap4r (2310+50);// pixel size- X
		lDicomData.XYZmm[2] := fswap4r (2310+54);//pixel size - Y
		lSliceThick := lDicomData.XYZmm[3];
		lGap :=  fswap4r (lHdrOffset+118);//1410 gap thickness mm
		if lGap > 0 then
		   lDicomData.XYZmm[3] := lDicomData.XYZmm[3] + lGap;

	 end else if {(lSpecial = false) and} (lDATFormatOffset = 0) then begin
		lDynStr := lDynStr+'GE Genesis Signa format'+kCR;
		if (not lGEodd) and (lExamHdr <> 0) then begin
		   lHdrOffset := swap32i(linitialoffset+132);//x132- int ptr to exam heade
//Patient ID
		   seek(fp,lHdrOffset+84);
		   BlockRead(fp, tx, 13*SizeOf(Char), n);
		   lDynStr := lDynStr + 'Patient ID: ';
		   for lI := 0 to 12 do
			lDynStr := lDynStr + tx[lI];
		   lDynStr := lDynStr + kCR;
//Patient Name
		   seek(fp,lHdrOffset+97);
		   BlockRead(fp, tx, 25*SizeOf(Char), n);
		   lDynStr := lDynStr + 'Patient Name: ';
		   for lI := 0 to 24 do
			lDynStr := lDynStr + tx[lI];
		   lDynStr := lDynStr + kCR;
//Patient Age
		lI := swap16i(lHdrOffset+122);
		lDynStr := lDynStr+'Patient Age: '+inttostr(lI)+kCR;
//Modality: MR or CT
		   seek(fp,lHdrOffset+305);
		   BlockRead(fp, tx, 3*SizeOf(Char), n);
		   lDynStr := lDynStr + 'Type: ';
		   for lI := 0 to 1 do
			lDynStr := lDynStr + tx[lI];
		   lDynStr := lDynStr + kCR;
//Read series header
		   lSeriesOffset := swap32i(linitialoffset+144);//read size of series header: only read if >0
		   if lSeriesOffset > 12 then begin
			  lSeriesOffset := swap32i(linitialoffset+140);//read size of series header: only read if >0
			  lI := swap16i(lSeriesOffset+10);
			  //lDynStr := lDynStr+'Series number: '+inttostr(lI)+kCR;
			  lDicomData.SeriesNum := lI;
		   end;


//image data
        lHdrOffset := swap32i(linitialoffset+148);//x148- int ptr to image heade
        end;
        if lGEodd then lHdrOffset := 2158+28;
        if ((lHdrOffset +58) < FileSz) and (lImgHdr <> 0) then begin
           lDicomData.AcquNum := swap16i(lHdrOffset+12); //note SERIES not IMAGE number, despite what Clunies FAQ says
           lDicomData.ImageNum := swap16i(lHdrOffset+14); //this is IMAGEnum

           //lDynStr := lDynStr +'Image number: '+inttostr(lDicomData.ImageNum)+ kCR;
           lDicomData.XYZmm[3] := fswap4r (lHdrOffset{linitialoffset+lHdrOffset}+26);// slice thickness mm
           lDicomData.XYZmm[1] := fswap4r (lHdrOffset{linitialoffset+lHdrOffset}+50);// pixel size- X
           lDicomData.XYZmm[2] := fswap4r (lHdrOffset{linitialoffset+lHdrOffset}+54);//pixel size - Y
           lSliceThick := lDicomData.XYZmm[3];
           lGap :=  fswap4r (lHdrOffset+118);//1410 gap thickness mm
           if lGap > 0 then
                  lDicomData.XYZmm[3] := lDicomData.XYZmm[3] + lGap;
        end;
     end;
     if (lCompress = 3) or (lCompress = 4) then begin
        lImageFormatOK := false;//xlDicomData.GenesisCpt := true;
        lDynStr := lDynStr+'Compressed data'+kCR;
     end else
         ;//xlDicomData.GenesisCpt := false;
     if (lCompress = 2) or (lCompress = 4) then begin
        lImageFormatOK := false;//xlDicomData.GenesisPackHdr := swap32i(linitialoffset+64);
        lDynStr := lDynStr+'Packed data'+kCR;
     end else
         //xlDicomData.GenesisPackHdr := 0;
	 lDynStr := lDynStr+'Series Number: '+inttostr(lDicomData.SeriesNum)
	 +kCR+'Acquisition Number: '+inttostr(lDicomData.AcquNum)
	 +kCR+'Image Number: '+inttostr(lDicomData.ImageNum)
	 +kCR+'Slice Thickness/Gap: '+floattostrf(lSliceThick,ffFixed,8,2)+'/'+floattostrf(lGap,ffFixed,8,2)
	 +kCR+'XYZ dim: ' +inttostr(lDicomData.XYZdim[1])+'/'+inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
	 +kCR+'XYZ mm: '+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
       +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2);
  lHdrOK := true;
  539:
       CloseFile(fp);
  FileMode := 2; //set to read/write
end;//read_ge


//start siemens
procedure read_siemens_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
label
  567;
var
  lI: word;
 lYear,lMonth,lDay,n,filesz,lFullSz,lMatrixSz,lIHour,lIMin,lISec{,lAHour,lAMin,lASec}: LongInt;
 lFlipAngle,lGap,lSliceThick: double;
  tx     : array [0..26] of Char;
  lMagField,lTE,lTR: double;
  lInstitution,lName, lID,lMinStr,lSecStr{,lAMinStr,lASecStr}: String;
  FP: file;
function swap32i(lPos: longint): Longint;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
   s : LongInt;
  inguy:swaptypep;
  outguy:swaptype;
begin
     seek(fp,lPos);
  BlockRead(fp, s, 4, n);
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  swap32i:=outguy.Long;
  //swap32i:=inguy.Long;
end;
function fswap8r (lPos: longint): double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(float:double);
  end;
  swaptypep = ^swaptype;
var
   s:double;
  inguy:swaptypep;
  outguy:swaptype;
begin
     seek(fp,lPos);
  BlockRead(fp, s, 8, n);
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  fswap8r:=outguy.float;
end;
begin
  lImageFormatOK := true;
  lHdrOK := false;
  if not fileexists(lFileName) then begin
     lImageFormatOK := false;
     exit;
  end;
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FIleSz := FileSize(fp);
  Clear_Dicom_Data(lDicomData);
     if filesz < (6144) then begin
        Msg('This file is to small to be a Siemens Magnetom Vision image.');
        goto 567;
     end;
     seek(fp, 96);
     BlockRead(fp, tx, 7*SizeOf(Char), n);
  if (tx[0] <> 'S') OR (tx[1] <> 'I') OR (tx[2] <> 'E') OR (tx[3] <> 'M') then begin {manufacturer is not SIEMENS}
        Msg('Is this a Siemens Magnetom Vision image [Manufacturer tag should be ''SIEMENS''].');
        goto 567;
  end; {manufacturer not siemens}
  seek(fp, 105);
  BlockRead(fp, Tx, 25*SizeOf(Char), n);
  lINstitution := '';
  for lI := 0 to 24 do begin
      if tx[lI] in ['/','\','a'..'z','A'..'Z',' ','+','-','.',',','0'..'9'] then lINstitution := lINstitution + tx[lI];
  end;  seek(fp, 768);
  BlockRead(fp, Tx, 25*SizeOf(Char), n);
  lName := '';
  for lI := 0 to 24 do begin
      if tx[lI] in ['/','\','a'..'z','A'..'Z',' ','+','-','.',',','0'..'9'] then lName := lName + tx[lI];
  end;
  seek(fp, 795);
  BlockRead(fp, Tx, 12*SizeOf(Char), n);
  lID := '';
  for lI := 0 to 11 do begin
      if tx[lI] in ['/','\','a'..'z','A'..'Z',' ','+','-','.',',','0'..'9'] then lID := lID + tx[lI];
  end;
     lDicomData.ImageStart := 6144;
     lYear := swap32i(0);
     lMonth := swap32i(4);
     lDay := swap32i(8);
     lIHour := swap32i(68);
     lIMin := swap32i(72);
     lISec := swap32i(76);
     lDicomData.XYZmm[3] := fswap8r (1544);
     lMagField := fswap8r (2560);
     lTR := fswap8r (1560);
     lTE := fswap8r (1568);
     lDIcomData.AcquNum := swap32i(3212);
     lMatrixSz := swap32i(2864);
     lDicomData.SiemensSlices := swap32i(4004); //1366
     //lFullSz := swap32i(4008);
     //lInterleaveIf4 := swap32i(2888);
     lFullSz := (2*lMatrixSz*lMatrixSz);//16bitdata
     if ((FileSz - 6144) mod lFullSz) = 0 then begin
        case ((FileSz-6144) div lFullSz) of
             4: lFullSz := 2*lMatrixSz;
             9: lFullSz := 3*lMatrixSz;
             16: lFullSz := 4*lMatrixSz;
             25: lFullSz := 5*lMatrixSz;
             36: lFullSz := 6*lMatrixSz;
             49: lFullSz := 7*lMatrixSz;
             64: lFullSz := 8*lMatrixSz;
             else lFullSz := lMatrixSz;
        end;
     end else lFullSz := lMatrixSz;
     {3744/3752 are XY FOV in mm!}
     lDicomData.XYZdim[1] := lFullSz;//lMatrixSz; //width
     lDicomData.XYZdim[2] := lFullSz;//lMatrixSz;//height
     {5000/5008 are size in mm, but wrong for mosaics}
     if lMatrixSz <> 0 then begin
        lDicomData.XYZmm[2] := fswap8r (3744)/lMatrixSz;
        lDicomData.XYZmm[1] := fswap8r (3752)/lMatrixSz;
        if ((lDicomData.XYZdim[1] mod lMatrixSz)=0) then
           lDicomData.SiemensMosaicX := lDicomData.XYZdim[1] div lMatrixSz;
        if ((lDicomData.XYZdim[2] mod lMatrixSz)=0) then
           lDicomData.SiemensMosaicY := lDicomData.XYZdim[2] div lMatrixSz;
        if lDicomData.SiemensMosaicX < 1 then lDicomData.SiemensMosaicX := 1; //1366
        if lDicomData.SiemensMosaicY < 1 then lDicomData.SiemensMosaicY := 1; //1366
     end;
     lFlipAngle := fswap8r (2112); //1414
{     lDicomData.XYZmm[2] := fswap8r (5000);
     lDicomData.XYZmm[1] := fswap8r (5008);}
     lSliceThick := lDicomData.XYZmm[3];
     lGap := fswap8r (4136); //gap as ratio of slice thickness?!?!
     if {lGap > 0} (lGap=-1) or (lGap=-19222) then //1410: exclusion values: do not ask me why 19222: from John Ashburner
     else begin
        //lDicomData.XYZmm[3] := abs(lDicomData.XYZmm[3] * (1+lGap));
        lGap := lDicomData.XYZmm[3] * (lGap);
        lDicomData.XYZmm[3] := abs(lDicomData.XYZmm[3] +lGap);
     end;
     lDicomData.Allocbits_per_pixel := 16;//bits
     //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
     //xlDicomData.GenesisCpt := false;
     //xlDicomData.GenesisPackHdr := 0;
     lMinStr := inttostr(lIMin);
     if length(lMinStr) = 1 then lMinStr := '0'+lMinStr;
     lSecStr := inttostr(lISec);
     if length(lSecStr) = 1 then lSecStr := '0'+lSecStr;



     lDynStr := 'Siemens Magnetom Vision Format'+kCR+'Name: '+lName+kCR+'ID: '+lID+kCR+'Institution: '+lInstitution+kCR+
     'Study DD/MM/YYYY: '+inttostr(lDay)+'/'+inttostr(lMonth)+'/'+inttostr(lYear)+kCR+
     'Image Hour/Min/Sec: '+inttostr(lIHour)+':'+lMinStr+':'+lSecStr+kCR+
     //'Acquisition Hour/Min/Sec: '+inttostr(lAHour)+':'+lAMinStr+':'+lASecStr+kCR+
     'Magnetic Field Strength: '+ floattostrf(lMagField,ffFixed,8,2)+kCR+
     'Image index: '+inttostr(lDIcomData.AcquNum)+kCR+
     'Time Repitition/Echo [TR/TE]: '+ floattostrf(lTR,ffFixed,8,2)+'/'+ floattostrf(lTE,ffFixed,8,2)+kCR+
     'Flip Angle: '+ floattostrf(lFlipAngle,ffFixed,8,2)+kCR+
     'Slice Thickness/Gap: '+floattostrf(lSliceThick,ffFixed,8,2)+'/'+floattostrf(lGap,ffFixed,8,2)+kCR+
     'XYZ dim:' +inttostr(lDicomData.XYZdim[1])+'/'
     +inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])+kCR+
     'XY matrix:' +inttostr(lDicomData.SiemensMosaicX)+'/'
     +inttostr(lDicomData.SiemensMosaicY)+kCR+
     'XYZ mm:'+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
     +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2);
  lHdrOK := true;
  //lDIcomData.AcquNum := 0;
567:
CloseFile(fp);
  FileMode := 2; //set to read/write
end;
//end siemens
//begin elscint
procedure read_elscint_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
label
  539;
var
  //lExamHdr,lImgHdr,lDATFormatOffset,lHdrOffset,
  {lDate,}lI,lCompress,n,filesz: LongInt;
  tx     : array [0..41] of Char;
  FP: file;
function readStr(lPos,lLen: integer):  string;
var lStr: string;
    lStrInc: integer;
begin
     seek(fp,lPos);
     BlockRead(fp, tx, lLen, n);
     lStr := '';
     for lStrInc := 0 to (lLen-1) do
         lStr := lStr + tx[lStrInc];
     result := lStr
end;
function read8ch(lPos: integer): char;
begin
     seek(fp,40);
     BlockRead(fp, result, 1, n);
     //lDicomData.ImageNum := ord(tx[0]);
end;
procedure read16i(lPos: longint; var lVal: integer);
var lInWord: word;
begin
  seek(fp,lPos);
  BlockRead(fp, lInWord, 2);
  lVal := lInWord;
end;
procedure read32i(lPos: longint; var lVal: integer);
var lInINt: integer;
begin
  seek(fp,lPos);
  BlockRead(fp, lInINt, 4);
  lVal :=lInINt;
end;

begin
  lImageFormatOK := true;
  lHdrOK := false;
  if not fileexists(lFileName) then begin
     lImageFormatOK := false;
     exit;
  end;
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FIleSz := FileSize(fp);
  Clear_Dicom_Data(lDicomData);
     if filesz < (3240) then begin
        Msg('This file is too small to be a Elscint format image.');
        goto 539;
     end;
     lDynStr:= '';
     read16i(0, lI);
     if (lI <> 64206) then begin
        Msg('Unable to read this file: it does start with the Elscint signature.');
        goto 539;
     end;
     lDicomdata.little_endian := 1;
     lDynStr:= 'Elscint Format'+kCR;
     lDynStr := lDynStr+'Patient Name: '+readstr(4,20)+kCR;
     lDynStr := lDynStr+'Patient ID: '+readstr(24,13)+kCR;
     read16i(38,lDicomData.AcquNum);
     lDicomData.ImageNum := ord(read8Ch(40));
     lDynStr := lDynStr+'Doctor & Ward: '+readstr(100,20)+kCR;
     lDynStr := lDynStr+'Comments: '+readstr(120,40)+kCR;
     if ord(read8Ch(163)) = 1 then
        lDynStr := lDynStr + 'Sex: M'+kCR
     else
        lDynStr := lDynStr + 'Sex: F'+kCR;
     read16i(200,lI);
     lDicomData.XYZmm[3] := lI * 0.1;
     read16i(370,lDicomData.XYZdim[1]);
     read16i(372,lDicomData.XYZdim[2]);
     read16i(374,lI);
     lDicomData.XYZmm[1] := lI / 256;
     lDicomData.XYZmm[2] := lDicomData.XYZmm[1];
     lCompress := ord(read8Ch(376));
     //xlDicomData.ElscintCompress := true;
     //xread16i(400,lDicomData.WindowWidth);
     //x read16i(398,lDicomData.WindowCenter);
     case lCompress of
          0: begin
               lDynStr := lDynStr + 'Compression: None'+kCR;
               //xlDicomData.ElscintCompress := false;
          end;
          1: lImageFormatOK := false;//xlDynStr := lDynStr + 'Compression: Old'+kCR;
          2: lImageFormatOK := false;//xlDynStr := lDynStr + 'Compression: 2400 Elite'+kCR;
          22: lImageFormatOK := false;//xlDynStr := lDynStr + 'Compression: Twin'+kCR;
          else begin
               lImageFormatOK := false;//xlDynStr := lDynStr + 'Compression: Unknown '+inttostr(lCOmpress)+kCR;
               //lDicomData.ElscintCompress := false;
          end;
     end;
     //lDicomData.XYZdim[1] := swap32i(linitialoffset+8); //width
     //lDicomData.XYZdim[2] := swap32i(linitialoffset+12);//height
     lDicomData.ImageStart := 396;
     lDicomData.Allocbits_per_pixel := 16;
     //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
     if (lDicomData.XYZdim[1]=160) and (lDicomData.XYZdim[2]= 160) and (FIleSz=52224) then begin
         lDicomData.ImageStart := 1024;
         lImageFormatOK := true;//x//xlDicomData.ElscintCompress := False;
     end;
     //lDicomData.XYZmm[3] := fswap4r (2310+26);// slice thickness mm
     lDynStr := lDynStr+'Image/Study Number: '+inttostr(lDicomData.ImageNum)+'/'+ inttostr(lDicomData.AcquNum)+kCR
     +'XYZ dim: ' +inttostr(lDicomData.XYZdim[1])+'/'
     +inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
     //x+kCR+'Window Center/Width: '+inttostr(lDicomData.WindowCenter)+'/'+inttostr(lDicomData.WindowWidth)
     +kCR+'XYZ mm: '+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
     +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2);
  lHdrOK := true;
  lImageFormatOK := true;
  539:
       CloseFile(fp);
  FileMode := 2; //set to read/write
end;
//end elscint



//start picker
procedure read_picker_data(lVerboseRead: boolean; var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
label 423;
const kPickerHeader =8192;
kRecStart = 280; //is this a constant?
var
  lDataStart,lVal,lDBPos,lPos,lRecSz, lNumRecs,lRec,FileSz,n: Longint;
  lThkM,lThkN,lSiz: double;
  tx     : array [0..6] of Char;
  FP: file;
  lDiskCacheRA: pChar;
function ReadRec(lRecNum: integer): boolean;
var
   lNameStr,lValStr: string;
   lOffset,lLen,lFPOs,lFEnd: integer;
function ValStrToFloat: double;
var lConvStr: string;
    lI: integer;
begin
     Result := 0.0;
     lLen := Length(lValStr);
     if lLen < 1 then exit;
     lConvStr := '';
     for lI := 1 to lLen do
         if lValStr[lI] in ['0'..'9'] then
            lConvStr := lConvStr+ lValStr[lI];
     if Length(lConvStr) < 1 then exit;
     Result := strtofloat(lConvStr);
end;
begin
  Result := false;
  lFPos := ((lRecNum-1) * lRecSz)+ kRecStart;
  lFEnd := lFpos + 6;
  lNameStr := '';
  for lFPos := lFPos to lFEnd do
         if ord(lDiskCacheRA[lFPos]) <> 0 then
            lNameStr := lNameStr +lDiskCacheRA[lFPos];
  if (lVerboseRead) or (lNameStr = 'RCNFSIZ') or (lNameStr='SCNTHKM') or (lNameStr='SCNTHKN') then begin
     lFPos := ((lRecNum-1) * lRecSz)+ kRecStart+8;
     lFEnd := lFPos+1;
     lOffset := 0;
     for lFPos := lFPos to lFend do
         lOffset := ((lOffset)shl 8)+(ord(lDiskCacheRA[lFPos]));
     lFPos := ((lRecNum-1) * lRecSz)+ kRecStart+10;
     lFEnd := lFPos+1;
     lLen := 0;
     for lFPos := lFPos to lFend do
         lLen := ((lLen)shl 8)+(ord(lDiskCacheRA[lFPos]));
     lOffset := lDataStart+lOffset+1;
     lFEnd := lOffset+lLen-1;
     if (lLen < 1) or  (lFEnd > kPickerHeader) then exit;
     lValStr := '';
     for lFPos := (lOffset) to lFEnd  do begin
         lValStr := lValStr+lDiskCacheRA[lFPos];
     end;
     if lVerboseRead then lDynStr := lDynStr+kCR+lNameStr+': '+ lValStr;
     if (lNameStr = 'RCNFSIZ') then lSiz := ValStrToFloat;
     if (lNameStr='SCNTHKM') then lThkM := ValStrToFloat;
     if (lNameStr='SCNTHKN') then lThkN := ValStrToFloat;
  end; //verboseread, or vital value
  result := true;
end;
function FindStr(l1,l2,l3,l4,l5: Char; lReadNum: boolean; var lNum: integer): boolean;
var //lMarker: integer;
    lNumStr: String;
begin
     Result := false;
     repeat
           if (lDiskCacheRA[lPos-4]=l1) and (lDiskCacheRA[lPos-3]=l2)
           and (lDiskCacheRA[lPos-2]=l3) and (lDiskCacheRA[lPos-1]=l4)
           and (lDiskCacheRA[lPos]=l5) then Result := true;
           inc (lPos);
     until (Result) or (lPos >= kPickerHeader);
     if not Result then exit;
     if not lReadNum then exit;
     Result := false;
     lNumStr := '';
     repeat
           if (lDiskCacheRA[lPos] in ['0'..'9']) then
           lNumStr := lNumStr + lDiskCacheRA[lPos]
           else if lNumStr <> '' then Result := true;
           inc(lPos);
     until (Result) or (lPos = kPickerHeader);
     lNum := strtoint(lNumStr);
end;
begin
  lSiz := 0.0;
  lThkM := 0.0;
  lThkN := 0.0;
  lImageFormatOK := true;
  lHdrOK := false;
  if not fileexists(lFileName) then begin
     lImageFormatOK := false;
     exit;
  end;
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FIleSz := FileSize(fp);
  Clear_Dicom_Data(lDicomData);
     if filesz < (kPickerHeader) then begin
        Msg('This file is to small to be a Picker image: '+lFileName );
       CloseFile(fp);
       FileMode := 2; //set to read/write
       exit;
     end;
     seek(fp, 0);
     BlockRead(fp, tx, 4*SizeOf(Char), n);
     if (tx[0] <> '*') OR (tx[1] <> '*') OR (tx[2] <> '*') OR (tx[3] <> ' ') then begin {manufacturer is not SIEMENS}
        Msg('Is this a Picker image? Expected ''***'' at the start of the file.'+ lFileName);
       CloseFile(fp);
       FileMode := 2; //set to read/write
       exit;
     end; {not picker}
     if filesz = (kPickerHeader + (1024*1024*2)) then begin
        lDICOMdata.XYZdim[1] := 1024;
        lDICOMdata.XYZdim[2] := 1024;
        lDICOMdata.XYZdim[3] := 1;
        lDICOMdata.ImageStart := 8192;
     end else
     if filesz = (kPickerHeader + (512*512*2)) then begin
        lDICOMdata.XYZdim[1] := 512;
        lDICOMdata.XYZdim[2] := 512;
        lDICOMdata.XYZdim[3] := 1;
        lDICOMdata.ImageStart := 8192;
     end else
     if filesz = (8192 + (256*256*2)) then begin
        lDICOMdata.XYZdim[1] := 256;
        lDICOMdata.XYZdim[2] := 256;
        lDICOMdata.XYZdim[3] := 1;
        lDICOMdata.ImageStart := 8192;
     end else begin
        Msg('This file is the incorrect size to be a Picker image.');
       CloseFile(fp);
       FileMode := 2; //set to read/write
       exit;
     end;
     getmem(lDiskCacheRA,kPickerHeader*sizeof(char));
     seek(fp, 0);
     BlockRead(fp, lDiskCacheRA, kPickerHeader, n);
     lRecSz := 0;
     lNumRecs := 0;
     lPos := 5;
     if not FindStr('d','b','r','e','c',false, lVal) then goto 423;
     lDBPos := lPos;
     if not FindStr('r','e','c','s','z',true, lRecSz) then goto 423;
     lPos := lDBPos;
     if not FindStr('n','r','e','c','s',true, lnumRecs) then goto 423;
     lPos := kRecStart; // IS THIS A CONSTANT???
     lDataStart :=kRecStart + (lRecSz*lnumRecs)-1; //file starts at 0, so -1
     if (lNumRecs = 0) or (lDataStart> kPickerHeader) then goto 423;
     lRec := 0;
     lDynStr := 'Picker Format';
     repeat
          inc(lRec);
     until (not (ReadRec(lRec))) or (lRec >= lnumRecs);
     if lSiz <> 0 then begin
        lDICOMdata.XYZmm[1] := lSiz/lDICOMdata.XYZdim[1];
        lDICOMdata.XYZmm[2] := lSiz/lDICOMdata.XYZdim[2];
        if lVerboseRead then
           lDynStr := lDynStr+kCR+'Voxel Size: '+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)
           +'x'+ floattostrf(lDicomData.XYZmm[2],ffFixed,8,2);
     end;
     if (lThkM <> 0) and (lThkN <> 0) then begin
        lDICOMdata.XYZmm[3] := lThkN/lThkM;
        if lVerboseRead then
           lDynStr := lDynStr+kCR+'Slice Thickness: '+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2);
     end;
  423:
     freemem(lDiskCacheRA);
     lHdrOK := true;
     CloseFile(fp);
     FileMode := 2; //set to read/write
end;
//end picker

procedure read_minc_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
var
//  lReal: double;
  lnOri,lnDim,lStartPosition,nelem0,jj,lDT0,vSizeRA,BeginRA,m,nnelem,nc_type,nc_size,lLen,nelem,j,lFilePosition,lDT,lFileSz,lSignature,lWord: integer;

  lOri: array [1..3] of double;
  //tx     : array [0..80] of Char;
  lVarStr,lStr: string;
  FP: file;
function dTypeStr (lV: integer): integer;
begin
     case lV of
          1,2: result := 1;
          3: result := 2; //int16
          4: result := 4; //int32
          5: result := 4; //single
          6: result := 8; //double
     end;
end; //nested fcn dTypeStr

function read32i: Longint;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
   s : LongInt;
  inguy:swaptypep;
  outguy:swaptype;
begin
  seek(fp,lFilePosition);
  lFilePosition := lFilePosition + 4;
  BlockRead(fp, s, 4);
  inguy := @s; //assign address of s to inguy
  if lDICOMdata.Little_Endian = 0 then begin
     outguy.Word1 := swap(inguy^.Word2);
     outguy.Word2 := swap(inguy^.Word1);
  end else
      outguy.long := inguy^.long;
  result:=outguy.Long;
end;

function read64r (lDataType: integer): Double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(Long:Double);
  end;
  swaptypep = ^swaptype;
var
   s : Double;
  inguy:swaptypep;
  outguy:swaptype;
begin
  result := 1;
  if lDataType <> 6 then begin
      Msg('Unknown data type: MRIcro is unable to determine the voxel size.');
      exit;
  end;
  seek(fp,lFilePosition);
  lFilePosition := lFilePosition + 8;
  BlockRead(fp, s, 8);
  inguy := @s; //assign address of s to inguy
  if lDICOMdata.Little_Endian = 0 then begin
     outguy.Word1 := swap(inguy^.Word4);
     outguy.Word2 := swap(inguy^.Word3);
     outguy.Word3 := swap(inguy^.Word2);
     outguy.Word4 := swap(inguy^.Word1);
  end else
      outguy.long := inguy^.long;
  result:=outguy.Long;
end;

function readname: String;
var lI,lLen: integer;
    lCh: char;
begin
  result := '';
  seek(fp,lFilePosition);
  lLen := read32i;
  if lLen < 1 then begin
     Msg('Terminal error reading netCDF/MINC header (String length < 1)');
     exit; //problem
  end;
  for lI := 1 to lLen do begin
      BlockRead(fp, lCh, 1);
      result := result + lCh;
  end;
  lFilePosition := lFilePosition + (((lLen+3) div 4) * 4);
end;

begin
  lImageFormatOK := true;
  lHdrOK := false;
  if not fileexists(lFileName) then begin
     lImageFormatOK := false;
     exit;
  end;
  for lnOri := 1 to 3 do
      lOri[lnOri] := 0;
  lnOri := 4;
  lnDim := 4;
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  lFileSz := FileSize(fp);
  Clear_Dicom_Data(lDicomData);
  if lFilesz < (77) then exit; //to small to be MINC

  lFilePosition := 0;
  lSignature := read32i;
  if not (lSignature=1128547841) then begin
     CloseFile(fp);
     FileMode := 2; //set to read/write
     Msg('Problem with MINC signature: '+ inttostr(lSignature));
     exit;
  end;
  //xlDicomData.Rotate180deg := true;
  lWord := read32i;//numrecs
  lDT := read32i;
  while (lDt=10) or (lDT=11) or (lDT=12) do begin
     if lDT = 10 then begin //DT=10, Dimensions
        nelem := read32i;
        for j := 1 to nelem do begin
            lStr := readname;
            lLen := read32i;
            if lStr = 'xspace' then lDicomData.XYZdim[3] := lLen;//DOES MINC always reverse X and Z? see also XYZmm
            if lStr = 'yspace' then lDicomData.XYZdim[2] := lLen;
            if lStr = 'zspace' then lDicomData.XYZdim[1] := lLen;
        end; //for 1..nelem
        lDT := read32i;
    end;//DT=10, Dimensions
    if lDT = 11 then begin //DT=11, Variables
        nelem := read32i;
        for j := 1 to nelem do begin
            lVarStr := readname;
            nnelem := read32i;
            for m := 1 to nnelem do
                lLen := read32i;
            lDT0 := read32i;
            if lDT0 = 12 then begin
               nelem0 := read32i;
               for jj := 1 to nelem0 do begin
                   lStr := readname;
                   nc_type := read32i;
                   nc_size := dTypeStr(nc_Type);
                   nnelem := read32i;
                   lStartPosition := lFilePosition;

                   if (lStr = 'step') then begin

                      if (lVarStr = 'xspace') or (lVarStr = 'yspace') or (lVarStr = 'zspace') then begin
                         dec(lnDim);
                         if (lnDim < 4) and (lnDim>0) then
                            lDicomData.XYZmm[lnDim] := read64r(nc_Type)
                      end;

                   end else if (lStr = 'start') then begin
                      if (lVarStr = 'xspace') or (lVarStr = 'yspace') or (lVarStr = 'zspace') then begin
                         dec(lnOri);
                         if (lnOri < 4) and (lnOri > 0) then
                            lOri[lnOri] := read64r(nc_Type)
                      end;
                   end;
                   lFilePosition := lStartPosition + ((((nnelem*nc_size)+3) div 4)*4);

               end;
               lDT0 := read32i;
               if lVarStr = 'image' then begin
                  case lDT0 of
                       1,2: lDicomData.Allocbits_per_pixel := 8;
                       3: lDicomData.Allocbits_per_pixel := 16; //int16
                       4: lDicomData.Allocbits_per_pixel := 32; //int32
                       5: lDicomData.Allocbits_per_pixel := 32; //single
                       6: lDicomData.Allocbits_per_pixel := 64; //double
                  end;
                  if (lDT0 = 5) or (lDT0 = 6) then
                     lDicomData.FloatData := true;
                  //xlDicomData.Storedbits_per_pixel := lDicomData.Allocbits_per_pixel;
                  //lImgNC_Type := lDT0;
               end;
            end;
            vSizeRA := read32i;
            BeginRA := read32i;
            if lVarStr = 'image' then begin
               lDICOMdata.ImageStart := BeginRA;
            end;
        end; //for 1..nelem
        lDT := read32i;
    end;//DT=11
    if lDT = 12 then begin //DT=12, Attributes
        nelem := read32i;
        for j := 1 to nelem do begin
            lStr := readname;
            nc_type := read32i;
            nc_size := dTypeStr(nc_Type);
            nnelem := read32i;
            lFilePosition := lFilePosition + ((((nnelem*nc_size)+3) div 4)*4);
        end; //for 1..nelem
        lDT := read32i;
    end;//DT=12, Dimensions
  end; //while DT

  if lOri[1] <> 0 then
     lDicomData.XYZori[1] := round((-lOri[1])/lDicomData.XYZmm[1])+1;
  if lOri[2] <> 0 then
     lDicomData.XYZori[2] := round((-lOri[2])/lDicomData.XYZmm[2])+1;
  if lOri[3] <> 0 then
     lDicomData.XYZori[3] := round((-lOri[3])/lDicomData.XYZmm[3])+1;

  lDynStr := 'MINC image'+kCR+
     'XYZ dim:' +inttostr(lDicomData.XYZdim[1])+'/'
     +inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
     +kCR+'XYZ origin:' +inttostr(lDicomData.XYZori[1])+'/'
     +inttostr(lDicomData.XYZori[2])+'/'+inttostr(lDicomData.XYZori[3])
     +kCR+'XYZ size [mm or micron]:'+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
     +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2)
     +kCR+'Bits per sample/Samples per pixel: '+inttostr( lDICOMdata.Allocbits_per_pixel)
     +kCR+'Data offset:' +inttostr(lDicomData.ImageStart);
  lHdrOK := true;
  lImageFormatOK := true;
  CloseFile(fp);
  FileMode := 2; //set to read/write
end; //read_minc



//start TIF
procedure read_tiff_data(var lDICOMdata: DICOMdata; var lReadOffsets, lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
label
  566, 564;
const
     kMaxnSLices = 6000;
var
  lLongRA: LongIntP;
  lStackSameDim,lContiguous: boolean;
  l1stDicomData: DicomData;
  //lDouble : double;
  //lXmm,lYmm,lZmm: double;
  lSingle: single;
  lImageDataEndPosition,lStripPositionOffset,lStripPositionType,lStripPositionItems,
  lStripCountOffset,lStripCountType,lStripCountItems,
  lItem,lTagItems,lTagItemBytes,lTagPointer,lNumerator, lDenominator,
  lImage_File_Directory,lTagType,lVal,lDirOffset,lOffset,lFileSz,
  lnDirectories,lDir,lnSlices: Integer;
  lTag,lWord,lWord2: word;
  FP: file;
(*FUNCTION longint2single ({var} s:longint): single;
//returns true if s is Infinity, NAN or Indeterminate
//4byte IEEE: msb[31] = signbit, bits[23-30] exponent, bits[0..22] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
VAR Overlay: Single ABSOLUTE s;
BEGIN
  result := Overlay;
END;*)

function read64r(lPos: integer):double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(float:double);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
  s: double;
begin
  seek(fp,lPos);
  BlockRead(fp, s, 8);
  inguy := @s; //assign address of s to inguy
  if lDICOMdata.Little_Endian = 0{false} then begin
     outguy.Word1 := swap(inguy^.Word4);
     outguy.Word2 := swap(inguy^.Word3);
     outguy.Word3 := swap(inguy^.Word2);
     outguy.Word4 := swap(inguy^.Word1);
  end else
      outguy.float := inguy^.float;
  result:=outguy.float;
end;

function read32i(lPos: longint): Longint;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
   s : LongInt;
  inguy:swaptypep;
  outguy:swaptype;
begin
  seek(fp,lPos);
  BlockRead(fp, s, 4);
  inguy := @s; //assign address of s to inguy
  if lDICOMdata.Little_Endian = 0 then begin
     outguy.Word1 := swap(inguy^.Word2);
     outguy.Word2 := swap(inguy^.Word1);
  end else
      outguy.long := inguy^.long;
  result:=outguy.Long;
end;
function read16(lPos: longint): Longint;
var
   s : word;
begin
  seek(fp,lPos);
  BlockRead(fp, s, 2);
  if lDICOMdata.Little_Endian = 0 then
     result := swap(s)
  else
      result := s;
end;

function read8(lPos: longint): Longint;
var
   s : byte;
begin
  seek(fp,lPos);
  BlockRead(fp, s, 1);
  result := s;
end;

function readItem(lItemNum,lTagTypeI,lTagPointerI: integer): integer;
begin
     if lTagTypeI= 4 then
        result := read32i(lTagPointerI+((lItemNum-1)*4))
     else
         result := read16(lTagPointerI+((lItemNum-1)*2));
end;

begin
  Clear_Dicom_Data(lDicomData);
  if gECATJPEG_table_entries <> 0 then begin
     freemem (gECATJPEG_pos_table);
     freemem (gECATJPEG_size_table);
     gECATJPEG_table_entries := 0;
  end;
  //lXmm := -1; //not read
  lImageFormatOK := true;
  lHdrOK := false;
  if not fileexists(lFileName) then begin
     lImageFormatOK := false;
     exit;
  end;
  //lLongRASz := kMaxnSlices * sizeof(longint);
  getmem(lLongRA,kMaxnSlices*sizeof(longint));
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  lFileSz := FileSize(fp);
  Clear_Dicom_Data(lDicomData);
  //xlDicomData.PlanarConfig:=0;
  if lFilesz < (28) then begin
        goto 566;
  end;
  //TmpStr := string(StrUpper(PChar(ExtractFileExt(lFileName))));
  //if not (TmpStr = '.TIF') or (TmpStr = '.TIFF') then exit;
  lWord   := read16(0);
  if lWord = $4d4d then
     lDICOMdata.little_endian := 0
  else if lWord = $4949 then lDICOMdata.little_endian := 1;
  lWord2   := read16(2); //bits per pixel
  if ((lWord=$4d4d) or (lWord=$4949)) and (lWord2 = $002a) then
  else goto 566;
  lOffset := read32i(4);
  lImage_File_Directory := 0;
  lContiguous := true;
  lnSlices := 0;
  //xlDicomData.SamplesPerPixel := 1;
  //START while for each image_file_directory
  while (lOffset > 0) and ((lOffset+2+12+4) < lFileSz) do begin
        inc(lImage_File_Directory);
        lnDirectories := read16(lOffset);
        if (lnDirectories < 1) or ((lOffset+2+(12*lnDirectories)+4) > lFileSz) then
           goto 566;
        for lDir := 1 to lnDirectories do begin
            lDirOffset := lOffset+2+((lDir-1)*12);
            lTag   := read16(lDirOffset);
            lTagType := read16(lDirOffset+2);
            lTagItems := read32i(lDirOffset+4);
            case lTagType of
                 1: lVal := 1;//bytes
                 3: lVal := 2;//word
                 4: lVal := 4;//long
                 5: lVal := 8;//rational
                 else lVal := 1; //CHAR variable length
            end;
            lTagItemBytes := lVal * lTagItems;
            if lTagItemBytes > 4 then
                 lTagPointer := read32i(lDirOffset+8)
            else
                lTagPointer := (lDirOffset+8);
            case lTagType of
                 1: lVal := read8(lDirOffset+8);
                 3: lVal := read16(lDirOffset+8);
                 4: lVal := read32i(lDirOffset+8);
                 5: begin //rational: two longs representing numerator and denominator
                     lVal := read32i(lDirOffset+8);
                     lNumerator := read32i(lVal);
                     lDenominator := read32i(lVal+4);
                     if lDenominator <> 0 then
                        lSingle := lNumerator/lDenominator
                     else
                         lSingle := 1;
                     if lSingle <> 0 then
                        lSingle := 1/lSingle; //Xresolution/Yresolution refer to number of pixels per resolution_unit
                     if lTag = 282 then  lDicomData.XYZmm[1] := lSingle;
                     if lTag = 283 then  lDicomData.XYZmm[2] := lSingle;
                 end;
                 else lVal := 0;
            end;
            case lTag of
                 //254: ;//NewSubFileType
                 256: lDicomData.XYZdim[1] := lVal;//image_width
                 257: lDicomData.XYZdim[2] := lVal;//image_height
                 258: begin  //bits per sample
                     if lTagItemBytes > 4 then lVal := 8;
                     //if lVal <> 8 then goto 566;
                     lDicomData.Allocbits_per_pixel := lVal;//bits
                     //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
                 end;
                 259: begin
                           if lVal <> 1 then begin
                              Msg('TIFF Read Error: Image data is compressed. Currently only uncompressed data is supported.');
                              goto 566; //compressed data
                           end;
                      end;
                 //x262: if lVal = 0 then lDicomdata.monochrome := 1;//invert colors //photometric_interpretation  //MinIsWhite,MinIsBlack,Palette
                 //270: ; //ImageDescription
                 273: begin //get offset to data
                      lStripPositionOffset := lTagPointer;
                      lStripPositionType := lTagType;
                      lStripPositionItems := lTagItems;
                      if (lImage_File_Directory=1) then
                         lDicomData.ImageStart := readItem(1,lStripPositionType,lStripPositionOffset);
                 end; //StripOffsets
                 //274: ; //orientation
                 277: begin
                      //xlDicomData.SamplesPerPixel := lVal;
                      //if lVal <> 1 then goto 566; //samples per pixel
                 end;
                 279: begin
                      lStripCountOffset := lTagPointer;
                      lStripCountType := lTagType;
                      lStripCountItems := lTagItems;
                 end;
                 //278: message('rows:'+inttostr(lVal));//StripByteCount
                 //279: message('count:'+inttostr(lVal));//StripByteCount
                 //282 and 283 are rational values and read separately
                 284: begin
                      {xif lVal = 1 then
                         lDicomData.PlanarConfig:= 0
                      else
                          lDicomData.PlanarConfig:= 1;//planarConfig
                } end;
                 34412: begin
                     //Zeiss data header
 //0020h  float       x size of a pixel (µm or s)
 //0024h  float       y size of a pixel (µm or s)
 //0028h  float       z distance in a sequence (µm or s)
        {stream.seek((int)position + 40);
        VOXELSIZE_X = swap(stream.readDouble());
        stream.seek((int)position + 48);
        VOXELSIZE_Y = swap(stream.readDouble());
        stream.seek((int)position + 56);
        VOXELSIZE_Z = swap(stream.readDouble());}
        lVal := read16(lTagPointer+2);
        if lVal = 1024 then begin //LSM510 v2.8 images
           lDicomData.XYZmm[1]{lXmm} := read64r(lTagPointer+40)*1000000;
           lDicomData.XYZmm[2]{lYmm} := read64r(lTagPointer+48)*1000000;
           lDicomData.XYZmm[3]{lZmm} := read64r(lTagPointer+56)*1000000;
        end;
        //following may work if lVal = 2, different type of LSM file I have not seen
                      //lXmm := longint2single(read32i(lTagPointer+$0020));
                      //lYmm := longint2single(read32i(lTagPointer+$0024));
                      //lZmm := longint2single(read32i(lTagPointer+$0028));
                 end;
                 //296: ;//resolutionUnit 1=undefined, 2=inch, 3=centimeter
                 //320??
                 //LEICA: 34412
  //SOFTWARE = 305
  //DATE_TIME = 306
  //ARTIST = 315
  //PREDICTOR = 317
  //COLORMAP = 320 => essntially custom LookUpTable
  //EXTRASAMPLES = 338
  //SAMPLEFORMAT = 339
  //JPEGTABLES = 347
                 //         lDicomData.ImageStart := lVal
                 //else if lImage_File_Directory = 1 then Msg(inttostr(lTag)+'@'+inttostr(lTagPointer)+' value: '+inttostr(lVal));
            end; //case lTag
        end; //For Each Directory in Image_File_Directory
        lOffset := read32i(lOffset+2+(12*lnDirectories));
        //NEXT: check that each slice in 3D slice is the same dimension
        lStackSameDim := true;
        if (lImage_File_Directory=1) then begin
          l1stDicomData := lDICOMdata;
          lnSlices := 1; //inc(lnSlices);
        end else begin
             if lDicomData.XYZdim[1] <> l1stDicomData.XYZdim[1] then lStackSameDim  := false;
             if lDicomData.XYZdim[2] <> l1stDicomData.XYZdim[2] then lStackSameDim  := false;
             if lDicomData.Allocbits_per_pixel <> l1stDicomData.Allocbits_per_pixel then lStackSameDim  := false;
             //xif lDicomData.SamplesPerPixel <> l1stDicomData.SamplesPerPixel then lStackSameDim  := false;
             //xif lDicomData.PlanarConfig <> l1stDicomData.PlanarConfig then lStackSameDim  := false;
             if not lStackSameDim then begin
                //Msg(inttostr(lDicomData.XYZdim[1])+'x'+inttostr(l1stDicomData.XYZdim[1]));
                if (lDicomData.XYZdim[1]*lDicomData.XYZdim[2]) > (l1stDicomData.XYZdim[1]*l1stDicomData.XYZdim[2]) then begin
                   l1stDicomData := lDICOMdata;
                   lnSlices := 1;
                   lStackSameDim := true;
                end;
                //Msg('TIFF Read Error: Different 2D slices in this 3D stack have different dimensions.');
                //goto 566;
             end else
                 inc(lnSlices); //if not samedim
        end; //check that each slice is same dimension as 1st
        //END check each 2D slice in 3D stack is same dimension
        //NEXT: check if image data is contiguous
        if (lStripCountItems > 0) and (lStripCountItems = lStripPositionItems) then begin
           if (lnSlices=1) then lImageDataEndPosition := lDicomData.ImageStart;
           for lItem := 1 to lStripCountItems do begin
               lVal := readItem(lItem,lStripPositionType,lStripPositionOffset);
               if (lVal <> lImageDataEndPosition) then
                  lContiguous := false;
                  //Msg(inttostr(lImage_File_Directory)+'@'+inttostr(lItem));
               lImageDataEndPosition := lImageDataEndPosition+readItem(lItem,lStripCountType,lStripCountOffset);
               if not lcontiguous then begin
                  if (lReadOffsets) and (lStackSameDim)  then begin
                     lLongRA^[lnSlices] := lVal;
                  end else if (lReadOffsets) then
                    //not correct size, but do not generate an error as we will read non-contiguous files
                  else begin
                      Msg('TIFF Read Error: Image data is not stored contiguously. '+
                      'Solution: convert this image using MRIcro''s ''Convert TIFF/Zeiss to Analyze...'' command [Import menu].');
                      goto 564;
                  end;
               end; //if not contiguous
           end; //for each item
        end;//at least one StripItem}
        //END check image data is contiguous
  end; //END while each Image_file_directory
  lDicomData := l1stDicomData;
  lDicomData.XYZdim[3] := lnSlices;
  if (lReadOffsets) and (lnSlices > 1) and (not lcontiguous) then begin
           gECATJPEG_table_entries := lnSlices; //Offset tables for TIFF
           getmem (gECATJPEG_pos_table, gECATJPEG_table_entries*sizeof(longint));
           getmem (gECATJPEG_size_table, gECATJPEG_table_entries*sizeof(longint));
           gECATJPEG_pos_table^[1]  := l1stDicomData.ImageStart;
           for lVal := 2 to gECATJPEG_table_entries do
               gECATJPEG_pos_table^[lVal] := lLongRA^[lVal]
  end;
  lHdrOK := true;
564:
  lDynStr := 'TIFF image'+kCR+
     'XYZ dim:' +inttostr(lDicomData.XYZdim[1])+'/'
     +inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
     +kCR+'XYZ size [mm or micron]:'+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
     +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2)
     +kCR+'Bits per sample/Samples per pixel: '+inttostr( lDICOMdata.Allocbits_per_pixel)
     +kCR+'Data offset:' +inttostr(lDicomData.ImageStart);
  {if lXmm > 0 then
      lDynStr := lDynStr +kCR+'Zeiss XYZ mm:'+floattostr(lXmm)+'/'
       +floattostr(lYmm)+'/'
       +floattostr(lZmm);}
566:
  freemem(lLongRA);
  CloseFile(fp);
  FileMode := 2; //set to read/write
end;

procedure read_biorad_data(var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;lFileName: string);
var
   lCh: char;
   lByte: Byte;
  lSpaces,liPos,lFileSz,lWord,lNotes,lStart,lEnd: integer;
  tx     : array [0..80] of Char;
  lInfo,lStr,lTmpStr: string;
  FP: file;
procedure read16(lPos: longint; var lVal: integer);
var lInWord: word;
begin
  seek(fp,lPos);
  BlockRead(fp, lInWord, 2);
  lVal := lInWord;
end;
procedure read32(lPos: longint; var lVal: integer);
var lInINt: integer;
begin
  seek(fp,lPos);
  BlockRead(fp, lInINt, 4);
  lVal :=lInINt;
end;

begin
  lImageFormatOK := true;
  lHdrOK := false;
  if not fileexists(lFileName) then begin
     lImageFormatOK := false;
     exit;
  end;
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  lFileSz := FileSize(fp);
  Clear_Dicom_Data(lDicomData);
  if lFilesz < (77) then exit; //to small to be biorad
  read16(54,lWord);
  if (lWord=12345) then begin
     lDicomData.little_endian := 1;
           read16(0,lDicomData.XYZdim[1]);
           read16(2,lDicomData.XYZdim[2]);
           read16(4,lDicomData.XYZdim[3]);
           read16(14,lWord);//byte format
           if lWord = 1 then
              lDicomData.Allocbits_per_pixel := 8
           else
               lDicomData.Allocbits_per_pixel := 16;//bits
           //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
           lDicomData.ImageStart := 76;
           read32(10,lNotes);
           lStart := (lDicomData.XYZdim[1]*lDicomData.XYZdim[2]*lDicomData.XYZdim[3])+76;
           lEnd := lStart + 96;
           lDynStr := 'BIORAD PIC image'+kCR;
           while (lNotes > 0) and (lFileSz >= lEnd)  do begin
             read32(lStart+2,lNotes); //final note has bytes 2..5 set to zero
             //read16(lStart+10,lNoteType);
             //if lNoteType <> 1 then begin //ignore 'LIVE' notes - they do not include calibration info
              seek(fp, lStart+16);
              BlockRead(fp, tx, 80{, n});
              lStr := '';
              liPos := 0;
              repeat
                  lCh := tx[liPos];
                  lByte := ord(lCh);
                  if (lByte >= 32) and (lByte <= 126) then
                     lStr := lStr+lCh
                  else lByte := 0;
                  inc(liPos);
              until (liPos = 80) or (lByte = 0);
              if length(lStr) > 6 then begin
                  lInfo := '';
                  for liPos := 1 to 6 do
                      lInfo := lInfo+upcase(lStr[liPos]);
                  ltmpstr := '';
                  lSpaces := 0;
                  for liPos := 1 to 80 do begin
                      if lStr[liPos]=' ' then inc(lSpaces)
                      else if lSpaces = 3 then
                         ltmpstr := ltmpstr + lStr[liPos];
                  end;
                 if ltmpstr = '' then {no value to read}
                 else if lInfo = 'AXIS_2' then
                     lDicomData.XYZmm[1] := strtofloat(ltmpstr)
                 else if lInfo = 'AXIS_3' then
                     lDicomData.XYZmm[2] := strtofloat(ltmpstr)
                 else if linfo = 'AXIS_4' then
                     lDicomData.XYZmm[3] := strtofloat(ltmpstr);
                  lDynStr := lDynStr+lStr+kCR;
              end; //Str length > 6
             //end;//notetype
              lStart := lEnd;
              lEnd := lEnd + 96;
           end; //while notes
           lHdrOK := true;
           //lImageFormatOK := true;
        end;//biorad signature
  CloseFile(fp);
  FileMode := 2; //set to read/write
    lDynStr := 'BioRad image'+kCR+
     'XYZ dim:' +inttostr(lDicomData.XYZdim[1])+'/'
     +inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
     +kCR+'XYZ size [mm or micron]:'+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
     +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2)
     +kCR+'Bits per sample/Samples per pixel: '+inttostr( lDICOMdata.Allocbits_per_pixel)
     +kCR+'Data offset:' +inttostr(lDicomData.ImageStart);
end; //biorad

function SiemensVersion (lStr: string): integer;
//Convert tag 0018,1020 from DICOM header to Siemens version number
(*Returned value: system is in 1000s, last two digits are version
 Siemens syngo MR 2006T 4VB12T //-> 0012
MR B13 4VB13A  //->0013
MR.VB15A123    //->0015
syngo MR B15  //-> 0015
syngo MR B17 //->0017 B= Trio, Verio, Etc
syngo MR C11 //->1011 C= Chinese C11 ~ B17
syngo MR D11 //->2011 D= Skyra D11 ~ B17*)
label 999;
var
  i,len: integer;
begin
  result := 0;
  len := length(lStr);
  if len < 3 then exit;
  for i := 1 to len-1 do
    if (upcase(lStr[i]) in ['A'..'Z']) and (lStr[i+1] in ['0'..'9']) then
      goto 999;
  exit; //not Siemens format
999:
  result :=  strtoint(lStr[i+1]);
  if lStr[i+2] in ['0'..'9'] then
    result := (result*10) + strtoint(lStr[i+2]);
  result := (100*( ord(upcase(lStr[i]))- ord('B'))) + result;
end;
(* Obsolete - replaced by SiemensVersion
function SiemensBversion (lStr: string): integer;
//'syngo MR B17' returns 17
//'MR.VB15A123' returns 15
//'syngo MR B15' returns 15
//'MR B13 4VB13A' returns 13
//'syngo MR 2006T 4VB12T' returns 12
var
   Len,P,B: integer;
   S: string;
begin
     result := 0;
     Len := length(lStr);
     if Len <2 then exit;
     B := 0;
     for P := 2 to (Len) do
         if (upcase(lStr[P-1])='B') and ( lStr[P] in ['0'..'9'])  then
            B := P;
     if B < 1 then
      exit;
     S := '';
     while (B<= Len) and (lStr[B] in ['0'..'9']) do begin
      S := S + lStr[B];
      inc(B);
     end;
     if length(S) < 1 then exit;
     result := strtoint(S);
end;*)

(*function Str2IntDig (lStr: string; lDig: integer): integer;
//robust stringtoint that strips out   any junk so that "Implementation Version Name=MR.VB15A123" returns 15
// warning, strips out decimals, so 15.3 will return 153!
//warning also ignores minus sign so -5.21 will return 521!
var
   Len,P: integer;
   S: string;
begin
     result := 0;
     Len := length(lStr);
     if Len <1 then exit;
     S := '';
     for P := 1 to Len do
         if (lStr[P] in ['0'..'9']) and (length(S) < lDig) then
            S := S + lStr[P];
     if length(S) < 1 then exit;
     result := strtoint(S);
end;  *)

function  ExpectedDicomBytes (var lDICOMdata: DICOMdata): integer;
begin
  if lDicomData.JPEGLosslessCpt then begin
    result :=  0; //actual compressed size unknown
    exit;
  end;
  result := lDicomdata.XYZdim[1]*lDicomdata.XYZdim[2]*lDicomdata.XYZdim[3]*(lDicomData.Allocbits_per_pixel DIV 8);
end;

procedure read_dicom_data_compat(lReadJPEGtables,lVerboseRead,lAutoDECAT7,lReadECAToffsetTables,lAutoDetectInterfile,lAutoDetectGenesis,lReadColorTables: boolean; var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string; var lPrefs: TPrefs);
label 666,777;
const
  kMaxTextBuf = 50000; //maximum for screen output
  kDiskCache = 16384; //size of disk buffer
  kNaNsingle : single = 1/0;

type
  dicom_types = (unknown, i8, i16, i32, ui8, ui16, ui32, _string{,_float} );
var
 // lTextF: TextFile; //abba
 lDICOMdataBackUp: DICOMdata;
 lWord,lWord2,lWord3: word;
 lWordRA: Wordp;
 lDiskCacheRA: pChar{ByteP};
 lRot1,lRot2,lRot3 : integer;//rotation dummies for AFNI
 FP: file;
   lT0,lT1,lT2,lT3:byte;
  lImagePositionPatientRead,
  lResearchMode,lManufacturerIsPhilips,lManufacturerIsBruker,lMediface0002_0013,lSiemensMosaic0008_0008,lDICM_at_128, lTextOverFlow,lGenesis,lFirstPass,lrOK,lBig,lBigSet,lGrp,explicitVR,first_one    : Boolean;
  lSwitchToImplicitAfterGroup0002, lTestError,lByteSwap,lGELX,time_to_quit,lProprietaryImageThumbnail,lFirstFragment,lOldSiemens_IncorrectMosaicMM : Boolean;
  group, element, e_len, remaining, tmp : uint32;
  tmpstr : kDICOMstr;
  lgrpstr,lStr,info,lDummyStr   : string;
  t      : dicom_types;
  lfloat1,lfloat2,lfloat3,lThickness: double;
  lTempInt,lEchoNum,lnVol,lnSlicePerVol,lJPEGentries,lErr,liPos,lCacheStart,lCachePos,lDiskCacheSz,n, i,value, Ht,Width,
  max16,min16,filesz,where,lMatrixSz,lPhaseEncodingSteps,lJunk,lJunk2,lJunk3 : LongInt;
  tx     : array [0..96] of Char;
  l4DDistanceBetweenSliceCenters,lPhilipsScaleSlope: single;
  buff: pCHar;
  lColorRA: bytep;
  lLongRA: Longintp;
  lSingleRA,lInterceptRA: Singlep;
  //lPapyrusnSlices,lPapyrusSlice : integer;
  //lPapyrusZero,lPapyrus : boolean;
procedure ByteSwap (var lInOut: integer);
var lWord: word;
begin
     lWord := lInOut;
     lWord := swap(lWord);
     lInOut := lWord;
end;
procedure dReadCache (lFileStart: integer);
begin
  lCacheStart := lFileStart{lCacheStart + lDiskCacheSz};//eliminate old start
  if lCacheStart < 0 then lCacheStart := 0;
  if lDiskCacheSz > 0 then freemem(lDiskCacheRA);
  if (FileSz-(lCacheStart)) < kDiskCache then
     lDiskCacheSz := FileSz - (lCacheStart)
  else
      lDiskCacheSz := kDiskCache;
  lCachePos := 0;
  if (lDiskCacheSz < 1) then exit{goto 666};
  if (lDiskCacheSz+lCacheStart) > FileSz then exit;
  Seek(fp, lCacheStart);

  GetMem(lDiskCacheRA, lDiskCacheSz {bytes});
  BlockRead(fp, lDiskCacheRA^, lDiskCacheSz, n);
end;

function dFilePos (var lInFP: file): integer;
begin
     Result := lCacheStart + lCachePos;
end;
procedure dSeek (var lInFP: file; lPos: integer);
begin
  if (lPos >= lCacheStart) and (lPos < (lDiskCacheSz+lCacheStart)) then begin
     lCachePos := lPos-lCacheStart;
     exit;
  end;
  dReadCache(lPos);
end;

procedure dBlockRead (var lInfp: file; lInbuff: pChar; e_len: integer; var n: integer);
var lN: integer;
begin
     N := 0;
     if e_len < 0 then exit;
     for lN := 0 to (e_len-1) do begin
         if lCachePos >= lDiskCacheSz then begin
            dReadCache(lCacheStart+lDiskCacheSz);
            if lDiskCacheSz < 1 then exit;
            lCachePos := 0;
         end;
         N := lN;
         lInBuff[N] := lDiskCacheRA[lCachePos];
         inc(lCachePos);
     end;
end;
procedure readfloats (var fp: file; remaining: integer; var lOutStr: string; var lf1, lf2: double; var lReadOK: boolean);
var  lDigit : boolean;
   li,lLen,n: integer;
    lfStr: string;
begin
    lf1 := 1;
    lf2 := 2;
    if e_len = 0 then begin
       lReadOK := true;
       exit;
    end;
    if (dFilePos(fp) > (filesz-remaining)) or (remaining < 1) then begin
       lOutStr := '';
       lReadOK := false;
       exit;
    end else
        lReadOK := true;
    lOutStr := '';
    GetMem( buff, e_len);
    dBlockRead(fp, buff{^}, e_len, n);
	for li := 0 to e_len-1 do
		if Char(buff[li]) in [{'/','\', delete: rev18}'e','E','+','-','.','0'..'9']
           then lOutStr := lOutStr +(Char(buff[li]))
        else begin
             lOutStr := lOutStr + ' ';
        end;
    FreeMem( buff);
    lfStr := '';
    lLen := length(lOutStr);

    li := 1;
    lDigit := false;
    repeat
      if (lOutStr[li] in ['+','-','e','E','.','0'..'9']) then
         lfStr := lfStr + lOutStr[li];
      if lOutStr[li] in ['0'..'9'] then lDigit := true;
      inc(li);
    until (li > lLen) or (lDigit);
    if not lDigit then exit;
    if li <= li then begin
       repeat
             if not (lOutStr[li] in ['+','-','e','E','.','0'..'9']) then lDigit := false
             else begin
                  if lOutStr[li] = 'E' then lfStr := lfStr+'e'
                  else
                      lfStr := lfStr + lOutStr[li];
             end;
             inc(li);
       until (li > lLen) or (not lDigit);
    end;
    //QStr(lfStr);
    try
       lf1 := strtofloat(lfStr);
    except
          on EConvertError do begin
             Msg('Unable to convert the string '+lfStr+' to a real number');
             lf1 := 1;
             exit;
          end;
    end; {except}
    lfStr := '';
    if li > llen then exit;
    repeat
             if (lOutStr[li] in ['+','E','e','.','-','0'..'9']) then begin
                  if lOutStr[li] = 'E' then lfStr := lfStr+'e'
                  else
                      lfStr := lfStr + lOutStr[li];
             end;
             if (lOutStr[li] in ['0'..'9']) then lDigit := true;
             inc(li);
    until (li > lLen) or ((lDigit) and (lOutStr[li]=' ')); //second half: rev18
    if not lDigit then exit;
    //QStr(lfStr);
    try
       lf2 := strtofloat(lfStr);
    except
          on EConvertError do begin
             Msg('Unable to convert the string '+lfStr+' to a real number');
             exit;
          end;
    end;

end;

procedure readfloats3 (var fp: file; remaining: integer; var lOutStr: string; var lf1, lf2,lf3: double; var lReadOK: boolean);
var  lDigit : boolean;
   lItem,li,lLen,n: integer;
    lfTemp: double;
    lfStr: string;
begin
    lf1 := 0;
    lf2 := 0;
    lf3 := 0;
    lOutStr := '';
    if e_len = 0 then begin
       lReadOK := true;
       exit;
    end;
    if (dFilePos(fp) > (filesz-remaining)) or (remaining < 1) then begin
       lReadOK := false;
       exit;
    end else
        lReadOK := true;
    GetMem( buff, e_len);
    dBlockRead(fp, buff{^}, e_len, n);
    for li := 0 to e_len-1 do
        if Char(buff[li]) in [{'/','\', delete: rev18}'e','E','+','-','.','0'..'9']
           then lOutStr := lOutStr +(Char(buff[li]))
        else lOutStr := lOutStr + ' ';
    FreeMem( buff);
	li := 1;
	lLen := length(lOutStr);
 for lItem := 1 to 3 do begin
    if li > llen then exit;
	lfStr := '';
	lLen := length(lOutStr);
    lDigit := false;
    repeat
      if (lOutStr[li] in ['+','-','e','E','.','0'..'9']) then
         lfStr := lfStr + lOutStr[li];
      if lOutStr[li] in ['0'..'9'] then lDigit := true;
      inc(li);
    until (li > lLen) or (lDigit);
    if not lDigit then exit;
    if li <= li then begin
       repeat
             if not (lOutStr[li] in ['+','-','e','E','.','0'..'9']) then lDigit := false
             else begin
                  if lOutStr[li] = 'E' then lfStr := lfStr+'e'
                  else
                      lfStr := lfStr + lOutStr[li];
             end;
             inc(li);
       until (li > lLen) or (not lDigit);
    end;
    //QStr(lfStr);
    try
       lftemp := strtofloat(lfStr);
    except
          on EConvertError do begin
             Msg('Unable to convert the string '+lfStr+' to a real number');
             //lftemp := 0;
             exit;
          end;
    end; {except}
    case lItem of
         2: lf2 := lftemp;
         3: lf3 := lftemp;
		 else lf1 := lftemp;
	end; //case of lItem
end; //for each of 3 lItems
end; //readfloats3

procedure CheckIntersliceDistance (var lMinDistance: single);
var
   lX,lY,lZ,lDx: double;
begin
             readfloats3 (fp, remaining, lDummyStr, lX, lY,lZ, lROK);
             //     fx(   lX, lY,lZ,6789);
             e_len := 0;
             remaining := 0;
             //compute Distance between current slice and 1st slice...
             lDx := sqrt( sqr(lX-lDicomData.PatientPosX)+sqr(lY-lDicomData.PatientPosY)+sqr(lZ-lDicomData.PatientPosZ));
             if (lDx > 0) and (lMinDistance = kNaNsingle) then //first value
                lMinDistance := lDx
             else if (lDx > 0) and (lDx < lMinDistance) then //if 0 then this is a repeat, not a new slice
                lMinDistance := lDx
             else
                exit;
end;

procedure readfloats6 (var fp: file; remaining: integer; var lOutStr: string; var lf1, lf2,lf3,lf4,lf5,lf6: double; var lReadOK: boolean);
var  lDigit : boolean;
   lItem,li,lLen,n: integer;
	lfTemp: single;
	lfStr: string;
begin
	lf1 := 0;
	lf2 := 0;
	lf3 := 0;
	lf4 := 0;
	lf5 := 0;
	lf6 := 0;
	lOutStr := '';
	if e_len = 0 then begin
	   lReadOK := true;
	   exit;
	end;
	if (dFilePos(fp) > (filesz-remaining)) or (remaining < 1) then begin
	   lReadOK := false;
	   exit;
	end else
		lReadOK := true;
	GetMem( buff, e_len);
	dBlockRead(fp, buff{^}, e_len, n);
	for li := 0 to e_len-1 do
		if Char(buff[li]) in [{'/','\', delete: rev18}'e','E','+','-','.','0'..'9']
		   then lOutStr := lOutStr +(Char(buff[li]))
		else lOutStr := lOutStr + ' ';
	FreeMem( buff);
	li := 1;
	lLen := length(lOutStr);
 for lItem := 1 to 6 do begin
	if li > llen then exit;
	lfStr := '';
	lLen := length(lOutStr);
	lDigit := false;
	repeat
	  if (lOutStr[li] in ['+','-','e','E','.','0'..'9']) then
		 lfStr := lfStr + lOutStr[li];
	  if lOutStr[li] in ['0'..'9'] then lDigit := true;
	  inc(li);
	until (li > lLen) or (lDigit);
	if not lDigit then exit;
	if li <= li then begin
	   repeat
			 if not (lOutStr[li] in ['+','-','e','E','.','0'..'9']) then lDigit := false
			 else begin
				  if lOutStr[li] = 'E' then lfStr := lfStr+'e'
				  else
					  lfStr := lfStr + lOutStr[li];
			 end;
			 inc(li);
	   until (li > lLen) or (not lDigit);
	end;

	//QStr(lfStr);
	try
	   lftemp := strtofloat(lfStr);
	except
		  on EConvertError do begin
			 Msg('Unable to convert the string '+lfStr+' to a real number');
			 //lftemp := 0;
			 exit;
		  end;
	end; {except}
	case lItem of
		 2: lf2 := lftemp;
		 3: lf3 := lftemp;
		 4: lf4 := lftemp;
		 5: lf5 := lftemp;
		 6: lf6 := lftemp;
		 else lf1 := lftemp;
	end; //case of lItem
end; //for each of 3 lItems
end;

function read16( var fp : File; var lReadOK: boolean ): uint16;
var
	t1, t2 : uint8;
  n      : Integer;
begin
if dFilePos(fp) > (filesz-2) then begin
   read16 := 0;
   lReadOK := false;
   exit;
end else
    lReadOK := true;
    GetMem( buff, 2);
    dBlockRead(fp, buff{^}, 2, n);
    T1 := ord(buff[0]);
    T2 := ord(buff[1]);
    freemem(buff);
    if lDICOMdata.little_endian <> 0
  	then Result := (t1 + t2*256) AND $FFFF
  	else Result := (t1*256 + t2) AND $FFFF;
end;

function  ReadStr(var fp: file; remaining: integer; var lReadOK: boolean; VAR lmaxval:integer) : string;
var lInc, lN,Val,n: integer;
	t1, t2 : uint8;
     lStr : String;
begin
lMaxVal := 0;
if dFilePos(fp) > (filesz-remaining) then begin
   lReadOK := false;
   exit;
end else
    lReadOK := true;
    Result := '';
    lN := remaining div 2;
    if lN < 1 then exit;
    lStr := '';
    for lInc := 1 to lN do begin
        GetMem( buff, 2);
        dBlockRead(fp, buff{^}, 2, n);
        T1 := ord(buff[0]);
        T2 := ord(buff[1]);
        freemem(buff);
        if lDICOMdata.little_endian <> 0 then
           Val := (t1 + t2*256) AND $FFFF
        else
            Val := (t1*256 + t2) AND $FFFF;
        if lInc < lN then
           lStr := lStr + inttostr(Val)+ ', '
        else
            lStr := lStr + inttostr(Val);
        if Val > lMaxVal then
           lMaxVal := Val;
    end;
    Result := lStr;
    if odd(remaining) then begin
           getmem(buff,1);
       dBlockRead(fp, buff{t1}, SizeOf(uint8), n);
           freemem(buff);
    end;
end;

(*function  ReadStrABC(var fp: file; remaining: integer; var lReadOK: boolean; VAR lA,lB,lC:integer) : string;
var lInc, lN,Val,n: integer;
	t1, t2 : uint8;
     lStr : String;
begin
lA := 0;
lB := 0;
lC := 0;
if dFilePos(fp) > (filesz-remaining) then begin
   lReadOK := false;
   exit;
end else
    lReadOK := true;
    Result := '';
    lN := remaining div 2;
    if lN < 1 then exit;
    lStr := '';
    for lInc := 1 to lN do begin
        GetMem( buff, 2);
        dBlockRead(fp, buff{^}, 2, n);
        T1 := ord(buff[0]);
        T2 := ord(buff[1]);
        freemem(buff);
        if lDICOMdata.little_endian <> 0 then
           Val := (t1 + t2*256) AND $FFFF
        else
            Val := (t1*256 + t2) AND $FFFF;
        if lInc < lN then
           lStr := lStr + inttostr(Val)+ ', '
        else
            lStr := lStr + inttostr(Val);
        if lInc = 1 then
           lA := Val;
        if lInc = 2 then
           lB := Val;
        if lInc = 3 then
           lC := Val;


    end;
    Result := lStr;
    if odd(remaining) then begin
           getmem(buff,1);
       dBlockRead(fp, buff{t1}, SizeOf(uint8), n);
           freemem(buff);
    end;
end; *)

function  ReadStrHex(var fp: file; remaining: integer; var lReadOK: boolean) : string;
var lInc, lN,Val,n: integer;
	t1, t2 : uint8;
     lStr : String;
begin
if dFilePos(fp) > (filesz-remaining) then begin
   lReadOK := false;
   exit;
end else
    lReadOK := true;
    Result := '';
    lN := remaining div 2;
    if lN < 1 then exit;
    lStr := '';
    for lInc := 1 to lN do begin
         GetMem( buff, 2);
    dBlockRead(fp, buff, 2, n);
    T1 := ord(buff[0]);
    T2 := ord(buff[1]);
    freemem(buff);
     if lDICOMdata.little_endian <> 0 then
        Val := (t1 + t2*256) AND $FFFF
     else
         Val := (t1*256 + t2) AND $FFFF;
     if lInc < lN then lStr := lStr + 'x'+inttohex(Val,4)+ ', '
     else lStr := lStr + 'x'+inttohex(Val,4);
    end;
    Result := lStr;
    if odd(remaining) then begin
       getmem(buff,1);
       dBlockRead(fp, {t1}buff, SizeOf(uint8), n);
       freemem(buff);
    end;
end;
function SomaTomFloat: double;
var lSomaStr: String;
begin
     //dSeek(fp,5992); //Slice Thickness from 5790 "SL   3.0"
     //dSeek(fp,5841); //Field of View from 5838 "FoV   281"
     //dSeek(fp,lPos);
     lSomaStr := '';
     tx[0] := 'x';
     while (length(lSomaStr) < 64) and (tx[0] <> chr(0))  and (tx[0] <> '/') do begin
                dBlockRead(fp, tx, 1, n);
                if tx[0] in ['+','-','.','0'..'9','e','E'] then
                   lSomaStr := lSomaStr + tx[0];
     end;
     if length(lSOmaStr) > 0 then
        result := StrToFloat(lSomaStr)
     else
         result := 0;
end;

function PGMreadInt: integer;
//reads integer from PGM header, disregards comment lines (which start with '#' symbol);
var lStr: string;
    lDigit: boolean;

begin
    Result := 1;
    lStr := '';
    repeat
          dBlockRead(fp, tx, 1, n);
          if tx[0] = '#' then begin //comment
             repeat
                   dBlockRead(fp, tx, 1, n);
             until (ord(tx[0]) = $0A) or (dFilePos(fp) > (filesz-4)); //eoln indicates end of comment
          end; //finished reading comment
          if tx[0] in ['0'..'9'] then begin
             lStr := lStr + tx[0];
             lDigit := true;
          end else
              lDigit := false;
    until ((lStr <> '') and (not lDigit)) or (dFilePos(fp) > (filesz-4)); //read digits until you hit whitespace
    if lStr <> '' then
       Result := strtoint(lStr);

     {lStr := '';
     tx[0] := 'x';
     while (length(lStr) < 64) and (ord(tx[0]) <> $0A) do begin
                dBlockRead(fp, tx, 1, n);
                if tx[0] in ['#','+','-','.','0'..'9','e','E',' ','a'..'z','A'..'Z'] then
                   lStr := lStr + tx[0];
     end;
     result := lStr;    }
end;

function read32 ( var fp : File; var lReadOK: boolean ): uint32;
var
	t1, t2, t3, t4 : byte;
  n : Integer;
begin
if dFilePos(fp) > (filesz-4) then begin
   Read32 := 0;
   lReadOK := false;
   exit;
end else
    lReadOK := true;
    GetMem( buff, 4);
    dBlockRead(fp, buff{^}, 4, n);
    T1 := ord(buff[0]);
    T2 := ord(buff[1]);
    T3 := ord(buff[2]);
    T4 := ord(buff[3]);
    freemem(buff);
    if lDICOMdata.little_endian <> 0 then
        Result := t1 + (t2 shl 8) + (t3 shl 16) + (t4 shl 24)
    else
        Result := t4 + (t3 shl 8) + (t2 shl 16) + (t1 shl 24)
    //if lDICOMdata.little_endian <> 0
    //then Result := (t1 + t2*256 + t3*256*256 + t4*256*256*256) AND $FFFFFFFF
    //else Result := (t1*256*256*256 + t2*256*256 + t3*256 + t4) AND $FFFFFFFF;
end;

function read32r ( var fp : File; var lReadOK: boolean ): single; //1382
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(float:single);
  end;
  swaptypep = ^swaptype;
var
   s:single;
  inguy:swaptypep;
  outguy:swaptype;
begin
  if dFilePos(fp) > (filesz-4) then begin
     read32r := 0;
     lReadOK := false;
     exit;
  end else
  lReadOK := true;
    //GetMem( buff, 8);
  dBlockRead(fp, @s, 4, n);
  inguy := @s; //assign address of s to inguy
  if lDICOMdata.little_endian <> 1 then begin
     outguy.Word1 := swap(inguy^.Word2);
     outguy.Word2 := swap(inguy^.Word1);
  end else
      outguy.float  := s; //1382 read64 needs to handle little endian in this way as well...
  read32r:=outguy.float;
end;

function read64 ( var fp : File; var lReadOK: boolean ): double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(float:double);
  end;
  swaptypep = ^swaptype;
var
   s:double;
  inguy:swaptypep;
  outguy:swaptype;
begin
  if dFilePos(fp) > (filesz-8) then begin
     Read64 := 0;
     lReadOK := false;
     exit;
  end else
    lReadOK := true;
    //GetMem( buff, 8);
  dBlockRead(fp, @s, 8, n);
  inguy := @s; //assign address of s to inguy
  if lDICOMdata.little_endian <> 1 then begin
     outguy.Word1 := swap(inguy^.Word4);
     outguy.Word2 := swap(inguy^.Word3);
     outguy.Word3 := swap(inguy^.Word2);
     outguy.Word4 := swap(inguy^.Word1);
  end else
      outguy.float := inguy^.float; //1382
  read64:=outguy.float;
end;

//magma
function SafeStrToInt(var lInput: string): integer;
var li,lLen: integer;
begin
     result := 0;
              lLen := length(lInput);
              lStr := '';
              if lLen < 1 then exit;
              for li := 1 to lLen do
                 if lInput[li] in ['+','-','0'..'9']
                     	then lStr := lStr +lInput[li];
              Val(lStr,li,lErr);
              if lErr = 0 then
               result := lI;//strtoint(lStr);
end;


procedure DICOMHeaderStringToInt (var lInput: integer);
var li: integer;
begin
              t := _string;
              lStr := '';
              if dFilePos(fp) > (filesz-e_len) then exit;//goto 666;
              GetMem( buff, e_len);
              dBlockRead(fp, buff{^}, e_len, n);
              for li := 0 to e_len-1 do
                   	if Char(buff[li]) in ['+','-','0'..'9']
                     	then lStr := lStr +(Char(buff[li]));
              FreeMem( buff);
              Val(lStr,li,lErr);
              if lErr = 0 then lInput := li;//strtoint(lStr);
              remaining := 0;
              tmp := lInput;
end;

procedure DICOMHeaderString (var lInput: kDICOMStr);
var li,lStartPos: integer;
begin
     t := _string;
             lStartPos := dFilePos(fp);
             lInput := '';
             if e_len < 1 then exit; //DICOM: should always be even
             GetMem( buff, e_len);
             dBlockRead(fp, buff{^}, e_len, n);
             for li := 0 to e_len-1 do
                   	if Char(buff[li]) in ['+','-','/','\',' ','0'..'9','a'..'z','A'..'Z'] then
                            lInput :=  lInput +(Char(buff[li]))
                   	else {if (buff[i] = 0) then}
                             lInput :=  lInput +' ';

              FreeMem( buff);
              dseek(fp, lStartPos);
end;
procedure DICOMHeaderStringTime (var lInput: kDICOMstr);
var li,lStartPos: integer;
begin
     t := _string;
             lStartPos := dFilePos(fp);
             lInput := '';
             if e_len < 1 then exit; //DICOM: should always be even
             GetMem( buff, e_len);
             dBlockRead(fp, buff{^}, e_len, n);
             for li := 0 to e_len-1 do
                   	if Char(buff[li]) in ['+','-','/','\',' ','0'..'9','a'..'z','A'..'Z','.'] then
                            lInput :=  lInput +(Char(buff[li]))
                   	else if li <> (e_len-1) then
                             lInput :=  lInput +':'
                        else
                            lInput :=  lInput +' ';

              FreeMem( buff);
              dseek(fp, lStartPos);
end;
label 1234;
var lIndent: integer;
  lprevGroup, lprevElement: uint32;
var lInside00209113, lInside2005140F, lPhilipsWarning: boolean;//philips can list two DIFFERENT spatial positions per slice - ignore the one hidden inside 2005,140FlPrev0020: boolean;
begin
  //Init
  //for lnVol := 1 to kMaxOrderVal do
  //    lDICOMdata.OrderSlope[lDICOMdata.nOrder] := 0; //show this was not set
  lInside00209113 := false;
  lprevGroup := 0;
  lprevElement := 0;
  lPhilipsWarning := false;
  lIndent := 0;
  lInside2005140F := false;
  lSwitchToImplicitAfterGroup0002 := false;
  lGELX := false;
  lByteSwap := false;
  Clear_Dicom_Data(lDicomData);
  Clear_Dicom_Data(lDICOMdataBackUp);
  lDicomData.XYZdim[1] := 1;
  lImagePositionPatientRead := false;// for 4D files, we need first volume
  l4DDistanceBetweenSliceCenters := kNaNsingle;
  lEchoNum := 0;
  lThickness := 0;
  lTestError := false;
  lPhilipsScaleSlope := 0;
  lManufacturerIsPhilips := false;
  lManufacturerIsBruker := false;
  lnVol := 0;
  lnSlicePerVol := 0;
  lResearchMode := false;
  lMatrixSz := 0;
  lPhaseEncodingSteps := 0;
  lSiemensMosaic0008_0008 := false;
  lMediface0002_0013 := false;//false wblate
  lOldSiemens_IncorrectMosaicMM := false;
  lCacheStart := 0;
  lDiskCacheSz := 0;
  lDynStr:= '';
  lJPEGEntries := 0;
  first_one    := true;
  info := '';
  lGrp:= false;
  lBigSet := false;
  lDICM_at_128 := false; //no DICOM signature
  lFirstFragment := true;
  lTextOverFlow := false;
  lImageFormatOK := true;
  lHdrOK := false;
  //if lverboseRead then msg('xxx'+lFileName);
  if not fileexists(lFileName) then begin
     lImageFormatOK := false;
     exit;
  end;
  //if lverboseRead then msg('zzzzz000000000');
  TmpStr := string(StrUpper(PChar(ExtractFileExt(lFileName))));
  lStr :='';
  if TmpStr = '.FDF' then begin
     if FDF( lFileName, lDicomData) then begin
        lHdrOK := true;
        lImageFormatOK := true;
        exit;
     end;
  end;
  if (TmpStr = '.REC') then begin //1417z: check in Unix: character upper/lower case may matter
        lStr := changefileext(lFilename,'.par');
        if fileexists(lStr) then
                lFilename := lStr
        else begin //Linux is case sensitive 1382...
                lStr := changefileext(lFilename,'.PAR');
                if fileexists(lStr) then
                        lFilename := lStr
        end;
  end;
  if (TmpStr = '.BRIK') then begin //1417z: check in Unix: character upper/lower case may matter
        lStr := changefileext(lFilename,'.HEAD');
        if fileexists(lStr) then lFilename := lStr;
  end;
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  FIleSz := FileSize(fp);
  if fileSz < 1 then begin
     lImageFormatOK := false;
     exit;
  end;
     lDICOMdata.Little_Endian := 1;
  if FileSz > 200 then begin
     dseek(fp, {0}128);
    dBlockRead(fp, tx, 4*SizeOf(Char), n);
     if (tx[0] = 'D') and (tx[1] = 'I') and (tx[2] = 'C') and (tx[3] = 'M') then
        lDICM_at_128 := true;
  end;//filesize > 200: check for 'DICM' at byte 128 - DICOM signature
  if (lAutoDetectGenesis) and (FileSz > (5820{114+35+4})) then begin
     dseek(fp, 0);
     if (ord(tx[0])=206) and (ord(tx[1])=250) then begin
        //Elscint format signature: check height and width to make sure

           dseek(fp, 370);
           group   := read16(fp,lrOK);//Width
           dseek(fp, 372);
           element := read16(fp,lrOK);//Ht
           if ((Group=160) or(Group =256) or (Group= 340) or (Group=512) or (group =640)) and
           ((element=160) or (element =256) or (element= 340) or (element=512) ) then begin
                     CloseFile(fp);
                     if lDiskCacheSz > 0 then
                        freemem(lDiskCacheRA);
                     FileMode := 2; //set to read/write
                     read_elscint_data(lDICOMdata, lHdrOK, lImageFormatOK,lDynStr,lFileName);
                     exit;
           end; //confirmed: Elscint
     end;
     lGenesis := false;
     if ((tx[0] <> 'I') OR (tx[1] <> 'M') OR (tx[2] <> 'G') OR (tx[3] <> 'F')) then begin {DAT format}
        {if (FileSz > 114+305+4) then begin
           dseek(fp, 114+305);
           dBlockRead(fp, tx, 3*SizeOf(Char), n);
           if ((tx[0]='M') and (tx[1] = 'R')) or ((tx[0] = 'C') and(tx[1] = 'T')) then
              lGenesis := true;
        end;}
     end else
         lGenesis := true;
     if (not lGenesis) and (FileSz > 3252) then begin
        dseek(fp, 3240);
        dBlockRead(fp, tx, 4*SizeOf(Char), n);
        if ((tx[0] = 'I') AND (tx[1] = 'M')AND (tx[2] = 'G') AND (tx[3] = 'F')) then
           lGenesis := true;
        if (not lGenesis) then begin
           dseek(fp, 3178);
           dBlockRead(fp, tx, 4*SizeOf(Char), n);
           if ((tx[0] = 'I') AND (tx[1] = 'M')AND (tx[2] = 'G') AND (tx[3] = 'F')) then
              lGenesis := true;
        end;
        if (not lGenesis) then begin
           dseek(fp, 3180);
           dBlockRead(fp, tx, 4*SizeOf(Char), n);
           if ((tx[0] = 'I') AND (tx[1] = 'M')AND (tx[2] = 'G') AND (tx[3] = 'F')) then
              lGenesis := true;
        end;
        if (not lGenesis) then begin //1499K
           dseek(fp, 0);
           dBlockRead(fp, tx, 4*SizeOf(Char), n);
           if ((tx[0] = 'I') AND (tx[1] = 'M')AND (tx[2] = 'G') AND (tx[3] = 'F')) then
              lGenesis := true;
        end;

     end;
     if (not lGenesis) and (FileSz > 3252) then begin
           dseek(fp, 3228);
           dBlockRead(fp, tx, 4*SizeOf(Char), n);
           if (tx[0] = 'I') AND (tx[1]= 'M') AND (tx[2] = 'G')AND (tx[3]= 'F') then
              lGenesis := true;
     end;
     if lGenesis then begin
        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        read_ge_data(lDICOMdata, lHdrOK, lImageFormatOK,lDynStr,lFileName);
        exit;
     end;
  end; //AutodetectGenesis                        xxDCIM

  if (lAutoDetectInterfile) and (FileSz > 256) and (not lDICM_at_128) then begin
     if Copy(extractfilename(lFileName), 1, 4) = 'COR-' then begin
        lStr := extractfiledir(lFilename) + '\COR-.info';
        TmpStr := extractfiledir(lFilename) + '\COR-128';
        if fileexists(lStr) and fileexists(TmpStr) then begin
           lFilename := TmpStr;
           lDynStr                        := 'FreeSurfer COR format' + kCR+'Only displaying image 128'+kCR+'Use MRIcro''s Import menu to convert this image'+kCR;
           with lDicomData do begin
                little_endian       := 0; // don't care
                ImageStart          := 0;
                Allocbits_per_pixel := 8;
                XYZdim[1]           := 256;
                XYZdim[2]           := 256;
                XYZdim[3]           := 1;
                XYZmm[1]            := 1;
                XYZmm[2]            := 1;
                XYZmm[3]            := 1;
                //xStoredbits_per_pixel:= Allocbits_per_pixel;
           END; //WITH
           lHdrOK                         := True;
           lImageFormatOK                 := True;
           exit;
        end; //COR-.info file exists
     end; //if filename is COR-
     //start TIF
     //TIF IMAGES DO NOT ALWAYS HAVE EXTENSION if (TmpStr = '.TIF') or (TmpStr = '.TIFF') then begin
        dseek(fp, 0);
        lWord   := read16(fp,lrOK);
        if lWord = $4d4d then
           lDICOMdata.little_endian := 0
        else if lWord = $4949 then lDICOMdata.little_endian := 1;
        //dseek(fp, 2);
        lWord2   := read16(fp,lrOK); //bits per pixel
        if ((lWord=$4d4d) or (lWord=$4949)) and (lWord2 = $002a) then begin
           CloseFile(fp);
           if lDiskCacheSz > 0 then
              freemem(lDiskCacheRA);
           FileMode := 2; //set to read/write
           read_tiff_data(lDICOMdata, lReadECAToffsetTables, lHdrOK, lImageFormatOK, lDynStr, lFileName);
           //if lHdrOk then exit;
           exit;
        end;//TIF signature
     //end; //.TIF extension
     //end TIF
     //start BMP 1667
     TmpStr := string(StrUpper(PChar(ExtractFileExt(lFileName))));
     if TmpStr = '.BMP' then begin
        dseek(fp, 0);
        lWord   := read16(fp,lrOK);
        dseek(fp, 28);
        lWord2   := read16(fp,lrOK); //bits per pixel
        if (lWord=19778) and (lWord2 = 8) then begin //bitmap signature
           dseek(fp, 10);
           lDicomData.ImageStart := read32(fp,lrOK);//1078;
           dseek(fp, 18);
           lDicomData.XYZdim[1] := read32(fp,lrOK);
           //dseek(fp, 22);
           lDicomData.XYZdim[2] := read32(fp,lrOK);
           lDicomData.XYZdim[3] := 1;//read16(fp,lrOK);
           lDicomData.Allocbits_per_pixel := 8;//bits
           //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
           lDynStr := 'BMP format';
           CloseFile(fp);
           if lDiskCacheSz > 0 then
              freemem(lDiskCacheRA);
           FileMode := 2; //set to read/write
           lHdrOK := true;
           lImageFormatOK:= true;
           exit;
        end;//bmp signature
     end; //.BMP extension
     //end BMP
     if TmpStr = '.VOL' then begin //start SPACE vol format 1382
        dseek(fp, 0);
        dBlockRead(fp, tx, 6*SizeOf(Char), n);
        if (tx[0] = 'm') and (tx[1] = 'd') and (tx[2] = 'v') and (tx[3] = 'o') and (tx[4] = 'l') and (tx[5] = '1') then begin
           lDicomData.ImageStart := read32(fp,lrOK);//1078;
           lDICOMdata.little_endian := 1;
           lDicomData.XYZdim[1] := read32(fp,lrOK);
           lDicomData.XYZdim[2] := read32(fp,lrOK);
           lDicomData.XYZdim[3] := read32(fp,lrOK);
           lDicomData.XYZmm[1] := read32r(fp,lrOK);
           lDicomData.XYZmm[2] := read32r(fp,lrOK);
           lDicomData.XYZmm[3] := read32r(fp,lrOK);
           lDicomData.Allocbits_per_pixel := 8;//bits
           //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
           lDynStr := 'Space VOL format';
           CloseFile(fp);
           if lDiskCacheSz > 0 then
              freemem(lDiskCacheRA);
           FileMode := 2; //set to read/write
           lHdrOK := true;
           lImageFormatOK:= true;
           exit;
        end;//vol signature
     end; //.VOL extension
     //end space .VOL format
     //start DF3 PovRay DF3 density files
     if (TmpStr = '.DF3') then begin
        dseek(fp, 0);
        lWord   := swap (read16(fp,lrOK));
        lWord2   := swap (read16(fp,lrOK));
        lWord3  := swap (read16(fp,lrOK));
        //note: I assume all df3 headers are little endian. is this always true? if not, unswapped values could be tested for filesize
        lMatrixSz := (lWord*lWord2*lWord3)+6;
        if (lMatrixSz=FileSz)then begin //df3 signature
           lDicomData.ImageStart := 6;//1078;
           lDicomData.XYZdim[1] := lWord;
           //dseek(fp, 22);
           lDicomData.XYZdim[2] := lWord2;
           lDicomData.XYZdim[3] := lWord3;
           lDicomData.Allocbits_per_pixel := 8;//bits
           //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
           CloseFile(fp);
           if lDiskCacheSz > 0 then
              freemem(lDiskCacheRA);
           FileMode := 2; //set to read/write
           lDynStr := 'PovRay DF3 density format';
           lHdrOK := true;
           lImageFormatOK:= true;
           exit;
        end;//df3 signature
     end;
     //end df3

     //start .PGM
     if (TmpStr = '.PGM') or (TmpStr = '.PPM') then begin
        dseek(fp, 0);
        lWord   := read16(fp,lrOK);
        if (lWord=13648){'P5'=1x8BIT GRAYSCALE} or (lWord=13904) {'P6'=3x8bit RGB} then begin //bitmap signature
          {repeat
                PGMreadStr(lDicomData.XYZdim[1],lDicomData.XYZdim[2]);
          until (lDicomData.XYZdim[2] > 0) ;}
          lDicomData.XYZdim[1] := PGMreadInt;
          lDicomData.XYZdim[2] := PGMreadInt;
          PGMreadInt; //read maximum value

           lDicomData.XYZdim[3] := 1;//read16(fp,lrOK);
           lDicomData.Allocbits_per_pixel := 8;//bits
           //xlDicomData.Storedbits_per_pixel:= lDicomData.Allocbits_per_pixel;
           lDicomData.ImageStart := dFilepos(fp);
          if lWord = 13904 then begin//RGB
             //xlDicomData.SamplesPerPixel := 3;
             //xlDicomData.PlanarConfig := 0;//RGBRGBRGB..., not RRR..RGGG..GBBB...B
          end;
           lDynStr:='PGM/PPM format 8-bit grayscale image [data saved in binary, not ASCII format]';
           CloseFile(fp);
           if lDiskCacheSz > 0 then
              freemem(lDiskCacheRA);
           FileMode := 2; //set to read/write
           lHdrOK := true;
           lImageFormatOK:= true;
           exit;
        end else if (lWord=12880){'P2'=1x8BIT ASCII} or (lWord=13136) {'P3'=3x8bit ASCI} then begin
            Msg('Warning: this image appears to be an ASCII ppm/pgm image. This software can only read binary ppm/pgm images');
        end;//pgm/ppm binary signature signature
     end; //.PPM/PGM extension

     //end .pgm

     //start BioRadPIC 1667
     if TmpStr = '.PIC' then begin
        dseek(fp, 54);
        lWord   := read16(fp,lrOK);
        if (lWord=12345) then begin
           CloseFile(fp);
           if lDiskCacheSz > 0 then
              freemem(lDiskCacheRA);
           FileMode := 2; //set to read/write
           read_biorad_data(lDICOMdata, lHdrOK, lImageFormatOK,lDynStr,lFileName);
           exit;
        end;//biorad signature
     end; //.PIC extension biorad?
     //end BIORAD PIC
     if TmpStr = '.HEAD' then begin
        read_afni_data(lDICOMdata, lHdrOK, lImageFormatOK, lDynStr, lFileName,lRot1,lRot2,lRot3);
        if (lHdrOK) and (lImageFormatOK) then begin
           CloseFile(fp);
           if lDiskCacheSz > 0 then
              freemem(lDiskCacheRA);
           FileMode := 2; //set to read/write
           exit;
        end;
     end;
     dseek(fp, 0);
     dBlockRead(fp, tx, 20*SizeOf(Char), n);
     if (tx[0] = 'n') and (tx[1] = 'c') and (tx[2] = 'a') and (tx[3] = 'a') then begin
         //SUN Vision File Format = .vff
        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        read_vff_data(lDICOMdata, lHdrOK, lImageFormatOK, lDynStr, lFileName);
        exit;
     end;
     liPos := 1;
     lStr :='';
     {999 While (liPos <= 20) and (lStr <> 'INTERFILE') do begin
        if tx[liPos] in ['i','n','t','e','r', 'f','i','l','e','I','N','T','E','R', 'F','I','L','E'] then
           lStr := lStr+upcase(tx[liPos]);
        inc(liPos);
     end; }
     if lStr = 'INTERFILE' then begin
        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        read_interfile_data(lDICOMdata, lHdrOK, lImageFormatOK, lDynStr, lFileName);
        if lHdrOk then exit;
        exit;
     end; //'INTERFILE' in first 20 char
  end;//detectint
  // try DICOM part 10 i.e. a 128 byte file preamble followed by "DICM"
  if filesz <= 300 then goto 666;
  {begin siemens somatom: DO THIS BEFORE MAGNETOM: BOTH HAVE 'SIEMENS' SIGNATURE, SO CHECK FOR 'SOMATOM'}
  if filesz = 530432 then begin
     dseek(fp, 281);
     dBlockRead(fp, tx, 8*SizeOf(Char), n);
     if (tx[0] = 'S') and (tx[1] = 'O') and (tx[2] = 'M') and (tx[3] = 'A') and (tx[4] = 'T') and (tx[5] = 'O') and (tx[6] = 'M') then begin
        lDicomData.ImageStart := 6144;
        lDicomData.Allocbits_per_pixel := 16;
        //xlDicomData.Storedbits_per_pixel := 16;
        lDicomData.little_endian := 0;
        lDicomData.XYZdim[1] := 512;
        lDicomData.XYZdim[2] := 512;
        lDicomData.XYZdim[3] := 1;
        dSeek(fp,5999); //Study/Image from 5292 "STU/IMA   1070/16"
        lDicomData.AcquNum := trunc(SomaTomFloat);//Slice Thickness from 5790 "SL   3.0"
        lDicomData.ImageNum := trunc(SomaTomFloat);//Slice Thickness from 5790 "SL   3.0"
        dSeek(fp,5792); //Slice Thickness from 5790 "SL   3.0"
        lDicomData.XYZmm[3] := SomaTomFloat;//Slice Thickness from 5790 "SL   3.0"
        dSeek(fp,5841); //Field of View from 5838 "FoV   281"
        lDicomData.XYZmm[1] := SomaTomFloat; //Field of View from 5838 "FoV   281"
        lDicomData.XYZmm[2] := lDicomData.XYZmm[1]/lDicomData.XYZdim[2];//do mm[2] first before FOV is overwritten
        lDicomData.XYZmm[1] := lDicomData.XYZmm[1]/lDicomData.XYZdim[1];
        if lVerboseRead then
           lDynStr := 'Siemens Somatom Format'+kCR+
           'Image Series/Number: '+inttostr(lDicomData.AcquNum)+'/'+inttostr(lDicomData.ImageNum)+kCR+
           'XYZ dim:' +inttostr(lDicomData.XYZdim[1])+'/'
           +inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
           +kCR+'XYZ mm:'+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
           +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2);
        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        lImageFormatOK := true;
        lHdrOK := true;
        exit;
     end; //signature found
  end; //correctsize for somatom
  {end siemens somatom}

{siemens magnetom}
  dseek(fp,96);
  dBlockRead(fp, tx, 7*SizeOf(Char), n);
  if (tx[0] = 'S') and (tx[1] = 'I') and (tx[2] = 'E') and (tx[3] = 'M') and (tx[4] = 'E') and (tx[5] = 'N') and (tx[6] = 'S') then begin
        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        read_siemens_data(lDICOMdata, lHdrOK, lImageFormatOK, lDynStr, lFileName);
        exit;
  end;
  {end siemens magnetom vision}
  {siemens somatom plus}
     dseek(fp, 0);
     dBlockRead(fp, tx, 8*SizeOf(Char), n);
  if (tx[0] = 'S') and (tx[1] = 'I') and (tx[2] = 'E') and (tx[3] = 'M') and (tx[4] = 'E') and (tx[5] = 'N') and (tx[6] = 'S') then begin
        lDicomData.ImageStart := 8192;
        lDicomData.Allocbits_per_pixel := 16;
        //xlDicomData.Storedbits_per_pixel := 16;
        lDicomData.little_endian := 0;
        dseek(fp, 1800); //slice thickness
        lDicomData.XYZmm[3] := read64(fp,lrOK);
        dseek(fp, 4100);
        lDicomData.AcquNum := read32(fp,lrOK);
        dseek(fp, 4108);
        lDicomData.ImageNum := read32(fp,lrOK);
        dseek(fp, 4992); //X FOV
        lDicomData.XYZmm[1] := read64(fp,lrOK);
        dseek(fp, 5000); //Y FOV
        lDicomData.XYZmm[2] := read64(fp,lrOK);
        dseek(fp, 5340);
        lDicomData.XYZdim[1] := read32(fp,lrOK);
        dseek(fp, 5344);
        lDicomData.XYZdim[2] := read32(fp,lrOK);
        lDicomData.XYZdim[3] := 1;
        if lDicomData.XYZdim[1] > 0 then
           lDicomData.XYZmm[1] := lDicomData.XYZmm[1]/lDicomData.XYZdim[1];
        if lDicomData.XYZdim[2] > 0 then
           lDicomData.XYZmm[2] := lDicomData.XYZmm[2]/lDicomData.XYZdim[2];
        if lVerboseRead then
           lDynStr := 'Siemens Somatom Plus Format'+kCR+
     'Image Series/Number: '+inttostr(lDicomData.AcquNum)+'/'+inttostr(lDicomData.ImageNum)+kCR+
     'XYZ dim:' +inttostr(lDicomData.XYZdim[1])+'/'
     +inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
     +kCR+'XYZ mm:'+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
     +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2);

        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        lImageFormatOK := true;
        lHdrOK := true;
        exit;
  end;
  {end siemens somatom plus }
  {picker}
  dseek(fp,0);
  dBlockRead(fp, tx, 8*SizeOf(Char), n);
  if (tx[0]='C') and (tx[1]='D') and (tx[2]='F') and (ord(tx[3]) = 1) then begin
        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        read_minc_data(lDICOMdata, lHdrOK, lImageFormatOK,lDynStr,lFileName);
        exit;
  end;
  if (lAutoDECAT7) and (tx[0]='M') and (tx[1]='A') and (tx[2]='T') and (tx[3]='R') and (tx[4]='I') and (tx[5]='X') then begin
        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        read_ecat_data(lDICOMdata, lVerboseRead,lReadECAToffsetTables,lHdrOK, lImageFormatOK, lDynStr, lFileName);
        exit;
  end;
  if (tx[0] = '*') AND (tx[1] = '*') AND (tx[2] = '*') AND (tx[3] = ' ') then begin {picker Standard}
        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        read_picker_data(lVerboseRead,lDICOMdata, lHdrOK, lImageFormatOK, lDynStr, lFileName);
        exit;
  end; {not picker standard}
  //Start Picker Prism
  ljunk := filesz-2048;
  lDICOMdata.little_endian := 0;
  //start: read x
  dseek(fp, 322);
  Width := read16(fp,lrOK);

  //start: read y
  dseek(fp, 326);
  Ht := read16(fp,lrOK);
  lMatrixSz := Width * Ht;

  //check if correct filesize for picker prism
  if (ord(tx[0]) = 1) and (ord(tx[1])=2) and ((ljunk mod lMatrixSz)=0){128*128*2bytes = 32768} then begin //Picker PRISM
      lDicomData.little_endian := 0;
      lDicomData.XYZdim[1] := Width;
      lDicomData.XYZdim[2] := Ht;
      lDicomData.XYZdim[3] := (ljunk div 32768);  {128*128*2bytes = 32768}
      lDicomData.Allocbits_per_pixel := 16;
      //xlDicomData.Storedbits_per_pixel := 16;
      lDicomData.ImageStart := 2048;
      //start: read slice thicness
      dseek(fp,462);
      dBlockRead(fp, tx, 12*SizeOf(Char), n);
      lStr := '';
      for ljunk := 0 to 11 do
         if tx[ljunk] in ['0'..'9','.'] then
            lStr := lStr+ tx[ljunk];
      if lStr <> '' then
         lDicomData.XYZmm[3] := strtofloat(lStr);
      //start: voxel size
      dseek(fp,594);
      dBlockRead(fp, tx, 12*SizeOf(Char), n);
      lStr := '';
      for ljunk := 0 to 11 do
         if tx[ljunk] in ['0'..'9','.'] then
            lStr := lStr+ tx[ljunk];
      if lStr <> '' then
         lDicomData.XYZmm[1] := strtofloat(lStr);
      lDicomData.XYZmm[2] := lDicomData.XYZmm[1];
      //end: read voxel sizes
      //start: patient name
      dseek(fp,26);
      dBlockRead(fp, tx, 22*SizeOf(Char), n);
      lStr := '';
      ljunk := 0;
      while (ljunk < 22) and (ord(tx[ljunk]) <> 0) do begin
            lStr := lStr+ tx[ljunk];
            inc(ljunk);
      end;
      lDicomData.PatientName := lStr;
      //start: patient ID
      dseek(fp,48);
      dBlockRead(fp, tx, 15*SizeOf(Char), n);
      lstr := '';
      ljunk := 0;
      while (ljunk < 15) and (ord(tx[ljunk]) <> 0) do begin
            lstr := lstr+ tx[ljunk];
            inc(ljunk);
      end;
      //xlDicomData.PatientID := lStr;
      //start: scan time
      dseek(fp,186);
      dBlockRead(fp, tx, 25*SizeOf(Char), n);
      lstr := '';
      ljunk := 0;
      while (ljunk < 25) and (ord(tx[ljunk]) <> 0) do begin
            lstr := lstr+ tx[ljunk];
            inc(ljunk);
      end;
      //start: scanner type
      dseek(fp,2);
      dBlockRead(fp, tx, 25*SizeOf(Char), n);
      lgrpstr := '';
      ljunk := 0;
      while (ljunk < 25) and (ord(tx[ljunk]) <> 0) do begin
            lgrpstr := lgrpstr+ tx[ljunk];
            inc(ljunk);
      end;
      //report results
        if lVerboseRead then
           lDynStr := 'Picker Format '+lgrpstr+kCR+
             'Patient Name: '+lDicomData.PatientName+kCR+
             //x'Patient ID: '+lDicomData.PatientID+kCR+
             'Scan Time: '+lStr+kCR+
     'XYZ dim:' +inttostr(lDicomData.XYZdim[1])+'/'
     +inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])
     +kCR+'XYZ mm:'+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'
     +floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2);
        CloseFile(fp);
        if lDiskCacheSz > 0 then
           freemem(lDiskCacheRA);
        FileMode := 2; //set to read/write
        lImageFormatOK := true;
        lHdrOK := true;
        exit;

  end; //end Picker PRISM
    lMatrixSz := 0;

  lDICOMdata.little_endian := 1;
  lBig := false;
  dseek(fp, {0}128);
  //where := FilePos(fp);
  dBlockRead(fp, tx, 4*SizeOf(Char), n);
  if (tx[0] <> 'D') OR (tx[1] <> 'I') OR (tx[2] <> 'C') OR (tx[3] <> 'M') then begin

     //if filesz > 132 then begin
        dseek(fp, 0{128}); //skip the preamble - next 4 bytes should be 'DICM'
  	   //where := FilePos(fp);
        dBlockRead(fp, tx, 4*SizeOf(Char), n);
     //end;
     if (tx[0] <> 'D') OR (tx[1] <> 'I') OR (tx[2] <> 'C') OR (tx[3] <> 'M') then begin
        dseek(fp, 0);
        group   := read16(fp,lrOK);

        if not lrOK then goto 666;

        if group > $0008 then begin
           group := swap(group);
           lBig := true;
        end;
        if NOT (group in [$0000, $0001, $0002,$0003, $0004, $0008]) then // one more group added
        begin

           goto 666;
        end;
        dseek(fp, 0);
        //Msg('DICM not at 0 or 128: ' +lFilename);
     end;
  end; //else Msg('DICM at 128{0}');;
  time_to_quit := FALSE;
  lProprietaryImageThumbnail := false;
     explicitVR := false;
    tmpstr := '';

      tmp := 0;

    while NOT time_to_quit do begin
  t := unknown;
  	where     := dFilePos(fp);
     lFirstPass := true;
777:
   	group     := read16(fp,lrOK);
     if (lSwitchToImplicitAfterGroup0002) and (group > 0002) then begin
        lSwitchToImplicitAfterGroup0002 := false;
        explicitVR := false;
     end;

     if not lrOK then goto 666;
     if (lFirstPass) and (group = 2048) then begin
         if lDicomData.little_endian = 1 then lDicomData.Little_endian := 0
         else lDicomData.little_endian := 1;
         dseek(fp,where);
         lFirstPass := false;
         goto 777;
     end;

     element   := read16(fp,lrOK);
     if not lrOK then goto 666;
     e_len:= read32(fp,lrOK);
     if not lrOK then goto 666;
lGrpStr := '';
    lt0 := e_len and 255;
    lt1 := (e_len shr 8) and 255;
    lt2 := (e_len shr 16) and 255;
    lt3 := (e_len shr 24) and 255;
 if (explicitVR) and (lT0=13) and (lT1=0) and (lT2=0) and (lT3=0) then
   e_len := 10;  //hack for some GE Dicom images


 if explicitVR or first_one then begin
   if group = $FFFE then else //1384  - ACUSON images switch off ExplicitVR for file image fragments
   if  ((lT0=kO) and (lT1=kB)) or ((lT0=kU) and (lT1=kN)){<-UN added 11/11/2011} or ((lT0=kO) and (lT1=kW)) or ((lT0=kS) and (lT1=kQ)) {11/11 add UT}or ((lT0=kU) and (lT1=kT))  then begin
       lGrpStr := chr(lT0)+chr(lT1);
           e_len:= read32(fp,lrOK);
           if not lrOK then goto 666;
           if first_one then explicitVR := true;
   end else if ((lT3=kO) and (lT2=kB)) or ((lT3=kU) and (lT2=kN)){<-UN added 11/11/2011} or((lT3=kO) and (lT2=kW)) or ((lT3=kS) and (lT2=kQ)) or ((lT3=kU) and (lT2=kT))then begin
           e_len:= read32(fp,lrOK);
           if not lrOK then goto 666;
           if first_one then explicitVR := true;
   end
   else if  ( ((lT0=kA) and (lT1=kE)) or ((lT0=kA) and (lT1=kS))
      or ((lT0=kA) and (lT1=kT)) or ((lT0=kC) and (lT1=kS)) or ((lT0=kD) and (lT1=kA))
      or ((lT0=kD) and (lT1=kS))
      or ((lT0=kD) and (lT1=kT)) or ((lT0=kF) and (lT1=kL)) or ((lT0=kF) and (lT1=kD))
      or ((lT0=kI) and (lT1=kS)) or ((lT0=kL) and (lT1=kO))or ((lT0=kL) and (lT1=kT))
      or ((lT0=kP) and (lT1=kN)) or ((lT0=kS) and (lT1=kH)) or ((lT0=kS) and (lT1=kL))
      or ((lT0=kS) and (lT1=kS)) or ((lT0=kS) and (lT1=kT)) or ((lT0=kT) and (lT1=kM))
      or ((lT0=kU) and (lT1=kI)) or ((lT0=kU) and (lT1=kL)) or ((lT0=kU) and (lT1=kS))
      or ((lT0=kA) and (lT1=kE)) or ((lT0=kA) and (lT1=kS)) )
      then begin
           lGrpStr := chr(lT0) + chr(lT1);
           if lDicomData.little_endian = 1 then
              e_len := (e_len and $ffff0000) shr 16
           else
              e_len := swap((e_len and $ffff0000) shr 16);
           if first_one then begin
              explicitVR := true;
           end;
   end else if (
           ((lT3=kA) and (lT2=kT)) or ((lT3=kC) and (lT2=kS)) or ((lT3=kD) and (lT2=kA))
           or ((lT3=kD) and (lT2=kS))
      or ((lT3=kD) and (lT2=kT)) or ((lT3=kF) and (lT2=kL)) or ((lT3=kF) and (lT2=kD))
      or ((lT3=kI) and (lT2=kS)) or ((lT3=kL) and (lT2=kO))or ((lT3=kL) and (lT2=kT))
      or ((lT3=kP) and (lT2=kN)) or ((lT3=kS) and (lT2=kH)) or ((lT3=kS) and (lT2=kL))
      or ((lT3=kS) and (lT2=kS)) or ((lT3=kS) and (lT2=kT)) or ((lT3=kT) and (lT2=kM))
      or ((lT3=kU) and (lT2=kI)) or ((lT3=kU) and (lT2=kL)) or ((lT3=kU) and (lT2=kS)))
      then begin
           if lDicomData.little_endian = 1 then
              e_len := (256 * lT0) + lT1
           else
              e_len := (lT0) + (256*lT1);
           if first_one then begin
              explicitVR := true;
           end;
   end;
end; //not first_one or explicit

   if (first_one) and (lDicomdata.little_endian =0) and (e_len = $04000000) then begin
      Msg('Switching to little endian');
      lDicomData.little_endian := 1;
      dseek(fp, where);
      first_one := false;
      goto 777;
   end else if (first_one) and (lDicomData.little_endian =1) and (e_len = $04000000) then begin
       Msg('Switching to little endian');
       lDicomData.little_endian := 0;
       dseek(fp, where);
       first_one := false;
       goto 777;
   end;

   if e_len = ($FFFFFFFF) then begin
    e_len := 0;
end;
	if lGELX then begin
   	   e_len := e_len and $FFFF;
   end;
   first_one    := false;
    remaining := e_len;
    info := '?';
    tmpstr := '';
   //10b1 if (lIndent > 0) and (not ((group= $FFFE) and (element = $E0DD))) and (not lManufacturerIsPhilips)  then
   //    goto 1234; //Philips stores slice positioning inside 0020,9113; lice orientation inside 0020,9116  but Siemens stores thumbnails in indented subheadings

	case group of
        $0001 : // group for normal reading elscint DICOM
        case element of
          $0010 : info := 'Name';
          $1001 : info := 'Elscint info';
         end;
    	$0002 :
      	case element of
        	$00 :  info := 'File Meta Elements Group Len';
          $01 :  info := 'File Meta Info Version';
          $02 :  info := 'Media Storage SOP Class UID';
          $03 :  info := 'Media Storage SOP Inst UID';
          $10 :  begin
              //lTransferSyntaxReported := true;
              info := 'Transfer Syntax UID';
              TmpStr := '';
              if dFilePos(fp) > (filesz-e_len) then goto 666;

              GetMem( buff, e_len);
              dBlockRead(fp, buff{^}, e_len, n);
              for i := 0 to e_len-1 do
                   	if Char(buff[i]) in ['+','-',' ', '0'..'9','a'..'z','A'..'Z']
                     	then TmpStr := TmpStr +(Char(buff[i]))
                      else TmpStr := TmpStr +('.');
              FreeMem( buff);
              lStr := '';
              //Msg(TmpStr);
              if TmpStr = '1.2.840.113619.5.2' then begin
                 lGELX := true;
				          LBigSet := true;
                 lBig := true;
              end;
              //
              if length(TmpStr) < 19 then begin
                  //12/2010 assume 1.2.840.10008.1.2
                  //Raw data, Implicit VR, Little Endian
                  //   explicitVR := false; //china
                  lSwitchToImplicitAfterGroup0002 := true;
              end;

              if length(TmpStr) >= 19 then begin

                  if TmpStr[19] = '1' then begin
                     lBigSet:= true;
                     explicitVR := true; //duran
                     lBig := false;
                  end else if TmpStr[19] = '2' then begin
                     lBigSet:= true;
                     explicitVR := true; //duran
                     lBig := true;
                  end else if TmpStr[19] = '4' then begin
                      if length(TmpStr) >= 21 then begin
                         //Dec 2012.... dcm2nii can handle JPEG 123456
                         if {not lReadJPEGtables} false then begin
                            lImageFormatOK := false;
                         end else begin

                             i := strtoint(TmpStr[21]+TmpStr[22]);
                             if (i <> 57) and (i <> 70) then begin
                                lImageFormatOK := false;
                                //lDicomData.JPEGLossyCpt := true
                             end else begin

                                  //lImageFormatOK := false;//123456
                                  lDicomData.JPEGLosslessCpt := true;
                             end;
                         end;
                      end else begin
                          lImageFormatOK := false;
                      end;
                  end else if TmpStr[19] = '5' then begin
                      lImageFormatOK := false;//xlDicomData.RunLengthEncoding := true;
                  end else begin
                      lImageFormatOK := false;
                  end;
                  if not lImageFormatOK then
                    Msg('Unsupported Transfer Syntax '+(TmpStr)+' Solution: use MRIcro');

              end; {length}
                  remaining := 0;
                  e_len := 0; {use tempstr}
              end;
          $12 :  begin
              info := 'Implementation Class UID';
              end;
          $13 : begin
              info := 'Implementation Version Name';
              if e_len > 4 then begin
				 TmpStr := '';
				 DICOMHeaderString(TmpStr);
               //lDicomData.ImplementationVersion := Str2Int(TmpStr);
               if TmpStr = 'MEDIFACE 1 5' then
                 lMediface0002_0013 := true; //detect MEDIFACE 1.5 error: error in length of two elements 0008:1111 and 0008:1140
              end; //length > 4
		  end; //element 13
          $16 :  info := 'Source App Entity Title';
          $100:  info := 'Private Info Creator UID';
          $102:  info := 'Private Info';
				end;
      $0008 :
        case element of
          $00 :  begin
              info := 'Identifying Group Length';
          end;
          $01 :  info := 'Length to End';
          $05 :  info := 'Specific Character Set';
          $08 :  begin
              info := 'Image Type';
              if dFilePos(fp) > (filesz-e_len) then goto 666;
              lSiemensMosaic0008_0008:= false;
              if (e_len >= 6) then begin //search for 'MOSAIC'
                 GetMem( buff, e_len);
                 dBlockRead(fp, buff{^}, e_len, n);
                 i := e_len -6;//MOSAIC
                 while (i>-1) and (not lSiemensMosaic0008_0008) do begin
                   if (upcase(Char(buff[i])) = 'M') and (upcase(Char(buff[i+1])) = 'O')
                      and (upcase(Char(buff[i+2])) = 'S') and (upcase(Char(buff[i+3])) = 'A')
                      and (upcase(Char(buff[i+4])) = 'I') and (upcase(Char(buff[i+5])) = 'C')
                   then //strip filler characters: DICOM elements must be padded for even length
                      lSiemensMosaic0008_0008 := true;
                   dec(i);
                 end;
                 FreeMem( buff);
                 remaining := 0;
                 e_len := 0; {use tempstr}
               end;
         end;
          $10 :  info := 'Recognition Code';
          $12 :  info := 'Instance Creation Date';
          $13 :  info := 'Instance Creation Time';
          $14 :  info := 'Instance Creator UID';
          $16 :  info := 'SOP Class UID';
          $18 :  info := 'SOP Instance UID';
          $20 :  begin
			  info := 'Study Date';
              //lDicomData.StudyDatePos  := dFilePos(fp);
			  DICOMHeaderString(lDicomData.StudyDate);
              end;
          $21 :  info := 'Series Date';
          $22 :  info := 'Acquisition Date';
          $23 :  info := 'Image Date';
		  $30 :   begin  info := 'Study Time';
			  DICOMHeaderStringTime(lDicomData.StudyTime);
		  end;
          $31 :  info := 'Series Time';
		  $32 : begin  info := 'Acquisition Time';
			  DICOMHeaderStringTime(TmpStr);
              lDicomData.SecSinceMidnight := SecSinceMidnightFloat(TmpStr);

		  end;
          $33 : begin  info := 'Image Time';
              //xxDICOMHeaderStringTime(lDicomData.ImgTime);
          end;
          $40 :  info := 'Data Set Type';
          $41 :  info := 'Data Set Subtype';
		  $50 :  begin
          //xDICOMHeaderStringtoInt(lDicomData.accession);
          info := 'Accession Number';
          end;

          $60 :  begin info := 'Modality';  t := _string; end;
          $64 :  begin info := 'Conversion Type';  t := _string; end;
          $70 : begin
          info := 'Manufacturer';
              //Only read last word, e.g. 'TYPE\MOSAIC' will be read as 'MOSAIC'
              TmpStr := '';

              if dFilePos(fp) > (filesz-e_len) then goto 666;
              GetMem( buff, e_len);
              dBlockRead(fp, buff{^}, e_len, n);
              i := e_len -1;
              while (i>-1) and (Char(buff[i]) in ['a'..'z','A'..'Z',' ']) do begin
                   if (Char(buff[i])) <> ' ' then //strip filler characters: DICOM elements must be padded for even length
                      TmpStr := upcase(Char(buff[i]))+TmpStr;
                   dec(i);
              end;
              FreeMem( buff);
                  remaining := 0;
                  e_len := 0; {use tempstr}
              if (length(TmpStr) > 3) and (TmpStr[1]='P') and (TmpStr[2]='H') and (TmpStr[3]='I') then
                 lManufacturerIsPhilips := true;
              if (length(TmpStr) > 3) and (TmpStr[1]='B') and (TmpStr[2]='R') and (TmpStr[3]='U') then
                 lManufacturerIsBruker := true;

              if lManufacturerIsPhilips then
                 lDicomData.ManufacturerID := kPhilipsID;

              if (length(TmpStr) > 3) and (TmpStr[1]='G') and (TmpStr[2]='E')  then
                 lDicomData.ManufacturerID := kGEID;
              if (length(TmpStr) > 3) and (TmpStr[1]='S') and (TmpStr[2]='I') and (TmpStr[3]='E') then
                 lDicomData.ManufacturerID := kSiemensID;

          end;
          $80 :  info := 'Institution Name';
		  $81 :  info := 'City Name';
          $90 :  info := 'Referring Physician''s Name';
          $100: info := 'Code Value';
          $102 : begin
            info := 'Coding Schema Designator';
            t := _string;
          end;
          $104: info := 'Code Meaning';
          $1010: info := 'Station Name';
          $1030: begin info := 'Study Description'; t := _string; end;
          $103e: begin info := 'Series Description'; t := _string; end;
          $1040: info := 'Institutional Dept. Name';
          $1050: info := 'Performing Physician''s Name';
          $1060: info := 'Name Phys(s) Read Study';
          $1070: begin info := 'Operator''s Name';  t := _string; end;
          $1080: info := 'Admitting Diagnosis Description';
          $1090: begin info := 'Manufacturer''s Model Name';t := _string; end;
          $1111: begin
                 if lMediface0002_0013 then E_LEN := 8;//+e_len;
             end; //ABBA: patches error in DICOM images seen from Sheffield 0002,0013=MEDIFACE.1.5; 0002,0016=PICKER.MR.SCU
          $1140: begin
                   if (lMediface0002_0013) and (E_LEN > 255) then E_LEN := 8;
                 end; //ABBA: patches error in DICOM images seen from Sheffield 0002,0013=MEDIFACE.1.5; 0002,0016=PICKER.MR.SCU
          $2111: info := 'Derivation Description';
          $2120: info := 'Stage Name';
          $2122: begin info := 'Stage Number';t := _string; end;
          $2124: begin info := 'Number of Stages';t := _string; end;
		  $2128: begin info := 'View Number';t := _string; end;
          $212A: begin info := 'Number of Views in stage';t := _string; end;
          $2204: info := 'Transducer Orientation';
          $9208: begin
             info := 'ComplexImageComponent';
				 TmpStr := '';
				 DICOMHeaderString(TmpStr);
              i := 0;

              if length(TmpStr) >= 2 then begin
                 if (TmpStr[1] = 'M') and (TmpStr[2] = 'A') then
                    i := 1; //magnitude
                 if (TmpStr[1] = 'P') and (TmpStr[2] = 'H') then
                    i := 2; //phase
                 if (TmpStr[1] = 'R') and (TmpStr[2] = 'E') then
                    i := 3; //real
                 if (TmpStr[1] = 'I') and (TmpStr[2] = 'M') then
                    i := 4; //imaginary
              end;
              //mixed will be followed by subsequent settings, so do not use it here....
              if (i > 0) and (lDICOMdata.nOrder < kMaxOrderVal) then begin
                  inc(lDICOMdata.nOrder);
                  //msg(TmpStr);
                  lDICOMdata.order[lDICOMdata.nOrder] := i;
              end;
(*[ magnitude * MAGNITUDE
[ phase * PHASE
[ real * REAL
[ imaginary * IMAGINARY
[ mixed * MIXED*)
              ///xxx xxx
          end;

        end;
        $0009: if element = $0010 then begin

             if e_len > 4 then begin
               TmpStr := '';
              if dFilePos(fp) > (filesz-e_len) then goto 666;
              GetMem( buff, e_len);
              dBlockRead(fp, buff{^}, e_len, n);
              i := e_len -1;

              while (i>-1) {and (Char(buff[i]) in ['a'..'z','A'..'Z',' '])} do begin
                  if (Char(buff[i])) in ['a'..'z','A'..'Z'] then //strip filler characters: DICOM elements must be padded for even length
                      TmpStr := upcase(Char(buff[i]))+TmpStr;
                   dec(i);
              end;
              FreeMem( buff);
              remaining := 0;
              if (Length(TmpStr)>4) and (TmpStr[1]='M') and (TmpStr[2]='E') and (TmpStr[3]='R') and (TmpStr[4]='G') then
                 lOldSiemens_IncorrectMosaicMM := true; //detect MERGE technologies mosaics
              e_len := 0; {use tempstr}
             end;


          end;
    	$0010 :
		case element of
        	$00 :  info := 'Patient Group Length';
          $10 :  begin info := 'Patient''s Name'; t := _string;
              //xlDicomData.NamePos := dFilePos(fp);
              DICOMHeaderString(lDicomData.PatientName);
          end;
          $20 :  begin info := 'Patient ID';
              //xDICOMHeaderString(lDicomData.PatientID);
              //xlDicomData.PatientIDInt := safestrtoint(lDicomData.PatientID);
          end;
          //11/2010
          //$30: info := 'Date of Birth'; //"Age String" type: e.g 067y for 67 years old, 067d for 67 days
          $30 :  begin info := 'DoB'; t := _string;
              //xlDicomData.NamePos := dFilePos(fp);
              //lDicomData.PatientDoB := '1111';
              DICOMHeaderString(lDicomData.PatientDoB);
          end;
          $32 : info := 'Patient Birth Time';
          //$40 :  begin info := 'Patient Sex';  t := _string; end;
          $40 :  begin info := 'Gender'; t := _string;
              //xlDicomData.NamePos := dFilePos(fp);
              DICOMHeaderString(lDicomData.PatientGender);
          end;

          $1000: info := 'Other Patient IDs';
          $1001: info := 'Other Patient Names';
          $1005: info := 'Patient''s Birth Name';
          $1010: begin info := 'Patient Age'; t := _string; end;
          $1030: info := 'Patient Weight';
          $21b0: info := 'Additional Patient History';
          $4000: info := 'Patient Comments';

				end;
    $0018 :
        case element of
			 $00 :  info := 'Acquisition Group Length';
          $10 :  begin info := 'Contrast/Bolus Agent'; t := _string; end;
          $15: info := 'Body Part Examined';
		  $20 :  begin
			info := 'Scanning Sequence';t := _string;
			TmpStr := '';
			DICOMHeaderString(TmpStr);
			if TmpStr = 'RM' then lResearchMode := true;
			end;
		  $21 :  begin info := 'Sequence Variant';t := _string; end;
		  $22 :  info := 'Scan Options';
		  $23 :  begin info := 'MR Acquisition Type'; t := _string; end;
		  $24 :  info := 'Sequence Name';
		  $25 :  begin info := 'Angio Flag';t := _string; end;
		  $30 :  info := 'Radionuclide';
		  $50 :  begin info := 'Slice Thickness';
			readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
			  if not lrOK then goto 666;
              e_len := 0;      remaining := 0;
             lDICOMdata.XYZmm[3] := lfloat1;
             
             lThickness := lfloat1;//lDICOMdata.Thickness := lfloat1; //1391b
          end;
          //$60: begin info := 'KVP [Peak Output, KV]';  t := _string; end; //aqw
          $60: begin
                info := 'KVP [Peak KV]';
                readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                if not lrOK then goto 666;
                e_len := 0; remaining := 0;
                lDicomData.kV := lFloat1;
          end;

          $70: begin t := _string; info := 'Counts Accumulated'; end;
          $71: begin t := _string; info := 'Acquisition Condition'; end;
		  //$80 :  begin info := 'Repetition Time';  t := _string; end; //aqw
          //$81 :  begin info := 'Echo Time'; t := _string; end;  //aqw
          $80 : begin info := 'Repetition Time [TR, ms]';
                readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                if not lrOK then goto 666;
				e_len := 0; remaining := 0;
                lDicomData.TR := lFloat1;
                end;

          $81 : begin
          info := 'Echo Time [TE, ms]';
                readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                if not lrOK then goto 666;
                e_len := 0; remaining := 0;
                lDicomData.TE := lFloat1;
          end;
          $82 :  begin t := _string; info := 'Inversion Time';end;
          $83 :  begin t := _string; info := 'Number of Averages'; end;
          $84 :  info := 'Imaging Frequency';
          $85 :  begin info := 'Imaged Nucleus';  t := _string; end;
          $86 :  begin info := 'Echo Number';t := _string;

             DICOMHeaderStringToInt(lEchoNum);
             //lDICOMdata.Echo := lEchoNum;

          end;
//qq
          $87 :  info := 'Magnetic Field Strength';
          $88 : begin
          info := 'Spacing Between Slices';
            readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
              if not lrOK then goto 666;
              e_len := 0;      remaining := 0; //1362 some use this for gap size, others for sum of gap and slicethickness!
            //3333 if (lfloat1 > lDICOMdata.XYZmm[3]) or (lDICOMdata.XYZmm[3]=1) then
             //lDICOMdata.XYZmm[3] := lfloat1;
             //fx(lDICOMdata.XYZmm[3],lThickness,lfloat1);
             if lfloat1 < 0 then
                lDICOMdata.XYZmm[3] := lFloat1//does not make sense - found in some eFilm images from Marconi P3000
             else if  ( (lThickness/2) > lfloat1 ) then
                 lDICOMdata.XYZmm[3] := lfloat1+lThickness
             else
                 lDICOMdata.XYZmm[3] := lfloat1;//1392
             //xldicomdata.spacing:=lfloat1;
             end;
          $89 : begin
             // t := _string;
              info := 'Number of Phase Encoding Steps';
            //1499c This is a indirect method for detecting SIemens Mosaics: check if image height is evenly divisible by encoding steps
            //      A real kludge due to Siemens not documenting mosaics explicitly: this workaround may incorrectly think rescaled images are mosaics!
            readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
			lPhaseEncodingSteps := round(lfloat1);
			//xxxMsg(floattostr(lFloat1));
              if not lrOK then goto 666;
              e_len := 0;      remaining := 0; //1362 some use this for gap size, others for sum of gap and slicethickness!
            //if (lfloat1 > lDICOMdata.XYZmm[3]) or (lDICOMdata.XYZmm[3]=1) then
            //lDICOMdata.XYZmm[3] := lfloat1;
             //ldicomdata.spacing:=lfloat1;


              end;
          $90 :  info := 'Data collection diameter';
          $91 :  begin info := 'Echo Train Length';t := _string; end;
          $93: begin info := 'Percent Sampling'; t := _string; end;
          $94: begin info := 'Percent Phase Field View'; t := _string; end;
          $95 : begin info := 'Pixel Bandwidth';  t := _string; end;
          $1000: begin t := _string; info := 'Device Serial Number'; end;
          $1004: info := 'Plate ID';
          $1020: begin
                 info := 'Software Version';
                 t := _string;
                 if e_len > 2 then begin
				 TmpStr := '';
				 DICOMHeaderString(TmpStr);
                                 lDicomData.Vers0018_1020 := Siemensversion(TmpStr);
                 end;
          
                 //showmsg(inttostr(lDicomData.Vers0018_1020)+' '+TmpStr);
          end;
		  $1030: begin
			info := 'Protocol Name';t := _string;
			TmpStr := '';
			DICOMHeaderString(TmpStr);
			lDicomData.ProtocolName := TmpStr;
			AplhaNumericStrDICOM (lDicomData.ProtocolName);
		  end;
		  $1040: info := 'Contrast/Bolus Route';
		  $1050 :  begin
              t := _string; info := 'Spatial Resolution'; end;
          $1060: info := 'Trigger Time';
          $1062: info := 'Nominal Interval';
          $1063: info := 'Frame Time';
          $1081: info := 'Low R-R Value';
		  $1082: info := 'High R-R Value';
          $1083: info := 'Intervals Acquired';
          $1084: info := 'Intervals Rejected';
          $1088: begin info := 'Heart Rate'; t := _string; end;
          $1090: begin info :=  'Cardiac Number of Images'; t := _string; end;
          $1094: begin info :=  'Trigger Window';t := _string; end;
          $1100: info := 'Reconstruction Diameter';
          $1110: info := 'Distance Source to Detector [mm]';
          $1111: info := 'Distance Source to Patient [mm]';
          $1120: info := 'Gantry/Detector Tilt';
          $1130: info := 'Table Height';
          $1140: info := 'Rotation Direction';
          $1147: info := 'Field of View Shape';
          $1149: begin
              t := _string; info := 'Field of View Dimension[s]'; end;
          $1150: begin
            info := 'Exposure Time [ms]';
            t := _string;
          end;
          $1151: begin
                info := 'X-ray Tube Current [mA]';
                readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                if not lrOK then goto 666;
                e_len := 0; remaining := 0;
                //xlDicomData.mA := lFloat1;
                end;

          $1152 :  info := 'Acquisition Device Processing Description';
		  $1155: info := 'Radiation Setting';
          $1160: info := 'Filter Type';
          $1164: info :='Imager Pixel Spacing';
          $1166: info := 'Grid';
          $1170 :  info := 'Generator Power';
          $1180 : info := 'Collimator/grid Name';
          $1190 : begin
             info := 'Focal Spot[s]';
             t := _string;
          end;
          $11A0 : begin
            info := 'Body Part Thickness';
            t := _string;
          end;
          $11A2 : info := 'Compression Force';
          $1200 :  info := 'Date of Last Calibration';
          $1201 :  info := 'Time of Last Calibration';
          $1210: info := 'Convolution Kernel';
          $1250: begin t := _string; info := 'Receiving Coil'; end;
          $1251: begin t := _string; info := 'Transmitting Coil'; end;
          $1260 :  begin
              t := _string; info := 'Plate Type'; end;
          $1261 :  begin
              t := _string; info := 'Phosphor Type';  end;
       $1310: begin info := 'Acquisition Matrix'; //Siemens Mosaics  converted by Merge can report the incorrect mm

         //nji2
       //NOTE: Matrix Information for MERGE converted images. Used Innocently for other uses by Siemens

       if (lOldSiemens_IncorrectMosaicMM) or ((lSiemensMosaic0008_0008) and (lMatrixSz < 1){B13}) then begin

          //TmpStr := ReadStrABC(fp, remaining,lrOK,lA,lB,lC);

          TmpStr := ReadStr(fp, remaining,lrOK,lMatrixSz);
          //ss//1362
          //fx(remaining);
          (*kEr := true;
          readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                if not lrOK then goto 666;
				e_len := 0; remaining := 0;
          kEr := false;

          lMatrixSz := round(lFloat1);
          msg(TmpStr);
          fx(lMatrixSz,lFLoat1,lFloat2,4321);*)
          {fx(lA,lB,lC);
          lMatrixSz := lB;
          lMatrixSzY := lC; }
       end else
          TmpStr := ReadStr(fp, remaining,lrOK,lJunk);//1362

                     if not lrOK then goto 666;
                     e_len := 0; remaining := 0;
       end;
          $1312: begin
              t := _string; info := 'Phase Encoding Direction';
              			TmpStr := '';
			DICOMHeaderString(TmpStr);
			lDicomData.PhaseEncoding := TmpStr;
			AplhaNumericStrDICOM (lDicomData.PhaseEncoding);
               end;
          $1314: begin
              t := _string; info := 'Flip Angle'; end;
          $1315: begin
              t := _string;info := 'Variable Flip Angle Flag'; end;
          $1316: begin
              t := _string;info := 'SAR'; end;
          $1400: info := 'Acquisition Device Processing Description';
          $1401: begin info := 'Acquisition Device Processing Code';t := _string; end;
          $1402: info := 'Cassette Orientation';
          $1403: info := 'Cassette Size';
                    $1404: info := 'Exposures on Plate';
          $1405: begin
            info := 'Relative X-Ray Exposure';
            t := _string;
          end;
          $1500: info := 'Positioner Motion';
          $1508: info := 'Positioner Type';
          $1510: begin
			info := 'Positioner Primary Angle';
            t := _string;
          end;
          $1511: info := 'Positioner Secondary Angle';
          $5020: info := 'Processing Function';
          $5100: begin
              t := _string; info := 'Patient Position';
              			TmpStr := '';
			DICOMHeaderString(TmpStr);
			lDicomData.PatientPos := TmpStr;
			AplhaNumericStrDICOM (lDicomData.PatientPos);
               end;
          $5101: begin info := 'View Position';t := _string; end;
          $6000: begin info := 'Sensitivity'; t := _string; end;
                 $7004: info := 'Detector Type';
          $7005: begin
            info := 'Detector Configuration';
            t := _string;
          end;
          $7006: info := 'Detector Description';
          $700A: info := 'Detector ID';
          $700C: info := 'Date of Last Detector Calibration';
          $700E: info := 'Date of Last Detector Calibration';
          $7048: info := 'Grid Period';
          $7050: info := 'Filter Material LT';
          $7060: info := 'Exposure Control Mode';
          //$9114: fx(1233);
       end;
$0019: begin
     (*case element of //1362
//3/3/2008 this old method for detecting mosaics has a problem - if image is interpolated x2, you will assume a 2x2 mosaic
        $1220: begin
            info := 'Matrix';t := _string;
            readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
              if not lrOK then goto 666;
			  e_len := 0;
              if lfloat2 > lfloat1 then lfloat1 := lfloat2;
              lMatrixSz := round(lfloat1);
                  //if >32767 then there will be wrap around if read as signed value!
                  remaining := 0;
       end;
        $14D4: begin
            info := 'Matrix';t := _string;
            readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
              if not lrOK then goto 666;
              e_len := 0;
              if lfloat2 > lfloat1 then lfloat1 := lfloat2;
              lMatrixSz := round(lfloat1);
                  //if >32767 then there will be wrap around if read as signed value!
                  remaining := 0;
        end;
        end; *) //case element

        if lDicomData.ManufacturerID = kSiemensID then begin
           case element of //1362
             (*$100A: begin //unsigned short $100A
                        info := 'Number Of Images in Mosaic';
                        tmp := read16(fp,lrOK);
                        if not lrOK then goto 666;
                        fx(e_len,tmp,remaining);

                 end;*)
                   $1028: begin   //7/2013
                             info := 'Siemens BandwidthPerPixelPhaseEncode';
                             lDICOMdata.BandwidthPerPixelPhaseEncode := read64 (fp,lrOK);
                             if not lrOK then goto 666;
			     e_len := 0; remaining := 0;
                end; // b-values

                $000C,$100C: begin
                             info := 'Siemens b-value';
                             readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                             if not lrOK then goto 666;
                             e_len := 0; remaining := 0;
                             tmpstr := floattostr(lFloat1);
                             lDICOMdata.DTI[1].bval := round(lFloat1);
                             lDICOMdata.SiemensDICOMDTI := true ;
                             //msgfx( 777,lDICOMdata.DTI[1].bval,lDICOMdata.DTI[1].bval,lDICOMdata.DTI[1].bval);
                end; // b-values
                $000E,$100E: begin
                             info := 'Siemens Gradient vector [x,y,z]';
                             //readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                             lDICOMdata.DTI[1].v1 := read64 (fp,lrOK);
                             if not lrOK then goto 666;
                             lDICOMdata.DTI[1].v2 := read64 (fp,lrOK);
                             if not lrOK then goto 666;
                             lDICOMdata.DTI[1].v3 := read64 (fp,lrOK);
                             if not lrOK then goto 666;
                             //msgfx( 666,lDICOMdata.DTI[1].v1,lDICOMdata.DTI[1].v2,lDICOMdata.DTI[1].v3);
                             //readfloats3 (fp, remaining, lDummyStr, lDICOMdata.DTI[1].v1,lDICOMdata.DTI[1].v2,lDICOMdata.DTI[1].v3, lROK);
                             //ShowMsg(lDummyStr);
                             //fx(e_len,lDICOMdata.DTI[1].v1,lDICOMdata.DTI[1].v2,lDICOMdata.DTI[1].v3);

                             e_len := 0; remaining := 0;
                             //lDICOMdata.DTI[1].v1 := lFloat1;
                end; // X diffusion direction


           end;//Case element
        end;//if Siemens

        if lDicomData.ManufacturerID = kGEID then begin
           case element of //1362
                $10BB,$a0bb: begin
                             info := 'GE Gradient vector [x]';
                             readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                             if not lrOK then goto 666;
                             e_len := 0; remaining := 0;
                             lDICOMdata.DTI[1].v1 := lFloat1;
                end; // X diffusion direction
                $10BC,$A0BC: begin
                             info := 'GE Gradient vector [y]';
                             readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                             if not lrOK then goto 666;
                             e_len := 0; remaining := 0;
                             lDICOMdata.DTI[1].v2 := lFloat1;
                end;//Y diffusion direction
                $10BD,$A0BD: begin
                             info := 'GE Gradient vector [z]';
                             readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                             if not lrOK then goto 666;
                             e_len := 0; remaining := 0;
                             lDICOMdata.DTI[1].v3 := lFloat1;
                 end;// Z diffusion direction
           end;//Case element
           //
           
        end;//if GE
        end;//$0019


$0020 :
        case element of
					$00 :  info := 'Relationship Group Length';
          $0d :  info := 'Study Instance UID';
          $0e :  info := 'Series Instance UID';
          $10 :  begin
            info := 'Study ID';
            t := _string;
		  end;
          $11 :  begin info := 'Series Number';
						DICOMHeaderStringToInt(lDicomData.SeriesNum);
 end;
          $12 : // begin info := 'Acquisition Number';  t := _string; end;
          begin info := 'Acquisition Number';
			  DICOMHeaderStringToInt(lDicomData.AcquNum);
          end;

          $13 :  begin info := 'Image Number';
              DICOMHeaderStringToInt(lTempInt);
              if (lDicomData.ImageNum < 2) and (lTempInt >= 0) then
                 lDicomData.ImageNum := lTempInt;
              //March2008 - some Philips data has multiple image numbers...
              //  0018,1020,Software Version=1.5.4\1.5.4.3\Gyroscan PMS/DICOM 2.0 MR .Id. datadefs.v 5.27 2004/10/18 06.50
              //msg(inttostr(lDicomData.ImageNum)+lDicomData.Filename);
		  end;
          $20 :  begin info := 'Patient Orientation';
                 t := _string;
           end;
          $30 :  info := 'Image Position';
          $32 :  begin
              info := 'Image Position Patient';
          //June 2009 - for Philips new 4D format we want value from the first slice...

          if lInside2005140F then begin
             if not (lPhilipsWarning) then
              Msg('*User: check slice thickness. Possible Philips R3.2.2 bug - scanner can report different 0020,0032 values for the same slice.');
             lPhilipsWarning := true;
          end else begin
            //5/2012: Philips R3.2.2 can save two instances of 0020:0032 for each slice: one from voxel center, one from voxel edge.

            if not lImagePositionPatientRead then begin
              readfloats3 (fp, remaining, lDummyStr, lDicomData.PatientPosX, lDicomData.PatientPosY,lDicomData.PatientPosZ, lROK);
              //fx(   lDicomData.PatientPosX, lDicomData.PatientPosY,lDicomData.PatientPosZ,56789);
              if not lrOK then goto 666;
              e_len := 0;
              remaining := 0;
              lImagePositionPatientRead := true;
              //we assume Philips reports the slice thickness correctly....
              //an alternative would be to read both 1st and 2nd ImagePositionPatient and
              //compute the function DICOMinterslicedistance
            end else begin

              CheckIntersliceDistance(l4DDistanceBetweenSliceCenters);

            end; //not 1st read

          end; //if lInside2005140F
          //lInside2005140F := false;
			  end;
		  $35 :  info := 'Image Orientation';
		  $37 : begin //nifti
			info := 'Image Orientation (Patient)';
			readfloats6 (fp, remaining, lDummyStr, lDicomData.Orient[1], lDicomData.Orient[2],lDicomData.Orient[3],lDicomData.Orient[4], lDicomData.Orient[5],lDicomData.Orient[6], lROK);
		  if not lrOK then goto 666;
		  e_len := 0;
		  remaining := 0;

			end;
          $50 :  info := 'Location';
          $52 :  info := 'Frame of Reference UID';
          $91 :  info := 'Echo Train Length';
          $70 :  info := 'Image Geometry Type';
          $60 :  info := 'Laterality';
          $0105 : begin
                //Apr2007
               
                DICOMHeaderStringToInt(lnVol);

           //Number of temporal positions=105
          end;
          $1001: info := 'Acquisitions in Series';
          $1002: info := 'Images in Acquisition';
          $1020: info := 'Reference';
          $1040: begin info :=  'Position Reference';  t := _string; end;
          $1041: begin info := 'Slice Location';
            readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
              if not lrOK then goto 666;
              e_len := 0;      remaining := 0;
              ldicomdata.location:=lfloat1;
             end;
		 $1070: begin
            info := 'Other Study Numbers';
            t := _string;
          end;
          $3401: info := 'Modifying Device ID';
          $3402: info := 'Modified Image ID';
          $3403: info := 'Modified Image Date';
          $3404: info := 'Modifying Device Mfg.';
          $3405: info := 'Modified Image Time';
          $3406: info := 'Modified Image Desc.';
          $4000: info := 'Image Comments';
          $5000: info := 'Original Image ID';
		  $5002: info := 'Original Image... Nomenclature';
      //$9113: xxxx
				end;
 $0021:case element of
      $104F: begin
          info :='GE Locations in acquisition';

                            if lPrefs.UseGE_0021_104F then begin
                              //June 2009 - Thomas Stephan sent me a GE image where this was set to 2, but should have been 1
                              //I hope removing this does not cause problems with other GE images...
                                  if e_len = 2 then begin
                                      lDicomData.SlicesPer3DVol := read16(fp,lrOK);
                                      e_len := 0; remaining := 0;
                                     /// fx(9999, lDicomData.SlicesPer3DVol);
                                   end;
                              end; //use 0021_104F
      end;

      $1341: begin
          info :='Siemens Mosaic Slice Count';
          DICOMHeaderStringToInt(lDicomData.SiemensSlices);

      end;
	  $134F: begin //1366
          info :='Siemens Order of Slices';
          t := _string;
              lDICOMdata.SiemensInterleaved := 0; //0=no,1=yes,2=undefined
              //look for "INTERLEAVED"
              lStr := '';
              if dFilePos(fp) > (filesz-e_len) then goto 666;
              GetMem( buff, e_len);
              dBlockRead(fp, buff{^}, e_len, n);
              for i := 0 to e_len-1 do
                   	if Char(buff[i]) in ['?','A'..'Z','a'..'z']
                     	then lStr := lStr +upcase(Char(buff[i]));
              FreeMem( buff);
          if(lStr[1]= 'I') then lDICOMdata.SiemensInterleaved := 1; //0=no,1=yes,2=undefined
          e_len := 0;
      end;
 end;
$0028 :   begin
        case element of
        	$00 :  info := 'Image Presentation Group Length';
		  $02 :  begin
              info := 'Samples Per Pixel';
              tmp := read16(fp,lrOK);
              if not lrOK then goto 666;
              lDicomData.SamplesPerPixel :=tmp;
                 if e_len > 255 then begin
                    explicitVR := true;  //kludge: switch between implicit and explicitVR
                 end;
                 tmpstr := inttostr(tmp);
                 e_len := 0;
                  remaining := 0;
              end;
          $04 :  begin
              info := 'Photometric Interpretation';
              TmpStr := '';
              if dFilePos(fp) > (filesz-e_len) then goto 666;
              GetMem( buff, e_len);
              dBlockRead(fp, buff{^}, e_len, n);
              for i := 0 to e_len-1 do
                   	if Char(buff[i]) in [{'+','-',' ', }'0'..'9','a'..'z','A'..'Z']
                     	then TmpStr := TmpStr +(Char(buff[i]));
              FreeMem( buff);
              (*xif TmpStr = 'MONOCHROME1' then lDicomdata.monochrome := 1
              else if TmpStr = 'MONOCHROME2' then lDicomdata.monochrome := 2
              else if (length(TMpStr)> 0) and (TmpStr[1] = 'Y') then lDICOMdata.monochrome := 4
              else lDICOMdata.monochrome := 3; *)
                  remaining := 0;
                  e_len := 0; {use tempstr}

          end;
          $05 :  info := 'Image Dimensions (ret)';
          $06 : begin
              info := 'Planar Configuration';
              tmp := read16(fp,lrOK);
              if not lrOK then goto 666;
              lDicomData.PlanarConfig :=tmp;
              remaining := 0;
              end;

          $08 :  begin
              //if lPapyrusnSlices < 1 then
              //   if remaining = 2 then begin
              //     tmp := read16(fp,lrOK);
              //
              //   end else               xx
                 DICOMHeaderStringToInt(lDicomData.XYZdim[3]);
                  if lDicomData.XYZdim[3] < 1 then lDicomData.XYZdim[3] := 1;
               info := 'Number of Frames';
                 end;
          $09: begin info := 'Frame Increment Pointer'; TmpStr := ReadStrHex(fp, remaining,lrOK);           if not lrOK then goto 666;
 e_len := 0; remaining := 0; end;
          $10 :  begin info := 'Rows';
          				lDicomData.XYZdim[2] := read16(fp,lrOK);
                                        if not lrOK then goto 666;
						tmp := lDicomData.XYZdim[2];
                  remaining := 0;
                 end;
          $11 :  begin info := 'Columns';
          				lDicomData.XYZdim[1] := read16(fp,lrOK);
                             if not lrOK then goto 666;
          				tmp := lDicomData.XYZdim[1];
                  remaining := 0;
                 end;
          $30 :  begin info := 'Pixel Spacing';
           readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
          if not lrOK then goto 666;
          //qq
               //row spacing [y], then column spacing [x]: see part 3 of DICOM
          e_len := 0;      remaining := 0;
             lDICOMdata.XYZmm[2] := lfloat1;
             lDICOMdata.XYZmm[1] := lfloat2;
          end;
          $31: info := 'Zoom Factor';
          $32: info := 'Zoom Center';
          $34: begin info :='Pixel Aspect Ratio';t := _string; end;
          $40: info := 'Image Format [ret]';
          $50 :  info := 'Manipulated Image [ret]';
          $51: info := 'Corrected Image';
          $60: begin info := 'Compression Code [ret]';t := _string; end;
          $100: begin info := 'Bits Allocated';
                 if remaining = 4 then
                    tmp := read32(fp,lrOK)
				 else
                     tmp := read16(fp,lrOK);
                 //lWord := read16(fp,lrOK);
                 //lWord := read16(fp,lrOK);

                            if not lrOK then goto 666;
                  if tmp = 8 then lDicomData.Allocbits_per_pixel := 8
                  else if tmp = 12 then lDicomData.Allocbits_per_pixel := 12
                  else if tmp = 16 then lDicomData.Allocbits_per_pixel := 16
                  else if tmp = 32 then lDicomData.Allocbits_per_pixel := 32
                  else if tmp = 24 then begin
                       //xlDicomData.SamplesPerPixel := 3;
                       lDicomData.Allocbits_per_pixel := 8
                  end else begin
                    lWord := tmp;
                    lWord := swap(lWord);
                    if lWord in [8,12,16,24,32] then begin
                       lDicomData.Allocbits_per_pixel := tmp;
                       lByteSwap := true;
                    end else begin
                        if lImageFormatOK then
                       Msg('This software only reads 8, 12, 16, 24 [RGB] and 32 bit DICOM files. This file allocates '+inttostr(tmp)+' bits per voxel.');
                      lImageFormatOK := false;
                    end;
                  end;
                  //remaining := 2;//remaining; //1371->
                  remaining := 0
                 end;
        	$0101: begin info := 'Bits Stored';
				 if remaining = 4 then
                    tmp := read32(fp,lrOK)
                 else
                     tmp := read16(fp,lrOK);

                             if not lrOK then goto 666;

                  (*if tmp <= 8 then lDicomData.Storedbits_per_pixel := 8
                  else if tmp <= 16 then lDicomData.Storedbits_per_pixel := 16
                  else if tmp <= 24 then begin
                       lDicomData.Storedbits_per_pixel := 24;
                       lDicomData.SamplesPerPixel := 3;
                  end else begin
                       lWord := tmp;
                       lWord := swap(lWord);
                       if lWord in [8,12,16] then begin
                          lDicomData.Storedbits_per_pixel := tmp;
                          lByteSwap := true;
                       end else begin
                           if lImageFormatOK then
                              Msg('This software can only read 8, 12 and 16 bit DICOM files. This file stores '+inttostr(tmp)+' bits per voxel.');
                           lDicomData.Storedbits_per_pixel := tmp;
                           lImageFormatOK := false;{ }
                       end;
                  end;*)
                  remaining := 0;
          			 end;
          $0102: begin info := 'High Bit';
				 if remaining = 4 then
                    tmp := read32(fp,lrOK)
                 else
                     tmp := read16(fp,lrOK);
                                        if not lrOK then
                                           goto 666;
                  remaining := 0;
                 end;
          $0103: begin
                 info := 'Pixel Representation';
                 if remaining = 2 then begin
                  tmp := read16(fp,lrOK);
                  //1= signed, 0=unsigned...
                  if tmp = 1 then
                    lDicomData.SignedData := true;
                  if tmp = 0 then
                    lDicomData.SignedData := false;
                  remaining := 0;
                 end;
            end;
          $0104: info := 'Smallest Valid Pixel Value';
          $0105: info := 'Largest Valid Pixel Value';
          $0106: begin
          //xlDicomData.MinIntensitySet:= true;
                 info := 'Smallest Image Pixel Value';
                 tmp := read16(fp,lrOK);
                 if not lrOK then goto 666;
                 //xlDicomData.Minintensity := tmp;
                  //if >32767 then there will be wrap around if read as signed value!
                  remaining := 0;
                 end;
		  $0107: begin
                 info := 'Largest Image Pixel Value';
               if remaining = 4 then
                 tmp := read32(fp,lrOK)
               else
                 tmp := read16(fp,lrOK);
                 if not lrOK then goto 666;
                 //xlDicomData.Maxintensity := tmp;
                  //if >32767 then there will be wrap around if read as signed value!
                  remaining := 0;
                 end;
          $120: info := 'Pixel Padding Value';
          $200: info := 'Image Location [ret]';
          $1040: begin t := _string; info := 'Pixel Intensity Relationship'; end;
          $1050: begin
              info := 'Window Center';
             if e_len > 0 then begin
             readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
              if not lrOK then goto 666;
              e_len := 0;      remaining := 0;
             //xlDICOMdata.WindowCenter := round(lfloat1);
             end;
          end;{float}
          $1051: begin info := 'Window Width';
            if e_len > 0 then begin
             readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
              if not lrOK then goto 666;
              e_len := 0;
			  remaining := 0;
             //xlDICOMdata.WindowWidth := round(lfloat1);
            end; //ignore empty elements, e.g. LeadTech's image6.dic
  end;
          $1052: begin t := _string;info :='Rescale Intercept';
             readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
              if not lrOK then goto 666;
              e_len := 0;      remaining := 0;
             lDICOMdata.intenIntercept := lfloat1;
             //if (lDICOMdata.nOrder > 0) and (lDICOMdata.nOrder < kMaxOrderVal) then
             //     lDICOMdata.OrderIntercept[lDICOMdata.nOrder] := lfloat1;
          end;  {float}

          $1053:begin
             t := _string; info :=  'Rescale Slope';
             readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
              if not lrOK then goto 666;
              e_len := 0;      remaining := 0;
             if lFloat1 < 0.000000001 then begin
                lFLoat1 := 1; //misused in some images, see IMG000025
             end;
             lDICOMdata.intenScale := lfloat1;
             //if (lDICOMdata.nOrder > 0) and (lDICOMdata.nOrder < kMaxOrderVal) then
             //     lDICOMdata.OrderSlope[lDICOMdata.nOrder] := lfloat1;
                     end; {float}
          $1054:begin t := _string; info := 'Rescale Type';end;
          $1100: info := 'Gray Lookup Table [ret]';
          $1101: begin  info := 'Red Palette Descriptor'; TmpStr := ReadStr(fp, remaining,lrOK,lJunk);
                     if not lrOK then goto 666;
e_len := 0; remaining := 0; end;
          $1102: begin info := 'Green Palette Descriptor'; TmpStr := ReadStr(fp, remaining,lrOK,lJunk);
                     if not lrOK then goto 666;
e_len := 0; remaining := 0; end;
          $1103: begin info := 'Blue Palette Descriptor'; TmpStr := ReadStr(fp, remaining,lrOK,lJunk);
                     if not lrOK then goto 666;
e_len := 0; remaining := 0; end;
         $1199: begin
                info := 'Palette Color Lookup Table UID';
         end;
          $1200: info := 'Gray Lookup Data [ret]';
          $1201, $1202,$1203: begin
                 case element of
                      $1201: info := 'Red Table'; {future}
                      $1202: info := 'Green Table'; {future}
                      $1203: info := 'Blue Table'; {future}
                 end;

                 if dFilePos(fp) > (filesz-remaining) then
                    goto 666;
                 if not lReadColorTables then begin
                    dSeek(fp, dFilePos(fp) + remaining);
                 end else begin {load color}
                   width := remaining div 2;

                   if width > 0 then begin
                     getmem(lWordRA,width*2);
                     for i := (width) downto 1 do
                         lWordRA^[i] := read16(fp,lrOK);
                     //value := 159;
                     value := lWordRA^[1];
					max16 := value;
  	                min16 := value;
                     for i := (width) downto 1 do begin
                         value := lWordRA^[i];
                            if value < min16 then min16 := value;
  	                    if value > max16 then max16 := value;
                     end; //width..1
                     if max16 - min16 = 0 then
                        max16 := min16+1; {avoid divide by 0}
                      if (lDicomData.Allocbits_per_pixel <= 8) and (width > 256) then width := 256; //currently only accepts palettes up to 8-bits
                     GetMem( lColorRA, width );(**)
                     for i := width downto 1 do
                         lColorRA^[i] := (lWordRA^[i] shr 8) {and 255};
                     FreeMem( lWordRA );
                     case element of
                          $1201: begin
                             red_table_size := width;
                             red_table   :=lColorRA;;
                          end;
                          $1202: begin
                             green_table_size := width;
                             green_table   :=lColorRA;;
                             end;
                          else {x$1203:} begin
                             blue_table_size := width;
                             blue_table   :=lColorRA;;
                          end; {else}
                     end; {case}
				   end; //width > 0;
                   if odd(remaining) then
                      dSeek(fp, dFilePos(fp) + 1{remaining});
                 end; {load color}
                 tmpstr := 'Custom';
                 remaining := 0;
                 e_len := 0; {show tempstr}
          end;
          $1221, $1222,$1223: begin
              info := 'Color Palette ['+inttostr(dFilePos(fp))+']';
              (*xcase element of
                   $1221: begin
                           lDicomData.RLEredOffset:= dFilePos(fp);
                           lDicomData.RLEredSz:= e_len;
                   end;
                   $1222: begin
                           lDicomData.RLEgreenOffset:= dFilePos(fp);
                           lDicomData.RLEgreenSz:= e_len;
                   end;
                   $1223: begin
                           lDicomData.RLEblueOffset:= dFilePos(fp);
                           lDicomData.RLEblueSz:= e_len;
                   end;
              end;*)//Case set offset and length

              tmpstr := inttostr(e_len);
              dSeek(fp, dFilePos(fp)+ e_LEN);
              e_len := 0;
		  end;

                    $3002: info := 'LUT Descriptor';
          $3003: info := 'LUT Explanation';
          $3006: info := 'LUT Data';
          $3010: begin
                 info := 'VOI LUT Sequence';
                 if (explicitVR) and (lT0=kS) and (lT1=kQ) then
                    e_len := 8;
             end;
     end; //case
end; //$0028
       $41: case element of //Papyrus Private Group
              $1010: begin
                  info := 'Papyrus Icon [bytes skipped]';
                  dSeek(fp, dFilePos(fp) + e_len);
                 tmpstr := inttostr(e_len);
                 remaining := 0;
                 e_len := 0;
              end; //element $0041:$1010
              $1015: begin

                  info := 'Papyrus Slices';
                  (*Papyrus format is buggy - see lsjpeg.pas for details, therefore, I have removed extensive support
                  if e_len = 2 then begin
                     lDicomData.XYZdim[3]   := read16(fp,lrOK);
                     if not lrOK then goto 666;
                  end;
				  if lDicomData.XYZdim[3] < 1 then lDicomData.XYZdim[3] := 1;
                  if {(false) and }(lDicomData.XYZdim[3] > 1) and (lReadJPEGtables) and (gECATJPEG_table_entries = 0) then begin
                     //Papyrus multislice files keep separate DICOM headers for each slice within a DICOM file
                     lPapyrusnSlices := lDicomData.XYZdim[3];
                     lPapyrusSlice := 0;
                     //lPapyrusData := lDicomData;
                    gECATJPEG_table_entries := lDICOMdata.XYZDim[3];
                    getmem (gECATJPEG_pos_table, gECATJPEG_table_entries*sizeof(longint));
                    getmem (gECATJPEG_size_table, gECATJPEG_table_entries*sizeof(longint));
                 end else
                  lDicomData.XYZdim[3] := 1;
                  tmpstr := inttostr(lDicomData.XYZdim[3]);
                  remaining := 0;
                  e_len := 0;*)
              end; //element $0041:$1015
              $1050: begin
                     info := 'Papyrus Bizarre Element'; //bizarre osiris problem
                     if (dfilepos(fp)+e_len)=  (filesz) then
                        e_len := 8;
              end; //element $0041:$1050
       end; //group $0041: Papyrus

     $43: begin

          if lDicomData.ManufacturerID = kGEID then begin
             case element of
                  $1039,$A039: begin
                          // 0043,1039 (or 0043,a039). b value (as the first number in the string).

                          info := 'GE Bvalue';
                          if e_len > 0 then begin
                             readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
                             if not lrOK then goto 666;
                             e_len := 0;      remaining := 0;
                             lDICOMdata.DTI[1].bval := round(lfloat1);
                             lDICOMdata.nDTIdir := 1;
                          end; //e_len>0
                  end;//1039 or Ao39
                    end;//Case
          end; //Manufacturer = GE
          end;//$0043 - GE bvalues

     $54: case element of
          $0: info := 'Nuclear Acquisition Group Length';
          $11: info := 'Number of Energy Windows';
          $21: info := 'Number of Detectors';
          $51: info := 'Number of Rotations';
          $80: begin info :=  'Slice Vector'; TmpStr := ReadStr(fp, remaining,lrOK,lJunk);           if not lrOK then goto 666;
 e_len := 0; remaining := 0; end;
		  $81: info := 'Number of Slices';
          $202: info := 'Type of Detector Motion';
          $400: info := 'Image ID';

          end;
     $2010 :
        case element of
             $0: info := 'Film Box Group Length';
             $100: info := 'Border Density';
        end;
      $4000 : info := 'Text';
     $0029 : begin
		case element of
                $1010: begin
			  //lSiemensMosaic0029_1010:= true;
        if (lDicomData.kV = 0) then begin //Siemens uses 0029:1010 for both CT and MRI, but only MRI is in CSA format
                lDicomData.CSAImageHeaderInfoPos := (dFilePos(fp));
                lDicomData.CSAImageHeaderInfoSz := e_len;
        end;
			  info := 'Private Sequence Delimiter ['+inttostr(dFilePos(fp))+']';
		  	  if not lImageFormatOK  then
            time_to_quit := TRUE;

                          //x(lDicomData.RunLengthEncoding) or ( ((lDicomData.JPEGLossycpt) or (lDicomData.JPEGLosslesscpt)) and (gECATJPEG_table_entries >= lDICOMdata.XYZdim[3]))}

			  dSeek(fp, dFilePos(fp) + e_len);
				 tmpstr := inttostr(e_len);
				 remaining := 0;
				 e_len := 0; {show tempstr}
		 end;
		 $1053: begin
				info :='Philips Scale Slope';
				readfloats (fp, remaining, lDummyStr, lfloat1, lfloat2, lROK);
			  if not lrOK then goto 666;
			  e_len := 0;      remaining := 0;
			 lPhilipsScaleSlope := lfloat1;
				{if e_len = 4 then begin
				   lPhilipsScaleSlope := read32r(fp,lrOK);
				   TmpStr := floattostr(lPhilipsScaleSlope);
				   t := _string;

				   if not lrOK then goto 666;
				   e_len := 0;
				   remaining := 0;
				end;  }
		 end;


		 else begin
		 end;
		END;
	 END; //gROUP 0029

	 (* $0045 : begin
		case element of
			 $103B: begin
				 msg('0045:103B');
			 end; //element $1010

		end; //CASE...element
	  end; //group 0045
           *)
	  $0089 : begin
		case element of
			 $1010: begin
				 e_len := 0;
				 lProprietaryImageThumbnail := true;
				 //lImageFormatOK := false;
			 end; //element $1010
			 $1020: begin
				 //thoravision files

				 if e_len > 12 then
					e_len := 0;
				 //lProprietaryImageThumbnail := true;
				 //lImageFormatOK := false;
			 end; //element $1010

		end; //CASE...element
	  end; //group 0089

  	  $2001 : begin
              if lDicomData.ManufacturerID = kPhilipsID then begin
		case element of

                     $1003: begin //bvalue
                            if e_len = 4 then begin
                               if lDICOMdata.nDTIdir < kMaxDTIdir then
                                  inc(lDICOMdata.nDTIdir);
                               lDICOMdata.DTI[lDICOMdata.nDTIdir].bval := round(read32r(fp,lrOK));
                               TmpStr := inttostr(lDICOMdata.DTI[lDICOMdata.nDTIdir].bval);
                               t := _string;
                               info :='DTI b-val';
                               if not lrOK then goto 666;
                               e_len := 0;      remaining := 0;
                            end; //e_len = 4
                           
                     end; //element 1003
                     $100B: begin
			info := 'philips: slice orientation';t := _string;
			TmpStr := '';
			DICOMHeaderString(TmpStr);
			lDicomData.PhilipsSliceOrient := TmpStr;
			AplhaNumericStrDICOM (lDicomData.PhilipsSliceOrient);
		        end;//PhilipsSliceOrient
			$1018: begin
				   if e_len = 4 then begin
					info :='number of slices';
					lDicomData.SlicesPer3DVol := read32(fp,lrOK);
					//uninterleave data
					e_len := 0; remaining := 0;
					if lResearchMode then
						lDicomData.SeriesNum   := lDicomData.SeriesNum + 50; //do not jumble research recons and normal images
					end; //e_len = 4
                                        TmpStr := floattostr(lDicomData.SlicesPer3DVol);
				end; //1018

                       $102D: begin
                              ///Apr2007

                                   if e_len = 2 then begin
                                      lnSlicePerVol := read16(fp,lrOK);
                                      e_len := 0; remaining := 0;
                                   end;
				   //fx(213,lnSlicePerVol);
				end; //102D

                       $105F: begin //Philips Stack Sequence

                              if e_len > 8 then e_len := 8;
				end; //105F
		end;
              end; //if manufacturer = Philips
	  end;

		  //2001,1004)

	  $2005 : begin


              //if lDicomData.ManufacturerID = kPhilipsID then Msg(inttohex(element,4));
              if lDicomData.ManufacturerID = kPhilipsID then begin
		case element of
      (* $140F: begin
        lInside2005140F := true;
      end;*)
			 $100E: begin
				if e_len = 4 then begin
				   lPhilipsScaleSlope := read32r(fp,lrOK);
				   TmpStr := floattostr(lPhilipsScaleSlope);
				   t := _string;
				   info :='Philips Scale Slope';
				   if not lrOK then goto 666;
				   e_len := 0;      remaining := 0;
				end;
			 end; //element $1010

             $1071: begin

				if e_len = 4 then begin
				   lDicomData.AngulationAP := read32r(fp,lrOK);
				   TmpStr := floattostr(lDicomData.AngulationAP);
				   t := _string;
				   info :='angulation midslice, AP (degrees)';
				   if not lrOK then goto 666;
				   e_len := 0;      remaining := 0;
				end;
			 end; // Philips AP angulation : -8.74086
             $1072: begin
				if e_len = 4 then begin
				   lDicomData.AngulationFH := read32r(fp,lrOK);
				   TmpStr := floattostr(lDicomData.AngulationFH);
				   t := _string;
				   info :='angulation midslice, FH (degrees)';
				   if not lrOK then goto 666;
				   e_len := 0;      remaining := 0;
				end;
			 end; // Philips Philips FH angulation : -3.53147
             $1073: begin
				if e_len = 4 then begin
				   lDicomData.AngulationRL := read32r(fp,lrOK);
				   TmpStr := floattostr(lDicomData.AngulationRL);
				   t := _string;
				   info :='angulation midslice, RL (degrees)';
				   if not lrOK then goto 666;
				   e_len := 0;      remaining := 0;
				end;
			 end; // Philips RL angulation
             $10b0: begin
                if e_len = 4 then begin
                   //msg('2005,10b0 @ '+ inttostr(dFilePos(fp)) +'  #'+inttostr(lDICOMdata.nDTIdir));
                   lDICOMdata.DTI[lDICOMdata.nDTIdir].v1 := read32r(fp,lrOK);
				   TmpStr := floattostr(lDICOMdata.DTI[lDICOMdata.nDTIdir].v1);
				   t := _string;
                   info :='Philips Gradient vector [x]';
                   if not lrOK then goto 666;
                   e_len := 0;      remaining := 0;
                end; //e_len = 4
             end; //element 10b0
             $10b1: begin

                if e_len = 4 then begin

                 lDICOMdata.DTI[lDICOMdata.nDTIdir].v2 := read32r(fp,lrOK);
				   TmpStr := floattostr(lDICOMdata.DTI[lDICOMdata.nDTIdir].v2);
				   t := _string;
                   info :='Philips Gradient vector [y]';
                   if not lrOK then goto 666;
                   e_len := 0;      remaining := 0;
                end; //e_len = 4
             end; //element 10b1
             $10b2: begin
                if e_len = 4 then begin
                   lDICOMdata.DTI[lDICOMdata.nDTIdir].v3 := read32r(fp,lrOK);
				   TmpStr := floattostr(lDICOMdata.DTI[lDICOMdata.nDTIdir].v3);
				   t := _string;
                   info :='Philips Gradient vector [z]';
                   //fx(lDICOMdata.DTI[lDICOMdata.nDTIdir].v1,lDICOMdata.DTI[lDICOMdata.nDTIdir].v2,lDICOMdata.DTI[lDICOMdata.nDTIdir].v3);
                   if not lrOK then goto 666;
                   e_len := 0;      remaining := 0;
                end; //e_len = 4


             end; //element 10b2
             $140f: begin
                    //msg('2005:140F@ '+inttostr(dFilePos(fp)) +'  len: '+inttostr(e_len) +' '+ chr(lT0)+chr(lT1));
                    if (e_len > 8) then begin
                       e_len := 8; remaining := e_len;     //qas 7/2013
                    end;
                 end;//140f
             $1455: begin
                    //msg('2005:1455@ '+inttostr(dFilePos(fp)) +'  len: '+inttostr(e_len) +' '+ chr(lT0)+chr(lT1));
                    (*if (e_len > 8) then begin
                       e_len := 8; remaining := e_len;     //qas 7/2013
                    end;   *)
                 end;//1455

		end; //CASE...element
          end; //if Manufacturer = Philips
	  end; //group 2005
          $5200 : begin
                case element of
                    $9230: begin
                       //msg('5200:9230@ '+inttostr(dFilePos(fp))+' e_len '+inttostr(e_len)+'  remaining '+inttostr(remaining));
                        //lverboseRead := true;   //qas
                       //fx(e_len,678xx9);
                     if (e_len > 8) then begin
                     e_len := 8; remaining := e_len;     //qas 7/2013
                      end;
                        (*   if (lDicomData.ManufacturerID = kPhilipsID) and (orientation_not_visible( lDICOMdata))then
                              read_philips_hidden(lFilename, dFilePos(fp),e_len,lDICOMdata);*)
                    end  //element 9230
                end; //case element
         end; //group 5200
          $5400 : begin
                case element of
                    $0100: begin
                      //can not convert sound files to images 12/2012
                      lImageFormatOK := false;
                      msg('Note: the DICOM file '+lFileName+' stores a waveform sequence (e.g. ECG) that will not be converted to an image');
                      info :='WaveformSequence';
                      //fx(lDICOMdata.DTI[lDICOMdata.nDTIdir].v1,lDICOMdata.DTI[lDICOMdata.nDTIdir].v2,lDICOMdata.DTI[lDICOMdata.nDTIdir].v3);
                      if not lrOK then goto 666;
                        e_len := 0;      remaining := 0;
                    end  //element 0100
                end; //case element
         end; //group 5400
     $DDFF : begin
               case element of
                    $00E0: begin
		           //For papyrus multislice format: if (lPapyrusSlice >=  lPapyrusnSlices) then
			   time_to_quit := TRUE;
                      end;
               end;
            end;
     $FFFE : begin
        case element of
        $E000 : begin
         //if (lIndent > 3) then lverboseRead := false;   //qas
         inc(lIndent);
         //msg('FFFE:E000@ '+inttostr(dFilePos(fp)) +'  len: '+inttostr(e_len) +' '+ chr(lT0)+chr(lT1));

                       e_len := 0; remaining := e_len;     //qas 7/2013
          lInside00209113 := (lprevGroup = $0020) and (lprevelement = $9113);
          lInside2005140F  := (lprevGroup = $2005) and (lprevelement = $140F);
         // if (lInside00209113) then fx(333);
        (*iif lJPEGEntries > 17 then
		   lTestError := true;


        if not lProprietaryImageThumbnail then begin
         f (lReadJPEGtables) and ((lDICOMdata.RunLengthEncoding) or (lDICOMdata.JPEGLossyCpt) or (lDICOMdata.JPEGLosslessCpt)) and (not lFirstFragment) and (e_len > 1024) {1384} and ( (e_len+dFilePos(fp)) <= FileSz) then begin
           //first fragment is the index table, so the previous line skips the first fragment
           if (gECATJPEG_table_entries = 0) then begin
              gECATJPEG_table_entries := lDICOMdata.XYZDim[3];
              getmem (gECATJPEG_pos_table, gECATJPEG_table_entries*sizeof(longint));
              getmem (gECATJPEG_size_table, gECATJPEG_table_entries*sizeof(longint));
           end;
           if lJPEGentries < gECATJPEG_table_entries then begin
               inc(lJPEGentries);
               gECATJPEG_pos_table^[lJPEGEntries] := dFilePos(fp);
               gECATJPEG_size_table^[lJPEGEntries] := e_len;
           end;
	   	end;

        if (lDICOMdata.CompressOffset =0) and ( (e_len+dFilePos(fp)) <= FileSz) and  (e_len > 1024){ALOKA} then begin
              lDICOMdata.CompressOffset := dFilePos(fp);
              lDICOMdata.CompressSz := e_len;
        end;
        //if e_len > lDICOMdata.CompressSz then lDICOMdata.CompressSz := e_len;
if (e_len > 1024) and (lDICOMdata.CompressSz=0) then begin //ABBA RLE ALOKA
            //Time_To_Quit := true;//ABBA
            lDICOMdata.CompressSz := e_len;
           lDICOMdata.CompressOffset := dFilePos(fp);
end;
        if  (lFirstFragment) or ((e_len > lDICOMdata.CompressSz) and not (lDicomData.RunLengthEncoding)) then
           lDICOMdata.CompressOffset := dFilePos(fp);
        if  (e_len > lDICOMdata.CompressSz)  and  (e_len > 1024){ALOKA} then
           lDICOMdata.CompressSz := e_len;
         lFirstFragment := false;
              lDICOMdataBackUp := lDICOMData;

		   if (gECATJPEG_table_entries = 1) then begin //updatex
               gECATJPEG_size_table^[1] := lDICOMdata.CompressSz;
               gECATJPEG_pos_table^[1] := lDICOMdata.CompressOffset;
           end; //updatex

end; //not proprietaryThumbnail
lProprietaryImageThumbnail := false; //1496
         *)
          lFirstFragment := false;//Dec09
          lDICOMdataBackUp := lDICOMData;//Dec09

         if ((e_len > 1024) and ((lDicomData.JPEGLosslessCpt)) or (e_len >= (lDicomData.XYZdim[1]*lDicomData.XYZdim[2]))){Apr 2011} and (lDicomData.XYZdim[1]> 1) then begin
            lDICOMdata.CompressOffset := dFilePos(fp);
            lDICOMdata.CompressSz  := e_len;
            
            Time_To_Quit := true;
            //msg('abba'+inttostr(lDICOMdata.CompressOffset)+'  '+inttostr(lDICOMdata.CompressSz));
         end;
              info := 'Image Fragment ['+inttostr(dFilePos(fp))+']';

         if  (dFilePos(fp) + e_len) >= filesz then
            Time_To_Quit := true;
              dSeek(fp, dFilePos(fp) + e_len);
                 tmpstr := inttostr(e_len);
                 remaining := 0;
                 e_len := 0;
              end;
       $E0DD : begin
              if (lIndent > 0) then dec(lIndent);
               lInside00209113 := false;
              lInside2005140F := false;
              info := 'Sequence Delimiter';
              if (lDICOMdata.XYZdim[1]<lDICOMdataBackUp.XYZdim[1]) then begin
                 lDICOMData := lDICOMdataBackUp;
                 dSeek(fp, dFilePos(fp) + e_len);
                 //lDICOMData := lDICOMdataBackUp;
              end else if not lImageFormatOK then  begin
              //x(lDicomData.RunLengthEncoding) or ( ((lDicomData.JPEGLossycpt) or (lDicomData.JPEGLosslesscpt)) and (gECATJPEG_table_entries >= lDICOMdata.XYZdim[3])) then
                  time_to_quit :=  TRUE;
              end;
             //RLE ABBA
             if (e_len = 0)  then begin //ALOKA
                explicitVR := true;
                time_to_quit :=  FALSE;//RLE16=false
             end;
             //END

              dSeek(fp, dFilePos(fp) + e_len);
              tmpstr := inttostr(e_len);
              remaining := 0;
              e_len := 0;
              end;
        end;
		end; 
        $FFFC : begin
              dSeek(fp, dFilePos(fp) + e_len);
                 tmpstr := inttostr(e_len);
                 remaining := 0;
                 e_len := 0;
              end;
	$72FF : case element of
		 $1041: time_to_quit := TRUE;
	end; //case 72FF
      $7FE0 :
        case element of
        	$00 :  begin
           info := 'Pixel Data Group Length';
           if not lImageFormatOK then time_to_quit := TRUE;
           end;
          $10 :  begin
              info := 'Pixel Data';
              TmpStr := inttostr(e_len);
              //Showmsg(inttostr(ExpectedDicomBytes(lDicomData) ) +'  '+ inttostr(e_len));
              if ((ExpectedDicomBytes(lDicomData) )  > e_len) or (lDICOMdata.XYZdim[1]<lDICOMdataBackUp.XYZdim[1]) then begin
                 lDICOMData := lDICOMdataBackUp;
				          dSeek(fp, dFilePos(fp) + e_len);
				          //lDICOMData := lDICOMdataBackUp;
			      end else if {(not lDicomData.RunLengthEncoding) and} (not lDicomData.JPEGLossycpt) and (not lDicomData.JPEGLosslesscpt) then begin
				      time_to_quit := TRUE;
				 //xlDicomData.ImageSz := e_len;

			  end;
			  e_len := 0;

		  end;


		  end;
	  else
		begin
		  if (group >= $6000) AND (group <= $601e) AND ((group AND 1) = 0)
			then  begin
					  info := 'Overlay'+inttostr(dfilepos(fp))+'x'+inttostr(e_len);
				end;
		  if element = $0000 then info := 'Group Length';
		  if element = $4000 then info := 'Comments';
				end;
	end;
lStr := '';

  1234:
     lprevGroup :=  Group;
     lprevElement := element;
if (Time_TO_Quit) and (not lImageFormatOK) then begin
   lHdrOK := true;
   goto 666;
end;

//Msg(inttohex(group,4) +':'+inttohex(element,4) +'   '+inttostr(e_len)+'@'+ inttostr(dfilepos(fp)));

 if (e_len + dfilepos(fp)) > FileSz then begin//patch for GE files that only fill top 16-bytes w Random data
	e_len := e_len and $FFFF;
 end;

	if (e_len > 131072) then begin
		//goto 666;
	end;//zebra
	if (NOT time_to_quit) AND (e_len > 0) and (remaining > 0) then begin
	 if (e_len + dfilepos(fp)) > FileSz then begin
		if not lImageFormatOK(*x(lDICOMdata.GenesisCpt) or (lDICOMdata.JPEGlosslessCpt) or (lDICOMdata.JPEGlossyCpt)*) then
		  lHdrOK := true
		else begin
                     Msg('dcm Error: not a DICOM image: '+lFilename);
                     {Msg('Diagnostics saved as: c:\dcmcrash.txt');
                     //diagnostics
                     assignfile(lTextF,'c:\dcmcrash.txt');
                     Filemode := 0;
                     rewrite(lTextF);
                     Write(lTextF,lDynStr);
                     closefile(lTextF); }

                     //Msg(inttohex(group,4) +':'+inttohex(element,4) +'   '+inttostr(e_len)+'@'+ inttostr(dfilepos(fp)));
                end;
		goto 666;
	 end;

	 if e_len > 0 then begin
		GetMem( buff, e_len);
	 dBlockRead(fp, buff, e_len, n);
	 if lVerboseRead then
	  case t of
		unknown :
			case e_len of
			1 : lStr := ( IntToStr(Integer(buff[0])));
			2 : Begin
					if lDicomData.little_endian <> 0
					then i := Integer(buff[0]) + 256*Integer(buff[1])
					else i := Integer(buff[0])*256 + Integer(buff[1]);
				  lStr :=( IntToStr(i));
							end;
			4 : Begin
					if lDicomData.little_endian <> 0
					then i :=               Integer(buff[0])
							  +         256*Integer(buff[1])
							  +     256*256*Integer(buff[2])
							  + 256*256*256*Integer(buff[3])
					else i :=   Integer(buff[0])*256*256*256
							  + Integer(buff[1])*256*256
							  + Integer(buff[2])*256
							  + Integer(buff[3]);
				  lStr := (IntToStr(i));
				end;
				else begin
						 if e_len > 0 then begin
							for i := 0 to e_len-1 do begin
							 if Char(buff[i]) in ['+','-','/','\',' ', '0'..'9','a'..'z','A'..'Z'] then
								   lStr := lStr+(Char(buff[i]))
								else
									lStr := lStr+('.');
							end;
						 end else
							 lStr := '*NO DATA*';
			end;
		   end;

		i8, i16, i32, ui8, ui16, ui32,
		_string  : for i := 0 to e_len-1 do
					if Char(buff[i]) in ['+','-','/','\',' ', '0'..'9','a'..'z','A'..'Z']
						then lStr := lStr +(Char(buff[i]))
					  else lStr := lStr +('.');
	  end;
	  FreeMem(buff);

	  end;
	end
	else if e_len > 0 then lStr := (IntToStr(tmp))
	else  begin
		 lStr := TmpStr;
	end;
 (*if (lGrp)  then if MessageDlg(lStr+'= '+info+' '+IntToHex(where,4)+': ('+IntToHex(group,4)+','+IntToHex(element,4)+')'+IntToStr(e_len)+'. Continue?',
	mtConfirmation, [mbYes, mbNo], 0) = mrNo then  GOTO 666;
   *)
 //if (Group > $2005)  then
 //   msg(info+' '+IntToStr(where)+': ('+IntToHex(group,4)+','+IntToHex(element,4)+')'+IntToStr(e_len));
{$IFDEF Troubleshoot}
      Msg( IntToHex(group,4)+','+IntToHex(element,4)+','+Info+'='+lStr);//+' Offset'+inttostr(dfilepos(fp))+' Length'+inttostr(e_len));
{$ENDIF Troubleshoot}


   if lverboseRead then begin
if length(lDynStr) > kMaxTextBuf then begin
   if not lTextOverFlow  then begin
	  lDynStr := lDynStr + 'Only showing the first '+inttostr(kMaxTextBuf) +' characters of this LARGE header';
	  lTextOverFlow := true;

   end;
   //goto 666;
end else
   lDynStr := lDynStr+IntToHex(group,4)+','+IntToHex(element,4)+','+Info+'='+lStr+kCR ;

    Msg(AddIndent(lIndent)+IntToHex(group,4)+','+IntToHex(element,4)+','+inttostr(e_len)+'@'+inttostr(dfilepos(fp))+','+Info+'='+lStr);
end; //not verbose read

  end;	// end for

  lDicomData.ImageStart := dfilepos(fp);

  if lBigSet then begin
	  if lBig then lDicomData.little_endian := 0
	  else lDicomData.little_endian := 1;
  end;
  lHdrOK := true;
if lByteSwap then begin
	ByteSwap(lDicomdata.XYZdim[1]);
	ByteSwap(lDicomdata.XYZdim[2]);
	if lDicomdata.XYZdim[3] <> 1 then
	 ByteSwap(lDicomdata.XYZdim[3]);
	 //xByteSwap(lDicomdata.SamplesPerPixel);
	 ByteSwap(lDicomData.Allocbits_per_pixel);
	 //xByteSwap(lDicomData.Storedbits_per_pixel);
end;

if (lDICOMdata.ManufacturerID = kPhilipsID) and (l4DDistanceBetweenSliceCenters <> kNaNsingle) then //some 3D and 4D Philips files do not correctly report interslice distance in 0018,0088 and 0018,0050...
   lDICOMdata.XYZmm[3] := (l4DDistanceBetweenSliceCenters);
if (lPrefs.PhilipsPrecise) and (lManufacturerIsPhilips) and (lPhilipsScaleSlope <> 0) then begin
  PhilipsPrecise (lDicomData.IntenScale, lDICOMdata.intenIntercept,lPhilipsScaleSlope, lDicomData.IntenScale, lDICOMdata.intenIntercept,true);
end; //if PARprecise
if (lDICOMdata.ManufacturerID = kPhilipsID) and (lDICOMdata.nDTIdir > 1) then begin
   lGELX := true;
   for i := 1 to lDICOMdata.nDTIdir do
       if lDICOMdata.DTI[lDICOMdata.nDTIdir].bval <> lDICOMdata.DTI[1].bval then
          lGELX := false;//multiple B0 directions
   if lGELX then
      lDICOMdata.nDTIdir := 1;
   lGELX := false;
end;
if (lMatrixSz > 1) and (lDicomData.CSAImageHeaderInfoPos > 0) and (lDicomData.CSAImageHeaderInfoSz > 0) and
   not (((lDicomdata.XYZdim[1] mod lMatrixSz) = 0) and  ((lDicomdata.XYZdim[2] mod lMatrixSz) = 0)) then begin
     //Slow method for non-square Siemens matrices - 0018:1310 based on phase/freq, so it is easier to read CSA to decode rows/columns

       GetCSAImageHeaderInfo (lFilename, lDicomData.CSAImageHeaderInfoPos ,lDicomData.CSAImageHeaderInfoSz, lTempInt,lDICOMdata.SiemensMosaicX,lDICOMdata.SiemensMosaicY, lfloat1,lfloat2,lfloat3)
end else
 if (lMatrixSz > 1) and ((lDicomdata.XYZdim[1] mod lMatrixSz) = 0) and ((lDicomdata.XYZdim[2] mod lMatrixSz) = 0) then begin

		if ((lDicomData.XYZdim[1] mod lMatrixSz)=0) then
		   lDicomData.SiemensMosaicX := lDicomData.XYZdim[1] div lMatrixSz;
		if ((lDicomData.XYZdim[2] mod lMatrixSz)=0) then
		   lDicomData.SiemensMosaicY := lDicomData.XYZdim[2] div lMatrixSz;
		if lDicomData.SiemensMosaicX < 1 then lDicomData.SiemensMosaicX := 1; //1366
		if lDicomData.SiemensMosaicY < 1 then lDicomData.SiemensMosaicY := 1; //1366

	  if  lOldSiemens_IncorrectMosaicMM then begin //old formats convert size in mm incorrectly - modern versions are correct and include transfer syntax
		 lDicomdata.XYZmm[1] := lDicomdata.XYZmm[1] * (lDicomdata.XYZdim[1] div lMatrixSz);
		 lDicomdata.XYZmm[2] := lDicomdata.XYZmm[2] * (lDicomdata.XYZdim[2] div lMatrixSz);
	  end;
end else if (lSiemensMosaic0008_0008) and (lPhaseEncodingSteps > 0) and (lPhaseEncodingSteps < lDicomdata.XYZdim[2]) and ((lDicomdata.XYZdim[2] mod lPhaseEncodingSteps) = 0) and ((lDicomdata.XYZdim[2] mod (lDicomdata.XYZdim[2] div lPhaseEncodingSteps)) = 0) then begin
	//1499c kludge for detecting new Siemens mosaics: WARNING may cause false positives - Siemens fault not mine!
	lDicomData.SiemensMosaicY :=lDicomdata.XYZdim[2] div lPhaseEncodingSteps;
	lDicomData.SiemensMosaicX := lDicomData.SiemensMosaicY;  //We also need to assume as many mosaic rows as columns, as Siemens does not save the phase encoding lines in the header...
end;
 // fx(lnSlicePerVol,lnVol, lDicomData.SlicesPer3DVol,lDicomdata.XYZdim[3]  );
//fx(lnVol,lnSlicePerVol,lDicomData.SlicesPer3DVol,lDicomdata.XYZdim[3]);
//fx(lnSlicePerVol,lDicomData.ManufacturerID,kPhilipsID );
if (lnSlicePerVol > 0) and (lDicomData.ManufacturerID = kPhilipsID) {and (lnVol > 1)} and (lDicomdata.XYZdim[3] > 1) and (lDicomData.SlicesPer3DVol > 0)and ((lDicomdata.XYZdim[3] mod lDicomData.SlicesPer3DVol) = 0)  then begin
   lDICOMdata.File4D := true;
   lnVol := lDicomdata.XYZdim[3] div lDicomData.SlicesPer3DVol;
end;
if lManufacturerIsBruker then
   lDicomData.AcquNum := 1; //Bruker varies this for every image

if (lEchoNum > 0) and (lEchoNum < 16) then begin
    lDicomData.AcquNum := lDicomData.AcquNum + (1000*lEchoNum);
end;

if lVerboseRead then begin
                      // lDicomData.PatientPosX, lDicomData.PatientPosY,lDicomData.PatientPosZ
           Msg ('DICOM data');
           Msg ('Series/Acquisition/Image/Xpos/YPos/ZPos:'+kTab+inttostr(lDicomData.SeriesNum)+kTab+inttostr(lDicomData.AcquNum)+kTab+inttostr(lDicomData.ImageNum)+kTab+floattostr(lDicomData.PatientPosX)+kTab+floattostr(lDicomData.PatientPosY)+kTab+floattostr(lDicomData.PatientPosZ));
           Msg ('BPP: '+inttostr(lDicomData.Allocbits_per_pixel));
           Msg ('XYZ dim:' +inttostr(lDicomData.XYZdim[1])+'/'+inttostr(lDicomData.XYZdim[2])+'/'+inttostr(lDicomData.XYZdim[3])  );
           Msg ('XYZ mm:'+floattostrf(lDicomData.XYZmm[1],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[2],ffFixed,8,2)+'/'+floattostrf(lDicomData.XYZmm[3],ffFixed,8,2) );
           Msg ('DTI bvalue:'+ inttostr(lDICOMdata.DTI[1].bval));
           Msg ('DTI bvec:'+floattostrf(lDicomData.DTI[1].v1,ffFixed,8,2)+'/'+floattostrf(lDicomData.DTI[1].v2,ffFixed,8,2)+'/'+floattostrf(lDicomData.DTI[1].v3,ffFixed,8,2) );
 end;
 //msg('abba'+inttostr(lDICOMdata.CompressOffset)+'  '+inttostr(lDICOMdata.CompressSz));
  666:
  //if not lHdrOk then Msg('zx'+lFilename);
  if lDiskCacheSz > 0 then
	 freemem(lDiskCacheRA);
  if not lHdrOK then lImageFormatOK := false;
  CloseFile(fp);
  FileMode := 2; //set to read/write
  //if kUseDateTimeForID then
	lDicomData.DateTime := StudyDateTime(lDicomData.StudyDate,lDicomData.StudyTime);
  if (lDicomData.SiemensMosaicX > 1) then
  	lDicomData.AcquNum := 1;
end;


end.