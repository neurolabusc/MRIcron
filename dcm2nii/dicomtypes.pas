unit dicomtypes;

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}
{$Include ..\common\isgui.inc}

interface
var kUseDateTimeForID: boolean = false;
const
     kGEID = 1;
     kPhilipsID = 2;
     kSiemensID = 3;
     kMaxDTIDir = 4096;//Maximum DTI directions
     kMaxOrderVal = 1024;
type
TDTI = record
   v1,v2,v3: double;   //4=volume, eg time: some EC*T7 images
   bval: integer
end;
  TDTIRA = array [1..kMaxDTIDir] of TDTI;//TDICOM;//unsigned 8-bit int
  TOrder= array [1..kMaxOrderVal] of byte;
    kDICOMStr = String[128];
   DICOMdata = record
             XYZdim: array [1..4] of integer;
             XYZori: array [1..3] of integer;
             XYZmm: array [1..3] of double;
             Orient: array [1..6] of double;
             SignedData,SiemensDICOMDTICSA,SiemensDICOMDTI,FloatData,file4D,JPEGLossyCpt,JPEGLosslessCpt: boolean;
             SecSinceMidnight,PatientPosX,PatientPosY,PatientPosZ,AngulationAP,AngulationFH,AngulationRL: double;
             FieldStrength, BandwidthPerPixelPhaseEncode, kV,TE, TR,IntenScale,IntenIntercept,location{,DTIv1,DTIv2,DTIv3}: single;
             {Bval,}SlicesPer3DVol,SiemensInterleaved {0=no,1=yes,2=not defined},SiemensSlices,SiemensMosaicX,SiemensMosaicY,
             nOrder,nDTIdir,AcquNum,ImageNum,SeriesNum,ImageStart,little_endian,Allocbits_per_pixel,SamplesPerPixel,
             CSAImageHeaderInfoPos,CSAImageHeaderInfoSz,
             CSASeriesHeaderInfoPos,CSASeriesHeaderInfoSz,ManufacturerID,PlanarConfig, //ImplementationVersion,
             Vers0018_1020,
             CompressOffset,CompresssZ: integer;
             DateTime: TDateTime;
             PatientHx, ImageComments,PatientGender,PatientDoB,PatientPos,PatientName,ProtocolName,StudyDate,StudyTime,PhilipsSliceOrient,ScanningSequence0018_0020 ,PhaseEncoding: kDICOMStr;
             Filename: string[255];        
             DTI: TDTI;//TDTIRA;
   Order: TOrder; //4D datasets
   //OrderSlope,OrderIntercept: TOrderScaling; //4D datasets
   end;//DICOMdata record

  TDICOMRA = array [1..1] of DicomData;//TDICOM;//unsigned 8-bit int
  TDICOMRAp = ^TDICOMRA;
(*  TNIFTIhdr  = packed record //Next: analyze Format Header structure
   HdrSz : longint; //MUST BE 348
   Data_Type: array [1..10] of char; //unused
   db_name: array [1..18] of char; //unused
   extents: longint; //unused
   session_error: smallint; //unused
   regular: char; ////unused: in Analyze 7.5 this must be 114
   dim_info: byte; //MRI slice order
   dim: array[0..7] of smallint; //Data array dimensions
   intent_p1, intent_p2, intent_p3: single;
   intent_code: smallint;
   datatype: smallint;
   bitpix: smallint;
   slice_start: smallint;
   pixdim: array[0..7]of single;
   vox_offset: single;
   scl_slope: single;//scaling slope
   scl_inter: single;//scaling intercept
   slice_end: smallint;
   slice_code: byte; //e.g. ascending
   xyzt_units: byte; //e.g. mm and sec
   cal_max,cal_min: single; //unused
   slice_duration: single; //time for one slice
   toffset: single; //time axis to shift
   glmax, glmin: longint; //UNUSED
   descrip: array[1..80] of char;
   aux_file: array[1..24] of char;
   qform_code, sform_code: smallint;
   quatern_b,quatern_c,quatern_d,
   qoffset_x,qoffset_y,qoffset_z: single;
   srow_x: array[0..3]of single;
   srow_y: array[0..3]of single;
   srow_z: array[0..3]of single;
   intent_name: array[1..16] of char;
   magic: longint;
 end; //TNIFTIhdr Header Structure


 const //nifti
kDT_BINARY                 =1;     // binary (1 bit/voxel)
kDT_UNSIGNED_CHAR          =2;     // unsigned char (8 bits/voxel)
kDT_SIGNED_SHORT           =4;     // signed short (16 bits/voxel)
kDT_SIGNED_INT             =8;     // signed int (32 bits/voxel)
kDT_FLOAT                 =16;     // float (32 bits/voxel)
kDT_COMPLEX               =32;     // complex (64 bits/voxel)
kDT_DOUBLE                =64;     // double (64 bits/voxel)
kDT_RGB                   =128;     // RGB triple (24 bits/voxel)
kDT_INT8                  =256;     // signed char (8 bits)
kDT_UINT16                =512;     // unsigned short (16 bits)
kDT_UINT32                =768;     // unsigned int (32 bits)
kDT_INT64                =1024;     // long long (64 bits)
kDT_UINT64               =1280;     // unsigned long long (64 bits)
kDT_FLOAT128             =1536;     // long double (128 bits)
kDT_COMPLEX128           =1792;     // double pair (128 bits)
kDT_COMPLEX256           =2048;     // long double pair (256 bits)
//   slice_code values
 kNIFTI_SLICE_SEQ_UNKNOWN = 0;
 kNIFTI_SLICE_SEQ_INC = 1;
 kNIFTI_SLICE_SEQ_DEC = 2;
 kNIFTI_SLICE_ALT_INC = 3;
 kNIFTI_SLICE_ALT_DEC = 4;
//xyzt_units values: note 3bit space and 3bit time packed into single byte
 kNIFTI_UNITS_UNKNOWN = 0;
 kNIFTI_UNITS_METER =  1;
 kNIFTI_UNITS_MM = 2;
 kNIFTI_UNITS_MICRON  = 3;
 kNIFTI_UNITS_SEC = 8;
 kNIFTI_UNITS_MSEC = 16;
 kNIFTI_UNITS_USEC = 24;
 kNIFTI_UNITS_HZ = 32;
 kNIFTI_UNITS_PPM = 40;
 //qform_code, sform_code values
 kNIFTI_XFORM_UNKNOWN = 0;
 kNIFTI_XFORM_SCANNER_ANAT = 1;//Scanner-based anatomical coordinates
 kNIFTI_XFORM_ALIGNED_ANAT = 2; //Coordinates aligned to another file e.g. EPI coregistered to T1
 kNIFTI_XFORM_TALAIRACH = 3; //Talairach-Tournoux Atlas; (0,0,0)=AC, etc.
 kNIFTI_XFORM_MNI_152 = 4; //MNI 152 normalized coordinates
 //Magic values
 kNIFTI_MAGIC_SEPARATE_HDR = $0031696E;//$6E693100;
 kNIFTI_MAGIC_EMBEDDED_HDR = $00312B6E;//$6E2B3100;
 //byte-swapped magic values
 kswapNIFTI_MAGIC_SEPARATE_HDR = $6E693100;
 kswapNIFTI_MAGIC_EMBEDDED_HDR = $6E2B3100;
 //Statistics Intention
 kNIFTI_INTENT_NONE        =0;
kNIFTI_INTENT_CORREL      =2;
kNIFTI_INTENT_TTEST       =3;
kNIFTI_INTENT_FTEST       =4;
kNIFTI_INTENT_ZSCORE      =5;
kNIFTI_INTENT_CHISQ       =6;
kNIFTI_INTENT_BETA        =7;
kNIFTI_INTENT_BINOM       =8;
kNIFTI_INTENT_GAMMA       =9;
kNIFTI_INTENT_POISSON    =10;
kNIFTI_INTENT_NORMAL     =11;
kNIFTI_INTENT_FTEST_NONC =12;
kNIFTI_INTENT_CHISQ_NONC =13;
kNIFTI_INTENT_LOGISTIC   =14;
kNIFTI_INTENT_LAPLACE    =15;
kNIFTI_INTENT_UNIFORM    =16;
kNIFTI_INTENT_TTEST_NONC =17;
kNIFTI_INTENT_WEIBULL    =18;
kNIFTI_INTENT_CHI        =19;
kNIFTI_INTENT_INVGAUSS   =20;
kNIFTI_INTENT_EXTVAL     =21;
kNIFTI_INTENT_PVAL       =22;
NIFTI_INTENT_LOGPVAL     =23;
NIFTI_INTENT_LOG10PVAL	 =24;
kNIFTI_LAST_STATCODE = 24;//kNIFTI_INTENT_PVAL;
kNIFTI_INTENT_ESTIMATE  =1001;
kNIFTI_FIRST_NONSTATCODE = kNIFTI_INTENT_ESTIMATE;
kNIFTI_INTENT_LABEL     =1002;
kNIFTI_INTENT_NEURONAME =1003;
kNIFTI_INTENT_GENMATRIX =1004;
kNIFTI_INTENT_SYMMATRIX =1005;
kNIFTI_INTENT_DISPVECT  =1006;
kNIFTI_INTENT_VECTOR    =1007;
kNIFTI_INTENT_POINTSET  =1008;
kNIFTI_INTENT_TRIANGLE  =1009;
kNIFTI_INTENT_QUATERNION =1010;
                       *)
const //dicom
kCR = chr (13);//PC EOLN
kA = ord('A');
kB = ord('B');
kC = ord('C');
kD = ord('D');
kE = ord('E');
kF = ord('F');
kH = ord('H');
kI = ord('I');
kL = ord('L');
kM = ord('M');
kN = ord('N');
kO = ord('O');
kP = ord('P');
kQ = ord('Q');
kS = ord('S');
kT = ord('T');
kU = ord('U');
kW = ord('W');

procedure PhilipsPrecise (lRS, lRI,lSS: single; var lSlope,lIntercept: single; Precise: boolean);
procedure clear_dicom_data (var lDicomdata:Dicomdata);
procedure Clear_DTIra(var lDTIra: TDTIra);
function DICOMinterslicedistance(var lDicomdata1,lDicomdata2:Dicomdata): single;//1392
function StudyDateTime (lInStudyDate, lInStudyTime: kDICOMStr): TDateTime;
function StudyDateTime2Str (lDateTime: TDateTime):string;
//function GetCSAImageHeaderInfoDTI (lFilename: string; lStart,lLength: integer; var lBval: integer; var ldti1,ldti2,ldti3: double): boolean;
//function GetCSAImageHeaderInfo (lFilename: string; lStart,lLength: integer; var lMosaicSlices,lMosaicX,lMosaicY: integer; var lv1,lv2,lv3: double): boolean;
procedure AplhaNumericStrDICOM (var lStr: kDICOMStr);
procedure PartialAcquisitionError;
function DICOMstr (i: integer; var lDICOMra: TDICOMrap;lOutname:string): string; overload;
function DICOMstr (i: integer; var lDICOMra: TDICOMrap): string; overload;
function DICOMstr (var lDICOM: DICOMdata): string; overload;


implementation

uses dicom,sysutils,define_types,dialogsx,dialogs_msg;

function YearsOld (lDICOM: DICOMdata): single;
var
   dob: TDateTime;
   lnoon:string;
begin
     result := 0;
     if length (lDICOM.PatientDoB) < 8 then
        exit; //YYYYMMDD
     try
       lnoon := '120000';
     dob := StudyDateTime (lDICOM.PatientDoB, lnoon);
       result := (lDICOM.DateTime-dob)/365.2425;
     except
           result := 0;
     end;
end;

function DICOMstr (var lDICOM: DICOMdata): string; overload;
begin

     result := lDICOM.Filename
     //ProtocolName,StudyDate,StudyTime,PhilipsSliceOrient,PhaseEncoding: kDICOMStr;
      +kTab+'Field Strength:'+kTab+floattostr(lDICOM.fieldStrength)
     +kTab+'ProtocolName:'+kTab+ lDICOM.ProtocolName
       +kTab+'ScanningSequence00180020:'+kTab+ lDICOM.ScanningSequence0018_0020
       +kTab+'TE:'+kTab+floattostr(lDICOM.TE)
       +kTab+'TR:'+kTab+floattostr(lDICOM.TR)

       +kTab+'SeriesNum:'+kTab+inttostr(lDICOM.SeriesNum)
       +kTab+'AcquNum:'+kTab+inttostr(lDICOM.AcquNum)
       +kTab+'ImageNum:'+kTab+inttostr(lDICOM.ImageNum)
       +kTab+'ImageComments:'+kTab+lDICOM.ImageComments

       +kTab+'DateTime:'+kTab+DateTimeToStr(lDICOM.DateTime)
       +kTab+'Name:'+kTab+lDICOM.PatientName
       +kTab+'PatientHistory:'+kTab+lDICOM.PatientHx
       +kTab+'DoB:'+kTab+lDICOM.PatientDoB
       +kTab+'Gender:'+kTab+lDICOM.PatientGender

       +kTab+'Age(Years):'+kTab+floattostr(YearsOld(lDICOM)) ;

end;

function DICOMstr (i: integer; var lDICOMra: TDICOMrap;lOutname: string): string;  overload;
var
   lS: string;
begin
     result :=  DICOMstr (lDICOMra^[i]);
     if lOutname <> '' then
        result := kTab+'SuggestedOutput:'+lOutname;
end;

(*function DICOMstr (i: integer; var lDICOMra: TDICOMrap;lOutname: string): string;  overload;
var
   lS: string;
begin
     if lOutname <> '' then
        lS := kTab+'SuggestedOutput:'+lOutname
     else
         lS := '';

     result := lDICOMra^[i].Filename
        +kTab+'ImageComments:'+lDICOMra^[i].ImageComments
       +kTab+'PatientHistory:'+lDICOMra^[i].PatientHx

            +kTab+'SeriesNum:'+kTab+inttostr(lDICOMra^[i].SeriesNum)
       +kTab+'AcquNum:'+inttostr(lDICOMra^[i].AcquNum)
       +kTab+'ImageNum:'+inttostr(lDICOMra^[i].ImageNum)
       +kTab+'Name:'+lDICOMra^[i].PatientName
       +kTab+'DoB:'+lDICOMra^[i].PatientDoB
       +kTab+'Gender:'+lDICOMra^[i].PatientGender
       +kTab+'DateTime:'+DateTimeToStr(lDICOMra^[i].DateTime)
       +kTab+'Age(Years):'+floattostr(YearsOld(lDICOMra^[i]))

       +lS ;

end; *)

function DICOMstr (i: integer; var lDICOMra: TDICOMrap): string; overload;
begin
    result := DICOMstr (i, lDICOMra,'')
end;

procedure PartialAcquisitionError;
begin
     dcmMsg('*  Potential partial acquisition or improper segmentation of files');
     {$IFDEF GUI}
        dcmMsg('*  Possible solution: check ''Collapse folders'' in Help/Preferences and select directory that contains all images in subfolders');
     {$ELSE}
        dcmMsg('*  Possible solution: use -c Y and use folder containing subdirectories as input');
        dcmMsg('*            or change .ini file to read: CollapseFolders=1');
     {$ENDIF}
end;

function  PhilipsPreciseVal (lPV, lRS, lRI,lSS: single): single;
begin
     if (lRS*lSS) = 0 then //avoid divide by zero
        result := 0
     else
         result := (lPV * lRS + lRI) / (lRS * lSS);
end;

procedure PhilipsPrecise (lRS, lRI,lSS: single; var lSlope,lIntercept: single; Precise: boolean);
var
   l0,l1: single;
begin
//# === PIXEL VALUES =============================================================
//#  PV = pixel value in REC file, FP = floating point value, DV = displayed value on console
//#  RS = rescale slope,           RI = rescale intercept,    SS = scale slope
//#  DV = PV * RS + RI             FP = DV / (RS * SS)
     if not Precise then begin //return DV not FP
        lSlope := lRS;
        if lSlope = 0 then
           lSlope := 1;
        lIntercept := lRI;
        exit;
     end; //if return DV
     l0 := PhilipsPreciseVal (0, lRS, lRI,lSS);
     l1 := PhilipsPreciseVal (1, lRS, lRI,lSS);
     if l0 = l1 then begin
        lSlope := 1;
        lIntercept := 0;
        exit;
     end;
     lIntercept := l0;
     lSlope := l1-l0;

end;


function SecSinceMidnight(H,Min,S: integer): integer;
//86,400 sec per day
begin
  //
	result := 3600*(H) + 60* Min + S;//H not H-1 as our clock runs from  0..23  not 1..24
end;

function BogusDateTime: TDateTime;
begin
     result := EncodeDate(1989,3,23) + (SecSinceMidnight(12,0,0) / 86400);
end;

function EncodeDateTime (Y,M,D,H,Min,S: integer): TDateTime;
begin

  try
	 result := EncodeDate(Y,M,D) + (SecSinceMidnight(H,Min,S) / 86400);
  except  //impossible date - set to cold fusion date
       result := BogusDateTime;
  end;
end;

procedure DecodeDateTime (lDateTime: TDateTime; var Y,M,D,H,Min,S: word);
var
   secs: integer;
begin
     try
        DecodeDate(lDateTime, Y, M, D);
     except  //unable to decode date - use cold fusion values
           Y := 1989;
           M := 3;
           D := 23;
     end;
     Secs := round(Frac(lDateTime)*86400);
     S := secs mod 60;
     Min := (secs div 60) mod 60;
     H := (secs div 3600);
end;

function StudyDateTime2Str (lDateTime: TDateTime):string;
var
  Y,M,D,H,Min,S: word;
begin
	 DecodeDateTime (lDateTime,Y,M,D,H,Min,S);
	 result := PadStr (Y, 4)+ PadStr (M, 2)+PadStr (D, 2)+'_'+PadStr (H, 2)+ PadStr (Min, 2)+PadStr (S, 2);
end;

function StudyDateTime (lInStudyDate, lInStudyTime: kDICOMStr): TDateTime;
var lStr,lStudyDate, lStudyTime: string;
	Y,M,D,H,Min,S: integer;
begin
	result := 0;
	if (length(lInStudyDate) < 8){YYYYMMDD} or (length(lInStudyTime) < 6) {hhmmss} then
		exit;
	//next compress string, e.g. Elscint saves time as 16:54:21
	lStudyDate :='';
	for S := 1 to length (lInStudyDate) do
		if lInStudyDate[S] in ['0'..'9'] then
			lStudyDate := lStudyDate + lInStudyDate[S];
	lStudyTime :='';
	for S := 1 to length (lInStudyTime) do
		if lInStudyTime[S] in ['0'..'9'] then
			lStudyTime := lStudyTime + lInStudyTime[S];

	if (length(lStudyDate) < 8){YYYYMMDD} or (length(lStudyTime) < 6) {hhmmss} then
		exit;
	lStr := lStudyDate[1]+lStudyDate[2]+lStudyDate[3]+lStudyDate[4];
	Y := strtoint(lStr);
	lStr := lStudyDate[5]+lStudyDate[6];
	M := strtoint(lStr);
	lStr := lStudyDate[7]+lStudyDate[8];
	D := strtoint(lStr);
	lStr := lStudyTime[1]+lStudyTime[2];
	H := strtoint(lStr);
	lStr := lStudyTime[3]+lStudyTime[4];
	Min := strtoint(lStr);
	lStr := lStudyTime[5]+lStudyTime[6];
	S := strtoint(lStr);
	result := EncodeDateTime (Y,M,D,H,Min,S);
end;

procedure AplhaNumericStrDICOM (var lStr: kDICOMStr);
var
	S: integer;
	lOutStr: string;
begin
	if length(lStr) < 1 then exit;
	lOutStr := '';

	for S := 1 to length (lStr) do
		if lStr[S] in ['0'..'9','A'..'Z','a'..'z'] then
			lOutStr := lOutStr+ lStr[S];
	lStr := lOutStr;
end;
(*
function GetCSAImageHeaderInfoRaw (lIsDTI: boolean; lFilename: string; lStart,lLength: integer; var li1,li2,li3: integer; var lf1,lf2,lf3: double): boolean;
//returns true if mosaic
//will return false for non-mosaics - even if the have DTI information!
//valid DTI signified by bval >= 0
const
        kMaxFloats = 6;
var
   //lZ: integer;
   lByteRA: Bytep;
   lNumarisTag: string;
   lInFile: file;
   lFloatRA: array [1..kMaxFloats] of double;

function Str2FloatLastNum ( lStr: string): boolean;
var
    lFStr: string;
    lP: integer;
begin
     lFloatRA[1] := 1;
     result := false;
     if (length(lStr) < 1) then
        exit;
     lFStr := '';
     lP := length(lStr);
     while (lP > 0) and ((lFStr = '') or (lStr[lP]  in ['+','-','0'..'9','.','e','E']))   do begin
           if lStr[lP]  in ['+','-','0'..'9','.','e','E'] then
              lFStr := lStr[lP]+lFStr;
           dec(lP);
     end;
     if (lFStr = '') then
        exit;
     try
        lFloatRA[1] := strtofloat(lFStr);
     except on EConvertError do
            lFloatRA[1] := 1;
     end;//except
     result := true;
end; //function Str2Float

function NumarisPos (lStr: string; lStart: integer): integer; //read 16 bit short integer
var
   lP,lLen,lMax,lMatch: integer;
begin
     result := 0;
     lLen := length(lStr);
     lMax := lLength-lLen;
     if (lStart < 1) or (lMax < 1) or (lLen < 1) then
        exit;
     for lP := lStart to lMax do begin
         lMatch := 0;
         while (lMatch < lLen) and (lStr[lMatch+1] = char( lByteRA[lP+lMatch]) ) do
               inc(lMatch);
         if lMatch = lLen then begin
             if (lP < lMax) and (char( lByteRA[lP+lMatch]) = '"') then begin
                 lMatch := 0;//We want DiffusionGradientDirection, but not "DiffusionGradientDirection"
             end else begin
                 result := lP;
                 exit;
             end;
         end;
     end;
end; //function NumarisPos

function Str2FloatNum ( lStr: string; lnFloats: integer): boolean;
var
    lFStr: string;
    lP,lnF: integer;
begin
     result := false;
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
                       dcmMsg('Unable to interpret '+lNumarisTag+ ' in '+extractfilename(lFilename));
                end;//except
                if lnF = lnFloats then begin
                   result := true;
                   exit;
                end;
                lFStr := '';
           end;
           inc(lP);
     end;
end; //function Str2Float

function NumarisStr (lStr,lIDStr: string): string;
var
   lP,lI: integer;
   lPrevNum : boolean;
begin
     result := '';
     lP := NumarisPos(lStr,1);
     if lP <1 then exit;
     if length(lIDstr) > 0 then begin
        lP := NumarisPos(lIDstr,lP);
        if lP <1 then exit;
     end;
     result := '';
     lI := lP + length(lStr);
     lPrevNum := false;
     While (lI < (lLength)) and (lByteRA^[lI] <> $CD) do begin
           if char(lByteRA[lI])  in ['-','0'..'9','.','p','*'] then begin
              result := result + char(lByteRA[lI]);
              lPrevNum := true;
           end else begin
               if lPrevNum then result := result + ' ';
               lPrevNum := false;
           end;
           inc(lI);
     end;
end;

function NumarisInt1 (lStr,lIDStr: string; var lI1: integer): boolean;
begin
    result := Str2FloatLastNum (NumarisStr(lStr,lIDStr));
    if not result then exit;
    lI1 := round(lFloatRA[1] );
end;

function NumarisFloat3 (lStr,lIDStr: string; var lF1,lF2,lF3: double): boolean;
begin
     //showmessage(lStr+' '+NumarisStr(lStr,lIDStr));
    result := Str2FloatNum (NumarisStr(lStr,lIDStr),3);
    if not result then exit;

     lF1 := (lFloatRA[1]);
     lF2 := (lFloatRA[2]);
     lF3 := (lFloatRA[3]);
end; //function NumarisFloat3

function NumarisInt2PStar (lStr,lIDStr: string; var lI1,lI2: integer): boolean;
var
   lLen,lPos,lStarPos: integer;
   lvStr,lpStarStr: string;
begin //a 96x96 mosaic is usually saved as '64*64', but in B13 you can see '96p*96' or '.95  96p*96'
     result := false;
     lvStr := NumarisStr(lStr,lIDStr);
     lLen := length(lvStr);
     if lLen < 4 then exit;//not found
     lStarPos := 0;
     for lPos := 1 to (lLen-1) do
         if (lvStr[lPos] = '*') then
            lStarPos := lPos;
     if lStarPos = 0 then exit;
     lpStarStr := '';
     lPos := lStarPos -1;
     while (lPos >= 1) and ((lpStarStr = '')  or (lvStr[lPos]  in ['0'..'9'])) do begin
           lpStarStr := lvStr[lPos] + lpStarStr;
           dec(lPos);
     end;
     lpStarStr := lpStarStr + ' ';
     lPos := lStarPos+1;
     while (lPos < lLen) and ((lpStarStr = '')  or (lvStr[lPos]  in ['0'..'9'])) do begin
           lpStarStr :=   lpStarStr+lvStr[lPos];
           inc(lPos);
     end;
     result := Str2FloatNum (lpStarStr,2);

     if not result then exit;
     lI1 := round(lFloatRA[1]);
     lI2 := round(lFloatRA[2]);
     //dcmMsg(lvStr+'  '+floattostr( lI1)+'x'+inttostr(lI2));
end;

begin // GetCSAImageHeaderInfoRaw
     result := false;
     if (lLength < 1) then
        exit;
     if FSize(lFilename) <= (lStart+lLength) then
        exit;
     li1 := -1; //impossible - should be >=0
     li2 := 0;
     li3 := 0;
     lf1 := 0;//impossible, therefore not DTI - should be -1..1
     lf2 := 0;//impossible, therefore not DTI
     lf3 := 0;//impossible, therefore not DTI
     GetMem(lByteRA,lLength);
     AssignFile(lInFile, lFileName);
     //dcmMsg('fz '+lFilename);
     FileMode := 0;  //Set file access to read only
     Reset(lInFile, 1);
     seek(lInFile,lStart);
     BlockRead(lInFile, lByteRA^[1], lLength);
     CloseFile(lInFile);
     FileMode := 2;
     if lIsDTI then begin

        result := NumarisInt1 ('B_value','IS',li1);
        //result := NumarisInt1 ('B_value','LO',li1);
        if li1 > 0 then begin
           NumarisFloat3('DiffusionGradientDirection','FD',lf1,lf2,lf3);
           //vx(lf1,lf2,lf3,123);
        end;
     end else begin //get mosaic info
         //fx(lStart,lLength);
         result := NumarisInt1 ('NumberOfImagesInMosaic','US',li1);
         if result then begin
            NumarisInt2pStar ('AcquisitionMatrixText','SH', li2,li3);
            NumarisFloat3('SliceNormalVector','FD',lf1,lf2,lf3);
         end;
     end;
     FreeMem(lByteRA);
end;//GetCSAImageHeaderInfoRaw

function GetCSAImageHeaderInfoDTI (lFilename: string; lStart,lLength: integer; var lBval: integer; var ldti1,ldti2,ldti3: double): boolean;
var
   li2,li3: integer; //not used
begin
     result := GetCSAImageHeaderInfoRaw (TRUE,lFilename, lStart,lLength, lBval,li2,li3, ldti1,ldti2,ldti3);
end;

function GetCSAImageHeaderInfo (lFilename: string; lStart,lLength: integer; var lMosaicSlices,lMosaicX,lMosaicY: integer; var lv1,lv2,lv3: double): boolean;
begin
     result := GetCSAImageHeaderInfoRaw (FALSE,lFilename, lStart,lLength, lMosaicSlices,lMosaicX,lMosaicY, lv1,lv2,lv3);
end;  *)

procedure Clear_DTIra(var lDTIra: TDTIra);
var
	lI: integer;
begin
     for lI := 1 to kMaxDTIDir do
         lDTIra[lI].Bval := -1;
end;

procedure clear_dicom_data (var lDicomdata:Dicomdata);
var
	lI: integer;
begin
  with lDicomData do begin
           lDicomData.CSAImageHeaderInfoPos  := 0;
           lDicomData.CSAImageHeaderInfoSz := 0;
           lDicomData.CSASeriesHeaderInfoPos := 0;
           lDicomData.CSASeriesHeaderInfoSz := 0;
	   for lI := 1 to 6 do
		Orient[lI] := 0;
	   DateTime := BogusDateTime;
       ManufacturerID := 0;
       kV := 0;
       //ImplementationVersion := 0;
       Vers0018_1020 := 0;
       AngulationFH := 0;
       AngulationRL := 0;
       AngulationAP := 0;
       nDTIdir := 0;
       nOrder := 0;
       PhilipsSliceOrient := 'NA';
       ScanningSequence0018_0020 := '';
       PhaseEncoding := 'NA';
       PatientPos := 'NA';
       DTI.Bval := -1;
       DTI.v1 := 0;
       DTI.v2 := 0;
       DTI.v3 := 0;
       SiemensDICOMDTI := true;
       SiemensDICOMDTICSA := false;
       file4D := false;
       PatientName := 'NO NAME';
       PatientDoB := 'NO DOB';
       PatientGender := 'NA';
       PatientHx := '';
       ImageComments := '';
       //PatientID := 'NO ID';
       StudyDate := '';
       StudyTime := '';
       SecSinceMidnight := 0;
       //AcqTime := '';
       //ImgTime := '';
       TR := 0;
       TE := 0;
       //Echo := 0;
       //kV := 0;
       //mA := 0;
       //Rotate180deg := false;
        //MaxIntensity := 0;
        //MinIntensity := 0;
        //MinIntensitySet := false;
        FloatData := false;
		ImageNum := -1;
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
        //Accession := 1;
        PlanarConfig:= 0; //only used in RGB values
        //runlengthencoding := false;
        //CompressSz := 0;
        //CompressOffset := 0;
        SamplesPerPixel := 1;
        //WindowCenter := 0;
        //WindowWidth := 0;
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
        //Storedbits_per_pixel:= Allocbits_per_pixel;
        //StudyDatePos := 0;
        //Spacing:=0;
        //Thickness:= 0;//1391
        Location:=0;
        //Modality:='MR';
        //ProtocolName := '';
        //serietag:='';
        PatientPosX := 0;//1392
        PatientPosY := 0;//1392
        PatientPosZ := 0;//1392
        JPEGLossyCpt := false;
        JPEGLosslessCpt := false;
        SignedData := true;
        CompressOffset := 0;
        CompresssZ := 0;
        BandwidthPerPixelPhaseEncode := 0; //7/2013
        FieldStrength := 0;

  end;
end;

function DICOMinterslicedistance(var lDicomdata1,lDicomdata2:Dicomdata): single;//1392
begin
  //dcmMsg(format('XYZ->XYZ %g %g %g   %g %g %g', [lDICOMdata1.PatientPosX, lDICOMdata1.PatientPosY, lDICOMdata1.PatientPosZ,lDICOMdata2.PatientPosX, lDICOMdata2.PatientPosY, lDICOMdata2.PatientPosZ]));

     result := sqrt(sqr(lDICOMdata1.PatientPosX-lDICOMdata2.PatientPosX)
            +sqr(lDICOMdata1.PatientPosY-lDICOMdata2.PatientPosY)
            +sqr(lDICOMdata1.PatientPosZ-lDICOMdata2.PatientPosZ));

end;

end.

