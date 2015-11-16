unit parconvert;
{$H+}
interface
uses
{$IFDEF FPC}gzio2, {$ELSE} gziod,{$ENDIF}
math, bvec,define_types,SysUtils,dicom,dicomtypes,filename,nii_4dto3d,niftiutil,nii_orient, nii_crop,GraphicsMathLibrary,prefs,dialogs_msg, nifti_types;

function LoadFileListPARREC  (var lInFilename, lOutDir: string; var lPrefs: TPrefs): boolean;
implementation
uses dialogsx;


procedure PAR2DICOMstudyDate(var lDicomData: DICOMdata);
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
	lDicomData.DateTime := StudyDateTime(lDicomData.StudyDate,lDicomData.StudyTime);
end;

procedure ShellSortItems (first, last: integer; var lPositionRA: longintp; lIndexRA: int64P; var lRepeatedValues: boolean);
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
end; //shellsort is fast and requires less memory than quicksort

function SinDeg(lDeg: double): double;
begin
	 result := sin(lDeg*PI/180);
end;

function CosDeg(lDeg: double): double;
begin
	 result := Cos(lDeg*PI/180);
end;

  FUNCTION Matrix3DL (CONST m11,m12,m13, m21,m22,m23,
						   m31,m32,m33:  DOUBLE):  TMatrix;
  BEGIN
    WITH RESULT DO
	BEGIN
      matrix[1,1] := m11; matrix[1,2] := m12;
	  matrix[1,3] := m13; matrix[1,4] := 0;
      matrix[2,1] := m21; matrix[2,2] := m22;
	  matrix[2,3] := m23; matrix[2,4] := 0;
      matrix[3,1] := m31; matrix[3,2] := m32;
	  matrix[3,3] := m33; matrix[3,4] := 0;
	  matrix[4,1] := 0; matrix[4,2] := 0;
      matrix[4,3] := 0; matrix[4,4] := 1;
      size := size3D
    END
  END {Matrix3D};
    // 'Defuzz' is used for comparisons and to avoid propagation of 'fuzzy',
  //  nearly-zero values.  DOUBLE calculations often result in 'fuzzy' values.
  //  The term 'fuzz' was adapted from the APL language.
(* FUNCTION  Defuzz(CONST x:  DOUBLE):  DOUBLE;
  BEGIN
    IF  ABS(x) < fuzz
    THEN RESULT := 0.0
    ELSE RESULT := x
  END {Defuzz};
  *)
   FUNCTION MultiplyMatrices (CONST a,b:  TMatrix):  TMatrix;
    VAR
      i,j,k:  TIndex;
      temp :  DOUBLE;
  BEGIN
    RESULT.size := a.size;
    IF  a.size = b.size
    THEN

      FOR i := 1 TO a.size DO
      BEGIN
        FOR j := 1 TO a.size DO
        BEGIN

          temp := 0.0;
          FOR k := 1 TO a.size DO
          BEGIN
            temp := temp + a.matrix[i,k]*b.matrix[k,j];
          END;
          RESULT.matrix[i,j] := Defuzz(temp)
		END
      END

    ELSE dcmMsg('MultiplyMatrices error: '+inttostr(a.size)+' <> '+inttostr(b.size))
  END {MultiplyMatrices};

 (* function RealToStr(lR: double {was extended}; lDec: integer): string;
begin
	 RealTOStr := FloatToStrF(lR, ffFixed,7,lDec);
end;

procedure ReportMatrix (lStr: string;lM:TMatrix);
begin
	dcmMsg(lStr);
	dcmMsg(	RealToStr(lM.matrix[1,1],6)+','+RealToStr(lM.matrix[1,2],6)+','+RealToStr(lM.matrix[1,3],6)+','+RealToStr(lM.matrix[1,4],6));
	dcmMsg(	RealToStr(lM.matrix[2,1],6)+','+RealToStr(lM.matrix[2,2],6)+','+RealToStr(lM.matrix[2,3],6)+','+RealToStr(lM.matrix[2,4],6));
	dcmMsg(	RealToStr(lM.matrix[3,1],6)+','+RealToStr(lM.matrix[3,2],6)+','+RealToStr(lM.matrix[3,3],6)+','+RealToStr(lM.matrix[3,4],6));
	dcmMsg(	RealToStr(lM.matrix[4,1],6)+','+RealToStr(lM.matrix[4,2],6)+','+RealToStr(lM.matrix[4,3],6)+','+RealToStr(lM.matrix[4,4],6));
end;   *)

FUNCTION Diag3D (CONST m1,m2,m3,m4:  DOUBLE):  TMatrix;
  BEGIN
	WITH RESULT DO
	BEGIN
	  matrix[1,1] := m1; matrix[1,2] := 0;
	  matrix[1,3] := 0; matrix[1,4] := 0;

	  matrix[2,1] := 0; matrix[2,2] := m2;
	  matrix[2,3] := 0; matrix[2,4] := 0;

	  matrix[3,1] := 0; matrix[3,2] := 0;
	  matrix[3,3] := m3; matrix[3,4] := 0;

	  matrix[4,1] := 0; matrix[4,2] := 0;
	  matrix[4,3] := 0; matrix[4,4] := m4;
	  size := size3D
	END
END {Diag3D};

FUNCTION Matrix3D (CONST m11,m12,m13,m14, m21,m22,m23,m24,
                           m31,m32,m33,m34, m41,m42,m43,m44:  DOUBLE):  TMatrix;
  BEGIN
    WITH RESULT DO
    BEGIN
      matrix[1,1] := m11; matrix[1,2] := m12;
      matrix[1,3] := m13; matrix[1,4] := m14;

	  matrix[2,1] := m21; matrix[2,2] := m22;
      matrix[2,3] := m23; matrix[2,4] := m24;

      matrix[3,1] := m31; matrix[3,2] := m32;
      matrix[3,3] := m33; matrix[3,4] := m34;

      matrix[4,1] := m41; matrix[4,2] := m42;
      matrix[4,3] := m43; matrix[4,4] := m44;
      size := size3D
	END
  END {Matrix3D};

function mat44_inverse(var R: Tmatrix ) : TMatrix;
var
	r11,r12,r13,r21,r22,r23,r31,r32,r33,v1,v2,v3 , deti : double;
	Q: TMatrix;
begin
   r11 := R.matrix[1,1]; r12 := R.matrix[1,2]; r13 := R.matrix[1,3];  //* [ r11 r12 r13 v1 ] */
   r21 := R.matrix[2,1]; r22 := R.matrix[2,2]; r23 := R.matrix[2,3];  //* [ r21 r22 r23 v2 ] */
   r31 := R.matrix[3,1]; r32 := R.matrix[3,2]; r33 := R.matrix[3,3];  //* [ r31 r32 r33 v3 ] */
   v1  := R.matrix[1,4]; v2  := R.matrix[2,4]; v3  := R.matrix[3,4];  //* [  0   0   0   1 ] */

   deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
		 +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;

   if( deti <> 0.0 ) then
	deti := 1.0 / deti ;

   Q.matrix[1,1] := deti*( r22*r33-r32*r23) ;
   Q.matrix[1,2] := deti*(-r12*r33+r32*r13) ;
   Q.matrix[1,3] := deti*( r12*r23-r22*r13) ;
   Q.matrix[1,4] := deti*(-r12*r23*v3+r12*v2*r33+r22*r13*v3
					 -r22*v1*r33-r32*r13*v2+r32*v1*r23) ;

   Q.matrix[2,1] := deti*(-r21*r33+r31*r23) ;
   Q.matrix[2,2] := deti*( r11*r33-r31*r13) ;
   Q.matrix[2,3] := deti*(-r11*r23+r21*r13) ;
   Q.matrix[2,4] := deti*( r11*r23*v3-r11*v2*r33-r21*r13*v3
					 +r21*v1*r33+r31*r13*v2-r31*v1*r23) ;

   Q.matrix[3,1] := deti*( r21*r32-r31*r22) ;
   Q.matrix[3,2] := deti*(-r11*r32+r31*r12) ;
   Q.matrix[3,3] := deti*( r11*r22-r21*r12) ;
   Q.matrix[3,4] := deti*(-r11*r22*v3+r11*r32*v2+r21*r12*v3
					 -r21*r32*v1-r31*r12*v2+r31*r22*v1) ;

   Q.matrix[4,1] := 0; Q.matrix[4,2] := 0; Q.matrix[4,3] := 0.0 ;
   Q.matrix[4,4] := 1;// (deti == 0.0l) ? 0.0l : 1.0l ; /* failure flag if deti == 0 */

   result :=  Q ;
end;

procedure SetLarger (var lA,lB: double);
begin
	if lA > lB then
		lB := lA
	else
		lA := lB;
end;


procedure matx(var lNHdr: TNiftiHdr; var lDICOMdata: DICOMdata; b,c,a,{b,c,a,}offa,offb,offc,lx,ly,lz,lAPFOV,lFHFOV,lRLFOV: single; lOrient: integer);
var
lxmm,lymm,lzmm,x,y,z,la,lb,lc: double;
dx,dy,dz: single;
	analyze_to_dicom,base,ra,rb,rc,lmm,patient_to_tal,lZm:TMatrix;
begin
	lNHdr.sform_code := kNIFTI_XFORM_UNKNOWN;
	if (lZ < 1) or (lY < 1) or (lX < 1) then exit;
	  {a=angle(3,1);
	  b=angle(1,1);
	  c=angle(2,1);}
	  ra := Matrix3DL(1, 0, 0, 0, cos(a*pi/180), -sin(a*pi/180), 0, sin(a*pi/180), cos(a*pi/180));
	  rb := Matrix3DL(cos(b*pi/180), 0, sin(b*pi/180), 0, 1, 0, -sin(b*pi/180), 0, cos(b*pi/180));
	  rc := Matrix3DL(cos(c*pi/180), -sin(c*pi/180), 0, sin(c*pi/180), cos(c*pi/180), 0, 0, 0, 1);
	  base.size := size3D;
	  base := MultiplyMatrices(rb,rc);
	  base := MultiplyMatrices(ra,base);
	  if lOrient = 2 then begin //sagittal
             //dcmMsg('sag');
                     lmm := Matrix3D (
			0, 0, -1,0,
			1, 0, 0, 0,
			0, -1, 0,0,
			0, 0, 0, 1);
		lYmm := lAPFOV /lX;
		lZmm := lFHFOV / lY;
		//use smallest in plane resolution...
		SetLarger (lYmm,lZmm);
		lXmm := lRLFOV /lZ;
	end else if lOrient = 3 then begin //coronal
                 //dcmMsg('Coronal');
                 lmm := Matrix3D (
			1, 0, 0,0,
			0,0, 1, 0,
			0, -1, 0,0,
			0, 0, 0, 1);
		lXmm := lRLFOV /lX;
		lZmm := lFHFOV / lY;
		//use smallest in plane resolution...
		SetLarger (lXmm,lZmm);
		lYmm := lAPFOV /lZ;

	end else begin
                 //dcmMsg('Axial '+inttostr(lOrient));
                  lmm :=  diag3D(1, 1, 1,1);
                 lXmm := lRLFOV /lX;
		lYmm := lAPFOV /lY;
		//use smallest in plane resolution...
		SetLarger (lXmm,lYmm);
		lZmm := lFHFOV / lZ;
	end;
		lZm := Matrix3D (lxmm,0,0,0,
					  0,lymm,0,0,
						   0,0,lZmm,0,
						   0,0,0,1);
    patient_to_tal   := diag3D(-1, -1, 1,1);
   analyze_to_dicom := Matrix3D (
	1, 0, 0,0,
	0,-1, 0,0,
	0, 0, 1,0,
	0, 0, 0, 1);

//correct- A_tot=patient_to_tal*R_tot*Zm*lmm*analyze_to_dicom;
//wrong - A_tot=patient_to_tal*Zm*R_tot*lmm*analyze_to_dicom;
{ReportMatrix('Rtot',base);
ReportMatrix('zoom',lZm);
ReportMatrix('p2tal',patient_to_tal);
ReportMatrix('lmm',lmm);
ReportMatrix('analyze_to_dicom',analyze_to_dicom);}

   base := MultiplyMatrices(patient_to_tal,base);
   base := MultiplyMatrices(base,lZm);
   base := MultiplyMatrices(base,lmm);//2/2007 suggested by Bas Neggers
   base:= MultiplyMatrices(base,analyze_to_dicom);

	x := (lx-1)/2;
	y := (ly-2)/2;
	z := (lz-1)/2;
	la :=(base.matrix[1,1]*x)+(base.matrix[1,2]*y)+(base.matrix[1,3]*z)+base.matrix[1,4];
	lb :=(base.matrix[2,1]*x)+(base.matrix[2,2]*y)+(base.matrix[2,3]*z)+base.matrix[2,4];
	lc :=(base.matrix[3,1]*x)+(base.matrix[3,2]*y)+(base.matrix[3,3]*z)+base.matrix[3,4];
	base.matrix[1,4] := -la-offa;
	base.matrix[2,4] := -lb-offb;
	base.matrix[3,4] := -lC+offc;
   //ReportMatrix('nifti final',base);
   lNHdr.sform_code := kNIFTI_XFORM_SCANNER_ANAT;
   lNHdr.srow_x[0] := base.matrix[1,1];
   lNHdr.srow_x[1] := base.matrix[1,2];
   lNHdr.srow_x[2] := base.matrix[1,3];
   lNHdr.srow_x[3] := base.matrix[1,4];
   lNHdr.srow_y[0] := base.matrix[2,1];
   lNHdr.srow_y[1] := base.matrix[2,2];
   lNHdr.srow_y[2] := base.matrix[2,3];
   lNHdr.srow_y[3] := base.matrix[2,4];
   lNHdr.srow_z[0] := base.matrix[3,1];
   lNHdr.srow_z[1] := base.matrix[3,2];
   lNHdr.srow_z[2] := base.matrix[3,3];
   lNHdr.srow_z[3] := base.matrix[3,4];
   lNHdr.qform_code := kNIFTI_XFORM_SCANNER_ANAT;
   nifti_mat44_to_quatern( base,
   lNHdr.quatern_b,lNHdr.quatern_c,lNHdr.quatern_d,
   lNHdr.qoffset_x,lNHdr.qoffset_y,lNHdr.qoffset_z,
                             dx, dy, dz, lNHdr.pixdim[0]{QFac});
 end;

function DTItextfiles (lImgName: string; lDTIra: TDTIra; lNumDir: integer): boolean;
//create text files that describe output vectors
var
   lOutDTIname: string;
   lTextF: TextFile;
   lSeries,lStart,lEnd: integer;
begin
     result := false;
     if lImgName = '' then exit;
     lStart := 1;
     lEnd := lNumDir;
     //ensure some variability
     lSeries := 1;
     while (lSeries <= lEnd) and (lDTIra[1].v1 = lDTIra[lSeries].v1) and (lDTIra[1].bval = lDTIra[lSeries].bval) do
       inc(lSeries);
     if (lSeries > lEnd) then exit; //no variability in bvec or bval
     //create bvec
     lOutDTIname := changefileextX(lImgName,'.bvec');
     assignfile(lTextF,lOutDTIname);
     Filemode := 0;
     rewrite(lTextF);
     for lSeries := lStart to lEnd do
         Write(lTextF,floattostr(lDTIra[lSeries].v1)+ ' ');
     Writeln(lTextF);
     for lSeries := lStart to lEnd do
         Write(lTextF,floattostr(lDTIra[lSeries].v2)+ ' ');
     Writeln(lTextF);
     for lSeries := lStart to lEnd do
         Write(lTextF,floattostr(lDTIra[lSeries].v3)+ ' ');
     Writeln(lTextF);
     closefile(lTextF);
     //create bval
     lOutDTIname := changefileextX(lOutDTIname,'.bval');
     assignfile(lTextF,lOutDTIname);
     Filemode := 0;
     rewrite(lTextF);
     for lSeries := lStart to lEnd do
         Write(lTextF,inttostr(lDTIra[lSeries].bval)+' ');
     Writeln(lTextF);
     closefile(lTextF);
     result := true;
end;// funct DTItextfiles


procedure read_PAR2NII(var lNHdr: TNIftIHdr; var lDICOMdata: DICOMdata; var lHdrOK, lImageFormatOK,lPrecise:boolean;  var lDynStr: string;var lFileName: string; lReadOffsetTables: boolean; var lOffset_pos_table: LongIntp; var lOffsetTableEntries,lRescaleEntries: integer;  var lSlopeRA,lInterceptRA: Singlep; var lnum4Ddatasets, lSliceOrient: integer; var lDTIra: TDTIra);
label 333; //1384 now reads up to 8 dimensional data....
type tRange = record
     Min,Val,Max: double; //some vals are ints, others floats
end;
const UNIXeoln = chr(10);
     kMaxnSLices = 18000;//delphi 32000 - lazarus fails >15000
     kXdim = 1;
     kYdim = 2;
     kBitsPerVoxel = 3;
     kSliceThick = 4;
     kSliceGap = 5;
     kXmm = 6;
	 kYmm = 7;
	 kRS = 8;
	 kRI = 9;
	 kSS = 10; //1393 - attempt to use calibrated values
	 kDynTime = 11;
	 kSlice = 12;
	 kEcho = 13;
	 kDyn = 14;
	 kCardiac = 15;
	 kType = 16;
	 kSequence = 17;
         kASL = 18;
	 kIndex = 19;
       lIsParVers3: boolean = true;
       lIsParVers42: boolean = false;
       lIsParVers41: boolean = false;
     lRepeatedValues : boolean = false;
     lSlicesNotInSequence: boolean = false;
     lMaxSlice : integer = 0;
     lMaxIndex : integer = 0;
     lSliceSz: integer = 0;
     lMatOrient: boolean = false;
     //lOffsetTablesRequired: boolean = false;
var
   lDTIraDyn, lDTIraDynUnSorted: array of TDTI;
   //lDTIra: TDTIra;
   //lHFSStr,
   lErrorStr,lInStr,lUpCaseStr,lReportedTRStr: string;
   lAPFOV,lFHFOV,lRLFOV,
   lScanResX,lScanResY,lAngleA,lAngleB,lAngleC,lOffset1,lOffset2,lOffset3{,lXFOV,lYFOV}: double;
   lSliceIndexRAx,lSliceSequenceRA,lSortedSliceSequence: int64P;
   //lSliceIndexRA: array [1..kMaxnSlices] of longint;
   //lSSx,lRSx,lRIx: array [1..kMaxnSlices] of single;
   lSlopeRAx,lInterceptRAx: array [1..kMaxnSlices] of single;
   lSliceHeaderRA: array [1..50] of double;
   //lRepeatedValues,lSlicesNotInSequence,lIsParVers3: boolean;//,lMissingVolumes,{,lLongRAtooSmall,lMissingVolumes,lConstantScale,lContiguousSlices,}
   lRangeRA: array [kXdim..kIndex] of tRange;
   lSliceInfoCount,lPos,lLen,lFileSz,lHdrPos,linPos,lInc,lOrient: integer;
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
			 dcmMsg('read_PAR2NII: Unable to convert the string '+lStr+' to a number');
			 result := 1;
			 exit;
		  end;
	end; {except}
end; //nested func ReadParFloat
begin
  //Initialize parameters
  lOrient := 0;
  lAPFOV := 1;
  lnum4Ddatasets := 1;
  lMatOrient := false;
  lIsParVers3 := true;
  lIsParVers41 := false;
  lIsParVers42 := false;
  lSliceInfoCount := 0;
  getmem(lSliceIndexRAx, kMaxnSLices* sizeof(int64));
  setlength(lDTIraDyn,kMaxnSLices+1);//+1 since indexed from zero

  for lInc := kXdim to kIndex do //initialize all values: important as PAR3 will not explicitly report all
	  MinMaxTRange(lRangeRA[lInc],0);
  lHdrOK := false;
  lImageFormatOK := false;
  lRescaleEntries := 0;
  lOffsetTableEntries := 0;
  Clear_Dicom_Data(lDicomData);
  lDynStr := '';

  //Read text header to buffer (lCharRA)
  FileMode := 0; //set to readonly
  AssignFile(fp, lFileName);
  Reset(fp, 1);
  lFileSz := FileSize(fp);
  GetMem( lCharRA, lFileSz+1 ); //note: must free dynamic memory: goto 333 if any error
  GetMem (lSliceSequenceRA, kMaxnSLices*sizeof(int64));  //note: must free dynamic memory: goto 333 if any error
  BlockRead(fp, lCharRA^, lFileSz, lInpos);
  if lInPos <> lFileSz then begin
     dcmMsg('read_PAR2NII: Disk error, unable to read full input file.');
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
	else if (lInStr[1] = '#') and (lHdrOK) then  begin//# -> comment   ignore UNLESS it reveals version
        if (Length(lInStr)> 16) and (lInStr[3] = 'C') and (Copy(lInStr,3,15) = 'CLINICAL TRYOUT') then begin
           lUpCaseStr := '';
           lHdrPos := Length(lInStr);
           while (lHdrPos > 0) and (UpCase(lInStr[lHdrPos]) <> 'V') do begin
                 if lInStr[lHdrPos] in ['.', '0'..'9'] then
                 lUpCaseStr := UpCase(lInStr[lHdrPos])+lUpCaseStr;
                 dec(lHdrPos);
           end;
           if lUpCaseStr = '3' then
              lIsParVers3 := true
           else if lUpCaseStr = '4' then
              lIsParVers3 := false
           else if lUpCaseStr = '4.1' then begin
              lIsParVers3 := false;
              lIsParVers41 := true;
              dcmMsg('PAR v4.1 not yet fully supported')
           end else if lUpCaseStr = '4.2' then begin //11/2007
              lIsParVers3 := false;
              lIsParVers41 := true;
              lIsParVers42 := true;
              //dcmMsg('PAR v4.2 : DTI bval/bvec support still experimental')
           end else
               dcmMsg('Warning: unknown PAR version '+lUpCaseStr);

        end;
    end else if (lInStr[1] = '.') or (not lHdrOK) then  begin  //  GENERAL_INFORMATION section (line starts with '.')
      //Note we also read in lines that do not have '.' if we have HdrOK=false, this allows us to detect the DATADESCRIPTIONFILE signature
      While (lPos <= lLen) and (lInStr[lPos] <> ':') and ((not lHdrOK) or (lInStr[lPos] <> '#')) do begin
        if lInStr[lPos] in ['[',']','(',')','/','+','-',{' ',} '0'..'9','a'..'z','A'..'Z'] then
           lUpCaseStr := lUpCaseStr+upcase(linStr[lPos]);
        inc(lPos);
      end; //while reading line
      inc(lPos); {read equal sign in := statement}
               lDynStr := lDynStr + lInStr+kCR;
	  //dcmMsg(inttostr(length(lUpCaseStr)));
      if (not lHdrOK) and (lUpcaseStr = ('DATADESCRIPTIONFILE')) then begin //1389 PAR file
            lHdrOK := true;
            lDicomData.little_endian := 1;
      end;

	  if (lUpCaseStr ='REPETITIONTIME[MSEC]') or (lUpCaseStr ='REPETITIONTIME[MS]') then
		 lDicomData.TR :=  round(readParFloat);
	  if (lUpCaseStr ='MAXNUMBEROFSLICES/LOCATIONS') then
		 lDicomData.XYZdim[3] :=  round(readParFloat);
	  if (lUpCaseStr ='SLICETHICKNESS[MM]') then
		 MinMaxTRange(lRangeRA[kSliceThick],readParFloat);
      if (lUpCaseStr ='SLICEGAP[MM]') then
         MinMaxTRange(lRangeRA[kSliceGap],readParFloat);
	  if lUpCaseStr = 'FOV(APFHRL)[MM]' then begin
		 lDicomData.XYZmm[2] :=  (readParFloat); //AP anterior->posterior
		 lDicomData.XYZmm[3] :=  (readParFloat); //FH foot head
		 lDicomData.XYZmm[1] :=  (readParFloat); //RL Right-Left
		 lAPFOV := lDicomData.XYZmm[2];
		 lFHFOV := lDicomData.XYZmm[3];
		 lRLFOV := lDicomData.XYZmm[1];
	  end;
	  if lUpCaseStr = 'SCANRESOLUTION(XY)' then begin
		 lScanResX :=  round(readParFloat);
		 lScanResY :=  round(readParFloat);
	  end;
	  {if lUpCaseStr = 'SCANPERCENTAGE' then begin
		 lScanPct :=  round(readParFloat);
      end; }
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
	  if (lUpCaseStr ='ANGULATIONMIDSLICE(APFHRL)[DEGR]') then begin
		  lAngleA :=  (readParFloat);
		  lAngleB :=  (readParFloat);
		  lAngleC :=  (readParFloat);
                  lDicomData.AngulationAP := lAngleA;
                  lDicomData.AngulationFH := lAngleB;
                  lDicomData.AngulationRL := lAngleC;
	  end;
	  if (lUpCaseStr ='OFFCENTREMIDSLICE(APFHRL)[MM]') then begin
		  lOffset2 :=  (readParFloat);
		  lOffset3 :=  (readParFloat);
		  lOffset1 :=  (readParFloat);
	  end;
	  if lUpCaseStr = 'PROTOCOLNAME' then
		 lDicomData.ProtocolName := readParStr;
          if lUpCaseStr = 'PATIENTPOSITION' then begin
             lDicomData.PatientPos := UpperCase  (readParStr); //upcase
             if (lDicomData.PatientPos <> 'HEAD FIRST SUPINE') then
                dcmMsg('*WARNING: participant was not head first supine - spatial transforms may be wrong :'+lDicomData.PatientPos)
             else
                 lDicomData.PatientPos := 'HFS';


          end;
      if lUpCaseStr = 'PATIENTNAME' then
         lDicomData.PatientName := readParStr;
      if lUpCaseStr ='IMAGEPIXELSIZE[8OR16BITS]' then begin
         MinMaxTRange(lRangeRA[kBitsPerVoxel],readParFloat);
      end;
      if not lHdrOK then  begin
         dcmMsg('read_PAR2NII: Error reading header');
         goto 333;
	  end;
	end else begin  //SliceInfo: IMAGE_INFORMATION (line does NOT start with '.' or '#')
         inc(lSliceInfoCount);
         if (lSliceInfoCount < 2) and (lRangeRA[kBitsPerVoxel].val < 1) then //PARvers3 has imagedepth in general header, only in image header for later versions
			lIsParVers3 := false;
         for lHdrPos := 1 to 26 do
			 lSliceHeaderRA[lHdrPos] := readparfloat;
		 //The next few values are in the same location for both PAR3 and PAR4
		 MinMaxTRange(lRangeRA[kSlice], round(lSliceHeaderRA[1]));
		 MinMaxTRange(lRangeRA[kEcho], round(lSliceHeaderRA[2]));
		 MinMaxTRange(lRangeRA[kDyn], round(lSliceHeaderRA[3]));
     if not lIsParVers42 then //if 4.2 then we will use combination of Cardiac and ASL for cardiac number
		  MinMaxTRange(lRangeRA[kCardiac], round(lSliceHeaderRA[4]));
		 MinMaxTRange(lRangeRA[kType], round(lSliceHeaderRA[5]));
		 MinMaxTRange(lRangeRA[kSequence], round(lSliceHeaderRA[6]));
		 MinMaxTRange(lRangeRA[kIndex], round(lSliceHeaderRA[7]));
		 if lIsParVers3 then begin //Read PAR3 data
			MinMaxTRange(lRangeRA[kRI], lSliceHeaderRA[8]);; //8=intercept in PAR3
			MinMaxTRange(lRangeRA[kRS],lSliceHeaderRA[9]); //9=slope in PAR3
			MinMaxTRange(lRangeRA[kSS],lSliceHeaderRA[10]);  //10=lcalibrated slope in PAR3 1393 - attempt to use calibrated values
			//MinMaxTRange(lRangeRA[kXmm],lSliceHeaderRA[23]); //23 PIXEL SPACING X  in PAR3
			//MinMaxTRange(lRangeRA[kYmm],lSliceHeaderRA[24]); //24 PIXEL SPACING Y IN PAR3
			MinMaxTRange(lRangeRA[kDynTime],(lSliceHeaderRA[26]));  //26= dyn_scan_begin_time in PAR3
		 end else begin  //not PAR: assume PAR4
			for lHdrPos := 27 to 32 do
				lSliceHeaderRA[lHdrPos] := readparfloat;
			MinMaxTRange(lRangeRA[kBitsPerVoxel],lSliceHeaderRA[8]);//8 BITS in PAR4
			MinMaxTRange(lRangeRA[kXdim], lSliceHeaderRA[10]); //10 XDim in PAR4
			MinMaxTRange(lRangeRA[kYdim], lSliceHeaderRA[11]); //11 YDim in PAR4
			MinMaxTRange(lRangeRA[kRI],lSliceHeaderRA[12]); //12=intercept in PAR4
			MinMaxTRange(lRangeRA[kRS],lSliceHeaderRA[13]); //13=lslope in PAR4
			MinMaxTRange(lRangeRA[kSS],lSliceHeaderRA[14]);  //14=lcalibrated slope in PAR4 1393 - attempt to use calibrated values
			MinMaxTRange(lRangeRA[kDynTime],(lSliceHeaderRA[32]));//32= dyn_scan_begin_time in PAR4
                        if lIsParVers41 then begin
                           for lHdrPos := 33 to 47 do
				lSliceHeaderRA[lHdrPos] := readparfloat;
                           if ({diff}lSliceHeaderRA[34]<> 0) and ({grad}lSliceHeaderRA[43]<> 0) then //DTI scan - treat as dynamics
                               MinMaxTRange(lRangeRA[kDyn], ({diff}lSliceHeaderRA[34]*100)+ ({gradient}lSliceHeaderRA[43]) );
                           if lIsParVers42 then begin
                              for lHdrPos := 48 to 49 do
				lSliceHeaderRA[lHdrPos] := readparfloat;
                              //fx(lSliceInfoCount,lSliceHeaderRA[46],lSliceHeaderRA[47],lSliceHeaderRA[48]);
                              lSliceOrient :=  round(lSliceHeaderRA[26]);
                              lDTIraDyn[lSliceInfoCount].bval := round(lSliceHeaderRA[34]);
                              //# diffusion (ap, fh, rl) (3*float) 46=AP=Y,47=FH=Z,48=RL=X
                              lDTIraDyn[lSliceInfoCount].v1 := lSliceHeaderRA[48];
                              lDTIraDyn[lSliceInfoCount].v2 := lSliceHeaderRA[46];
                              lDTIraDyn[lSliceInfoCount].v3 := lSliceHeaderRA[47];
                              MinMaxTRange(lRangeRA[kCardiac], ({cardiac}lSliceHeaderRA[49]*100)+ ({asl}lSliceHeaderRA[4]) );

                           end; //PAR42

                        end; //PAR41
		 end; //PAR4
		 if lSliceInfoCount < kMaxnSlices then begin
			lSliceSequenceRA^[lSliceInfoCount] :=  (round(lRangeRA[kSequence].val) shl 48)+(round(lRangeRA[kType].val) shl 40)+(round(lRangeRA[kCardiac].val) shl 32)+(round(lRangeRA[kEcho].val) shl 24)+(round(lRangeRA[kDyn].val) shl 10)+round(lRangeRA[kSlice].val);
			(*lRSx [lSliceInfoCount] := lRangeRA[kRS].Val;
			lRIx [lSliceInfoCount] := lRangeRA[kRI].val;
			lSSx [lSliceInfoCount] := lRangeRA[kSS].Val;
                        *)
                        // fx( lRangeRA[kType].val ,lRangeRA[kEcho].val);
                        PhilipsPrecise (lRangeRA[kRS].Val, lRangeRA[kRI].val,lRangeRA[kSS].Val,lSlopeRAx[lSliceInfoCount],lInterceptRAx[lSliceInfoCount],lPrecise);
			lSliceIndexRAx^[lSliceInfoCount]:= round(lRangeRA[kIndex].val);
		 end;
		 if (not lMatOrient) and (lSliceHeaderRA[1]=1) and (lSliceHeaderRA[2]=1) {and (lSliceHeaderRA[3]=1)} and (lSliceHeaderRA[4]=1) then begin
                        lMatOrient := true;
                        //first slice/echo/-dynamic/cardiac --- take slice position information from this slice...
			//par4 - 20,21,22 ; par3 16,17,18
			if lIsParVers3 then
				lOrient := round(lSliceHeaderRA[19])
			else
				lOrient := round(lSliceHeaderRA[26]);
                        //#  slice orientation ( TRA/SAG/COR )        (integer)
                          matx(lNHdr,lDicomData,lAngleA,lAngleB,lAngleC,lOffset1,lOffset2,lOffset3,
				lRangeRA[kXdim].Val,lRangeRA[kYdim].Val,lDicomData.XYZdim[3],
				lAPFOV,lFHFOV,lRLFOV,lOrient);
			//procedure mat(b,c,a,offa,offb,offc,lx,ly,lz,lxmm,lymm,lzmm: single);
		 end;
	end; //SliceInfo Line
  until (linPos >= lFileSz);//until done reading entire file...
  //describe generic DICOM parameters
  lDicomData.XYZdim[1] := round(lRangeRA[kXdim].Val);
  lDicomData.XYZdim[2] := round(lRangeRA[kYdim].Val);
  lDicomData.XYZdim[3] := 1+round(lRangeRA[kSlice].Max-lRangeRA[kSlice].Min);
  if (lSliceInfoCount mod lDicomData.XYZdim[3]) <> 0 then
	 dcmMsg('read_PAR2NII: Total number of slices not divisible by number of slices per volume. Reconstruction error?');
  if lDicomData.XYZdim[3] > 0 then
	 lDicomData.XYZdim[4] := lSliceInfoCount div lDicomData.XYZdim[3] //nVolumes = nSlices/nSlicePerVol
  else
	  lDicomData.XYZdim[4] := 1;
  if lOrient = 2 then begin //sagittal
	lDicomData.XYZmm[1] := lAPFOV /lDicomData.XYZdim[1];
	lDicomData.XYZmm[2] := lFHFOV / lDicomData.XYZdim[2];
	lDicomData.XYZmm[3] := lRLFOV /lDicomData.XYZdim[3];
  end else if lOrient = 3 then begin //coronal
	lDicomData.XYZmm[1] := lRLFOV /lDicomData.XYZdim[1];
	lDicomData.XYZmm[2] := lFHFOV / lDicomData.XYZdim[2];
	lDicomData.XYZmm[3] := lAPFOV /lDicomData.XYZdim[3];
  end else begin //axial
	lDicomData.XYZmm[1] := lRLFOV /lDicomData.XYZdim[1];
	lDicomData.XYZmm[2] := lAPFOV /lDicomData.XYZdim[2];
	lDicomData.XYZmm[3] := lFHFOV / lDicomData.XYZdim[3];
  end;
  //use smallest in plane resolution...
  SetLarger (lDicomData.XYZmm[1],lDicomData.XYZmm[2]);
  lDicomData.Allocbits_per_pixel :=  round(lRangeRA[kBitsPerVoxel].Val);
  lDicomData.IntenScale := lRangeRA[kRS].Val;
  lDicomData.IntenIntercept := lRangeRA[kRI].Val;
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
  if lSliceInfoCount < 1 then begin
     dcmMsg('No valid images found.') ;
     goto 333;
  end;
  //next: see if slices are in sequence
  lSlicesNotInSequence := false;
  if lSliceInfoCount > 1 then begin
     lMaxSlice := lSliceSequenceRA^[1];
	 lMaxIndex := lSliceIndexRAx^[1];
     lInc := 1;
     repeat
        inc(lInc);
        if lSliceSequenceRA^[lInc] < lMaxSlice then //not in sequence if image has lower slice order than previous image
           lSlicesNotInSequence := true
        else
           lMaxSlice := lSliceSequenceRA^[lInc];
        if lSliceIndexRAx^[lInc] < lMaxIndex then //not in sequence if image has lower slice index than previous image
           lSlicesNotInSequence := true
        else
           lMaxIndex := lSliceIndexRAx^[lInc];
     until (lInc = lSliceInfoCount) or (lSlicesNotInSequence);
  end; //at least 2 slices

  //Next: report any errors
       lErrorStr := '';
  if (lSlicesNotInSequence) and (not lReadOffsetTables) then
     lErrorStr := lErrorStr + ' Slices not saved sequentially [using MRIcro''s ''Philips PAR to Analyze'' command may solve this]'+kCR;
  if lSliceInfoCount > kMaxnSlices then
     lErrorStr := lErrorStr + ' Too many slices: >'+inttostr(kMaxnSlices)+kCR;
  if (lRangeRA[kBitsPerVoxel].min <> lRangeRA[kBitsPerVoxel].max) then  //5D file space+time+cardiac
     lErrorStr := lErrorStr + ' Differing bits per voxel'+kCR;
  //if (lRangeRA[kCardiac].min <> lRangeRA[kCardiac].max) then  //5D file space+time+cardiac
  //   lErrorStr := lErrorStr + 'Multiple cardiac timepoints'+kCR;
  //if (lRangeRA[kEcho].min <> lRangeRA[kEcho].max) then  //5D file space+time+echo
  //   lErrorStr := lErrorStr + 'Multiple echo timepoints'+kCR;
  if (lRangeRA[kSliceThick].min <> lRangeRA[kSliceThick].max) or (lRangeRA[kSliceGap].min <> lRangeRA[kSliceGap].max)
    or (lRangeRA[kXdim].min <> lRangeRA[kXdim].max) or (lRangeRA[kYDim].min <> lRangeRA[kYDim].max)
    or (lRangeRA[kXmm].min <> lRangeRA[kXmm].max) or (lRangeRA[kYmm].min <> lRangeRA[kYmm].max) then
     lErrorStr := lErrorStr + ' Multiple/varying slice dimensions'+kCR;
  //if any errors were encountered, report them....

  if lErrorStr <> '' then begin
      dcmMsg('read_PAR2NII: This software can not convert this Philips data:'+kCR+lErrorStr);
      goto 333;
  end;
       //Next sort image indexes here...
  if (lSliceInfoCount > 1) and(lSlicesNotInSequence) and ( lReadOffsetTables) then begin //sort image order...
     //ShellSort (first, last: integer; var lPositionRA, lIndexLoRA,lIndexHiRA: LongintP; var lRepeatedValues: boolean)
     GetMem (lOffset_pos_table, lSliceInfoCount*sizeof(int64));
     for lInc := 1 to  lSliceInfoCount do
         lOffset_pos_table^[lInc] := lInc;
	 ShellSortItems (1, lSliceInfoCount,lOffset_pos_table,lSliceSequenceRA, lRepeatedValues);
    (* if lRepeatedValues then begin
         dcmMsg('read_PAR2NII: fatal error, slices do not appear to have unique indexes [multiple copies of same slice]');
         FreeMem (lOffset_pos_table);
         goto 333;
     end; *)
     lOffsetTableEntries := lSliceInfoCount;
  end; //sort image order...


       //Next, generate list of scale slope
  (*lOffsetTablesRequired := false;
  if (lSliceInfoCount > 1) and ( (lRangeRA[kSS].min <> lRangeRA[kSS].max)
    or  (lRangeRA[kRS].min <> lRangeRA[kRS].max)
    or (lRangeRA[kRI].min <> lRangeRA[kRI].max)) then
       lOffsetTablesRequired := true;
    *)
   lDicomData.IntenScale :=  lSlopeRAx[1];
   lDicomData.IntenIntercept := lInterceptRAx[1];
  //PhilipsPrecise (lRSx[lInc], lRIx[lInc],lSSx[lInc], lDicomData.IntenScale,lDicomData.IntenIntercept);

  //if lOffsetTablesRequired then begin
  //    dcmMsg('Image saved as 32-bit data: varying intensity scaling factors or complicated Pixel to Precise transform');

  if  (lRangeRA[kSS].min = lRangeRA[kSS].max)
    and (lRangeRA[kRS].min  = lRangeRA[kRS].max)
    and (lRangeRA[kRI].min  = lRangeRA[kRI].max) then
          lRescaleEntries := 0
   else begin
       lRescaleEntries := lSliceInfoCount;
      getmem (lSlopeRA, lRescaleEntries*sizeof(single));
      getmem (lInterceptRA, lRescaleEntries*sizeof(single));
      if  lOffsetTableEntries = lSliceInfoCount then begin //need to sort slices
         setlength(lDTIraDynUnSorted,lSliceInfoCount+1);//+1 since indexed from zero
         for lInc := 1 to lSliceInfoCount do
             lDTIraDynUnSorted[lInc] := lDTIraDyn[lInc];
          for lInc := 1 to lSliceInfoCount do begin
              lSlopeRA^[lInc] := lSlopeRAx[lOffset_pos_table^[lInc]];
              lInterceptRA^[lInc] := lInterceptRAx[lOffset_pos_table^[lInc]];
              lDTIraDyn[lInc] := lDTIraDynUnSorted[lOffset_pos_table^[lInc]];
          end;
      end else begin //if sorted, else unsorted
          for lInc := 1 to lSliceInfoCount do begin
              lSlopeRA^[lInc] := lSlopeRAx[lInc];
              lInterceptRA^[lInc] := lInterceptRAx[lInc];
          end;
      end; //slices sorted
  end;//read scale factors
  //Next: now adjust Offsets to point to byte offset instead of slice number
  lSliceSz := lDicomData.XYZdim[1]*lDicomData.XYZdim[2]*(lDicomData.Allocbits_per_pixel div 8);
  if lOffsetTableEntries = lSliceInfoCount then
          for lInc := 1 to lSliceInfoCount do
              lOffset_pos_table^[lInc] := lSliceSz * (lSliceIndexRAx^[lOffset_pos_table^[lInc]]);
  //report if 5D/6D/7D file is being saved as 4D
  if (lRangeRA[kCardiac].min <> lRangeRA[kCardiac].max)
    or (lRangeRA[kEcho].min <> lRangeRA[kEcho].max)   //5D file space+time+echo
    or (lRangeRA[kType].min <> lRangeRA[kType].max)   //5D file space+time+echo
    or (lRangeRA[kSequence].min <> lRangeRA[kSequence].max) then begin//5D file space+time+echo
      dcmMsg('Warning: note that this image has more than 4 dimensions (multiple Cardiac/Echo/Type/Sequence)');
      dcmMsg('Cardiac min..max '+floattostr(lRangeRA[kCardiac].min)+'..'+floattostr(lRangeRA[kCardiac].max) );
      dcmMsg('Echo min..max '+floattostr(lRangeRA[kEcho].min)+'..'+floattostr(lRangeRA[kEcho].max) );
      dcmMsg('Type min..max '+floattostr(lRangeRA[kType].min)+'..'+floattostr(lRangeRA[kType].max) );
      dcmMsg('Sequence min..max '+floattostr(lRangeRA[kSequence].min)+'..'+floattostr(lRangeRA[kSequence].max) );
      
    end;


  //if we get here, the Image Format is OK
  lImageFormatOK := true;
  lFileName := changefileextX(lFilename,'.rec'); //for Linux: case sensitive extension search '.rec' <> '.REC'
  //next save dti b-values
  if (lIsParVers42) and (lDicomData.XYZdim[4] <= kMaxDTIDir) and (lDicomData.XYZdim[4] > 1) then begin

     for lInc := 1 to lDicomData.XYZdim[4] do
         lDTIra[lInc] := lDTIraDyn[(lDicomData.XYZdim[3]*(lInc-1))+1];
     //see if bval or bvec varies...
     lInc := 1;
     while (lInc <= lDicomData.XYZdim[4]) and (lDTIra[lInc].bval = lDTIra[1].bval) and (lDTIra[lInc].v1 = lDTIra[1].v1) do
           inc(lInc);
     //warn if untested orientation
     if (lInc <= lDicomData.XYZdim[4]) and (lOrient <> 1) then
        dcmMsg('WARNING: DTI vectors only tested for transsaxially oriented Philips data: bvec values may be inaccurate!');

     (*if (lInc <= lDicomData.XYZdim[4]) then begin//bvec or bval vary
        //#  slice orientation ( TRA/SAG/COR )        (integer)
        if lOrient <> 1 then
           dcmMsg('WARNING: DTI vectors only tested for transsaxially oriented Philips data: bvec values may be inaccurate!');
        PhilipsCorrectBvecs(lDICOMdata, lDTIra, lDicomData.XYZdim[4]);
        DTItextfiles (lFileName, lDTIra, lDicomData.XYZdim[4]);
     end;  *)
end else begin
    //not DTI data - provide empty bvec/bval file
     lDTIra[1].bval := 0;
     lDTIra[1].v1 := 0;
     lDTIra[1].v2 := 0;
     lDTIra[1].v3 := 0;
     for lInc := 1 to lDicomData.XYZdim[4] do
         lDTIra[lInc] := lDTIra[1];
end;
333: //abort clause: skips lHdrOK and lImageFormatOK
 //next: free dynamically allocated memory
 FreeMem( lCharRA);
 FreeMem (lSliceSequenceRA);
 Freemem(lSliceIndexRAx);
end;


(*function StudySecSince2KStr (lInSec: integer): string;
var
  days,secs,Y,M,D,H,Min,S, l,n,i,j: integer;
begin
	 result := 'DateNA';//bogus
	 days := (lInSec div 86400)+2451547;//+2451547 as we convert to julian
	 //dcmMsg(inttostr(days));
	 //next convert Y,M,D
	 l := days + 68569;
	 n := trunc(( 4 * l ) / 146097);
	 l := trunc(l - ( 146097 * n + 3 ) / 4);
	 i := trunc(( 4000 * ( l + 1 ) ) / 1461001);
	 l := trunc(l - ( 1461 * i ) / 4 + 31);
	 j := trunc(( 80 * l ) / 2447 );
	 d := trunc(l - ( 2447 * j ) / 80);
	 l := trunc(j / 11);
	 m := j + 2 - ( 12 * l );
	 y := 100 * ( n - 49 ) + i + l;
	 //next convert H,Min,Sec
	 if lInSec < 0 then begin//date prior to 2000 -saved as negative
		 secs := (lInSec - ( (lInSec div 86400)*86400)+86400) mod 86400
	 end else
		secs := lInSec mod 86400; //value 0..86399
	 S := secs mod 60;
	 Min := (secs div 60) mod 60;
	 H := (secs div 3600)+1;
	 result := PadStr (Y, 4)+ PadStr (M, 2)+PadStr (D, 2)+'_'+PadStr (H, 2)+ PadStr (Min, 2)+PadStr (S, 2);
end;*)

function UniqueFileName (var lInStr: string): boolean;
var
  lInc: integer;
  lPathWName,lExt: string;
begin
    result := true;
    if not Fileexists(lInStr) then exit;
	ExtractFileParts(lInStr,lPathWName,lExt);
    lInc := ord('A');
    while (lInc <= ord('Z')) and ( Fileexists(lPathWName+chr(lInc)+lExt))  do
      inc(lInc);
    if lInc > ord('Z') then
      result := false
    else
      lInStr := lPathWName+chr(lInc)+lExt;
end;

type
TiVec = array[1..3] of integer;
const
     kSliceOrientTra = 1;
     kSliceOrientSag = 2;
     kSliceOrientCor = 3;

procedure nifti_mat33_reorder_cols(var  m: TMatrix; v: TiVec ) ;
var
  inMat : TMatrix;
  r,c: integer;
begin
    // matlab equivalent ret = m(:, v); where v is 1,2,3 [INDEXED FROM ONE!!!!]
    InMat := m;
    for r := 1 to 3 do
        for c := 1 to 3 do
            m.matrix[r,c] := InMat.matrix[r,v[c]];
//v[c]

end; //nifti_mat33_reorder_cols()

procedure computeOrient(var d: dicomdata; lSliceOrients: integer);
var
  ang1, ang2,ang3: double;
  ca, sa: TVector;
  rx,ry,rz,R: TMatrix;
  row,col: integer;
  ixyz: TiVec;
begin
 //see Xiangrui Li 's dicm2nii (also BSD license)
    // http://www.mathworks.com/matlabcentral/fileexchange/42997-dicom-to-nifti-converter
    // Rotation order and signs are figured out by try and err, not 100% sure
    ang1 := d.AngulationRL;
    ang2 := d.AngulationAP;
    ang3 := d.AngulationFH;
    ca := Vector3D(cos(DegToRad(ang1)),cos(DegToRad(ang2)),cos(DegToRad(ang3)));
    sa := Vector3D(sin(DegToRad(ang1)),sin(DegToRad(ang2)),sin(DegToRad(ang3)));

    rx := Matrix2D(1.0, 0.0, 0.0, 0.0, ca.vector[1], -sa.vector[1], 0.0, sa.vector[1], ca.vector[1]);
    ry := Matrix2D(ca.vector[2], 0.0, sa.vector[2], 0.0, 1.0, 0.0, -sa.vector[2], 0.0, ca.vector[2]);
    rz := Matrix2D(ca.vector[3], -sa.vector[3], 0.0, sa.vector[3], ca.vector[3], 0.0, 0.0, 0.0, 1.0);
    R := MultiplyMatrices(rx,ry );
    R := MultiplyMatrices(R,rz );
    //ReportMatrix('mult',R);
    ixyz[1]:= 1; ixyz[2] := 2; ixyz[3] := 3; //axial slice order ijk = xyz
    if (lSliceOrients = kSliceOrientSag) then begin//(d.sliceOrient == kSliceOrientSag) {
            ixyz[1]:= 2; ixyz[2] := 3; ixyz[3] := 1;
            for row := 1 to 3 do
                for col := 1 to 3 do begin
                    if (col <> 2) then
                       R.matrix[row,col] :=  -R.matrix[row,col]; //invert first and final columns
                end;
            nifti_mat33_reorder_cols(R,ixyz);
    end else if (lSliceOrients = kSliceOrientCor) then begin //(d.sliceOrient == kSliceOrientCor) {
             ixyz[1]:= 1; ixyz[2] := 3; ixyz[3] := 2;
             for row := 1 to 3 do
                 R.matrix[row,3] :=  -R.matrix[row,3];
             nifti_mat33_reorder_cols(R,ixyz);
    end;
    d.orient[1] := R.matrix[1,1]; d.orient[2] := R.matrix[2,1]; d.orient[3] := R.matrix[3,1];
    d.orient[4] := R.matrix[1,2]; d.orient[5] := R.matrix[2,2]; d.orient[6] := R.matrix[3,2];
    //ReportMatrix('x',R);
    //dcmmsg(Format('v = %g %g %g %g %g %g', [d.Orient[1], d.Orient[2], d.Orient[3], d.Orient[4], d.Orient[5], d.Orient[6] ])) ;
 end;

function ConvertPhilipsPARtoAnalyze (var lInFilename, lOutDir: string; var lPrefs: TPrefs): boolean;
label
	 678;
var
   //lVaryingScaleFactorsTableEntries,
   lEnd, lLines,lColBytes,lRows,lRowsdiv2,lSwap,lInc,l4Doffset,lcurrent4Dvol,lnum4Ddatasets,lSlicePixelsx, lnSlicesx,
   lSliceSzOutx,lSLiceSzx,lRescaleEntries, lPos,lOffsetTableEntries,lSliceOrient: longint;
   lOutF,lInF: File;
   lNHdr,lAHdr: TNIFTIhdr;
   lP,lBuffer : Bytep;
   lFileName,lRECFilename,lOutImgName,lOutHdrName,lOutHdrNameGz,lDynStr,lOutDirPath: String;
   lDICOMdata: dicomdata;
   lScaleFactorVariesInThis4DVolume,lAbort,lHdrOK, lImageFormatOK: boolean;
   lSlopeRA,lInterceptRA,
   lSingleBuffer: Singlep;
   lOffset_pos_table: LongIntp;
   lDTIra: TDTIra;
begin
     result := false;
     lSliceOrient := kSliceOrientTra;
     lFileName := lInFilename;
  if (lOutDir = '') then
	lOutDirPath := ExtractFilePath(lFileName)//ExtractFileDirWithPathDelim(lFilename)//ExtractFilePath(lFileName)
  else if not direxists(lOutDir) then begin
	dcmMsg('Unable to find output directory '+lOutDir);
	lOutDirPath := ExtractFilePath(lFileName)
  end else
	lOutDirPath := lOutDir;
  if (length(lOutDirPath) > 0) and (lOutDirPath[length(lOutDirPath)] <> pathdelim) then
    lOutDirPath := lOutDirPath + pathdelim;
	lAbort := false;
	lRecFilename :=ChangeFileExt(lFileName,'.rec');
        dcmMsg('input name '+ lInFilename);
        dcmMsg('input REC name '+lRecFilename);
	//Apr08 problems with filenames with . in them lRecFilename :=ExtractFilePath(lFileName)+ParseFileName(ExtractFileName(lFileName))+'.rec';
	if not fileexists(lRecFilename) then //might be Linux: case sensitive extensions
	       lRecFilename :=ChangeFileExt(lFileName,'.REC');
	       //	lRecFilename :=ExtractFilePath(lFileName)+ParseFileName(ExtractFileName(lFileName))+'.REC';
	if not fileexists(lRecFilename) then
		dcmMsg('Unable to find REC image data file named '+lRecFileName)
	else if fileexists(lRecFilename) and fileexists(lFilename) then begin //convert
		read_par2NII(lNHdr,lDICOMdata,lHdrOK,lImageFormatOK,lPRefs.PhilipsPrecise, lDynStr,lFileName,true,lOffset_pos_table,lOffsetTableEntries,lRescaleEntries, lSlopeRA,lInterceptRA,lnum4Ddatasets,lSliceOrient,lDTIra);
	 if (lnum4Ddatasets > 1) and ((lDicomData.XYZdim[4] mod lnum4Ddatasets) = 0) then  //break 5D files into separate 4D files
		lDicomData.XYZdim[4] := lDicomData.XYZdim[4] div lnum4Ddatasets
	 else
		 lnum4Ddatasets := 1;
			lRows := lDicomData.XYZdim[2];
			lRowsdiv2 := lRows div 2;
			lColBytes := lDicomData.XYZdim[1]*(lDicomData.Allocbits_per_pixel div 8);
			lSlicePixelsx := (lDicomData.XYZdim[1]*lDicomData.XYZdim[2]);
			lSliceSzx := lSlicePixelsx*(lDicomData.Allocbits_per_pixel div 8);
  lnSlicesx := lDicomData.XYZdim[3] * lDicomData.XYZdim[4];
  lcurrent4Dvol := 0;
	 l4DOffset := 0;
  //  exit;    //crucial critical test exit
  if lHdrOK then begin
     repeat //for each 4D volume
            inc(lcurrent4Dvol);
	    lOutHdrName :=lOutDirPath+{Pathdelim+}OutputFilename(lRecFilename,lDicomData,lPrefs);//Pathdelim 11/2007
            if lnum4Ddatasets > 1 then begin
	       l4DOffset :=  (lcurrent4Dvol-1)* lnSlicesx;
	       lOutHdrName :=(lOutHdrName)+'x'+inttostr(lcurrent4Dvol)+'.hdr'
	    end else
	        lOutHdrName :=(lOutHdrName)+'.hdr';
			lOutImgName :=changefileext(lOutHdrName,'.img');
			if lPrefs.SingleNIIFile then begin
				lOutHdrName :=  changefileext(lOutHdrName,'.nii');
				lOutImgName := lOutHdrName;
			end;
	if (lPrefs.SingleNIIFile) and (lPrefs.GZip) then begin
		lOutHdrNameGz := lOutHdrName+'.gz';
		if (not UniqueFileName(lOutHdrNameGz))  then begin
			dcmMsg('File already exists '+lOutImgName+' '+lOutHdrNameGz);
			exit;
		end;

		//we now need to remove the .gz - not that unique filename may have appended postfix, e.g. filename.nii.gz -> filenameA.nii.gz
		//StripGZExt(lOutHdrName);
		lOutImgName := lOutHdrName;
	end else begin
		if (not UniqueFileName(lOutHdrName)) or (not UniqueFileName(lOutImgName)) then begin
			dcmMsg('File already exists '+lOutImgName+' '+lOutHdrName);
			exit;
		end;
	end;
	dcmMsg(lFileName+' -> '+ lOutImgName);
        //exit; //trap
        if (lDicomData.XYZdim[4] > 5) then begin //if 4D: save DTI data
           lInc := 1;
           while (lInc <= lDicomData.XYZdim[4]) and (lDTIra[lInc].bval = lDTIra[1].bval) and (lDTIra[lInc].v1 = lDTIra[1].v1) do
                 inc(lInc);
           lEnd := 0;
           if (lInc <= lDicomData.XYZdim[4]) then begin//bvec or bval vary
              for linc := 1 to lDicomData.XYZdim[4] do
                  if (lDTIra[lInc].bval = 0) or (lDTIra[lInc].v1 <> 0) or (lDTIra[lInc].v2 <> 0) or (lDTIra[lInc].v3 <> 0) then
                         lEnd := lInc;
              if (lEnd = (lDicomData.XYZdim[4]-1)) then begin
                 dcmMsg('Warning: final volume is computed ADC and will not be converted (as it would disrupt processing). You can re-create a better ADC image after eddy current correction.');
                 lDicomData.XYZdim[4] := lEnd;
              end;
              computeOrient(lDICOMData,lSliceOrient);
              siemensPhilipsCorrectBvecs(lDICOMdata, lDTIra, lDicomData.XYZdim[4], false);
              DTItextfiles (lOutImgName, lDTIra, lDicomData.XYZdim[4]);
           end;
        end; //if 4D: save DTI data

              {$IFDEF LINUX}
						//perhaps the file is .REC, not .rec
                       if (lSliceSzx * lnSLicesx) > FSize(lFileName)   then
								lRecFilename := changefileext(lFileName,'.REC');
						{$ENDIF}
			if (lSliceSzx * lnSLicesx) > FSize(lRecFilename)   then begin

			   dcmMsg('Conversion error: the REC file '+lRecFilename+ ' is not as large as described by the PAR file X*Y*Z*T*BytesPerPixel='
                                           + inttostr(lDicomData.XYZdim[1])+'*'+inttostr(lDicomData.XYZdim[2])+'*'+inttostr(lDicomData.XYZdim[3])+'*'+inttostr(lDicomData.XYZdim[4])+'*'+inttostr(lDicomData.Allocbits_per_pixel div 8)
                                           +' = '+ inttostr(lSliceSzx* lnSLicesx)+' <>  '+inttostr(FSize(lFileName)) );
						{$IFDEF LINUX}
			   dcmMsg(' Suggestion: in UNIX .REC and .rec are different files - check file extension'  );
						{$ENDIF}
			   lAbort := true;
			end else if ((sizeof(TNIFTIhdr)+(lSliceSzx*lnSlicesx))> DiskFreeEx(lOutImgName)) then begin
			   dcmMsg('There is not enough free space on the destination disk to save the converted image. '+kCR+
			   lOutImgName+ kCR+' Bytes Required: '+inttostr(sizeof(TNIFTIhdr)+(lSliceSzx*lnSlicesx)) );
			   lAbort := true;
			end else if fileexists(lOutHdrName) or fileexists(lOutImgName) then
			   dcmMsg('Unable to convert images:  file already exists named: '+lOutHdrName)
			else if  (not lHdrOK)   then
                             dcmMsg('Problem with header...')
                        else if (not lImageFormatOK) then
                             dcmMsg('Problem with image...')

                        else if (lHdrOK) and (lImageFormatOK) and (lDicomData.XYZdim[3] > 0) and (lSliceSzx > 0) then begin
			   DICOM2AnzHdr(lAHdr,lPrefs.Anonymize,lFilename,lDICOMdata);
                           lSliceSzOutx := lSliceSzx;
                           lScaleFactorVariesInThis4DVolume := false;
                           //check if 4D scale slope changes for this 4D dataset...
                           if lRescaleEntries > 0 then begin
                              lAHdr.scl_slope := lSlopeRA^[l4DOffset+1];
                              lAHdr.scl_inter := lInterceptRA^[l4DOffset+1];

                              if lRescaleEntries > 0 then begin
                                for lInc := 1 to lnSlicesx do begin
                                  if lAHdr.scl_slope <> lSlopeRA^[l4DOffset+lInc] then
                                     lScaleFactorVariesInThis4DVolume := true;
                                  if lAHdr.scl_inter <> lInterceptRA^[l4DOffset+lInc] then
                                     lScaleFactorVariesInThis4DVolume := true;
                                end;
                              end;
                              if lScaleFactorVariesInThis4DVolume then begin
				  lAHdr.bitpix := 32;
				  lAHdr.DataType := 16;
				  lAHdr.scl_slope := 1;
				  lAHdr.scl_inter := 0;
                                  lSliceSzOutx := lSlicePixelsx*sizeof(single);
                              end;
                           end;
                           //end of 4D scale factor variation...
                           lAHdr.sform_code := lNHdr.sform_code;
                           lAHdr.srow_x[0] := lNHdr.srow_x[0];
                           lAHdr.srow_x[1] := lNHdr.srow_x[1];
                           lAHdr.srow_x[2] := lNHdr.srow_x[2];
                           lAHdr.srow_x[3] := lNHdr.srow_x[3];
                           lAHdr.srow_y[0] := lNHdr.srow_y[0];
                           lAHdr.srow_y[1] := lNHdr.srow_y[1];
                           lAHdr.srow_y[2] := lNHdr.srow_y[2];
                           lAHdr.srow_y[3] := lNHdr.srow_y[3];
                           lAHdr.srow_z[0] := lNHdr.srow_z[0];
                           lAHdr.srow_z[1] := lNHdr.srow_z[1];
                           lAHdr.srow_z[2] := lNHdr.srow_z[2];
                           lAHdr.srow_z[3] := lNHdr.srow_z[3];
                           lAHdr.qform_code := lNHdr.qform_code;
   lAHdr.quatern_b := lNHdr.quatern_b;
   lAHdr.quatern_c := lNHdr.quatern_c;
   lAHdr.quatern_d := lNHdr.quatern_d;
   lAHdr.qoffset_x := lNHdr.qoffset_x;
   lAHdr.qoffset_y := lNHdr.qoffset_y;
   lAHdr.qoffset_z := lNHdr.qoffset_z;
   lAHdr.pixdim[0] := lNHdr.pixdim[0];
   {$IFDEF ENDIAN_BIG}
   if SaveHdr (lOutHdrName,lAHdr, true,lPrefs.SPM2) then begin
   {$ELSE}
   if SaveHdr (lOutHdrName,lAHdr, false,lPrefs.SPM2) then begin
   {$ENDIF}
				  Filemode := 2;//1385: read-write
				  AssignFile(lOutF, lOutImgName);
				  if lPrefs.SingleNIIFile then begin
					Reset(lOutF,1);
					Seek(lOutF,352);
                                        lAHdr.vox_offset := 352;
				  end else
					Rewrite(lOutF,1); //setting block size only about 12% speed increase: HD cache must help
				  Filemode := 0;//1385: read-only
				  AssignFile(lInF, lRecFilename);
				  Reset(lInF,lSliceSzx);
				  GetMem(lBuffer,lSliceSzx);
				  if lScaleFactorVariesInThis4DVolume then
					 GetMem(lSingleBuffer,lSliceSzOutx);
				  for lInc := 1 to lnSlicesx do begin
					  //application.ProcessMessages;
					  if lOffsetTableEntries > 1 then //data not contiguous
						 Seek(lInF,  (lOffset_pos_table^[lInc+l4DOffset] div lSliceSzx))
					  else
						  Seek(lInF, (l4DOffset+lInc-1));
					  Filemode := 0;  //ReadONly
					  BlockRead(lInF, lBuffer^, 1);
					  Filemode := 2;  //read and write
					  GetMem ( lP ,  lColBytes);
					  for lLines := 1 to lRowsdiv2 do begin
						  Move(lBuffer[((lLines-1)*lColBytes)+1],lP^,lColBytes);
						  Move(lBuffer[(( lRows-lLines)*lColBytes)+1],lBuffer[((lLines-1)*lColBytes)+1],lColBytes);
						  Move(lP^,lBuffer[(( lRows-lLines)*lColBytes)+1],lColBytes);
					  end;
					  FreeMem(lP);
					  if lScaleFactorVariesInThis4DVolume  then begin
						  if lDicomData.Allocbits_per_pixel = 8 then begin
							 for lLines := 1 to lSlicePixelsx do
							  lSingleBuffer^[lLines] := lBuffer^[lLines]*lSlopeRA^[l4DOffset+lInc]+lInterceptRA^[l4DOffset+lInc];
						  end else if lDicomData.Allocbits_per_pixel = 16 then begin
							 lPos := 1;
							 for lLines := 1 to lSlicePixelsx do begin
							  lSingleBuffer^[lLines] := lBuffer^[lLines]*lSlopeRA^[l4DOffset+lInc]+lInterceptRA^[l4DOffset+lInc];
							  //lSingleBuffer^[lLines] := lBuffer^[lPos]*lRS+lRI;
							  inc(lPos,2);
							 end;
						  end else
							  dcmMsg('Error: can only convert 8/16bit PAR/REC files with varying scaling values.');
						  BlockWrite(lOutF, lSingleBuffer^, lSliceSzOutx);
					  end else
						  BlockWrite(lOutF, lBuffer^, lSliceSzOutx);
				  end;
				  CloseFile(lOutF);
				  CloseFile(lInF);
				  freemem(lBuffer);
				  if lScaleFactorVariesInThis4DVolume then
					 FreeMem(lSingleBuffer);
			   end else
				   lAbort := true; //save header failed: probably read only disk, or less than 348 bytes: do not force inidividual to see message for each file
                //if (lPrefs.StartClip > 0) or (lPrefs.EndClip > 0) then
                //   Clip4D(lOutHdrName, lAHdr, false,lPrefs.SPM2,lPrefs.SingleNIIFile,lPrefs.GZip,true, lPrefs.StartClip,lPrefs.EndClip);
                (*if (not lPrefs.FourD) and (lAHdr.dim[4] > 1) then begin
                   Convert4Dto3D(lOutImgName, lAHdr, false,lPrefs.SPM2,lPrefs.SingleNIIFile, lPrefs.Gzip);
                end else*)
                if lPrefs.SingleNIIFile and lPrefs.Gzip then
                   GZipFile(lOutImgName,lOutImgName+'.gz',true)
                else if ((not lPrefs.FourD) and (lAHdr.dim[4] > 1)) {or ((lPrefs.SingleNIIFile) and (lPrefs.Gzip))} then
                   if ChangeNIfTISubformat(lOutImgName, lAHdr,lPrefs) then begin
                      deleteFile(lOutImgName);//11/2007 : delete original
                   end;
               end; //file OK


	 until (lcurrent4Dvol>=lnum4Ddatasets) or (lAbort); //for each 4D dataset
  end; //lHdrOK
	 if lOffsetTableEntries > 0 then begin
			   freemem (lOffset_pos_table);
			   lOffsetTableEntries := 0;
	 end; //slice offset table filled
	 if lRescaleEntries > 0 then begin
			   freemem ( lSlopeRA);
			   freemem (lInterceptRA);
	 end; //slice offset table filled
end; //REC exists
		 if lAbort then goto 678;
   result := true;
   ExitCode := 0;
   if (lDicomData.XYZdim[2] > lPrefs.MinReorientMatrix) and (lDicomData.XYZdim[1] > lPrefs.MinReorientMatrix) and (lAHdr.dim[4] < 2) then begin
           lOutImgName := Reorient(lOutImgName,lAHdr,lPrefs,false,false);
           if (lOutImgName <> '') {success}and (lDicomData.TE < 25) then //T1 image
              CropNIfTI(lOutImgName,lPrefs);
   end;

	 678:
	 Filemode := 2; //1385
end;

function LoadFileListPARREC  (var lInFilename, lOutDir: string; var lPrefs: TPrefs): boolean;
var
 lFilePath,lMaskExt,lPARname,lOutDirName: String;
 lError: boolean;
 lSearchRec: TSearchRec;
begin
     lOutDirName := lOutDir;
     if (lPrefs.OutDirMode <> kOutDirModeInput) and (DirExists(lPrefs.OutDir)) then begin
        //For kOutDirModePrompt one should set OutDir before getting here
        //This is required so recursive searches do not repetitively prompt the user...
        lOutDirName := lPrefs.OutDir;
     end; //1/2010
     lOutDirName := ExtractFileDirWithPathDelim(lOutDirName);
     if not DirWritePermission(lOutDirName) then begin // <- tested with Unix
        dcmMsg('Error: output directory is read-only: '+lOutDirName);
        exit;
     end;
   lError := false;
   if lPrefs.EveryFile = true then begin
      lFilePath := ExtractFileDirWithPathDelim(lInFilename);
      {$IFDEF Linux}
      lMaskExt := '*';
      {$ELSE}
      lMaskExt := '*.*';
      {$ENDIF}
      Filemode := 0; //readonly
      if FindFirst(lFilePath{+PathDelim}+lMaskExt, faAnyFile-faSysFile-faDirectory, lSearchRec) = 0 then begin
         repeat
	       if UpCaseExt(lSearchRec.Name) = '.PAR' then begin
				lPARname := (lFilePath+lSearchRec.Name);
				result := ConvertPhilipsPARtoAnalyze(lPARname, lOutDirName, lPrefs);
                                if not result then
                                   lError := true;
			end;
              until (FindNext(lSearchRec) <> 0);
         end else
               dcmMsg( 'Error: Unable to find PAR files in '+lFilePath{+PathDelim}+lMaskExt); //some files found
	 SysUtils.FindClose(lSearchRec);
	 Filemode := 2;
   end else begin
      if FileExists(lInFilename) then begin
	 lError := ConvertPhilipsPARtoAnalyze(lInFilename, lOutDirName, lPrefs);
      end else
	 dcmMsg( 'Unable to find PAR file named '+lInFilename); //some files found
   end;
   if lError then
      result := false //at least one error
   else
       result := true;
end;

end.

