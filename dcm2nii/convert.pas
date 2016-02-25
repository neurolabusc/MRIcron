unit convert;
{$H+}
interface
uses
{$IFNDEF UNIX}
Windows,
{$ENDIF}
{$IFDEF FPC}
gzio2,
{$ELSE}

{$ENDIF}
filename,define_types,classes,SysUtils,dicom,dicomtypes, nifti_types,
niftiutil,GraphicsMathLibrary,  userdir,csaread,dialogs_msg, math,
nii_4dto3d,nii_orient,nii_crop,prefs,lsjpeg, bvec;
function Dicom2NII(var lDICOMra: TDICOMrap; var l4dDTIra: TDTIra; lFirstDICOM, lLastDICOM: integer; var lOutDirOrig: string; var lPrefs: TPrefs; lVols: integer): boolean;
implementation
uses
sortdicom,dialogsx;

function getDeterminant(r: TMatrix): double;
var
r11,r12,r13,r21,r22,r23,r31,r32,r33: double;
begin
	   r11 := r.matrix[1,1];//[0][0];
	   r12 := r.matrix[1,2];//r[0][1];
	   r13 := r.matrix[1,3];//r[0][2];
	   r21 := r.matrix[2,1];//[0][0];
	   r22 := r.matrix[2,2];//r[0][1];
	   r23 := r.matrix[2,3];//r[0][2];
	   r31 := r.matrix[3,1];//[0][0];
	   r32 := r.matrix[3,2];//r[0][1];
	   r33 := r.matrix[3,3];//r[0][2];
	  result := r11*r22*r33 - r11*r32*r23 - r21*r12*r33 +
		 r21*r32*r13 + r31*r12*r23 - r31*r22*r13;
end;

function RealToStr(lR: double ; lDec: integer): string;
begin
	 RealTOStr := FloatToStrF(lR, ffFixed,7,lDec);
end;

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

FUNCTION Diag2D (CONST m1,m2,m3:  DOUBLE):  TMatrix;
  BEGIN
	WITH RESULT DO
	BEGIN
	  matrix[1,1] := m1; matrix[1,2] := 0;
	  matrix[1,3] := 0;

	  matrix[2,1] := 0; matrix[2,2] := m2;
	  matrix[2,3] := 0;

	  matrix[3,1] := 0; matrix[3,2] := 0;
	  matrix[3,3] := m3;
	  size := size2D
	END
  END {Diag2D};
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

(*procedure AdjMosaic (var Q: TMatrix; var lDicomData: DicomData);
//Changes suggested by Antonin Skoch" <ansk@medicon.cz<mailto:ansk@medicon.cz>>
// September 23, 2011 10:38:05 AM
var
   lFactorX: double;
   lFactorY: double;
begin
   lFactorX := (lDicomData.XYZdim[1] -(lDicomData.XYZdim[1]/lDicomData.SiemensMosaicX)   )/2;
   lFactorY := (lDicomData.XYZdim[2] -(lDicomData.XYZdim[2]/lDicomData.SiemensMosaicY)   )/2;
   Q.matrix[1,4] :=(Q.matrix[1,1]*lFactorX)+(Q.matrix[1,2]*lFactorY)+Q.matrix[1,4];
   Q.matrix[2,4] :=(Q.matrix[2,1]*lFactorX)+(Q.matrix[2,2]*lFactorY)+Q.matrix[2,4];
   Q.matrix[3,4] :=(Q.matrix[3,1]*lFactorX)+(Q.matrix[3,2]*lFactorY)+Q.matrix[3,4];
end; *)


(*procedure get_numaris4_val(lFilename,lTagName1,lTagName2: string; var lnmosaic: integer; var lv1,lv2,lv3:single); // spm_dicom_convert
label 121;
const
     kMaxHdrSz = 24000;
var
   lHdr  : array [1..kMaxHdrSz] of Char;
   lTagName,lStr: string;
   lLoop,lFileSz,lFilePos,lTagLen,lSubLoop,lnSubloop: integer;
   lFile: file;
function IsTag: boolean;
var
   lTagPos: integer;
begin
    result := false;
    for lTagPos := 1 to lTagLen do
        if lHdr[lFilePos+lTagPos-1] <> lTagName[lTagPos] then
           exit;
    result := true;
end;

function IsNumber: boolean;
begin
     if lHdr[lFilePos] in ['-','.','0'..'9'] then
           result := true
     else
         result := false;
end;

begin
  lnmosaic := 0;//detect if function found header
  if not fileexists(lFilename) then
     exit;

  FileMode := 0; //set to readonly
  AssignFile(lFile, lFileName);
  Reset(lFile, 1);
  lFileSz := FileSize(lFile);
  if lFileSz > kMaxHdrSz then
     lFileSz := kMaxHdrSz;
  if lFilesz < (2000) then begin
        //to small to be DICOM mosaic
        CloseFile(lFile);
        Filemode := 2;
        exit;
  end;
  BlockRead(lFile, lHdr, lFileSz*SizeOf(Char));
  FileMode := 0; //set to readonly
  CloseFile(lFile);
  lFilePos := 1;
for lLoop := 1 to 2 do begin
    if lLoop = 1 then begin
       lnSubloop := 1;
       lTagName := lTagName1;
    end else begin
        lnSubloop := 3;
        lTagName := lTagName2;
    end;
  lTagLen := length (lTagName);
  while (lFilePos < (lFileSz-lTagLen)) and (not (IsTag)) do
        inc(lFilePos);
  if (lFilePos >= (lFileSz-lTagLen)) then
     exit;
  lFilePos := lFilePos + 72; //lots of space between name and value
for lSubloop := 1 to lnSubloop do begin
  while (lFilePos < (lFileSz)) and (not (IsNumber)) do
        inc(lFilePos);
  if (lFilePos >= (lFileSz-lTagLen)) then
     exit;
  lStr :='';// lHdr[lFilePos];
  while (lFilePos < (lFileSz)) and  (IsNumber) do begin
        lStr := lStr+lHdr[lFilePos];
        inc(lFilePos);
  end;
  if lStr = '' then
     exit;
  if lLoop = 1 then
     lnmosaic := strtoint(lStr)
  else begin
      case lSubloop of
           1: lv1 := strtofloat(lStr);
           2: lv2 := strtofloat(lStr);
           3: lv3 := strtofloat(lStr);
      end; //case
  end; //else
end; //sublooop
end; //loop
end;    *)



function IsNormalMosaic(var lDicomData: DicomData; var lMosaicSlices: integer; var lFilename: string): boolean;
var
   Q: TMatrix;
   lX,lY,lnmos,lImagesPerRow: integer;
   lv1,lv2,lv3: double;
begin
     lX := lDICOMdata.SiemensMosaicX;
     lY := lDICOMdata.SiemensMosaicY;
     result := false;
     lMosaicSlices := lDICOMdata.SiemensSlices;
     if lMosaicSlices = 0 then
        lMosaicSlices := lDICOMdata.SiemensMosaicX*lDICOMdata.SiemensMosaicY;

     if (lDICOMdata.SiemensMosaicX < 1) then
        exit; //not mosaic
     if (lDICOMdata.SiemensSlices > 0) then
        exit; //pre-Trio Siemens Mosaic Data
     if not GetCSAImageHeaderInfo (lFilename, lDicomData.CSAImageHeaderInfoPos ,lDicomData.CSAImageHeaderInfoSz, lnmos,lDicomData.SiemensMosaicX,lDicomData.SiemensMosaicY, lv1,lv2,lv3) then begin
        lDICOMdata.SiemensMosaicX := lX;
        lDICOMdata.SiemensMosaicY := lY;
        lMosaicSlices := lX * lY;
        exit;
     end;
     if lnmos < 1 then
        exit;
     //4/4/2008 I used to read AcquisitionMatrixText from CSA Image Header... however this is a problem if the images are interpolated
     lImagesPerRow := 1;
     while sqr(lImagesPerRow) < lnMos do
           inc(lImagesPerRow);
     lDICOMdata.SiemensMosaicX := lDicomData.XYZdim[1] div lImagesPerRow;
     lDICOMdata.SiemensMosaicY := lDicomData.XYZdim[2] div lImagesPerRow;  //29Sept2008
     //fx(lDicomData.XYZdim[1],lDicomData.XYZdim[2],lnmos,lDICOMdata.SiemensMosaicY);
     lDicomData.SiemensMosaicX := lDicomData.XYZdim[1] div lDICOMdata.SiemensMosaicX;
     lDicomData.SiemensMosaicY := lDicomData.XYZdim[2] div lDICOMdata.SiemensMosaicY;
     //get_numaris4_val(lFilename,'NumberOfImagesInMosaic','SliceNormalVector',lnmos,lv1,lv2,lv3);
     //b13
     Q := Matrix2D(lDicomData.Orient[1], lDicomData.Orient[4],lv1,
              lDicomData.Orient[2],lDicomData.Orient[5],lv2,
              lDicomData.Orient[3],lDicomData.Orient[6],lv3);
     if  nifti_mat33_determ(Q) < 0 then begin

         //dcmMsg('Note: raw slice order R>>L (Siemens convention), dcm2niiprior to 2014 stored these as L>>R, NIfTI convention). Benefit: simpler slice timing correction (ascending is 1..N, descending is N..1)');
         result := true;
     end;
     lMosaicSlices := lnmos;
end;


function IsSiemensDTI(var lDicomData: DicomData;  var lDTI: TDTI;var lFilename: string; var lPrefs: TPrefs): boolean;
begin
     result := false;
     if (lDICOMdata.SiemensDICOMDTI) and (lDICOMdata.DTI.bval >= 0) and (lDicomData.Vers0018_1020 {ImplementationVersion} >= lPrefs.SiemensDTIUse0019If00181020atleast {IgnoreDTIRotationsIf_0002_0013_atleast}) then begin
        lDTI.Bval := lDICOMdata.DTI.bval;
        ldti.v1 := lDicomData.DTI.v1;
        ldti.v2 := lDicomData.DTI.v2;
        ldti.v3 := lDicomData.DTI.v3;
        result := true;
        exit;
     end;
     if (lDICOMdata.ManufacturerID <> kSiemensID) then
        exit;

     if not GetCSAImageHeaderInfoDTI (lFilename, lDicomData.CSAImageHeaderInfoPos ,lDicomData.CSAImageHeaderInfoSz,lDTI.Bval,ldti.v1,ldti.v2,ldti.v3) then
     //if not GetCSAImageHeaderInfoDTI (lFilename, lDicomData.CSAImageHeaderInfoPos ,lDicomData.CSAImageHeaderInfoSz, lDTI.Bval, ldti.v1,ldti.v2,ldti.v3) then
        exit;
     if lDTI.bval >= 0 then begin
      result := true;
      lDicomData.SiemensDICOMDTICSA := true;
     end;
     //fx(lDTI.bval);
end;

(*procedure ReportMatrix(s: string; q: TMatrix);
begin
     dcmmsg(s+Format('=[ %g %g %g %g; %g %g %g %g; %g %g %g %g; 0 0 0 1]',  [
     q.matrix[1,1],q.matrix[1,2],q.matrix[1,3],q.matrix[1,4]  ,
     q.matrix[2,1],q.matrix[2,2],q.matrix[2,3],q.matrix[2,4] ,
     q.matrix[3,1],q.matrix[3,2],q.matrix[3,3],q.matrix[3,4]]));
end;

procedure ReportMat33(s: string; q: TMatrix);
begin
     dcmmsg(s+Format('=[ %g %g %g; %g %g %g; %g %g %g]',  [
     q.matrix[1,1],q.matrix[1,2],q.matrix[1,3]  ,
     q.matrix[2,1],q.matrix[2,2],q.matrix[2,3] ,
     q.matrix[3,1],q.matrix[3,2],q.matrix[3,3]]));
end;   *)

procedure dicom_2_niftiMosaic(var Q44: TMatrix; var d: DicomData; var lHdr: TNiftiHdr ; lMosaicSlices: integer; var lFlipMosaic: boolean);
var
  det: TMatrix;
  nRowCol,lFactorX,lFactorY: double;
  c,r: integer;
  v,vO: TVector;
begin
     nRowCol := ceil(sqrt(lMosaicSlices));
     lFactorX := (d.xyzDim[1] -(d.xyzDim[1]/nRowCol)   )/2.0;
     lFactorY := (d.xyzDim[2] -(d.xyzDim[2]/nRowCol)   )/2.0;
     Q44.matrix[1,4] :=(Q44.matrix[1,1]*lFactorX)+(Q44.matrix[1,2]*lFactorY)+Q44.matrix[1,4];
     Q44.matrix[2,4] :=(Q44.matrix[2,1]*lFactorX)+(Q44.matrix[2,2]*lFactorY)+Q44.matrix[2,4];
     Q44.matrix[3,4] :=(Q44.matrix[3,1]*lFactorX)+(Q44.matrix[3,2]*lFactorY)+Q44.matrix[3,4];
     Q44.size := size3D;
     //ReportMatrix('mega',Q44);
     for c:=1 to 2 do
         for r := 1 to 4 do
             Q44.matrix[c,r] := -Q44.matrix[c,r];

      if lFlipMosaic then begin
         det := Diag3D(1,1,-1,1);
         Q44 := MultiplyMatrices(Q44,det);
      end;
      //next we have flipped images on the Y dimension
      vO := Vector3D(0,lHdr.dim[2]-1,0); //find coordinate of new origin voxel - located on opposite side of Y-dimension
      for r := 1 to 3 do begin
        v.vector[r] := 0;
        for  c :=1 to 4 do
            v.vector[r] := v.vector[r] + (Q44.matrix[r,c]*vO.vector[c]);
      end;
      det := Diag3D(1,-1,1,1);
      Q44 := MultiplyMatrices(Q44,det);
      Q44.matrix[1,4] := v.x;
      Q44.matrix[2,4] := v.y;
      Q44.matrix[3,4] := v.z;


end;

procedure dicom_2_nifti(var lDicomData: DicomData; var lHdr: TNiftiHdr ; lMosaicSlices: integer; var lFlipMosaic: boolean);
var
  Q,res,diagVox,patient_to_tal,analyze_to_dicom: TMatrix;
  lx,ly: integer;
  val: double;
  dx,dy,dz: single;
  lOK: boolean;
begin
   lHdr.sform_code := kNIFTI_XFORM_UNKNOWN;
   lOK := false;
   for lx := 1 to 6 do
		if lDicomData.Orient[lx] <> 0 then lOK := true;
   if not lOK then exit;
   Q := Diag3D(1,1,1,1);//set column 4 and row 4 to zeros, except [4,4]
   Q.matrix[1,1] :=lDicomData.Orient[1] ; Q.matrix[1,2] := lDicomData.Orient[2] ; Q.matrix[1,3] := lDicomData.Orient[3] ; //* load Q */
   Q.matrix[2,1] := lDicomData.Orient[4] ; Q.matrix[2,2] := lDicomData.Orient[5] ; Q.matrix[2,3] := lDicomData.Orient[6] ;
   //ReportMatrix('bar',Q);
   // normalize row 1
   val := Q.matrix[1,1]*Q.matrix[1,1] + Q.matrix[1,2]*Q.matrix[1,2] + Q.matrix[1,3]*Q.matrix[1,3] ;
   if( val > 0.0 ) then begin
	 val := 1.0 / sqrt(val) ;
	 Q.matrix[1,1] := Q.matrix[1,1]* val ; Q.matrix[1,2] := Q.matrix[1,2]* val ; Q.matrix[1,3] := Q.matrix[1,3]* val ;
   end else begin
	 Q.matrix[1,1] := 1.0 ; Q.matrix[1,2] := 0.0 ; Q.matrix[1,3] := 0.0 ;
   end;
   // normalize row 2
   val := Q.matrix[2,1]*Q.matrix[2,1] + Q.matrix[2,2]*Q.matrix[2,2] + Q.matrix[2,3]*Q.matrix[2,3] ;
   if( val > 0.0 ) then begin
	 val := 1.0 / sqrt(val) ;
	 Q.matrix[2,1] := Q.matrix[2,1]* val ; Q.matrix[2,2] := Q.matrix[2,2]* val ; Q.matrix[2,3] := Q.matrix[2,3]* val ;
   end else begin
	 Q.matrix[2,1] := 0.0 ; Q.matrix[2,2] := 1.0 ; Q.matrix[2,3] := 0.0 ;
   end;
   //ReportMat33('norm',Q);
   //row 3 is cross product of previous rows
   Q.matrix[3,1] := Q.matrix[1,2]*Q.matrix[2,3] - Q.matrix[1,3]*Q.matrix[2,2] ;
   Q.matrix[3,2] := Q.matrix[1,3]*Q.matrix[2,1] - Q.matrix[1,1]*Q.matrix[2,3] ;
   Q.matrix[3,3] := Q.matrix[1,1]*Q.matrix[2,2] - Q.matrix[1,2]*Q.matrix[2,1] ;
   res := Diag3D(1,1,1,1); //set forth column/row
   //next: transpose matrix
   for lx := 1 to 3 do
		for ly := 1 to 3 do
			res.matrix[lx,ly] := Q.matrix[ly,lx];
   Q := res;
   //next if det(orient)<0, orient(:,3) = -orient(:,3); end;
   //showmessage(realtostr(getDeterminant(Q),2));
   if getDeterminant(Q) < 0 then begin
	   Q.matrix[1,3] := -Q.matrix[1,3];
	   Q.matrix[2,3] := -Q.matrix[2,3];
	   Q.matrix[3,3] := -Q.matrix[3,3];
   end;
   //reportMatrix('preScale',Q);
   //next scale matrix
   diagVox := Diag2D(lDicomData.XYZmm[1],lDicomData.XYZmm[2],lDicomData.XYZmm[3]);
   Q.size := size2D;
   Q := MultiplyMatrices(Q,diagVox);
   //reportMatrix('postScale',Q);
   //next - add translations
   Q.matrix[1,4] := lDicomData.PatientPosX;
   Q.matrix[2,4] := lDicomData.PatientPosY;
   Q.matrix[3,4] := lDicomData.PatientPosZ;
   //reportMatrix('postOffset',Q);
   if (lDICOMdata.SiemensMosaicX > 1) or (lDICOMdata.SiemensMosaicY > 1) then begin
        dicom_2_niftiMosaic(Q, lDicomData, lHdr, lMosaicSlices, lFlipMosaic);


   end else begin
   val := lDicomData.XYZdim[2];
   (*if (lDICOMdata.SiemensMosaicX > 1) or (lDICOMdata.SiemensMosaicY > 1) then begin
	AdjMosaic(Q,lDICOMdata);
	val := lDicomData.XYZdim[2]/lDICOMdata.SiemensMosaicY;
        //lFlipMosaic := IsNormalMosaic(lDicomData,lMosaicSlices,lFilename);
   end; *)
   Q.matrix[4,1] := 0;
   Q.matrix[4,2] := 0;
   Q.matrix[4,3] := 0;
   Q.matrix[4,4] := 1;
   Q.size := size3D;
   //Q now equals 'dicom_to_patient' in spm_dicom_convert
   //result := q; exit; //escape to compare with SPM
   //next - convert space
   patient_to_tal   := diag3D(-1, -1, 1,1);
   analyze_to_dicom := Matrix3D (
	1, 0, 0,-1,
	0,-1, 0, val,
	0, 0, 1,-1,
	0, 0, 0, 1);
   //reportMatrix('d2pat',Q);
   //reportmatrix('p2tal',patient_to_tal);
   //reportMatrix('a2d',analyze_to_dicom);
   Q := MultiplyMatrices(patient_to_tal,Q);
   //reportMatrix('postTal',Q);
   Q := MultiplyMatrices(Q,analyze_to_dicom);
   //reportMatrix('mat',Q);
   //Q now equals 'mat' in spm_dicom_convert
   //subasgn.m in SPM5 translates by one voxel...
   analyze_to_dicom := Matrix3D (
	1, 0, 0,1,
	0,1, 0, 1,
	0, 0,1,1,
	0, 0, 0, 1);
   Q := MultiplyMatrices(Q,analyze_to_dicom);//not used for flips
  (*if ((lDICOMdata.SiemensMosaicX > 1) or (lDICOMdata.SiemensMosaicY > 1)) and (lFlipMosaic) then begin

              patient_to_tal   := diag3D(1, 1, 1,1);
              patient_to_tal.matrix[3,4] := 1-lMosaicSlices;
              Q := MultiplyMatrices(Q,patient_to_tal);
     end else if ((lDICOMdata.SiemensMosaicX > 1) or (lDICOMdata.SiemensMosaicY > 1)) and (getDeterminant(Q) < 0)  then begin
            //patient_to_tal   := diag3D(1, 1, 1,1);
            //patient_to_tal.matrix[3,4] := 1-lMosaicSlices;
        patient_to_tal   := diag3D(1, 1, -1,1);
            Q := MultiplyMatrices(Q,patient_to_tal);
   end;  *)
     end;  //if mosaic else not mosaic
   //if (lDICOMdata.SiemensMosaicX = 1) and (lDICOMdata.SiemensMosaicY = 1) then
   //reportmatrix('final',Q);
   //mat44_to_quatern(Q); *)
   //reportMatrix('nii',Q);
   lHdr.sform_code := kNIFTI_XFORM_SCANNER_ANAT;
   lHdr.srow_x[0] := Q.matrix[1,1];
   lHdr.srow_x[1] := Q.matrix[1,2];
   lHdr.srow_x[2] := Q.matrix[1,3];
   lHdr.srow_x[3] := Q.matrix[1,4];
   lHdr.srow_y[0] := Q.matrix[2,1];
   lHdr.srow_y[1] := Q.matrix[2,2];
   lHdr.srow_y[2] := Q.matrix[2,3];
   lHdr.srow_y[3] := Q.matrix[2,4];
   lHdr.srow_z[0] := Q.matrix[3,1];
   lHdr.srow_z[1] := Q.matrix[3,2];
   lHdr.srow_z[2] := Q.matrix[3,3];
   lHdr.srow_z[3] := Q.matrix[3,4];
   //finally, create Quat from matrix
   lHdr.qform_code := kNIFTI_XFORM_SCANNER_ANAT;
   nifti_mat44_to_quatern( Q,
   lHdr.quatern_b,lHdr.quatern_c,lHdr.quatern_d,
   lHdr.qoffset_x,lHdr.qoffset_y,lHdr.qoffset_z,
                             dx, dy, dz, lHdr.pixdim[0]{QFac});

   //msgq(lHdr.quatern_b,lHdr.quatern_c,lHdr.quatern_d,lHdr.qoffset_x,lHdr.qoffset_y,lHdr.qoffset_z);
end;

(*function CountOrders(var lDICOMdata: dicomdata): integer;
//for 4D files, if you have 'M'agnitude and 'Phase' maps in order M M P P M P this will return 1 1 2 2 1 2
const
     kTypes = 10;
var
   ltype,i: integer;
   noveltype: array [1..kTypes] of boolean;
begin
     result := 0;
     for i := 1 to kTypes do
         noveltype[i] := true;
     for i := 1 to lDICOMdata.nOrder do begin
         ltype := lDICOMdata.order[i];
         if (ltype < 1) or (lType > kTypes) then
            lType := 1;
         if (noveltype[ltype])then
            inc(result); //we found a new order...
         noveltype[ltype] := false;
     end;
end;*)

(*function SortOrders(var lDICOMdata: dicomdata; var lImageOrder: bytep): integer;
//for 4D files, if you have 'M'agnitude and 'Phase' maps in order M M P P M P this will return 1 1 2 2 1 2
const
     kTypes = 10;
var
   ltype,i: integer;
   noveltype: array [1..kTypes] of boolean;
begin
     result := 0;
     for i := 1 to kTypes do
         noveltype[i] := true;
     getmem(lImageOrder,lDICOMdata.nOrder);
     for i := 1 to lDICOMdata.nOrder do begin
         ltype := lDICOMdata.order[i];
         if (ltype < 1) or (lType > kTypes) then
            lType := 1;
         lImageOrder^[i] := lType;
         if (noveltype[ltype])then
            inc(result); //we found a new order...
         noveltype[ltype] := false;
     end;
end;

function CountOrders(var lDICOMdata: dicomdata): integer;
var
   lImageOrder: bytep;
begin
     if (lDICOMdata.nOrder < 1) then
        exit;
    result := SortOrders(lDICOMdata,lImageOrder);
    freemem(lImageOrder);
end;

function ParseOrder(var lDICOMdataAllTypes,lDICOMdataSelectedType: dicomdata; lSelectedType: integer): integer;
var
   lnTypes: integer;
   lImageOrder: bytep;
begin
     lDICOMdataSelectedType := lDICOMdataAllTypes;
     if (lDICOMdataAllTypes.nOrder < 1) then
        exit;
    lnTypes := SortOrders(lDICOMdataAllTypes,lImageOrder);
    if lnTypes <= lSelectedType then begin
          TDTIRA = array [1..kMaxDTIDir] of TDTI;//TDICOM;//unsigned 8-bit int
  TOrder= array [1..kMaxOrderVal] of byte;

    end;
    freemem(lImageOrder);
end;  *)


///321
function MultiOrder(var lDICOMdata: dicomdata): integer;
//how many slices are of the same type [magnitude, phase, etc]
//returns 0 if all types are the same
var
   i: integer;
begin
     result := 0;
     if (lDICOMdata.nOrder < 1) then
        exit;
     if (lDicomData.XYZdim[3] > kMaxOrderVal) then
        exit;
     result := 0;
     for i := 1 to lDICOMdata.nOrder do begin
         if lDICOMdata.order[i] = lDICOMdata.order[1] then
            inc(result); //count how many have the same order as first...
     end;
     if result = lDICOMdata.nOrder then
        result := 0; //all are like the first
end;

function CompressMultiOrder(var lDICOMdata: dicomdata): integer;
//convert multiorder so values are 1..n,
//e.g. if the values were 2,3,4 it would be converted to 1..3
//e.g. if the values are 3,4 it will be converted to 1..2
var
   min,max,i,j: integer;
   SlotUsed: boolean;
begin
     result := 1;
     if (lDICOMdata.nOrder < 1) then
        exit;
     if (lDicomData.XYZdim[3] > kMaxOrderVal) then
        exit;
     min := lDICOMdata.order[1];
     for i := 1 to lDICOMdata.nOrder do
         if lDICOMdata.order[i] < min then
            min := lDICOMdata.order[i]; //count how many have the same order as first...
     max := lDICOMdata.order[1];
     for i := 1 to lDICOMdata.nOrder do
         if lDICOMdata.order[i] > max then
            max := lDICOMdata.order[i]; //count how many have the same order as first...
     result := 1;
     for j := min to max do begin
         SlotUsed := false;
         for i := 1 to lDICOMdata.nOrder do begin
             if lDICOMdata.order[i] = j then begin
                SlotUsed := true;
                lDICOMdata.order[i] := result;
             end;
         end;//for each value
         if SLotUsed then
            inc(result);
     end;
     result := result - 1;
end;

(*procedure msx (a,b,c,d: integer);
begin
     msg(inttostr(a)+'x'+inttostr(b)+'x'+inttostr(c)+'x'+inttostr(d));
end;*)
procedure SwapTimeMultiOrder (var lDICOMdata: dicomdata; var lBuffer: bytep);
//data is stored X,Y,T,Z+some order effect (e.g. magnitude and phase stroed in the same 4D image) - swap to O,X,Y,Z,T
var
   Order,nOrders,lSlicesPerOrder,lSlice,l4DBytes,lSliceBytes,lZo,lTo,lZmax,lTmax,lOrderVol:integer;
   lTempBuffer: ByteP;
begin
     lSlicesPerOrder := MultiOrder(lDICOMdata);
     if (lSlicesPerOrder = 0) then
        exit;
     nOrders := CompressMultiOrder(lDICOMdata);
     dcmMsg('Swizzling with multiple ComplexImageComponents: '+inttostr(lSlicesPerOrder)+' slices per order, total '+inttostr(lDicomData.XYZdim[3])+' slices');
     if not lDICOMdata.file4D then
        exit;
     lTMax := lDicomData.XYZdim[3] div lDICOMdata.SlicesPer3DVol div nOrders;
     lZMax := lDICOMdata.SlicesPer3DVol;
     l4DBytes :=  lDicomData.XYZdim[1]*lDicomData.XYZdim[2]*lDicomData.XYZdim[3]*trunc(((lDicomData.Allocbits_per_pixel)+7)/8);
     lSliceBytes := lDicomData.XYZdim[1]*lDicomData.XYZdim[2]*trunc(((lDicomData.Allocbits_per_pixel)+7)/8);
     GetMem(lTempBuffer,l4DBytes);
     Move(lBuffer^,lTempBuffer^,l4DBytes); //move(src,dest,sz)
     fillchar(lBuffer^,l4DBytes,0);//abba
     lZo := 0;
     lTo := 0;
     lOrderVol := 0;
     //for lSlice := 1 to lDicomData.XYZdim[3] do
     //msg(inttostr(lDICOMdata.order[lSLice]));
     for Order := 1 to nOrders do begin
         for lSlice := 1 to lDicomData.XYZdim[3] do begin
             if lDICOMdata.order[lSLice] = Order then begin
                Move(lTempBuffer[((lSlice-1)*lSliceBytes)+1],lBuffer[(lZo*lSliceBytes)+((lOrderVol+lTo)*lZMax*lSliceBytes)+1],lSliceBytes);
                inc(lTo);
                if lTo >= lTMax then begin
                   lTo := 0;
                   inc(lZo);
                end;
             end; //desired order
         end;
         lOrderVol := lOrderVol + lTMax;
         lZo := 0;
     end;
     freemem(lTempBuffer);
end;

procedure SwapTime (var lDICOMdata: dicomdata; var lBuffer: bytep);
//data is stored X,Y,T,Z - swap to X,Y,Z,T
var
   lSlice,l4DBytes,lSliceBytes,lZo,lTo,lZmax,lTmax:integer;
   lTempBuffer: ByteP;
begin
     if lDicomData.XYZdim[4] < 2 then
        exit;
     if MultiOrder(lDICOMdata) <> 0 then begin
         SwapTimeMultiOrder (lDICOMdata,lBuffer);
         exit;
     end;
     dcmMsg('Swizzling: XYTZ -> XYZT');
     if not lDICOMdata.file4D then
        exit;
     lTMax := lDicomData.XYZdim[3] div lDICOMdata.SlicesPer3DVol;
     lZMax := lDICOMdata.SlicesPer3DVol;
     l4DBytes :=  lDicomData.XYZdim[1]*lDicomData.XYZdim[2]*lDicomData.XYZdim[3]*trunc(((lDicomData.Allocbits_per_pixel)+7)/8);
     lSliceBytes := lDicomData.XYZdim[1]*lDicomData.XYZdim[2]*trunc(((lDicomData.Allocbits_per_pixel)+7)/8);
     GetMem(lTempBuffer,l4DBytes);
     Move(lBuffer^,lTempBuffer^,l4DBytes); //move(src,dest,sz)
     lZo := 0;
     lTo := 0;
     for lSlice := 1 to lDicomData.XYZdim[3] do begin
         Move(lTempBuffer[((lSlice-1)*lSliceBytes)+1],lBuffer[(lZo*lSliceBytes)+(lTo*lZMax*lSliceBytes)+1],lSliceBytes);
         inc(lTo);
         if lTo >= lTMax then begin
            lTo := 0;
            inc(lZo);
         end;
     end;
     freemem(lTempBuffer);
end;

procedure FlipTB (var lDICOMdata: dicomdata; var lBuffer: bytep);
var
  l16Buf : SmallIntP;
  l32Buf : SingleP;
  lSwap16: SmallInt;
  lSwap32: Single;
  lSwap8: byte;
 lXPos,lYPos,lZPos,lX,lY,lZ,lHlfY,lDecLineOffset,lLineOffset: integer;
begin
  lX := lDicomData.XYZdim[1];
  lY := lDicomData.XYZdim[2];
  lZ := lDicomData.XYZdim[3];
  if lDicomData.SamplesPerPixel = 3 then
     lZ := lZ * 3;
  if  (lY < 2) then exit;
  lHlfY := lY div 2;
  if lDicomData.Allocbits_per_pixel = 8 then begin
	for lZPos := 1 to lZ do begin
		lLineOffset := (lZPos-1)*(lX*lY);
		lDecLineOffset := lLineOffset+(lX*lY)-lX;
	  for lYPos := 1 to lHlfY do begin
		  for lXPos := 1 to lX do begin
			  lSwap8 := lBuffer^[lXPos+lLineOffset];
			  lBuffer^[lXPos+lLineOffset] := lBuffer^[lXPos+lDecLineOffset];
			  lBuffer^[lXPos+lDecLineOffset] := lSwap8;
		  end; //for X
		  lLineOffset := lLineOffset + lX;
		  lDecLineOffset := lDecLineOffset - lX;
	  end; //for Y
	end; //for Z
  end else if lDicomData.Allocbits_per_pixel = 32 then begin
	l32Buf := SingleP(lBuffer);
	for lZPos := 1 to lZ do begin
		lLineOffset := (lZPos-1)*(lX*lY);
		lDecLineOffset := lLineOffset+(lX*lY)-lX;
	  for lYPos := 1 to lHlfY do begin
		  for lXPos := 1 to lX do begin
			  lSwap32 := l32Buf^[lXPos+lLineOffset];
			  l32Buf^[lXPos+lLineOffset] := l32Buf^[lXPos+lDecLineOffset];
			  l32Buf^[lXPos+lDecLineOffset] := lSwap32;
		  end; //for X
		  lLineOffset := lLineOffset + lX;
		  lDecLineOffset := lDecLineOffset - lX;
	  end; //for Y
	end; //for Z
  end else begin
	l16Buf := SmallIntP(lBuffer);
	for lZPos := 1 to lZ do begin
		lLineOffset := (lZPos-1)*(lX*lY);
		lDecLineOffset := lLineOffset+(lX*lY)-lX;
	  for lYPos := 1 to lHlfY do begin
		  for lXPos := 1 to lX do begin
			  lSwap16 := l16Buf^[lXPos+lLineOffset];
			  l16Buf^[lXPos+lLineOffset] := l16Buf^[lXPos+lDecLineOffset];
			  l16Buf^[lXPos+lDecLineOffset] := lSwap16;
		  end; //for X
		  lLineOffset := lLineOffset + lX;
		  lDecLineOffset := lDecLineOffset - lX;
	  end; //for Y
	end; //for Z
  end;
end; //proc FlipTB

procedure MakePackedTriplet (var lBuffer: bytep; var lDicomData: DICOMdata);
//data is saved as RRRR GGGG BBBB  save to RGBRGBRGB...
var
   lRA: bytep;
   lPix,lnPix,lSlice,lI: integer;
begin
  if (lDicomData.SamplesPerPixel <> 3) or (lDicomData.XYZdim[1] < 1) or (lDicomData.XYZdim[2] < 1) or (lDicomData.XYZdim[3] < 1) then exit;
  lnPix := lDicomData.XYZdim[1]*lDicomData.XYZdim[2]; //*lDicomData.XYZdim[3]
  GetMem(lRA,3*lnPix);   //*3 as red, green, blue
  for lSlice := 1 to lDicomData.XYZdim[3] do begin
      lI := 1 + ((lSlice-1)* (3*lnPix)); //data from input slice
      for lPix := 1 to lnPix do begin
          lRA^[lI] := lBuffer^[lPix]; //red plane
          inc(lI);
          lRA^[lI] := lBuffer^[lPix+lnPix]; //green plane
          inc(lI);
          lRA^[lI] := lBuffer^[lPix+lnPix+lnPix]; //blue plane
          inc(lI);
      end;
      Move(lRA^,lBuffer^[1 + ((lSlice-1)* (3*lnPix))],3*lnPix);
  end;
  Freemem(lRA);
end;

procedure MakePlanar (var lBuffer: bytep; var lDicomData: DICOMdata);
//data is saved as RGBRGBRGB - convert to RRRR GGGG BBBB
var
   lRA: bytep;
   lPix,lnPix,lSlice,lI: integer;
begin
     if (lDicomData.XYZdim[1] < 1) or (lDicomData.XYZdim[2] < 1) or (lDicomData.XYZdim[3] < 1) then exit;
     lnPix := lDicomData.XYZdim[1]*lDicomData.XYZdim[2]; //*lDicomData.XYZdim[3]
     GetMem(lRA,3*lnPix);   //*3 as red, green, blue
     for lSlice := 1 to lDicomData.XYZdim[3] do begin
         lI := 1 + ((lSlice-1)* (3*lnPix)); //data from input slice
         for lPix := 1 to lnPix do begin
             lRA^[lPix] := lBuffer^[lI]; //red plane
             inc(lI);
             lRA^[lPix+lnPix] := lBuffer^[lI]; //green plane
             inc(lI);
             lRA^[lPix+lnPix+lnPix] := lBuffer^[lI]; //blue plane
             inc(lI);
         end;
         Move(lRA^,lBuffer^[1 + ((lSlice-1)* (3*lnPix))],3*lnPix);
     end;
     Freemem(lRA);
end;

procedure DeMosaic (var lBuffer: bytep;lmosX,lmosY,lSlices: integer; lFlip: boolean; var lDicomData: DICOMdata);
//unMosaic
var
        lPos,lH,lW,lnMos,lMos,lMosW,lMosH, lStripBytes,lPanelBytes,lStartOffset: integer;
	lTempBuffer: ByteP;
begin
	lnMos := lSlices;// lDICOMdata.SiemensMosaicX*lDICOMdata.SiemensMosaicY;
	if (lmosX < 2) and (lmosY < 2) then exit;
        if ((lmosX*lmosY) < lSlices) then begin
            dcmMsg('This '+inttostr(lmosx)+'*'+inttostr(lmosy)+' mosaic can not hold '+inttostr(lSlices)+' slices.');
            exit;
        end;
	lMosW := lDICOMdata.XYZdim[1] div lmosX;
	lMosH := lDICOMdata.XYZdim[2] div lmosY;
	lStripBytes := lMosW*trunc(((lDicomData.Allocbits_per_pixel)+7)/8);
	lPanelBytes := lDICOMdata.XYZdim[1] *trunc(((lDicomData.Allocbits_per_pixel)+7)/8);
	GetMem(lTempBuffer,lPanelBytes*lDICOMdata.XYZdim[2]);
	Move(lBuffer^,lTempBuffer^,lPanelBytes*lDICOMdata.XYZdim[2]);
        //lImgBytes := (lPanelBytes*lDICOMdata.XYZdim[2]);
	lPos := 0;
if  lFlip then begin

dcmmsg('*** WARNING: CSA "ProtocolSliceNumber" SUGGESTS REVERSED SLICE ORDER: SPATIAL AND DTI COORDINATES UNTESTED ****');
dcmmsg('*** SOLUTION: open sequence on scanner, go to System tab, select Miscellaneous sub-tab, set default image numbering (F>>H, R>>L, A>>P');
dcmmsg('***  If this is impossible and you wish to use these sequences for DTI please conduct DTI validation described on the dcm2nii web page');
end;
     (* for lMos :=  lnMos downto 1 do begin
            lStartOffset := ((lMos-1) mod lmosX)*lStripBytes+ ( ((lMos-1) div lmosX)* (lPanelBytes*lMosH));
	    for lH := 1 to lMosH do begin
	        for lW := 1 to lStripBytes do begin
		    inc(lPos);
		    lBuffer^[lPos] := lTempBuffer^[lStartOffset+lW ];
		end;
		//lBuffer^[lPos-1] := 255;//crx
		lStartOffset := lStartOffset + lPanelBytes;
	    end;
	end;
end else *)begin
	for lMos := 1 to lnMos do begin
            lStartOffset := ((lMos-1) mod lmosX)*lStripBytes+ ( ((lMos-1) div lMosX)* (lPanelBytes*lMosH));
	    for lH := 1 to lMosH do begin
	        for lW := 1 to lStripBytes do begin
		    inc(lPos);
		    lBuffer^[lPos] := lTempBuffer^[lStartOffset+lW ];
		end;
		//lBuffer^[lPos-1] := 255;//crx
		lStartOffset := lStartOffset + lPanelBytes;
	    end;
	end;
end;
  	FreeMem(lTempBuffer);
	//FlipTB needs new coordinates
	lDicomData.XYZdim[1] := lMosW;
	lDicomData.XYZdim[2] := lMosH;
	lDicomData.XYZdim[3] := lnMos;
	FlipTB (lDICOMdata, lBuffer);
end;

function UnInterleaved (lVal, ln3D,ln4D: integer;lFLip: boolean): integer;
var
	lVol,lSlice,lOut: integer;
begin
	lSlice := ((lVal-1) mod ln3D) ;
	lVol := ((lVal-1) div ln3D) ;
	if lFlip then lSlice := ln3D-lSlice-1;
	lOut := (lSlice*ln4D)+lVol;
	//if lVol = 1 then Msg(inttostr(lSlice)+'  '+inttostr(lVol)+'  '+inttostr(lOut));
	result := lOut;
end;

function UnFlip (lVal, ln3D: integer): integer;
var
	lVol: integer;
begin
	lVol := ((lVal-1) div ln3D);
        result := lVal-((lVol)*ln3d);//{ln3D -} (lVal-((lVol-1)*ln3d));
        result := ((lVol+1)*ln3D) - result;
end;

function CheckSliceDirection( var lD1,lD2: dicomdata):boolean;
var
	lFloat: single;
begin
	result := false;
	lFloat := (ld2.PatientPosX-ld1.PatientPosX)-(ld2.PatientPosY-ld1.PatientPosY)-(ld2.PatientPosZ-ld1.PatientPosZ);
	if lFloat > 0 then
		result := true;
	//if result then Msg('yikes'+floattostr(ld2.PatientPosX-ld1.PatientPosX)+'y'+floattostr(ld2.PatientPosY-ld1.PatientPosY)+'z'+floattostr(ld2.PatientPosZ-ld1.PatientPosZ) );
end;

function Index (lSeries,lFirstDICOM: integer; lInterleaved,lFlip: boolean; var lAHdr: TNIFTIhdr ): integer;
begin
     if lInterleaved then
        result := UnInterleaved (lSeries, lAHdr.dim[3],lAHdr.dim[4],lFlip)
     else if not lFlip then
          result := lSeries-1
     else
         result := UnFlip (lSeries, lAHdr.dim[3]);
     result := result + lFirstDICOM;
end;

procedure SiemensFlipYBvecs (var lDTIra: TDTIra; nVec: integer);
var
   lI: integer;
   V: double;
begin
     if nVec < 1 then exit;
    for lI := 1 to nVec do begin
        if lDTIra[lI].v2 <> 0 then //people do not like seeing -0, even though this is a valid ieee value
           lDTIra[lI].v2 := -lDTIra[lI].v2;
    end;
    //next: normalize
    for lI := 1 to nVec do begin

        V := sqr(lDTIra[lI].v1)+ sqr(lDTIra[lI].v2)+sqr(lDTIra[lI].v3);
        if V = 0 then
           V := 1
        else
            V := sqrt(V);

        lDTIra[lI].v1 := lDTIra[lI].v1/V;
        lDTIra[lI].v2 := lDTIra[lI].v2/V;
        lDTIra[lI].v3 := lDTIra[lI].v3/V;
    end;

end;

procedure GECorrectBvecs (var lDICOMdata:dicomdata; var lDTIra: TDTIra; nVec: integer);
//0018,1312 phase encoding is either in row or column direction
//0043,1039 (or 0043,a039). b value (as the first number in the string).
//0019,10bb (or 0019,a0bb). Phase-gradient diffusion direction
//0019,10bc (or 0019,a0bc). Frequency-gradient diffusion direction
//0019,10bd (or 0019,a0bd). Slice diffusion direction
//If 0018,1312 = Col then X=-x0bb,  Y=x0bc, Z=x0bd,
//If 0018,1312 = Row then X=-x0bc,  Y=-x0bb, Z=x0bd,
var
   lI: integer;
   lCol: boolean;
   lSwap: double;
begin
     if nVec < 1 then exit;
     if (length(lDicomData.PatientPos) >= 3) and (lDicomData.PatientPos[1] = 'H') and (lDicomData.PatientPos[2] = 'F') and (lDicomData.PatientPos[3] = 'S') then
     else begin
          dcmMsg('DTI vector error: Position is not head first supine');
          exit;
     end;
     if  (length(lDicomData.PhaseEncoding) >= 3)  and (upcase(lDicomData.PhaseEncoding[1]) = 'C')  then
        lCol := true
     else
         lCol := false;
     //dcmMsg('>>>>'+lDicomData.PhaseEncoding[1]+lDicomData.PhaseEncoding[2]+lDicomData.PhaseEncoding[3]+'<<<'+inttostr(length(lDicomData.PhaseEncoding)));
     for lI := 1 to nVec do begin
         lDTIra[lI].v3 := lDTIra[lI].v3;
         if (lDTIra[lI].bval <= 0) or  ((lDTIra[lI].v1 = 0) and (lDTIra[lI].v2 = 0) and (lDTIra[lI].v3 = 0)) then begin
            lDTIra[lI].v1 := 0;
            lDTIra[lI].v2 := 0;
            lDTIra[lI].v3 := 0;
        end else begin  //if bval=0 or null vector, else real vector
            if lCol then begin
            lDTIra[lI].v1 := -lDTIra[lI].v1;
            lDTIra[lI].v2 := lDTIra[lI].v2;
            end else begin
                lSwap := lDTIra[lI].v1;
                lDTIra[lI].v1 := -lDTIra[lI].v2;
                lDTIra[lI].v2 := -lSwap;
            end;
        end; //real vector  - not 0,0,0
        if (lDTIra[lI].v1 = -0.0) then lDTIra[lI].v1 := 0.0;
        if (lDTIra[lI].v2 = -0.0) then lDTIra[lI].v2 := 0.0;
        if (lDTIra[lI].v3 = -0.0) then lDTIra[lI].v3 := 0.0;
    end;//for each bvec
end;

(*procedure doBVecs;
var
 lDICOMData:dicomdata;
 lDTI: TDTI;

begin
    lDICOMData.Orient[1] := 0.99872048491662;
    lDICOMData.Orient[2] :=  -0.0015021527936;
    lDICOMData.Orient[3] :=  -0.0505483584788;
    lDICOMData.Orient[4] :=  -1.12378993e-008;
    lDICOMData.Orient[5] :=  0.99955873135595;
    lDICOMData.Orient[6] :=  -0.0297042517172;
    lDTI.v1 := 0.99899346;
    lDTI.v2 :=  0.00503525;
    lDTI.v3 := -0.00604230;
    correctBvecs(lDICOMdata, lDTI);

end; *)



function MkDICOMDir (var lDICOMdata: DICOMdata;  var lOutDir: string): boolean;
var
   lBlank,lName: string;
   lPrefs: TPrefs;
begin
  result := false;
  if not direxists(lOutDir) then
     exit;
  lBlank := '';
  lPrefs.AppendDate := true;
  lPrefs.AppendAcqSeries := false;
  lPrefs.AppendProtocolName := false;
  lPrefs.AppendPatientName := true;
  lPrefs.AppendFilename := false;
  lName := OutputFilename(lBlank,lDicomData, lPrefs);
  if lName = '' then
     exit;
  lOutDir := lOutDir +lName;
  dcmMsg('Creating folder '+lOutDir);
  {$I-}
  MkDir(lOutDir);
  if IOResult <> 0 then begin
    //MessageDlg('Cannot create directory', mtWarning, [mbOk], 0)
  end;

  {$I+}

  lOutDir := lOutDir + pathdelim;
  result := true;
end;



function ImageScalingOrIntensityVaries(var lDICOMra: TDICOMrap; lFirstDICOM, lLastDICOM: integer): boolean;
var
  lIndex: integer;
begin
  result := false;
  if (lFirstDICOM >= lLastDICOM) then exit; //only one image
  result := true;
  for lIndex := (lFirstDICOM +1) to lLastDICOM do begin
    if lDICOMra^[lIndex].IntenIntercept <> lDICOMra^[lFirstDICOM].IntenIntercept then
		  exit; //1492
    if lDICOMra^[lIndex].IntenScale <> lDICOMra^[lFirstDICOM].IntenScale then
		  exit; //1492
    if lDICOMra^[lIndex].Allocbits_per_pixel <> lDICOMra^[lFirstDICOM].Allocbits_per_pixel then
      exit;
  end;
  result := false;
end;

procedure MakeFloat (var lBuffer: bytep; var lDicomData: DICOMdata; var lSliceBytesOut: integer);
//data is saved as RGBRGBRGB - convert to RRRR GGGG BBBB
var
   lRA: bytep;
   lPix,lnPix,lnBytes: integer;
   l8i : byteP;
   l16ui : WordP;
   l16i: SmallIntP;
   l32i: LongIntP;
   l32fo, l32f: SingleP;
   //lByteSwap: boolean;
begin
     if (lDicomData.XYZdim[1] < 1) or (lDicomData.XYZdim[2] < 1) or (lDicomData.XYZdim[3] < 1) then exit;
     dcmMsg(' Converting data to 32-bit float to correct for differences slope/intercept/precision: '+chr(9)+realtostr(lDicomData.IntenScale,8)+chr(9)+realtostr(lDicomData.IntenIntercept,8)+chr(9)+inttostr(lDicomData.Allocbits_per_pixel));
       {$IFDEF ENDIAN_BIG}
  //lByteSwap := odd(lDICOMdata.little_endian);
  {$ELSE}
  //lByteSwap := not odd(lDICOMdata.little_endian);
 {$ENDIF}
     lnPix := lDicomData.XYZdim[1]*lDicomData.XYZdim[2]*lDicomData.XYZdim[3];
     //msg('  '+inttostr(lDicomData.XYZdim[1])+' '+inttostr(lDicomData.XYZdim[2])+' '+inttostr(lDicomData.XYZdim[3]) );
     lnBytes := lnPix *trunc(((lDicomData.Allocbits_per_pixel)+7)/8);
     GetMem(lRA,lnBytes );
     Move(lBuffer^,lRA^,lnBytes); //move(src,dest,sz)
     Freemem(lBuffer);
     GetMem(lBuffer,lnPix * 4); //save as 32 bit float: 4 bytes per pixel
     l32fo := SingleP(@lBuffer^[1]);
     if lDicomData.Allocbits_per_pixel = 8 then begin //8bit - byte swapping is not a problem...
      for lPix := 1 to lnPix do
         l32fo^[lPix] := lRA^[lPix]*lDicomData.IntenScale + lDicomData.IntenIntercept;
     end; //8bit
     //next 16 bit
     (*if (lDicomData.Allocbits_per_pixel = 16) and (lByteSwap) then begin //UNSWAP 16bit
      l16ui := WordP(@lRA^[1]);

      for lPix := 1 to lnPix do
         l16ui^[lPix] := swap(l16ui^[lPix]);
     end; *)//UNSWAP 16bit
     if (lDicomData.Allocbits_per_pixel = 16) and (not lDicomData.SignedData) then begin //16bit UNSIGNED

      l16ui := WordP(@lRA^[1]);
      for lPix := 1 to lnPix do
         l32fo^[lPix] := l16ui^[lPix]*lDicomData.IntenScale + lDicomData.IntenIntercept;
     end; //16bit UNSIGNED
     if (lDicomData.Allocbits_per_pixel = 16) and (lDicomData.SignedData) then begin //16bit SIGNED
      l16i := SmallIntP(@lRA^[1]);
      for lPix := 1 to lnPix do
         l32fo^[lPix] := l16i^[lPix]*lDicomData.IntenScale + lDicomData.IntenIntercept;
     end; //16bit SIGNED
     //NEXT 32bit
     (*if (lDicomData.Allocbits_per_pixel = 32) and (lByteSwap) then begin //UNSWAP 32bit
      l32i := LongIntP(@lRA^[1]);
      for lPix := 1 to lnPix do
         pswap4i(l32i^[lPix]);
     end; //UNSWAP 32bit *)
     if (lDicomData.Allocbits_per_pixel = 32) and (not lDicomData.FloatData) then begin //32bit INTEGER
      l32i := LongIntP(@lRA^[1]);
      for lPix := 1 to lnPix do
         l32fo^[lPix] := l32i^[lPix]*lDicomData.IntenScale + lDicomData.IntenIntercept;
     end; //32bit INTEGER
     if (lDicomData.Allocbits_per_pixel = 32) and ( lDicomData.FloatData) then begin //32bit FLOAT
      l32f := SingleP(@lRA^[1]);
      for lPix := 1 to lnPix do
         l32fo^[lPix] := l32f^[lPix]*lDicomData.IntenScale + lDicomData.IntenIntercept;
     end; //32bit FLOAT
     Freemem(lRA);
     lDicomData.Allocbits_per_pixel := 32;
     lDicomData.FloatData := true;
     lDicomData.IntenScale := 1;
     lDicomData.IntenIntercept := 0;
     lSliceBytesOut := lnPix * 4;
end;

procedure SaveTextReport(lOutImgName: string; var lDICOM: dicomdata;  var lAHdr: TNIFTIhdr ; lDTIdir: integer);
var
   lFile : TextFile;
   lname,lstr: string;
begin
  lname := changefileext(lOutImgName,'.txt');
  AssignFile(lFile, lname);
  ReWrite(lFile);
  // Write a couple of well known words to this file
  lStr :=   DICOMstr(lDICOM)+kTab+'DimXYZT:'+kTab+inttostr(lAHdr.dim[1])+kTab+inttostr(lAHdr.dim[2])+kTab+inttostr(lAHdr.dim[3])+kTab+inttostr(lAHdr.dim[4]) ;
  if lDTIdir > 1 then
     lStr := lStr+'DTIdir:'+kTab+inttostr(lDTIdir);
  WriteLn(lFile, lStr);
  // Close the file
  CloseFile(lFile);
end;

function reportSliceTimes(lSliceTimes: TSliceTimes): integer;
//returns multiband factor
var
   i: integer;
   s: string;
begin
  result := 1;
  if length(lSliceTImes) < 1 then exit;
  s := '  MosaicRefAcqTimes ('+inttostr(length(lSliceTimes))+' values for Slice Time Correction) ';
  for i := 0 to (length(lSliceTimes)-1) do
     s := s+kTab+floattostr(lSliceTImes[i]);
  dcmmsg(s);
  for i := 1 to (length(lSliceTimes)-1) do
     if SameValue(lSliceTImes[i], lSliceTImes[0]) then
        inc(result);
  if (result > 1) then
     dcmmsg(' These values suggest a multiband factor of '+inttostr(result));
end;


function padS(ins: string; outl: integer): string;
var
   i: integer;
begin
     result := '';
     for i := 1 to outl do begin
         if i <= length(ins) then
            result := result + ins[i]
         else
             result := result + chr(0);
     end;
end;

procedure SetNiiStr(var  lH: TNIFTIhdr; lSdb, lSaux: string);
var
   i,l: integer;
   s: string;
begin
     s := padS (lSdb,18);
     for i := 1 to 18 do
            lH.db_name[i] := s[i];
     s := padS (lSaux,24);
     for i := 1 to 24 do
            lH.aux_file[i] := s[i];
end;

procedure AddNiiDescrip(var  lH: TNIFTIhdr; lS: string);
var
   i,l: integer;
   s: string;
begin
  s := padS (lS,80);
  l := 0;
  for i := 1 to 80 do
      if lH.descrip[i] <> chr(0) then l := i;
  if l >= 80 then exit;
  for i := 1+l to 80 do
      lH.descrip[i] := s[i-l];
end;

procedure UnswapEndian (var lImgBuffer: byteP; lBytes, lBitPix: integer); //ensures image data is in native space
//returns data in native endian
//sets 'ByteSwap' flag to false. E.G. a big-endian image will be saved as little-endian on little endian machines
var
   lInc,lImgSamples : integer;
   l32i : LongIntP;
   l16i : SmallIntP;
begin
     if lBitPix < 9 then exit;
     lImgSamples := lBytes div (lBitPix div 8);
     if lImgSamples < 1 then
        exit;
     case lBitPix of
	  16: begin
             l16i := SmallIntP(@lImgBuffer^[1]);
             for lInc := 1 to lImgSamples do
                 l16i^[lInc] := Swap(l16i^[lInc]);
          end; //l16i
          32: begin
             //note: for the purposes of byte swapping, floats and long ints are the same
             l32i := LongIntP(@lImgBuffer^[1]);
             for lInc := 1 to lImgSamples do
                 Swap4(l32i^[lInc]);
          end;//32i

     end; //case
end;

function Dicom2NII(var lDICOMra: TDICOMrap; var l4dDTIra: TDTIra; lFirstDICOM, lLastDICOM: integer; var lOutDirOrig: string; var lPrefs: TPrefs; lVols: integer): boolean;
var
   lDTIra: TDTIra;
   lCSA:TCSA;
   lSliceTimes: TSliceTimes;
 lPref: TPrefs;
 lDTIdir,lRGB: integer;
 lVolGb : double;
 lAllocSLiceSz,
 lStart,lEnd,lmosX,lmosY,lIndex,lSecondDICOM,lSeries,lnSeries,lSliceBytes,
 lMosaicSlices,lSliceBytesOut,lvolOffset,lvolOffsetInit,lvolBytesOut,lSliceOrder: integer;
 lDX: single;
 lDicomImgName,lOutHdrName,lOutImgName,lOutImgNameGZ,lOutDir,lOutDTIname, lStr:string;
 lDICOMData:dicomdata;
 lReadOK,lFlip,lIntenScaleVaries,lInterleaved,lVolSave : boolean;  //,lByteSwap
 lAHdr: TNIFTIhdr;
 lTextF: TextFile;
 lOutF,lInF: File;
 lvBuffer,lsBuffer: bytep;
 lFlipMosaicMatrix,lFlipMosaic: boolean;
begin
     lDicomImgName := lDICOMra^[lFirstDICOM].Filename;
     lDicomData := lDICOMra^[lFirstDICOM];
  if lPrefs.DebugMode2 then begin
        dcmMsg( DICOMstr(1,lDICOMra,OutputFilename(lDicomImgName,lDICOMra^[lFirstDICOM],lPref)));
        dcmMsg(inttostr(lDICOMdata.XYZdim[1])+'x'+inttostr(lDICOMdata.XYZdim[2])+'x'+inttostr(lDICOMdata.XYZdim[3])+'x'+inttostr(lDICOMdata.XYZdim[4]));
        result := true;
        exit;
  end;
  result := false;
  lSliceOrder := kNIFTI_SLICE_SEQ_UNKNOWN; //unknown
  lPref := lPrefs;
  CorrectPrefs(lPref);
  lmosX := 1;
  lmosY := 1;
  lSecondDICOM := lFirstDICOM+1;
  lFlipMosaicMatrix := false;
  lInterleaved := false;

  lnSeries :=  (lLastDICOM+1) -lFirstDICOM; //e.g. first=10, last=10 means 1 image
  if lnSeries < 1 then
	  exit;
  //next if magnitude and phase maps are saved in the same 4D file, extract to separate files...
  if (lDICOMra^[lFirstDICOM].file4D) and (MultiOrder(lDICOMra^[lFirstDICOM]) > 0) then
     lPref.fourD := false;



  if (lDicomData.SamplesPerPixel = 3) then begin
      dcmMsg('Warning: RGB to NIfTI conversion poorly tested: '+lDicomImgName);
  end;
  (*{$IFDEF ENDIAN_BIG}
  lByteSwap := odd(lDICOMdata.little_endian);
  {$ELSE}
  lByteSwap := not odd(lDICOMdata.little_endian);
 {$ENDIF}   *)
  lMosaicSlices := lDicomData.SiemensSlices;
  lOutDir := ExtractFileDirWithPathDelim2(lOutDirOrig);
  if (lOutDir = '') then begin
	lOutDir := ExtractFilePath(lDicomImgName);
  end;
  if not DirWritePermission(lOutDir) then begin // <- tested with Unix
      dcmMsg('Error: output directory is read-only: '+lOutDir);
      exit;
  end;
  if lPref.createoutputfolder then
     MkDICOMDir(lDICOMdata,lOutDir);
  if not direxists(lOutDir) then begin
	dcmMsg('Unable to find output directory '+lOutDir);
	lOutDir := ExtractFileDirWithPathDelim2(lDicomImgName)
  end; //else directory exists


 //lOutHdrName :=lOutDir+OutputFilename(lDicomImgName,lDicomData,lPrefs.AppendDate,lPrefs.AppendAcqSeries,lPrefs.AppendProtocolName,lPrefs.AppendPatientName,lPrefs.FourD,lPrefs.AppendFilename)+'.hdr';
	lOutHdrName :=lOutDir+OutputFilename(lDicomImgName,lDicomData,lPref)+'.hdr';
	lOutImgName :=changefileext(lOutHdrName,'.img');
	if lPref.SingleNIIFile then begin
		lOutHdrName :=  changefileext(lOutHdrName,'.nii');
		lOutImgName := lOutHdrName;
	end;
	if (lPref.SingleNIIFile) and (lPref.GZip) then begin
		lOutHdrName := lOutHdrName+'.gz';
		if (not UniqueFileName(lOutHdrName))  then begin
			dcmMsg('File already exists '+lOutImgName+' '+lOutHdrName);
			exit;
		end;
		//we now need to remove the .gz - not that unique filename may have appended postfix, e.g. filename.nii.gz -> filenameA.nii.gz
		StripGZExt(lOutHdrName);
		lOutImgName := lOutHdrName;
	end else begin
		if (not UniqueFileName(lOutHdrName)) or (not UniqueFileName(lOutImgName)) then begin
			dcmMsg('File already exists '+lOutImgName+' '+lOutHdrName);
			exit;
		end;
	end;
	dcmMsg(extractfilename(lDicomImgName)+'->'+extractfilename(lOutImgName));
	DICOM2AnzHdr(lAHdr,lPref.Anonymize,lDicomImgName,lDICOMdata);

  if lPrefs.DebugMode2 then begin
      dcmMsg('slice/vols/series '+inttostr(lDICOMdata.SlicesPer3DVol )+'  '+inttostr(lVols)+'   '+inttostr(lnSeries));
      dcmMsg('x/y/z '+inttostr(lDICOMdata.XYZdim[1])+'x'+inttostr(lDICOMdata.XYZdim[2])+'x'+inttostr(lDICOMdata.XYZdim[3])+'x'+inttostr(lDICOMdata.XYZdim[4]));
        result := true;
        exit;
  end;

        if (lVols > 1) and ((lnSeries mod lVols)=0) then
           lDICOMdata.SlicesPer3DVol := round(lnSeries/lVols);
        lDTIra[1].bval := -1; //not DTI
        lDTIdir := 0;
        IsSiemensDTI(lDicomData,lDTIra[1], lDicomImgName, lPrefs);//see if this is a Siemens DTI image - mosaics in B13, non-mosaic in B12
        if (lDICOMdata.SiemensMosaicX > 1) or (lDICOMdata.SiemensMosaicY > 1) then begin
           lFlipMosaicMatrix := IsNormalMosaic(lDicomData,lMosaicSlices, lDicomImgName);
	   lAHdr.dim[1] := lDicomData.XYZdim[1] div lDICOMdata.SiemensMosaicX;
	   lAHdr.dim[2] := lDicomData.XYZdim[2] div lDICOMdata.SiemensMosaicY;
           lmosX := lDICOMdata.SiemensMosaicX;
           lmosY := lDICOMdata.SiemensMosaicY;
           //lSlices := lDICOMdata.SiemensSlices;//(lDicomImgName,'NumberOfImagesInMosaic');
           if lMosaicSlices > 1 then
              lAHdr.dim[3] := lMosaicSlices
           else
               lAHdr.dim[3] := lDICOMdata.SiemensMosaicX *lDICOMdata.SiemensMosaicY;
	   lAHdr.dim[4] := lnSeries;
           if ((lmosX*lmosY) < lAHdr.dim[3]) then begin
              dcmMsg('Aborted '+lDicomData.Filename+ ' : This '+inttostr(lmosx)+'*'+inttostr(lmosy)+' mosaic can not hold '+inttostr(lAHdr.dim[3])+' slices.');
              exit;
           end;

	end else if lDICOMdata.File4D then begin//(lDicomData.XYZdim[3] > 1) and (lnSeries = 1) and (lDICOMdata.SlicesPer3DVol > 1) and ((lAHdr.dim[3] mod lDICOMdata.SlicesPer3DVol)=0) then begin
                lAHdr.dim[4] := lAHdr.dim[3] div lDICOMdata.SlicesPer3DVol;
                lAHdr.dim[3] := lDICOMdata.SlicesPer3DVol;

	end else if (lDicomData.XYZdim[3] > 1) then
		lAHdr.dim[4] := lnSeries
	else begin
             if (lDICOMdata.SlicesPer3DVol > 1) and ((lnSeries mod lDICOMdata.SlicesPer3DVol)=0)  then begin
	        lAHdr.dim[3] := lDICOMdata.SlicesPer3DVol;
		lAHdr.dim[4] := round(lnSeries / lDICOMdata.SlicesPer3DVol);
                if (lnSeries > 1) and (DICOMinterslicedistance( lDICOMra^[lFirstDICOM], lDICOMra^[lSecondDICOM]) < 0.01) then
                   lInterleaved := true;
             end else
                 lAHdr.dim[3] := lnSeries;
	end;
        if (lDICOMdata.ManufacturerID = kSiemensID) and (lDicomData.CSASeriesHeaderInfoPos > 0) and (lDicomData.CSASeriesHeaderInfoSz > 0) then begin
           lStr := GetCSASeriesHeaderInfo (lDicomImgName, lDicomData.CSASeriesHeaderInfoPos,lDicomData.CSASeriesHeaderInfoSz,lAHdr.dim[3], lSliceOrder);
           if (lSliceOrder < kNIFTI_SLICE_SEQ_UNKNOWN) or (lSliceOrder > kNIFTI_SLICE_ALT_DEC2) then  lSliceOrder :=  kNIFTI_SLICE_SEQ_UNKNOWN;
           lAHdr.slice_code := lSliceOrder;
           lAHdr.dim_info:= 3 shl 4;
           lAHdr.slice_start:= 0;
           lAHdr.slice_end :=  lAHdr.dim[3]-1;
           if lAHdr.slice_code <> kNIFTI_SLICE_SEQ_UNKNOWN then begin
           //read final not first image https://github.com/eauerbach/CMRR-MB/issues/29
           //DecodeCSA2 (lDicomImgName, lDicomData.CSAImageHeaderInfoPos,lDicomData.CSAImageHeaderInfoSz, lCSA, lSliceTimes, lFlippedMosaic);
           DecodeCSA2 (lDICOMra^[lLastDICOM].Filename, lDICOMra^[lLastDICOM].CSAImageHeaderInfoPos,lDICOMra^[lLastDICOM].CSAImageHeaderInfoSz, lCSA, lSliceTimes, lFlipMosaic);
           reportSliceTimes(lSliceTimes);
           SetNiiStr(lAHdr, lStr, lDICOMra^[lLastDICOM].ImageComments);
           if (lCSA.PhaseDirectionPositive = 1) then
              AddNiiDescrip(lAHdr,';phaseDir=+')
           else if (lCSA.PhaseDirectionPositive = 0) then
              AddNiiDescrip(lAHdr,';phaseDir=-');
           lSliceTimes := nil;
           end;
           //
           dcmmsg('For slice timing correction: the slice order is '+kSliceOrderStr[lSliceOrder]);
        end;
        if (lDICOMdata.BandwidthPerPixelPhaseEncode > 0) then begin
           //do this AFTER mosaics have reset dim[1] and dim[2]
           if (length(lDICOMdata.PhaseEncoding) > 0) and ((lDICOMdata.PhaseEncoding[1]='C') or (lDICOMdata.PhaseEncoding[1]='R')) then begin
              //fx( lAHdr.dim[1],lAHdr.dim[2]);

              if (lDICOMdata.PhaseEncoding[1]='C') then begin//columns
                 lAHdr.pixdim[6] :=  1000/lDICOMdata.BandwidthPerPixelPhaseEncode/lAHdr.dim[2];
                 lAHdr.slice_duration:= 0;
              end else begin  //rows
                  lAHdr.pixdim[6] :=  1000/lDICOMdata.BandwidthPerPixelPhaseEncode/lAHdr.dim[1];
                  lAHdr.slice_duration:= 0;
                  //dcmMsg(inttostr(lAHdr.dim[1]));
              end;
              AddNiiDescrip(lAHdr,';dwell='+realtostr( lAHdr.pixdim[6],3));
              dcmMsg('Effective echo spacing: '+floattostr( lAHdr.pixdim[6])+'ms, BandwidthPerPixelPhaseEncode: '+floattostr(lDICOMdata.BandwidthPerPixelPhaseEncode));
           end else
                dcmMsg('Unable to determine echo spacing: not sure of phase encoding direction');
        end;
lFlip := false;
	if lnSeries > 1 then  begin//check slice order
		lFlip := CheckSliceDirection(lDICOMra^[lFirstDICOM],lDICOMra^[lLastDICOM]);
		if lFlip then begin
			lDicomImgName := lDICOMra^[lLastDICOM].Filename;
                        lDICOMdata := lDICOMra^[lLastDICOM];
		end;
	end;

  //next compute dx between slices
  if (lAHdr.dim[3] > 1) and (lnSeries > 1) and (lDICOMdata.SiemensMosaicX <2)  then begin
        lDX := abs(DICOMinterslicedistance( lDICOMra^[ Index (1,lFirstDICOM,lInterleaved,lFlip,lAHdr)], lDICOMra^[ Index (2,lFirstDICOM,lInterleaved,lFlip,lAHdr)]) );
        if lDX <> 0 then begin
           lDicomData.XYZmm[3] := lDX;
           lAHdr.pixdim[3]  := lDX;
        end;
  end;
  dicom_2_nifti(lDICOMdata,lAHdr,lMosaicSlices,lFlipMosaicMatrix);
  //all slices in a NIFTI image must be of the same precision and have the same scaling intercept and slope - see if this applies
  (*lBaseIntenScale := lDICOMdata.IntenScale;
  lBaseIntenIntercept := lDICOMdata.IntenIntercept;
  lBaseBitDepth := lDicomData.Allocbits_per_pixel;
  lIntenScaleVaries := false;
  for lSeries := 1 to lnSeries do begin
    lIndex :=  Index (lSeries,lFirstDICOM,lInterleaved,lFlip,lAHdr);
    if lDICOMra^[lIndex].IntenIntercept <> lBaseIntenIntercept then
		  lIntenscaleVaries := true; //1492
    if lDICOMra^[lIndex].IntenScale <> lBaseIntenScale then
		  lIntenscaleVaries := true; //1492
    if lDICOMra^[lIndex].Allocbits_per_pixel <> lBaseBitDepth then
      lIntenscaleVaries := true;
  end; //for lnSeries   *)
  lIntenScaleVaries := ImageScalingOrIntensityVaries(lDICOMra, lFirstDICOM, lLastDICOM);

 (* for lSeries := 1 to lnSeries do begin
                lIndex :=  Index (lSeries,lFirstDICOM,lInterleaved,lFlip,lAHdr);
                lDicomData := lDICOMra^[lIndex];

                msgfx(lSeries, lDICOMdata.DTI[1].v1,lDICOMdata.DTI[1].v2,lDICOMdata.DTI[1].v3);
  end; *)
 // exit;//get out of here - crucial critical -- last chance before data saved to disk
     
    if (lAHdr.bitpix = 8) and (lDicomData.SamplesPerPixel = 3) then begin
       if (lIntenScaleVaries) then begin
          dcmMsg('RGB files can not have varying intensity scales!');
          lIntenScaleVaries := false;
       end;
       lRGB := 3;
       lAHdr.datatype := kDT_RGB;
       lAHdr.bitpix := 24;
    end else
        lRGB := 1;
    if (lIntenScaleVaries) then begin
       lAHdr.datatype := kDT_FLOAT;
       lAHdr.bitpix := 32;
       dcmMsg('Warning: images have different precision or intensity scaling - saving as 32-bit float');
    end;
	lSliceBytes := lDicomData.XYZdim[1]*lDicomData.XYZdim[2]*lDicomData.XYZdim[3]*trunc(((lDicomData.Allocbits_per_pixel)+7)/8)*lRGB;
	GetMem(lsBuffer,lSliceBytes);

        lSliceBytesOut :=lAHdr.dim[1]*lAHdr.dim[2]*lAHdr.dim[3]*trunc((lAHdr.bitpix+7)/8)*lRGB;
        if lPrefs.DebugMode2 then
           dcmMsg(' bytes/bitPix '+inttostr( lSliceBytesOut)+'  '+inttostr(lAHdr.bitpix) );
        lVolBytesOut := lSliceBytesOut * lAHdr.dim[4];
        lVolOffset := 1;
        lVolGB := (lSliceBytesOut/ 1073741824) * lAHdr.dim[4]; //bytes *1024 (kB) *1024 (Mb) * 1024 (Gb)
        //note.nii files are 352bytes larger than calculated by lVolGb...
        //Msg(floattostr(lVolGb)+' Gb');
        if lVolGb < 0.95 then
           lVolSave := true
        else begin
             dcmMsg('Very large volume: '+floattostr(lVolGb)+' Gb: slice-by-slice conversion required.');
             if lPref.GZip then begin
                 lPref.GZip := false;
                 dcmMsg('Unable to automatically GZip such a large file.');
             end;
             lVolSave := false;
        end;
	  if lVolSave then begin //save entire volume
      if lPref.SingleNIIFile then begin
              lVolOffset := kNIIImgOffset+1;// 353; //first 352 bytes empty
              lVolBytesOut := lVolBytesOut + lVolOffset -1;
      end else lVolOffset := 1;
	      GetMem(lvBuffer,lVolBytesOut);
        //showmessage(inttostr(lVolBytesOut));
        //we could copy NIfTI header to Buffer, but this would need to be changed for
        //4D->3D images or images where we swap 3rd and 4th dimension....
      end else begin //save slice by slice - slower but low RAM usage...
            if not SaveHdr (lOutHdrName,lAHdr, false,lPref.SPM2) then begin
               dcmMsg('Error saving data - do you have permission and space for '+lOutHdrName+'?');
               exit;
            end;
            Filemode := 2;
	    AssignFile(lOutF, lOutImgName);
	    if lPref.SingleNIIFile then begin
		Reset(lOutF,1);
		Seek(lOutF,352);
                lAHdr.vox_offset := 352;
            end else
		Rewrite(lOutF,1);
        end; //end slice-bylslice
	Filemode := 0; //set to read only
        lVolOffsetInit := lVolOffset;
	for lSeries := 1 to lnSeries do begin
                lIndex :=  Index (lSeries,lFirstDICOM,lInterleaved,lFlip,lAHdr);
                lDicomImgName := lDICOMra^[lIndex].Filename;
                lDicomData := lDICOMra^[lIndex];
                if (lDICOMdata.ManufacturerID = kPhilipsID) and (lDICOMdata.nDTIdir > 1) and (lAHdr.dim[4] < kMaxDTIDir) and (lDICOMdata.nDTIdir >= lAHdr.dim[4]) then begin //
                   dcmMsg('4D Philips DTI data '+inttostr(lDICOMdata.nDTIdir));
                   for lDTIdir := 1 to lAHdr.dim[4] do begin
                       lDTIra[lDTIdir].bval := l4dDTIra[lDTIdir].bval;
                       lDTIra[lDTIdir].v1 := l4dDTIra[lDTIdir].v1;
                       lDTIra[lDTIdir].v2 := l4dDTIra[lDTIdir].v2;
                       lDTIra[lDTIdir].v3 := l4dDTIra[lDTIdir].v3;
                   end;
                   lDTIdir := lAHdr.dim[4];
                end else if (lDICOMdata.ManufacturerID = kSiemensID) and (lDTIra[1].Bval >= 0) and (lDTIdir < kMaxDTIDir) and ( ((lSeries mod lAHdr.dim[3]) = 1)  or((lMosX > 1) or (lMosY > 1))) then begin //
                   inc(lDTIdir);
                   IsSiemensDTI(lDicomData,lDTIra[lDTIdir], lDicomImgName,lPrefs);

                end else if (lDICOMdata.nDTIdir = 1) and (lDICOMdata.DTI.Bval >= 0) and (lDTIdir < kMaxDTIDir) and ( (lSeries mod lAHdr.dim[3]) = 1)   then begin //
                   inc(lDTIdir);
                   lDTIra[lDTIdir].bval := lDICOMdata.DTI.bval;
                   lDTIra[lDTIdir].v1 := lDICOMdata.DTI.v1;
                   lDTIra[lDTIdir].v2 := lDICOMdata.DTI.v2;
                   lDTIra[lDTIdir].v3 := lDICOMdata.DTI.v3;
                end;
                lReadOK := true;
                if (lDicomData.JPEGLosslessCpt) then begin
                   AssignFile(lInF, lDicomImgName);
                   Reset(lInF,1);
                   //lDICOMdata.CompressSz := FileSize(lInF)-lDicomData.CompressOffset;
                   Filemode := 0;  //ReadONly
                   //lSliceBytesOut := lSliceBytes;
                   dcmMsg('Decoding lossless '+inttostr(lDICOMdata.XYZdim[1])+'x'+inttostr(lDICOMdata.XYZdim[2])+' JPEG starting from byte '+ inttostr(lDicomData.CompressOffset)+' with '+inttostr(lDICOMdata.CompressSz)+' bytes');
                   if ( lDicomData.XYZdim[3] > 1) or ( lDicomData.XYZdim[4] > 1) then
                    dcmMsg('*Warning: this software will only convert the first slice of this multislice lossless compressed JPEG');
                   lAllocSLiceSz := (lDICOMdata.XYZdim[1]*lDICOMdata.XYZdim[2] * lDICOMdata.Allocbits_per_pixel+7) div 8 ;
                   DecodeJPEG(lInF,SmallIntP0(lsBuffer),ByteP0(lsBuffer),lAllocSliceSz,lDicomData.CompressOffset,lDICOMdata.CompressSz,false);
                   CloseFile(lInF);
                   (*FlipTB(lDICOMdata,lsBuffer);
                   if lVolSave then begin{save entire volume}
                       Move(lsBuffer^,lvBuffer^[lvolOffset],lSliceBytesOut);
                       //Msg(inttostr(lSeries));
                       lVolOffset := lVolOffset + lSliceBytesOut;
                   end else begin //save slice-by-slice
                      Filemode := 2;  //read and write
		                BlockWrite(lOutF, lsBuffer^, lSliceBytesOut);
                   end;*)
                end else if (FSize(lDicomImgName) >= (lSliceBytes+lDicomData.imagestart)) then begin
                  Filemode := 0;  //ReadONly
                  AssignFile(lInF, lDicomImgName);
		              Reset(lInF,1);
		              Seek(lInF,lDicomData.imagestart);
                  Filemode := 0;  //ReadONly
                  BlockRead(lInF, lsBuffer^, lSliceBytes);
                  //ShowMsg(inttostr(lsBuffer^[lSliceBytes-1])+' '+inttostr(lsBuffer^[lSliceBytes-2])+' '+inttostr(lsBuffer^[lSliceBytes-3])+' '+inttostr(lsBuffer^[lSliceBytes-4])+' '+inttostr(lsBuffer^[lSliceBytes-6])+' '+inttostr(lsBuffer^[lSliceBytes-7]));
                  CloseFile(lInF);
                  (*if (lDICOMdata.file4D) and (lPrefs.Swizzle4D) then
                    SwapTime(lDICOMdata,lsBuffer);//data is stored X,Y,T,Z - swap to X,Y,Z,T
                  lSliceBytesOut := lSliceBytes;
                  if (lDICOMdata.PlanarConfig = 0) and (lDicomData.SamplesPerPixel = 3) then
                       MakePlanar(lsBuffer,lDICOMdata);
                  if (lMosX > 1) or (lMosY > 1) then begin
			                  DeMosaic(lsBuffer,lmosX,lmosY,lMosaicSlices,lFlipMosaic,lDICOMdata);
                        lSliceBytesOut :=lAHdr.dim[1]*lAHdr.dim[2]*lAHdr.dim[3]*trunc(((lDicomData.Allocbits_per_pixel)+7)/8);
		              end else
			              FlipTB(lDICOMdata,lsBuffer);

                   if lVolSave then begin{save entire volume}
                       Move(lsBuffer^,lvBuffer^[lvolOffset],lSliceBytesOut);
                       lVolOffset := lVolOffset + lSliceBytesOut;
                   end else begin //save slice-by-slice
                      Filemode := 2;  //read and write
		                  BlockWrite(lOutF, lsBuffer^, lSliceBytesOut);
                   end;     *)
                end else begin
                    dcmMsg('Serious error with file '+ extractfilename(lDicomImgName));
                    lReadOK := false;
                end; //if JPEG else if UNCOMPRESSED else ERROR
                if lReadOK then begin
                     {$IFDEF ENDIAN_BIG}
                     if odd(lDICOMdata.little_endian) then UnswapEndian (lsBuffer, lSliceBytes, lAHdr.bitpix);
                     {$ELSE}
                     if not odd(lDICOMdata.little_endian) then UnswapEndian (lsBuffer, lSliceBytes, lAHdr.bitpix);
                     {$ENDIF}

                   lDicomData.XYZdim[4] := lAHdr.dim[4];  //do this now - depending on slice order DicomData can be first or last volume
                  if (lDICOMdata.file4D) and (lPrefs.Swizzle4D) then
                    SwapTime(lDICOMdata,lsBuffer);//data is stored X,Y,T,Z - swap to X,Y,Z,T
                  lSliceBytesOut := lSliceBytes;
                   if (lIntenScaleVaries) then begin

                       MakeFloat(lsBuffer,lDICOMdata, lSliceBytesOut);
                       //lByteSwap := false; //Un-swapped during conversion
                   end;
                  if (lDICOMdata.PlanarConfig = 0) and (lDicomData.SamplesPerPixel = 3) then
                       MakePlanar(lsBuffer,lDICOMdata);
                  if (lMosX > 1) or (lMosY > 1) then begin
			DeMosaic(lsBuffer,lmosX,lmosY,lMosaicSlices,lFlipMosaic,lDICOMdata);
                        lSliceBytesOut :=lAHdr.dim[1]*lAHdr.dim[2]*lAHdr.dim[3]*trunc((lAHdr.bitpix+7)/8);
		  end else
		      FlipTB(lDICOMdata,lsBuffer);
                  //ShowMsg(inttostr(lAHdr.HdrSz)+'x'+inttostr(lDICOMdata.little_endian)+' : '+  inttostr(lsBuffer^[lSliceBytes-1])+' '+inttostr(lsBuffer^[lSliceBytes-2])+' '+inttostr(lsBuffer^[lSliceBytes-3])+' '+inttostr(lsBuffer^[lSliceBytes-4])+' '+inttostr(lsBuffer^[lSliceBytes-6])+' '+inttostr(lsBuffer^[lSliceBytes-7]));
                  if (not lPrefs.PlanarRGB) then
                       MakePackedTriplet(lsBuffer,lDICOMdata);
                   //msg(inttostr(lSliceBytesOut)+ '  '+inttostr(lAHdr.dim[1]*lAHdr.dim[2]*lAHdr.dim[3]*trunc((lAHdr.bitpix+7)/8)));
                   //lSliceBytesOut :=lAHdr.dim[1]*lAHdr.dim[2]*lAHdr.dim[3]*trunc((lAHdr.bitpix+7)/8);
                   if lVolSave then begin{save entire volume}
                       Move(lsBuffer^,lvBuffer^[lvolOffset],lSliceBytesOut);
                       lVolOffset := lVolOffset + lSliceBytesOut;
                   end else begin //save slice-by-slice
                      Filemode := 2;  //read and write
		      BlockWrite(lOutF, lsBuffer^, lSliceBytesOut);
                   end;
                end; //if lReadOK
	    end;
	    freemem(lsBuffer);
      Filemode := 2;  //read and write
      lOutImgNameGZ := lOutImgName;
      if lPref.TxtReport then
         SaveTextReport(lOutImgName, lDICOMdata, lAHdr,lDTIdir);
      if lVolSave then begin{save slice-by-slice}
           lOutImgNameGZ :=  SaveNIfTICore (lOutImgName, lvBuffer, lVolOffsetInit, lAHdr, lPref)
      end else //data saved slice by slice
	      CloseFile(lOutF);
                 //if (lPref.StartClip > 0) or (lPref.EndClip > 0) then
            //   Clip4D(lOutHdrName, lAHdr, false,lPref.SPM2,lPref.SingleNIIFile,lPref.GZip, true, lPref.StartClip,lPref.EndClip);

        if lDTIdir > 1 then begin
           //bvec file
           lStart := -1;//ensure this is a DTI image - some scans must have a bvalue > 1
           for lIndex := 1 to lDTIdir do
               if lDTIra[lIndex].bval = 0 then
                  lStart := lIndex;
           if lStart < 1 then begin
               dcmMsg('* Warning: diffusion acquisition does not have b-0 image');
                PartialAcquisitionError;
           end;
           lStart := -1;//ensure this is a DTI image - some scans must have a bvalue > 1
           for lIndex := 1 to lDTIdir do
               if lDTIra[lIndex].bval > 0 then
                  lStart := lIndex;

           if lStart > 0 then begin
              lStart := 1;
              lEnd := lDTIdir;
              lOutDTIname := lOutImgName;
              dcmMsg('Number of diffusion directions = '+inttostr(lDTIdir));
              if lDicomData.ManufacturerID = kSiemensID then begin
                 if lDicomData.Vers0018_1020 = 13 then
                    dcmMsg('  *Warning: some Siemens VB13 set DiffusionGradientDirection incorrectly. Please check manually validate');                    
                 if lDicomData.Vers0018_1020 >= lPrefs.SiemensDTINoAngulationCorrectionIf00181020atleast  then begin
                    dcmMsg('Note: detected Siemens Software version [0018:1020] = '+inttostr(lDicomData.Vers0018_1020) );
                    dcmMsg('  -Will use 0019:000E or 0019:100E instead of 0029:1020 if version >= ' +inttostr(lPrefs.SiemensDTIUse0019If00181020atleast));
                    dcmMsg('  -Will stack across Acquisitions if version >=' +inttostr(lPrefs.SiemensDTIStackIf00181020atleast));
                    dcmMsg('  -No slice angulation correction of vectors if version >=' +inttostr(lPRefs.SiemensDTINoAngulationCorrectionIf00181020atleast));
                    dcmMsg('  To adjust, edit '+IniName );
                    SiemensFlipYBvecs(lDTIra,lDTIdir)
                 end else
                     siemensPhilipsCorrectBvecs(lDicomData,lDTIra,lDTIdir, lFlipMosaicMatrix);
              end else if lDicomData.ManufacturerID = kPhilipsID then begin
                   //-->PhilipsCorrectBvecs(lDicomData,lDTIra,lDTIdir);
                 siemensPhilipsCorrectBvecs(lDicomData,lDTIra,lDTIdir,false);
                   //next: philips scans can include DWI images with bval>0 and v1=0,v2=0,v3=0 - we want to exclude these
                   //for lIndex := lDTIdir downto 1 do
                   //    msg(inttostr(lIndex)+ kTab+floattostr(lDTIra[lIndex].bval)+kTab+floattostr(lDTIra[lIndex].v1)+kTab+floattostr(lDTIra[lIndex].v2)+kTab+floattostr(lDTIra[lIndex].v3));
                   for lIndex := lDTIdir downto 1 do
                       if (lDTIra[lIndex].bval = 0) or (lDTIra[lIndex].v1 <> 0) or (lDTIra[lIndex].v2 <> 0) or (lDTIra[lIndex].v3 <> 0) then
                         lStart := lIndex;
                   for lIndex := 1 to lDTIdir do
                       if (lDTIra[lIndex].bval = 0) or (lDTIra[lIndex].v1 <> 0) or (lDTIra[lIndex].v2 <> 0) or (lDTIra[lIndex].v3 <> 0) then
                         lEnd := lIndex;


                   if ((lStart >1) or (lEnd < lDTIdir)) and (lStart <= lEnd) then begin
                      if lVolSave then {save slice-by-slice}
                         lOutDTIname := SaveNIfTICoreCrop (lOutImgName, lvBuffer, lVolOffsetInit,lStart-1,lDTIdir-lEnd, lAHdr, lPref)
                       else
                          lOutDTIname := Clip4D(lOutHdrName, lAHdr, false,lPref, lStart-1,lDTIdir-lEnd);
                          //lOutDTIname := Clip4D(lOutHdrName, lAHdr, false,lPref.SPM2,lPref.SingleNIIFile,lPref.GZip, false, lStart-1,lDTIdir-lEnd);
                      //Msg(lOutDTIName);
                      dcmMsg('Removed DWI from DTI scan - saving volumes '+inttostr(lStart)+'..'+inttostr(lEnd));
                   end;//exclude scans
              end else if lDicomData.ManufacturerID = kGEID then
                GECorrectBvecs(lDicomData,lDTIra,lDTIdir)
              else
                  dcmMsg('WARNING: Unkown manufacturer - DTI BVecs are probably incorrect.');//beta software
              if lStart <= lEnd then begin
              //create output vectors
              if lOutDTIname <> '' then begin //image file created
              lOutDTIname := changefileextX(lOutDTIname,'.bvec');
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
              //create bvals
              lOutDTIname := changefileextX(lOutDTIname,'.bval');
              assignfile(lTextF,lOutDTIname);
              Filemode := 0;
              rewrite(lTextF);
              for lSeries := lStart to lEnd do
                  Write(lTextF,inttostr(lDTIra[lSeries].bval)+' ');
              Writeln(lTextF);
              closefile(lTextF);
              end;// if lOutDTIname <> '' then begin //image file created
              end; //lStart <= lEnd
           end; //some bvals > 0
        end; //DTIdir

        if lVolSave then //do this AFTER DTI extraction - allows rapid cropping of Philips DTI
           Freemem ( lvBuffer)
        else begin
             if ((not lPref.FourD) and (lAHdr.dim[4] > 1)) or ((lPref.SingleNIIFile) and (lPref.Gzip)) then begin
                ChangeNIfTISubformat(lOutHdrName, lAHdr,lPref) ;
             end;
        end; //slice-by-slice
	(*if 	lIntenscaleVaries then begin
		beep;
		Msg('Intensity scale/slope or bit-depth varies across slices: perhaps convert with MRIcro.');
	end;*)
        if (lPref.enablereorient) and (lDicomData.XYZdim[2] > lPref.MinReorientMatrix) and (lDicomData.XYZdim[1] > lPref.MinReorientMatrix) and (lAHdr.dim[3] > 64) and (lAHdr.dim[4] < 2) then begin
           lOutImgName := Reorient(lOutImgNameGZ,lAHdr,lPref,false,false);
           if (lOutImgName <> '') and (lDicomData.TE < 25)  and (lDicomData.TE > 0) then //T1 image
              CropNIfTI(lOutImgName,lPref);
        end;
        result := true;
        Filemode := 0;  //ReadONly
        ExitCode := 0;
end;

end.
