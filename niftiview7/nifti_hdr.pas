unit nifti_hdr;
interface
uses Dialogs ,gzio,ZLib,SysUtils,DiskSpaceKludge{DiskSpaceEX}, define_types,Windows,GraphicsMathLibrary, nifti_types, nifti_foreign;

type

 TMRIcroHdr =  record //Next: analyze Format Header structure
   NIFTIhdr : TNIFTIhdr;
   //Ori: array [1..3] of single;
   AutoBalMinUnscaled,AutoBalMaxUnscaled
   ,WindowScaledMin,WindowScaledMax
   ,GlMinUnscaledS,GlMaxUnscaledS,Zero8Bit,Slope8bit: single; //brightness and contrast
   NIfTItransform,DiskDataNativeEndian,UsesCustomPalette,LutFromZero,usesLabels,UsesCustomPaletteRandomRainbow: boolean;
   HdrFileName,ImgFileName: string;
   gzBytesX: int64;
   LUTindex,ScrnBufferItems,ImgBufferItems,RenderBufferItems,ImgBufferBPP,RenderDim,Index: longint;
   ImgBufferUnaligned: Pointer; //raw address of Image Buffer: address may not be aligned
   ScrnBuffer,ImgBuffer,RenderBuffer: Bytep;
   LUTinvisible: DWord;//DWord;
   LUT: TLUT;
   Mat: TMatrix;
 end; //TNIFTIhdr Header Structure


 function IsVOIROIExt (var lFName: string):boolean;
 function ComputeImageDataBytes (var lHdr: TMRIcroHdr): longint; //size of image data in bytes
 function ComputeImageDataBytes8bpp (var lHdr: TMRIcroHdr): longint; //size of image as 32-bit per voxel data in bytes
 function ComputeImageDataBytes32bpp (var lHdr: TMRIcroHdr): longint; //size of image as 32-bit per voxel data in bytes
 procedure NIFTIhdr_SwapBytes (var lAHdr: TNIFTIhdr); //Swap Byte order for the Analyze type
 procedure NIFTIhdr_ClearHdr (var lHdr: TMRIcroHdr); //set all values of header to something reasonable
 function NIFTIhdr_LoadHdr (var lFilename: string; var lHdr: TMRIcroHdr): boolean;
 function NIFTIhdr_SaveHdr (var lFilename: string; var lHdr: TMRIcroHdr; lAllowOverwrite: boolean): boolean;
 //procedure NIFTIhdr_SetIdentityMatrix (var lHdr: TMRIcroHdr); //create neutral rotation matrix
 //procedure NII_SetIdentityMatrix (var lHdr: TNIFTIhdr); //create neutral rotation matrix
 function IsNIfTIHdrExt (var lFName: string):boolean; //1494
 function IsNifTiMagic (var lHdr: TNIFTIhdr): boolean;
 procedure nifti_mat2mricronmat (var lHdr: TMRIcroHdr);
 //procedure NearestOrtho(var lHdr: TMRIcroHdr);
//function nifti_mat44_orthog( lR :TMatrix; lImm,lJmm,lKmm: double): TMatrix;

 function CopyNiftiHdr (var lInHdr,lOutHdr: TNIFTIhdr): boolean;
 procedure WriteNiftiMatrix (var lHdr: TNIFTIhdr;
	m11,m12,m13,m14,
	m21,m22,m23,m24,
	m31,m32,m33,m34:  Single);
 procedure nifti_mat44_to_quatern( lR :TMatrix;
                             var qb, qc, qd,
                             qx, qy, qz,
                             dx, dy, dz, qfac : single);
 procedure nifti_mat2quat (var lHdr: TNIfTIhdr);


implementation
uses dicomhdr;//2/2208

procedure nifti_mat2mricronmat (var lHdr: TMRIcroHdr);
begin
	lHdr.Mat:= Matrix3D(
		lHdr.NIFTIhdr.srow_x[0],lHdr.NIFTIhdr.srow_x[1],lHdr.NIFTIhdr.srow_x[2],lHdr.NIFTIhdr.srow_x[3],      // 3D "graphics" matrix
		lHdr.NIFTIhdr.srow_y[0],lHdr.NIFTIhdr.srow_y[1],lHdr.NIFTIhdr.srow_y[2],lHdr.NIFTIhdr.srow_y[3],      // 3D "graphics" matrix
		lHdr.NIFTIhdr.srow_z[0],lHdr.NIFTIhdr.srow_z[1],lHdr.NIFTIhdr.srow_z[2],lHdr.NIFTIhdr.srow_z[3],      // 3D "graphics" matrix
		0,0,0,1);
end;



procedure nifti_mat2quat (var lHdr: TNIfTIhdr);
var
  lM: TMatrix;
  var qb, qc, qd,qx, qy, qz,dx, dy, dz {, qfac} : single;
begin
  lM := Matrix3D (
  lHdr.srow_x[0],lHdr.srow_x[1],lHdr.srow_x[2],lHdr.srow_x[3],      // 3D "graphics" matrix
  lHdr.srow_y[0],lHdr.srow_y[1],lHdr.srow_y[2],lHdr.srow_y[3],      // 3D "graphics" matrix
  lHdr.srow_z[0],lHdr.srow_z[1],lHdr.srow_z[2],lHdr.srow_z[3],      // 3D "graphics" matrix
						   0,0,0,1);
 nifti_mat44_to_quatern( lM,qb, qc, qd,qx, qy, qz,dx, dy, dz, lHdr.pixdim[0]);
 lHdr.quatern_b := qb;
 lHdr.quatern_c := qc;
 lHdr.quatern_d := qd;
 lHdr.qoffset_x := qx;
 lHdr.qoffset_y := qy;
 lHdr.qoffset_z := qz;
 //lHdr.pixdim[0] := qfac;
end;



function CopyNiftiHdr (var lInHdr,lOutHdr: TNIFTIhdr): boolean;
begin
     move(lInHdr,lOutHdr,sizeof(TNIFTIhdr));
    result := true;
end;

procedure WriteNiftiMatrix (var lHdr: TNIFTIhdr;
	m11,m12,m13,m14,
	m21,m22,m23,m24,
	m31,m32,m33,m34:  Single);
begin
 with lHdr do begin
	srow_x[0] := m11;
	srow_x[1] := m12;
	srow_x[2] := m13;
	srow_x[3] := m14;
	srow_y[0] := m21;
	srow_y[1] := m22;
	srow_y[2] := m23;
	srow_y[3] := m24;
	srow_z[0] := m31;
	srow_z[1] := m32;
	srow_z[2] := m33;
	srow_z[3] := m34;
 end; //with lHdr
end;


function IsNifTiMagic (var lHdr: TNIFTIhdr): boolean;
begin
	if (lHdr.magic =kNIFTI_MAGIC_SEPARATE_HDR) or (lHdr.Magic = kNIFTI_MAGIC_EMBEDDED_HDR ) then
		result := true
	else
		result :=false; //analyze
end;

function IsNIfTIHdrExt (var lFName: string):boolean;
var
	lExt: string;
begin
	lExt := UpCaseExt(lFName);
	if (lExt='.NII') or (lExt = '.HDR') or (lExt = '.NII.GZ') or (lExt = '.VOI') then
		result := true
	else
		result := false;
end;

function IsVOIROIExt (var lFName: string):boolean;
var
	lExt: string;
begin
	lExt := UpCaseExt(lFName);
	if (lExt = '.VOI') or (lExt = '.ROI') then
		result := true
	else
		result := false;
end;

function ComputeImageDataBytes32bpp (var lHdr: TMRIcroHdr): integer;
var
   lDim, lSzInBits : integer;
begin
     result := 0;
     with lHdr.NIFTIhdr do begin
          if Dim[0] < 1 then begin
             showmessage('NIFTI format error: datasets must have at least one dimension (dim[0] < 1).');
             exit;
          end;
          lSzInBits := 32; //bits per voxel
          for lDim := 1 to 3 {Dim[0]}  do
              lSzInBits := lSzInBits * Dim[lDim];
     end; //with niftihdr
     result := (lSzInBits + 7) div 8; //+7 to ensure binary data not clipped
end; //func ComputeImageDataBytes32bpp

function ComputeImageDataBytes8bpp (var lHdr: TMRIcroHdr): integer;
var
   lDim, lSzInBits : integer;
begin
     result := 0;
     with lHdr.NIFTIhdr do begin
          if Dim[0] < 1 then begin
             showmessage('NIFTI format error: datasets must have at least one dimension (dim[0] < 1).');
             exit;
          end;
          lSzInBits := 8; //bits per voxel
		  for lDim := 1 to 3 {Dim[0]}  do
              lSzInBits := lSzInBits * Dim[lDim];
     end; //with niftihdr
     result := (lSzInBits + 7) div 8; //+7 to ensure binary data not clipped
end; //func ComputeImageDataBytes8bpp
function ComputeImageDataBytes (var lHdr: TMRIcroHdr): integer;
var
   lDim, lVox : integer;
begin
     result := 0;
     with lHdr.NIFTIhdr do begin
          if Dim[0] < 1 then begin
             showmessage('NIFTI format error: datasets must have at least one dimension (dim[0] < 1).');
             exit;
          end;
		  //lSzInBits := bitpix; //bits per voxel
      if bitpix < 1 then exit;
      lVox := 1;
		  for lDim := 1 to 3 {Dim[0]} do
			  lVox := lVox * Dim[lDim];
      if lVox < 1 then
        exit;
      if (bitpix mod 8) = 0 then
        result := lVox * (bitpix div 8)
      else
        result := (lVox*bitpix + 7) div 8;
   //showmessage(inttostr(bitpix));
	 end; //with niftihdr

	  //+7 to ensure binary data not clipped
end; //func ComputeImageDataBytes


function orthogonalMatrix(var lHdr: TMRIcroHdr): boolean;
var
 lM: TMatrix;
 lRow,lCol,lN0: integer;
begin
  result := false;
  lM := Matrix3D (
  lHdr.NIFTIhdr.srow_x[0],lHdr.NIFTIhdr.srow_x[1],lHdr.NIFTIhdr.srow_x[2],lHdr.NIFTIhdr.srow_x[3],      // 3D "graphics" matrix
  lHdr.NIFTIhdr.srow_y[0],lHdr.NIFTIhdr.srow_y[1],lHdr.NIFTIhdr.srow_y[2],lHdr.NIFTIhdr.srow_y[3],      // 3D "graphics" matrix
  lHdr.NIFTIhdr.srow_z[0],lHdr.NIFTIhdr.srow_z[1],lHdr.NIFTIhdr.srow_z[2],lHdr.NIFTIhdr.srow_z[3],      // 3D "graphics" matrix
						   0,0,0,1);
  for lRow := 1 to 3 do begin
	  lN0 := 0;
	  for lCol := 1 to 3 do
		if lM.matrix[lRow,lCol] = 0 then
			inc(lN0);
	  if lN0 <> 2 then exit; //exactly two values are zero
  end;
  for lCol := 1 to 3 do begin
	  lN0 := 0;
	  for lRow := 1 to 3 do
		if lM.matrix[lRow,lCol] = 0 then
			inc(lN0);
	  if lN0 <> 2 then exit; //exactly two values are zero
  end;
  result := true;
end;

function EmptyRow (lRow: integer; var lM: TMatrix): boolean;
begin
  //fx(lM.matrix[lRow,1],lM.matrix[lRow,2],lM.matrix[lRow,3]);
    if (abs(lM.matrix[lRow,1]) < 0.00000001) and (abs(lM.matrix[lRow,2]) < 0.00000001) and (abs(lM.matrix[lRow,3]) < 0.00000001) then
      result := true
    else
      result := false;
end;

procedure ReportMatrix (lStr: string;lM:TMatrix);
begin
     showmessage(lStr+kCR+
        RealToStr(lM.matrix[1,1],6)+','+RealToStr(lM.matrix[1,2],6)+','+RealToStr(lM.matrix[1,3],6)+','+RealToStr(lM.matrix[1,4],6)+
	kCR+RealToStr(lM.matrix[2,1],6)+','+RealToStr(lM.matrix[2,2],6)+','+RealToStr(lM.matrix[2,3],6)+','+RealToStr(lM.matrix[2,4],6)+
	kCR+RealToStr(lM.matrix[3,1],6)+','+RealToStr(lM.matrix[3,2],6)+','+RealToStr(lM.matrix[3,3],6)+','+RealToStr(lM.matrix[3,4],6)+
	kCR+RealToStr(lM.matrix[4,1],6)+','+RealToStr(lM.matrix[4,2],6)+','+RealToStr(lM.matrix[4,3],6)+','+RealToStr(lM.matrix[4,4],6));
end;

function EmptyMatrix(var lHdr: TMRIcroHdr): boolean;
var
 lM: TMatrix;
 lRow,lCol: integer;
begin
  result := false;
  lM := Matrix3D (
  lHdr.NIFTIhdr.srow_x[0],lHdr.NIFTIhdr.srow_x[1],lHdr.NIFTIhdr.srow_x[2],lHdr.NIFTIhdr.srow_x[3],      // 3D "graphics" matrix
  lHdr.NIFTIhdr.srow_y[0],lHdr.NIFTIhdr.srow_y[1],lHdr.NIFTIhdr.srow_y[2],lHdr.NIFTIhdr.srow_y[3],      // 3D "graphics" matrix
  lHdr.NIFTIhdr.srow_z[0],lHdr.NIFTIhdr.srow_z[1],lHdr.NIFTIhdr.srow_z[2],lHdr.NIFTIhdr.srow_z[3],      // 3D "graphics" matrix
						   0,0,0,1);
  //ReportMatrix('x',lm);
  if EmptyRow(1,lM) or EmptyRow(2,lM) or EmptyRow(3,lM) then begin
      //ReportMatrix('Matrix appears bogus',lm);
  end else begin
   for lRow := 1 to 3 do begin {3/2008}
	  for lCol := 1 to 4 do begin
              if (lRow = lCol) then begin
		if lM.matrix[lRow,lCol] <> 1 then
			exit;
              end else begin
		if lM.matrix[lRow,lCol] <> 0 then
			exit;
              end// unity matrix does not count - mriconvert creates bogus [1 0 0 0; 0 1 0 0; 0 0 1 0; 0 0 0 0]
          end; //each col
   end;//each row
  end;

  result := true;
end;



procedure FromMatrix (M: TMatrix; var  m11,m12,m13, m21,m22,m23,
						   m31,m32,m33:  DOUBLE)  ;
  BEGIN

   m11 := M.Matrix[1,1];
   m12 := M.Matrix[1,2];
   m13 := M.Matrix[1,3];
   m21 := M.Matrix[2,1];
   m22 := M.Matrix[2,2];
   m23 := M.Matrix[2,3];
   m31 := M.Matrix[3,1];
   m32 := M.Matrix[3,2];
   m33 := M.Matrix[3,3];
END {FromMatrix3D};


function nifti_mat33_determ( R: TMatrix ):double;   //* determinant of 3x3 matrix */
begin
   result := r.matrix[1,1]*r.matrix[2,2]*r.matrix[3,3]
          -r.matrix[1,1]*r.matrix[3,2]*r.matrix[2,3]
          -r.matrix[2,1]*r.matrix[1,2]*r.matrix[3,3]
         +r.matrix[2,1]*r.matrix[3,2]*r.matrix[1,3]
         +r.matrix[3,1]*r.matrix[1,2]*r.matrix[2,3]
         -r.matrix[3,1]*r.matrix[2,2]*r.matrix[1,3] ;
end;

procedure FixCrapMat(var lMat: TMatrix);
var
 lVec000,lVec100,lVec010,lVec001: TVector;
begin
 lVec000 := Vector3D  (0, 0, 0);
 lVec100 := Vector3D  (1, 0, 0);
 lVec010 := Vector3D  (0, 1, 0);
 lVec001 := Vector3D  (0, 0, 1);
 lVec000 := Transform (lVec000, lMat);
 lVec100 := Transform (lVec100, lMat);
 lVec010 := Transform (lVec010, lMat);
 lVec001 := Transform (lVec001, lMat);

 if SameVec(lVec000,lVec100) or
    SameVec(lVec000,lVec010) or
    SameVec(lVec000,lVec001) then begin
    lMat := eye3D;
    showmessage('Warning: the transformation matrix is corrupt [some dimensions have zero size]');
 end;
end;


function nifti_mat33_rownorm( A: TMatrix ): single;  //* max row norm of 3x3 matrix */
var
   r1,r2,r3: single ;
begin
   r1 := abs(A.matrix[1,1])+abs(A.matrix[1,2])+abs(A.matrix[1,3]) ;
   r2 := abs(A.matrix[2,1])+abs(A.matrix[2,2])+abs(A.matrix[2,3]) ;
   r3 := abs(A.matrix[3,1])+abs(A.matrix[3,2])+abs(A.matrix[3,3]) ;
   if( r1 < r2 ) then r1 := r2 ;
   if( r1 < r3 ) then r1 := r3 ;
   result := r1 ;
end;

function nifti_mat33_colnorm( A: TMatrix ): single;  //* max column norm of 3x3 matrix */
var
   r1,r2,r3: single ;
begin
   r1 := abs(A.matrix[1,1])+abs(A.matrix[2,1])+abs(A.matrix[3,1]) ;
   r2 := abs(A.matrix[1,2])+abs(A.matrix[2,2])+abs(A.matrix[3,2]) ;
   r3 := abs(A.matrix[1,3])+abs(A.matrix[2,3])+abs(A.matrix[3,3]) ;
   if( r1 < r2 ) then r1 := r2 ;
   if( r1 < r3 ) then r1 := r3 ;
   result := r1 ;
end;

function nifti_mat33_inverse( R: TMatrix ): TMatrix;   //* inverse of 3x3 matrix */
var
   r11,r12,r13,r21,r22,r23,r31,r32,r33 , deti: double ;
   Q: TMatrix ;
begin
   FromMatrix(R,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
         +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;

   if( deti <> 0.0 ) then deti := 1.0 / deti ;

   Q.matrix[1,1] := deti*( r22*r33-r32*r23) ;
   Q.matrix[1,2] := deti*(-r12*r33+r32*r13) ;
   Q.matrix[1,3] := deti*( r12*r23-r22*r13) ;

   Q.matrix[2,1] := deti*(-r21*r33+r31*r23) ;
   Q.matrix[2,2] := deti*( r11*r33-r31*r13) ;
   Q.matrix[2,3] := deti*(-r11*r23+r21*r13) ;

   Q.matrix[3,1] := deti*( r21*r32-r31*r22) ;
   Q.matrix[3,2] := deti*(-r11*r32+r31*r12) ;
   Q.matrix[3,3] := deti*( r11*r22-r21*r12) ;
   result := Q;
end;

function nifti_mat33_polar( A: TMatrix ): TMatrix;
var
   k:integer;
   X , Y , Z: TMatrix ;
   dif,alp,bet,gam,gmi : single;
begin
dif := 1;
k := 0;
   X := A ;
   // force matrix to be nonsingular
   //reportmatrix('x',X);
   gam := nifti_mat33_determ(X) ;
   while( gam = 0.0 )do begin        //perturb matrix
     gam := 0.00001 * ( 0.001 + nifti_mat33_rownorm(X) ) ;
     X.matrix[1,1] := X.matrix[1,1]+gam ;
     X.matrix[2,2] := X.matrix[2,2]+gam ;
     X.matrix[3,3] := X.matrix[3,3] +gam ;
     gam := nifti_mat33_determ(X) ;
   end;

   while true do begin
     Y := nifti_mat33_inverse(X) ;
     if( dif > 0.3 )then begin     // far from convergence
       alp := sqrt( nifti_mat33_rownorm(X) * nifti_mat33_colnorm(X) ) ;
       bet := sqrt( nifti_mat33_rownorm(Y) * nifti_mat33_colnorm(Y) ) ;
       gam := sqrt( bet / alp ) ;
       gmi := 1.0 / gam ;
     end else begin
       gam := 1.0;
       gmi := 1.0 ;  //close to convergence
     end;
     Z.matrix[1,1] := 0.5 * ( gam*X.matrix[1,1] + gmi*Y.matrix[1,1] ) ;
     Z.matrix[1,2] := 0.5 * ( gam*X.matrix[1,2] + gmi*Y.matrix[2,1] ) ;
     Z.matrix[1,3] := 0.5 * ( gam*X.matrix[1,3] + gmi*Y.matrix[3,1] ) ;
     Z.matrix[2,1] := 0.5 * ( gam*X.matrix[2,1] + gmi*Y.matrix[1,2] ) ;
     Z.matrix[2,2] := 0.5 * ( gam*X.matrix[2,2] + gmi*Y.matrix[2,2] ) ;
     Z.matrix[2,3] := 0.5 * ( gam*X.matrix[2,3] + gmi*Y.matrix[3,2] ) ;
     Z.matrix[3,1] := 0.5 * ( gam*X.matrix[3,1] + gmi*Y.matrix[1,3] ) ;
     Z.matrix[3,2] := 0.5 * ( gam*X.matrix[3,2] + gmi*Y.matrix[2,3] ) ;
     Z.matrix[3,3] := 0.5 * ( gam*X.matrix[3,3] + gmi*Y.matrix[3,3] ) ;

     dif := abs(Z.matrix[1,1]-X.matrix[1,1])+abs(Z.matrix[1,2]-X.matrix[1,2])
          +abs(Z.matrix[1,3]-X.matrix[1,3])+abs(Z.matrix[2,1]-X.matrix[2,1])
          +abs(Z.matrix[2,2]-X.matrix[2,2])+abs(Z.matrix[2,3]-X.matrix[2,3])
          +abs(Z.matrix[3,1]-X.matrix[3,1])+abs(Z.matrix[3,2]-X.matrix[3,2])
          +abs(Z.matrix[3,3]-X.matrix[3,3])                          ;
     k := k+1 ;
     if( k > 100) or (dif < 3.e-6 ) then begin
         result := Z;
         break ; //convergence or exhaustion
     end;
     X := Z ;
   end;
   result := Z ;
end;






  (*





{
polar decomposition of a 3x3 matrix

   This finds the closest orthogonal matrix to input A
   (in both the Frobenius and L2 norms).

   Algorithm is that from NJ Higham, SIAM J Sci Stat Comput, 7:1160-1174.}



procedure nifti_mat44_orthog11( lR :TMatrix);
//reutrns orthogonalized matrix
var
m11,m12,m13 , m21,m22,m23 , m31,m32,m33,
   u11,u21,u31,u22,u32,u33,
   v33,v32,v31,v22,v21,v11,n11,n12,n13,n22,n23,n33,
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, xd,yd,zd: double;

   Q: TMatrix;  //3x3
begin
   // load 3x3 matrix into local variables
   FromMatrix(lR,r11,r12,r13,r21,r22,r23,r31,r32,r33);
    // Step 1 -- form symmetric M = A.transpose * A
m11 := r11*r11 + r21*r21 + r31*r31;
m12 := r11*r12 + r21*r22 + r31*r32;
m13 := r11*r13 + r21*r23 + r31*r33;
m22 :=  r12*r12 + r22*r22 + r32*r32;
m23 :=  r12*r13 + r22*r23 + r32*r33;
m33 := r13*r13 + r23*r23 + r33*r33;

// Step 2 -- find lower-triangular U such that U * U.transpose = M
u11 := sqrt(m11);
u21 := m12/u11;
u31 := m13/u11;
u22 := sqrt(m22-u21*u21);
u32 := (m23-m12*m13/m11)/u22;
u33 := sqrt(m33 - u31*u31 - u32*u32);
// Step 3 -- find V such that V*V = U.  U is also lower-triangular
v33 := 1/u33;
v32 := -v33*u32/u22;
v31 := -(v32*u21+v33*u31)/u11;
v22 := 1/u22;
v21 := -v22*u21/u11;
v11 := 1/u11;
// Step 4 -- N = V.transpose * V is inverse(sqrt(A.transpose()*A.inverse()))
n11 := v11*v11 + v21*v21 + v31*v31;
n12 := v11*v21 + v21*v22 + v31*v32;
n13 := v11*v31 + v21*v32 + v31*v33;
n22 := v21*v21 + v22*v22 + v32*v32;
n23 := v21*v31 + v22*v32 + v32*v33;
n33 := v31*v31 + v32*v32 + v33*v33;

// Step 5 -- The new matrix is A * N
m11 := r11*n11 + r12*n12 + r13*n13;
m12 := r11*n12 + r12*n22 + r13*n23;
m13 := r11*n13 + r12*n23 + r13*n33;
m21 := r21*n11 + r22*n12 + r23*n13;
m22 := r21*n12 + r22*n22 + r23*n23;
m23 := r21*n13 + r22*n23 + r23*n33;
m31 := r31*n11 + r32*n12 + r33*n13;
m32 := r31*n12 + r32*n22 + r33*n23;
m33 := r31*n13 + r32*n23 + r33*n33;
   Q :=  Matrix2D (m11,m12,m13,          // 2D "graphics" matrix
                           m21,m22,m23,
                           m31,m32,m33);
    ReportMatrix('pre',Q);

end;


procedure nifti_mat44_orthog22( lR :TMatrix);
//reutrns orthogonalized matrix
var
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, xd,yd,zd: double;
   P,Q: TMatrix;  //3x3
begin
   // load 3x3 matrix into local variables
   FromMatrix(lR,r11,r12,r13,r21,r22,r23,r31,r32,r33);

   // compute lengths of each column; these determine grid spacings

   xd := sqrt( r11*r11 + r21*r21 + r31*r31 ) ;
   yd := sqrt( r12*r12 + r22*r22 + r32*r32 ) ;
   zd := sqrt( r13*r13 + r23*r23 + r33*r33 ) ;

   // if a column length is zero, patch the trouble

   if( xd = 0.0 )then begin r11 := 1.0 ; r21 := 0; r31 := 0.0 ; xd := 1.0 ; end;
   if( yd = 0.0 )then begin r22 := 1.0 ; r12 := 0; r32 := 0.0 ; yd := 1.0 ; end;
   if( zd = 0.0 )then begin r33 := 1.0 ; r13 := 0; r23 := 0.0 ; zd := 1.0 ; end;

   // assign the output lengths
   //dx := xd;
  // dy := yd;
   //dz := zd;

   // normalize the columns

   r11 := r11/xd ; r21 := r21/xd ; r31 := r31/xd ;
   r12 := r12/yd ; r22 := r22/yd ; r32 := r32/yd ;
   r13 := r13/zd ; r23 := r23/zd ; r33 := r33/zd ;

   { At this point, the matrix has normal columns, but we have to allow
      for the fact that the hideous user may not have given us a matrix
      with orthogonal columns.

      So, now find the orthogonal matrix closest to the current matrix.

      One reason for using the polar decomposition to get this
      orthogonal matrix, rather than just directly orthogonalizing
      the columns, is so that inputting the inverse matrix to R
      will result in the inverse orthogonal matrix at this point.
      If we just orthogonalized the columns, this wouldn't necessarily hold. }
   Q :=  Matrix2D (r11,r12,r13,          // 2D "graphics" matrix
                           r21,r22,r23,
                           r31,r32,r33);



   P := nifti_mat33_polar(Q) ;  // P is orthog matrix closest to Q
   //FromMatrix(P,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   // compute the determinant to determine if it is proper
   FromMatrix(P,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   {zd := r11*r22*r33-r11*r32*r23-r21*r12*r33
       +r21*r32*r13+r31*r12*r23-r31*r22*r13 ; // should be -1 or 1
   if( zd <= 0 )then begin            //proper
     r13 := -r13 ; r23 := -r23 ; r33 := -r33 ;
   end;
   P := Matrix2D( r11,r12,r13,r21,r22,r23,r31,r32,r33);
   Q := MultiplyMatrices(Q,P);
   }
    ReportMatrix('pre',Q);

    //ReportMatrix('xxx',Q);
    ReportMatrix('svd',P);
end;                *)

(*function OrthoMat (lPerm: integer): TMatrix;
var
   lx,ly,lz,lPx:integer;
   m11,m12,m13,m21,m22,m23,m31,m32,m33: double;

begin
  if (lPerm < 1) or (lPerm > 48) then begin
     result :=Matrix2D(1,0,0,0,1,0,0,0,1);
     exit;
  end;
  m11 := 0;
  m12 := 0;
  m13 := 0;
  m21 := 0;
  m22 := 0;
  m23 := 0;
  m31 :=0;
  m32 := 0;
  m33 := 0;
  lPx := ((lPerm-1) div 6)+1; //1..8
  if lPx <= 4 then
     lx := 1
  else
      lx := -1;
  if odd(lPx) then
     ly := 1
  else
      ly := -1;
  if (lPx = 1) or (lPx = 2) or (lPx = 5) or (lPx = 6) then
     lz := 1
  else
      lz := -1;
  lPx := lPerm mod 6;
  case lPx of
       1: begin m11:= lx; m22 := ly; m33 := lz end;
       2: begin m11:= lx; m23 := ly; m32 := lz end;
       3: begin m12:= lx; m21 := ly; m33 := lz end;
       4: begin m12:= lx; m23 := ly; m31 := lz end;
       5: begin m13:= lx; m21 := ly; m32 := lz end;
     else begin m13:= lx; m22 := ly; m31 := lz end;
  end;

  result := Matrix2D (m11,m12,m13,          // 2D "graphics" matrix
                           m21,m22,m23,
                           m31,m32,m33);

end;

function ErrorSqr (lA,lB: TMatrix): double;
var
   lr,lc: integer;
begin
     result := 0;
     for lr := 1 to 3 do
         for lc := 1 to 3 do
             result := result + sqr(lA.matrix[lr,lc]-lB.matrix[lr,lc])

end;

function nifti_mat44_orthog( lR :TMatrix; lImm,lJmm,lKmm: double): TMatrix;
//reutrns orthogonalized matrix
var
   i,lmini: integer;
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, val,lmin: double;
   P,Q,R: TMatrix;  //3x3
begin
   // load 3x3 matrix into local variables
   FromMatrix(lR,r11,r12,r13,r21,r22,r23,r31,r32,r33);

   //R.m[3][0] = R.m[3][1] = R.m[3][2] = 0.0l ; R.m[3][3] = 1.0l ;
   Q := Matrix2D( r11,r12,r13,r21,r22,r23,r31,r32,r33);
   //* normalize row 1 */

   val := Q.matrix[1,1]*Q.matrix[1,1] + Q.matrix[1,2]*Q.matrix[1,2] + Q.matrix[1,3]*Q.matrix[1,3] ;
   if( val > 0.0 )then begin
     val := 1.0 / sqrt(val) ;
     Q.matrix[1,1] := Q.matrix[1,1]*val ;
     Q.matrix[1,2] := Q.matrix[1,2]*val ;
     Q.matrix[1,3] := Q.matrix[1,3]*val ;
   end else begin
     Q.matrix[1,1] := 1.0 ; Q.matrix[1,2] := 0.0; Q.matrix[1,3] := 0.0 ;
   end;

   //* normalize row 2 */
   val := Q.matrix[2,1]*Q.matrix[2,1] + Q.matrix[2,2]*Q.matrix[2,2] + Q.matrix[2,3]*Q.matrix[2,3] ;
   if( val > 0.0 ) then begin
     val := 1.0 / sqrt(val) ;
     Q.matrix[2,1] := Q.matrix[2,1]* val ;
     Q.matrix[2,2] := Q.matrix[2,2] * val ;
     Q.matrix[2,3] := Q.matrix[2,3] * val ;
   end else begin
     Q.matrix[2,1] := 0.0 ; Q.matrix[2,2] := 1.0 ; Q.matrix[2,3] := 0.0 ;
   end;

   //* normalize row 3 */
   val := Q.matrix[3,1]*Q.matrix[3,1] + Q.matrix[3,2]*Q.matrix[3,2] + Q.matrix[3,3]*Q.matrix[3,3] ;
   if( val > 0.0 ) then begin
     val := 1.0 / sqrt(val) ;
     Q.matrix[3,1] := Q.matrix[3,1] *val ;
      Q.matrix[3,2] := Q.matrix[3,2] *val ;
      Q.matrix[3,3] := Q.matrix[3,3] *val ;
   end else begin
     Q.matrix[3,1] := Q.matrix[1,2]*Q.matrix[2,3] - Q.matrix[1,3]*Q.matrix[2,2] ;  //* cross */
     Q.matrix[3,2] := Q.matrix[1,3]*Q.matrix[2,1] - Q.matrix[1,1]*Q.matrix[2,3] ;  //* product */
     Q.matrix[3,3] := Q.matrix[1,1]*Q.matrix[2,2] - Q.matrix[1,2]*Q.matrix[2,1] ;
   end;
   //ReportMatrix(inttostr(lmini),Q);
   P := OrthoMat(1);
   lmin := ErrorSqr(Q,P);
   lmini := 1;
   for i := 2 to 48 do begin
       P := OrthoMat(i);
       val := ErrorSqr(Q,P);
       //if val = lmin then
       //   showmessage('tie');
       if val < lmin then begin
          lmin := val;
          lmini := i;
       end;
       //ReportMatrix(inttostr(i),OrthoMat(i));
   end;

   P := OrthoMat(lmini) ;
   //finally - rescale by input dimensions...
   P.matrix[1,1] := P.matrix[1,1] * lImm;
   P.matrix[1,2] := P.matrix[1,2] * lImm;
   P.matrix[1,3] := P.matrix[1,3] * lImm;

   P.matrix[2,1] := P.matrix[2,1] * lJmm;
   P.matrix[2,2] := P.matrix[2,2] * lJmm;
   P.matrix[2,3] := P.matrix[2,3] * lJmm;

   P.matrix[3,1] := P.matrix[3,1] * lKmm;
   P.matrix[3,2] := P.matrix[3,2] * lKmm;
   P.matrix[3,3] := P.matrix[3,3] * lKmm;
   result := P;
   //ReportMatrix(inttostr(lmini),OrthoMat(lmini));
end;
    *)
    {
}

(*function EyeMatrix (lM: TMatrix): boolean;
//check that diagonals are positive and all other cells are zero
//negative diagonals suggests flipping...
//non-negative other cells suggests the image is not pure axial
var
   lr,lc: integer;
begin
    result := false;
    for lr := 1 to 3 do
        for lc := 1 to 3 do begin
            if (lr = lc) and (lM.matrix[lr,lc] <= 0) then
               exit;
            if (lr <> lc) and (lM.matrix[lr,lc] <> 0) then
               exit;

        end;
    result := true;
end;


function nifti_mat44_orthogx( lR :TMatrix): TMatrix;
//returns rotation matrix required to orient image so it is aligned nearest to the identity matrix =
// 1 0 0 0
// 0 1 0 0
// 0 0 1 0
// 0 0 0 1
//Therefore, image is approximately oriented in space
var
   i,lrow,lcol,lMaxRow,lMaxCol,l2ndMaxRow,l2ndMaxCol,l3rdMaxRow,l3rdMaxCol: integer;
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, val,lAbsmax,lAbs: double;
   Q: TMatrix;  //3x3
begin
   // load 3x3 matrix into local variables
   FromMatrix(lR,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   Q := Matrix2D( r11,r12,r13,r21,r22,r23,r31,r32,r33);
   // normalize row 1
   val := Q.matrix[1,1]*Q.matrix[1,1] + Q.matrix[1,2]*Q.matrix[1,2] + Q.matrix[1,3]*Q.matrix[1,3] ;
   if( val > 0.0 )then begin
     val := 1.0 / sqrt(val) ;
     Q.matrix[1,1] := Q.matrix[1,1]*val ;
     Q.matrix[1,2] := Q.matrix[1,2]*val ;
     Q.matrix[1,3] := Q.matrix[1,3]*val ;
   end else begin
     Q.matrix[1,1] := 1.0 ; Q.matrix[1,2] := 0.0; Q.matrix[1,3] := 0.0 ;
   end;
   // normalize row 2
   val := Q.matrix[2,1]*Q.matrix[2,1] + Q.matrix[2,2]*Q.matrix[2,2] + Q.matrix[2,3]*Q.matrix[2,3] ;
   if( val > 0.0 ) then begin
     val := 1.0 / sqrt(val) ;
     Q.matrix[2,1] := Q.matrix[2,1]* val ;
     Q.matrix[2,2] := Q.matrix[2,2] * val ;
     Q.matrix[2,3] := Q.matrix[2,3] * val ;
   end else begin
     Q.matrix[2,1] := 0.0 ; Q.matrix[2,2] := 1.0 ; Q.matrix[2,3] := 0.0 ;
   end;
   // normalize row 3
   val := Q.matrix[3,1]*Q.matrix[3,1] + Q.matrix[3,2]*Q.matrix[3,2] + Q.matrix[3,3]*Q.matrix[3,3] ;
   if( val > 0.0 ) then begin
     val := 1.0 / sqrt(val) ;
     Q.matrix[3,1] := Q.matrix[3,1] *val ;
      Q.matrix[3,2] := Q.matrix[3,2] *val ;
      Q.matrix[3,3] := Q.matrix[3,3] *val ;
   end else begin
     Q.matrix[3,1] := Q.matrix[1,2]*Q.matrix[2,3] - Q.matrix[1,3]*Q.matrix[2,2] ;  //* cross */
     Q.matrix[3,2] := Q.matrix[1,3]*Q.matrix[2,1] - Q.matrix[1,1]*Q.matrix[2,3] ;  //* product */
     Q.matrix[3,3] := Q.matrix[1,1]*Q.matrix[2,2] - Q.matrix[1,2]*Q.matrix[2,1] ;
   end;
   //next - find closest orthogonal coordinates - each matrix cell must be 0,-1 or 1
   //First: find axis most aligned to a principal axis
   lAbsmax := 0;
   lMaxRow := 1;
   lMaxCol := 1;
   for lrow := 1 to 3 do begin
       for lcol := 1 to 3 do begin
           lAbs := abs(Q.matrix[lrow,lcol]);
           if lAbs > lAbsMax then begin
              lAbsmax := lAbs;
              lMaxRow := lRow;
              lMaxCol := lCol;
           end;
       end; //for rows
   end; //for columns
   //Second - find find axis that is 2nd closest to principal axis
   lAbsmax := 0;
   l2ndMaxRow := 2;
   l2ndMaxCol := 2;
   for lrow := 1 to 3 do begin
       for lcol := 1 to 3 do begin
           if (lrow <> lMaxRow) and (lCol <> lMaxCol) then begin
              lAbs := abs(Q.matrix[lrow,lcol]);
              if lAbs > lAbsMax then begin
                 lAbsmax := lAbs;
                 l2ndMaxRow := lRow;
                 l2ndMaxCol := lCol;
              end; //new max
           end; //do not check MaxRow/MaxCol
       end; //for rows
   end; //for columns
   //next - no degrees of freedom left: third prinicple axis is the remaining axis
   if ((lMaxRow = 1) or (l2ndMaxRow = 1)) and ((lMaxRow = 2) or (l2ndMaxRow = 2)) then
      l3rdMaxRow := 3
   else if ((lMaxRow = 1) or (l2ndMaxRow = 1)) and ((lMaxRow = 3) or (l2ndMaxRow = 3)) then
        l3rdMaxRow := 2
   else
       l3rdMaxRow := 1;
   if ((lMaxCol = 1) or (l2ndMaxCol = 1)) and ((lMaxCol = 2) or (l2ndMaxCol = 2)) then
      l3rdMaxCol := 3
   else if ((lMaxCol = 1) or (l2ndMaxCol = 1)) and ((lMaxCol = 3) or (l2ndMaxCol = 3)) then
        l3rdMaxCol := 2
   else
       l3rdMaxCol := 1;
   //finally, fill in our rotation matrix
   //cells in the canonical rotation transform can only have values 0,1,-1
   result := Matrix3D( 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0);
   if Q.matrix[lMaxRow,lMaxCol] < 0 then
      result.matrix[lMaxRow,lMaxCol] := -1
   else
       result.matrix[lMaxRow,lMaxCol] := 1;

   if Q.matrix[l2ndMaxRow,l2ndMaxCol] < 0 then
      result.matrix[l2ndMaxRow,l2ndMaxCol] := -1
   else
       result.matrix[l2ndMaxRow,l2ndMaxCol] := 1;

   if Q.matrix[l3rdMaxRow,l3rdMaxCol] < 0 then
      result.matrix[l3rdMaxRow,l3rdMaxCol] := -1
   else
       result.matrix[l3rdMaxRow,l3rdMaxCol] := 1;
end;


FUNCTION QuickInvertMatrix3D  (CONST Input:TMatrix):  TMatrix;
//http://www.cellperformance.com/articles/2006/06/a_4x4_matrix_inverse_1.html
//Most of the time in the video games, programmers are not doing a standard inverse matrix.
//It is too expensive. Instead, to inverse a matrix, they consider it as orthonormal
//and they just do a 3x3 transpose of the rotation part with a dot product for the translation.
//Sometimes the full inverse algorithm is necessary....
var
   i,j: integer;
begin
 result.size := Input.size;
 for i := 1 to 3 do
     for j := 1 to 3 do
         result.matrix[i,j] := input.matrix[j,i];
 //next - fill in edge if 3D
 if result.size <> size3D then
    exit; //do not fill in final column for 2D matrices
 for i := 1 to 3 do
     result.matrix[4,i] := 0;
 for i := 1 to 3 do
     result.matrix[i,4] := 0;
 result.matrix[4,4] := 1;
end;

procedure NearestOrtho(var lHdr: TMRIcroHdr);
var
 lIn,lM: TMatrix;
 qb, qc, qd,
 qx, qy, qz,
 dx, dy, dz, qfac : single;
begin
  if (lHdr.NIFTIhdr.pixdim[1] = 0) or (lHdr.NIFTIhdr.pixdim[2]=0) or (lHdr.NIFTIhdr.pixdim[3]=0) then
     exit;
  lIn := Matrix3D (
    lHdr.NIFTIhdr.srow_x[0],lHdr.NIFTIhdr.srow_x[1],lHdr.NIFTIhdr.srow_x[2],lHdr.NIFTIhdr.srow_x[3],
    lHdr.NIFTIhdr.srow_y[0],lHdr.NIFTIhdr.srow_y[1],lHdr.NIFTIhdr.srow_y[2],lHdr.NIFTIhdr.srow_y[3],
    lHdr.NIFTIhdr.srow_z[0],lHdr.NIFTIhdr.srow_z[1],lHdr.NIFTIhdr.srow_z[2],lHdr.NIFTIhdr.srow_z[3],
    0,0,0,1);
  if EyeMatrix (lIn) then
     exit;
   lM := nifti_mat44_orthogx( lIn);
  lM.matrix[1,4] := lHdr.Mat.matrix[1,4];
  lM.matrix[2,4] := lHdr.Mat.matrix[2,4];
  lM.matrix[3,4] := lHdr.Mat.matrix[3,4];
  lHdr.Mat:= lM;
  //reportmatrix('rx',lIn);
  lM := QuickInvertMatrix3D(lM);
  //reportmatrix('invx',lM);
  lM := multiplymatrices(lIn,lM);
end;     *)

procedure nifti_mat44_to_quatern( lR :TMatrix;
                             var qb, qc, qd,
                             qx, qy, qz,
                             dx, dy, dz, qfac : single);
var
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, xd,yd,zd , a,b,c,d : double;
   P,Q: TMatrix;  //3x3
begin
   (* offset outputs are read write out of input matrix  *)
   qx := lR.matrix[1,4];
   qy := lR.matrix[2,4];
   qz := lR.matrix[3,4];

   (* load 3x3 matrix into local variables *)
   FromMatrix(lR,r11,r12,r13,r21,r22,r23,r31,r32,r33);

   (* compute lengths of each column; these determine grid spacings  *)

   xd := sqrt( r11*r11 + r21*r21 + r31*r31 ) ;
   yd := sqrt( r12*r12 + r22*r22 + r32*r32 ) ;
   zd := sqrt( r13*r13 + r23*r23 + r33*r33 ) ;

   (* if a column length is zero, patch the trouble *)

   if( xd = 0.0 )then begin r11 := 1.0 ; r21 := 0; r31 := 0.0 ; xd := 1.0 ; end;
   if( yd = 0.0 )then begin r22 := 1.0 ; r12 := 0; r32 := 0.0 ; yd := 1.0 ; end;
   if( zd = 0.0 )then begin r33 := 1.0 ; r13 := 0; r23 := 0.0 ; zd := 1.0 ; end;

   (* assign the output lengths *)
   dx := xd;
   dy := yd;
   dz := zd;

   (* normalize the columns *)

   r11 := r11/xd ; r21 := r21/xd ; r31 := r31/xd ;
   r12 := r12/yd ; r22 := r22/yd ; r32 := r32/yd ;
   r13 := r13/zd ; r23 := r23/zd ; r33 := r33/zd ;

   (* At this point, the matrix has normal columns, but we have to allow
      for the fact that the hideous user may not have given us a matrix
      with orthogonal columns.

      So, now find the orthogonal matrix closest to the current matrix.

      One reason for using the polar decomposition to get this
      orthogonal matrix, rather than just directly orthogonalizing
      the columns, is so that inputting the inverse matrix to R
      will result in the inverse orthogonal matrix at this point.
      If we just orthogonalized the columns, this wouldn't necessarily hold. *)
   Q :=  Matrix2D (r11,r12,r13,          // 2D "graphics" matrix
                           r21,r22,r23,
                           r31,r32,r33);



   P := nifti_mat33_polar(Q) ;  (* P is orthog matrix closest to Q *)
   FromMatrix(P,r11,r12,r13,r21,r22,r23,r31,r32,r33);

    //ReportMatrix('xxx',Q);
    //ReportMatrix('svd',P);
   (*                            [ r11 r12 r13 ]               *)
   (* at this point, the matrix  [ r21 r22 r23 ] is orthogonal *)
   (*                            [ r31 r32 r33 ]               *)

   (* compute the determinant to determine if it is proper *)

   zd := r11*r22*r33-r11*r32*r23-r21*r12*r33
       +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;  (* should be -1 or 1 *)

   if( zd > 0 )then begin             (* proper *)
     qfac  := 1.0 ;
   end else begin                  (* improper ==> flip 3rd column *)
     qfac := -1.0 ;
     r13 := -r13 ; r23 := -r23 ; r33 := -r33 ;
   end;

   (* now, compute quaternion parameters *)

   a := r11 + r22 + r33 + 1.0;

   if( a > 0.5 ) then begin                (* simplest case *)
     a := 0.5 * sqrt(a) ;
     b := 0.25 * (r32-r23) / a ;
     c := 0.25 * (r13-r31) / a ;
     d := 0.25 * (r21-r12) / a ;
   end else begin                       (* trickier case *)
     xd := 1.0 + r11 - (r22+r33) ;  (* 4*b*b *)
     yd := 1.0 + r22 - (r11+r33) ;  (* 4*c*c *)
     zd := 1.0 + r33 - (r11+r22) ;  (* 4*d*d *)
     if( xd > 1.0 ) then begin
       b := 0.5 * sqrt(xd) ;
       c := 0.25* (r12+r21) / b ;
       d := 0.25* (r13+r31) / b ;
       a := 0.25* (r32-r23) / b ;
     end else if( yd > 1.0 ) then begin
       c := 0.5 * sqrt(yd) ;
       b := 0.25* (r12+r21) / c ;
       d := 0.25* (r23+r32) / c ;
       a := 0.25* (r13-r31) / c ;
     end else begin
       d := 0.5 * sqrt(zd) ;
       b := 0.25* (r13+r31) / d ;
       c := 0.25* (r23+r32) / d ;
       a := 0.25* (r21-r12) / d ;
     end;
     if( a < 0.0 )then begin b:=-b ; c:=-c ; d:=-d; {a:=-a; this is not used} end;
   end;

   qb := b ;
   qc := c ;
   qd := d ;
   //fx(qb,qc,qd);
end;

{procedure nifti_mat44_to_quatern( lR :TMatrix;
                             var qb, qc, qd,
                             qx, qy, qz,
                             dx, dy, dz, qfac : single);
var
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, xd,yd,zd , a,b,c,d : double;
   P,Q: TMatrix;  //3x3
begin


   (* offset outputs are read write out of input matrix  *)
   qx := lR.matrix[1,4];
   qy := lR.matrix[2,4];
   qz := lR.matrix[3,4];

   (* load 3x3 matrix into local variables *)
   FromMatrix(lR,r11,r12,r13,r21,r22,r23,r31,r32,r33);

   (* compute lengths of each column; these determine grid spacings  *)

   xd := sqrt( r11*r11 + r21*r21 + r31*r31 ) ;
   yd := sqrt( r12*r12 + r22*r22 + r32*r32 ) ;
   zd := sqrt( r13*r13 + r23*r23 + r33*r33 ) ;

   (* if a column length is zero, patch the trouble *)

   if( xd = 0.0 )then begin r11 := 1.0 ; r21 := 0; r31 := 0.0 ; xd := 1.0 ; end;
   if( yd = 0.0 )then begin r22 := 1.0 ; r12 := 0; r32 := 0.0 ; yd := 1.0 ; end;
   if( zd = 0.0 )then begin r33 := 1.0 ; r13 := 0; r23 := 0.0 ; zd := 1.0 ; end;

   (* assign the output lengths *)
   dx := xd;
   dy := yd;
   dz := zd;

   (* normalize the columns *)

   r11 := r11/xd ; r21 := r21/xd ; r31 := r31/xd ;
   r12 := r12/yd ; r22 := r22/yd ; r32 := r32/yd ;
   r13 := r13/zd ; r23 := r23/zd ; r33 := r33/zd ;

   (* At this point, the matrix has normal columns, but we have to allow
      for the fact that the hideous user may not have given us a matrix
      with orthogonal columns.

      So, now find the orthogonal matrix closest to the current matrix.

      One reason for using the polar decomposition to get this
      orthogonal matrix, rather than just directly orthogonalizing
      the columns, is so that inputting the inverse matrix to R
      will result in the inverse orthogonal matrix at this point.
      If we just orthogonalized the columns, this wouldn't necessarily hold. *)
   Q :=  Matrix2D (r11,r12,r13,          // 2D "graphics" matrix
                           r21,r22,r23,
                           r31,r32,r33);



   P := nifti_mat33_polar(Q) ;  (* P is orthog matrix closest to Q *)
   FromMatrix(P,r11,r12,r13,r21,r22,r23,r31,r32,r33);

    //ReportMatrix('xxx',Q);
    //ReportMatrix('svd',P);
   (*                            [ r11 r12 r13 ]               *)
   (* at this point, the matrix  [ r21 r22 r23 ] is orthogonal *)
   (*                            [ r31 r32 r33 ]               *)

   (* compute the determinant to determine if it is proper *)

   zd := r11*r22*r33-r11*r32*r23-r21*r12*r33
       +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;  (* should be -1 or 1 *)

   if( zd > 0 )then begin             (* proper *)
     qfac  := 1.0 ;
   end else begin                  (* improper ==> flip 3rd column *)
     qfac := -1.0 ;
     r13 := -r13 ; r23 := -r23 ; r33 := -r33 ;
   end;

   (* now, compute quaternion parameters *)

   a := r11 + r22 + r33 + 1.0;

   if( a > 0.5 ) then begin                (* simplest case *)
     a := 0.5 * sqrt(a) ;
     b := 0.25 * (r32-r23) / a ;
     c := 0.25 * (r13-r31) / a ;
     d := 0.25 * (r21-r12) / a ;
   end else begin                       (* trickier case *)
     xd := 1.0 + r11 - (r22+r33) ;  (* 4*b*b *)
     yd := 1.0 + r22 - (r11+r33) ;  (* 4*c*c *)
     zd := 1.0 + r33 - (r11+r22) ;  (* 4*d*d *)
     if( xd > 1.0 ) then begin
       b := 0.5 * sqrt(xd) ;
       c := 0.25* (r12+r21) / b ;
       d := 0.25* (r13+r31) / b ;
       a := 0.25* (r32-r23) / b ;
     end else if( yd > 1.0 ) then begin
       c := 0.5 * sqrt(yd) ;
       b := 0.25* (r12+r21) / c ;
       d := 0.25* (r23+r32) / c ;
       a := 0.25* (r13-r31) / c ;
     end else begin
       d := 0.5 * sqrt(zd) ;
       b := 0.25* (r13+r31) / d ;
       c := 0.25* (r23+r32) / d ;
       a := 0.25* (r21-r12) / d ;
     end;
     if( a < 0.0 )then begin b:=-b ; c:=-c ; d:=-d;  end;
   end;

   qb := b ;
   qc := c ;
   qd := d ;
   //fx(qb,qc,qd);
end; }

procedure nifti_quatern_to_mat44( var lR :TMatrix;
                             var qb, qc, qd,
                             qx, qy, qz,
                             dx, dy, dz, qfac : single);
var
   a,b,c,d,xd,yd,zd: double;
begin
   //a := qb;
   b := qb;
   c := qc;
   d := qd;
   //* last row is always [ 0 0 0 1 ] */
   lR.matrix[4,1] := 0;
   lR.matrix[4,2] := 0;
   lR.matrix[4,3] := 0;
   lR.matrix[4,4] := 1;
   //* compute a parameter from b,c,d */
   a := 1.0 - (b*b + c*c + d*d) ;
   if( a < 1.e-7 ) then begin//* special case */
     a := 1.0 / sqrt(b*b+c*c+d*d) ;
     b := b*a ; c := c*a ; d := d*a ;//* normalize (b,c,d) vector */
     a := 0.0 ;//* a = 0 ==> 180 degree rotation */
   end else begin
     a := sqrt(a) ; //* angle = 2*arccos(a) */
   end;
   //* load rotation matrix, including scaling factors for voxel sizes */
   if dx > 0 then
      xd := dx
   else
       xd := 1;
   if dy > 0 then
      yd := dy
   else
       yd := 1;
   if dz > 0 then
      zd := dz
   else
       zd := 1;
   if( qfac < 0.0 ) then zd := -zd ;//* left handedness? */
   lR.matrix[1,1]:=        (a*a+b*b-c*c-d*d) * xd ;
   lR.matrix[1,2]:= 2.0 * (b*c-a*d        ) * yd ;
   lR.matrix[1,3]:= 2.0 * (b*d+a*c        ) * zd ;
   lR.matrix[2,1]:=  2.0 * (b*c+a*d        ) * xd ;
   lR.matrix[2,2]:=        (a*a+c*c-b*b-d*d) * yd ;
   lR.matrix[2,3]:=  2.0 * (c*d-a*b        ) * zd ;
   lR.matrix[3,1]:= 2.0 * (b*d-a*c        ) * xd ;
   lR.matrix[3,2]:=  2.0 * (c*d+a*b        ) * yd ;
   lR.matrix[3,3]:=         (a*a+d*d-c*c-b*b) * zd ;
   //* load offsets */
   lR.matrix[1,4]:= qx ;
   lR.matrix[2,4]:= qy ;
   lR.matrix[3,4]:= qz ;

end;

function TryQuat2Matrix( var lHdr: TNIfTIHdr ): boolean;
var lR :TMatrix;
begin

    result := false;
    if (lHdr.qform_code <= kNIFTI_XFORM_UNKNOWN) or (lHdr.qform_code > kNIFTI_XFORM_MNI_152) then
       exit;
    result := true;
    nifti_quatern_to_mat44(lR,lHdr.quatern_b,lHdr.quatern_c,lHdr.quatern_d,
   lHdr.qoffset_x,lHdr.qoffset_y,lHdr.qoffset_z,
   lHdr.pixdim[1],lHdr.pixdim[2],lHdr.pixdim[3],
   lHdr.pixdim[0]);
   lHdr.srow_x[0] := lR.matrix[1,1];
   lHdr.srow_x[1] := lR.matrix[1,2];
   lHdr.srow_x[2] := lR.matrix[1,3];
   lHdr.srow_x[3] := lR.matrix[1,4];
   lHdr.srow_y[0] := lR.matrix[2,1];
   lHdr.srow_y[1] := lR.matrix[2,2];
   lHdr.srow_y[2] := lR.matrix[2,3];
   lHdr.srow_y[3] := lR.matrix[2,4];
   lHdr.srow_z[0] := lR.matrix[3,1];
   lHdr.srow_z[1] := lR.matrix[3,2];
   lHdr.srow_z[2] := lR.matrix[3,3];
   lHdr.srow_z[3] := lR.matrix[3,4];
		lHdr.sform_code := 1;
end;

{procedure ReportMatrix (lM:TMatrix);
var lStr: string;
begin

	lStr := (	RealToStr(lM.matrix[1,1],6)+','+RealToStr(lM.matrix[1,2],6)+','+RealToStr(lM.matrix[1,3],6)+','+RealToStr(lM.matrix[1,4],6))
	+kCR+(	RealToStr(lM.matrix[2,1],6)+','+RealToStr(lM.matrix[2,2],6)+','+RealToStr(lM.matrix[2,3],6)+','+RealToStr(lM.matrix[2,4],6))
	+kCR+(	RealToStr(lM.matrix[3,1],6)+','+RealToStr(lM.matrix[3,2],6)+','+RealToStr(lM.matrix[3,3],6)+','+RealToStr(lM.matrix[3,4],6))
	+kCR+(	RealToStr(lM.matrix[4,1],6)+','+RealToStr(lM.matrix[4,2],6)+','+RealToStr(lM.matrix[4,3],6)+','+RealToStr(lM.matrix[4,4],6));
showmessage(lStr);
end; }

function FixDataType (var lHdr: TMRIcroHdr {; lCompress: boolean}): boolean;
//correct mistakes of datatype and bitpix - especially for software which only sets one
label
  191;
var
  ldatatypebpp,lbitpix: integer;
begin
  result := true;
  lbitpix := lHdr.NIFTIhdr.bitpix;
  case lHdr.NIFTIhdr.datatype of
    kDT_BINARY : ldatatypebpp := 1;
    kDT_UNSIGNED_CHAR  : ldatatypebpp := 8;     // unsigned char (8 bits/voxel)
    kDT_SIGNED_SHORT  : ldatatypebpp := 8;      // signed short (16 bits/voxel)
    kDT_SIGNED_INT : ldatatypebpp := 32;      // signed int (32 bits/voxel)
    kDT_FLOAT : ldatatypebpp := 32;      // float (32 bits/voxel)
    kDT_COMPLEX : ldatatypebpp := 64;      // complex (64 bits/voxel)
    kDT_DOUBLE  : ldatatypebpp := 64;      // double (64 bits/voxel)
    kDT_RGB : ldatatypebpp := 24;      // RGB triple (24 bits/voxel)
    kDT_INT8 : ldatatypebpp := 8;     // signed char (8 bits)
    kDT_UINT16 : ldatatypebpp := 16;      // unsigned short (16 bits)
    kDT_UINT32 : ldatatypebpp := 32;     // unsigned int (32 bits)
    kDT_INT64 : ldatatypebpp := 64;     // long long (64 bits)
    kDT_UINT64 : ldatatypebpp := 64;     // unsigned long long (64 bits)
    kDT_FLOAT128 : ldatatypebpp := 128;     // long double (128 bits)
    kDT_COMPLEX128 : ldatatypebpp := 128;   // double pair (128 bits)
    kDT_COMPLEX256 : ldatatypebpp := 256;     // long double pair (256 bits)
    else
      ldatatypebpp := 0;
  end;
  if (ldatatypebpp = lHdr.NIFTIhdr.bitpix) and (ldatatypebpp <> 0) then
    exit;
  if (lbitpix = 0) and (ldatatypebpp <> 0) then begin
    //use bitpix from datatype...
    lHdr.NIFTIhdr.bitpix := ldatatypebpp;
    exit;
  end;
  if (lbitpix <> 0) and (ldatatypebpp = 0) then begin
    //assume bitpix is correct....
    //note that several datatypes correspond to each bitpix, so assume most popular...
    case lbitpix of
      1: lHdr.NIFTIhdr.datatype := kDT_BINARY;
      8: lHdr.NIFTIhdr.datatype :=  kDT_UNSIGNED_CHAR;
      16: lHdr.NIFTIhdr.datatype := kDT_SIGNED_SHORT;
      24: lHdr.NIFTIhdr.datatype :=     kDT_RGB;
      32: lHdr.NIFTIhdr.datatype :=     kDT_FLOAT;
      64: lHdr.NIFTIhdr.datatype := kDT_DOUBLE;
      else goto 191; //impossible bitpix
    end;
    exit;
  end;
191:
  //ComputeImageDataBytes(lHdr);
  lHdr.NIFTIhdr.bitpix := 16;
  lHdr.NIFTIhdr.datatype := kDT_SIGNED_SHORT;
  //fx(lHdr.NIFTIhdr.bitpix, lHdr.NIFTIhdr.datatype);
end;

function NIFTIhdr_LoadHdr (var lFilename: string; var lHdr: TMRIcroHdr): boolean;
var
  lHdrFile: file;
  lOri: array [1..3] of single;
  lBuff: Bytep;
  lAHdr: TAnalyzeHdrSection;
  swapEndian: boolean;
  lTemp,lReportedSz, lSwappedReportedSz,lHdrSz,lFileSz: Longint;
  lExt: string; //1494
begin
  Result := false; //assume error
  if lFilename = '' then exit;
  lExt := UpCaseExt(lFilename);
  if lExt = '.IMG' then
	  lFilename := changeFileExt(lFilename,'.hdr');
  if (lExt = '.BRIK') or (lExt = '.BRIK.GZ') then 
	  lFilename := changeFileExtX(lFilename,'.HEAD');

  lExt := UpCaseExt(lFilename);
  lHdrSz := sizeof(TniftiHdr);
  lFileSz := FSize (lFilename);
  if lFileSz = 0 then begin
	  ShowMessage('Unable to find NIFTI header named '+lFilename);
	  exit;
  end;
  swapEndian := false;
  lHdr.gzBytesX := K_gzBytes_headerAndImageUncompressed;
  lHdr.HdrFileName:= lFilename;
  lHdr.ImgFileName:= lFilename;
  FileMode := 0;  { Set file access to read only }
  if (lExt = '.MGH') or (lExt = '.MGZ') or (lExt = '.MHD') or (lExt = '.MHA') or (lExt = '.NRRD') or (lExt = '.NHDR') or (lExt = '.HEAD') then begin
    result := readForeignHeader( lFilename, lHdr.NIFTIhdr,lHdr.gzBytesX, swapEndian);  //we currently ignore result!
    lHdr.ImgFileName := lFilename;
  end else begin //native NIfTI
    if (lExt = '.NII.GZ') or (lExt = '.VOI') then begin//1388
      lBuff := @lHdr;
      UnGZip(lFileName,lBuff,0,lHdrSz); //1388
      lHdr.gzBytesX := K_gzBytes_headerAndImageCompressed;
    end else begin //if gzip else uncompressed
        if (lFileSz < lHdrSz)  then begin
          ShowMessage('Error in reading NIFTI header: NIfTI headers need to be at least '+inttostr(lHdrSz)+ ' bytes: '+lFilename);
          result := false;
        end else begin
          {$I-}
          AssignFile(lHdrFile, lFileName);
          FileMode := 0;  { Set file access to read only }
          Reset(lHdrFile, 1);
          {$I+}
          if ioresult <> 0 then begin
            ShowMessage('Error in reading NIFTI header.'+inttostr(IOResult));
            CloseFile(lHdrFile);
            FileMode := 2;
            exit;
          end;
          BlockRead(lHdrFile, lHdr, lHdrSz);
          CloseFile(lHdrFile);
        end;
    end;
    if ((lHdr.niftiHdr.magic = kNIFTI_MAGIC_EMBEDDED_HDR) and (lFileSz > lHdrSz)) or (lExt = '.NII.GZ') or (lExt = '.VOI') or (lExt = '.NII'){1494} then
	    lHdr.ImgFileName:= lFilename
    else
	    lHdr.ImgFileName:= changefileext(lFilename,'.img');
  end; //native NIFTI
  FileMode := 2;
  if (IOResult <> 0) then exit;
  lReportedSz := lHdr.niftiHdr.HdrSz;
  lSwappedReportedSz := lReportedSz;
  swap4(lSwappedReportedSz);
  if lReportedSz = lHdrSz then begin
	 lHdr.DiskDataNativeEndian := true;
  end else if lSwappedReportedSz = lHdrSz then begin
	  lHdr.DiskDataNativeEndian := false;
	  NIFTIhdr_SwapBytes (lHdr.niftiHdr);
  end else begin
    result := NIFTIhdr_LoadDCM (lFilename,lHdr); //2/2008
    if not result then
	     ShowMessage('Warning: the header file is not in NIfTi format [the first 4 bytes do not have the value 348].');
	  exit;
  end;

  if (lHdr.NIFTIhdr.dim[0] > 7) or (lHdr.NIFTIhdr.dim[0] < 1) then begin //only 1..7 dims, so this
	  Showmessage('Illegal NIfTI Format Header: this header does not specify 1..7 dimensions.');
	  exit;
  end;
  FixDataType (lHdr{,lCompress});
  result := true;
  if  IsNifTiMagic(lHdr.niftiHdr) then begin  //must match MAGMA in nifti_img
	 lOri[1] := (lHdr.NIFTIhdr.dim[1]+1) div 2;
	 lOri[2] := (lHdr.NIFTIhdr.dim[2]+1) div 2;
	 lOri[3] := (lHdr.NIFTIhdr.dim[3]+1) div 2;
        //TryQuat2Matrix(lHdr.NiftiHdr);
	 if  (lHdr.NIFTIhdr.sform_code <= kNIFTI_XFORM_UNKNOWN) or (lHdr.NIFTIhdr.sform_code > kNIFTI_XFORM_MNI_152) then
		TryQuat2Matrix(lHdr.NiftiHdr);
         if emptymatrix(lHdr) then begin

                 (*if HasQuat(lHdr.NiftiHdr) then
                     //HasQuat will specify
                 else*) begin
                        for lTemp := 1 to 3 do //Sept 2008
                          if lHdr.NIFTIhdr.pixdim[lTemp] = 0 then begin
                            lHdr.NIFTIhdr.pixdim[lTemp] := 1;
                          end;
                        lHdr.NIFTIhdr.srow_x[0] := lHdr.NIFTIhdr.pixdim[1];
                        lHdr.NIFTIhdr.srow_x[1] := 0;
                        lHdr.NIFTIhdr.srow_x[2] := 0;

                        lHdr.NIFTIhdr.srow_y[0] := 0;
                        lHdr.NIFTIhdr.srow_y[1] := lHdr.NIFTIhdr.pixdim[2];
                        lHdr.NIFTIhdr.srow_y[2] := 0;
                        lHdr.NIFTIhdr.srow_z[0] := 0;
                        lHdr.NIFTIhdr.srow_z[1] := 0;
                        lHdr.NIFTIhdr.srow_z[2] := lHdr.NIFTIhdr.pixdim[3];

			                  lHdr.NIFTIhdr.srow_x[3] := -round(lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.pixdim[1]*0.5);
			                  lHdr.NIFTIhdr.srow_y[3] := -round(lHdr.NIFTIhdr.dim[2]*lHdr.NIFTIhdr.pixdim[2]*0.5);
			                  lHdr.NIFTIhdr.srow_z[3] := -round(lHdr.NIFTIhdr.dim[3]*lHdr.NIFTIhdr.pixdim[3]*0.5);
		                    lHdr.NIFTIhdr.sform_code := 1;
                end;
         end;


	 if (lHdr.NIFTIhdr.srow_x[0] > 0) and (lHdr.NIFTIhdr.srow_y[1] > 0) and (lHdr.NIFTIhdr.srow_z[2] > 0) and
		(lHdr.NIFTIhdr.srow_x[3] > 0) and (lHdr.NIFTIhdr.srow_y[3] > 0) and (lHdr.NIFTIhdr.srow_z[3] > 0) then begin
			lHdr.NIFTIhdr.srow_x[3] := -lHdr.NIFTIhdr.srow_x[3];
			lHdr.NIFTIhdr.srow_y[3] := -lHdr.NIFTIhdr.srow_y[3];
			lHdr.NIFTIhdr.srow_z[3] := -lHdr.NIFTIhdr.srow_z[3];
		lHdr.NIFTIhdr.sform_code := 1;
	 end; //added 4Mar2006 -> corrects for improperly signed offset values...

  end else begin //not NIFT: Analyze
    lHdr.NIfTItransform := false;//Analyze
	  if not lHdr.DiskDataNativeEndian then begin
		NIFTIhdr_SwapBytes (lHdr.niftiHdr);
		move(lHdr.niftiHdr,lAHdr,sizeof(lAHdr));
		NIFTIhdr_SwapBytes (lHdr.niftiHdr);
		lAHdr.Originator[1] := swap(lAHdr.Originator[1]);
		lAHdr.Originator[2] := swap(lAHdr.Originator[2]);
		lAHdr.Originator[3] := swap(lAHdr.Originator[3]);
	  end else
		 move(lHdr.niftiHdr,lAHdr,sizeof(lAHdr));
	  lOri[1] :=lAHdr.Originator[1];
	  lOri[2] := lAHdr.Originator[2];
	  lOri[3] := lAHdr.Originator[3];
          if (lOri[1]=76) and (lOri[2]=116) and (lOri[3]=64)
             and (lHdr.NIFTIhdr.dim[1]=151) and (lHdr.NIFTIhdr.dim[2]=188) and (lHdr.NIFTIhdr.dim[3]=154) then begin
              lOri[2] := 111;
              lOri[3] := 68;
          end; //2/2008 Juelich fudge factor

          if ((lOri[1]<1) or (lOri[1]> lHdr.NIFTIhdr.dim[1])) and
            ((lOri[2]<1) or (lOri[2]> lHdr.NIFTIhdr.dim[2])) and
            ((lOri[3]<1) or (lOri[3]> lHdr.NIFTIhdr.dim[3])) then begin
	     lOri[1] := (lHdr.NIFTIhdr.dim[1]+1) / 2; //May07 use / not div
	     lOri[2] := (lHdr.NIFTIhdr.dim[2]+1) / 2; //May07 use / not div
	     lOri[3] := (lHdr.NIFTIhdr.dim[3]+1) / 2; //May07 use / not div : if 20 slices, then origin is between 10 and 11

          end;
	  //showmessage(inttostr(sizeof(lAHdr))+'  '+realtostr(lHdr.Ori[1],1)+' '+ realtostr(lHdr.Ori[2],1)+' '+realtostr(lHdr.Ori[3],1) );
	  //DANGER: This header was from ANALYZE format, not NIFTI: make sure the rotation matrix is switched off
	  NII_SetIdentityMatrix(lHdr.NIFTIhdr);
	  lHdr.NIFTIhdr.qform_code := kNIFTI_XFORM_UNKNOWN;
	  lHdr.NIFTIhdr.sform_code := kNIFTI_XFORM_UNKNOWN;
                        for lTemp := 1 to 3 do //Sept 2008
                          if lHdr.NIFTIhdr.pixdim[lTemp] = 0 then begin
                            lHdr.NIFTIhdr.pixdim[lTemp] := 1;
                          end;

          //test - input estimated orientation matrix
          lHdr.NIFTIhdr.sform_code := kNIFTI_XFORM_SCANNER_ANAT ;
          lHdr.NIFTIhdr.srow_x[0] := lHdr.NIFTIhdr.pixdim[1];
          lHdr.NIFTIhdr.srow_y[1] := lHdr.NIFTIhdr.pixdim[2];
          lHdr.NIFTIhdr.srow_z[2] := lHdr.NIFTIhdr.pixdim[3];

			lHdr.NIFTIhdr.srow_x[3] := (lOri[1]-1)*-lHdr.NIFTIhdr.pixdim[1];
			lHdr.NIFTIhdr.srow_y[3] := (lOri[2]-1)*-lHdr.NIFTIhdr.pixdim[2];
			lHdr.NIFTIhdr.srow_z[3] := (lOri[3]-1)*-lHdr.NIFTIhdr.pixdim[3];
                        //fx(lHdr.NIFTIhdr.srow_z[3],lOri[3]);
          //end test
	  //Warning: some of the NIFTI float values that do exist as integer values in Analyze may have bizarre values like +INF, -INF, NaN
	  lHdr.NIFTIhdr.toffset := 0;
	  lHdr.NIFTIhdr.intent_code := kNIFTI_INTENT_NONE;
	  lHdr.NIFTIhdr.dim_info := kNIFTI_SLICE_SEQ_UNKNOWN + (kNIFTI_SLICE_SEQ_UNKNOWN shl 2) + (kNIFTI_SLICE_SEQ_UNKNOWN shl 4); //Freq, Phase and Slie all unknown
	  lHdr.NIFTIhdr.xyzt_units := kNIFTI_UNITS_UNKNOWN;
	  lHdr.NIFTIhdr.slice_duration := 0; //avoid +inf/-inf, NaN
	  lHdr.NIFTIhdr.intent_p1 := 0;  //avoid +inf/-inf, NaN
	  lHdr.NIFTIhdr.intent_p2 := 0;  //avoid +inf/-inf, NaN
	  lHdr.NIFTIhdr.intent_p3 := 0;  //avoid +inf/-inf, NaN
	  lHdr.NIFTIhdr.pixdim[0] := 1; //QFactor should be 1 or -1
  end;
  if (lHdr.NIFTIhdr.sform_code > kNIFTI_XFORM_UNKNOWN) and (lHdr.NIFTIhdr.sform_code <= kNIFTI_XFORM_MNI_152) then begin //DEC06
	lHdr.Mat:= Matrix3D(
		lHdr.NIFTIhdr.srow_x[0],lHdr.NIFTIhdr.srow_x[1],lHdr.NIFTIhdr.srow_x[2],lHdr.NIFTIhdr.srow_x[3],      // 3D "graphics" matrix
		lHdr.NIFTIhdr.srow_y[0],lHdr.NIFTIhdr.srow_y[1],lHdr.NIFTIhdr.srow_y[2],lHdr.NIFTIhdr.srow_y[3],      // 3D "graphics" matrix
		lHdr.NIFTIhdr.srow_z[0],lHdr.NIFTIhdr.srow_z[1],lHdr.NIFTIhdr.srow_z[2],lHdr.NIFTIhdr.srow_z[3],      // 3D "graphics" matrix
		0,0,0,1);
  end else begin
	lHdr.Mat:= Matrix3D(
		lHdr.NIFTIhdr.pixdim[1],0,0,(lOri[1]-1)*-lHdr.NIFTIhdr.pixdim[1],      // 3D "graphics" matrix
		0,lHdr.NIFTIhdr.pixdim[2],0,(lOri[2]-1)*-lHdr.NIFTIhdr.pixdim[2],      // 3D "graphics" matrix
		0,0,lHdr.NIFTIhdr.pixdim[3],(lOri[3]-1)*-lHdr.NIFTIhdr.pixdim[3],      // 3D "graphics" matrix
		0,0,0,1);
  end;

  FixCrapMat(lHdr.Mat);
  if swapEndian then
    lHdr.DiskDataNativeEndian := false;//foreign data with swapped image data
  //ReportMatrix(lHdr.mat);
end; //func NIFTIhdr_LoadHdr

procedure NIFTIhdr_ClearHdr (var lHdr: TMRIcroHdr); //put sensible default values into header
begin
  lHdr.UsesCustomPalette := false;
  lHdr.DiskDataNativeEndian := true;
  lHdr.gzBytesX := K_gzBytes_headerAndImageUncompressed;
   lHdr.UsesCustomPaletteRandomRainbow := false;

  //lHdr.LUTinvert := false;
  lHdr.LutFromZero := false;
  lHdr.NIfTItransform := true;//assume genuine NIfTI, not Analyze
  NII_Clear( lHdr.NIFTIhdr);
  with lHdr do begin
    usesLabels := false;
    ScrnBufferItems := 0;
    ImgBufferItems := 0;
    ImgBufferBPP := 0;
    RenderBufferItems := 0;
    ScrnBuffer:= nil;
    ImgBuffer := nil;
  end;
end; //proc NIFTIhdr_ClearHdr

function NIFTIhdr_SaveHdr (var lFilename: string; var lHdr: TMRIcroHdr; lAllowOverwrite: boolean): boolean;
var lOutHdr: TNIFTIhdr;
	lExt: string;
    lF: File;
    lOverwrite: boolean;
begin
     lOverwrite := false; //will we overwrite existing file?
     result := false; //assume failure
	 if lHdr.NIFTIhdr.magic = kNIFTI_MAGIC_EMBEDDED_HDR then begin
		 lExt := UpCaseExt(lFileName);
		 if (lExt = '.GZ') or (lExt = '.NII.GZ') then begin
			showmessage('Unable to save .nii.gz headers (first ungzip your image if you wish to edit the header)');
			exit;
		 end;
		 lFilename := changefileext(lFilename,'.nii')
	 end else
         lFilename := changefileext(lFilename,'.hdr');
     if ((sizeof(TNIFTIhdr))> DiskFreeEx(lFileName)) then begin
        ShowMessage('There is not enough free space on the destination disk to save the header. '+kCR+
        lFileName+ kCR+' Bytes Required: '+inttostr(sizeof(TNIFTIhdr)) );
        exit;
     end;
     if Fileexists(lFileName) then begin
         if lAllowOverwrite then begin
            case MessageDlg('Do you wish to modify the existing file '+lFilename+'?', mtConfirmation,[mbYes, mbNo], 0) of	{ produce the message dialog box }
             6: lOverwrite := true; //6= mrYes, 7=mrNo... not sure what this is for Linux. Hardcoded as we do not include Form values
        end;//case
         end else
             showmessage('Error: the file '+lFileName+' already exists.');
         if not lOverwrite then Exit;
	 end;
     if lHdr.NIFTIhdr.magic = kNIFTI_MAGIC_EMBEDDED_HDR then
        if lHdr.NIFTIhdr.vox_offset < sizeof(TNIFTIHdr) then
           lHdr.NIFTIhdr.vox_offset := sizeof(TNIFTIHdr); //embedded images MUST start after header
     if lHdr.NIFTIhdr.magic = kNIFTI_MAGIC_SEPARATE_HDR then
           lHdr.NIFTIhdr.vox_offset := 0; //embedded images MUST start after header
     result := true;
     move(lHdr.NIFTIhdr, lOutHdr, sizeof(lOutHdr));
     if lHdr.DiskDataNativeEndian = false then
        NIFTIhdr_SwapBytes (lOutHdr);{swap to big-endianformat}
     Filemode := 1;
     AssignFile(lF, lFileName); {WIN}
     if lOverwrite then //this allows us to modify just the 348byte header of an existing NII header without touching image data
         Reset(lF,sizeof(TNIFTIhdr))
     else
         Rewrite(lF,sizeof(TNIFTIhdr));
     BlockWrite(lF,lOutHdr, 1  {, NumWritten});
     CloseFile(lF);
     Filemode := 2;
end; //func NIFTIhdr_SaveHdr

procedure NIFTIhdr_SwapBytes (var lAHdr: TNIFTIhdr); //Swap Byte order for the Analyze type
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
{roi scale = 1}
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
		 for lInc := 0 to 3 do //alpha
			 Xswap4r(srow_x[lInc]);
		 for lInc := 0 to 3 do //alpha
			 Xswap4r(srow_y[lInc]);
		 for lInc := 0 to 3 do //alpha
             Xswap4r(srow_z[lInc]);
    end; //with NIFTIhdr
end; //proc NIFTIhdr_SwapBytes

end.
 