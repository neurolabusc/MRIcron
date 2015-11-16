unit ortho_reorient;
//reorient image to nearest orthogonal plane
interface

uses
  SysUtils,define_types,GraphicsMathLibrary,prefs,nifti_hdr,dialogs, nifti_types;

function OrthoReorientCore(var lHdr: TMRIcroHdr; l4D: boolean): boolean;

implementation


function NIfTIAlignedM (var lM: TMatrix): boolean;
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


function NIfTIAligned (var lHdr: TNIFTIhdr): boolean;
//check that diagonals are positive and all other cells are zero
//negative diagonals suggests flipping...
//non-negative other cells suggests the image is not pure axial
var
   lM: TMatrix;
begin
    lM := Matrix3D (
    lHdr.srow_x[0],lHdr.srow_x[1],lHdr.srow_x[2],lHdr.srow_x[3],
    lHdr.srow_y[0],lHdr.srow_y[1],lHdr.srow_y[2],lHdr.srow_y[3],
    lHdr.srow_z[0],lHdr.srow_z[1],lHdr.srow_z[2],lHdr.srow_z[3],
    0,0,0,1);
    result := NIfTIAlignedM(lM);
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

function nifti_mat44_orthogx( lR :TMatrix): TMatrix;
//returns rotation matrix required to orient image so it is aligned nearest to the identity matrix =
// 1 0 0 0
// 0 1 0 0
// 0 0 1 0
// 0 0 0 1
//Therefore, image is approximately oriented in space
var
   lrow,lcol,lMaxRow,lMaxCol,l2ndMaxRow,l2ndMaxCol,l3rdMaxRow,l3rdMaxCol: integer;
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

procedure FindMatrixPt (lX,lY,lZ: single; var lXout,lYOut,lZOut: single; var lMatrix: TMatrix);
begin
	lXOut := (lX*lMatrix.matrix[1,1])+(lY*lMatrix.matrix[1,2])+(lZ*lMatrix.matrix[1,3])+lMatrix.matrix[1,4];
	lYOut := (lX*lMatrix.matrix[2,1])+(lY*lMatrix.matrix[2,2])+(lZ*lMatrix.matrix[2,3])+lMatrix.matrix[2,4];
	lZOut := (lX*lMatrix.matrix[3,1])+(lY*lMatrix.matrix[3,2])+(lZ*lMatrix.matrix[3,3])+lMatrix.matrix[3,4];
end;

procedure CheckMin(var lX,lY,lZ,lXMin,lYMin,lZMin: single);
begin
	if lX < lXMin then lXMin := lX;
	if lY < lYMin then lYMin := lY;
	if lZ < lZMin then lZMin := lZ;
end;

procedure Mins (var lMatrix: TMatrix; var lHdr: TNIFTIhdr; var lXMin,lYMin,lZMin: single);
var
   lPos,lXc,lYc,lZc: integer;
   lx,ly,lz: single;
begin
  FindMatrixPt(0,0,0,lX,lY,lZ,lMatrix);
  lXMin := lX;
  lYMin := lY;
  lZMin := lZ;
  for lPos := 1 to 7 do begin
	if odd(lPos) then
		lXc := lHdr.Dim[1]-1
	else
		lXc := 0;
	if odd(lPos shr 1) then
		lYc := lHdr.Dim[2]-1
	else
		lYc := 0;
	if odd(lPos shr 2) then
		lZc := lHdr.Dim[3]-1
	else
		lZc := 0;
	FindMatrixPt(lXc,lYc,lZc,lX,lY,lZ,lMatrix);
	CheckMin(lX,lY,lZ,lXMin,lYMin,lZMin);
  end;
end;

(*procedure ReportMatrix (lM:TMatrix);
const
	kCR = chr (13);
begin
	showmessage(RealToStr(lM.matrix[1,1],6)+','+RealToStr(lM.matrix[1,2],6)+','+RealToStr(lM.matrix[1,3],6)+','+RealToStr(lM.matrix[1,4],6)+kCR+
		RealToStr(lM.matrix[2,1],6)+','+RealToStr(lM.matrix[2,2],6)+','+RealToStr(lM.matrix[2,3],6)+','+RealToStr(lM.matrix[2,4],6)+kCR+
		RealToStr(lM.matrix[3,1],6)+','+RealToStr(lM.matrix[3,2],6)+','+RealToStr(lM.matrix[3,3],6)+','+RealToStr(lM.matrix[3,4],6)+kCR
    +RealToStr(lM.matrix[4,1],6)+','+RealToStr(lM.matrix[4,2],6)+','+RealToStr(lM.matrix[4,3],6)+','+RealToStr(lM.matrix[4,4],6)
	  );
end;*)

function OrthoReorientCore(var lHdr: TMRIcrohdr; l4D: boolean): boolean;
var
   //lF: File;
   lOutHdr: TNIFTIhdr;
   lOutName: string;
   lResidualMat: TMatrix;
   lInMinX,lInMinY,lInMinZ,lOutMinX,lOutMinY,lOutMinZ,
   dx, dy, dz: single;  //, QFac
   //lStartY,lStartZ,
   lStartX,lZ,lY,lX,lB,
   lOutZ,lOutY,
   lXInc, lYInc, lZInc,lBPP,lVol,lnVol: integer;
   lInPos,lVolBytes,lOutPos,lInOffset: integer;
   lBufferOut: bytep;
   lByteSwap,lFlipX,lFlipY,lFlipZ: boolean;
   lInMat,lRotMat: TMatrix;
begin
   result := false;
   if {(lhdr.NIfTIhdr.dim[4] > 1) or} (lhdr.NIfTIhdr.dim[3] < 2) then begin
      //Showmessage('Can only orient 3D images '+inttostr(lhdr.NIfTIhdr.dim[3])+' '+inttostr(lhdr.NIfTIhdr.dim[4]));
      exit;
   end;
   if (lHdr.ImgBufferItems < lhdr.NIfTIhdr.dim[1]*lhdr.NIfTIhdr.dim[2]*lhdr.NIfTIhdr.dim[3]) then
    exit;
   //Msg(lHdrName);
   lInMat := Matrix3D (
    lhdr.NIfTIhdr.srow_x[0],lhdr.NIfTIhdr.srow_x[1],lhdr.NIfTIhdr.srow_x[2],lhdr.NIfTIhdr.srow_x[3],
    lhdr.NIfTIhdr.srow_y[0],lhdr.NIfTIhdr.srow_y[1],lhdr.NIfTIhdr.srow_y[2],lhdr.NIfTIhdr.srow_y[3],
    lhdr.NIfTIhdr.srow_z[0],lhdr.NIfTIhdr.srow_z[1],lhdr.NIfTIhdr.srow_z[2],lhdr.NIfTIhdr.srow_z[3],
    0,0,0,1);
   //ReportMatrix(lInMat);
   if (NIfTIAlignedM (lInMat)) then begin
     //Msg('According to header, image is already canonically oriented');
     exit;
   end;
   lRotMat := nifti_mat44_orthogx( lInMat);
   if NIfTIAlignedM (lRotMat) then begin
     //Msg('According to header, image is already approximately canonically oriented');
     exit; //already as close as possible
   end;
   lOutHdr := lHdr.NIFTIhdr;
   //Some software uses negative pixdims to represent a spatial flip - now that the image is canonical, all dimensions are positive
   lOutHdr.pixdim[1] := abs(lhdr.NIfTIhdr.pixdim[1]);
   lOutHdr.pixdim[2] := abs(lhdr.NIfTIhdr.pixdim[2]);
   lOutHdr.pixdim[3] := abs(lhdr.NIfTIhdr.pixdim[3]);
   //sort out dim1
   lFlipX := false;
   if lRotMat.Matrix[1,2] <> 0 then begin
       lXinc := lhdr.NIfTIhdr.dim[1];
       lOutHdr.dim[1] := lhdr.NIfTIhdr.dim[2];
       lOutHdr.pixdim[1] := abs(lhdr.NIfTIhdr.pixdim[2]);
       if lRotMat.Matrix[1,2] < 0 then lFlipX := true
   end else if lRotMat.Matrix[1,3] <> 0 then begin
       lXinc := lhdr.NIfTIhdr.dim[1]*lhdr.NIfTIhdr.dim[2];
       lOutHdr.dim[1] := lhdr.NIfTIhdr.dim[3];
       lOutHdr.pixdim[1] := abs(lhdr.NIfTIhdr.pixdim[3]);
       if lRotMat.Matrix[1,3] < 0 then lFlipX := true
   end else begin
       lXinc := 1;
       if lRotMat.Matrix[1,1] < 0 then lFlipX := true
   end;
   //sort out dim2
   lFlipY := false;
   if lRotMat.Matrix[2,2] <> 0 then begin
       lYinc := lhdr.NIfTIhdr.dim[1];
       //lOutHdr.dim[2] := lhdr.NIfTIhdr.dim[2];
       //lOutHdr.pixdim[2] := lhdr.NIfTIhdr.pixdim[2];
       if lRotMat.Matrix[2,2] < 0 then lFlipY := true
   end else if lRotMat.Matrix[2,3] <> 0 then begin
       lYinc := lhdr.NIfTIhdr.dim[1]*lhdr.NIfTIhdr.dim[2];
       lOutHdr.dim[2] := lhdr.NIfTIhdr.dim[3];
       lOutHdr.pixdim[2] := abs(lhdr.NIfTIhdr.pixdim[3]);
       if lRotMat.Matrix[2,3] < 0 then lFlipY := true
   end else begin
       lYinc := 1;
       lOutHdr.dim[2] := lhdr.NIfTIhdr.dim[1];
       lOutHdr.pixdim[2] := abs(lhdr.NIfTIhdr.pixdim[1]);
       if lRotMat.Matrix[2,1] < 0 then lFlipY := true
   end;
   //sort out dim3
   lFlipZ := false;
   if lRotMat.Matrix[3,2] <> 0 then begin
       lZinc := lhdr.NIfTIhdr.dim[1];
       lOutHdr.dim[3] := lhdr.NIfTIhdr.dim[2];
       lOutHdr.pixdim[3] := lhdr.NIfTIhdr.pixdim[2];
       if lRotMat.Matrix[3,2] < 0 then lFlipZ := true;
   end else if lRotMat.Matrix[3,3] <> 0 then begin
       lZinc := lhdr.NIfTIhdr.dim[1]*lhdr.NIfTIhdr.dim[2];
       //lOutHdr.dim[3] := lhdr.NIfTIhdr.dim[3];
       //lOutHdr.pixdim[3] := lhdr.NIfTIhdr.pixdim[3];
       if lRotMat.Matrix[3,3] < 0 then lFlipZ := true;
   end else begin
       lZinc := 1;
       lOutHdr.dim[3] := lhdr.NIfTIhdr.dim[1];
       lOutHdr.pixdim[3] := lhdr.NIfTIhdr.pixdim[1];
       if lRotMat.Matrix[3,1] < 0 then lFlipZ := true;
   end;
   //details for writing...
   lBPP := (lhdr.NIfTIhdr.bitpix div 8); //bytes per pixel
   if lBPP > 4 then
    lBPP := 4;//64bit data is stored as 32-bit precision June 2009
   lXinc := lXinc * lBPP;
   lYinc := lYinc * lBPP;
   lZinc := lZinc * lBPP;
   lVolBytes := lhdr.NIfTIhdr.dim[1]*lhdr.NIfTIhdr.dim[2]*lhdr.NIfTIhdr.dim[3]*lBPP;
   //now write header...
   //create Matrix of residual orientation...
  lResidualMat := QuickInvertMatrix3D(lRotMat);
  //the next steps are inelegant - the translation values are computed by brute force
  //at the moment, our lResidualMat looks like this
  //lResidualMat  =  [ 0  -1  0  0; 0  0 1 0; 1  0 0  0; 0 0 0 1];
  //however, it should specify the dimensions in mm of the dimensions that are flipped
  //However, note that whenever you reverse the direction of
  //voxel coordinates, you need to include the appropriate offset
  //in the 'a' matrix.  That is:
  //lResidualMat = [0 0 1 0; -1 0 0 Nx-1; 0 1 0 0; 0 0 0 1]
  //where Nx is the number of voxels in the x direction.
  //So, if you took Nx=256, then for your values before, you'd get:
  //TransRot  =  [ 0  -1  0  255; 0  0 1 0; 1  0 0  0; 0 0 0 1];
  //Because we do not do this, we use the function mins to compute the translations...
  //I have not implemented refined version yet - require sample volumes to check
  //Ensure Nx is voxels not mm, etc....
  //start of kludge
  lResidualMat := multiplymatrices(lInMat,lResidualMat); //source
  lResidualMat.Matrix[1,4] := 0;
  lResidualMat.Matrix[2,4] := 0;
  lResidualMat.Matrix[3,4] := 0;
  Mins (lInMat, lHdr.NIFTIHdr,lInMinX,lInMinY,lInMinZ);
  Mins (lResidualMat, lOutHdr,lOutMinX,lOutMinY,lOutMinZ);
  lResidualMat.Matrix[1,4] :=  lInMinX-lOutMinX;
  lResidualMat.Matrix[2,4]  := lInMinY-lOutMinY;
  lResidualMat.Matrix[3,4] := lInMinZ-lOutMinZ;
  //End of kuldge
  lOutHdr.srow_x[0] := lResidualMat.Matrix[1,1];
  lOutHdr.srow_x[1] := lResidualMat.Matrix[1,2];
  lOutHdr.srow_x[2] := lResidualMat.Matrix[1,3];
  lOutHdr.srow_y[0] := lResidualMat.Matrix[2,1];
  lOutHdr.srow_y[1] := lResidualMat.Matrix[2,2];
  lOutHdr.srow_y[2] := lResidualMat.Matrix[2,3];
  lOutHdr.srow_z[0] := lResidualMat.Matrix[3,1];
  lOutHdr.srow_z[1] := lResidualMat.Matrix[3,2];
  lOutHdr.srow_z[2] := lResidualMat.Matrix[3,3];
  lOutHdr.srow_x[3] := lResidualMat.Matrix[1,4];
  lOutHdr.srow_y[3] := lResidualMat.Matrix[2,4];
  lOutHdr.srow_z[3] := lResidualMat.Matrix[3,4];
  nifti_mat44_to_quatern( lResidualMat,
   lOutHdr.quatern_b,lOutHdr.quatern_c,lOutHdr.quatern_d,
   lOutHdr.qoffset_x,lOutHdr.qoffset_y,lOutHdr.qoffset_z,
                             dx, dy, dz,  lOutHdr.pixdim[0]);
   GetMem(lBufferOut,lVolBytes);
   lnVol := 1;
   if (lhdr.NIfTIhdr.dim[4] > 1) and (l4D) then
    lnVol := lhdr.NIfTIhdr.dim[4];
   //convert
   (*if lFlipX then fx(1);
   if lFlipY then fx(2);
   if lFlipZ then fx(3);*)

   if lFlipX then
      lXInc := -lXInc;
   if lFlipY then
      lYInc := -lYInc;
   if lFlipZ then
      lZInc := -lZInc;
   for lVol := 1 to lnVol do begin
    lOutPos := 0;
    if lFlipX then
      lStartX := (lOutHdr.dim[1]-1)*-lXInc
    else
       lStartX := 0;
    if lFlipY then
      lStartX := lStartX + (lOutHdr.dim[2]-1)*-lYInc;
    if lFlipZ then
      lStartX := lStartX + (lOutHdr.dim[3]-1)*-lZInc;
    lStartX := lStartX+ ((lVol-1)*lVolBytes);
    for lZ := 1 to lOutHdr.dim[3] do begin
       lOutZ := lStartX + (lZ-1) * lZInc;
       for lY := 1 to lOutHdr.dim[2] do begin
           lOutY := ((lY-1) * lYInc) + lOutZ;
           for lX := 1 to lOutHdr.dim[1] do begin
               for lB := 1 to (lBPP) do begin
                   inc(lOutPos);
                   //lInPos := ((lX-1) * lXInc) + lOutY + lB;
                   lInPos := lOutY + lB;
                   lBufferOut^[lOutPos] := lHdr.ImgBuffer^[lInPos];
               end;
               inc(lOutY,lXinc);
           end;
       end; //for Y
    end; //for Z
    Move(lBufferOut^,lHdr.ImgBuffer^[1+((lVol-1)*lVolBytes)],lVolBytes);
   end; //for each volume
     (*   Filemode := 2;
     AssignFile(lF,'C:\Documents and Settings\Admin\Desktop\rorden\perisample\shit.img'); {WIN}
     Rewrite(lF,1);
     BlockWrite(lF,lHdr.ImgBuffer^,lnVol*lVolBytes);
     CloseFile(lF);*)
   Freemem(lBufferOut);
   lHdr.NIFTIhdr := lOutHdr;
   //fx(lOutHdr.srow_x[3],lOutHdr.srow_y[3],lOutHdr.srow_z[3]);
   result := true;
end;//ReorientCore

end.