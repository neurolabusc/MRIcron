unit nii_orient;
{$H+}
//reorients a NIfTI image to canonical space ...
//closest to canonical rotation matrix [1 0 0; 0 1 0; 0 0 1]
interface

uses
{$IFDEF FPC}gzio2,{$ENDIF}
  SysUtils,define_types,dicomtypes,niftiutil,GraphicsMathLibrary,prefs,dialogs_msg, nifti_types;

function Reorient(var lHdrName: string; var lHdr: TNIFTIhdr; lPrefs: TPrefs; lOverwrite,lForce: boolean): string;
//function SuperReorient(var lHdrName: string;  lPrefs: TPrefs):string;
function LRFlip(lFilename: string; lPrefs: TPrefs): boolean;

implementation
uses dialogsx;
(*procedure Mx(var lM: TMatrix);
begin
    Msg('=['+
             floattostr(lM.matrix[1,1])+', '+floattostr(lM.matrix[1,2])+', '+floattostr(lM.matrix[1,3])+', '+floattostr(lM.matrix[1,4])+'; '+
             floattostr(lM.matrix[2,1])+', '+floattostr(lM.matrix[2,2])+', '+floattostr(lM.matrix[2,3])+', '+floattostr(lM.matrix[2,4])+'; '+
             floattostr(lM.matrix[3,1])+', '+floattostr(lM.matrix[3,2])+', '+floattostr(lM.matrix[3,3])+', '+floattostr(lM.matrix[3,4])+'; '+
             ' 0, 0, 0, 1]');
end;*)


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

function nifti_mat44_orthogx( lR :TMatrix; lPrefs: TPrefs): TMatrix;
//returns rotation matrix required to orient image so it is aligned nearest to the identity matrix =
// 1 0 0 0
// 0 1 0 0
// 0 0 1 0
// 0 0 0 1
//Therefore, image is approximately oriented in space
var
   lrow,lcol,lMaxRow,lMaxCol,l2ndMaxRow,l2ndMaxCol,l3rdMaxRow,l3rdMaxCol: integer;
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, val,lAbsmax,lAbs: double;
   Q,Flip: TMatrix;  //3x3
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
   result := Matrix3D( 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1);

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
   if  lPrefs.OrthoFlipXDim then begin
      Flip := Matrix3D(-1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
      Q := result;
      result := multiplymatrices(Flip,Q); 
   end;
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

function ReorientCore(var lHdrName: string; lPrefix: string; var lHdr: TNIFTIhdr; lPrefs: TPrefs; lOverwrite,lKeepOrigHdr: boolean; var lInMat,lRotMat: TMatrix): string;
var
   lOutHdr: TNIFTIhdr;
   lOutName: string;
   lResidualMat: TMatrix;
   lInMinX,lInMinY,lInMinZ,lOutMinX,lOutMinY,lOutMinZ,
   dx, dy, dz, QFac: single;
   lStartX,lStartY,lStartZ,
   lZ,lY,lX,lB,
   lOutZ,lOutY,
   lVol,lNumVol,lXInc, lYInc, lZInc,lBPP: integer;
   lInPos,lVolBytes,lOutPos,lInOffset: integer;
   lBufferIn,lBufferOut,lIBuffer,lOBuffer: bytep;
   lOpts: TNIIOpts;
   lFlipX,lFlipY,lFlipZ: boolean;
   lOutF,lInF: File;
begin
   result := '';
   lOutHdr := lHdr;
   //Some software uses negative pixdims to represent a spatial flip - now that the image is canonical, all dimensions are positive
   lOutHdr.pixdim[1] := abs(lHdr.pixdim[1]);
   lOutHdr.pixdim[2] := abs(lHdr.pixdim[2]);
   lOutHdr.pixdim[3] := abs(lHdr.pixdim[3]);
   //sort out dim1
   lFlipX := false;
   if lRotMat.Matrix[1,2] <> 0 then begin
       lXinc := lHdr.dim[1];
       lOutHdr.dim[1] := lHdr.dim[2];
       lOutHdr.pixdim[1] := abs(lHdr.pixdim[2]);
       if lRotMat.Matrix[1,2] < 0 then lFlipX := true
   end else if lRotMat.Matrix[1,3] <> 0 then begin
       lXinc := lHdr.dim[1]*lHdr.dim[2];
       lOutHdr.dim[1] := lHdr.dim[3];
       lOutHdr.pixdim[1] := abs(lHdr.pixdim[3]);
       if lRotMat.Matrix[1,3] < 0 then lFlipX := true
   end else begin
       lXinc := 1;
       if lRotMat.Matrix[1,1] < 0 then lFlipX := true
   end;
   //sort out dim2
   lFlipY := false;
   if lRotMat.Matrix[2,2] <> 0 then begin
       lYinc := lHdr.dim[1];
       //lOutHdr.dim[2] := lHdr.dim[2];
       //lOutHdr.pixdim[2] := lHdr.pixdim[2];
       if lRotMat.Matrix[2,2] < 0 then lFlipY := true
   end else if lRotMat.Matrix[2,3] <> 0 then begin
       lYinc := lHdr.dim[1]*lHdr.dim[2];
       lOutHdr.dim[2] := lHdr.dim[3];
       lOutHdr.pixdim[2] := abs(lHdr.pixdim[3]);
       if lRotMat.Matrix[2,3] < 0 then lFlipY := true
   end else begin
       lYinc := 1;
       lOutHdr.dim[2] := lHdr.dim[1];
       lOutHdr.pixdim[2] := abs(lHdr.pixdim[1]);
       if lRotMat.Matrix[2,1] < 0 then lFlipY := true
   end;
   //sort out dim3
   lFlipZ := false;
   if lRotMat.Matrix[3,2] <> 0 then begin
       lZinc := lHdr.dim[1];
       lOutHdr.dim[3] := lHdr.dim[2];
       lOutHdr.pixdim[3] := lHdr.pixdim[2];
       if lRotMat.Matrix[3,2] < 0 then lFlipZ := true;
   end else if lRotMat.Matrix[3,3] <> 0 then begin
       lZinc := lHdr.dim[1]*lHdr.dim[2];
       //lOutHdr.dim[3] := lHdr.dim[3];
       //lOutHdr.pixdim[3] := lHdr.pixdim[3];
       if lRotMat.Matrix[3,3] < 0 then lFlipZ := true;
   end else begin
       lZinc := 1;
       lOutHdr.dim[3] := lHdr.dim[1];
       lOutHdr.pixdim[3] := lHdr.pixdim[1];
       if lRotMat.Matrix[3,1] < 0 then lFlipZ := true;
   end;
   //details for writing...
   lBPP := (lHdr.bitpix div 8); //bytes per pixel
   lXinc := lXinc * lBPP;
   lYinc := lYinc * lBPP;
   lZinc := lZinc * lBPP;
   lVolBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*lBPP;
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
  Mins (lInMat, lHdr,lInMinX,lInMinY,lInMinZ);
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
                             dx, dy, dz,lOutHdr.pixdim[0]  {QFac});
   //read input
   if not NIFTIhdr_LoadImg (lHdrName, lHdr, lIBuffer, lInOffset,lOpts) then  exit;

   lNumVol := lOutHdr.dim[4];
   if lNumVol < 1 then //hopefully this nevee happens
      lNumVol := 1;
   GetMem(lOBuffer,lNumVol*lVolBytes+kNIIImgOffset);
   lBufferIn := (@liBuffer^[lInOffset+1]);
   lOutPos := 0;
   lBufferOut := (@loBuffer^[kNIIImgOffset+1+lOutPos]);
   if lFlipX then
      lXInc := -lXInc;
   if lFlipY then
      lYInc := -lYInc;
   if lFlipZ then
      lZInc := -lZInc;
   for lVol := 1 to lNumVol do begin
   //convert
   if lFlipX then
      lStartX := (lOutHdr.dim[1]-1)*-lXInc
   else
       lStartX := 0;
   if lFlipY then
      lStartX := lStartX + (lOutHdr.dim[2]-1)*-lYInc;
   if lFlipZ then
      lStartX := lStartX + (lOutHdr.dim[3]-1)*-lZInc;
   lStartX := lStartX + ((lVol-1)*lVolBytes);
   for lZ := 1 to lOutHdr.dim[3] do begin
       lOutZ := lStartX + (lZ-1) * lZInc;
       for lY := 1 to lOutHdr.dim[2] do begin
           lOutY := ((lY-1) * lYInc) + lOutZ;
           for lX := 1 to lOutHdr.dim[1] do begin
               for lB := 1 to lBPP do begin
                   inc(lOutPos);
                   lInPos := ((lX-1) * lXInc) + lOutY + lB;
                   lBufferOut^[lOutPos] := lBufferIn^[lInPos];
               end;
           end;
       end; //for Y
   end; //for Z
  end;//For each volume
   Freemem(lIBuffer);
   if lOverwrite then
      lOutName := lHdrName
   else
       lOutName := ChangeFilePrefix (lHdrName,lPrefix);
     dcmMsg('Reorienting as '+lOutName);
   if lKeepOrigHdr then
       result :=  SaveNIfTICore (lOutName, lOBuffer, kNIIImgOffset+1, lHdr, lPrefs)
   else
       result :=  SaveNIfTICore (lOutName, lOBuffer, kNIIImgOffset+1, lOutHdr, lPrefs);
   Freemem(lOBuffer);
end;//ReorientCore

(*function ReorientCore(var lHdrName: string; lPrefix: string; var lHdr: TNIFTIhdr; lPrefs: TPrefs; lOverwrite: boolean; var lInMat,lRotMat: TMatrix): string;
var
   lOutHdr: TNIFTIhdr;
   lOutName: string;
   lResidualMat: TMatrix;
   lInMinX,lInMinY,lInMinZ,lOutMinX,lOutMinY,lOutMinZ,
   dx, dy, dz, QFac: single;
   lStartX,lStartY,lStartZ,
   lZ,lY,lX,lB,
   lOutZ,lOutY,
   lXInc, lYInc, lZInc,lBPP: integer;
   lInPos,lVolBytes,lOutPos,lInOffset: integer;
   lBufferIn,lBufferOut,lIBuffer,lOBuffer: bytep;
   lByteSwap,lFlipX,lFlipY,lFlipZ: boolean;
   lOutF,lInF: File;
begin
   result := '';
   lOutHdr := lHdr;
   if lOutHdr.dim[4] > 1 then begin
       showmessage('Reorient only designed for 3D images.');
       exit;
   end;
   //Some software uses negative pixdims to represent a spatial flip - now that the image is canonical, all dimensions are positive
   lOutHdr.pixdim[1] := abs(lHdr.pixdim[1]);
   lOutHdr.pixdim[2] := abs(lHdr.pixdim[2]);
   lOutHdr.pixdim[3] := abs(lHdr.pixdim[3]);
   //sort out dim1
   lFlipX := false;
   if lRotMat.Matrix[1,2] <> 0 then begin
       lXinc := lHdr.dim[1];
       lOutHdr.dim[1] := lHdr.dim[2];
       lOutHdr.pixdim[1] := abs(lHdr.pixdim[2]);
       if lRotMat.Matrix[1,2] < 0 then lFlipX := true
   end else if lRotMat.Matrix[1,3] <> 0 then begin
       lXinc := lHdr.dim[1]*lHdr.dim[2];
       lOutHdr.dim[1] := lHdr.dim[3];
       lOutHdr.pixdim[1] := abs(lHdr.pixdim[3]);
       if lRotMat.Matrix[1,3] < 0 then lFlipX := true
   end else begin
       lXinc := 1;
       if lRotMat.Matrix[1,1] < 0 then lFlipX := true
   end;
   //sort out dim2
   lFlipY := false;
   if lRotMat.Matrix[2,2] <> 0 then begin
       lYinc := lHdr.dim[1];
       //lOutHdr.dim[2] := lHdr.dim[2];
       //lOutHdr.pixdim[2] := lHdr.pixdim[2];
       if lRotMat.Matrix[2,2] < 0 then lFlipY := true
   end else if lRotMat.Matrix[2,3] <> 0 then begin
       lYinc := lHdr.dim[1]*lHdr.dim[2];
       lOutHdr.dim[2] := lHdr.dim[3];
       lOutHdr.pixdim[2] := abs(lHdr.pixdim[3]);
       if lRotMat.Matrix[2,3] < 0 then lFlipY := true
   end else begin
       lYinc := 1;
       lOutHdr.dim[2] := lHdr.dim[1];
       lOutHdr.pixdim[2] := abs(lHdr.pixdim[1]);
       if lRotMat.Matrix[2,1] < 0 then lFlipY := true
   end;
   //sort out dim3
   lFlipZ := false;
   if lRotMat.Matrix[3,2] <> 0 then begin
       lZinc := lHdr.dim[1];
       lOutHdr.dim[3] := lHdr.dim[2];
       lOutHdr.pixdim[3] := lHdr.pixdim[2];
       if lRotMat.Matrix[3,2] < 0 then lFlipZ := true;
   end else if lRotMat.Matrix[3,3] <> 0 then begin
       lZinc := lHdr.dim[1]*lHdr.dim[2];
       //lOutHdr.dim[3] := lHdr.dim[3];
       //lOutHdr.pixdim[3] := lHdr.pixdim[3];
       if lRotMat.Matrix[3,3] < 0 then lFlipZ := true;
   end else begin
       lZinc := 1;
       lOutHdr.dim[3] := lHdr.dim[1];
       lOutHdr.pixdim[3] := lHdr.pixdim[1];
       if lRotMat.Matrix[3,1] < 0 then lFlipZ := true;
   end;
   //details for writing...
   lBPP := (lHdr.bitpix div 8); //bytes per pixel
   lXinc := lXinc * lBPP;
   lYinc := lYinc * lBPP;
   lZinc := lZinc * lBPP;
   lVolBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*lBPP;
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
  Mins (lInMat, lHdr,lInMinX,lInMinY,lInMinZ);
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
                             dx, dy, dz, QFac);
   //read input
   if not NIFTIhdr_LoadImg (lHdrName, lHdr, lIBuffer, lInOffset,lByteSwap) then  exit;

   GetMem(lOBuffer,lVolBytes+kNIIImgOffset);
   lBufferIn := (@liBuffer^[lInOffset+1]);
   lOutPos := 0;
   lBufferOut := (@loBuffer^[kNIIImgOffset+1+lOutPos]);
   //convert
   if lFlipX then begin
      lStartX := (lOutHdr.dim[1]-1)*lXInc;
      lXInc := -lXInc;
   end else
       lStartX := 0;
   if lFlipY then begin
      lStartX := lStartX + (lOutHdr.dim[2]-1)*lYInc;
      lYInc := -lYInc;
   end;
   if lFlipZ then begin
      lStartX := lStartX + (lOutHdr.dim[3]-1)*lZInc;
      lZInc := -lZInc;
   end;

   for lZ := 1 to lOutHdr.dim[3] do begin
       lOutZ := lStartX + (lZ-1) * lZInc;
       for lY := 1 to lOutHdr.dim[2] do begin
           lOutY := ((lY-1) * lYInc) + lOutZ;
           for lX := 1 to lOutHdr.dim[1] do begin
               for lB := 1 to lBPP do begin
                   inc(lOutPos);
                   lInPos := ((lX-1) * lXInc) + lOutY + lB;
                   lBufferOut^[lOutPos] := lBufferIn^[lInPos];
               end;
           end;
       end; //for Y
   end; //for Z
   Freemem(lIBuffer);
   if lOverwrite then
      lOutName := lHdrName
   else
       lOutName := ChangeFilePrefix (lHdrName,lPrefix);
     Msg('Reorienting as '+lOutName);
   result :=  SaveNIfTICore (lOutName, lOBuffer, kNIIImgOffset+1, lOutHdr, lPrefs,lByteSwap);
   Freemem(lOBuffer);
end;//ReorientCore   *)

(*function SuperReorient(var lHdrName: string; {var lHdr: TNIFTIhdr;} lPrefs: TPrefs): string;
//super-reorient generates  copies of the source image with different orthogonal rotations
//useful for testing viewing software
var
  lRot,lRotMod: integer;
  lByteSwap: boolean;
  lInMat,lRotMat,lTempMat,lTransformMat,lNormalMat,lFlipMat: TMatrix;
  lPrefix: string;
  lHdr: TNIFTIhdr;
begin
         if not NIFTIhdr_LoadHdr (lHdrName, lHdr, lByteSwap) then begin
            Msg('Unable to read as NifTI/Analyze' + lHdrName);
            exit;
         end;
   result := '';
   lInMat := Matrix3D (
    lHdr.srow_x[0],lHdr.srow_x[1],lHdr.srow_x[2],lHdr.srow_x[3],
    lHdr.srow_y[0],lHdr.srow_y[1],lHdr.srow_y[2],lHdr.srow_y[3],
    lHdr.srow_z[0],lHdr.srow_z[1],lHdr.srow_z[2],lHdr.srow_z[3],
    0,0,0,1);
    lNormalMat := nifti_mat44_orthogx(lInMat);
    //lRot := 3; begin
    for lRot := 1 to 24 do begin
       lRotMod := lRot mod 6;
       case lRotMod of
            1: lTempMat := Matrix3D(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
            2: lTempMat := Matrix3D(1,0,0,0, 0,0,1,0, 0,1,0,0, 0,0,0,1);
            3: lTempMat := Matrix3D(0,1,0,0, 1,0,0,0, 0,0,1,0, 0,0,0,1);
            4: lTempMat := Matrix3D(0,1,0,0, 0,0,1,0, 1,0,0,0, 0,0,0,1);
            5: lTempMat := Matrix3D(0,0,1,0, 1,0,0,0, 0,1,0,0, 0,0,0,1);
            else lTempMat := Matrix3D(0,0,1,0, 0,1,0,0, 1,0,0,0, 0,0,0,1);
       end;
       case lRot of
           1..6: lFlipMat := Eye3D;
           7..12: lFlipMat := Matrix3D(-1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
           13..18: lFlipMat := Matrix3D(1,0,0,0, 0,-1,0,0, 0,0,1,0, 0,0,0,1);
           19..24: lFlipMat  := Matrix3D(1,0,0,0, 0,1,0,0, 0,0,-1,0, 0,0,0,1);
       end;
       lTransformMat := MultiplyMatrices(lTempMat,lFlipMat);
       lRotMat := MultiplyMatrices(lNormalMat,lTransformMat);
       lPrefix := floattostr(lTransformMat.Matrix[1,1])+floattostr(lTransformMat.Matrix[1,2])+floattostr(lTransformMat.Matrix[1,3])
               +'_'+floattostr(lTransformMat.Matrix[2,1])+floattostr(lTransformMat.Matrix[2,2])+floattostr(lTransformMat.Matrix[2,3])
               +'_'+floattostr(lTransformMat.Matrix[3,1])+floattostr(lTransformMat.Matrix[3,2])+floattostr(lTransformMat.Matrix[3,3]);
       Msg(lPrefix);
       result := ReorientCore(lHdrName, lPrefix,lHdr, lPrefs, false, lInMat,lRotMat);
   end;
end;//proc SuperReorient
*)

function Reorient(var lHdrName: string; var lHdr: TNIFTIhdr; lPrefs: TPrefs; lOverwrite,lForce: boolean): string;
//returns output filename if successful
//reslice an image so it is in canonical space
var
   lInMat,lRotMat: TMatrix;
begin
   result := '';
   if (lHdr.dim[4] > 1) or (lHdr.dim[3] < 2) then begin
      dcmMsg('Can only orient 3D images '+inttostr(lHdr.dim[3])+' '+inttostr(lHdr.dim[4]));
      exit;
   end;
   if (lHdr.dim[1] > lPrefs.MaxReorientMatrix) or (lHdr.dim[2] > lPrefs.MaxReorientMatrix) or(lHdr.dim[3] > lPrefs.MaxReorientMatrix) then begin
      dcmMsg('This image will not be reoriented (larger than MaxReorientMatrix= '+inttostr(lPrefs.MaxReorientMatrix));
      exit;
   end;
   lInMat := Matrix3D (
    lHdr.srow_x[0],lHdr.srow_x[1],lHdr.srow_x[2],lHdr.srow_x[3],
    lHdr.srow_y[0],lHdr.srow_y[1],lHdr.srow_y[2],lHdr.srow_y[3],
    lHdr.srow_z[0],lHdr.srow_z[1],lHdr.srow_z[2],lHdr.srow_z[3],
    0,0,0,1);
   if (not lForce) and (NIfTIAlignedM (lInMat)) then begin
     result := lHdrName;
     dcmMsg('Image is already canonically oriented: '+lHdrName);
     exit;
   end;
   lRotMat := nifti_mat44_orthogx( lInMat,lPrefs);
   if NIfTIAlignedM (lRotMat) then begin
     result := lHdrName;
     dcmMsg('According to header, image is already approximately canonically oriented');
     exit; //already as close as possible
   end;
   result := ReorientCore(lHdrName, 'o',lHdr, lPrefs, lOverwrite,false, lInMat,lRotMat);
end;

function LRFlip(lFilename: string; lPrefs: TPrefs): boolean;
//function Reorient(var lHdrName: string; var lHdr: TNIFTIhdr; lPrefs: TPrefs; lOverwrite,lForce: boolean): string;
//returns output filename if successful
//reslice an image so it is in canonical space
var
   lHdr: TNIFTIhdr;
   lO: TNIIOPts;
   lInMat,lRotMat: TMatrix;
begin
   result := false;
   if not NIFTIhdr_LoadHdr (lFilename, lHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
   end;
   if (lHdr.dim[1] > lPrefs.MaxReorientMatrix) or (lHdr.dim[2] > lPrefs.MaxReorientMatrix) or(lHdr.dim[3] > lPrefs.MaxReorientMatrix) then begin
      dcmMsg('This image will not be reoriented (larger than MaxReorientMatrix= '+inttostr(lPrefs.MaxReorientMatrix));
      exit;
   end;
   lInMat := Matrix3D (
    lHdr.srow_x[0],lHdr.srow_x[1],lHdr.srow_x[2],lHdr.srow_x[3],
    lHdr.srow_y[0],lHdr.srow_y[1],lHdr.srow_y[2],lHdr.srow_y[3],
    lHdr.srow_z[0],lHdr.srow_z[1],lHdr.srow_z[2],lHdr.srow_z[3],
    0,0,0,1);

   lRotMat := Matrix3D(
    -1,0,0,0,
    0,1,0,0,
    0,0,1,0,
    0,0,0,1);
   result := (ReorientCore(lFilename, 'lr',lHdr, lPrefs, false,true, lInMat,lRotMat)<> '');
end;


end.
