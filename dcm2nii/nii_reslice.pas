unit nii_reslice;
interface
{$H+}
uses
    niftiutil,define_types,sysutils,dicomtypes,prefs,dialogs_msg, nifti_types;

//function ResliceImgNIfTI (lTargetImgName,lSrcImgName,lOutputName: string): boolean;
function Reslice2Targ (lSrcName,lTargetName,lDestName: string; lPrefs: TPrefs):string;

implementation

uses GraphicsMathLibrary, dialogsx;


function Hdr2Mat (lHdr:  TNIFTIhdr): TMatrix;
begin
  Result := Matrix3D (
  lHdr.srow_x[0],lHdr.srow_x[1],lHdr.srow_x[2],lHdr.srow_x[3],      // 3D "graphics" matrix
  lHdr.srow_y[0],lHdr.srow_y[1],lHdr.srow_y[2],lHdr.srow_y[3],      // 3D "graphics" matrix
  lHdr.srow_z[0],lHdr.srow_z[1],lHdr.srow_z[2],lHdr.srow_z[3],      // 3D "graphics" matrix
						   0,0,0,1);
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
end;     *)

(*
procedure  SPMmat(var lDestMat: TMatrix);
//SPM matrices are indexed from 1
//This function is only useful for direct comparisons with SPM
var
  lTemp,lVS: TMatrix;
begin
  lVS := Matrix3D (1,0,0,-1,
    0,1,0,-1,
    0,0,1,-1, 0,0,0,1);//VoxelShift
  lTemp := lDestMat;
  lDestMat := MultiplyMatrices(lTemp,lVS);
end;*)

procedure  Coord(var lV: TVector; var lMat: TMatrix);
//transform X Y Z by matrix
var
  lXi,lYi,lZi: single;
begin
  lXi := lV.x; lYi := lV.y; lZi := lV.z;
  lV.x := (lXi*lMat.matrix[1][1]+lYi*lMat.matrix[1][2]+lZi*lMat.matrix[1][3]+lMat.matrix[1][4]);
  lV.y := (lXi*lMat.matrix[2][1]+lYi*lMat.matrix[2][2]+lZi*lMat.matrix[2][3]+lMat.matrix[2][4]);
  lV.z := (lXi*lMat.matrix[3][1]+lYi*lMat.matrix[3][2]+lZi*lMat.matrix[3][3]+lMat.matrix[3][4]);

end;

procedure  Transposemat(var lMat: TMatrix);
var
  lTemp: TMatrix;
  i,j: integer;
begin
  lTemp := lMat;
  for i := 1 to lMat.size do
    for j := 1 to lMat.size do
      lMat.matrix[i,j] := lTemp.matrix[j,i];
end;

PROCEDURE gaussj(VAR a: TMatrix);//Invert a Matrix - see Numerical Recipes
VAR
   big,dum,pivinv: real;
   n,i,icol,irow,j,k,l,ll: integer;
   indxc,indxr,ipiv: array [1..4] of integer;
BEGIN
   icol := 1;//not used - avoids compiler warning
   irow := 1;//not used - avoids compiler warning
   n := a.size;
   FOR j := 1 TO n DO BEGIN
      ipiv[j] := 0
   END;
   FOR i := 1 TO n DO BEGIN
      big := 0.0;
      FOR j := 1 TO n DO BEGIN
         IF (ipiv[j] <> 1) THEN BEGIN
            FOR k := 1 TO n DO BEGIN
               IF (ipiv[k] = 0) THEN BEGIN
                  IF (abs(a.matrix[j,k]) >= big) THEN BEGIN
                     big := abs(a.matrix[j,k]);
                     irow := j;
                     icol := k
                  END
               END ELSE IF (ipiv[k] > 1) THEN BEGIN
                  writeln('pause 1 in GAUSSJ - singular matrix'); readln
               END
            END
         END
      END;
      ipiv[icol] := ipiv[icol]+1;
      IF (irow <> icol) THEN BEGIN
         FOR l := 1 TO n DO BEGIN
            dum := a.matrix[irow,l];
            a.matrix[irow,l] := a.matrix[icol,l];
            a.matrix[icol,l] := dum
         END;
      END;
      indxr[i] := irow;
      indxc[i] := icol;
      IF (a.matrix[icol,icol] = 0.0) THEN BEGIN
         dcmMsg('pause 2 in GAUSSJ - singular matrix');
         exit;
      END;
      pivinv := 1.0/a.matrix[icol,icol];
      a.matrix[icol,icol] := 1.0;
      FOR l := 1 TO n DO BEGIN
         a.matrix[icol,l] := a.matrix[icol,l]*pivinv
      END;
      FOR ll := 1 TO n DO BEGIN
         IF (ll <> icol) THEN BEGIN
            dum := a.matrix[ll,icol];
            a.matrix[ll,icol] := 0.0;
            FOR l := 1 TO n DO BEGIN
               a.matrix[ll,l] := a.matrix[ll,l]-a.matrix[icol,l]*dum
            END;
         END
      END
   END;
   FOR l := n DOWNTO 1 DO BEGIN
      IF (indxr[l] <> indxc[l]) THEN BEGIN
         FOR k := 1 TO n DO BEGIN
            dum := a.matrix[k,indxr[l]];
            a.matrix[k,indxr[l]] := a.matrix[k,indxc[l]];
            a.matrix[k,indxc[l]] := dum
         END
      END
   END
END;

procedure SubVec (var lVx: TVector; lV0: TVector);
begin
  lVx.x := lVx.x - lV0.x;
  lVx.y := lVx.y - lV0.y;
  lVx.z := lVx.z - lV0.z;
end;

function Voxel2Voxel (var lDestHdr,lSrcHdr: TNIFTIhdr): TMatrix;
//returns matrix for transforming voxels from one image to the other image
//results are in VOXELS not mm
var
   lV0,lVx,lVy,lVz: TVector;
   lDestMat,lSrcMatInv,lSrcMat: TMatrix;

begin
     //Step 1 - compute source coordinates in mm for 4 voxels
     //the first vector is at 0,0,0, with the
     //subsequent voxels being left, up or anterior
     lDestMat := Hdr2Mat(lDestHdr);
     //SPMmat(lDestMat);
     lV0 := Vector3D  (0,0,0);
     lVx := Vector3D  (1,0,0);
     lVy := Vector3D  (0,1,0);
     lVz := Vector3D  (0,0,1);
     Coord(lV0,lDestMat);
     Coord(lVx,lDestMat);
     Coord(lVy,lDestMat);
     Coord(lVz,lDestMat);
     lSrcMat := Hdr2Mat(lSrcHdr);
     //SPMmat(lSrcMat);
     lSrcMatInv := lSrcMat;
     gaussj(lSrcMatInv);
     //the vectors should be rows not columns....
     //therefore we transpose the matrix
     Transposemat(lSrcMatInv);
     //the 'transform' multiplies the vector by the matrix
     lV0 := Transform (lV0,lSrcMatInv);
     lVx := Transform (lVx,lSrcMatInv);
     lVy := Transform (lVy,lSrcMatInv);
     lVz := Transform (lVz,lSrcMatInv);
     //subtract each vector from the origin
     // this reveals the voxel-space influence for each dimension
     SubVec(lVx,lV0);
     SubVec(lVy,lV0);
     SubVec(lVz,lV0);
     result := Matrix3D(lVx.x,lVy.x,lVz.x,lV0.x,
      lVx.y,lVy.y,lVz.y,lV0.y,
      lVx.z,lVy.z,lVz.z,lV0.z, 0,0,0,1);
end;

procedure CopyHdrMat(var lTarg,lDest: TNIfTIHdr);
//destination has dimensions and rotations of destination
var
   lI: integer;
begin
     //destination will have dimensions of target
   lDest.dim[0] := 3; //3D
   for lI := 1 to 3 do
       lDest.dim[lI] := lTarg.dim[lI];
   lDest.dim[4] := 1; //3D
   //destination will have pixdim of target
   for lI := 0 to 7 do
       lDest.pixdim[lI] := lTarg.pixdim[lI];
   lDest.xyzt_units := lTarg.xyzt_units; //e.g. mm and sec
   lDest.qform_code := lTarg.qform_code;
   lDest.sform_code := lTarg.sform_code;
   lDest.quatern_b := lTarg.quatern_b;
   lDest.quatern_c := lTarg.quatern_c;
   lDest.quatern_d := lTarg.quatern_d;
   lDest.qoffset_x := lTarg.qoffset_x;
   lDest.qoffset_y := lTarg.qoffset_y;
   lDest.qoffset_z := lTarg.qoffset_z;
   for lI := 0 to 3 do begin
       lDest.srow_x[lI] := lTarg.srow_x[lI];
       lDest.srow_y[lI] := lTarg.srow_y[lI];
       lDest.srow_z[lI] := lTarg.srow_z[lI];
   end;
end;

function Reslice2Targ (lSrcName,lTargetName,lDestName: string; lPrefs: TPrefs):string;
var
   lPos,lXYs,lXYZs,lXs,lYs,lZs,lXi,lYi,lZi,lX,lY,lZ,
   lXo,lYo,lZo,lMinY,lMinZ,lMaxY,lMaxZ,lSrcOffset,lBPP,lXYZ: integer;
   lXrM1,lYrM1,lZrM1,lXreal,lYreal,lZreal,
   lZx,lZy,lZz,lYx,lYy,lYz,
   lInMinX,lInMinY,lInMinZ, lOutMinX,lOutMinY,lOutMinZ: single;
   lXx,lXy,lXz: Singlep0;
   l32fs,l32f : SingleP;
   l32is,l32i : LongIntP;
   l16is,l16i : SmallIntP;
   l8i,l8is,lSrcBuffer,lBuffUnaligned,lBuffAligned: bytep;
   lMat: TMatrix;
      lTargHdr,lSrcHdr,lDestHdr: TNIFTIhdr;
  lS,lT: TNIIOpts;
begin
     result := '';
     if not NIFTIhdr_LoadHdr (lSrcname, lSrcHdr, lS) then exit;
     if not NIFTIhdr_LoadHdr (lTargetName, lTargHdr, lT) then exit;
     case lSrcHdr.datatype of
           kDT_UNSIGNED_CHAR : lBPP := 1;
	  kDT_SIGNED_SHORT: lBPP := 2;
          kDT_SIGNED_INT:lBPP := 4;
	  kDT_FLOAT: lBPP := 4;
         else begin
             dcmMsg('NII reslice error: datatype not supported.');
             exit;
         end;
     end; //case
     lMat := Voxel2Voxel (lTargHdr,lSrcHdr);
     lDestHdr := lSrcHdr; //destination has the comments and voxel BPP of source
     CopyHdrMat(lTargHdr,lDestHdr);//destination has dimensions and rotations of destination
     lXs := lSrcHdr.Dim[1];
     lYs := lSrcHdr.Dim[2];
     lZs := lSrcHdr.Dim[3];

     lXYs:=lXs*lYs; //slicesz
     lXYZs := lXYs*lZs;
     lX := lDestHdr.Dim[1];
     lY := lDestHdr.Dim[2];
     lZ := lDestHdr.Dim[3];
     lDestHdr.Dim[4] := 1;
     //load dataset
     if not NIFTIhdr_LoadImg (lSrcName, lSrcHdr, lSrcBuffer, lSrcOffset,lS) then  exit;
     l8is := (@lSrcBuffer^[lSrcOffset+1]);
     GetMem(lBuffUnaligned ,(lBPP*lX*lY*lZ) + 16+kNIIImgOffset);
     {$IFDEF FPC}
     lBuffAligned := Align(lBuffUnaligned,16); // not commented - check this
     {$ELSE}
     lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
     {$ENDIF}
     lPos := 1;
     case lSrcHdr.datatype of
          kDT_UNSIGNED_CHAR : l8i  := @lBuffAligned^[kNIIImgOffset+lPos];
	  kDT_SIGNED_SHORT: l16i := SmallIntP(@lBuffAligned^[kNIIImgOffset+lPos] );
          kDT_SIGNED_INT:l32i := LongIntP(@lBuffAligned^[kNIIImgOffset+lPos] );
	  kDT_FLOAT: l32f := SingleP(@lBuffAligned^[kNIIImgOffset+lPos] );
     end; //case
     case lSrcHdr.datatype of
           //kDT_UNSIGNED_CHAR : l8is := l8is;
	  kDT_SIGNED_SHORT: l16is := SmallIntP(l8is );
          kDT_SIGNED_INT:l32is := LongIntP(l8is );
	  kDT_FLOAT: l32fs := SingleP(l8is );
     end; //case
     //next clear image
     case lSrcHdr.datatype of
           kDT_UNSIGNED_CHAR : for lPos := 1 to (lX*lY*lZ) do l8i^[lPos] := 0;
	  kDT_SIGNED_SHORT: for lPos := 1 to (lX*lY*lZ) do l16i^[lPos] := 0;
          kDT_SIGNED_INT:for lPos := 1 to (lX*lY*lZ) do l32i^[lPos] := 0;
	  kDT_FLOAT: for lPos := 1 to (lX*lY*lZ) do l32f^[lPos] := 0;
     end; //case
     //now we can apply the transforms...
     //build lookup table - speed up inner loop
     getmem(lXx, lX*sizeof(single));
     getmem(lXy, lX*sizeof(single));
     getmem(lXz, lX*sizeof(single));
     for lXi := 0 to (lX-1) do begin
      lXx^[lXi] := lXi*lMat.matrix[1][1];
      lXy^[lXi] := lXi*lMat.matrix[2][1];
      lXz^[lXi] := lXi*lMat.matrix[3][1];
     end;
     //compute trilinear interpolation
     lPos := 0;
     for lZi := 0 to (lZ-1) do begin
         //these values are the same for all voxels in the slice
         // compute once per slice
         lZx := lZi*lMat.matrix[1][3];
         lZy := lZi*lMat.matrix[2][3];
         lZz := lZi*lMat.matrix[3][3];
         for lYi := 0 to (lY-1) do begin
             //these values change once per row
             // compute once per row
             lYx :=  lYi*lMat.matrix[1][2];
             lYy :=  lYi*lMat.matrix[2][2];
             lYz :=  lYi*lMat.matrix[3][2];
             for lXi := 0 to (lX-1) do begin
                 //compute each column
                 inc(lPos);

                 lXreal := (lXx^[lXi]+lYx+lZx+lMat.matrix[1][4]);
                 lYreal := (lXy^[lXi]+lYy+lZy+lMat.matrix[2][4]);
                 lZreal := (lXz^[lXi]+lYz+lZz+lMat.matrix[3][4]);
                 //need to test Xreal as -0.01 truncates to zero
                 if (lXreal >= 0) and (lYreal >= 0{1}) and (lZreal >= 0{1}) and
                     (lXreal < (lXs -1)) and (lYreal < (lYs -1) ) and (lZreal < (lZs -1))
                  then begin
                    //compute the contribution for each of the 8 source voxels
                    //nearest to the target
			              lXo := trunc(lXreal);
			              lYo := trunc(lYreal);
			              lZo := trunc(lZreal);
			              lXreal := lXreal-lXo;
			              lYreal := lYreal-lYo;
			              lZreal := lZreal-lZo;
                    lXrM1 := 1-lXreal;
			              lYrM1 := 1-lYreal;
			              lZrM1 := 1-lZreal;
			              lMinY := lYo*lXs;
			              lMinZ := lZo*lXYs;
			              lMaxY := lMinY+lXs;
			              lMaxZ := lMinZ+lXYs;
                    inc(lXo);//images incremented from 1 not 0
     case lSrcHdr.datatype of
          kDT_UNSIGNED_CHAR : begin// l8is := l8is;
                          l8i^[lPos] :=
                           round (
		 	   {all min} ( (lXrM1*lYrM1*lZrM1)*l8is^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*l8is^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*l8is^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*l8is^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*l8is^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*l8is^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*l8is^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*l8is^[lXo+1+lMaxY+lMaxZ]) );
          end;
	  kDT_SIGNED_SHORT: begin
                          l16i^[lPos] :=
                           round (
		 	   {all min} ( (lXrM1*lYrM1*lZrM1)*l16is^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*l16is^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*l16is^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*l16is^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*l16is^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*l16is^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*l16is^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*l16is^[lXo+1+lMaxY+lMaxZ]) );
          end;
          kDT_SIGNED_INT:begin
                          l32i^[lPos] :=
                           round (
		 	   {all min} ( (lXrM1*lYrM1*lZrM1)*l32is^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*l32is^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*l32is^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*l32is^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*l32is^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*l32is^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*l32is^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*l32is^[lXo+1+lMaxY+lMaxZ]) );
          end;
	  kDT_FLOAT: begin  //note - we do not round results - all intensities might be frational...
                          l32f^[lPos] :=
                            (
		 	   {all min} ( (lXrM1*lYrM1*lZrM1)*l32fs^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*l32fs^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*l32fs^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*l32fs^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*l32fs^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*l32fs^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*l32fs^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*l32fs^[lXo+1+lMaxY+lMaxZ]) );
          end;
     end; //case

                 end; //if voxel is in source image's bounding box
             end;//z
         end;//y
     end;//z
     //release lookup tables
     freemem(lXx);
     freemem(lXy);
     freemem(lXz);
     //check to see if image is empty...
     lPos := 1;
     case lSrcHdr.datatype of
           kDT_UNSIGNED_CHAR : while (lPos <= (lX*lY*lZ)) and (l8i^[lPos] = 0) do inc(lPos);
	  kDT_SIGNED_SHORT: while (lPos <= (lX*lY*lZ)) and (l16i^[lPos] = 0) do inc(lPos);
          kDT_SIGNED_INT:while (lPos <= (lX*lY*lZ)) and (l32i^[lPos] = 0) do inc(lPos);
	  kDT_FLOAT: while (lPos <= (lX*lY*lZ)) and (l32f^[lPos] = 0) do inc(lPos);
     end; //case
     if lPos <= (lX*lY*lZ) then begin //image not empty
        result :=  SaveNIfTICore (lDestName, lBuffAligned, kNIIImgOffset+1, lDestHdr, lPrefs);
     end else begin
         dcmMsg('no voxels in output');
     end;
     Freemem(lBuffUnaligned);
     Freemem(lSrcBuffer);
end;

(*function ResliceImgNIfTI (lTargetImgName,lSrcImgName,lOutputName: string): boolean;
label
 666;
var
   lReslice : boolean;
   lDestHdr,lSrcHdr: TMRIcroHdr;
   lSrcMat,lDestMat,lSrcMatINv,lDestMatInv,lMat: TMatrix;
   lOffX,lOffY,lOffZ: single;
   D: double;
begin
     result := false;
     if not fileexists(lTargetImgName) then exit;
     if not fileexists(lSrcImgName) then exit;
     ImgForm.CloseImagesClick(nil);
     lReslice := gBGImg.ResliceOnLoad;
     gBGImg.ResliceOnLoad := false;
     //if not HdrForm.OpenAndDisplayHdr(lTargetImgName,lDestHdr) then goto 666;
     if not NIFTIhdr_LoadHdr(lTargetImgName, lDestHdr) then goto 666;
     if not NIFTIhdr_LoadHdr(lSrcImgName, lSrcHdr) then goto 666;
     if not ImgForm.OpenAndDisplayImg(lSrcImgName,false) then exit;
     if not Qx(lDestHdr,lSrcHdr,lOutputName) then goto 666;

     result := true;
666:
     if not result then
        showmessage('Error applying transform '+lSrcImgName+'->'+lTargetImgName);
     gBGImg.ResliceOnLoad := lReslice;
end;  *)

end.