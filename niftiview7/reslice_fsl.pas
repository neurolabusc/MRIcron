unit reslice_fsl;
{$H+}
interface
uses
    nifti_hdr,define_types,metagraph,sysutils;

function ResliceImg (lTargetImgName,lSrcImgName,lSrc2TargetMatName,lOutputName: string): boolean;
procedure ResliceFSL;

implementation

uses nifti_img_view,dialogs,nifti_img,text,graphx,math,nifti_hdr_view,GraphicsMathLibrary,classes;

procedure ResliceFSL;
label
  666;
var
	lInc,lNumberofFiles: integer;
  lSrc2TargetMatName,lSrcImgName,lTargetImgName,lOutputName:string;
  lStrings : TStringList;
begin
	 ImgForm.CloseImagesClick(nil);
	  if not OpenDialogExecute(kImgFilter,'Select source image[s]',true) then exit;
	  lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
    if  lNumberofFiles < 1 then
		  exit;
    lStrings := TStringList.Create;
    lStrings.AddStrings(HdrForm.OpenHdrDlg.Files);
	  if not OpenDialogExecute('FSL (*.mat)|*.mat','Select FSL source-to-target matrix',false) then goto 666;
    lSrc2TargetMatName :=  HdrForm.OpenHdrDlg.Filename;
	  if not OpenDialogExecute(kImgFilter,'Select target image (source image will be warped to this)',false) then goto 666;
    lTargetImgName :=  HdrForm.OpenHdrDlg.Filename;

    TextForm.MemoT.Lines.Clear;
    for lInc:= 1 to lNumberofFiles do begin
            lSrcImgName := lStrings[lInc-1];
            lOutputName := ChangeFilePrefix (lSrcImgName,'w');
            TextForm.MemoT.Lines.Add(' Source->Matrix->Target '+lSrcImgName+'->'+ lSrc2TargetMatName+'->'+lTargetImgName);
            ResliceImg (lTargetImgName,lSrcImgName,lSrc2TargetMatName,lOutputName);
    end;//lLoop
        TextForm.Show;
        666:
    lStrings.free;
end;

function ReadFSLMat (var lMat: TMatrix; lSrc2TargetMatName: string):boolean;
var
   lF: TextFile;
   xx,xy,xz,xo
   ,yx,yy,yz,yo
   ,zx,zy,zz,zo: double;
begin
     result := false;
     if not fileexists(lSrc2TargetMatName) then exit;
     Assign(lF,lSrc2TargetMatName);
     Filemode := 0;
     Reset(lF);
     readln(lF,xx,xy,xz,xo,yx,yy,yz,yo,zx,zy,zz,zo);
      //read all with one readln -
      // separate readlns only work for native eoln
     CloseFile(lF);
          lMat:= Matrix3D (xx,xy,xz,xo
   ,yx,yy,yz,yo
   ,zx,zy,zz,zo
						  ,0,0,0,1);
    result := true;
    Filemode := 2;
end;

function Rx (var lDestHdr,lSrcHdr: TMRIcroHdr; var lInMat: TMatrix; var lOutputName: string):boolean;
var
   lPos,lXYs,lXYZs,lXs,lYs,lZs,lXi,lYi,lZi,lX,lY,lZ,
   lXo,lYo,lZo,lMinY,lMinZ,lMaxY,lMaxZ: integer;
   lXrM1,lYrM1,lZrM1,
   lXreal,lYreal,lZreal: double;
   lOutImg: bytep;
   lScale,lMat: TMatrix;
begin
     result := false;
     lXs := lSrcHdr.NIFTIhdr.Dim[1];
     lYs := lSrcHdr.NIFTIhdr.Dim[2];
     lZs := lSrcHdr.NIFTIhdr.Dim[3];
     if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems <> lXs*lYs*lZs) then begin
        showmessage('Reslice error: background image not loaded.');
        exit;
     end;
     lXYs:=lXs*lYs; //slicesz
     lXYZs := lXYs*lZs;
     lX := lDestHdr.NIFTIhdr.Dim[1];
     lY := lDestHdr.NIFTIhdr.Dim[2];
     lZ := lDestHdr.NIFTIhdr.Dim[3];
     //TextForm.Memo1.Lines.Add(inttostr(lXs)+'x'+inttostr(lYs)+'x'+inttostr(lZs)+'->'+inttostr(lX)+'x'+inttostr(lY)+'x'+inttostr(lZ));
     lDestHdr.NIFTIhdr.Dim[4] := 1;
     getmem(lOutImg, lX*lY*lZ*sizeof(byte));
     lPos := 0;
     //http://eeg.sourceforge.net/MJenkinson_coordtransforms.pdf
     //FLIRT transforms are in world coordinates [mm]
     //to convert to a vxl-vxl transform, the matrix must be
     //PRE-multiplied by inv(Dest) and POST-multiplied by Src
     //where Dest and Src are the spatial dimensions in mm
lScale:= Matrix3D (abs(lSrcHdr.NIFTIhdr.pixdim[1]),0,0,0
   ,0,abs(lSrcHdr.NIFTIhdr.pixdim[2]),0,0
   ,0,0,abs(lSrcHdr.NIFTIhdr.pixdim[3]),0
						  ,0,0,0,1);
   lScale := InvertMatrix3D(lScale);
   lMat := MultiplyMatrices(lScale,lInMat);
   lScale:= Matrix3D (abs(lDestHdr.NIFTIhdr.pixdim[1]),0,0,0
   ,0,abs(lDestHdr.NIFTIhdr.pixdim[2]),0,0
   ,0,0,abs(lDestHdr.NIFTIhdr.pixdim[3]),0
						  ,0,0,0,1);
   lMat := MultiplyMatrices(lMat,lScale);
     for lZi := 0 to (lZ-1) do begin
         for lYi := 0 to (lY-1) do begin
             for lXi := 0 to (lX-1) do begin
                 inc(lPos);
                 lOutImg^[lPos] := 0;
                 lXreal := (lXi*lMat.matrix[1][1]+lYi*lMat.matrix[1][2]+lZi*lMat.matrix[1][3]+lMat.matrix[1][4]);
                 lYreal := (lXi*lMat.matrix[2][1]+lYi*lMat.matrix[2][2]+lZi*lMat.matrix[2][3]+lMat.matrix[2][4]);
                 lZreal := (lXi*lMat.matrix[3][1]+lYi*lMat.matrix[3][2]+lZi*lMat.matrix[3][3]+lMat.matrix[3][4]);
                 //need to test Xreal as -0.01 truncates to zero
                 if (lXreal >= 0) and (lYreal >= 1) and (lZreal >= 1) and
                     (lXreal < (lXs -1)) and (lYreal < (lYs -1) ) and (lZreal < (lZs -1))
                  then begin
			        lXo := trunc(lXreal);
			        lYo := trunc(lYreal);
			        lZo := trunc(lZreal);
			        lXreal := lXreal-lXo;
			        lYreal := lYreal-lYo;
			        lZreal := lZreal-lZo;
			        lXrM1 := 1-lXreal;
			        lYrM1 := 1-lYreal;
			        lZrM1 := 1-lZreal;
			        lMinY := ((lYo)*lXs);
			        lMinZ := ((lZo)*lXYs);
			        lMaxY := ((lYo+1)*lXs);
			        lMaxZ := ((lZo+1)*lXYs);
                    inc(lXo);//images incremented from 1 not 0
                    {if lMax <(lXreal) then
                       lMax := lXreal;
                    if lMin >(lXreal) then
                       lMin := lXreal;  }
                          lOutImg^[lPos] :=
                           round (
		 	   {all min} ( (lXrM1*lYrM1*lZrM1)*gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lXo+1+lMaxY+lMaxZ]) );
                 end;
             end;//z
         end;//y
     end;//z
     deletefile(lOutputName);
     SaveAsVOIorNIFTIcore (lOutputName,lOutImg, lX*lY*lZ, 1,1,lDestHdr.NIFTIhdr);
     lPos := 1;
     while (lPos <= (lX*lY*lZ)) and (lOutImg^[lPos] = 0) do
           inc(lPos);
     if lPos > (lX*lY*lZ) then
        result := false
     else
         result := true;
     freemem(lOutImg);
end;

function ResliceImg (lTargetImgName,lSrcImgName,lSrc2TargetMatName,lOutputName: string): boolean;
label
 666;
var
   lReslice,lOrtho : boolean;
   lDestHdr,lSrcHdr: TMRIcroHdr;
   lMat: TMatrix;
begin
     result := false;
     if not fileexists(lTargetImgName) then exit;
     if not fileexists(lSrcImgName) then exit;
     if not fileexists(lSrc2TargetMatName) then exit;
     if not ReadFSLMat(lMat,lSrc2TargetMatName) then exit;
     ImgForm.CloseImagesClick(nil);
     lReslice := gBGImg.ResliceOnLoad;
     lOrtho := gBGImg.OrthoReslice;
     gBGImg.OrthoReslice := false;
     gBGImg.ResliceOnLoad := false;
     //if not HdrForm.OpenAndDisplayHdr(lTargetImgName,lDestHdr) then goto 666;
     if not NIFTIhdr_LoadHdr(lTargetImgName, lDestHdr) then goto 666;
     if not NIFTIhdr_LoadHdr(lSrcImgName, lSrcHdr) then goto 666;
     ImgForm.OpenAndDisplayImg(lSrcImgName,True);
     if not Rx(lDestHdr,lSrcHdr,lMat,lOutputName) then goto 666;
     result := true;
666:
     if not result then
        showmessage('Error applying transform '+lSrcImgName+'->'+lTargetImgName+' using '+lSrc2TargetMatName);
     gBGImg.ResliceOnLoad := lReslice;
     gBGImg.OrthoReslice := lOrtho;
end;


end.
 
