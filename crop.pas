unit crop;

interface
function CropNIfTI(lL,lR,lA,lP,lD,lV: integer):boolean;
function GrowNeck (lFilename: string; lVox: integer): boolean;

implementation

uses nifti_hdr_view, nifti_hdr, nifti_img,define_types, GraphicsMathLibrary,dialogs, nifti_img_view, nifti_types;

   // nifti_img_view, nifti_img,nifti_hdr, nifti_hdr_view,{ShellAPI,}ShlObj,periutils, reslice_fsl;
procedure NIFTIhdr_SlicesToCoord (var lHdr: TNIFTIhdr; lXslice,lYslice,lZslice: integer; var lXmm,lYmm,lZmm: single);
//ignores origin offset
begin
    lXmm := (lHdr.srow_x[0]*lXslice)+ (lHdr.srow_x[1]*lYslice)+(lHdr.srow_x[2]*lzslice);
    lYmm := (lHdr.srow_y[0]*lXslice)+ (lHdr.srow_y[1]*lYslice)+(lHdr.srow_y[2]*lzslice);
    lZmm := (lHdr.srow_z[0]*lXslice)+ (lHdr.srow_z[1]*lYslice)+(lHdr.srow_z[2]*lzslice);
end;



function CropNIfTI(lL,lR,lA,lP,lD,lV: integer):boolean;
//to do : data swapping (errors on detection and writing zero in reverse order)
var
   lInHdr,lOutHdr: TNIFTIhdr;
   lOutname,lExt: string;
   lXmm,lYmm,lZmm: single;
   lMat: TMatrix;
   lOutPos,lSlice,lVol,lOutVolBytes,lInVolBytes,lImgSamples,lInc,
   lX,lY,lZ,lBPP, lB,
   lInZOffset,lInYOffset,lInSliceSz,lInXSz,lInPos,lImgOffset: integer;
   lBuffer: bytep;
   lWordX: Word;
   lSPM2: boolean;
   lOutF,lInF: File;
   lACrop,lPCrop,lDorsalCrop,lVentralCrop,lLCrop,lRCrop: integer;
   lByteSwap: boolean;
begin
     result := false;
     if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1) or (gBGImg.ScrnDim[3] < 2) or (gBGImg.ScrnMM[3] = 0) then begin
        showmessage('Please load a 3D background image for neck removal.');
        exit;
     end;
     if (gBGImg.Resliced) then begin
        showmessage('You must switch reslicing OFF (Help/Preferences) for image cropping.');
        exit;
     end;
     lInHdr := gMRIcroOverlay[kBGOverlayNum].NIFTIHdr;
     //check orthogonal alignment....
     if lInHdr.dim[4] > 1 then begin
        Showmessage('Only Cropping 1st 3D image (reorienting 4D could disrupt slice timing and diffusion directions.');
        //exit;
     end;
     //Next create reordered or trimmed image in the correct format
   case lInHdr.datatype of
     kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT,kDT_UINT16, kDT_SIGNED_INT,kDT_FLOAT:;//Supported
     else begin
         Showmessage('Crop 3D unsupported datatype.');
         exit;
     end;
   end;
   lOutHdr := lInHdr;
   lImgSamples := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3];
   lBPP := (lInHdr.bitpix div 8); //bytes per pixel
   lDorsalCrop := lD;
   lVentralCrop := lV;
   lLCrop := lL;
   lRCrop := lR;
   lACrop := lA;
   lPCrop := lP;
   //FreeMem(lBuffUnaligned);
   if (lDorsalCrop = 0) and (lVentralCrop = 0)
      and (lLCrop = 0) and (lRCrop = 0)
      and (lACrop = 0) and (lPCrop = 0) then begin
             Showmessage('Grow 3D quitting: no need to add or delete slices.');
             //Freemem(lSrcBuffer);
   end;
   if (lDorsalCrop < 0) or (lVentralCrop < 0)
      or (lLCrop < 0) or (lRCrop < 0)
      or (lACrop < 0) or (lPCrop < 0) then begin
             Showmessage('Grow 3D quitting: negative values should be impossible.');
             //Freemem(lSrcBuffer);
   end;
   //next compute size of cropped volume
   lInVolBytes := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*lBPP;
   lOutHdr.Dim[1] := lInHdr.Dim[1]-lLCrop-lRCrop;
   lOutHdr.Dim[2] := lInHdr.Dim[2]-lACrop-lPCrop;
   lOutHdr.Dim[3] := lInHdr.Dim[3]-lDorsalCrop-lVentralCrop;
   lOutVolBytes := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3]*lBPP;
   //next: readjust origin to take into account removed slices
   //REQUIRES images to be aligned to nearest orthogonal to canonical space [1 0 0; 0 1 0; 0 0 1]
   NIFTIhdr_SlicesToCoord (lInHdr,lLCrop,lPCrop,lVentralCrop, lXmm,lYmm,lZmm);
   lOutHdr.srow_x[3] := lInHdr.srow_x[3] + lXmm;
   lOutHdr.srow_y[3] := lInHdr.srow_y[3] + lYmm;
   lOutHdr.srow_z[3] := lInHdr.srow_z[3] + lZmm;
   lMat := Matrix3D (
	lOutHdr.srow_x[0], lOutHdr.srow_x[1], lOutHdr.srow_x[2], lOutHdr.srow_x[3],
	lOutHdr.srow_y[0], lOutHdr.srow_y[1], lOutHdr.srow_y[2], lOutHdr.srow_y[3],
	lOutHdr.srow_z[0], lOutHdr.srow_z[1], lOutHdr.srow_z[2], lOutHdr.srow_z[3],
	0, 0, 0, 1);
   nifti_mat44_to_quatern( lMat,
   lOutHdr.quatern_b,lOutHdr.quatern_c,lOutHdr.quatern_d,
   lOutHdr.qoffset_x,lOutHdr.qoffset_y,lOutHdr.qoffset_z,
                             lXmm, lYmm, lZmm, lOutHdr.pixdim[0]{QFac});
   //note we write to a different buffer, as we may need to grow output
   //no need to byteswap data - we will save in the save format as stored
   lOutPos := 0;
   lInSliceSz := lInHdr.dim[1]*lInHdr.dim[2]*lBPP;
   lInXSz := lInHdr.dim[1]*lBPP;
   GetMem(lBuffer,lOutVolBytes);
   //Move(gMRIcroOverlay[kBGOverlayNum].ImgBuffer^,lTempBuf^,gBGImg.VOIUndoVolItems);


   for lZ := 1 to lOutHdr.dim[3] do begin
       lInZOffset := (lVentralCrop+lZ-1) * lInSliceSz;
       if lInZOffset < 0 then
        lInZOffset := 0;
       for lY := 1 to lOutHdr.dim[2] do begin
           lInYOffset := ((lPCrop+lY-1) * lInXSz) + lInZOffset + (lLCrop*lBPP);
           for lX := 1 to lOutHdr.dim[1] do begin
               for lB := 1 to lBPP do begin
                   inc(lOutPos);
                   lInPos := ((lX-1) * lBPP) + lInYOffset + lB;
                   if (lInPos < 1) or (lInPos > lInVolBytes) then
                    lBuffer^[lOutPos] := 128
                   else
                    lBuffer^[lOutPos] := gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lInPos];
               end;
           end;
       end; //for Y
   end; //for Z
   lOutname := ChangeFilePrefix (gMRIcroOverlay[kBGOverlayNum].HdrFileName,'c');
   //result := SaveNIfTICore (lOutName, lSrcBuffer, kNIIImgOffset+1, lOutHdr, lPrefs,lByteSwap);
   result := gBGImg.UseReorientHdr;
   gBGImg.UseReorientHdr := false;
    SaveAsVOIorNIFTI (lBuffer,lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3], lBPP,1, false,  lOutHdr, lOutname);
   gBGImg.UseReorientHdr := result;
   result := true;
   Freemem(lBuffer);
end;


function GrowNeck (lFilename: string; lVox: integer): boolean;
//to do : data swapping (errors on detection and writing zero in reverse order)
var
   lInHdr,lOutHdr: TNIFTIhdr;
   lOutname,lExt: string;
   lXmm,lYmm,lZmm: single;
   lMat: TMatrix;
   lOutPos,lSlice,lVol,lOutVolBytes,lInVolBytes,lImgSamples,lInc,
   lX,lY,lZ,lBPP, lB,
   lVolOffset,lInZOffset,lInYOffset,lInSliceSz,lInXSz,lInPos,lImgOffset: integer;
   lBuffer: bytep;
   lWordX: Word;
   lSPM2: boolean;
   lOutF,lInF: File;
   lACrop,lPCrop,lDorsalCrop,lVentralCrop,lLCrop,lRCrop: integer;
   lByteSwap: boolean;
begin
    gBGImg.Prompt4DVolume := false;
    if not HdrForm.OpenAndDisplayHdr(lFilename,gMRIcroOverlay[kBGOverlayNum]) then exit;
     gBGImg.Prompt4DVolume := true;
    if not OpenImg(gBGImg,gMRIcroOverlay[kBGOverlayNum],false,false,false,false,true {4D!}) then exit;
     lInHdr := gMRIcroOverlay[kBGOverlayNum].NIFTIHdr;
     result := false;
     if (gMRIcroOverlay[kBGOverlayNum].ImgBufferItems < 1) or (lInHdr.dim[1] < 2) or (lInHdr.dim[2] < 2) then begin
        showmessage('Please load a 3D background image for neck removal.');
        exit;                     
     end;
     if (gBGImg.Resliced) then begin
        showmessage('You must switch reslicing OFF (Help/Preferences) for image cropping.');
        exit;
     end;

     //check orthogonal alignment....
   lOutHdr := lInHdr;
   lImgSamples := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3];
   lBPP := (lInHdr.bitpix div 8); //bytes per pixel
   lDorsalCrop := 0;
   lVentralCrop := lVox;
   lLCrop := 0;
   lRCrop := 0;
   lACrop := 0;
   lPCrop := 0;
   //FreeMem(lBuffUnaligned);
   if (lDorsalCrop = 0) and (lVentralCrop = 0)
      and (lLCrop = 0) and (lRCrop = 0)
      and (lACrop = 0) and (lPCrop = 0) then begin
             Showmessage('Grow 3D quitting: no need to add or delete slices.');
             //Freemem(lSrcBuffer);
   end;
   if (lDorsalCrop < 0) or (lVentralCrop < 0)
      or (lLCrop < 0) or (lRCrop < 0)
      or (lACrop < 0) or (lPCrop < 0) then begin
             Showmessage('Grow 3D quitting: negative values should be impossible.');
             //Freemem(lSrcBuffer);
   end;
   //next compute size of cropped volume

   lOutHdr.Dim[1] := lInHdr.Dim[1]-lLCrop-lRCrop;
   lOutHdr.Dim[2] := lInHdr.Dim[2]-lACrop-lPCrop;
   lOutHdr.Dim[3] := lInHdr.Dim[3]-lDorsalCrop-lVentralCrop;

   //next: readjust origin to take into account removed slices
   //REQUIRES images to be aligned to nearest orthogonal to canonical space [1 0 0; 0 1 0; 0 0 1]
   NIFTIhdr_SlicesToCoord (lInHdr,lLCrop,lPCrop,lVentralCrop, lXmm,lYmm,lZmm);
   lOutHdr.srow_x[3] := lInHdr.srow_x[3] + lXmm;
   lOutHdr.srow_y[3] := lInHdr.srow_y[3] + lYmm;
   lOutHdr.srow_z[3] := lInHdr.srow_z[3] + lZmm;
   lMat := Matrix3D (
	lOutHdr.srow_x[0], lOutHdr.srow_x[1], lOutHdr.srow_x[2], lOutHdr.srow_x[3],
	lOutHdr.srow_y[0], lOutHdr.srow_y[1], lOutHdr.srow_y[2], lOutHdr.srow_y[3],
	lOutHdr.srow_z[0], lOutHdr.srow_z[1], lOutHdr.srow_z[2], lOutHdr.srow_z[3],
	0, 0, 0, 1);
   nifti_mat44_to_quatern( lMat,
   lOutHdr.quatern_b,lOutHdr.quatern_c,lOutHdr.quatern_d,
   lOutHdr.qoffset_x,lOutHdr.qoffset_y,lOutHdr.qoffset_z,
                             lXmm, lYmm, lZmm, lOutHdr.pixdim[0]{QFac});
   //note we write to a different buffer, as we may need to grow output
   //no need to byteswap data - we will save in the save format as stored
   lOutPos := 0;
   lInSliceSz := lInHdr.dim[1]*lInHdr.dim[2]*lBPP;
   lInXSz := lInHdr.dim[1]*lBPP;
      lInVolBytes := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*lInHdr.dim[4]*lBPP;
   lOutVolBytes := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3]*lOutHdr.dim[4]*lBPP;
   GetMem(lBuffer,lOutVolBytes);
   //Move(gMRIcroOverlay[kBGOverlayNum].ImgBuffer^,lTempBuf^,gBGImg.VOIUndoVolItems);

   for lVol := 1 to lOutHdr.dim[4] do begin
      lVolOffset := (lVol-1) * lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]* lBPP;
      for lZ := 1 to lOutHdr.dim[3] do begin

       if lZ > -lVentralCrop then
        lInZOffset := ((lVentralCrop+lZ-1) * lInSliceSz)
       else
        lInZOffset := 0;
       for lY := 1 to lOutHdr.dim[2] do begin
           lInYOffset := ((lPCrop+lY-1) * lInXSz) + lInZOffset + (lLCrop*lBPP);
           for lX := 1 to lOutHdr.dim[1] do begin
               for lB := 1 to lBPP do begin
                   inc(lOutPos);
                   lInPos := ((lX-1) * lBPP) + lInYOffset + lB;
                   if (lInPos < 1) or (lInPos > lInVolBytes) then
                    lBuffer^[lOutPos] := 0
                   else
                    lBuffer^[lOutPos] := gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lInPos+lVolOffset];
               end;
           end;
       end; //for Y
      end; //for Z
   end; //lvol
   lOutname := ChangeFilePrefix (gMRIcroOverlay[kBGOverlayNum].HdrFileName,'c');
   //result := SaveNIfTICore (lOutName, lSrcBuffer, kNIIImgOffset+1, lOutHdr, lPrefs,lByteSwap);
   result := gBGImg.UseReorientHdr;
   gBGImg.UseReorientHdr := false;
   SaveAsVOIorNIFTI (lBuffer,lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3], lBPP,lOutHdr.dim[4], false,  lOutHdr, lOutname);
   gBGImg.UseReorientHdr := result;
   result := true;
   Freemem(lBuffer);
end;


end.