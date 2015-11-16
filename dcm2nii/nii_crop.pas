unit nii_crop;
{$H+}
//TO DO: ByteSwap, DataTypes, Orthogonality

//VENTRAL direction: attempts to remove excess neck ...
//OTHER directions: zeros slices where signal intensity is <5%
//assumes image is oriented in canonical space, e.g.
//closest to rotation matrix [1 0 0; 0 1 0; 0 0 1]
//if your image is not rotated in this manner, use nii_orient first
interface

uses
{$IFDEF FPC}gzio2,{$ENDIF}
//distr,
  SysUtils,define_types,dicomtypes,niftiutil,GraphicsMathLibrary,prefs, nifti_types;
//function Int16LogPtoZNIfTI32Z(lFilename: string; lPrefs: TPrefs): string;
function CropNIfTI(lFilename: string; lPrefs: TPrefs): string;//returns output filename if successful
function Float32NIfTI(lFilename: string; lPrefs: TPrefs): string;
function FormulaNIfTI(lFilename: string; lPrefs: TPrefs; lScale,lPower: double): string;
//function RescaleNIfTI(lFilename: string; lPrefs: TPrefs; lScale: double): string;
function RemoveNIfTIscalefactor(lFilename: string; lPrefs: TPrefs): string;
function CropNIfTIX(lFilename: string; lPrefs: TPrefs; lDorsalCrop, lVentralCrop, lLCrop,lRCrop, lACrop, lPCrop : integer ): string;
function SiemensPhase2RadiansNIfTI(lFilename: string; lPrefs: TPrefs): string;

implementation
uses dialogsx,math,dialogs_msg;

function RemoveNIfTIscalefactor(lFilename: string; lPrefs: TPrefs): string;
//rescale data by  lScale
var
   lInHdr,lOutHdr: TNIFTIhdr;
   lOutname: string;
   l4DBytes,lInOffset,l1: integer;
   lInBuffer,lOutBuffer: bytep;
   lO: TNIIOpts;
begin
     result := '';
     //lExt := UpCaseExt(lFilename);
     if not NIFTIhdr_LoadHdr (lFilename, lInHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
   case lInHdr.datatype of
     kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT,kDT_UINT16, kDT_SIGNED_INT,kDT_FLOAT:;//Supported
     else begin
         dcmMsg('rescaleNIfTI unsupported datatype.');
         exit;
     end;
   end;
   dcmMsg('removing scale factor NIfTI/Analyze image '+lFileName);
   lOutHdr := lInHdr;
   lOutHdr.scl_slope := 1;
   lOutHdr.scl_inter := 0;
   l4DBytes := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*lInHdr.dim[4]*(lInHdr.bitpix div 8);
   if not NIFTIhdr_LoadImg (lFileName, lInHdr, lInBuffer, lInOffset,lO) then  exit;
   GetMem(lOutBuffer,l4DBytes+kNIIImgOffset);
   //lOutPos := kNIIImgOffset + 1;
   l1 := 1;
   Move(lInBuffer^[lInOffset+l1],lOutBuffer^[kNIIImgOffset + l1],l4DBytes);
   lOutname := ChangeFilePrefix (lFileName,'r');
   dcmMsg(lOutName);
   if  SaveNIfTICore (lOutName, lOutBuffer, kNIIImgOffset+1, lOutHdr, lPrefs) = '' then begin
      dcmMsg('Remove scale error');
      Freemem(lInBuffer);
      Freemem(lOutBuffer);
      exit;
   end;
      Freemem(lInBuffer);
      Freemem(lOutBuffer);
   result := lOutname;
end;

function FormulaNIfTI(lFilename: string; lPrefs: TPrefs; lScale,lPower: double): string;
//apply formula to dataset, then save results as 32-bit float....
var
   lInHdr,lOutHdr: TNIFTIhdr;
   lOutname,lExt: string;
   lAdj: single;
   lImgSamples,lInc,lImgOffset,lVol,lnVol,lPos: integer;
   lSrcBuffer,lBuffer, lBuffUnaligned,lBuffAligned: bytep;
   l32Buf,lImgBuffer: singlep;
   l16Buf : SmallIntP;
   lByteSwap: boolean;
   lO: TNIIOpts;
begin
     result := '';
     lExt := UpCaseExt(lFilename);
     if not NIFTIhdr_LoadHdr (lFilename, lInHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
   case lInHdr.datatype of
     kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT,kDT_UINT16, kDT_SIGNED_INT,kDT_FLOAT:;//Supported
     else begin
         dcmMsg('Error with nii_crop: unsupported datatype.');
         exit;
     end;
   end;
   dcmMsg('Applying formula to NIfTI/Analyze image'+lFileName);
   lOutHdr := lInHdr;
   lOutHdr.datatype := kDT_FLOAT;
   lOutHdr.bitpix := 32;
   lImgSamples := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*lInHdr.dim[4];
   if not NIFTIhdr_LoadImg (lFileName, lInHdr, lSrcBuffer, lImgOffset,lO) then  exit;
   //Msg('Automatically Cropping image');
   lBuffer := (@lSrcBuffer^[lImgOffset+1]);
   GetMem(lBuffUnaligned ,(sizeof(single)*lImgSamples) + 16+kNIIImgOffset);
   {$IFDEF FPC}
    lBuffAligned := Align(lBuffUnaligned,16); // not commented - check this
   {$ELSE}
   lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
   {$ENDIF}
   lInc := 1;
   lImgBuffer := SingleP(@lBuffAligned^[kNIIImgOffset+lInc]);
   case lInHdr.datatype of
           kDT_UNSIGNED_CHAR : begin //8 bit
             for lInc := 1 to lImgSamples do
                 lImgBuffer^[lInc] := lBuffer^[lInc];
             end;
	  kDT_SIGNED_SHORT{,kDT_UINT16}: begin //16-bit int
		l16Buf := SmallIntP(lBuffer );
		if lByteSwap then begin
                   for lInc := 1 to lImgSamples do
			  lImgBuffer^[lInc] := Swap(l16Buf^[lInc]);
                end else begin
                    for lInc := 1 to lImgSamples do
                        lImgBuffer^[lInc] := l16Buf^[lInc];
                end;
          end;//16bit
          kDT_SIGNED_INT: begin
		l32Buf := SingleP(lBuffer );
		if lByteSwap then //unswap and convert integer to float
			 for lInc := 1 to lImgSamples do
			  lImgBuffer^[lInc] := (Swap4r4i(l32Buf^[lInc]))
		else  //convert integer to float
                      for lInc := 1 to lImgSamples do
			  lImgBuffer^[lInc] := Conv4r4i(l32Buf^[lInc]);
		end; //32-bit int
	  kDT_FLOAT: begin
		l32Buf := SingleP(lBuffer);
                for lInc := 1 to lImgSamples do
                    lImgBuffer[lInc] := l32Buf[lInc];
		if lByteSwap then
                   for lInc := 1 to lImgSamples do
                       pswap4r(lImgBuffer^[lInc]);  //faster as procedure than function see www.optimalcode.com
		for lInc := 1 to lImgSamples do
			if specialsingle(lImgBuffer^[lInc]) then lImgBuffer^[lInc] := 0.0;
		 //invert= for lInc := 1 to lImgSamples do l32Buf[lInc] := -l32Buf[lInc];
          end; //32-bit float
         else begin
             dcmMsg('Serious error: format not supported by Float32.');
             exit;
         end;
   end; //case
   //apply formula
   lImgSamples := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]; //3D data
   lnVol := lInHdr.dim[4]; //4th dimension
   lPos := 0;
   //fx(lScale,lPower);
   for lVol := 1 to lnVol do begin
       lAdj := 1/(lScale* Power(lVol,lPower));
       for lInc := 1 to lImgSamples do begin
           inc(lPos);
           lImgBuffer^[lPos] := lAdj*lImgBuffer^[lPos];
           //lImgBuffer[lPos] := lImgBuffer[lPos] * lScale;
       end;
   end;
   lOutname := ChangeFilePrefix (lFileName,'f');
   result := SaveNIfTICore (lOutName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs);
   Freemem(lBuffUnaligned);
   Freemem(lSrcBuffer);
end;


function Float32NIfTI(lFilename: string; lPrefs: TPrefs): string;
//convert any data format as 32-bit float....
var
   lInHdr,lOutHdr: TNIFTIhdr;
   lOutname,lExt: string;
   lImgSamples,lInc,lImgOffset: integer;
   lSrcBuffer,lBuffer, lBuffUnaligned,lBuffAligned: bytep;
   l32Buf,lImgBuffer: singlep;
   l16Buf : SmallIntP;
   lO: TNIIOpts;
begin
     result := '';
     lExt := UpCaseExt(lFilename);
     if not NIFTIhdr_LoadHdr (lFilename, lInHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
   if lInHdr.datatype = kDT_FLOAT then begin
         dcmMsg('No need to apply Float32 : data is already 32-bit real: '+lFilename);
         exit;
   end;
   case lInHdr.datatype of
     kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT,kDT_UINT16, kDT_SIGNED_INT,kDT_FLOAT:;//Supported
     else begin
         dcmMsg('Float32 unsupported datatype.');
         exit;
     end;
   end;
   dcmMsg('Converting NIfTI/Analyze image to 32-bit float'+lFileName);
   lOutHdr := lInHdr;
   lOutHdr.datatype := kDT_FLOAT;
   lOutHdr.bitpix := 32;
   lImgSamples := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*lInHdr.dim[4];
   if not NIFTIhdr_LoadImg (lFileName, lInHdr, lSrcBuffer, lImgOffset,lO) then  exit;
   //Msg('Automatically Cropping image');
   lBuffer := (@lSrcBuffer^[lImgOffset+1]);
   GetMem(lBuffUnaligned ,(sizeof(single)*lImgSamples) + 16+kNIIImgOffset);
   {$IFDEF FPC}
   lBuffAligned := align(lBuffUnaligned,16);
   {$ELSE}
   lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
   {$ENDIF}
   lInc := 1;
   lImgBuffer := SingleP(@lBuffAligned^[kNIIImgOffset+lInc]);
   case lInHdr.datatype of
           kDT_UNSIGNED_CHAR : begin //8 bit
             for lInc := 1 to lImgSamples do
                 lImgBuffer^[lInc] := lBuffer^[lInc];
             end;
	  kDT_SIGNED_SHORT{,kDT_UINT16}: begin //16-bit int
		l16Buf := SmallIntP(lBuffer );

                    for lInc := 1 to lImgSamples do
                        lImgBuffer^[lInc] := l16Buf^[lInc];
          end;//16bit
          kDT_SIGNED_INT: begin

          for lInc := 1 to lImgSamples do
			        lImgBuffer^[lInc] := Conv4r4i(l32Buf^[lInc]);
		end; //32-bit int
	  kDT_FLOAT: begin
		l32Buf := SingleP(lBuffer);
                for lInc := 1 to lImgSamples do
                    lImgBuffer[lInc] := l32Buf[lInc];
		for lInc := 1 to lImgSamples do
			if specialsingle(lImgBuffer^[lInc]) then lImgBuffer^[lInc] := 0.0;
		 //invert= for lInc := 1 to lImgSamples do l32Buf[lInc] := -l32Buf[lInc];
          end; //32-bit float
         else begin
             dcmMsg('Serious error: format not supported by Float32.');
             exit;
         end;
   end; //case
   lOutname := ChangeFilePrefix (lFileName,'f');
   result := SaveNIfTICore (lOutName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs);
   Freemem(lBuffUnaligned);
   Freemem(lSrcBuffer);
end;


(*function LogPtoZ (lLogP: single): single;
var
   lD: double;
begin
     ///lD := Log10(lLogp);
     lD := Power(10,-lLogP);
     result := pNormalInv(lD);
     //fx(lD,lZ);
end;

function Int16LogPtoZNIfTI32Z(lFilename: string; lPrefs: TPrefs): string;
//convert any data format as 32-bit float....
var
   lInHdr,lOutHdr: TNIFTIhdr;
   lOutname,lExt: string;
   lImgSamples,lInc,lImgOffset: integer;
   lSrcBuffer,lBuffer, lBuffUnaligned,lBuffAligned: bytep;
   l32Buf,lImgBuffer: singlep;
   l16Buf : SmallIntP;
   lByteSwap: boolean;
begin
     result := '';
     lExt := UpCaseExt(lFilename);
     if not NIFTIhdr_LoadHdr (lFilename, lInHdr, lByteSwap) then begin
        Msg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
   if lInHdr.datatype = kDT_FLOAT then begin
         Msg('No need to apply Float32 : data is already 32-bit real: '+lFilename);
         exit;
   end;
   case lInHdr.datatype of
     kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT,kDT_UINT16, kDT_SIGNED_INT,kDT_FLOAT:;//Supported
     else begin
         Msg('Float32 unsupported datatype.');
         exit;
     end;
   end;
   Msg('Converting NIfTI/Analyze image to 32-bit float'+lFileName);
   lOutHdr := lInHdr;
   lOutHdr.datatype := kDT_FLOAT;
   lOutHdr.bitpix := 32;
   lImgSamples := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*lInHdr.dim[4];
   if not NIFTIhdr_LoadImg (lFileName, lInHdr, lSrcBuffer, lImgOffset,lByteSwap) then  exit;
   //Msg('Automatically Cropping image');
   lBuffer := (@lSrcBuffer^[lImgOffset+1]);
   GetMem(lBuffUnaligned ,(sizeof(single)*lImgSamples) + 16+kNIIImgOffset);
   {$IFDEF FPC}
   lBuffAligned := align(lBuffUnaligned,16);
   {$ELSE}
   lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
   {$ENDIF}
   lInc := 1;
   lImgBuffer := SingleP(@lBuffAligned^[kNIIImgOffset+lInc]);
   lOutHdr.scl_slope := 1;
   case lInHdr.datatype of
	  kDT_SIGNED_SHORT{,kDT_UINT16}: begin //16-bit int
		l16Buf := SmallIntP(lBuffer );
		if lByteSwap then begin
                   for lInc := 1 to lImgSamples do
			  lImgBuffer^[lInc] := Swap(l16Buf^[lInc]);
                end else begin
                    for lInc := 1 to lImgSamples do begin
                        if l16Buf^[lInc] =0 then
                           lImgBuffer^[lInc] := 0
                        else if l16Buf^[lInc] =32767 then
                           lImgBuffer^[lInc] := 0
                        else
                            lImgBuffer^[lInc] := LogPtoZ(0.01*l16Buf^[lInc]);
                    end;
                end;
          end;//16bit
         else begin
             Msg('Serious error: format not supported by Float32.');
             exit;
         end;
   end; //case
   lOutname := ChangeFilePrefix (lFileName,'f');
   result := SaveNIfTICore (lOutName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs,lByteSwap);
   Freemem(lBuffUnaligned);
   Freemem(lSrcBuffer);
end;*)


procedure SmoothRA (var lRA: Doublep; lItems: integer);
var
 lRecip: double;
 lTempRA,lTempRAUnaligned: Doublep;
 lI: integer;
begin
     if lItems < 3 then exit;
     GetMem(lTempRAUnaligned,(lItems*sizeof(double))+16);
     {$IFDEF FPC}
     lTempRA := align(lTempRAUnaligned,16);
     {$ELSE}
     lTempRA := DoubleP($fffffff0 and (integer(lTempRAUnaligned)+15));
     {$ENDIF}

     for lI := 1 to lItems do
         lTempRA^[lI] := lRA^[lI];
     lRecip := 1/3; //multiplies faster than divides
     for lI := 2 to (lItems-1) do
         lRA^[lI] := (lTempRA^[lI-1]+lTempRA^[lI]+lTempRA^[lI+1])*lRecip;
     FreeMem(lTempRAUnaligned);
end;

function MaxRA (var lRA: Doublep; lStart,lItems: integer): integer;
var
 lMax: double;
 lI: integer;
begin
     result := lStart;
     if (lItems < 2) or (lStart >= lItems) or ((lItems-lStart)< 1) then exit;
     lMax := lRA^[lStart];
     for lI := lStart to lItems do
         if lRA^[lI] > lMax then begin
             result := lI;
             lMax := lRA^[lI]
         end;
end;

function MinRA (var lRA: Doublep; lStart,lItems: integer): integer;
var
 lMin: double;
 lI: integer;
begin
     result := lStart;
     if (lItems < 2) or (lStart >= lItems) or ((lItems-lStart)< 1) then exit;
     lMin := lRA^[lStart];
     for lI := lStart to lItems do
         if lRA^[lI] < lMin then begin
             result := lI;
             lMin := lRA^[lI]
         end;
end;


function FindDVCrop2 (var lHdr: TNIFTIhdr;  var lDorsalCrop,lVentralCrop: integer): boolean;
const
     kMaxDVmm = 200;
var
   lSliceMM: double;
begin
     result := false;
     if lHdr.pixdim[3] < 0.0001 then
        exit;
     lSliceMM := lHdr.pixdim[3]* (lHdr.Dim[3]-lDorsalCrop-lVentralCrop);
     if lSliceMM > kMaxDVmm then begin //decide how many more ventral slices to remove
        lSliceMM := lSliceMM - kMaxDVmm;
        lSliceMM := lSliceMM / lHdr.pixdim[3];
        //msg(inttostr(lVentralCrop));
        lVentralCrop := lVentralCrop + round(lSliceMM);
        //msg(inttostr(lVentralCrop));
     end;
     result := true;
end;

function FindDVCrop (var lHdr: TNIFTIhdr; var ScrnBuffer: Singlep; var lDorsalCrop,lVentralCrop: integer; lPct: integer): boolean;
var
   lSliceMax: double;
   lSliceSum,lSliceSumUnaligned: Doublep;
   lXY,lZ,lSlices,lSliceSz,lSliceStart,lVentralMaxSlice,lMaxSlice,lMinSlice,lGap: integer;
begin
     result := false;
     lDorsalCrop := 0;
     lVentralCrop := 0;
     if (lPct < 1) or (lPct > 100) then
        exit;
     lSlices := lHdr.dim[3];
     lSliceSz := lHdr.dim[1]*lHdr.dim[2];
     GetMem(lSliceSumUnaligned,(lSlices*sizeof(double))+16);
     {$IFDEF FPC}
     lSliceSum := align(lSliceSumUnaligned,16);
     {$ELSE}
     lSliceSum := DoubleP($fffffff0 and (integer(lSliceSumUnaligned)+15));
     {$ENDIF}
     lSliceMax := 0;
     for lZ := 1 to lSlices do begin
         lSliceSum^[lZ] := 0;
         lSliceStart := (lZ-1)*lSliceSz;
         for lXY := 1 to lSliceSz do
             lSliceSum^[lZ] := lSliceSum^[lZ]+ ScrnBuffer^[lXY+lSliceStart];
         if  lSliceMax < lSliceSum^[lZ] then
             lSliceMax := lSliceSum^[lZ];
     end; //for each slice
     if lSliceMax = 0 then begin //no data variance
        Freemem(lSliceSumUnaligned);
        exit;
     end; //VolSum = 0
     //next: normalize so each slice is normalized to brightest axial slice
     for lZ := 1 to lSlices do
         lSliceSum^[lZ] := lSliceSum^[lZ]/lSliceMax;
     result := true;
     //next: smooth
     SmoothRA(lSliceSum,lSlices);
     //next - top cropping - removing slices that are <5%  of maximum slice
     lZ := lSlices;
     while (lZ > 1) and (lSliceSum^[lZ] < (lPct/100)) do
           dec(lZ);
     lDorsalCrop := lSlices-lZ;
     //next findMax
     lMaxSlice := MaxRA(lSliceSum,1,lSlices);
     //next - ensure there is at least 60mm from max to bottom of an image - enough spine to worry about
     lVentralMaxSlice := lMaxSlice-round(60/abs(lHdr.pixdim[3]));
     if lVentralMaxSlice < 1 then
        exit;
     lVentralMaxSlice := MaxRA(lSliceSum,1,lVentralMaxSlice);
     //finally: find minima between these two points...
     lMinSlice := MinRA(lSliceSum,lVentralMaxSlice,lMaxSlice);
     lGap := round((lMaxSlice-lMinSlice)*0.9);//add 40% for cerebellum
     if (lMinSlice-lGap) > 1 then begin
        result := true;
        lVentralCrop := lMinSlice-lGap;
     end;
     //fx(lVentralCrop,lDorsalCrop);
     //next show output...
     {TextForm.Memo1.Lines.Clear;
     for lZ := 1 to lSlices do
          TextForm.Memo1.Lines.add(inttostr(lZ)+','+floattostr(lSliceSum^[lZ]));
     TextForm.Show; }
     //cleanup
     Freemem(lSliceSumUnaligned);
     //next - max 200mm from top of head to spinal column....

     //if (lSliceMM > kMaxDVmm

end;

function FindLRCrop (var lHdr: TNIFTIhdr;  var ScrnBuffer: Singlep;  var lLCrop,lRCrop:integer; lPct,lDCrop,lVCrop: integer): boolean;
//amount of image to crop from left/right for N% signal intensity
var
   lSliceMax: double;
   lSliceSum,lSliceSumUnaligned: Doublep;
   lZmin,lZmax,lX,lY,lZ,lSlices,lSliceSz,lSliceStart: integer;
begin
     result := false;
     lLCrop := 0;
     lRCrop := 0;
     if (lPct < 1) or (lPct > 100) then
        exit;
     lZMin := lVCrop;
     lZMax := lHdr.Dim[3]-lDCrop;
     SortInt(lZMin,lZMax);
     lZMin := Bound(lZMin,1,lHdr.Dim[3]);
     lZMax := Bound(lZMax,1,lHdr.Dim[3]);
     if lZMin >= lZMax then
        exit;

     lSlices := lHdr.Dim[1];
     lSliceSz := lHdr.Dim[1]*lHdr.Dim[2];
     GetMem(lSliceSumUnaligned,(lSlices*sizeof(double))+16);
     {$IFDEF FPC}
     lSliceSum := align(lSliceSumUnaligned,16);
     {$ELSE}
     lSliceSum := DoubleP($fffffff0 and (integer(lSliceSumUnaligned)+15));
     {$ENDIF}
     lSliceMax := 0;
     for lX := 1 to lSlices do begin
         lSliceSum^[lX] := 0;
         for lZ := {1 to lHdr.Dim[3]} lZMin to lZMax do begin
             lSliceStart := lX+ ((lZ-1)*lSliceSz);
             for lY := 1 to lHdr.Dim[2] do begin
                 lSliceSum^[lX] := lSliceSum^[lX]+ ScrnBuffer^[lSliceStart];
                 lSliceStart := lSliceStart + lHdr.Dim[1];
             end;
         end;
         //for lYZ := 1 to lSliceSz do
         //    lSliceSum^[lZ] := lSliceSum^[lZ]+ gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lXY+lSliceStart];
         if  lSliceMax < lSliceSum^[lX] then
             lSliceMax := lSliceSum^[lX];
     end; //for each slice
     if lSliceMax = 0 then begin //no data variance
        Freemem(lSliceSumUnaligned);
        exit;
     end; //VolSum = 0
     //next: smooth
     SmoothRA(lSliceSum,lSlices);
     //next: normalize so each slice is normalized to brightest axial slice
     for lX := 1 to lSlices do
         lSliceSum^[lX] := lSliceSum^[lX]/lSliceMax;
     //next - Left cropping- removing slices that are <5%  of maximum slice
     lX := lSlices;
     while (lX > 1) and (lSliceSum^[lX] < (lPct/100)) do
           dec(lX);
     lRCrop := lSlices-lX;
     //next - Left cropping- removing slices that are <5%  of maximum slice
     lX := 1;
     while (lX <= lSlices) and (lSliceSum^[lX] < (lPct/100)) do
           inc(lX);
     lLCrop := lX-1;
     //fx(lLCrop,lRCrop);
     result := true;
     Freemem(lSliceSumUnaligned);
end;

function FindAPCrop (var lHdr: TNIFTIhdr; var ScrnBuffer: Singlep;  var lACrop,lPCrop: integer; lPct,lDCrop,lVCrop: integer): boolean;
//amount of image to crop from anterior/posterior for 5% signal intensity
var
   lSliceMax: double;
   lSliceSum,lSliceSumUnaligned: Doublep;
   lZMin,lZMax,lX,lY,lZ,lSlices,lSliceSz,lSliceStart: integer;
begin
     result := false;
     lACrop := 0;
     lPCrop := 0;
     lZMin := lVCrop;
     lZMax := lHdr.Dim[3]-lDCrop;
     SortInt(lZMin,lZMax);
     lZMin := Bound(lZMin,1,lHdr.Dim[3]);
     lZMax := Bound(lZMax,1,lHdr.Dim[3]);
     if lZMin >= lZMax then
        exit;
     if (lPct < 1) or (lPct > 100) then
        exit;
     lSlices := lHdr.Dim[2];
     lSliceSz := lHdr.Dim[1]*lHdr.Dim[2];
     //lCoroSliceSz := lHdr.Dim[1]*lHdr.Dim[3];
     GetMem(lSliceSumUnaligned,(lSlices*sizeof(double))+16);
     {$IFDEF FPC}
     lSliceSum := align(lSliceSumUnaligned,16);
     {$ELSE}
     lSliceSum := DoubleP($fffffff0 and (integer(lSliceSumUnaligned)+15));
     {$ENDIF}
     lSliceMax := 0;
     for lY := 1 to lSlices do begin
         lSliceSum^[lY] := 0;
         //lSliceStart := lY;
         for lZ := {1 to lHdr.Dim[3]} lZMin to lZMax do begin
             lSliceStart := ((lY-1)* lHdr.Dim[1])+ ((lZ-1)*lSliceSz);
             //if lSliceStart > (lSliceSz*lHdr.Dim[3]) then
             //   Msg('xx');
             for lX := 1 to lHdr.Dim[1] do
                 lSliceSum^[lY] := lSliceSum^[lY]+ ScrnBuffer^[lSliceStart+lX];
         end; //for lZ
         //for lYZ := 1 to lSliceSz do
         //    lSliceSum^[lY] := lSliceSum^[lY]+ gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lXY+lSliceStart];
         if  lSliceMax < lSliceSum^[lY] then
             lSliceMax := lSliceSum^[lY];
     end; //for each slice
     if lSliceMax = 0 then begin //no data variance
        Freemem(lSliceSumUnaligned);
        exit;
     end; //VolSum = 0
     //next: smooth
     SmoothRA(lSliceSum,lSlices);
     //next: normalize so each slice is normalized to brightest axial slice
     for lY := 1 to lSlices do
         lSliceSum^[lY] := lSliceSum^[lY]/lSliceMax;
     //next - Left cropping- removing slices that are <5%  of maximum slice
     lY := lSlices;
     while (lY > 1) and (lSliceSum^[lY] < (lPct/100)) do
           dec(lY);
     lACrop := lSlices-lY;
     //next - Left cropping- removing slices that are <5%  of maximum slice
     lY := 1;
     while (lY <= lSlices) and (lSliceSum^[lY] < (lPct/100)) do
           inc(lY);
     lPCrop := lY-1;
     result := true;
     Freemem(lSliceSumUnaligned);
end;

function CropNIfTIX(lFilename: string; lPrefs: TPrefs; lDorsalCrop, lVentralCrop, lLCrop,lRCrop, lACrop, lPCrop : integer ): string;
//to do : data swapping (errors on detection and writing zero in reverse order)
var
   lInHdr,lOutHdr: TNIFTIhdr;
   lOutname,lExt: string;
   lXmm,lYmm,lZmm: single;
   lMat: TMatrix;
   lOutPos,//lInc,     //lImgSamples,
   lX,lY,lZ,lBPP, lB,
   lVol, lInVol,lInZOffset,lInYOffset,lInSliceSz,lInXSz,lInPos,lImgOffset: integer;
   lSrcBuffer,lBuffer//, lBuffUnaligned
   : bytep;
   //l32Buf,lImgBuffer: singlep;
   //l16Buf : SmallIntP;
   //lWordX: Word;
   //lSPM2: boolean;
   lOutF,lInF: File;
   lO: TNIIOpts;
begin
   result := '';
   if (lDorsalCrop = 0) and (lVentralCrop = 0)
      and (lLCrop = 0) and (lRCrop = 0)
      and (lACrop = 0) and (lPCrop = 0) then begin
             dcmMsg('Crop slices quitting: no need to delete slices.');
             exit;   //25 Sept 2008
   end;
   if (lDorsalCrop < 0) or (lVentralCrop < 0)
      or (lLCrop < 0) or (lRCrop < 0)
      or (lACrop < 0) or (lPCrop < 0) then begin
             dcmMsg('Crop slices quitting: negative values should be impossible.');
             exit;
   end;
     result := '';
     lExt := UpCaseExt(lFilename);
     if not NIFTIhdr_LoadHdr (lFilename, lInHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
     //Next create reordered or trimmed image in the correct format
   case lInHdr.datatype of
     kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT,kDT_UINT16, kDT_SIGNED_INT,kDT_FLOAT:;//Supported
     else begin
         dcmMsg('Crop 3D unsupported datatype.');
         exit;
     end;
   end;
   dcmMsg('Cropping NIfTI/Analyze image '+lFileName);
   lOutHdr := lInHdr;
   //lImgSamples := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3];
   lBPP := (lInHdr.bitpix div 8); //bytes per pixel
  if not NIFTIhdr_LoadImg (lFileName, lInHdr, lSrcBuffer, lImgOffset,lO) then  exit;
   //dcmMsg('Automatically Cropping image');
    lBuffer := (@lSrcBuffer^[lImgOffset+1]);
   //next compute size of cropped volume
   lOutHdr.Dim[1] := lInHdr.Dim[1]-lLCrop-lRCrop;
   lOutHdr.Dim[2] := lInHdr.Dim[2]-lACrop-lPCrop;
   lOutHdr.Dim[3] := lInHdr.Dim[3]-lDorsalCrop-lVentralCrop;
   if (lOutHdr.Dim[1] < 1) or (lOutHdr.Dim[2] <12) or (lOutHdr.Dim[3] < 1) then begin
        dcmMsg('Requested to crop more slices than available.');
        Freemem(lSrcBuffer);
        exit;
   end;
   //lVolBytes := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3]*lBPP;
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
   //note we write and read to the same buffer - we will always SHRINK output
   //no need to byteswap data - we will save in the save format as stored

   lOutPos := 0;
   lInSliceSz := lInHdr.dim[1]*lInHdr.dim[2]*lBPP;
   lInXSz := lInHdr.dim[1]*lBPP;
   for lVol := 1 to lOutHdr.dim[4] do begin
     lInVol := (lVol-1) * (lInSliceSz * lInHdr.dim[3]);
     //fx(lInVol,lVol);
     for lZ := 1 to lOutHdr.dim[3] do begin
       lInZOffset := (lVentralCrop+lZ-1) * lInSliceSz;
       for lY := 1 to lOutHdr.dim[2] do begin
           lInYOffset := ((lPCrop+lY-1) * lInXSz) + lInZOffset + (lLCrop*lBPP);
           for lX := 1 to lOutHdr.dim[1] do begin
               for lB := 1 to lBPP do begin
                   inc(lOutPos);
                   lInPos := ((lX-1) * lBPP) + lInYOffset + lB;
                   lBuffer^[lOutPos] := lBuffer^[lInPos+lInVol];
               end;
           end;
       end; //for Y
     end; //for Z
   end; //for Vol
   lOutname := ChangeFilePrefix (lFileName,'c');
   result := SaveNIfTICore (lOutName, lSrcBuffer, kNIIImgOffset+1, lOutHdr, lPrefs);
   Freemem(lSrcBuffer);
end;

function CropNIfTI(lFilename: string; lPrefs: TPrefs): string;
//to do : data swapping (errors on detection and writing zero in reverse order)
var
   lInHdr,lOutHdr: TNIFTIhdr;
   lOutname,lExt: string;
   lXmm,lYmm,lZmm: single;
   lMat: TMatrix;
   lOutPos,lImgSamples,lInc,
   lX,lY,lZ,lBPP, lB,
   lInZOffset,lInYOffset,lInSliceSz,lInXSz,lInPos,lImgOffset: integer;
   lSrcBuffer,lBuffer, lBuffUnaligned: bytep;
   l32Buf,lImgBuffer: singlep;
   l16Buf : SmallIntP;
   //lOutF,lInF: File;
   lACrop,lPCrop,lDorsalCrop,lVentralCrop,lLCrop,lRCrop: integer;
   lO: TNIIOpts;
begin
     result := '';
     lExt := UpCaseExt(lFilename);
     if not NIFTIhdr_LoadHdr (lFilename, lInHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
     if (lInHdr.dim[1] > lPrefs.MaxReorientMatrix) or (lInHdr.dim[2] > lPrefs.MaxReorientMatrix) or(lInHdr.dim[3] > lPrefs.MaxReorientMatrix) then begin
      dcmMsg('This image will not be cropped (larger than MaxReorientMatrix= '+inttostr(lPrefs.MaxReorientMatrix));
      exit;
     end;
     //check orthogonal alignment....
     if lInHdr.dim[4] > 1 then begin
        dcmMsg('Only able to Crop 3D images (reorienting 4D could disrupt slice timing and diffusion directions.');
        exit;
     end;
     //Next create reordered or trimmed image in the correct format
   case lInHdr.datatype of
     kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT,kDT_UINT16, kDT_SIGNED_INT,kDT_FLOAT:;//Supported
     else begin
         dcmMsg('Crop 3D unsupported datatype.');
         exit;
     end;
   end;

   dcmMsg('Cropping NIfTI/Analyze image '+lFileName);
   lOutHdr := lInHdr;
   lImgSamples := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3];
   lBPP := (lInHdr.bitpix div 8); //bytes per pixel
   //lVolBytes := lImgSamples*lBPP;
  if not NIFTIhdr_LoadImg (lFileName, lInHdr, lSrcBuffer, lImgOffset,lO) then  exit;
   //dcmMsg('Automatically Cropping image');
    lBuffer := (@lSrcBuffer^[lImgOffset+1]);
   GetMem(lBuffUnaligned ,(sizeof(single)*lImgSamples) + 16);
   {$IFDEF FPC}
   lImgBuffer :=align(lBuffUnaligned,16);
   {$ELSE}
   lImgBuffer := SingleP($fffffff0 and (integer(lBuffUnaligned)+15));
   {$ENDIF}
  case lInHdr.datatype of
           kDT_UNSIGNED_CHAR : begin //8 bit
             for lInc := 1 to lImgSamples do
                 lImgBuffer^[lInc] := lBuffer^[lInc];
             end;

	  kDT_SIGNED_SHORT{,kDT_UINT16}: begin //16-bit int
		l16Buf := SmallIntP(lBuffer );

                    for lInc := 1 to lImgSamples do
                        lImgBuffer^[lInc] := l16Buf^[lInc];

          end;//16bit
          kDT_SIGNED_INT: begin
		l32Buf := SingleP(lBuffer );
		 //convert integer to float
                      for lInc := 1 to lImgSamples do
			  lImgBuffer^[lInc] := Conv4r4i(l32Buf^[lInc]);
		end; //32-bit int
	  kDT_FLOAT: begin
		l32Buf := SingleP(lBuffer);
                for lInc := 1 to lImgSamples do
                    lImgBuffer[lInc] := l32Buf[lInc];
				for lInc := 1 to lImgSamples do
			if specialsingle(lImgBuffer^[lInc]) then lImgBuffer^[lInc] := 0.0;
		 //invert= for lInc := 1 to lImgSamples do l32Buf[lInc] := -l32Buf[lInc];
          end; //32-bit float
         else begin
             dcmMsg('Serious error: format not supported by Crop3D.');
             exit;
         end;
   end; //case
   FindDVCrop (lInHdr, lImgBuffer, lDorsalCrop,lVentralCrop, 5);
   FindDVCrop2 (lInHdr, lDorsalCrop,lVentralCrop);
   FindLRCrop (lInHdr, lImgBuffer, lLCrop,lRCrop,3,lDorsalCrop,lVentralCrop);//3% often sagittal scans near brain
   FindAPCrop (lInHdr, lImgBuffer, lACrop,lPCrop, 5,lDorsalCrop,lVentralCrop);
   FreeMem(lBuffUnaligned);
   if (lDorsalCrop = 0) and (lVentralCrop = 0)
      and (lLCrop = 0) and (lRCrop = 0)
      and (lACrop = 0) and (lPCrop = 0) then begin
             dcmMsg('Crop 3D quitting: no need to delete slices.');
             Freemem(lSrcBuffer);
             exit;   //25 Sept 2008
   end;
   if (lDorsalCrop < 0) or (lVentralCrop < 0)
      or (lLCrop < 0) or (lRCrop < 0)
      or (lACrop < 0) or (lPCrop < 0) then begin
             dcmMsg('Crop 3D quitting: negative values should be impossible.');
             beep;
             Freemem(lSrcBuffer);
   end;
   //next compute size of cropped volume
   lOutHdr.Dim[1] := lInHdr.Dim[1]-lLCrop-lRCrop;
   lOutHdr.Dim[2] := lInHdr.Dim[2]-lACrop-lPCrop;
   lOutHdr.Dim[3] := lInHdr.Dim[3]-lDorsalCrop-lVentralCrop;
   //lVolBytes := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3]*lBPP;
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
   //note we write and read to the same buffer - we will always SHRINK output
   //no need to byteswap data - we will save in the save format as stored


   lOutPos := 0;
   lInSliceSz := lInHdr.dim[1]*lInHdr.dim[2]*lBPP;
   lInXSz := lInHdr.dim[1]*lBPP;
   for lZ := 1 to lOutHdr.dim[3] do begin
       lInZOffset := (lVentralCrop+lZ-1) * lInSliceSz;
       for lY := 1 to lOutHdr.dim[2] do begin
           lInYOffset := ((lPCrop+lY-1) * lInXSz) + lInZOffset + (lLCrop*lBPP);
           for lX := 1 to lOutHdr.dim[1] do begin
               for lB := 1 to lBPP do begin
                   inc(lOutPos);
                   lInPos := ((lX-1) * lBPP) + lInYOffset + lB;
                   lBuffer^[lOutPos] := lBuffer^[lInPos];
               end;
           end;
       end; //for Y
   end; //for Z
   lOutname := ChangeFilePrefix (lFileName,'c');
   result := SaveNIfTICore (lOutName, lSrcBuffer, kNIIImgOffset+1, lOutHdr, lPrefs);
   Freemem(lSrcBuffer);
end;


(*function CropNIfTI(lFilename: string; lPrefs: TPrefs): string;
var

   lTempName,lExt,lNameWOExt: string;
   lHdr: TNIFTIhdr;
   lByteSwap: boolean;
begin
     result := '';
     lExt := UpCaseExt(lFilename);
     if not NIFTIhdr_LoadHdr (lFilename, lHdr, lByteSwap) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
     //check orthogonal alignment....
     if lHdr.dim[4] > 1 then begin
        dcmMsg('Only able to Crop 3D images (reorienting 4D could disrupt slice timing and diffusion directions.');
        exit;
     end;
     //next - determine output format

     if lExt = '.NII.GZ' then begin
        //lTempName := lFilename;//ChangeFilePrefixExt (lFileName,'x');
        ExtractFileParts (lFileName, lNameWOExt,lExt);
        lTempName := lNameWOExt+'.nii';
        Gunzip(lFileName,lTempName);
        lFilename := lTempName;
     end else //not gzip
         lTempName := '';
     //Next create reordered or trimmed image in the correct format
     result := Crop(lFileName, lHdr,lByteSwap, lPrefs.SPM2,lPrefs.SingleNIIFile, false);

     if (result <> '') and (lPrefs.GZip) then begin
             GZipFile(lFileName,lFileName+'.gz',true);
             result := result +'.gz';
     end;

     if lTempName <> '' then //delete GZip temp
        deletefile(lTempName);
end;  *)

function SiemensPhase2RadiansNIfTI(lFilename: string; lPrefs: TPrefs): string;
//convert any data format as 32-bit float....
var
   lInHdr,lOutHdr: TNIFTIhdr;
   lOutname,lExt: string;
   lMax,lMin,lImgSamples,lInc,lImgOffset: integer;
   lSrcBuffer,lBuffer, lBuffUnaligned,lBuffAligned: bytep;
   //l32Buf,
   lImgBuffer: singlep;
   l16Buf : SmallIntP;
   lO: TNIIOpts;
begin
     result := '';
     lExt := UpCaseExt(lFilename);
     if not NIFTIhdr_LoadHdr (lFilename, lInHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lFilename);
        exit;
     end;
   if lInHdr.datatype <> kDT_SIGNED_SHORT then begin
         dcmMsg('Unable to run SiemensPhase2Radians : input image must be 16-bit NIfTI image with intensities 0..4096 corresponding to -pi..+pi : '+lFilename);
         exit;
   end;
   //dcmMsg('SiemensPhase2Radians converting 16-bit image (0..4095) to 32-bit float (-pi..+pi).'+lFileName);
   lOutHdr := lInHdr;
   lOutHdr.datatype := kDT_FLOAT;
   lOutHdr.bitpix := 32;
   lOutHdr.scl_slope := 1;
   lOutHdr.scl_inter := 0;
   lImgSamples := lInHdr.dim[1]*lInHdr.dim[2]*lInHdr.dim[3]*lInHdr.dim[4];
   if not NIFTIhdr_LoadImg (lFileName, lInHdr, lSrcBuffer, lImgOffset,lO) then  exit;
   lBuffer := (@lSrcBuffer^[lImgOffset+1]);
   GetMem(lBuffUnaligned ,(sizeof(single)*lImgSamples) + 16+kNIIImgOffset);
   {$IFDEF FPC}
   lBuffAligned := align(lBuffUnaligned,16);
   {$ELSE}
   lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
   {$ENDIF}
   lInc := 1;
   lImgBuffer := SingleP(@lBuffAligned^[kNIIImgOffset+lInc]);
   l16Buf := SmallIntP(lBuffer );
   lMax := l16Buf^[1];
   for lInc := 1 to lImgSamples do
       if l16Buf^[lInc] > lMax then
          lMax := l16Buf^[lInc];
   lMin := l16Buf^[1];
   for lInc := 1 to lImgSamples do
       if l16Buf^[lInc] < lMin then
          lMin := l16Buf^[lInc];
   if (lMin < 0) or (lMax > 4096) then
      dcmMsg('Error: SiemensPhase2Radians expects input data with raw intensity ranging from 0..4096 (corresponding to -pi..+pi) - this image''s intensity is not in these bounds'+lFileName)
   else begin
       dcmMsg('SiemensPhase2Radians converting 0..4096 to -pi..+pi '+ lFilename);
       //Excel formula  =((A1-2048)/2048)*PI()
       //fx(lMin,lMax);
       for lInc := 1 to lImgSamples do
           lImgBuffer^[lInc] := ((l16Buf^[lInc]-2048)/2048)*pi;
       lOutname := ChangeFilePrefix (lFileName,'rad');
       result := SaveNIfTICore (lOutName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs);
   end;
   Freemem(lBuffUnaligned);
   Freemem(lSrcBuffer);
end;



end.
