unit sliceinterpolate;

interface
uses define_types, dialogs, nifti_hdr;

procedure ROISliceInterpolate (var lHdr: TMRIcroHdr);

implementation

procedure ROISliceInterpolate (var lHdr: TMRIcroHdr);
const
  knRef = 14;
  kRefSlices: array [1..knRef] of integer	= (32,40,48,56,64,72,80,88,96,104,112,122,132,142);
var
  l8Buf: Bytep;
  l16Buf : SmallIntP;
  l32Buf : SingleP;
  lXDim,lYDim,lZDim,
	lLo,lHi,lRef,lZ,lY,lX,lV,lSliceSz,lLoSliceStart,lHiSliceStart,lSliceStart,lPos: integer;
	lLoFrac,lHiFrac: single;
begin
     lXDim := lHdr.NIFTIhdr.dim[1];
     lYDim := lHdr.NIFTIhdr.dim[2];
     lZDim := lHdr.NIFTIhdr.dim[3];
	for lRef := 1 to knRef do
		if kRefSlices[lRef] > lZDim then begin
			showmessage('Out of bounds');
			exit;
		end;
	 lSliceSz := lXDim * lYDim;

	 for lZ := kRefSlices[1] to kRefSlices[knRef] do begin
		 for lRef := 1 to knRef do
			if  kRefSlices[lRef]  <= lZ then
				lLo := kRefSlices[lRef];
		 for lRef :=  knRef downto 1 do
			if kRefSlices[lRef] >= lZ then
				lHi := kRefSlices[lRef];
		 if lLo <> lHi then begin //do not interpolate reference slices
			lHiFrac := (lZ - lLo) /(lHi-lLo);
			lLoFrac := 1 - lHiFrac;
			lSliceStart := lSliceSz * (lZ-1);
			lLoSliceStart := lSliceSz * (lLo-1);
			lHiSliceStart := lSliceSz * (lHi-1);
      if lHdr.ImgBufferBPP = 4 then begin
	      l32Buf := SingleP(lHdr.ImgBuffer);
			  for lPos := 1 to lSliceSz do
				 l32Buf^[lPos + lSliceStart] :=((lLoFrac*l32Buf^[lPos + lLoSliceStart]) +(lHiFrac*l32Buf^[lPos + lHiSliceStart]));
      end else if lHdr.ImgBufferBPP = 2 then begin  //not 32bit - if 16bit input
        l16Buf := SmallIntP(lHdr.ImgBuffer);
			  for lPos := 1 to lSliceSz do
          l16Buf^[lPos + lSliceStart] := round((lLoFrac*l16Buf^[lPos + lLoSliceStart]) +(lHiFrac*l16Buf^[lPos + lHiSliceStart])   );
      end else if lHdr.ImgBufferBPP = 1 then begin  //not 8bit input
        l8Buf := lHdr.ImgBuffer;
			  for lPos := 1 to lSliceSz do
          l8Buf^[lPos + lSliceStart] := round((lLoFrac*l8Buf^[lPos + lLoSliceStart]) +(lHiFrac*l8Buf^[lPos + lHiSliceStart])   );
      end; //
		 end; //if lLo <> lHi
	 end;//for lZ
end; //ezInterpolate

end.
 