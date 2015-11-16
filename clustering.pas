unit clustering;
//USED by stats to select only regions with a given number of connected/contiguous voxels
interface
uses define_types,dialogs,SysUtils,nifti_hdr,nifti_img;

//procedure FindClusters (lMultiBuf: SingleP; lXdim, lYDim, lZDim, lThreshClusterSz: integer; lMinNeg, lMinPos: single);

function ClusterFilterScrnImg (var lHdr: TMRIcroHdr;  lThreshClusterSz: integer; lThresh: double ): boolean;


implementation





procedure FindClusters (var lHdr: TMRIcroHdr; lXdim, lYDim, lZDim, lThreshClusterSz: integer; lThresh: double);
var
   lThreshClusterSzM1,lScaledThresh,lClusterSign,lClusterSz,lClusterFillValue,lQTail,lQHead,lSliceSz,lQSz,lInc,lVolSz: integer;
   lClusterBuff, lQra: LongIntP;
   lBuffIn32 : SingleP;
   lBuffIn16 : SmallIntP;
   lScaledThreshFloat: double;
  //lFdata: file;//abba - test
const
     kFillValue = -2;
Procedure IncQra(var lVal, lQSz: integer);
begin
    inc(lVal);
    if lVal >= lQSz then
     lVal := 1;
end;

 procedure Check(lPixel: integer);
 begin
    //if lClusterFillValue = kFillvalue then showmessage(inttostr(lPixel)+'@');
    if (lClusterBuff^[lPixel]=lClusterSign) then begin//add item
        //if lClusterFillValue = kFillvalue then  showmessage(inttostr(lPixel));
        incQra(lQHead,lQSz);
        inc(lClusterSz);
        lClusterBuff^[lPixel] := lClusterFillValue;
        lQra^[lQHead] := lPixel;
   end;
 end;


PROCEDURE RetirePixel; //FIFO cleanup , 1410: added 18-voxel check
VAR
   lXDimM,lVal,lValX,lXPos,lYPos,lZPos: integer;
BEGIN
   lVal := lQra^[lQTail];
   if lVal = 0 then begin
      //should never happen: unmarked voxel  = increment lQTail so not infinite loop
      incQra(lQTail,lQSz); //done with this pixel
      exit;
   end;
   lXpos := lVal mod lXdim;
   if lXpos = 0 then lXPos := lXdim;

   lYpos := (1+((lVal-1) div lXdim)) mod lYDim;
   if lYPos = 0 then lYPos := lYdim;

   lZpos := ((lVal-1) div lSliceSz)+1;
   if (lXPos <= 1) or (lXPos >= lXDim) or
    (lYPos <= 1) or (lYPos >= lYDim) or
    (lZPos <= 1) or (lZPos >= lZDim) then
     // retire and exit
else begin
lXDimM := lXDim;
   Check(lVal-1); //left
   Check(lVal+1); //right
   Check(lVal-lXDimM); //up
   Check(lVal+lXDimM); //down
   Check(lVal-lSliceSz); //up
   Check(lVal+lSliceSz); //down
   //check plane above
   lValX := lVal + lSLiceSz;
   Check(lValX-1); //left
   Check(lValX+1); //right
   Check(lValX-lXDimM); //up
   Check(lValX+lXDimM); //down
   //check plane below
   lValX := lVal - lSLiceSz;
   Check(lValX-1); //left
   Check(lValX+1); //right
   Check(lValX-lXDimM); //up
   Check(lValX+lXDimM); //down
   //check diagonals of current plane
   Check(lVal-lXDimM-1); //up, left
   Check(lVal-lXDimM+1); //up, right

   Check(lVal+lXDimM-1); //down, left
   Check(lVal+lXDimM+1); //down, right
end;{} //not edge
   incQra(lQTail,lQSz); //done with this pixel
END;

procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
var lI: integer;
begin
  if (lClusterBuff^[lPt]<>lClusterSign) then exit;
  for lI := 1 to lQsz do
      lQra^[lI] := 0;
  lQHead := 0;
  lQTail := 1;
  Check(lPt);
  RetirePixel;
  // check that there was anything in the cluster at all
  //showmessage('head'+inttostr(lQHead)+'.'+inttostr(lQTail));
  //if lQHead > 2 then begin
    // and do the recursion to get rid of it
  while ((lQHead+1) <> lQTail) do begin//complete until all voxels in buffer have been tested
        RetirePixel;
        if (lQHead = lQSz) and (lQTail = 1) then
           exit; //break condition: avoids possible infinite loop where QTail is being incremented but QHead is stuck at maximum value
  end;
  //end;
  //showmessage('alldone');
end;

procedure SelectClusters (lSign: integer);
var lInc: integer;
begin
     for lInc := 1 to lVolSz do begin
         if lClusterBuff^[lInc] = lSign then begin
            // measure size of the cluster and fill it with kFillValue
            lClusterSz := 0;
            lClusterSign := lSign;
            lClusterFillValue := kFillValue;
            FillStart(lInc);
            // now fill the cluster with its size (=1 if the voxel was isolated)
            lClusterFillValue := lClusterSz;
            lClusterSign := kFillValue;
            //if lClusterSz > 1 then ShowMessage(inttostr(lClusterSz)+'@'+inttostr(lInc));
            if lClusterSz > 1 then
                FillStart(lInc)
            else
                lClusterBuff^[lInc] := 1; //fill all voxels in cluster with size of voxel
            end;
     end;
end;

begin
     lVolSz := lXdim*lYdim*lZdim;
     lSliceSz := lXdim * lYdim;
     if (lXDim < 4) or (lYDim < 4) or (lZDim < 4) or (lVolSz < 1)  then exit;
     GetMem(lClusterBuff, lVolSz* sizeof(LongInt));
     for lInc := 1 to lVolSz do
            lClusterBuff^[lInc] := 0;
     if lHdr.ImgBufferBPP = 4 then begin
	    lBuffIn32 := SingleP(lHdr.ImgBuffer);
        lScaledThreshFloat := Scaled2RawIntensity (lHdr, lThresh);
         for lInc := 1 to lVolSz do
             if lBuffIn32^[lInc] > lScaledThreshFloat then
                lClusterBuff^[lInc] := 1;
        lScaledThreshFloat := Scaled2RawIntensity (lHdr, -lThresh);
         for lInc := 1 to lVolSz do
             if lBuffIn32^[lInc] < lScaledThreshFloat then
                lClusterBuff^[lInc] := -1;
     end else if lHdr.ImgBufferBPP = 2 then begin  //not 32bit - if 16bit input
	     lBuffIn16 := SmallIntP(lHdr.ImgBuffer);
         lScaledThresh := round(Scaled2RawIntensity (lHdr, lThresh));
         for lInc := 1 to lVolSz do
             if lBuffIn16^[lInc] > lScaledThresh then
                lClusterBuff^[lInc] := 1;
         lScaledThresh := round(Scaled2RawIntensity (lHdr, -lThresh));
         for lInc := 1 to lVolSz do
             if lBuffIn16^[lInc] < lScaledThresh then
                lClusterBuff^[lInc] := -1;
     end else begin  //not 16 or 32 bit input
         lScaledThresh := round(Scaled2RawIntensity (lHdr, lThresh));
         for lInc := 1 to lVolSz do
             if lHdr.ImgBuffer^[lInc] > lScaledThresh then
                lClusterBuff^[lInc] := 1;
         lScaledThresh := round(Scaled2RawIntensity (lHdr, -lThresh));
         for lInc := 1 to lVolSz do
             if lHdr.ImgBuffer^[lInc] < lScaledThresh then
                lClusterBuff^[lInc] := -1;
     end; //8-bit input
     lThreshClusterSzM1 := lThreshClusterSz;
     if lThreshClusterSzM1 < 1 then
        lThreshClusterSzM1 := 1;
     if  (lThreshClusterSzM1 > 1) then begin
         //Next - START count cluster size
         lQSz := (lVolSz div 4)+8;
         GetMem(lQra,lQsz * sizeof(longint) );
         //check positive clusters....
         SelectClusters(1);
         //Check negative clusters
         SelectClusters(-1);
         Freemem(lQra);
         //END check clusters
     end; //only count clusters if minimum size > 1, otherwise simple intensity threshold...
     //NEXT: mask image data with cluster size

     if lHdr.ImgBufferBPP = 4 then begin
	    lBuffIn32 := SingleP(lHdr.ImgBuffer);
        for lInc := 1 to lVolSz do
            if lClusterBuff^[lInc] < lThreshClusterSzM1 then
                lBuffIn32^[lInc] := 0;
     end else if lHdr.ImgBufferBPP = 2 then begin
	      lBuffIn16 := SmallIntP(lHdr.ImgBuffer);

         for lInc := 1 to lVolSz do
             if lClusterBuff^[lInc] < lThreshClusterSzM1 then
                lBuffIn16^[lInc] := 0;
     end else  begin
         for lInc := 1 to lVolSz do
             if lClusterBuff^[lInc] < lThreshClusterSzM1 then
                lHdr.ImgBuffer^[lInc] := 0;
     end;
     Freemem(lClusterBuff);
end;

function ClusterFilterScrnImg (var lHdr: TMRIcroHdr; lThreshClusterSz: integer; lThresh: double ): boolean;
var
   lX,lY,lZ: integer;
begin
  result := false;
  lX := lHdr.NIFTIhdr.Dim[1];
  lY := lHdr.NIFTIhdr.Dim[2];
  lZ := lHdr.NIFTIhdr.Dim[3];

    if (lHdr.ImgBufferItems < (lX*lY*lZ)) then
     exit;
  FindClusters (lHdr, lX, lY, lZ, lThreshClusterSz, lThresh);
  result := true;
end;

end.
