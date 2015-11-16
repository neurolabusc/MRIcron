unit fastsmooth;

{$mode delphi}

interface

uses
 // LCLIntf,//<- only for gettickcount
  Classes, SysUtils, define_types, otsuml;
 procedure DilateSphere (var lImg: Bytep; lXi,lYi,lZi: integer; lVoxDistance: single; lChange: byte  );
procedure SmoothFWHM2Vox (var lImg: Bytep; lXi,lYi,lZi: integer);
procedure Dilate (var lImg: Bytep; lXi,lYi,lZi,lCycles: integer; lChange: byte  );
procedure PreserveLargestCluster (var lImg: Bytep; lXi,lYi,lZi: integer; lClusterValue,ValueForSmallClusters: byte  );
 procedure MaskBackground  (var lImg: Bytep; lXi,lYi,lZi,lOtsuLevels: integer; lDilateVox: single; lOneContiguousObject: boolean );
implementation

procedure MaskBackground  (var lImg: Bytep; lXi,lYi,lZi,lOtsuLevels: integer; lDilateVox: single; lOneContiguousObject: boolean );
var
  lMask: ByteP;
  lX,lY,lZ,lV,lXYZ: integer;
begin
   lXYZ := lXi * lYi * lZi;
   if (lXi < 3) or (lYi < 3) or (lZi < 1) then
      exit;
   getmem(lMask, lXYZ);
   Move(lImg^[1], lMask^[1],lXYZ);
   SmoothFWHM2Vox(lMask, lXi,lYi,lZi);
   ApplyOtsuBinary (lMask, lXYZ,lOtsuLevels);
   //Dilate (lMask, lXi,lYi,lZi,5,255 );

   if lOneContiguousObject then begin
      PreserveLargestCluster (lMask, lXi,lYi,lZi,255,0 ); //only preserve largest single object
      if lDilateVox >= 1 then
         DilateSphere (lMask, lXi,lYi,lZi,lDilateVox,255 );
   end else begin
         if lDilateVox >= 1 then
            DilateSphere (lMask, lXi,lYi,lZi,lDilateVox,255 );
         PreserveLargestCluster (lMask, lXi,lYi,lZi,0,255 ); //only erase outside air
   end;
   lV:=0;
   for lZ := 1 to lZi do
       for lY := 1 to lYi do
           for lX := 1 to lXi do begin
             inc(lV);
             if (lMask^[lV] = 0) or (lX=1) or (lX=lXi) or (lY=1) or (lY=lYi) or (lZ=1) or (lZ=lZi) then
                  lImg^[lV] := 0;

           end;
   freemem(lMask);
end;

(*procedure MaskBackground  (var lImg: Bytep; lXi,lYi,lZi: integer);
var
  lMask: ByteP;
  lX,lXYZ: integer;
begin
   lXYZ := lXi * lYi * lZi;
   if (lXi < 3) or (lYi < 3) or (lZi < 1) then
      exit;
   getmem(lMask, lXYZ);
   Move(lImg^[1], lMask^[1],lXYZ);
   SmoothFWHM2Vox(lMask, lXi,lYi,lZi);
   ApplyOtsuBinary (lMask, lXYZ);
   //Dilate (lMask, lXi,lYi,lZi,5,255 );
   DilateSphere (lMask, lXi,lYi,lZi,5,255 );
   PreserveLargestCluster (lMask, lXi,lYi,lZi,0,255 );
   for lX := 1 to lXYZ do
       if lMask^[lX] = 0 then
          lImg^[lX] := 0;
   freemem(lMask);
end;*)

procedure CountClusterSize (var lImg: Bytep; var lClusterBuff: longintp; lXi,lYi,lZi: integer; lClusterValue: byte);
//Given volume lImg, will generate volume lCount with number of connected voxels with value lCluster
var
  lStart: DWord;
  lTopSlice,lInc,lXY,lXYZ,lClusterSign,lQTail,lQHead,lQSz,lClusterSz,lClusterFillValue: integer;
    lQra: LongIntP;
const
     kFillValue = -2;
Procedure IncQra(var lVal, lQSz: integer);
begin
    inc(lVal);
    if lVal >= lQSz then
     lVal := 1;
end; //nested incQra
procedure Check(lPixel: integer);
 begin
    if (lClusterBuff^[lPixel]=lClusterSign) then begin//add item
        incQra(lQHead,lQSz);
        inc(lClusterSz);
        lClusterBuff^[lPixel] := lClusterFillValue;
        lQra^[lQHead] := lPixel;
   end;
end;//nested Check
PROCEDURE RetirePixel; //FIFO cleanup , 1410: added 18-voxel check
var
    lVal: integer;
BEGIN
   lVal := lQra^[lQTail];

   if (lVal < lTopSlice) and (lVal > lXY) then begin
   (*   //next code avoids left-right and anterior-posterior wrapping...
      if lVal = 0 then begin
      //should never happen: unmarked voxel  = increment lQTail so not infinite loop
      incQra(lQTail,lQSz); //done with this pixel
      exit;
   end;
   lXpos := lVal mod lXi;
   if lXpos = 0 then lXPos := lXi;

   lYpos := (1+((lVal-1) div lXi)) mod lYi;
   if lYPos = 0 then lYPos := lYi;

   lZpos := ((lVal-1) div lXY)+1;
   if (lXPos <= 1) or (lXPos >= lXi) or
    (lYPos <= 1) or (lYPos >= lYi) or
    (lZPos <= 1) or (lZPos >= lZi) then
     // retire and exit
else begin  *)

   Check(lVal-1); //left
   Check(lVal+1); //right
   Check(lVal-lXi); //up
   Check(lVal+lXi); //down
   Check(lVal-lXY); //up
   Check(lVal+lXY); //down
(*   //check plane above
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
   Check(lVal+lXDimM+1); //down, right *)
end;{} //not edge
   incQra(lQTail,lQSz); //done with this pixel
END;
procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
//var lI: integer;
begin
  (*if (lClusterBuff^[lPt]<>lClusterSign) then exit;*)
  lQHead := 0;
  lQTail := 1;
  Check(lPt);
  RetirePixel;
  while ((lQHead+1) <> lQTail) do begin//complete until all voxels in buffer have been tested
        RetirePixel;
        if (lQHead = lQSz) and (lQTail = 1) then
           exit; //break condition: avoids possible infinite loop where QTail is being incremented but QHead is stuck at maximum value
  end;
end;

procedure SelectClusters (lSign: integer);
var lInc,lV: integer;
begin
     for lInc := 1 to lXYZ do begin
         if lClusterBuff^[lInc] = lSign then begin
            // measure size of the cluster and fill it with kFillValue
            lClusterSz := 0;
            lClusterSign := lSign;
            lClusterFillValue := kFillValue;
            FillStart(lInc);
            // now fill the cluster with its size (=1 if the voxel was isolated)
            if lClusterSz > 1 then begin
                for lV := 1 to lXYZ do
                    if lClusterBuff^[lV] = kFillValue then
                       lClusterBuff^[lV] :=  lClusterSz;
            end else
                lClusterBuff^[lInc] := 1; //fill all voxels in cluster with size of voxel
         end;//target color
     end; //for each voxel
end; //nested SelectClusters
begin //proc CountClusterSize
  if (lXi < 5) or (lYi < 5) or (lZi < 3) then exit;
  lXY := lXi*lYi; //offset one slice
  lTopSlice := (lZi-1) * lXY;
  lXYZ :=lXY*lZi;
  lQSz := (lXYZ div 4)+8;
  GetMem(lQra,lQsz * sizeof(longint) );
  for lInc := 1 to lXYZ do begin
      if lImg^[lInc] = lClusterValue then
         lClusterBuff^[lInc] := -1  //target voxel - will be part of a cluster of size 1..XYZ
      else
            lClusterBuff^[lInc] := 0;//not a target, not part of a cluser, size = 0
  end;
  //lStart := GetTickCount;
  SelectClusters(-1);   //for each voxel with intensity=-1, change value to number of connected voxels in cluster
  //fx(GetTickCount-lStart);
  //we did not fill bottom slice...
  for lInc := 1 to lXY do
      if lImg^[lInc] = lClusterValue then
         lClusterBuff^[lInc] := lClusterBuff^[lInc+lXY];
  //we did not fill top slice
  for lInc := (lTopSlice+1) to (lTopSlice+lXY) do
      if lImg^[lInc] = lClusterValue then
         lClusterBuff^[lInc] := lClusterBuff^[lInc-lXY];

  Freemem(lQra);
end; //proc CountClusterSize

procedure PreserveLargestCluster (var lImg: Bytep; lXi,lYi,lZi: integer; lClusterValue,ValueForSmallClusters: byte  );
var
  lC,lXYZ,lX: integer;
  lTemp: longintp;
begin
   if (lXi < 5) or (lYi < 5) or (lZi < 1) then exit;
   lXYZ :=lXi*lYi*lZi;
   //ensure at least some voxels exist with clusterValue
   lC := 0;
   for lX := 1 to lXYZ do
       if lImg^[lX] = lClusterValue then inc (lC);
   if lC < 2 then
      exit;//e.g. if lC = 1 then only a single voxel, which is in fact largest cluster
   getmem(lTemp,lXYZ*sizeof(longint));
   CountClusterSize(lImg,lTemp,lXi,lYi,lZi,lClusterValue);
   lC := 0;
   for lX := 1 to lXYZ do
       if lTemp^[lX] > lC then lC := lTemp^[lX];
   if ValueForSmallClusters = 0 then begin
      for lX := 1 to lXYZ do
          if (lTemp^[lX] >= 0) and (lTemp^[lX] < lC) then //cluster, but not biggest one...
             lImg^[lX] := ValueForSmallClusters;
   end else for lX := 1 to lXYZ do
          if (lTemp^[lX] > 0) and (lTemp^[lX] < lC) then //cluster, but not biggest one...
             lImg^[lX] := ValueForSmallClusters;

   freemem(lTemp);

end;

procedure Dilate (var lImg: Bytep; lXi,lYi,lZi,lCycles: integer; lChange: byte  );
//Dilates Diamonds - neighbor coefficient = 0
//Dilate if Change=1 then all voxels where intensity <> 1 but where any neighbors = 1 will become 1
//Erode  if Change=0 then all voxels where intensity <>0 but where any neighbors = 0 will become 0
//step is repeated  for lCycles
var
  lC,lX,lY,lZ, lXY,lXYZ,lPos,lOffset,lN: integer;
  lTemp: bytep;
begin
   if (lXi < 5) or (lYi < 5) or (lZi < 1) then exit;
   lXY := lXi*lYi; //offset one slice
   lXYZ :=lXY*lZi;
   getmem(lTemp,lXYZ);
   for lC := 1 to lCycles do begin
   Move(lImg^[1], lTemp^[1],lXYZ);
   for lZ := 1 to lZi do begin
       for lY := 1 to lYi do begin
           lOffset := ((lY-1)*lXi) + ((lZ-1) * lXY);
           for lX := 1 to lXi do begin
             lPos := lOffset + lX;
             if (lTemp^[lPos] <> lChange) then begin
               if (lX>1) and (lTemp^[lPos-1] = lChange) then lImg^[lPos] := lChange;
               if (lX<lXi) and (lTemp^[lPos+1] = lChange) then lImg^[lPos] := lChange;
               if (lY>1) and (lTemp^[lPos-lXi] = lChange) then lImg^[lPos] := lChange;
               if (lY<lYi) and (lTemp^[lPos+lXi] = lChange) then lImg^[lPos] := lChange;
               if (lZ>1) and (lTemp^[lPos-lXY] = lChange) then lImg^[lPos] := lChange;
               if (lZ<lZi) and (lTemp^[lPos+lXY] = lChange) then lImg^[lPos] := lChange;
             end; //voxel <> lChange
           end;
       end;//Y
   end; //Z
   end;
   freemem(lTemp);
end;

procedure SmoothFWHM2Vox (var lImg: Bytep; lXi,lYi,lZi: integer);
const
  k0=240;//weight of center voxel
  k1=120;//weight of nearest neighbors
  k2=15;//weight of subsequent neighbors
  kTot=k0+k1+k1+k2+k2; //weight of center plus all neighbors within 2 voxels
  kWid = 2; //we will look +/- 2 voxels from center
var
  lyPos,lPos,lWSum,lX,lY,lZ,lXi2,lXY,lXY2: integer;
  lTemp: bytep;
begin
   if (lXi < 5) or (lYi < 5) then exit;
   lXY := lXi*lYi; //offset one slice
   lXY2 := lXY * 2; //offset two slices
   lXi2 := lXi*2;//offset to voxel two lines above or below
   getmem(lTemp,lXi*lYi*lZi*sizeof(byte));
   for lPos := 1 to (lXi*lYi*lZi) do
       lTemp^[lPos] := lImg^[lPos];
   //smooth horizontally
   for lZ := 1 to lZi do begin
     for lY := (1) to (lYi) do begin
       lyPos := ((lY-1)*lXi) + ((lZ-1)*lXY) ;
       for lX := (1+kWid) to (lXi-kWid) do begin
           lPos := lyPos + lX;
           lWSum := lImg^[lPos-2]*k2+lImg^[lPos-1]*k1
                 +lImg^[lPos]*k0
                 +lImg^[lPos+1]*k1+lImg^[lPos+2]*k2;
           lTemp^[lPos] := lWSum div kTot;
       end; {lX}
     end; {lY}
   end; //lZi
   //smooth vertically

   for lPos := 1 to (lXi*lYi*lZi) do
       lImg^[lPos] := lTemp^[lPos];//fill in sides
   for lZ := 1 to lZi do begin
     for lX := (1) to (lXi) do begin
       for lY := (1+kWid) to (lYi-kWid) do begin
           lPos := ((lY-1)*lXi) + lX + ((lZ-1)*lXY) ;
           lWSum := lTemp^[lPos-lXi2]*k2+lTemp^[lPos-lXi]*k1
                 +lTemp^[lPos]*k0
                 +lTemp^[lPos+lXi]*k1+lTemp^[lPos+lXi2]*k2;
           lImg^[lPos] := lWSum div kTot;
       end; {lX}
     end; //lY
   end; //lZ
   //if 3rd dimension....
   if lZi >= 5 then begin
     //smooth across slices
     for lPos := 1 to (lXi*lYi*lZi) do
         lTemp^[lPos] := lImg^[lPos]; //fill in sides
     for lZ := (1+kWid) to (lZi-kWid) do begin
       for lY := (1) to (lYi) do begin
         lyPos := ((lY-1)*lXi) + ((lZ-1)*lXY) ;
         for lX := (1) to (lXi) do begin
             lPos := lyPos + lX;
             lWSum := lImg^[lPos-lXY2]*k2+lImg^[lPos-lXY]*k1
                   +lImg^[lPos]*k0
                   +lImg^[lPos+lXY]*k1+lImg^[lPos+lXY2]*k2;
             lTemp^[lPos] := lWSum div kTot;
         end; {lX}
       end; {lY}
     end; //lZi
     for lPos := 1 to (lXi*lYi*lZi) do
         lImg^[lPos] := lTemp^[lPos];
   end; //at least 5 slices...
   //free memory
   freemem(lTemp);
end;

procedure DilateSphere (var lImg: Bytep; lXi,lYi,lZi: integer; lVoxDistance: single; lChange: byte  );
//INPUT: Img is array of bytes 1..XYZ that represents 3D volume, lXi,lYi,lZi are number of voxels in each dimension
//             lVoxDistance is search radius (in voxels)
//            lChange is the intensity to be changed - if background color: erosion, if foreground color: dilation
//OUTPUT: Eroded/Dilated Img
var
  lDxI,lXY,lXYZ,lZ,lY,lX, lVoxOK,lPos: integer;
  lDx: single;
  lSearch: array of integer;
  lTemp: bytep;
function HasNeighbor (lVox: integer): boolean;
var
  s,t: integer;
begin
   result := true;
   for s := 0 to (lVoxOK-1) do begin
     t := lVox +lSearch[s];
     if (t > 0) and (t <= lXYZ) and (lTemp^[t] = lChange) then
        exit;
   end;
   result := false;
end; //nested HasNeighbor
begin  //proc DilateSphere
   if lVoxDistance < 1 then exit;
   if lVoxDistance = 1 then begin //much faster to use classic neighbor dilation
      Dilate(lImg,Lxi,lYi,lZi,1,lChange);
      exit;
   end;
   if (lXi < 3) or (lYi < 3) or (lZi < 3) then
      exit;
   lXY := lXi*lYi; //voxels per slice
   lXYZ := lXY*lZi; //voxels per volume
   //next: make 1D array of all voxels within search sphere: store offset from center
   lDxI := trunc(lVoxDistance); //no voxel will be searched further than DxI from center
   setlength(lSearch,((lDxI *2)+1)*((lDxI *2)+1)*((lDxI *2)+1) );
   lVoxOK := 0;
   for lZ := -lDxI to lDxI do
         for lY := -lDxI to lDxI do
             for lX := -lDxI to lDxI do begin
               lDx := sqrt( sqr(lX)+ sqr(lY)+ sqr(lZ)  );
               if (lDx < lVoxDistance) and (lDx > 0) then begin
                  lSearch[lVoxOK] := lX + (lY*lXi)+(lZ * lXY); //offset to center
                  inc(lVoxOK);
               end; //in range, not center
             end; //lX
   getmem(lTemp, lXYZ);//we need a temporary buffer, as we will be dilating the original image
   Move(lImg^[1], lTemp^[1],lXYZ);
   lPos := 0;
   for lX := 1 to lXYZ do begin
         inc(lPos);
         if (lTemp^[lPos] <> lChange)  and HasNeighbor(lPos) then
               lImg^[lPos] := lChange;
   end; //for X, each voxel
   freemem(lTemp); //free temporary buffer
   lSearch := nil; //free 1D search space
end; //proc DilateSphere

end.

