unit tfce_clustering;
//USED by stats to select only regions with a given number of connected/contiguous voxels
{$IFDEF FPC} {$mode delphi}{$H+} {$ENDIF}
{$Include ..\common\isgui.inc}
interface
uses
{$IFNDEF UNIX} Windows,
  {$ELSE}
  {$IFDEF GUI} LCLType, LCLintf,{$ENDIF}
  {$ENDIF}
define_types,dialogsx,SysUtils,nifti_hdr,nifti_img, math,unpm, nifti_types;

//procedure FindClusters (lMultiBuf: SingleP; lXdim, lYDim, lZDim, lThreshClusterSz: integer; lMinNeg, lMinPos: single);

//function ClusterTFCE (var lHdr: TMRIcroHdr;  lThreshClusterSz: integer; lThresh: double ): boolean;

function doTFCE (var lHdr: TNIFTIhdr;  lImg: SingleP; NumConn: integer; H, E, minT, deltaT: single ): boolean;
//mimics  FSL's function "TFCE" in newimagefns.h

function doTFCEbothPolarities (var lHdr: TNIFTIhdr;  lImg: SingleP; NumConn: integer; H, E, minT, deltaT, minNegT, NegdeltaT: single; var maxTFCE, maxNegTFCE: single): boolean;
//both polarities computes TFCE for both positive and negative values

implementation


procedure countClusterSize (lX,lY,lZ, lnumConnIn: integer; var lClusterBuff: LongIntP);
//input CountImg is volume X*Y*Z where voxels are either 0 or 1
// output: CountImg voxels report number of connected neighbors
const
 lClusterSign = 1; //input CountImg has this value set to 1
 lClusterFillValue = -1; //impossible cluster size - used to denote actively growing cluster
var
  lQHead,lV,lXY, lXYZ,lClusterSz, lQTail,lnumConn,lI,lNeighbor: integer;
  lQra: LongIntP;
  ConnOffset : ARRAY [1..26] of integer;
procedure InitConn;
begin
    //first 6 share face
    ConnOffset[1] := -1;//L
    ConnOffset[2] := 1; //R
    ConnOffset[3] := -lX; //A
    ConnOffset[4] := lX; //P
    ConnOffset[5] := -lXY;//I
    ConnOffset[6] := lXY;//S
    if lnumConnIn < 7 then begin
        lnumConn := 6;
        exit;
    end;
    //share edge
    //..check plane above
    ConnOffset[7] := (lXY-1); //left
    ConnOffset[8] := (lXY+1); //right
    ConnOffset[9] := (lXY-lX); //up
    ConnOffset[10] := (lXY+lX); //down
    //..check plane below
    ConnOffset[11] := (-lXY-1); //left
    ConnOffset[12] := (-lXY+1); //right
    ConnOffset[13] := (-lXY-lX); //up
    ConnOffset[14] := (-lXY+lX); //down
    //..check diagonals of current plane
    ConnOffset[15] := (-lX-1); //up, left
    ConnOffset[16] := (-lX+1); //up, right
    ConnOffset[17] := (+lX-1); //down, left
    ConnOffset[18] := (+lX+1); //down, right
    if lnumConnIn < 19 then begin
        lnumConn := 18;
        exit;
    end;
    //share corner
    //..check plane above
    ConnOffset[19] := (lXY-1-lX); //left
    ConnOffset[20] := (lXY-1+lX); //right
    ConnOffset[21] := (lXY+1-lX); //up
    ConnOffset[22] := (lXY+1+lX); //down
    //..check plane BELOW
    ConnOffset[23] := (-lXY-1-lX); //left
    ConnOffset[24] := (-lXY-1+lX); //right
    ConnOffset[25] := (-lXY+1-lX); //up
    ConnOffset[26] := (-lXY+1+lX); //down
    lnumConn := 26;
end;  //InitConn
begin
  lXY := lX * lY;
  lXYZ := lX*lY*lZ;
  InitConn;
  if lXYZ < 1 then exit;
  GetMem(lQra,lXYZ * sizeof(longint) );
  //check every voxel to see if it is isolated
  for lV := 1 to lXYZ do begin
    if (lClusterBuff^[lV]=lClusterSign) then begin //new cluster detected
      lClusterSz := 1;
      lQHead := 1;
      lQTail := 1;
      lQra^[lQTail] := lV;
      lClusterBuff^[lV] := lClusterFillValue;
      while (lQHead >= lQTail) do begin
        //RetirePixel: lQTail incremented once, lQHead is incremented 0..nummConn
        for lI := 1 to lnumConn do begin
          lNeighbor := lQra^[lQTail]+ConnOffset[lI];
          if (lClusterBuff^[lNeighbor]=lClusterSign) then begin//add item
            inc(lQHead);
            inc(lClusterSz);
            lClusterBuff^[lNeighbor] := lClusterFillValue;
            lQra^[lQHead] := lNeighbor;
          end; //if new item detected
        end; //for each connector
        inc(lQTail); //done with this pixel
      end; //while items in Queue
      for lI := lV to lXYZ do
        if (lClusterBuff^[lI]=lClusterFillValue) then
          lClusterBuff^[lI] := lClusterSz;
    end; //new item found
  end; //for each voxel
  freemem(lQra);
end;

procedure ZeroFaces (var lHdr: TNIFTIhdr;  lImg: SingleP );
var
  lV,lX,lY,lZ,lZi,lYi,lXi: integer;
begin
  lX := lHdr.Dim[1];
  lY := lHdr.Dim[2];
  lZ := lHdr.Dim[3];
  if (lX < 3) or (lY < 3) or (lZ < 3) then exit;
  for lV := 1 to (lX*lY) do lImg[lV] := 0; //bottom slice
  for lV := ((lX*lY*lZ)-(lX*lY)) to (lX*lY*lZ) do lImg[lV] := 0; //top slice
  //left side
  lV := 1;
  for lZi := 1 to lZ do begin
    for lYi := 1 to lY do begin
      lImg[lV] := 0;
      lV := lV+lX;
    end;
  end;
  //right side
  lV := lX;
  for lZi := 1 to lZ do begin
    for lYi := 1 to lY do begin
      lImg[lV] := 0;
      lV := lV+lX;
    end;
  end;
  //anterior
  for lZi := 1 to lZ do begin
    lV := (lZi-1) * lX*lY;
    for lXi := 1 to lX do begin
      lV := lV+1;
      lImg[lV] := 0;
    end;
  end;
  //posterior
  for lZi := 1 to lZ do begin
    lV := (lZi-1) * lX*lY;
    lV := lV + ((lY-1) *lX);
    for lXi := 1 to lX do begin
      lV := lV+1;
      lImg[lV] := 0;
    end;
  end;
end;


function doTFCEbothPolarities (var lHdr: TNIFTIhdr;  lImg: SingleP; NumConn: integer; H, E, minT, deltaT, minNegT, NegdeltaT: single; var maxTFCE, maxNegTFCE: single): boolean;
var
  lV,lXYZ,lX,lY,lZ: integer;

  lNegImg: SingleP;

begin
  result := false;
  lX := lHdr.Dim[1];
  lY := lHdr.Dim[2];
  lZ := lHdr.Dim[3];
  lXYZ := lX*lY*lZ;
  if lXYZ < 1 then exit;
  getmem(lNegImg,lXYZ*sizeof(single));
  for lV := 1 to lXYZ do 
    lNegImg[lV] := -lImg[lV];


  doTFCE (lHdr, lImg, NumConn, H, E, minT, deltaT);
  maxTFCE :=lImg[lV];
  for lV := 1 to lXYZ do
    if (maxTFCE < lImg[lV]) then
       maxTFCE:= lImg[lV];


  doTFCE (lHdr, lNegImg, NumConn, H, E, abs(minNegT), abs(NegdeltaT));
  maxNegTFCE :=lImg[lV];
  for lV := 1 to lXYZ do
    if (maxNegTFCE < lNegImg[lV]) then
       maxNegTFCE:= lNegImg[lV];
  maxNegTFCE := -maxNegTFCE;

  for lV := 1 to lXYZ do begin
    if (lNegImg[lV] > 0) then
      lImg[lV] := -lNegImg[lV];
  end;

  freemem(lNegImg);
end;



function doTFCE (var lHdr: TNIFTIhdr;  lImg: SingleP; NumConn: integer; H, E, minT, deltaT: single ): boolean;
const
  kSteps = 100;
label
  777;
var
  lV,lXYZ,lX,lY,lZ: integer;
  maxval, lThresh, ThreshPowerHxdh, dh: single;
  lThreshImg: SingleP;
  lCountImg: LongIntP;
  lStartTime: DWord;
begin
  lX := lHdr.Dim[1];
  lY := lHdr.Dim[2];
  lZ := lHdr.Dim[3];
  {$IFDEF GUI} lStartTime := GetTickCount; {$ENDIF}
  result := false;//assume failure
  lXYZ := lX*lY*lZ;
  if lXYZ < 1 then exit;
  //E := 0.5; //0.5
  //H := 2;//2
  getmem(lThreshImg,lXYZ*sizeof(single));
  getmem(lCountImg,lXYZ*sizeof(longint));
  ZeroFaces (lHdr,  lImg );
  maxval := lImg[1];
  for lV := 1 to lXYZ do begin
    lThreshImg[lV] := lImg[lV];
    if lImg[lV] > maxval then maxval := lImg[lV];
    lImg[lV] := 0; //initialize sum map to zero
  end;

  if (maxval <= 0) then goto 777;
  if (maxval < minT) then goto 777;
  if (deltaT = 0) then
    dh := (maxval-minT) / kSteps
  else
    dh := deltaT;
  NPMmsg('max = '+floattostr(maxval)+'  deltaT = '+floattostr(dh));
  lThresh := minT+dh;
  while lThresh < maxval do begin


    for lV := 1 to lXYZ do begin
      if (lThreshImg[lV] <= lThresh) then
        lCountImg[lV] := 0
      else
        lCountImg[lV] := 1;
    end;
    countClusterSize (lX,lY,lZ,NumConn, lCountImg);
    ThreshPowerHxdh := power(lThresh,H)*dh;
    for lV := 1 to lXYZ do
      if (lCountImg[lV] > 0) then
        lImg[lV] := lImg[lV] + (exp(E*ln(lCountImg[lV])) * ThreshPowerHxdh); //faster than power
    (*for lV := 1 to lXYZ do
      if (lCountImg[lV] > 0) then
        lImg[lV] := lImg[lV] + (power(lCountImg[lV],E) * ThreshPowerHxdh); *)
    lThresh := lThresh + dh;
  end;
777:
  {$IFDEF GUI} NPMmsg('Time = '+inttostr(GetTickCount - lStartTime)); {$ENDIF}
  freemem(lCountImg);
  freemem(lThreshImg);
  result := true; //report success!
end;




end.