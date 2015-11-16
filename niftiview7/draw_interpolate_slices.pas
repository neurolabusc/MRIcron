unit draw_interpolate_slices;
//USED by stats to select only regions with a given number of connected/contiguous voxels
interface
uses define_types, Classes, SysUtils;


function Interpolate_Slices (var lVol: bytep; lX,lY,lZ, lOrient:integer; var lNotes: TStringList): boolean;


implementation

procedure Smooth_Slice (var lSlice: array of single; lX,lY: integer);
var
  lSliceVox,lVox,lxx,lyy: integer;
  lSliceOrig: array of single;
begin
  if (lX <3) or (lY < 3) then exit;
  lSliceVox := lX * lY;
  SetLength(lSliceOrig, lSliceVox);
  //lSliceOrig := Copy(lSlice, Low(lSlice), Length(lSlice));  //really clean, but unnecessary
  move(lSlice[0],lSliceOrig[0],sizeof(lSlice)); // it works
  lVox := 0;
  for lyy := 1 to lY do begin
    for lxx := 1 to lX do begin
      if (lyy > 1) and (lyy < lY) and (lxx > 1) and (lxx < lX) then begin  //not on edges
        lSlice[lVox] := ((4*lSliceOrig[lVox])+(2*lSliceOrig[lVox+1])+ (2*lSliceOrig[lVox-1])+
          (2*lSliceOrig[lVox+lX])+ (2*lSliceOrig[lVox-lX])+
          (lSliceOrig[lVox+lX+1])+ (lSliceOrig[lVox-lX+1])+ (lSliceOrig[lVox+lX-1])+ (lSliceOrig[lVox-lX-1])
               )/16;
      end;
      inc(lVox);
    end;//each column X
  end; //each row Y
end;

procedure  Binarize_Slice(var lVol: bytep; lX,lY,lSliceTarget,lMax: integer);
var
  lThresh,lSliceVox,lVox,lOffset: integer;
begin
  lThresh := lMax div 2;
  lSliceVox := lX * lY;
  lOffset := lSliceVox * lSliceTarget;
  for lVox := 1 to lSliceVox do
    if (lVol[lVox+lOffset] > lThresh) then
      lVol[lVox+lOffset] := lMax
    else
      lVol[lVox+lOffset] := 0;

end;

procedure Interpolate_Slice (var lVol: bytep; lX, lY, lSliceLo,lSliceTarget,lSliceHi: integer);
//e.g. if lowSlice = 0, targetSlice = 1 and highSlice=4 we will interpolate a new slice 1 weighted mostly by slice 0 with some influence of slice 4
var
  lSliceVox,lVox,lOffsetLo,lOffset,lOffsetHi: integer;
  lFracLo,lFracHi: single;
  lSlice: array of single;
begin
  lSliceVox := lX * lY;
  SetLength(lSlice, lSliceVox);
  lFracHi :=   (lSliceTarget-lSliceLo)/ (lSliceHi-lSliceLo); //weighting from top slice
  lFracLo := 1 - lFracHi; //weighting from lower slice
  lOffsetLo := lSliceVox * lSliceLo;
  lOffset := lSliceVox * lSliceTarget;
  lOffsetHi := lSliceVox * lSliceHi;
  for lVox := 1 to lSliceVox do
    lSlice[lVox-1] := (lFracLo * lVol[lVox+lOffsetLo])+ (lFracHi * lVol[lVox+lOffsetHi]) ;

  Smooth_Slice (lSlice, lX,lY);
  for lVox := 1 to lSliceVox do
    lVol[lVox+lOffset] := round(lSlice[lVox-1]);
end;

function Interpolate_SlicesAx (var lVol: bytep; lX,lY,lZ:integer; var lNotes: TStringList): boolean;
var
   lSliceVox,lVox, lSlice, lSliceOffset,lBottomDrawnSlice,lTopDrawnSlice,lLastDrawnSlice,lNextDrawnSlice,lS,lMax:integer;
   lSliceDrawn: array of boolean;
   lGaps: boolean;
begin
  result := false;
  if (lZ < 3) or (lX < 3) or (lY <3) then exit;
  //Determine which slices are already drawn
  lSliceVox := lX * lY;
  SetLength(lSliceDrawn, lZ);
  //
  lBottomDrawnSlice := maxint;
  lTopDrawnSlice := -1;
  for lSlice := 0 to (lZ-1) do begin
    lSliceDrawn[lSlice] := false;
    lVox := 0;
    lSliceOffset := (lSlice * lSliceVox);
    repeat
      inc(lVox);
      if (lVol[lVox+lSliceOffset] > 0) then lSliceDrawn[lSlice] := true;
    until ( lSliceDrawn[lSlice]) or (lVox >= lSliceVox);
    if (lSliceDrawn[lSlice]) and (lBottomDrawnSlice > lSlice) then lBottomDrawnSlice := lSlice;
    if (lSliceDrawn[lSlice]) and (lTopDrawnSlice < lSlice) then lTopDrawnSlice := lSlice;
    //if (lSliceDrawn[lSlice]) then lNotes.Add('drawing on slice '+inttostr(lSlice));
  end;
  if (lBottomDrawnSlice > lTopDrawnSlice) then begin
    lNotes.Add('No drawing found');
    exit;
  end;
  lGaps := false;
  for lSlice := lBottomDrawnSlice to lTopDrawnSlice do
    if (not lSliceDrawn[lSlice]) then lGaps := true;
  if (not lGaps) then begin
    lNotes.Add('No gaps in drawing found');
    exit;
  end;
  //images are binary - find non-zero value
  lMax := 0;
  lSliceOffset := (lBottomDrawnSlice * lSliceVox);
  for lVox := 1 to lSliceVox do
    if (lVol[lVox+lSliceOffset] > lMax) then lMax := lVol[lVox+lSliceOffset];
  //now fill slices

  for lSlice := lBottomDrawnSlice to lTopDrawnSlice do begin
    if lSliceDrawn[lSlice] then
      lLastDrawnSlice := lSlice
    else begin//gap
      for lS := lTopDrawnSlice downto lSlice do
        if lSliceDrawn[lS] then lNextDrawnSlice := lS;
      lNotes.Add('Interpolate '+inttostr(lSlice)+' using '+inttostr(lLastDrawnSlice)+' and '+inttostr(lNextDrawnSlice));
      Interpolate_Slice (lVol, lX,lY, lLastDrawnSlice,lSlice,lNextDrawnSlice);
      Binarize_Slice(lVol, lX,lY,lSlice,lMax);
    end;
  end;
  result := true;
end;

procedure OrientCor (var lVol: bytep; lX,lY,lZ:integer; Reverse: boolean);
//XZY -> XYZ
begin

end;

procedure OrientSag (var lVol: bytep; lX,lY,lZ:integer; Reverse: boolean);
//YZX -> XYZ
begin

end;


function Interpolate_Slices (var lVol: bytep; lX,lY,lZ, lOrient:integer; var lNotes: TStringList): boolean;
begin
  if lOrient = 3 then OrientCor(lVol, lX,lY,lZ,true);
  if lOrient = 2 then OrientSag( lVol, lX,lY,lZ,true);
  result := Interpolate_SlicesAx (lVol, lX,lY,lZ, lNotes);
  if lOrient = 3 then OrientCor(lVol, lX,lY,lZ,false);
  if lOrient = 2 then OrientSag( lVol, lX,lY,lZ,false);

end;



end.
