unit fill;

interface
uses define_types,Windows;
procedure BorderFill(var lBMP: Bytep; lBGInvisibleColor: byte; lMaskHt,lMaskWid: integer);

implementation

procedure BorderFill(var lBMP: Bytep; lBGInvisibleColor: byte; lMaskHt,lMaskWid: integer);
//lMaskP should have all invis voxels as 128, non as 255
//sets all invis boundary voxels to 0
var
	lMaskP: ByteP;
	lPos,lMaskSz,
	lQSz,lQHead,lQTail: integer;
	lQRA: LongIntp;
Procedure IncQra(var lVal, lQSz: integer);
begin
	inc(lVal);
	if lVal >= lQSz then
	 lVal := 1;
end;
PROCEDURE RetirePixel; //FIFO cleanup
VAR
   lVal,lPos: integer;
BEGIN
   lVal := lQra[lQTail];
   lPos := lVal-1;
   if (lPos > 0) and (lMaskP[lPos]=128) then begin//add item to left
		incQra(lQHead,lQSz);
		lMaskP[lPos] := 0;
		lQra[lQHead] := lPos;
   end;
   if (lPos > 0) then lMaskP[lPos] := 0;
   lPos := lVal+1;
   if (lPos < lMaskSz) and (lMaskP[lPos]=128) then begin//add item to right
		incQra(lQHead,lQSz);
		lMaskP[lPos] := 0;
		lQra[lQHead] := lPos;
   end;
   if (lPos < lMaskSz) then lMaskP[lPos] := 0;
   lPos := lVal-lMaskWid;
   if (lPos > 0) and (lMaskP[lPos]=128) then begin//add item above
		incQra(lQHead,lQSz);
		lMaskP[lPos] := 0;
		lQra[lQHead] := lPos;
   end;
   if (lPos > 0) then lMaskP[lPos] := 0;
   lPos := lVal+lMaskWid;
   if (lPos < lMaskSz) and(lMaskP[lPos]=128) then begin//add item below
		incQra(lQHead,lQSz);
		lMaskP[lPos] := 0;
		lQra[lQHead] := lPos;
   end;
   if (lPos < lMaskSz) then lMaskP[lPos] := 0;
   incQra(lQTail,lQSz); //done with this pixel
END;

procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
begin
  if (lPt < 1) or (lPt > lMaskSz) or (lMaskP[lPt] <> 128) then exit;
  //lQSz := 8000;//size of FIFO Queue Array
  lQHead := 1;
  lQTail := 1;
  lQra[lQTail] := (lPt); //NOTE: both X and Y start from 0 not 1
  lMaskP[lPt] := 0;
  RetirePixel;
  if lQHead >= lQTail then begin
	while lQHead <> lQTail do
		RetirePixel;
  end;
end;
begin //proc DefineBG
  lMaskSz := lMaskWid * lMaskHt;
  Getmem(lMaskP,lMaskSz);
  for lPos := 1 to lMaskSz do
	if lBMP[lPos] <> lBGInvisibleColor then
		lMaskP[lPos] := 128
	else
		lMaskP[lPos] := 255;
  lQSz := lMaskSz div 4;
  GetMem(lQra,lQSz*sizeof(LongInt));
  //erase all rows
  for lPos := 1 to lMaskHt do begin
	  FillStart( (lPos-1)*lMaskWid + 1);
	  FillStart( (lPos)*lMaskWid);
  end;
  //erase all cols
  for lPos := 1 to lMaskWid do begin
	  FillStart( lPos + 1);
	  FillStart( ((lMaskHt-1) *lMaskWid) + lPos);
  end;
  Freemem(lQRa);
  //make sure bright blue 0000FF becauses neighbor 0000FE instead of 000100
  //now, fill in islands so they are not transparent
  for lPos := 1 to lMaskSz do
	if lMaskP[lPos] = 128 then
		lBMP[lPos] := lBGInvisibleColor;
        //else
        //    lBMP[lPos] := 0;
  //for lPos := 1 to lMaskSz do
  //		lBMP[lPos] := lBGInvisibleColor ;

  Freemem(lMaskP);

end;


end.
 