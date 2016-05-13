unit MultiSlice;
interface
            {$mode delphi}
uses
{$IFNDEF Unix} Windows,wgraphics,
{$ELSE}
//not used by Darwin... RGBGraphics,rgbroutines,
{$ENDIF}
 LResources,LCLType,SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls,nifti_img,define_types,nifti_img_view,
  StdCtrls,GraphicsMathLibrary, Menus,ClipBrd,IniFiles,userdir;
const
	kMaxMultiSlice  = 24;
type
 TMultiSlice =  record
   Orient,nSlices,OverslicePct: integer;
   OrthoView,SliceLabel: boolean;
   SliceList: array [1..kMaxMultiSlice] of integer;
 end;//TMultiSlice

  { TMultiSliceForm }

  TMultiSliceForm = class(TForm)
	MainMenu1: TMainMenu;
	File1: TMenuItem;
	Closewindow1: TMenuItem;
	Saveasbitmap1: TMenuItem;
	Edit1: TMenuItem;
	Copy1: TMenuItem;
	MultiPanel: TScrollBox;
	MultiImage: TImage;
	View1: TMenuItem;
    OrientMenu: TMenuItem;
	Axial1: TMenuItem;
	Sagittal1: TMenuItem;
	Coronal1: TMenuItem;
    Orthoview: TMenuItem;
	Slices1: TMenuItem;
	Savesettings1: TMenuItem;
	Settings1: TMenuItem;
    MultiSaveDialog: TSaveDialog;
	SliceLabelCheck: TMenuItem;
    OversliceMenu: TMenuItem;
    N501: TMenuItem;
    N331: TMenuItem;
    N201: TMenuItem;
    N01: TMenuItem;
    N202: TMenuItem;
    N351: TMenuItem;
    N502: TMenuItem;
	procedure Copy1Click(Sender: TObject);
procedure MenuItem1Click(Sender: TObject);
	procedure Saveasbitmap1Click(Sender: TObject);
	procedure OrientClick(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure CreateMultiAx;
	procedure CreateMultiCor;
	procedure CreateMultiSag;
	procedure CreateMultiSlice;
	procedure OrthoviewClick(Sender: TObject);
procedure Settings1Click(Sender: TObject);
	procedure Slices1Click(Sender: TObject);
	procedure Closewindow1Click(Sender: TObject);
	procedure FormCreate(Sender: TObject);
	procedure UpdateMultiSliceDisplay;
	procedure OpenMultiMRU(Sender:TObject);
	procedure UpdateMultiSliceMRU;
 {$IFNDEF FPC}
	procedure FormClose(Sender: TObject; var Action: TCloseAction);
 {$ELSE}
	procedure FormClose(Sender: TObject);

 {$ENDIF}
	procedure Savesettings1Click(Sender: TObject);
    procedure SliceLabelCheckClick(Sender: TObject);
    procedure OverlsiceClick(Sender: TObject);
  private
	{ Private declarations }
  public
	{ Public declarations }
  end;

var
  MultiSliceForm: TMultiSliceForm;
  gMulti:TMultiSlice;
  gMultiSliceDir,gMultiSliceStartupFilename,gMultiSliceDefaultsFilename:string;
{$IFDEF FPC}
  gMultiBuff: RGBQuadp;
  gMultiWid,gMultiHt: Integer;
  gMultiXCenterRA: array [1..kMaxMultiSlice] of integer;
{$ENDIF}
implementation

 {$IFNDEF FPC}
{$R *.DFM}
 {$ENDIF}

function MultiSliceNum2String: string;
var
	lSlice: integer;
begin
 if gMulti.nSlices = 0 then begin
     gMulti.nSlices := 1;
     gMulti.SliceList[1] := 1;
 end;
 result := '';
	for lSlice := 1 to gMulti.nSlices do  begin
		result := result+inttostr(gMulti.SliceList[lSlice]);
		if lSlice < gMulti.nSlices then
			result := result+',';
	end; //for each slice
end;

procedure MultiSliceString2Num (var lStr: string);
var
	lSliceStr: string;
	lStrPos,lStrLen,lSlice: integer;
begin
 //showmessage(lStr);
 lStrLen := length(lStr);
	if lStrLen < 1 then exit;
	lSlice := 0;
	lSliceStr := '';
	for lStrPos := 1 to lStrLen do begin
		if lStr[lStrPos] in  ['0'..'9'] then
			lSliceStr := lSliceStr+lStr[lStrPos];
		if ((not (lStr[lStrPos] in  ['0'..'9'])) or (lStrPos=lStrLen)) and (lSliceStr<>'') then begin
			inc(lSlice);
			if lSlice <= kMaxMultiSlice then
				gMulti.SliceList[lSlice] := strtoint(lSliceStr);
			lSliceStr := '';
		end; //if white space or eoln
	end; //for lStrPos
	gMulti.nSlices := lSlice;
	if lSlice > kMaxMultiSlice then begin
		showmessage('Warning: maximum number of slices is '+inttostr(kMaxMultiSlice));
		gMulti.nSlices := kMaxMultiSlice;
	end;
end;

procedure WriteMultiSliceIniFile (lFilename: string);
var
  lIniFile: TIniFile;
begin
  if DiskFreeEx(lFilename) < 1 then
	exit;
  if not DirectoryExists(extractfiledir(lFilename)) then begin
		mkDir(extractfiledir(lFilename));
  end;
  lIniFile := TIniFile.Create(lFilename);
  //Slice Index
  lIniFile.WriteString('STR', 'Slices', MultiSliceNum2String);
  //Booleans
  lIniFile.WriteString('BOOL', 'OrthoView',Bool2Char( gMulti.OrthoView));
  lIniFile.WriteString('BOOL', 'SliceLabel',Bool2Char( gMulti.SliceLabel));
  //Integers        LicenseID
  lIniFile.WriteString('INT', 'Orient',IntToStr(gMulti.Orient));
  lIniFile.WriteString('INT', 'OverslicePct',IntToStr(gMulti.OverslicePct));
  lIniFile.Free;
end;

procedure ReadMultiSliceIniFile (lFilename: string);
var
  lStr: string;
  lIniFile: TIniFile;
begin
	if not FileexistsEx(lFilename) then begin
		exit;
	end;
  lIniFile := TIniFile.Create(lFilename);
  lStr := lIniFile.ReadString('STR', 'Slices', '10,20,30');//file0 - last file viewed
  MultiSliceString2Num(lStr);
  gMulti.OrthoView := IniBool(lIniFile,'OrthoView',gMulti.OrthoView);
  gMulti.SliceLabel := IniBool(lIniFile,'SliceLabel',gMulti.SliceLabel);
  gMulti.Orient:= IniInt(lIniFile,'Orient',gMulti.Orient);
  gMulti.OverslicePct:= IniInt(lIniFile,'OverslicePct',gMulti.OverslicePct);
	lIniFile.Free;
end;

procedure TMultiSliceForm.OpenMultiMRU(Sender:TObject);
var
	lFilename: string;
begin
   lFilename := gMultiSliceDir +(Sender as TMenuItem).caption+'.ini' ;
   ReadMultiSliceIniFile(lFilename);
   UpdateMultiSliceDisplay;
   CreateMultiSlice;
end;

procedure TMultiSliceForm.UpdateMultiSliceMRU;
var
	NewItem: TMenuItem;
	lSearchRec: TSearchRec;
begin
  While Settings1.Count > 0 do Settings1.Items[0].Free;
  if FindFirst(gMultiSliceDir +'*.ini', faAnyFile, lSearchRec) = 0 then
	 repeat
		   NewItem := TMenuItem.Create(Self);
		   NewItem.Caption := ParseFileName(ExtractFileName(lSearchRec.Name));
                   {$IFDEF FPC}
                    NewItem.Onclick := OpenMultiMRU; //Lazarus
                    {$ELSE}
                     NewItem.Onclick := OpenMultiMRU;
                     {$ENDIF}
		   Settings1.Add(NewItem);
		until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
end;

procedure TMultiSliceForm.Copy1Click(Sender: TObject);
{$IFNDEF FPC}
var
  MyFormat : Word;
  AData: THandle;
  APalette : HPalette;
  {$ENDIF}
begin
           {$IFDEF Darwin}
        Showmessage('Copy not yet supported with OSX: use File/Save');
        {$ENDIF}
	 if (MultiImage.Picture.Graphic = nil) then begin //1420z
		Showmessage('You need to load an image before you can copy it to the clipboard.');
		exit;
	 end;
 {$IFNDEF FPC}
	 MultiImage.Picture.Bitmap.SaveToClipBoardFormat(MyFormat,AData,APalette);
	 ClipBoard.SetAsHandle(MyFormat,AData);
 {$ELSE}
        MultiSliceForm.MultiImage.Picture.Bitmap.SaveToClipboardFormat(2);
 {$ENDIF}
end;

procedure TMultiSliceForm.MenuItem1Click(Sender: TObject);
begin


end;



procedure TMultiSliceForm.Saveasbitmap1Click(Sender: TObject);
begin
	 SaveImgAsPNGBMP (MultiImage);
end;



procedure CreateBlankBitmap (lPGHt,lPGWid:integer;var lImage: TImage);
var
   lPos: integer;
   lBGInvisibleColor: TRGBQuad;
begin
{$IFDEF ENDIAN_BIG}
lBGInvisibleColor :=TColor2TRGBQuad(clBlack);
 {$ELSE}
  //lBGInvisibleColor := gMRIcroOverlay[kBGOverlayNum].LUTinvisible;
  lBGInvisibleColor := gMRIcroOverlay[kBGOverlayNum].LUT[0];
 {$ENDIF}
   gMultiWid := lPGWid;
  gMultiHt := lPGHt;
  if (gMultiWid < 1) or (gMultiHt < 1) then
     exit;
  getmem (gMultiBuff, gMultiHt*gMultiWid*sizeof(TRGBQuad) );
  //fillchar(gMultiBuff^,gMultiHt*gMultiWid*sizeof(TRGBQuad),0);
  for lPos := 1 to (gMultiHt*gMultiWid) do
      gMultiBuff^[lPos] :=  lBGInvisibleColor;
end;

procedure MultiHLine (lX1,lX2,lY1,lThick: integer; lClr: TRGBQuad);
var
   lLine,lY,lYPos,lX,lXlo,lXhi: integer;
begin
  if (lThick < 1) or (gMultiWid < 1) or (gMultiHt < 1) or (lY1 < 1) or (lY1 >gMultiHt) or (gMultiBuff = nil) then
     exit;
  lXlo := lX1;
  lXHi := lX2;
  SortInteger(lXlo,lXhi);
  if lXlo < 1 then
     lXlo := 1;
  if lXlo > gMultiWid then
     lXlo := gMultiWid;
  if lXhi < 1 then
     lXhi := 1;
  if lXhi > gMultiWid then
     lXhi := gMultiWid;
  lY := lY1-((lThick{+1}) div 2);
  for lLine := 1 to lThick do begin
      lYPos := (lY)*gMultiWid;
      if lY < gMultiHt then
          for lX := lXlo to lXhi do
          gMultiBuff^[lYPos+lX] := lClr;
      inc(lY);
  end;
end;

procedure MultiVLine (lX1,lY1,lY2,lThick: integer; lClr: TRGBQuad);
var
   lXs, lX,lY,lYlo,lYhi: integer;
begin
  if (lThick < 1) or (gMultiWid < 1) or (gMultiHt < 1) or (lX1 < 1) or (lX1 >gMultiWid) or (gMultiBuff = nil) then
     exit;
  lYlo := lY1;
  lYHi := lY2;
  SortInteger(lYlo,lYhi);
  if lYlo < 1 then
     lYlo := 1;
  if lYlo > gMultiHt then
     lYlo := gMultiHt;
  if lYhi < 1 then
     lYhi := 1;
  if lYhi > gMultiHt then
     lYhi := gMultiHt;
  lXs := lX1-((lThick{+1}) div 2)-2;//-2 as indexed from 0 and line is at least 1 pixel thick
  for lX := lXs to (lXs+lThick-1) do
      if (lX >= 0) and (lX < gMultiWid) then
         for lY := lYlo to lYHi do
             gMultiBuff^[((lY-1)*gMultiWid)+lX] := lClr;

end;

procedure DefineBackGround(var lBMP: DWordp; lBGInvisibleColor: DWord; lMaskHt,lMaskWid: integer);
//lMaskP should have all invis voxels as 128, non as 255
//sets all invis boundary voxels to 0
var
	lMaskP: ByteP;
	lBGvisibleColor: DWord;
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
   lVal := lQra^[lQTail];
   lPos := lVal-1;
   if (lPos > 0) and (lMaskP^[lPos]=128) then begin//add item to left
		incQra(lQHead,lQSz);
		lMaskP^[lPos] := 0;
		lQra^[lQHead] := lPos;
   end;
   if (lPos > 0) then lMaskP^[lPos] := 0;
   lPos := lVal+1;
   if (lPos < lMaskSz) and (lMaskP^[lPos]=128) then begin//add item to right
		incQra(lQHead,lQSz);
		lMaskP^[lPos] := 0;
		lQra^[lQHead] := lPos;
   end;
   if (lPos < lMaskSz) then lMaskP^[lPos] := 0;
   lPos := lVal-lMaskWid;
   if (lPos > 0) and (lMaskP^[lPos]=128) then begin//add item above
		incQra(lQHead,lQSz);
		lMaskP^[lPos] := 0;
		lQra^[lQHead] := lPos;
   end;
   if (lPos > 0) then lMaskP^[lPos] := 0;
   lPos := lVal+lMaskWid;
   if (lPos < lMaskSz) and(lMaskP^[lPos]=128) then begin//add item below
		incQra(lQHead,lQSz);
		lMaskP^[lPos] := 0;
		lQra^[lQHead] := lPos;
   end;
   if (lPos < lMaskSz) then lMaskP^[lPos] := 0;
   incQra(lQTail,lQSz); //done with this pixel
END;

procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
begin
  if (lPt < 1) or (lPt > lMaskSz) or (lMaskP^[lPt] <> 128) then exit;
  //lQSz := 8000;//size of FIFO Queue Array
  lQHead := 1;
  lQTail := 1;
  lQra^[lQTail] := (lPt); //NOTE: both X and Y start from 0 not 1
  lMaskP^[lPt] := 0;
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
	if lBMP^[lPos] = lBGInvisibleColor then
		lMaskP^[lPos] := 128
	else
		lMaskP^[lPos] := 255;
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
  if (lBGInvisibleColor and 255) = 255 then
	lBGVisibleColor:= lBGInvisibleColor-1
  else
	lBGVisibleColor:= lBGInvisibleColor+1;
  //now, fill in islands so they are not transparent
  for lPos := 1 to lMaskSz do
	if lMaskP^[lPos] = 128 then
		lBMP^[lPos] := lBGVisibleColor;
  Freemem(lMaskP);
end;

{$IFDEF FLIPV}
procedure SetDim (lInPGHt,lInPGWid,lWriteColumn: integer; var l32OutBitP : DWordp);
var
   lLen,lSrc,lDest,lY: integer;
   lTBuff:    RGBQuadp;
begin
        getmem(lTBuff,lInPGHt*lWriteColumn*4);
        lLen := lWriteColumn*4;
        lSrc := 1;
        lDest := 1;
        for lY := 1 to lInPGHt do begin
           Move(l32OutBitP^[lSrc],lTBuff^[lDest],lLen);
           lSrc := lSrc + lInPGWid;
           lDest := lDest + lWriteColumn;
        end;
        DrawBMP( lWriteColumn, lInPGHt, lTBuff, MultiSliceForm.MultiImage);
        freemem(lTBuff);
end;
{$ELSE}
procedure SetDim (lInPGHt,lInPGWid,lWriteColumn: integer; var l32OutBitP : DWordp);
var
   lLen,lSrc,lDest,lY: integer;
   lTBuff:    RGBQuadp;
begin
        getmem(lTBuff,lInPGHt*lWriteColumn*4);
        lLen := lWriteColumn*4;
        lSrc := 1;
        //lDest := 1;
        lDest := 1+ ((lInPGHt-1) * lWriteColumn);

        for lY := 1 to lInPGHt do begin
           Move(l32OutBitP^[lSrc],lTBuff^[lDest],lLen);
           lSrc := lSrc + lInPGWid;
           lDest := lDest - lWriteColumn;
        end;
        DrawBMP( lWriteColumn, lInPGHt, lTBuff, MultiSliceForm.MultiImage);
        freemem(lTBuff);
end;
{$ENDIF}


procedure RemoveHorizGaps (lMaxOverlapWid,lColWid: integer); //will overlap gaps from 1..lMaxOverlapWid, leave right non-overlapped);
var
 l32BitP,l32OutBitP : DWordp;
 lBGInvisibleColor,lBGInvisibleColorShr8: DWord;
 lIsGap,lPrevIsGap: boolean;
 lInc,lPrevSliceStart,lPrevSliceEnd,lPrevWriteColumn,lWid,lHt,lReadRow,
 lMaxWriteColumn,lReadColumn,lWriteColumn,lReadOffset,lWriteOffset,lPos,x,y: integer;
 lTextPos,lTextReadColumn: integer;
begin
      (*freemem (gMultiBuff );
 gMultiBuff := nil;
 exit;*)


     for lTextPos := 1 to kMaxMultiSlice do
         gMultiXCenterRA[lTextPos] := 0;
       lTextPos := 0;
       lTextReadColumn := lColWid div 2;
       if (gMultiWid < 1) or (gMultiHt < 1) or (gMultiBuff = nil) then
          exit;
     lBGInvisibleColor := TRGBQuad2DWord(gMRIcroOverlay[kBGOverlayNum].LUTinvisible);
     //fx(lBGInvisibleColor);
     //lBGInvisibleColorShr8 := lBGInvisibleColor Shr 8;
	lHt := gMultiHt;//MultiSliceForm.MultiImage.Picture.Bitmap.Height;
	lWid := gMultiWid; //MultiSliceForm.MultiImage.Picture.Bitmap.Width;
	if (lHt < 2) or (lWid < 2) then exit;
	//next: prepare input
        l32BitP := DWordP(gMultiBuff);
        lBGInvisibleColor := l32BitP^[1];
	DefineBackGround(l32BitP,lBGInvisibleColor, lHt,lWid);
	//next prepare output
         GetMem(l32OutBitP,lHt*lWid*sizeof(DWord));
	 for lInc := 1 to (lwid*lHt) do
		l32OutBitP^[lInc] := lBGInvisibleColor;
	//next: compress by deleting empty columns
	lWriteColumn := 0;
	lPrevIsGap := true;
	lPrevSliceStart := maxint -10;
	lPrevSliceEnd := 0;
	lPrevWriteColumn := maxint-10;//do not degap 1st line


if gMulti.OverSlicePct = 0 then begin //simply remove gaps between slice
	for lReadColumn := 1 to lWid do begin
		lReadOffset := lReadColumn;
		lIsGap := true;
		lReadRow := 1;
                if lReadColumn >= lTextReadColumn then begin
                   inc(lTextPos);
                   lTextReadColumn := lTextReadColumn+lColWid;
                   if lTextPos <= kMaxMultiSlice then
                      gMultiXCenterRA[lTextPos] := lWriteColumn;
                end;
		while (lReadRow < lHt) and (lIsGap) do begin
			if l32BitP^[lReadOffset] <> lBGInvisibleColor then
				lIsGap := false;
			inc(lReadOffset,lWid);
			inc(lReadRow);
		end; //while each readrow
		if not lIsGap then begin//data in this column
			if lReadColumn > (lPrevWriteColumn+1) then begin //leave one pixel gap between noncontiguous columns
				inc(lWriteColumn);
				lReadOffset := lReadColumn-1;
				lWriteOffset := lWriteColumn;
				//showmessage(inttostr(lWriteColumn)+'  '+inttostr(lReadOffset));
				for lReadRow := 1 to lHt do begin
					l32OutBitP[lWriteOffset] := l32BitP[lReadOffset];
					inc(lReadOffset,lWid);
					inc(lWriteOffset,lWid);
				end;
			end; //leave 1 pixel gap
			inc(lWriteColumn);
			lReadOffset := lReadColumn;
			lWriteOffset := lWriteColumn;
			for lReadRow := 1 to lHt do begin
				l32OutBitP[lWriteOffset] := l32BitP[lReadOffset];
				inc(lReadOffset,lWid);
				inc(lWriteOffset,lWid);
			end;
			lPrevWriteColumn := lReadColumn;
		end; //not Gap - write this column
	end; //for each column
end else begin //overslice <> 0: show subsequent slices above/below each other
         lMaxWriteColumn := -maxint;
         for lReadColumn := 1 to lMaxOverlapWid do begin
		lReadOffset := lReadColumn;
		lIsGap := true;
		lReadRow := 1;
		while (lReadRow < lHt) and (lIsGap) do begin
			//ovx
			if l32BitP^[lReadOffset] <> lBGInvisibleColor then
				lIsGap := false;
			inc(lReadOffset,lWid);
			inc(lReadRow);
		end; //while each readrow
		if (lPrevIsGap <> lIsGap) then begin//change from prev column
			if not (lIsGap) then begin
                              //fx(lPrevSliceStart,lPrevSliceEnd,lReadColumn,abs(((lPrevSliceEnd-lPrevSliceStart) * gMulti.OverSlicePct)div 100));
			   if lPrevSliceEnd > lPrevSliceStart then
				lWriteColumn := lPrevSliceEnd-abs(((lPrevSliceEnd-lPrevSliceStart) * gMulti.OverSlicePct)div 100);
			   lPrevSliceStart := lWriteColumn;

			end;
			if (lIsGap) then
			   lPrevSliceEnd := lWriteColumn;
		end;
		lPrevIsGap := lIsGap;
		if gMulti.OverSlicePct > 0 then begin
		  if not lIsGap then begin//data in this column
			inc(lWriteColumn);
			lReadOffset := lReadColumn;
			lWriteOffset := lWriteColumn;
			for lReadRow := 1 to lHt do begin
				if l32BitP^[lReadOffset] <> lBGInvisibleColor then
					l32OutBitP^[lWriteOffset] := l32BitP^[lReadOffset];
				inc(lReadOffset,lWid);
				inc(lWriteOffset,lWid);
			end;
		  end; //not Gap - write this column
		end else begin //if overwrite, else underwrite
		  if not lIsGap then begin//data in this column
			inc(lWriteColumn);
			lReadOffset := lReadColumn;
			lWriteOffset := lWriteColumn;
		        for lReadRow := 1 to lHt do begin
				if l32OutBitP^[lWriteOffset] = lBGInvisibleColor then
					l32OutBitP^[lWriteOffset] := l32BitP^[lReadOffset];
				inc(lReadOffset,lWid);
				inc(lWriteOffset,lWid);
			end;
		  end; //not Gap - write this column
		end;
                if lReadColumn >= lTextReadColumn then begin //text
                   inc(lTextPos);
                   lTextReadColumn := lTextReadColumn+lColWid;
                   if lTextPos <= kMaxMultiSlice then
                      gMultiXCenterRA[lTextPos] := lWriteColumn;
                end;  //text
       		if lWriteColumn > lMaxWriteColumn then
			lMaxWriteColumn := lWriteColumn;
	end; //for each column
	if lWriteColumn < lMaxWriteColumn then
		lWriteColumn := lMaxWriteColumn;
       if lMaxOverlapWid < lWid then begin
		lReadColumn := lMaxOverlapWid;
		if (lWriteColumn) < lReadColumn then //add gap if some compression
			inc(lWriteColumn);
		for lReadColumn := (lMaxOverlapWid+1) to lWid do begin
		  lReadOffset := lReadColumn;
		  lIsGap := true;
		  lReadRow := 1;
		  while (lReadRow < lHt) and (lIsGap) do begin
			if l32BitP^[lReadOffset] <> lBGInvisibleColor then
				lIsGap := false;
			inc(lReadOffset,lWid);
			inc(lReadRow);
		  end; //while each readrow
		  if not lIsGap then begin
			inc(lWriteColumn);
			lReadOffset := lReadColumn;
			lWriteOffset := lWriteColumn;
			for lReadRow := 1 to lHt do begin
				l32OutBitP[lWriteOffset] := l32BitP[lReadOffset];
				inc(lReadOffset,lWid);
				inc(lWriteOffset,lWid);
			end; //for each row
		  end; //not gap
		end; //for each column
		if (lWriteColumn+1) < lWid then
			inc(lWriteColumn);
	end; //if maxwid < wid - unoverlapped
end;

 SetDim (lHt,lWid,lWriteColumn,l32OutBitP);
 FreeMem(l32OutBitP);
   freemem (gMultiBuff );
 gMultiBuff := nil;
end;


procedure TMultiSliceForm.CreateMultiSag;
var
	lSlice,lHt,lWid,lSlicePos,lSliceWid: integer;
begin

  lHt:= gBGIMg.ScrnDim[3];
  lSliceWid :=gBGIMg.ScrnDim[2]+2;//+1 for 1-voxel gap between slices - ensures we can detect slice boundary
  lWid := (lSliceWid*gMulti.nSlices);
  if lWid < 2 then exit;
  if gMulti.OrthoView then //coro crossview
	lWid := lWid + gBGIMg.ScrnDim[1]+2;
  if lWid < 2 then exit;

  CreateBlankBitmap (lHt,lWid, MultiImage);
  for lSlice := 1 to gMulti.nSlices do begin
	DrawSag (gMulti.SliceList[lSlice],1+((lSlice-1)*lSliceWid));//+lSlice because we want 1-voxel gap between slices
	//if gMulti.SliceLabel then DrawLabel(MultiImage,DimToMM(gMulti.SliceList[lSlice],1),((lSlice-1)*lSliceWid)+(lSliceWid div 2),lWid);
  end;
  if gMulti.OrthoView then begin //coro crossview
	DrawCor (gBGImg.ScrnDim[2] div 2,(lSliceWid*gMulti.nSlices)-1);
        //MultiImage.Canvas.Pen.Color := clWhite;
	//MultiImage.Canvas.Pen.Color := gBGIMg.XBarClr;
        //MultiImage.Canvas.Pen.Width := gBGImg.XBarThick;
        for lSlice := 1 to gMulti.nSlices do begin //draw lines
		lSlicePos := (gMulti.nSlices*lSliceWid)+(gMulti.SliceList[lSlice]);
                MultiVLine (lSlicePos,0,lHt,gBGImg.XBarThick,TColor2TRGBQuad(gBGImg.XBarClr));
		{MultiImage.Canvas.MoveTo(lSlicePos,0);
		MultiImage.Canvas.LineTo(lSlicePos,lHt);}
	end;//line for each slice
  end;//if cross view
  RemoveHorizGaps(lSliceWid*gMulti.nSlices,lSliceWid);
end; //CreateMultiSag

procedure TMultiSliceForm.CreateMultiCor;
var
	lSlice,lHt,lWid,lLeft,lSliceWid: integer;
begin
  lHt:= gBGIMg.ScrnDim[3];
  lSliceWid :=gBGIMg.ScrnDim[1]+2;//+1 for 1-voxel gap between slices - ensures we can detect slice boundary
  lWid := lSliceWid*gMulti.nSlices;
  if lWid < 2 then exit;
  if gMulti.OrthoView then  //sag crossview
	lWid := lWid + gBGIMg.ScrnDim[2]+2;
  if lWid < 2 then exit;
  CreateBlankBitmap (lHt,lWid, MultiImage);
  for lSlice := 1 to gMulti.nSlices do begin
	//ImgForm.YViewEdit.value := gMulti.SliceList[lSlice];
	DrawCor (gMulti.SliceList[lSlice],1+((lSlice-1)*lSliceWid));
	//if gMulti.SliceLabel then DrawLabel(MultiImage,DimToMM(gMulti.SliceList[lSlice],2),((lSlice-1)*lSliceWid)+(gBGIMg.ScrnDim[1] div 2),lWid);
  end;
  if gMulti.OrthoView then begin
	DrawSag (gBGImg.ScrnDim[1] div 2,(gMulti.nSlices*lSliceWid)-1);
	//MultiImage.Canvas.Pen.Color := gBGIMg.XBarClr;
        //MultiImage.Canvas.Pen.Color := clWhite;
	MultiImage.Canvas.Pen.Color := gBGIMg.XBarClr;
        MultiImage.Canvas.Pen.Width := gBGImg.XBarThick;

        for lSlice := 1 to gMulti.nSlices do begin
		lLeft := gMulti.nSlices*lSliceWid+(gMulti.SliceList[lSlice]);
                MultiVLine (lLeft,0,lHt,gBGImg.XBarThick,TColor2TRGBQuad(gBGImg.XBarClr));

		{MultiImage.Canvas.MoveTo(lLeft,0);
		MultiImage.Canvas.LineTo(lLeft,lHt);}
	end;
  end;//if orthoview
  RemoveHorizGaps(lSliceWid*gMulti.nSlices,lSliceWid);
end; //CreateMultiCor

procedure TMultiSliceForm.CreateMultiAx;
var
	lSliceWid,lSlice,lHt,lWid,lLeft: integer;
begin
  lHt:= gBGIMg.ScrnDim[2];
  lSliceWid :=gBGIMg.ScrnDim[1]+2;//+1 for 1-voxel gap between slices - ensures we can detect slice boundary
  lWid := lSliceWid*gMulti.nSlices;
  if lWid < 2 then exit;
  if gMulti.OrthoView then begin //sag crossview
	lWid := lWid + gBGIMg.ScrnDim[2]+2;
	if gBGIMg.ScrnDim[3]> lHt then
		lHt := gBGIMg.ScrnDim[3];
  end;
  if lWid < 2 then exit;
  CreateBlankBitmap (lHt,lWid, MultiImage);
  for lSlice := 1 to gMulti.nSlices do begin
	DrawAxial (gMulti.SliceList[lSlice],1+((lSlice-1)*lSliceWid));
	//if gMulti.SliceLabel then DrawLabel(MultiImage,DimToMM(gMulti.SliceList[lSlice],3),((lSlice-1)*lSliceWid)+(gBGIMg.ScrnDim[1] div 2),lWid);
  end;
  if gMulti.OrthoView then begin
	lLeft := gMulti.nSlices*lSliceWid;
	//DrawSag (gBGImg.ScrnDim[1] div 2,lLeft);
	DrawSag (gBGImg.ScrnDim[1] div 2,lLeft-1);

        //MultiImage.Canvas.pen.Color := clWhite;
        //MultiImage.Canvas.Pen.Color := gBGIMg.XBarClr;
        //MultiImage.Canvas.Pen.Width := gBGImg.XBarThick;

        for lSlice := 1 to gMulti.nSlices do begin
		lHt := gBGImg.ScrnDim[3]-(gMulti.SliceList[lSlice]);
                MultiHLine (lLeft,lWid,lHt,gBGImg.XBarThick,TColor2TRGBQuad(gBGImg.XBarClr));
	end;
  end;
  RemoveHorizGaps(lSliceWid*gMulti.nSlices,lSliceWid);
end; //CreateMultiAx

procedure DrawLabels;
var
   lSlice,lOrient: integer;
begin
 case gMulti.Orient of
	3: lOrient := 2;
	2: lOrient := 1;
	else lOrient := 3;
 end;//case

  if not gMulti.SliceLabel then
     exit;
  for lSlice := 1 to gMulti.nSlices do begin
	if gMultiXCenterRA[lSlice] > 0 then DrawLabel(MultiSliceForm.MultiImage,DimToMM(gMulti.SliceList[lSlice],gMulti.SliceList[lSlice],gMulti.SliceList[lSlice],lOrient),gMultiXCenterRA[lSlice],maxint);
  end;
end;
//gMultiXCenterRA

procedure TMultiSliceForm.CreateMultiSlice;
//test var lI: integer;
begin
 if gMulti.nSlices < 1 then begin
	showmessage('No valid slices selected - please use View/Slices.');
 end;
 //MultiImage.Canvas.Font.Color := clWhite;
//for lI := 1 to 32 do begin //test
 case gMulti.Orient of
	3: CreateMultiCor;
	2: CreateMultiSag;
	else CreateMultiAx;
 end;//case
 DrawLabels;
   // end; //test
end;//CreateMultiSlice

procedure TMultiSliceForm.OrientClick(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gMulti.Orient := (sender as TMenuItem).tag;
	CreateMultiSlice;
end;

procedure TMultiSliceForm.FormShow(Sender: TObject);
begin
   ReadMultiSliceIniFile (gMultiSliceStartupFilename );
   UpdateMultiSliceMRU;
   UpdateMultiSliceDisplay;
   CreateMultiSlice;
   MultiSliceForm.BringToFront;
end;

procedure TMultiSliceForm.OrthoviewClick(Sender: TObject);
begin
	OrthoView.checked := not OrthoView.Checked;
	gMulti.OrthoView := OrthoView.checked;
	CreateMultiSlice;
end;

procedure TMultiSliceForm.Settings1Click(Sender: TObject);
begin

end;

procedure TMultiSliceForm.Slices1Click(Sender: TObject);
var
	lStr: string;
begin
	lStr := InputBox('Select multislices', 'Slice numbers [e.g. 10,16,24]',MultiSliceNum2String);
	//now parse line
	MultiSliceString2Num(lStr);
	CreateMultiSlice;
end;

procedure TMultiSliceForm.Closewindow1Click(Sender: TObject);
begin
	MultiSliceForm.Close;
end;

procedure TMultiSliceForm.UpdateMultiSliceDisplay;
begin
	SetSubmenuWithTag(OversliceMenu, gMulti.OverslicePct);
	SetSubmenuWithTag(OrientMenu, gMulti.Orient);
	OrthoView.Checked := gMulti.OrthoView;
	SliceLabelCheck.Checked := gMulti.SliceLabel;
end;

procedure TMultiSliceForm.FormCreate(Sender: TObject);
var
	lSlice:integer;
begin
 {$IFDEF Linux}ImgForm.InitImg(MultiImage);{$ENDIF}
     gMultiBuff := nil;
     gMultiSliceDir  := DefaultsDir('multislice');
     //gMultiSliceDir := extractfiledir(paramstr(0))+pathdelim+'multislice'+pathdelim;
     gMultiSliceDefaultsFilename := gMultiSliceDir + 'default.ini';
     gMultiSliceStartupFilename := gMultiSliceDefaultsFilename;
     gMulti.Orient := 1;
     gMulti.OverslicePct := 0;
     gMulti.nSlices:= 4;
     gMulti.OrthoView := true;
     gMulti.SliceLabel := true;
     for lSlice := 1 to gMulti.nSlices do
	gMulti.SliceList[lSlice] := 62+10*lSlice;
      {$IFDEF Darwin}
        {$IFNDEF LCLgtk} //only for Carbon compile
        Copy1.ShortCut := ShortCut(Word('C'), [ssMeta]);
         Savesettings1.ShortCut := ShortCut(Word('S'), [ssMeta]);
         Closewindow1.ShortCut := ShortCut(Word('W'), [ssMeta]);
         {$ENDIF}
        {$ENDIF}
end;

 {$IFNDEF FPC}
procedure TMultiSliceForm.FormClose(Sender: TObject; var Action: TCloseAction);
 {$ELSE}
procedure TMultiSliceForm.FormClose(Sender: TObject);
 {$ENDIF}
begin
WriteMultiSliceIniFile (gMultiSliceDefaultsFilename );
end;

procedure TMultiSliceForm.Savesettings1Click(Sender: TObject);
begin
  MultiSaveDialog.InitialDir := extractfiledir(gMultiSliceDir );
  if not MultiSaveDialog.Execute then exit;
  {$IFDEF Unix}
  WriteMultiSliceIniFile(extractfiledir(gMultiSliceDir)+pathdelim+extractfilename(MultiSaveDialog.Filename));

  {$ELSE}
  WriteMultiSliceIniFile(MultiSaveDialog.Filename);
  {$ENDIF}
  UpdateMultiSliceMRU;
end;

procedure TMultiSliceForm.SliceLabelCheckClick(Sender: TObject);
begin
	SliceLabelCheck.checked := not SliceLabelCheck.Checked;
	gMulti.SliceLabel := SliceLabelCheck.checked;
	CreateMultiSlice;
end;

procedure TMultiSliceForm.OverlsiceClick(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gMulti.OverslicePct := (sender as TMenuItem).tag;
	CreateMultiSlice;
end;

  {$IFDEF FPC}
initialization
  {$I MultiSlice.lrs}
{$ENDIF}

end.
