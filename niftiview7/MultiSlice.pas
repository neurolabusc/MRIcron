unit MultiSlice;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus,ClipBrd,nifti_img, nifti_hdr,define_types,
  nifti_img_view,INiFiles,FileCtrl;
const
	kMaxMultiSlice  = 24;
type
 TMultiSlice =  record
   Orient,nSlices,OverslicePct: integer;
   OrthoView,SliceLabel: boolean;
   SliceList: array [1..kMaxMultiSlice] of integer;
 end;//TMultiSlice
  TMultiSliceForm = class(TForm)
	MainMenu1: TMainMenu;
	File1: TMenuItem;
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
    Close1: TMenuItem;
    NoOver: TMenuItem;
	procedure Copy1Click(Sender: TObject);
	procedure Saveasbitmap1Click(Sender: TObject);
	procedure OrientClick(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure CreateMultiAx;
	procedure CreateMultiCor;
	procedure CreateMultiSag;
	procedure CreateMultiSlice;
	procedure OrthoviewClick(Sender: TObject);
	procedure Slices1Click(Sender: TObject);
	procedure Closewindow1Click(Sender: TObject);
	procedure FormCreate(Sender: TObject);
	procedure UpdateMultiSliceDisplay;
	procedure OpenMultiMRU(Sender:TObject);
	procedure UpdateMultiSliceMRU;
	procedure FormClose(Sender: TObject; var Action: TCloseAction);
	procedure Savesettings1Click(Sender: TObject);
    procedure SliceLabelCheckClick(Sender: TObject);
    procedure OverlsiceClick(Sender: TObject);
    procedure Close1Click(Sender: TObject);
  private
	{ Private declarations }
  public
	{ Public declarations }
  end;

var
  MultiSliceForm: TMultiSliceForm;
  gMulti:TMultiSlice;
  gMultiSliceDir,gMultiSliceStartupFilename,gMultiSliceDefaultsFilename:string;
implementation

{$R *.DFM}
procedure HLineWithFlatEndCaps (lCanvas: TCanvas; lColor: TColor; X1,X2,Y1,lThick: integer);
var
  Y : integer;
begin
  Y := Y1 - (lThick div 2);
  lCanvas.Brush.Color := lColor;
  lCanvas.FillRect(Rect(x1, y, x2, y+lThick));
end;

procedure VLineWithFlatEndCaps (lCanvas: TCanvas; lColor: TColor; X1,Y1,Y2,lThick: integer);
var
  X : integer;
begin
  X := X1 - (lThick div 2)-1;
  lCanvas.Brush.Color := lColor;
  lCanvas.FillRect(Rect(x, y1, x+lThick,Y2));
end;


function MultiSliceNum2String: string;
var
	lSlice: integer;
begin
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
  //ovx
  lIniFile.Free;
end;

procedure ReadMultiSliceIniFile (lFilename: string);
var
  lStr: string;
  lIniFile: TIniFile;
begin
	if not FileexistsEx(lFilename) then begin
    showmessage('Unable to find '+lFilename);
		exit;
	end;
  lIniFile := TIniFile.Create(lFilename);
  lStr := lIniFile.ReadString('STR', 'Slices', '10,20,30');//file0 - last file viewed
  MultiSliceString2Num(lStr);
  gMulti.OrthoView := IniBool(lIniFile,'OrthoView',gMulti.OrthoView);
  gMulti.SliceLabel := IniBool(lIniFile,'SliceLabel',gMulti.SliceLabel);
  gMulti.Orient:= IniInt(lIniFile,'Orient',gMulti.Orient);
  gMulti.OverslicePct:= IniInt(lIniFile,'OverslicePct',gMulti.OverslicePct);
  //ovx
	lIniFile.Free;
end;

procedure TMultiSliceForm.OpenMultiMRU(Sender:TObject);
var
	lFilename: string;
begin
   //showmessage(    (Sender as TMenuItem).Caption);
   lFilename := gMultiSliceDir +(Sender as TMenuItem).caption+'.ini' ;
   ReadMultiSliceIniFile(lFilename);
   //showmessage(lFilename);
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
		   NewItem.Onclick := OpenMultiMRU;
       NewItem.AutoHotkeys := maManual;
		   Settings1.Add(NewItem);
		until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
end;

procedure TMultiSliceForm.Copy1Click(Sender: TObject);
var
  MyFormat : Word;
  AData: THandle;
  APalette : HPalette;   //For later versions of Delphi: APalette : THandle;
begin
	 if (MultiImage.Picture.Graphic = nil) then begin //1420z
		Showmessage('You need to load an image before you can copy it to the clipboard.');
		exit;
	 end;
	 MultiImage.Picture.Bitmap.SaveToClipBoardFormat(MyFormat,AData,APalette);
	 ClipBoard.SetAsHandle(MyFormat,AData);
end;

procedure TMultiSliceForm.Saveasbitmap1Click(Sender: TObject);
begin
	 SaveImgAsPNGBMP (MultiImage);
end;

procedure CreateBlankBitmap (lPGHt,lPGWid:integer;var lImage: TImage);
var
 sbBits : PByteArray;
 l32BitP: DWordp;
 lBGInvisibleColor: DWord;
   lBMP: TBitmap;
   lInc : integer;
begin
	 lBMP := TBitmap.Create;
	 TRY
			 lBMP.PixelFormat := pf32bit;
			 lBMP.Width := lPGwid;
			 lBMP.Height := lPGHt;
			 sbBits := lBmp.ScanLine[lPGHt-1];
			 //FillChar(sbBits^,(lPGHt*lPGwid*4), 0);
			 //FillChar fills with black, the next bit will fill current background color
			 lBGInvisibleColor := gMRIcroOverlay[kBGOverlayNum].LUTinvisible;
			 l32BitP := DWordp(sbBits);
			 for lInc := 1 to (lPGwid*lPGHt) do
				l32BitP[lInc] := lBGInvisibleColor;
			 lImage.Width := (lBmp.Width);//xx
			 lImage.Height := (lBmp.Height);//xx
			 lImage.Picture.Graphic := lBMP;
	 FINALLY
			   lBMP.Free;
	 END; //try..finally
end; //proc CreateBlankBitmap

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
	if lBMP[lPos] = lBGInvisibleColor then
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
  if (lBGInvisibleColor and 255) = 255 then
	lBGVisibleColor:= lBGInvisibleColor-1
  else
	lBGVisibleColor:= lBGInvisibleColor+1;
  //now, fill in islands so they are not transparent
  for lPos := 1 to lMaskSz do
	if lMaskP[lPos] = 128 then
		lBMP[lPos] := lBGVisibleColor;
  Freemem(lMaskP);

end;


procedure RemoveHorizGaps (lMaxOverlapWid: integer); //will overlap gaps from 1..lMaxOverlapWid, leave right non-overlapped);
var
 sbBits,sbOutBits : PByteArray;
 lBMP,lcompressedBMP: TBitmap;
 l32BitP,l32OutBitP : DWordp;
 lBGInvisibleColor: DWord;
 lIsGap,lPrevIsGap: boolean;
 lMaxWriteColumn,lInc,lPrevSliceStart,lPrevSliceEnd,lPrevWriteColumn,lWid,lHt,lReadRow,lReadColumn,lWriteColumn,lReadOffset,lWriteOffset,lScanLineBytes: integer;
begin
	lBGInvisibleColor := gMRIcroOverlay[kBGOverlayNum].LUTinvisible;
	lHt := MultiSliceForm.MultiImage.Picture.Bitmap.Height;
	lWid := MultiSliceForm.MultiImage.Picture.Bitmap.Width;
	if (lHt < 2) or (lWid < 2) then exit;
	//next: prepare input
	sbBits := MultiSliceForm.MultiImage.Picture.Bitmap.ScanLine[lHt-1];
	l32BitP := DWordp(sbBits);
	DefineBackGround(l32BitP, lBGInvisibleColor, lHt,lWid);

	//next prepare output
	 lBMP := TBitmap.Create;
	 lBMP.PixelFormat := pf32bit;
	 lBMP.Width := lWid;
	 lBMP.Height := lHt;
	 sbOutBits := lBmp.ScanLine[lHt-1];
	 l32OutBitP := DWordp(sbOutBits);
	 for lInc := 1 to (lwid*lHt) do
		l32OutBitP[lInc] := lBGInvisibleColor;
	//next: compress by deleting empty columns
	lWriteColumn := 0;
	lPrevIsGap := true;
	lPrevSliceStart := maxint -10;
	lPrevSliceEnd := 0;
	lPrevWriteColumn := maxint-10;//do not degap 1st line
if (gMulti.OverSlicePct = 0) or (gMulti.OverSlicePct = 100) then begin
	for lReadColumn := 1 to lWid do begin
		lReadOffset := lReadColumn;
		if (gMulti.OverSlicePct = 0) then begin
		   lIsGap := true;
                   lReadRow := 1;
		   while (lReadRow < lHt) and (lIsGap) do begin
			if l32BitP[lReadOffset] <> lBGInvisibleColor then
				lIsGap := false;
			inc(lReadOffset,lWid);
			inc(lReadRow);
                   end; //while each readrow
                end else
                    lIsGap := false;//May2008
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
			//showmessage(inttostr(lReadColumn)+'  '+inttostr(lReadRow)+'  '+inttostr(l32BitP[lReadOffset]));
		end; //not Gap - write this column
	end; //for each column
end else begin //show overslice
	lMaxWriteColumn := -maxint;
	for lReadColumn := 1 to lMaxOverlapWid do begin
		lReadOffset := lReadColumn;
		lIsGap := true;
		lReadRow := 1;
		while (lReadRow < lHt) and (lIsGap) do begin
			//see if this column is a gap...
			if l32BitP[lReadOffset] <> lBGInvisibleColor then
				lIsGap := false;
			inc(lReadOffset,lWid);
			inc(lReadRow);
		end; //while each read row
		if (lPrevIsGap <> lIsGap) then begin//change from prev column
			if not (lIsGap) then begin
			   if lPrevSliceEnd > lPrevSliceStart then
				lWriteColumn := lPrevSliceEnd-abs(((lPrevSliceEnd-lPrevSliceStart) * gMulti.OverSlicePct)div 100);
			   lPrevSliceStart := lWriteColumn;
			end else
			   lPrevSliceEnd := lWriteColumn;
		end;
		lPrevIsGap := lIsGap;
		if gMulti.OverSlicePct > 0 then begin
		  if not lIsGap then begin//data in this column
			inc(lWriteColumn);
			lReadOffset := lReadColumn;
			lWriteOffset := lWriteColumn;
			for lReadRow := 1 to lHt do begin
				if l32BitP[lReadOffset] <> lBGInvisibleColor then
					l32OutBitP[lWriteOffset] := l32BitP[lReadOffset];
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
				if l32OutBitP[lWriteOffset] = lBGInvisibleColor then
					l32OutBitP[lWriteOffset] := l32BitP[lReadOffset];
				inc(lReadOffset,lWid);
				inc(lWriteOffset,lWid);
			end;
		  end; //not Gap - write this column
		end;
		if lWriteColumn > lMaxWriteColumn then
			lMaxWriteColumn := lWriteColumn;
	end; //for each column
	if lWriteColumn < lMaxWriteColumn then
		lWriteColumn := lMaxWriteColumn;
	{if lWriteColumn < lMaxWriteColumn then
		showmessage('Some of your slices are much smaller than others. Your images will probably look better with the overslice set to zero.');
	}
	if lMaxOverlapWid < lWid then begin
		lReadColumn := lMaxOverlapWid;
		if (lWriteColumn) < lReadColumn then //add gap if some compression
			inc(lWriteColumn);
		for lReadColumn := (lMaxOverlapWid+1) to lWid do begin
		  lReadOffset := lReadColumn;
		  lIsGap := true;
		  lReadRow := 1;
		  while (lReadRow < lHt) and (lIsGap) do begin
			if l32BitP[lReadOffset] <> lBGInvisibleColor then
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
	//next prepare compressed output
	 lcompressedBMP := TBitmap.Create;
	 lcompressedBMP.PixelFormat := pf32bit;
	 lcompressedBMP.Width := lWriteColumn;
	 lScanLineBytes := lWriteColumn * 4;
	 lcompressedBMP.Height := lHt;
	 for lReadRow := 1 to lHt do
		Move(lBMP.ScanLine[lReadRow-1]^,lcompressedBMP.ScanLine[lReadRow-1]^,lScanLineBytes);
	 lBMP.Free;
	MultiSliceForm.MultiImage.Width := lcompressedBMP.Width;
	MultiSliceForm.MultiImage.Picture.Graphic := lcompressedBMP;
	lcompressedBMP.Free;
end; //proc RemoveHorizGaps

(*procedure RemoveHorizGaps;
var
 sbBits,sbOutBits : PByteArray;
 lBMP,lcompressedBMP: TBitmap;
 l32BitP,l32OutBitP : DWordp;
 lBGInvisibleColor: DWord;
 lIsGap: boolean;
 lPrevWriteColumn,lWid,lHt,lReadRow,lReadColumn,lWriteColumn,lReadOffset,lWriteOffset,lScanLineBytes: integer;
begin
	lBGInvisibleColor := gMRIcroOverlay[kBGOverlayNum].LUTinvisible;
	lHt := MultiSliceForm.MultiImage.Picture.Bitmap.Height;
	lWid := MultiSliceForm.MultiImage.Picture.Bitmap.Width;
	if (lHt < 2) or (lWid < 2) then exit;
	//next: prepare input
	sbBits := MultiSliceForm.MultiImage.Picture.Bitmap.ScanLine[lHt-1];
	l32BitP := DWordp(sbBits);
	//next prepare output
	 lBMP := TBitmap.Create;
	 lBMP.PixelFormat := pf32bit;
	 lBMP.Width := lWid;
	 lBMP.Height := lHt;
	 sbOutBits := lBmp.ScanLine[lHt-1];
	 l32OutBitP := DWordp(sbOutBits);
	 //FillChar(sbOutBits^,(lHt*lWid*4), 0); //default all to black
	//next: compress by deleting empty columns
	lWriteColumn := 0;
	lPrevWriteColumn := maxint-10;//do not degap 1st line
	for lReadColumn := 1 to lWid do begin
		lReadOffset := lReadColumn;
		lIsGap := true;
		lReadRow := 1;
		while (lReadRow < lHt) and (lIsGap) do begin
			if l32BitP[lReadOffset] <> lBGInvisibleColor then
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
			//showmessage(inttostr(lReadColumn)+'  '+inttostr(lReadRow)+'  '+inttostr(l32BitP[lReadOffset]));
		end; //not Gap - write this column
	end; //for each column
	//next prepare compressed output
	 lcompressedBMP := TBitmap.Create;
	 lcompressedBMP.PixelFormat := pf32bit;
	 lcompressedBMP.Width := lWriteColumn;
	 lScanLineBytes := lWriteColumn * 4;
	 lcompressedBMP.Height := lHt;
	 for lReadRow := 1 to lHt do
		Move(lBMP.ScanLine[lReadRow-1]^,lcompressedBMP.ScanLine[lReadRow-1]^,lScanLineBytes);
	 lBMP.Free;
	MultiSliceForm.MultiImage.Width := lcompressedBMP.Width;
	MultiSliceForm.MultiImage.Picture.Graphic := lcompressedBMP;
	lcompressedBMP.Free;
end; //proc RemoveHorizGaps*)

procedure TMultiSliceForm.CreateMultiSag;
var
	lSlice,lHt,lWid,lSlicePos,lSliceWid: integer;
begin
  lHt:= gBGIMg.ScrnDim[3];
  lSliceWid :=gBGIMg.ScrnDim[2]+2;//+1 for 1-voxel gap between slices - ensures we can detect slice boundary
  lWid := (lSliceWid*gMulti.nSlices);
  if gMulti.OrthoView then //coro crossview
	lWid := lWid + gBGIMg.ScrnDim[1];
  CreateBlankBitmap (lHt,lWid, MultiImage);
  for lSlice := 1 to gMulti.nSlices do begin
	DrawSag (gMulti.SliceList[lSlice],((lSlice-1)*lSliceWid));//+lSlice because we want 1-voxel gap between slices
	if gMulti.SliceLabel then DrawLabel(MultiImage,DimToMM(gMulti.SliceList[lSlice],gMulti.SliceList[lSlice],gMulti.SliceList[lSlice],1),((lSlice-1)*lSliceWid)+(lSliceWid div 2),lWid);
  end;
  if gMulti.OrthoView then begin //coro crossview
	DrawCor (gBGImg.ScrnDim[2] div 2,lSliceWid*gMulti.nSlices);
	//MultiImage.Canvas.Pen.Color := gBGIMg.XBarClr;
  //MultiImage.Canvas.Pen.Width := gBGImg.XBarThick;
	for lSlice := 1 to gMulti.nSlices do begin //draw lines
		lSlicePos := (gMulti.nSlices*lSliceWid)+(gMulti.SliceList[lSlice]);
    VLineWithFlatEndCaps (MultiImage.Canvas, gBGIMg.XBarClr,lSlicePos,0,lHt,gBGImg.XBarThick);
		//MultiImage.Canvas.MoveTo(lSlicePos,0);
		//MultiImage.Canvas.LineTo(lSlicePos,lHt);
	end;//line for each slice
  end;//if cross view
  RemoveHorizGaps(lSliceWid*gMulti.nSlices);
end; //CreateMultiSag

procedure TMultiSliceForm.CreateMultiCor;
var
	lSlice,lHt,lWid,lLeft,lSliceWid: integer;
begin
  lHt:= gBGIMg.ScrnDim[3];
  lSliceWid :=gBGIMg.ScrnDim[1]+2;//+1 for 1-voxel gap between slices - ensures we can detect slice boundary
  lWid := lSliceWid*gMulti.nSlices;
  if gMulti.OrthoView then  //sag crossview
	lWid := lWid + gBGIMg.ScrnDim[2];
  CreateBlankBitmap (lHt,lWid, MultiImage);
  for lSlice := 1 to gMulti.nSlices do begin
	  DrawCor (gMulti.SliceList[lSlice],((lSlice-1)*lSliceWid));
	  if gMulti.SliceLabel then DrawLabel(MultiImage,DimToMM(gMulti.SliceList[lSlice],gMulti.SliceList[lSlice],gMulti.SliceList[lSlice],2),((lSlice-1)*lSliceWid)+(gBGIMg.ScrnDim[1] div 2),lWid);
  end;
  if gMulti.OrthoView then begin
	DrawSag (gBGImg.ScrnDim[1] div 2,gMulti.nSlices*lSliceWid);
	//MultiImage.Canvas.Pen.Color := gBGIMg.XBarClr;
  //MultiImage.Canvas.Pen.Width := gBGImg.XBarThick;
	for lSlice := 1 to gMulti.nSlices do begin
		lLeft := gMulti.nSlices*lSliceWid+(gMulti.SliceList[lSlice]);
		//MultiImage.Canvas.MoveTo(lLeft,0);
		//MultiImage.Canvas.LineTo(lLeft,lHt);
    VLineWithFlatEndCaps (MultiImage.Canvas, gBGIMg.XBarClr,lLeft,0,lHt,gBGImg.XBarThick);

	end;
  end;//if orthoview
  RemoveHorizGaps(lSliceWid*gMulti.nSlices);
end; //CreateMultiCor


procedure TMultiSliceForm.CreateMultiAx;
var
	lSliceWid,lSlice,lHt,lWid,lLeft: integer;
begin
  lHt:= gBGIMg.ScrnDim[2];
  lSliceWid :=gBGIMg.ScrnDim[1]+2;//+1 for 1-voxel gap between slices - ensures we can detect slice boundary
  lWid := lSliceWid*gMulti.nSlices;
  if gMulti.OrthoView then begin //sag crossview
	lWid := lWid + gBGIMg.ScrnDim[2];
	if gBGIMg.ScrnDim[3]> lHt then
		lHt := gBGIMg.ScrnDim[3];
  end;
  CreateBlankBitmap (lHt,lWid, MultiImage);
  for lSlice := 1 to gMulti.nSlices do begin
	//ImgForm.ZViewEdit.value := gMulti.SliceList[lSlice];
	DrawAxial (gMulti.SliceList[lSlice],(lSlice-1)*lSliceWid);
	if gMulti.SliceLabel then DrawLabel(MultiImage,DimToMM(gMulti.SliceList[lSlice],gMulti.SliceList[lSlice],gMulti.SliceList[lSlice],3),((lSlice-1)*lSliceWid)+(gBGIMg.ScrnDim[1] div 2),lWid);
  end;
  if gMulti.OrthoView then begin
	lLeft := gMulti.nSlices*lSliceWid;
	DrawSag (gBGImg.ScrnDim[1] div 2,lLeft);
	//MultiImage.Canvas.Pen.Color := gBGIMg.XBarClr;
  //MultiImage.Canvas.Pen.Width := gBGImg.XBarThick;
  //HLineWithFlatEndCaps (MultiImage.Canvas, gBGIMg.XBarClr,lLeft,lWid,1);
	for lSlice := 1 to gMulti.nSlices do begin
		lHt := gBGImg.ScrnDim[3]-(gMulti.SliceList[lSlice]);
		//MultiImage.Canvas.MoveTo(lLeft,lHt);
		//MultiImage.Canvas.LineTo(lWid,lHt);
    HLineWithFlatEndCaps (MultiImage.Canvas, gBGIMg.XBarClr,lLeft,lWid,lHt,gBGImg.XBarThick);
	end;
  end;
  RemoveHorizGaps(lSliceWid*gMulti.nSlices);
end; //CreateMultiAx
	  
procedure TMultiSliceForm.CreateMultiSlice;
begin
 if gMulti.nSlices < 1 then begin
	showmessage('No valid slices selected - please use View/Slices.');
 end;
 case gMulti.Orient of
	3: CreateMultiCor;
	2: CreateMultiSag;
	else CreateMultiAx;
 end;//case
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
end;

procedure TMultiSliceForm.OrthoviewClick(Sender: TObject);
begin
	OrthoView.checked := not OrthoView.Checked;
	gMulti.OrthoView := OrthoView.checked;
	CreateMultiSlice;
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
	Close;
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
	gMultiSliceDir := extractfiledir(paramstr(0))+'\multislice\';
	gMultiSliceDefaultsFilename := gMultiSliceDir + 'default.ini';
	gMultiSliceStartupFilename := gMultiSliceDefaultsFilename;
   gMulti.Orient := 1;
   gMulti.OverslicePct := 0;
   gMulti.nSlices:= 4;
   gMulti.OrthoView := true;
   gMulti.SliceLabel := true;
   for lSlice := 1 to gMulti.nSlices do
	gMulti.SliceList[lSlice] := 62+10*lSlice;
end;

procedure TMultiSliceForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
WriteMultiSliceIniFile (gMultiSliceDefaultsFilename );
end;

procedure TMultiSliceForm.Savesettings1Click(Sender: TObject);
begin
  MultiSaveDialog.InitialDir := extractfiledir(gMultiSliceDir );
  if not MultiSaveDialog.Execute then exit;
  WriteMultiSliceIniFile(MultiSaveDialog.Filename);
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

procedure TMultiSliceForm.Close1Click(Sender: TObject);
begin
     close;
end;

end.
