unit define_types;

interface
{$IFDEF LINUX}
uses
  SysUtils,QDialogs,QControls;
{$ELSE}
uses
  delphiselectfolder,DiskSpaceKludge,Windows,SysUtils,Dialogs,Controls,classes,GZio,IniFiles;
{$ENDIF}
{$H+}
const
      NaN : double = 1/0;
	 //kImgFilter = 'Neuroimage (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz;*.head;*.nrrd;*.nhdr;*.mgh;*.mgz;*.mha;*.mhd|Volume of interest (*.voi)|*.voi';
	 //kImgPlusVOIFilter = 'NIfTI/Analyze/VOI|*.hdr;*.nii;*.nii.gz;*.voi;*.head;*.nrrd;*.nhdr;*.mgh;*.mgz;*.mha;*.mhd|NIfTI/Analyze Header (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz|Volume of interest (*.voi)|*.voi|Other|*.head;*.nrrd;*.nhdr;*.mgh;*.mgz;*.mha;*.mhd';
   kImgPlusVOIFilter = 'Neuroimaging/VOI|*.hdr;*.nii;*.nii.gz;*.voi;*.HEAD;*.mgh;*.mgz;*.mha;*.mhd;*.nhdr;*.nrrd|NIfTI/Analyze Header (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz|Volume of interest (*.voi)|*.voi|Anything (*.*)|*.*';
   kImgFilter = 'Neuroimaging|*.hdr;*.nii;*.nii.gz;*.HEAD;*.mgh;*.mgz;*.mha;*.mhd;*.nhdr;*.nrrd|Volume of interest (*.voi)|*.voi';
   kTxtFilter = 'Text (*.txt)|*.txt|Comma Separated (*.csv)|*.csv';
   kHistoBins = 256;//numbers of bins for histogram/image balance
	 PixelCountMax = 32768;
	 kTab = chr(9);
	 kEsc = chr(27);
	 kCR = chr (13);
	 kBS = #8 ; { Backspace }
	 kDel = #127 ; { Delete }
   kTextSep = kTab;//','; //',' for CSV, kTab for Tab-delimited values

    UNIXeoln = chr(10);
	 kLUTalpha = 128;
	 kVOI8bit = 1;//May07
{$IFDEF LINUX}
	   PathDelim = '/';
{$ELSE}
	   PathDelim = '\';
{$ENDIF}

type
    TStrRA = Array of String;
  TPSPlot =  RECORD //peristimulus plot
    TRSec,BinWidthSec: single;
    nNegBins,nPosBins,SPMDefaultsStatsFmriT,SPMDefaultsStatsFmriT0: integer;
    TextOutput,GraphOutput,
    SliceTime,SavePSVol,BaselineCorrect,PctSignal,RemoveRegressorVariability,TemporalDeriv,PlotModel,Batch: boolean
  end;
 TStretchQuality = (sqLow, sqHigh );
	  TLUT = array[0..255] of TRGBQuad;
	  kStr20 = string[20];
	  kStr50 = string[50];

    kStr255 = string[255];
  {TRGBquad = PACKED RECORD
		rgbBlue,rgbGreen,rgbRed,rgbreserved: byte;
  end;  }
  TCutout =  RECORD
		Lo : array [1..3] of integer;
		Hi : array [1..3] of integer;
  end;
  pRGBQuadArray = ^TRGBQuad;
  TRGBQuadeArray = ARRAY[0..PixelCountMax-1] OF TRGBQuad;
  RGBQuadRA = array [1..1] of TRGBQuad;
  RGBQuadp = ^RGBQuadRA;
	int32  = LongInt;
	uint32 = Cardinal;
	int16  = SmallInt;
	uint16 = Word;
	int8   = ShortInt;
	uint8  = Byte;
	SingleRA0 = array [0..0] of Single;
	Singlep0 = ^SingleRA0;
	ByteRA0 = array [0..0] of byte;
	Bytep0 = ^ByteRA0;
	WordRA0 = array [0..0] of Word;
	Wordp0 = ^WordRA0;
	SmallIntRA0 = array [0..0] of SmallInt;
	SMallIntp0 = ^SmallIntRA0;
	LongIntRA0 = array [0..0] of LongInt;
	LongIntp0 = ^LongIntRA0;
	DWordRA = array [1..1] of DWord;
	DWordp = ^DWordRA;
	ByteRA = array [1..1] of byte;
	Bytep = ^ByteRA;
	WordRA = array [1..1] of Word;
	Wordp = ^WordRA;
	SmallIntRA = array [1..1] of SmallInt;
	SMallIntp = ^SmallIntRA;
	LongIntRA = array [1..1] of LongInt;
	LongIntp = ^LongIntRA;
	SingleRA = array [1..1] of Single;
	Singlep = ^SingleRA;
	DoubleRA = array [1..1] of Double;
	Doublep = ^DoubleRA;
	DoubleRA0 = array [0..0] of Double;
	Doublep0 = ^DoubleRA0;
	HistoRA = array [0..kHistoBins] of longint;
        HistoDoubleRA = array [0..kHistoBins] of double;
	pRGBTripleArray = ^TRGBTripleArray;
	TRGBTripleArray = ARRAY[0..PixelCountMax-1] OF TRGBTriple;
function swap64r(s : double):double;
FUNCTION specialsingle (var s:single): boolean; //check if 32-bit float is Not-A-Number, infinity, etc
function FSize (lFName: String): longint;
function FileExistsEX(Name: String): Boolean;
function ParseFileName (lFilewExt:String): string;
function ParseFileFinalDir (lFileName:String): string;
function ExtractFileDirWithPathDelim(lInFilename: string): string;
function PadStr (lValIn, lPadLenIn: integer): string;
function ChangeFileExtX( var lFilename: string; lExt: string): string;
function swap4r4i (s:single): longint; //swap and convert: endian-swap and then typecast 32-bit float as 32-bit integer
function conv4r4i (s:single): longint; //convert: typecast 32-bit float as 32-bit integer
function swap8r(s : double):double; //endian-swap 64-bit float
procedure pswap4i(var s : LongInt); //procedure to endian-swap 32-bit integer
procedure pswap4r ( var s:single);  //procedure to endian-swap 32-bit integer
function specialdouble (d:double): boolean;
function RealToStr(lR: double {was extended}; lDec: integer): string;
procedure UnGZip (var lFname: string; var lBuf: ByteP{}; lOffset,lMaxSz: integer); //unzip
procedure UnGZip2 (var lFname: string; var lBuf: ByteP; lOffset,lMaxSz, lUncompressedInitialBytes: integer); //unzip
procedure UnGZipCore (var infile : gzFile; var lBuf: ByteP; lReadBytes: integer; lWrite: boolean);
function UpCaseExt(lFileName: string): string;
procedure swap4(var s : LongInt);
procedure Xswap4r ( var s:single);
function Bool2Char (lBool: boolean): char;
function Char2Bool (lChar: char): boolean;
function Log(X, Base: single): single;
procedure GZipBuffer(var FGzipFilename,FFileDestination: String;lxInBuffer: byteP;lInSize: Integer; lOverwritewarn: boolean);
//procedure GZipBuffer(var FGzipFilename,FFileDestination: String;lxInBuffer: byteP;lInSize: Integer);
function DiskFreeEx (DriveStr: String): Integer;
procedure SortSingle(var lLo,lHi: single);
function IniInt(lIniFile: TIniFile; lIdent: string;  lDefault: integer): integer;
function IniBool(var lIniFile: TIniFile; lIdent: string;  lDefault: boolean): boolean;
procedure CopyFileEX (lInName,lOutName: string);
procedure CopyFileEXoverwrite (lInName,lOutName: string);
procedure fx (a: double); overload;
procedure fx (a,b: double); overload;
procedure fx (a,b,c: double); overload;
procedure fx (a,b,c,d: double); overload;
function ChangeFilePostfixExt (lInName,lPostfix,lExt: string): string;
function Bound (lDefault,lMin,lMax: integer): integer;
procedure SortInt (var lMin,lMax: integer);
function ChangeFilePrefix (lInName,lPrefix: string): string;
//function Mod1(lVal,lDiv: integer): integer; //returns 1..n instead of 0..[n-1] - e.g. mod1(360,180)=180 not 0
//function Div1(lVal,lMod: integer): integer; //for input 1..inifinity, returns 1..lMod,1..lMod,1..lMod usage: Y coordinates
procedure SortCutout (var lCutout : TCutout); //ensure Lo < Hi
function freeRam: Int64;
function OKMsg(lMsg: string): boolean; //shows dialog with OK/Cancel returns true if user presses OK
function DirExists (lDir: String): boolean;
function IsExtNIFTIHdr(lStr: string): boolean;
function GetDirPrompt (lDefault: string): string;
function GzExt(lFileName: string): boolean;

implementation

function GzExt(lFileName: string): boolean;
var lExt: string;
begin
     lExt := UpCaseExt(lFilename);
     if (lExt = '.VOI') or (lExt = '.NII.GZ') or (lExt = '.GZ') then
        result := true
     else
         result := false;
end;

function GetDirPrompt (lDefault: string): string;
// Old versions of Delphi have a clumsy SelectDirectory function, and locks the folder until you quit your application...
var
   lD: string;
begin
  lD := lDefault;
  if not DirExists(lD) then
     lD := 'c:\';
  result := lD;  // Set the starting directory
  {$IFDEF FPC}
  //Delphi SelectDirectory uses FileCtrl
  //Lazarus SelectDirectory uses Dialogs
  chdir(result); //start search from previous dir...
  if SelectDirectory(result, [sdAllowCreate,sdPerformCreate,sdPrompt], 0) then begin
     chdir(result);
     exit;
  end;
  {$ELSE}
  if SelectDirectoryDelphi('Select  folder', result, true) then
     exit;
  {$ENDIF}
  //if the user aborts, make sure we use the default directory...
  result := '';//lD;
end;

function IsExtNIFTIHdr(lStr: string): boolean;
//detect hdr, nii,niigz
var
   lExt: string;
begin
     result := false;
     lExt := UpCaseExt(lStr);
     if (lExt = '.NII') or (lExt = '.NII.GZ') then
        result := true;
     if (lExt = '.HDR') and (FSize(ChangeFileExt(lStr,'.img'))> 0) then
        result := true;
     (*if (lExt = '.IMG') and (FSize(ChangeFileExt(lStr,'.hdr'))> 0) then
        result := true;  *)
end;

function OKMsg(lMsg: string): boolean; //shows dialog with OK/Cancel returns true if user presses OK
begin
     result := false;
	 case MessageDlg(lMsg, mtConfirmation,
		[mbYes, mbCancel], 0) of
		mrCancel: exit;
     end; //case
     result := true;
end;

function DirExists (lDir: String): boolean;
var lSearchRec: TSearchRec;
begin
  FindFirst(lDir, faAnyFile, lSearchRec);
    if (faDirectory and lSearchRec.attr) = faDirectory then
       DirExists := true
    else
        DirExists := false;
  FindClose(lSearchRec);{}
end;

procedure SortCutout (var lCutout : TCutout); //ensure Lo < Hi
var lInc,lSwap: integer;
begin
	 for lInc := 1 to 3 do
		if lCutout.Lo[lInc] > lCutout.Hi[lInc] then begin
			lSwap := lCutout.Lo[lInc];
			lCutout.Lo[lInc] := lCutout.Hi[lInc];
			lCutout.Hi[lInc] := lSwap;
		end;
end;

function ChangeFilePrefix (lInName,lPrefix: string): string;
var
	lC,lLen,lPos: integer;
	lStr: string;
begin
	//result := changefileext(lInName,lExt);
        result := lInName;
	lLen := length (result);
	if lLen < 1 then exit;
	lPos := lLen;
	while (lPos > 1) and (result[lPos] <> pathdelim) do
		dec(lPos);
	lStr := '';
	for lC := 1 to lPos do
		lStr := lStr+result[lC];
	lStr := lStr+lPrefix;
	if lPos < lLen then
		for lC := (lPos+1) to lLen do
			lStr := lStr+result[lC];
	result := lStr;
end;

procedure SortInt (var lMin,lMax: integer);
var
   lSwap: integer;
begin
     if lMin <= lMax then
        exit;
     lSwap := lMax;
     lMax := lMin;
     lMin := lSwap;
end;

function Bound (lDefault,lMin,lMax: integer): integer;
begin
    result := lDefault;
    if result < lMin then
       result := lMin;
    if result > lMax then
       result := lMax;
end;

procedure fx (a: double); overload; //fx used to help debugging - reports number values
begin
	showmessage(floattostr(a));
end;

procedure fx (a,b: double); overload; //fx used to help debugging - reports number values
begin
	showmessage(floattostr(a)+'x'+floattostr(b));
end;

procedure fx (a,b,c: double); overload; //fx used to help debugging - reports number values
begin
	showmessage(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c));
end;

procedure fx (a,b,c,d: double); overload; //fx used to help debugging - reports number values
begin
	showmessage(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c)+'x'+floattostr(d));
end;
                   
function freeRam: Int64;
var
  memory:TMemoryStatus;

begin
  memory.dwLength:=sizeof(memory);
  GlobalMemoryStatus(memory);
  result := memory.dwavailPhys;
  //result := 1024;
end;


function ChangeFilePostfixExt (lInName,lPostfix,lExt: string): string;
var
	lC,lLen,lPos: integer;
	lStr: string;
begin
	result := changefileext(lInName,lExt);
	lLen := length (result);
	if lLen < 1 then exit;
	lPos := lLen;
	while (lPos > 1) and (result[lPos] <> pathdelim) and (result[lPos] <> '.') do
		dec(lPos);
        if result[lPos] = '.' then
           dec(lPos);
	lStr := '';
	for lC := 1 to lPos do
		lStr := lStr+result[lC];
	lStr := lStr+lPostfix;
	if lPos < lLen then
		for lC := (lPos+1) to lLen do
			lStr := lStr+result[lC];
	result := lStr;
end;

procedure CopyFileEXoverwrite (lInName,lOutName: string);
var lFSize: Integer;
   lBuff: bytep0;
   lFData: file;
begin
	 lFSize := FSize(lInName);
	 if (lFSize < 1)  then exit;
	 assignfile(lFdata,lInName);
	 filemode := 0;
	 reset(lFdata,lFSize{1});
	 GetMem( lBuff, lFSize);
	 BlockRead(lFdata, lBuff^, 1{lFSize});
	 closefile(lFdata);
	 assignfile(lFdata,lOutName);
	 filemode := 2;
	 Rewrite(lFdata,lFSize);
	 BlockWrite(lFdata,lBuff^, 1  {, NumWritten});
	 closefile(lFdata);
	 freemem(lBuff);
end;

procedure CopyFileEX (lInName,lOutName: string);
var lFSize: Integer;
begin

	 lFSize := FSize(lInName);
	 if (lFSize < 1) or (fileexistsEX(lOutName)) then exit;
	CopyFileEXoverwrite (lInName,lOutName);
end;

function IniInt(lIniFile: TIniFile; lIdent: string;  lDefault: integer): integer;
var
	lStr: string;
begin
	result := lDefault;
	lStr := lIniFile.ReadString('INT',lIdent, '');
	if length(lStr) > 0 then
		result := StrToInt(lStr);
end; //nested IniInt

function IniBool(var lIniFile: TIniFile; lIdent: string;  lDefault: boolean): boolean;
var
	lStr: string;
begin
	result := lDefault;
	lStr := lIniFile.ReadString('BOOL',lIdent, '');
	//showmessage('x'+lStr+'x');
	if length(lStr) > 0 then
		result := Char2Bool(lStr[1]);
end; //nested IniBool


procedure SortSingle(var lLo,lHi: single);
var lSwap: single;
begin
	if lLo > lHi then begin
		lSwap := lLo;
		lLo := lHi;
		lHi := lSwap;
	end; //if Lo>Hi
end; //proc SortSingle

function DiskFreeEx (DriveStr: String): Integer;
var
  lOutDisk: Integer;
  lDiskDir : string;
  lSize8: Tinteger8;
begin
     result := 0;
     if length(DriveStr) < 1 then
      exit;
     lOutDisk := ord(upcase(DriveStr[1]))+1-ord('A');
     if (lOutDisk >= ord('A')) and (lOutDisk <= ord('Z')) then begin
        DiskFreeEx := DiskFree(lOutDisk);
     end else begin
         lDiskDir :=(ExtractFileDrive(DriveStr))+'\';
         lSize8 := DiskFreeStr (lDiskDir);
         if lSize8 > MaxINt then DiskFreeEx := MaxInt
         else DiskFreeEx := round(lSize8);
     end;
end;

function gz_compressBuffer (lxInBuffer: ByteP;lInSize: integer;outfile:gzFile): integer;
var
  len   : Integer;
  lInBufferPos,ioerr : integer;
  buf  : packed array [0..Z_BUFSIZE-1] of byte; { Global uses BSS instead of stack }
  errorcode : byte;
function blocktransfer(var lInBuffer: ByteP;  lSizeRequested: integer; var lSizeTransferred:integer): integer;
begin
	result := 0;
	if  lInBufferPos > lInSize then begin
		result := 666;
		exit;
	end else if (lInBufferPos + lSizeRequested) <= lInSize then
		lSizeTransferred := lSizeRequested
	else
		lSizeTransferred := lInSize-lInBufferPos;
	//for lC := 1 to lSizeTransferred do
	//	buf[lC-1] := lInBuffer[lInBufferPos+lC] ;
	move(lInbuffer[lInBufferPos+1],buf,lSizeTransferred);
	//move(src,dest,count);

	lInBufferPos := lInBufferPos+lSizeTransferred;
end;
begin
//showmessage(inttostr(Z_BUFSIZE));
  lInBufferPos := 0;
  errorcode := 0;
  //Progress := 0;
  //fsize := lInSize;
  //lensize := 0;
  //if FProgressStep > 0 then DoOnProgress;
  while true do begin
	//lll{$I-}blockread (infile, buf, Z_BUFSIZE, len);{$I+}
	ioerr := blocktransfer(lxInBuffer,Z_BUFSIZE, len);
	if (ioerr <> 0) then begin
	  errorcode := 1;
	  break
	end;
	if (len = 0) then break;
	{$WARNINGS OFF}{Comparing signed and unsigned types}
	if (gzwrite (outfile, @buf, len) <> len) then begin
	{$WARNINGS OFF}
	  errorcode := 2;
	  break
	end;
  end; {WHILE}
  if (gzclose (outfile) <> 0{Z_OK}) then errorcode := 3;
  result := errorcode;
end;

procedure GZipBuffer(var FGzipFilename,FFileDestination: String;lxInBuffer: byteP;lInSize: Integer; lOverwritewarn: boolean);
var
   FGzipComments ,outmode,s : string;
	infile  : file;
	outfile : gzFile;
	FCompressionLevel{,errorcode} : integer;
	flags : uInt;
	stream : gz_streamp;
	p : PChar;
begin
FGzipComments := '';
//FProgress := 0;
FCompressionLevel := 6;//MainForm.CompressEdit.value;
if (FCompressionLevel > 9) or (FCompressionLevel<0) then FCompressionLevel := 6;
 if lOverwritewarn and fileexists(FFileDestination) then begin
		case MessageDlg('Overwrite the file '+FFileDestination+'?', mtConfirmation,[mbYes, mbAbort], 0) of	{ produce the message dialog box }
			 id_Abort: exit;
		end;
 end;
	  //w adds .gz extension-> outmode := 'w  ';
	  outmode := 'w  ';
	  s := IntToStr(FCompressionLevel);
	  outmode[2] := s[1];
		  outmode[3] := ' ';
	  (*case FCompressionType of
		   Standard    : outmode[3] := ' ';
		   HuffmanOnly : outmode[3] := 'h';
		   Filtered    : outmode[3] := 'f';
	  end;*)
	  //flags := 0;
	  //if (zfilename in FGzipHeader) then
		  flags := ORIG_NAME;
	  //if (comment  in FGzipHeader) then flags := flags + COMMENT_;
	  outfile := gzopenZ (FFileDestination, outmode, flags);
	  //showmessage(FFileDestination);
	  if (outfile = NIL) then begin
		 //if FWindowOnError then
			  MessageDlg('Can''t open: '+FFileDestination, mtError, [mbAbort], 0);
		 close( infile);
		 //errorcode := 2
				 exit;
	  end
	  else begin
		 stream := gz_streamp(outfile);
		 if {(zfilename in FGzipHeader)} true then begin
						//s := ExtractFilename(lInFileName);
			s := ExtractFilename(FGzipFilename);
			p := PChar(s);
			blockWrite( stream^.gzfile, p[0], length(s)+1);
			stream^.startpos := stream^.startpos + length(s) + 1
		 end;
		 gz_compressBuffer (lxInBuffer,lInSize,outfile);
	  end
end;


(*function gz_compress (var infile:file; outfile:gzFile): integer;
var
  len   : uInt;
  ioerr : integer;
  buf  : packed array [0..Z_BUFSIZE-1] of byte; { Global uses BSS instead of stack }
  errorcode : byte;
  fsize, lensize : DWord;
begin
  errorcode := 0;
  //Progress := 0;
  fsize := FileSize(infile);
  lensize := 0;
  //if FProgressStep > 0 then DoOnProgress;
  while true do begin
	{$I-}blockread (infile, buf, Z_BUFSIZE, len);{$I+}
	ioerr := IOResult;
	if (ioerr <> 0) then begin
	  errorcode := 1;
	  break
	end;
	if (len = 0) then break;
	{$WARNINGS OFF}{Comparing signed and unsigned types}
	if (gzwrite (outfile, @buf, len) <> len) then begin
	{$WARNINGS OFF}
	  errorcode := 2;
	  break
	end;
	(*if FProgressStep > 0 then begin
	   {$WARNINGS OFF}{Calculate progress and raise event}
	   lensize := lensize + len;
	   if ((lensize / fsize) * 100 >= FProgress + FProgressStep)
						or (lensize = fsize) then begin
		  FProgress := Trunc((lensize / fsize) * 100);
		  DoOnProgress
	   end
	   {$WARNINGS ON}
	end   *)
(*  end; {WHILE}
  closeFile (infile);
  if (gzclose (outfile) <> 0{Z_OK}) then errorcode := 3;
  gz_compress := errorcode;
end;

procedure GZipFile(lSourceFile: String);
var
   //FGzipHeader : THeader;
   //FCompressionLevel,FProgress,Progress: integer;
   FGzipFilename : string;
   FGzipComments : string;
   outmode : string;
	s,FFileDestination : string;
	infile  : file;
	outfile : gzFile;
	FCompressionLevel{,errorcode} : integer;
	flags : uInt;
	stream : gz_streamp;
	p : PChar;
	//lProceed: TModalResult;
	ioerr : integer;
begin
//FGzipHeader := [zFilename];
FGzipFilename:= lSourceFile;
FGzipComments := '';
//FProgress := 0;
FCompressionLevel := 6;//MainForm.CompressEdit.value;
if (FCompressionLevel > 9) or (FCompressionLevel<0) then FCompressionLevel := 6;
//MainForm.ProgressBar1.position :=1;
//Gzip (lFile,lMulti);
//MainForm.ProgressBar1.position := 0;
 FFileDestination := lSourceFile+'.gz';
 //result := 2; //return error if user aborts
 if fileexists(FFileDestination) then begin
		case MessageDlg('Overwrite the file '+FFileDestination+'?', mtConfirmation,[mbYes, mbAbort], 0) of	{ produce the message dialog box }
			 id_Abort: exit;
		end;
 end;
  AssignFile (infile, lSourceFile);
  {$I-}
  Reset (infile,1);
  {$I+}
  ioerr := IOResult;
  if (ioerr <> 0) then begin
	//if FWindowOnError then
		 MessageDlg('Can''t open: '+lSourceFile, mtError, [mbAbort], 0);
	//errorcode := 1
  end
  else begin
	  outmode := 'w  ';
	  s := IntToStr(FCompressionLevel);
	  outmode[2] := s[1];
		  outmode[3] := ' ';
	  (*case FCompressionType of
		   Standard    : outmode[3] := ' ';
		   HuffmanOnly : outmode[3] := 'h';
		   Filtered    : outmode[3] := 'f';
	  end;*)
(*
	  //flags := 0;
	  //if (zfilename in FGzipHeader) then
		  flags := ORIG_NAME;
	  //if (comment  in FGzipHeader) then flags := flags + COMMENT_;
	  outfile := gzopenZ ({FFileDestination}lSourceFile, outmode, flags);
	  if (outfile = NIL) then begin
		 //if FWindowOnError then
			  MessageDlg('Can''t open: '+FFileDestination, mtError, [mbAbort], 0);
		 close( infile);
		 //errorcode := 2
				 exit;
	  end
	  else begin
		 { if flags are set then write them }
		 stream := gz_streamp(outfile);
		 if {(zfilename in FGzipHeader)} true then begin
						s := ExtractFilename(lSourceFile);
			p := PChar(s);
			blockWrite( stream^.gzfile, p[0], length(s)+1);
			stream^.startpos := stream^.startpos + length(s) + 1
		 end;
		 {if (zcomment  in FGzipHeader) then begin
			p := PChar(FGzipComments);
			blockWrite( stream^.gzfile, p[0], length(FGzipComments)+1);
			stream^.startpos := stream^.startpos + length(FGzipComments) + 1
		 end;
		 { start compressing }
		 {errorcode :=} gz_compress(infile, outfile);
		 {if errorcode <> 0 then errorcode := errorcode+100
		 else
			if FDeleteSource then erase (infile);}
	  end
   end;
   //MainForm.ProgressBar1.position := 0;
end; *)

function Log(X, Base: single): single;
begin
  if X = 0 then
	result := 0
  else
  	Log := Ln(X) / Ln(Base);
end;

function Bool2Char (lBool: boolean): char;
begin
	if lBool then
		result := '1'
	else
		result := '0';
end;

function Char2Bool (lChar: char): boolean;
begin
	if lChar = '1' then
		result := true
	else
		result := false;
end;


procedure Xswap4r ( var s:single);
type
  swaptype = packed record
	case byte of
	  0:(Word1,Word2 : word); //word is 16 bit
	  //1:(float:single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  inguy.Word1 := outguy.Word1;
  inguy.Word2 := outguy.Word2;
end;


procedure swap4(var s : LongInt);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s:=outguy.Long;
end;

function UpCaseExt(lFileName: string): string;
var lI: integer;
l2ndExt,lExt : string;
begin
	 lExt := ExtractFileExt(lFileName);
	 if length(lExt) > 0 then
		for lI := 1 to length(lExt) do
			lExt[lI] := upcase(lExt[lI]);
	 result := lExt;
	 if lExt <> '.GZ' then exit;
	 lI := length(lFileName) - 6;
	 if li < 1 then exit;
	 l2ndExt := upcase(lFileName[lI])+upcase(lFileName[lI+1])+upcase(lFileName[li+2])+upcase(lFileName[li+3]);
	 if (l2ndExt = '.NII')then
		result :=  l2ndExt+lExt
         else if  (l2ndExt = 'BRIK') and (lI > 1) and (lFileName[lI-1] = '.') then
              result := '.BRIK'+lExt;
end;

procedure UnGZipCore (var infile : gzFile; var lBuf: ByteP; lReadBytes: integer; lWrite: boolean);
const
     BUFLEN = 16384;
var
  buf  : packed array [0..BUFLEN-1] of byte; { Global uses BSS instead of stack }
  len,lI,written : integer;
begin
  written := 0;
  if lReadBytes < 1 then exit;
  Len := lReadBytes div BUFLEN;
  if Len > 0 then
        for lI := 1 to Len do begin
            gzread (infile, @buf, BUFLEN {1388});
            if lWrite then
               Move(buf,lbuf[Written+1],BUFLEN);
            Written := Written + BUFLEN;
        end;
  Len := lReadBytes mod BUFLEN;
  if Len = 0 then exit;
  gzread (infile, @buf, Len);
  if lWrite then
        Move(buf,lbuf[Written+1],len);
end; //ungzipCore

procedure UnGZip2 (var lFname: string; var lBuf: ByteP; lOffset,lMaxSz, lUncompressedInitialBytes: integer); //unzip
const
  BUFLEN =  4096;//16384;
var
	infile : gzFile;
  lbufsz,len,lI     : integer;
  written : integer;
 buf  : packed array [0..BUFLEN-1] of byte; { Global uses BSS instead of stack }
begin
  infile := gzopenZskip (lFName, 'r', 0, lUncompressedInitialBytes);

  written := 0;
 if lOffset > 0 then begin     //gzseek(infile,lOffset,0);
     Len := lOffset div BUFLEN;
     if Len > 0 then
        for lI := 1 to Len do
            gzread (infile, @buf, BUFLEN {1388});
     Len := lOffset mod BUFLEN;
     gzread (infile, @buf, Len);
  end;
  lbufsz := BUFLEN;
  if lMaxSz < BUFLEN then
     lbufsz := lMaxSz;
  while true do begin
	len := gzread (infile, @buf, lbufsz);
	if (len < 0) then begin
	   break
	end;
	if (len = 0) then
           break;
        if (Written+len) > lMaxSz then begin
           if Written < lMaxSz then
              Move(buf,lbuf[Written+1],lMaxSz-Written); //cr2007
           break;
        end;
        Move(buf,lbuf[Written+1],len);
        Written := Written + len;
  end; {WHILE}
  gzclose (infile);
end;

procedure UnGZip (var lFname: string; var lBuf: ByteP; lOffset,lMaxSz: integer); //unzip
begin
  UnGZip2(lFname, lBuf, lOffset,lMaxSz,0);
end;
(*procedure UnGZip (var lFname: string; var lBuf: ByteP; lOffset,lMaxSz: integer); //unzip
const
  BUFLEN =  4096;//16384;
var
	infile : gzFile;
  lbufsz,len,lI     : integer;
  written : integer;
 buf  : packed array [0..BUFLEN-1] of byte; { Global uses BSS instead of stack }
begin
  infile := gzopenZ (lFName, 'r', 0);
  written := 0;
 if lOffset > 0 then begin
     Len := lOffset div BUFLEN;
     if Len > 0 then
        for lI := 1 to Len do
            gzread (infile, @buf, BUFLEN {1388});
     Len := lOffset mod BUFLEN;
     gzread (infile, @buf, Len);
  end;
  lbufsz := BUFLEN;
  if lMaxSz < BUFLEN then
     lbufsz := lMaxSz;
  while true do begin
	len := gzread (infile, @buf, lbufsz);
	if (len < 0) then begin
	   break
	end;
	if (len = 0) then
           break;
        if (Written+len) > lMaxSz then begin
           if Written < lMaxSz then
              Move(buf,lbuf[Written+1],lMaxSz-Written); //cr2007
           break;
        end;
        Move(buf,lbuf[Written+1],len);
        Written := Written + len;
  end; {WHILE}
  gzclose (infile);
end;  *)



function RealToStr(lR: double {was extended}; lDec: integer): string;
begin
     RealTOStr := FloatToStrF(lR, ffFixed,7,lDec);
end;

FUNCTION specialdouble (d:double): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//8byte IEEE: msb[63] = signbit, bits[52-62] exponent, bits[0..51] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
CONST kSpecialExponent = 2047 shl 20;
VAR Overlay: ARRAY[1..2] OF LongInt ABSOLUTE d;
BEGIN
  IF ((Overlay[2] AND kSpecialExponent) = kSpecialExponent) THEN
     RESULT := true
  ELSE
      RESULT := false;
END;

function swap8r(s : double):double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(float:double);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  try
    result:=outguy.float;
  except
        result := 0;
        exit;
  end;
end; //func swap8r

procedure pswap4i(var s : LongInt);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s:=outguy.Long;
end; //proc swap4

procedure pswap4r ( var s:single);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      //1:(float:single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  inguy.Word1 := outguy.Word1;
  inguy.Word2 := outguy.Word2;
end; //proc Xswap4r

function swap64r(s : double):double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(float:double);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  try
    swap64r:=outguy.float;
  except
        swap64r := 0;
        exit;
  end;{}
end;

function conv4r4i (s:single): longint;
type
  swaptype = packed record
    case byte of
      1:(long:longint);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
begin
  inguy := @s; //assign address of s to inguy
  conv4r4i:=inguy.long;
end;

function swap4r4i (s:single): longint; //
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(long:longint);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  swap4r4i:=outguy.long;
end;//swap4r4i

function ChangeFileExtX( var lFilename: string; lExt: string): string;
begin
    result := ParseFileName(lFilename)+lExt;
    //showmessage(ParseFileName(lFilename));
    //result := ChangeFileExt(lFilename,lExt);
end;

function PadStr (lValIn, lPadLenIn: integer): string;
var lOrigLen,lPad : integer;
begin
 lOrigLen := length(inttostr(lValIn));
 result := inttostr(lValIn);
 if lOrigLen < lPadLenIn then begin
    lOrigLen := lPadLenIn-lOrigLen;
    for lPad := 1 to lOrigLen do
        result := '0'+result;
 end;
end;

function ExtractFileDirWithPathDelim(lInFilename: string): string;
//F:\filename.ext -> 'F:\' and F:\dir\filename.ext -> 'F:\dir\'
//Despite documentation, Delphi3's ExtractFileDir does not always retain final pathdelim
var lFilePath: string;
begin
     result := '';
     lFilePath := ExtractFileDir(lInFilename);
     if length(lFilepath) < 1 then exit;
     if lFilePath[length(lFilepath)] <> pathdelim then
        lFilepath := lFilepath + pathdelim; //Delphi3 bug: sometimes forgets pathdelim
     result := lFilepath;
end;

function ParseFileFinalDir (lFileName:String): string;
var
   lLen,lInc,lPos: integer;
   lInName,lName: String;
begin
     lInName := extractfiledir(lFilename);
     lName := '';
     lLen := length(lInName);
     if  lLen < 1 then exit;
     lInc := lLen;
     repeat
		dec(lInc);
     until (lInName[lInc] = pathdelim) or (lInc = 1);
     if lInName[lInc] = pathdelim then inc(lInc); //if '\folder' then return 'folder'
	 for lPos := lInc to lLen do
		lName := lName + lInName[lPos];
	 ParseFileFinalDir := lName;
end;

function ParseFileName (lFilewExt:String): string;
var
  lExt: string;
  i: integer;
begin
  lExt := UpCaseExt(lFilewExt);
  if (length(lExt) < 1) or (length(lExt) >= length(lFilewExt)) then exit;
  result := '';
  for i := 1 to ( length(lFilewExt)- length(lExt)) do
    result := result + lFileWExt[i] ;
end;

Function FileExistsEX(Name: String): Boolean;
var
   F: File;
begin
  result := false;
  if Name = '' then
     exit;
  result := FileExists(Name);
  if result then exit;
   //the next bit attempts to check for a file to avoid WinNT bug
   AssignFile(F, Name);
   {$I-}
   Reset(F);
   {$I+}
   Result:=IOresult = 0;
   if Result then
     CloseFile(F);
end;

function FSize (lFName: String): longint;
var SearchRec: TSearchRec;
begin
  result := 0;
  if not fileexistsex(lFName) then exit;
  FindFirst(lFName, faAnyFile, SearchRec);
  result := SearchRec.size;
  FindClose(SearchRec);
end;

procedure Xswap8r(var s : double);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      //1:(float:double);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  inguy.Word1 := outguy.Word1;
  inguy.Word2 := outguy.Word2;
  inguy.Word3 := outguy.Word3;
  inguy.Word4 := outguy.Word4;
end;

FUNCTION specialsingle (var s:single): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//4byte IEEE: msb[31] = signbit, bits[23-30] exponent, bits[0..22] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
CONST kSpecialExponent = 255 shl 23;
VAR Overlay: LongInt ABSOLUTE s;
BEGIN
  IF ((Overlay AND kSpecialExponent) = kSpecialExponent) THEN
     RESULT := true
  ELSE
      RESULT := false;
END;

end.
