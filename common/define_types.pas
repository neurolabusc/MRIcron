unit define_types;
interface
{$H+}
{$include isgui.inc}

        uses
       {$IFNDEF FPC}
               {$IFDEF GUI} FileCtrl, delphiselectfolder,  {$ENDIF}
                 DiskSpaceKludge,  Controls,
       {$ELSE}
         {$IFDEF GUI} lclintf,LResources,{$ENDIF}
        {$ENDIF}
        {$IFNDEF Unix} Windows,
        {$ELSE}
        BaseUnix,{$IFDEF GUI} LCLType, {$ENDIF}//lclintf, LMessages,LCLType,//gettickcount
        {$ENDIF}

        SysUtils,classes,IniFiles,
        {$IFDEF GUI} forms,userdir, dialogs{$ELSE}dialogsx{$ENDIF};
const
     kMRIcronVersDate = '13JAN2016';
     {$IFDEF LCLCocoa}
     kMRIcronAPI = 'Cocoa';
    {$ELSE}
      {$IFDEF LCLCarbon}
      kMRIcronAPI = 'Carbon';
      {$ELSE}
       kMRIcronAPI = ''; //windows, GTK, QT
      {$ENDIF}
    {$ENDIF}
      {$ifdef CPU32}
      kMRIcronCPU = '32';
     {$ELSE}
      kMRIcronCPU = '64';
     {$ENDIF}
     kMRIcronVers = kMRIcronVersDate+' '+ kMRIcronCPU +'bit BSD License '+kMRIcronAPI;
     NaN : double = 1/0;
     kMagicDouble : double = -111666222;
     kTxtFilter = 'Text (*.txt)|*.txt;*.csv|Comma Separated (*.csv)|*.csv';
     kAnyFilter =  'Anything (*)|*';
     kAnaHdrFilter = 'Analyze Header (*.hdr)|*.hdr';

     //kNIIFilter = 'NIfTI (*.nii)|*.nii';
     //kImgPlusVOIFilter = 'NIfTI/Analyze/VOI|*.hdr;*.nii;*.nii.gz;*.voi|NIfTI/Analyze Header (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz|Volume of interest (*.voi)|*.voi';
     //kImgFilter = 'NIfTI/Analyze Header (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz|Volume of interest (*.voi)|*.voi';
     //kImgFilterPlusAny = 'NIfTI/Analyze Header (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz|Volume of interest (*.voi)|*.voi|Any file (*.*)|*.*';

     kNIIFilter = 'Neuroimaging (*.nii)|*.hdr;*.nii;*.nii.gz;*.voi;*.HEAD;*.mgh;*.mgz;*.mha;*.mhd;*.nhdr;*.nrrd';
     kImgFilter = 'Neuroimaging|*.hdr;*.nii;*.nii.gz;*.HEAD;*.mgh;*.mgz;*.mha;*.mhd;*.nhdr;*.nrrd|Volume of interest (*.voi)|*.voi';
     kImgPlusVOIFilter = 'Neuroimaging/VOI|*.hdr;*.nii;*.nii.gz;*.voi;*.HEAD;*.mgh;*.mgz;*.mha;*.mhd;*.nhdr;*.nrrd|NIfTI/Analyze Header (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz|Volume of interest (*.voi)|*.voi';
     kImgFilterPlusAny = 'Neuroimaging/VOI|*.hdr;*.nii;*.nii.gz;*.voi;*.HEAD;*.mgh;*.mgz;*.mha;*.mhd;*.nhdr;*.nrrd|NIfTI/Analyze Header (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz|Volume of interest (*.voi)|*.voi|Anything (*.*)|*.*';
     kHistoBins = 256;//numbers of bins for histogram/image balance
     PixelCountMax = 32768;
     kTab = chr(9);
     kEsc = chr(27);
     kCR = chr (13);
     kBS = #8 ; // Backspace
     kDel = #127 ; // Delete
     UNIXeoln = chr(10);
        kTextSep = kTab;//','; //',' for CSV, kTab for Tab-delimited values
     {$IFDEF Darwin}
       kLUTalpha = 255; //255
     {$ELSE}
        kLUTalpha = 0; //255
     {$ENDIF}
     kVOI8bit = 1;//May07 100;
{$IFDEF unix}
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
  TRGBquad = PACKED RECORD
      {$IFDEF ENDIAN_BIG} //OSX PPC
           rgbreserved,rgbRed,rgbGreen,rgbBlue: byte;
           //rgbBlue,rgbGreen,rgbRed,rgbreserved: byte;
      {$ELSE}
           {$IFDEF UNIX}
              {$IFDEF DARWIN}
                     rgbreserved,rgbRed,rgbGreen,rgbBlue: byte;
              {$ELSE}
                     rgbRed,rgbGreen,rgbBlue,rgbreserved: byte;
              {$ENDIF}
           {$ELSE} //not unix - windows
             //rgbreserved,rgbRed,rgbGreen,rgbBlue: byte;
           rgbBlue,rgbGreen,rgbRed,rgbreserved: byte;
           {$ENDIF}
//           rgbBlue,rgbGreen,rgbRed,rgbreserved: byte;
      {$ENDIF}
  end;
 TStretchQuality = (sqLow, sqHigh);

 //TLUTrgb = array[0..255] of TRGBQuad;
 //TLUTtype = DWORD;
 TLUT = array[0..255] of TRGBQuad;
   kStr20 = string[20];
   kStr50 = string[50];

   kStr255 = string[255];

  TCutout =  RECORD
		Lo : array [1..3] of integer;
		Hi : array [1..3] of integer;
  end;
	int32  = LongInt;
	uint32 = Cardinal;
	int16  = SmallInt;
	uint16 = Word;
	int8   = ShortInt;
	uint8  = Byte;
  	Int64RA = array [1..1] of int64;
	Int64p = ^Int64RA;

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
 	SingleRARA = array [1..1] of Singlep;
	SingleRAp = ^SingleRARA;
	DoubleRA = array [1..1] of Double;
	Doublep = ^DoubleRA;
	DoubleRA0 = array [0..0] of Double;
	Doublep0 = ^DoubleRA0;
	HistoRA = array [0..kHistoBins] of longint;
         HistoDoubleRA = array [0..kHistoBins] of double;
  //pRGBQuadArray = ^TRGBQuad;
  //TRGBQuadeArray = ARRAY[0..PixelCountMax-1] OF TRGBQuad;
  //RGBQuadRA = array [1..1] of TRGBQuad;
  //RGBQuadp = ^RGBQuadRA;
  TQuadRA = array [1..1] of TRGBQuad;

  RGBQuadp = ^TQuadRA;


//	pRGBTripleArray = ^TRGBTripleArray;
//	TRGBTripleArray = ARRAY[0..PixelCountMax-1] OF TRGBTriple;
FUNCTION specialsingle (var s:single): boolean; //check if 32-bit float is Not-A-Number, infinity, etc
function FSize (lFName: String): Int64;
function FileExistsEX(Name: String): Boolean;
function ParseFileName (lFilewExt:String): string;
function ParseFileFinalDir (lFileName:String): string;
function ExtractFileDirWithPathDelim(lInFilename: string): string;
function PadStr (lValIn, lPadLenIn: integer): string;
function ChangeFileExtX( var lFilename: string; lExt: string): string;
//function swap2i(SmallInt): Smallint;
function swap4r4i (s:single): longint; //swap and convert: endian-swap and then typecast 32-bit float as 32-bit integer
function conv4r4i (s:single): longint; //convert: typecast 32-bit float as 32-bit integer
function swap8r(s : double):double; //endian-swap 64-bit float
procedure pswap4i(var s : LongInt); //procedure to endian-swap 32-bit integer
procedure pswap4r ( var s:single);  //procedure to endian-swap 32-bit integer
function swap64r(s : double):double;
function specialdouble (d:double): boolean;
function RealToStr(lR: double {was extended}; lDec: integer): string;
function UpCaseExt(lFileName: string): string;//file.brik.gz->BRIK.GZ, file.nii.gz -> NII.GZ
function ExtGZ (lFilename: string): boolean;
procedure swap4(var s : LongInt);
procedure Xswap4r ( var s:single);
function Bool2Char (lBool: boolean): char;
function Char2Bool (lChar: char): boolean;
function Log(X, Base: single): single;
//procedure GZipBuffer(var FGzipFilename,FFileDestination: String;lxInBuffer: byteP;lInSize: Integer; lOverwritewarn: boolean);
//procedure GZipBuffer(var FGzipFilename,FFileDestination: String;lxInBuffer: byteP;lInSize: Integer);
{$IFNDEF FPC}
function DiskFreeEx (DriveStr: String): Integer;
{$ELSE}
function DiskFreeEx (DriveStr: String): Int64;
{$ENDIF}
procedure SortSingle(var lLo,lHi: single);
procedure SortInteger(var lLo,lHi: integer);
function IniInt(lIniFile: TIniFile; lIdent: string;  lDefault: integer): integer;
function IniBool(var lIniFile: TIniFile; lIdent: string;  lDefault: boolean): boolean;
procedure CopyFileEX (lInName,lOutName: string);
procedure CopyFileEXoverwrite (lInName,lOutName: string);
procedure fx (a: double); overload; //fx used to help debugging - reports number values
procedure fx (a,b: double); overload;
procedure fx (a,b,c: double); overload;
procedure fx (a,b,c,d: double); overload;
function Swap2(s: smallint): smallint;
//function DefaultsDir (lSubFolder: string): string;
function ChangeFilePostfixExt (lInName,lPostfix,lExt: string): string;
procedure SortCutout (var lCutout : TCutout); //ensure Lo < Hi
function freeRam: Int64;

function OKMsg(lMsg: string): boolean; //shows dialog with OK/Cancel returns true if user presses OK
function DirExists (lFolderName: String): boolean;
function FilenameParts (lInName: string; var lPath,lName,lExt: string): boolean;
function AddIndexToFilename (lInName: string; lIndex: integer): string;

procedure createArray64 (var ptr: pointer; var ra :Doublep0; Sz: integer); overload;
procedure createArray64 (var ptr: pointer; var ra :Doublep; Sz: integer); overload;
function GzExt(lFileName: string): boolean;
function ChangeFilePrefixExt (lInName,lPrefix,lExt: string): string;
function ChangeFilePrefix(lInName,lPrefix: string): string;
function makesmallint (b0,b1: byte): smallint;
function makesingle( b0,b1,b2,b3: byte): single;
procedure SortInt (var lMin,lMax: integer);
function Bound (lDefault,lMin,lMax: integer): integer;
function IsNiftiExt(lStr: string): boolean;
function IsExtNIFTIHdr(lStr: string): boolean;
function IsVOIExt(lStr: string): boolean;
//procedure ax(a,b,c,d,e,fx: double);
procedure EnsureDirEndsWithPathDelim (var lDir: string);
//function IsReadOnly(const FileName: string): Boolean;//I think this only works for existing files... not folders and new files
function DirWritePermission(Where: string): Boolean; //I think this is better than above
function ExtractDir (lFilepath: string): string;
{$IFDEF GUI}
function GetDirPrompt (lDefault: string): string;
{$ENDIF}
function Str2Int (lStr: string): integer;
function ResetDefaults : boolean;

implementation

function ResetDefaults : boolean;
const
     {$IFDEF LINUX}
     kKey = 'Right button';
     {$ELSE}
     kKey = 'Shift key';
     {$ENDIF}
var
   lKey: boolean;
begin
     result := false;
{$IFDEF GUI}
     {$IFDEF LINUX}
     lKey := (GetKeyState(VK_RBUTTON) And $80)<>0;
     {$ELSE}
     lKey := (ssShift in KeyDataToShiftState(vk_Shift));
     {$ENDIF}
     if not lKey then
        exit;
     {$IFDEF GUI}
    case MessageDlg(kKey+' down during launch: do you want to reset the default preferences?', mtConfirmation,
				[mbYes, mbNo], 0) of	{ produce the message dialog box }
				idYes: result := true;
        end; //case
     {$ENDIF}
{$ENDIF}
end;

function Str2Int (lStr: string): integer;
//robust stringtoint that strips out   any junk so that "Implementation Version Name=MR.VB15A" returns 15
// warning, strips out decimals, so 15.3 will return 153!
//warning also ignores minus sign so -5.21 will return 521!
var
   Len,P: integer;
   S: string;
begin
     result := 0;
     Len := length(lStr);
     if Len <1 then exit;
     S := '';
     for P := 1 to Len do
         if lStr[P] in ['-','0'..'9'] then
            S := S + lStr[P];
     if length(S) < 1 then exit;
     result := strtoint(S);
end;


{$IFDEF GUI}
function GetDirPrompt (lDefault: string): string;
// Old versions of Delphi have a clumsy SelectDirectory function, and locks the folder until you quit your application...
var
   lD: string;
begin
  lD := lDefault;
  if not DirExists(lD) then
     lD := UserDataFolder;
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
  result := lD;
end;
{$ENDIF} //GUI

function ExtractDir (lFilepath: string): string;
//if passed file \usr\temp\data.txt returns \usr\temp\
//if passed dir \usr\temp returns \usr\temp\
//note returned always includes pathdelim
var
   lName,lExt: string;
begin
  FilenameParts (lFilepath,Result,lName,lExt);
end;

function DirWritePermission(Where: string): Boolean;
{$IFDEF UNIX}
//Uses BaseUnix;
begin
  result := (fpAccess (ExtractDir(Where),W_OK)=0);
end;
{$ELSE}
Var
  i : Longint;
  lFilename: string;
Begin
  result := false;
  if length(Where) < 1 then
     exit;

  if DirExists (Where) then begin
     if Where[length(Where)] <>  PathDelim then
      lFilename := Where + pathdelim + 'dummy.dum'
     else
      lFilename := Where + 'dummy.dum';
  end else
      lFilename := Where;
  if fileexists (lFilename) then
     exit; //do not overwrite existing file
  i:=FileCreate (lFilename);
  if i=-1 then
    Halt(1);
  FileClose(i);
  DeleteFile(lFilename);
  result := true;
end;
{$ENDIF}
(*function IsReadOnly(const FileName: string): Boolean;
var
  sr: TSearchRec;
begin
  // Assume not read only
  Result := False;
  if FindFirst(FileName, faAnyFile, sr) = 0 then
  begin
    Result := (sr.Attr and faReadOnly) <> 0;
    FindClose(sr);
  end;
end; *)

procedure EnsureDirEndsWithPathDelim (var lDir: string);
begin
   if length(lDir) < 1 then
      exit;
   if lDir[length(lDir)] = pathdelim then
      exit;
   lDir := lDir + pathdelim;
end;


function AddIndexToFilename (lInName: string; lIndex: integer): string;
var lPath,lName,lExt: string;
begin
     result := '';
     if not FilenameParts (lInName, lPath,lName,lExt) then exit;
     result := lPath+lName+inttostr(lIndex)+lExt;
end;

function Bound (lDefault,lMin,lMax: integer): integer;
begin
    result := lDefault;
    if result < lMin then
       result := lMin;
    if result > lMax then
       result := lMax;
end;

function IsVOIExt(lStr: string): boolean;
var
   lExt: string;
begin
     result := false;
     lExt := UpCaseExt(lStr);
     if (lExt = '.VOI')  then
        result := true;
end;
function IsNiftiExt(lStr: string): boolean;
var
   lExt: string;
begin
     result := false;
     lExt := UpCaseExt(lStr);
     if (lExt = '.MGH') or (lExt = '.MGZ') then
        result := true;
     if (lExt = '.MHA') or (lExt = '.MHD') then
        result := true;
     if (lExt = '.HEAD') then
        result := true;
     if (lExt = '.NRRD') then
        result := true;

     if (lExt = '.NII') or (lExt = '.NII.GZ') then
        result := true;
     if (lExt = '.HDR') and (FSize(ChangeFileExt(lStr,'.img'))> 0) then
        result := true;
     if (lExt = '.IMG') and (FSize(ChangeFileExt(lStr,'.hdr'))> 0) then
        result := true;
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

function makesmallint (b0,b1: byte): smallint;
type
  swaptype = packed record
    case byte of
      0:(b0,b1 : byte); //word is 16 bit
      1:(s:smallint);
  end;
  swaptypep = ^swaptype;
var
  //inguy:swaptypep;
  outguy:swaptype;
begin
  //inguy := @s; //assign address of s to inguy
  outguy.b0 := b0;
  outguy.b1 := b1;
  result:=outguy.s;
end;//makesmallint


function makesingle( b0,b1,b2,b3: byte): single;
type
  swaptype = packed record
    case byte of
      0:(b0,b1,b2,b3 : byte); //word is 16 bit
      1:(long:single);
  end;
  swaptypep = ^swaptype;
var
  outguy:swaptype;
begin
  //inguy := @s; //assign address of s to inguy
  outguy.b0 := b0;
  outguy.b1 := b1;
  outguy.b2 := b2;
  outguy.b3 := b3;
  result:=outguy.long;
end;//swap4r4i

function ChangeFilePrefix(lInName,lPrefix: string): string;
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

function ChangeFilePrefixExt (lInName,lPrefix,lExt: string): string;
var
	lC,lLen,lPos: integer;
	lStr: string;
begin
	result := changefileext(lInName,lExt);
	lLen := length (result);
	if lLen < 1 then exit;
	lPos := lLen;
	while (lPos > 1) and (result[lPos] <> pathdelim) do
		dec(lPos);
	lStr := '';
	for lC := 1 to lPos do
		lStr := lStr+result[lC];
	lStr := lStr+lPrefix;
	if lPos < lLen then begin
            lC := lPos+1;
            while (lC <= lLen) and (result[lC] <> '.') do begin
                  lStr := lStr + result[lC];
                  inc(lC);
            end;
        end;
        lStr := lStr + lExt;
	result := lStr;
end;


function GzExt(lFileName: string): boolean;
var lExt: string;
begin
     lExt := UpCaseExt(lFilename);
     if (lExt = '.VOI') or (lExt = '.NII.GZ') or (lExt = '.GZ') then
        result := true
     else
         result := false;
end;

function FilenameParts (lInName: string; var lPath,lName,lExt: string): boolean;
var
   lLen,lPos,lExtPos,lPathPos: integer;
begin
    result := false;
    lPath := '';
    lName := '';
    lExt := '';
    lLen := length(lInName);
    if lLen < 1 then
       exit;
    if DirExists(lInName) then begin //we have been passed a folder, not a file
       if lInName[lLen] = PathDelim then
          lPath := lInName
       else
           lPath := lInName + pathdelim;
       exit;
    end;
    //next find final pathdelim
    lPathPos := lLen;
    while (lPathPos > 0) and (lInName[lPathPos] <> '\') and (lInName[lPathPos] <> '/') do
          dec(lPathPos);
    if (lInName[lPathPos] = '\') or (lInName[lPathPos] = '/') then begin
       for lPos := 1 to lPathPos do
           lPath := lPath + lInName[lPos];
    end;
    // else
    //    dec(lPathPos);
    inc(lPathPos);
    //next find first ext
    //lExtPos := 1;
    lExtPos := length(lPath);//July 2009 -- beware of '.' in foldername...
    while (lExtPos <= lLen) and (lInName[lExtPos] <> '.') do
          inc(lExtPos);
    if (lInName[lExtPos] = '.')  then begin
       for lPos := lExtPos to lLen do
           lExt := lExt + lInName[lPos];
    end;
    // else
    //    inc(lExtPos);
    dec(lExtPos);
    //next extract filename
    //fx(lPathPos,lExtPos);
    if (lPathPos <= lExtPos) then
       for lPos := lPathPos to lExtPos do
           lName := lName + lInName[lPos];
    result := true;

end;

procedure createArray64 (var ptr: pointer; var ra :Doublep0; Sz: integer); overload;
var i: integer;
begin
	 getmem(ptr,16+(sizeof(double)*Sz));
         {$IFDEF FPC}
         ra := align(ptr,16);
         {$ELSE}
	 ra := DoubleP0((integer(ptr) and $FFFFFFF0)+16);
         {$ENDIF}
        for i := (Sz-1) downto 0 do //initialise array
		 ra^[i] := 0;
end;

procedure createArray64 (var ptr: pointer; var ra :Doublep; Sz: integer); overload;
var i: integer;
begin
	 getmem(ptr,16+(sizeof(double)*Sz));
         {$IFDEF FPC}
         ra := align(ptr,16);
         {$ELSE}
	 ra := DoubleP((integer(ptr) and $FFFFFFF0)+16);
         {$ENDIF}
         for i := (Sz) downto 1 do //initialise array
		 ra^[i] := 0;
end;


function OKMsg(lMsg: string): boolean; //shows dialog with OK/Cancel returns true if user presses OK
begin
     result := false;
     {$IFDEF GUI}
	 case MessageDlg(lMsg, mtConfirmation,
		[mbYes, mbCancel], 0) of
		idCancel {mrCancel}: exit;
     end; //case
     {$ELSE}
	 case MsgDlg(lMsg, mtConfirmation,
		[mbYes, mbCancel], 0) of
		mrCancel: exit;
     end; //case
     {$ENDIF}
     result := true;
end;

(*function DirExists (lDir: String): boolean;
var lSearchRec: TSearchRec;
begin
  FindFirst(lDir, faAnyFile, lSearchRec);
    if (faDirectory and lSearchRec.attr) = faDirectory then
       DirExists := true
    else
        DirExists := false;
  FindClose(lSearchRec);{}
end;*)

{$IFNDEF GUI}
 {$IFNDEF FPC}
 //The FileCtrl unit is pretty bulky, and we only need this one call that it links from SysUtils
 function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
  {$ENDIF}
{$ENDIF}

function DirExists (lFolderName: string): boolean;
(*{$IFNDEF GUI}
var
	lSearchRec: TSearchRec;
begin
     result := false;
  if fileexists(lFoldername) then //File not folder
     exit;
  Filemode := 0; //readonly
	 if FindFirst(lFolderName, faDirectory, lSearchRec) = 0 then begin
	    result := true;
            FindClose(lSearchRec);
	 end else
	     result := false; //some files found
	 Filemode := 2;
{$ELSE}
*)
begin
         result :=  DirectoryExists(lFolderName);
//{$ENDIF}
end;

function freeRam: Int64;
{$IFDEF UNIX}
begin
     result := maxint;
end;
{$ELSE}
var
  memory:TMemoryStatus;

begin
  memory.dwLength:=sizeof(memory);
  GlobalMemoryStatus(memory);
  result := memory.dwavailPhys;
  //result := 1024;
end;
{$ENDIF}

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


function ChangeFilePostfixExt (lInName,lPostfix,lExt: string): string;
var
   lPath,lName,lExtIn: string;
begin
     FilenameParts (lInName, lPath,lName,lExtIn);
     result := lPath+lName+lPostFix+lExt;
     //showmessage(result);
end;

(*var
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
end;     *)

(*procedure ApplySaveDlgFilter (lSaveDlg: TSaveDialog);
var
   lLen,lPos,lPipes,lPipesReq: integer;
   lExt: string;
begin
     lPipesReq := (lSaveDlg.FilterIndex * 2)-1;
     if lPipesReq < 1 then exit;
     lLen := length(lSaveDlg.Filter);
     lPos := 1;
     lPipes := 0;
     while (lPos < lLen) and (lPipes < lPipesReq) do begin
           if lSaveDlg.Filter[lPos] = '|' then
              inc(lPipes);
           inc(lPos);
     end;
     if (lPos >= lLen) or (lPipes < lPipesReq) then
        exit;
     lExt := '';
     while (lPos <= lLen) and (lSaveDlg.Filter[lPos] <> '|') do begin
           if lSaveDlg.Filter[lPos] <> '*' then
              lExt := lExt + lSaveDlg.Filter[lPos];
           inc(lPos);
     end;
     if lExt <> '' then
        lSaveDlg.Filename := ChangeFileExt(lSaveDlg.Filename,lExt);
end;  *)

(*function DefaultsDir (lSubFolder: string): string;
//for Linux: DefaultsDir is ~/appname/SubFolder/, e.g. /home/username/mricron/subfolder/
//for Windows: DefaultsDir is in the location of the executable, e.g. c:\program files\mricron\subfolder\
//Note: Final character is pathdelim
var
   lBaseDir: string;
begin
     {$IFDEF Unix}
     lBaseDir := GetEnvironmentVariable ('HOME')+pathdelim+'.' +ParseFileName(ExtractFilename(paramstr(0) ) );
     if not DirectoryExists(lBaseDir) then begin
        {$I-}
        MkDir(lBaseDir);
        if IOResult <> 0 then begin
               showmessage('Unble to create new folder '+lBaseDir);
        end;
        {$I+}
     end;
     lBaseDir := lBaseDir+pathdelim;
     {$ELSE}
     lBaseDir := extractfiledir(paramstr(0))+pathdelim;
     {$ENDIF}
     //if not DirectoryExists(extractfiledir(lBaseDir)) then
     //mkDir(extractfiledir(lBaseDir));
     if lSubFolder <> '' then begin
         lBaseDir := lBaseDir + lSubFolder;
         if not DirectoryExists(lBaseDir) then begin
            {$I-}
            MkDir(lBaseDir);
            if IOResult <> 0 then begin
               showmessage('Unable to create new folder '+lBaseDir);
            end;
            {$I+}
         end;
         result := lBaseDir + pathdelim;
     end else
         result := lBaseDir;
end; *)

function Swap2(s : SmallInt): smallint;
type
  swaptype = packed record
    case byte of
      0:(Word1 : word); //word is 16 bit
      1:(Small1: SmallInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word1);
  result :=outguy.Small1;
end;

{$IFDEF GUI}
procedure ShowMsg(s: string);
begin
  showmessage(s);
end;
{$ENDIF}
procedure fx (a: double); overload; //fx used to help debugging - reports number values
begin
	ShowMsg(floattostr(a));
end;

procedure fx (a,b: double); overload; //fx used to help debugging - reports number values
begin
    ShowMsg(floattostr(a)+'x'+floattostr(b));
end;

procedure fx (a,b,c: double); overload; //fx used to help debugging - reports number values
begin
    ShowMsg(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c));
end;

procedure fx (a,b,c,d: double); overload; //fx used to help debugging - reports number values
begin
    ShowMsg(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c)+'x'+floattostr(d));
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
end; //proc IniInt

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


procedure SortInteger(var lLo,lHi: integer);
var lSwap: integer;
begin
	if lLo > lHi then begin
		lSwap := lLo;
		lLo := lHi;
		lHi := lSwap;
	end; //if Lo>Hi
end; //proc SortSingle

procedure SortSingle(var lLo,lHi: single);
var lSwap: single;
begin
	if lLo > lHi then begin
		lSwap := lLo;
		lLo := lHi;
		lHi := lSwap;
	end; //if Lo>Hi
end; //proc SortSingle

{$IFDEF FPC}
   {$IFDEF UNIX} //FPC and Unix
   function DiskFreeEx (DriveStr: String): Int64;
      var
      lOutDisk: Integer;
   begin

    lOutDisk :=  AddDisk(DriveStr);
    result := DiskFree(lOutDisk);
    if result < 0 then
       result := 9223372036854775807;
   end;
   {$ELSE} //FPC and Windows
   function DiskFreeEx (DriveStr: String): Int64;
   var
      lOutDisk: Integer;
   begin
     lOutDisk := ord(upcase(DriveStr[1]))+1-ord('A');
     if (lOutDisk >= 0) and (lOutDisk <= 26) then
        result := DiskFree(lOutDisk)
     else
         result := 0;
     //showmessage(DriveStr+'->*'+inttostr(lOutDisk)+'*  :'+inttostr(result));
     //showmessage(inttostr(DiskFree(0){current drive})+'  :'+inttostr(DiskFree(3) {C drive}));
   end;
   {$ENDIF}
{$ELSE} //Delphi Windows

function DiskFreeEx (DriveStr: String): Integer;
var
  lOutDisk: Integer;
  lDiskDir : string;
  lSize8:  Tinteger8;
begin
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
  {$ENDIF}

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
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  inguy^.Word1 := outguy.Word1;
  inguy^.Word2 := outguy.Word2;
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

function ExtGZ (lFilename: string): boolean;
var
   lI: integer;
         lExt : string;
begin
     lExt := ExtractFileExt(lFileName);
     if length(lExt) > 0 then
     for lI := 1 to length(lExt) do
         lExt[lI] := upcase(lExt[lI]);
     if lExt = '.GZ' then
        result := true
     else
         result := false;
end;

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

procedure pswap4r ( var s:single);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  inguy^.Word1 := outguy.Word1;
  inguy^.Word2 := outguy.Word2;
end; //proc Xswap4r

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
  conv4r4i:=inguy^.long;
end;

function swap4r4i (s:single): longint;
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

(*function ChangeFileExtX( var lFilename: string; lExt: string): string;
begin
    result := ChangeFileExt(lFilename,lExt);
end;      *)

function ChangeFileExtX(var  lFilename: string; lExt: string): string;// overload;
//sees .nii.gz as single extension
var
   lPath,lName,lOrigExt: string;
begin
     if FilenameParts (lFilename, lPath,lName,lOrigExt) then begin
        //showmessage('12222'+lPath +'**'+lName+'**'+lOrigExt);
        result := lPath+lName+lExt;
     end else begin
         //showmessage('z');
         result := ChangeFileExt(lFilename,lExt);
     end;
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
     if DirExists(lInFilename) then
        lFilePath := lInFilename
     else
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
  for i := 1 to (length(lFilewExt)-length(lExt)) do
      result := result + lFilewExt[i];
end;

(*function ParseFileName (lFilewExt:String): string;
var
   lLen,lInc: integer;
   lName: String;
begin
	lName := '';
     lLen := length(lFilewExt);
	lInc := lLen+1;
	 if  lLen > 0 then begin
	   repeat
			  dec(lInc);
		until (lFileWExt[lInc] = '.') or (lInc = 1);
		if (UpCaseExt(lFilewExt) = '.NII.GZ') and (lInc > 1) then
			repeat
			  dec(lInc);
			until (lFileWExt[lInc] = '.') or (lInc = 1);
	 end;
     if lInc > 1 then
        for lLen := 1 to (lInc - 1) do
            lName := lName + lFileWExt[lLen]
     else
         lName := lFilewExt; //no extension
        ParseFileName := lName;
end;   *)

Function {TMainForm.}FileExistsEX(Name: String): Boolean;
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

function FSize (lFName: String): Int64;
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
  inguy^.Word1 := outguy.Word1;
  inguy^.Word2 := outguy.Word2;
  inguy^.Word3 := outguy.Word3;
  inguy^.Word4 := outguy.Word4;
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
