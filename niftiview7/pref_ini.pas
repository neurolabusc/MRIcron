unit pref_ini;
//save and reads prefs from ini file
{$H+}

interface
uses
  Windows,
  IniFiles,SysUtils,define_types,graphics,Dialogs,Classes,ShellAPI,
  registry, Admin,graphicsMathLibrary,NIFTI_hdr, nifti_types;
type
 TBGImg =  record //Next: Background image information
   ScrnDim: array [1..3] of smallint;
   ScrnMM,ScrnOri: array [1..3] of single;
   XViewCenter,YViewCenter,ZViewCenter,BasePenThick: single;
   SliceView,SPMDefaultsStatsFmriT,SPMDefaultsStatsFmriT0,
   MaxDim,LicenseID,XBarGap,XBarThick,VOIUndoSlice,VOIUndoOrient,VOIUndoVolItems,
   RenderDepthBufferItems,VOIInvZoom,ZoomPct,BGTransPct,OverlayTransPct, PlanarRGB,
   Zoom,
   ImageSeparation,RenderDim,SigDig,{Apr07}TabletPressure,TabletErasePressure,LesionSmooth,LesionDilate,FontSize: integer;
   //ResizeBeforeRescale - 0=intensity rescale, then resize;  1= nearest neighbor resize, then rescale;1=trilinear resize, then rescale;
   //Show2ndSliceViews,
   FlipAx,FlipSag,SingleRow,ResliceOnLoad,OrthoReslice,UseReorientHdr,AutoFill,
   ThinPen,Mirror,OverlaySmooth,VOIchanged,VOImirrored,SaveDefaultIni,Prompt4DVolume,
   KnownAlignment,Resliced,
   Smooth2D,ShowDraw,XBar,Yoke: boolean;
   MinChar,MaxChar: array [1..3] of char; //May07
   StretchQuality : TStretchQuality;
   VOIClr,XBarClr: TColor;
   BackupLUT: array[0..255] of TRGBQuad;
   FSLDIR,FSLOUTPUTTYPE: kStr255;
   //LabelStr20 : Array[0..kHistoBins] of kstr20;
    LabelRA: TStrRA;
   InvMat: TMatrix;
   ReorientHdr: TNIFTIHdr;
   //Cutout: TCutout;
      VOIUndoVol: bytep;
   RenderDepthBuffer: SmallIntp;
 end; //BGImg Header Structure

//function zIniFile(lRead: boolean; lFilename: string; var lPrefs: TBGImg): boolean;
procedure ReadIniFile; //read init file values
procedure WriteIniFile;

	function IniName: string;

procedure SetBGImgDefaults (var lBGImg: TBGImg);

implementation
uses nifti_img_view {,nifti_img};

function IniName: string;
//only administrators can write to c:\program files -use AppDataFolder for non-Administrators
begin
     if isAdmin then
        result := changefileext(paramstr(0),'.ini')
     else
         result := AppDataFolder+pathdelim{+ParseFileName(extractfilename(paramstr(0)) ) +pathdelim}+changefileext(extractfilename(paramstr(0)),'.ini');
end;

procedure SetBGImgDefaults (var lBGImg: TBGImg);
begin
     with lBGImg do begin
          XBar := true;
          Yoke := false;
          OverlayTransPct := -1;
          BGTransPct := 0;
          ResliceOnLoad := false;
          OrthoReslice := true;
      Prompt4DVolume := true;
    FlipAx := false;
    FlipSag := false;
	  LicenseID := 0;
	  MaxDim := 512;
	  XBarGap := 7;
	  XBarThick := 3;
	  XBarClr := clBlue;
	  VOIClr := 255;//clRed;
	  VOIInvZoom := 1 shl 10; //1024 = 100%
    PlanarRGB := 2;
	  VOIUndoSlice := 0;
	  VOIUndoOrient := 0;
	  VOIChanged := false;
	  VOImirrored := false;
          TabletPressure := 70;
          TabletErasePressure := 30;
          LesionSmooth  := 3;//3mm smoothing
          LesionDilate := 8;
	  VOIUndoVolItems := 0;
	  RenderDepthBufferItems := 0;
    FontSize := 12;
	  SigDig := 5;
    ImageSeparation := 0;
    SliceView := 0;//multiple slices
      SPMDefaultsStatsFmriT := 16;
      SPMDefaultsStatsFmriT0  := 1;
	  SaveDefaultIni := true;
    SingleRow := false;
	  ThinPen := true;
          AutoFill := false;
          KnownAlignment := false;
        StretchQuality := sqHigh;
          //XMinChar := ' ';XMaxChar := ' ';YMinChar :=' ';ZMinChar:=' '; //May07
         OverlaySmooth := true;
     end; //with lBGImg
end; //set BGDef              ,

(*procedure SetIniMenus;
begin
  //XBarBtn.Down := gBGImg.XBarVisible;
  with ImgForm do begin
  YokeMenu.Checked := gYoke;
  Menu2DSmooth.checked := gBGImg.StretchQuality = sqHigh;
  Menu2DSmoothClick(nil);//set qualit
  end;
end;  *)


function registerfiletype(inft,inkey,desc,icon:string): boolean;
var myreg : treginifile;
	ct : integer;
	ft,key: string;
begin
	 result := true;
(*	 ft := inft;
	 key := inkey;
	 ct := pos('.',ft);
	 while ct > 0 do begin
		   delete(ft,ct,1);
		   ct := pos('.',ft);
	 end;
	 if (ft = '') or (ImgForm.Application.ExeName = '') then exit; //not a valid file-ext or ass. app
	 ft := '.'+ft;
	 myreg := treginifile.create('');
	 try
		myreg.rootkey := hkey_classes_root; // where all file-types are described
		if key = '' then key := copy(ft,2,maxint)+'_auto_file'; // if no key-name is given, create one
		myreg.writestring(ft,'',key); // set a pointer to the description-key
		myreg.writestring(key,'',desc); // write the description
		myreg.writestring(key+'\DefaultIcon','',icon); // write the def-icon if given
		myreg.writestring(key+'\shell\open\command','',Application.ExeName+' %1'); //association
	 except
		   result := false;
		   showmessage('Only administrators can change file associations. You are currently logged in as a restricted user.');
	 end;
	 myreg.free;              *)
end;


procedure SetDefaultPrefs (var lPrefs: TBGImg);
begin
  with lPrefs do begin
    //ProportionalStretch := true;
  end;//with lPrefs
end; //Proc SetDefaultPrefs

procedure IniInt(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: integer);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('INT',lIdent,IntToStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('INT',lIdent, '');
	if length(lStr) > 0 then
		lValue := StrToInt(lStr);
end; //IniInt

procedure IniBool(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: boolean);
//read or write a boolean value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('BOOL',lIdent,Bool2Char(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('BOOL',lIdent, '');
	if length(lStr) > 0 then
		lValue := Char2Bool(lStr[1]);
end; //IniBool

procedure IniStr255(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: kStr255);
//read or write a string value to the initialization file
begin
  if not lRead then begin
    lIniFile.WriteString('STR',lIdent,lValue);
    exit;
  end;
	lValue := lIniFile.ReadString('STR',lIdent, '');
end; //IniStr

procedure IniMRU(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: string);
//read or write a string value to the initialization file
begin
  if not lRead then begin
    lIniFile.WriteString('MRU',lIdent,lValue);
    exit;
  end;
	lValue := lIniFile.ReadString('MRU',lIdent, '');
end; //IniMRU

function TColorToHex( Color : TColor ) : string;
begin
  Result :=
    { red value }
    IntToHex( GetRValue( Color ), 2 ) +
    { green value }
    IntToHex( GetGValue( Color ), 2 ) +
    { blue value }
    IntToHex( GetBValue( Color ), 2 );
end;

function HexToTColor( sColor : string ) : TColor;
begin
  Result :=
    RGB(
      { get red value }
      StrToInt( '$'+Copy( sColor, 1, 2 ) ),
      { get green value }
      StrToInt( '$'+Copy( sColor, 3, 2 ) ),
      { get blue value }
      StrToInt( '$'+Copy( sColor, 5, 2 ) )
    );
end;

procedure IniColor(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: TColor);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('CLR',lIdent,TColorToHex(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('CLR',lIdent, '');
	if length(lStr) > 0 then
		lValue := HexToTColor(lStr);
end; //IniColor

function IniFile(lRead: boolean; lFilename: string; var lPrefs: TBGImg): boolean;
//Read or write initialization variables to disk
var
  lINc: integer;
  lIniFile: TIniFile;
begin
  result := false;
  if (lRead) and (not Fileexists(lFilename)) then
        exit;
  lIniFile := TIniFile.Create(lFilename);
  //STR
  IniStr255(lRead,lIniFile,'FSLDIR',lPrefs.FSLDIR);
  IniStr255(lRead,lIniFile,'FSLOUTPUTTYPE',lPrefs.FSLOUTPUTTYPE);
  //recent files
  IniMRU(lRead,lIniFile,'file0', gMRIcroOverlay[kBGOverlayNum].HdrFilename);
  for lInc := 1 to knMRU do
	  IniMRU(lRead,lIniFile,'file'+inttostr(lInc), gMRUstr[lINc]);
 // if lRead then
 //   showmessage( gMRUstr[1] +' : '+gMRUstr[2]);
  //Booleans
  with lPrefs do begin
  IniBool(lRead,lIniFile,  'AutoFill',AutoFill);
  IniBool(lRead,lIniFile,  'FlipAx',FlipAx);
  IniBool(lRead,lIniFile,  'FlipSag',FlipSag);
  IniBool(lRead,lIniFile,  'LRmirror',Mirror);
  IniBool(lRead,lIniFile,  'OverlaySmooth',OverlaySmooth);
  IniBool(lRead,lIniFile,  'Reslice',ResliceOnLoad);
  IniBool(lRead,lIniFile,  'ResliceOrtho',OrthoReslice);
  IniBool(lRead,lIniFile,  'ShowDraw',ShowDraw);
  IniBool(lRead,lIniFile,  'SingleRow',SingleRow);
  IniBool(lRead,lIniFile,  'Smooth2D',Smooth2D);
  IniBool(lRead,lIniFile,  'ThinPen',ThinPen);
  IniBool(lRead,lIniFile,  'XBar',XBar);
  IniBool(lRead,lIniFile,  'Yoke',Yoke);
  //Integers
  IniInt(lRead,lIniFile,  'BGTransPct',BGTransPct);
  IniInt(lRead,lIniFile,  'ImageSeparation',ImageSeparation);
  IniInt(lRead,lIniFile, 'LesionSmooth',LesionSmooth);
  IniInt(lRead,lIniFile, 'LesionDilate',LesionDilate);
  IniInt(lRead,lIniFile, 'LicenseID',LicenseID);
  IniInt(lRead,lIniFile,  'LUT',gMRIcroOverlay[kBGOverlayNum].LUTindex);//read
  IniInt(lRead,lIniFile, 'MaxDim',MaxDim);
  IniInt(lRead,lIniFile, 'MaxThreads',gnCPUThreads);
  IniInt(lRead,lIniFile, 'OverlayTransPct',OverlayTransPct);
  IniInt(lRead,lIniFile, 'SigDigits',SigDig);
  IniInt(lRead,lIniFile,  'SPMDefaultsStatsFmriT',SPMDefaultsStatsFmriT);
  IniInt(lRead,lIniFile, 'SPMDefaultsStatsFmriT0',SPMDefaultsStatsFmriT0);
  IniInt(lRead,lIniFile, 'TabletPressure',TabletPressure);
  IniInt(lRead,lIniFile, 'TabletErasePressure',TabletErasePressure);
  IniInt(lRead,lIniFile, 'FontSize',FontSize);

  IniColor(lRead,lIniFile,  'VOIClr',VOIClr);
  IniColor(lRead,lIniFile,  'XBarClr',XBarClr);
  IniInt(lRead,lIniFile,  'XBarGap',XBarGap);
  IniInt(lRead,lIniFile,  'XBarThick',XBarThick);
  IniInt(lRead,lIniFile,  'Zoom',Zoom);
  IniInt(lRead,lIniFile,  'PlanarRGB',PlanarRGB);
 end;//with
  lIniFile.Free;
(*
  YokeTimer.Enabled := gYoke;
  if (gBGImg.BGTransPct < 0) or (gBGImg.BGTransPct > 90) then
    gBGImg.BGTransPct := 20; //additive or transparent values can confuse users
  if (gBGImg.OverlayTransPct < 0) or (gBGImg.OverlayTransPct > 90) then
    gBGImg.OverlayTransPct := 20; //additive or transparent values can confuse users
*)

end;

procedure WriteIniFile;
begin
  if (DiskFreeEx(paramstr(0)) < 1) or (not gBGIMg.SaveDefaultIni) then
	exit;
  IniFile(false,IniName,gBGImg);
end;

procedure ReadIniFile;
var
  lFilename: string;
  lOK: boolean;
begin
	lFilename := IniName;
  IniFile(true,IniName,gBGImg);
end; //ReadIniFile

end.
