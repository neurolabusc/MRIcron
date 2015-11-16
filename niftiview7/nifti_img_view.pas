unit nifti_img_view;
interface

uses
   CommDlg,//FiltEdit,
   //DsgnIntf,
               draw_interpolate_slices,        ExtCtrls, Dialogs, Menus, Forms,
  StdCtrls, RXSpin, Controls, Buttons, PngSpeedButton, Classes,
  Windows, Messages, SysUtils,  Graphics,  Math,
   ToolWin, ComCtrls, NIFTI_hdr,GraphicsMathLibrary,
  ClipBrd,ShellAPI,define_types, nifti_hdr_view,nifti_img,IniFiles,
  ReadInt,registry,Distr,E_memmap,Tablet,gzio,batch,readfloat, Mask,
  imgutil,pref_ini,sliceinterpolate, fastsmooth, nifti_types;

type
  TImgForm = class(TForm)
    Recent1: TMenuItem;
	File1: TMenuItem;
	Open1: TMenuItem;
	Templates1: TMenuItem;
        CloseImages: TMenuItem;
        SaveasNIfTI1: TMenuItem;
        Saveaspicture1: TMenuItem;
        Exit1: TMenuItem;
	MainMenu1: TMainMenu;
	Edit1: TMenuItem;
	Copy1: TMenuItem;
	Help1: TMenuItem;
	About1: TMenuItem;
	ControlPanel: TPanel;
	Crosshair1: TMenuItem;
	Pen1: TMenuItem;
	Penautoclose1: TMenuItem;
	CircleSquare1: TMenuItem;
	MagPanel: TPanel;
	ProgressBar1: TProgressBar;
	StatusLabel: TLabel;
	LabelX: TLabel;
	LabelY: TLabel;
	LabelZ: TLabel;
	Controls1: TMenuItem;
    Multiple1: TMenuItem;
	Panel1: TPanel;
	MagnifyPanel: TPanel;
	SaveDialog1: TSaveDialog;
	ColorDialog1: TColorDialog;
	RefreshImagesTimer: TTimer;
	MagnifyMenuItem: TMenuItem;
	OverlayMenu: TMenuItem;
	OverlayOpen: TMenuItem;
	LayerMenu: TMenuItem;
	Noneopen1: TMenuItem;
	OverlaySmoothMenu: TMenuItem;
	CloseOverlayImg: TMenuItem;
	BGTransPctMenu: TMenuItem;
	OverlayTransPctMenu: TMenuItem;
	BGtrans0: TMenuItem;
	BGtrans20: TMenuItem;
	BGtrans40: TMenuItem;
	BGtrans50: TMenuItem;
	BGtrans60: TMenuItem;
	BGtrans80: TMenuItem;
	BGtrans100: TMenuItem;
	N0opaque1: TMenuItem;
	N201: TMenuItem;
	N401: TMenuItem;
	N501: TMenuItem;
	N601: TMenuItem;
	N801: TMenuItem;
	N100transparent1: TMenuItem;
	Layerrange1: TMenuItem;
	Noneopen2: TMenuItem;
	BGAdditive: TMenuItem;
	OverlayAdditive: TMenuItem;
	ShowRender: TMenuItem;
	DrawMenu: TMenuItem;
	OpenVOI: TMenuItem;
	SaveVOI: TMenuItem;
	CloseVOI: TMenuItem;
	VOIColor: TMenuItem;
	UndoImg: TImage;
	DrawImg: TImage;
	Undo1: TMenuItem;
	Paste1: TMenuItem;
	Applyintensityfiltertovolume1: TMenuItem;
	Quicksmooth1: TMenuItem;
	MaskimagewithVOI1: TMenuItem;
	VOImaskDelete: TMenuItem;
	VOImaskPreserve: TMenuItem;
	Circle1: TMenuItem;
	Overlaycomparisons1: TMenuItem;
	IntersectionmutualtoVOIandoverlays1: TMenuItem;
	UnionVOIoroverlays1: TMenuItem;
	MaskVOIbutnotoverlays1: TMenuItem;
	RescaleImagesTimer: TTimer;
	SmoothVOI1: TMenuItem;
	Circle2: TMenuItem;
	Beta1: TMenuItem;
	Chisquare1: TMenuItem;
	Convert1: TMenuItem;
	ROIVOI1: TMenuItem;
	Statistics1: TMenuItem;
	ShowMultislice: TMenuItem;
	DescriptiveMenuItem: TMenuItem;
	N1: TMenuItem;
	ToolPanel: TPanel;
	N2: TMenuItem;
	Display1: TMenuItem;
	N3: TMenuItem;
	FlipLRmenu: TMenuItem;
	N4: TMenuItem;
	Menu2DSmooth: TMenuItem;
	VOI2NII: TMenuItem;
	Nudge1: TMenuItem;
	Up1: TMenuItem;
	Left1: TMenuItem;
	LeftX1: TMenuItem;
	RightX1: TMenuItem;
	Posterior1: TMenuItem;
	Posterior2: TMenuItem;
	YokeTimer: TTimer;
    YokeMenu: TMenuItem;
    BrainExtraction1: TMenuItem;
    N5: TMenuItem;
    MNICoordinates1: TMenuItem;
    Histogram1: TMenuItem;
    N4DTraces1: TMenuItem;
    Sagittal1: TMenuItem;
    Axial1: TMenuItem;
    Coronal1: TMenuItem;
    N6: TMenuItem;
    CropEdges1: TMenuItem;
    HideVOI1: TMenuItem;
    HideROITimer: TTimer;
    Preferences1: TMenuItem;
    GenerateSPM5maskslesions1: TMenuItem;
    Header1: TMenuItem;
    RescaleMenu: TMenuItem;
    Brainmask1: TMenuItem;
    BatchROImean1: TMenuItem;
    NIIVOI1: TMenuItem;
    ZoomDrop: TComboBox;
    XViewEdit: TRxSpinEdit;
    YViewEdit: TRxSpinEdit;
    ZViewEdit: TRxSpinEdit;
    MirrorNII1: TMenuItem;
    LRFlip1: TMenuItem;
    Blackborders1: TMenuItem;
    Applyclusterthreshold1: TMenuItem;
    Batchprobmaps1: TMenuItem;
    ExportasRGBAnalyzeimage1: TMenuItem;
    HideROIBtn: TPngSpeedButton;
    XBarBtn: TPngSpeedButton;
    PenBtn: TPngSpeedButton;
    ClosedPenBtn: TPngSpeedButton;
    FillBtn: TPngSpeedButton;
    EllipseBtn: TPngSpeedButton;
    Fill3DBtn: TPngSpeedButton;
    Resliceimage1: TMenuItem;
    Batchclusterprobmaps1: TMenuItem;
    VOI2Text: TMenuItem;
    AdjustimagessoVOIintensityiszero1: TMenuItem;
    TriplePanel: TScrollBox;
    PGImageSag: TImage;
    PGImageCor: TImage;
    PGImageAx: TImage;
    LayerPanel: TPanel;
    LayerDrop: TComboBox;
    AutoContrastBtn: TPngSpeedButton;
    MinWindowEdit: TRxSpinEdit;
    MaxWindowEdit: TRxSpinEdit;
    LUTDrop: TComboBox;
    LutFromZeroBtn: TPngSpeedButton;
    ColorBarBtn: TPngSpeedButton;
    Display2: TMenuItem;
    RotateMenu: TMenuItem;
    Axial2: TMenuItem;
    Coronal2: TMenuItem;
    Sagittal2: TMenuItem;
    DilateVOIs1: TMenuItem;
    Left2: TMenuItem;
    Nudge2D1: TMenuItem;
    Left3: TMenuItem;
    Right1: TMenuItem;
    Anterior1: TMenuItem;
    Posterior3: TMenuItem;
    Interpolate1: TMenuItem;
    SaveSmooth1: TMenuItem;
    Landmarks1: TMenuItem;
    Extract1: TMenuItem;
    AcceptLandmark1: TMenuItem;
    Batchlandmarks1: TMenuItem;
    DrawHiddenMenu: TMenuItem;
    ShowDrawMenuItem: TMenuItem;
    HideDrawMenuItem: TMenuItem;
  //procedure SetIniMenus;
  procedure SaveOrCopyImages(lCopy: boolean);
  function ImgIntensityString(var lHdr: TMRIcroHdr; lVox: integer): string;
  function ImgIntensityStringXYZ(var lHdr: TMRIcroHdr; lX,lY,lZ: integer): string;
  	procedure UpdateColorSchemes;
	procedure UpdateTemplates;
	procedure UpdateMRU;
	procedure UpdateStatusLabel;
	procedure Exit1Click(Sender: TObject);
	procedure About1Click(Sender: TObject);
	procedure DisplayHdrClick(Sender: TObject);
	procedure Open1Click(Sender: TObject);
	procedure ToolSelectClick(Sender: TObject);
	procedure Copy1Click(Sender: TObject);
	procedure FormCreate(Sender: TObject);
	function OpenAndDisplayImg(var lFilename: string; lAdd2MRU: boolean): boolean;
	procedure OpenTemplateMRU(Sender: TObject);
	procedure XViewEditChange(Sender: TObject);
        procedure SetAutoFill;
        function ActiveLayer:integer;
	procedure FormClose(Sender: TObject; var Action: TCloseAction);
	procedure PGImageMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
	procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
	  MousePos: TPoint; var Handled: Boolean);
  procedure ShowDescriptive (lOverlayNum: integer; lShowFilename: boolean);

	procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
	  MousePos: TPoint; var Handled: Boolean);
	procedure PGImageMouseDown(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure PGImageMouseUp(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure LUTdropLoad(var lLayer: integer);
	procedure LUTdropSelect(Sender: TObject);
	procedure ZoomDropSelect(Sender: TObject);
	procedure ColorBarBtnMouseDown(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
//	procedure OptimizeViewMenuItemClick(Sender: TObject);
	procedure Saveaspicture1Click(Sender: TObject);
	procedure XBarBtnClick(Sender: TObject);
	procedure XBarBtnMouseDown(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure AutoContrastBtnClick(Sender: TObject);
	procedure RefreshImagesTimerTimer(Sender: TObject);
	procedure MinContrastWindowEditChange(Sender: TObject);
	//procedure ImgPanelClick(Sender: TObject);
	procedure MagnifyMenuItemClick(Sender: TObject);
	procedure CloseImagesClick(Sender: TObject);
	procedure UpdateLayerMenu;
  procedure LoadOverlay (lFilename: string);
  procedure LoadOverlayIncludingRGB (lFilename: string);
	procedure OverlayOpenCore (var lFilename: string; lOverlayNum: integer);
	procedure OverlayOpenClick(Sender: TObject);
	procedure CloseOverlayImgClick(Sender: TObject);
	procedure BGtrans100Click(Sender: TObject);
	procedure OverlayTransClick(Sender: TObject);
	procedure LayerDropSelect(Sender: TObject);
	procedure OverlaySmoothMenuClick(Sender: TObject);
	procedure MaxContrastWindowEditChange(Sender: TObject);
	procedure ShowRenderClick(Sender: TObject);
	procedure PenBtnClick(Sender: TObject);
	procedure OpenVOIClick(Sender: TObject);
	procedure OpenVOICore(var lFilename : string);
	procedure SaveVOIcore(lPromptFilename: boolean);
	procedure SaveVOIClick(Sender: TObject);
	procedure VOIColorClick(Sender: TObject);
	procedure CloseVOIClick(Sender: TObject);
	procedure SetDimension8(lInPGHt,lInPGWid:integer; lBuff: ByteP; lUndoOnly: boolean);
	procedure Undo1Click(Sender: TObject);
	procedure Paste1Click(Sender: TObject);
	procedure HideROIBtnMouseDown(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure HideROIBtnMouseUp(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure Applyintensityfiltertovolume1Click(Sender: TObject);
	procedure Quicksmooth1Click(Sender: TObject);
	procedure VOImaskClick(Sender: TObject);
	procedure SaveasNIfTI1Click(Sender: TObject);
	procedure ROIcomparisonClick(Sender: TObject);
	procedure RescaleImagesTimerTimer(Sender: TObject);
	procedure Fill3DBtnClick(Sender: TObject);
	procedure SmoothVOI1Click(Sender: TObject);
	procedure CreateOverlap(Sender: TObject);
	procedure Chisquare1Click(Sender: TObject);
	procedure ROIVOI1Click(Sender: TObject);
	procedure LUTinvertBtnClick(Sender: TObject);
	procedure LutFromZeroBtnClick(Sender: TObject);
	procedure ShowMultisliceClick(Sender: TObject);
	procedure DescriptiveMenuItemClick(Sender: TObject);
  procedure DrawInterpolateSlicesClick (Sender: TObject);
	procedure FormResize(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure FlipLRmenuClick(Sender: TObject);
	procedure Menu2DSmoothClick(Sender: TObject);
	procedure VALclick(Sender: TObject);
	procedure VOI2NIIClick(Sender: TObject);
	procedure TtoP1Click(Sender: TObject);
	procedure DesignVALClick(Sender: TObject);
	procedure Up1Click(Sender: TObject);
procedure SetShareMem (lXmm,lYmm,lZmm: single);
procedure CreateShareMem;
procedure CloseShareMem;
    procedure YokeTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure YokeMenuClick(Sender: TObject);
    procedure X1Click(Sender: TObject);
    procedure BrainExtraction1Click(Sender: TObject);
    procedure NZ1Click(Sender: TObject);
    procedure MNICoordinates1Click(Sender: TObject);
    procedure Histogram1Click(Sender: TObject);
    procedure N4DTraces1Click(Sender: TObject);
    procedure Sagittal1Click(Sender: TObject);
    procedure HideVOI1Click(Sender: TObject);
    procedure HideROITimerTimer(Sender: TObject);
    procedure CropEdges1Click(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    procedure GenerateSPM5maskslesions1Click(Sender: TObject);
    procedure Header1Click(Sender: TObject);
    procedure RescaleMenuClick(Sender: TObject);
    procedure Brainmask1Click(Sender: TObject);
    procedure BatchROImean1Click(Sender: TObject);
    procedure NIIVOI1Click(Sender: TObject);
    procedure MirrorNII1Click(Sender: TObject);
    procedure MagPanelClick(Sender: TObject);
    procedure FillBtnClick(Sender: TObject);
    procedure Blackborders1Click(Sender: TObject);
    procedure Applyclusterthreshold1Click(Sender: TObject);
    procedure Batchprobmaps1Click(Sender: TObject);
    procedure ExportasRGBAnalyzeimage1Click(Sender: TObject);
    procedure PGImageSagDblClick(Sender: TObject);
    procedure Resliceimage1Click(Sender: TObject);
    procedure Batchclusterprobmaps1Click(Sender: TObject);
    procedure SetSaveDlgFileExt;
    procedure VOI2TextClick(Sender: TObject);
    procedure AdjustimagessoVOIintensityiszero1Click(Sender: TObject);
    procedure ControlPanelDblClick(Sender: TObject);
    procedure ResizeControlPanel (lRows: integer);
    procedure DefaultControlPanel;
    procedure RotateMenuClick(Sender: TObject);
    procedure DilateVOIs1Click(Sender: TObject);
    procedure Nudge2D(Sender: TObject);
    procedure Interpolate1Click(Sender: TObject);
    procedure SaveSmooth1Click(Sender: TObject);
    procedure SaveDialog1TypeChange(Sender: TObject);
    procedure Landmarks1Click(Sender: TObject);
    procedure Extract1Click(Sender: TObject);
    procedure AcceptLandmark1Click(Sender: TObject);
    procedure Batchlandmarks1Click(Sender: TObject);
    procedure ToggleDrawMenu(Sender: TObject);

 private
	{ Private declarations }
    EMemMap : TEMemMap;
	procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
	procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
  public
	{ Public declarations }
public
   //procedure WMSysCommand (var Msg: TWMSysCommand) ; message WM_SYSCOMMAND;
  end;

const
 kYokeItems= 12;
 knMRU = 5;//max items in most recently used list
 knMaxOverlay = 22;
 kVOIOverlayNum = knMaxOverlay;
 kBGOverlayNum = 0;
 knAutoLUT = 7;
 kVOIFilter =   'Volume of interest (*.voi)|*.voi|MRIcro ROI (*.roi)|*.roi|'+kImgFilter;
var
 gYoke: boolean = false;
 //gReslice : boolean = true;
  ImgForm: TImgForm;
  gBGImg: TBGImg;
  gMRIcroOverlay: array [0..knMaxOverlay] of TMRIcroHdr;
  gColorSchemeDir,gTemplateDir: String;
  gMRUstr: array [0..knMRU] of String; //most recently used files
  gMouseDownX,gMouseDownY: integer;
  gSelectOrigin: TPoint;
  gSelectRect: TRect;
  //gMaxCPUThreads : integer = 8;
  gnCPUThreads : integer = 1;
  gOrigBGTransPct : integer= 50;
Type
	 SingleArr = Array[1..kYokeItems] Of Single;
	 SingleArrPtr = ^SingleArr;

implementation



uses reslice_fsl,render, ROIfilt,autoroi,smoothvoi, MultiSlice, Text, histoform, statclustertable,
  about, cropedges, bet, mni, graphx, prefs, admin,fill, cutout {, swrender},activex,clustering,
  rotation,Dilate, landmarks;

{$R *.DFM}
{$R WindowsXP.RES}
procedure ReadForm2Ini (var lBGImg: TBGImg);
begin
  lBGImg.ShowDraw := ImgForm.DrawMenu.Visible;
  lBGImg.Smooth2D := ImgForm.Menu2DSmooth.checked;
  lBGImg.XBar := ImgForm.XBarBtn.Down;
  lBGImg.OverlaySmooth := ImgForm.OverlaySmoothMenu.Checked;
  lBGImg.Yoke := gYoke;//ImgForm.YokeMenu.checked;
end;

procedure WriteIni2Form (lBGImg: TBGImg);
begin
     ImgForm.ToolPanel.Visible := lBGImg.ShowDraw;
     ImgForm.DrawMenu.Visible := lBGImg.ShowDraw;
     ImgForm.DrawHiddenMenu.Visible := not lBGImg.ShowDraw;
  ImgForm.Menu2DSmooth.checked := lBGImg.Smooth2D;
  ImgForm.XBarBtn.Down := lBGImg.XBar;
  gYoke := lBGImg.Yoke;
  ImgForm.YokeMenu.Checked := gYoke;
  ImgForm.OverlaySmoothMenu.Checked := lBGImg.OverlaySmooth;
  SetSubmenuWithTag(ImgForm.OverlayTransPctMenu, lBGImg.OverlayTransPct);
  SetSubmenuWithTag(ImgForm.BGTransPctMenu, lBGImg.BGTransPct);
end;

function GetLogicalCpuCount: Integer;
var
  SystemInfo: _SYSTEM_INFO;
begin
  GetSystemInfo(SystemInfo);
  Result := SystemInfo.dwNumberOfProcessors;
end;

procedure TImgForm.CloseShareMem;
begin
  //if not gYoke then exit;
  EMemMap.Free;
end;

procedure TImgForm.SetShareMem (lXmm,lYmm,lZmm: single);
begin
  if not gYoke then exit;
	EMemMap.EnterCriticalSection;
	Try
	  SingleArrPtr(EMemMap.MemMap)^[1]:=(lXmm);
	  SingleArrPtr(EMemMap.MemMap)^[2]:=(lYmm);
	  SingleArrPtr(EMemMap.MemMap)^[3]:=(lZmm);
	Finally
	  EMemMap.LeaveCriticalSection;
	end;
end;

procedure TImgForm.CreateShareMem;
var
 IArr    : SingleArrPtr;
 I: integer;
begin
  EMemMap:=TEMemMap.Create(Self);
  EMemMap.CreateMutex('MRICROMUTEX2');
  If NOT EMemMap.MapExisting('MRICROMAP2',SizeOf(SingleArr)) then begin
	New(IArr);
	For I:=1 To kYokeItems do
	  IArr^[I]:=0;
	Try
	  If NOT EMemMap.CreateMemMap('MRICROMAP2',SizeOf(SingleArr),IArr^) then
		EMemMap.RaiseMappingException;
	Finally
	  Dispose(IArr);
	end;
	 SetShareMem (0,0,0)
  end;
end;

procedure TImgForm.YokeTimerTimer(Sender: TObject);
var
lXmm,lYmm,lZmm: single;
lX,lY,lZ: integer;
begin
	if not gYoke then begin
		YokeTimer.Enabled := false;
		exit;
	end;
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then
		exit;
	EMemMap.EnterCriticalSection;
	Try
	  lXmm:=SingleArrPtr(EMemMap.MemMap)^[1];
	  lYmm:=SingleArrPtr(EMemMap.MemMap)^[2];
	  lZmm:=SingleArrPtr(EMemMap.MemMap)^[3];
	Finally
	  EMemMap.LeaveCriticalSection;
	end;
	MMToImgCoord(lX,lY,lZ,lXmm,lYmm,lZmm);
	if lX <> XViewEdit.value then XViewEdit.value := lX;
	if lY <> YViewEdit.value then YViewEdit.value := lY;
	if lZ <> ZViewEdit.value then ZViewEdit.value := lZ;
end;

procedure TImgForm.WMSysCommand;
begin
   if (Msg.CmdType = SC_MINIMIZE)  then
		Application.Minimize
	else
		DefaultHandler(Msg) ;
   if (Msg.CmdType = SC_MAXIMIZE) then RefreshImagesTimer.Enabled := true;
end;   

function SelectedImagePanel: TScrollBox;
begin
(*yui
	case SelectedImageNum of
		3: result := ImgForm.ImgPanel3;
		2: result := ImgForm.ImgPanel2;
		else result := ImgForm.ImgPanel1;
	end;*)
end;

function DrawToolSelected: boolean;
begin
	if ( ImgForm.PenBtn.Down) or ( ImgForm.ClosedPenBtn.Down) or (ImgForm.FillBtn.Down) or (ImgForm.EllipseBtn.Down) then
		result := true
	else
		result := false;
end;

Procedure TImgForm.SetAutoFill;
begin
     if gBGImg.AutoFill then begin
        if FillBtn.Down then
           ClosedPenBtn.Down := true;
        FillBtn.GroupIndex := 0
     end else
         FillBtn.GroupIndex :=ClosedPenBtn.GroupIndex;
end;


procedure TImgForm.UpdateColorSchemes;
var
	lSearchRec: TSearchRec;
begin
  LUTdrop.Items.Clear;
  LUTdrop.Items.Add('Grayscale');
  LUTdrop.Items.Add('Red');
  LUTdrop.Items.Add('Blue');
  LUTdrop.Items.Add('Green');
  LUTdrop.Items.Add('Violet [r+b]');
  LUTdrop.Items.Add('Yellow [r+g]');
  LUTdrop.Items.Add('Cyan [g+b]');
  if FindFirst(gColorSchemeDir+pathdelim+'*.lut', faAnyFile, lSearchRec) = 0 then
	 repeat
		  LUTdrop.Items.Add(ParseFileName(ExtractFileName(lSearchRec.Name)));
	 until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
end;//UpdateColorSchemes

procedure Add2MRU (var lNewFilename: string); //add new file to most-recent list
var
  lStr: string;
  lPos,lN : integer;
begin
  //first, increase position of all old MRUs
  lN := 0; //Number of MRU files
  for lPos := 1 to (knMRU) do begin//first, eliminate duplicates
	  lStr := gMRUstr[lPos];
	  if (lStr <> '') and (lStr <> lNewFileName) then begin
		 inc(lN);
		 gMRUstr[lN] := lStr;
	  end; //keep in MRU list
  end; //for each MRU
  //next, increment positions
  if lN >= knMRU then
	 lN := knMRU - 1;
  for lPos := lN downto 1 do
	  gMRUstr[lPos+1] := gMRUstr[lPos];
  if (lN+2) < (knMRU) then //+1 as we have added a file
	 for lPos := (lN+2) to knMRU do
	   gMRUstr[lPos] := '';
  gMRUstr[1] := lNewFilename;
  ImgForm.UpdateMRU;
  ImgForm.SaveDialog1.FileName := lNewFilename;
end;//Add2MRU

procedure TImgForm.UpdateMRU;//most-recently-used menu
var
  NewItem: TMenuItem;
  lPos,lN : integer;
begin
  While Recent1.Count > 0 do Recent1.Items[0].Free;
  lN := 0; //Number of MRU files
  for lPos := 1 to knMRU do begin//for each MRU
	  if gMRUstr[lPos] <> '' then begin
		   inc(lN);
		   NewItem := TMenuItem.Create(Self);
		   NewItem.Caption := ExtractFileName(gMRUstr[lPos]);//(ParseFileName(ExtractFileName(gMRUstr[lPos])));
		   NewItem.Tag := lN;
		   NewItem.RadioItem := true;
		   NewItem.Onclick := OpenTemplateMRU;
		   NewItem.ShortCut := ShortCut(Word('1')+ord(lN-1), [ssCtrl]);
		   Recent1.Add(NewItem);
		   gMRUstr[lN] := gMRUstr[lPos]; //eliminate empty items
	  end; //add new item
  end; //for each possible MRU
  if lN < knMRU then //empty unused strings
	 for lPos := (lN +1) to knMRU do
		gMRUstr[lPos] := '';
end;//UpdateMRU

procedure TImgForm.UpdateTemplates;
var
  NewItem: TMenuItem;
	lN : integer;
	lFName : String;
	lSearchRec: TSearchRec;
begin
  While Templates1.Count > 0 do Templates1.Items[0].Free;
  lN := 0;
  if FindFirst(gTemplateDir+pathdelim+'*.*', faAnyFile, lSearchRec) = 0 then begin
	 repeat
		  lFName := lSearchRec.Name;
		  if IsNIfTIHdrExt (lFName) then begin
		   inc(lN);
		   NewItem := TMenuItem.Create(Self);
		   NewItem.Caption :=ExtractFileName(lFName);//(ParseFileName(ExtractFileName(lFName)));
                   //showmessage(newItem.caption);
		   NewItem.Tag := 0;
		   NewItem.RadioItem := true;
		   NewItem.Onclick := OpenTemplateMRU;
		   NewItem.ShortCut := ShortCut(Word('1')+knMRU+ord(lN-1), [ssCtrl]);
		   Templates1.Add(NewItem);
		  end;
	 until (FindNext(lSearchRec) <> 0)
  end;
  FindClose(lSearchRec);
end;//UpdateTemplates

function txt(str: string): string;
//Delphi6 and later add special characters...
var
 lp,llen: integer;
begin
    result := '';
    llen := length(str);
    if llen < 1 then
        exit;
    for lp := 1 to llen do
        if str[lp] in  [' ','[',']','+','-','.','\','~','/', '0'..'9','a'..'z','A'..'Z']  then
                result := result + str[lp];
end;

function Findfile  ( lDir, lExt: string): string;
//findfile ('c:\myfolder','.nii')
var
 lLen: integer;
 lDirx,lExtx,lPath: string;
 lSearchRec: TSearchRec;
begin
  //1st : make sure pathdelim at end of folder name
  lDirx := lDir;
  lLen := length(lDir);
  if (lLen > 1) and (lDirx[lLen] <> pathdelim) then
    lDirx := lDirx + pathdelim;
  //2nd : make sure extension is '.hdr' not 'hdr'
  lExtx := lExt;
  lLen := length(lExt);
  if (lLen > 0) and (lExtx[1] <> '.') then
    lExtx := '.'+lExtx;
  lPath := lDirx+'*'+lExtx;
  Filemode := 0; //readonly
  result := '';
  if FindFirst(lPath, faAnyFile-faSysFile-faDirectory, lSearchRec) = 0 then
  	       result := lDirx + lSearchRec.Name;
  SysUtils.FindClose(lSearchRec);
	 Filemode := 2;
end;

procedure TImgForm.OpenTemplateMRU(Sender: TObject);//open template or MRU
//Templates have tag set to 0, Most-Recently-Used items have tag set to position in gMRUstr
var
	lFilename: string;
begin
        CloseImagesClick(nil);
	 if sender = nil then begin
		//autolaunch with last image, or last template image in list
		lFilename :=  gMRUstr[0];
		if (lFilename = '') or (not FileExistsEX(lFilename)) then begin
              lFilename := Findfile(GetCurrentDir,'.hdr');
              if lFilename = '' then
                lFilename := Findfile(GetCurrentDir,'.nii');
              if lFilename = '' then
                lFilename := Findfile(GetCurrentDir,'.nii.gz');
                //lStr := extractfiledir(ParamStr(0))+'\templates\xz.voi';
                //lStr := 'c:\zx\c1fxcbruce.nii';
                if fileexists(lFilename) then begin
                   OpenAndDisplayImg(lFilename,false);
                   exit;
                end;
		            if Templates1.Count > 0 then
			            Templates1.Items[Templates1.Count-1].click;
		          exit;
		end;
		  OpenAndDisplayImg(lFilename,false); //open but do not add templates to MRU
	 end else if (Sender as TMenuItem).tag = 0 then begin
		lFilename := gTemplateDir+pathdelim+txt((Sender as TMenuItem).caption) ;//+ '.hdr';
		  OpenAndDisplayImg(lFilename,false); //open but do not add templates to MRU
	 end else if (Sender as TMenuItem).tag <= knMRU then begin
		 lFilename := gMRUstr[(Sender as TMenuItem).tag];
		 OpenAndDisplayImg(lFilename,true);
	 end else
		 Showmessage('OpenTemplateMRU error.');
end;

function TImgForm.OpenAndDisplayImg(var lFilename: string; lAdd2MRU: boolean): boolean;
var
  lInName: string;
begin
         CloseImagesClick(nil);
	 Result := false;
  imgForm.Triplepanel.VertScrollBar.Position := 0;
  imgForm.Triplepanel.HorzScrollBar.Position := 0;
  lInName := lFilename;
	 if not HdrForm.OpenAndDisplayHdr(lFilename,gMRIcroOverlay[kBGOverlayNum]) then exit;
   if (ssCtrl in KeyDataToShiftState(vk_Shift)) and (gBGIMg.OrthoReslice) then begin
    gBGIMg.OrthoReslice := false;
    OpenImg(gBGImg,gMRIcroOverlay[0],true,false,false,false,false);
    gBGIMg.OrthoReslice := true;
   end else if (ssShift in KeyDataToShiftState(vk_Shift)) then begin
    if not OpenImg(gBGImg,gMRIcroOverlay[0],true,false,false,not gBGImg.ResliceOnLoad,false) then exit
   end else
    if not OpenImg(gBGImg,gMRIcroOverlay[0],true,false,false,gBGImg.ResliceOnLoad,false) then exit;
	 XViewEdit.MaxValue := gBGImg.ScrnDim[1];//gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[1];
	 YViewEdit.MaxValue := gBGImg.ScrnDim[2];//gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[2];
	 ZViewEdit.MaxValue :=gBGImg.ScrnDim[3];// gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[3];
	 //XViewEdit.Value := round(gBGImg.ScrnOri[1]);//gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[1] div 2;
	 //YViewEdit.Value := round(gBGImg.ScrnOri[2]);//gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[2]div 2;
	 //ZViewEdit.Value := round(gBGImg.ScrnOri[3]);//gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[3] div 2;
   XViewEdit.Value := Bound ( round(gBGImg.ScrnOri[1]),1,round(XViewEdit.MaxValue));
   YViewEdit.Value := Bound ( round(gBGImg.ScrnOri[2]),1,round(YViewEdit.MaxValue));
   ZViewEdit.Value := Bound ( round(gBGImg.ScrnOri[3]),1,round(ZViewEdit.MaxValue));
	 ImgForm.Caption := extractfilename(paramstr(0))+' - '+lInName;
   ImgForm.SaveDialog1.Filename := lInName;
	 StatusLabel.caption := 'opened: '+lInName;
	 Result := true;
	 if lAdd2MRU then
		   Add2MRU(lInName); //inname not filename, so if user selects im.nhdr that points to im.nhdr.gz
   if gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.datatype = kDT_RGB then begin   //RGB
       //we have loaded the first [red] plane - now load green and blue...
       LoadOverlay(lFilename);
       LoadOverlay(lFilename);
       //must use additive blending
       //gBGImg.BGTransPct := -1;
       //gBGImg.OverlayTransPct := -1;
       OverlayAdditive.Click;
       BGAdditive.Click;
   end;
   AnatForm.OpenAnat( ChangeFileextx(lFilename,'.anat')); 
end; //OpenAndDisplayImg

procedure TImgForm.WMDropFiles(var Msg: TWMDropFiles);  //implement drag and drop
var
  CFileName: array[0..MAX_PATH] of Char;
  lFilename: string;
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then
    begin
         lFilename := CFilename;
         OpenAndDisplayImg(lFilename,true);
         Msg.Result := 0;
    end;
  finally
	DragFinish(Msg.Drop);
  end;
end;

procedure TImgForm.Exit1Click(Sender: TObject);
begin
   ImgForm.Close;
end;

function XToStr(lR: extended; lDec: integer): string;
begin
	 result := FloatToStrF(lR, ffFixed,7,lDec);
end;

(*procedure RZ;
var
   lC: integer;
begin
     TextForm.Memo1.Lines.clear;
  for lC := 0 to knMaxOverlay do begin
         TextForm.Memo1.Lines.add(inttostr(lC)+'= '+inttostr (gMRIcroOverlay[lC].ScrnBufferItems)+'= '+inttostr (gMRIcroOverlay[lC].ImgBufferItems));
  end;
  TextForm.show;
end;

procedure KillBlueX;
var
  lVox,lVolVox: integer;
begin
     lVolVox := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]*gBGImg.ScrnDim[3];
     if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems <> lVolVox then exit;//r
     if gMRIcroOverlay[kBGOverlayNum+1].ScrnBufferItems <> lVolVox then exit;//g
     if gMRIcroOverlay[kBGOverlayNum+2].ScrnBufferItems <> lVolVox then exit;//b
     for lVox := 1 to lVolVox do begin
        if (gMRIcroOverlay[kBGOverlayNum+2].ScrnBuffer^[lVox] > (gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lVox]+24))
          and (gMRIcroOverlay[kBGOverlayNum+2].ScrnBuffer^[lVox] > (gMRIcroOverlay[kBGOverlayNum+1].ScrnBuffer^[lVox]+24)) then begin
            gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lVox] := 0;
            gMRIcroOverlay[kBGOverlayNum+1].ScrnBuffer^[lVox] := 0;
            gMRIcroOverlay[kBGOverlayNum+2].ScrnBuffer^[lVox] := 0;

          end;

     end;
end;

procedure KillBlue;
var
  lVox,lVolVox: integer;
begin
     lVolVox := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]*gBGImg.ScrnDim[3];
     if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems <> lVolVox then exit;//r
     if gMRIcroOverlay[kBGOverlayNum+1].ScrnBufferItems <> lVolVox then exit;//g
     if gMRIcroOverlay[kBGOverlayNum+2].ScrnBufferItems <> lVolVox then exit;//b
     if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems <> lVolVox then exit;//b
     for lVox := 1 to lVolVox do begin
        if (gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lVox] = 0) and (gMRIcroOverlay[kBGOverlayNum+2].ScrnBuffer^[lVox] > (gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lVox]))
          and (gMRIcroOverlay[kBGOverlayNum+2].ScrnBuffer^[lVox] > (gMRIcroOverlay[kBGOverlayNum+1].ScrnBuffer^[lVox])) then begin
            gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lVox] := 0;
            gMRIcroOverlay[kBGOverlayNum+1].ScrnBuffer^[lVox] := 0;
            gMRIcroOverlay[kBGOverlayNum+2].ScrnBuffer^[lVox] := 0;

          end;
     end;
end;   *)



procedure TImgForm.About1Click(Sender: TObject);
begin
//KillBlue; exit;
//     Graph4DForm.XL;
     AboutForm.ThreadLabel.Caption := 'Threads = '+inttostr(gnCPUThreads);
	 AboutForm.Showmodal;
end;

function TImgForm.ActiveLayer:integer;
begin
	result := ImgForm.LayerDrop.ItemIndex;
        if result < 0 then begin
            result := 0;
            ImgForm.LayerDrop.ItemIndex := 0;

        end;
end;

procedure TImgForm.DisplayHdrClick(Sender: TObject);
var
	lLayer:integer;
begin
	lLayer := ActiveLayer;
	HdrForm.SaveHdrDlg.Filename := gMRIcroOverlay[lLayer].HdrFilename;
	HdrForm.WriteHdrForm (gMRIcroOverlay[lLayer]);
	HdrForm.Show;
end;

procedure TImgForm.Open1Click(Sender: TObject);
var
	lFilename: string;
begin
	 CloseImagesClick(nil);
	 if not OpenDialogExecute(kImgFilter,'Select background image',false) then exit;
	 lFilename := HdrForm.OpenHdrDlg.Filename;
	 OpenAndDisplayImg(lFilename,True);
end;

procedure TImgForm.ToolSelectClick(Sender: TObject);
begin
	if (not ToolPanel.Visible) and ((Sender as TMenuItem).Tag > 0) then exit; //tools disabled
	 case (Sender as TMenuItem).Tag of
		  0: XBarBtn.Down := not XBarBtn.Down;
		  2: PenBtn.Down := true;
		  3: ClosedPenBtn.Down := true;
		  4: begin
                          if gBGImg.AutoFill then
                             FillBtnClick(nil)
                          else
                              FillBtn.Down := true;
                  end;
		  5: EllipseBtn.Down := true;
		  6: begin
			  PenBtn.Down := false;
			  ClosedPenBtn.Down := false;
			  FillBtn.Down := false;
			  EllipseBtn.Down := false;
		  end;
	 end; //case
	 RefreshImagesTimer.Enabled := true;
end;

function SelectedImage: TImage;
begin
 case SelectedImageNum of
	  3: result := ImgForm.PGImageCor;
	  2: result := ImgForm.PGImageSag;
	  else
		  result := ImgForm.PGImageAx;
 end;
end;

procedure TImgForm.SetDimension8(lInPGHt,lInPGWid:integer; lBuff: ByteP; lUndoOnly: boolean);
var
   PixMap: pointer;
   Bmp     : TBitmap;
   hBmp    : HBITMAP;
   BI      : PBitmapInfo;
   BIH     : TBitmapInfoHeader;
   ImagoDC : hDC;
   lPixmapInt,lBuffInt,
   I,lScanLineSz,lScanLineSz8: integer;
begin
	 BIH.biSize := Sizeof(BIH);
	 BIH.biWidth := lInPGwid;
	 BIH.biHeight := lInPGHt;
	 BIH.biPlanes := 1;
	 BIH.biBitCount := 8;//lBits;
	 BIH.biCompression := BI_RGB;
	 BIH.biSizeImage := 0;
	 BIH.biXPelsPerMeter := 0;
	 BIH.biYPelsPerMeter := 0;
	 BIH.biClrUsed := 0;
	 BIH.biClrImportant := 0;
	 BI := AllocMem(SizeOf(TBitmapInfoHeader) + 256*Sizeof(TRGBQuad));
	 BI^.bmiHeader := BIH;
	 for I:=0 to 255 do begin
			 BI^.bmiColors[I].rgbRed     := i;
			 BI^.bmiColors[I].rgbGreen    := i;
			 BI^.bmiColors[I].rgbBlue      := i;
			 BI^.bmiColors[I].rgbReserved := 0;
	 end;
	 I := kVOI8bit;
	 BI^.bmiColors[I].rgbRed     := (gBGImg.VOIClr ) and 255;;
	 BI^.bmiColors[I].rgbGreen    := (gBGImg.VOIClr shr 8) and 255;;
	 BI^.bmiColors[I].rgbBlue      := (gBGImg.VOIClr shr 16) and 255;;
	 Bmp        := TBitmap.Create;
	 Bmp.Height := lInPGHt;
	 Bmp.Width  := lInPGwid;
	 ImagoDC := GetDC(Self.Handle);
	 hBmp:= CreateDIBSection(imagodc,bi^,DIB_RGB_COLORS,pixmap,0,0);
	 lScanLineSz := lInPGwid;
	 if(lInPGwid mod 4) <> 0 then lScanLineSz8 := 4*((lInPGWid + 3)div 4)
	 else lScanLineSz8 := lInPGwid;
	 if lBuff <> nil then begin
		lPixmapInt  := Integer(pixmap);
		lBuffInt := Integer(lBuff);
		   For i:= (Bmp.Height-1)  downto 0 do
			   CopyMemory(Pointer(lPixmapInt+lScanLineSz8*(i)),
					 Pointer(lBuffInt+i*lScanLineSz),lScanLineSz);
	 end; //lBuff full
	 ReleaseDC(0,ImagoDC);
	 Bmp.Handle := hBmp;
	 UndoImg.Picture.Assign(Bmp);
	 UndoImg.width := Bmp.Width;
	 UndoImg.height := Bmp.Height;
	 if not lUndoOnly then begin
		DrawImg.Picture.Assign(Bmp);
		DrawImg.width := Bmp.Width;
		DrawImg.height := Bmp.Height;
	 end;
	 Bmp.Free;
	 FreeMem( BI);
end;

procedure WriteAxialVOI (lUndoOnly: boolean);
var lX,lY,lSliceOffset,lSliceSz,lSlicePos: integer;
	lInBuff: ByteP;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lSliceSz := lX*lY;
	if lSliceSz < 1 then exit;
	lSliceOffset := (ImgForm.ZViewEdit.asInteger-1)*lX*lY;
	gBGImg.VOIUndoSlice := ImgForm.ZViewEdit.asInteger;
	getmem(lInBuff,lSliceSz);
	for lSlicePos := 1 to lSliceSz do
		lInBuff[lSlicePos]  :=  gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lSliceOffset+lSlicePos];
	ImgForm.SetDimension8(lY,lX, lInBuff,lUndoOnly);
	freemem(lInBuff);
end;

procedure  WriteCorVOI (lUndoOnly: boolean);
var lX,lY,lZ,lYOffset,lZOffset,lXYSliceSz,lPixel,lZPos,lXPos: integer;
	lInBuff: ByteP;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
	lYOffset := (lX) * (round(ImgForm.YViewEdit.asInteger)-1);
	gBGImg.VOIUndoSlice := ImgForm.YViewEdit.asInteger;
	lXYSliceSz := (lX*lY);
	getmem(lInBuff,lZ*lX);
	lPixel := 0;
	for lZPos := 1 to lZ do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
	  for lXPos := 1 to lX do begin
		  inc(lPixel);
		lInBuff[lPixel] :=
			gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lZOffset+lYOffset+lXPos];
	  end; //for each Y
	end; //for each Z
	ImgForm.SetDimension8(lZ,lX, lInBuff,lUndoOnly);
	freemem(lInBuff);
end;

procedure WriteSagVOI (lUndoOnly: boolean);
var lX,lY,lZ,lXOffset,lYOffset,lZOffset,lXYSliceSz,lPixel,lZPos,lYPos: integer;
	lInBuff: ByteP;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
	lXYSliceSz := lX*lY;
	lXOffset := round(ImgForm.XViewEdit.Value);
	gBGImg.VOIUndoSlice := ImgForm.XViewEdit.asInteger;
	getmem(lInBuff,lZ*lY);
	lPixel := 0;
	for lZPos := 1 to lZ do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
	  lYOffset := 0;
	  for lYPos := 1 to lY do begin
		  inc(lPixel);
		  lInBuff[lPixel] := gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lZOffset+lYOffset+lXOffset];
		  lYOffset := lYOffset+ lX;
	  end; //for each Y
	end; //for each Z
	ImgForm.SetDimension8(lZ,lY, lInBuff, lUndoOnly);
	freemem(lInBuff);
end;

procedure WriteUndoVOI(lPanel: integer;lUndoOnly: boolean);
begin
	EnsureVOIOPen;
	case lPanel of
		3: WriteCorVOI(lUndoOnly);
		2: WriteSagVOI(lUndoOnly);
		else WriteAxialVOI(lUndoOnly);
	end;
	gBGImg.VOIchanged := true;
	if gBGImg.VOIUndoOrient = 4 then
		FreeUndoVol; //release 3D undo buffer when creating 2D buffer
	gBGImg.VOIUndoOrient := lPanel;
end;

procedure TImgForm.FormCreate(Sender: TObject);
var
   lInc: longint;
begin
gnCPUThreads := GetLogicalCpuCount;
randomize;
	 DecimalSeparator := '.';
	 gMouseDownX := -1;
	 ImgForm.Caption := extractfilename(paramstr(0));
     {$IFNDEF VER150}
	 ImgForm.DoubleBuffered := true;   //bug in D7 causes form transparency issues
     {$ENDIF}
	 for lInc := 0 to knMaxOverlay do begin
		InitImgMemory(gMRIcroOverlay[lInc]);
		NIFTIhdr_ClearHdr(gMRIcroOverlay[lInc]);
		//gMRIcroOverlay[lInc].ScrnBufferItems := 0;
		//gMRIcroOverlay[lInc].ImgBufferItems := 0;
                //gMRIcroOverlay[lInc].RenderBufferItems := 0;
		if lInc < knAutoLUT then
			gMRIcroOverlay[lInc].LUTindex := lInc
		else
			gMRIcroOverlay[lInc].LUTindex := lInc;//B&W
		LoadMonochromeLUT(gMRIcroOverlay[lInc].LUTindex,gBGImg,gMRIcroOverlay[lInc]);
	 end;
	 lInc:=maxint;
	 LoadMonochromeLUT(lInc,gBGImg,gMRIcroOverlay[kVOIOverlayNum]);
	 SetBGImgDefaults(gBGImg);
	 CloseImagesClick(nil);
	 gColorSchemeDir := extractfilepath(paramstr(0))+'lut';
	 DragAcceptFiles(Handle, True); //engage drag and drop
	 UpdateColorSchemes;
	 LUTdrop.ItemIndex := (0);
	 Zoomdrop.ItemIndex := (0);
	 LayerDrop.ItemIndex :=(0);
	 gTemplateDir := extractfilepath(paramstr(0))+'templates';
	 UpdateTemplates;
	 for lInc := 1 to knMRU do
		 gMRUstr[lInc] := '';
   if (ssShift in KeyDataToShiftState(vk_Shift))  then begin
    	case MessageDlg('Shift key down during launch: do you want to reset the default preferences?', mtConfirmation,
				[mbYes, mbNo], 0) of	{ produce the message dialog box }
				id_No: ReadIniFile;
	    end; //case
   end else
	  ReadIniFile;
   //SetIniMenus;
   WriteIni2Form(gBGImg);
   SetAutoFill;
   DefaultControlPanel;
	 UpdateMRU;
	 //SmoothBtnClick(nil);
	 OverlaySmoothMenuClick(nil);
	 LUTDropSelect(nil);
	 ZoomDropSelect(nil);
	 CreateShareMem;
	 if YokeMenu.checked then YokeTimer.enabled := true;
end;

function ImgIntensity(var lHdr: TMRIcroHdr; lPos: integer): single; overload;
var
	l16Buf : SmallIntP;
	l32Buf : SingleP;
begin

  result := 0;
  if (lPos > lHdr.ImgBufferItems) or (lPos < 1) then exit;
  if (lHdr.ImgBufferBPP  = 4) then begin
	l32Buf := SingleP(lHdr.ImgBuffer );
	result := l32Buf^[lPos];
  end else if (lHdr.ImgBufferBPP  = 2) then begin
	   l16Buf := SmallIntP(lHdr.ImgBuffer );
	result := l16Buf^[lPos];
  end else if lHdr.ImgBufferBPP  = 1 then
	 result := lHdr.ImgBuffer^[lPos]
  else begin
	showmessage('Unknown Image Buffer Bytes Per Pixel: '+inttostr(lHdr.ImgBufferBPP)+'  '+lHdr.HdrFileName);
	exit;
  end;
  result := Raw2ScaledIntensity (lHdr,result);
end;

function ImgIntensity(var lHdr: TMRIcroHdr; lX,lY,lZ: integer): single; overload;
var
	lPos: integer;
begin
  lPos := lX + ((lY-1)*gBGImg.ScrnDim[1])+((lZ-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
  ImgIntensity(lHdr,lPos);
end;

function TImgForm.ImgIntensityString(var lHdr: TMRIcroHdr; lVox: integer): string;
var
   lV: integer;
begin
  if (lVox > lHdr.ImgBufferItems) or (lVox < 1) then exit;
  if lHdr.UsesLabels  then begin
    lV := round(ImgIntensity(lHdr,lVox));
    if lV <= High(gBGImg.LabelRA) then
       result := gBGImg.LabelRA[lV];
    exit;
  end;
  if (not lHdr.UsesCustomPalette) or (lHdr.NIFTIhdr.datatype = kDT_RGB) then begin
     result := realtostr(ImgIntensity(lHdr,lVox),gBGImg.SigDig);
	exit;
  end;
end;

function TImgForm.ImgIntensityStringXYZ(var lHdr: TMRIcroHdr; lX,lY,lZ: integer): string; 
var
   lVox: integer;
begin
  lVox := lX + ((lY-1)*gBGImg.ScrnDim[1])+((lZ-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
  result := ImgIntensityString(lHdr,lVox);
end;

procedure TImgForm.UpdateStatusLabel;
var
	lX,lY,lZ,lOverlay,lLen: integer;
	lXmm,lYmm,lZmm: single;
	lIntenStr : string;
begin
	lX := XviewEdit.asInteger;
	lY := YViewEdit.asInteger;
	lZ := ZViewEdit.asInteger;
	ImgCoordToMM(lX,lY,lZ,lXmm,lYmm,lZmm);
	lIntenStr := '';
	for lOverlay := kBGOverlayNum to (kVOIOverlayNum-1) do
		if gMRIcroOverlay[lOverlay].ImgBufferItems > 0 then
			lIntenStr := lIntenStr + ImgIntensityStringXYZ(gMRIcroOverlay[lOverlay],lX,lY,lZ)+', ';
	lLen := length (lIntenstr);
	if lLen > 2 then
		lIntenStr[lLen-1] := ' ';
	StatusLabel.Caption := realtostr(lXmm,0)+'x'+realtostr(lYmm,0)+'x'+realtostr(lZmm,0)+'= '+lIntenStr;
	SetShareMem (lXmm,lYmm,lZmm);
end;

procedure TImgForm.XViewEditChange(Sender: TObject);
begin
	 gBGImg.XViewCenter := XviewEdit.value;
	 gBGImg.YViewCenter := YViewEdit.asInteger;
	 gBGImg.ZViewCenter := ZViewEdit.asInteger;
	 RefreshImagesTimer.Enabled := true;
	 //Oct2007:better only once per ImagesTimer:  UpdateStatusLabel;
end;

procedure TImgForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 ReadForm2Ini (gBGImg);
 WriteIniFile;
 CloseImagesClick(nil);
end;

procedure SelectPanel (lPanelNumber: integer);
begin
(* with ImgForm do begin
  case lPanelNumber of
		2: begin ImgPanel1.BorderStyle := bsNone; ImgPanel2.BorderStyle := bsSingle; ImgPanel3.BorderStyle := bsNone; end;
		3: begin ImgPanel1.BorderStyle := bsNone; ImgPanel2.BorderStyle := bsNone; ImgPanel3.BorderStyle := bsSingle; end;
		else begin ImgPanel1.BorderStyle := bsSingle; ImgPanel2.BorderStyle := bsNone; ImgPanel3.BorderStyle := bsNone; end;
   end;
  end; //with ImgForm *)
gSelectedImageNum := lPanelNumber;
end; //Proc SelectPanel

procedure ShowFocusRect(lInRect: TRect);
var LImage: TImage;
begin
	lImage := SelectedImage;
   lImage.Canvas.DrawFocusRect(lInRect);
end; //proc ShowFocusRect

procedure XYscrn2Img (lImage: TImage;lPanel,lXinRaw,lYinRaw: integer; var lXout,lYOut,lZOut: integer);
var
	lYin,lXin,lZoom : integer;
	lOffset: single;
begin
	 //amx - must match XYscrn2Img and DrawXBar - e.g. +0.5 for middle of zoomed slice
	  lZoom := ImageZoomPct(lImage);
	  if lZoom = 0 then lZoom := 100;
	 if lZoom > 100 then lOffset := 0.5 else
		lOffset := 0;
	 lXIn := lXinRaw + 1; //index from 0
	  lYin := lImage.Height-lYinRaw;
	  case lPanel of
		   2: begin
				lXOut := ImgForm.XViewEdit.asInteger;
        if gBGImg.FlipSag then
	        lXin := lImage.Width-lXinRaw;
				lYOut := round((lXin*100) / lZoom +lOffset);
				lZOut := round((lYin*100) / lZoom +lOffset);
		   end;
		   3: begin
				lXOut := round((lXin*100) / lZoom +lOffset);
				lYOut := ImgForm.YViewEdit.asInteger;
				lZOut := round((lYin*100) / lZoom +lOffset);

		   end;
		   else begin  //Axial
        if gBGImg.FlipAx then
	        lYin := lYinRaw;
				lXOut := round((lXin*100) / lZoom +lOffset);
				lYOut := round((lYin*100) / lZoom +lOffset);
				lZOut := ImgForm.ZViewEdit.asInteger;
		   end; //else
	  end;//case lPanel
	  //ImgForm.Caption := inttostr(lXOut)+' '+inttostr(lYOut)+'  '+Inttostr(lZOut);
end; //proc XYscrn2Img


function DX (lImage: TIMage;lPanel: integer; lInRect: TRect): single;
var lX,lY,lZ: integer;
 lXmm,lYmm,lZmm,lXmm2,lYmm2,lZmm2: single;
begin
     //XYscrn2Img (lImage: TIMage;lPanel,lXinRaw,lYinRaw: integer; var lXout,lYOut,lZOut: integer);
     XYscrn2Img (lImage,lPanel,lInRect.Left,lInRect.Top, lX,lY,lZ);
     ImgCoordToMM(lX,lY,lZ,lXmm,lYmm,lZmm);
     XYscrn2Img (lImage,lPanel,lInRect.Right,lInRect.Bottom, lX,lY,lZ);
     ImgCoordToMM(lX,lY,lZ,lXmm2,lYmm2,lZmm2);
     result :=  sqrt(sqr( lXmm-lXmm2 ) + sqr(lYmm-lYmm2)+ sqr(lZmm-lZmm2));
     (*lView := SelectedImageNum;
     case lView of
          3: begin //coronal
                  lY := 1;
                  lX := lInRect.Left;
                  lZ := lInRect.Top;
                  xx
                  ImgCoordToMM(lX,lY,lZ,lXmm,lYmm,lZmm);
                  lX := lInRect.Right;
                  lZ := lInRect.Bottom;
                  ImgCoordToMM(lX,lY,lZ,lXmm2,lYmm2,lZmm2);
                  result :=  sqrt(sqr( lXmm-lXmm2 ) + sqr(lZmm-lZmm2));
             end;
          2: begin //sagittal
                  lX := 1;
                  lY := lInRect.Left;
                  lZ := lInRect.Top;
                  ImgCoordToMM(lX,lY,lZ,lXmm,lYmm,lZmm);
                  lY := lInRect.Right;
                  lZ := lInRect.Bottom;
                  ImgCoordToMM(lX,lY,lZ,lXmm2,lYmm2,lZmm2);
                  result :=  sqrt(sqr( lYmm-lYmm2 ) + sqr(lZmm-lZmm2));
             end;
          else begin //axial
                  lZ := 1;
                  lX := lInRect.Left;
                  lY := lInRect.Top;
                  ImgCoordToMM(lX,lY,lZ,lXmm,lYmm,lZmm);
                  lX := lInRect.Right;
                  lY := lInRect.Bottom;
                  ImgCoordToMM(lX,lY,lZ,lXmm2,lYmm2,lZmm2);
                  result :=  sqrt(sqr( lXmm-lXmm2 ) + sqr(lYmm-lYmm2));
             end;
     end; //case  *)
end;//func DX

procedure ShowDXLine(lImage: TIMage;lPanel: integer; lInRect: TRect);
begin
     if lPanel <> gSelectOrigin.Y then
        exit; //only draw on source
     RefreshActiveImage;
     lImage.Canvas.Pen.Color:=gBGImg.XBarClr;
     lImage.Canvas.Pen.Width := gBGImg.XBarThick;
          lImage.Canvas.MoveTo(lInRect.Left,lInRect.Top);
     lImage.Canvas.LineTo(lInRect.Right,lInRect.Bottom);
     ImgForm.StatusLabel.Caption := realtostr(DX(lImage,lPanel, lInRect),gBGImg.SigDig);
end; //proc ShowFocusRect

procedure AdjustContrastRectangle (lImage: TImage; lRect: TRect);
var
 lXpos,lYPos,lXOut,lYOut,lZOut,lPanel,lLayer: integer;
 lMinInten,lMaxInten,lVal: single;
begin
   lPanel := SelectedImageNum;
   lLayer := ImgForm.ActiveLayer;
   XYscrn2Img (lImage,lPanel,gSelectRect.Left,gSelectRect.Top, lXout,lYOut,lZOut);
   lMinInten := ImgIntensity(gMRIcroOverlay[lLayer],lXout,lYOut,lZOut);
   lMaxInten := lMinInten;
   for lYpos := gSelectRect.Top to gSelectRect.Bottom do begin
	   for lXpos := gSelectRect.Left to gSelectRect.Right do begin
		   XYscrn2Img (lImage,lPanel,lXpos,lYPos, lXout,lYOut,lZOut);
			lVal:= ImgIntensity(gMRIcroOverlay[lLayer],lXout,lYOut,lZOut);
			if lVal < lMinInten then lMinInten := lVal;
			if lVal > lMaxInten then lMaxInten := lVal;
	   end; //for PGX each column
   end; //for PGY2 - each row
   ImgForm.StatusLabel.caption := 'Intensity range '+(RealToStr(lMinInten,4))+'..'+({x} RealToStr(lMaxInten,4));
   if lMinInten = lMaxInten then exit; //no range
   ImgForm.MinWindowEdit.value  := lMinInten;
   ImgForm.MaxWindowEdit.value := lMaxInten;
end;

procedure sortLTRB(var lXoutLow,lYOutLow,lXoutHi,lYOutHi: integer); //left<right, top<bottom
var lXin1,lYin1,lXin2,lYin2: integer;
begin
	lXin1 := lXoutLow;
	lYin1 := lYOutLow;
	lXin2 := lXoutHi;
	lYin2 := lYOutHi;
	if lXIn1 < lXin2 then begin
	   lXoutLow := lXIn1;
	   lXOutHi := lXIn2;
	end else begin
	   lXoutLow := lXIn2;
	   lXOutHi := lXIn1;
	end;
	if lYIn1 < lYin2 then begin
	   lYoutLow := lYIn1;
	   lYOutHi := lYIn2;
	end else begin
	   lYoutLow := lYIn2;
	   lYOutHi := lYIn1;
	end;
end; //sortLTRB

procedure DrawEllipse (lImage: TImage; lRect: TRect; lShift: TShiftState; lPanel: integer);
begin
   ScaleBMP2Draw(gBGImg.VOIInvZoom, lRect.Left,lRect.Top, lPanel,lImage);
   ScaleBMP2Draw(gBGImg.VOIInvZoom, lRect.Right,lRect.Bottom,lPanel, lImage);
   if (ssCtrl in lShift) then
		ImgForm.DrawImg.Canvas.Rectangle(lRect.Left,lRect.Top,lRect.Right,lRect.Bottom)
   else
		ImgForm.DrawImg.Canvas.Ellipse(lRect.Left,lRect.Top,lRect.Right,lRect.Bottom);
end; //DrawEllipse

function PenThick (lWidth: integer): integer;
//gives scaled pen thickness, e.g. 3-width pen at 200% = 6pixels
begin
     result := round(gBGImg.BasePenThick * lWidth);
     if result < 1 then
        result := 1;
end;

procedure TImgForm.PGImageMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
label 131;
var lZoom,lPanel,lX, lY,lXout,lYOut,lZOut,lX2, lY2: integer;
	lImage: TImage;
        lDX: boolean;
begin
   if gSelectOrigin.X = -666 then
      lDX := true
   else
       lDX := false;
   gSelectOrigin.X := -1;
   lX := X; lY := Y;
   lImage := Sender as TImage;
   if lImage.Name = 'PGImageCor' then lPanel := 3
   else if lImage.Name = 'PGImageSag' then lPanel := 2
   else lPanel := 1;
   SelectPanel(lPanel);
   gBGImg.VOIInvZoom := ComputeInvZoomShl10(lPanel,lImage);
if DrawToolSelected and  (ssAlt in Shift) then
	goto 131;
   if  DrawToolSelected then begin //paint tool
	   WriteUndoVOI(lPanel,false);
	   if (ssShift in Shift)  then begin //erase
			lImage.Canvas.Brush.Color:=clBlack;
			lImage.Canvas.Pen.Color := clBlack;
			DrawImg.Canvas.Brush.Color:=clBlack;
			DrawImg.Canvas.Pen.Color := clBlack;
	   end else begin
			lImage.Canvas.Brush.Color:=gBGImg.VOIClr;
			lImage.Canvas.Pen.Color := gBGImg.VOIClr;
			DrawImg.Canvas.Brush.Color:=gBGImg.VOIClr;
			DrawImg.Canvas.Pen.Color :=gBGImg.VOIClr;
	   end;
	   if (gBGImg.ThinPen)  then
			gBGImg.BasePenThick := 1
	   else begin //adjust pen thickness for zoom level
		 if gBGImg.ZoomPct < 100 then begin
			lZoom := ComputeZoomPct(lPanel,lImage);
			if lZoom = 100 then
				gBGImg.BasePenThick := 1
			else
			  gBGImg.BasePenThick :=  ComputeZoomPct(lPanel,lImage) / 100;//mar07 round((ComputeZoomPct(lPanel,lImage)+50) / 100);
		 end else if gBGImg.ZoomPct > 100 then
			gBGImg.BasePenThick := gBGImg.ZoomPct / 100//mar07 gBGImg.ZoomPct div 100
		 else
			gBGImg.BasePenThick := 1;
	   end; //if not thinpen
               //gBGImg.BasePenThick := lBasePenThick;
		if (ssCtrl in Shift) then begin
			lImage.Canvas.Pen.Width := PenThick(3);
			DrawImg.Canvas.Pen.Width := 3;
		end else begin
			lImage.Canvas.Pen.Width := PenThick(1);
			DrawImg.Canvas.Pen.Width := 1;

			//lImage.Canvas.Pen.Width := PenThick(20); //thick pen!
			//DrawImg.Canvas.Pen.Width := 20; //thick pen!
		end;
   end; //paint tool selected
   if  (FillBtn.Down) and (ssCtrl in Shift) then begin  //3D fill
			XYscrn2Img (lImage,lPanel,lX,lY, lXout,lYOut,lZOut);
				XViewEdit.asInteger := lXOut;
				YViewEdit.asInteger := lYOut;
				ZViewEdit.asInteger := lZOut;
			if (ssShift in Shift) then //erase
				ROICluster(gBGImg.ScrnDim[1], gBGImg.ScrnDim[2], gBGImg.ScrnDim[3],XViewEdit.asInteger,YViewEdit.asInteger,ZViewEdit.asInteger,true)
			else //draw
				ROICluster(gBGImg.ScrnDim[1], gBGImg.ScrnDim[2], gBGImg.ScrnDim[3],XViewEdit.asInteger,YViewEdit.asInteger,ZViewEdit.asInteger,false);
			exit;
   end; //end 3D fill
   if (not PenBtn.Down) and (not ClosedPenBtn.Down) and (not FillBtn.Down)  then begin
		if  (EllipseBtn.Down) or (ssRight in Shift) then begin
			lImage.Canvas.Brush.Color:=gBGImg.VOIClr;
			//lImage.Canvas.Pen.Color :=gBGImg.VOIClr;
			ScaleScrn2BMP(lX,lY, lImage);
			gSelectRect.Left := lX;
			gSelectRect.Top := lY;
			gSelectRect.Right := lX;
			gSelectRect.Bottom := lY;
			ShowFocusRect(gSelectRect);
			gSelectOrigin.X := gSelectRect.Left;
			gSelectOrigin.Y := gSelectRect.Top;
			exit;
		end;
131:
                //show distance line if shift
		if   (not lDX) and (ssShift in Shift) then begin
			ScaleScrn2BMP(lX,lY, lImage);
			gSelectRect.Left := lX;
			gSelectRect.Top := lY;
			gSelectOrigin.X := -666;//length line
                        gSelectOrigin.Y := lPanel;
		end;
		//next no paint tools selected - show position where click occurred
		XYscrn2Img (lImage,lPanel,lX,lY, lXout,lYOut,lZOut);
				XViewEdit.asInteger := lXOut;
				YViewEdit.asInteger := lYOut;
				ZViewEdit.asInteger := lZOut;
				//showmessage(floattostr(lXOut)+'x'+floattostr(lYOut)+'x'+floattostr(lZOut));
				//ImgCoordToMM(lXOut,lYOut,lZOut,lXmm,lYmm,lZmm);
				//showmessage(floattostr(lXmm)+'x'+floattostr(lYmm)+'x'+floattostr(lZmm));

				//showmessage(floattostr(gBGImg.ScrnOri[1])+'x'+floattostr(gBGImg.ScrnOri[2])+'x'+floattostr(gBGImg.ScrnOri[3]));
				//MMToImgCoord(lXOut,lYOut,lZOut,lXmm,lYmm,lZmm);
				//showmessage(floattostr(lXOut)+'x'+floattostr(lYOut)+'x'+floattostr(lZOut));

				//SetShareMem (lXmm,lYmm,lZmm);
		exit;
   end;
   //if (lX < 2) or (lY < 2) then exit;
   ScaleScrn2BMP(lX,lY, lImage);
   //lImage.Canvas.MoveTo(lX,lY);
   lX2 := X; lY2 := Y;
   ScaleBMP2Draw(gBGImg.VOIInvZoom, lX2,lY2,lPanel,lImage);
   if  (FillBtn.Down) or(ssRight in Shift) then begin
           gMouseDownX := -1; //5/5/2008 - Wacom Stylus patch
	   if (ssShift in Shift) then  //8/8/2008 - removed -1 from lX2 and lY2
			DrawImg.Canvas.FloodFill(lX2,lY2,gBGImg.VOIClr, fsSurface)
	   else
			DrawImg.Canvas.FloodFill(lX2,lY2,gBGImg.VOIClr, fsBorder);

	   exit;
   end;
   lImage.Canvas.MoveTo(lX,lY);
   DrawImg.Canvas.MoveTo(lX2,lY2);
   gMouseDownX := lX;
   gMouseDownY := lY;
end; //PGImageMouseDown

procedure TImgForm.PGImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var lX, lY,lPanel,lXout,lYOut,lZOut,lPressurePct,lPenThick: integer;
lErase: boolean;
	lImage: TImage;
begin
	 lImage := Sender as TImage;
	 lX := X; lY := Y;
	 ScaleScrn2BMP(lX,lY,lImage);
             if lImage.Name = 'PGImageCor' then lPanel := 3
             else if lImage.Name = 'PGImageSag' then lPanel := 2
             else lPanel := 1;

	 if  (gSelectOrigin.X = -666)  then begin
		gSelectRect.Right := lX;
		gSelectRect.Bottom := lY;
		//sortLTRB(gSelectRect.Left,gSelectRect.Top,gSelectRect.Right,gSelectRect.Bottom);
		ShowDXLine(lImage,lPanel,gSelectRect);
		exit;
	 end;

	 if  (gSelectOrigin.X > 0) then begin
		ShowFocusRect(gSelectRect);
		gSelectRect.Left := gSelectOrigin.X;
		gSelectRect.Top := gSelectOrigin.Y;
		gSelectRect.Right := lX;
		gSelectRect.Bottom := lY;
		sortLTRB(gSelectRect.Left,gSelectRect.Top,gSelectRect.Right,gSelectRect.Bottom);
		ShowFocusRect(gSelectRect);
		exit;
	 end;
         if (not DrawToolSelected) and ((ssLeft in Shift)) then begin
   if lImage.Name = 'PGImageCor' then lPanel := 3
   else if lImage.Name = 'PGImageSag' then lPanel := 2
   else lPanel := 1;
          		XYscrn2Img (lImage,lPanel,lX,lY, lXout,lYOut,lZOut);
                        if lXOut < 1 then lXOut := 1;//11/2007 : bound values
                        if lYOut < 1 then lYOut := 1;
                        if lZOut < 1 then lZOut := 1;

                        //if (lXOut < 1) or (lYOut < 1) or (lZOut < 1) then exit;
				XViewEdit.asInteger := lXOut;
				YViewEdit.asInteger := lYOut;
				ZViewEdit.asInteger := lZOut;
		exit;
         end;
	 if (not (ssLeft in Shift)) or (gMouseDownX < 0) then exit;
         if PenBtn.Down or ClosedPenBtn.Down then begin
            if {(gBGImg.Tablet) and} (TabletAvailable) then begin
               TabletState(lPressurePct,lErase);
               if lPressurePct > -1 then begin //using tablet
                  if (ssShift in Shift)  then
                     lErase := not lErase;
                  //next detemine pen thickness
                  lPenThick := 1;
	          if ((lErase) and (lPressurePct > (2*gBGImg.TabletErasePressure)))
                     or ((not lErase) and (lPressurePct > (2*gBGImg.TabletPressure))) then
			lPenThick := 5
                  else if ((lErase) and (lPressurePct > gBGImg.TabletErasePressure))
                       or ((not lErase) and (lPressurePct > gBGImg.TabletPressure)) then
			lPenThick := 3;
                  DrawImg.Canvas.Pen.Width := lPenThick;
                  lImage.Canvas.Pen.Width := PenThick(lPenThick);
                  if  (lErase) then begin //erase
			lImage.Canvas.Brush.Color:=clBlack;
			lImage.Canvas.Pen.Color := clBlack;
			DrawImg.Canvas.Brush.Color:=clBlack;
			DrawImg.Canvas.Pen.Color := clBlack;
                  end else begin
			lImage.Canvas.Brush.Color:=gBGImg.VOIClr;
			lImage.Canvas.Pen.Color := gBGImg.VOIClr;
			DrawImg.Canvas.Brush.Color:=gBGImg.VOIClr;
			DrawImg.Canvas.Pen.Color :=gBGImg.VOIClr;
                  end;
               end;//TabletPressure > -1 = tablet being used
            end; //Tablet
		   lImage.Canvas.LineTo(lX,lY);
		   lX := X; lY := Y;
		   ScaleBMP2Draw(gBGImg.VOIInvZoom, lX,lY,lPanel,lImage);
		   DrawImg.Canvas.LineTo(lX,lY);
	 end;
end; //PGImageMouseMove

(*procedure VOI2Scrn (var lImage: TImage; lXvoi,lYvoi: integer; var lVOIBuffer: ByteP);
//copy data from VOIbuffer to lImage
begin
	ImgForm.SetDimension8(lYvoi,lXvoi, lVOIBuffer,false);
end;    *)

procedure Scrn2VOI (var lImage: TImage; lXvoi,lYvoi: integer; var lVOIBuffer: ByteP);
const
 kSh = 10; //bits to shift
 kSHval = 1 shl kSh;
 kHalfSHval = kSHval div 2;
var
	lInc,lXpos,lYPos,lVOISliceSz,lScanLineSz8,lLineStart: integer;
	srcBmp : TBitmap;
	lInBuffer: Bytep;
begin
  srcBmp := lImage.Picture.Bitmap;//ImgForm.DrawImg.Picture.Bitmap;
  lVOISliceSz := lXvoi*lYvoi;
  linBuffer := srcBmp.ScanLine[lYvoi-1];
  GetMem (lVOIBuffer , lVOISliceSz);
  if(lXvoi mod 4) <> 0 then lScanLineSz8 := 4*((lXvoi + 3)div 4)
  else lScanLineSz8 := lXvoi;
  lLineStart := 0;
  lInc := 0;
  for lYPos := 1 to lYvoi do begin
	for lXPos := 1 to lXvoi do begin
		inc(lInc);
		lVOIBuffer[lInc] := linBuffer[lLineStart+lXPos];
	end;
	lLineStart := lLineStart + lScanLineSz8;
  end;
end; //Scrn2VOI

procedure ReadCorVOI (var lImage: TImage; lSlice: integer);
var lX,lY,lZ,lYOffset,lZOffset,lXYSliceSz,lPixel,lZPos,lXPos: integer;
	lInBuff: ByteP;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
	lYOffset := (lX) * (round(lSlice)-1);
	lXYSliceSz := (lX*lY);
	Scrn2VOI (lImage,lX,lZ, lInBuff);
	lPixel := 0;
	for lZPos := 1 to lZ do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
	  for lXPos := 1 to lX do begin
		  inc(lPixel);
		gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lZOffset+lYOffset+lXPos] :=lInBuff[lPixel];
	  end; //for each Y
	end; //for each Z
	freemem(lInBuff);
end;

procedure ReadSagVOI (var lImage: TImage;lSlice: integer);
var lX,lY,lZ,lXOffset,lYOffset,lZOffset,lXYSliceSz,lPixel,lZPos,lYPos: integer;
	lInBuff: ByteP;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
	lXYSliceSz := lX*lY;
	lXOffset := round(lSlice);
	Scrn2VOI (lImage,lY,lZ, lInBuff);
	lPixel := 0;
	for lZPos := 1 to lZ do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
	  lYOffset := 0;
	  for lYPos := 1 to lY do begin
		  inc(lPixel);
		gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lZOffset+lYOffset+lXOffset] := lInBuff[lPixel];
		  lYOffset := lYOffset+ lX;
	  end; //for each Y
	end; //for each Z
	freemem(lInBuff);
end;

procedure ReadAxialVOI (var lImage: TImage;lSlice: integer);
var lX,lY,lSliceOffset,lSliceSz: integer;
	lInBuff: ByteP;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lSliceSz := lX*lY;
	lSliceOffset := (lSlice-1)*lX*lY;
	Scrn2VOI (lImage,lX,lY, lInBuff);
	for lX := 1 to lSliceSz do
		gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lSliceOffset+lX] := lInBuff[lX];
	freemem(lInBuff);
end;

procedure ReadScrnVOI (lImage: TImage);
var
	lView: integer;
begin
	 if (gBGImg.VOIUndoSlice < 1) or (gBGImg.VOIUndoOrient < 1) or (gBGImg.VOIUndoOrient > 3) then exit;
	if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems  < 1) or (lImage.Picture.Bitmap.Width < 1) or (lImage.Picture.Bitmap.Height < 1) then
		exit;
	EnsureVOIOpen;
	lView := SelectedImageNum;
	case lView of
		3: ReadCorVOI(ImgForm.DrawImg,ImgForm.YViewEdit.asInteger);
		2: ReadSagVOI(ImgForm.DrawImg,ImgForm.XViewEdit.asInteger);
		1: ReadAxialVOI(ImgForm.DrawImg,ImgForm.ZViewEdit.asInteger);
	end;
	ImgForm.RefreshImagesTimer.Enabled := true;
end;


procedure TImgForm.PGImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var lPanel,lX, lY: integer;
lImage: TImage;
begin
	 lImage := Sender as TImage;
      lPanel := SelectedImageNum;

	 lX := X; lY := Y;
	 ScaleScrn2BMP(lX,lY,lImage);
if (gSelectOrigin.X > 0)  then begin
	 sortLTRB(gSelectRect.Left,gSelectRect.Top,gSelectRect.Right,gSelectRect.Bottom);
	 ShowFocusRect(gSelectRect);
	 gSelectOrigin.X := -1;
	 if (EllipseBtn.Down) then
		DrawEllipse(lImage,gSelectRect,Shift,lPanel)
	 else begin
		AdjustContrastRectangle(lImage,gSelectRect);
                gMouseDownX := -1;
		exit;
	 end;
end;
	 if  ((PenBtn.Down) or (ClosedPenBtn.Down)) and (gMouseDownX > 0) then begin
		ScaleBMP2Draw(gBGImg.VOIInvZoom, gMouseDownX,gMouseDownY,lPanel,lImage);
		//next: draw single pxiel if user clicks on image without moving the mouse
		DrawImg.Canvas.Pixels[gMouseDownX,gMouseDownY] := DrawImg.Canvas.Pen.Color;
	       	if  (ClosedPenBtn.Down) then
	       		DrawImg.Canvas.LineTo(gMouseDownX,gMouseDownY);
	 end;
	 (*if  (ClosedPenBtn.Down)and (gMouseDownX > 0) then begin
		ScaleBMP2Draw(gBGImg.VOIInvZoom, gMouseDownX,gMouseDownY);
		DrawImg.Canvas.LineTo(gMouseDownX,gMouseDownY);
	 end;*)
	 gMouseDownX := -1; //disable draws
	 if DrawToolSelected  and (not (ssAlt in Shift)) then
	 ReadScrnVOI (lImage);
end; //PGImageMouseUp

procedure DecViewEdit(var lEdit: TRXSpinEdit);
begin
    if lEdit.Value > 1 then
       lEdit.value := lEdit.value -1
	else
        lEdit.Value := lEdit.MaxValue;
end; //DecViewEdit

procedure IncViewEdit(var lEdit: TRXSpinEdit);
begin
    if lEdit.Value < lEdit.MaxValue then
       lEdit.value := lEdit.value +1
    else
		lEdit.Value := 1;
end; //IncViewEdit
var
  gX: integer = 0;

procedure TImgForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  inc(gX);
  if (gX mod 3) <> 0 then
    exit;
  Case SelectedImageNum of
	  3: DecViewEdit(YViewEdit);
      2: DecViewEdit(XViewEdit);
      else DecViewEdit(ZViewEdit);
  end;
end;

procedure TImgForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  //if ZoomDrop.Focused then
   //   XViewEdit.SetFocus;
  inc(gX);
  if (gX mod 3) <> 0 then
    exit;
  Case SelectedImageNum of
	  3: IncViewEdit(YViewEdit);
	  2: IncViewEdit(XViewEdit);
	  else IncViewEdit(ZViewEdit);
  end;
end;

procedure TImgForm.ZoomDropSelect(Sender: TObject);
begin
	 gBGImg.ZoomPct := (ZoomDrop.ItemIndex-1)*100;
	 RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.ColorBarBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var lLTRB,lLayer: integer;
  lImage: TImage;
begin
	 if (ssAlt in Shift) then begin
		//lImage := SelectedImage;
		lLayer := ActiveLayer;
		DrawHistogram(gMRIcroOverlay[lLayer],HistogramForm.HistoImage{lImage});
		HistogramForm.Caption := 'Histogram: '+extractfilename(gMRIcroOverlay[lLayer].HdrFileName);
		HistogramForm.show;
                if (ssCtrl in Shift) then
	           TextReportHisto(gMRIcroOverlay[lLayer]);;
		exit;
	 end;
	 lLTRB := 1;
     if  (ssRight in Shift) then
	   lLTRB := lLTRB + 1;
     if (ssCtrl in Shift) then
	   lLTRB := lLTRB + 2;
	 lImage := SelectedImage;
	 intenBar(lImage,gMRIcroOverlay[ActiveLayer],lLTRB,0,0);
end;


(*procedure SaveAllAx;
var
  lZ,lSlices: integer;
begin
  lSlices := round(ImgForm.ZViewEdit.maxvalue);
  if lSlices < 1 then exit;
  for lZ := 1 to lSlices do begin
    ImgForm.ZViewEdit.value := lZ;
    ImgForm.RefreshImagesTimer.Enabled := false;
	  RefreshImages;
    SaveImgAsPNGBMPCore (ImgForm.PGImageAx,'c:\temp\'+padstr(lZ,3)+'.png');
  end;
end;(**)



procedure TImgForm.XBarBtnClick(Sender: TObject);
begin
	 RefreshImagesTimer.Enabled := true;

end;

procedure RepositionOrigin;
begin
     gBGImg.ScrnOri[1] := ImgForm.XViewEdit.asInteger;
     gBGImg.ScrnOri[2] := ImgForm.YViewEdit.asInteger;
     gBGImg.ScrnOri[3] := ImgForm.ZViewEdit.asInteger;
     ImgForm.SetShareMem (0,0,0);
end;


procedure TImgForm.XBarBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  label 555;
begin
	 if not (ssRight in shift) then exit;
	 if (ssAlt in Shift) and (ssCtrl in Shift) then begin
		inc(gBGImg.FontSize,2);
		if gBGImg.FontSize > 24 then
			gBGImg.FontSize := 8;
    goto 555;
	 end;
   if (ssShift in Shift) then begin
             RepositionOrigin;
		    goto 555;
   end;



	 if (ssAlt in Shift) then begin
		inc(gBGImg.XBarThick,2);
		if gBGImg.XBarThick > 10 then
			gBGImg.XBarThick := 1;

		goto 555;
	 end;
	 if (ssCtrl in Shift) then begin
		ColorDialog1.Color := gBGImg.XBarClr;
		if not ColorDialog1.Execute then exit;
		gBGImg.XBarClr := ColorDialog1.Color;
		goto 555;
	 end;
	 inc(gBGImg.XBarGap);
	 if gBGImg.XBarGap > 10 then
		gBGImg.XBarGap := 0;
555:
RefreshImagesTimer.Enabled := true;
    if MultiSliceForm.Visible then
    MultiSliceForm.CreateMultiSlice;
end; //XBarBtnMouseDown

procedure TImgForm.RefreshImagesTimerTimer(Sender: TObject);
begin
	 RefreshImagesTimer.Enabled := false;
	 RefreshImages;
   UpdateStatusLabel;

end;

(*procedure TImgForm.ImgPanelClick(Sender: TObject);
begin
	 SelectPanel((Sender as TScrollBox).tag);
end; *)

procedure TImgForm.MagnifyMenuItemClick(Sender: TObject);
begin
(*  if MagnifyPanel.Height < 20 then //Height constrained by Y
	 MagnifyPanel.Height := 128
  else
	  MagnifyPanel.Height := MagnifyPanel.Constraints.MinHeight;*)
end;

procedure TImgForm.CloseImagesClick(Sender: TObject);
var
	lC: integer;
begin
        //fx(gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems);//FreeImgMemory(gMRIcroOverlay[kBGOverlayNum]);
	CloseVOIClick(nil);
	FreeUndoVol;
        //qaz

	for lC := 0 to knMaxOverlay do //background, all overlays, VOI
		FreeImgMemory(gMRIcroOverlay[lC]);
	gBGImg.VOIUndoSlice := 0;
	//next- set layers menu
    LayerDrop.ItemIndex := (0);
	LayerDrop.Items.Clear;
	LayerDrop.Items.Add('Background');
	LayerDropSelect(nil);
end;


procedure TImgForm.OverlayOpenCore (var lFilename: string; lOverlayNum: integer);
begin
         if (lOverLayNum <= kBGOverlayNum) or (lOverlayNum > knMaxOverlay) then exit;
	 if not HdrForm.OpenAndDisplayHdr(lFilename,gMRIcroOverlay[lOverlayNum]) then exit;
         (*if ReorientForm.visible then
            ReorientForm.ApplyTransform(gMRIcroOverlay[lOverlayNum]);     *)
         if (ssShift in KeyDataToShiftState(vk_Shift)) then begin
             if not OpenImg(gBGImg,gMRIcroOverlay[lOverlayNum],false,false,false,not gBGImg.ResliceOnLoad,false) then exit;
         end else
	     if not OpenImg(gBGImg,gMRIcroOverlay[lOverlayNum],false,false,false,gBGImg.ResliceOnLoad,false) then exit;
	 ImgForm.UpdateLayerMenu;
	 ImgForm.RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.LoadOverlay (lFilename: string);
var
lOverlay,lC: integer;
begin
    	 lOverlay := 0;
	 for lC := 1 to (knMaxOverlay-1) do //-1: save final overlay for VOI
		  if (lOverlay = 0) and (gMRIcroOverlay[lC].ImgBufferItems = 0) then
			lOverlay := lC;
	 if lOverlay = 0 then begin
		showmessage('Unable to add an overlay. You have loaded the maximum number of overlays.');
		exit;
	 end;
	 OverlayOpenCore ( lFilename, lOverlay);
end;


procedure  TImgForm.LoadOverlayIncludingRGB (lFilename: string);
var
lOverlay,lC: integer;
begin
    	 lOverlay := 0;
	 for lC := 1 to (knMaxOverlay-1) do //-1: save final overlay for VOI
		  if (lOverlay = 0) and (gMRIcroOverlay[lC].ImgBufferItems = 0) then
			lOverlay := lC;
	 if lOverlay = 0 then begin
		showmessage('Unable to add an overlay. You have loaded the maximum number of overlays.');
		exit;
	 end;
	 OverlayOpenCore ( lFilename, lOverlay);
   if (gMRIcroOverlay[lOverlay].NIFTIhdr.datatype = kDT_RGB) then begin
    OverlayOpenCore ( lFilename, lOverlay+1);
    OverlayOpenCore ( lFilename, lOverlay+2);
        OverlayAdditive.click;
   end;
end;

procedure TImgForm.OverlayOpenClick(Sender: TObject);
var
	lFilename: string;
  lINc: integer;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image (''File''/''Open'') before adding an overlay.');
		exit;
	end;
	if not OpenDialogExecute(kImgFilter,'Select overlay image[s]',true) then exit;
  if HdrForm.OpenHdrDlg.Files.Count < 1 then
    exit;
  for lInc := 1 to HdrForm.OpenHdrDlg.Files.Count do begin //vcx
    lFilename := HdrForm.OpenHdrDlg.Files[lInc-1];
    LoadOverlayIncludingRGB(lFilename);

    (*   LoadOverlay(lFilename);
       LoadOverlay(lFilename);*)
    (*if gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.datatype = kDT_RGB then begin   //RGB
       //we have loaded the first [red] plane - now load green and blue...
       LoadOverlay(lFilename);
       LoadOverlay(lFilename);
    //garbo xxx
       xxx
       *)
    LayerDrop.ItemIndex := (LayerDrop.Items.Count-1);
	  LayerDropSelect(nil);
  end;
(*	if not OpenDialogExecute(kImgFilter,'Select overlay image',false) then exit;
         lFilename := HdrForm.OpenHdrDlg.Filename;
        LoadOverlay(lFilename);
 	LayerDrop.ItemIndex := (LayerDrop.Items.Count-1);
	  LayerDropSelect(nil);
  *)
end; //OverlayOpenClick

procedure TImgForm.BGtrans100Click(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gBGImg.BGTransPct := (sender as TMenuItem).tag;
	RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.OverlayTransClick(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gBGImg.OverlayTransPct := (sender as TMenuItem).tag;
	RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.LayerDropSelect(Sender: TObject);
var
	lLayer: integer;
begin

	 lLayer := ActiveLayer;
	 MaxWindowEdit.Value := gMRIcroOverlay[lLayer].WindowScaledMax;
	 MinWindowEdit.Value := gMRIcroOverlay[lLayer].WindowScaledMin;
	 if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then exit;
	 LUTdrop.ItemIndex := (gMRIcroOverlay[lLayer].LUTindex);
	 //LUTinvertBtn.down := gMRIcroOverlay[lLayer].LUTinvert;
	 LutFromZeroBtn.down := gMRIcroOverlay[lLayer].LutFromZero;
end;

procedure TImgForm.UpdateLayerMenu;
var
	lStrings: TStringList;
	lPos,lLayer:integer;
begin
	lStrings := TStringList.Create;
	lStrings.Add('Background');
	lLayer := 0;
	for lPos := 1 to (knMaxOverlay-1) do //-1 as max overlay is VOI
	  if (gMRIcroOverlay[lPos].ImgBufferItems > 0) then begin
		   lStrings.Add(ParseFileName(ExtractFileName(gMRIcroOverlay[lPos].HdrFileName)));
		   inc(lLayer);
		   LUTdropLoad(lLayer);
	  end;
	LayerDrop.Items := lStrings;
	if LayerDrop.ItemIndex >= LayerDrop.Items.Count then
		LayerDrop.ItemIndex := (LayerDrop.Items.Count-1);
	LayerDropSelect(nil);
	lStrings.Free;
end;

procedure TImgForm.CloseOverlayImgClick(Sender: TObject);
var
	lOverlay: integer;
begin
  for lOverlay := 1 to (knMaxOverlay-1) do
	FreeImgMemory(gMRIcroOverlay[lOverlay]);
  UpdateLayerMenu;
  RefreshImagesTimer.Enabled := true;
end;



procedure TImgForm.LUTdropLoad(var lLayer: integer);
var
   lStr: string;
begin
	 if gMRIcroOverlay[lLayer].UsesCustomPalette then begin
		exit;
	 end;
	 //gMRIcroOverlay[lLayer].LUTindex := LUTdrop.ItemIndex;
	 if gMRIcroOverlay[lLayer].LUTindex < knAutoLUT then begin
		LoadMonochromeLUT(gMRIcroOverlay[lLayer].LUTindex,gBGImg,gMRIcroOverlay[lLayer]);
		RefreshImagesTimer.Enabled := true;
		exit;
	 end; //if B&W lut
	 lStr := gColorSchemeDir+pathdelim+LUTdrop.Items.Strings[gMRIcroOverlay[lLayer].LUTindex]+'.lut';
	 if not FileExistsEX(lStr) then
		showmessage('Can not find '+lStr);
	 LoadColorScheme(lStr, gMRIcroOverlay[lLayer]);
	 RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.LUTdropSelect(Sender: TObject);
var
   lLayer: integer;
begin
	 lLayer := ActiveLayer;
	 gMRIcroOverlay[lLayer].LUTindex := LUTdrop.ItemIndex;
	 //gMRIcroOverlay[lLayer].LUTinvert := LUTinvertBtn.down;
	 //gMRIcroOverlay[lLayer].LutFromZero := LutFromZeroBtn.down;
	 LUTdropLoad(lLayer);
	 //RescaleImagesTimer.Enabled := true;
end; //proc LUTdropSelect

procedure TImgForm.AutoContrastBtnClick(Sender: TObject);
var
	lLayer: integer;
begin
	 lLayer := ActiveLayer;
	MinWindowEdit.Value := raw2ScaledIntensity(gMRIcroOverlay[lLayer], gMRIcroOverlay[lLayer].AutoBalMinUnscaled);
	MaxWindowEdit.Value := raw2ScaledIntensity(gMRIcroOverlay[lLayer],gMRIcroOverlay[lLayer].AutoBalMaxUnscaled);{}
	RescaleImgIntensity(gBGImg,gMRIcroOverlay[lLayer], llayer);
	RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.MinContrastWindowEditChange(Sender: TObject);
var
	lLayer: integer;
begin
	lLayer := ActiveLayer;
	if gMRIcroOverlay[lLayer].WindowScaledMin = MinWindowEdit.Value then exit;
	gMRIcroOverlay[lLayer].WindowScaledMin := MinWindowEdit.Value;
	RescaleImagesTimer.Enabled := true;
end;

procedure TImgForm.MaxContrastWindowEditChange(Sender: TObject);
var
	lLayer: integer;
begin
	 lLayer := ActiveLayer;
	 if gMRIcroOverlay[lLayer].WindowScaledMax = MaxWindowEdit.Value then exit;
	 gMRIcroOverlay[lLayer].WindowScaledMax := MaxWindowEdit.Value;
	RescaleImagesTimer.Enabled := true;
end;

procedure TImgForm.OverlaySmoothMenuClick(Sender: TObject);
var
	lC: integer;
begin
	 if Sender = nil then begin
		gBGImg.OverlaySmooth := OverlaySmoothMenu.Checked;
		 exit;
	 end;
	 OverlaySmoothMenu.Checked := not OverlaySmoothMenu.Checked;
	 gBGImg.OverlaySmooth := OverlaySmoothMenu.Checked;
	 for lC := 1 to knMaxOverlay do
		if gMRIcroOverlay[lC].ScrnBufferItems > 0 then
		RescaleImgIntensity(gBGImg,gMRIcroOverlay[lC], lC);
	RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.ShowRenderClick(Sender: TObject);
begin
	RenderForm.Show;
end;

procedure TImgForm.PenBtnClick(Sender: TObject);
begin
	RefreshImagesTimer.Enabled := true;
end;

procedure OpenMRIcroROI (lFilename: string);
const
		  kMax12bit = 4095;
	 kMax16bit = (256*256)-1;
	 kMax15bit = kMax16bit shr 1;
	 kMax20bit = (16*256*256)-1;
	 k20v16bit = kMax20bit - kMax16bit;
	kMaxRuns = 10000;
	kMaxFile =  65536;
	 k16v12bit = kMax16bit - kMax12bit;
var
	lFile32bitItems,lFileSz,lFilePos,lSliceSz,lZ,lRunsOnSlice,
	lRunLength,lRun,lRunOffset,lOutputSliceOffset,lRunPos: integer;
	lROIformatRA: LongIntp;
	lF: File;
	lBigFormat: boolean;
begin
	lFileSz := FSize(lFilename);
	if (lFileSz < 1) or ((lFileSz mod 4) <> 0) then begin
		showmessage('Unable to open ROI: file size should be divisible by 4.');
		exit;
	end;
	lFile32bitItems := lFileSz div 4; //how many 32-bit items?
	lSliceSz := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
	if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0 then
		freemem(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer);
	gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems := lSliceSz * lZ;
	getmem(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,lSliceSz * lZ);
	fillchar(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems,0);
	if lSliceSz > 65535 then
		lBigFormat := true
	else
		lBigFormat := false;
	getmem(lROIformatRA,lFileSz); //file size must be divisible by 4
  {$I-}
  AssignFile(lF, lFilename);
  FileMode := 0;  { Set file access to read only }
  Reset(lF, 1);
  BlockRead(lF,lROIformatRA^,lFileSz);
  CloseFile(lF);
  FileMode := 2;
  {$I+}
  //next: check MSB of first byte to see if this is big format images
  if lBigFormat <> odd((lROIformatRA[1] and kMax16bit) shr 15) then
	Showmessage('Warning: this ROI does not appear to be designed for the currently loaded background image.');
  lFilePos := 1;
if lBigFormat then begin //20-byte offset, 12-byte runlength
  while lFilePos < lFile32bitItems do begin
		lRunsOnSlice := (lROIformatRA[lFilePos] shr 17) - 1; //shr 17: shift 16 bits, then div 2 (words instead of longints). Subtract 1 as the we have read slice number/ number of runs
		lZ := (lROIformatRA[lFilePos]  and kMax15bit);
		inc(lFilePos);
		lOutputSliceOffset := (lZ-1) * lSliceSz;
		for lRun := 1 to lRunsOnSlice do begin
			if (lFilePos <= lFileSz) then begin
				lRunLength := (lROIformatRA[lFilePos] shr 16) and kMax12bit;
				lRunOffset := (lROIformatRA[lFilePos]  and kMax16bit)+ ((lROIformatRA[lFilePos] shr 28) shl 16);
				if (lOutputSliceOffset+lRunLength+lRunOffset-1)> gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems then
					//showmessage('Overrun on slice '+inttostr(lZ))
				else for lRunPos := lRunOffset to (lRunLength+lRunOffset-1) do
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lRunPos+lOutputSliceOffset] := kVOI8bit;
			end;
			inc(lFilePos);
		end;//for all runs
  end; //while lPos < lFSz
end else begin //not big format format - 16-byte offset, 16-byte length
  while lFilePos < lFile32bitItems do begin
		//lRunsOnSlice := (lROIformatRA[lFilePos] shr 16) and kMax16bit;
		lRunsOnSlice := (lROIformatRA[lFilePos] shr 17) - 1; //shr 17: shift 16 bits, then div 2 (words instead of longints). Subtract 1 as the we have read slice number/ number of runs
		lZ := (lROIformatRA[lFilePos]  and kMax15bit);
		inc(lFilePos);
		lOutputSliceOffset := (lZ-1) * lSliceSz;
		//showmessage(inttostr(lZ)+'  '+inttostr(lRunsOnSlice)+'  '+inttostr(lFilePos)+'  '+inttostr(lFileSz));
		for lRun := 1 to lRunsOnSlice do begin
			if (lFilePos <= lFileSz) then begin
				lRunLength := (lROIformatRA[lFilePos] shr 16) and kMax16bit;
				lRunOffset := (lROIformatRA[lFilePos]  and kMax16bit);
				{if (lRunLength+lRunOffset-1)> gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems then
					showmessage('Overrun on slice '+inttostr(lZ))
				else} for lRunPos := lRunOffset to (lRunLength+lRunOffset-1) do
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lRunPos+lOutputSliceOffset] := kVOI8bit;
			end;
			inc(lFilePos);
		end;//for all runs
  end; //while lPos < lFSz
end; //if bigformat ... else little format
  freemem(lROIformatRA);
  lRun := maxint;
  LoadMonochromeLUT(lRun,gBGImg,gMRIcroOverlay[kVOIOverlayNum]);
end;

function ComputeCC (lOverlayNum: integer): single;
var
  lInc, lVol: integer;
begin
  result := 0;
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then
      exit;
   lVol := 0;
   for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do
		if gMRIcroOverlay[lOverlayNum].ScrnBuffer[lInc] > 0 then
      inc(lVol);
   result := ((lVol/1000)*gBGImg.ScrnMM[1]*gBGImg.ScrnMM[2]*gBGImg.ScrnMM[3]);
end;
procedure TImgForm.OpenVOICore(var lFilename : string);
var
	lExt: string;
begin
	 if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0 then
		ImgForm.CloseVOIClick(nil);
	lExt := UpCaseExt(lFileName);
	gBGImg.VOIchanged := false;
	if (lExt='.ROI') then begin

		Showmessage('Warning: MRIcro ROI format does not save image dimensions. The background image must be in the same dimensions as the ROI.');
		OpenMRIcroROI (lFileName);
                if (gBGImg.Resliced) then begin
                    Showmessage('If the ROI appears distorted, you may want to open you background image without reslicing and try again. '+
                    '(hold down the shift key when you load the background image)');
                end;
		ImgForm.RefreshImagesTimer.Enabled := true;
		exit;
	end;
	if not HdrForm.OpenAndDisplayHdr(lFilename,gMRIcroOverlay[kVOIOverlayNum]) then exit;
	if not OpenImg(gBGImg,gMRIcroOverlay[kVOIOverlayNum],false,true,false,gBGImg.ResliceOnLoad,false) then exit;
    caption := lFilename +'  cc=' +realtostr( ComputeCC(kVOIOverlayNum),2);
	ImgForm.RefreshImagesTimer.Enabled := true;
end;//OpenVOIClick


procedure TImgForm.OpenVOIClick(Sender: TObject);
var
	lFilename: string;
   State : TKeyboardState;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image (''File''/''Open'') before adding a VOI.');
		exit;
	end;
	//HdrForm.OpenHdrDlg.Filter := '*.roi';//kVOIFilter;
	//if not HdrForm.OpenHdrDlg.Execute then exit;
	 if not OpenDialogExecute(kVOIFilter,'Select Volume of Interest drawing',false) then exit;
	lFilename := HdrForm.OpenHdrDlg.Filename;
	OpenVOICore(lFilename);
  GetKeyboardState(State) ;
  if  ((State[vk_shift] And 128) <> 0) then begin
      Showmessage('Overlay loaded left-right flipped (shift key depressed)');
      MirrorScrnBuffer(gBGImg,gMRIcroOverlay[kVOIOverlayNum]);
      //MirrorImgBuffer (gMRIcroOverlay[kVOIOverlayNum] );
  end;
end;//OpenVOIClick

(*procedure SaveVOIunmirror;
var lHdr: TMRIcroHdr;
begin
  if  gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems= 0 then begin
		Showmessage('You need to create a VOI before you can save it.');
		exit;
  end;
  //Start 10/2007: adjust scl_slope;? 10/2007
  CopyNiftiHdr(gMRIcroOverlay[kBGOverlayNum].NiftiHdr,lNIFTIhdr);
  lNIFTIhdr.scl_slope := 1;
  lNIFTIhdr.scl_inter := 0;
  if gBGImg.Mirror then begin
     lHdr.ScrnBufferItems := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
     Getmem(lHdr.ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems);
     Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[1],lHdr.ScrnBuffer^[1],lHdr.ScrnBufferItems);
     MirrorScrnBuffer(gBGImg,lHdr);
     SaveAsVOIorNIFTI(lHdr.ScrnBuffer,lHdr.ScrnBufferItems,1,1,true,lNIFTIhdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName);
     Freemem(lHdr.ScrnBuffer);
     exit; //sept2007
  end;
  SaveAsVOIorNIFTI(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems,1,1,true,lNiftiHdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName);
  //656
end;*)



procedure TImgForm.SaveVOIcore(lPromptFilename: boolean);
   var lHdr: TMRIcroHdr;
   lNIFTIhdr: TNIFTIhdr;
begin
  if  gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems= 0 then begin
		Showmessage('You need to create a VOI before you can save it.');
		exit;
  end;
  //Start 10/2007: adjust scl_slope;? 10/2007
  CopyNiftiHdr(gMRIcroOverlay[kBGOverlayNum].NiftiHdr,lNIFTIhdr);
  lNIFTIhdr.scl_slope := 1;
  lNIFTIhdr.scl_inter := 0;
  //end
  if gBGImg.Mirror then begin
     lHdr.ScrnBufferItems := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
     Getmem(lHdr.ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems);
     Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[1],lHdr.ScrnBuffer^[1],lHdr.ScrnBufferItems);
     MirrorScrnBuffer(gBGImg,lHdr);
     if lPromptFilename then
      SaveAsVOIorNIFTI(lHdr.ScrnBuffer,lHdr.ScrnBufferItems,1,1,true,lNIFTIhdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName)
     else
      SaveAsVOIorNIFTIcore(gMRIcroOverlay[kVOIOverlayNum].HdrFileName,lHdr.ScrnBuffer,lHdr.ScrnBufferItems,1,1,lNIFTIhdr);
     Freemem(lHdr.ScrnBuffer);
     exit; //12/2010
  end;
  if lPromptFilename then
    SaveAsVOIorNIFTI(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems,1,1,true,lNiftiHdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName)
  else
    SaveAsVOIorNIFTIcore(gMRIcroOverlay[kVOIOverlayNum].HdrFileName,gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems,1,1,lNiftiHdr);
end;

procedure TImgForm.SaveVOIClick(Sender: TObject);
   var lHdr: TMRIcroHdr;
   lNIFTIhdr: TNIFTIhdr;
begin
  if  gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems= 0 then begin
		Showmessage('You need to create a VOI before you can save it.');
		exit;
  end;
  //Start 10/2007: adjust scl_slope;? 10/2007
  CopyNiftiHdr(gMRIcroOverlay[kBGOverlayNum].NiftiHdr,lNIFTIhdr);
  lNIFTIhdr.scl_slope := 1;
  lNIFTIhdr.scl_inter := 0;
  //end
  if gBGImg.Mirror then begin
     lHdr.ScrnBufferItems := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
     Getmem(lHdr.ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems);
     Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[1],lHdr.ScrnBuffer^[1],lHdr.ScrnBufferItems);
     MirrorScrnBuffer(gBGImg,lHdr);
     SaveAsVOIorNIFTI(lHdr.ScrnBuffer,lHdr.ScrnBufferItems,1,1,true,lNIFTIhdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName);
     Freemem(lHdr.ScrnBuffer);
     exit; //sept2007
  end;
  SaveAsVOIorNIFTI(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems,1,1,true,lNiftiHdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName);
end;

procedure TImgForm.VOIColorClick(Sender: TObject);
var
	lMaxi: longint;
begin
		ColorDialog1.Color := gBGImg.VOIClr;
		if not ColorDialog1.Execute then exit;
		gBGImg.VOIClr := ColorDialog1.Color;
		if gBGImg.VOIClr = clBlack then
			gBGImg.VOIClr := 1; //reserve 0 for deleting
		lMaxi:=maxint;
		LoadMonochromeLUT(lMaxi,gBGImg,gMRIcroOverlay[kVOIOverlayNum]);
		RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.CloseVOIClick(Sender: TObject);
begin
  if (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems>0) and (gBGImg.VOIChanged) then begin
	case MessageDlg('Do you wish to save the VOI drawing?', mtConfirmation,
				[mbYes, mbNo], 0) of	{ produce the message dialog box }
				id_Yes: SaveVOIClick(nil);
	end; //case
  end;//if changed
  FreeUndoVol;
  FreeImgMemory(gMRIcroOverlay[kVOIOverlayNum]);
  gBGImg.VOIUndoSlice := 0;
  gBGImg.VOIchanged := false;
  gBGImg.VOIUndoOrient := 0;
  RefreshImagesTimer.Enabled := true;
end;

procedure ImageRB (var lMaxR,lMaxB: integer; var lImage: TImage);
var
  lPos: integer;
begin
    if not lImage.Visible then
      exit;
    lPos := lImage.Left+lImage.Width;
    if lPos > lMaxR then
      lMaxR := lPos;
    lPos := lImage.Top+lImage.Height;
    if lPos > lMaxB then
      lMaxB := lPos;
end;

procedure CreateImg(lPGHt,lPGWid:integer; var lImage: TImage);
var
 sbBits : PByteArray;
 nBytesInImage: integer;
   lBMP: TBitmap;
   lSrcRect,lDestRect: TRect;
begin

	 lBMP := TBitmap.Create;
	 TRY
			 lBMP.PixelFormat := pf32bit;
			 lBMP.Width := lPGwid;
			 lBMP.Height := lPGHt;
			 sbBits := lBmp.ScanLine[lPGHt-1];
			 nBytesInImage := lPGWid*lPGHt * 4;
			 //CopyMemory(Pointer(sbBits),Pointer(lBuff),nBytesInImage);
                         FillChar(sbBits^,({lPGHt*{}lPGHt*lPGwid*4), 255);
			 lImage.Canvas.CopyMode := cmSrcCopy;
			 lImage.Width := (lBmp.Width);//xx
			 lImage.Height := (lBmp.Height);//xx
			 lImage.Picture.Graphic := lBMP;
	 FINALLY
			   lBMP.Free;
	 END; //try..finally
end; //proc SetDimension32

procedure CopyImg(var lSourceImg,lDestImg: TImage);
var
  lPos: integer;
begin
    if not lSourceImg.Visible then
      exit;
    lDestImg.Canvas.Draw(lSourceImg.Left,lSourceImg.Top,lSourceImg.Picture.Graphic);
end;

procedure TImgForm.SaveOrCopyImages(lCopy: boolean);
//Requires 'ClipBrd' in uses section
var
  lMaxR,lMaxB: integer;
  lOutImg: TImage;
begin
  lMaxR := 0;
  lMaxB := 0;
  ImageRB(lMaxR,lMaxB,ImgForm.PGImageAx);
  ImageRB(lMaxR,lMaxB,ImgForm.PGImageCor);
  ImageRB(lMaxR,lMaxB,ImgForm.PGImageSag);
  if (lMaxR < 1) or (lMaxB < 1) then
    exit;
  lOutImg :=  TImage.Create(ImgForm);
  try
    //use the object
    CreateImg(lMaxB,lMaxR,lOutImg);
    lOutImg.Canvas.Brush.color := ImgForm.TriplePanel.color;
    lOutImg.Canvas.Rectangle(0,0,lMaxR+1,lMaxB+1);
    CopyImg(ImgForm.PGImageAx,lOutImg);
    CopyImg(ImgForm.PGImageCor,lOutImg);
    CopyImg(ImgForm.PGImageSag,lOutImg);
    if lCopy then
      Clipboard.Assign(lOutImg.Picture.Graphic)
    else
      	SaveImgAsPNGBMP (lOutImg);
  finally
    FreeAndNil (lOutImg);
  end;

end;

procedure TImgForm.Saveaspicture1Click(Sender: TObject);
begin
  SaveOrCopyImages(false);
end; //Proc Saveaspicture1Click

procedure TImgForm.Copy1Click(Sender: TObject); //Requires 'ClipBrd' in uses section
begin
  SaveOrCopyImages(true);
end;

(*procedure TImgForm.Copy1Click(Sender: TObject); //Requires 'ClipBrd' in uses section
var
  MyFormat : Word;
  lImage: TImage;
  AData: THandle;
  APalette : HPalette;   //For later versions of Delphi: APalette : THandle;
begin
	 lImage := SelectedImage;
	 if (lImage.Picture.Graphic = nil) then begin //1420z
		Showmessage('You need to load an image before you can copy it to the clipboard.');
		exit;
	 end;
	 lImage.Picture.Bitmap.SaveToClipBoardFormat(MyFormat,AData,APalette);
	 ClipBoard.SetAsHandle(MyFormat,AData);
	 if (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems>0) then
	 	WriteUndoVOI(SelectedImageNum,false);
end; *)

procedure TImgForm.Undo1Click(Sender: TObject);
begin
	if gBGImg.VOIUndoSlice < 1 then exit;
	case gBGImg.VOIUndoOrient of
		4: UndoVolVOI;
		3: ReadCorVOI(ImgForm.UndoImg,gBGImg.VOIUndoSlice);
		2: ReadSagVOI(ImgForm.UndoImg,gBGImg.VOIUndoSlice);
		1: ReadAxialVOI(ImgForm.UndoImg,gBGImg.VOIUndoSlice);
	end;
	ImgForm.RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.Paste1Click(Sender: TObject);
begin
	if (gBGImg.VOIUndoSlice < 1) then exit;
  if gBGImg.VOIUndoOrient <> SelectedImageNum then //12/2007
    exit;
	WriteUndoVOI(SelectedImageNum,true);
	case gBGImg.VOIUndoOrient of
		3: ReadCorVOI(ImgForm.DrawImg,ImgForm.YViewEdit.asInteger);
		2: ReadSagVOI(ImgForm.DrawImg,ImgForm.XViewEdit.asInteger);
		1: ReadAxialVOI(ImgForm.DrawImg,ImgForm.ZViewEdit.asInteger);
		else exit;
	end;
	ImgForm.RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.HideROIBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	gOrigBGTransPct := gBGImg.BGTransPct;
	gBGImg.BGTransPct := 100;
	refreshimagestimer.enabled := true;
end;

procedure TImgForm.HideROIBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	gBGImg.BGTransPct := gOrigBGTransPct;
	Refreshimagestimer.enabled := true;
end;

procedure TImgForm.Applyintensityfiltertovolume1Click(Sender: TObject);
begin
  if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then begin
	showmessage('You must have open a background image in order to apply an intensity filter (use File/Open).');
	exit;
  end;
	 FilterROIform.showmodal;
end;

procedure TImgForm.Quicksmooth1Click(Sender: TObject);
var
 lHdr: TMRicroHdr;
 lXDim,lYDim,lZDim,lMaxGray,lSum,lMinWt,lMaxWt,lMinInten,lMaxInten,lOutVolVox,lOutSliceSz,lX,lY,lZ,lXxi,l2,lZyi: integer;
 lSum32,lMinInten32,lMaxInten32: single;
 lTempBuff,lSrcBuff: Bytep;
 l16TempBuff,l16SrcBuff: SmallIntP;
 l32TempBuff,l32SrcBuff: SingleP;
procedure AddPoint (lInten,lWeight:integer);
begin
	lSum := lSum + (lInten*lWeight);
	if lInten <= lMinInten then begin
	   lMinWt := lWeight;
	   lMinInten := lInten;
	end else if lInten >= lMaxInten then begin
	   lMaxWt := lWeight;
	   lMaxInten := lInten;
	end;
end;  //nested AddPoint
procedure AddPoint32 (lInten32: single; lWeight:integer);
begin
	lSum32 := lSum32 + (lInten32*lWeight);
	if lInten32 <= lMinInten32 then begin
	   lMinWt := lWeight;
	   lMinInten32 := lInten32;
	end else if lInten32 >= lMaxInten32 then begin
       lMaxWt := lWeight;
	   lMaxInten32 := lInten32;
	end;
end;  //nested AddPoint32
begin
  lHdr := gMRIcroOverlay[kBGOverlayNum];
  lXDim := gBGImg.ScrnDim[1];
  lYDim := gBGImg.ScrnDim[2];
  lZDim := gBGImg.ScrnDim[3];
  lOutSliceSz := gBGImg.ScrnDim[1] * gBGImg.ScrnDim[2];
  lOutVolVox :=  lOutSliceSz * lZDim;
  if (lXDim < 3) or (lYDim < 3) or (lZDim < 3) or (lOutVolVox < 36) then begin
	 showmessage('The 3D smoothing can only be applied to images with at least 3 slices in each dimension.'); 
	 exit;
  end;
  if (lHdr.ImgBufferItems < 1) then begin
	  showmessage('Please first load the image you would like to smooth.');
	  exit;
  end;
  ProgressBar1.Min := 0;
  ProgressBar1.Max :=lZDim;
  StatusLabel.caption := 'Removing noise speckles and smoothing data [blur]';
  if  lHdr.ImgBufferBPP = 4 then begin //32-bit float data
	l32SrcBuff := SingleP(lHdr.ImgBuffer);
	GetMem(l32TempBuff,lOutVolVox*sizeof(single));
    Move(l32SrcBuff^,l32TempBuff^,lOutVolVox*sizeof(single));
	for lZ := 1 to lOutVolVox do
		l32SrcBuff[lZ] := 0;
	for lZ := lZDim-1 downto 2 do begin
		ProgressBar1.Position := (lZDim-lZ);
		for lY := lYDim-1 downto 2 do begin
			lZyi := ((lZ-1)*lOutSliceSz) + ((lY-1) * lXDim);
			for lX := lXDim-1 downto 2 do begin
                lXxi := lZyi + lX;
				//next: gaussian mean after min/max values are excluded
                lSum32 := 0;
				lMinInten32 := l32TempBuff[lXxi];
                lMaxInten32 := l32TempBuff[lXxi];
				lMinWt := 12;
                lMaxWt := 12;
				AddPoint32(l32TempBuff[lXxi],12);//quad-weight center
				AddPoint32(l32TempBuff[lXxi-lOutSliceSz],2);//prev slice
				AddPoint32(l32TempBuff[lXxi+lOutSliceSz],2);//next slices
				AddPoint32(l32TempBuff[lXxi-1],2);//Left
				AddPoint32(l32TempBuff[lXxi+1],2);//right
				AddPoint32(l32TempBuff[lXxi-lXDim],2);//up
				AddPoint32(l32TempBuff[lXxi+lXDim],2);//down
				AddPoint32(l32TempBuff[lXxi-lOutSliceSz-1],1);
				AddPoint32(l32TempBuff[lXxi-lOutSliceSz+1],1);
				AddPoint32(l32TempBuff[lXxi-lOutSliceSz-lXDim],1);
				AddPoint32(l32TempBuff[lXxi-lOutSliceSz+lXDim],1);
				AddPoint32(l32TempBuff[lXxi+lOutSliceSz-1],1);
				AddPoint32(l32TempBuff[lXxi+lOutSliceSz+1],1);
				AddPoint32(l32TempBuff[lXxi+lOutSliceSz-lXDim],1);
				AddPoint32(l32TempBuff[lXxi+lOutSliceSz+lXDim],1);
				AddPoint32(l32TempBuff[lXxi-lXDim-1],1);
				AddPoint32(l32TempBuff[lXxi+lXDim-1],1);
				AddPoint32(l32TempBuff[lXxi-lXDim+1],1);
				AddPoint32(l32TempBuff[lXxi+lXDim+1],1);
				if lMinInten32 = lMaxInten32 then
				   l32SrcBuff[lXxi] := lMaxInten32 //no variability in data
				else begin
					 l2 := 36 - lMinWt -lMaxWt;  //weight after we exceed brightest and darkest
					 lSum32 := lSum32 -(lMinWt*lMinInten32) - (lMaxWt*lMaxInten32); //exclude brightest/darkest
					 l32SrcBuff[lXxi] := (lSum32/l2);
				end;
			end; //forX
		end; //forY
	end; //forZ
	Freemem(l32TempBuff);
  end else if (lHdr.ImgBufferBPP = 2) then begin //16-bit int data*)
	l16SrcBuff :=  SmallIntP(lHdr.ImgBuffer );
	GetMem(l16TempBuff,lOutVolVox*sizeof(word));
	Move(l16SrcBuff^,l16TempBuff^,lOutVolVox*sizeof(word));
	for lZ := 1 to lOutVolVox do
		l16SrcBuff[lZ] := 0;
	for lZ := lZDim-1 downto 2 do begin
		ProgressBar1.Position := (lZDim-lZ);
		for lY := lYDim-1 downto 2 do begin
			lZyi := ((lZ-1)*lOutSliceSz) + ((lY-1) * lXDim);
			for lX := lXDim-1 downto 2 do begin
				lXxi := lZyi + lX;
				//next: gaussian mean after min/max values are excluded
				lSum := 0;
				lMinInten := l16TempBuff[lXxi];
				lMaxInten := l16TempBuff[lXxi];
				lMinWt := 12;
				lMaxWt := 12;
				AddPoint(l16TempBuff[lXxi],12);//quad-weight center
				AddPoint(l16TempBuff[lXxi-lOutSliceSz],2);//prev slice
				AddPoint(l16TempBuff[lXxi+lOutSliceSz],2);//next slices
                AddPoint(l16TempBuff[lXxi-1],2);//Left
				AddPoint(l16TempBuff[lXxi+1],2);//right
				AddPoint(l16TempBuff[lXxi-lXDim],2);//up
				AddPoint(l16TempBuff[lXxi+lXDim],2);//down
                AddPoint(l16TempBuff[lXxi-lOutSliceSz-1],1);
				AddPoint(l16TempBuff[lXxi-lOutSliceSz+1],1);
				AddPoint(l16TempBuff[lXxi-lOutSliceSz-lXDim],1);
				AddPoint(l16TempBuff[lXxi-lOutSliceSz+lXDim],1);
				AddPoint(l16TempBuff[lXxi+lOutSliceSz-1],1);
				AddPoint(l16TempBuff[lXxi+lOutSliceSz+1],1);
                AddPoint(l16TempBuff[lXxi+lOutSliceSz-lXDim],1);
				AddPoint(l16TempBuff[lXxi+lOutSliceSz+lXDim],1);
				AddPoint(l16TempBuff[lXxi-lXDim-1],1);
				AddPoint(l16TempBuff[lXxi+lXDim-1],1);
                AddPoint(l16TempBuff[lXxi-lXDim+1],1);
				AddPoint(l16TempBuff[lXxi+lXDim+1],1);
                if lMinInten = lMaxInten then
				   l16SrcBuff[lXxi] := lMaxInten //no variability in data
                else begin
					 l2 := 36 - lMinWt -lMaxWt;  //weight after we exceed brightest and darkest
                     lSum := lSum -(lMinWt*lMinInten) - (lMaxWt*lMaxInten); //exclude brightest/darkest
					 l16SrcBuff[lXxi] := round(lSum/l2);
                end;
			end; //forX
		end; //forY
	end; //forZ
	Freemem(l16TempBuff);
	//OptimizeSingle(nil);
  end else if lHdr.ImgBufferBPP = 1 then begin //8-bit data
	  lSrcBuff := lHdr.ImgBuffer;
   GetMem(lTempBuff,lOutVolVox);
   Move(lSrcBuff^,lTempBuff^,lOutVolVox);
   fillchar(lSrcBuff^,lOutVolVox,0); //set edges to 0, as outside voxel is not smoothed
   for lZ := lZDim-1 downto 2 do begin
		ProgressBar1.Position := (lZDim-lZ);
		for lY := lYDim-1 downto 2 do begin
			lZyi := ((lZ-1)*lOutSliceSz) + ((lY-1) * lXDim);
			for lX := lXDim-1 downto 2 do begin
				lXxi := lZyi + lX;
				//next: gaussian mean after min/max values are excluded
				lSum := 0;
				lMinInten := lTempBuff[lXxi];
				lMaxInten := lTempBuff[lXxi];
				lMinWt := 12;
				lMaxWt := 12;
				AddPoint(lTempBuff[lXxi],12);//quad-weight center
				AddPoint(lTempBuff[lXxi-lOutSliceSz],2);//prev slice
                AddPoint(lTempBuff[lXxi+lOutSliceSz],2);//next slices
				AddPoint(lTempBuff[lXxi-1],2);//Left
				AddPoint(lTempBuff[lXxi+1],2);//right
				AddPoint(lTempBuff[lXxi-lXDim],2);//up
                AddPoint(lTempBuff[lXxi+lXDim],2);//down
				AddPoint(lTempBuff[lXxi-lOutSliceSz-1],1);
                AddPoint(lTempBuff[lXxi-lOutSliceSz+1],1);
				AddPoint(lTempBuff[lXxi-lOutSliceSz-lXDim],1);
                AddPoint(lTempBuff[lXxi-lOutSliceSz+lXDim],1);
				AddPoint(lTempBuff[lXxi+lOutSliceSz-1],1);
				AddPoint(lTempBuff[lXxi+lOutSliceSz+1],1);
				AddPoint(lTempBuff[lXxi+lOutSliceSz-lXDim],1);
                AddPoint(lTempBuff[lXxi+lOutSliceSz+lXDim],1);
				AddPoint(lTempBuff[lXxi-lXDim-1],1);
                AddPoint(lTempBuff[lXxi+lXDim-1],1);
				AddPoint(lTempBuff[lXxi-lXDim+1],1);
				AddPoint(lTempBuff[lXxi+lXDim+1],1);
				if lMinInten = lMaxInten then
				   lSrcBuff[lXxi] := lMaxInten //no variability in data
				else begin
					 l2 := 36 - lMinWt -lMaxWt;  //weight after we exceed brightest and darkest
					 lSum := lSum -(lMinWt*lMinInten) - (lMaxWt*lMaxInten); //exclude brightest/darkest
					 lSrcBuff[lXxi] := round(lSum/l2);
				end;
			end; //forX
		end; //forY
	end; //forZ
	Freemem(lTempBuff);
  end else begin //8bit data
      showmessage('Unknown bits per pixel '+inttostr(lHdr.ImgBufferBPP) );
  end;
	ProgressBar1.Position := 0;
		RescaleImgIntensity(gBGImg,gMRIcroOverlay[kBGOverlayNum],kBGOverlayNum);
	RefreshImagesTimer.Enabled := true;
end; //quicksmooth

function GetReal (lDefault: single): single;
var
   lOK: boolean;
   lS: string;
begin
    result := lDefault;
    lS := floattostr(lDefault);
    lOK := InputQuery('Enter a number', 'Enter a value', lS);
    if not lOK then
       exit;
    result := strtofloat(lS);
end;

procedure TImgForm.VOImaskClick(Sender: TObject);
const
  kMax = 0.995;
var
 lFillS: single;
 lHdr,lMaskHdr: TMRicroHdr;
 lPreserve, lFillI, lLayer,lXDim,lYDim,lZDim,lOutVolVox,lOutSliceSz,lZ: integer;
 lSrcBuff,lMaskBuff: Bytep;
 l16SrcBuff: SmallIntP;
  lScale : boolean; //make region brighter or darker...
 l32SrcBuff: SingleP;
begin
  lScale := false;
	lPreserve := (sender as TMenuItem).tag;
  if (ssCtrl in KeyDataToShiftState(vk_Shift)) then begin
    lScale := true;
    lFillS := GetReal(0);
  end else if (ssShift in KeyDataToShiftState(vk_Shift)) then begin
    lFillS := GetReal(0);
  end else
    lFillS := 0;
  lFillI := round(lFillS);

  lHdr := gMRIcroOverlay[kBGOverlayNum];
  lMaskHdr := gMRIcroOverlay[kVOIOverlayNum];
  lXDim := gBGImg.ScrnDim[1];
  lYDim := gBGImg.ScrnDim[2];
  lZDim := gBGImg.ScrnDim[3];
  lOutSliceSz := gBGImg.ScrnDim[1] * gBGImg.ScrnDim[2];
  lOutVolVox :=  lOutSliceSz * lZDim;
  if (lXDim < 2) or (lYDim < 2) or (lZDim < 2) then begin
	 showmessage('Masking can only be applied to images with multiple slices in 3 dimensions.');
	 exit;
  end;
  if (lHdr.ImgBufferItems <>  lMaskHdr.ScrnBufferItems) or (lHdr.ImgBufferItems < 8) then begin
	  showmessage('Please first load both an image (File/Open) and a masking VOI (Draw/Open).');
	  exit;
  end;
  if gBGImg.Mirror then
     MirrorScrnBuffer(gBGImg,lMaskHdr);//4/2008
  lMaskBuff := (lMaskHdr.ScrnBuffer);
  ProgressBar1.Min := 0;
  ProgressBar1.Max :=lZDim;
  StatusLabel.caption := 'Masking data';
  for lLayer := kBGOverlayNum to ( kVOIOverlayNum-1) do begin
    lHdr := gMRIcroOverlay[lLayer {kBGOverlayNum}];
    if (lHdr.ImgBufferItems =  lMaskHdr.ScrnBufferItems) and (lScale) then begin
      showmessage('Scaling region under VOI by '+floattostr(lFillS));
      if  lHdr.ImgBufferBPP = 4 then begin //32-bit float data
	      l32SrcBuff := SingleP(lHdr.ImgBuffer);
	      if lPreserve = 1 then begin
		      for lZ := 1 to lOutVolVox do
			      if lMaskBuff[lZ] = 0 then
				      l32SrcBuff[lZ] := lFillS*l32SrcBuff[lZ];
	      end else begin
		      for lZ := 1 to lOutVolVox do
			      if lMaskBuff[lZ] <> 0 then
				      l32SrcBuff[lZ] := lFillS*l32SrcBuff[lZ];
	      end; //if preserve
        for lZ := 1 to lOutVolVox do
			      if l32SrcBuff[lZ] > kMax then
				      l32SrcBuff[lZ] := kMax;
      end else
        showmessage('Scaling only works for 32-bit float images.');
    end else if (lHdr.ImgBufferItems =  lMaskHdr.ScrnBufferItems) then begin
      if  lHdr.ImgBufferBPP = 4 then begin //32-bit float data
	      l32SrcBuff := SingleP(lHdr.ImgBuffer);
	      if lPreserve = 1 then begin
		      for lZ := 1 to lOutVolVox do
			      if lMaskBuff[lZ] = 0 then
				      l32SrcBuff[lZ] := lFillS;
	      end else begin
		      for lZ := 1 to lOutVolVox do
			      if lMaskBuff[lZ] <> 0 then
				      l32SrcBuff[lZ] := lFillS;
	      end; //if preserve
      end else if (lHdr.ImgBufferBPP = 2) then begin //16-bit int data*)
	      l16SrcBuff :=  SmallIntP(lHdr.ImgBuffer );
        //lMin := round( lHdr.GlMinUnscaledS);

	      if lPreserve = 1 then begin
		      for lZ := 1 to lOutVolVox do
			      if lMaskBuff[lZ] = 0 then
				      l16SrcBuff[lZ] := lFillI;//lMin;
	      end else begin
		      for lZ := 1 to lOutVolVox do
			      if lMaskBuff[lZ] <> 0 then
				      l16SrcBuff[lZ] := lFillI;//lMin;
	      end;
      end else if lHdr.ImgBufferBPP = 1 then begin //8-bit data
	      lSrcBuff := lHdr.ImgBuffer;
	      if lPreserve = 1 then begin
		      for lZ := 1 to lOutVolVox do
			      if lMaskBuff[lZ] = 0 then
				      lSrcBuff[lZ] := lFillI
	      end else begin

		      for lZ := 1 to lOutVolVox do
			      if lMaskBuff[lZ] <> 0 then
				      lSrcBuff[lZ] := lFillI;
            //else if lSrcBuff[lZ] = 0 then
            //  lSrcBuff[lZ] := 1;
	      end;
      end else begin //8bit data
	      showmessage('Unknown bits per pixel '+inttostr(lHdr.ImgBufferBPP) );
      end;
    end;//layer exists
  end; //for each layer

  if gBGImg.Mirror then
     MirrorScrnBuffer(gBGImg,lMaskHdr);//4/2008

	ProgressBar1.Position := 0;
		RescaleImgIntensity(gBGImg,gMRIcroOverlay[kBGOverlayNum],kBGOverlayNum);
	RefreshImagesTimer.Enabled := true;
end;  //VOImaskClick

(*procedure RepositionOrigin (var lNiftiHdr: TNIFTIHdr);
var
	lX,lY,lZ,lOverlay,lLen: integer;
	lXmm,lYmm,lZmm: single;
	lIntenStr : string;
begin
	lX := ImgForm.XViewEdit.asInteger;
	lY := ImgForm.YViewEdit.asInteger;
	lZ := ImgForm.ZViewEdit.asInteger;
        for lLen := 0 to 2 do begin
            lNiftiHdr.srow_x[lLen] := abs(lNiftiHdr.srow_x[lLen]);
            lNiftiHdr.srow_y[lLen] := abs(lNiftiHdr.srow_y[lLen]);
            lNiftiHdr.srow_z[lLen] := abs(lNiftiHdr.srow_z[lLen]);
        end;
        lNiftiHdr.srow_x[3] := -lNiftiHdr.srow_x[0]*(lX-1);
        lNiftiHdr.srow_y[3] := -lNiftiHdr.srow_y[1]*(lY-1);
        lNiftiHdr.srow_z[3] := -lNiftiHdr.srow_z[2]*(lZ-1);
        //fx(lNiftiHdr.srow_x[3],lNiftiHdr.srow_y[3],lNiftiHdr.srow_z[3]);
        //fx(lX,lY,lZ);
end;

function CtrlDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Control] And 128) <> 0) ;
end;*)

(*procedure RescaleImageIntensity (var lImgBuffer: ByteP; lImgBufferItems, lImgBufferBPP,lnVol: integer; DefaultFormatVOI: boolean; var lNiftiHdr: TNIFTIHdr; lDefFilename: string);
const
  kSlopeChange = 4;
  kInterceptChange = -2000;
var
  lFilename: string;
  l16Buf : SmallIntP;
  lInterChange : single;
  lC: integer;
begin
    if lImgBufferItems < 1 then
      exit;
    if lImgBufferBPP <> 2 then begin
        showmessage('rescale only currently works with 16-bit data...');
    end;
    lFilename := 'c:\nx.nii';
    lInterChange := 0;
    if lNiftiHdr.scl_slope <> 0 then
      lInterChange := kInterceptChange/lNiftiHdr.scl_slope;
    l16Buf := SmallIntP(lImgBuffer );
    for lC := 1 to lImgBufferItems do
      l16Buf[lC] := round((l16Buf[lC]-lInterChange)*kSlopeChange);
    lNiftiHdr.scl_slope := lNiftiHdr.scl_slope/kSlopeChange;
    lNiftiHdr.scl_inter := lNiftiHdr.scl_inter + kInterceptChange;
    SaveAsVOIorNIFTIcore (lFilename,lImgBuffer, lImgBufferItems, lImgBufferBPP,lnVol,lNiftiHdr);
end;*)

(*procedure MinImageIntensity (var lImgBuffer: ByteP; lImgBufferItems, lImgBufferBPP,lnVol: integer; DefaultFormatVOI: boolean; var lNiftiHdr: TNIFTIHdr; lDefFilename: string);
var
  lFilename: string;
  l16Buf : SmallIntP;
  lInterChange : single;
  lC: integer;
begin
    if lImgBufferItems < 1 then
      exit;
    if lImgBufferBPP <> 2 then begin
        showmessage('rescale only currently works with 16-bit data...');
    end;
    lFilename := 'c:\nx.nii';
    lInterChange := 0;
    l16Buf := SmallIntP(lImgBuffer );
    for lC := 1 to lImgBufferItems do
      if l16Buf[lC] < 400 then
      l16Buf[lC] := 0;
    SaveAsVOIorNIFTIcore (lFilename,lImgBuffer, lImgBufferItems, lImgBufferBPP,lnVol,lNiftiHdr);
end;*)

procedure TImgForm.SaveasNIfTI1Click(Sender: TObject);
var
	lLayer: integer;
begin
	lLayer := ActiveLayer;
	if gMRIcroOverlay[lLayer].ImgBufferItems=0 then begin
		Showmessage('You must load an image [File/Open] before you can save the image.');
		exit;
	end;
        (*if CtrlDown then begin
           RepositionOrigin(gMRIcroOverlay[lLayer].NiftiHdr);
           Showmessage('Ctrl pressed while saving - current coordinates will be used for origin.');
        end;*)
	if (not IsNifTiMagic(gMRIcroOverlay[lLayer].niftiHdr)) then
		Showmessage('Warning: image will be saved with NIfTI spatial transform - ensure this image matches the orientation of the template images.');
    //RescaleImageIntensity(gMRIcroOverlay[lLayer].ImgBuffer,gMRIcroOverlay[lLayer].ImgBufferItems,gMRIcroOverlay[lLayer].ImgBufferBPP,1,false,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[lLayer].HdrFilename);
    //MinImageIntensity(gMRIcroOverlay[lLayer].ImgBuffer,gMRIcroOverlay[lLayer].ImgBufferItems,gMRIcroOverlay[lLayer].ImgBufferBPP,1,false,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[lLayer].HdrFilename);
  SaveAsVOIorNIFTI(gMRIcroOverlay[lLayer].ImgBuffer,gMRIcroOverlay[lLayer].ImgBufferItems,gMRIcroOverlay[lLayer].ImgBufferBPP,1,false,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[lLayer].HdrFilename);
end;

procedure TImgForm.ROIcomparisonClick(Sender: TObject);
var lComparison,lVolItems,lOverlay,lnOverlays,lPos: integer;
begin
	lComparison := (Sender as TMenuItem).tag; //0=intersect AND,1=union OR ,2=mask
	lVolItems := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]* gBGImg.ScrnDim[3];
	if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems <> lVolItems) or (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems <> lVolItems) then begin
		Showmessage('VOI comparisons require a VOI loaded onto a background image (Draw/Open).');
		exit;
	end;
	lnOverlays := 0;
	for lOverlay := 1 to knMaxOverlay do
		if gMRIcroOverlay[lOverlay].ScrnBufferItems = lVolItems then
			inc(lnOverlays);
	if (lnOverlays = 0) then begin
		Showmessage('VOI comparisons require loaded overlays (Overlay/Add).');
		exit;
	end;
	CreateUndoVol;
	{case MessageDlg('Warning: Unable to undo this operation. You should save a backup copy prior to this (Draw/Save). Are you sure you wish to filter your VOI?', mtConfirmation,
		[mbYes, mbCancel], 0) of
		id_Cancel: exit;
	end; //case {}
	if lComparison = 0 then begin //intersect AND
	  for lOverlay := 1 to (knMaxOverlay-1) do begin
		if gMRIcroOverlay[lOverlay].ScrnBufferItems = lVolItems then begin
			for lPos := 1 to lVolItems do
				if gMRIcroOverlay[lOverlay].ScrnBuffer[lPos] = 0 then
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] := 0;
		end; //if overlay loaded
	  end; //for each overlay
	end else if lComparison = 1 then begin //if intersect else UNION OR
	  for lOverlay := 1 to (knMaxOverlay-1) do begin
		if gMRIcroOverlay[lOverlay].ScrnBufferItems = lVolItems then begin
			for lPos := 1 to lVolItems do
				if gMRIcroOverlay[lOverlay].ScrnBuffer[lPos] > 0 then
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] := kVOI8bit;
		end; //if overlay loaded
	  end; //for each overlay
	end else if lComparison = 2 then begin //if union else MASK
	  for lOverlay := 1 to (knMaxOverlay-1) do begin
		if gMRIcroOverlay[lOverlay].ScrnBufferItems = lVolItems then begin
			for lPos := 1 to lVolItems do
				if gMRIcroOverlay[lOverlay].ScrnBuffer[lPos] > 0 then
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] := 0;
		end; //if overlay loaded
	  end; //for each overlay
	end; //if ..else MASK
	RefreshImagesTimer.Enabled := true;
end;  //ROIcomparisonClick

procedure TImgForm.RescaleImagesTimerTimer(Sender: TObject);
var
	lLayer: integer;
begin
	lLayer := ActiveLayer;
	 RescaleImagesTimer.Enabled := false;
	 RescaleImgIntensity(gBGImg,gMRIcroOverlay[lLayer],lLayer);
	 RefreshImages;
end;

procedure TImgForm.Fill3DBtnClick(Sender: TObject);
begin
	AutoROIForm.Show;
end;

procedure TImgForm.SmoothVOI1Click(Sender: TObject);
begin
	SmoothVOIForm.Showmodal
end;

procedure TImgForm.GenerateSPM5maskslesions1Click(Sender: TObject);
begin
     SmoothVOIForm.SmoothVOI_SPM5masks;
end;


procedure TImgForm.CreateOverlap(Sender: TObject);
var
	lNumberofFiles,lC,lOverlay,lPos: integer;
        lMin: byte;
	lFilename,lExt: string;
	lOverlapBuffer: ByteP;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image (''File''/''Open'') before adding an overlay.');
		exit;
	end;
        UpdateLayerMenu;
        lOverlay := 0;
	 for lC := 1 to (knMaxOverlay-1) do //-1: save final overlay for VOI
		  if (lOverlay = 0) and (gMRIcroOverlay[lC].ImgBufferItems = 0) then
			lOverlay := lC;
	 if lOverlay = 0 then begin
		showmessage('Too many overlays loaded to create an overlap image (Choose ''Close Overlays'' from the ''Overlay'' menu).');
		exit;
	 end;
	if not OpenDialogExecute(kVOIFilter,'Select VOIs you wish to combine',true) then exit;
	lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
	if  lNumberofFiles < 2 then begin
		Showmessage('Error: This function is designed to overlay MULTIPLE images. You selected less than two images.');
		exit;
	end;
        ProgressBar1.Min := 0;
        ProgressBar1.Max :=lNumberofFiles;
        ProgressBar1.Position := 0;
        getmem(lOverlapBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems);
        fillchar(lOverlapBuffer^,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems,0);
        for lC:= 1 to lNumberofFiles do begin
		lFilename := HdrForm.OpenHdrDlg.Files[lC-1];
		lExt := UpCaseExt(lFileName);
		gBGImg.VOIchanged := false;
		if not HdrForm.OpenAndDisplayHdr(lFilename,gMRIcroOverlay[lOverlay]) then exit;
		if not OpenImg(gBGImg,gMRIcroOverlay[lOverlay],false,false,false,gBGImg.ResliceOnLoad,false) then exit;
		ProgressBar1.Position := lC;
                //July07 - correct for scaling
                lMin := 255;
                for lPos := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do
			if gMRIcroOverlay[lOverlay].ScrnBuffer[lPos] < lMin then
				lMin := gMRIcroOverlay[lOverlay].ScrnBuffer[lPos];
                //end July07
		for lPos := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do
			if gMRIcroOverlay[lOverlay].ScrnBuffer[lPos] > lMin {July 07 0} then
				lOverlapBuffer[lPos] :=  lOverlapBuffer[lPos]+1;
		FreeImgMemory(gMRIcroOverlay[lOverlay]);
	end; //for each image
        GetMem(gMRIcroOverlay[lOverlay].ImgBufferUnaligned ,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems + 16); //July072007
        gMRIcroOverlay[lOverlay].ImgBuffer := ByteP($fffffff0 and (integer(gMRIcroOverlay[lOverlay].ImgBufferUnaligned)+15));
        gMRIcroOverlay[lOverlay].ImgBufferItems := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
	for lPos := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do
		gMRIcroOverlay[lOverlay].ImgBuffer[lPos] := lOverlapBuffer[lPos];
	freemem(lOverlapBuffer);
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],gMRIcroOverlay[lOverlay],0, lNumberofFiles,1,0,0,kNIFTI_INTENT_ESTIMATE,'N'+inttostr(lNumberofFiles) );
	UpdateLayerMenu;
	RescaleImgIntensity(gBGImg,gMRIcroOverlay[lOverlay],lOverlay);
	ProgressBar1.Position := 0;
	SaveAsVOIorNIFTI(gMRIcroOverlay[lOverlay].ImgBuffer,gMRIcroOverlay[lOverlay].ScrnBufferItems,1,1,false,gMRIcroOverlay[lOverlay].niftiHdr,'sum'+inttostr(lNumberofFiles));
	RefreshImagesTimer.Enabled := true;
end;//proc CreateOverlap

procedure TImgForm.Chisquare1Click(Sender: TObject);
var
	lNegativeNumbers: boolean;
	lVolVoxels,lPos,lLoop:integer;

        lBuffer: ByteP;
	lFilename: string;
	lTotal,lYes,lNo: array [1..2] of integer;
	lMRIcroHdr: TMRIcroHdr;
        //code below for chi2
	//lBufferAligned,lBufferUnAligned: byteP;
        //lnTotalThreshold,,lnVoxelsTested: integer
	//l32Buf : SingleP;
	//lMinExp,lChi,lChip,luChi, luChiP: double;
	//lMaxChi,lMinChi: single;
begin
	lVolVoxels := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
	if lVolVoxels < 1 then begin
		showmessage('Please load a background image (''File''/''Open'') before adding an overlay.');
		exit;
	end;
	CloseOverlayImgClick(nil);
	for lLoop := 1 to 2 do begin //open two images
		if lLoop = 1 then begin
			if not OpenDialogExecute(kImgFilter,'Select POSITIVE overlap image',false) then exit
		end else begin
			if not OpenDialogExecute(kImgFilter,'Select NEGATIVE overlap image',false) then exit;
		end;
		lFilename := HdrForm.OpenHdrDlg.Filename;
		if not HdrForm.OpenAndDisplayHdr(lFilename,gMRIcroOverlay[lLoop]) then exit;
		if not OpenImg(gBGImg,gMRIcroOverlay[lLoop],false,false,true,gBGImg.ResliceOnLoad,false) then exit;
		lTotal[lLoop] := round(gMRIcroOverlay[lLoop].NIFTIhdr.glmax);
		if (gMRIcroOverlay[lLoop].NIFTIhdr.intent_code <> kNIFTI_INTENT_ESTIMATE) then
			showmessage('Warning: header intent_code is not set to ESTIMATE. Compute Chi-squared only with cumulative maps created with this program.');
		if (gMRIcroOverlay[lLoop].NIFTIhdr.intent_name[1] <> 'N') then
			showmessage('Warning: header intention not N. Compute Chi-squared only with cumulative maps created with this program.');
		UpdateLayerMenu;
		RefreshImagesTimer.Enabled := true;
	end;
	if (lVolVoxels<> gMRIcroOverlay[1].ScrnBufferItems)
		or (lVolVoxels<> gMRIcroOverlay[2].ScrnBufferItems) then begin
		showmessage('Error loading images.');
		exit;
	end;
	//next - chi squared
	(*lnTotalThreshold:= ReadIntForm.GetInt('Only test voxels damaged in at least N patients [A+B]', 1,1,(lTotal[1]+lTotal[2]));
	GetMem(lBufferUnaligned ,(lVolVoxels *sizeof(single) )+16);
	lBufferAligned := ByteP($fffffff0 and (integer(lBufferUnaligned)+15));
	l32Buf := SingleP(lBufferAligned);
	lnVoxelsTested := 0;
	lNegativeNumbers := false;
	lMaxChi := 0;
	lMinChi := 0;
	for lPos := 1 to lVolVoxels do begin
		l32Buf[lPos] := 0;
		lYes[1] := gMRIcroOverlay[1].ScrnBuffer[lPos];
		lNo[1] := lTotal[1]-lYes[1];
		lYes[2] := gMRIcroOverlay[2].ScrnBuffer[lPos];
		lNo[2] := lTotal[2]-lYes[2];
		if (lYes[1] < 0) or (lNo[1] < 0) or (lYes[2] < 0) or (lNo[2] < 0) then
			lNegativeNumbers := true
		else if (lYes[1]+lYes[2]) >= lnTotalThreshold then begin//e.g. at least 30% of all patients
				  inc(lnVoxelsTested);
				  Chi2x2 (lYes[1], lNo[1], lYes[2], lNo[2],lMinExp,lChi,lChip,luChi, luChiP);
				  if (luChi) > lMaxChi then
					lMaxChi := (luChi)
				  else if (luChi < lMinChi) then
					lMinChi := luChi;
				  if (lYes[1]/lTotal[1]) > (lYes[2]/lTotal[2]) then
					 l32Buf[lPos] := luChi//100-(100*luChip) //positives more likely than negative
				  else
					  l32Buf[lPos] := -luChi;//-100+(100*luChip); //negatives more common
		end;//> threshold
	end; //for each voxel
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lMRIcroHdr,lMinChi, lMaxChi,1{df},0,lnVoxelsTested,kNIFTI_INTENT_CHISQ,inttostr(lnVoxelsTested) );
	if lNegativeNumbers then
		Showmessage('Serious error: some group sizes were negative. This should be impossible with a Chi-Squared.');
	SaveAsVOIorNIFTI(lBufferAligned,lVolVoxels,4,1,false,lMRIcroHdr.NiftiHdr,'chi'+inttostr(lnTotalThreshold));
	//next - save log10 p values...
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lMRIcroHdr,lMinChi, lMaxChi,1{df},0,lnVoxelsTested,NIFTI_INTENT_LOG10PVAL,inttostr(lnVoxelsTested) );
	for lPos := 1 to lVolVoxels do
		if l32Buf[lPos] > 0 then
			 l32Buf[lPos] := -log(abs(gammq(0.5, 0.5 * l32Buf[lPos])),10)
		else
			l32Buf[lPos] :=0;
	SaveAsVOIorNIFTI(lBufferAligned,lVolVoxels,4,1,false,lMRIcroHdr.NiftiHdr,'log10p'+inttostr(lnTotalThreshold));
	//next - free float buffer
	FreeMem(lBufferUnaligned);
	StatusLabel.Caption := 'Voxels tested: '+inttostr(lnVoxelsTested);
        *)
	//next - subtraction
	GetMem(lBuffer ,(lVolVoxels ));
	lNegativeNumbers := false;
	fillchar(lBuffer^,lVolVoxels,100);
	for lPos := 1 to lVolVoxels do begin
		lYes[1] := gMRIcroOverlay[1].ScrnBuffer[lPos];
		lNo[1] := lTotal[1]-lYes[1];
		lYes[2] := gMRIcroOverlay[2].ScrnBuffer[lPos];
		lNo[2] := lTotal[2]-lYes[2];
		if (lYes[1] < 0) or (lNo[1] < 0) or (lYes[2] < 0) or (lNo[2] < 0) then
			lNegativeNumbers := true
		else if (lYes[1] >0) or (lYes[2] > 0) then begin
			lBuffer[lPos] := round((100* ((lYes[1]/lTotal[1])-(lYes[2]/lTotal[2])))+100);
		end;//> threshold
	end; //for each voxel
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lMRIcroHdr,-100, 100,1,0,0,kNIFTI_INTENT_ESTIMATE,'%'+inttostr(lTotal[1])+':'+inttostr(lTotal[2]) );
	lMRIcroHdr.NIFTIhdr.scl_inter:= -100;
	if lNegativeNumbers then
		Showmessage('Serious error: some group sizes were negative. This should be impossible with a subtraction analysis.');
	SaveAsVOIorNIFTI(lBuffer,lVolVoxels,1,1,false,lMRIcroHdr.NiftiHdr,'Sub'+inttostr(lTotal[1])+'_'+inttostr(lTotal[2]));
	FreeMem(lBuffer);
end; //procedure Chisquare1Click

procedure TImgForm.ROIVOI1Click(Sender: TObject);
var
	lNumberofFiles,lC: integer;
	lFilename: string;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image (''File''/''Open'') before adding an overlay.');
		exit;
	end;
        if gBGImg.Resliced then begin
           if not HdrForm.OpenAndDisplayHdr(gMRIcroOverlay[kBGOverlayNum].HdrFileName,gMRIcroOverlay[kBGOverlayNum]) then exit;
           if not OpenImg(gBGImg,gMRIcroOverlay[0],true,false,false,false,false) then exit;
        end;

	showmessage('Warning: the currently open background image must have the dimensions (size, space between slices, etc) as the image used when creating the ROIs.');
	if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0 then
		CloseVOIClick(nil);
	if not OpenDialogExecute('MRIcro ROI (.roi)|*.roi','Select MRIcro format ROIs to convert',true) then exit;
	lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := HdrForm.OpenHdrDlg.Files[lC-1];
		OpenMRIcroROI (lFileName);
		lFilename := changefileextx(lFilename,'.voi'); //Xversion 10/2007 - removes .nii.gz not just gz
		SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
		CloseVOIClick(nil);
		ProgressBar1.Position := lC;
	end;
	ProgressBar1.Position := 0;
end;

procedure TImgForm.LUTinvertBtnClick(Sender: TObject);
begin
end; //proc LUTdropSelect

procedure TImgForm.LutFromZeroBtnClick(Sender: TObject);
var
   lLayer: integer;
begin
	 lLayer := ActiveLayer;
	 gMRIcroOverlay[lLayer].LUTfromZero := LUTfromZeroBtn.down;
	 LUTdropLoad(lLayer);
	 RescaleImagesTimer.Enabled := true;
end;

procedure TImgForm.ShowMultisliceClick(Sender: TObject);
begin
	MultiSliceForm.Show;
end;

procedure DescribeVOIonLabels (lOverlayNum: integer; lShowFilename: boolean);
const
     kT = kTextSep;
     PositiveInfinityBits :  Int64 = $7FF0000000000000;
     NegativeInfinityBits :  Int64 = $FFF0000000000000;
VAR
    dPositiveInfinity     :  DOUBLE ABSOLUTE PositiveInfinityBits;
    dNegativeInfinity     :  DOUBLE ABSOLUTE NegativeInfinityBits;
var
	l16Buf : SmallIntP;
	l32Buf : SingleP;
    l8Buf: byteP;
  type
    TVxStat =  RECORD //peristimulus plot
            n,  nNot0, minPos,maxPos: integer;
            sum,sumNot0,min,max: double;
    end;
function clearVxStat: TVxStat;
begin
     result.sum:=0;
     result.sumNot0:= 0;
     result.n:=0;
     result.nNot0 := 0;
     result.minPos:= 0;
     result.maxPos:=0;
     result.min := dPositiveInfinity;
     result.max := dNegativeInfinity;
end;
function roiIntensity(var lHdr: TMRIcroHdr; lPos: integer): integer;
var
	l16Buf : SmallIntP;
begin
  if (lHdr.ImgBufferBPP  = 2) then begin
     l16Buf := SmallIntP(lHdr.ImgBuffer );
     result := l16Buf^[lPos];
  end else
	 result := lHdr.ImgBuffer^[lPos];
end;
function overlayIntensity(var lHdr: TMRIcroHdr; lPos: integer): single;

begin
  if (lHdr.ImgBufferBPP  = 4) then begin
	result := l32Buf^[lPos];
  end else if (lHdr.ImgBufferBPP  = 2) then begin
	result := l16Buf^[lPos];
  end else
	 result := l8Buf^[lPos];
end;
procedure scaleIntensity(var valn: double);
begin
   valn := (valn * gMRIcroOverlay[lOverlayNum].NIFTIhdr.scl_slope)+gMRIcroOverlay[lOverlayNum].NIFTIhdr.scl_inter
end;
var
   lROI,lVx: integer;
   lStat: array of  TVxStat;
   lVal,loMax,hiMax: double;
   lStartTime: DWord;
   lBinaryOverlay: boolean;
   lLabelStr,lStr: string;
begin
  if (not gMRIcroOverlay[kBGOverlayNum].UsesLabels) or (High(gBGImg.LabelRA) < 1) then exit;
  if (gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems) then exit;
  if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP > 2) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 2) then exit;
  //pointers to image data
  l32Buf := SingleP(gMRIcroOverlay[lOverlayNum].ImgBuffer );
  l16Buf := SmallIntP(gMRIcroOverlay[lOverlayNum].ImgBuffer );
  if gMRIcroOverlay[lOverlayNum].ScrnBufferItems = gMRIcroOverlay[lOverlayNum].ImgBufferItems then
      l8Buf := gMRIcroOverlay[lOverlayNum].ImgBuffer
  else
      l8Buf := gMRIcroOverlay[lOverlayNum].ScrnBuffer;

  lStartTime := GetTickCount;
  setlength(lStat, High(gBGImg.LabelRA)+1);
  for lROI := 0 to High(gBGImg.LabelRA) do
   lStat[lROI] := clearVxStat;
  for lVx := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
       lROI :=roiIntensity(gMRIcroOverlay[kBGOverlayNum], lVx);
       inc(lStat[lROI].n);
       lVal := overlayIntensity(gMRIcroOverlay[lOverlayNum],lVx);
       lStat[lROI].sum := lStat[lROI].sum+ lVal;
       if lVal <> 0 then begin
          lStat[lROI].sumNot0 := lStat[lROI].sumNot0+ lVal;
          inc(lStat[lROI].nNot0);
       end;
       if lVal > lStat[lROI].max then
          lStat[lROI].max := lVal;
       if lVal < lStat[lROI].min then
             lStat[lROI].min := lVal;
   end; //for each voxel
   //calibrate values with rescale slope/intercept, see if overlay has variablility
   loMax := dPositiveInfinity;
   hiMax := dNegativeInfinity;
   if gMRIcroOverlay[lOverlayNum].NIFTIhdr.scl_slope = 0 then gMRIcroOverlay[lOverlayNum].NIFTIhdr.scl_slope := 1;
   for lROI := 0 to High(gBGImg.LabelRA) do begin
         if (lStat[lROI].nNot0 > 0) and (lStat[lROI].max > hiMax) then hiMax := lStat[lROI].max;
       if (lStat[lROI].nNot0 > 0) and (lStat[lROI].min < loMax) then loMax := lStat[lROI].max;
       scaleIntensity  (lStat[lROI].max);
       scaleIntensity  (lStat[lROI].min);
       scaleIntensity  (lStat[lROI].sum);
       scaleIntensity  (lStat[lROI].sumNot0);
    end;
    lBinaryOverlay := (hiMax <= loMax);
   if lShowFilename then begin
       if gMRIcroOverlay[lOverlayNum].HdrFileName = '' then
               lLabelStr := 'VOI'+kT
       else
           lLabelStr := gMRIcroOverlay[lOverlayNum].HdrFileName+kT;
   end else
       lLabelStr := '';
   TextForm.MemoT.Lines.add(lLabelStr+'Custom Region Analysis');
    //add header
    lStr := 'Index'+kT+'Name'+kT+'numVox'+kT+'numVoxNotZero'+kT+'fracNotZero';
    if not lBinaryOverlay then
       lStr := lStr+kT+'peak'+kT+'min'+kT+'mean'+kT+'meanNotZero';
    TextForm.MemoT.Lines.Add(lLabelStr+lStr);
   //report values
   for lROI := 0 to High(gBGImg.LabelRA) do begin
       if (lStat[lROI].nNot0 > 0) then begin
          lStr := inttostr(lROI)+kT+gBGImg.LabelRA[lROI]
            +kT+inttostr(lStat[lROI].n)+kT+inttostr(lStat[lROI].nNot0)+kT+ realtoStr(lStat[lROI].nNot0/lStat[lROI].n,3);
          if not lBinaryOverlay then
             lStr := lStr+kT+floattostr(lStat[lROI].max)+kT+floattostr(lStat[lROI].min)
               +kT+floattostr(lStat[lROI].sum/lStat[lROI].n) +kT+floattostr(lStat[lROI].sumNot0/lStat[lROI].nNot0);
          TextForm.MemoT.Lines.Add(lLabelStr+lStr );
       end;

   end;
end;

function Mode (lOverlayNum: integer): double;
const
  kBins = 4095;
var
  lInc,lS,lMaxI: integer;
  lV,lMin,lMax,lScale: single;
  lRA: LongIntP0;
begin
  result := nan; //error
  if (gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems ) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1)then
      exit;
  lMin :=  RawBGIntensity(1);
  lMax := lMin;
	for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
		if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then begin
			lV := RawBGIntensity(lInc);
			if lV < lMin then
				lMin := lV;
			if lV > lMax then
				lMax := lV;
		end; //if VOI voxel
	end; //for each voxel
  if lMin = lMax then begin //no variability
    result := Raw2ScaledIntensity(gMRIcroOverlay[kBGOverlayNum],lMin);
    exit;
  end;
  lScale := kBins/(lMax-lMin);
  getmem(lRA,(kBins+1) * sizeof(longint) ); //0..kBins
	for lInc := 0 to kBins do
    lRA^[lInc] := 0;
	for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
		if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then begin
			lV := RawBGIntensity(lInc);
      lS := round((lV-lMin)*lScale);
      inc(lRA^[lS]);
		end; //if VOI voxel
	end; //for each voxel
  lMaxI := 0;
	for lInc := 1 to kBins do
    if lRA^[lInc] > lRA^[lMaxI] then
      lMaxI := lInc;
  result := lMin+ (lMaxI/kBins * (lMax-lMin));
  result := Raw2ScaledIntensity(gMRIcroOverlay[kBGOverlayNum],result);
  freemem(lRA);
end;

procedure TImgForm.ShowDescriptive (lOverlayNum: integer; lShowFilename: boolean);
var
	lROIVol: array [1..3] of integer;
	lInc: integer;
	lCenterOfMass,lROISum,lROISumSqr,lROImin,lROImax:array [1..3] of double;
	lMode,lCC,lVal,lSD,lROImean: double;
	lLabelStr,lStr: string;
procedure  AddVal( lRA: integer);
begin
			inc(lROIVol[lRA]);
			lROISum[lRA] := lROISum[lRA]+lVal;
			lROISumSqr[lRA] := lROISumSqr[lRA] + sqr(lVal);
			if lVal > lROImax[lRA] then
				lROImax[lRA] := lVal;
			if lVal < lROImin[lRA] then
				lROImin[lRA] := lVal;
end; //proc AddVal
begin //proc ShowDescript
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then
      exit;

   if lShowFilename then
      lLabelStr := gMRIcroOverlay[lOverlayNum].HdrFileName
   else
       lLabelStr := '';
   for lInc := 1 to 3 do begin
		lROIVol[lInc] := 0;
		lROISum[lInc] := 0;
		lROISumSqr[lInc] := 0;
		lROImin[lInc] := maxint;
		lROImax[lInc] := -maxint;
   end;
	for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
		if gMRIcroOverlay[lOverlayNum].ScrnBuffer[lInc] > 0 then begin
      //fx(lInc);
			lVal := RawBGIntensity(lInc);
			AddVal(1);
			if lVal <> 0 then
				AddVal(2);
			if lVal > 0 then
				AddVal(3);
		end; //if VOI voxel
	end; //for each voxel
	//next - compute StDev
	//compute descriptives for each set of values
           if lOverlayNum =  kVOIOverlayNum then
              lStr :=  'VOI notes '
           else
               lStr := 'Overlay #'+inttostr(lOverlayNum);
        if not lShowFilename then begin
	   TextForm.MemoT.Lines.Add(lStr+'  '+gMRIcroOverlay[lOverlayNum].HdrFileName);
        end;
        //TextForm.Memo1.Lines.Add('CoM');
        if  CenterOfMass (lOverlayNum, lCenterOfMass[1],lCenterOfMass[2],lCenterOfMass[3]) > 0 then
            TextForm.MemoT.Lines.Add(' '+lLabelStr+' Center of mass XYZ '+RealToStr(lCenterOfMass[1],2)+'x'+RealToStr(lCenterOfMass[2],2)+'x'+RealToStr(lCenterOfMass[3],2));
	for lInc := 1 to 3 do begin
		if lROIVol[lInc] > 1 then begin
			lSD := (lROISumSqr[lInc] - ((Sqr(lROISum[lInc]))/lROIVol[lInc]));
			if  (lSD > 0) then
				lSD :=  Sqrt ( lSD/(lROIVol[lInc]-1))
			else
				lSD := 0;
		end else
			lSD := 0;
		//next compute mean
		if lROIVol[lInc] > 0 then begin
			lROImean := lROISum[lInc]/lROIVol[lInc];
		        //next - calibrate values
		        lROImin[lInc] := Raw2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],lROImin[lInc]);
		        lROIMean := Raw2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],lROIMean);
		        lROImax[lInc] := Raw2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],lROImax[lInc]);
		        lSD := Raw2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],lSD);

		end else begin //2/2008
                     lROImin[lInc] := 0;
                     lROImax[lInc] := 0;
                     lROImean := 0;
                end;
		lcc := ((lROIVol[lInc]/1000)*gBGImg.ScrnMM[1]*gBGImg.ScrnMM[2]*gBGImg.ScrnMM[3]);
		case lInc of
			3: lStr := 'VOI  >0 ';
			2: lStr := 'VOI <>0 ';
			else lStr := 'VOI     ';
		end;
		lStr := lStr+' nvox[cc]=min/mean/max=SD:'+kTextSep+inttostr(round(lROIVol[lInc]))+kTextSep+RealToStr(lCC,2)+kTextSep+'='+kTextSep+RealToStr(lROIMin[lInc],4)+kTextSep+realToStr(lROIMean,4)+kTextSep+realToStr(lROIMax[lInc],4)+kTextSep+'='+kTextSep+realtostr(lSD,4);
		TextForm.MemoT.Lines.Add(lLabelStr+ lStr);
	end;
  lMode := Mode(lOverlayNum);
  if lMode <> NaN then
    TextForm.MemoT.Lines.Add('Mode:'+kTextSep+floattostr(lMode));
	//June07 if (gMRIcroOverlay[kBGOverlayNum].UsesCustomPalette) or (lShowFilename) then
		DescribeVOIonLabels(lOverlayNum,lShowfilename);
	TextForm.MemoT.Lines.Add('');
        ImgForm.SaveDialog1.Filename := ExtractFileDirWithPathDelim(gMRIcroOverlay[lOverlayNum].HdrFileName)+'desc.csv';
end;

procedure TImgForm.DrawInterpolateSlicesClick (Sender: TObject);
var
  lStrings: TStringList;
  lOrient:integer;
  lOK: boolean;
begin
  if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems=0 then begin
	  showmessage('Please open the drawing you wish to interpolate.');
	  exit;
  end;
  lOrient := gBGImg.VOIUndoOrient;
  if (lOrient < 1) or (lOrient > 3) then begin
    showmessage('Unknown orient');
    exit;
  end;
	(*	4: UndoVolVOI;
		3: ReadCorVOI(ImgForm.UndoImg,gBGImg.VOIUndoSlice);
		2: ReadSagVOI(ImgForm.UndoImg,gBGImg.VOIUndoSlice);
		1: ReadAxialVOI(ImgForm.UndoImg,gBGImg.VOIUndoSlice);
	end;     *)

  CreateUndoVol;
  lStrings := TStringList.Create;

  TextForm.MemoT.Lines.Clear;
  lStrings.Add('Background');
  lOK := Interpolate_Slices (gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gBGImg.ScrnDim[1],gBGImg.ScrnDim[2],gBGImg.ScrnDim[3],lOrient, lStrings);
  if not lOK then begin
    TextForm.MemoT.Lines.AddStrings(lStrings);
    TextForm.Show;
  end;
	ImgForm.RefreshImagesTimer.Enabled := true;
	lStrings.Free;
end;

procedure TImgForm.DescriptiveMenuItemClick(Sender: TObject);
var
	lInc,lOverlayNum,lImgSz: integer;
begin
  // DrawInterpolateSlicesClick (Sender); exit; //666
	lImgSz := 0;
	for lOverlayNum := 1 to knMaxOverlay do
		if gMRIcroOverlay[lOverlayNum].ScrnBufferItems > lImgSz then
			lImgSz := gMRIcroOverlay[lOverlayNum].ScrnBufferItems;
	if (lImgSz < 1) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < lImgSz) then begin
		Showmessage('You need to create or load an overlay (Overlay/Open or Draw/OpenVOI) to get overlay statistics.');
		exit;
	end;
        TextForm.MemoT.Lines.Clear;
        for lInc := 1 to knMaxOverlay do begin
            ShowDescriptive(lInc,false);
        end;
        //SaveDialog1.Filename := ExtractFileDirWithPathDelim(HdrForm.OpenHdrDlg.Files[0])+'desc.csv';
        TextForm.Show;
end;

procedure TImgForm.BatchROImean1Click(Sender: TObject);
var
	lInc,lNumberofFiles: integer;
  lFilename:string;
begin                   

//OpenAndDisplayImg(lStr,false);
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image for rescaling.');
		exit;
	end;
  for lInc := 1 to (knMaxOverlay-1) do
	    FreeImgMemory(gMRIcroOverlay[lInc]);
  UpdateLayerMenu;
  if not OpenDialogExecute(kImgFilter,'Select regions of interest you wish to analyze',true) then exit;
  lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
  if  lNumberofFiles < 1 then
    exit;
  TextForm.MemoT.Lines.Clear;
  for lInc:= 1 to lNumberofFiles do begin
    lFilename := HdrForm.OpenHdrDlg.Files[lInc-1];
	  OverlayOpenCore ( lFilename, 2);
    ShowDescriptive(2,true);
  end;
  FreeImgMemory(gMRIcroOverlay[2]);
  UpdateLayerMenu;
  TextForm.Show;
end;
(*procedure TImgForm.BatchROImean1Click(Sender: TObject);
var
	lInc,lNumberofFiles: integer;
  lFilename:string;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image for rescaling.');
		exit;
	end;
  for lInc := 1 to (knMaxOverlay-1) do
	    FreeImgMemory(gMRIcroOverlay[lInc]);
  UpdateLayerMenu;
  if not OpenDialogExecute(kImgFilter,'Select regions of interest you wish to analyze',true) then exit;
  lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
  if  lNumberofFiles < 1 then
    exit;
  TextForm.MemoT.Lines.Clear;
  for lInc:= 1 to lNumberofFiles do begin
    lFilename := HdrForm.OpenHdrDlg.Files[lInc-1];
	  OverlayOpenCore ( lFilename, 2);
    ShowDescriptive(2,true);
  end;
  FreeImgMemory(gMRIcroOverlay[2]);
  UpdateLayerMenu;
  TextForm.Show;
end; *)

procedure TImgForm.FormResize(Sender: TObject);
begin
	//if ImgForm.WindowState = wsMaximized then
	RefreshImagesTimer.Enabled := true;
end;

function ParamStrFilename (var lParamPos: integer): string;
var
	I: integer;
	lStr: string;
begin
  result := '';
  if (ParamCount < lParamPos) then exit;
  I := lParamPos;
  repeat
		if I = lParamPos then
			lStr := ParamStr(I)
		else
			lStr := lStr +' '+ ParamStr(I);
		inc(I);
  until (I>ParamCount) or (fileexistsex(lStr));
  lParamPos := I;
  if fileexistsex(lStr) then
	result := lStr;
  //Showmessage(lStr+ '-> '+result);
end;

{$DEFINE notTEST}

{$IFDEF TEST}
(*procedure Merge;
var
   lInc,lLayer: integer;
   lOut: double;
begin
        showmessage(inttostr(kBGOverlayNum));

	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image for merging.');
		exit;
	end;
        lLayer := 1;
	if gMRIcroOverlay[lLayer].ScrnBufferItems < 1 then begin
		showmessage('Please load an overlay image for merging.');
		exit;
	end;
        //lImgSamples := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
        for lInc := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do begin
            //if (gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lInc] <> 0) then
            //lOut := (gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lInc]/255) * 80;
            //lOut := lOut+((gMRIcroOverlay[lLayer].ScrnBuffer[lInc]/255)*130) ;
            lOut := 0;
            if (gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lInc]+gMRIcroOverlay[lLayer].ScrnBuffer[lInc])> 52 then
               lOut := 100;
            gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lInc] := round(lOut);

        end;
        SaveAsVOIorNIFTI(gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems,1,1,false,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName);
end;  *)
{$ENDIF}




procedure TImgForm.FormShow(Sender: TObject);
var
   lStr: String;
   lMaximize,lRender,lMultislice : boolean;
   lCommandChar: Char;
   I,lError,lOverlayNum,lInc,lLUT: integer;
   lSingle: single;
procedure ReadCmdVal;//nested
begin
		   inc(I);
		   lStr := ParamStr(I);
		   lStr := string(StrUpper(PChar(lStr))) ;
end; //nested ReadCmdVal
begin

    (*ResliceImg (
    'C:\cygwin\home\express\20070420_132327fMRIcontin30x30x36s004a001.feat\example_func.nii.gz',
    'C:\fatigue\v1.nii.gz',
    'C:\cygwin\home\express\20070420_132327fMRIcontin30x30x36s004a001.feat\reg\example_func2standard.mat',
    'C:\fatigue\crapp.nii.gz') ;
      *)
    {$IFDEF TEST}
    //ResliceImgNIfTI ('c:\fx\target.nii.gz','c:\fx\source.nii.gz','c:\fx\junk2.nii.gz');
    //ResliceImgNIfTI ('c:\fx\target.nii.gz','c:\fx\target.nii.gz','c:\fx\junk24.nii.gz');
    //ResliceImgNIfTI ('c:\fx\hires.nii.gz','c:\fx\higher.nii.gz','c:\fx\junk.nii.gz');

    //ResliceImg('C:\fatigue\example_func.nii.gz' ,'C:\fatigue\avg152T1.hdr','C:\fatigue\example_func2standard.mat');
    //ResliceImg('C:\fatigue\lowres.nii.gz' ,'C:\fatigue\hires.nii.gz','C:\fatigue\example_func2standard.mat','c:\fatigue\fzz.nii.gz');
    lStr := 'c:\drawx\c.nii.gz';
    OpenAndDisplayImg(lStr,false);
    lStr := 'C:\drawx\c.voi';
    OpenVOICore(lStr);
    //LoadOverlay(lStr);
    //lStr := 'c:\fatigue\jx.nii';
    //OpenAndDisplayImg(lStr,false);
    //'C:\fatigue\example_func2standard.mat');
    //Graph4DForm.show;
    //Graph4DForm.FSLbatch1Click(nil);
    //Graph4DForm.OpenDataClick(nil);

    exit;
    {$ENDIF}
        //Graph4DForm.rfx;
	if (ParamCount  < 1) then begin
              {$IFNDEF TEST}
                  lStr := gMRIcroOverlay[kBGOverlayNum].HdrFilename;
                  if fileexists (lStr) then
                    OpenAndDisplayImg(lStr,false)
                  else
                    ImgForm.OpenTemplateMRU(nil);
                 //  Reorient1Click(nil);
                exit;
              {$ELSE}
                lStr := extractfiledir(ParamStr(0))+'\templates\ch2bet.nii.gz';
                //lStr := extractfiledir(ParamStr(0))+'\templates\xz.voi';
                //lStr := 'c:\zx\c1fxcbruce.nii';
                if fileexists(lStr) then begin
                   OpenAndDisplayImg(lStr,false);
                   //OverlayOpenCore (kBGOverlayNum);
                //showmessage('22');xxx
                   //gMRIcroOverlay[1].LUTfromZero := true;
                   //gMRIcroOverlay[1].WindowScaledMin := -4;
                   //gMRIcroOverlay[1].WindowScaledMax := -2;
                   //RescaleImgIntensity(gBGImg,gMRIcroOverlay[1]);
	           RefreshImages;
                end;

                lStr := extractfiledir(ParamStr(0))+'\example\attention.nii.gz';
                lInc := 1+kBGOverlayNum;
                if fileexists(lStr) then begin
                   OverlayOpenCore (lStr,lInc);
                   gMRIcroOverlay[lInc].LUTfromZero := true;
                   gMRIcroOverlay[lInc].WindowScaledMin  := 4;
                   gMRIcroOverlay[lInc].WindowScaledMax  := 4;
                   RescaleImgIntensity(gBGImg,gMRIcroOverlay[lInc],lInc);
                end else
                    showmessage('Can not find '+lStr);
                //Merge;
		exit;
                {$ENDIF}
	end;
	lMaximize := false;
	lRender := false;
	lMultislice := false;
	lOverlayNum := 0;
	I := 1;
	lStr := ParamStrFilename(I);
	if lStr <> '' then
		ImgForm.OpenAndDisplayImg(lStr,True)
	 else begin //no requested image
		ImgForm.OpenTemplateMRU(nil);
		I := 1;//exit;
	end;
	I := I-1;
		//ShowMultisliceClick(nil);
	if I >= ParamCount then exit;
	gBGIMg.SaveDefaultIni := false; //do not store changes loaded by script
	repeat
	 lStr := '';
	 repeat
		inc(I);
		if I = 1 then
			lStr := ParamStr(I)
		else begin
			if lStr <> '' then
			   lStr := lStr +' '+ ParamStr(I)
			else
				lStr := ParamStr(I);
		end;
		if (length(lStr)>1) and (lStr[1] = '-')  then begin //special command
		   lCommandChar := UpCase(lStr[2]);
		   case lCommandChar of
				'B': begin //background transparency
						  ReadCmdVal;
						  Val(lStr,lSingle,lError);
						  if lError = 0 then begin
							gBGImg.BGTransPct  := round(lSingle);
  SetSubmenuWithTag(BGTransPctMenu, gBGImg.BGTransPct);
                                                  end;
					end;
				'C': begin //color look up table
					ReadCmdVal;
					if (Length(lStr)>1) then begin
					  if lStr[1] = '-' then begin //LUT index number
						  Val(lStr,lSingle,lError);
						  if lError = 0 then
							lLUT := abs(round(lSingle))
						  else
							lLUT := -1;
					  end else begin
						lStr := ParseFileName(ExtractFileName(lStr));
						lLUT := -1;
						for lInc := 1 to (LUTdrop.Items.Count-1) do
						  if lStr = string(StrUpper(PChar(LUTdrop.Items.Strings[lINc]))) then
							lLUT := lInc;
					  end; //else text LUTname
					  if lLUT >= 0 then begin
						gMRIcroOverlay[lOverlayNum].LUTindex := lLUT;
						LUTdropLoad(lOverlayNum);
					  end;
					end; //str length > 1
				end;
				'D': gBGIMg.SaveDefaultIni := true; //save defaults
                                'F': gBGImg.ResliceOnLoad := false; //turn off reslicing... loads files flat
				'H': begin //High intensity scale
						  ReadCmdVal;
						  Val(lStr,lSingle,lError);
						  if lError = 0 then
								 gMRIcroOverlay[lOverlayNum].WindowScaledMax  := (lSingle);
					end; //not 'A' or 'H'}
				'L': begin //Low intensity scale
						  ReadCmdVal;
						  Val(lStr,lSingle,lError);
						  if lError = 0 then
								 gMRIcroOverlay[lOverlayNum].WindowScaledMin  := (lSingle);
					end;
				'M': begin //multislice
						lMultislice := true;
						ReadCmdVal;
						if (lStr <> '') and (lStr <> '-')and (FileexistsEx(lStr)) and (lOverlayNum < (knMaxOverlay-1)) then
							gMultiSliceStartupFilename := (lStr);

							//CopyFileEXoverwrite (lStr,extractfiledir(paramstr(0))+'\multislice\default.ini');
					 end; //if 'M'

				'O': begin//Overlay
						ReadCmdVal;
						if (lStr <> '') and (FileexistsEx(lStr)) and (lOverlayNum < (knMaxOverlay-1)) then begin
							inc(lOverlayNum);
							OverlayOpenCore (lStr,lOverlayNum);
						end;
					 end; //if 'O'
				'R': begin//Overlay
						lRender := true;//Render
						ReadCmdVal;
						if (lStr <> '') and (lStr <> '-')and (FileexistsEx(lStr)) and (lOverlayNum < (knMaxOverlay-1)) then
							gRenderStartupFilename := (lStr);
					 end; //if 'R'
				'S': begin //smooth
					ReadCmdVal;
					Val(lStr,lSingle,lError);
					if lError = 0 then begin
						if odd(round(lSingle)) then begin
							gBGImg.StretchQuality := sqHigh;
							Menu2DSmooth.checked := true;
						end else begin
							gBGImg.StretchQuality := sqLow;
							Menu2DSmooth.checked := false;
						end;
						if lSingle > 1 then
							gBGIMg.OverlaySmooth := true
						else
							gBGIMg.OverlaySmooth := false;
						OverlaySmoothMenu.Checked := gBGIMg.OverlaySmooth;
					end;//error=0
				end;
				'T': begin //overlay transparency
						  ReadCmdVal;
						  Val(lStr,lSingle,lError);
						  if lError = 0 then
								 gBGImg.OverlayTransPct  := round(lSingle);
                  SetSubmenuWithTag(OverlayTransPctMenu, gBGImg.OverlayTransPct);

				end;
				'V': begin //open voi
						  ReadCmdVal;
						if (lStr <> '') and (FileexistsEx(lStr)) then
						OpenVOICore(lStr);
					end;
				'X': lMaximize := true; //open maximized
				'Z': gMRIcroOverlay[lOverlayNum].LUTfromZero := true;
		   end; //case lStr[2]
		   lStr := '';
		end; //special command
	 until (I=ParamCount) or (fileexists(lStr)) {or (gAbort)};
	until I >= ParamCount;
   LayerDropSelect(nil);
   //RescaleImagesTimer.Enabled := true;
   //RescaleImagesTimerTimer(nil);
   for lInc := 0 to lOverlayNum do
	 RescaleImgIntensity(gBGImg,gMRIcroOverlay[lInc],lInc);
	 RefreshImages;
   if lMultiSlice then
		ShowMultisliceClick(nil);
   if lRender then
		ShowRenderClick(nil);
   if lMaximize then begin
	   ImgForm.WindowState := wsMaximized;
	   RefreshImagesTimer.Enabled := true;
   end;

end;

procedure TImgForm.FlipLRmenuClick(Sender: TObject);
var
	lC: integer;
	lStr: string;
begin
	(sender as TMenuItem).checked := not (sender as TMenuItem).checked;
	 gBGImg.Mirror := (sender as TMenuItem).checked ;
	 gBGImg.VOImirrored := true;
	 for lC := 0 to knMaxOverlay do
		if gMRIcroOverlay[lC].ScrnBufferItems > 0 then
			RescaleImgIntensity(gBGImg,gMRIcroOverlay[lC],lC);
	 RefreshImagesTimer.Enabled := true;
	if gBGImg.Mirror then
		lStr := 'radiological [right on left side]'
	else
		lStr := 'neurological [left on left side]';
	showmessage('Warning: left-right flips can be confusing. From now on, this software will attempt to show NIfTI images in '+lStr+' orientation.');
  if MultiSliceForm.Visible then
       MultiSliceForm.CreateMultiSlice;
end;

procedure TImgForm.Menu2DSmoothClick(Sender: TObject);
begin
	if Sender <> nil then
		(sender as TMenuItem).checked := not (sender as TMenuItem).checked;
	 if Menu2DSmooth.checked then
		 gBGImg.StretchQuality := sqHigh
	 else
		  gBGImg.StretchQuality := sqLow;
	 RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.VALclick(Sender: TObject);
begin
        showmessage('Please use NPM');
	//ComputeValFile( (sender as Tmenuitem).tag);
end;

procedure TImgForm.VOI2NIIClick(Sender: TObject);
var
	lNumberofFiles,lC: integer;
	lFilename: string;
        lNIFTIhdr: TNIFTIhdr;
begin
	CloseImagesClick(nil);
	if not OpenDialogExecute('VOI Drawings (.VOI)|*.VOI','Select VOI format images to convert',true) then exit;
	lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := HdrForm.OpenHdrDlg.Files[lC-1];
		ImgForm.OpenAndDisplayImg(lFilename,True);
		lFilename := changefileextx(lFilename,'.nii'); ////Xversion 10/2007 - removes .nii.gz not just gz
                //Start 10/2007: adjust scl_slope;? 10/2007
                CopyNiftiHdr(gMRIcroOverlay[kBGOverlayNum].NiftiHdr,lNIFTIhdr);
                lNIFTIhdr.scl_slope := 1;
                lNIFTIhdr.scl_inter := 0;
                //end
		SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,lNiftiHdr);
		CloseVOIClick(nil);
		ProgressBar1.Position := lC;
	end;
	ProgressBar1.Position := 0;
end;//VOI->NII

procedure TImgForm.NIIVOI1Click(Sender: TObject);
var
	lNumberofFiles,lC: integer;
	lFilename: string;
begin
	CloseImagesClick(nil);
	if not OpenDialogExecute(kImgFilter {10/2007},'Select NIfTI format images to convert',true) then exit;
	lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := HdrForm.OpenHdrDlg.Files[lC-1];
		ImgForm.OpenAndDisplayImg(lFilename,True);
		lFilename := changefileextx(lFilename,'.voi'); ////Xversion 10/2007 - removes .nii.gz not just gz
		//SaveAsVOIorNIFTIcore (lFilename, lByteP, lVoxels, 1, gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
		SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
		CloseVOIClick(nil);
		ProgressBar1.Position := lC;
	end;
	ProgressBar1.Position := 0;
end;

procedure TImgForm.TtoP1Click(Sender: TObject);
var
	lBufferAligned,lBufferUnAligned: ByteP;
	l32Buf,l32BufSrc : SingleP;
	l16BufSrc : SmallIntP;
	lSlope,lIntercept: single;
	//l32Buf : SingleP;

	lMRIcroHdr: TMRIcroHdr;
	lVolVoxels,lPos: integer;
begin
//alfa - currently open image
	lVolVoxels := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
	if lVolVoxels < 1 then begin
		showmessage('Please load a background image (''File''/''Open'') before adding an overlay.');
		exit;
	end;
	GetMem(lBufferUnaligned ,(lVolVoxels *sizeof(single) )+16);
	lBufferAligned := ByteP($fffffff0 and (integer(lBufferUnaligned)+15));
	l32Buf := SingleP(lBufferAligned);
	//next load values
	case gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP of
		4: begin
		   l32BufSrc := SingleP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
		   for lPos := 1 to lVolVoxels do
				l32Buf[lPos] := l32BufSrc[lPos];
		end;
		2: begin
		   l16BufSrc := SmallIntP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
		   for lPos := 1 to lVolVoxels do
				l32Buf[lPos] := l16BufSrc[lPos];
		end;
		1: begin
		   for lPos := 1 to lVolVoxels do
				l32Buf[lPos] := gMRIcroOverlay[kBGOverlayNum].ImgBuffer[lPos];
		end;
		else begin
			showmessage('unknown datatype');
		end;
	end;
	//next calibrate values
	lSlope := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.scl_slope;
	lIntercept := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.scl_inter;
	if (lSlope=0) or ((lSlope=1) and (lIntercept=0)) then
		//no slope
	else begin
        for lPos := 1 to lVolVoxels do
				l32Buf[lPos] := (l32Buf[lPos] * lSlope)+lIntercept;
	end;
	//next - save log10 p values...
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lMRIcroHdr,0, 255,1{df},0,666,NIFTI_INTENT_LOG10PVAL,inttostr(666) );
	for lPos := 1 to lVolVoxels do
		if l32Buf[lPos] > 0 then
			 l32Buf[lPos] := -log(abs(pTdistr(42,l32Buf[lPos])),10)
		else
			l32Buf[lPos] :=0;
	SaveAsVOIorNIFTI(lBufferAligned,lVolVoxels,4,1,false,lMRIcroHdr.NiftiHdr,'log10p'+inttostr(666));
	//next - free float buffer
	FreeMem(lBufferUnaligned);

end;

procedure TImgForm.DesignVALClick(Sender: TObject);
begin
        showmessage('Please use NPM');
	//SpreadForm.Show;
end;

procedure TImgForm.Up1Click(Sender: TObject);
var lVolVox,lPos,lShift: integer;
begin
  if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then begin
	showmessage('You must have open a background image in order to apply an intensity filter (use File/Open).');
	exit;
  end;
  if not IsVOIOpen then begin
	 ShowMessage('You have not created or opened a region of interest.');
	 exit;
   end;
   CreateUndoVol;//create gBGImg.VOIUndoVol
   Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVol^,gBGImg.VOIUndoVolItems);
   lVolVox := gBGImg.ScrnDim[1]* gBGImg.ScrnDim[2]*gBGImg.ScrnDim[3];
   case (Sender as TMenuItem).tag of
		0: lShift := 1;
		1: lShift := -1;
		2: lShift :=  gBGImg.ScrnDim[1];
		3: lShift :=  -gBGImg.ScrnDim[1];
		4: lShift :=  gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
		else {5} lShift :=  -gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
   end;
   if lShift > 0 then begin
	  for lPos := 1 to (lVolVox-lShift) do
		 gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] := gBGImg.VOIUndoVol[lPos+lShift];
   end else begin
		for lPos := (1+abs(lShift)) to lVolVox do
			gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] := gBGImg.VOIUndoVol[lPos+lShift];
   end;
   gBGImg.VOIchanged := true;
   ImgForm.ProgressBar1.Position := 0;
   ImgForm.RefreshImagesTimer.Enabled := true;
end;


procedure TImgForm.FormDestroy(Sender: TObject);
begin
CloseShareMem;
end;

procedure TImgForm.YokeMenuClick(Sender: TObject);
begin
	(sender as TMenuItem).checked := not (sender as TMenuItem).checked;
	 gYoke := (sender as TMenuItem).checked ;
	 YokeTimer.Enabled := gYoke;
end;

(*function A2R(var lIn: string): string;
begin
    result := lIn;
    if length(lIn) < 6 then exit;
    //result[1] := 'r';
    result[length(lIn)-4] := 'R';
end;

procedure VolComp(var lAVol,lOldVol,lNewVol,lARdistance: integer);
var
   lAX,lAY,lAZ,lRX,lRY,lRZ: double;
   lRVol,lXd,lYd,lZd,lImgSz,lX,lY,lZ,lInc: integer;

begin
     lAX := 0; lAY := 0; lAZ := 0; lRX := 0; lRY := 0; lRZ := 0;
     lAVol := 0;
     lOldVol := 0;
     lNewVol := 0;
     lImgSz := gMRIcroOverlay[0].ScrnBufferItems;
     if lImgSz <>  gMRIcroOverlay[1].ScrnBufferItems then exit;
     lXd :=  gMRIcroOverlay[0].NIFTIhdr.dim[1];
     lYd :=  gMRIcroOverlay[0].NIFTIhdr.dim[2];
     lZd :=  gMRIcroOverlay[0].NIFTIhdr.dim[3];
     //fx(lXd,lYd,lZd);
     lInc := 0;
     for lZ := 1 to lZd do begin
         for lY := 1 to lYd do begin
             for lX := 1 to lXd do begin
                 inc(lInc);
                 if gMRIcroOverlay[0].ScrnBuffer[lInc] > 0 then begin

                    lAX := lAX + lX;
                    lAY := lAY + lY;
                    lAZ := lAZ + lZ;
                 end;

                 if gMRIcroOverlay[1].ScrnBuffer[lInc] > 0 then begin
                    lRX := lRX + lX;
                    lRY := lRY + lY;
                    lRZ := lRZ + lZ;
                 end;

             end; //lX
         end;//Y
     end;//Z
     for lInc := 1 to lImgSz do begin
         if gMRIcroOverlay[0].ScrnBuffer[lInc] > 0 then begin
            inc(lAVol);//acute volume
            if gMRIcroOverlay[1].ScrnBuffer[lInc] > 0 then
             inc(lOldVol);
         end else if gMRIcroOverlay[1].ScrnBuffer[lInc] > 0 then
             inc(lNewVol);
     end; //for each voxel
     if lAVol > 0 then begin
        lAX := lAX / lAVol;
        lAY := lAY / lAVol;
        lAZ := lAZ / lAVol;
     end;

     lRVol := lOldVol + lNewVol;
     if lRVol > 0 then begin
        lRX := lRX / lRVol;
        lRY := lRY / lRVol;
        lRZ := lRZ / lRVol;
     end;
     lARDistance := round(sqrt( sqr(lRX-lAX)+sqr(lRY-lAY)+sqr(lRZ-lAZ)));
     //fx(lAX,lAY,lAZ);
     //fx(lRX,lRY,lRZ);
     //fx(sqrt( sqr(lRX-lAX)+sqr(lRY-lAY)+sqr(lRZ-lAZ)),lARdistance);
end;

function Age(var lIn: string): integer;
var
   lStr: string;

begin
    result := length(lIn);
    if result < 12 then exit;
    lStr := lIn[result-9]+lIn[result-8]+lIn[result-7]+lIn[result-6];
    //showmessage(lStr);
    result := 2006- strtoint(lStr);
    //result[length(lIn)-4] := 'R';
end;

procedure TImgForm.x1Click(Sender: TObject);
var
	lSearchRec: TSearchRec;
        lAName,lRName,lDir: string;
        lOrigVol,lOldVol,lNewVol,lAge,lARdistance: integer;
        lGroupChar : char;
begin
  TextForm.Memo1.Lines.Clear;
  TextForm.Memo1.Lines.Add('Aimg,Rimg,group,Avol,Rvol,RinsideA,RoutsideA,Age,ARdistance'   );
  lDir := 'C:\t\';
  if FindFirst(lDir+'*A.voi', faAnyFile, lSearchRec) = 0 then
	 repeat
               lAName := lSearchRec.Name;
               lRName := A2R(lAName);
               lAge := Age(lAName);
               lGroupChar := lAName[1];
               lAName := lDir+lAName;
               lRName := lDir+lRName;
               if not FileExistsEx(lRName) then
                  TextForm.Memo1.Lines.Add('*Error:,'+lAName+','+lRName)
               else begin
                    //LoadBG
	            CloseImagesClick(nil);
	            OpenAndDisplayImg(lAname,True);
                    OverlayOpenCore ( lRname, 1);
                    VolComp(lOrigVol,lOldVol,lNewVol,lARdistance);
                   TextForm.Memo1.Lines.Add(lAName+','+lRName+','+lGroupChar+','+inttostr(lOrigVol)+','+inttostr(lOldVol+lNewVol)+','+inttostr(lOldVol)+','+inttostr(lNewVol)+','+inttostr(lAge)+','+inttostr(lARdistance)   );
               end;
	 until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
  TextForm.Show;
end;*)

procedure TImgForm.X1Click(Sender: TObject);
begin
     CropEdgeForm.Show;
end;

procedure TImgForm.BrainExtraction1Click(Sender: TObject);
begin
     BETForm.show;
end;

procedure TImgForm.NZ1Click(Sender: TObject);
begin
end;


(*var
        lFilename: string;
	lPos,lInc:  Integer;
begin

     showmessage('Convert images to .nii.gz '+kImgFilter);
     if not OpenDialogExecute(kImgFilter,'Select images',true) then
        exit;
     if HdrForm.OpenHdrDlg.Files.Count < 1 then
        exit;

     for lPos := 1 to HdrForm.OpenHdrDlg.Files.Count do begin
         lFilename := HdrForm.OpenHdrDlg.Files[lPos-1];
         OpenAndDisplayImg(lFilename,True);
         for lInc := 1 to 18 do
             gMRIcroOverlay[kBGOverlayNum].NIfTIHdr.db_name[lInc] := chr(0);
         lFilename := extractfiledir(lFilename)+'\'+inttostr(lPos)+'.nii.gz';
         SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems,gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
     end;
end;*)

procedure TImgForm.MNICoordinates1Click(Sender: TObject);
begin
     MNIForm.show;
end;

procedure TImgForm.Histogram1Click(Sender: TObject);
var
   lLayer: integer;
begin
		lLayer := ActiveLayer;
		DrawHistogram(gMRIcroOverlay[lLayer],HistogramForm.HistoImage{lImage});
		HistogramForm.Caption := 'Histogram: '+extractfilename(gMRIcroOverlay[lLayer].HdrFileName);
		HistogramForm.show;
end;

procedure TImgForm.N4DTraces1Click(Sender: TObject);
begin
     Graph4DForm.show;
end;

procedure TImgForm.Sagittal1Click(Sender: TObject);
begin
  gBGImg.SliceView :=  (Sender as TMenuItem).Tag;
  imgForm.Triplepanel.VertScrollBar.Position := 0;
  imgForm.Triplepanel.HorzScrollBar.Position := 0;
    RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.HideVOI1Click(Sender: TObject);
begin
     if HideROITimer.enabled then
        exit; //still hiding - do not forget desired transparency
     HideROIBtnMouseDown(nil,mbleft,[],0,0);
     HideROITimer.enabled := true;
end;

procedure TImgForm.HideROITimerTimer(Sender: TObject);
begin
     HideROITimer.enabled := false;
     HideROIBtnMouseUp (nil,mbleft,[],0,0);
end;

(*procedure Stripe (L,T,R,B: integer);
var
   lL,lR,lT,lB,lX,lY,lZ,lLinePos,lSlicePos: integer;
begin
	if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems <> (gBGImg.ScrnDim[1] * gBGImg.ScrnDim[2] * gBGImg.ScrnDim[3])) or
          (gBGImg.ScrnDim[1] < 2) or (gBGImg.ScrnDim[2] < 2) or ( gBGImg.ScrnDim[3] < 2) then exit;
        lL := Bound(L,1,gBGImg.ScrnDim[1]);
        lT := Bound(T,1,gBGImg.ScrnDim[2]);
        lR := Bound(R,1,gBGImg.ScrnDim[1]);
        lB := Bound(B,1,gBGImg.ScrnDim[2]);
        SortInt(lL,lR);
        SortInt(lT,lB);
        for lZ := 1 to  gBGImg.ScrnDim[3] do begin
          lSlicePos := (lZ-1) * gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
          for lY := lT to lB do begin
            lLinePos := lSlicePos + ((lY-1)* gBGImg.ScrnDim[1]);
            for lX := lL to lR do begin
                gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lX+lLinePos] := 255;

            end;//X
          end;//Y
        end;//Z
end;

procedure StripeVol;
const
     kStipeThick = 2;
     kStipeSpacing = 20;
var
	lRow,lNumberofFiles,lX,lY,lZ: integer;
	lFilename: string;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image for striping.');
		exit;
	end;
        if ((gBGImg.ScrnDim[1] * gBGImg.ScrnDim[2] * gBGImg.ScrnDim[3]) <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems) then begin
            showmessage('Unable to stripe.');
            exit;
        end;
        if (gBGImg.ScrnDim[1] < kStipeSpacing) or (gBGImg.ScrnDim[2] < kStipeSpacing) then begin
            showmessage('Image resolution to low to stripe.');
            exit;
        end;
        {for lRow := 1 to (1+(gBGImg.ScrnDim[1] div kStipeSpacing)) do
            Stripe(  1+((lRow-1)*kStipeSpacing), 1,2+((lRow-1)*kStipeSpacing),gBGImg.ScrnDim[2]);
        for lRow := 1 to ((gBGImg.ScrnDim[2] div kStipeSpacing)+1) do
            Stripe(  1, 1+((lRow-1)*kStipeSpacing), gBGImg.ScrnDim[1],2+((lRow-1)*kStipeSpacing)); }
        Stripe(46,1,46,gBGImg.ScrnDim[2]);
        Stripe(1,64,gBGImg.ScrnDim[1],64);

        lFilename := 'c:\striped2.hdr';
        SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);

end;    *)



procedure TImgForm.CropEdges1Click(Sender: TObject);
begin
//StripeVol;
CropEdgeForm.Show;
end;

procedure TImgForm.Preferences1Click(Sender: TObject);
begin
     PrefForm.ShowModal;
end;


procedure TImgForm.Header1Click(Sender: TObject);
begin
DisplayHdrClick(nil);
end;


function AbsImg: boolean;
//generate absolute image
var
        lHdr:TMRIcroHdr;
        lImgSamples,lInc,lBPP: integer;
        l32Buf,lo32Buf : SingleP;
        l16Buf : SmallIntP;
begin
     //note ignores input slope/intercept scaling values
     result := false;
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image for calculating abolutes.');
		exit;
	end;
        if ((gBGImg.ScrnDim[1] * gBGImg.ScrnDim[2] * gBGImg.ScrnDim[3]) <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems) then begin
            showmessage('Unable to rescale.');
            exit;
        end;
        lBPP := gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP;//check if BitsPerPixel is supported
        if  (lBPP = 1) then begin
            showmessage('Can not make absoulte image for (unsigned) 8-bit data.');
            exit;
        end;

        if  (lBPP <> 4) and (lBPP <> 2) and (lBPP <> 1) then begin
            showmessage('AbsoluteImg Error: Unsupported BPP: '+inttostr(lBPP));
            exit;
        end;
        lImgSamples := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lHdr,0{min}, 0{max},0{p1},0{p2},0{p3},kNIFTI_INTENT_NONE,'abs' );
        GetMem(lHdr.ImgBufferUnaligned ,(lImgSamples*4)+16);
        lHdr.ImgBuffer := ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
        lo32Buf := SingleP( lHdr.ImgBuffer );
        if lBPP = 4 then begin
           l32Buf := SingleP( gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	   for lInc := 1 to lImgSamples do
               lo32Buf[lInc] :=  abs(l32Buf[lInc]) ;
        end else if lBPP = 2 then begin //lBPP=4 else
           l16Buf := SmallIntP( gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	   for lInc := 1 to lImgSamples do
               lo32Buf[lInc] :=  abs(l16Buf[lInc]);
        end else if lBPP = 1 then begin //lBPP=2 else
	   for lInc := 1 to lImgSamples do
               lo32Buf[lInc] :=  (gMRIcroOverlay[kBGOverlayNum].ImgBuffer[lInc]);
        end;//lBPP = 1
	SaveAsVOIorNIFTI(bytep(lo32Buf),lImgSamples,4,1,false,lHdr.NiftiHdr,'rscl'+extractfilename(gMRIcroOverlay[kBGOverlayNum].HdrFilename));
        //SaveAsVOIorNIFTI(gMRIcroOverlay[lLayer].ImgBuffer,gMRIcroOverlay[lLayer].ImgBufferItems,gMRIcroOverlay[lLayer].ImgBufferBPP,1,false,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[lLayer].HdrFilename);
        FreeMem(lHdr.ImgBufferUnaligned);
        //lFilename := 'c:\striped2.hdr';
        //SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
     result := true;
end;


function RescaleImg( lRescaleIntercept,lRescaleSlope: double): boolean;
var
	//lRow,lNumberofFiles,lX,lY,lZ: integer;
	//lFilename: string;
        lHdr:TMRIcroHdr;
        lImgSamples,lInc,lBPP: integer;
        l32Buf,lo32Buf : SingleP;
        l16Buf : SmallIntP;
begin
     //note ignores input slope/intercept scaling values
     result := false;
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image for rescaling.');
		exit;
	end;
        if ((gBGImg.ScrnDim[1] * gBGImg.ScrnDim[2] * gBGImg.ScrnDim[3]) <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems) then begin
            showmessage('Unable to rescale.');
            exit;
        end;
        lBPP := gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP;//check if BitsPerPixel is supported
        if  (lBPP <> 4) and (lBPP <> 2) and (lBPP <> 1) then begin
            showmessage('RescaleImg Error: Unsupported BPP: '+inttostr(lBPP));
            exit;
        end;
        lImgSamples := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lHdr,0{min}, 0{max},0{p1},0{p2},0{p3},kNIFTI_INTENT_NONE,floattostr(lRescaleSlope) );
        GetMem(lHdr.ImgBufferUnaligned ,(lImgSamples*4)+16);
        lHdr.ImgBuffer := ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
        lo32Buf := SingleP( lHdr.ImgBuffer );
        if lBPP = 4 then begin
           l32Buf := SingleP( gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	   for lInc := 1 to lImgSamples do
               lo32Buf[lInc] :=  (l32Buf[lInc]+lRescaleIntercept) * lRescaleSlope;
        end else if lBPP = 2 then begin //lBPP=4 else
           l16Buf := SmallIntP( gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	   for lInc := 1 to lImgSamples do
               lo32Buf[lInc] :=  (l16Buf[lInc]+lRescaleIntercept) * lRescaleSlope;
        end else if lBPP = 1 then begin //lBPP=2 else
	   for lInc := 1 to lImgSamples do
               lo32Buf[lInc] :=  (gMRIcroOverlay[kBGOverlayNum].ImgBuffer[lInc]+lRescaleIntercept) * lRescaleSlope;
        end;//lBPP = 1
	SaveAsVOIorNIFTI(bytep(lo32Buf),lImgSamples,4,1,false,lHdr.NiftiHdr,'rscl'+extractfilename(gMRIcroOverlay[kBGOverlayNum].HdrFilename));
        //SaveAsVOIorNIFTI(gMRIcroOverlay[lLayer].ImgBuffer,gMRIcroOverlay[lLayer].ImgBufferItems,gMRIcroOverlay[lLayer].ImgBufferBPP,1,false,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[lLayer].HdrFilename);
        FreeMem(lHdr.ImgBufferUnaligned);
        //lFilename := 'c:\striped2.hdr';
        //SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
     result := true;
end;

procedure TImgForm.RescaleMenuClick(Sender: TObject);
var ldTE,lScale,lTE1,lTE2: double;
    //lStr: string;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image for rescaling.');
		exit;
	end;
        if gBGImg.Resliced then begin
           if not HdrForm.OpenAndDisplayHdr(gMRIcroOverlay[kBGOverlayNum].HdrFileName,gMRIcroOverlay[kBGOverlayNum]) then exit;
           if not OpenImg(gBGImg,gMRIcroOverlay[0],true,false,false,false,false) then exit;
        end;
        if (gMRIcroOverlay[kBGOverlayNum].GlMinUnscaledS < 0) or (gMRIcroOverlay[kBGOverlayNum].GlMaxUnscaledS > 4096) then begin
            showmessage('Error: you need to load a Siemens format Phase map with raw values in the range 0..4096');
            exit;
        end;
        lTE1 := ReadFloatForm.GetFloat('Please enter the first TE (ms) used for phasemap. ', 0,5.19,9999);
        lTE2 := ReadFloatForm.GetFloat('Please enter the second TE (ms) used for phasemap. ', 0,7.65,9999);
        (*lStr := floattostr(5.19); //use floattostr for local decimal separator
        if not InputQuery('TEs used to create phasemap','Please enter the first TE in ms', lStr) then
           exit;
        try
           lTE1 := strtofloat(lStr);
        except
             showmessage('Unable to convert the string '+lStr+' to a number');
             exit;
        end;
        lStr := floattostr(7.65);
        if not InputQuery('TEs used to create phasemap','Please enter the second TE in ms', lStr) then
           exit;
        try
           lTE2 := strtofloat(lStr);
        except
             showmessage('Unable to convert the string '+lStr+' to a number');
             exit;
        end; *)
        if lTE1 = lTE2 then begin
           showmessage('In order to compute Rad/S the two TEs must be different.');
           exit;
        end;
        //fx(lTE1,lTE2);
        //exit;
//the fieldmap is simply a phase
//difference image and is not scaled to any particular units.  In Siemens
//phase images the data goes from 0 to 4095 with 0 being -pi radians, 2048
//is 0 radians, and 4095 is just short of +pi radians.
  //So, to get units of radians/s you would need to know the difference in
  //echo times (dTE) in units of s (not ms).  You would then take
  //(x-2048)(2pi/4096)/dTE
//Note ignore original intercept and scale values
  //ldTE := abs(5.19 - 7.65)/1000; // div 1000 to scale ms to sec
  ldTE := abs(lTE1 - lTE2)/1000; // div 1000 to scale ms to sec
  lScale := (2*pi/4096)/ldTE;
  //showmessage(floattostr(lScale));
  rescaleImg(-2048,lScale);
end;

procedure TImgForm.Brainmask1Click(Sender: TObject);
var
   lInc: integer;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image for rescaling.');
		exit;
	end;
        //lImgSamples := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
        for lInc := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do
            if gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lInc] <> 0 then
               gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lInc] := 1;
        SaveAsVOIorNIFTI(gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems,1,1,true,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName);
end;





procedure TImgForm.MirrorNII1Click(Sender: TObject);
var
	lNumberofFiles,lC: integer;
	lFilename: string;
begin
        Showmessage('WARNING: This will flip the images in the Left-Right dimension: this has serious consequences');
	CloseImagesClick(nil);
	if not OpenDialogExecute(kImgFilter,'Select NIfTI format images to convert',true) then exit;
	lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := HdrForm.OpenHdrDlg.Files[lC-1];
		ImgForm.OpenAndDisplayImg(lFilename,True);
		lFilename := changefileextX(lFilename,'lr.nii.gz');
                //zap
                //showmessage(lFilename);
                if MirrorImgBuffer (gMRIcroOverlay[kBGOverlayNum] ) then begin
                   //showmessage(lFilename);
                   SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems,gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
                end;
		CloseVOIClick(nil);
		ProgressBar1.Position := lC;            
	end;
	ProgressBar1.Position := 0;
end;

procedure TImgForm.MagPanelClick(Sender: TObject);
var
   lM: TMouse;
   lB: boolean;
   lX,lW: integer;
begin
     lW := MagPanel.width;

(*jULY2008     lM:= TMouse.Create;
     lX := lM.CursorPos.X- ImgForm.Left;  //position relative to window
     if (lW > 1) and (lX > 1) and ((lX/lW) > 0.5) then
        FormMouseWheelUp(nil,[],lM.CursorPos,lB)
     else
         FormMouseWheelDown(nil,[],lM.CursorPos,lB);
          *)
  {Case SelectedImageNum of
	  3: IncViewEdit(YViewEdit);
	  2: IncViewEdit(XViewEdit);
	  else IncViewEdit(ZViewEdit);
  end;}
end;

procedure TImgForm.FillBtnClick(Sender: TObject);
var
   lX,lY,lPanel: integer;
   lBuffer: ByteP;
   lImage: TImage;
begin
     if FillBtn.GroupIndex <> 0 then begin
	RefreshImagesTimer.Enabled := true;
        exit;
     end;
     lPanel := SelectedImageNum;
        SelectPanel(lPanel);
   WriteUndoVOI(lPanel,false);//false -write undo and draw
     lX := DrawImg.Width;
     lY := DrawImg.Height;
     Scrn2VOI (DrawImg,lX,lY, lBuffer);
     BorderFill(lBuffer,kVOI8Bit, lY,lX);
     ImgForm.SetDimension8(lY,lX, lBuffer,false);
     freemem(lBuffer);
     if lPanel = 3 then lImage:= PGImageCor
     else if lPanel = 2 then lImage:= PGImageSag
     else lImage:= PGImageAx;
     ReadScrnVOI (lImage);
end;

procedure TImgForm.Blackborders1Click(Sender: TObject);
begin
     if not blackborders1.checked then begin
        ImgForm.Color := clBlack;
        //ImgForm.Font.Color := clWhite;
     end else begin
        ImgForm.Color := clBtnFace;//clButtonFace;
        //ImgForm.Font.Color := clWindowText;

     end;
     RenderForm.Color := ImgForm.Color;
     //RenderForm.Font.Color := ImgForm.Font.Color;
     MultiSliceForm.Color := ImgForm.Color;
     //MultiSliceForm.Font.Color := ImgForm.Font.Color;
     //CutoutForm.Color := ImgForm.Color;
     //CutoutForm.Font.Color := ImgForm.Font.Color;
     blackborders1.checked := not blackborders1.checked;
end;

procedure TImgForm.Applyclusterthreshold1Click(Sender: TObject);
var
	lNumberofFiles,lC,lClusterSz: integer;
    lThresh: double;
	lFilename: string;
begin
	CloseImagesClick(nil);
	if not OpenDialogExecute(kImgFilter,'Select NIfTI format images to convert',true) then exit;
	lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
    lClusterSz := ReadIntForm.GetInt('Minimum cluster size [in voxels]: ', 1,32,9999);
    lThresh := ReadFloatForm.GetFloat('Include voxels with an intensity above: ', 0,2,9999);
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := HdrForm.OpenHdrDlg.Files[lC-1];
		ImgForm.OpenAndDisplayImg(lFilename,True);
		lFilename := changefileprefix(lFilename,'I'+inttostr(round(lThresh))+'C'+inttostr(lClusterSz));
		//lFilename := changefileextX(lFilename,'I'+inttostr(round(lThresh))+'C'+inttostr(lClusterSz)+'.nii.gz');
                if ClusterFilterScrnImg (gMRIcroOverlay[kBGOverlayNum],lClusterSz,lThresh ) then
                if ImgVaries(gMRIcroOverlay[kBGOverlayNum]) then
                          SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems,gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr)
                else
                    showmessage('No clusters survive filter '+ HdrForm.OpenHdrDlg.Files[lC-1]);
                ProgressBar1.Position := lC;
	end;
        if fileexistsEX(lFilename) then
           ImgForm.OpenAndDisplayImg(lFilename,True);
	ProgressBar1.Position := 0;
end;

procedure TImgForm.Batchprobmaps1Click(Sender: TObject);
begin
     BatchVOI;
end;

procedure TImgForm.ExportasRGBAnalyzeimage1Click(Sender: TObject);
var
   lFlip: boolean;
begin
     lFlip := gBGImg.Mirror;
     gBGImg.Mirror := true;
     CreateAnaRGB;
     gBGImg.Mirror := lFlip;

end;

procedure TImgForm.PGImageSagDblClick(Sender: TObject);
begin
     if Graph4DForm.visible then
        Graph4DForm.RefreshBtn.click;
end;

procedure TImgForm.Resliceimage1Click(Sender: TObject);
//use dcm2nii for this function
begin
 ResliceFSL;
end;

procedure TImgForm.Batchclusterprobmaps1Click(Sender: TObject);
begin
     BatchCluster;
end;

function Img2Txt (lFilename: string): boolean;
var
  lF: textfile;
   lZ,lY,lX,lXDim,lYDim,lZDim,lPos: integer;
begin
   result := false;
   lXDim := gBGImg.ScrnDim[1];
   lYDim := gBGImg.ScrnDim[2];
   lZDim := gBGImg.ScrnDim[3];
   if (lXDim < 1) or (lYDim < 1) or (lZDim < 1) then
    exit;
   if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems <> (lXDim*lYDim*lZDim) then
    exit;
   if fileexists (lFilename) then begin
       Showmessage('Problem: there is already a file named '+lFilename);
       exit;
   end;
   //Filemode := 2;//random access
   Filemode := 0;
   AssignFile(lF, lFilename);
   rewrite(lF);
   Writeln(lF, '#Min '+inttostr(1)+' '+inttostr(1)+' '+inttostr(1));
   Writeln(lF, '#Max '+inttostr(lXDim)+' '+inttostr(lYDim)+' '+inttostr(lZDim));
   lPos := 0;
   for lZ := 1 to lZdim do begin
    for lY := 1 to lYdim do begin
      for lX := 1 to lXdim do begin
        inc(lPos);
        if gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lPos] <> 0 then
          Writeln(lF, inttostr(lX)+' '+inttostr(lY)+' '+inttostr(lZ));
      end;//lX
    end;//lY
   end;//lZ
  CloseFile(lF); (**)
  result := true;
end; //Img2Txt

procedure TImgForm.VOI2TextClick(Sender: TObject);
var
	lNumberofFiles,lC: integer;
	lFilename: string;
begin
	CloseImagesClick(nil);
	if not OpenDialogExecute('VOI Drawings (.VOI)|*.VOI','Select VOI format images to convert',true) then exit;
	lNumberofFiles:= HdrForm.OpenHdrDlg.Files.Count;
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := HdrForm.OpenHdrDlg.Files[lC-1];
		if ImgForm.OpenAndDisplayImg(lFilename,True) then begin
		  //SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,lNiftiHdr);
      lFilename := ChangeFileExtX(lFilename,'.txt');
      Img2Txt(lFilename);
    end;
    CloseImagesClick(nil);
		//CloseVOIClick(nil);
		ProgressBar1.Position := lC;
	end;
	ProgressBar1.Position := 0;
end;//VOI->Text


procedure TImgForm.AdjustimagessoVOIintensityiszero1Click(Sender: TObject);
begin
    BatchChangeInterceptSoVOIEqualsZero;
end;

procedure TImgForm.ResizeControlPanel (lRows: integer);
begin
  if lRows = 2 then begin
    ControlPanel.Tag := 2;
    LayerPanel.Top := 36;
    LayerPanel.Left := 1;
    ToolPanel.Left := 292;
    ControlPanel.Height := 72;

        HideROIBtn.left := 226;
    XBarBtn.Left := 258;
  end else begin
    ControlPanel.Tag := 1;
    LayerPanel.Top := 0;
    LayerPanel.Left := 226;
    HideROIBtn.left := 680;
    XBarBtn.Left := 712;

    ToolPanel.Left := 746;
    ControlPanel.Height := 34;
  end;
end;

procedure TImgForm.ControlPanelDblClick(Sender: TObject);
begin
  if ControlPanel.Tag = 1 then
    ResizeControlPanel(2)
  else
    ResizeControlPanel(1);
  ImgForm.RefreshImagesTimer.enabled := true;
end;

procedure TImgForm.DefaultControlPanel;
begin
  if gBGImg.SingleRow then begin
      ResizeControlPanel(1);
      ImgForm.Width := 924;
      ImgForm.Height :=  469;
  end else begin
    ResizeControlPanel(2);
      ImgForm.Width := 460;
      ImgForm.Height :=  640;
   end;
end;

procedure TImgForm.RotateMenuClick(Sender: TObject);
begin
  RotationForm.show;
end;


procedure TImgForm.DilateVOIs1Click(Sender: TObject);
begin
    if (ssShift in KeyDataToShiftState(vk_Shift)) then begin
      MakeShells;
      exit;

 end else if (ssCtrl in KeyDataToShiftState(vk_Shift)) then
    BatchDilate
   else begin
     if  gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems= 0 then begin
		    Showmessage('You need to create a VOI before you can save it.');
		    exit;
    end;
    CreateUndoVol;//create gBGImg.VOIUndoVol
      DilateVOI(1, gBGImg.VOIUndoVol);
   gBGImg.VOIchanged := true;

   UndoVolVOI;
   ImgForm.RefreshImagesTimer.Enabled := true;
   end;
   // DilateOpenVOI(10,true);

end;

procedure TImgForm.Nudge2D(Sender: TObject);
var lSliceStart,lSliceEnd,lVolVox,lPos,lShift: integer;
begin
  if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then begin
	showmessage('You must have open a background image in order to apply an intensity filter (use File/Open).');
	exit;
  end;
  if not IsVOIOpen then begin
	 ShowMessage('You have not created or opened a region of interest.');
	 exit;
   end;
   CreateUndoVol;//create gBGImg.VOIUndoVol
   Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVol^,gBGImg.VOIUndoVolItems);
  lVolVox := gBGImg.ScrnDim[1]* gBGImg.ScrnDim[2]*gBGImg.ScrnDim[3];
   case (Sender as TMenuItem).tag of
		0: lShift := 1;
		1: lShift := -1;
		2: lShift :=  gBGImg.ScrnDim[1];
		3: lShift :=  -gBGImg.ScrnDim[1];
		4: lShift :=  gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
		else {5} lShift :=  -gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
   end;
   lSliceStart := ((ZviewEdit.asinteger-1) * ( gBGImg.ScrnDim[1]* gBGImg.ScrnDim[2]))+1;
   lSliceEnd := lSliceStart + (gBGImg.ScrnDim[1]* gBGImg.ScrnDim[2]);
   if (lSliceEnd > lVolVox) or (lSliceStart < 1) then
    exit;
   //lSliceOffset := ((XviewEdit.value-1) * SliceVox)+1
   if lShift > 0 then begin
	  for lPos := lSliceStart to (lSliceEnd-lShift) do
		 gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] := gBGImg.VOIUndoVol[lPos+lShift];
   end else begin
		for lPos := (lSliceStart+abs(lShift)) to lSliceEnd do
			gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] := gBGImg.VOIUndoVol[lPos+lShift];
   end;
   //caption := inttostr(random(888))+'   '+inttostr(lShift);
   gBGImg.VOIchanged := true;
   ImgForm.ProgressBar1.Position := 0;
   ImgForm.RefreshImagesTimer.Enabled := true;
end;


procedure TImgForm.Interpolate1Click(Sender: TObject);
begin
  ROISliceInterpolate (gMRIcroOverlay[kBGOverlayNum]);   
  	RescaleImagesTimer.Enabled := true;
end;

procedure LoadAll (lFilename: string; lBytes: integer; var lB: bytep);
var
	lF: File;
begin
   getmem(lB,lBytes );
   AssignFile(lF, lFilename);
  FileMode := 0;  { Set file access to read only }
  Reset(lF, 1);
  BlockRead(lF,lB^,lBytes);
  CloseFile(lF);
  FileMode := 2;

end;


var
  gSz: singlep;
procedure Copy2;
var
  lVox,lI: integer;
   l32SrcBuff: SingleP;
begin

  lVox :=  gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]*gBGImg.ScrnDim[3];
  getmem(gSz,lVox*4);
  l32SrcBuff := SingleP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer);
  for lI := 1 to lVox do
    if gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lI] <> 0 then
      gSz^[lI] := l32SrcBuff^[lI]
    else
      gSz^[lI] := 0;
    ImgForm.caption := 'Copied to RAM';
end;

procedure CopyFrom;
var
  lVox,lI: integer;
   l32SrcBuff: SingleP;
begin
  lVox :=  gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]*gBGImg.ScrnDim[3];
  l32SrcBuff := SingleP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer);
  for lI := 1 to lVox do
    if gSz^[lI] <> 0 then
       l32SrcBuff^[lI] := {l32SrcBuff^[lI]+}gSz^[lI];
  ImgForm.caption := 'Copied from RAM';
  ImgForm.RefreshImagesTimer.Enabled := true;

end;

procedure TImgForm.SaveSmooth1Click(Sender: TObject);
begin
  if (ssShift in KeyDataToShiftState(vk_Shift)) then begin
    Copy2;
  end else
    CopyFrom;
end;  //VOImaskClick
(*procedure TImgForm.SaveSmooth1Click(Sender: TObject);
function OK (lV,lMax: integer):boolean;
begin
  if (lV < 2) or (lV >= lMax) then
    result := false
  else
    result := true;
end;
function Empty (lP: integer): boolean;
begin
   result := (gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lP]= 0);
end;
var lVolItems,lPos,lX,lY,lZ: integer;
  lXok,lYOK,lZOK: boolean;
begin
	lVolItems := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]* gBGImg.ScrnDim[3];
	if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems <> lVolItems) or (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems <> lVolItems) then begin
		Showmessage('VOI fill requires a VOI loaded onto a background image (Draw/Open).');
		exit;
	end;
	CreateUndoVol;
  lPos := 0;
  for lZ := 1 to gBGImg.ScrnDim[3] do begin
    lZOK :=  OK(lZ,gBGImg.ScrnDim[3]);
    for lY := 1 to gBGImg.ScrnDim[2] do begin
        lYOK :=  OK(lY,gBGImg.ScrnDim[2]);
        for lX := 1 to gBGImg.ScrnDim[1] do begin
          lXOK :=  OK(lX,gBGImg.ScrnDim[1]);
          inc(lPos);
        if (Empty(lPos)) and (lXOK) and (lYOK) and (lZOK) then begin
          //x CountCluster(lPos);
        end;
      end;
    end;
  end;
  //gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] := 0;
	RefreshImagesTimer.Enabled := true;
end;  //ROIcomparisonClick

(*procedure TImgForm.SaveSmooth1Click(Sender: TObject);
var
	lF: File;
  lLname,lSname: string;
  lVox,lSo,lLo,lVol,lI,lnVol,lOffset,lLbytespervol,lSbytespervol: integer;
  lLb,lSb: bytep;
begin
   lSName := 'C:\walker\TPM.nii';
   lLName := 'C:\walker\TPMX.nii';
   lSbytespervol := 121*145*121*4; //X*Y*Z*bytespervoxel
   lLbytespervol := 121*145*199*4; //X*Y*Z*bytespervoxel
   lOffset := 352;
   lnVol := 6;
   LoadAll (lSname,lOffset + (lnVol *lSbytespervol ),lSb);
   LoadAll (lLname,lOffset + (lnVol *lLbytespervol ),lLb);
   lSo := lOffset;
   lLo := lOffset+ (lLbytespervol-lSbytespervol);
   for lVol := 1 to lnVol do begin
    for lVox := 1 to lSbytespervol do
      lLb^[lLo+lVox] :=  lSb^[lSo+lVox];
    lSo := lSo + lSbytespervol;
    lLo := lLo + lLbytespervol;

   end;
   //fx(    lOffset + (lnVol *lLbytespervol ));
    AssignFile(lF, 'C:\shit.nii');
	 Rewrite(lF,1);
	 BlockWrite(lF, lLb^, lOffset + (lnVol *lLbytespervol ));
	 CloseFile(lF);
   freemem(lSb);
   freemem(lLb);

end;
  
(*procedure TImgForm.SaveSmooth1Click(Sender: TObject);
var
  lBGname,lmaskname: string;
  lI: integer;
  lB: bytep;
begin
   if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems < 1 then begin
       showmessage('Please load a VOI drawing.');
       exit;
   end;
   lBGname := gMRIcroOverlay[kBGOverlayNum].HdrFileName;
   lmaskname := ChangeFilePrefix(lBGname,'m');
   lmaskname := changefileext(lmaskname, '.nii');
   if (fileexists(lmaskname))  then begin
       showmessage ('Files already exist named '+lmaskname);
       exit;
   end;
   ImgForm.StatusLabel.caption := 'Saving mask as '+lmaskname;
   getmem(lB,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems);
   for lI := 1 to gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems do
    if  gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lI] = 0 then
      lB^[lI] := 1
    else
      lB^[lI] := 0;
   SaveAsVOIorNIFTIcore (lmaskname, lB,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
   ImgForm.StatusLabel.caption := 'Saving mask as '+lmaskname;
   freemem(lB);
end;*)

function ExtX (lItem: integer): string;
var
   lLen,lPos,lI,lDelim,lEnd : Integer;
   lFilt: string;
begin
  lFilt := ImgForm.SaveDialog1.Filter;
  result := '';
  //There is one | before each item, and one after
  //therefore, the 2nd item will be preceded by 3 |s
  lDelim :=  lItem * 2 - 1;
  lI := 0;
  lLen := length(lFilt);
  lPos := 1;
  while (lI < lDelim) and (lPos <= lLen) do begin
    if lFilt[lPos] = '|' then
      inc(lI);
    inc(lPos);
  end;
  if lPos >= lLen then
    exit;
  while (lPos <= lLen) and (lFilt[lPos] <> '|') do begin
    if lFilt[lPos] <> '*' then
      result := result + lFilt[lPos];
    inc(lPos);
  end;
end;

procedure TImgForm.SetSaveDlgFileExt;
var
  Fn: string;
begin
  Fn := ExtractFilename(SaveDialog1.FileName);
  Fn:=ChangeFileExtX(Fn,ExtX(SaveDialog1.FilterIndex));
  SaveDialog1.FileName := Fn;
  //showmessage('666'+Fn);
end;

procedure TImgForm.SaveDialog1TypeChange(Sender: TObject);
var
   Fn : String;
const
     CB_FILENAME_ID = 1148;
begin
     Fn := ExtractFilename(SaveDialog1.FileName);
     Fn:=ChangeFileExtX(Fn,ExtX(SaveDialog1.FilterIndex));
     //Not sure if LongInt cast for string is 64-bit safe...
     SendMessage( GetParent(SaveDialog1.Handle), CDM_SETCONTROLTEXT, CB_FILENAME_ID, LongInt(Pchar(Fn)));
end;

procedure TImgForm.Landmarks1Click(Sender: TObject);
begin
  AnatForm.show;
end;

procedure TImgForm.Extract1Click(Sender: TObject);
var
   lMin : smallint;
   lnVox,lVox,lDilate,lOtsuLevels: integer;
  lOneContiguousObject : boolean;
  l16Buf : SmallIntP;
  l32Buf : SingleP;
  lMinS: single;
begin
 lnVox :=  gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
 if lnVox < 9 then begin
    showmessage('Please load a background image.');
    exit;
 end;
  lOtsuLevels :=  ReadIntForm.GetInt('Otsu levels: larger values for larger volumes',1,5,5);
 lDilate :=  ReadIntForm.GetInt('Edge dilation voxels: larger values for larger volumes',0,2,12);
 lOneContiguousObject := OKMsg('Only extract single largest object?');
 MaskBackground(gMRIcroOverlay[kBGOverlayNum].ScrnBuffer, gBGImg.ScrnDim[1],gBGImg.ScrnDim[2],gBGImg.ScrnDim[3],lOtsuLevels,lDilate,lOneContiguousObject);
 //ExtractTexture (gTexture3D, lOtsuLevels, lDilate, lOneContiguousObject);

   if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 4) then begin
	l32Buf := SingleP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
        lMinS := l32Buf^[1];
        for lVox := 1 to lnVox do
	    if l32Buf^[lVox] < lMinS then
               lMinS := l32Buf^[lVox];
        for lVox := 1 to lnVox do
	    if gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lVox] = 0 then
               l32Buf^[lVox] := lMinS;
   end else if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 2) then begin
	   l16Buf := SmallIntP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
           lMin := l16Buf^[1];
           for lVox := 1 to lnVox do
               if l16Buf^[lVox] < lMin then
                  lMin := l16Buf^[lVox];
           for lVox := 1 to lnVox do
   	    if gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lVox] = 0 then
                  l16Buf^[lVox] := lMin;
   end else if gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 1 then begin
         lMin := gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[1];
         for lVox := 1 to lnVox do
             if gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lVox] < lMin then
                lMin := gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lVox];
         for lVox := 1 to lnVox do
 	    if gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lVox] = 0 then
                gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lVox] := lMin;

   end;
end;


procedure TImgForm.AcceptLandmark1Click(Sender: TObject);
begin
   AnatForm.AcceptLandmark;
end;

procedure TImgForm.Batchlandmarks1Click(Sender: TObject);
begin
  AnatForm.BatchLandmarks;
end;

procedure TImgForm.ToggleDrawMenu(Sender: TObject);
begin
  gBGImg.ShowDraw := not  DrawMenu.Visible;
  WriteIni2Form(gBGImg);
end;

initialization
  {$IFNDEF UNIX}
   Set8087CW($133F); //Windows 64-bit can generate spurious FPU exceptions
   {$ENDIF}
  OleInitialize(nil);
  for gMouseDownY := 0 to knMaxOverlay do
    gMRIcroOverlay[gMouseDownY].index := gMouseDownY; //RGB

finalization
  OleUninitialize
end.
