unit nifti_img_view;
  {$mode delphi}
{$DEFINE DICOMdrop} //drag and drop DICOM
interface
{$IFDEF UNIX}
		{$IFNDEF ENDIAN_BIG}{$DEFINE COMPILEYOKE}{$ENDIF} //not supported on PPC
{$ELSE}
 {$DEFINE COMPILEYOKE} //windows supports yoking
{$ENDIF}
uses
{$H+}
{$IFDEF LCLCocoa} nsappkitext, {$ENDIF}
{$IFDEF Darwin}Process,{$ENDIF}  //CarbonOpenDoc,
{$IFDEF Unix}
  lclintf,LCLType,fileutil,//gettickcount ,LMessages
{$ELSE}
  Windows,ShellAPI,
{$ENDIF}
{$IFDEF COMPILEYOKE}
yokesharemem,
{$ENDIF}
{$IFDEF DICOMdrop} dcm_load, {$ENDIF}
{$IFDEF FPC} fphttpclient, strutils, {$ENDIF}
LResources, fx8, cpucount, SysUtils, Classes, Graphics, Controls, Forms,
Dialogs, Menus, ComCtrls, ExtCtrls, StdCtrls, GraphicsMathLibrary, ClipBrd,
define_types, Spin, Buttons, nifti_hdr, nifti_hdr_view, nifti_img, voismooth,
IniFiles, ReadInt,  stat, Distr, bet, mni, prefs, CropEdges,nifti_types,
draw_interpolate_slices,
userdir, graphx, GraphType, IntfGraphics, landmarks,fastsmooth, nii_label, dcm2nii, ImgList, Types;//registry


type

  { TImgForm }

  TImgForm = class(TForm)
    ImageList1: TImageList;
    LabelX: TLabel;
    LabelY: TLabel;
    LabelZ: TLabel;
    LayerDrop: TComboBox;
    LUTdrop: TComboBox;
	MainMenu1: TMainMenu;
	File1: TMenuItem;
MaxWindowEdit: TFloatSpinEdit;
MenuItem1: TMenuItem;
HistoMenu: TMenuItem;
Header1: TMenuItem;
ApplyClusterThreshold1: TMenuItem;
LRFlip1: TMenuItem;
ExportasRGBAnalyzeimage1: TMenuItem;
BatchROImean1: TMenuItem;
Batchprobmaps1: TMenuItem;
Batchclusterprobmaps1Batchclusterprobmaps1Click: TMenuItem;
Axial1: TMenuItem;
Coronal1: TMenuItem;
Axial2: TMenuItem;
Coronal2: TMenuItem;
Landmarks1: TMenuItem;
Extract1: TMenuItem;
HideDrawMenuItem: TMenuItem;
DrawHiddenMenu: TMenuItem;
MenuItem2: TMenuItem;
AppleMenu: TMenuItem;
MenuItem3: TMenuItem;
DilateVOI1: TMenuItem;
DilateShellsMenu1: TMenuItem;
ImportMenu: TMenuItem;
dcm2niiMenu: TMenuItem;
CheckUpdatesMenu: TMenuItem;
Interpolate1: TMenuItem;
OpenHdrDlg: TOpenDialog;
VOImaskCustom: TMenuItem;
NewWindow1: TMenuItem;
ColorBarBtn: TToolButton;
HideROIBtn: TToolButton;
AutoContrastPanel: TPanel;
SlicePanel: TPanel;
AutoContrastToolbar: TToolBar;
AutoContrastBtn: TToolButton;
XBarBtn: TToolButton;
ViewToolBar: TToolBar;
LutFromZeroBtn: TToolButton;
ViewPanel: TPanel;
Sagittal2: TMenuItem;
Sagittal1: TMenuItem;
Multiple1: TMenuItem;
PGImageAx: TImage;
PGImageSag: TImage;
Resliceimage1: TMenuItem;
AdjustimagessoVOIintensityiszero1: TMenuItem;
Brainmask1: TMenuItem;
GenerateSPM5maskslesions1: TMenuItem;
RescaleMenu: TMenuItem;
BrainExtraction1: TMenuItem;
CropEdges1: TMenuItem;
NIIVOI: TMenuItem;
MinWindowEdit: TFloatSpinEdit;
N4DTraces1: TMenuItem;
LayerPanel: TPanel;
n5: TMenuItem;
Preferences1: TMenuItem;
Display2: TMenuItem;
MNIMenu: TMenuItem;
	Open1: TMenuItem;
	CloseImages: TMenuItem;
	Exit1: TMenuItem;
	Edit1: TMenuItem;
	Copy1: TMenuItem;
	Help1: TMenuItem;
	About1: TMenuItem;
	ControlPanel: TPanel;
	Crosshair1: TMenuItem;
	Pen1: TMenuItem;
	Penautoclose1: TMenuItem;
	CircleSquare1: TMenuItem;
        ToolToolBar: TToolBar;
        PenBtn: TToolButton;
        ClosedPenBtn: TToolButton;
        FillBtn: TToolButton;
        EllipseBtn: TToolButton;
        Fill3DBtn: TToolButton;
        ToolPanel: TPanel;
        XViewEdit: TSpinEdit;
        YokeTimer: TTimer;
        YViewEdit: TSpinEdit;
        ZoomDrop: TComboBox;
	MagPanel: TPanel;
	ProgressBar1: TProgressBar;
	StatusLabel: TLabel;
	Templates1: TMenuItem;
	Recent1: TMenuItem;
	Controls1: TMenuItem;
	Panel1: TPanel;
	Saveaspicture1: TMenuItem;
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
	TriplePanel: TScrollBox;
	PGImageCor: TImage;
	Undo1: TMenuItem;
	Paste1: TMenuItem;
	Applyintensityfiltertovolume1: TMenuItem;
	Quicksmooth1: TMenuItem;
	MaskimagewithVOI1: TMenuItem;
	VOImaskDelete: TMenuItem;
	VOImaskPreserve: TMenuItem;
	SaveasNIfTI1: TMenuItem;
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
	N2: TMenuItem;
	Display1: TMenuItem;
	N3: TMenuItem;
	FlipLRmenu: TMenuItem;
	N4: TMenuItem;
	Menu2DSmooth: TMenuItem;
	VOI2NII: TMenuItem;
	Nudge1: TMenuItem;
	Left1: TMenuItem;
	Right1: TMenuItem;
	Posterior1: TMenuItem;
	Anterior1: TMenuItem;
	Inferior1: TMenuItem;
	Superior1: TMenuItem;
        YokeMenu: TMenuItem;
        ZViewEdit: TSpinEdit;
        procedure ControlPanelClick(Sender: TObject);
        procedure dcm2niiMenuClick(Sender: TObject);
        procedure DilateVOI1Click(Sender: TObject);
        procedure Extract1Click(Sender: TObject);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
          WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
        procedure GetWidthForPPI(Sender: TCustomImageList; AImageWidth,
          APPI: Integer; var AResultWidth: Integer);
        procedure Interpolate1Click(Sender: TObject);
        procedure NewWindow1Click(Sender: TObject);
        procedure ToggleDrawMenu(Sender: TObject);
        procedure SaveVOIcore(lPromptFilename: boolean);
procedure FormOpenFileMethod(const FileName : string);
procedure CheckForUpdates(Sender: TObject);
procedure Landmarks1Click(Sender: TObject);
procedure SetIniMenus;
function ActiveLayer:integer;
procedure Batchclusterprobmaps1Batchclusterprobmaps1ClickClick(Sender: TObject);
procedure Batchprobmaps1Click(Sender: TObject);
procedure BatchROImean1Click(Sender: TObject);
procedure BrainMask1Click(Sender: TObject);
procedure ControlPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
procedure GenerateSPM5maskslesions1Click(Sender: TObject);
procedure LoadOverlay (lFilename: string);
procedure  LoadOverlayIncludingRGB (lFilename: string);
procedure ApplyClusterThreshold1Click(Sender: TObject);
procedure BETmenuClick(Sender: TObject);
procedure C(Sender: TObject);
procedure CropMenuClick(Sender: TObject);
procedure ExportasRGBAnalyzeimage1Click(Sender: TObject);
procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
function OpenAndDisplayHdr (var lFilename: string; var lHdr: TMRIcroHdr): boolean;
//procedure DropFilesOSX(Sender: TObject; const FileNames: array of String);
procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
procedure FormKeyPress(Sender: TObject; var Key: char);
procedure Header1Click(Sender: TObject);
procedure HistoMenuClick(Sender: TObject);
procedure LayerDropChange(Sender: TObject);
procedure LUTdropChange(Sender: TObject);
procedure AdjustimagessoVOIintensityiszero1Click(Sender: TObject);
procedure MirrorNII1Click(Sender: TObject);
procedure MNIMenuClick(Sender: TObject);
procedure N4DTraces1Click(Sender: TObject);
procedure NIIVOIClick(Sender: TObject);
procedure PGImageCorDblClick(Sender: TObject);
procedure Preferences1Click(Sender: TObject);
procedure RescaleMenuClick(Sender: TObject);
procedure Resliceimage1Click(Sender: TObject);
procedure SaveasNIfTI1Click(Sender: TObject);
procedure SaveDialog1Close(Sender: TObject);
procedure ToolBar2Click(Sender: TObject);
procedure ToolPanelClick(Sender: TObject);
procedure TriplePanelMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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
	procedure ReadIniFile; //read init file values
	procedure WriteIniFile;
 {$IFNDEF FPC}
	procedure FormClose(Sender: TObject; var Action: TCloseAction);
 {$ELSE}
	procedure FormClose(Sender: TObject);

 {$ENDIF}
         procedure MagnifyTimerTimer(Sender: TObject);
	procedure MagnifyPanelResize(Sender: TObject);
	procedure PGImageMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
	procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
	  MousePos: TPoint; var Handled: Boolean);
	procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
	  MousePos: TPoint; var Handled: Boolean);
	procedure PGImageMouseDown(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure PGImageMouseUp(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure LUTdropLoad(var lLayer: integer);
	procedure LUTdropSelect(Sender: TObject);
        procedure ZoomDropChange(Sender: TObject);
	procedure ZoomDropSelect(Sender: TObject);
	procedure ColorBarBtnMouseDown(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure Saveaspicture1Click(Sender: TObject);
	procedure XBarBtnClick(Sender: TObject);
        procedure XBarBtnMouseDown(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure AutoContrastBtnClick(Sender: TObject);
	procedure RefreshImagesTimerTimer(Sender: TObject);
	procedure MinContrastWindowEditChange(Sender: TObject);
	procedure ImgPanelClick(Sender: TObject);
	procedure MagnifyMenuItemClick(Sender: TObject);
	procedure CloseImagesClick(Sender: TObject);
	procedure UpdateLayerMenu;
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
 procedure XBarColor;
 procedure Applyintensityfiltertovolume1Click(Sender: TObject);
	procedure Quicksmooth1Click(Sender: TObject);
	procedure VOImaskClick(Sender: TObject);
	procedure Sagittal1Click(Sender: TObject);
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
	procedure FormResize(Sender: TObject);
	procedure FormShow(Sender: TObject);
        procedure InitImg(var lImage: TImage);
    procedure OnLaunch;
	procedure FlipLRmenuClick(Sender: TObject);
	procedure Menu2DSmoothClick(Sender: TObject);
	procedure VALclick(Sender: TObject);
	procedure VOI2NIIClick(Sender: TObject);
	procedure TtoP1Click(Sender: TObject);
	procedure DesignVALClick(Sender: TObject);
	procedure Left1Click(Sender: TObject);
        procedure SetShareMem (lXmm,lYmm,lZmm: single);
        procedure CreateShareMem;
        procedure CloseShareMem;
        procedure YokeTimerTimer(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure YokeMenuClick(Sender: TObject);
        procedure DefaultControlPanel;
        procedure ControlPanelDblClick(Sender: TObject);
        procedure ResizeControlPanel (lRows: integer);
        procedure SaveOrCopyImages(lCopy: boolean);
        function ImgIntensityString(var lHdr: TMRIcroHdr; lVox: integer): string;  overload;
        function ImgIntensityString(var lHdr: TMRIcroHdr; lX,lY,lZ: integer): string;  overload;
        function OpenDialogExecute (lFilter,lCaption: string; lAllowMultiSelect: boolean): boolean;
            {$IFDEF LCLCocoa}
    procedure SetDarkMode;
    {$ENDIF}
 private
	{ Private declarations }

{$IFDEF FPC} function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;MousePos: TPoint): Boolean; override;{$ENDIF}
  public


	{ Public declarations }
public
   //procedure WMSysCommand (var Msg: TWMSysCommand) ; message WM_SYSCOMMAND;
  published
           property OnMouseWheel;
  end;

const
 kYokeItems= 12;
 knMRU = 12;//max items in most recently used list
 knMaxOverlay = 20;
 kVOIOverlayNum = knMaxOverlay;
 kBGOverlayNum = 0;
 knAutoLUT = 7;
 kVOIFilter =   'Volume of interest (*.voi)|*.voi|MRIcro ROI (*.roi)|*.roi|'+kImgFilter;
var
 gYoke: boolean = false;
  ImgForm: TImgForm;
  gBGImg: TBGImg;
  gMRIcroOverlay: array [0..knMaxOverlay] of TMRIcroHdr;
  gColorSchemeDir,gTemplateDir: String;
  gMRUstr: array [0..knMRU] of String; //most recently used files
  gMouseDownX,gMouseDownY: integer;
  gSelectOrigin: TPoint;
  gSelectRect: TRect;
  gOrigBGTransPct : integer= 50;
  //gMaxCPUThreads : integer = 8;
  gnCPUThreads : integer = 1;
  gUndoImg,gDrawImg: Tfx8;

Type
	 SingleArr = Array[1..kYokeItems] Of Single;
	 SingleArrPtr = ^SingleArr;

implementation

uses statclustertable,batch,imgutil, reslice_fsl,render,ROIfilt,autoroi, MultiSlice, Text,  histoform,
  about,clustering,ReadFloat, dilate;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.DFM}
{$ENDIF}

function TImgForm.OpenDialogExecute (lFilter,lCaption: string; lAllowMultiSelect: boolean): boolean;
begin
  OpenHdrDlg.Filter := lFilter;
  {$IFDEF Darwin}
  OpenHdrDlg.Filter := '';
  {$ENDIF}
  OpenHdrDlg.FilterIndex := 1;
  OpenHdrDlg.Title := lCaption;
  if lAllowMultiSelect then
    OpenHdrDlg.Options := [ofAllowMultiSelect,ofFileMustExist];
  result := OpenHdrDlg.Execute;
  OpenHdrDlg.Options := [ofFileMustExist];
end;

procedure TImgForm.XBarColor;
begin
    		ColorDialog1.Color := gBGImg.XBarClr;
		if not ColorDialog1.Execute then exit;
		gBGImg.XBarClr := ColorDialog1.Color;
		RefreshImagesTimer.Enabled := true;
		exit;
end;


procedure DecViewEdit(var lEdit: TSpinEdit);
begin
    if lEdit.Value > 1 then
       lEdit.value := lEdit.value -1
	else
        lEdit.Value := lEdit.MaxValue;
    {$IFDEF FPC} ImgForm.XViewEditChange(nil); {$ENDIF}
end; //DecViewEdit

procedure IncViewEdit(var lEdit: TSpinEdit);
begin
    if lEdit.Value < lEdit.MaxValue then
       lEdit.value := lEdit.value +1
    else
		lEdit.Value := 1;
    {$IFDEF FPC} ImgForm.XViewEditChange(nil); {$ENDIF}
end; //IncViewEdit


function TImgForm.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if WheelDelta = 0 then exit;
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
     //ImgForm.Caption := inttostr(random(888));
     //... actions after a possible OnMouseWheel[Down|Up]
     //ImgForm.Caption := inttostr(WheelDelta)+'  '+inttostr(random(888))+'  '+inttostr(MousePos.X);
     if WheelDelta < 0 then begin
        Case SelectedImageNum of
             3: DecViewEdit(YViewEdit);
             2: DecViewEdit(XViewEdit);
             else DecViewEdit(ZViewEdit);
        end;
     end else begin
         Case SelectedImageNum of
	      3: IncViewEdit(YViewEdit);
	      2: IncViewEdit(XViewEdit);
	      else IncViewEdit(ZViewEdit);
         end;
     end;
end;



procedure TImgForm.CloseShareMem;
begin
{$IFDEF COMPILEYOKE}
YokeTimer.Enabled := false;
  CloseSharedMem;
{$ENDIF}
end;

{$IFDEF COMPILEYOKE}
var
  isNewYoke : boolean = false;
{$ENDIF}

procedure TImgForm.SetShareMem (lXmm,lYmm,lZmm: single);
begin
{$IFDEF COMPILEYOKE}
  if not gYoke then
         exit;
  if isNewYoke then begin
     isNewYoke := false;
     exit;
  end;
  SetShareFloats(lXmm,lYmm,lZmm);

{$ENDIF}
end;

procedure TImgForm.CreateShareMem;
begin
     {$IFDEF COMPILEYOKE}
          CreateSharedMem(self);
          SetShareMem (0,0,0);
          YokeTimer.Enabled := gYoke;
     {$ENDIF}
end;

procedure TImgForm.YokeTimerTimer(Sender: TObject);
var
   lX,lY,lZ: integer;
   lXmm,lYmm,lZmm: single;
begin
  if not gYoke then
     YokeTimer.Enabled := false;
  {$IFDEF COMPILEYOKE}
  if not gYoke then
     exit;
  if not GetShareFloats(lXmm,lYmm,lZmm) then
     exit;
  MMToImgCoord(lX,lY,lZ,lXmm,lYmm,lZmm);
  if lX <> XViewEdit.value then XViewEdit.value := lX;
  if lY <> YViewEdit.value then YViewEdit.value := lY;
  if lZ <> ZViewEdit.value then ZViewEdit.value := lZ;
  XViewEditChange(nil);
  isNewYoke := true;
  {$ENDIF}
end;

(*var
lXmm,lYmm,lZmm: single;
lX,lY,lZ: integer;
begin
	if not gYoke then begin
		YokeTimer.Enabled := false;
		exit;
	end;
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then
		exit;
{$IFDEF FPC}
    {$IFDEF COMPILEYOKE}
      lXmm:=gShareIntBuf^[1];
      lYmm:=gShareIntBuf^[2];
      lZmm:=gShareIntBuf^[3];
    {$ELSE}
    YokeTimer.Enabled := false;
    exit;
    {$ENDIF}

{$ELSE}
	EMemMap.EnterCriticalSection;
	Try
	  lXmm:=SingleArrPtr(EMemMap.MemMap)^[1];
	  lYmm:=SingleArrPtr(EMemMap.MemMap)^[2];
	  lZmm:=SingleArrPtr(EMemMap.MemMap)^[3];
	Finally
	  EMemMap.LeaveCriticalSection;
	end;
{$ENDIF}
	MMToImgCoord(lX,lY,lZ,lXmm,lYmm,lZmm);
	if lX <> XViewEdit.value then XViewEdit.value := lX;
	if lY <> YViewEdit.value then YViewEdit.value := lY;
	if lZ <> ZViewEdit.value then ZViewEdit.value := lZ;
    YokeTimer.Enabled := false;
end;  *)

{$IFNDEF FPC}
procedure TImgForm.WMSysCommand;
begin
   if (Msg.CmdType = SC_MINIMIZE)  then
		Application.Minimize
	else
		DefaultHandler(Msg) ;
   if (Msg.CmdType = SC_MAXIMIZE) then RefreshImagesTimer.enabled := true;
end;
{$ENDIF}

function SelectedImagePanel: TScrollBox;
begin
	case SelectedImageNum of
		3: result := ImgForm.TriplePanel;
		2: result := ImgForm.TriplePanel;
		else result := ImgForm.TriplePanel;
	end;
end;

function DrawToolSelected: boolean;
begin
	if ( ImgForm.PenBtn.Down) or ( ImgForm.ClosedPenBtn.Down) or (ImgForm.FillBtn.Down) or (ImgForm.EllipseBtn.Down) then
		result := true
	else
		result := false;
end;

procedure TImgForm.WriteIniFile;
var
  lInc: integer;
  lIni: string;
  lIniFile: TIniFile;
begin
  lIni:= IniName;
  if (DiskFreeEx(lIni) < 1) or (not gBGIMg.SaveDefaultIni) then
	exit;
  //lIniFile := TIniFile.Create(changefileext(paramstr(0),'.ini'));
  lIniFile := TIniFile.Create(lIni);//DefaultsDir('')+ParseFileName(extractfilename(paramstr(0)))+'.ini');
  //recent files
  lIniFile.WriteString('MRU', 'file0', gMRIcroOverlay[kBGOverlayNum].HdrFilename);
  for lInc := 1 to knMRU do
	  lIniFile.WriteString('MRU', 'file'+inttostr(lInc), gMRUstr[lINc]);
  //STR
  //lIniFile.WriteString('STR', 'FSLDIR',gBGImg.FSLDIR);
  //lIniFile.WriteString('STR', 'FSLBETEXE',gBGImg.FSLBETEXE);

  lIniFile.WriteString('STR', 'FSLBASE',gBGImg.FSLBASE);
  lIniFile.WriteString('STR', 'FSLOUTPUTTYPE',gBGImg.FSLOUTPUTTYPE);
  //Booleans
  lIniFile.WriteString('BOOL', 'LoadUInt16asFloat32',Bool2Char(gBGImg.LoadUInt16asFloat32));
  lIniFile.WriteString('BOOL', 'DarkMode',Bool2Char(gBGImg.DarkMode));
  lIniFile.WriteString('BOOL', 'Reslice',Bool2Char(gBGImg.ResliceOnLoad));
  lIniFile.WriteString('BOOL', 'ResliceOrtho',Bool2Char(gBGImg.OrthoReslice));
  lIniFile.WriteString('BOOL', 'ShowDraw',Bool2Char(gBGImg.ShowDraw));
  lIniFile.WriteString('BOOL', 'ShowDraw',Bool2Char(gBGImg.ShowDraw));
  {$IFDEF LCLCocoa}lIniFile.WriteString('BOOL', 'DarkMode',Bool2Char(gBGImg.DarkMode)); {$ENDIF}
  lIniFile.WriteString('BOOL', 'ThinPen',Bool2Char(gBGImg.ThinPen));

  lIniFile.WriteString('BOOL', 'Smooth2D',Bool2Char(Menu2DSmooth.checked));
  lIniFile.WriteString('BOOL', 'XBar',Bool2Char(XBarBtn.Down));
  lIniFile.WriteString('BOOL', 'OverlaySmooth',Bool2Char(OverlaySmoothMenu.Checked));
  lIniFile.WriteString('BOOL', 'LRmirror',Bool2Char(gBGImg.Mirror));
  lIniFile.WriteString('BOOL', 'Yoke',Bool2Char(gYoke));
  lIniFile.WriteString('BOOL', 'SingleRow',Bool2Char(gBGImg.SingleRow));
  lIniFile.WriteString('BOOL', 'FlipAx',Bool2Char(gBGImg.FlipAx));
  lIniFile.WriteString('BOOL', 'FlipSag',Bool2Char(gBGImg.FlipSag));
  YokeTimer.Enabled := gYoke;
  //Integers
  //lIniFile.WriteString('INT', 'ResizeBeforeRescale',IntToStr(gBGImg.ResizeBeforeRescale));
  lIniFile.WriteString('INT', 'FontSize',IntToStr(gBGImg.FontSize));
  lIniFile.WriteString('INT', 'SaveImgFilter',IntToStr(gBGImg.SaveImgFilter));
  lIniFile.WriteString('INT', 'SaveVoiFilter',IntToStr(gBGImg.SaveVoiFilter));
  lIniFile.WriteString('INT', 'PlanarRGB',IntToStr(gBGImg.PlanarRGB));

  lIniFile.WriteString('INT', 'MaxDim',IntToStr(gBGImg.MaxDim));
  lIniFile.WriteString('INT', 'LicenseID',IntToStr(gBGImg.LicenseID));
  lIniFile.WriteString('INT', 'Zoom',IntToStr(ZoomDrop.ItemIndex));
  lIniFile.WriteString('INT', 'LUT',IntToStr(gMRIcroOverlay[kBGOverlayNum].LUTindex));
  lIniFile.WriteString('INT', 'XBarGap',IntToStr(gBGImg.XBarGap));
  lIniFile.WriteString('INT', 'XBarThick',IntToStr(gBGImg.XBarThick));
  lIniFile.WriteString('INT', 'XBarClr',IntToStr(gBGIMg.XBarClr));
  lIniFile.WriteString('INT', 'VOIClr',IntToStr(gBGIMg.VOIClr));
  if (gBGImg.BGTransPct < 0) or (gBGImg.BGTransPct > 90) then
    gBGImg.BGTransPct := 20; //additive or transparent values can confuse users
  if (gBGImg.OverlayTransPct < 0) or (gBGImg.OverlayTransPct > 90) then
    gBGImg.OverlayTransPct := 20; //additive or transparent values can confuse users
  lIniFile.WriteString('INT', 'BGTransPct',IntToStr(gBGImg.BGTransPct));
  lIniFile.WriteString('INT', 'OverlayTransPct',IntToStr(gBGImg.OverlayTransPct));
  lIniFile.WriteString('INT','MaxThreads',IntToStr(gnCPUThreads));

  lIniFile.WriteString('INT', 'LesionDilate',IntToStr(gBGImg.LesionDilate));
  lIniFile.WriteString('INT', 'LesionSmooth',IntToStr(gBGImg.LesionSmooth));
//  {$ELSE}
//  lIniFile.WriteString('INT', 'MaxThreads',IntToStr(gMaxCPUThreads));
//  {$ENDIF}
    lIniFile.WriteString('INT', 'SigDigits',IntToStr(gBGImg.SigDig));
    lIniFile.WriteString('INT', 'ImageSeparation',IntToStr(gBGImg.ImageSeparation));


  lIniFile.WriteString('INT', 'SPMDefaultsStatsFmriT',IntToStr(gBGImg.SPMDefaultsStatsFmriT));
  lIniFile.WriteString('INT', 'SPMDefaultsStatsFmriT0',IntToStr(gBGImg.SPMDefaultsStatsFmriT0));

  lIniFile.Free;
end;
(*
function registerfiletype(inft,inkey,desc,icon:string): boolean;
var myreg : treginifile;
	ct : integer;
	ft,key: string;
begin
	 result := true;
	 ft := inft;
	 key := inkey;
	 ct := pos('.',ft);
	 while ct > 0 do begin
		   delete(ft,ct,1);
		   ct := pos('.',ft);
	 end;
	 if (ft = '') or (Application.ExeName = '') then exit; //not a valid file-ext or ass. app
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
	 myreg.free;
end;   *)

procedure WriteIni2Form (lBGImg: TBGImg);
begin
     ImgForm.ToolPanel.Visible := lBGImg.ShowDraw;
     ImgForm.DrawMenu.Visible := lBGImg.ShowDraw;
     ImgForm.DrawHiddenMenu.Visible := not lBGImg.ShowDraw;
end;

procedure TImgForm.SetIniMenus;
begin
  XBarBtn.Down := gBGImg.XBarVisible;
  CrossHair1.Checked:= XBarBtn.Down;
  YokeMenu.Checked := gYoke;
  if (gBGImg.StretchQuality = sqLow) then
    Menu2DSmooth.checked := false
  else begin
       Menu2DSmooth.checked := true;
       gBGImg.StretchQuality := sqHigh;
  end;
  //Menu2DSmoothClick(nil);//set quality
end;

procedure TImgForm.ReadIniFile;
var
  lInc,lFilenum: integer;
  lFilename: string;
  lIniFile: TIniFile;
begin
  //lFilename := changefileext(paramstr(0),'.ini');

  lFilename := ininame;//DefaultsDir('')+ParseFileName(extractfilename(paramstr(0)))+'.ini';
  if not FileexistsEx(lFilename) then begin
    //DrawMenu.Visible := ToolPanel.visible;
     WriteIni2Form(gBGImg);
    exit;
  end;
  lIniFile := TIniFile.Create(lFilename);
  gMRUstr[0] := lIniFile.ReadString('MRU', 'file0', '');//file0 - last file viewed
  lFileNum := 0;
  for lInc := 1 to knMRU do begin
	  lFilename := lIniFile.ReadString('MRU', 'file'+inttostr(lInc), '');
	  if (length(lFilename) > 0) and (fileexistsex(lFilename)) then begin
		 Inc(lFileNum);
		 gMRUstr[lFileNum] := lFilename;
	  end;
  end;
  gBGImg.FSLOUTPUTTYPE := lIniFile.ReadString('STR', 'FSLOUTPUTTYPE', gBGImg.FSLOUTPUTTYPE);
  //gBGImg.FSLDIR := lIniFile.ReadString('STR', 'FSLDIR', gBGImg.FSLDIR);
  //gBGImg.FSLBETEXE := lIniFile.ReadString('STR', 'FSLBETEXE', gBGImg.FSLBETEXE);
  gBGImg.FSLBASE := lIniFile.ReadString('STR', 'FSLBASE', gBGImg.FSLBASE);
  gBGImg.LoadUInt16asFloat32 := IniBool(lIniFile,'LoadUInt16asFloat32', gBGImg.LoadUInt16asFloat32);
  gBGImg.DarkMode := IniBool(lIniFile,'DarkMode',gBGImg.DarkMode);
  gBGImg.ResliceOnLoad := IniBool(lIniFile,'Reslice',gBGImg.ResliceOnLoad);
  gBGImg.OrthoReslice := IniBool(lIniFile,'ResliceOrtho',gBGImg.OrthoReslice);
  gBGImg.ThinPen := IniBool(lIniFile, 'ThinPen',True);

  gBGImg.ShowDraw := IniBool(lIniFile, 'ShowDraw',gBGImg.ShowDraw);
  WriteIni2Form(gBGImg);
  if IniBool(lIniFile,'Smooth2D',Menu2DSmooth.checked) then
     gBGImg.StretchQuality := sqHigh
  else
      gBGImg.StretchQuality := sqLow;
  //Menu2DSmooth.checked := IniBool(lIniFile,'Smooth2D',Menu2DSmooth.checked);
  Menu2DSmoothClick(nil);//set quality
  gBGImg.XBarVisible := IniBool(lIniFile,'XBar',XBarBtn.Down);
  gBGImg.OverlaySmooth := IniBool(lIniFile,'OverlaySmooth',gBGImg.OverlaySmooth);
  OverlaySmoothMenu.Checked := gBGImg.OverlaySmooth;
  gBGImg.Mirror := IniBool(lIniFile,'LRmirror',gBGImg.Mirror);
  FlipLRmenu.Checked := gBGImg.Mirror;
  gYoke := IniBool(lIniFile,'Yoke',gYoke);
    gBGImg.SingleRow := IniBool(lIniFile,'SingleRow',gBGImg.SingleRow);
   gBGImg.FlipAx := IniBool(lIniFile,'FlipAx',gBGImg.FlipAx);
  gBGImg.FlipSag := IniBool(lIniFile,'FlipSag',gBGImg.FlipSag);
  gBGImg.MaxDim := IniInt(lIniFile,'MaxDim',gBGImg.MaxDim);
  gBGImg.PlanarRGB := IniInt(lIniFile,'PlanarRGB',gBGImg.PlanarRGB);

  gBGImg.LicenseID := IniInt(lIniFile,'LicenseID',gBGImg.LicenseID);
{$IFNDEF FPC}
  ZoomDrop.SetItemIndex(IniInt(lIniFile,'Zoom',ZoomDrop.ItemIndex));
  LUTDrop.SetItemIndex(IniInt(lIniFile,'LUT',LUTDrop.ItemIndex));
{$ELSE}
  ZoomDrop.ItemIndex := (IniInt(lIniFile,'Zoom',ZoomDrop.ItemIndex));
  LUTDrop.ItemIndex:= (IniInt(lIniFile,'LUT',LUTDrop.ItemIndex));
{$ENDIF}
  gBGImg.XBarGap := IniInt(lIniFile,'XBarGap',gBGImg.XBarGap);
  gBGImg.XBarThick := IniInt(lIniFile,'XBarThick',gBGImg.XBarThick);
  gBGImg.XBarClr := IniInt(lIniFile,'XBarClr',gBGImg.XBarClr);
  gBGImg.VOIClr := IniInt(lIniFile,'VOIClr',gBGImg.VOIClr);
  gBGImg.BGTransPct := IniInt(lIniFile,'BGTransPct',gBGImg.BGTransPct);
  gBGImg.OverlayTransPct := IniInt(lIniFile,'OverlayTransPct',gBGImg.OverlayTransPct);
  gnCPUThreads := IniInt(lIniFile,'MaxThreads',gnCPUThreads);
    gBGImg.SigDig := IniInt(lIniFile,'SigDigits',gBGImg.SigDig);
    gBGImg.ImageSeparation := IniInt(lIniFile,'ImageSeparation',gBGImg.ImageSeparation);
     gBGImg.FontSize := IniInt(lIniFile,'FontSize',gBGImg.FontSize);
     gBGImg.SaveImgFilter := IniInt(lIniFile,'SaveImgFilter',gBGImg.SaveImgFilter);
     gBGImg.SaveVoiFilter := IniInt(lIniFile,'SaveVoiFilter',gBGImg.SaveVoiFilter);
  gBGImg.SPMDefaultsStatsFmriT := IniInt(lIniFile,'SPMDefaultsStatsFmriT',gBGImg.SPMDefaultsStatsFmriT);
  gBGImg.SPMDefaultsStatsFmriT0 := IniInt(lIniFile,'SPMDefaultsStatsFmriT0',gBGImg.SPMDefaultsStatsFmriT0);
  gBGImg.LesionSmooth := IniInt(lIniFile,'LesionSmooth',gBGImg.LesionSmooth);
  gBGImg.LesionDilate := IniInt(lIniFile,'LesionDilate',gBGImg.LesionDilate);



  SetSubmenuWithTag(BGTransPctMenu, gBGImg.BGTransPct);
  SetSubmenuWithTag(OverlayTransPctMenu, gBGImg.OverlayTransPct);
  lIniFile.Free;

end; //ReadIniFile

//lStrings := TStringList.Create;


procedure TImgForm.UpdateColorSchemes;
var
	lSearchRec: TSearchRec;
        lStrings : TStringList;
begin
  LUTdrop.Items.Clear;
  LUTdrop.Items.Add('Grayscale');
  LUTdrop.Items.Add('Red');
  LUTdrop.Items.Add('Blue');
  LUTdrop.Items.Add('Green');
  LUTdrop.Items.Add('Violet [r+b]');
  LUTdrop.Items.Add('Yellow [r+g]');
  LUTdrop.Items.Add('Cyan [g+b]');
  lStrings := TStringList.Create;
  if FindFirst(gColorSchemeDir+pathdelim+'*.lut', faAnyFile, lSearchRec) = 0 then
	 repeat
                lStrings.Add(ParseFileName(ExtractFileName(lSearchRec.Name)));
                //LUTdrop.Items.Add(ParseFileName(ExtractFileName(lSearchRec.Name)));
	 until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
  lStrings.Sort;
  LUTdrop.Items.AddStrings(lStrings);
  lStrings.Free;
  //LUTDrop.DropDownCount := 66;//LUTDrop.Items.Count;
end;//UpdateColorSchemes

(*procedure TImgForm.UpdateColorSchemes;
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
 xxx
  //LUTDrop.DropDownCount := 66;//LUTDrop.Items.Count;
end;//UpdateColorSchemes
*)

procedure TImgForm.BETmenuClick(Sender: TObject);
begin
  BetForm.show;
end;

procedure TImgForm.ApplyClusterThreshold1Click(Sender: TObject);
var
	lNumberofFiles,lC,lClusterSz: integer;
    lThresh: double;
	lFilename: string;
begin
	CloseImagesClick(nil);
	if not OpenDialogExecute(kImgFilter,'Select NIfTI format images to convert',true) then exit;
	lNumberofFiles:= ImgForm.OpenHdrDlg.Files.Count;
    lClusterSz := ReadIntForm.GetInt('Minimum cluster size [in voxels]: ', 1,32,9999);
    lThresh := ReadFloatForm.GetFloat('Include voxels with an intensity above: ', 0,2,9999);
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := ImgForm.OpenHdrDlg.Files[lC-1];
		ImgForm.OpenAndDisplayImg(lFilename,True);
		//lFilename := changefileextX(lFilename,'I'+inttostr(round(lThresh))+'C'+inttostr(lClusterSz)+'.nii.gz');
  		lFilename := changefileprefix(lFilename,'I'+inttostr(round(lThresh))+'C'+inttostr(lClusterSz));
                if ClusterFilterScrnImg (gMRIcroOverlay[kBGOverlayNum],lClusterSz,lThresh ) then
                if ImgVaries(gMRIcroOverlay[kBGOverlayNum]) then
                          SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems,gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr)
                else
                    showmessage('No clusters survive filter '+ ImgForm.OpenHdrDlg.Files[lC-1]);
                ProgressBar1.Position := lC;
	end;
        if fileexistsEX(lFilename) then
           ImgForm.OpenAndDisplayImg(lFilename,True);
	ProgressBar1.Position := 0;
end;

procedure TImgForm.C(Sender: TObject);
begin

end;

procedure TImgForm.CropMenuClick(Sender: TObject);
begin
  CropEdgeForm.Show;
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

function isNifti(fnm: string): boolean;
var
 lExt: string;
begin
     result := true;
     lExt := uppercase(extractfileext(fnm));
     if (lExt = '.NII') or (lExt = '.HDR') or (lExt = '.VOI') then exit;
     if (lExt = '.GZ') then begin
        lExt := uppercase(extractfileext(changefileext(fnm,'')));
        if (lExt = '.NII') then exit;
     end;
     result := false;
end;

function isDICOM(fnm: string): boolean;
var
   f: file;
   sz: integer;
   magic: array [0..3] of char; //signature of DICOM = 'DICM'
begin
     if (isNifti(fnm)) then
        exit(false);
     result := true;
     if DirectoryExists(fnm) then exit;
     AssignFile(f, fnm);
     FileMode := fmOpenRead;
     Reset(f,1);
     sz := FileSize(f);
     if sz < 256 then begin
        CloseFile(f);
        exit(false);
     end;
     Seek(f, 128);
     magic[0] := 'x'; //just to hide compiler warning
     blockread(f, magic[0],  sizeof(magic));
     //showmessage(magic); //will report DICM for DICOM images, but not DICOM meta objects
     if (magic[0] <> 'D') or (magic[1] <> 'I') or (magic[2] <> 'C') or (magic[3] <> 'M') then
        result := false;
end;

procedure TImgForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
   fnm: string;
   ss: TShiftState;
begin
  ss:=getKeyshiftstate;
  if length(FileNames) < 1 then
     exit;
  fnm := Filenames[0];
  if isDICOM(fnm) then begin //part-10 compliant DICOM images should have "DICM" signature, but this is missing for some DICOM meta data
     //if (not isNifti(Filenames[0])) then begin
     //printf('>drop:'+fnm);
     fnm := dcm2Nifti(dcm2niiForm.getCurrentDcm2niix, fnm);
     //printf('>got:'+fnm);
     if fnm = '' then exit;
     OpenAndDisplayImg(fnm,true);
     if fnm <> Filenames[0] then
        deletefile(fnm);
     exit;
  end;
  if (ssMeta in ss) or (ssCtrl in ss) then begin
     LoadOverlay(fnm);
     exit;
  end;
  OpenAndDisplayImg(fnm,true);
end;

procedure TImgForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (XViewEdit.focused) or (YViewEdit.focused) or (ZViewEdit.focused) or (MinWindowEdit.focused) or (MaxWindowEdit.focused) then
     exit;
  Case Key of
    36: DecViewEdit(YViewEdit);
    35: IncViewEdit(YViewEdit);
    37: DecViewEdit(XViewEdit);
    38: IncViewEdit(ZViewEdit);
    39: IncViewEdit(XViewEdit);
    40: DecViewEdit(ZViewEdit);

  end; //case Key
      (* if WheelDelta < 0 then begin
        Case SelectedImageNum of
             3: DecViewEdit(YViewEdit);
             2: DecViewEdit(XViewEdit);
             else DecViewEdit(ZViewEdit);
        end;
     end else begin
         Case SelectedImageNum of
	      3: IncViewEdit(YViewEdit);
	      2: IncViewEdit(XViewEdit);
	      else IncViewEdit(ZViewEdit);
         end;
     end;*)
end;

procedure TImgForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  //imgform.caption := 'zzz';
end;



procedure TImgForm.Header1Click(Sender: TObject);
begin
  DisplayHdrClick(nil);
end;

function TImgForm.ActiveLayer:integer;
begin
     result := ImgForm.LayerDrop.ItemIndex;
     if result < 0 then
        result := 0;
end;

{$DEFINE NoTEST}

{$IFDEF TEST}
procedure DrawBMP2( lx, ly: integer; var lBuff: RGBQuadp; var lImage: TImage);
//uses  GraphType, IntfGraphics
var
  IntfImage: TLazIntfImage;
  ScanLineImage: TLazIntfImage;
  ImgFormatDescription: TRawImageDescription;
  lBitmap: TBitmap;
begin
  lBitmap:=TBitmap.Create;
  ScanLineImage:=TLazIntfImage.Create(0,0);
  ImgFormatDescription.Init_BPP32_B8G8R8_BIO_TTB(lx,ly);
  ScanLineImage.DataDescription:=ImgFormatDescription;
  // call the pf24bit specific drawing function
  Move(lBuff^[1],PByte(ScanLineImage.GetDataLineStart(0))[1],lx*ly*sizeof(TRGBquad) );
  lBitmap.Width:=ScanLineImage.Width;
  lBitmap.Height:=ScanLineImage.Height;
  IntfImage:=lBitmap.CreateIntfImage;
  // convert the content from the very specific to the current format
  IntfImage.CopyPixels(ScanLineImage);
  lBitmap.LoadFromIntfImage(IntfImage);
  ScanLineImage.Free;
  IntfImage.Free;
  lImage.Picture.Bitmap := lBitmap;
  lBitmap.Free;
end;

procedure FZ;
var
   l2Time,lTime: DWord;
   y,x,lx, ly, lpos: integer;
   lBuff: RGBQuadp ;
begin
  lx := 320;
  ly := 320;
  getmem(lBuff,(lx*ly)*sizeof( TRGBquad));
  lpos := 0;
  for y := 1 to ly do begin
      for x := 1 to lx do begin
          inc(lpos);
          lBuff^[lpos].rgbblue := (y mod 255);
          lBuff^[lpos].rgbgreen :=(y mod 255);
          lBuff^[lpos].rgbred := (x mod 255) ;
          lBuff^[lpos].rgbreserved := 0;
      end;
  end;
  l2Time := GetTickCount;
  for y := 1 to 100 do
      DrawBMP2( lx, ly, lBuff,HistogramForm.HistoImage{lImage});
  l2Time := GetTickCount - l2Time;
  lTime := GetTickCount;
  for y := 1 to 100 do
      DrawBMP( lx, ly, lBuff,HistogramForm.HistoImage{lImage});
  lTime := GetTickCount - lTime;
  HistogramForm.Caption := inttostr(lTime)+'  '+inttostr(l2Time);
  freemem(lBuff);
end;
{$ENDIF}

procedure TImgForm.HistoMenuClick(Sender: TObject);
VAR
   lLayer: integer;
begin
     {$IFDEF TEST}
     FZ;
     {$ELSE}

		lLayer := ActiveLayer;
		DrawHistogram(gMRIcroOverlay[lLayer],HistogramForm.HistoImage{lImage});
                HistogramForm.Caption := 'Histogram: '+extractfilename(gMRIcroOverlay[lLayer].HdrFileName);
         {$ENDIF}
   HistogramForm.show;
  //HistogramForm.BringToFront;
end;



procedure TImgForm.MNIMenuClick(Sender: TObject);
begin
  MNIForm.show;
  //MNIForm.BringToFront;

end;

procedure TImgForm.N4DTraces1Click(Sender: TObject);
begin
    Graph4DForm.show;
    //Graph4DForm.BringToFront;
end;

procedure TImgForm.NIIVOIClick(Sender: TObject);
 var
	lNumberofFiles,lC: integer;
	lFilename: string;
begin
	CloseImagesClick(nil);
	if not OpenDialogExecute(kImgFilter {10/2007},'Select NIfTI format images to convert',true) then exit;
	lNumberofFiles:= ImgForm.OpenHdrDlg.Files.Count;
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := ImgForm.OpenHdrDlg.Files[lC-1];
		ImgForm.OpenAndDisplayImg(lFilename,True);
		lFilename := changefileextx(lFilename,'.voi'); ////Xversion 10/2007 - removes .nii.gz not just gz
		//SaveAsVOIorNIFTIcore (lFilename, lByteP, lVoxels, 1, gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
		SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
		CloseVOIClick(nil);
		ProgressBar1.Position := lC;
	end;
	ProgressBar1.Position := 0;

end;


procedure TImgForm.PGImageCorDblClick(Sender: TObject);
begin
        if Graph4DForm.visible then
        Graph4DForm.RefreshBtn.click;
end;

procedure TImgForm.Preferences1Click(Sender: TObject);
begin
     PrefForm.Show; //Cocoa has odd effects with showmodal: to exhibit try opening the pref form twice. The second time the text is scrambled
     //PrefForm.ShowModal;
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
        //svn lHdr.ImgBuffer := ByteP($fffffff0 and (integer(lHdr.ImgBufferUnaligned)+15));
        lHdr.ImgBuffer := align(lHdr.ImgBufferUnaligned, 16);
        lo32Buf := SingleP( lHdr.ImgBuffer );
        if lBPP = 4 then begin
           l32Buf := SingleP( gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	   for lInc := 1 to lImgSamples do
               lo32Buf^[lInc] :=  (l32Buf^[lInc]+lRescaleIntercept) * lRescaleSlope;
        end else if lBPP = 2 then begin //lBPP=4 else
           l16Buf := SmallIntP( gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	   for lInc := 1 to lImgSamples do
               lo32Buf^[lInc] :=  (l16Buf^[lInc]+lRescaleIntercept) * lRescaleSlope;
        end else if lBPP = 1 then begin //lBPP=2 else
	   for lInc := 1 to lImgSamples do
               lo32Buf^[lInc] :=  (gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lInc]+lRescaleIntercept) * lRescaleSlope;
        end;//lBPP = 1
	SaveAsVOIorNIFTI(bytep(lo32Buf),lImgSamples,4,1,false,lHdr.NiftiHdr,'rscl'+extractfilename(gMRIcroOverlay[kBGOverlayNum].HdrFilename));
        //SaveAsVOIorNIFTI(gMRIcroOverlay[lLayer].ImgBuffer,gMRIcroOverlay[lLayer].ImgBufferItems,gMRIcroOverlay[lLayer].ImgBufferBPP,1,false,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[lLayer].HdrFilename);
        FreeMem(lHdr.ImgBufferUnaligned);
        //lFilename := 'c:\striped2.hdr';
        //SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
     result := true;
end;


function TImgForm.OpenAndDisplayHdr (var lFilename: string; var lHdr: TMRIcroHdr): boolean;
var lFileDir: string;
begin
  FreeImgMemory(lHdr);
  result := false;
  NIFTIhdr_ClearHdr(lHdr);
  if not NIFTIhdr_LoadHdr(lFilename, lHdr) then exit;
  HdrForm.WriteHdrForm(lHdr.NIFTIhdr, lHdr.DiskDataNativeEndian, lFilename);
  lFileDir := extractfiledir(lFilename);
  if lFileDir <> gTemplateDir then
     OpenHdrDlg.InitialDir := lFileDir;
  HdrForm.SaveHdrDlg.InitialDir := lFileDir;
  HdrForm.SaveHdrDlg.FileName := lFilename; //make this default file to write
  if length(lFilename) < 79 then
     HdrForm.StatusBar1.Panels[1].text := lFilename
  else
      HdrForm.StatusBar1.Panels[1].text := extractfilename(lFilename);
  HdrForm.StatusBar1.Panels[0].text := 'Img= '+inttostr(ComputeImageDataBytes(lHdr));
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
           if not OpenAndDisplayHdr(gMRIcroOverlay[kBGOverlayNum].HdrFileName,gMRIcroOverlay[kBGOverlayNum]) then exit;
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
        end;*)
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

procedure TImgForm.Resliceimage1Click(Sender: TObject);
begin
   ResliceFSL;
end;

procedure TImgForm.SaveasNIfTI1Click(Sender: TObject);
  var
	lLayer: integer;
begin
  //if not SaveDialog2.Execute then exit;
	lLayer := ActiveLayer;
	if gMRIcroOverlay[lLayer].ImgBufferItems=0 then begin
		Showmessage('You must load an image [File/Open] before you can save the image.');
		exit;
	end;
	if (not IsNifTiMagic(gMRIcroOverlay[lLayer].niftiHdr)) then
		Showmessage('Warning: image will be saved with NIfTI spatial transform - ensure this image matches the orientation of the template images.');
	SaveAsVOIorNIFTI(gMRIcroOverlay[lLayer].ImgBuffer,gMRIcroOverlay[lLayer].ImgBufferItems,gMRIcroOverlay[lLayer].ImgBufferBPP,1,false,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[lLayer].HdrFilename);
end;

procedure ApplySaveDlgFilter (lSaveDlg: TSaveDialog);
var
   lLen,lPos,lPipes,lPipesReq: integer;
   lExt,lName: string;
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
     lName :=  lSaveDlg.Filename;
     if lExt <> '' then
        lSaveDlg.Filename := ChangeFileExtX(lName,lExt);
end;



procedure TImgForm.SaveDialog1Close(Sender: TObject);
begin
  //ApplySaveDlgFilter(SaveDialog1);
end;

procedure TImgForm.ToolBar2Click(Sender: TObject);
begin

end;

procedure TImgForm.ToolPanelClick(Sender: TObject);
begin

end;

procedure TImgForm.TriplePanelMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
    //LabelX.caption := inttostr(random(888));
end;


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
	lPos: integer;
begin
  While Recent1.Count < knMRU do begin
        NewItem := TMenuItem.Create(Self);
        Recent1.Add(NewItem);
  end;
  for lPos := 1 to knMRU do begin//for each MRU
      Recent1.Items[lPos-1].Visible:=gMRUstr[lPos] <> '';
      Recent1.Items[lPos-1].Caption :=ExtractFileName(gMRUstr[lPos]);
      Recent1.Items[lPos-1].Tag := lPos;
      Recent1.Items[lPos-1].onclick :=  OpenTemplateMRU;
      {$IFDEF Darwin}
      Recent1.Items[lPos-1].ShortCut := ShortCut(Word('1')+ord(lPos-1), [ssMeta]);
      {$ELSE}
      Recent1.Items[lPos-1].ShortCut := ShortCut(Word('1')+ord(lPos-1), [ssCtrl]);
      {$ENDIF}
  end;//for each MRU
end;  //UpdateMRU

procedure TImgForm.UpdateTemplates;
var
  NewItem: TMenuItem;
	lN : integer;
	lFName : String;
	lSearchRec: TSearchRec;
begin
  While Templates1.Count < knMRU do begin
        NewItem := TMenuItem.Create(Self);
        Templates1.Add(NewItem);
  end;
  lN := 0;
  if FindFirst(gTemplateDir+pathdelim+'*.*', faAnyFile, lSearchRec) = 0 then begin
	 repeat
	       lFName := lSearchRec.Name;
	       if IsNIfTIHdrExt (lFName) then begin
                Templates1.Items[lN].Caption :=ExtractFileName(lFName);//(ParseFileName(ExtractFileName(lFName)));
		   Templates1.Items[lN].Tag := 0;
                   Templates1.Items[lN].visible := true;
                   Templates1.Items[lN].onclick :=  OpenTemplateMRU;
                    {$IFDEF Darwin}
                    Templates1.Items[lN].ShortCut := ShortCut(Word('1')+ord(lN), [ssMeta, ssAlt]);
                    {$ELSE}
                   Templates1.Items[lN].ShortCut := ShortCut(Word('1')+ord(lN), [ssCtrl, ssShift]);
                    {$ENDIF}
                   inc(lN);
	       end;
	 until (FindNext(lSearchRec) <> 0) or (lN >= knMRU);
  end;// else
  if (lN = 0) then ImgForm.Caption :=('Unable to find any files in the folder '+gTemplateDir+pathdelim);

  while lN < knMRU do begin
        Templates1.Items[lN].visible := false;
        inc(lN);
  end;
  FindClose(lSearchRec);
end;//UpdateTemplates

(*NOT OSX 10.7 friendly... procedure TImgForm.UpdateMRU;//most-recently-used menu
var
  NewItem: TMenuItem;
	lPos,lN : integer;
begin
  //Recent1.Clear;
  //While Recent1.Count > 1 do Recent1.Delete(0);
  // While Recent1.Count > 0 do Recent1.Items[0].Free;
  lN := 0;
  for lPos := 1 to knMRU do begin//for each MRU
      if gMRUstr[lPos] <> '' then begin
		   inc(lN);
		   NewItem := TMenuItem.Create(Self);
		   NewItem.Caption :=ExtractFileName(gMRUstr[lPos]);//(ParseFileName(ExtractFileName(lFName)));
		   NewItem.Tag := lN;
                   {$IFDEF FPC}
                    NewItem.onclick :=  OpenTemplateMRU; //Lazarus
                    {$ELSE}
                     NewItem.onclick :=  OpenTemplateMRU;
                     {$ENDIF}
		   NewItem.ShortCut := ShortCut(Word('1')+ord(lN-1), [ssCtrl]);
		   Recent1.Add(NewItem);
      end;//if mru exists
  end;//for each MRU
end;  //UpdateMRU

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
		   NewItem.Tag := 0;
                   {$IFDEF FPC}
                    NewItem.onclick :=  OpenTemplateMRU; //Lazarus
                    {$ELSE}
                     NewItem.onclick :=  OpenTemplateMRU;
                     {$ENDIF}
		   if (lN+knMRU) <= 9 then
                      NewItem.ShortCut := ShortCut(Word('1')+knMRU+ord(lN-1), [ssCtrl]);
		   Templates1.Add(NewItem);
		  end;
	 until (FindNext(lSearchRec) <> 0)
  end;
  FindClose(lSearchRec);
end;//UpdateTemplates   *)

procedure TImgForm.OpenTemplateMRU(Sender: TObject);//open template or MRU
//Templates have tag set to 0, Most-Recently-Used items have tag set to position in gMRUstr
var
	lFilename: string;
        ss: TShiftState;
begin
  ss:=getKeyshiftstate;
     if sender = nil then begin
        //autolaunch with last image, or last template image in list
        lFilename :=  gMRUstr[0];
	if (lFilename = '') or (not FileExistsEX(lFilename)) then begin
	   if Templates1.Count > 0 then
	      Templates1.Items[Templates1.Count-1].click;
	   exit;
	end;
        OpenAndDisplayImg(lFilename,true); //open but do not add templates to MRU
     end else if (Sender as TMenuItem).tag = 0 then begin
		lFilename := gTemplateDir+pathdelim+(Sender as TMenuItem).caption ;//+ '.hdr';
                if (ssMeta in ss) or (ssCtrl in ss) then
                   LoadOverlay(lFilename)
                else
                    OpenAndDisplayImg(lFilename,false); //open but do not add templates to MRU
	 end else if (Sender as TMenuItem).tag <= knMRU then begin
		 lFilename := gMRUstr[(Sender as TMenuItem).tag];
                 if (ssMeta in ss) or (ssCtrl in ss) then
                   LoadOverlay(lFilename)
                else
		    OpenAndDisplayImg(lFilename,true);
	 end else
		 Showmessage('OpenTemplateMRU error.');
end;

function TImgForm.OpenAndDisplayImg(var lFilename: string; lAdd2MRU: boolean): boolean;
var
   lVal: integer;
   x: string;
   isTempNii: boolean = false;
begin
	 Result := false;
         {$IFDEF DICOMdrop}
         if not IsNIfTIHdrExt(lFilename) then begin
            x := dcm2niiForm.getCustomDcm2niix();
            if fileexists(x) then begin
                lFilename := dcm2Nifti(x, lFilename);
                if fileexists(lFilename) and (lFilename <> x) then
                   isTempNii := true;
            end;
         end;
         {$ENDIF}
         if (DirectoryExists(lFilename)) then exit;
         if (FSize(lFilename)) < 348 then exit; //to small to be a header or DICOM image
	 if not OpenAndDisplayHdr(lFilename,gMRIcroOverlay[kBGOverlayNum]) then exit;


         //if (ssShift in KeyDataToShiftState(vk_Shift)) then begin
         //    if not OpenImg(gBGImg,gMRIcroOverlay[0],true,false,false,not gBGImg.ResliceOnLoad,false) then exit
         //end else
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
	 (*XViewEdit.Value := round(gBGImg.ScrnOri[1]);//gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[1] div 2;
	 YViewEdit.Value := round(gBGImg.ScrnOri[2]);//gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[2]div 2;
         lVal :=    round(gBGImg.ScrnOri[3]);
         if lVal < 1 then
            lVal := 1;
         ZViewEdit.Value := lVal;//gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[3] div 2;*)

      XViewEdit.Value := Bound ( round(gBGImg.ScrnOri[1]),1,round(XViewEdit.MaxValue));
      YViewEdit.Value := Bound ( round(gBGImg.ScrnOri[2]),1,round(YViewEdit.MaxValue));
      ZViewEdit.Value := Bound ( round(gBGImg.ScrnOri[3]),1,round(ZViewEdit.MaxValue));
	 //ImgForm.Caption := extractfilename(paramstr(0))+' - '+lFilename;
      if length(lFilename) > 80 then
          StatusLabel.caption := 'Opened: '+extractfilename(lFilename)
      else
          StatusLabel.caption := 'Opened: '+lFilename;

	 Result := true;
  //LayerDrop.ItemIndex := 0;
  //LayerDropSelect(nil);
  if lAdd2MRU then Add2MRU(lFilename);
   if gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.datatype = kDT_RGB then begin   //RGB
       //we have loaded the first [red] plane - now load green and blue...
       OverlayOpenCore(lFilename,1);
       OverlayOpenCore(lFilename,2);
       //must use additive blending
       //gBGImg.BGTransPct := -1;
       //gBGImg.OverlayTransPct := -1;
       OverlayAdditive.Click;
       BGAdditive.Click;
   end;
  {$IFDEF FPC}
  XViewEditChange(nil);
  {$ENDIF}
  //showmessage(lFilename+' 666  '+ChangeFileext(lFilename,'.anat'));
   AnatForm.OpenAnat( ChangeFileextx(lFilename,'.anat'));
  if isTempNii then
     deletefile(lFilename);
end; //OpenAndDisplayImg

{$IFNDEF FPC}
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
{$ENDIF}

procedure TImgForm.Exit1Click(Sender: TObject);
begin
   ImgForm.Close;
end;

function XToStr(lR: extended; lDec: integer): string;
begin
	 result := FloatToStrF(lR, ffFixed,7,lDec);
end;

procedure TImgForm.DisplayHdrClick(Sender: TObject);
var
	lLayer:integer;
begin
  	lLayer := ActiveLayer;
        {$IFDEF LCLCocoa}
        setThemeMode(HdrForm, gBGImg.DarkMode);
        {$ENDIF}
	HdrForm.SaveHdrDlg.Filename := gMRIcroOverlay[lLayer].HdrFilename;
	HdrForm.WriteHdrForm (gMRIcroOverlay[lLayer].NIFTIhdr, gMRIcroOverlay[lLayer].DiskDataNativeEndian, gMRIcroOverlay[lLayer].HdrFilename);
	//HdrForm.ShowModal;
        HdrForm.Show;
        //HdrForm.BringToFront;
 //HdrForm.BringToFront;
end;

procedure TImgForm.Open1Click(Sender: TObject);
var
	lFilename: string;
begin
     CloseImagesClick(nil);
     if not OpenDialogExecute(kImgFilterPlusAny,'Select background image',false) then exit;
     lFilename := OpenHdrDlg.Filename;
     OpenAndDisplayImg(lFilename,True);
end;

procedure TImgForm.ToolSelectClick(Sender: TObject);
begin
	if (not ToolPanel.Visible) and ((Sender as TMenuItem).Tag > 0) then exit; //tools disabled
	 case (Sender as TMenuItem).Tag of
		  0: begin
                          XBarBtn.Down := not XBarBtn.Down;
                          CrossHair1.Checked := XBarBtn.Down;
                      {$IFDEF Darwin} XBarbtnClick(nil); exit;{$ENDIF}
                  end;
		  2: PenBtn.Down := true;
		  3: ClosedPenBtn.Down := true;
		  4: FillBtn.Down := true;
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
	  kSagView0: result := ImgForm.PGImageSag;
	  kCoroView0: result := ImgForm.PGImageCor;
	  else
		  result := ImgForm.PGImageAx;
 end;
end;

procedure TImgForm.SetDimension8(lInPGHt,lInPGWid:integer; lBuff: ByteP; lUndoOnly: boolean);
begin
    DefineBuffFX8(gDrawImg, lInPGWid,lInPGHt,lBuff);
    DefineBuffFX8(gUndoImg, lInPGWid,lInPGHt,lBuff);

end;

procedure WriteAxialVOI (lUndoOnly, lUpdateSliceNumber: boolean);
var lX,lY,lSliceOffset,lSliceSz,lSlicePos: integer;
	lInBuff: ByteP;
begin

	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lSliceSz := lX*lY;
	if lSliceSz < 1 then exit;
        if lUpdateSliceNumber then
           gBGImg.VOIUndoSlice := round(ImgForm.ZViewEdit.Value);
        if (gBGImg.VOIUndoSlice < 1) or (gBGImg.VOIUndoSlice > gBGImg.ScrnDim[3]) then exit;
	lSliceOffset := (gBGImg.VOIUndoSlice-1)*lX*lY;
	getmem(lInBuff,lSliceSz);
	for lSlicePos := 1 to lSliceSz do
		lInBuff^[lSlicePos]  :=  gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lSliceOffset+lSlicePos];
	ImgForm.SetDimension8(lY,lX, lInBuff,lUndoOnly);
	freemem(lInBuff);
end;

procedure  WriteCorVOI (lUndoOnly, lUpdateSliceNumber: boolean);
var lX,lY,lZ,lYOffset,lZOffset,lXYSliceSz,lPixel,lZPos,lXPos: integer;
	lInBuff: ByteP;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
        if lUpdateSliceNumber then
           gBGImg.VOIUndoSlice := round(ImgForm.YViewEdit.Value);
	if (gBGImg.VOIUndoSlice < 1) or (gBGImg.VOIUndoSlice > gBGImg.ScrnDim[2]) then exit;
        lYOffset := (lX) * (gBGImg.VOIUndoSlice-1);
	lXYSliceSz := (lX*lY);
	getmem(lInBuff,lZ*lX);
	lPixel := 0;
	for lZPos := 1 to lZ do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
	  for lXPos := 1 to lX do begin
		  inc(lPixel);
		lInBuff^[lPixel] :=
			gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lZOffset+lYOffset+lXPos];
	  end; //for each Y
	end; //for each Z
	ImgForm.SetDimension8(lZ,lX, lInBuff,lUndoOnly);
	freemem(lInBuff);
end;

procedure WriteSagVOI (lUndoOnly, lUpdateSliceNumber: boolean);
var lX,lY,lZ,lXOffset,lYOffset,lZOffset,lXYSliceSz,lPixel,lZPos,lYPos: integer;
	lInBuff: ByteP;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
	lXYSliceSz := lX*lY;
        if lUpdateSliceNumber then
           gBGImg.VOIUndoSlice := ImgForm.XViewEdit.Value;
        if (gBGImg.VOIUndoSlice < 1) or (gBGImg.VOIUndoSlice > gBGImg.ScrnDim[1]) then exit;
        lXOffset := round(gBGImg.VOIUndoSlice);
  //dec(lXOffset);//999+8

	getmem(lInBuff,lZ*lY);
	lPixel := 0;
	for lZPos := 1 to lZ do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
          if gBGImg.FlipSag then begin
            lYOffset := (lY - 1) * lX;
	    for lYPos := 1 to lY do begin
		    inc(lPixel);
		    lInBuff^[lPixel] := gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lZOffset+lYOffset+lXOffset];
		    lYOffset := lYOffset - lX;
	    end; //for each Y
          end else begin
            lYOffset := 0;
	    for lYPos := 1 to lY do begin
		    inc(lPixel);
		    lInBuff^[lPixel] := gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lZOffset+lYOffset+lXOffset];
		    lYOffset := lYOffset+ lX;
	    end; //for each Y
          end; //if flipped else
        end; //for each Z
	ImgForm.SetDimension8(lZ,lY, lInBuff, lUndoOnly);
	freemem(lInBuff);
end;

procedure WriteUndoVOI(lPanel: integer; lUndoOnly, lUpdateSliceNumber: boolean);
begin
	EnsureVOIOPen;
	case lPanel of
		3: WriteCorVOI(lUndoOnly, lUpdateSliceNumber);
		2: WriteSagVOI(lUndoOnly, lUpdateSliceNumber);
		else WriteAxialVOI(lUndoOnly, lUpdateSliceNumber);
	end;
	gBGImg.VOIchanged := true;
	if gBGImg.VOIUndoOrient = 4 then
		FreeUndoVol; //release 3D undo buffer when creating 2D buffer
	gBGImg.VOIUndoOrient := lPanel;
end;

procedure TImgForm.FormOpenFileMethod(const FileName : string);
var
   lFilename: string;
begin
    lFilename := Filename;
    OpenAndDisplayImg(lFilename,true);
end;

procedure TImgForm.Landmarks1Click(Sender: TObject);
begin
     //Graph4DForm.show;
 AnatForm.show;
end;

procedure TImgForm.InitImg(var lImage: TImage);
var
  lx,ly: integer;
  lTBuff: RGBQuadp;
begin
  lx := 121;
  ly := 8;
     getmem(lTBuff,lx*ly*4);
     Fillchar(lTBuff^,lx*ly*4,0); //set all to zero
     DrawBMP( lx, ly, lTBuff, lImage);
     freemem(lTBuff);
     lImage.Canvas.clear;
end;

procedure TImgForm.FormCreate(Sender: TObject);
var
   lInc: longint;
begin
  Application.ShowButtonGlyphs := sbgNever;
 KeyPreview := true;
 {$IFDEF LINUX} //Lazarus Linux 1.6 has odd behavior if image width divisible by 8, but after single image of different width all is well
 InitImg(PGImageAx);
 InitImg(PGImageCor);
  InitImg(PGImageSag);
 {$ENDIF}
 //PGImageCor.Canvas.clear;
 //  PGImageCor.Picture.Bitmap.Clear;
  {$IFDEF Darwin}
          LabelX.Top := LabelX.Top + 2;
          LabelY.Top := LabelY.Top + 2;
          LabelZ.Top := LabelZ.Top + 2;

       //InitOpenDocHandler;//allows files to be associated...
        {$IFNDEF LCLgtk} //for Carbon or Cocoa
        AppleMenu.Visible:= true;
        NewWindow1.Visible := true;
            Open1.ShortCut := ShortCut(Word('O'), [ssMeta]);
            SaveasNIfTI1.ShortCut := ShortCut(Word('S'), [ssMeta,ssAlt]);
            Saveaspicture1.ShortCut := ShortCut(Word('S'), [ssMeta]);
            Copy1.ShortCut := ShortCut(Word('C'), [ssMeta]);
            Paste1.ShortCut := ShortCut(Word('V'), [ssMeta]);
            Undo1.ShortCut := ShortCut(Word('Z'), [ssMeta]);
            OverlayOpen.ShortCut := ShortCut(Word('A'), [ssMeta]);
            Applyintensityfiltertovolume1.ShortCut := ShortCut(Word('F'), [ssMeta]);
            HistoMenu.ShortCut := ShortCut(Word('H'), [ssMeta]);
            ShowRender.ShortCut := ShortCut(Word('R'), [ssMeta]);
            ShowMultislice.ShortCut := ShortCut(Word('M'), [ssMeta]);
            N4DTraces1.ShortCut := ShortCut(Word('D'), [ssMeta]);
            Header1.ShortCut := ShortCut(Word('I'), [ssMeta]);
            YokeMenu.ShortCut := ShortCut(Word('Y'), [ssMeta]);
             // OnDropFiles := OnDropFiles;

        {$ENDIF}
 {$ENDIF}
{$IFDEF Darwin}
        //x Exit1.visible := false;//with OSX users quit from application menu
        //With Lazarus 1.9 and Cocoa this causes double  loading
        //Application.OnDropFiles := FormDropFiles;
 {$ENDIF}
     CreateFX8(gUndoImg);
     CreateFX8(gDrawImg);
           TriplePanel.OnMouseWheelDown:=  FormMouseWheelDown;
     TriplePanel.OnMouseWheelUp:=  FormMouseWheelUp;
     TriplePanel.OnMouseWheelDown:=  FormMouseWheelDown;
     TriplePanel.OnMouseWheelUp:=  FormMouseWheelUp;
     TriplePanel.OnMouseWheelDown:=  FormMouseWheelDown;
     TriplePanel.OnMouseWheelUp:=  FormMouseWheelUp;
     randomize;
     gnCPUThreads := GetLogicalCpuCount;
     gMouseDownX := -1;
     ImgForm.Caption := extractfilename(paramstr(0));
     ImgForm.DoubleBuffered := true;
     TriplePanel.DoubleBuffered := true;
     TriplePanel.DoubleBuffered := true;
     TriplePanel.DoubleBuffered := true;
	 for lInc := 0 to knMaxOverlay do begin
		FreeImgMemory(gMRIcroOverlay[lInc]);
		NIFTIhdr_ClearHdr(gMRIcroOverlay[lInc]);
		gMRIcroOverlay[lInc].ScrnBufferItems := 0;
		gMRIcroOverlay[lInc].ImgBufferItems := 0;
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
         gColorSchemeDir := extractfilepath(paramstr(0))+'Resources'+pathdelim+'lut';
	 if not direxists(gColorSchemeDir) then
            gColorSchemeDir := extractfilepath(paramstr(0))+'lut';
         {$IFDEF Darwin}
         if not fileexists(gColorSchemeDir) then
            gColorSchemeDir := AppDir + 'lut';
         //showmessage(gTemplateDir);
         {$ENDIF}

	 {$IFNDEF Unix} DragAcceptFiles(Handle, True); //engage drag and drop
         {$ENDIF}
	 UpdateColorSchemes;
        {$IFNDEF FPC}
	 LUTdrop.SetItemIndex(0);
	 Zoomdrop.SetItemIndex(0);
	 LayerDrop.SetItemIndex(0);
  {$ELSE}

	 LUTdrop.ItemIndex:=(0);
	 Zoomdrop.ItemIndex:=(0);
	 LayerDrop.ItemIndex:=(0);
         //x MagnifyMenuItem.visible := false;
         {$IFNDEF COMPILEYOKE}
         YokeMenu.visible := false;
         {$ENDIF}
{$ENDIF}
     gTemplateDir := extractfilepath(paramstr(0))+'Resources'+pathdelim+'templates';
	 if not direxists(gTemplateDir) then
            gTemplateDir := extractfilepath(paramstr(0))+'templates';
         {$IFDEF Darwin}
         if not fileexists(gTemplateDir) then
            gTemplateDir := AppDir + 'templates';
         //showmessage(gTemplateDir);
         {$ENDIF}
	 UpdateTemplates;
	 for lInc := 1 to knMRU do
		 gMRUstr[lInc] := '';
         if  ResetDefaults then
             //DrawMenu.Visible := ToolPanel.visible
         else
	    ReadIniFile;
         SetIniMenus;
         UpdateMRU;

	 OverlaySmoothMenuClick(nil);
	 LUTDrop.OnSelect(nil);
	 ZoomDrop.OnSelect(nil);
	 CreateShareMem;
	 if YokeMenu.checked then YokeTimer.enabled := true;
   //gBGIMg.SaveDefaultIni := true;
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

function XYZ2Vox(lX,lY,lZ: integer): integer;
begin
  if (gBGImg.Mirror) then //2019
      result := (gBGImg.ScrnDim[1]- lX + 1) + ((lY-1)*gBGImg.ScrnDim[1])+((lZ-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2])
  else
      result := lX + ((lY-1)*gBGImg.ScrnDim[1])+((lZ-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);

end;

function ImgIntensity(var lHdr: TMRIcroHdr; lX,lY,lZ: integer): single; overload;
var
	lPos: integer;
begin
   lPos := XYZ2Vox(lX,lY,lZ);
   //lPos := lX + ((lY-1)*gBGImg.ScrnDim[1])+((lZ-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
  result := ImgIntensity(lHdr,lPos);
end;

function TImgForm.ImgIntensityString(var lHdr: TMRIcroHdr; lVox: integer): string;  overload;
var
   lV: integer;
begin
  result := '';
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

function TImgForm.ImgIntensityString(var lHdr: TMRIcroHdr; lX,lY,lZ: integer): string;  overload;
var
   lVox: integer;
begin
  //lVox := lX + ((lY-1)*gBGImg.ScrnDim[1])+((lZ-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
  lVox := XYZ2Vox(lX,lY,lZ);
  result := ImgIntensityString(lHdr,lVox);
end;

procedure TImgForm.UpdateStatusLabel;
var
	lX,lY,lZ,lOverlay,lLen: integer;
	lXmm,lYmm,lZmm: single;
	lIntenStr : string;
begin

	lX := XviewEdit.value;
	lY := YviewEdit.value;
	lZ := ZviewEdit.value;
	ImgCoordToMM(lX,lY,lZ,lXmm,lYmm,lZmm);

 lIntenStr := '';
//StatusLabel.Caption := realtostr(lXmm,0)+'x'+realtostr(lYmm,0)+'x'+realtostr(lZmm,0);
//lIntenStr := realtostr(lXmm,0)+'x'+realtostr(lYmm,0)+'x'+realtostr(lZmm,0)+'= '+lIntenStr;;

//StatusLabel.Caption := lIntenStr;
//StatusLabel.Caption := realtostr(lXmm,0)+'x'+realtostr(lYmm,0)+'x'+realtostr(lZmm,0)+'= '+lIntenStr;
//crash!
 for lOverlay := kBGOverlayNum to (kVOIOverlayNum-1) do
		if gMRIcroOverlay[lOverlay].ImgBufferItems > 0 then
			lIntenStr := lIntenStr + ImgIntensityString(gMRIcroOverlay[lOverlay],lX,lY,lZ)+', ';
	lLen := length (lIntenstr);
	if lLen > 2 then
		lIntenStr[lLen-1] := ' ';
	//StatusLabel.Caption := realtostr(lXmm,0)+'x'+realtostr(lYmm,0)+'x'+realtostr(lZmm,0)+'= '+lIntenStr;
	Caption :=realtostr(lXmm,0)+'x'+realtostr(lYmm,0)+'x'+realtostr(lZmm,0)+'= '+lIntenStr;
	SetShareMem (lXmm,lYmm,lZmm);
end;

procedure TImgForm.XViewEditChange(Sender: TObject);
begin
  gBGImg.XViewCenter := XviewEdit.value;
	 gBGImg.YViewCenter := YviewEdit.value;
	 gBGImg.ZViewCenter := ZviewEdit.value;
	 RefreshImagesTimer.Enabled := true;
	 //UpdateStatusLabel;  //caused crash! - only with refreshimagestimes
end;

  {$IFNDEF FPC}
procedure TImgForm.FormClose(Sender: TObject; var Action: TCloseAction);
  {$ELSE}
procedure TImgForm.FormClose(Sender: TObject);
  {$ENDIF}
begin

 WriteIniFile;
 CloseImagesClick(nil);
 FreeFX8(gDrawImg);
 FreeFX8(gUndoImg);
end;

procedure TImgForm.MagnifyTimerTimer(Sender: TObject);
  {$IFDEF FPC}
  begin
//      MagnifyTimer.Enabled := false;
  end;
  {$ELSE}
  var
  Srect,Drect,PosForme,ImgForme:TRect;
  lZoomSlider,iWidth,iHeight,DmX,DmY:Integer;
  iTmpX,iTmpY:Real;
  C:TCanvas;
  hDesktop: Hwnd;
  Kursor:TPoint;
begin

 MagnifyTimer.Enabled := false;
 lZoomSlider := 2;
 If not IsIconic(Application.Handle) then begin
  hDesktop:= GetDesktopWindow;
  GetCursorPos(Kursor);
  ImgForme := Rect(ImgForm.Left+ImgForm.TriplePanel.Left,ImgForm.Top+ImgForm.TriplePanel.Top,ImgForm.Left+ImgForm.Width,ImgForm.Top+ImgForm.Height);
  PosForme:=Rect(MagnifyPanel.Left,MagnifyPanel.Top,MagnifyPanel.Left+MagnifyPanel.Width,MagnifyPanel.Top+MagnifyPanel.Height);
  if true then begin
  iWidth:=MagnifyImage.Width;
  iHeight:=MagnifyImage.Height;
  if iHeight < 6 then exit;
    Drect:=Rect(0,0,iWidth,iHeight);
    iTmpX:=iWidth / (lZoomSlider*4);//(Slider.Position * 4);
    iTmpY:=iHeight / (lZoomSlider*4);//(Slider.Position * 4);
    Srect:=Rect(Kursor.x,Kursor.y,Kursor.x,Kursor.y);
    InflateRect(Srect,Round(iTmpX),Round(iTmpY));
	If Srect.Left<0 then OffsetRect(Srect,-Srect.Left,0);
    If Srect.Top<0 then OffsetRect(Srect,0,-Srect.Top);
    If Srect.Right>Screen.Width then OffsetRect(Srect,-(Srect.Right-Screen.Width),0);
    If Srect.Bottom>Screen.Height then OffsetRect(Srect,0,-(Srect.Bottom-Screen.Height));
   C:=TCanvas.Create;
   try
     C.Handle:=GetDC(GetDesktopWindow);
      SetStretchBltMode(C.Handle,COLORONCOLOR);
      //SetStretchBltMode(C.Handle, STRETCH_DELETESCANS);
      //SetStretchBltMode(C.Handle,{BILINEAR}TransparencyEdit.value);
     MagnifyImage.Canvas.CopyRect(Drect,C,Srect);
    finally
      ReleaseDC(hDesktop, C.Handle);
      C.Free;
    end;
    If True then begin // show crosshair
        MagnifyImage.Canvas.Pen.Color := gBGIMg.XBarClr;
    	with MagnifyImage.Canvas do begin
        DmX:=lZoomSlider * 2 * (Kursor.X-Srect.Left);
        DmY:=lZoomSlider * 2 * (Kursor.Y-Srect.Top);
   		 MoveTo(1,DmY); // -
   		 LineTo(iWidth,DmY); // -
    		 MoveTo(DmX,1); // |
   		 LineTo(DmX,iHeight); // |
      end; // with MagnifyImage.Canvas
    end; // show crosshair
    	Application.ProcessMessages;
    end // Cursor not inside form
 end; // IsIconic
end;  //magnify image
  {$ENDIF}

procedure TImgForm.MagnifyPanelResize(Sender: TObject);
begin
(* MagnifyImage.Picture:=nil;
 if MagnifyPanel.Width < MagnifyPanel.Constraints.MinWidth then
     MagnifyPanel.Width := MagnifyPanel.Constraints.MinWidth;
     *)
end; //Proc MagnifyPanelResize

procedure SelectPanel (lPanelNumber: integer);
begin
gSelectedImageNum := lPanelNumber;
end; //Proc SelectPanel

procedure ShowFocusRect(lInRect: TRect);
var LImage: TImage;
begin
	lImage := SelectedImage;
   lImage.Canvas.DrawFocusRect(lInRect);
end; //proc ShowFocusRect

procedure XYscrn2Img (lImage: TIMage;lPanel,lXinRaw,lYinRaw: integer; var lXout,lYOut,lZOut: integer);
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
                      // ImgForm.LabelX.Caption := '2Sag';
                      if gBGImg.FlipSag then
	                 lXin := lImage.Width-lXinRaw;
		      lXOut := ImgForm.XViewEdit.value;
		      lYOut := round((lXin*100) / lZoom +lOffset);
		      lZOut := round((lYin*100) / lZoom +lOffset);
		   end;
		   3: begin
                      //ImgForm.LabelX.Caption := '3Cor';
				lXOut := round((lXin*100) / lZoom +lOffset);
				lYOut := ImgForm.YViewEdit.value;
				lZOut := round((lYin*100) / lZoom +lOffset);

		   end;
		   else begin

                        if gBGImg.FlipAx then
	                                 lYin := lYinRaw;
         		lXOut := round((lXin*100) / lZoom +lOffset);
			lYOut := round((lYin*100) / lZoom +lOffset);
			lZOut := ImgForm.ZViewEdit.value;
		   end; //else
	  end;//case lPanel
	  //ImgForm.Caption := inttostr(lXOut)+' '+inttostr(lYOut)+'  '+Inttostr(lZOut);
end; //proc XYscrn2Img

procedure AdjustContrastRectangle (lImage: TImage);
var
 lXpos,lYPos,lXOut,lYOut,lZOut,lPanel,lLayer: integer;
 lMinInten,lMaxInten,lVal: single;
begin
   lPanel := SelectedImageNum;
   lLayer := ImgForm.ActiveLayer;
   if gMRIcroOverlay[lLayer].UsesCustomPaletteRandomRainbow then exit;
   XYscrn2Img (lImage,lPanel,gSelectRect.Left,gSelectRect.Top, lXout,lYOut,lZOut);
   lMinInten := ImgIntensity(gMRIcroOverlay[lLayer],lXout,lYOut,lZOut);
   lMaxInten := lMinInten;
   for lYpos := gSelectRect.Top to gSelectRect.Bottom do begin
	   for lXpos := gSelectRect.Left to gSelectRect.Right do begin
		   XYscrn2Img (lImage,lPanel,lXpos,lYPos, lXout,lYOut,lZOut);
                  // lVox := lXout + ((lYout-1)*gBGImg.ScrnDim[1])+((lZout-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
                  // lVal := ImgIntensity(gMRIcroOverlay[lLayer],lVox);
		lVal:= ImgIntensity(gMRIcroOverlay[lLayer],lXout,lYOut,lZOut);
			if lVal < lMinInten then lMinInten := lVal;

			if lVal > lMaxInten then lMaxInten := lVal;
	   end; //for PGX each column
   end; //for PGY2 - each row
   //ImgForm.StatusLabel.caption := (RealToStr(lMinInten,4))+'..'+({x} RealToStr(lMaxInten,4))+'bexx'+ inttostr(lXout)+'x'+inttostr(lYOut)+'x'+inttostr(lZOut)+' '+inttostr(ActiveLayer);

   //  ImgForm.StatusLabel.caption := 'bexx'+ inttostr(gSelectRect.Top)+'..'+inttostr(gSelectRect.Bottom)+' -> '+inttostr(gSelectRect.Left)+'..'+inttostr(gSelectRect.Right);
  ImgForm.StatusLabel.caption := 'Intensity range '+(RealToStr(lMinInten,4))+'..'+({x} RealToStr(lMaxInten,4));
   if lMinInten = lMaxInten then exit; //no range
   ImgForm.MinWindowEdit.value  := lMinInten;
   ImgForm.MaxWindowEdit.value := lMaxInten;
   {$IFDEF FPC} ImgForm.MinContrastWindowEditChange(nil); {$ENDIF}
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

procedure DrawEllipse (lImage: TImage;lRect: TRect; lShift: TShiftState; lPanel: integer);
var
   i: integer;
begin
   ScaleBMP2Draw(gBGImg.VOIInvZoom, lRect.Left,lRect.Top,lPanel,Limage);
   ScaleBMP2Draw(gBGImg.VOIInvZoom, lRect.Right,lRect.Bottom,lPanel,lImage);
   if ssShift in lShift then
      i := 0
   else
       i := kVOI8bit;
   if (ssCtrl in lShift) then
      FillRectFX8(gDrawImg,lRect.Left,lRect.Top,lRect.Right,lRect.Bottom,i)
   else
	FillEllipseFX8(gDrawImg,lRect.Left,lRect.Top,lRect.Right,lRect.Bottom,i);
end; //DrawEllipse


procedure TImgForm.PGImageMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var lZoom,lPanel,lX, lY,lXout,lYOut,lZOut,lBasePenThick,lX2, lY2: integer;
	lImage: TImage;
begin
   gSelectOrigin.X := -1;

   lX := X; lY := Y;
   lImage := Sender as TImage;
   if lImage.Name = PGImageCor.Name {'PGImageCor'} then lPanel := kCoroView0
   else if lImage.Name = PGImageSag.Name {'PGImageSag'} then begin
        lPanel := kSagView0;
        //LabelX.Caption:=inttostr(random(888));

   end else lPanel := kAxView0;


   //lImage.Canvas.Pen.Width := 1;
   //   lImage.Canvas.Pen.Color :=gBGImg.VOIClr;
   SelectPanel(lPanel);
   gBGImg.VOIInvZoom := ComputeInvZoomShl10(lPanel,lImage);
   if  DrawToolSelected then begin //paint tool
	   WriteUndoVOI(lPanel,false, true);
	   if (ssShift in Shift) then begin //erase
			lImage.Canvas.Brush.Color:=clBlack;
			lImage.Canvas.Pen.Color := clBlack;
	   end else begin
			lImage.Canvas.Brush.Color:=gBGImg.VOIClr;
			lImage.Canvas.Pen.Color := gBGImg.VOIClr;
	   end;
	   if gBGImg.ThinPen then
			lBasePenThick := 1
	   else begin //adjust pen thickness for zoom level
		 if gBGImg.ZoomPct < 100 then begin
			lZoom := ComputeZoomPct(lPanel,lImage);
			if lZoom = 100 then
				lBasePenThick := 1
			else
			  lBasePenThick :=  round((ComputeZoomPct(lPanel,lImage)+50) / 100);
		 end else if gBGImg.ZoomPct > 100 then
			lBasePenThick := gBGImg.ZoomPct div 100
		 else
			lBasePenThick := 1;
	   end; //if not thinpen
   		if (ssCtrl in Shift) then begin
			lImage.Canvas.Pen.Width := lBasePenThick*3;
			gDrawImg.PenThick := 3;
		end else begin
			lImage.Canvas.Pen.Width := lBasePenThick;
			gDrawImg.PenThick := 1;
		end;
   end; //paint tool selected
   //lImage.Canvas.Pen.Width := 1;
   if  (FillBtn.Down) and (ssCtrl in Shift) then begin  //3D fill
			XYscrn2Img (lImage,lPanel,lX,lY, lXout,lYOut,lZOut);
				XViewEdit.value := lXOut;
				YViewEdit.value := lYOut;
				ZViewEdit.value := lZOut;
			if (ssShift in Shift) then //erase
				ROICluster(gBGImg.ScrnDim[1], gBGImg.ScrnDim[2], gBGImg.ScrnDim[3],XViewEdit.value,YViewEdit.value,ZViewEdit.value,true)
			else //draw
				ROICluster(gBGImg.ScrnDim[1], gBGImg.ScrnDim[2], gBGImg.ScrnDim[3],XViewEdit.value,YViewEdit.value,ZViewEdit.value,false);
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
		//next no paint tools selected - show position where click occurred
		XYscrn2Img (lImage,lPanel,lX,lY, lXout,lYOut,lZOut);
				XViewEdit.value := lXOut;
				YViewEdit.value := lYOut;
				ZViewEdit.value := lZOut;
				//showmessage(floattostr(lXOut)+'x'+floattostr(lYOut)+'x'+floattostr(lZOut));
				//ImgCoordToMM(lXOut,lYOut,lZOut,lXmm,lYmm,lZmm);
				//showmessage(floattostr(lXmm)+'x'+floattostr(lYmm)+'x'+floattostr(lZmm));

				//showmessage(floattostr(gBGImg.ScrnOri[1])+'x'+floattostr(gBGImg.ScrnOri[2])+'x'+floattostr(gBGImg.ScrnOri[3]));
				//MMToImgCoord(lXOut,lYOut,lZOut,lXmm,lYmm,lZmm);
				//showmessage(floattostr(lXOut)+'x'+floattostr(lYOut)+'x'+floattostr(lZOut));

				//SetShareMem (lXmm,lYmm,lZmm);

        {$IFDEF FPC}
  XViewEditChange(nil);
  {$ENDIF}
   	exit;
   end;
   ScaleScrn2BMP(lX,lY, lImage);
   lImage.Canvas.MoveTo(lX,lY);

   lX2 := X; lY2 := Y;
   ScaleBMP2Draw(gBGImg.VOIInvZoom, lX2,lY2,lPanel,lImage);
   if  (FillBtn.Down) or(ssRight in Shift) then begin

           if (ssShift in Shift) then
              FloodFillFX8 (gDrawImg, lX2,lY2,kVOI8bit,0,true)
   	      //FloodFillX(DrawImg2,lX2-1,lY2-1,gBGImg.VOIClr, fsSurface)
	   else
               FloodFillFX8 (gDrawImg, lX2,lY2,kVOI8bit,kVOI8bit,false);
   	//FloodFillX(DrawImg2,lX2-1,lY2-1,gBGImg.VOIClr, fsBorder);

	   exit;
   end;
   //ImgForm.caption := inttostr(lX2);
   MoveToFX8(gDrawImg,lX2,lY2);
   if lImage.Canvas.Pen.Color = clBlack then //ensure single pixel is drawn if user clicks without dragging
      LineToFX8(gDrawImg,lX2,lY2,0)
   else
       LineToFX8(gDrawImg,lX2,lY2,kVOI8bit);
   gMouseDownX := lX;
   gMouseDownY := lY;

end; //PGImageMouseDown

var
   gDragX,gDragY,gDragZ : integer;
   //gDragRefresh : boolean = false; //only redraw one snapshot at a time

procedure TImgForm.PGImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var lX, lY,lPanel,lXOut,lYOut,lZOut: integer;
	lImage: TImage;
begin
  lImage := Sender as TImage;
	 lX := X; lY := Y;
	 ScaleScrn2BMP(lX,lY,lImage);
	 //if MagnifyImage.Height > 10 then
	//	MagnifyTimer.Enabled := true;//MagnifyBtn.Down;
	 //StatusLabel.Caption := inttostr(lX)+','+inttostr(lY);

	 if {(ssShift in Shift) and} (gSelectOrigin.X > 0) then begin
		ShowFocusRect(gSelectRect);
		gSelectRect.Left := gSelectOrigin.X;
		gSelectRect.Top := gSelectOrigin.Y;
		gSelectRect.Right := lX;
		gSelectRect.Bottom := lY;
		sortLTRB(gSelectRect.Left,gSelectRect.Top,gSelectRect.Right,gSelectRect.Bottom);
		ShowFocusRect(gSelectRect);
		exit;
	 end;
                     if lImage.Name = PGImageCor.Name then lPanel := kCoroView0
            else if lImage.Name = PGImageSag.Name then lPanel := kSagView0
            else lPanel := kAxView0;
         if (not DrawToolSelected) and ((ssLeft in Shift)) then begin
            //RefreshImagesTimer.Enabled := false;
            //gDragRefresh := true;

            XYscrn2Img (lImage,lPanel,lX,lY, lXout,lYOut,lZOut);
            if (lXout = gDragX) and (lYout = gDragY) and (lZOut = gDragZ) then
               exit;//no change
            XViewEdit.value := lXOut;
	    YViewEdit.value := lYOut;
	    ZViewEdit.value := lZOut;

            {$IFDEF FPC}XViewEditChange(nil);{$ENDIF}   //can generate crash!
            //gDragRefresh := false;
            exit;
         end;

	 if (not (ssLeft in Shift)) or (gMouseDownX < 0) then exit;
	 if PenBtn.Down or ClosedPenBtn.Down then begin
		lImage.Canvas.LineTo(lX,lY);
		lX := X; lY := Y;
		ScaleBMP2Draw(gBGImg.VOIInvZoom, lX,lY,lPanel,lImage);
		//DrawImg2.Canvas.LineTo(lX,lY);
                if lImage.Canvas.Pen.Color = clBlack then
                    LineToFX8(gDrawImg,lX,lY,0)//zzzxx
                else
                    LineToFX8(gDrawImg,lX,lY,kVOI8bit);//zzzxx
	 end;
end; //PGImageMouseMove

(*procedure Scrn2VOI (var lImage: TImage; lXvoi,lYvoi: integer; var lVOIBuffer: ByteP);

const
 kSh = 10; //bits to shift
var
	lInc,lXpos,lYPos,lVOISliceSz: integer;
	srcBmp : TBitmap;
begin
  srcBmp := lImage.Picture.Bitmap;
  lVOISliceSz := lXvoi*lYvoi;
  GetMem (lVOIBuffer , lVOISliceSz);
  lInc := 0;
  for lYpos:=(lYvoi-1) downto 0 do begin
      for lXpos:=0 to lXvoi-1 do begin
        inc(lInc);                   //zax
        if srcBmp.Canvas.Pixels[lXpos,lYPos] = clBlack then
           lVOIBuffer^[lInc] := 0
        else
            lVOIBuffer^[lInc] := 100;
      end;
  end;
end; //Scrn2VOI *)


procedure ReadCorVOI (var lImage: TFX8; lSlice: integer);
var lX,lY,lZ,lYOffset,lZOffset,lXYSliceSz,lPixel,lZPos,lXPos: integer;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
	lYOffset := (lX) * (round(lSlice)-1);
	lXYSliceSz := (lX*lY);

	lPixel := 0;
	for lZPos := 1 to lZ do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
	  for lXPos := 1 to lX do begin
		  inc(lPixel);
		gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lZOffset+lYOffset+lXPos] :=lImage.Img^[lPixel];
	  end; //for each Y
	end; //for each Z
end;

procedure ReadSagVOI (var lImage: TFX8;lSlice: integer);
var lX,lY,lZ,lXOffset,lYOffset,lZOffset,lXYSliceSz,lPixel,lZPos,lYPos: integer;
begin
	lX := gBGImg.ScrnDim[1];
	lY := gBGImg.ScrnDim[2];
	lZ := gBGImg.ScrnDim[3];
	lXYSliceSz := lX*lY;
	lXOffset := round(lSlice);

	lPixel := 0;
	for lZPos := 1 to lZ do begin
	  lZOffset := (lZPos-1) * lXYSliceSz;
          if gBGImg.FlipSag then begin
	    lYOffset := (lY - 1) * lX;
	    for lYPos := 1 to lY do begin
	        inc(lPixel);
		gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lZOffset+lYOffset+lXOffset] := lImage.Img^[lPixel];
		lYOffset := lYOffset - lX;
	    end; //for each Y
          end else begin //if flipped else
            lYOffset := 0;
            for lYPos := 1 to lY do begin
      	      inc(lPixel);
      	      gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lZOffset+lYOffset+lXOffset] := lImage.Img^[lPixel];
      	      lYOffset := lYOffset+ lX;
            end; //for each Y
          end; //if flipped else
        end; //for each Z
	//freemem(lInBuff);
end;

procedure ReadAxialVOI (var lImage: TFX8;lSlice: integer);
var
  lX,lY,lSliceOffset,lSliceSz: integer;
  //lSwapBuffer: ByteP;
begin
  lX := gBGImg.ScrnDim[1];
  lY := gBGImg.ScrnDim[2];
  lSliceSz := lX*lY;
  lSliceOffset := (lSlice-1)*lX*lY;

  //Move(src,dst,count);
  //ping-pong buffers: copy current screen to swap, copy undo buffer to screen, copy swap to undo buffer
  //getmem(lSwapBuffer,lSliceSz);
  //Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lSliceOffset+1], lSwapBuffer^[1], lSliceSz);
  Move(lImage.Img^[1],gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lSliceOffset+1], lSliceSz);
  //Move(lSwapBuffer^[1], lImage.Img^[1], lSliceSz);
  //freemem(lSwapBuffer);

  //WriteAxialVOI (lUndoOnly, lUpdateSliceNumber: boolean);
  //for lX := 1 to lSliceSz do
  //    gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lSliceOffset+lX] := lImage.Img^[lX];

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
		3: ReadCorVOI(gDrawImg,ImgForm.YViewEdit.Value);
		2: ReadSagVOI(gDrawImg,ImgForm.XViewEdit.Value);
		1: ReadAxialVOI(gDrawImg,ImgForm.ZViewEdit.Value);
	end;
	ImgForm.RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.PGImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var lX, lY,lPanel: integer;
lImage: TImage;
begin
  lPanel := SelectedImageNum;
        lImage := Sender as TImage;
	 lX := X; lY := Y;
	 ScaleScrn2BMP(lX,lY,lImage);
        if (gSelectOrigin.X > 0)  then begin
	         sortLTRB(gSelectRect.Left,gSelectRect.Top,gSelectRect.Right,gSelectRect.Bottom);
	         ShowFocusRect(gSelectRect);
	         gSelectOrigin.X := -1;
	         if (EllipseBtn.Down) then
		        DrawEllipse(Limage,gSelectRect,Shift,lPanel)
	         else begin
		        AdjustContrastRectangle(lImage);
		        exit;
	         end;
        end;

	 if  ((PenBtn.Down) or (ClosedPenBtn.Down)) and (gMouseDownX > 0) then begin
		ScaleBMP2Draw(gBGImg.VOIInvZoom, gMouseDownX,gMouseDownY,lPanel,lImage);
		//next: draw single pxiel if user clicks on image without moving the mouse
		//DrawImg2.Canvas.Pixels[gMouseDownX,gMouseDownY] := DrawImg2.Canvas.Pen.Color;
		if  (ClosedPenBtn.Down) then begin
                    if lImage.Canvas.Pen.Color = clBlack then
			LineToFX8(gDrawImg,gMouseDownX,gMouseDownY,0)
                    else
			LineToFX8(gDrawImg,gMouseDownX,gMouseDownY,kVOI8Bit);
                end;
	 end;

	 gMouseDownX := -1; //disable draws
	 //if DrawToolSelected then
         if DrawToolSelected  and (not (ssAlt in Shift)) then
		ReadScrnVOI (lImage);
end; //PGImageMouseUp


procedure TImgForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  Case SelectedImageNum of
	  3: DecViewEdit(YViewEdit);
      2: DecViewEdit(XViewEdit);
      else DecViewEdit(ZViewEdit);
  end;
end;

procedure TImgForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
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
                   TextReportHisto(gMRIcroOverlay[lLayer]);
		exit;
	 end;
	 lLTRB := 1;
     if  (ssRight in Shift) then
	   lLTRB := lLTRB + 1;
     if (ssCtrl in Shift) then
	   lLTRB := lLTRB + 2;
	 lImage := SelectedImage;
                 //Caption := inttostr(random(888));
	 intenBar(lImage,gMRIcroOverlay[ActiveLayer],lLTRB,0,0);
end;





procedure TImgForm.XBarBtnClick(Sender: TObject);
begin
  gBGImg.XBarVisible := XBarBtn.Down;
  CrossHair1.Checked := XBarBtn.Down;
  RefreshImagesTimer.Enabled := true;
end;

procedure RepositionOrigin;
begin
     gBGImg.ScrnOri[1] := ImgForm.XviewEdit.value;
     gBGImg.ScrnOri[2] := ImgForm.YviewEdit.value;
     gBGImg.ScrnOri[3] := ImgForm.ZviewEdit.value;
end;

procedure TImgForm.XBarBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
label 555;
begin
	 if not (ssRight in shift) then exit;
         if (ssShift in Shift) then begin
             RepositionOrigin;
	     goto 555;
         end;
         if (ssAlt in Shift) and (ssCtrl in Shift) then begin
		inc(gBGImg.FontSize,2);
		if gBGImg.FontSize > 24 then
			gBGImg.FontSize := 8;
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

procedure TImgForm.ImgPanelClick(Sender: TObject);
begin
	 SelectPanel((Sender as TScrollBox).tag);
end;

procedure TImgForm.MagnifyMenuItemClick(Sender: TObject);
begin
  (*if MagnifyPanel.Height < 20 then //Height constrained by Y
	 MagnifyPanel.Height := 128
  else
	  MagnifyPanel.Height := MagnifyPanel.Constraints.MinHeight;  *)
end;

procedure TImgForm.CloseImagesClick(Sender: TObject);
var
	lC: integer;
begin
	CloseVOIClick(nil);
	FreeUndoVol;
	for lC := 0 to knMaxOverlay do //background, all overlays, VOI
		FreeImgMemory(gMRIcroOverlay[lC]);
	gBGImg.VOIUndoSlice := 0;

	//next- set layers menu
	LayerDrop.Items.Clear;
	LayerDrop.Items.Add('Background');
   {$IFNDEF FPC}
    LayerDrop.SetItemIndex(0);
  {$ELSE}
    LayerDrop.ItemIndex :=(0);
  {$ENDIF}
	LayerDropSelect(nil);
end;

procedure TImgForm.OverlayOpenCore (var lFilename: string; lOverlayNum: integer);
begin
     if not OpenAndDisplayHdr(lFilename,gMRIcroOverlay[lOverlayNum]) then exit;
     //if not OpenImg(gBGImg,gMRIcroOverlay[lOverlayNum],false,false,false) then exit;
         //if (ssShift in KeyDataToShiftState(vk_Shift)) then begin
         //    if not OpenImg(gBGImg,gMRIcroOverlay[lOverlayNum],false,false,false,not gBGImg.ResliceOnLoad,false) then exit;
         //end else
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

procedure TImgForm.BrainMask1Click(Sender: TObject);
var
   lInc: integer;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image for rescaling.');
		exit;
	end;
        //lImgSamples := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
        for lInc := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do
            if gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lInc] <> 0 then
               gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lInc] := 1;
        SaveAsVOIorNIFTI(gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems,1,1,true,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName);
end;

procedure TImgForm.ControlPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

(*procedure DescribeVOIonLabelsz (lOverlayNum: integer; lShowFilename: boolean);
var
   lLocalMax,lLocalSum : HistoDoubleRA;
   l16Buf : SmallIntP;
   l32Buf : SingleP;
   l8Buf: byteP;
   lInten: double;
   lXmm,lYmm,lZmm: single;
   lHisto,lRegionVol,lLocalMaxPos: HistoRA;
   lInc,lRegion: Integer;
   lLabelStr: string;
   lVOI: boolean;
   lLabelStr20 : Array[0..kHistoBins] of kstr20;
begin
     lInten := 0;//just to hide compiler hint...
     if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP = 2) and ('ratlas.nii.gz' = (extractfilename( gMRIcroOverlay[kBGOverlayNum].HdrFileName))) then begin
        // specific for PCDescribeVOIonLabelsRAT(lOverlayNum,lShowFilename);
        exit;
     end;
     if (gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems) or (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP <> 1) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 2) then
        exit;
     TextForm.MemoT.Lines.add(' Custom Region Analysis');
     TextForm.MemoT.Lines.add(' For Speculative Brodmann Map: 0=not cortical and 48=no Brodmann label');
     lVOI := IsVOIROIExt(gMRIcroOverlay[lOverlayNum].HdrFileName);
     if (not lVOI) and (lOverlayNum = kVOIOverlayNum) then
        lVOI := true;
     //next describe format
     if lShowfilename then
        lLabelStr := ' Filename,'
     else
         lLabelStr := ' ';
     if lVOI then //intensity min/max position are not important
        TextForm.MemoT.Lines.add(lLabelStr+'Area'+kTextSep+'N>0'+kTextSep+'%N>0')
     else
         TextForm.MemoT.Lines.add(lLabelStr+'Area'+kTextSep+'N>0'+kTextSep+'%N>0'+kTextSep+'Sum>0'+kTextSep+'Mean>0'+kTextSep+'Max'+kTextSep+'MaxX'+kTextSep+'MaxY'+kTextSep+'MaxZ');
   //next initialize
   if lShowFilename then
      lLabelStr := gMRIcroOverlay[lOverlayNum].HdrFileName+kTextSep
   else
       lLabelStr := '';
     for lInc := 0 to kHistoBins do begin
         lHisto[lInc] := 0;
         lLocalMax[lInc] := 0;
         lLocalSum[lInc] := 0;
         lRegionVol[lInc] := 0;
         if (gMRIcroOverlay[kBGOverlayNum].UsesCustomPalette) then
            lLabelStr20[lInc] := gBGImg.LabelStr20[lInc]
         else
             lLabelStr20[lInc] := inttostr(lInc);
     end;
     for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do
         if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then
            inc(lHisto[gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lInc]]);
     //local max start
     l32Buf := SingleP(gMRIcroOverlay[lOverlayNum].ImgBuffer );
     l16Buf := SmallIntP(gMRIcroOverlay[lOverlayNum].ImgBuffer );
     //NEXT if..else July07 - ROIs only use screen buffer, not imgbuffer...
     if gMRIcroOverlay[lOverlayNum].ScrnBufferItems = gMRIcroOverlay[lOverlayNum].ImgBufferItems then
        l8Buf := gMRIcroOverlay[lOverlayNum].ImgBuffer
     else
         l8Buf := gMRIcroOverlay[lOverlayNum].ScrnBuffer;
     for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
         if (gMRIcroOverlay[lOverlayNum].ImgBufferBPP  = 4) then
           lInten := l32Buf^[lInc]
         else if (gMRIcroOverlay[lOverlayNum].ImgBufferBPP  = 2) then
              lInten := l16Buf^[lInc]
         else if gMRIcroOverlay[lOverlayNum].ImgBufferBPP  = 1 then
	      lInten := l8Buf^[lInc];//July07
         lRegion := gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lInc];
         if lInten > 0 then
            lLocalSum[lRegion] := lLocalSum[lRegion]+lInten;
         if  lInten > lLocalMax[lRegion] then begin
             lLocalMax[lRegion] := lInten;//intensity
             lLocalMaxPos[lRegion] := lInc;//location
         end;
         inc(lRegionVol[lRegion]);
     end;
     for lInc := 0 to kHistoBins do  begin
         if (not lVOI) and (lLocalMax[lInc] > 0) then begin
            lLocalMax[lInc] := Raw2ScaledIntensity (gMRIcroOverlay[lOverlayNum],lLocalMax[lInc]);
            lLocalSum[lInc] := Raw2ScaledIntensity (gMRIcroOverlay[lOverlayNum],lLocalSum[lInc]);
            ImgPosToMM(lLocalMaxPos[lInc], lXmm,lYmm,lZmm);
            TextForm.MemoT.Lines.Add(lLabelStr+ lLabelStr20[lInc] + kTextSep + inttostr(lHisto[lInc])+kTextSep+floattostr( lHisto[lInc]/lRegionVol[lInc])
               +kTextSep+floattostr( lLocalSum[lInc])+kTextSep+floattostr( lLocalSum[lInc]/lRegionVol[lInc]) //Sum>0, mean>0
               +kTextSep + floattostr(lLocalMax[lInc])+kTextSep+floattostr(lXmm)+kTextSep+floattostr(lYmm)+kTextSep+floattostr(lZmm) );
         end else if (lHisto[lInc] > 0) {necessarily also and (lRegionVol[lInc] > 0)} then
	 		TextForm.MemoT.Lines.Add(lLabelStr+ lLabelStr20[lInc] +kTextSep+ inttostr(lHisto[lInc])+kTextSep+floattostr( lHisto[lInc]/lRegionVol[lInc])) ;
     end; //for each row
end;     *)
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

procedure ShowDescriptive (lOverlayNum: integer; lShowFilename: boolean);
var
	lROIVol: array [1..3] of integer;
	lInc: integer;
	lCenterOfMass,lROISum,lROISumSqr,lROImin,lROImax:array [1..3] of double;
	lCC,lVal,lSD,lROImean: double;
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
		if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then begin
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
		lStr := lStr+' nvox(cc)=min/mean/max=SD: '+inttostr(round(lROIVol[lInc]))+kTextSep+RealToStr(lCC,2)+kTextSep+'='+kTextSep+RealToStr(lROIMin[lInc],4)+kTextSep+realToStr(lROIMean,4)+kTextSep+realToStr(lROIMax[lInc],4)+kTextSep+'='+kTextSep+realtostr(lSD,4);
		TextForm.MemoT.Lines.Add(lLabelStr+ lStr);
	end;
	//June07 if (gMRIcroOverlay[kBGOverlayNum].UsesCustomPalette) or (lShowFilename) then
		DescribeVOIonLabels(lOverlayNum,lShowfilename);
	TextForm.MemoT.Lines.Add('');
        ImgForm.SaveDialog1.Filename := ExtractFileDirWithPathDelim(gMRIcroOverlay[lOverlayNum].HdrFileName)+'desc.csv';
end;


procedure TImgForm.BatchROImean1Click(Sender: TObject);
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
	if not OpenDialogExecute(kImgFilter,'Select images you wish to analyze',true) then exit;
	lNumberofFiles:= OpenHdrDlg.Files.Count;
        if  lNumberofFiles < 1 then
		exit;
        TextForm.MemoT.Lines.Clear;
        for lInc:= 1 to lNumberofFiles do begin
		lFilename := OpenHdrDlg.Files[lInc-1];
	        OverlayOpenCore ( lFilename, 2);
                ShowDescriptive(2,true);
 	        //LayerDrop.SetItemIndex(LayerDrop.Items.Count-1);
	        //LayerDropSelect(nil);
        end;
        FreeImgMemory(gMRIcroOverlay[2]);
        UpdateLayerMenu;
        //SaveDialog1.Filename := ExtractFileDirWithPathDelim(HdrForm.OpenHdrDlg.Files[0])+'desc.csv';
        TextForm.Show;
end;

procedure TImgForm.Batchprobmaps1Click(Sender: TObject);
begin
  BatchVOI;
end;

procedure TImgForm.Batchclusterprobmaps1Batchclusterprobmaps1ClickClick(
  Sender: TObject);
begin
       BatchCluster;
end;

procedure TImgForm.GenerateSPM5maskslesions1Click(Sender: TObject);
begin
     VOISmoothForm.SmoothVOI_SPM5masks;
end;


procedure TImgForm.OverlayOpenClick(Sender: TObject);
var
	lFilename: string;
	lInc: integer;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image (''File''/''Open'') before adding an overlay.');
		exit;
	end;
	if not OpenDialogExecute(kImgFilter,'Select overlay image[s]',true) then exit;
  if OpenHdrDlg.Files.Count < 1 then
    exit;
  for lInc := 1 to OpenHdrDlg.Files.Count do begin //vcx
    lFilename := OpenHdrDlg.Files[lInc-1];
    LoadOverlayIncludingRGB(lFilename);
    {$IFNDEF FPC}
 	LayerDrop.SetItemIndex(LayerDrop.Items.Count-1);
    {$ELSE}
        if LayerDrop.Items.Count < 1 then
         LayerDrop.ItemIndex := 0
        else
 	    LayerDrop.ItemIndex :=(LayerDrop.Items.Count-1);
    {$ENDIF}
  end;
  LayerDropSelect(nil);
end; //OverlayOpenClick()

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
  {$IFNDEF FPC}
  	 LUTdrop.SetItemIndex(gMRIcroOverlay[lLayer].LUTindex);
  {$ELSE}
  	 LUTdrop.ItemIndex :=(gMRIcroOverlay[lLayer].LUTindex);
  {$ENDIF}
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
           inc(lLayer);
           {$IFDEF LCLCocoa} //http://stackoverflow.com/questions/2320059/how-to-add-multiple-menu-items-with-the-same-title-to-nspopupbuttonnsmenu
                     lStrings.Add(ParseFileName(ExtractFileName(gMRIcroOverlay[lPos].HdrFileName))+':'+inttostr(lLayer));
             {$ELSE}
		   lStrings.Add(ParseFileName(ExtractFileName(gMRIcroOverlay[lPos].HdrFileName)));
             {$ENDIF}
                   //lStrings.Add(inttostr(lLayer));  //qball

		   LUTdropLoad(lLayer);
	  end;
        //Clipboard.AsText:= lStrings.Text;
	LayerDrop.Items := lStrings;
        //Clipboard.AsText:= LayerDrop.Items.Text;
   {$IFNDEF FPC}
	if LayerDrop.ItemIndex >= LayerDrop.Items.Count then
		LayerDrop.SetItemIndex(LayerDrop.Items.Count-1);
  {$ELSE}
	if LayerDrop.ItemIndex >= LayerDrop.Items.Count then
		LayerDrop.ItemIndex :=(LayerDrop.Items.Count-1);
    {$ENDIF}

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
         (*if gMRIcroOverlay[lLayer].NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL then begin
            createLutLabel (gMRIcroOverlay[lLayer], 1.0);
            //RefreshImagesTimer.Enabled := true;
             exit;
         end;
         if gMRIcroOverlay[lLayer].UsesCustomPaletteRandomRainbow then
            exit;   *)
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
  //ImgForm.PGImageSag.Refresh;
  //caption := inttostr(ImgForm.PGImageSag.left) +'x'+inttostr(ImgForm.PGImageSag.Top) ;
  //exit;
	 lLayer := ActiveLayer;
	MinWindowEdit.Value := raw2ScaledIntensity(gMRIcroOverlay[lLayer], gMRIcroOverlay[lLayer].AutoBalMinUnscaled);
	MaxWindowEdit.Value := raw2ScaledIntensity(gMRIcroOverlay[lLayer],gMRIcroOverlay[lLayer].AutoBalMaxUnscaled);{}

	gMRIcroOverlay[lLayer].WindowScaledMin := MinWindowEdit.Value;
        gMRIcroOverlay[lLayer].WindowScaledMax := MaxWindowEdit.Value;
	RescaleImgIntensity(gBGImg,gMRIcroOverlay[lLayer],lLayer);

	RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.MinContrastWindowEditChange(Sender: TObject);
var
	lLayer: integer;
begin
	lLayer := ActiveLayer;
        if MinWindowEdit.ValueEmpty then exit;
        //if gMRIcroOverlay[lLayer].WindowScaledMin = MinWindowEdit.Value then exit;
	gMRIcroOverlay[lLayer].WindowScaledMin := MinWindowEdit.Value;
        gMRIcroOverlay[lLayer].WindowScaledMax := MaxWindowEdit.Value;
	RescaleImagesTimer.Enabled := true;
end;

procedure TImgForm.MaxContrastWindowEditChange(Sender: TObject);
var
	lLayer: integer;
begin
	 lLayer := ActiveLayer;
         if MaxWindowEdit.ValueEmpty then exit;
	 if gMRIcroOverlay[lLayer].WindowScaledMax = MaxWindowEdit.Value then exit;
 	gMRIcroOverlay[lLayer].WindowScaledMin := MinWindowEdit.Value;
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
		RescaleImgIntensity(gBGImg,gMRIcroOverlay[lC],lC);
	RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.ShowRenderClick(Sender: TObject);
begin
	RenderForm.Show;
 //RenderForm.BringToFront;
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
	 //kMax20bit = (16*256*256)-1;
	// k20v16bit = kMax20bit - kMax16bit;
	//kMaxRuns = 10000;
	//kMaxFile =  65536;
	 //k16v12bit = kMax16bit - kMax12bit;
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
  if lBigFormat <> odd((lROIformatRA^[1] and kMax16bit) shr 15) then
	Showmessage('Warning: this ROI does not appear to be designed for the currently loaded background image.');
  lFilePos := 1;
if lBigFormat then begin //20-byte offset, 12-byte runlength
  while lFilePos < lFile32bitItems do begin
		lRunsOnSlice := (lROIformatRA^[lFilePos] shr 17) - 1; //shr 17: shift 16 bits, then div 2 (words instead of longints). Subtract 1 as the we have read slice number/ number of runs
		lZ := (lROIformatRA^[lFilePos]  and kMax15bit);
		inc(lFilePos);
		lOutputSliceOffset := (lZ-1) * lSliceSz;
		for lRun := 1 to lRunsOnSlice do begin
			if (lFilePos <= lFileSz) then begin
				lRunLength := (lROIformatRA^[lFilePos] shr 16) and kMax12bit;
				lRunOffset := (lROIformatRA^[lFilePos]  and kMax16bit)+ ((lROIformatRA^[lFilePos] shr 28) shl 16);
				if (lOutputSliceOffset+lRunLength+lRunOffset-1)> gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems then
					//showmessage('Overrun on slice '+inttostr(lZ))
				else for lRunPos := lRunOffset to (lRunLength+lRunOffset-1) do
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lRunPos+lOutputSliceOffset] := kVOI8bit;
			end;
			inc(lFilePos);
		end;//for all runs
  end; //while lPos < lFSz
end else begin //not big format format - 16-byte offset, 16-byte length
  while lFilePos < lFile32bitItems do begin
		//lRunsOnSlice := (lROIformatRA[lFilePos] shr 16) and kMax16bit;
		lRunsOnSlice := (lROIformatRA^[lFilePos] shr 17) - 1; //shr 17: shift 16 bits, then div 2 (words instead of longints). Subtract 1 as the we have read slice number/ number of runs
		lZ := (lROIformatRA^[lFilePos]  and kMax15bit);
		inc(lFilePos);
		lOutputSliceOffset := (lZ-1) * lSliceSz;
		//showmessage(inttostr(lZ)+'  '+inttostr(lRunsOnSlice)+'  '+inttostr(lFilePos)+'  '+inttostr(lFileSz));
		for lRun := 1 to lRunsOnSlice do begin
			if (lFilePos <= lFileSz) then begin
				lRunLength := (lROIformatRA^[lFilePos] shr 16) and kMax16bit;
				lRunOffset := (lROIformatRA^[lFilePos]  and kMax16bit);
				{if (lRunLength+lRunOffset-1)> gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems then
					showmessage('Overrun on slice '+inttostr(lZ))
				else} for lRunPos := lRunOffset to (lRunLength+lRunOffset-1) do
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lRunPos+lOutputSliceOffset] := kVOI8bit;
			end;
			inc(lFilePos);
		end;//for all runs
  end; //while lPos < lFSz
end; //if bigformat ... else little format
  freemem(lROIformatRA);
  lRun := maxint;
  LoadMonochromeLUT(lRun,gBGImg,gMRIcroOverlay[kVOIOverlayNum]);
end;

procedure TImgForm.OpenVOICore(var lFilename : string);
var
	lExt: string;
        isOverlaySmooth: boolean;
begin
	 if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0 then
	ImgForm.CloseVOIClick(nil);
	lExt := UpCaseExt(lFileName);
	gBGImg.VOIchanged := false;
	if (lExt='.ROI') then begin
		Showmessage('Warning: MRIcro ROI format does not save image dimensions. The background image must be in the same dimensions as the ROI.');
		OpenMRIcroROI (lFileName);
		ImgForm.RefreshImagesTimer.Enabled := true;
		exit;
	end;
        if not OpenAndDisplayHdr(lFilename,gMRIcroOverlay[kVOIOverlayNum]) then exit;
        isOverlaySmooth := gBGImg.OverlaySmooth;
        gBGImg.OverlaySmooth := false;
	if not OpenImg(gBGImg,gMRIcroOverlay[kVOIOverlayNum],false,true,false,gBGImg.ResliceOnLoad,false) then begin
           gBGImg.OverlaySmooth := isOverlaySmooth;
           exit;
        end;
        gBGImg.OverlaySmooth := isOverlaySmooth;

        ImgForm.RefreshImagesTimer.Enabled := true;
end;//OpenVOIClick


procedure TImgForm.OpenVOIClick(Sender: TObject);
var
	lFilename: string;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image (''File''/''Open'') before adding a VOI.');
		exit;
	end;
	 if not OpenDialogExecute(kVOIFilter,'Select Volume of Interest drawing',false) then exit;
	lFilename := OpenHdrDlg.Filename;
	OpenVOICore(lFilename);
end;//OpenVOIClick

(*procedure TImgForm.SaveVOIClick(Sender: TObject);
var lHdr: TMRIcroHdr;
begin
  if  gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems= 0 then begin
		Showmessage('You need to create a VOI before you can save it.');
		exit;
  end;
  if gBGImg.Mirror then begin
     lHdr.ScrnBufferItems := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
     Getmem(lHdr.ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems);
     Move(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[1],lHdr.ScrnBuffer^[1],lHdr.ScrnBufferItems);
     MirrorScrnBuffer(gBGImg,lHdr);
     SaveAsVOIorNIFTI(lHdr.ScrnBuffer,lHdr.ScrnBufferItems,1,1,true,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName);
     Freemem(lHdr.ScrnBuffer);
     exit; //sept2007
  end;
  SaveAsVOIorNIFTI(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer,gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems,1,1,true,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,gMRIcroOverlay[kVOIOverlayNum].HdrFileName);
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

procedure TImgForm.Extract1Click(Sender: TObject);
var
   lMin : smallint;
   lOtsuLevels,lnVox,lVox,lDilate: integer;
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
 lOtsuLevels  :=  ReadIntForm.GetInt('Otsu levels: larger values for larger volumes',1,4,5);
 lDilate :=  ReadIntForm.GetInt('Edge dilation voxels: larger values for larger volumes',0,2,12);
 lOneContiguousObject := OKMsg('Only extract single largest object?');
 //MaskBackground  (var lImg: Bytep; lXi,lYi,lZi,lOtsuLevels: integer; lDilateVox: single; lOneContiguousObject: boolean );
 MaskBackground(gMRIcroOverlay[kBGOverlayNum].ScrnBuffer, gBGImg.ScrnDim[1],gBGImg.ScrnDim[2],gBGImg.ScrnDim[3],lOtsuLevels,lDilate,lOneContiguousObject);

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

procedure TImgForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //LabelX.caption := inttostr(random(888));
end;

procedure TImgForm.GetWidthForPPI(Sender: TCustomImageList; AImageWidth,
  APPI: Integer; var AResultWidth: Integer);
begin
  case AResultWidth of
    20..24: AResultWidth:=22;
    30..36: AResultWidth:=24;//32;


  end;
  //AResultWidth:=64;
  //LabelX.caption := inttostr(AResultWidth);
end;

procedure TImgForm.Interpolate1Click(Sender: TObject);
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




procedure TImgForm.DilateVOI1Click(Sender: TObject);
begin
    if ((sender as TMenuItem).tag = 1) or (ssShift in KeyDataToShiftState(vk_Shift)) then begin
      MakeShells;
      exit;
 end;
 if (ssCtrl in KeyDataToShiftState(vk_Shift)) then begin
    BatchDilate;
    exit;
 end;
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

procedure TImgForm.dcm2niiMenuClick(Sender: TObject);
begin
  dcm2niiForm.showmodal;
end;

procedure TImgForm.ControlPanelClick(Sender: TObject);
begin
  {$IFDEF Darwin} //release focus so arrow keys move through image
  ImgForm.ActiveControl :=  MagPanel;
  {$ELSE}
  ImgForm.ActiveControl := nil;
  {$ENDIF}
end;

procedure TImgForm.NewWindow1Click(Sender: TObject);
{$IFDEF Darwin}
var
    AProcess: TProcess;
    i : integer;
    //http://wiki.freepascal.org/Executing_External_Programs
begin
  AProcess := TProcess.Create(nil);
  AProcess.InheritHandles := False;
  //AProcess.Options := [poNoConsole];  //poNoConsole is Windows only! http://lazarus-ccr.sourceforge.net/docs/fcl/process/tprocess.options.html
  //AProcess.ShowWindow := swoShow; //Windows only http://www.freepascal.org/docs-html/fcl/process/tprocess.showwindow.html
  for I := 1 to GetEnvironmentVariableCount do
      AProcess.Environment.Add(GetEnvironmentString(I));
  AProcess.Executable := 'open';
  AProcess.Parameters.Add('-n');
  AProcess.Parameters.Add('-a');
  AProcess.Parameters.Add(paramstr(0));
  AProcess.Execute;
  AProcess.Free;
end;
{$ELSE}
begin
   //only OSX/Darwin
end;
{$ENDIF}

procedure TImgForm.ToggleDrawMenu(Sender: TObject);
begin
  gBGImg.ShowDraw := not  DrawMenu.Visible;
  WriteIni2Form(gBGImg);
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
				{id_Yes}mrYes: SaveVOIClick(nil);
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

procedure CopyImg(var lSourceImg,lDestImg: TImage);
var
  lPos: integer;
begin
    if not lSourceImg.Visible then
      exit;
    lDestImg.Canvas.Draw(lSourceImg.Left,lSourceImg.Top,lSourceImg.Picture.Graphic);
end;

procedure TImgForm.SaveOrCopyImages(lCopy: boolean);
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
    {$IFDEF FPC}
    lOutImg.Width := lMaxR;
    lOutImg.Height := lMaxB;
    {$ELSE}
    CreateImg(lMaxB,lMaxR,lOutImg);
    {$ENDIF}
    lOutImg.Canvas.Brush.color := ImgForm.TriplePanel.color;
    lOutImg.Canvas.Rectangle(0,0,lMaxR+1,lMaxB+1);
    CopyImg(ImgForm.PGImageAx,lOutImg);
    CopyImg(ImgForm.PGImageCor,lOutImg);
    CopyImg(ImgForm.PGImageSag,lOutImg);
    if lCopy then begin
       {$IFDEF DARWIN}
         {$IFDEF LCLCocoa}
         Clipboard.Assign(lOutImg.Picture.Graphic);
         {$ELSE}
          Clipboard.Assign(lOutImg.Picture.Graphic);
         showmessage('Copy to clipboard not not tested on Carbon Widgetset');
         {$ENDIF}
       {$ELSE}
       {$IFDEF FPC}
       lOutImg.Picture.Bitmap.SaveToClipboardFormat(CF_BITMAP);//2
       //Clipboard.Assign(lOutImg.Picture.Bitmap);
       {$ENDIF}
       Clipboard.Assign(lOutImg.Picture.Graphic);
       {$ENDIF}
    end else
        SaveImgAsPNGBMP (lOutImg);
  finally
    FreeAndNil (lOutImg);
  end;
end;

procedure TImgForm.Saveaspicture1Click(Sender: TObject);
begin
  SaveOrCopyImages(false);
end;


(*var
  lImage: TImage;
begin
	 lImage := SelectedImage;
	SaveImgAsPNGBMP (lImage);
end; //Proc Saveaspicture1Click
*)

procedure TImgForm.Paste1Click(Sender: TObject);
begin
  if (gBGImg.VOIUndoSlice < 1) then exit;
  if gBGImg.VOIUndoOrient <> SelectedImageNum then //12/2007
     exit;
  WriteUndoVOI(SelectedImageNum,true, false);
  case gBGImg.VOIUndoOrient of
	  3: ReadCorVOI(gDrawImg,ImgForm.YViewEdit.Value);
	  2: ReadSagVOI(gDrawImg,ImgForm.XViewEdit.Value);
	  1: ReadAxialVOI(gDrawImg,ImgForm.ZViewEdit.Value);
	  else exit;
  end;
  ImgForm.RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.Undo1Click(Sender: TObject);
begin
	if gBGImg.VOIUndoSlice < 1 then exit;
	case gBGImg.VOIUndoOrient of
		4: UndoVolVOI;
		3: ReadCorVOI(gUndoImg,gBGImg.VOIUndoSlice);
		2: ReadSagVOI(gUndoImg,gBGImg.VOIUndoSlice);
		1: ReadAxialVOI(gUndoImg,gBGImg.VOIUndoSlice);
	end;
	ImgForm.RefreshImagesTimer.Enabled := true;
end;

procedure TImgForm.Copy1Click(Sender: TObject); //Requires 'ClipBrd' in uses section
begin
  {$IFDEF Darwin} //release focus so arrow keys move through image
  ImgForm.ActiveControl :=  MagPanel;
  {$ELSE}
  ImgForm.ActiveControl := nil;
  {$ENDIF}
  SaveOrCopyImages(true);
end;

(*procedure TImgForm.Copy1Click(Sender: TObject); //Requires 'ClipBrd' in uses section
var
  MyFormat : Word;
  lImage: TImage;
  AData: THandle;
  {$IFNDEF FPC}APalette : HPalette;{$ENDIF}
begin
	 lImage := SelectedImage;
	 if (lImage.Picture.Graphic = nil) then begin //1420z
		Showmessage('You need to load an image before you can copy it to the clipboard.');
		exit;
	 end;
    {$IFNDEF FPC}
	 lImage.Picture.Bitmap.SaveToClipBoardFormat(MyFormat,AData,APalette);
	 ClipBoard.SetAsHandle(MyFormat,AData);
  {$ELSE}
         lImage.Picture.Bitmap.SaveToClipboardFormat(2);
  {$ENDIF}
	 if (gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems>0) then
	 	WriteUndoVOI(SelectedImageNum,false);

end;  *)

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
 lXDim,lYDim,lZDim,lSum,lMinWt,lMaxWt,lMinInten,lMaxInten,lOutVolVox,lOutSliceSz,lX,lY,lZ,lXxi,l2,lZyi: integer;
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
		l32SrcBuff^[lZ] := 0;
	for lZ := lZDim-1 downto 2 do begin
		ProgressBar1.Position := (lZDim-lZ);
		for lY := lYDim-1 downto 2 do begin
			lZyi := ((lZ-1)*lOutSliceSz) + ((lY-1) * lXDim);
			for lX := lXDim-1 downto 2 do begin
                lXxi := lZyi + lX;
				//next: gaussian mean after min/max values are excluded
                lSum32 := 0;
				lMinInten32 := l32TempBuff^[lXxi];
                lMaxInten32 := l32TempBuff^[lXxi];
				lMinWt := 12;
                lMaxWt := 12;
				AddPoint32(l32TempBuff^[lXxi],12);//quad-weight center
				AddPoint32(l32TempBuff^[lXxi-lOutSliceSz],2);//prev slice
				AddPoint32(l32TempBuff^[lXxi+lOutSliceSz],2);//next slices
				AddPoint32(l32TempBuff^[lXxi-1],2);//Left
				AddPoint32(l32TempBuff^[lXxi+1],2);//right
				AddPoint32(l32TempBuff^[lXxi-lXDim],2);//up
				AddPoint32(l32TempBuff^[lXxi+lXDim],2);//down
				AddPoint32(l32TempBuff^[lXxi-lOutSliceSz-1],1);
				AddPoint32(l32TempBuff^[lXxi-lOutSliceSz+1],1);
				AddPoint32(l32TempBuff^[lXxi-lOutSliceSz-lXDim],1);
				AddPoint32(l32TempBuff^[lXxi-lOutSliceSz+lXDim],1);
				AddPoint32(l32TempBuff^[lXxi+lOutSliceSz-1],1);
				AddPoint32(l32TempBuff^[lXxi+lOutSliceSz+1],1);
				AddPoint32(l32TempBuff^[lXxi+lOutSliceSz-lXDim],1);
				AddPoint32(l32TempBuff^[lXxi+lOutSliceSz+lXDim],1);
				AddPoint32(l32TempBuff^[lXxi-lXDim-1],1);
				AddPoint32(l32TempBuff^[lXxi+lXDim-1],1);
				AddPoint32(l32TempBuff^[lXxi-lXDim+1],1);
				AddPoint32(l32TempBuff^[lXxi+lXDim+1],1);
				if lMinInten32 = lMaxInten32 then
				   l32SrcBuff^[lXxi] := lMaxInten32 //no variability in data
				else begin
					 l2 := 36 - lMinWt -lMaxWt;  //weight after we exceed brightest and darkest
					 lSum32 := lSum32 -(lMinWt*lMinInten32) - (lMaxWt*lMaxInten32); //exclude brightest/darkest
					 l32SrcBuff^[lXxi] := (lSum32/l2);
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
		l16SrcBuff^[lZ] := 0;
	for lZ := lZDim-1 downto 2 do begin
		ProgressBar1.Position := (lZDim-lZ);
		for lY := lYDim-1 downto 2 do begin
			lZyi := ((lZ-1)*lOutSliceSz) + ((lY-1) * lXDim);
			for lX := lXDim-1 downto 2 do begin
				lXxi := lZyi + lX;
				//next: gaussian mean after min/max values are excluded
				lSum := 0;
				lMinInten := l16TempBuff^[lXxi];
				lMaxInten := l16TempBuff^[lXxi];
				lMinWt := 12;
				lMaxWt := 12;
				AddPoint(l16TempBuff^[lXxi],12);//quad-weight center
				AddPoint(l16TempBuff^[lXxi-lOutSliceSz],2);//prev slice
				AddPoint(l16TempBuff^[lXxi+lOutSliceSz],2);//next slices
                AddPoint(l16TempBuff^[lXxi-1],2);//Left
				AddPoint(l16TempBuff^[lXxi+1],2);//right
				AddPoint(l16TempBuff^[lXxi-lXDim],2);//up
				AddPoint(l16TempBuff^[lXxi+lXDim],2);//down
                AddPoint(l16TempBuff^[lXxi-lOutSliceSz-1],1);
				AddPoint(l16TempBuff^[lXxi-lOutSliceSz+1],1);
				AddPoint(l16TempBuff^[lXxi-lOutSliceSz-lXDim],1);
				AddPoint(l16TempBuff^[lXxi-lOutSliceSz+lXDim],1);
				AddPoint(l16TempBuff^[lXxi+lOutSliceSz-1],1);
				AddPoint(l16TempBuff^[lXxi+lOutSliceSz+1],1);
                AddPoint(l16TempBuff^[lXxi+lOutSliceSz-lXDim],1);
				AddPoint(l16TempBuff^[lXxi+lOutSliceSz+lXDim],1);
				AddPoint(l16TempBuff^[lXxi-lXDim-1],1);
				AddPoint(l16TempBuff^[lXxi+lXDim-1],1);
                AddPoint(l16TempBuff^[lXxi-lXDim+1],1);
				AddPoint(l16TempBuff^[lXxi+lXDim+1],1);
                if lMinInten = lMaxInten then
				   l16SrcBuff^[lXxi] := lMaxInten //no variability in data
                else begin
					 l2 := 36 - lMinWt -lMaxWt;  //weight after we exceed brightest and darkest
                     lSum := lSum -(lMinWt*lMinInten) - (lMaxWt*lMaxInten); //exclude brightest/darkest
					 l16SrcBuff^[lXxi] := round(lSum/l2);
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
				lMinInten := lTempBuff^[lXxi];
				lMaxInten := lTempBuff^[lXxi];
				lMinWt := 12;
				lMaxWt := 12;
				AddPoint(lTempBuff^[lXxi],12);//quad-weight center
				AddPoint(lTempBuff^[lXxi-lOutSliceSz],2);//prev slice
                AddPoint(lTempBuff^[lXxi+lOutSliceSz],2);//next slices
				AddPoint(lTempBuff^[lXxi-1],2);//Left
				AddPoint(lTempBuff^[lXxi+1],2);//right
				AddPoint(lTempBuff^[lXxi-lXDim],2);//up
                AddPoint(lTempBuff^[lXxi+lXDim],2);//down
				AddPoint(lTempBuff^[lXxi-lOutSliceSz-1],1);
                AddPoint(lTempBuff^[lXxi-lOutSliceSz+1],1);
				AddPoint(lTempBuff^[lXxi-lOutSliceSz-lXDim],1);
                AddPoint(lTempBuff^[lXxi-lOutSliceSz+lXDim],1);
				AddPoint(lTempBuff^[lXxi+lOutSliceSz-1],1);
				AddPoint(lTempBuff^[lXxi+lOutSliceSz+1],1);
				AddPoint(lTempBuff^[lXxi+lOutSliceSz-lXDim],1);
                AddPoint(lTempBuff^[lXxi+lOutSliceSz+lXDim],1);
				AddPoint(lTempBuff^[lXxi-lXDim-1],1);
                AddPoint(lTempBuff^[lXxi+lXDim-1],1);
				AddPoint(lTempBuff^[lXxi-lXDim+1],1);
				AddPoint(lTempBuff^[lXxi+lXDim+1],1);
				if lMinInten = lMaxInten then
				   lSrcBuff^[lXxi] := lMaxInten //no variability in data
				else begin
					 l2 := 36 - lMinWt -lMaxWt;  //weight after we exceed brightest and darkest
					 lSum := lSum -(lMinWt*lMinInten) - (lMaxWt*lMaxInten); //exclude brightest/darkest
					 lSrcBuff^[lXxi] := round(lSum/l2);
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

procedure TImgForm.VOImaskClick(Sender: TObject);
var
 lPreserve: integer;
 lMaskVal : single;
 lMaskVal16: smallint;
 lMaskVal8 : byte;
 lHdr,lMaskHdr: TMRicroHdr;
 lXDim,lYDim,lZDim,lOutVolVox,lOutSliceSz,lZ: integer;
 lSrcBuff,lMaskBuff: Bytep;
 l16SrcBuff: SmallIntP;
 l32SrcBuff: SingleP;
begin
  lPreserve := (sender as TMenuItem).tag;
  lHdr := gMRIcroOverlay[kBGOverlayNum];
  if lPreserve = 2 then begin
     lMaskVal := ReadFloatForm.GetFloat('Change voxels with VOI to what value? ', -9999,2,9999);
     lMaskVal := Scaled2RawIntensity (lHdr, lMaskVal);
  end else
     lMaskVal := lHdr.GlMinUnscaledS;
  //else
  //  lMaskVal := 0.0;
  lMaskVal16 := round(lMaskVal);
  lMaskVal8 := round(lMaskVal);


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

  if  lHdr.ImgBufferBPP = 4 then begin //32-bit float data
	l32SrcBuff := SingleP(lHdr.ImgBuffer);
	if lPreserve = 1 then begin
		for lZ := 1 to lOutVolVox do
			if lMaskBuff^[lZ] = 0 then
				l32SrcBuff^[lZ] := lMaskVal;
	end else begin
		for lZ := 1 to lOutVolVox do
			if lMaskBuff^[lZ] <> 0 then
				l32SrcBuff^[lZ] := lMaskVal;
	end; //if preserve
  end else if (lHdr.ImgBufferBPP = 2) then begin //16-bit int data*)
	l16SrcBuff :=  SmallIntP(lHdr.ImgBuffer );
	if lPreserve = 1 then begin
		for lZ := 1 to lOutVolVox do
			if lMaskBuff^[lZ] = 0 then
				l16SrcBuff^[lZ] := lMaskVal16;
	end else begin
		for lZ := 1 to lOutVolVox do
			if lMaskBuff^[lZ] <> 0 then
				l16SrcBuff^[lZ] := lMaskVal16;
	end;
  end else if lHdr.ImgBufferBPP = 1 then begin //8-bit data
	  lSrcBuff := lHdr.ImgBuffer;
	if lPreserve = 1 then begin
		for lZ := 1 to lOutVolVox do
			if lMaskBuff^[lZ] = 0 then
				lSrcBuff^[lZ] := lMaskVal8
	end else begin
		for lZ := 1 to lOutVolVox do
			if lMaskBuff^[lZ] <> 0 then
				lSrcBuff^[lZ] := lMaskVal8;
	end;
  end else begin //8bit data
	  showmessage('Unknown bits per pixel '+inttostr(lHdr.ImgBufferBPP) );
  end;
  if gBGImg.Mirror then
     MirrorScrnBuffer(gBGImg,lMaskHdr);//4/2008

  ProgressBar1.Position := 0;
  RescaleImgIntensity(gBGImg,gMRIcroOverlay[kBGOverlayNum],kBGOverlayNum);
  RefreshImagesTimer.Enabled := true;
end;  //VOImaskClick

procedure TImgForm.Sagittal1Click(Sender: TObject);
begin
      gBGImg.SliceView :=  (Sender as TMenuItem).Tag;
    RefreshImagesTimer.Enabled := true;
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
	if lComparison = 0 then begin //intersect AND
	  for lOverlay := 1 to (knMaxOverlay-1) do begin
		if gMRIcroOverlay[lOverlay].ScrnBufferItems = lVolItems then begin
			for lPos := 1 to lVolItems do
				if gMRIcroOverlay[lOverlay].ScrnBuffer^[lPos] = 0 then
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lPos] := 0;
		end; //if overlay loaded
	  end; //for each overlay
	end else if lComparison = 1 then begin //if intersect else UNION OR
	  for lOverlay := 1 to (knMaxOverlay-1) do begin
		if gMRIcroOverlay[lOverlay].ScrnBufferItems = lVolItems then begin
			for lPos := 1 to lVolItems do
				if gMRIcroOverlay[lOverlay].ScrnBuffer^[lPos] > 0 then
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lPos] := kVOI8bit;
		end; //if overlay loaded
	  end; //for each overlay
	end else if lComparison = 2 then begin //if union else MASK
	  for lOverlay := 1 to (knMaxOverlay-1) do begin
		if gMRIcroOverlay[lOverlay].ScrnBufferItems = lVolItems then begin
			for lPos := 1 to lVolItems do
				if gMRIcroOverlay[lOverlay].ScrnBuffer^[lPos] > 0 then
					gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lPos] := 0;
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
	AutoROIForm.show;
        AutoROIForm.Refresh;
end;


procedure TImgForm.SmoothVOI1Click(Sender: TObject);
begin
 voismoothform.showmodal;
 //SmoothVOIForm.Showmodal
end;

procedure TImgForm.CreateOverlap(Sender: TObject);
var
	lNumberofFiles,lC,lOverlay,lPos: integer;
	lFilename,lExt: string;
	lOverlapBuffer: ByteP;
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then begin
		showmessage('Please load a background image (''File''/''Open'') before adding an overlay.');
		exit;
	end;
	 lOverlay := 0;
	 for lC := 1 to (knMaxOverlay-1) do //-1: save final overlay for VOI
		  if (lOverlay = 0) and (gMRIcroOverlay[lC].ImgBufferItems = 0) then
			lOverlay := lC;
	 if lOverlay = 0 then begin
		showmessage('Unable to add an overlay. You have loaded the maximum number of overlays.');
		exit;
	 end;
	if not OpenDialogExecute(kVOIFilter,'Select VOIs you wish to combine',true) then exit;
	lNumberofFiles:= OpenHdrDlg.Files.Count;
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
		lFilename := OpenHdrDlg.Files[lC-1];
		lExt := UpCaseExt(lFileName);
		gBGImg.VOIchanged := false;
		if not OpenAndDisplayHdr(lFilename,gMRIcroOverlay[lOverlay]) then exit;
		if not OpenImg(gBGImg,gMRIcroOverlay[lOverlay],false,false,false,gBGImg.ResliceOnLoad,false) then exit;
		ProgressBar1.Position := lC;
		for lPos := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do
			if gMRIcroOverlay[lOverlay].ScrnBuffer^[lPos] > 0 then
				lOverlapBuffer^[lPos] :=  lOverlapBuffer^[lPos]+1;
		FreeImgMemory(gMRIcroOverlay[lOverlay]);
	end; //for each image
	//July07 getmem for unaligned buffer getmem(gMRIcroOverlay[lOverlay].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems);
        GetMem(gMRIcroOverlay[lOverlay].ImgBufferUnaligned ,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems + 16); //July072007
        //gMRIcroOverlay[lOverlay].ImgBuffer := ByteP($fffffff0 and (integer(gMRIcroOverlay[lOverlay].ImgBufferUnaligned)+15));
         gMRIcroOverlay[lOverlay].ImgBuffer := system.align(gMRIcroOverlay[lOverlay].ImgBufferUnaligned, 16);
        gMRIcroOverlay[lOverlay].ImgBufferItems := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
	for lPos := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do
		gMRIcroOverlay[lOverlay].ImgBuffer[lPos] := lOverlapBuffer[lPos];
	freemem(lOverlapBuffer);
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],gMRIcroOverlay[lOverlay],0, lNumberofFiles,1,0,0,kNIFTI_INTENT_ESTIMATE,'N'+inttostr(lNumberofFiles) );
	UpdateLayerMenu;
	RescaleImgIntensity(gBGImg,gMRIcroOverlay[lOverlay],lOverlay);
	ProgressBar1.Position := 0;
	//SaveAsVOIorNIFTI(gMRIcroOverlay[lOverlay].ImgBuffer,gMRIcroOverlay[lOverlay].ScrnBufferItems,1,false,gMRIcroOverlay[lOverlay].niftiHdr,'sum'+inttostr(lNumberofFiles));
	SaveAsVOIorNIFTI(gMRIcroOverlay[lOverlay].ImgBuffer,gMRIcroOverlay[lOverlay].ScrnBufferItems,1,1,false,gMRIcroOverlay[lOverlay].niftiHdr,'sum'+inttostr(lNumberofFiles));
	RefreshImagesTimer.Enabled := true;
end;//proc CreateOverlap

procedure TImgForm.Chisquare1Click(Sender: TObject);
var
	lNegativeNumbers: boolean;
	lVolVoxels,lPos,lnTotalThreshold,lLoop,lnVoxelsTested:integer;
	lMinExp,lChi,lChip,luChi, luChiP: double;
	lMaxChi,lMinChi: single;
	lBufferAligned,lBufferUnAligned,lBuffer: ByteP;
	l32Buf : SingleP;
	lFilename: string;
	lTotal,lYes,lNo: array [1..2] of integer;
	lMRIcroHdr: TMRIcroHdr;
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
		lFilename := OpenHdrDlg.Filename;
		if not OpenAndDisplayHdr(lFilename,gMRIcroOverlay[lLoop]) then exit;
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
	lnTotalThreshold:= ReadIntForm.GetInt('Only test voxels damaged in at least N patients [A+B]', 1,1,(lTotal[1]+lTotal[2]));
	GetMem(lBufferUnaligned ,(lVolVoxels *sizeof(single) )+16);
	//lBufferAligned := ByteP($fffffff0 and (integer(lBufferUnaligned)+15));
        lBufferAligned := system.align(lBufferUnaligned, 16);
        l32Buf := SingleP(lBufferAligned);
	lnVoxelsTested := 0;
	lNegativeNumbers := false;
	lMaxChi := 0;
	lMinChi := 0;
	for lPos := 1 to lVolVoxels do begin
		l32Buf^[lPos] := 0;
		lYes[1] := gMRIcroOverlay[1].ScrnBuffer^[lPos];
		lNo[1] := lTotal[1]-lYes[1];
		lYes[2] := gMRIcroOverlay[2].ScrnBuffer^[lPos];
		lNo[2] := lTotal[2]-lYes[2];
		if (lYes[1] < 0) or (lNo[1] < 0) or (lYes[2] < 0) or (lNo[2] < 0) then
			lNegativeNumbers := true
		else if (lYes[1]+lYes[2]) >= lnTotalThreshold then begin//e.g. at least 30% of all patients
				  inc(lnVoxelsTested);
				  //showmessage(inttostr(lYes[1])+'x'+inttostr(lNo[1])+'x'+ inttostr(lYes[2])+'x'+inttostr(lNo[2]) );
				  Chi2x2 (lYes[1], lNo[1], lYes[2], lNo[2],lMinExp,lChi,lChip,luChi, luChiP);
				  if (luChi) > lMaxChi then
					lMaxChi := (luChi)
				  else if (luChi < lMinChi) then
					lMinChi := luChi;
				  if (lYes[1]/lTotal[1]) > (lYes[2]/lTotal[2]) then
					 l32Buf^[lPos] := luChi//100-(100*luChip) //positives more likely than negative
				  else
					  l32Buf^[lPos] := -luChi;//-100+(100*luChip); //negatives more common
		end;//> threshold
	end; //for each voxel
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lMRIcroHdr,lMinChi, lMaxChi,1{df},0,lnVoxelsTested,kNIFTI_INTENT_CHISQ,inttostr(lnVoxelsTested) );
	if lNegativeNumbers then
		Showmessage('Serious error: some group sizes were negative. This should be impossible with a Chi-Squared.');
	//SaveAsVOIorNIFTI(lBufferAligned,lVolVoxels,4,false,lMRIcroHdr.NiftiHdr,'chi'+inttostr(lnTotalThreshold));
	SaveAsVOIorNIFTI(lBufferAligned,lVolVoxels,4,1,false,lMRIcroHdr.NiftiHdr,'log10p'+inttostr(lnTotalThreshold));
	//next - save log10 p values...
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lMRIcroHdr,lMinChi, lMaxChi,1{df},0,lnVoxelsTested,NIFTI_INTENT_LOG10PVAL,inttostr(lnVoxelsTested) );
	for lPos := 1 to lVolVoxels do
		if l32Buf^[lPos] > 0 then
			 l32Buf^[lPos] := -log(abs(gammq(0.5, 0.5 * l32Buf^[lPos])),10)
		else
			l32Buf^[lPos] :=0;
	SaveAsVOIorNIFTI(lBufferAligned,lVolVoxels,4,1,false,lMRIcroHdr.NiftiHdr,'log10p'+inttostr(lnTotalThreshold));
	//next - free float buffer
	FreeMem(lBufferUnaligned);
	StatusLabel.Caption := 'Voxels tested: '+inttostr(lnVoxelsTested);
	//next - subtraction
	GetMem(lBuffer ,(lVolVoxels ));
	lNegativeNumbers := false;
	fillchar(lBuffer^,lVolVoxels,100);
	for lPos := 1 to lVolVoxels do begin
		lYes[1] := gMRIcroOverlay[1].ScrnBuffer^[lPos];
		lNo[1] := lTotal[1]-lYes[1];
		lYes[2] := gMRIcroOverlay[2].ScrnBuffer^[lPos];
		lNo[2] := lTotal[2]-lYes[2];
		if (lYes[1] < 0) or (lNo[1] < 0) or (lYes[2] < 0) or (lNo[2] < 0) then
			lNegativeNumbers := true
		else if (lYes[1] >0) or (lYes[2] > 0) then begin
			lBuffer^[lPos] := round((100* ((lYes[1]/lTotal[1])-(lYes[2]/lTotal[2])))+100);
		end;//> threshold
	end; //for each voxel
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lMRIcroHdr,-100, 100,1,0,0,kNIFTI_INTENT_ESTIMATE,'%'+inttostr(lTotal[1])+':'+inttostr(lTotal[2]) );
	lMRIcroHdr.NIFTIhdr.scl_inter:= -100;
	if lNegativeNumbers then
		Showmessage('Serious error: some group sizes were negative. This should be impossible with a subtraction analysis.');
	SaveAsVOIorNIFTI(lBuffer,lVolVoxels,1,1,false,lMRIcroHdr.NiftiHdr,'Sub'+inttostr(lTotal[1])+'_'+inttostr(lTotal[2]));
	FreeMem(lBuffer);
end; //procedure Chisquare1Click

procedure Paris(lFilename: string);
begin
     ImgForm.CloseImagesClick(nil);

     ImgForm.OpenAndDisplayImg(lFilename,True);
  ImgForm.caption := 'x';
  //if not HdrForm.OpenAndDisplayHdr(lFilename,gMRIcroOverlay[kBGOverlayNum]) then exit;
  ImgForm.Caption := 'y';
end;


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
           if not OpenAndDisplayHdr(gMRIcroOverlay[kBGOverlayNum].HdrFileName,gMRIcroOverlay[kBGOverlayNum]) then exit;
           if not OpenImg(gBGImg,gMRIcroOverlay[0],true,false,false,false,false) then exit;
        end;
	showmessage('Warning: the currently open background image must have the dimensions (size, space between slices, etc) as the image used when creating the ROIs.');
	if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0 then
		CloseVOIClick(nil);
	if not OpenDialogExecute('MRIcro ROI (.roi)|*.roi','Select MRIcro format ROIs to convert',true) then exit;
	lNumberofFiles:= OpenHdrDlg.Files.Count;
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := OpenHdrDlg.Files[lC-1];
		OpenMRIcroROI (lFileName);
		lFilename := changefileextX(lFilename,'.voi');
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
(*	if gBGImg.XBarClr = TColor(gMRIcroOverlay[kBGOverlayNum].LUTinvisible) then
		MultiSliceForm.MultiImage.canvas.font.Color := clBlack//clWhite;//gLUT[lClr].rgbRed+(gLUT[lClr].rgbGreen shl 8)+(gLUT[lClr].rgbBlue shl 16);
	 else
		MultiSliceForm.MultiImage.canvas.font.Color := gBGImg.XBarClr;*)
	MultiSliceForm.Show;
 //MultiSliceForm.BringToFront;
end;

function RawBGIntensity(lPos: integer): single;
var
	l16Buf : SmallIntP;
	l32Buf : SingleP;
begin
  result := 0;
  if (lPos > gMRIcroOverlay[kBGOverlayNum].ImgBufferItems) or (lPos < 1) then exit;
  if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 4) then begin
	l32Buf := SingleP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	result := l32Buf^[lPos];
  end else if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 2) then begin
	   l16Buf := SmallIntP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
	result := l16Buf^[lPos];
  end else if gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 1 then
	 result := gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lPos]
  else begin
	showmessage('Unknown Background Buffer Bytes Per Pixel');
	exit;
  end;
end;

(*procedure DescribeVOIonLabelsX (lOverlayNum: integer);
var
   lShowfilename: boolean = true;
   lLocalMax,lLocalSum : HistoDoubleRA;
   l16Buf : SmallIntP;
   l32Buf : SingleP;
   l8Buf: byteP;
   lInten: double;
   lXmm,lYmm,lZmm: single;
   lHisto,lRegionVol,lLocalMaxPos: HistoRA;
   lInc,lRegion: Integer;
   lLabelStr: string;
   lVOI: boolean;
   lLabelStr20 : Array[0..kHistoBins] of kstr20;
begin
     lInten := 0;//just to hide compiler hint...
     if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP = 2) and ('ratlas.nii.gz' = (extractfilename( gMRIcroOverlay[kBGOverlayNum].HdrFileName))) then begin
        //DescribeVOIonLabelsRAT(lOverlayNum,lShowFilename);
        Showmessage('Please use Windows version.');
        exit;
     end;
     if (gMRIcroOverlay[lOverlayNum].ScrnBufferItems <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems) or (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP <> 1) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 2) then
        exit;
     TextForm.MemoT.Lines.add(' Custom Region Analysis');
     TextForm.MemoT.Lines.add(' For Speculative Brodmann Map: 0=not cortical and 48=no Brodmann label');
     lVOI := IsVOIROIExt(gMRIcroOverlay[lOverlayNum].HdrFileName);
     if (not lVOI) and (lOverlayNum = kVOIOverlayNum) then
        lVOI := true;
     //next describe format
     if lShowfilename then
        lLabelStr := ' Filename,'
     else
         lLabelStr := ' ';
     if lVOI then //intensity min/max position are not important
        TextForm.MemoT.Lines.add(lLabelStr+'Area'+kTextSep+'N>0'+kTextSep+'%N>0')
     else
         TextForm.MemoT.Lines.add(lLabelStr+'Area'+kTextSep+'N>0'+kTextSep+'%N>0'+kTextSep+'Sum>0'+kTextSep+'Mean>0'+kTextSep+'Max'+kTextSep+'MaxX'+kTextSep+'MaxY'+kTextSep+'MaxZ');
   //next initialize
   if lShowFilename then
      lLabelStr := gMRIcroOverlay[lOverlayNum].HdrFileName+','
   else
       lLabelStr := '';
     for lInc := 0 to kHistoBins do begin
         lHisto[lInc] := 0;
         lLocalMax[lInc] := 0;
         lLocalSum[lInc] := 0;
         lRegionVol[lInc] := 0;
         if (gMRIcroOverlay[kBGOverlayNum].UsesLabels) then
            lLabelStr20[lInc] := gBGImg.LabelRA[lInc]// gBGImg.LabelStr20[lInc]
         else
             lLabelStr20[lInc] := inttostr(lInc);
     end;
     for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do
         if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then
            inc(lHisto[gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lInc]]);
     //local max start
     l32Buf := SingleP(gMRIcroOverlay[lOverlayNum].ImgBuffer );
     l16Buf := SmallIntP(gMRIcroOverlay[lOverlayNum].ImgBuffer );
     //NEXT if..else July07 - ROIs only use screen buffer, not imgbuffer...
     if gMRIcroOverlay[lOverlayNum].ScrnBufferItems = gMRIcroOverlay[lOverlayNum].ImgBufferItems then
        l8Buf := gMRIcroOverlay[lOverlayNum].ImgBuffer
     else
         l8Buf := gMRIcroOverlay[lOverlayNum].ScrnBuffer;
     for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
         if (gMRIcroOverlay[lOverlayNum].ImgBufferBPP  = 4) then
           lInten := l32Buf^[lInc]
         else if (gMRIcroOverlay[lOverlayNum].ImgBufferBPP  = 2) then
              lInten := l16Buf^[lInc]
         else if gMRIcroOverlay[lOverlayNum].ImgBufferBPP  = 1 then
	      lInten := l8Buf^[lInc];//July07
         lRegion := gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lInc];
         if lInten > 0 then
            lLocalSum[lRegion] := lLocalSum[lRegion]+lInten;
         if  lInten > lLocalMax[lRegion] then begin
             lLocalMax[lRegion] := lInten;//intensity
             lLocalMaxPos[lRegion] := lInc;//location
         end;
         inc(lRegionVol[lRegion]);
     end;

     for lInc := 0 to kHistoBins do  begin
         if (not lVOI) and (lLocalMax[lInc] > 0) then begin
            lLocalMax[lInc] := Raw2ScaledIntensity (gMRIcroOverlay[lOverlayNum],lLocalMax[lInc]);
            lLocalSum[lInc] := Raw2ScaledIntensity (gMRIcroOverlay[lOverlayNum],lLocalSum[lInc]);
            ImgPosToMM(lLocalMaxPos[lInc], lXmm,lYmm,lZmm);
            TextForm.MemoT.Lines.Add(lLabelStr+ lLabelStr20[lInc] +kTextSep+ inttostr(lHisto[lInc])+kTextSep+floattostr( lHisto[lInc]/lRegionVol[lInc])
               +kTextSep+floattostr( lLocalSum[lInc])+kTextSep+floattostr( lLocalSum[lInc]/lRegionVol[lInc]) //Sum>0, mean>0
               +kTextSep + floattostr(lLocalMax[lInc])+kTextSep+floattostr(lXmm)+kTextSep+floattostr(lYmm)+kTextSep+floattostr(lZmm) );
         end else if (lHisto[lInc] > 0) {necessarily also and (lRegionVol[lInc] > 0)} then
	 		TextForm.MemoT.Lines.Add(gBGImg.LabelRA[lInc] + kTextSep+ inttostr(lHisto[lInc])+kTextSep+floattostr( lHisto[lInc]/lRegionVol[lInc])) ;
     end; //for each row
end; 2014: no longer used (16 bit LabelRA)*)


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

procedure TImgForm.DescriptiveMenuItemClick(Sender: TObject);
var
	lROIVol: array [1..3] of integer;
	lInc,lOverlayNum,lImgSz: integer;
	lCenterOfMass,lROISum,lROISumSqr,lROImin,lROImax:array [1..3] of double;
	lMode,lCC,lVal,lSD,lROImean: double;
	lStr: string;
procedure  AddVal( lRA: integer);
begin
			inc(lROIVol[lRA]);
			lROISum[lRA] := lROISum[lRA]+lVal;
			lROISumSqr[lRA] := lROISumSqr[lRA] + sqr(lVal);
			if lVal > lROImax[lRA] then
				lROImax[lRA] := lVal;
			if lVal < lROImin[lRA] then
				lROImin[lRA] := lVal;
end;
begin
	lImgSz := 0;
	for lOverlayNum := 1 to knMaxOverlay do
		if gMRIcroOverlay[lOverlayNum].ScrnBufferItems > lImgSz then
			lImgSz := gMRIcroOverlay[lOverlayNum].ScrnBufferItems;
	if (lImgSz < 1) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < lImgSz) then begin
		Showmessage('You need to create or load an overlay (Overlay/Open or Draw/OpenVOI) to get overlay statistics.');
		exit;
	end;

 TextForm.MemoT.Lines.Clear;
 for lOverlayNum := 1 to knMaxOverlay do begin
   if gMRIcroOverlay[lOverlayNum].ScrnBufferItems = gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then begin
	for lInc := 1 to 3 do begin
		lROIVol[lInc] := 0;
		lROISum[lInc] := 0;
		lROISumSqr[lInc] := 0;
		lROImin[lInc] := maxint;
		lROImax[lInc] := -maxint;

	end;
	for lInc := 1 to gMRIcroOverlay[lOverlayNum].ScrnBufferItems do begin
		if gMRIcroOverlay[lOverlayNum].ScrnBuffer^[lInc] > 0 then begin
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
	TextForm.MemoT.Lines.Add('Overlay '+gMRIcroOverlay[lOverlayNum].HdrFileName);
        if  CenterOfMass (lOverlayNum, lCenterOfMass[1],lCenterOfMass[2],lCenterOfMass[3]) > 0 then
            TextForm.MemoT.Lines.Add(' Center of mass XYZ '+RealToStr(lCenterOfMass[1],2)+'x'+RealToStr(lCenterOfMass[2],2)+'x'+RealToStr(lCenterOfMass[3],2));
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
		if lROIVol[lInc] > 0 then
			lROImean := lROISum[lInc]/lROIVol[lInc]
		else
			lROImean := 0;
		//next - calibrate values
		lROImin[lInc] := Raw2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],lROImin[lInc]);
		lROIMean := Raw2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],lROIMean);
		lROImax[lInc] := Raw2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],lROImax[lInc]);
		lSD := Raw2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],lSD);
		lcc := ((lROIVol[lInc]/1000)*gBGImg.ScrnMM[1]*gBGImg.ScrnMM[2]*gBGImg.ScrnMM[3]);
		case lInc of
			3: lStr := 'VOI  >0 ';
			2: lStr := 'VOI <>0 ';
			else lStr := 'VOI     ';
		end;
		lStr := lStr+' nvox(cc)=min/mean/max=SD: '+inttostr(round(lROIVol[lInc]))+kTextSep+RealToStr(lCC,2)+kTextSep+'='+RealToStr(lROIMin[lInc],4)+kTextSep+realToStr(lROIMean,4)+kTextSep+realToStr(lROIMax[lInc],4)+kTextSep+'='+kTextSep+realtostr(lSD,4);
		TextForm.MemoT.Lines.Add(lStr);
	end;
        lMode := Mode(lOverlayNum);
        if lMode <> NaN then
           TextForm.MemoT.Lines.Add('Mode:'+kTextSep+floattostr(lMode));
        if 	gMRIcroOverlay[kBGOverlayNum].UsesLabels then
		DescribeVOIonLabels(lOverlayNum,false);
	TextForm.MemoT.Lines.Add('');
   end; //overlaynum loaded
 end; //for each overlay
	TextForm.Show;
end;

procedure TImgForm.FormResize(Sender: TObject);
begin
        if not ImgForm.visible then
        exit;

     RefreshImagesTimer.enabled := true;
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
end;

procedure TImgForm.OnLaunch;
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
  {$IFNDEF FPC}
		   lStr := string(StrUpper(PChar(lStr))) ;
  {$ELSE}
     {$IFNDEF UNIX}
                   lStr := UpCase(lStr); //unix file names are case specific /EXAMPLE/ATTENTION.NII <> /Example/Attention
     {$ENDIF}
  {$ENDIF}
end; //nested ReadCmdVal
begin
  {$IFDEF Darwin}
  //Darwin starts passing a strange paramstr....
  //with Darwin, opening a file can interfere with opening by association...
  exit;
  //ResliceImg ('/Users/crlab/Documents/example_func.nii.gz','/Users/crlab/Documents/v1x.voi','/Users/crlab/Documents/example_func2standard.mat','/Users/crlab/Documents/z1x.nii.gz');
  {$ENDIF}

 if (ParamCount  < 1) then begin
  ImgForm.OpenTemplateMRU(nil);
                RefreshImagesTimer.enabled := true;
                               exit;

	end;
	lMaximize := false;
	lRender := false;
	lMultislice := false;
	lOverlayNum := 0;
	I := 1;
	lStr := ParamStrFilename(I);
	if lStr <> '' then
		OpenAndDisplayImg(lStr,True)
	 else begin //no requested image
		OpenTemplateMRU(nil);
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
						  if lError = 0 then
							gBGImg.BGTransPct  := round(lSingle);
                                                                 SetSubmenuWithTag(BGTransPctMenu, gBGImg.BGTransPct);
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
                                                {$IFDEF UNIX}
                                                lStr := UpCase(lStr);
                                                {$ENDIF}
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
				'D': gBGIMg.SaveDefaultIni := true;
                                'F': gBGImg.ResliceOnLoad := false; //turn off reslicing... loads files flat
    'H': begin
						  ReadCmdVal;
						  Val(lStr,lSingle,lError);
						  if lError = 0 then
								 gMRIcroOverlay[lOverlayNum].WindowScaledMax  := (lSingle);
					end;
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

					 end; //if 'M'
				'O': begin//Overlay
				                   ReadCmdVal;
                                                   //Showmessage('o'+lStr);
                                             if (lStr <> '') and (FileexistsEx(lStr)) and (lOverlayNum < (knMaxOverlay-1)) then begin
                                                   //Showmessage('oexists'+lStr);
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
   for lInc := 0 to lOverlayNum do
	 RescaleImgIntensity(gBGImg,gMRIcroOverlay[lInc],lINc);
	 RefreshImages;
   if lMultiSlice then
		ShowMultisliceClick(nil);
   if lRender then
		ShowRenderClick(nil);
   if lMaximize then begin
	   ImgForm.WindowState := wsMaximized;
          RefreshImagesTimer.enabled := true;
   end;
end;

{$IFDEF FPC}
function latestGitRelease(url: string): string;
//Returns string for latest release (error will return empty string)
//example
// latestGitRelease('https://api.github.com/repos/rordenlab/dcm2niix/releases/latest');
//will return
// "v1.0.20171204"
const
     key = '"tag_name":"';
var
  s, e: integer;
  cli: TFPHTTPClient;
begin
  result := '';
  cli := TFPHTTPClient.Create(nil);
  cli.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  try
    try
      result := Cli.Get(url);
    except
      result := '';
    end;
  finally
    cli.free
  end;
  if length(result) < 1 then exit;
  s := posex(key, result);
  if s < 1 then begin
     result := '';
     exit;
  end;
  s := s+length(key);
  e:= posex('"', result, s);
  if e < 1 then begin
     result := '';
     exit;
  end;
  result := copy(result, s, e-s);
end;

procedure ReportGitVer(localVer, api, url, exe: string);
var
  gitVer, exeNam: string;
  git, local: integer;
begin
  exeNam := ExtractFileName(exe);
  if length(localVer) < 8 then begin  //last 8 digits are date: v.1.0.20170101
     MessageDlg(exeNam,'Unable to detect version:  '+exe, mtConfirmation,[mbOK],0) ;
     //showmessage('Unable to detect latest version:  '+exe);
     Clipboard.AsText := exe+' : '+ localVer;
     exit;
  end;
  gitVer := latestGitRelease(api);
  if length(gitVer) < 8 then begin  //last 8 digits are date: v.1.0.20170101
      showmessage('Unable to detect latest version: are you connected to the web and do you have libssl installed? '+api);
      exit;
  end;
  if CompareText(gitVer, localVer) = 0 then begin
      //showmessage('You are running the latest release '+localVer);
      MessageDlg(exeNam,'You are running the latest release '+localVer, mtConfirmation,[mbOK],0) ;
      exit;
  end;
  git := strtointdef(RightStr(gitVer,8),0);
  local := strtointdef(RightStr(localVer,8),0);
  if local > git then
     MessageDlg(exeNam,'You are running a beta release '+localVer+', the latest stable release is '+gitVer+' Visit '+url +' to update '+exe, mtConfirmation,[mbOK],0)
     //showmessage('You are running a beta release '+localVer+', the latest stable release is '+gitVer+' Visit '+url +' to update '+exe)
  else
    MessageDlg(exeNam,'You are running an old release '+localVer+', the latest stable release is '+gitVer+' Visit '+url +' to update '+exe, mtConfirmation,[mbOK],0)
          //showmessage('You are running an old release '+localVer+', the latest stable release is '+gitVer+' Visit '+url +' to update '+exe);
end;

procedure CheckForUpdatesMRIcron;
const
     //https://github.com/neurolabusc/MRIcron/releases
     kBase = '/neurolabusc/MRIcron/releases/latest';
     kUrl = 'https://github.com' + kBase;
     kApi = 'https://api.github.com/repos' + kBase;
begin
     ReportGitVer(kVers, kApi, kUrl, paramstr(0));
end;

function delimStr(s, default: string; idx: integer): string;
//e.g. delimStr('Chris Rorden's dcm2niiX version v1.0.20171215 GCC6.1.0',5) returns 'v1.0.20171215'
var
   strs : TStringList;
begin
     result := default;
     strs := TStringList.Create;
     strs.DelimitedText := s;
     if (strs.Count >= idx) then
        result := strs[idx-1]; //string lists are indexed from 0
     strs.Free;
end;

procedure TImgForm.CheckForUpdates(Sender: TObject);
begin
     CheckForUpdatesMRIcron;
end;
{$ELSE}
procedure TImgForm.CheckForUpdates(Sender: TObject);
begin
	//not available for windows
end;

{$ENDIF}

{$IFDEF LCLCocoa}
procedure TImgForm.SetDarkMode;
begin
  //setThemeMode(Self.Handle, gBGImg.DarkMode);
  setThemeMode(Self, gBGImg.DarkMode);
end;
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
  {$IFNDEF FPC}
		   lStr := string(StrUpper(PChar(lStr))) ;
  {$ELSE}
     {$IFNDEF UNIX}
                   lStr := UpCase(lStr); //unix file names are case specific /EXAMPLE/ATTENTION.NII <> /Example/Attention
     {$ENDIF}
  {$ENDIF}
end; //nested ReadCmdVal
begin
  //CheckForUpdates;
     {$IFDEF LCLCocoa}
   SetDarkMode;
   {$ENDIF}
  DrawMenu.Visible := ToolPanel.visible;
  {$IFDEF FPC}MagnifyMenuItem.visible := false;
  {$ENDIF}
  {$IFDEF Darwin}
  Exit1.visible := false;//with OSX users quit from application menu

  //Darwin starts passing a strange paramstr....
  //with Darwin, opening a file can interfere with opening by association...

 (*lStr := '/Users/rorden/desktop/mricrox/templates/aal.nii.gz';

 ImgForm.OpenAndDisplayImg(lStr,True);
     lStr := '/Users/rorden/desktop/mricrox/templates/crap.voi';
 LoadOverlayIncludingRGB{LoadOverlay}(lStr);    *)
 lRender := gBGImg.Prompt4DVolume;
 gBGImg.Prompt4DVolume := false;
 ImgForm.OpenTemplateMRU(nil);
 RefreshImagesTimer.enabled := true;
 gBGImg.Prompt4DVolume := lRender;

 //ShowMultisliceClick(nil);
 exit;
  //ResliceImg ('/Users/crlab/Documents/example_func.nii.gz','/Users/crlab/Documents/v1x.voi','/Users/crlab/Documents/example_func2standard.mat','/Users/crlab/Documents/z1x.nii.gz');
  {$ENDIF}

 if (ParamCount  < 1) then begin
    lRender := gBGImg.Prompt4DVolume;
    gBGImg.Prompt4DVolume := false;
    ImgForm.OpenTemplateMRU(nil);
    gBGImg.Prompt4DVolume := lRender;
    RefreshImagesTimer.enabled := true;
    exit;
  end;
	lMaximize := false;
	lRender := false;
	lMultislice := false;
	lOverlayNum := 0;
	I := 1;
	lStr := ParamStrFilename(I);
	if lStr <> '' then
		OpenAndDisplayImg(lStr,True)
	 else begin //no requested image
		OpenTemplateMRU(nil);
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
						  if lError = 0 then
							gBGImg.BGTransPct  := round(lSingle);
                                                                 SetSubmenuWithTag(BGTransPctMenu, gBGImg.BGTransPct);
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
                                                {$IFDEF UNIX}
                                                lStr := UpCase(lStr);
                                                {$ENDIF}
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
				'D': gBGIMg.SaveDefaultIni := true;
                                'F': gBGImg.ResliceOnLoad := false; //turn off reslicing... loads files flat
    'H': begin
						  ReadCmdVal;
						  Val(lStr,lSingle,lError);
						  if lError = 0 then
								 gMRIcroOverlay[lOverlayNum].WindowScaledMax  := (lSingle);
					end;
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

					 end; //if 'M'
				'O': begin//Overlay
				                   ReadCmdVal;
                                                   //Showmessage('o'+lStr);
                                             if (lStr <> '') and (FileexistsEx(lStr)) and (lOverlayNum < (knMaxOverlay-1)) then begin
                                                   //Showmessage('oexists'+lStr);
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
   for lInc := 0 to lOverlayNum do
	 RescaleImgIntensity(gBGImg,gMRIcroOverlay[lInc],lINc);
	 RefreshImages;
   if lMultiSlice then
		ShowMultisliceClick(nil);
   if lRender then
		ShowRenderClick(nil);
   if lMaximize then begin
	   ImgForm.WindowState := wsMaximized;
          RefreshImagesTimer.enabled := true;
   end;
   DefaultControlPanel;
end;


procedure TImgForm.FlipLRmenuClick(Sender: TObject);
var
	lC: integer;
	lStr: string;
begin
	(sender as TMenuItem).checked := not (sender as TMenuItem).checked;
        Caption := 'MRIcron';
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
	//ComputeValFile( (sender as Tmenuitem).tag);
end;

procedure TImgForm.VOI2NIIClick(Sender: TObject);
var
	lNumberofFiles,lC: integer;
	lFilename: string;
begin
	CloseImagesClick(nil);
	if not OpenDialogExecute('VOI Drawings (.VOI)|*.VOI','Select VOI format images to convert',true) then exit;
	lNumberofFiles:= OpenHdrDlg.Files.Count;
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := OpenHdrDlg.Files[lC-1];
		OpenAndDisplayImg(lFilename,True);
		lFilename := changefileextx(lFilename,'.nii');
		//SaveAsVOIorNIFTIcore (lFilename, lByteP, lVoxels, 1, gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
		SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
		CloseVOIClick(nil);
		ProgressBar1.Position := lC;
	end;
	ProgressBar1.Position := 0;
end;//VOI->NII

procedure TImgForm.TtoP1Click(Sender: TObject);
var
	lBufferAligned,lBufferUnAligned: ByteP;
	l32Buf,l32BufSrc : SingleP;
	l16BufSrc : SmallIntP;
	lSlope,lIntercept: single;
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
	//lBufferAligned := ByteP($fffffff0 and (integer(lBufferUnaligned)+15));
        lBufferAligned := system.align(lBufferUnaligned, 16);
        l32Buf := SingleP(lBufferAligned);
	//next load values
	case gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP of
		4: begin
		   l32BufSrc := SingleP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
		   for lPos := 1 to lVolVoxels do
				l32Buf^[lPos] := l32BufSrc^[lPos];
		end;
		2: begin
		   l16BufSrc := SmallIntP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
		   for lPos := 1 to lVolVoxels do
				l32Buf^[lPos] := l16BufSrc^[lPos];
		end;
		1: begin
		   for lPos := 1 to lVolVoxels do
				l32Buf^[lPos] := gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lPos];
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
				l32Buf^[lPos] := (l32Buf^[lPos] * lSlope)+lIntercept;
	end;
	//next - save log10 p values...
	MakeStatHdr (gMRIcroOverlay[kBGOverlayNum],lMRIcroHdr,0, 255,1{df},0,666,NIFTI_INTENT_LOG10PVAL,inttostr(666) );
	for lPos := 1 to lVolVoxels do
		if l32Buf^[lPos] > 0 then
			 l32Buf^[lPos] := -log(abs(pTdistr(42,l32Buf^[lPos])),10)
		else
			l32Buf^[lPos] :=0;
	SaveAsVOIorNIFTI(lBufferAligned,lVolVoxels,4,1,false,lMRIcroHdr.NiftiHdr,'log10p'+inttostr(666));
	//next - free float buffer
	FreeMem(lBufferUnaligned);
end;

procedure TImgForm.DesignVALClick(Sender: TObject);
begin
	//SpreadForm.Show;
end;

procedure TImgForm.Left1Click(Sender: TObject);
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
		5: lShift :=  -gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
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
{$IFDEF Darwin}
 FormClose(nil); //OSX does not send a FormClose Event if you choose the Application/Quit option
{$ENDIF}
        CloseShareMem;
end;

procedure TImgForm.YokeMenuClick(Sender: TObject);
begin
	(sender as TMenuItem).checked := not (sender as TMenuItem).checked;
	 gYoke := (sender as TMenuItem).checked ;
         if gYoke then
            CreateShareMem
         else
             CloseShareMem;

end;

procedure TImgForm.About1Click(Sender: TObject);
begin
        AboutForm.ThreadLabel.Caption := '  '+inttostr(gnCPUThreads)+' threads'+' '+ininame;
     AboutForm.Showmodal;
end;
procedure TImgForm.LayerDropChange(Sender: TObject);
begin
     {$IFDEF LCLgtk2}
     LayerDropSelect(nil);
     {$ENDIF}
end;

procedure TImgForm.LUTdropChange(Sender: TObject);
begin
     {$IFDEF LCLgtk2}
     LutDropSelect(nil);
     {$ENDIF}
end;

procedure TImgForm.AdjustimagessoVOIintensityiszero1Click(Sender: TObject);
begin
    BatchChangeInterceptSoVOIEqualsZero;
end;

procedure TImgForm.MirrorNII1Click(Sender: TObject);
var
	lNumberofFiles,lC: integer;
	lFilename: string;
begin
        Showmessage('WARNING: This will flip the images in the Left-Right dimension: this has serious consequences');
	CloseImagesClick(nil);
	if not OpenDialogExecute(kImgFilter,'Select NIfTI format images to convert',true) then exit;
	lNumberofFiles:= OpenHdrDlg.Files.Count;
	ProgressBar1.Min := 0;
	ProgressBar1.Max :=lNumberofFiles;
	ProgressBar1.Position := 0;
	for lC:= 1 to lNumberofFiles do begin
		lFilename := OpenHdrDlg.Files[lC-1];
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

procedure TImgForm.ZoomDropChange(Sender: TObject);
begin
     {$IFDEF LCLgtk2}
     ZoomDropSelect(nil);
     {$ENDIF}
end;



procedure TImgForm.ResizeControlPanel (lRows: integer);
begin
 // exit;
  if lRows = 2 then begin
    ControlPanel.Tag := 2;
    ControlPanel.Height := (2 * LayerPanel.Height)+4;
    ViewPanel.Left := 0;
    ToolPanel.Left := ViewPanel.Width + 2;
    ViewPanel.Top := LayerPanel.Height+2;
    ToolPanel.Top := LayerPanel.Height+2;
    (*LayerPanel.Top := LayerPanel.Height+2;
    LayerPanel.Left := 1;
    HideROIBtn.Left := ZoomDrop.Left+ZoomDrop.Width+2;
    XBarBtn.Left := HideROIBtn.Left + HideROIBtn.Width + 2;
    ToolPanel.Left := XBarBtn.Left+XBarBtn.Width+2;()

    (*LayerPanel.Top := ScaleX(36,96);
    LayerPanel.Left := 1;
    ControlPanel.Height := ScaleY(72,96);
    HideROIBtnXX.left := ScaleX(307,96);
    XBarBtnXX.Left := ScaleX(307+29, 96);
    ToolPanel.Left := ScaleX(307+61,96); *)
  end else begin
    ControlPanel.Tag := 1;
    ControlPanel.Height := LayerPanel.Height+2;
    ViewPanel.Left := LayerPanel.Left+LayerPanel.Width+2;
    ToolPanel.Left := LayerPanel.Left+LayerPanel.Width+2 + ViewPanel.Width + 2;
    ViewPanel.Top := 0;
    ToolPanel.Top := 0;
    (*LayerPanel.Top := 2;
    LayerPanel.Left := ZoomDrop.Left+ZoomDrop.Width+2;
    HideROIBtn.Left := LayerPanel.Left+LayerPanel.Width+2;
    XBarBtn.Left := HideROIBtn.Left + HideROIBtn.Width + 2;
    ToolPanel.Left := XBarBtn.Left+XBarBtn.Width+2;  *)
    (*LayerPanel.Top := 1;
    LayerPanel.Left := ScaleX(307,96);
    HideROIBtnXX.left := ScaleX(809,96);
    XBarBtnXX.Left := ScaleX(809+29,96);
    ToolPanel.Left := ScaleX(809+61,96);
    ControlPanel.Height := ScaleY(40,96);  *)
  end;
end;

procedure TImgForm.ControlPanelDblClick(Sender: TObject);
begin
  if ControlPanel.Tag = 1 then
    ResizeControlPanel(2)
  else
    ResizeControlPanel(1);
  ImgForm.RefreshImagesTimer.enabled := true;
  (*if ControlPanel.Tag = 2 then begin
      ResizeControlPanel(1);
      ImgForm.Width := ScaleX(1025,96);
      ImgForm.Height :=  ScaleY(469,96);
  end else begin
    ResizeControlPanel(2);
      ImgForm.Width := ScaleX(524,96);
      ImgForm.Height :=  ScaleY(640,96);
   end;*)
end;

procedure TImgForm.DefaultControlPanel;
begin
  if gBGImg.SingleRow then begin
      ResizeControlPanel(1);
      ImgForm.Width := ScaleX(1025,96);
      ImgForm.Height :=  ScaleY(469,96);
  end else begin
    ResizeControlPanel(2);
      ImgForm.Width := ScaleX(524,96);
      ImgForm.Height :=  ScaleY(640,96);
   end;
end;

initialization

  for gMouseDownY := 0 to knMaxOverlay do
    gMRIcroOverlay[gMouseDownY].index := gMouseDownY; //RGB

end.
