unit yrender;

interface
                                                                                                 
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons,nifti_img, nifti_hdr,define_types,nifti_img_view,
  StdCtrls, GraphicsMathLibrary, Menus,ClipBrd,ReadInt,cutout,IniFiles,FileCtrl,
  RenderThds, ComCtrls, RXSpin,render_composite;

type
   (*TRender =  record
	 CutoutFrac,Cutout: TCutout;
         Zoom: single;
	 cutoutLUTindex, ShadePct,BGNearClipFrac,OverlayNearClipFrac,BGNearClip,OverlayNearClip,Azimuth,Elevation,
	 OverlayFromBGSurface,BGSurface,OverlaySurface,BGDepth,OverlayDepth,CutoutBias: integer;
	 SmoothBG,SmoothOverlay,Trilinear,ShowCutout,FlipLR: boolean;
  end;*)

  TRenderForm = class(TForm)
	RenderBar: TPanel;
	MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    Close1: TMenuItem;
	Edit1: TMenuItem;
    Copy1: TMenuItem;
    Save1: TMenuItem;
    Label4: TLabel;
    Volume1: TMenuItem;
    RenderBGSurfaceMenu: TMenuItem;
    N1: TMenuItem;
	N101: TMenuItem;
    N401: TMenuItem;
    N601: TMenuItem;
    N801: TMenuItem;
	N403: TMenuItem;
    N404: TMenuItem;
	N405: TMenuItem;
    RenderBGDepthMenu: TMenuItem;
	N1voxel1: TMenuItem;
    N2voxels1: TMenuItem;
    N4voxels1: TMenuItem;
    N8voxels1: TMenuItem;
    N16voxels1: TMenuItem;
    N16voxels: TMenuItem;
    RenderSmoothBG: TMenuItem;
	RenderPreciseInterpolation: TMenuItem;
    Label1: TLabel;
    Overlay1: TMenuItem;
    RenderOverlaySurfaceMenu: TMenuItem;
    N701: TMenuItem;
    N602: TMenuItem;
	N501: TMenuItem;
    N402: TMenuItem;
	N301: TMenuItem;
    N201: TMenuItem;
	N102: TMenuItem;
    N01: TMenuItem;
    RenderOverlayDepthMenu: TMenuItem;
    N16voxels2: TMenuItem;
	N12voxels1: TMenuItem;
    N8voxels2: TMenuItem;
    N4voxels2: TMenuItem;
    N2voxels2: TMenuItem;
    N1voxel2: TMenuItem;
    Quality1: TMenuItem;
    RenderRefreshTimer: TTimer;
    RenderPanel: TScrollBox;
    RenderImage: TImage;
    RenderImageBup: TImage;
	Cutout1: TMenuItem;
    RenderSmoothOverlay: TMenuItem;
    FlipLRcheck: TMenuItem;
    Settings1: TMenuItem;
    Savesettings1: TMenuItem;
    N2: TMenuItem;
    Infinite1: TMenuItem;
    Infinite2: TMenuItem;
    Search1: TMenuItem;
    BehindBG1: TMenuItem;
    Infront1: TMenuItem;
    Anydepth1: TMenuItem;
    MIP1: TMenuItem;
    Saveas36bitmaps1: TMenuItem;
    BiasTrack: TTrackBar;
    GainTrack: TTrackBar;
    AzimuthEdit: TRxSpinEdit;
    ElevationEdit: TRxSpinEdit;
    QualityBtn: TSpeedButton;
    ShadeEdit: TRxSpinEdit;
    Label2: TLabel;
    N3: TMenuItem;
	procedure Save1Click(Sender: TObject);
    procedure RenderImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Copy1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure N1Click(Sender: TObject);
	procedure N01Click(Sender: TObject);
    procedure N1voxel1Click(Sender: TObject);
    procedure N16voxels2Click(Sender: TObject);
    procedure RenderSmoothClick(Sender: TObject);
    procedure RenderPreciseInterpolationClick(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure RenderRefreshTimerTimer(Sender: TObject);
	procedure EditChange(Sender: TObject);
	procedure OverlayRenderDepthItem(Sender: TObject);
	procedure RenderImageMouseDown(Sender: TObject; Button: TMouseButton;
	  Shift: TShiftState; X, Y: Integer);
	procedure Cutout1Click(Sender: TObject);
	procedure Savesettings1Click(Sender: TObject);
	procedure UpdateRenderMRU;
	procedure OpenRenderMRU(Sender:TObject);
	procedure UpdateRenderDisplay;
	procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CapBtnMenu1Click(Sender: TObject);
    procedure SetSearch(Sender: TObject);
procedure RefreshRotation;
         //procedure VolumeRotateMatrixX (var lBGImg: TBGImg; var lHdr: TMRIcroHdr; var lMatrixIn: TMatrix; lBilinearSmooth,lRenderCutout,lIsBG: boolean;lNearSlicesClipInFrac: integer);
    procedure Saveas36bitmaps1Click(Sender: TObject);
    procedure BiasTrackChange(Sender: TObject);
    procedure QualityBtnClick(Sender: TObject);
    procedure Generateoversampledrenderingslow1Click(Sender: TObject);

  private

         //    ThreadsRunning: Integer;
         //procedure ThreadDone(Sender: TObject);
	{ Private declarations }
  public
     //procedure SliceToFrac;	{ Public declarations }
  end;
var
  RenderForm: TRenderForm;

  gRenderDir,gRenderStartupFilename,gRenderDefaultsFilename:string;
  (*CritSect : TRTLCriticalSection;
  gRender:TRender;
  gZoom : single = 1;*)
implementation

uses MultiSlice,Math {power};
const
	//kAnywhere = 0;
	kBelow = 1;
	kInFront = 2;
        gInc: integer = 0;

{x$R *.DFM}

procedure LUTbiasX (var lOutLUT : TLUT; lBiasIn: integer {0..9});
{http://dept-info.labri.fr/~schlick/DOC/gem2.html
http://dept-info.labri.fr/~schlick/publi.html
Fast Alternatives to Perlin's Bias and Gain Functions
Christophe Schlick Graphics Gems IV, p379-382, April 1994  }
var
	lIndex: integer;
	lA,lT,lBias: single;
	lLUT: TLUT;
begin
	if lBiasIn = 4  then exit;
	lA := (lBiasIn+1)/10;
        if lA = 0 then
           lA := 0.000001;
	 for lIndex := 1 to 254 do begin
		 lT := lIndex/255;
		 lBias := 255*(lt/((1/la-2)*(1-lt)+1)) ;
		 lLUT[lIndex] := lOutLUT[round(lBias)];
	 end;
	 for lIndex := 1 to 254 do
		lOutLUT[lIndex] := lLUT[lIndex];
end;

procedure LUTgainX (var lOutLUT : TLUT; lBiasIn,lGainIn: integer {0..99});
{http://dept-info.labri.fr/~schlick/DOC/gem2.html
http://dept-info.labri.fr/~schlick/publi.html
Fast Alternatives to Perlin's Bias and Gain Functions
Christophe Schlick Graphics Gems IV, p379-382, April 1994  }
var
	lIndex,lV: integer;
	lA,lG,lT,lGain: single;
	lLUT: TLUT;
begin
	if (lGainIn = 50) and (lBiasIn = 50) then exit;
	lA := (lBiasIn)/100;
        if lA = 0 then
           lA := 0.000001;
	lG := (lGainIn)/100;
        if lG = 0 then
           lG := 0.00001;
        if lG = 1 then
           lG := 0.99999;
	 for lIndex := 1 to 254 do begin
		 lT := lIndex/255;
                 //apply bias
		 lT := (lt/((1/la-2)*(1-lt)+1)) ;
                 //next apply gain
                 if lT < 0.5 then
                      lGain := (lT/((1/lG-2)*(1-2*lT)+1))
                 else
                     lGain := (( (1/lG-2)*(1-2*lT)-lT ) / ( (1/lG-2)*(1-2*lT)-1 ) );
                 lGain := lGain / lT;
                 lV := round(255*lT*lGain);
                 if lV > 255 then
                    lV := 255;
                 if lV < 0 then
                    lV := 0;
		 //lBias := 255*(lt/((1/la-2)*(1-lt)+1)) ;
		 lLUT[lIndex] := lOutLUT[lV];
	 end;
	 for lIndex := 1 to 254 do
		lOutLUT[lIndex] := lLUT[lIndex];
end;


(*procedure TRenderForm.ThreadDone(Sender: TObject);
begin
              EnterCriticalSection(CritSect);
Dec(ThreadsRunning);
              LeaveCriticalSection(CritSect);
end;  *)

procedure TRenderForm.UpdateRenderDisplay;
begin
	SetSubmenuWithTag(RenderBGSurfaceMenu,gRender.BGSurface);
	SetSubmenuWithTag(RenderOverlaySurfaceMenu,gRender.OverlaySurface);
	SetSubmenuWithTag(RenderBGDepthMenu,gRender.BGDepth);
	SetSubmenuWithTag(RenderOverlayDepthMenu,gRender.OverlayDepth);
	RenderSmoothBG.checked := gRender.SmoothBG;
	RenderSmoothOverlay.checked := gRender.SmoothOverlay;
	RenderPreciseInterpolation.Checked := gRender.Trilinear;
	//RenderSurfaceOverlay.Checked := gRender.OverlayFromBGSurface;
	SetSubmenuWithTag(Search1,gRender.OverlayFromBGSurface);
	FlipLRCheck.Checked := gRender.FlipLR;
	AzimuthEdit.value := gRender.Azimuth;
	ElevationEdit.value := gRender.Elevation;
        ShadeEdit.value := gRender.ShadePct;
	RenderRefreshTimer.tag := -1;
	RenderRefreshTimer.enabled := true;
end;

procedure WriteRenderIniFile (lFilename: string);
var
  lIniFile: TIniFile;
  lInc: integer;
begin
  if DiskFreeEx(lFilename) < 1 then
	exit;
  if not DirectoryExists(extractfiledir(lFilename)) then begin
		mkDir(extractfiledir(lFilename));
  end;
  lIniFile := TIniFile.Create(lFilename);
  with gRender do begin
	//Booleans
	//SmoothBG,SmoothOverlay,Trilinear,OverlayFromBGSurface,ShowCutout

	lIniFile.WriteString('BOOL', 'SmoothBG',Bool2Char( SmoothBG));
	lIniFile.WriteString('BOOL', 'SmoothOverlay',Bool2Char( SmoothOverlay));
	lIniFile.WriteString('BOOL', 'Trilinear',Bool2Char( Trilinear));
	lIniFile.WriteString('BOOL', 'ShowCutout',Bool2Char( ShowCutout));
	lIniFile.WriteString('BOOL', 'FlipLR',Bool2Char( FlipLR));
	//Integers
	//BGNearClip,OverlayNearClip,Azimuth,Elevation,
	//BGSurface,OverlaySurface,BGDepth,OverlayDepth: integer;
	lIniFile.WriteString('INT', 'OverlayFromBGSurface',IntToStr( OverlayFromBGSurface));
	lIniFile.WriteString('INT', 'BGNearClipFrac',IntToStr(BGNearClipFrac));
	lIniFile.WriteString('INT', 'OverlayNearClipFrac',IntToStr(OverlayNearClipFrac));
	lIniFile.WriteString('INT', 'Azimuth',IntToStr(Azimuth));
	lIniFile.WriteString('INT', 'Elevation',IntToStr(Elevation));
	lIniFile.WriteString('INT', 'BGSurface',IntToStr(BGSurface));
	lIniFile.WriteString('INT', 'OverlaySurface',IntToStr(OverlaySurface));
	lIniFile.WriteString('INT', 'BGDepth',IntToStr(BGDepth));
	lIniFile.WriteString('INT', 'OverlayDepth',IntToStr(OverlayDepth));
	lIniFile.WriteString('INT', 'CutoutBias',IntToStr(CutoutBias));
	lIniFile.WriteString('INT', 'ShadePct',IntToStr(ShadePct));
	lIniFile.WriteString('INT', 'cutoutLUTindex',IntToStr(cutoutLUTindex));
	for lInc := 1 to 3 do begin
		lIniFile.WriteString('INT', 'CutoutLoFrac'+inttostr(lInc),IntToStr(CutoutFrac.Lo[lInc]));
		lIniFile.WriteString('INT', 'CutoutHiFrac'+inttostr(lInc),IntToStr(CutoutFrac.Hi[lInc]));
	end;
  end;//with gRender
  lIniFile.Free;
end;

procedure ReadRenderIniFile (lFilename: string);
var
  lStr: string;
  lIniFile: TIniFile;
  lInc: integer;
begin
	if not FileexistsEx(lFilename) then begin
		exit;
	end;
  lIniFile := TIniFile.Create(lFilename);
  lStr := lIniFile.ReadString('STR', 'Slices', '10,20,30');//file0 - last file viewed
  with gRender do begin
	//Booleans
	//SmoothBG,SmoothOverlay,Trilinear,OverlayFromBGSurface,ShowCutout
	SmoothBG := IniBool(lIniFile,'SmoothBG',SmoothBG);
	SmoothOverlay := IniBool(lIniFile,'SmoothOverlay',SmoothOverlay);
	Trilinear := IniBool(lIniFile,'Trilinear',Trilinear);
	//OverlayFromBGSurface := IniBool(lIniFile,'OverlayFromBGSurface',OverlayFromBGSurface);
	ShowCutout := IniBool(lIniFile,'ShowCutout',ShowCutout);
	FlipLR := IniBool(lIniFile,'FlipLR',FlipLR);
	//lIniFile.WriteString('BOOL', 'FlipLR',Bool2Char( FlipLR));
	//Integers
	//BGNearClip,OverlayNearClip,Azimuth,Elevation,
	//BGSurface,OverlaySurface,BGDepth,OverlayDepth: integer;
	OverlayFromBGSurface:= IniInt(lIniFile,'OverlayFromBGSurface',OverlayFromBGSurface);
	BGNearClip:= IniInt(lIniFile,'BGNearClip',0);
	OverlayNearClip:= IniInt(lIniFile,'OverlayNearClip',0);
	BGNearClipFrac:= IniInt(lIniFile,'BGNearClipFrac',-1);
	OverlayNearClipFrac:= IniInt(lIniFile,'OverlayNearClipFrac',-1);
	Azimuth:= IniInt(lIniFile,'Azimuth',Azimuth);
	Elevation:= IniInt(lIniFile,'Elevation',Elevation);
	BGSurface:= IniInt(lIniFile,'BGSurface',BGSurface);
	OverlaySurface:= IniInt(lIniFile,'OverlaySurface',OverlaySurface);
	BGDepth:= IniInt(lIniFile,'BGDepth',BGDepth);
        if BGDepth > 32000 then
           BGDepth := 32000;
	OverlayDepth:= IniInt(lIniFile,'OverlayDepth',OverlayDepth);
        if OverlayDepth > 32000 then
           OverlayDepth := 32000;
	CutoutBias:= IniInt(lIniFile,'CutoutBias',	CutoutBias);
	ShadePct:= IniInt(lIniFile,'ShadePct',	0);
	cutoutLUTindex:= IniInt(lIniFile,'cutoutLUTindex',cutoutLUTindex);
	for lInc := 1 to 3 do begin
	  Cutout.Lo[lInc] := IniInt(lIniFile,'CutoutLo'+inttostr(lInc),Cutout.Lo[lInc]);
	  Cutout.Hi[lInc] := IniInt(lIniFile,'CutoutHi'+inttostr(lInc),Cutout.Hi[lInc]);
	end;

	for lInc := 1 to 3 do begin
	  CutoutFrac.Lo[lInc] := IniInt(lIniFile,'CutoutLoFrac'+inttostr(lInc),-1);
	  CutoutFrac.Hi[lInc] := IniInt(lIniFile,'CutoutHiFrac'+inttostr(lInc),-1);
	end;
  end;//with gRender
  lIniFile.Free;
end;

procedure TRenderForm.OpenRenderMRU(Sender:TObject);
var
	lFilename: string;
begin
   lFilename := gRenderDir+(Sender as TMenuItem).caption+'.ini' ;
   ReadRenderIniFile(lFilename);
   CutoutForm.Prep;
   UpdateRenderDisplay;
end;

procedure TRenderForm.UpdateRenderMRU;
var
	NewItem: TMenuItem;
	lSearchRec: TSearchRec;
begin
  While Settings1.Count > 0 do Settings1.Items[0].Free;
  if FindFirst(gRenderDir+'*.ini', faAnyFile, lSearchRec) = 0 then
	 repeat
		   NewItem := TMenuItem.Create(Self);
		   NewItem.Caption := ParseFileName(ExtractFileName(lSearchRec.Name));
		   NewItem.Onclick := OpenRenderMRU;
		   Settings1.Add(NewItem);
		until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
end;

Function AziElevMatrix: TMatrix;var	lLRFlipMatrix: TMatrix;begin
	  gRender.Azimuth := RenderForm.AzimuthEdit.asInteger;
  gRender.Elevation := RenderForm.ElevationEdit.asInteger;
	result := ViewTransformMatrix(
		   coordSpherical,
		   ToRadians(RenderForm.AzimuthEdit.Value),
		   ToRadians(RenderForm.ElevationEdit.Value),
		   3{Distance.Value},6{ScreenWidthHeight.Value},6{ScreenWidthHeight.Value},{ScreenToCamera.Value}3);
 {The ViewTransformMatrix is all that is needed for other objects defined in world coordinates.}
	if  gRender.FlipLR then begin
             //   Mx(result);
	    	lLRFlipMatrix := Matrix3D (-1,0,0,0,      // 3D "graphics" matrix
                                          0,1,0,0,
                                          0,0,1,0,
                                          0,0,0,1);

	result := MultiplyMatrices(lLRFlipMatrix,Result);
  end;
end;
procedure InvertMatrixPoint (var lBackgroundImg: TBGImg; var lInMatrix: TMatrix; var lXin,lYin,lZIn, lXout,lYout,lZout: integer);//convert mouse click to position
var
 lZ,lY,lX,lOutDim,lOutPivot,lXPivotIn,lYPivotIn,lZPivotIn: integer;
 lMatrix: TMatrix;
begin
  //lOutDim := gBGImg.RenderDim;//MaxDim(lBackgroundImg.ScrnDim[1],lBackgroundImg.ScrnDim[2],lBackgroundImg.ScrnDim[3]);
  if gRender.Zoom > 0 then
     lOutDim := round(gBGImg.RenderDim/gRender.Zoom)
  else
      lOutDim :=gBGImg.RenderDim;  //11/2007b
  lOutPivot := (lOutDim+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lXPivotIn := (lBackgroundImg.ScrnDim[1]+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lYPivotIn := (lBackgroundImg.ScrnDim[2]+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lZPivotIn := (lBackgroundImg.ScrnDim[3]+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lX := (lXin-lOutPivot);
  lY := ({lYin-}lOutPivot-lYin);
  lZ := (lZin-lOutPivot);
  lMatrix :=  InvertMatrix3D(lInMatrix);
  lXout := round( (lX*lMatrix.matrix[1,1])+(lY * lMatrix.matrix[2,1])+(lZ*lMatrix.matrix[3,1]));
  lYout := round( (lX*(lMatrix.matrix[1,2]))+(lY * lMatrix.matrix[2,2])+(lZ*lMatrix.matrix[3,2]));
  lZout := round( (lX*(lMatrix.matrix[1,3]))+(lY * lMatrix.matrix[2,3])+(lZ*lMatrix.matrix[3,3]));
  lXOut := (lXOut+lXPivotIn);
  lYOut := (lYOut+lYPivotIn);
  lZOut := (lZOut+lZPivotIn);
end;
procedure TRenderForm.Save1Click(Sender: TObject);
begin
	 //if (RenderImage.Picture.Graphic = nil) then begin
	 SaveImgAsPNGBMP (RenderImage);
end;

procedure TRenderForm.RenderImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
	 if ImgForm.MagnifyImage.Width > 10 then
		ImgForm.MagnifyTimer.Enabled := true;//MagnifyBtn.Down;
end;

procedure TRenderForm.Copy1Click(Sender: TObject);
var
  MyFormat : Word;
  AData: THandle;
  APalette : HPalette;   //For later versions of Delphi: APalette : THandle;
begin
	 if (RenderImage.Picture.Graphic = nil) then begin //1420z
        Showmessage('You need to load an image before you can copy it to the clipboard.');
        exit;
     end;
     RenderImage.Picture.Bitmap.SaveToClipBoardFormat(MyFormat,AData,APalette);
	 ClipBoard.SetAsHandle(MyFormat,AData);
end;

procedure TRenderForm.Close1Click(Sender: TObject);
begin
	RenderForm.Close;
end;

procedure TRenderForm.N1Click(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gRender.BGSurface := (sender as TMenuItem).tag;
	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.N01Click(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gRender.OverlaySurface := (sender as TMenuItem).tag;
	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.N1voxel1Click(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gRender.BGDepth := (sender as TMenuItem).tag;
	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.N16voxels2Click(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gRender.OverlayDepth := (sender as TMenuItem).tag;
	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.RenderSmoothClick(Sender: TObject);
begin
	(sender as TMenuItem).checked := not (sender as TMenuItem).checked;
	gRender.FlipLR := FlipLRCheck.Checked;
	//RenderSmoothSurface.checked := not RenderSmoothSurface.Checked;
	gRender.SmoothBG := RenderSmoothBG.checked;
	gRender.SmoothOverlay := RenderSmoothOverlay.checked;
	RenderRefreshTimer.Tag := -1;//force a new rotation matrix to be generated
	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.RenderPreciseInterpolationClick(Sender: TObject);
begin
	RenderPreciseInterpolation.Checked := not RenderPreciseInterpolation.Checked;
	gRender.Trilinear := RenderPreciseInterpolation.Checked;
	RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.FormShow(Sender: TObject);
var
	lInc: integer;
begin
     gRender.Bias := 50;
     gRender.Gain := 50;
        gRender.cutoutLUTindex := 0;
	gRender.BGSurface := 51;
	gRender.OverlaySurface := 1;
	gRender.BGDepth := 12;
	gRender.OverlayDepth := 8;
	gRender.Azimuth := 90;
	gRender.Elevation := 45;
        gRender.ShadePct := 0;
	gRender.OverlayNearClip := 0;
	gRender.BGNearClip := 0;
	gRender.OverlayNearClipFrac := -1;
	gRender.BGNearClipFrac := -1;
	gRender.SmoothBG	:= true;
	gRender.SmoothOverlay	:= false;
	gRender.Trilinear	:= true;
	gRender.FlipLR := false;
	gRender.OverlayFromBGSurface	:= kBelow;
	gRender.ShowCutout := false;//10/10/2006
	gRender.CutoutBias := 4;
	for lInc := 1 to 3 do begin
		gRender.CutoutFrac.Lo[lInc] := kMaxFrac div 2;
		gRender.CutoutFrac.Hi[lInc] := kMaxFrac;
	end;
	ReadRenderIniFile (gRenderStartupFilename);
	UpdateRenderMRU;
	UpdateRenderDisplay;
end;

procedure ClipFracCheck (var lFrac,lSlice: integer);
//provide backward compatibility for files that explicitly report slices not fraction
var
   lMax: integer;
begin
    if lFrac >= 0 then
       exit;
    lFrac := 0;
    lMax := MaxDim(gBGImg.ScrnDim[1],gBGImg.ScrnDim[2],gBGImg.ScrnDim[3]);
    if (lSlice <= 0) or (lSlice > lMax) then
       exit;
    lFrac := round(lSlice/lMax*kMaxFrac);

end;

procedure TRenderForm.RefreshRotation;
var
	lC: integer;
	lMatrix: TMatrix;
        lStartTime: DWord;
begin
  lMatrix := AziElevMatrix;        Application.processmessages;  gRender.Zoom := gZoom; //11/2007b  gZoom := 1;  lStartTime := GetTickCount;  ClipFracCheck (gRender.BGNearClipFrac,gRender.BGNearClip);  ClipFracCheck (gRender.OverlayNearClipFrac,gRender.OverlayNearClip);	  gRender.Azimuth := round(AzimuthEdit.value);  gRender.Elevation := round(ElevationEdit.value);  VolumeRotateMatrix (gBGImg, gMRIcroOverlay[0],lMatrix, gRender.Trilinear,gRender.ShowCutout,true,gRender.BGNearClipFrac);  if RenderRefreshTimer.Enabled  then exit;  Refresh;  for lC := 1 to knMaxOverlay do begin	VolumeRotateMatrix (gBGImg, gMRIcroOverlay[lC],lMatrix, gRender.Trilinear,false,false,gRender.OverlayNearClipFrac);        if RenderRefreshTimer.Enabled  then exit;  end;  ImgForm.StatusLabel.caption :=('update(ms): '+inttostr(GetTickCount-lStartTime));end;
procedure TRenderForm.RenderRefreshTimerTimer(Sender: TObject);
begin
	RenderRefreshTimer.Enabled := false;
	if gMRIcroOverlay[0].ScrnBufferItems=0 then begin
		RenderImage.Width := 0;		exit;	end;
        gRender.ShadePct := ShadeEdit.asInteger;
	if (gMRIcroOverlay[0].RenderBufferItems=0) or (RenderRefreshTimer.Tag <> 0) or (AzimuthEdit.value<>gRender.Azimuth) or (ElevationEdit.value<>gRender.Elevation) then
		RefreshRotation;
        if RenderRefreshTimer.Enabled  then exit;
        //gZoom := 1;
	RenderRefreshTimer.Tag := 0;
	DrawRender;
end;

procedure TRenderForm.EditChange(Sender: TObject);
begin
   RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.OverlayRenderDepthItem(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gRender.OverlayDepth := (sender as TMenuItem).tag;
	RenderRefreshTimer.Enabled := true;
end;

procedure RenderDrawXBar ( lHorPos, lVerPos: integer;var lImage: TImage);
var lL,lT,lW,lH,lZoomPct: integer;
begin
	 lImage.Picture.Graphic := RenderForm.RenderImageBup.Picture.Graphic;
	 lZoomPct := 100; //ImageZoomPct(lImage);
	 lL := (lHorPos * lZoomPct) div 100;
	 lT := (lVerPos * lZoomPct) div 100;
	 lW := lImage.Width;// div 100;
	 lH := lImage.Height;// div 100;
	 lImage.Canvas.Pen.Color:=gBGImg.XBarClr;
	 lImage.Canvas.Pen.Width := gBGImg.XBarThick;
	 //next horizontal lines
	 lImage.Canvas.MoveTo(0,lT);
	 lImage.Canvas.LineTo(lL-gBGImg.XBarGap,lT);
	 lImage.Canvas.MoveTo(lL+gBGImg.XBarGap,lT);
	 lImage.Canvas.LineTo(lW,lT);
	 //next vertical lines
	 lImage.Canvas.MoveTo(lL,0);
	 lImage.Canvas.LineTo(lL,lT-gBGImg.XBarGap);
	 lImage.Canvas.MoveTo(lL,lT+gBGImg.XBarGap);
	 lImage.Canvas.LineTo(lL,lH);
end; //Proc RenderDrawXBar

procedure TRenderForm.RenderImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var lXrender,lYrender,lZrender,lXout,lYout,lZOut,lPixelOffset,lZoom: integer;
lMatrix: TMatrix;
begin
	if ImgForm.XBarBtn.Down then
		RenderDrawXBar ( X,Y,RenderImage);
	//Next: find coordinates for orthogonal views:
	lZoom := ImageZoomPct(RenderImage);
	lXrender := round((X*100) / lZoom );
	lYrender := round(((Y)*100) / lZoom );

	lPixelOffset := lXrender+ ((gBGImg.RenderDim-lYrender)*gBGImg.RenderDim);
	//ImgForm.StatusLabel.caption := inttostr(lXrender)+'x'+inttostr(lYrender)+' ->  '+inttostr(gMRIcroOverlay[kBGOverlayNum].RenderDepthBufferItems );
	if (lPixelOffset < 1) or (lPixelOffset >gBGImg.RenderDepthBufferItems ) then exit;
	lZrender := gBGImg.RenderDepthBuffer[lPixelOffset];
        lXrender := round(lXrender / gRender.Zoom);
        lYrender := round(lYrender / gRender.Zoom);
        lZrender := round(lZrender / gRender.Zoom);
        //caption := inttostr(lXrender)+'x'+inttostr(lYrender)+'x'+inttostr(LZrender)+'  '+inttostr(gBGImg.RenderDepthBuffer[lPixelOffset]);
	lMatrix := AziElevMatrix;
	InvertMatrixPoint (gBGImg,lMatrix,lXrender,lYrender,lZrender, lXout,lYout,lZOut);
	ImgForm.XViewEdit.value := lXOut;
	ImgForm.YViewEdit.asInteger := lYOut;
	ImgForm.ZViewEdit.asInteger := lZOut;
end;

procedure TRenderForm.Cutout1Click(Sender: TObject);
begin
	CutoutForm.Show;
end;

procedure TRenderForm.Savesettings1Click(Sender: TObject);
begin
  MultiSliceForm.MultiSaveDialog.InitialDir := extractfiledir(gRenderDir);
  MultiSliceForm.MultiSaveDialog.FileName := 'a'+inttostr(gRender.Azimuth)+'e'+inttostr(gRender.Elevation);
  if not MultiSliceForm.MultiSaveDialog.Execute then exit;
  WriteRenderIniFile(MultiSliceForm.MultiSaveDialog.Filename);
  UpdateRenderMRU;
end;

procedure TRenderForm.FormHide(Sender: TObject);
begin
	WriteRenderIniFile (gRenderDefaultsFilename);
end;

procedure TRenderForm.FormCreate(Sender: TObject);
begin
//ThreadsRunning := 0;
	gRenderDir := extractfiledir(paramstr(0))+'\render\';
	gRenderDefaultsFilename := gRenderDir + 'default.ini';
	gRenderStartupFilename := gRenderDefaultsFilename;
end;

procedure TRenderForm.CapBtnMenu1Click(Sender: TObject);
begin
	RenderForm.RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
	RenderForm.RenderRefreshTimer.enabled := true;
end;

procedure TRenderForm.SetSearch(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gRender.OverlayFromBGSurface := (sender as TMenuItem).tag;
	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.Saveas36bitmaps1Click(Sender: TObject);
var
   lnViews,lC,lAngle,lStartA: integer;
   lAzi,lZoom: boolean;
   lBaseFilename,lFilename: string;
begin
     lnViews:= ReadIntForm.GetInt('How many bitmaps for a 360-degree rotation?', 4,24,72);

     ImgForm.SaveDialog1.Filter := 'PNG bitmap|*.png';
     ImgForm.SaveDialog1.DefaultExt := '*.png';
     if not ImgForm.SaveDialog1.Execute then exit;
     lBaseFilename := ImgForm.SaveDialog1.Filename;
     lAzi := false;
     lZoom := false;//11/2007b
	case MessageDlg('Rotate azimuth?', mtConfirmation,
		[mbYes, mbNo], 0) of
		id_Yes: lAzi := true;
	end; //case
	case MessageDlg('Generate super-sampled (high quality) renderings?', mtConfirmation,
		[mbYes, mbNo], 0) of
		id_Yes: lZoom := true;
	end; //case

     if lAzi then
        lStartA := AzimuthEdit.asInteger
     else
         lStartA := ElevationEdit.asInteger;
    for lC := 1 to lnViews do begin
        lAngle := round((lC-1) * (360/lnviews));
        if lAzi then
           AzimuthEdit.value := lAngle
        else
            ElevationEdit.value := lAngle - 180;
        RenderRefreshTimer.enabled := false;
        if lZoom then
           gZoom := 2;
        RefreshRotation;
        DrawRender;
        lFilename :=  ChangeFilePostfixExt (lBaseFilename,PadStr(lAngle,3),'.png');
        SaveImgAsPNGBMPCore(RenderImage,lFilename);
    end; //for each of 36 views
    if lAzi then
       AzimuthEdit.value := lStartA
    else
        ElevationEdit.value := lStartA;
end;

procedure TRenderForm.BiasTrackChange(Sender: TObject);
begin
     gRender.Bias := BiasTrack.position;
     gRender.Gain := GainTrack.Position;
	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.QualityBtnClick(Sender: TObject);
begin
     gZoom := 2;
	RenderRefreshTimer.Tag := -1;//force a new rotation matrix to be generated
	RenderRefreshTimer.Enabled := true;

end;

procedure TRenderForm.Generateoversampledrenderingslow1Click(
  Sender: TObject);
begin
     gZoom := 2;
	RenderRefreshTimer.Tag := -1;//force a new rotation matrix to be generated
	RenderRefreshTimer.Enabled := true;
end;

end.
