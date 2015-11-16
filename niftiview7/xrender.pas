unit xrender;

interface
                                                                                                 
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons,nifti_img, nifti_hdr,define_types,nifti_img_view,
  StdCtrls, GraphicsMathLibrary, Menus,ClipBrd,ReadInt,cutout,IniFiles,FileCtrl,
  RenderThds, ComCtrls, RXSpin;

type
   TRender =  record
	 CutoutFrac,Cutout: TCutout;
         Zoom: single;
	 cutoutLUTindex, ShadePct,BGNearClipFrac,OverlayNearClipFrac,BGNearClip,OverlayNearClip,Azimuth,Elevation,
	 OverlayFromBGSurface,BGSurface,OverlaySurface,BGDepth,OverlayDepth,CutoutBias: integer;
	 SmoothBG,SmoothOverlay,Trilinear,ShowCutout,FlipLR: boolean;
  end;

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
         procedure VolumeRotateMatrix (var lBGImg: TBGImg; var lHdr: TMRIcroHdr; var lMatrixIn: TMatrix; lBilinearSmooth,lRenderCutout,lIsBG: boolean;lNearSlicesClipInFrac: integer);
    procedure Saveas36bitmaps1Click(Sender: TObject);
    procedure BiasTrackChange(Sender: TObject);
    procedure QualityBtnClick(Sender: TObject);
    procedure Generateoversampledrenderingslow1Click(Sender: TObject);

  private

             ThreadsRunning: Integer;
         procedure ThreadDone(Sender: TObject);
	{ Private declarations }
  public
     procedure SliceToFrac;	{ Public declarations }
  end;
var
   CritSect : TRTLCriticalSection;
  RenderForm: TRenderForm;
  gRender:TRender;
  gRenderDir,gRenderStartupFilename,gRenderDefaultsFilename:string;
  gZoom : single = 1;
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


procedure TRenderForm.ThreadDone(Sender: TObject);
begin
              EnterCriticalSection(CritSect);
Dec(ThreadsRunning);
              LeaveCriticalSection(CritSect);
end;

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

procedure Smooth2DImage (lX,lY: integer; lInBuffer: ByteP);
var
	lSmoothBuffer: ByteP;
	lLine,lLineStart,lInc,lOutPixel,lV: integer;
begin	 GetMem (lSmoothBuffer ,  lX*lY);	 FillChar(lSmoothBuffer^,lX*lY, 0); //zero array
	  for lLine:= (lY-1) downto 2 do begin
		 lLineStart := ((lLine-1)*(lX));
		 for lInc := (lX-1) downto 2 do begin
			  lOutPixel := lLineStart+lInc;
				 lV := (lInBuffer[lOutPixel] shl 3)
				   +(lInBuffer[lOutPixel+1] shl 1)+(lInBuffer[lOutPixel-1] shl 1)
				   +(lInBuffer[lOutPixel+lX] shl 1)+(lInBuffer[lOutPixel-lX] shl 1)
				   +(lInBuffer[lOutPixel+lX+1])+(lInBuffer[lOutPixel+lX-1])
				   +(lInBuffer[lOutPixel-lX+1])+(lInBuffer[lOutPixel-lX-1])
				   ;
				 lV := lV div 20;
				 lSmoothBuffer[lOutPixel] := lV;//lV;
		 end; //for each column
	 end; //for each line (row)
	 Move(lSmoothBuffer[1],lInBuffer[1],lX*lY);
	 FreeMem(lSmoothBuffer);
end; //proc Smooth2DImage

(*function MinFilt (var lHdr: TMRIcroHdr): integer;
var lMin,lMax: single;
lFiltMin8bit, lFiltMax8bit: integer;
begin
ReturnMinMax (lHdr,lMin,lMax, lFiltMin8bit, lFiltMax8bit);
result := lFiltMin8bit;
end;*)
procedure MinMaxFilt (var lHdr: TMRIcroHdr; var lFiltMin8bit, lFiltMax8bit: integer);var lMin,lMax: single;
begin
ReturnMinMax (lHdr,lMin,lMax, lFiltMin8bit, lFiltMax8bit);
end;procedure CreateOverlayRenderBehind(var lBGHdr,lHdr: TMRIcroHdr; var lX,lY,lZ,lInRenderSurface,lInRenderDepth: Integer; var lQuadP: RGBQuadp; Smooth2D: boolean);var	lSrc,lOutBuffer: Bytep;        lLow,lHigh,	lIntensity,lDepth,lPixel,lSliceOffset,lSliceSz,lVolSz,lRenderSurface,lRenderDepth: integer;begin  if gBGImg.RenderDepthBufferItems < 1 then exit;  lSrc := lHdr.RenderBuffer;//lHdr.ScrnBuffer;  lSliceSz := lX*lY;  lVolSz := lSliceSz * lZ;  GetMem (lOutBuffer ,  lSliceSz);  fillchar(lOutBuffer^,lSliceSz,0);  lRenderDepth := lInRenderDepth;  if (lRenderDepth < 1) or (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then	lRenderDepth := 1;  lRenderSurface := lInRenderSurface;  if (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then	lRenderSurface := 1;  for lPixel := 1 to lSliceSz do begin	if gBGImg.RenderDepthBuffer[lPixel] <> 0 then begin //background surface at this voxel		lDepth := 0;		lIntensity := 0;		lSliceOffset := (abs(gBGImg.RenderDepthBuffer[lPixel])-1)*lSliceSz; //start with nearest slice		while (lDepth < lRenderDepth) and (lSliceOffset < lVolSz) do begin			if (lSrc[lSliceOffset+lPixel] > lRenderSurface) and (lSrc[lSliceOffset+lPixel] > lIntensity) then				lIntensity := lSrc[lSliceOffset+lPixel];			inc(lSliceOffset,lSliceSz);			inc(lDepth);			if gBGImg.RenderDepthBuffer[lPixel] < 0 then				lDepth := lRenderDepth; //only show surface for cutout		end;		lOutBuffer[lPixel]:= lIntensity;	end; //background surface at this voxel  end;  if (Smooth2D) and (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then //do not smooth labels	Smooth2DImage (lX,lY, lOutBuffer);//Mar2007 startif lHdr.LUTfromZero then begin  MinMaxFilt(lHdr,lLow,lHigh);  //fx(lLow,lHigh);  if lLow > 0 then    for lPixel := 1 to (lSliceSz) do      if lOutBuffer[lPixel] < lLow then         lOutBuffer[lPixel] := 0;  if lHigh < 255 then    for lPixel := 1 to (lSliceSz) do      if lOutBuffer[lPixel] < lHigh then         lOutBuffer[lPixel] := 0;    //xxxend;//Mar2007 end  for lPixel := 1 to lSliceSz do	lQuadP[lPixel]:= lHdr.LUT[lOutBuffer[lPixel]];  Freemem(lOutBuffer);end;procedure CreateOverlayRenderInfrontNear(var lBGHdr,lHdr: TMRIcroHdr; var lX,lY,lZ,lInRenderSurface,lInRenderDepth: Integer; var lQuadP: RGBQuadp; Smooth2D: boolean);//changes Aug2007 - make sure search depth is not MAxInt - we get wrap aroundvar	lSrc,lOutBuffer: Bytep;        lLow,lHigh,	lIntensity,lDepth,lPixel,lSliceOffset,lSliceSz,lVolSz,lRenderSurface,lRenderDepth,lSamples: integer;begin  if gBGImg.RenderDepthBufferItems < 1 then exit;  lSrc := lHdr.RenderBuffer;//lHdr.ScrnBuffer;  lSliceSz := lX*lY;  lVolSz := lSliceSz * lZ;  GetMem (lOutBuffer ,  lSliceSz);  fillchar(lOutBuffer^,lSliceSz,0);  //lRenderDepth := lInRenderDepth;  //if (lRenderDepth < 1) or (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then  //	lRenderDepth := 1;  lRenderSurface := lInRenderSurface;  if (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then	lRenderSurface := 1;  for lPixel := 1 to lSliceSz do begin	if gBGImg.RenderDepthBuffer[lPixel] <> 0 then begin //background surface at this voxel		lDepth := 0;		lIntensity := 0;                lSliceOffset := 0;                lSamples := 0;                lRenderDepth := (abs(gBGImg.RenderDepthBuffer[lPixel])-1)+lInRenderDepth;		//lSliceOffset := (abs(gBGImg.RenderDepthBuffer[lPixel])-1)*lSliceSz; //start with nearest slice		while (lDepth < lRenderDepth) and (lSliceOffset < lVolSz) do begin			if (lSrc[lSliceOffset+lPixel] > lRenderSurface)  then begin				lIntensity := lIntensity+lSrc[lSliceOffset+lPixel];                                inc(lSamples);                        end;			inc(lSliceOffset,lSliceSz);			inc(lDepth);			if gBGImg.RenderDepthBuffer[lPixel] < 0 then				lDepth := lRenderDepth; //only show surface for cutout		end;                if lSamples > 0 then			lOutBuffer[lPixel]:= lIntensity div lSamples;		//lOutBuffer[lPixel]:= lIntensity;	end ; //if background   end;  if (Smooth2D) and (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then //do not smooth labels	Smooth2DImage (lX,lY, lOutBuffer);//Mar2007 startif lHdr.LUTfromZero then begin  MinMaxFilt(lHdr,lLow,lHigh);  //fx(lLow,lHigh);  if lLow > 0 then    for lPixel := 1 to (lSliceSz) do      if lOutBuffer[lPixel] < lLow then         lOutBuffer[lPixel] := 0;  if lHigh < 255 then    for lPixel := 1 to (lSliceSz) do      if lOutBuffer[lPixel] < lHigh then         lOutBuffer[lPixel] := 0;    //xxxend;  for lPixel := 1 to lSliceSz do	lQuadP[lPixel]:= lHdr.LUT[lOutBuffer[lPixel]];  Freemem(lOutBuffer);end;(*procedure Mx (M: TMatrix);beginTextForm.Memo1.Lines.Add(floattostr(M.matrix[1,1])+'x'+floattostr(M.matrix[1,2])+'x'+floattostr(M.matrix[1,3])+'x'+floattostr(M.matrix[1,4]));TextForm.Memo1.Lines.Add(floattostr(M.matrix[2,1])+'x'+floattostr(M.matrix[2,2])+'x'+floattostr(M.matrix[2,3])+'x'+floattostr(M.matrix[2,4]));TextForm.Memo1.Lines.Add(floattostr(M.matrix[3,1])+'x'+floattostr(M.matrix[3,2])+'x'+floattostr(M.matrix[3,3])+'x'+floattostr(M.matrix[3,4]));TextForm.Memo1.Lines.Add(floattostr(M.matrix[4,1])+'x'+floattostr(M.matrix[4,2])+'x'+floattostr(M.matrix[4,3])+'x'+floattostr(M.matrix[4,4]));TextForm.Memo1.Lines.Add('-');end;*)Function AziElevMatrix: TMatrix;var	lLRFlipMatrix: TMatrix;begin
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
procedure ShadeCutoutCrease (var lRenderBuffer: bytep);var
lZ,lY,lX: single;
  lXin,lYin,lZIn,lXm,lYm,lZm,lPixel,
 lOutDim,lOutPivot,lXPivotIn,lYPivotIn,lZPivotIn,
 lXlo,lXhi,lYlo,lYhi,lZlo,lZhi,lYOffset: integer;
 lClose,lScale: single;
 lMatrix: TMatrix;
begin
  lOutDim := gBGImg.RenderDim;//MaxDim(lBackgroundImg.ScrnDim[1],lBackgroundImg.ScrnDim[2],lBackgroundImg.ScrnDim[3]);
  if gRender.Zoom > 0 then
     lOutPivot := (round(gBGImg.RenderDim/gRender.Zoom)+1) shr 1
  else
      lOutPivot :=(gBGImg.RenderDim+1) shr 1;  //11/2007b
  //lOutPivot := (gRender.UnscaledRenderDim+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  //lOutPivot := (lOutDim+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lXPivotIn := (gBGImg.ScrnDim[1]+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lYPivotIn := (gBGImg.ScrnDim[2]+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lZPivotIn := (gBGImg.ScrnDim[3]+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  lMatrix :=  InvertMatrix3D(AziElevMatrix);
	 //next: dilate borders by 1 pixel - draw crease INSIDE cutout
	 lXlo := gRender.CutOut.Lo[1]-1;
	 lXhi := gRender.CutOut.Hi[1]+1;
	 lYlo := gRender.CutOut.Lo[2]-1;
	 lYhi := gRender.CutOut.Hi[2]+1;
	 lZlo := gRender.CutOut.Lo[3]-1;
	 lZhi := gRender.CutOut.Hi[3]+1;
          lScale := 1/gRender.Zoom; //11/2007
  //renderform.caption := inttostr(gRender.UnscaledRenderDim)+' '+inttostr(gRender.Zoom);
  for lYin := 1 to lOutDim do begin
	lYOffset := ((gBGImg.RenderDim-lYin)*gBGImg.RenderDim);
	for lXin := 1 to lOutDim do begin
		lPixel := lXin+ lYOffset;
		if gBGImg.RenderDepthBuffer[lPixel]<0 then begin
			lZin := abs(gBGImg.RenderDepthBuffer[lPixel]);
			{lX := (lXin-lOutPivot);
			lY := (lOutPivot-(lYin));
			lZ := (lZin-lOutPivot);}
                        lX := (lXin *lScale)-lOutPivot ;
                        lY := lOutPivot -(lYin * lScale);
                        lZ := (lZin * lScale)-lOutPivot;
			lXm := round( (lX*lMatrix.matrix[1,1])+(lY * lMatrix.matrix[2,1])+(lZ*lMatrix.matrix[3,1]));
			lYm := round( (lX*(lMatrix.matrix[1,2]))+(lY * lMatrix.matrix[2,2])+(lZ*lMatrix.matrix[3,2]));
			lZm := round( (lX*(lMatrix.matrix[1,3]))+(lY * lMatrix.matrix[2,3])+(lZ*lMatrix.matrix[3,3]));
			lXm := (lXm+lXPivotIn);
			lYm := (lYm+lYPivotIn);
			lZm := (lZm+lZPivotIn);
			if abs(lXlo-lXm) < abs(lXhi-lXm)  then
				lXm := abs(lXlo-lXm)
			else
				lXm := abs(lXhi-lXm);
			if abs(lYlo-lYm) < abs(lYhi-lYm) then
				lYm := abs(lYlo-lYm)
			else
				lYm := abs(lYhi-lYm);
			if abs(lZlo-lZm) < abs(lZhi-lZm) then
				lZm := abs(lZlo-lZm)
			else
				lZm := abs(lZhi-lZm);
			if (lXm < lYm) and (lZm < lYm) then
				lYm := lZm //Y is furthest, replace with Z
			else if lZm < lXm then  //X is furthest, replace with Z
				lXm := lZm;
			lClose := sqrt((lXm*lXm) + (lYm*lYm));
			if  lClose < 8 then begin
				lClose := 1-sqr(1-(lClose/8));
				lRenderBuffer[lPixel] := round(lRenderBuffer[lPixel]*(0.33+(0.67*lClose)));
			end;
		end;
	end; //for lYin
  end; //for lXin
end;


function SmoothShading (lX,lY: integer;  lRenderDepthBuffer: SmallintP): boolean;
var
   kRenderInfiniteDepth,lPrevLineStart,lNextLineStart,lLineStart,lScanLines,
   lGap,lDepthSum,lWeightSum,lFar,lClose,lCenter,lInc,lXmG: integer;
   lRenderDepthBufferS: SmallIntP;
procedure AddPt (lI,lW: integer; var lSumI,lSumW: integer);
begin
    if lI = kRenderInfiniteDepth then exit;
    lSumI := lSumI + (lW*lI); //add scaled value
    lSumW := lSumW + lW;//add weight
end;
//problem - smoothing gives embossed look!
begin //func Smoothshading
  kRenderInfiniteDepth := 0;
  result := false;
  if (gRender.Zoom < 1) or (lY < 5) or (lX < 5) or (gBGImg.RenderDepthBufferItems <> (lX * lY)) then
     exit;
  lFar := 2;
  lClose := 3;
  lCenter := 5;
  lGap := trunc((gRender.Zoom-0.001)/1)+1; //must be at least 1!
  lXmG := lX-lGap;
  Getmem(lRenderDepthBufferS,lX*lY*sizeof(smallint));
  for lInc := 1 to (lX*lY) do
      lRenderDepthBufferS^[lInc] := lRenderDepthBuffer^[lInc];

  for lScanlines := (1+lGap) to (lY - lGap) do begin //can not compute angle for 1st and last scanline
      lLineStart := (lScanLines-1)*lX; //inc from 0
      lPrevLineStart := lLineStart-(lX*lGap); //inc from 0
      lNextLineStart := lLineStart+(lX*lGap); //inc from 0
      for lInc := (1+lGap) to (lXmG) do begin
          lWeightSum := 0;
          lDepthSum := 0;
          AddPt (lRenderDepthBuffer^[lPrevLineStart+lInc-1],lFar,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lPrevLineStart+lInc],lClose,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lPrevLineStart+lInc+1],lFar,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lLineStart+lInc-1],lClose,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lLineStart+lInc],lCenter,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lLineStart+lInc+1],lClose,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lNextLineStart+lInc-1],lFar,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lNextLineStart+lInc],lClose,lDepthSum,lWeightSum);
          AddPt (lRenderDepthBuffer^[lNextLineStart+lInc+1],lFar,lDepthSum,lWeightSum);
          if lWeightSum > 0 then
              lRenderDepthBufferS^[lLineStart+lInc] := round(lDepthSum/lWeightSum);
      end; //columns
  end; //for scanlines: rows
  for lInc := 1 to (lX*lY) do
      lRenderDepthBuffer^[lInc] :=  lRenderDepthBufferS^[lInc];
  freemem(lRenderDepthBufferS);
  result := true;
end; //function SmoothShading


function IlluminationShading (lX,lY,lPct: integer; lImgBuffer: bytep;  lRenderDepthBuffer: SmallintP): boolean;
var
   kRenderInfiniteDepth,lXmG,lPrevLineStart,lNextLineStart,lLineStart,lScanLines,
   lGap,lIntensity,lInc,lGrayMin,lGrayMax: integer;
   lShadeFrac,lImgFrac,
   lPhongMagic,lMagic,lYVal,lXVal,lNormalPlane,lXLight,lYLight,lZLight,lLightVectorNormalise: single;
   lShadeBuffer: bytep;
begin //func illumination shading

  result := false;
  if {(gRender.Zoom < 1) or} (lPct < 1) or (lY < 5) or (lX < 5) or (gBGImg.RenderDepthBufferItems <> (lX * lY)) then
     exit;
       lMagic := 1;
  lPhongMagic := 1;
   kRenderInfiniteDepth := 0;
  lXLight := 0;//RenderForm.XL.value / 100;//lXLight / lLightVectorNormalise;
  lYLight := -0.5;//Renderform.YL.value / 100;//lYLight / lLightVectorNormalise;
  lZLight := -1;//RenderForm.ZL.value / 100;//lZLight / lLightVectorNormalise;
  lLightVectorNormalise := sqrt(sqr(lXLight)+sqr(lYLight)+sqr(lZLight));
  lXLight := lXLight / lLightVectorNormalise;
  lYLight := lYLight / lLightVectorNormalise;
  lZLight := lZLight / lLightVectorNormalise;
  lGrayMin := 0{64};
  lGrayMax := 255 - lGrayMin;
  lGap := 1;
  lXmG := lX-lGap;
  Getmem(lShadeBuffer,lX*lY*sizeof(byte));
  fillchar(lShadeBuffer^,lX*lY,0);

  for lScanlines := (1+lGap) to (lY - lGap) do begin //can not compute angle for 1st and last scanline
      lLineStart := (lScanLines-1)*lX; //inc from 0
      lPrevLineStart := lLineStart-(lGap*lX); //inc from 0
      lNextLineStart := lLineStart+(lGap*lX); //inc from 0
      for lInc := (1+lGap) to (lXmG) do begin
        if  lImgBuffer^[lLineStart+lInc] <> 0 then begin //only shade non-zero intensities
         if ( lRenderDepthBuffer^[lPrevLineStart+lInc-1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lPrevLineStart+lInc]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lPrevLineStart+lInc+1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lLineStart+lInc-1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lLineStart+lInc]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lLineStart+lInc+1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lNextLineStart+lInc-1]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lNextLineStart+lInc]<>kRenderInfiniteDepth)
          and (lRenderDepthBuffer^[lNextLineStart+lInc+1]<>kRenderInfiniteDepth) then begin
              lYVal := lRenderDepthBuffer^[lPrevLineStart+lInc-1]+lRenderDepthBuffer^[lPrevLineStart+lInc]+lRenderDepthBuffer^[lPrevLineStart+lInc+1]
              -lRenderDepthBuffer^[lNextLineStart+lInc-1]-lRenderDepthBuffer^[lNextLineStart+lInc]-lRenderDepthBuffer^[lNextLineStart+lInc+1];
              lXVal := lRenderDepthBuffer^[lPrevLineStart+lInc-1]+lRenderDepthBuffer^[lLineStart+lInc-1]+lRenderDepthBuffer^[lNextLineStart+lInc-1]
              -lRenderDepthBuffer^[lPrevLineStart+lInc+1]-lRenderDepthBuffer^[lLineStart+lInc+1]-lRenderDepthBuffer^[lNextLineStart+lInc+1];
              lNormalPlane := sqrt(sqr(lXVal)+sqr(lYVal)+sqr(lMagic));
              if lNormalPlane <> 0 then begin
                lNormalPlane := -((-lXLight*lXVal)-(lYLight*lYVal)+lMagic*lZLight)/lNormalPlane;
                if {lImageAndShade} false then begin
                    lNormalPlane := Power(lNormalPlane,lPhongMagic);
                        //lIntensity := gProjBuffer[lLineStart+lInc];
                        //lIntensity := lPropShadingPivot+round((lPctImage*(lIntensity-lPropShadingPivot))+(lPctShade*(lNormalPlane-0.5)) );
                        if lIntensity > 254 then lIntensity := 254;
                        lShadeBuffer^[lLineStart+lInc] := lIntensity;
                end else begin //shading only
                  //if lAbbaRandom then //abba
                  lNormalPlane := (lNormalPlane+1) / 2;
                  if lNormalPlane > 0 then begin
                        lNormalPlane := Power(lNormalPlane,lPhongMagic);
                        //if lAbbaRandom then //abba
                        //if lNormalPlane < 0.5 then lNormalPlane := 1-lNormalPlane; //backlighting
                        lShadeBuffer^[lLineStart+lInc] := lGrayMin{64}+ round(lNormalPlane*(lGrayMax));
                  end else
                      lShadeBuffer^[lLineStart+lInc] :=  lGrayMin;
                end; //Shading vs ImageAndShading
              end; //NormalPlane = 0
         end else begin //samples for each pixel
             if {lImageAndShade}false then
               lShadeBuffer^[lLineStart+lInc] := 0//lPropShadingPivot+round((lPctImage*(gProjBuffer[lLineStart+lInc]-lPropShadingPivot))+(lPctShade*(-0.5)) )//1362
             else
                lShadeBuffer^[lLineStart+lInc] := lGrayMin;//1363;'# 20{64};
         end;
        end; //only shade non-zero intensities
      end; //columns
  end; //for scanlines: rows
  if lPct > 99 then begin
      for lInc := 1 to (lX*lY) do
          lImgBuffer^[lInc] := lShadeBuffer^[lInc];

  end else begin //partial shade
      lImgFrac := (100-lPct)/100;
      lShadeFrac := lPct/100;
      for lInc := 1 to (lX*lY) do
          lImgBuffer^[lInc] := round((lImgBuffer^[lInc]* lImgFrac) + (lShadeBuffer^[lInc]*lShadeFrac ));
  end;
  freemem(lShadeBuffer);
  result := true;
end; //function illuminationshading

procedure LUTLoad( lLUTindex: integer; var lLUT: TLUT);
var
   lHdr: TMRIcroHdr;
   lStr: string;
   lInc: integer;
begin
	 //gMRIcroOverlay[lLayer].LUTindex := LUTdrop.ItemIndex;
	 if lLUTindex < knAutoLUT then begin
		LoadMonochromeLUT(lLUTindex,gBGImg,lHdr);
	 end else begin //if B&W lut
	     lStr := gColorSchemeDir+pathdelim+ImgForm.LUTdrop.Items.Strings[lLUTindex]+'.lut';
	     if not FileExistsEX(lStr) then
		showmessage('Can not find '+lStr);
             LoadColorScheme(lStr, lHdr);
         end;
         for lInc := 0 to 255 do
		 lLUT[lInc] :=  lHdr.LUT[lInc];
end;


procedure CreateRender(var lBGHdr, lHdr: TMRIcroHdr; var lX,lY,lZ,lInRenderSurface,lInRenderDpeth: Integer; var lQuadP: RGBQuadp; Smooth2D, NormalizeIntensity,lCreateDepthBuffer: boolean;lUseDepthBuffer: integer);
var	lLUT : array [0..255] of byte;	lrgbLUT: TLUT;// array[0..255] of TRGBQuad;	lSrc,lOutBuffer: Bytep;        lShade: boolean;        lPreciseDepthBuffer: Smallintp;        //lTime: Dword;	lNear,lSubPixel,lMaxInten,lDepth,lPixel,lSamples,lSliceOffset,        //lEnd,        lIntensity,lSliceSz,lVolSz,lRenderDepth,lRenderSurface,lTemp: integer;begin  lShade := false;  if (gRender.BGNearClipFrac<>0) or (gRender.ShowCutout) then     lMaxInten := 254  else      lMaxInten := 257;  lRenderDepth := lInRenderDpeth;	if (lRenderDepth < 0) or (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then	lRenderDepth := 1;  lRenderSurface := lInRenderSurface;  if (lHdr.NIFTIhdr.intent_code = kNIFTI_INTENT_LABEL) then	lRenderSurface := 1  else begin  //make sure at least some voxels are below air-surface threshold	if (lHdr.WindowScaledMin <= (Raw2ScaledIntensity(lHdr,lHdr.GlMinUnscaledS) )) and (lHdr.WindowScaledMax <> 0)then begin		lTemp := round( (Raw2ScaledIntensity(lHdr,lHdr.GlMinUnscaledS)-lHdr.WindowScaledMin)/(lHdr.WindowScaledMax)*255);		if lTemp >= lRenderSurface then			lRenderSurface := lTemp + 1;	end;  end;  if (lUseDepthBuffer=kBelow)  then begin	CreateOverlayRenderBehind(lBGHdr,lHdr, lX,lY,lZ,lRenderSurface,lRenderDepth, lQuadP, Smooth2D);	exit;  end;  if (lUseDepthBuffer=kInFront) then begin	CreateOverlayRenderInfrontNear(lBGHdr,lHdr, lX,lY,lZ,lRenderSurface,lRenderDepth, lQuadP, Smooth2D);	exit;  end;  lSrc := lHdr.RenderBuffer;  lSliceSz := lX*lY;  lVolSz := lSliceSz * lZ;  GetMem (lOutBuffer ,  lX*lY);  if lCreateDepthBuffer then begin     if (gRender.ShadePct > 0) then begin        lShade := true;        getmem(lPreciseDepthBuffer,lSliceSz * sizeof(smallint));             fillchar(lPreciseDepthBuffer^,lSliceSz* sizeof(smallint),0);     end;     if gBGImg.RenderDepthBufferItems <> lSliceSz then begin	  if gBGImg.RenderDepthBufferItems > 0 then		Freemem(gBGImg.RenderDepthBuffer);	  gBGImg.RenderDepthBufferItems := lSliceSz;	  GetMem(gBGImg.RenderDepthBuffer,lSliceSz*sizeof(smallint));     end; //no RenderDepthBuffer     fillchar(gBGImg.RenderDepthBuffer^,lSliceSz{* sizeof(smallint)},0);     if lRenderDepth = 0 then begin//MIP        //MIP follows       //lTime := GetTickCount;       (*for lPixel := 1 to lSliceSz do begin	lIntensity := 0;	lSliceOffset := (lPixel-1)*lX; //start with nearest slice        lEnd := lSliceOffset + lX;	while (lSliceOffset < lEnd) do begin		if  (lSrc[lSliceOffset] < lMaxInten) and (lSrc[lSliceOffset] > lIntensity) then begin                        lIntensity := lSrc[lSliceOffset];                        gBGImg.RenderDepthBuffer[lPixel] := lSliceOffset div lSliceSz+1;                        if lShade then                           lPreciseDepthBuffer[lPixel] :=  gBGImg.RenderDepthBuffer[lPixel] * 10;		end;		inc(lSliceOffset,1);	end; //while no voxel found       (* *)       for lPixel := 1 to lSliceSz do begin	lIntensity := 0;	lSliceOffset := 0; //start with nearest slice	while (lSliceOffset < lVolSz) do begin		if  (lSrc[lSliceOffset+lPixel] < lMaxInten) and (lSrc[lSliceOffset+lPixel] > lIntensity) then begin                        lIntensity := lSrc[lSliceOffset+lPixel];                        gBGImg.RenderDepthBuffer[lPixel] := lSliceOffset div lSliceSz+1;                        if lShade then                           lPreciseDepthBuffer[lPixel] :=  gBGImg.RenderDepthBuffer[lPixel] * 10;		end;		inc(lSliceOffset,lSliceSz);	end; //while no voxel found (**)	lOutBuffer[lPixel]:= lIntensity;       end; //for each pixel 1..sliceSz        inc(gInc);        //RenderForm.caption := inttostr(gettickcount-lTime)+'  '+inttostr(gInc);        //MIP end     end else begin //not MIP       for lPixel := 1 to lSliceSz do begin	lDepth := 0;	lSamples := 0;	lIntensity := 0;	lSliceOffset := 0; //start with nearest slice	while (lDepth < lRenderDepth) and (lSliceOffset < lVolSz) do begin		if  (lSrc[lSliceOffset+lPixel] < lMaxInten) and ((lDepth > 0) or (lSrc[lSliceOffset+lPixel] > lRenderSurface)) then begin			inc(lDepth);			if (lSrc[lSliceOffset+lPixel] > lRenderSurface) then begin				lIntensity := lIntensity+ lSrc[lSliceOffset+lPixel];				inc(lSamples);			end;			if (lDepth = 1) then begin				gBGImg.RenderDepthBuffer[lPixel] := lSliceOffset div lSliceSz+1;                                if lShade then begin                                   if (lSliceOffset > 0) then begin //estimate surface depth with sub-pixel accuracy                                      lNear := lSrc[lSliceOffset+lPixel-lSliceSz];                                      lSubPixel := lIntensity-lNear; //delta
                                      lSubPixel := round(((lRenderSurface-lNear)/lSubPixel)*100);
                                      if lNear >= lMaxInten then //cutout                                         lSubPixel := 0;                                   end else                                       lSubpixel := 0;                                   lPreciseDepthBuffer[lPixel] :=  (gBGImg.RenderDepthBuffer[lPixel] * 100)+lSubPixel;                                end;				if (gBGImg.RenderDepthBuffer[lPixel] > 1) and (lSrc[lSliceOffset+lPixel-lSliceSz]>= lMaxInten) then begin //cutout					if lSrc[lSliceOffset+lPixel-lSliceSz]=lMaxInten-1 then						lIntensity := 0;					lDepth := lRenderDepth;					gBGImg.RenderDepthBuffer[lPixel] := -gBGImg.RenderDepthBuffer[lPixel];  //negative: this is a cutout				end;			end;		end;		inc(lSliceOffset,lSliceSz);	end; //while no voxel found	if lDepth > 0 then		lIntensity := lIntensity div lSamples;		//lIntensity := lIntensity div lDepth; //mean of nDepth voxels	lOutBuffer[lPixel]:= lIntensity;	//lOutBuffer[lPixel]:= lHdr.RenderDepthBuffer[lPixel]; //use this to show Z-buffer       end; //for each pixel 1..sliceSz     end; //NOT MIP     {if  true then	for lPixel := 1 to lSliceSz do	 if gBGImg.RenderDepthBuffer[lPixel] > 0 then		lOutBuffer[lPixel]:= lZ- gBGImg.RenderDepthBuffer[lPixel]; {}//use this to show Z-buffer  end else begin //do not create depth buffer      for lPixel := 1 to lSliceSz do begin	lDepth := 0;	lSamples := 0;	lIntensity := 0;	lSliceOffset := 0; //start with nearest slice	while ((lDepth < lRenderDepth) and (lSliceOffset < lVolSz)) do begin		if  (lDepth > 0) or (lSrc[lSliceOffset+lPixel] > lRenderSurface) then begin			inc(lDepth);			if (lSrc[lSliceOffset+lPixel] > lRenderSurface) then begin				lIntensity := lIntensity+ lSrc[lSliceOffset+lPixel];				inc(lSamples);			end;		end;		inc(lSliceOffset,lSliceSz);	end;	if lDepth > 0 then		lIntensity := lIntensity div lSamples;		//lIntensity := lIntensity div lDepth; //mean of nDepth voxels	lOutBuffer[lPixel]:= lIntensity;  end; //for each lpixelend; //volume render without depth buffer if (NormalizeIntensity) and (lRenderSurface < 254) then begin  //must be done BEFORE shading	for lPixel := 0 to 255  do		lLUT[lPixel] := 0;	for lPixel := lRenderSurface to 255 do		lLUT[lPixel] := round(255*(lPixel-lRenderSurface)/(255-lRenderSurface));	for lPixel := 1 to lSliceSz do	   lOutBuffer[lPixel] := lLUT[lOutBuffer[lPixel]];  end;  if lShade then begin      SmoothShading (lX,lY,lPreciseDepthBuffer);      IlluminationShading(lX,lY,gRender.ShadePct,lOutBuffer,lPreciseDepthBuffer{gBGImg.RenderDepthBuffer} );      freemem(lPreciseDepthBuffer);  end;//shading  if (Smooth2D) and (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then //do not smooth labels	Smooth2DImage (lX,lY, lOutBuffer);  //lrgbLUT := lHdr.LUT; //Mar2007
  for lPixel := 0 to 255 do
      lrgbLUT[lPixel] := lHdr.LUT[lPixel];
  if (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then     LUTGainX(lrgbLUT,RenderForm.BiasTrack.Position,RenderForm.GainTrack.Position {RenderForm.BiasTrack.Position}); //Mar2007  for lPixel := 1 to lSliceSz do        lQuadP[lPixel]:= lrgbLUT[lOutBuffer[lPixel]]; //Mar2007	//lQuadP[lPixel]:= lHdr.LUT[lOutBuffer[lPixel]];  if ( (gRender.BGNearClipFrac<>0) or (gRender.ShowCutout)) and (lCreateDepthBuffer)  then begin //make cutout grayscale, shade edges	if gRender.ShowCutout then		ShadeCutoutCrease(lOutBuffer);        LUTLoad(gRender.cutoutLUTindex,lrgblut);//11/2007
	(*for lPixel := 0 to 255 do begin
		lrgbLUT[lPixel].rgbRed := lPixel;
		lrgbLUT[lPixel].rgbGreen := lPixel;
		lrgbLUT[lPixel].rgbBlue := lPixel;
		lrgbLUT[lPixel].rgbReserved := kLUTalpha;

	end;//create grayscale LUT *)	LUTBiasX(lrgbLUT,gRender.CutoutBias);	for lPixel := 1 to lSliceSz do		if gBGImg.RenderDepthBuffer[lPixel]<0 then //cutout			lQuadP[lPixel]:= lrgbLUT[lOutBuffer[lPixel]];  end; //if BGimg with Cutout  Freemem(lOutBuffer);end;function RenderDepth (lVal: integer): integer;//11/2007begin  if (lVal > 0) and (lVal < 16000) and (gBGImg.ScrnMM[1] > 0.1) and (gBGImg.ScrnMM[1] < 10) then begin     result:= round (lVal / gBGImg.ScrnMM[1]);     if result < 1 then        result := 1;  end else      result := lVal;  result := round(result * gRender.Zoom);end;procedure DrawRender;var   lBGQuadP, lOverlayQuadP, l2ndOverlayQuadP: RGBQuadp;   lUseBGSurface,lnOverlay,lOverlay, lX,lY,lZ,lSliceSz,lRenderSurface,lRenderDepth: longint;   lBG0Clr,lOverlay0Clr: DWord;   lSmooth : boolean;begin  lRenderSurface := gRender.BGSurface;  lRenderDepth:= RenderDepth(gRender.BGDepth);//11/2007  lSmooth := gRender.SmoothBG;  lUseBGSurface := gRender.OverlayFromBGSurface ;  lX := gMRIcroOverlay[kBGOverlayNum].RenderDim;  lY := lX;  lZ := lX;  lSliceSz := (lX * lY);  if (gMRIcroOverlay[kBGOverlayNum].RenderBufferItems=0)or (lX < 2) or (lY < 2) or (lZ < 2) or ((lX*lY*lZ) > gMRIcroOverlay[kBGOverlayNum].RenderBufferItems{ScrnBufferItems}) then	 exit;  GetMem ( lBGQuadP,  lSliceSz*4);  CreateRender(gMRIcroOverlay[kBGOverlayNum],gMRIcroOverlay[kBGOverlayNum], lX,lY,lZ,lRenderSurface,lRenderDepth, lBGQuadP, lSmooth, true,true,0);//next: overlays  lSmooth := gRender.SmoothOverlay;  lRenderSurface := gRender.OverlaySurface;  lRenderDepth:=  RenderDepth(gRender.OverlayDepth);//11/2007lnOverlay := 0;lBG0Clr:= (gMRIcroOverlay[0].LUTinvisible);//just to avoid compiler warning hint - never used...for lOverlay := knMaxOverlay downto 1 do begin  if gMRIcroOverlay[lOverlay].RenderBufferItems{ScrnBufferItems} > 0 then begin        if lOverlay = kVOIOverlayNum then //Aug2007           lRenderSurface := 0        else            lRenderSurface := gRender.OverlaySurface;//	inc(lnOverlay);	if lnOverlay = 1 then begin //top overlay		GetMem ( lOverlayQuadP ,  lSliceSz*4);		lBG0Clr:= (gMRIcroOverlay[lOverlay].LUTinvisible);		CreateRender(gMRIcroOverlay[kBGOverlayNum],gMRIcroOverlay[lOverlay],lX,lY,lZ,lRenderSurface,lRenderDepth,lOverlayQuadP,lSmooth,false,false,lUseBGSurface);	end else begin //2nd or lower overlay		if lnOverlay = 2 then  //2nd overlay			GetMem ( l2ndOverlayQuadP ,  lSliceSz*4);		CreateRender(gMRIcroOverlay[kBGOverlayNum],gMRIcroOverlay[lOverlay], lX,lY,lZ,lRenderSurface,lRenderDepth,l2ndOverlayQuadP,lSmooth,false,false,lUseBGSurface);		lOverlay0Clr:= (gMRIcroOverlay[lOverlay].LUTinvisible);		AlphaBlend32(lOverlayQuadP,l2ndOverlayQuadP, lBG0Clr,lOverlay0Clr, lSliceSz,gBGImg.OverlayTransPct);	end; //2nd overlay or more  end; //overlay loadedend; //for knOverlay..1//Finally: draw overlays on BGif lnOverlay > 0 then begin	lOverlay0Clr := lBG0Clr;	//lBG0Clr := DWord(lHdr.LUTinvisible);	lBG0Clr := 0;//0=impossible, no alpha DWord(lHdr.LUT[0]);	if lnOverlay > 1 then		FreeMem ( l2ndOverlayQuadP);	AlphaBlend32(lBGQuadP,lOverlayQuadP, lBG0Clr,lOverlay0Clr, lSliceSz,gBGImg.BGTransPct);	FreeMem ( lOverlayQuadP);end;//draw image  SetDimension32(lY,lX,  lBGQuadP, gBGImg, RenderForm.RenderImage, RenderForm.RenderPanel);  SetDimension32(lY,lX,  lBGQuadP, gBGImg, RenderForm.RenderImageBup, RenderForm.RenderPanel);  FreeMem ( lBGQuadP);  if gBGImg.RenderDepthBufferItems > 0 then //negative depth was used for cutouts, now set to true depth	for lX := 1 to gBGImg.RenderDepthBufferItems do		gBGImg.RenderDepthBuffer[lX] := abs(gBGImg.RenderDepthBuffer[lX]);end;
procedure TRenderForm.SliceToFrac;
var
   lInc: integer;
begin
            SortCutOut (gRender.CutOut);
	for lInc := 1 to 3 do begin
            if gBGImg.ScrnDim[lInc] < 1 then begin
               gRender.CutoutFrac.Lo[lInc] := round (0.5* kMaxFrac);
               gRender.CutoutFrac.Hi[lInc] := kMaxFrac;
            end else begin
                gRender.CutoutFrac.Lo[lInc] := round(kMaxFrac * gRender.Cutout.Lo[lInc]/gBGImg.ScrnDim[lInc]);
                gRender.CutoutFrac.Hi[lInc] := round(kMaxFrac * gRender.Cutout.Hi[lInc]/gBGImg.ScrnDim[lInc]);
            end;
	end;
end;


procedure SetLimits(var lBGImg: TBGImg);
var lInc: integer;
lUpdateCutout: boolean;
lScale: single;
begin
        SortCutOut (gRender.CutOutFrac);
        if gRender.CutoutFrac.Lo[1] < 0 then
           RenderForm.SliceToFrac;
        lScale := 1/kMaxFrac;
        for lInc := 1 to 3 do begin
            gRender.Cutout.Lo[lInc] := round(gBGImg.ScrnDim[lInc] * lScale * gRender.CutoutFrac.Lo[lInc]);
            gRender.Cutout.Hi[lInc] := round(gBGImg.ScrnDim[lInc] * lScale * gRender.CutoutFrac.Hi[lInc]);
        end;
        //renderform.caption := inttostr(gRender.Cutout.Lo[1])+'  '+inttostr(gRender.Cutout.Hi[1])+'  '+inttostr(random(888));
	SortCutout (gRender.Cutout); //ensure Lo < Hi
	lUpdateCutout := true;
	for lInc := 1 to 3 do
	  if gRender.Cutout.Lo[lInc] <> gRender.Cutout.Hi[lInc] then lUpdateCutout := false;
	if lUpdateCutout then
		for lInc := 1 to 3 do begin
			gRender.Cutout.Lo[lInc] := gBGImg.ScrnDim[lInc] div 2;
			gRender.Cutout.Hi[lInc] := gBGImg.ScrnDim[lInc];
		end;
	for lInc := 1 to 3 do begin
		if gRender.Cutout.Lo[lInc] < 1 then gRender.Cutout.Lo[lInc] := 1;
		if gRender.Cutout.Lo[lInc] > lBGImg.ScrnDim[lInc] then gRender.Cutout.Lo[lInc] := lBGImg.ScrnDim[lInc];
		if gRender.Cutout.Hi[lInc] < 1 then gRender.Cutout.Hi[lInc] := 1;
		if gRender.Cutout.Hi[lInc] > lBGImg.ScrnDim[lInc] then gRender.Cutout.Hi[lInc] := lBGImg.ScrnDim[lInc];
	end;
end;

{function ClipFracSlices (var lClipFrac,lClip: integer): integer;
var
   lMax : integer;
begin
     lMax := gBGImg.RenderDim;
     RenderForm.Caption := inttostr(lClipFrac)+'  '+inttostr(lClip);
     if (lClipFrac < 0) and (lClip > 0) then
         lClipFrac := round(lClip/lMax*kMaxFrac);
     if lClipFrac < 1 then
        lClipFrac := 0;
     if lClipFrac > kMaxFrac then
        lClipFrac := kMaxFrac div 2;

     lClip := round(lClipFrac/kMaxFrac*lMax* gRender.Zoom);

end;}
function ClipFracSlices(lClipFrac,lMax: integer): integer;
begin
     if (lClipFrac <= 0) then
        result := 0
     else if (lClipFrac >= kMaxFrac) then
          result := lMax div 2
     else
         result := round(lClipFrac/kMaxFrac*lMax);
end;



procedure TRenderForm.VolumeRotateMatrix (var lBGImg: TBGImg; var lHdr: TMRIcroHdr; var lMatrixIn: TMatrix; lBilinearSmooth,lRenderCutout,lIsBG: boolean;lNearSlicesClipInFrac: integer);
label 345;
const
 //lZoom = true;
 kUgly2 = 10000;
 // kSh = 10; //bits to shift
 kUgly1 = (kUgly2 shl kSh) + (1 shl kSh);
var
 l:   TRotateVals;
 lNearSlicesClip,lZinc,lZ,lY,lX,lOutVolSz,
 lOutPos,lInVolSz,
 lYo,lZo,lnThreads: integer;
 lBuffIn,lSrcBuff,lBuffOut: Bytep;
 lXxp,lXyp,lXzp: Pointer;
 lScale ,lMatrix: TMatrix;
 lZoomRatio : single;
 begin
 lMatrix := lMatrixIn;
 //gRender.Zoom := gZoom;
 //gZoom := 1;
 if (gRender.Zoom <> 0) and (gRender.Zoom <> 1 )then begin
      lZoomRatio := 1/gRender.Zoom;
      lScale := Matrix3D(lZoomRatio,0,0,0,   0,lZoomRatio,0,0,  0,0,lZoomRatio,0, 0,0,0,0);
      lMatrix := MultiplyMatrices(lMatrixIn,lScale);
  end else
      gRender.Zoom := 1;

  
  //lScale := Matrix3D(0,1,0,0,   0,0,1,0,  1,0,0,0, 0,0,0,0);
  //    lMatrix := MultiplyMatrices(lMatrixIn,lScale);

  l.XdimIn := lBGImg.ScrnDim[1];
  l.YdimIn := lBGImg.ScrnDim[2];
  l.ZdimIn := lBGImg.ScrnDim[3];
  l.InSliceSz := l.XDimIn*l.YDimIn;
  lInVolSz := l.XdimIn*l.YdimIn*l.ZdimIn; //InVolSz!
  if  (lHdr.ScrnBufferItems < lInVolSz) then
	exit;
  lSrcBuff := lHdr.ScrnBuffer;
  l.OutDim := MaxDim(l.XDimIn,l.YDimIn,l.ZDimIn);
  //if gRender.Zoom  then
  //gRender.UnscaledRenderDim := l.OutDim;
  l.OutDim := round(gRender.Zoom * l.OutDim); //11/2007
  lNearSlicesClip := ClipFracSlices(lNearSlicesClipInFrac,l.OutDim);//May07
  if lNearSlicesClip >= l.OutDim then //May07
     lNearSlicesClip := 0; //May07
  lBGImg.RenderDim := l.OutDim;
  lHdr.RenderDim := l.OutDim;
  if (lNearSlicesClip> 0) or (lRenderCutout) then begin
	 SetLimits(lBGImg);
	 GetMem(lBuffIn, lInVolSz);
	 Move(lSrcBuff^,lBuffIn^,lInVolSz);
	 for lZ := 1 to lInVolSz do
		 if lBuffIn[lZ] >= 254 then lBuffIn[lZ] := 253;
   if	lRenderCutout then begin

        for lZ := gRender.Cutout.Lo[3] to gRender.Cutout.Hi[3] do begin
		 lZo := (lZ-1) * l.InSliceSz;
		 Application.ProcessMessages;
		 for lY := gRender.Cutout.Lo[2] to gRender.Cutout.Hi[2] do begin
			 lYo := (lY-1) * l.XdimIn;
			 for lX := gRender.Cutout.Lo[1] to gRender.Cutout.Hi[1] do
				 lBuffIn[lX+lYo+lZo] := 255;
		 end; //for lY
	 end; //for lZ
   end;
  end else
   lBuffIn := lSrcBuff;
  l.OutPivot := (lHdr.RenderDim+1) shr 1; //e.g. if DimMax=9, then pivot is 5
  l.XPivotIn := ((l.XdimIn+1) shr 1); //e.g. if DimMax=9, then pivot is 5
  l.YPivotIn := ((l.YdimIn+1) shr 1); //e.g. if DimMax=9, then pivot is 5
  l.ZPivotIn := ((l.ZdimIn+1) shr 1); //e.g. if DimMax=9, then pivot is 5
  l.YDimStart := -l.OutPivot+1; //e.g. if 9, start from -4
  l.ZDimStart := l.YDimStart + lNearSlicesClip;

  l.YDimEnd := l.YDimStart+lHdr.RenderDim-1; //e.g. if 9, go to 4
  l.ZDimEnd := l.YDimEnd;
  if l.ZDimStart >= l.ZDimEnd then
	l.ZDImStart := l.ZDimStart;
  l.OutSliceSz :=  sqr(lHdr.RenderDim);
  lOutVolSz := lHdr.RenderDim*l.OutSliceSz;
  if lHdr.RenderBufferItems <> lOutVolSz then begin
	if lHdr.RenderBufferItems > 0 then
		Freemem(lHdr.RenderBuffer);
	lHdr.RenderBufferItems := lOutVolSz;
	GetMem(lHdr.RenderBuffer,lOutVolSz);
  end;
  lBuffOut := lHdr.RenderBuffer;
  fillchar(lBuffOut^,lOutVolSz,0); //set all to zero
  //next shade clipping
  if (lIsBG) and (lNearSlicesClip > 1) then begin
	lOutPos := (lNearSlicesClip-1)*l.OutDim*l.OutDim;
	for lY := 1 to l.OutDim do
		for lX := 1 to l.OutDim do
			lBuffOut[((lY-1)*l.OutDim)+lX+lOutPos] := 255;
  end;
  //lMatrix :=  InvertMatrix3D(lMatrix);
  lZ := (sizeof(longint)* l.OutDim)+16;
  GetMem(lXxp, lZ);
  GetMem(lXyp, lZ);
  GetMem(lXzp, lZ);
//  if RenderForm.RenderRefreshTimer.enabled then goto 345;//abort
  l.XxRA := LongIntP($fffffff0 and (integer(lXxP)+15)); //data aligned to quad-word boundary
  l.XyRA := LongIntP($fffffff0 and (integer(lXyP)+15)); //quad-word boundary
  l.XzRA := LongIntP($fffffff0 and (integer(lXzP)+15)); //quad-word boundary
  for lX := 1 to  l.OutDim do begin
		   l.XxRA[lX] := round((lX-l.OutPivot)*lMatrix.matrix[1,1]* (1 shl kSh)  )+kUgly1;
		   l.XyRA[lX] := round((lX-l.OutPivot)*lMatrix.matrix[2,1]* (1 shl kSh)  )+kUgly1;
		   l.XzRA[lX] := round((lX-l.OutPivot)*lMatrix.matrix[3,1]* (1 shl kSh)  )+kUgly1;
  end;
  l.XPivotInU2 := l.XPivotIn-kUgly2;
  l.YPivotInU2 := l.YPivotIn-kUgly2;
  l.ZPivotInU2 := l.ZPivotIn-kUgly2;

     lnThreads := gnCPUThreads;
   //if lIsBG then
   //TextForm.Memo1.Lines.Add( 'bg'+(inttostr(RenderForm.ThreadsRunning)+'  '+inttostr(lnThreads)))

   //else
   //TextForm.Memo1.Lines.Add( 'xx'+(inttostr(RenderForm.ThreadsRunning)+'  '+inttostr(lnThreads)));
     lZ := l.ZDimStart;
     lZo := l.ZDimEnd;
     lZinc := (l.ZDimEnd - l.ZDimStart) div lnThreads;
     l.ZDimEnd := l.ZDimStart + lZinc;
  //showmessage( inttostr(l.ZDimStart)+'..'+inttostr(l.ZDimEnd) +'  '+inttostr(lZo));
  if l.ZDimEnd > ImgForm.ProgressBar1.Min then begin //crashes if max < min, so write order important...
     ImgForm.ProgressBar1.Max := l.ZDimEnd+1;
     ImgForm.ProgressBar1.Min := l.ZDimStart;
  end else begin
     ImgForm.ProgressBar1.Min := l.ZDimStart;
     ImgForm.ProgressBar1.Max := l.ZDimEnd+1;

  end;
  //l.ZDimEnd;
        Application.processmessages;

     for lX := 1 to lnThreads do begin
         if lX = lnThreads then
            l.ZDimEnd := lZo; //avoid integer rounding error
         //TextForm.Memo1.Lines.Add('+'+inttostr(lX));
         if (lBilinearSmooth) and (lHdr.NIFTIhdr.intent_code <> kNIFTI_INTENT_LABEL) then
            with TTriRender.Create(ImgForm.ProgressBar1,lX,l,lMatrix, lRenderCutout, lBuffIn,lBuffOut) do
                 OnTerminate := ThreadDone
         else
           with TNNRender.Create(ImgForm.ProgressBar1,lX,l,lMatrix, lRenderCutout, lBuffIn,lBuffOut) do
           OnTerminate := ThreadDone;
         inc(ThreadsRunning);
         l.ZDimStart := l.ZDimEnd + 1;
         l.ZDimEnd := l.ZDimEnd + lZInc;

     end; //for each thread
     l.ZDimStart := lZ;

  repeat
        Application.processmessages;
  until ThreadsRunning = 0;
        Application.processmessages;

  Refresh;
//345:
	   FreeMem(lXxp);
	   FreeMem(lXyp);
	   FreeMem(lXzp);
  if (lRenderCutout) or (lNearSlicesClip> 0) then begin
	 FreeMem(lBuffIn);
	 //for lZ := 1 to lInVolSz do
	 //	if lBuffOut[lZ] = 255 then lBuffOut[lZ] := 0;
  end;
  ImgForm.ProgressBar1.Position := l.ZDimStart;
end; //proceudre VolumeRotate;(**)


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
  lMatrix := AziElevMatrix;        Application.processmessages;  gRender.Zoom := gZoom; //11/2007b  gZoom := 1;  lStartTime := GetTickCount;  ClipFracCheck (gRender.BGNearClipFrac,gRender.BGNearClip);  ClipFracCheck (gRender.OverlayNearClipFrac,gRender.OverlayNearClip);  VolumeRotateMatrix (gBGImg, gMRIcroOverlay[0],lMatrix, gRender.Trilinear,gRender.ShowCutout,true,gRender.BGNearClipFrac);  if RenderRefreshTimer.Enabled  then exit;  Refresh;  for lC := 1 to knMaxOverlay do begin	VolumeRotateMatrix (gBGImg, gMRIcroOverlay[lC],lMatrix, gRender.Trilinear,false,false,gRender.OverlayNearClipFrac);        if RenderRefreshTimer.Enabled  then exit;  end;  ImgForm.StatusLabel.caption :=('update(ms): '+inttostr(GetTickCount-lStartTime));end;
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
ThreadsRunning := 0;
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

initialization
  InitializeCriticalSection(CritSect);


finalization
  DeleteCriticalSection(CritSect);
end.
