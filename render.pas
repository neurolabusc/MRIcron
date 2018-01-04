unit render;
interface
{$include isthreaded.inc}
{$mode delphi}
uses
{$IFDEF Unix}
lclintf, //gettickcount
{$ELSE}
Windows,
{$ENDIF}
{$IFNDEF NoThreads}
 RenderThds,
{$ELSE}
rendernothreads,
{$ENDIF}
 LResources,SysUtils, GraphicsMathLibrary,Classes, Graphics, Controls, Forms, Dialogs,ExtCtrls,Buttons,
 nifti_img, nifti_hdr,define_types,nifti_img_view,StdCtrls, Spin, Menus,ClipBrd,ReadInt,IniFiles,
 ComCtrls,userdir,render_composite;
type
  { TRenderForm }
  TRenderForm = class(TForm)
    CutoutMenu: TMenuItem;
    ClipTrack: TTrackBar;
    MenuItem1: TMenuItem;
    SaveClipMenu: TMenuItem;
    MIPItem: TMenuItem;
    ShadeEdit: TSpinEdit;
    Label5: TLabel;
    RotationBMPMenu: TMenuItem;
	RenderBar: TPanel;
	AzimuthEdit: TSpinEdit;
    ElevationEdit: TSpinEdit;
    MainMenu1: TMainMenu;
 FileMenu: TMenuItem;
    Close1: TMenuItem;
	Edit1: TMenuItem;
    Copy1: TMenuItem;
    Save1: TMenuItem;
    Label4: TLabel;
    RefreshBtn: TSpeedButton;
    BiasTrack: TTrackBar;
    GainTrack: TTrackBar;
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
    RenderImageBUP: TImage;
    //RenderImage2: TImage;

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
    procedure BiasTrackChange(Sender: TObject);
    procedure ClipTrackChange(Sender: TObject);
    procedure GainTrackChange(Sender: TObject);
    procedure RenderSmoothBGClick(Sender: TObject);
    procedure RotationBMPMenuClick(Sender: TObject);
    procedure SaveClipMenuClick(Sender: TObject);
    procedure SetSearch(Sender: TObject);
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
    procedure RefreshClick(Sender: TObject);
    procedure  RefreshRotation;
  private
	{ Private declarations }
  public
	{ Public declarations }
  end;

var
  RenderForm: TRenderForm;
  gZoom : single = 1;
  gRenderDir,gRenderStartupFilename,gRenderDefaultsFilename:string;
implementation

uses MultiSlice,math,cutout;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.DFM}
 {$ENDIF}
procedure MinMaxFilt (var lHdr: TMRIcroHdr; var lFiltMin8bit, lFiltMax8bit: integer);var lMin,lMax: single;
begin
ReturnMinMax (lHdr,lMin,lMax, lFiltMin8bit, lFiltMax8bit);
end;

procedure TRenderForm.UpdateRenderDisplay;
begin
	SetSubmenuWithTag(RenderBGSurfaceMenu,gRender.BGSurface);
	SetSubmenuWithTag(RenderOverlaySurfaceMenu,gRender.OverlaySurface);
	SetSubmenuWithTag(RenderBGDepthMenu,gRender.BGDepth);
	SetSubmenuWithTag(RenderOverlayDepthMenu,gRender.OverlayDepth);
	RenderSmoothBG.checked := gRender.SmoothBG;
        BiasTrack.Position:= gRender.Bias;
        GainTrack.Position:=gRender.Gain;
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
	lIniFile.WriteString('BOOL', 'SmoothBG',Bool2Char( SmoothBG));
	lIniFile.WriteString('BOOL', 'SmoothOverlay',Bool2Char( SmoothOverlay));
	lIniFile.WriteString('BOOL', 'Trilinear',Bool2Char( Trilinear));
	lIniFile.WriteString('BOOL', 'ShowCutout',Bool2Char( ShowCutout));
	lIniFile.WriteString('BOOL', 'FlipLR',Bool2Char( FlipLR));
	lIniFile.WriteString('INT', 'OverlayFromBGSurface',IntToStr( OverlayFromBGSurface));
	//lIniFile.WriteString('INT', 'BGNearClip',IntToStr(BGNearClip));
	//lIniFile.WriteString('INT', 'OverlayNearClip',IntToStr(OverlayNearClip));
	lIniFile.WriteString('INT', 'Azimuth',IntToStr(Azimuth));
	lIniFile.WriteString('INT', 'Elevation',IntToStr(Elevation));
	lIniFile.WriteString('INT', 'BGSurface',IntToStr(BGSurface));
	lIniFile.WriteString('INT', 'OverlaySurface',IntToStr(OverlaySurface));
	lIniFile.WriteString('INT', 'BGDepth',IntToStr(BGDepth));
	lIniFile.WriteString('INT', 'OverlayDepth',IntToStr(OverlayDepth));
	lIniFile.WriteString('INT', 'CutoutBias',IntToStr(CutoutBias));
	lIniFile.WriteString('INT', 'cutoutLUTindex',IntToStr(cutoutLUTindex));
	lIniFile.WriteString('INT', 'ShadePct',IntToStr(ShadePct));
        lIniFile.WriteString('INT', 'Bias',IntToStr(Bias));
        lIniFile.WriteString('INT', 'Gain',IntToStr(Gain));
	for lInc := 1 to 3 do begin
		lIniFile.WriteString('INT', 'CutoutLoFrac'+inttostr(lInc),IntToStr(CutoutFrac.Lo[lInc]));
		lIniFile.WriteString('INT', 'CutoutHiFrac'+inttostr(lInc),IntToStr(CutoutFrac.Hi[lInc]));
	end;
  end;//with gRender
  lIniFile.Free;
end;

procedure ReadRenderIniFile (lFilename: string);
var
  lIniFile: TIniFile;
  //lStr: string;
  lInc: integer;
begin
	if not FileexistsEx(lFilename) then begin
		exit;
	end;
  lIniFile := TIniFile.Create(lFilename);
  //lStr := lIniFile.ReadString('STR', 'Slices', '10,20,30');//file0 - last file viewed
  with gRender do begin
	//Booleans
	//SmoothBG,SmoothOverlay,Trilinear,OverlayFromBGSurface,ShowCutout
	SmoothBG := IniBool(lIniFile,'SmoothBG',SmoothBG);
	SmoothOverlay := IniBool(lIniFile,'SmoothOverlay',SmoothOverlay);
	Trilinear := IniBool(lIniFile,'Trilinear',Trilinear);
	//OverlayFromBGSurface := IniBool(lIniFile,'OverlayFromBGSurface',OverlayFromBGSurface);
	ShowCutout := IniBool(lIniFile,'ShowCutout',ShowCutout);
	FlipLR := IniBool(lIniFile,'FlipLR',FlipLR);
	OverlayFromBGSurface:= IniInt(lIniFile,'OverlayFromBGSurface',OverlayFromBGSurface);
	//BGNearClip:= IniInt(lIniFile,'BGNearClip',BGNearClip);
	//OverlayNearClip:= IniInt(lIniFile,'OverlayNearClip',OverlayNearClip);
	Azimuth:= IniInt(lIniFile,'Azimuth',Azimuth);
	Elevation:= IniInt(lIniFile,'Elevation',Elevation);
	BGSurface:= IniInt(lIniFile,'BGSurface',BGSurface);
	OverlaySurface:= IniInt(lIniFile,'OverlaySurface',OverlaySurface);
	BGDepth:= IniInt(lIniFile,'BGDepth',BGDepth);
	OverlayDepth:= IniInt(lIniFile,'OverlayDepth',OverlayDepth);
	CutoutBias:= IniInt(lIniFile,'CutoutBias',	CutoutBias);
 	ShadePct:= IniInt(lIniFile,'ShadePct',	0);
	cutoutLUTindex:= IniInt(lIniFile,'cutoutLUTindex',cutoutLUTindex);
	Bias:= IniInt(lIniFile,'Bias',Bias);
	Gain:= IniInt(lIniFile,'Gain',Gain);
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
   //07 CutoutForm.Prep;
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
                   {$IFDEF FPC}
                    NewItem.onclick :=  OpenRenderMRU; //Lazarus
                   {$ELSE}
                     NewItem.onclick :=  OpenRenderMRU;
                     {$ENDIF}
		   Settings1.Add(NewItem);
		until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
end;

Function AziElevMatrix: TMatrix;
var
	lLRFlipMatrix: TMatrix;
begin
	  gRender.Azimuth := RenderForm.AzimuthEdit.value;
  gRender.Elevation := RenderForm.ElevationEdit.value;
	result := ViewTransformMatrix(
		   coordSpherical,
		   ToRadians(RenderForm.AzimuthEdit.Value),
		   ToRadians(RenderForm.ElevationEdit.Value),
		   3{Distance.Value},6{ScreenWidthHeight.Value},6{ScreenWidthHeight.Value},{ScreenToCamera.Value}3);
	  {The ViewTransformMatrix is all that is needed for other objects defined
	  in world coordinates.}
	if {RenderForm.FlipLRcheck.checked} gRender.FlipLR then begin
		lLRFlipMatrix := Matrix3D (-1,0,0,0,      // 3D "graphics" matrix
						   0,1,0,0,
						   0,0,1,0,
						   0,0,0,0);
	result := MultiplyMatrices(lLRFlipMatrix,Result);
  end;

end;


procedure InvertMatrixPoint (var lBackgroundImg: TBGImg; var lInMatrix: TMatrix; var lXin,lYin,lZIn, lXout,lYout,lZout: integer);
//convert mouse click to position
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
//this code is required for OSX Lazarus, not sure about Windows/Delphi
var
lOutImg: TImage;
begin
  lOutImg :=  TImage.Create(ImgForm);
       lOutImg.Width := RenderImage.Width;
    lOutImg.Height := RenderImage.Height;
    lOutImg.Canvas.Draw(0,0,RenderImage.Picture.Graphic);
    SaveImgAsPNGBMP (lOutImg);
    FreeAndNil (lOutImg);
end;

(*procedure TRenderForm.Save1Click(Sender: TObject);
begin
	 //if (RenderImage.Picture.Graphic = nil) then begin
	 SaveImgAsPNGBMP (RenderImage);
         ///xxxx
end;*)

procedure TRenderForm.RenderImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TRenderForm.Copy1Click(Sender: TObject);
{$IFDEF FPC}
var
lOutImg: TImage;
begin
  lOutImg :=  TImage.Create(ImgForm);
  lOutImg.Width := RenderImage.Width;
  lOutImg.Height := RenderImage.Height;
  lOutImg.Canvas.Draw(0,0,RenderImage.Picture.Graphic);
  lOutImg.Picture.Bitmap.SaveToClipboardFormat(2);
  Clipboard.Assign(lOutImg.Picture.Graphic);
  FreeAndNil (lOutImg);
end;

(*begin
        {$IFDEF zxDarwin}
        Showmessage('Copy not yet supported with OSX: use File/Save');
        exit;
        {$ENDIF}
	 if (RenderImage.Picture.Graphic = nil) then begin //1420z
        Showmessage('You need to load an image before you can copy it to the clipboard.');
        exit;
     end;
        RenderImage.Picture.Bitmap.SaveToClipboardFormat(2);
end;*)
{$ELSE}
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
{$ENDIF}


procedure TRenderForm.RotationBMPMenuClick(Sender: TObject);
var
   lnViews,lC,lAngle,lStartA: integer;
   lZoom,lAzi: boolean;
   lBaseFilename,lFilename: string;
begin
     lnViews:= ReadIntForm.GetInt('How many bitmaps for a 360-degree rotation?', 4,24,72);
      {$IFDEF ENDIAN_BIG}
	ImgForm.SaveDialog1.Filter := 'Bitmap|*.xpm';
	ImgForm.SaveDialog1.DefaultExt := '.xpm';
      {$ELSE}
	ImgForm.SaveDialog1.Filter := 'Bitmap|*.bmp';
	ImgForm.SaveDialog1.DefaultExt := '.bmp';
      {$ENDIF}
      if not ImgForm.SaveDialog1.Execute then exit;
     lBaseFilename := ImgForm.SaveDialog1.Filename;
     lAzi := false;
	case MessageDlg('Rotate azimuth?', mtConfirmation,
		[mbYes, mbNo], 0) of
		mrYes: lAzi := true;
	end; //case
  	case MessageDlg('Generate super-sampled (high quality) renderings?', mtConfirmation,
		[mbYes, mbNo], 0) of
		mrYes: lZoom := true;
	end; //case

     if lAzi then
        lStartA := AzimuthEdit.value
     else
         lStartA := ElevationEdit.value;
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
      {$IFDEF ENDIAN_BIG}
        lFilename :=  ChangeFilePostfixExt (lBaseFilename,PadStr(lAngle,3),'.xpm');
      {$ELSE}
        lFilename :=  ChangeFilePostfixExt (lBaseFilename,PadStr(lAngle,3),'.bmp');
      {$ENDIF}
        RenderImage.Picture.Bitmap.SaveToFile(lFilename);
        //SaveImgAsPNGBMPCore(RenderImage,lFilename);
    end; //for each of 36 views
    if lAzi then
       AzimuthEdit.value := lStartA
    else
        ElevationEdit.value := lStartA;
        RenderRefreshTimer.enabled := false;
        RefreshRotation;
        DrawRender;
end;

procedure TRenderForm.SaveClipMenuClick(Sender: TObject);
var
   lStartClip,lnClips,lC: integer;
   lBaseFilename,lFilename: string;
   lStartTime: DWord;
begin
     lStartClip := gRender.ClipFrac;
     lnClips:= ReadIntForm.GetInt('How many bitmaps for a 360-degree rotation?', 4,24,200);
     ImgForm.SaveDialog1.Filter := 'PNG bitmap|*.png';
     ImgForm.SaveDialog1.DefaultExt := '*.png';
     if not ImgForm.SaveDialog1.Execute then exit;
     lBaseFilename := ImgForm.SaveDialog1.Filename;
    lStartTime := GetTickCount;
    for lC := 1 to lnClips do begin
        gRender.ClipFrac := round(  ((lC-1)/lnClips)*kMaxFrac   );
        DrawRender;
        refresh;
      {$IFDEF ENDIAN_BIG}
        lFilename :=  ChangeFilePostfixExt (lBaseFilename,PadStr(lC,3),'.xpm');
      {$ELSE}
        lFilename :=  ChangeFilePostfixExt (lBaseFilename,PadStr(lC,3),'.bmp');
      {$ENDIF}
        RenderImage.Picture.Bitmap.SaveToFile(lFilename);

        //SaveImgAsPNGBMPCore(RenderImage,lFilename);
    end; //for each of 36 views
      ImgForm.StatusLabel.caption :=('batchtime(ms): '+inttostr(GetTickCount-lStartTime));
    gRender.ClipFrac := lStartClip;
end;




procedure TRenderForm.BiasTrackChange(Sender: TObject);
begin
     gRender.Bias := BiasTrack.position;
     //gRender.Gain := GainTrack.Position;

 	RenderRefreshTimer.Enabled := true;
  //RenderForm.caption := inttostr(BiasTrack.position)+'zzz'+inttostr(GainTrack.Position);
end;

procedure TRenderForm.ClipTrackChange(Sender: TObject);
begin
       gRender.ClipFrac := ClipTrack.Position;
	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.GainTrackChange(Sender: TObject);
begin
       gRender.Gain := GainTrack.Position;

 	RenderRefreshTimer.Enabled := true;
end;

procedure TRenderForm.RenderSmoothBGClick(Sender: TObject);
begin
	(sender as TMenuItem).checked := not (sender as TMenuItem).checked;

 gRender.SmoothBG := RenderSmoothBG.checked;
	gRender.SmoothOverlay := RenderSmoothOverlay.checked;
  	RenderRefreshTimer.Enabled := true;
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
     gRender.ClipFrac := 0;
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
	//gRender.OverlayNearClip := 0;
	//gRender.BGNearClip := 0;
	gRender.SmoothBG	:= true;
	gRender.SmoothOverlay	:= false;
	gRender.Trilinear	:= true;
	gRender.FlipLR := false;
	gRender.OverlayFromBGSurface	:= kBelow;
	gRender.ShowCutout := false;
	gRender.CutoutBias := 4;
	{for lInc := 1 to 3 do begin
		gRender.Cutout.Lo[lInc] := gBGImg.ScrnDim[lInc] div 2;
		gRender.Cutout.Hi[lInc] := gBGImg.ScrnDim[lInc];
	end;}
	for lInc := 1 to 3 do begin
		gRender.CutoutFrac.Lo[lInc] := kMaxFrac div 2;
		gRender.CutoutFrac.Hi[lInc] := kMaxFrac;
	end;
	ReadRenderIniFile (gRenderStartupFilename);
	UpdateRenderMRU;
	UpdateRenderDisplay;
 RenderForm.BringToFront;
end;


function RAMok (var lBGImg: TBGImg): boolean;
var
        lOutDim,lOutBytes,lBytesNeeded,lFreeRam: int64;
        lBGSz,lC: integer;
begin
        lBGSz := lBGImg.ScrnDim[1]*lBGImg.ScrnDim[2]*lBGImg.ScrnDim[3];
        lOutDim := round(MaxDim(lBGImg.ScrnDim[1],lBGImg.ScrnDim[2],lBGImg.ScrnDim[3]) * gRender.Zoom);
        lOutBytes := lOutDim*lOutDim*lOutDim;
        lBytesNeeded := 0;
        for lC := 0 to knMaxOverlay do begin
                if  (gMRIcroOverlay[lC].ScrnBufferItems >= lBGSz) then begin

                    lBytesNeeded := lBytesNeeded + (lOutBytes - gMRIcroOverlay[lC].RenderBufferItems);
                end;

        end;
        if (lBytesNeeded > freeRam) then begin
                beep;
                ImgForm.StatusLabel.Caption := 'Memory exhausted: unable to render at this quality';
                result := false;

        end else
                result := true;
end;

procedure  TRenderForm.RefreshRotation;
var
   lC: integer;
   lMatrix: TMatrix;
   lStartTime: DWord;
begin
  lMatrix := AziElevMatrix;
  {$IFNDEF FPC} //refresh causes flicker with lazarus
  	Application.processmessages;
  	Refresh;
  {$ENDIF}
  gRender.Zoom := gZoom; //11/2007b
  gZoom := 1;
  gRender.ClipFrac := ClipTrack.position;
  gRender.Bias := BiasTrack.position;
  gRender.Gain := GainTrack.Position;
  gRender.Azimuth := round(AzimuthEdit.value);
  gRender.Elevation := round(ElevationEdit.value);

  if not RAMok(gBGImg) then exit;
  lStartTime := GetTickCount;
  VolumeRotateMatrix (gBGImg, gMRIcroOverlay[0],lMatrix, gRender.Trilinear,gRender.ShowCutout,true{,round(gRender.BGNearClip*gRender.Zoom)});
  for lC := 1 to knMaxOverlay do
	VolumeRotateMatrix (gBGImg, gMRIcroOverlay[lC],lMatrix, gRender.Trilinear,false,false{,round(gRender.OverlayNearClip*gRender.Zoom)});
end;
var
   gRendering: boolean = false;

procedure TRenderForm.RenderRefreshTimerTimer(Sender: TObject);
begin
	if gMRIcroOverlay[0].ScrnBufferItems=0 then begin
	   RenderRefreshTimer.Enabled := false;
		RenderImage.Width := 0;
		exit;
	end;
        if gRendering then exit;
       	RenderRefreshTimer.Enabled := false;

          gRender.ShadePct := ShadeEdit.value;
        gRendering := true;

      if (gMRIcroOverlay[0].RenderBufferItems=0) or (RenderRefreshTimer.Tag <> 0) or (AzimuthEdit.value<>gRender.Azimuth) or (ElevationEdit.value<>gRender.Elevation) then
		RefreshRotation;
        //RenderRefreshTimer.Enabled := false;
        (*if RenderRefreshTimer.Enabled  then begin
            gRendering := false;
            exit;
        end;     *)
	RenderRefreshTimer.Tag := 0;

	DrawRender;
 //RenderRefreshTimer.Enabled := false;
 gRendering := false;
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
     lImage.Picture.Graphic := RenderForm.RenderImageBUP.Picture.Graphic;
     {$IFNDEF Darwin}
       //make sure next line required on this OS!
     {$ENDIF}
     lImage.Canvas.Draw(0,0,RenderForm.RenderImageBUP.Picture.Graphic);
     //lImage.Picture.Bitmap := RenderForm.RenderImageBUP.Picture.Bitmap;    //xxxx
         //redraw image even if not drawing X-bar: hide visible X-bar if use toggles X-bars off.
         if not ImgForm.XBarBtn.Down then
            exit; //only draw xbars if requested
         //lImage.Refresh;
         lZoomPct := 100; //ImageZoomPct(lImage);
         lL := (lHorPos * lZoomPct) div 100;
	 lT := (lVerPos * lZoomPct) div 100;
	 lW := lImage.Width;// div 100;
	 lH := lImage.Height;// div 100;
         lImage.Canvas.Pen.Color:= gBGImg.XBarClr;
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
     RenderDrawXBar ( X,Y,RenderImage);
	//Next: find coordinates for orthogonal views:
	lZoom := ImageZoomPct(RenderImage);
	lXrender := round((X*100) / lZoom );
	lYrender := round(((Y)*100) / lZoom );
	lPixelOffset := lXrender+ ((gBGImg.RenderDim-lYrender)*gBGImg.RenderDim);
	//ImgForm.StatusLabel.caption := inttostr(lXrender)+'x'+inttostr(lYrender)+' ->  '+inttostr(gMRIcroOverlay[kBGOverlayNum].RenderDepthBufferItems );
	if (lPixelOffset < 1) or (lPixelOffset >gBGImg.RenderDepthBufferItems ) then exit;
	lZrender := gBGImg.RenderDepthBuffer^[lPixelOffset];

        lXrender := round(lXrender / gRender.Zoom);
        lYrender := round(lYrender / gRender.Zoom);
        lZrender := round(lZrender / gRender.Zoom);
 lMatrix := AziElevMatrix;
	InvertMatrixPoint (gBGImg,lMatrix,lXrender,lYrender,lZrender, lXout,lYout,lZOut);
	ImgForm.XViewEdit.value := lXOut;
	ImgForm.YViewEdit.value := lYOut;
	ImgForm.ZViewEdit.value := lZOut;
        {$IFDEF FPC}
  ImgForm.XViewEditChange(nil);
  {$ENDIF}
end;

procedure TRenderForm.Cutout1Click(Sender: TObject);
begin
  CutoutForm.Show;
end;

procedure TRenderForm.Savesettings1Click(Sender: TObject);
begin
  //showmessage(gRenderDir+'   '+extractfiledir(gRenderDir));
  MultiSliceForm.MultiSaveDialog.InitialDir := extractfiledir(gRenderDir);
  MultiSliceForm.MultiSaveDialog.FileName :=  'a'+inttostr(gRender.Azimuth)+'e'+inttostr(gRender.Elevation);
  if not MultiSliceForm.MultiSaveDialog.Execute then exit;
  {$IFDEF Unix}
  WriteRenderIniFile(extractfiledir(gRenderDir)+pathdelim+extractfilename(MultiSliceForm.MultiSaveDialog.Filename));
  {$ELSE}
  WriteRenderIniFile(MultiSliceForm.MultiSaveDialog.Filename);
  {$ENDIF}
  UpdateRenderMRU;
end;

procedure TRenderForm.FormHide(Sender: TObject);
begin
	WriteRenderIniFile (gRenderDefaultsFilename);
     //not sure how to make this safe for currently rendering threads...
     if gBGImg.RenderDepthBufferItems > 0 then
		Freemem(gBGImg.RenderDepthBuffer);
	  gBGImg.RenderDepthBufferItems := 0;
       {$IFDEF Darwin}Application.MainForm.SetFocus;{$ENDIF}
end;

procedure TRenderForm.FormCreate(Sender: TObject);
begin
     {$IFDEF Linux}ImgForm.InitImg(RenderImage);{$ENDIF}
     {$IFDEF Darwin}
     Save1.ShortCut := ShortCut(Word('S'), [ssMeta]);
     Close1.ShortCut := ShortCut(Word('W'), [ssMeta]);
     {$ENDIF}
     gRenderDir  := DefaultsDir('render');
     {$IFDEF Darwin}
     if not fileexists(gRenderDir) then
        gRenderDir := AppDir + 'render';
     //showmessage(gTemplateDir);
     {$ENDIF}
     //showmessage(gRenderDir);
     //gRenderDir := extractfiledir(paramstr(0))+pathdelim+'render'+pathdelim;
     gRenderDefaultsFilename := gRenderDir + 'default.ini';
     gRenderStartupFilename := gRenderDefaultsFilename;
     RenderForm.DoubleBuffered := true;
end;

procedure TRenderForm.RefreshClick(Sender: TObject);
begin
      gZoom := 2;
      	RenderForm.RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
	RenderForm.RenderRefreshTimer.enabled := true;
end;

procedure TRenderForm.SetSearch(Sender: TObject);
begin
	(sender as TMenuItem).checked := true;
	gRender.OverlayFromBGSurface := (sender as TMenuItem).tag;
	RenderRefreshTimer.Enabled := true;
end;




end.
