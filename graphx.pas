unit graphx;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface
{$DEFINE noFFTs}
uses
{$IFDEF FFTs}
  FFTs,
{$ENDIF}
{$IFDEF FPC}
LResources,
Spin,
{$ELSE}
ShlObj,Windows,RXSpin,
{$ENDIF}
   Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ToolWin, ComCtrls,define_types, ExtCtrls,Text, StdCtrls,
  perisettings, Menus,ClipBrd,metagraph,periplot,userdir;

Type

  { TGraph4DForm }

  TGraph4DForm = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    Edit1: TMenuItem;
    CopyMenu: TMenuItem;
    CloseMenu: TMenuItem;
    FSLBatchMenu: TMenuItem;
    FFTMenu: TMenuItem;
    Extract4Drois: TMenuItem;
    BatchMenu: TMenuItem;
    SaveMenu: TMenuItem;
    OpenMenu: TMenuItem;
    MinEdit: TFloatSpinEdit;
    MaxEdit: TFloatSpinEdit;
    HSpeedDrop: TComboBox;
    PlotBtn: TSpeedButton;
    TextBtn: TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    TrackBar1: TTrackBar;
    TREdit: TFloatSpinEdit;
    FourDBar: TPanel;
    TRLabel: TLabel;
    OpenDataBtn: TSpeedButton;
    RefreshBtn: TSpeedButton;


    //procedure Plot4DFFT(lStartSample: integer);
    //function XL: boolean;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function ReadGraf(lFilename: string; lBatch,lTRcritical: boolean): boolean;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Plot4DTrace(lStartSample: integer);
    procedure TextBtnClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure OpenDataClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PSPlotClick(Sender: TObject);
    procedure PSTextClick(Sender: TObject);
    //procedure rfx;
    procedure Copy1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Closewindow1Click(Sender: TObject);
    procedure SaveasEMF1Click(Sender: TObject);
    procedure FFTitemClick(Sender: TObject);
    procedure RefreshBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Extract4DroisClick(Sender: TObject);
    procedure Batchdata1Click(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure FSLbatch1Click(Sender: TObject);
    procedure FSLtest1Click(Sender: TObject);

  private

  public
    { Public declarations }
  end;

var
  Graph4DForm: TGraph4DForm;

implementation

uses
    nifti_img_view, nifti_img,nifti_hdr, nifti_hdr_view,periutils, reslice_fsl;
const
     //kMaxCond = 6;
     kMaxLines = kMaxCond* knMaxOverlay;
     //kClrRA: array [1..kMaxCond] of TColor = (clRed,clBlue,clGreen,clTeal,clAqua,clSilver);
     //kPenStyleRA: array[1..kVOIOverlayNum] of TPenStyle = (psDot,psDot,psDash,psDashDot,psDashDotDot);//abba
     //kPenStyleRA: array[1..kVOIOverlayNum] of TPenStyle = (psSolid,psDot,psDash,psDashDot,psDashDotDot);

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}
var
   g4DHdr: TMRIcroHdr;
   g4Ddata: T4DTrace;


(*procedure PrepPlot(var lImage: TMetafileCanvas; lL,lT,lR,lB,lWid,lHt,lFontSize: integer);
begin
     lImage.Font.Name := 'Arial';
     lImage.Font.Size := 12;

     lImage.pen.color := clBlack;
     lImage.Font.color := clBlack;
     lImage.Brush.Style := bsSolid;
     lImage.Brush.color := clWhite;
     lImage.Rectangle(1,1,lWid,lHt);
     lImage.Rectangle(lL,lT,lR,lB);
end;  *)


{$IFDEF FFTs}
procedure ROI2FFT (var l4DHdr: TMRIcroHdr; lROInum: integer; var  lFFTLines: SingleP);
var

   lVolSz,lnVol,lVol,lVox,lCount,lVolOffset,lnFFTOut,lP: integer;
   l16Buf : SmallIntP;
   lFFT,lFFTOut,l32Buf : SingleP;
   lFFTsum: doubleP;
begin
     lnVol := l4DHdr.NIFTIhdr.dim[4];
     if lnVol < 5 then
        exit;
     lVolSz :=l4DHdr.NIFTIhdr.dim[1]*l4DHdr.NIFTIhdr.dim[2]*l4DHdr.NIFTIhdr.dim[3];
     Getmem(lFFT,(lnVol) * Sizeof(Single));
     lnFFTout := ((lnVol) div 2)-1 ;
     Getmem(lFFTout,(lnFFTout) * Sizeof(Single));
     Getmem(lFFTsum,(lnFFTout) * Sizeof(double));
     for lP := 1 to lnFFTout do
         lFFTSum[lP] := 0;

               for lP := 1 to lnFFTout do
                   lFFTout[lP] := 0;
     lVolOffset := lVolSz;
     //next - compute sum of signal - unrolled loops for each datatype
     lCount := 0;
     if (l4DHdr.ImgBufferBPP  = 4) then begin
	l32Buf := SingleP(l4DHdr.ImgBuffer );
        for lVox := 1 to lVolSz do begin
            if gMRIcroOverlay[lROInum].ScrnBuffer[lVox] > 0 then begin
               for lVol := 1 to lnVol do
                   lFFT[lVol] := l32Buf[lVox+((lVol-1)*lVolOffset)];
               FFTPower(lFFT,lFFTout,lnVol);
               for lP := 1 to lnFFTout do
                   lFFTSum[lP] := lFFTSum[lP]+lFFTout[lP];
               inc(lCount);
            end; //part of ROI
        end; //for each vox
     end else if (l4DHdr.ImgBufferBPP  = 2) then begin
        l16Buf := SmallIntP(l4DHdr.ImgBuffer );
        for lVox := 1 to lVolSz do begin
            if gMRIcroOverlay[lROInum].ScrnBuffer[lVox] > 0 then begin
               for lVol := 1 to lnVol do
                   lFFT[lVol] := l16Buf[lVox+((lVol-1)*lVolOffset)];
               FFTPower(lFFT,lFFTout,lnVol);
               //FFTPower(lFFT,lFFx,lnVol);
               for lP := 1 to lnFFTout do
                   lFFTSum[lP] := lFFTSum[lP]+lFFTout[lP];
               inc(lCount);
            end; //part of ROI
        end; //for each vox
     end else if l4DHdr.ImgBufferBPP  = 1 then begin
        for lVox := 1 to lVolSz do begin
            if gMRIcroOverlay[lROInum].ScrnBuffer[lVox] > 0 then begin
               for lVol := 1 to lnVol do
                   lFFT[lVol] := l4DHdr.ImgBuffer[lVox+((lVol-1)*lVolOffset)];
               FFTPower(lFFT,lFFTout,lnVol);
               for lP := 1 to lnFFTout do
                   lFFTSum[lP] := lFFTSum[lP]+lFFTout[lP];
               inc(lCount);
            end; //part of ROI
        end; //for each vox
     end else
         showmessage('Serious error: unknown data size!');
     //now compute mean signal
     if lCount > 0 then begin
        for lP := 1 to lnFFTout do
            lFFTSum[lP] := lFFTSum[lP] / lCount;
        for lP := 1 to lnFFTout do
            if specialdouble(lFFTSum[lP]) then
               lFFTSum[lP] := 0;
     end;
      for lP := 1 to lnFFTout do
            lFFTLines[lP] :=  lFFTSum[lP];
   freemem(lFFT);
   freemem(lFFTout);
   freemem(lFFTsum);
end;

procedure Plot4DFFT(lStartSample: integer);
var
    //lDataOut: SingleP;
    lLines,N,I: Integer;
    l4DTrace: T4DTrace;
begin
     if (g4dData.lines[1].events < 5)  then exit;
     lLines := 1;
     for I := 2 to kMaxLines do
         if g4dData.lines[I].events =g4dData.lines[1].events  then
            inc(lLines);
     N := g4dData.lines[1].events;
     N := (N div 2)-1;
     Create4DTrace ( l4DTrace);
     Init4DTrace(N,lLines,l4DTrace,false);
     lLines := 0;
     for I := 1 to kMaxLines do
         if g4dData.lines[I].events =g4dData.lines[1].events  then begin
            inc(lLines);
            l4DTrace.lines[lLines].eLabel := ROIoverlayNameShort(0);// g4dData.lines[I].eLabel;
            N := g4dData.lines[I].events;
            FFTPower(g4dData.lines[I].EventRA,l4DTrace.lines[lLines].EventRA,N);
         end; //events[i] = events[1]
   MinMax4DTrace(l4dtrace);
   l4dtrace.HorzMin := 0;
   //range will be 0.. 1/TR*Nyquist Sec/Cycle
   if Graph4DForm.TREdit.value = 0 then
       l4dtrace.HorzWidPerBin := (0.5)/(l4dTrace.lines[1].events-1)
   else
       l4dtrace.HorzWidPerBin := ((1/Graph4DForm.TREdit.value)*0.5)/(l4dTrace.lines[1].events-1);
   CorePlot4DTrace(l4Dtrace,Graph4DForm.Image1,lStartSample,Graph4DForm.HSpeedDrop.ItemIndex,-1,Graph4DForm.TREdit.value,Graph4DForm.MinEdit.value,Graph4DForm.MaxEdit.value,false);
   Close4DTrace(l4Dtrace,true);
end;


procedure FFT4ROI (var l4DHdr: TMRIcroHdr);
var
 l4DTrace: T4DTrace;
 lnROI,lROI,lnVol,lnFFTOut: integer;
begin
     lnVol := l4DHdr.NIFTIhdr.dim[4];
     if lnVol < 5 then
        exit;
     lnROI := numROI;
     if lnROI < 1 then begin
        Plot4DFFT(1);
        exit;
     end;
    Create4DTrace ( l4DTrace);
    lnFFTout := (lnVol div 2) -1;
    Init4DTrace(lnFFTout,lnROI,l4DTrace,false);
    for lROI := 1 to lnROI do begin
        ROI2FFT(l4DHdr,ROIoverlayNum(lROI),l4DTrace.Lines[lROI].EventRA);
        l4DTrace.Lines[lROI].elabel := ROIoverlayNameShort(lROI);
    end;
   MinMax4DTrace(l4dtrace);
   l4dtrace.HorzMin := 0;
   //range will be 0.. 1/TR*Nyquist Sec/Cycle
   if Graph4DForm.TREdit.value = 0 then
       l4dtrace.HorzWidPerBin := (0.5)/(l4dTrace.lines[1].events-1)
   else
       l4dtrace.HorzWidPerBin := ((1/Graph4DForm.TREdit.value)*0.5)/(l4dTrace.lines[1].events-1);
   CorePlot4DTrace(l4Dtrace,Graph4DForm.Image1,1,Graph4DForm.HSpeedDrop.ItemIndex,-1,Graph4DForm.TREdit.value,Graph4DForm.MinEdit.value,Graph4DForm.MaxEdit.value,false);
   Close4DTrace(l4Dtrace,true);
end;
{$ENDIF}

procedure TGraph4DForm.Plot4DTrace(lStartSample: integer);
begin


     g4Ddata.HorzWidPerBin := TREdit.value;
     CorePlot4DTrace(g4Ddata,Image1,lStartSample,HSpeedDrop.ItemIndex,-1,TREdit.value,MinEdit.value,MaxEdit.value,false);
     //StatusBar1.Panels[1].Text := 'Offset:'+inttostr(lStartSample);
     //ShowLegend(g4Ddata,Image1, 50,5);
end;

procedure TGraph4DForm.TextBtnClick(Sender: TObject);
begin

end;



procedure TextToTrace (var l4DTrace: T4DTrace);
var
   lStr: string;
   lCond, lnCond,lE: integer;
begin
     lncond := 0;
     for lCond := 1 to kMaxCond do
         if l4DTrace.Lines[lCond].Events > 0 then
            inc(lnCond);
     if lncond = 0 then
     exit;
     for lCond := 1 to kMaxCond do begin
         if l4DTrace.Lines[lCond].Events > 0 then begin
            lStr := gMRIcroOverlay[kBGOverlayNum].HdrFileName+kTextSep+l4DTrace.Lines[lCond].ELabel;
            for lE := 1 to l4DTrace.Lines[lCond].Events do
                lStr := lStr + kTextSep+ realtostr(l4DTrace.Lines[lCond].EventRA^[lE],4) ;
            TextForm.MemoT.lines.add(lStr);

         end;
     end;
end;

function TGraph4DForm.ReadGraf(lFilename: string; lBatch,lTRcritical: boolean): boolean;
label
     666;
var
   lnVol: integer;
   lReslice : boolean;
begin
     ImgForm.CloseImagesClick(nil);
     Close4DTrace(g4Ddata,true);
     FreeImgMemory(g4DHdr);
     result := false;
     if not fileexists(lFilename) then exit;
     Graph4DForm.Caption := 'Viewing: '+lFilename;
     lReslice := gBGImg.ResliceOnLoad;
     gBGImg.ResliceOnLoad := false;
     gBGImg.Prompt4DVolume := false;
     //if not lBatch then begin  //12/2007
    ImgForm.OpenAndDisplayImg(lFilename,True);
    lnVol := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[4];
    if (lnVol < 2) then begin
           showmessage('You need to open a 4D image.');
           goto 666;
    end;
    if not HdrForm.OpenAndDisplayHdr(lFilename,g4DHdr) then goto 666;
    if not OpenImg(gBGImg,g4DHdr,false,false,false,false,true {4D!}) then goto 666;
    TrackBar1.Max := lnVol;
    if gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.PixDim[4] = 0 then begin
      beep;
      ImgForm.StatusLabel.caption := 'Assuming TR = '+floattostr(TREdit.value);
    end else
        TREdit.value := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.PixDim[4];//TR
    if (TREdit.value = 0) and (lTRcritical) then
       showmessage('Please set the TR value [in seconds]');
     result := true;
666:
     gBGImg.ResliceOnLoad := lReslice;
     gBGImg.Prompt4DVolume := true;
end;

procedure TGraph4DForm.FormShow(Sender: TObject);
begin
end;

procedure TGraph4DForm.FormHide(Sender: TObject);
begin
    {$IFDEF Darwin}Application.MainForm.SetFocus;{$ENDIF}
end;

(*{x$IFDEF FFTs}
procedure TGraph4DForm.FormShow(Sender: TObject);
var
   lFilename: string;
begin
     //abba
     lFilename := 'C:\cygwin\home\mscae\20061220_140508\';
     //ReadCond(lFilename+'puls.txt',g4Ddata,1);
     //ReadCond(lFilename+'resp.txt',g4Ddata,2);
     HdrForm.OpenHdrDlg.Filename := lFilename+'rachris.nii.gz';
     ReadGraf(HdrForm.OpenHdrDlg.Filename );
     ImgForm.XViewEdit. value := 43;
     ImgForm.YViewEdit. value := 37;
     ImgForm.ZViewEdit. value := 22;
     PSForm.BinWidthEdit.value := 0.1;
     PSForm.PreBinEdit.value := 5;
     PSForm.PostBinEdit.value := 5;
     lFilename :=  'C:\cygwin\home\mscae\20061220_140508\ravoi.voi';
     //ImgForm.OverlayOpenCore ( lFilename, kBGOverlayNum+1);

     RefreshBtnClick(nil);
end;
{x$ELSE} //no FFT

procedure TGraph4DForm.FormShow(Sender: TObject);
var
   lFilename: string;
   //lReslice : boolean;
begin
     //lReslice :=gReslice;
     //gReslice := false;
    ReadCond(extractfiledir(paramstr(0))+'\L_Tap.txt',g4Ddata,1);
     ReadCond(extractfiledir(paramstr(0))+'\R_Tap.txt',g4Ddata,2);
     HdrForm.OpenHdrDlg.Filename := extractfiledir(paramstr(0))+'\filtered_func_data.nii.gz';
     ReadGraf(HdrForm.OpenHdrDlg.Filename );
     ImgForm.XViewEdit. value := 42;
     ImgForm.YViewEdit. value := 29;
     ImgForm.ZViewEdit. value := 28;

     lFilename := extractfiledir(paramstr(0))+'\Left.voi';
     ImgForm.OverlayOpenCore ( lFilename, kBGOverlayNum+1);
     //VR( lFilename, 1);
     //ImgForm.OpenVOICore(lFilename);
     lFilename := extractfiledir(paramstr(0))+'\Right.voi';
     ImgForm.OverlayOpenCore ( lFilename,kBGOverlayNum+2);
     //VR(lFilename,2);
     RefreshBtnClick(nil);
     //gReslice := lReslice;
end;
{x$ENDIF}
*)


procedure TGraph4DForm.FormCreate(Sender: TObject);
begin
     {$IFNDEF FPC}
     gWmf := TMetafile.Create;
  gWmf.Enhanced := True;
  {$ENDIF}
     Create4DTrace(g4Ddata);
     Graph4DForm.DoubleBuffered := true;
     HSpeedDrop.ItemIndex := 0;
     InitImgMemory(g4DHdr);
end;

procedure TGraph4DForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

     Close4DTrace(g4Ddata,true);
     FreeImgMemory(g4DHdr);
     //gWmf.Free;
end;

procedure TGraph4DForm.TrackBar1Change(Sender: TObject);
begin
        Trackbar1.visible := (HSpeedDrop.ItemIndex > 0);
     Plot4DTrace(TrackBar1.position);
end;

{$DEFINE notTest4D}
procedure TGraph4DForm.OpenDataClick(Sender: TObject);
var
   lI,lCnt: integer;
   lStr: string;
begin
     Close4DTrace(g4Ddata,true);
     FreeImgMemory(g4DHdr);
{$IFDEF Test4D}
     if not ReadGraf('C:\tx\20091006\fsl\filtered_func_data.nii.gz',false,true) then exit;
     ReadCond('C:\tx\20091006\fsl\timing.txt',g4Ddata,1);
     //ReadCond('C:\fatigue\TD\b.txt',g4Ddata,2);
     PSPlotClick(nil);
     exit;

     if not ReadGraf('C:\fatigue\perisample\filtered_func_data.nii.gz',false,true) then exit;
     ReadCond('C:\fatigue\perisample\L_Tap.txt',g4Ddata,1);
     ReadCond('C:\fatigue\perisample\R_Tap.txt',g4Ddata,2);
     lI := 1;
     lStr :=  'C:\fatigue\perisample\left.voi';
     ImgForm.OverlayOpenCore(lStr,lI+kBGOverlayNum);
     PSPlotClick(nil);
     exit;
{$ENDIF}
     if not OpenDialogExecute(kImgFilter,'Select 4D image',false) then exit;
     if not ReadGraf(HdrForm.OpenHdrDlg.Filename,false,true) then exit;
     ImgForm.XViewEdit.value := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[1] div 2;
     ImgForm.YViewEdit.value := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[2] div 2;
     ImgForm.ZViewEdit.value := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[3] div 2;
     if OpenDialogExecute(kTxtFilter,'Select 3-column event onset time files [optional]',true) then begin
        if HdrForm.OpenHdrDlg.Files.Count > 0 then begin
           lCnt := HdrForm.OpenHdrDlg.Files.Count;
           if lCnt > kMaxCond then begin
               showmessage('Can only load '+inttostr(kMaxCond)+'conditions');
               lCnt := kMaxCond;
           end;
           for lI := 1 to lCnt do
               ReadCond(HdrForm.OpenHdrDlg.Files[lI-1],g4Ddata,lI);
        end;//if count > 1
     end; //if opendialog
     if OpenDialogExecute(kImgPlusVOIFilter,'Select regions of interest',true) then begin
        if HdrForm.OpenHdrDlg.Files.Count > 0 then begin
           lCnt := HdrForm.OpenHdrDlg.Files.Count;
           //Apr07
           if lCnt > (knMaxOverlay-2) then begin
               showmessage('Can only load '+inttostr(knMaxOverlay-2)+'conditions');
               lCnt := knMaxOverlay;
           end;
           for lI := 1 to lCnt do begin
               lStr := HdrForm.OpenHdrDlg.Files[lI-1];
               ImgForm.OverlayOpenCore(lStr,lI+kBGOverlayNum);
           end;
        end;//if count > 1
     end; //if opendialog
          RefreshBtnMouseDown(nil,mbleft,[],1,1);

end;

procedure TGraph4DForm.FormResize(Sender: TObject);
begin
     if not Graph4DForm.visible then
        exit;
     GraphResize(Image1);
     Plot4DTrace(TrackBar1.position);
end;

procedure TGraph4DForm.PSPlotClick(Sender: TObject);
var
lPSPlot: TPSPlot;
{var
lTRSec,lBinWidthSec: single; lnNegBins,lnPosBins: integer; lSliceTime,
lSavePSVol,lTextOutput,lGraphOutput,lBaselineCorrect,lPctSignal,
lRemoveRegressorVariability,lTemporalDeriv,lPlotModel: boolean;  }
begin
     if NCond ( g4Ddata) < 1 then begin
        RefreshBtnMouseDown(nil,mbleft,[],1,1);
        exit;
     end;
     lPSPlot.TRSec := TREdit.value;
     if not PSForm.GetPeriSettings(lPSPlot) then
        exit;
     lPSPlot.TextOutput := false;
    lPSPlot.GraphOutput := true;
    lPSPlot.batch := false;
     CreatePeristimulusPlot (g4DHdr,g4Ddata, lPSPlot);
end;

procedure TGraph4DForm.PSTextClick(Sender: TObject);
var lPSPlot: TPSPlot;
begin
     if NCond ( g4Ddata) < 1 then begin
        RefreshBtnMouseDown(nil,mbleft,[],1,1);
        TextForm.MemoT.Lines.Clear;//prepare to report results
        TextToTrace (g4Ddata);
        TextForm.show;
        exit;
     end;
     lPSPlot.TRSec := TREdit.value;
     if not PSForm.GetPeriSettings(lPSPlot) then
        exit;
     lPSPlot.TextOutput := true;
     lPSPlot.GraphOutput := true;
     lPSPlot.batch := false;
     CreatePeristimulusPlot (g4DHdr,g4Ddata, lPSPlot);
end;

procedure TGraph4DForm.Copy1Click(Sender: TObject);
{$IFDEF FPC}
begin
     if (Image1.Picture.Graphic = nil) then begin //1420z
    Showmessage('You need to generate an image before you can copy it to the clipboard.');
    exit;
 end;
    Image1.Picture.Bitmap.SaveToClipboardFormat(2);
end;

{$ELSE}
var
  MyFormat : Word;
  AData: THandle;
  APalette : HPalette;
begin
 if gWMF.Empty then begin
     showmessage('Please Open a dataset first.');
     exit;
 end;
 gWmf.SaveToClipboardFormat(MyFormat,AData,APalette);
 ClipBoard.SetAsHandle(MyFormat,AData);
end;
{$ENDIF}


procedure TGraph4DForm.FormDestroy(Sender: TObject);
begin
//gWmf.Free;
end;

procedure TGraph4DForm.Closewindow1Click(Sender: TObject);
begin
     Graph4DForm.Close;
end;

procedure TGraph4DForm.SaveasEMF1Click(Sender: TObject);
begin
 {$IFDEF FPC}
   	SaveImgAsPNGBMP (Image1);
 {$ELSE}
 if gWMF.Empty then begin
     showmessage('Please Open a dataset first.');
     exit;
 end;
	ImgForm.SaveDialog1.Filter := 'Enhanced Metafile|*.emf';
	ImgForm.SaveDialog1.DefaultExt := '*.emf';
	if not ImgForm.SaveDialog1.Execute then exit;
        gWmf.SaveToFile (ChangeFileExt(ImgForm.SaveDialog1.FileName,'.emf'));
 {$ENDIF}
end;

procedure TGraph4DForm.FFTitemClick(Sender: TObject);
begin
{$IFDEF FFTs}
 FFT4ROI (g4DHdr);
 exit;
{$ENDIF}
showmessage('FFT not included with this build.');
end;

procedure TGraph4DForm.RefreshBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
      if (g4DHdr.ImgBufferItems = 0) then begin
         showmessage('You must first load 4D data [Press the ''Open Data'' button.');
         exit;
      end;
       ConvertToTrace(g4DHdr,g4Ddata,ImgForm.XViewEdit.value,ImgForm.YViewEdit.value,ImgForm.ZViewEdit.value);
         Plot4DTrace(TrackBar1.position);
end;

procedure TGraph4DForm.Extract4DroisClick(Sender: TObject);
const
     kMin8bit = 0;
     kMax8bit = 255;
var
   lROInum,lVol,lnVol,lPos,lROI,lVolSz,lVolOffset: integer;
   lStr: string;
   SumRA : array [kMin8bit..kMax8bit] of double;
   nRA : array [kMin8bit..kMax8bit] of longint;
   l16Buf : SmallIntP;
   l32Buf : SingleP;
   lOutStr: string;
begin
     Close4DTrace(g4Ddata,true);
     FreeImgMemory(g4DHdr);
     if not OpenDialogExecute(kImgFilter,'Select 4D image',false) then exit;
     if not ReadGraf(HdrForm.OpenHdrDlg.Filename,false,true) then exit;
     ImgForm.XViewEdit.value := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[1] div 2;
     ImgForm.YViewEdit.value := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[2] div 2;
     ImgForm.ZViewEdit.value := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[3] div 2;
     lVolSz :=  gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[1]*gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[2]*gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[3];
     if not OpenDialogExecute(kImgPlusVOIFilter,'Select regions of interest',false) then
        exit;
     lROInum :=  1+kBGOverlayNum;
     lStr := HdrForm.OpenHdrDlg.Filename;
     ImgForm.OverlayOpenCore(lStr,lROInum);
     if gMRIcroOverlay[lROInum].ImgBufferBPP  <> 1 then begin
        showmessage('Overlay must be 8-bit image');
        exit;
     end;
     if (gMRIcroOverlay[lROInum].ImgBufferItems  <> lVolSz) or (lVOlSz < 1) then begin
        showmessage('Overlay must have identical dimensions as 4D image');
        exit;
     end;
     lnVol := gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.dim[4];
     if lnVol < 2 then begin
         showmessage('Requires 4D data');
         exit;
     end;
     if (g4DHdr.ImgBufferItems  <> ({lnVol*}lVolSz)) then begin
        showmessage('4D image not loaded correctly '+inttostr(g4DHdr.ImgBufferItems)+'  <> '+inttostr(lVolSz));
        exit;
     end;
      TextForm.MemoT.Lines.Clear;//prepare to report results
     //count frequency of each column...
     for lPos := kMin8Bit to kMax8bit do
             nRA[lPos] := 0;
     for lPos := 1 to lVolSz do begin
         lROI := gMRIcroOverlay[lROInum].ImgBuffer^[lPos]; //ROI must be 8-bit!
         nRA[lROI] := nRA[lROI] + 1;
     end;
     //report detected ROI volumes
     lOutStr := 'vol';
     for lROI := kMin8Bit to kMax8bit do
         if nRA[lROI] > 0 then
            lOutStr := lOutStr+kTextSep+inttostr(nRA[lROI]);
     TextForm.MemoT.lines.add(lOutStr);
     //report detected ROIs [column labels]
     lOutStr := 'ROI';
     for lROI := kMin8Bit to kMax8bit do
         if nRA[lROI] > 0 then
            lOutStr := lOutStr+kTextSep+inttostr(lROI);
     TextForm.MemoT.lines.add(lOutStr);
     //compute mean intensity for each ROI at each timepoint
     l32Buf := SingleP(g4DHdr.ImgBuffer);
     l16Buf := SmallIntP(g4DHdr.ImgBuffer);
     for lVol := 1 to lnVol do begin
         lVolOffset := (lVol-1)*lVolSz;
         for lPos := kMin8Bit to kMax8bit do //initialize all ROIs for this volume
             SumRA[lPos] := 0;
         if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 4) then begin
             for lPos := 1 to lVolSz do begin
                 lROI := gMRIcroOverlay[lROInum].ImgBuffer^[lPos]; //ROI must be 8-bit!
                 SumRA[lROI] := SumRA[lROI] + l32Buf^[lPos+lVolOffset];
             end;
         end else if (gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 2) then begin
             for lPos := 1 to lVolSz do begin
                 lROI := g4DHdr.ImgBuffer^[lPos]; //ROI must be 8-bit!
                 SumRA[lROI] := SumRA[lROI] + l16Buf^[lPos+lVolOffset];
             end;
         end else if gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP  = 1 then begin
             for lPos := 1 to lVolSz do begin
                 lROI := gMRIcroOverlay[lROInum].ImgBuffer^[lPos]; //ROI must be 8-bit!
                 SumRA[lROI] := SumRA[lROI] + gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lPos+lVolOffset];
             end;
         end else begin
             showmessage('Serious error: unsupported datatype!');
             exit;
         end;
         lOutStr := inttostr(lvol);
         for lROI := kMin8Bit to kMax8bit do
             if nRA[lROI] > 0 then
                lOutStr := lOutStr+kTextSep+realtostr(SumRA[lROI]/nRA[lROI],4);
         TextForm.MemoT.lines.add(lOutStr);
     end; //for each volume
     TextForm.show;
     RefreshBtnMouseDown(nil,mbleft,[],1,1);
end;

procedure TGraph4DForm.Batchdata1Click(Sender: TObject);
label
     111;
var
   lStr: string;
   l4D,lVectors,lVOI: TStringList;
   lPSPlot: TPSPlot;
   lImg,lI: integer;
   (*lTRSec,lBinWidthSec: single;
   lI,lImg,lnNegBins,lnPosBins: integer;
   lSliceTime,lSavePSVol,lTextOutput,lGraphOutput,lBaselineCorrect,lPctSignal,
   lRemoveRegressorVariability,lTemporalDeriv,lPlotModel: boolean; *)
begin
     ImgForm.CloseImagesClick(nil);
     Close4DTrace(g4Ddata,true);
     FreeImgMemory(g4DHdr);
     if not OpenDialogExecute(kImgFilter,'Select 4D images',true) then exit;
     l4D := TStringList.Create;
     lVectors := TStringList.Create;//empty
     lVOI := TStringList.Create;
     l4D.AddStrings(HdrForm.OpenHdrDlg.Files);
     if  OpenDialogExecute(kTxtFilter,'Select 3-column event onset time files',true) then begin
         if HdrForm.OpenHdrDlg.Files.Count > kMaxCond then begin
               showmessage('Can only load '+inttostr(kMaxCond)+'conditions');
               goto 111;
         end;
         lVectors.AddStrings(HdrForm.OpenHdrDlg.Files);
     end;
     if not OpenDialogExecute(kImgPlusVOIFilter,'Select region[s] of interest',true) then
        goto 111;
     if HdrForm.OpenHdrDlg.Files.Count > (knMaxOverlay-2) then begin
        showmessage('Can only load '+inttostr(knMaxOverlay-2)+'conditions');
        goto 111;
     end;
     lVOI.AddStrings(HdrForm.OpenHdrDlg.Files);
     if not ReadGraf(l4D[0],false, (lVectors.count > 0) ) then
        goto 111; //read first dataset to set TR!
     //get plot settings....
     lPSPlot.TRSec := TREdit.value;
     if lVectors.count > 0 then
        if not PSForm.GetPeriSettings(lPSPlot) then
           goto 111;
     lPSPlot.TextOutput := true;
     lPSPlot.GraphOutput := false;
     lPSPlot.Batch := true;
     TextForm.MemoT.Lines.Clear;//prepare to report results
     for lImg := 1 to l4D.Count do begin
         //showmessage(l4D[lImg-1]);
         if lImg > 1 then begin//we have already read 1st img
            Refresh;
            Close4DTrace(g4Ddata,true);
            ImgForm.CloseImagesClick(nil);
            FreeImgMemory(g4DHdr);
            if not ReadGraf(l4D[lImg-1],true,(lVectors.count > 0)) then
               goto 111; //read first dataset to set TR!
         end; //all except 1st image
         if lVectors.count > 0 then begin
            for lI := 1 to lVectors.count do
                ReadCond(lVectors[lI-1],g4Ddata,lI);
         end;//vectors > 0
         if lVOI.count > 0 then begin
            for lI := 1 to lVOI.count do begin
               lStr := lVOI[lI-1];
               ImgForm.OverlayOpenCore(lStr,lI+kBGOverlayNum);
            end;//for each VOI
         end; //VOI > 0
         if lVectors.Count > 0 then
          CreatePeristimulusPlot (g4DHdr,g4Ddata, lPSPlot)
         else begin
             // RefreshBtnMouseDown(nil,mbleft,[],1,1);
              ConvertToTrace(g4DHdr,g4Ddata,ImgForm.XViewEdit.value,ImgForm.YViewEdit.value,ImgForm.ZViewEdit.value);
             TextToTrace (g4Ddata);
             RegressTrace(g4Ddata);
         end;
     end;
          TextForm.show;
     111:
     lVOI.Free;
     lVectors.Free;
     l4D.Free;
end;

procedure TGraph4DForm.RefreshBtnClick(Sender: TObject);
begin
        RefreshBtnMouseDown(nil,mbleft,[],1,1);
end;





function ResliceFSLVOIs(var lFeatDirs,lVOI: TStringList): boolean;
//uses reslice
var
   lDir,lV: integer;
   lMatName,lFuncName,lReslicedVOIName:string;
begin
    result := false;
    if lFeatDirs.count < 1 then exit;
    if lVOI.count < 1 then exit;
    for lDir := 1 to (lFeatDirs.Count) do begin
        lMatName := FSLMatName (lFeatDirs[lDir-1]);
        lFuncName := FSLFuncName (lFeatDirs[lDir-1]);
        for lV := 1 to lVOI.Count do begin
            lReslicedVOIName := FSLReslicedVOIName (lFeatDirs[lDir-1], lVOI[lV-1]);
            if not ResliceImg (lFuncName,lVOI[lV-1],lMatName,lReslicedVOIName) then begin
                Showmessage('graphx reslice FSL failed.');
                exit;
            end;
        end;//for each VOI
    end;//for each Dir
    result := true;
end;
{$DEFINE notTEST}
function FindFEATFolders (var lFeatDirs:TStringList): boolean;
var
   lDir,lFeatPath: string;
   lSearchRec: TSearchRec;
begin
     result := false;
     {$IFDEF TEST}
     lDir := 'C:\cygwin\home\express';
     {$ELSE}
     //lDir := 'C:\cygwin\home\express';
     //lDir := SelectDirectory('Choose root folder that contains .feat folders', BIF_RETURNONLYFSDIRS);
     LDir := UserDataFolder;
     lDir := GetDirPrompt (lDir);
     {$ENDIF}
     if lDir = '' then exit;
     lFeatDirs := TStringList.Create;
     if FindFirst(lDir+pathdelim+'*'+'.feat', faAnyFile, lSearchRec) = 0 then begin
	    repeat
          if (faDirectory and lSearchRec.attr) = faDirectory then begin
             lFeatPath := lDir+pathdelim+lSearchRec.Name;
             if Fileexists(FSLMatName(lFeatPath)) and Fileexists(FSLFuncName(lFeatPath)) then
		        lFeatDirs.Add(lFeatPath)
             else
                 Showmessage('Can not find '+FSLMatName(lFeatPath) +' or '+FSLFuncName(lFeatPath) );
          end;
        until (FindNext(lSearchRec) <> 0);
     end;
     FindClose(lSearchRec);
     if lFeatDirs.Count < 1  then begin
        Showmessage('Unable to find any feat dirs in path '+lDir);
        lFeatDirs.free;
        exit;
     end;
     result := true;
end;


procedure TGraph4DForm.FSLbatch1Click(Sender: TObject);
label
     111;
var
   lStr: string;
   lFeatDirs,lVectors,lVOI: TStringList;
   //lTRSec,lBinWidthSec: single;
   lI,lImg: integer;
   lUseFSLEVs: Boolean;
   lPSPlot: TPSPlot;
   {lSliceTime,lSavePSVol,lTextOutput,lBaselineCorrect,lPctSignal,
   lRemoveRegressorVariability,lTemporalDeriv,lUseFSLEVs,lPlotModel: boolean; }
begin
     ImgForm.CloseImagesClick(nil);
     Close4DTrace(g4Ddata,true);
     FreeImgMemory(g4DHdr);
     if not FindFEATFolders (lFeatDirs) then
      exit;
     lVectors := TStringList.Create;//empty
     lVOI := TStringList.Create;
     {$IFDEF TEST}
     lUseFSLEVs := false;
lFeatDirs.AddStrings(lFeatDirs);
lFeatDirs.AddStrings(lFeatDirs);
lFeatDirs.AddStrings(lFeatDirs);
lFeatDirs.AddStrings(lFeatDirs);
lFeatDirs.AddStrings(lFeatDirs);
     lVectors.Add('C:\cygwin\home\express\20070420_132327fMRIcontin30x30x36s004a001.feat\custom_timing_files\ev1.txt');
     lVectors.Add('C:\cygwin\home\express\20070420_132327fMRIcontin30x30x36s004a001.feat\custom_timing_files\ev2.txt');
     lVectors.Add('C:\cygwin\home\express\20070420_132327fMRIcontin30x30x36s004a001.feat\custom_timing_files\ev3.txt');
     lVOI.Add('C:\fatigue\v1.nii.gz');
     lVOI.Add('C:\fatigue\v2.nii.gz');
          {$ELSE}
     FSLEVNames (lFeatDirs[0], lVectors);
     lUseFSLEVs := false;
     if lVectors.count > 0 then
        lUseFSLEVs := OKMsg('Use event vectors from the .FEAT'+pathdelim+'custom_timing_files folder?'); //shows dialog with OK/Cancel returns true if user presses OK
     if not lUseFSLEVs then begin
        lVectors.clear;
        if  OpenDialogExecute(kTxtFilter,'Select 3-column event onset time files',true) then begin
         if HdrForm.OpenHdrDlg.Files.Count > kMaxCond then begin
               showmessage('Can only load '+inttostr(kMaxCond)+'conditions');
               goto 111;
         end;
         lVectors.AddStrings(HdrForm.OpenHdrDlg.Files);
        end;
     end; //manually select EVs

     if not OpenDialogExecute(kImgPlusVOIFilter,'Select volume[s] of interest [2mm MNI space]',true) then
        goto 111;
     if HdrForm.OpenHdrDlg.Files.Count > (knMaxOverlay-2) then begin
        showmessage('Can only load '+inttostr(knMaxOverlay-2)+'conditions');
        goto 111;
     end;
     lVOI.AddStrings(HdrForm.OpenHdrDlg.Files);
     {$ENDIF}
     if not ResliceFSLVOIs(lFeatDirs,lVOI) then begin
        showmessage('Unable to reslice VOIs!');
        goto 111;
     end;
     lPSPlot.TextOutput := true;
     lPSPlot.GraphOutput := false;
     TextForm.MemoT.Lines.Clear;//prepare to report results


     if not ReadGraf(FSLFuncName (lFeatDirs[0]),false, (lVectors.count > 0) ) then
        goto 111; //read first dataset to set TR!
     //la1 := (FreeRAM);
     //get plot settings....
     lPSPlot.TRSec := TREdit.value;
     if lVectors.count > 0 then
        if not PSForm.GetPeriSettings(lPSPlot) then
           goto 111;

     for lImg := 1 to lFeatDirs.Count do begin
         if lImg > 1 then begin//we have already read 1st img
            Refresh;
            Application.processmessages;
            Close4DTrace(g4Ddata,true);
            ImgForm.CloseImagesClick(nil);
            FreeImgMemory(g4DHdr);
            //Textform.memo1.lines.add(inttostr(FreeRAM));//rascal
            if not ReadGraf(FSLFuncName (lFeatDirs[lImg-1]),true,(lVectors.count > 0)) then goto 111; //read first dataset to set TR!
            if lUseFSLEVs then
               FSLEVNames (lFeatDirs[lImg-1], lVectors)
         end; //all except 1st image
         if lVectors.count > 0 then begin
            for lI := 1 to lVectors.count do
                ReadCond(lVectors[lI-1],g4Ddata,lI);
         end;//vectors > 0
         if lVOI.count > 0 then begin
            for lI := 1 to lVOI.count do begin
               lStr := FSLReslicedVOIName (lFeatDirs[lImg-1], lVOI[lI-1]);
               ImgForm.OverlayOpenCore(lStr,lI+kBGOverlayNum);
            end;//for each VOI
         end; //VOI > 0

         if lVectors.Count > 0 then begin
          if lImg = lFeatDirs.Count then
             lPSPlot.GraphOutput := true;
          CreatePeristimulusPlot (g4DHdr,g4Ddata, lPSPlot)
         end else begin
              ConvertToTrace(g4DHdr,g4Ddata,ImgForm.XViewEdit.value,ImgForm.YViewEdit.value,ImgForm.ZViewEdit.value);
             TextToTrace (g4Ddata);
             RegressTrace(g4Ddata);
         end;
     end;
          TextForm.show;
     111:
     lVOI.Free;
     lVectors.Free;
     lFeatDirs.free;
end;

{$DEFINE TEST}
procedure TGraph4DForm.FSLtest1Click(Sender: TObject);
label
     111;
var
   lStr: string;
   lFeatDirs,lVectors,lVOI: TStringList;
   lI,lImg: integer;
   lUseFSLEVs: Boolean;
   lPSPlot: TPSPlot;
   {lSliceTime,lSavePSVol,lTextOutput,lBaselineCorrect,lPctSignal,
   lRemoveRegressorVariability,lTemporalDeriv,lUseFSLEVs,lPlotModel: boolean; }
begin
     ImgForm.CloseImagesClick(nil);
     Close4DTrace(g4Ddata,true);
     FreeImgMemory(g4DHdr);
 //x    if not FindFEATFolders (lFeatDirs) then
 //x     exit;
     lVectors := TStringList.Create;//empty
     lVOI := TStringList.Create;
     {$IFDEF TEST}

     lFeatDirs := TStringList.Create;
     for lI := 1 to 100 do
     lFeatDirs.Add('C:\mri\fds.feat');

     {lUseFSLEVs := true;
     FSLEVNames (lFeatDirs[0], lVectors);
      }
     lUseFSLEVs := false;
     lVectors.Add('C:\mri\fds.feat\custom_timing_files\ev1.txt');
     lVectors.Add('C:\mri\fds.feat\custom_timing_files\ev2.txt');
     //lVectors.Add('C:\cygwin\home\express\20070420_132327fMRIcontin30x30x36s004a001.feat\custom_timing_files\ev3.txt');
     lVOI.Add('C:\mri\left.voi');
     lVOI.Add('C:\mri\right.voi');
     lVOI.Add('C:\mri\v1.voi');
               {$ELSE}
     FSLEVNames (lFeatDirs[0], lVectors);
     lUseFSLEVs := false;
     if lVectors.count > 0 then
        lUseFSLEVs := OKMsg('Use event vectors from the .FEAT'+pathdelim+'custom_timing_files folder?'); //shows dialog with OK/Cancel returns true if user presses OK
     if not lUseFSLEVs then begin
        lVectors.clear;
        if  OpenDialogExecute(kTxtFilter,'Select 3-column event onset time files',true) then begin
         if HdrForm.OpenHdrDlg.Files.Count > kMaxCond then begin
               showmessage('Can only load '+inttostr(kMaxCond)+'conditions');
               goto 111;
         end;
         lVectors.AddStrings(HdrForm.OpenHdrDlg.Files);
        end;
     end; //manually select EVs

     if not OpenDialogExecute(kImgPlusVOIFilter,'Select volume[s] of interest [2mm MNI space]',true) then
        goto 111;
     if HdrForm.OpenHdrDlg.Files.Count > (knMaxOverlay-2) then begin
        showmessage('Can only load '+inttostr(knMaxOverlay-2)+'conditions');
        goto 111;
     end;
     lVOI.AddStrings(HdrForm.OpenHdrDlg.Files);
     {$ENDIF}
     if not ResliceFSLVOIs(lFeatDirs,lVOI) then begin
        showmessage('Unable to reslice VOIs!');
        goto 111;
     end;
     lPSPlot.TextOutput := true;
     lPSPlot.GraphOutput := false;
     TextForm.MemoT.Lines.Clear;//prepare to report results


     if not ReadGraf(FSLFuncName (lFeatDirs[0]),false, (lVectors.count > 0) ) then
        goto 111; //read first dataset to set TR!
     //la1 := (FreeRAM);
     //get plot settings....
     lPSPlot.TRSec := TREdit.value;
     if lVectors.count > 0 then
        if not PSForm.GetPeriSettings(lPSPlot) then
           goto 111;

     for lImg := 1 to lFeatDirs.Count do begin
         if lImg > 1 then begin//we have already read 1st img
            Refresh;
            Application.processmessages;
            Close4DTrace(g4Ddata,true);
            ImgForm.CloseImagesClick(nil);
            FreeImgMemory(g4DHdr);
            //Textform.memo1.lines.add(inttostr(FreeRAM));//rascal
            if not ReadGraf(FSLFuncName (lFeatDirs[lImg-1]),true,(lVectors.count > 0)) then goto 111; //read first dataset to set TR!
            if lUseFSLEVs then
               FSLEVNames (lFeatDirs[lImg-1], lVectors)
         end; //all except 1st image
         if lVectors.count > 0 then begin
            for lI := 1 to lVectors.count do
                ReadCond(lVectors[lI-1],g4Ddata,lI);
         end;//vectors > 0
         if lVOI.count > 0 then begin
            for lI := 1 to lVOI.count do begin
               lStr := FSLReslicedVOIName (lFeatDirs[lImg-1], lVOI[lI-1]);
               ImgForm.OverlayOpenCore(lStr,lI+kBGOverlayNum);
            end;//for each VOI
         end; //VOI > 0

         if lVectors.Count > 0 then begin
          if lImg = lFeatDirs.Count then
             lPSPlot.GraphOutput := true;
          CreatePeristimulusPlot (g4DHdr,g4Ddata, lPSPlot)
         end else begin
              ConvertToTrace(g4DHdr,g4Ddata,ImgForm.XViewEdit.value,ImgForm.YViewEdit.value,ImgForm.ZViewEdit.value);
             TextToTrace (g4Ddata);
             RegressTrace(g4Ddata);
         end;
     end;
          TextForm.show;
     111:
     lVOI.Free;
     lVectors.Free;
     lFeatDirs.free;
     showmessage('done');
end; //test

initialization
{$IFDEF FPC}
  {$I graphx.lrs}
{$ENDIF}  
  
end.
