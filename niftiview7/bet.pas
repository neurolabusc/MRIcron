unit bet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls,nifti_img,nifti_img_view,ShellAPI, RXSpin,define_types,
  Mask;

type
  TBETForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    GoBtn: TSpeedButton;
    AboutBtn: TSpeedButton;
    SmoothnessEdit: TRxSpinEdit;
    CropBtn: TSpeedButton;
    procedure GoBtnClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure CropBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BETForm: TBETForm;

implementation

uses cropedges;

{$R *.DFM}

function DoBET(lInFile,lOutFile: string; lFrac: single):boolean;
var
  lCmd: String;
   SEInfo: TShellExecuteInfo;
   ExitCode: DWORD;
   ExecuteFile, lParamStr, lExecStr: string;
begin
   Result := false;
   lExecStr := extractfilepath(paramstr(0))+'bet';
   if FSize(lExecStr+'.exe') < 1 then
          BETForm.Memo1.Lines.Add('Unable to find executable named '+lCmd);
   ExecuteFile:=lExecStr;
   lParamStr := '"'+lInFile+'" "'+lOutFile +'" -f '+floattostr(lFrac);
   FillChar(SEInfo, SizeOf(SEInfo), 0) ;
   SEInfo.cbSize := SizeOf(TShellExecuteInfo) ;
   with SEInfo do begin
     fMask := SEE_MASK_NOCLOSEPROCESS;
     Wnd := Application.Handle;
     lpFile := PChar(ExecuteFile) ;
     lpParameters := PChar(lParamStr) ;
     // lpDirectory := PChar(StartInString) ;
     nShow := SW_SHOWNORMAL;
   end;
   if ShellExecuteEx(@SEInfo) then begin
     repeat
       Application.ProcessMessages;
       GetExitCodeProcess(SEInfo.hProcess, ExitCode) ;
     until (ExitCode <> STILL_ACTIVE) or
     Application.Terminated;
     result := true;
   end;
end;


function Bright95Pct: byte;//returns intensity of 95th percentile
var
   lPos,l5Pct,lCumulative: integer;
   lHisto: array [0..255] of integer;
begin
     result := 0;
     if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems<1) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems<>gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems) then exit;
     //next - create histogram of intensity
     for lPos := 0 to 255 do
         lHisto[lPos] := 0;
     for lPos := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do
         inc(lHisto[gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lPos]]);
     //next find 95th percentile
     l5Pct := (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems div 20);
     lCumulative := 0;
     lPos := 256;
     while (lPos > 0) and (lCumulative < l5Pct) do begin
         dec(lPos);
         lCumulative := lCumulative + lHisto[lPos];
     end;
     result := lPos;
end;

procedure CropVOI (lVOIIntensity: byte);
var
   lPos: integer;
begin
     if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems<1) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems<>gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems) then exit;
     for lPos := 1 to gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems do
         if gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] = kVOI8bit then
            gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lPos]  := lVOIIntensity;
end;

procedure DeleteHdrImg(lFilename: string);
begin
     if lFilename = '' then
        exit;
     DeleteFile(lFilename);
     if (UpCaseExt(lFileName)='.IMG') then
            DeleteFile(changefileext(lFilename,'.hdr'));
     if (UpCaseExt(lFileName)='.HDR') then
        DeleteFile(changefileext(lFilename,'.img'));
end;

function Mask8BitImg(lImgName,lMaskName: string): boolean;//should be two 8-bit image files of identical dimensions
//all non-zero voxels in the mask are written with value of img
//Warning - only works with .img files with zero voxoffset - can corrupt .nii files - would need to read header....
var
   lPos2,lPos,lC,lSz,lMaskSz,lBPP: integer;
   lImg,lMask: bytep;
   lInF: File;
begin
    result := false;
    lSz := FSize(lImgName);
    lMaskSz := FSize(lMaskName);
    if lSz = lMaskSz then
       lBPP := 1
    else if lSz = (2*lMaskSz) then
         lBPP := 2
    else if lSz = (2*lMaskSz) then
         lBPP := 4
    else
        lBPP := 0;
        //showmessage('a');
    if (lSz < 1) or (lBPP = 0 ) then
       exit;
       //showmessage('b');
    //next read mask
    GetMem(lMask,lSz);
    AssignFile(lInF, lMaskName);
    Reset(lInF,1);
    BlockRead(lInF, lMask^, lMaskSz);
    CloseFile(lInF);
    //next: read image
    GetMem(lImg,lSz);
    AssignFile(lInF, lImgName);
    Reset(lInF,1);
    BlockRead(lInF, lImg^, lSz);
    CloseFile(lInF);
    //next mask image
    for lPos := 1 to lMaskSz do
        if lMask^[lPos] = 0 then begin
           lPos2 := ((lPos-1)*lBPP);
           for lC := 1 to lBPP do
               lImg^[lC+lPos2] := 0;
        end;
    Freemem(lMask);
    //next save masked image
    AssignFile(lInF, lImgName);     //1/2008....
    //AssignFile(lInF, lMaskName);
    Rewrite(lInF,1);
    BlockWrite(lInF, lImg^, lSz);
    CloseFile(lInF);
    Freemem(lImg);
    result := true;
end;

procedure TBETForm.GoBtnClick(Sender: TObject);
var
   lTempNameOrig,lTempName8bitMask,lTempBetName: string;
begin
  Memo1.Clear;
    Memo1.lines.add('Startup Timestamp: '+DateTimeToStr(Now));
    if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1)  then begin
        showmessage('BET error: please use File/Open to display the image you want to brain extract.');
    end;
    lTempNameOrig := extractfilepath(paramstr(0))+'orig.hdr';
    SaveAsVOIorNIFTIcore (lTempNameOrig, gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems, gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
    if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0 then begin
       	case MessageDlg('Do you wish to protect tissue shown by the VOI drawing?', mtConfirmation, [mbYes, mbNo], 0) of
                     mrYes: CropVOI(Bright95Pct);
        end; //case for protecting VOI
       ImgForm.CloseVOIClick(nil);
       //lTempBetName := 'c:\bet.nii';
       //SaveAsVOIorNIFTIcore (lTempBetName, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);

    end;
    if gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP <> 1 then
       Memo1.lines.add('Warning: converted image downsampled to 8-bit precision.');
    lTempName8bitMask := extractfilepath(paramstr(0))+'temp8.hdr';
    SaveAsVOIorNIFTIcore (lTempName8bitMask, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
    lTempName8bitMask := changefileext(lTempName8bitMask,'.hdr');  //SaveAs renames the .hdr to .img
    lTempBetName := extractfilepath(paramstr(0))+'btemp8.hdr';
    DoBET(lTempName8bitMask,lTempBetName,SmoothnessEdit.value);
    Memo1.lines.add('Shutdown Timestamp: '+DateTimeToStr(Now));

    CopyFileEXoverwrite(lTempName8bitMask,lTempBetName); //the old version of BET corrupts some NIfTI information
    Mask8BitImg(changefileext(lTempNameOrig,'.img'),changefileext(lTempBetName,'.img'));
    ImgForm.OpenAndDisplayImg(lTempNameOrig,True);
    Memo1.lines.add('Use File/SaveAsNIfTI to save the stripped 8-bit image.');
    DeleteHdrImg(lTempBetName);
    DeleteHdrImg(lTempNameOrig);
    DeleteHdrImg(lTempName8bitMask); 
end;

procedure TBETForm.AboutBtnClick(Sender: TObject);
begin
 Showmessage('You can skull strip scans to allow you to render the surface of the brain.'+chr (13)
+ 'This uses Steve Smith''s Brain Extraction Tool [BET].'+chr (13)+
'Default smoothness is 0.50, smaller values generate larger estimates of brain size.'+chr (13)
+'http://www.fmrib.ox.ac.uk/fsl');
end;

procedure TBETForm.CropBtnClick(Sender: TObject);
begin
     CropEdgeForm.Show;
end;

end.
