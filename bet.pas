unit bet;

{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Spin,Process,nifti_img,define_types,CropEdges, userdir;

type
  { TBETForm }
  TBETForm = class(TForm)
    Memo1: TMemo;
    SmoothnessEdit: TFloatSpinEdit;
    Panel1: TPanel;
    GoBtn: TSpeedButton;
    AboutBtn: TSpeedButton;
    CropBtn: TSpeedButton;
    procedure CropBtnClick(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  BETForm: TBETForm;

implementation
uses
 nifti_img_view;
{ TBETForm }

procedure TBETForm.SpeedButton2Click(Sender: TObject);
begin
    Showmessage('You can skull strip scans to allow you to render the surface of the brain.'+chr (13)
+ 'This uses Steve Smith''s Brain Extraction Tool [BET].'+chr (13)+
'Default smoothness is 0.50, smaller values generate larger estimates of brain size.'+chr (13)
+'http://www.fmrib.ox.ac.uk/fsl');
end;

//lCmd := extractfilepath(paramstr(0))+'bet "'+lSourceFilename+'" "'+SaveDialog1.Filename
// +'" -f '+floattostr(SmoothnessEdit.value{/100});
function PathExists (lCmd: string): boolean;
begin
     result := false;
     if FSize(lCmd) < 1 then begin
          BETForm.Memo1.Lines.Add('Unable to find executable named '+lCmd);
          exit;
     end;
     result := true;
end;

procedure RunCmd (lCmd: string);
 var
 AProcess: TProcess;
   AStringList: TStringList;
   ACMD,PATH,FSLDIR,FSLCONF,lS,FULL: string;
 begin
   ACmd := lCmd;
   AProcess := TProcess.Create(nil);
   AStringList := TStringList.Create;

   //AProcess.CommandLine := lCmd;
   {$IFDEF UNIX}
    (*if direxists (gBGImg.FSLBASE) then
     BETForm.Memo1.Lines.Add('Using folder specified in mricron.ini file [FSLBASE] '+gBGImg.FSLBASE)
    else
     BETForm.Memo1.Lines.Add('Warning: unable to find folder specified in mricron.ini file [FSLBASE] '+gBGImg.FSLBASE);
    AProcess.Environment.Add(gBGImg.FSLBASE);
    lBinDir := gBGImg.FSLBASE+'/bin';
    if direxists (lBinDir) then begin
      AProcess.Environment.Add(lBinDir);
     BETForm.Memo1.Lines.Add('Adding path to environment: '+lBinDir);
    end else
     BETForm.Memo1.Lines.Add('Warning: unable to find binary folder '+lBinDir);*)


   PATH:=GetEnvironmentVariable('PATH');
   FSLDIR := gBGImg.FSLBASE;
   if (length(FSLDIR)<1) or (not DirExists(FSLDIR)) then begin
      if direxists (GetEnvironmentVariable('FSLDIR')) then
         FSLDIR:=GetEnvironmentVariable('FSLDIR');
   end;
   if direxists (FSLDIR) then
     BETForm.Memo1.Lines.Add('Setting FSL folder (if incorrect edit FSLBASE in mricron):'+FSLDIR)
    else
     BETForm.Memo1.Lines.Add('Warning: unable to find folder specified in mricron.ini file [FSLBASE] '+FSLDIR);
    FULL :=   PATH+':'+FSLDIR+':'+FSLDIR+'/bin' ;

    lS := 'FSLDIR='+FSLDIR;
    BETForm.Memo1.Lines.Add(lS);
    AProcess.Environment.Add(lS);


    lS := 'LD_LIBRARY_PATH='+FSLDIR+'/bin';
    BETForm.Memo1.Lines.Add(lS);
    AProcess.Environment.Add(lS);

    //lS := '. '+FSLDIR+'/etc/fslconf/fsl.sh';
    //BETForm.Memo1.Lines.Add(lS);
    //AProcess.Environment.Add(lS);


    lS := 'PATH='+FULL;
    BETForm.Memo1.Lines.Add(lS);
    AProcess.Environment.Add(lS);


    //AProcess.Environment.Add(FULL);

    lS := 'FSLCLUSTER_MAILOPTS="n"';
    BETForm.Memo1.Lines.Add(lS);
    AProcess.Environment.Add(lS);
    //lS := 'export '+FULL;
    //lS := 'export FSLDIR PATH';
    //BETForm.Memo1.Lines.Add(lS);
    //AProcess.Environment.Add(lS);


    BETForm.Memo1.Lines.Add(gBGImg.FSLOUTPUTTYPE);
   AProcess.Environment.Add(gBGImg.FSLOUTPUTTYPE);
   {$ENDIF}
   AProcess.CommandLine := ACmd;
   AProcess.Options := AProcess.Options + [poWaitOnExit, poStderrToOutPut, poUsePipes];
  BETForm.Memo1.Lines.Add(ACmd);
   AProcess.Execute;
   AStringList.LoadFromStream(AProcess.Output);
   BetForm.Memo1.Lines.AddStrings(AStringList);
   AStringList.Free;
   AProcess.Free;
 end;

(*procedure RunCmdX;
 var
   AProcess: TProcess;
   AStringList: TStringList;
   lBinDir: string;
 begin
   AProcess := TProcess.Create(nil);
   AStringList := TStringList.Create;

   AProcess.CommandLine := 'bet /home/crlab/t.nii /home/crlab/xt.nii';
   {$IFDEF UNIX}
   AProcess.Environment.Add('FSLOUTPUTTYPE=NIFTI_GZ');
   AProcess.Environment.Add('/usr/local/fsl/');
   AProcess.Environment.Add('/usr/local/fsl/bin');

   AProcess.Environment.Add('FSLDIR=/usr/local/fsl');
   //AProcess.Environment.Add('FSLOUTPUTTYPE=NIFTI_GZ');
   {$ENDIF}
   AProcess.Options := AProcess.Options + [poWaitOnExit, poStderrToOutPut, poUsePipes];
   AProcess.Execute;
   AStringList.LoadFromStream(AProcess.Output);
   BetForm.Memo1.Lines.AddStrings(AStringList);
   AStringList.Free;
   AProcess.Free;
 end;*)

function DoBET(lInFile,lOutFile: string; lFrac: single):boolean;
var
  lCmd: string;
begin
     result := false;
     lCmd :=  extractfilepath(paramstr(0))+'bet';
     {$IFNDEF Unix}
         lCmd := lCmd+'.exe';
     {$ELSE}
     if not fileexists(lCmd) then begin
        lCmd := (gBGImg.FSLBASE+'/bin/bet');
        if fileexists(lCmd) then
         BETForm.Memo1.Lines.Add('Using executable location from mricron.ini file [FSLBASE] '+lCmd)
        else
         BETForm.Memo1.Lines.Add('Unable to find executable suggested by mricron.ini file [FSLBASE] '+lCmd)

     end;
     {$ENDIF}
     if not PathExists (lCmd) then begin
        lCmd := '/usr/local/fsl/bin/bet_8UI';
        if not PathExists (lCmd) then begin
           lCmd := '/usr/local/fsl/bin/bet';
           if not PathExists (lCmd) then
                 exit;
        end;
     end; //no bet in home folder...
  lCmd := lCmd+' "'+lInFile+'" "'+lOutFile +'" -R -f '+floattostr(lFrac);

  RunCmd(lCmd);
  (*AProcess := TProcess.Create(nil);
  {$IFDEF UNIX}
  AProcess.Environment.Add('FSLDIR=/usr/local/fsl');
  AProcess.Environment.Add('FSLOUTPUTTYPE=NIFTI_GZ');
  {$ENDIF}
  AProcess.CommandLine := lCmd;
  //AProcess.CommandLine := 'C:\bet "C:\txx.hdr" "C:\btxx.hdr" -f 0.5';
  AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  AProcess.Free;  *)
  result := true;
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
         inc(lHisto[gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lPos]]);
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
         if gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lPos] = kVOI8bit then
            gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^[lPos]  := lVOIIntensity;
end;

procedure DeleteHdrImg(lFilename: string);
begin
//exit;
     if lFilename = '' then
        exit;
     DeleteFile(lFilename);
     if (UpCaseExt(lFileName)='.IMG') then
            DeleteFile(changefileext(lFilename,'.hdr'));
     if (UpCaseExt(lFileName)='.HDR') then
        DeleteFile(changefileext(lFilename,'.img'));
end;

function Mask8BitImg(lImgName,lMaskName: string): boolean;
//should be two 8-bit image files of identical dimensions
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
    if (lSz < 1) or (lBPP = 0 ) then
       exit;
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

(*function Mask8BitImg(lImgName,lMaskName: string): boolean;//should be two 8-bit image files of identical dimensions
//all non-zero voxels in the mask are written with value of img
//Note: the mask file is changed - not the image
var
   lPos,lSz: integer;
   lImg,lMask: bytep;
   lInF: File;
begin
    //showmessage( lMaskName+'xx'+lImgName);
    result := false;
    lSz := FSize(lImgName);
    if (lSz < 1) or (lSz <> FSize(lMaskName)) then
       exit;
    //fx(lSz,778899);
    //next read mask
    GetMem(lMask,lSz);
    AssignFile(lInF, lMaskName);
    Reset(lInF,1);
    BlockRead(lInF, lMask^, lSz);
    CloseFile(lInF);
    //next: read image
    GetMem(lImg,lSz);
    AssignFile(lInF, lImgName);
    Reset(lInF,1);
    BlockRead(lInF, lImg^, lSz);
    CloseFile(lInF);
    //next mask image
    for lPos := 1 to lSz do
        if lMask^[lPos] = 0 then
           lImg^[lPos] := 0;
    Freemem(lMask);
    //next save masked image
    AssignFile(lInF, lMaskName);
    Rewrite(lInF,1);
    BlockWrite(lInF, lImg^, lSz);
    CloseFile(lInF);
    Freemem(lImg);
    result := true;
end;*)

function DefaultsDirCmd: string;
//Lazarus for Unix does not seem to execute TProcess commands to ~/.. we need to write them to /Home/chris/..
var
   lLen,lP: integer;
   lStr: string;
begin
  {$IFDEF UNIX}
    lStr := extractfiledir(GetAppConfigFile(false));
    lLen := length(lStr);
    if lLen < 1 then exit;
    lP := lLen;
    while (lP > 0) and (lStr[lP] <> '.') do
          dec(lP);
    if lP > 1 then begin
       for lLen := 1 to (lP-1) do
           result := result + lStr[lLen];
    end;
   {$ELSE} //else ... assume windows
    result := DefaultsDir('')
   {$ENDIF}
    //showmessage('x'+result+'x');
end;

procedure TBETForm.GoBtnClick(Sender: TObject);
label
666;
var
   lTempNameOrig,lTempName8bitMask,lTempBetName,lTempGZName: string;
begin
  Memo1.Clear;
    Memo1.lines.add('Startup Timestamp: '+DateTimeToStr(Now));
    if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1)  then begin
        showmessage('BET error: please use File/Open to display the image you want to brain extract.');
    end;
    //showmessage(DefaultsDirCmd);
    lTempNameOrig :=DefaultsDirCmd+'orig.hdr';//lTempNameOrig := extractfilepath(paramstr(0))+'orig.hdr';
    SaveAsVOIorNIFTIcore (lTempNameOrig, gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems, gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
    //SaveAsVOIorNIFTIcore (lTempNameOrig, gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems, gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
    if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0 then begin
       	case MessageDlg('Do you wish to protect tissue shown by the VOI drawing?', mtConfirmation, [mbYes, mbNo], 0) of
                     mrYes: CropVOI(Bright95Pct);
        end; //case for protecting VOI
       ImgForm.CloseVOIClick(nil);
    end;
    if gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP <> 1 then
       Memo1.lines.add('Warning: converted image downsampled to 8-bit precision.');
    lTempName8bitMask := DefaultsDirCmd+'temp8.hdr';//lTempName8bitMask := extractfilepath(paramstr(0))+'temp8.hdr';
    SaveAsVOIorNIFTIcore (lTempName8bitMask, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
    lTempName8bitMask := changefileext(lTempName8bitMask,'.hdr');  //SaveAs renames the .hdr to .img
    lTempBetName := DefaultsDirCmd+'btemp8.hdr';//lTempBetName := extractfilepath(paramstr(0))+'btemp8.hdr';
    // showmessage(lTempBetName);
    if not DoBET(lTempName8bitMask,lTempBetName,SmoothnessEdit.value) then goto 666;


    Memo1.lines.add('Shutdown Timestamp: '+DateTimeToStr(Now));
    if Fileexists(lTempBetName) then begin
       CopyFileEXoverwrite(lTempName8bitMask,lTempBetName); //the old version of BET corrupts some NIfTI information
    end else begin
        //assume new version of bet_8UI has saved as .nii.gz
        lTempGZName := ChangeFileExt(lTempBetName,'.nii.gz');
        if not Fileexists(lTempGZName) then begin
           Memo1.lines.add('BET Error: unable to find BET image '+lTempBetName+ ' or '+lTempGZName);
           {$IFDEF Darwin}
            Memo1.lines.add(' Try relaunching MRIcron from the Terminal command line, e.g.   /Applications/mricron.app/mricron &');
           {$ENDIF}
           goto 666;
        end;
        //convert .nii.gz to hdr/.img so we can mask it...
        ImgForm.CloseImagesClick(nil);
        ImgForm.OpenAndDisplayImg(lTempGZName,True);
        SaveAsVOIorNIFTIcore (lTempBetName,gMRIcroOverlay[kBGOverlayNum].ScrnBuffer, gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
          DeleteHdrImg(lTempGZName);
 goto 666;
//        ImgForm.CloseImagesClick(nil);
    end;

    Mask8BitImg(changefileext(lTempNameOrig,'.img'),changefileext(lTempBetName,'.img'));
    ImgForm.OpenAndDisplayImg(lTempNameOrig,True);
    Memo1.lines.add('Use File/SaveAsNIfTI to save the stripped 8-bit image.');
666:

    DeleteHdrImg(lTempBetName);
    DeleteHdrImg(lTempNameOrig);
    DeleteHdrImg(lTempName8bitMask);
end;

procedure TBETForm.CropBtnClick(Sender: TObject);
begin
  CropEdgeForm.Show;
end;

(*var
   lTempNameOrig,lTempName8bitMask,lTempBetName: string;
begin
  Memo1.Clear;
    Memo1.lines.add('Startup Timestamp: '+DateTimeToStr(Now));
    if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1)  then begin
        showmessage('BET error: please use File/Open to display the image you want to brain extract.');
    end;
    lTempNameOrig := '';
    if gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems > 0 then begin
       	case MessageDlg('Do you wish to remove tissue shown by the VOI drawing?', mtConfirmation,
             [mbYes, mbNo], 0) of
                     mrYes: CropVOI(0);
                     else case MessageDlg('Do you wish to protect tissue shown by the VOI drawing?', mtConfirmation,
                          [mbYes, mbNo], 0) of
                     mrYes: begin
                             lTempNameOrig := DefaultsDir('')+'orig8.hdr';
                             SaveAsVOIorNIFTIcore (lTempNameOrig, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
                             CropVOI(Bright95Pct);
                     end;

	             end; //case for protecting VOI
	end; //case for deleting VOI

       ImgForm.CloseVOIClick(nil);
    end;
    if gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP <> 1 then
       Memo1.lines.add('Warning: converted image downsampled to 8-bit precision.');
    lTempName8bitMask := DefaultsDir('')+'temp8.hdr';
    SaveAsVOIorNIFTIcore (lTempName8bitMask, gMRIcroOverlay[kBGOverlayNum].ScrnBuffer,gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems, 1,gMRIcroOverlay[kBGOverlayNum].NiftiHdr);
    lTempName8bitMask := changefileext(lTempName8bitMask,'.hdr');  //SaveAs renames the .hdr to .img
    lTempBetName := DefaultsDir('')+'btemp8.hdr';
    DoBET(lTempName8bitMask,lTempBetName,SmoothnessEdit.value);
    Memo1.lines.add('Shutdown Timestamp: '+DateTimeToStr(Now));

    CopyFileEXoverwrite(lTempName8bitMask,lTempBetName); //the old version of BET corrupts some NIfTI information
    DeleteHdrImg(lTempName8bitMask);
    if lTempNameOrig <> '' then begin
       Mask8BitImg(changefileext(lTempNameOrig,'.img'),changefileext(lTempBetName,'.img'));
       DeleteHdrImg(lTempNameOrig);
    end;
    ImgForm.OpenAndDisplayImg(lTempBetName,True);
    Memo1.lines.add('Use File/SaveAsNIfTI to save the stripped 8-bit image.');
    DeleteHdrImg(lTempBetName);
end;*)
(*procedure TBETForm.GoBtnClick(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.lines.add('Startup Timestamp: '+DateTimeToStr(Now));
  DoBET('C:\txx.hdr','C:\btxx.hdr',SmoothnessEdit.value);
  Memo1.lines.add('Shutdown Timestamp: '+DateTimeToStr(Now));
end; *)

initialization
  {$I bet.lrs}

end.

