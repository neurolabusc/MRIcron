unit ROIfilt;

interface

uses
 {$IFNDEF FPC}
   RXSpin,
 {$ELSE}
 Spin,LResources,
 {$ENDIF}
 {$IFNDEF Unix} Windows,{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,  Buttons,define_types, nifti_hdr, nifti_types;

type
  TFilterROIform = class(TForm)
    Label42: TLabel;
    FilterROIBtn: TSpeedButton;
    Label43: TLabel;
    Filter2NIfTIBtn: TSpeedButton;
    FiltROILabel: TLabel;
    MinROIfilt: TSpinEdit;
    MaxROIfilt: TSpinEdit;
    procedure MinROIfiltChange(Sender: TObject);
    procedure FilterROIBtnClick(Sender: TObject);
  {$IFNDEF FPC}
  procedure FormClose(Sender: TObject; var Action: TCloseAction);
 {$ELSE}
    procedure FormClose(Sender: TObject);
 {$ENDIF}

    procedure FormShow(Sender: TObject);
    procedure Filter2NIfTIBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FilterROIform: TFilterROIform;

implementation
uses nifti_img_view,nifti_img;

 {$IFNDEF FPC}
{$R *.DFM}
 {$ENDIF}

procedure TFilterROIform.MinROIfiltChange(Sender: TObject);
begin
	if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1 then exit;
	FilterLUT (gBGImg, gMRIcroOverlay[kBGOverlayNum], round(MinROIFilt.Value),round(MaxROIfilt.value)); //lLUT: 0=gray,1=red,2=green,3=blue
	FiltROILabel.caption := 'Calibrated range: '+realtostr(Scrn2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],MinROIfilt.value),3)
		+'...'+realtostr(Scrn2ScaledIntensity (gMRIcroOverlay[kBGOverlayNum],MaxROIfilt.value),3);
	ImgForm.RefreshImagesTimer.enabled := true;
end;

procedure TFilterROIform.FilterROIBtnClick(Sender: TObject);
var	lBGBuffer,lVOIBuffer:ByteP;
	lInc,lMin,lMax,lBufferItems,lVOIvoxelsAfter,lVOIvoxelsBefore: integer;
begin
  lBufferItems := gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems;
  if lBufferItems < 1 then begin
		showmessage('You need to open up a VOI (Draw/Open) in order to apply an intensity filter to the VOI.');
		exit;
  end;
  if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems <> lBufferItems then begin
		showmessage('Error: VOI dimensions do not match background image.');
		exit;
  end;
  CreateUndoVol;
  (*case MessageDlg('Unable to undo this operation. You should save a backup copy prior to this (Draw/Save). Are you sure you wish to filter your VOI?', mtConfirmation,
		[mbYes, mbCancel], 0) of
		id_Cancel: exit;
  end; //case *)
  lMin := round(MinROIFilt.value);
  lMax := round(MaxROIFilt.value);
  if lMin > lMax then begin //swap
	lInc := lMin;
	lMin := lMax;
	lMax := lInc;
  end; //swap
  if lBufferItems < 1 then
	showmessage('Error: no background image open to filter.')
  else begin
	lBGBuffer := gMRIcroOverlay[kBGOverlayNum].ScrnBuffer;
	lVOIBuffer := gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer;
	lVOIvoxelsBefore := 0;
	for lInc := 1 to lBufferItems do
		if (lVOIBuffer^[lInc] > 0) then
			inc(lVOIvoxelsBefore);
	for lInc := 1 to lBufferItems do
		if (lBGBuffer^[lInc] < lMin) or (lBGBuffer^[lInc] > lMax) then
			lVOIBuffer^[lInc] := 0;
	lVOIvoxelsAfter := 0;
	for lInc := 1 to lBufferItems do
		if (lVOIBuffer^[lInc] > 0) then
			inc(lVOIvoxelsAfter);
	showmessage('VOI voxels prior to filter = '+inttostr(lVOIvoxelsBefore)+kCR
		+ 'VOI voxels after filter = '+inttostr(lVOIvoxelsAfter));
	gBGImg.VOIchanged := true;
	//Save8BitAsVOIorNIFTI(lFilteredBuffer,lBufferItems);
  end; //BGimage open
  FilterROIForm.Close;
//nn
end;

 {$IFNDEF FPC}
procedure TFilterROIform.FormClose(Sender: TObject; var Action: TCloseAction);
 {$ELSE}
procedure TFilterROIform.FormClose(Sender: TObject);
 {$ENDIF}
begin
	FilterLUT (gBGImg, gMRIcroOverlay[kBGOverlayNum], -1,-1); //lLUT: 0=gray,1=red,2=green,3=blue
	ImgForm.RefreshImagesTimer.enabled := true;
end;

procedure TFilterROIform.FormShow(Sender: TObject);
var lInc: integer;
begin
	for lInc := 0 to 255 do
		gBGImg.BackupLUT[lInc]:= gMRIcroOverlay[kBGOverlayNum].LUT[lInc];
	MinROIfiltChange(nil);
end;

procedure MirrorBuffer(var lBuffer8:ByteP; lX,lXYZ: integer );
var
  lnRow,lRow,lHlfX,lLineOffset,lXPos,lTemp: integer;
begin
  if (lXYZ < 2) or (lX > lXYZ) or ((lXYZ mod lX) <> 0)  then
    exit;
  lnRow := lXYZ div lX;
  lHlfX := lX div 2;
  lLineOffset := 0;
  for lRow := 1 to lnRow do begin
    for lXPos := 1 to lHlfX do begin
      lTemp := lBuffer8^[lXPos+lLineOffset];
      lBuffer8^[lXPos+lLineOffset] := lBuffer8^[1+lX-lXPos+lLineOffset];
      lBuffer8^[1+lX-lXPos+lLineOffset] := lTemp;
    end; //for X
    lLineOffset := lLineOffset + lX;
  end;//for each row...

end; //MirrorBuffer

procedure TFilterROIform.Filter2NIfTIBtnClick(Sender: TObject);
var	lFilteredBuffer:ByteP;
	lInc,lMin,lMax,lBufferItems: integer;
        lNiftiHdr : TNIFTIhdr;
begin
  lBufferItems := gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems;
  lMin := round(MinROIFilt.value);
  lMax := round(MaxROIFilt.value);
  if lMin > lMax then begin //swap
	lInc := lMin;
	lMin := lMax;
	lMax := lInc;
  end; //swap
  if lBufferItems < 1 then
	showmessage('Error: no background image open to filter.')
  else begin
	getmem(lFilteredBuffer,lBufferItems);
	move(gMRIcroOverlay[kBGOverlayNum].ScrnBuffer^,lFilteredBuffer^,lBufferItems);
	for lInc := 1 to lBufferItems do
		if (lFilteredBuffer^[lInc] < lMin) or (lFilteredBuffer^[lInc] > lMax) then
			lFilteredBuffer^[lInc] := 0;
        lNiftiHdr := gMRIcroOverlay[kBGOverlayNum].NiftiHdr;
        if gBGImg.Mirror then
           MirrorBuffer(lFilteredBuffer,gMRIcroOverlay[kBGOverlayNum].NIFTIhdr.Dim[1], lBufferItems); //10/2010
        SaveAsVOIorNIFTI(lFilteredBuffer,lBufferItems,1,1,true,{gMRIcroOverlay[kBGOverlayNum].}lNiftiHdr,'');
	//SaveAsVOIorNIFTI(lFilteredBuffer,lBufferItems,1,true,gMRIcroOverlay[kBGOverlayNum].NiftiHdr,'');
	freemem(lFilteredBuffer);
  end;
  FilterROIForm.Close;
end;

  {$IFDEF FPC}
initialization
  {$I ROIfilt.lrs}
{$ENDIF}

end.