unit CropEdges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Spin, Buttons,nifti_img,define_types;

type

  { TCropEdgeForm }

  TCropEdgeForm = class(TForm)
    ApplyBtn: TSpeedButton;
    CropFileSzBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    Timer1: TTimer;
    DEdit: TSpinEdit;
    PEdit: TSpinEdit;
    AEdit: TSpinEdit;
    VEdit: TSpinEdit;
    REdit: TSpinEdit;
    LEdit: TSpinEdit;
    procedure ApplyCrop;
    procedure ApplyCrop2Img;
    procedure ApplyBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CropEditChange(Sender: TObject);
    procedure CropFileSzBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  CropEdgeForm: TCropEdgeForm;

implementation
uses
    nifti_img_view, crop;
{ TCropEdgeForm }

procedure TCropEdgeForm.ApplyBtnClick(Sender: TObject);
begin
  	CropEdgeForm.ModalResult := mrOK;
	CropEdgeForm.close;
end;

procedure TCropEdgeForm.CancelBtnClick(Sender: TObject);
begin
	CropEdgeForm.close;
end;

procedure TCropEdgeForm.CropEditChange(Sender: TObject);
begin
     if not CropEdgeForm.visible then exit;
     Timer1.Enabled := true;
end;


procedure TCropEdgeForm.CropFileSzBtnClick(Sender: TObject);
  var
   lV,lD,lA,lP,lL,lR: integer;
begin
     lV := VEdit.value;
     lD := DEdit.value;
     lL := LEdit.value;
     lR := REdit.value;
     lA := AEdit.value;
     lP := PEdit.value;
     CropNIfTI(lL,lR,lA,lP,lD,lV);
end;

procedure TCropEdgeForm.FormCreate(Sender: TObject);
begin

end;

procedure TCropEdgeForm.FormHide(Sender: TObject);
begin
         UndoVolVOI;
	 if not (CropEdgeForm.ModalResult = mrCancel) then
	 	ApplyCrop2Img
         else
	     ImgForm.RefreshImagesTimer.Enabled := true;
end;

procedure TCropEdgeForm.FormShow(Sender: TObject);
begin
       EnsureVOIOpen;
     CreateUndoVol;
     CropEdgeForm.ModalResult := mrCancel;
     CropEditChange(nil);
end;

procedure TCropEdgeForm.ApplyCrop2Img;
var
	lZLo,lZHi,lXLo,lXHi,lYLo,lYHi,lPos,lX,lY,lZ: integer;
        l32Buf : SingleP;
        l16Buf : SmallIntP;
begin
     if (gMRIcroOverlay[kBGOverlayNum].ImgBufferItems<1) then exit;
     if (gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]*gBGImg.ScrnDim[3]) <> gMRIcroOverlay[kBGOverlayNum].ImgBufferItems then begin
        Showmessage('Can not crop edges of a rotated image.');
        exit;
     end;
     lXlo := round(LEdit.value);
     lXHi := gBGImg.ScrnDim[1] - round(REdit.value);
     lYlo := round(PEdit.value);
     lYHi := gBGImg.ScrnDim[2] - round(AEdit.value);
     lZLo := round(VEdit.value);
     lZHi := gBGImg.ScrnDim[3] - round(DEdit.value);
     lPos := 0;
     case gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP of
      1: begin
           for lZ := 1 to gBGImg.ScrnDim[3] do
               for lY := 1 to gBGImg.ScrnDim[2] do
                   for lX := 1 to gBGImg.ScrnDim[1] do begin
				inc(lPos);
				if (lZ >= lZHi) or (lZ <= lZLo) or(lY >= lYHi) or (lY <= lYLo) or (lX >= lXHi) or (lX <= lXLo) then
				  gMRIcroOverlay[kBGOverlayNum].ImgBuffer^[lPos] := 0;
                   end; //for X
           end;
      2: begin
           l16Buf := SmallIntP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
           for lZ := 1 to gBGImg.ScrnDim[3] do
               for lY := 1 to gBGImg.ScrnDim[2] do
                   for lX := 1 to gBGImg.ScrnDim[1] do begin
				inc(lPos);
				if (lZ >= lZHi) or (lZ <= lZLo) or(lY >= lYHi) or (lY <= lYLo) or (lX >= lXHi) or (lX <= lXLo) then
				  l16Buf^[lPos] := 0;
                   end; //for X
           end;
      4: begin
           l32Buf := SingleP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
           for lZ := 1 to gBGImg.ScrnDim[3] do
               for lY := 1 to gBGImg.ScrnDim[2] do
                   for lX := 1 to gBGImg.ScrnDim[1] do begin
				inc(lPos);
				if (lZ >= lZHi) or (lZ <= lZLo) or(lY >= lYHi) or (lY <= lYLo) or (lX >= lXHi) or (lX <= lXLo) then
				  l32Buf^[lPos] := 0;
                   end; //for X
           end;
       else begin showmessage('Unsupported data type'); end
     end; //case
     ImgForm.RescaleImagesTimer.Enabled := true;
end;

procedure TCropEdgeForm.ApplyCrop;
var
	lZLo,lZHi,lXLo,lXHi,lYLo,lYHi,lPos,lX,lY,lZ: integer;
begin
     if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems<1) or (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems<>gBGImg.VOIUndoVolItems) then exit;
     if gBGImg.VOIUndoVolItems <> gMRIcroOverlay[kVOIOverlayNum].ScrnBufferItems then exit;
     if (gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]*gBGImg.ScrnDim[3]) <> gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems then exit;
     //CreateUndoVol;
     //Move(gBGImg.VOIUndoVol^,gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVolItems);
     FillChar(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVolItems,0);
	lXlo := round(LEdit.value);
	lXHi := gBGImg.ScrnDim[1] - round(REdit.value);
	lYlo := round(PEdit.value);
	lYHi := gBGImg.ScrnDim[2] - round(AEdit.value);
	lZLo := round(VEdit.value);
	lZHi := gBGImg.ScrnDim[3] - round(DEdit.value);
	lPos := 0;
	for lZ := 1 to gBGImg.ScrnDim[3] do begin
		for lY := 1 to gBGImg.ScrnDim[2] do begin
			for lX := 1 to gBGImg.ScrnDim[1] do begin
				inc(lPos);
				if (lZ >= lZHi) or (lZ <= lZLo) or(lY >= lYHi) or (lY <= lYLo) or (lX >= lXHi) or (lX <= lXLo) then
				  gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^[lPos] := kVOI8bit;
			end; //for X
		end; //for Y
	end; //for Z
        //gBGImg.VOIchanged := true;
	ImgForm.RefreshImagesTimer.enabled := true;
end;

procedure TCropEdgeForm.Timer1Timer(Sender: TObject);
begin
     Timer1.Enabled := false;
     ApplyCrop;

end;




initialization
  {$I cropedges.lrs}

end.

