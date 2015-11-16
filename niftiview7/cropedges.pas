unit cropedges;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin, Buttons, nifti_img,ExtCtrls,define_types,crop, Mask;

type
  TCropEdgeForm = class(TForm)
    Timer1: TTimer;
    ApplyBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    LEdit: TRxSpinEdit;
    VEdit: TRxSpinEdit;
    REdit: TRxSpinEdit;
    AEdit: TRxSpinEdit;
    PEdit: TRxSpinEdit;
    DEdit: TRxSpinEdit;
    CropFileSzBtn: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ApplyCrop;
    procedure ApplyCrop2Img;
    procedure CropEditChange(Sender: TObject);
    procedure CropFileSzBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CropEdgeForm: TCropEdgeForm;

implementation

uses nifti_img_view, Text;

{$R *.DFM}

procedure SmoothRA (var lRA: Doublep; lItems: integer);
var
 lRecip: double;
 lTempRA,lTempRAUnaligned: Doublep;
 lI: integer;
begin
     if lItems < 3 then exit;
     GetMem(lTempRAUnaligned,(lItems*sizeof(double))+16);
     lTempRA := DoubleP($fffffff0 and (integer(lTempRAUnaligned)+15));
     for lI := 1 to lItems do
         lTempRA^[lI] := lRA[lI];
     lRecip := 1/3; //multiplies faster than divides
     for lI := 2 to (lItems-1) do
         lRA^[lI] := (lTempRA[lI-1]+lTempRA[lI]+lTempRA[lI+1])*lRecip;
     FreeMem(lTempRAUnaligned);
end;

function MaxRA (var lRA: Doublep; lStart,lItems: integer): integer;
var
 lMax: double;
 lI: integer;
begin
     result := lStart;
     if (lItems < 2) or (lStart >= lItems) or ((lItems-lStart)< 1) then exit;
     lMax := lRA^[lStart];
     for lI := lStart to lItems do
         if lRA^[lI] > lMax then begin
             result := lI;
             lMax := lRA^[lI]
         end;
end;

function MinRA (var lRA: Doublep; lStart,lItems: integer): integer;
var
 lMin: double;
 lI: integer;
begin
     result := lStart;
     if (lItems < 2) or (lStart >= lItems) or ((lItems-lStart)< 1) then exit;
     lMin := lRA^[lStart];
     for lI := lStart to lItems do
         if lRA^[lI] < lMin then begin
             result := lI;
             lMin := lRA^[lI]
         end;
end;




function VentralClip (var lDorsalCrop,lVentralCrop: integer; lPct: integer): boolean;
var
   lSliceMax: double;
   lSliceSum,lSliceSumUnaligned: Doublep;
   lXY,lZ,lSlices,lSliceSz,lSliceStart,lVentralMaxSlice,lMaxSlice,lMinSlice,lGap: integer;
begin
     result := false;
     lDorsalCrop := 0;
     lVentralCrop := 0;
     if (lPct < 1) or (lPct > 100) then
        exit;
     if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1) or (gBGImg.ScrnDim[3] < 2) or (gBGImg.ScrnMM[3] = 0) then begin
        showmessage('Please load a 3D background image for neck removal.');
        exit;
     end;
     if not gBGImg.resliced then begin
         //showmessage('Neck removal can only be done on resliced images.');
         exit;
     end;
     lSlices := gBGImg.ScrnDim[3];
     lSliceSz := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
     GetMem(lSliceSumUnaligned,(lSlices*sizeof(double))+16);
     lSliceSum := DoubleP($fffffff0 and (integer(lSliceSumUnaligned)+15));
     lSliceMax := 0;
     for lZ := 1 to lSlices do begin
         lSliceSum^[lZ] := 0;
         lSliceStart := (lZ-1)*lSliceSz;
         for lXY := 1 to lSliceSz do
             lSliceSum^[lZ] := lSliceSum^[lZ]+ gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lXY+lSliceStart];
         if  lSliceMax < lSliceSum^[lZ] then
             lSliceMax := lSliceSum^[lZ];
     end; //for each slice
     if lSliceMax = 0 then begin //no data variance
        Freemem(lSliceSumUnaligned);
        exit;
     end; //VolSum = 0
     //next: normalize so each slice is normalized to brightest axial slice
     for lZ := 1 to lSlices do
         lSliceSum^[lZ] := lSliceSum^[lZ]/lSliceMax;
     result := true;
     //next: smooth
     SmoothRA(lSliceSum,lSlices);
     //next - top cropping - removing slices that are <5%  of maximum slice
     lZ := lSlices;
     while (lZ > 1) and (lSliceSum^[lZ] < (lPct/100)) do
           dec(lZ);
     lDorsalCrop := lSlices-lZ;
     //next findMax
     lMaxSlice := MaxRA(lSliceSum,1,lSlices);
     //next - ensure there is at least 60mm from max to bottom of an image - enough spine to worry about
     lVentralMaxSlice := lMaxSlice-round(60/abs(gBGImg.ScrnMM[3]));
     if lVentralMaxSlice < 1 then
        exit;
     lVentralMaxSlice := MaxRA(lSliceSum,1,lVentralMaxSlice);
     //finally: find minima between these two points...
     lMinSlice := MinRA(lSliceSum,lVentralMaxSlice,lMaxSlice);
     lGap := round((lMaxSlice-lMinSlice)*0.35);
     if (lMinSlice-lGap) > 1 then begin
        result := true;
        lVentralCrop := lMinSlice-lGap;
     end;
     //fx(lVentralCrop,lDorsalCrop);
     //next show output...            
     {TextForm.Memo1.Lines.Clear;
     for lZ := 1 to lSlices do
          TextForm.Memo1.Lines.add(inttostr(lZ)+','+floattostr(lSliceSum^[lZ]));
     TextForm.Show; }
     //cleanup
     Freemem(lSliceSumUnaligned);
end;

function LRClip (var lLCrop,lRCrop:integer; lPct,lDClip,lVClip: integer): boolean;
//amount of image to crop from left/right for N% signal intensity
var
   lSliceMax: double;
   lSliceSum,lSliceSumUnaligned: Doublep;
   lZmin,lZmax,lX,lY,lZ,lSlices,lSliceSz,lSliceStart: integer;
begin
     result := false;
     lLCrop := 0;
     lRCrop := 0;
     if (lPct < 1) or (lPct > 100) then
        exit;
     lZMin := lVClip;
     lZMax := gBGImg.ScrnDim[3]-lDClip;
     SortInt(lZMin,lZMax);
     lZMin := Bound(lZMin,1,gBGImg.ScrnDim[3]);
     lZMax := Bound(lZMax,1,gBGImg.ScrnDim[3]);
     if lZMin >= lZMax then
        exit;


     if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1) or (gBGImg.ScrnDim[1] < 2) or (gBGImg.ScrnDim[2] < 2) or (gBGImg.ScrnDim[3] < 2)  then begin
        showmessage('Please load a 3D background image for neck removal.');
        exit;
     end;
     if not gBGImg.resliced then begin
         showmessage('Neck removal can only be done on resliced images.');
         exit;
     end;

     lSlices := gBGImg.ScrnDim[1];
     lSliceSz := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
     GetMem(lSliceSumUnaligned,(lSlices*sizeof(double))+16);
     lSliceSum := DoubleP($fffffff0 and (integer(lSliceSumUnaligned)+15));
     lSliceMax := 0;
     for lX := 1 to lSlices do begin
         lSliceSum^[lX] := 0;
         for lZ := {1 to gBGImg.ScrnDim[3]} lZMin to lZMax do begin
             lSliceStart := lX+ ((lZ-1)*lSliceSz);
             for lY := 1 to gBGImg.ScrnDim[2] do begin
                 lSliceSum^[lX] := lSliceSum^[lX]+ gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lSliceStart];
                 lSliceStart := lSliceStart + gBGImg.ScrnDim[1];
             end;
         end;
         //for lYZ := 1 to lSliceSz do
         //    lSliceSum^[lZ] := lSliceSum^[lZ]+ gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lXY+lSliceStart];
         if  lSliceMax < lSliceSum^[lX] then
             lSliceMax := lSliceSum^[lX];
     end; //for each slice
     if lSliceMax = 0 then begin //no data variance
        Freemem(lSliceSumUnaligned);
        exit;
     end; //VolSum = 0
     //next: smooth
     SmoothRA(lSliceSum,lSlices);
     //next: normalize so each slice is normalized to brightest axial slice
     for lX := 1 to lSlices do
         lSliceSum^[lX] := lSliceSum^[lX]/lSliceMax;
     //next - Left cropping- removing slices that are <5%  of maximum slice
     lX := lSlices;
     while (lX > 1) and (lSliceSum^[lX] < (lPct/100)) do
           dec(lX);
     lRCrop := lSlices-lX;
     //next - Left cropping- removing slices that are <5%  of maximum slice
     lX := 1;
     while (lX <= lSlices) and (lSliceSum^[lX] < (lPct/100)) do
           inc(lX);
     lLCrop := lX-1;
     //fx(lLCrop,lRCrop);
     result := true;
     Freemem(lSliceSumUnaligned);
end;

function APClip (var lACrop,lPCrop: integer; lPct,lDClip,lVClip: integer): boolean;
//amount of image to crop from anterior/posterior for 5% signal intensity
var
   lSliceMax: double;
   lSliceSum,lSliceSumUnaligned: Doublep;
   lZMin,lZMax,lX,lY,lZ,lSlices,lSliceSz,lSliceStart: integer;
begin
     result := false;
     lACrop := 0;
     lPCrop := 0;
     lZMin := lVClip;
     lZMax := gBGImg.ScrnDim[3]-lDClip;
     SortInt(lZMin,lZMax);
     lZMin := Bound(lZMin,1,gBGImg.ScrnDim[3]);
     lZMax := Bound(lZMax,1,gBGImg.ScrnDim[3]);
     if lZMin >= lZMax then
        exit;
     if (lPct < 1) or (lPct > 100) then
        exit;
     if (gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems < 1) or (gBGImg.ScrnDim[1] < 2) or (gBGImg.ScrnDim[2] < 2) or (gBGImg.ScrnDim[3] < 2)  then begin
        showmessage('Please load a 3D background image for neck removal.');
        exit;
     end;
     if not gBGImg.resliced then begin
         showmessage('Neck removal can only be done on resliced images.');
         exit;
     end;
     lSlices := gBGImg.ScrnDim[2];
     lSliceSz := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2];
     //lCoroSliceSz := gBGImg.ScrnDim[1]*gBGImg.ScrnDim[3];
     GetMem(lSliceSumUnaligned,(lSlices*sizeof(double))+16);
     lSliceSum := DoubleP($fffffff0 and (integer(lSliceSumUnaligned)+15));
     lSliceMax := 0;
     for lY := 1 to lSlices do begin
         lSliceSum^[lY] := 0;
         //lSliceStart := lY;
         for lZ := {1 to gBGImg.ScrnDim[3]} lZMin to lZMax do begin
             lSliceStart := ((lY-1)* gBGImg.ScrnDim[1])+ ((lZ-1)*lSliceSz);
             if lSliceStart > (lSliceSz*gBGImg.ScrnDim[3]) then
                showmessage('xx');
             for lX := 1 to gBGImg.ScrnDim[1] do
                 lSliceSum^[lY] := lSliceSum^[lY]+ gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lSliceStart+lX];
         end; //for lZ
         //for lYZ := 1 to lSliceSz do
         //    lSliceSum^[lY] := lSliceSum^[lY]+ gMRIcroOverlay[kBGOverlayNum].ScrnBuffer[lXY+lSliceStart];
         if  lSliceMax < lSliceSum^[lY] then
             lSliceMax := lSliceSum^[lY];
     end; //for each slice
     if lSliceMax = 0 then begin //no data variance
        Freemem(lSliceSumUnaligned);
        exit;
     end; //VolSum = 0
     //next: smooth
     SmoothRA(lSliceSum,lSlices);
     //next: normalize so each slice is normalized to brightest axial slice
     for lY := 1 to lSlices do
         lSliceSum^[lY] := lSliceSum^[lY]/lSliceMax;
     //next - Left cropping- removing slices that are <5%  of maximum slice
     lY := lSlices;
     while (lY > 1) and (lSliceSum^[lY] < (lPct/100)) do
           dec(lY);
     lACrop := lSlices-lY;
     //next - Left cropping- removing slices that are <5%  of maximum slice
     lY := 1;
     while (lY <= lSlices) and (lSliceSum^[lY] < (lPct/100)) do
           inc(lY);
     lPCrop := lY-1;
     result := true;
     Freemem(lSliceSumUnaligned);
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
				  gMRIcroOverlay[kBGOverlayNum].ImgBuffer[lPos] := 0;
                   end; //for X
           end;
      2: begin
           //fx(gMRIcroOverlay[kBGOverlayNum].ImgBufferItems);
           l16Buf := SmallIntP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
           for lZ := 1 to gBGImg.ScrnDim[3] do
               for lY := 1 to gBGImg.ScrnDim[2] do
                   for lX := 1 to gBGImg.ScrnDim[1] do begin
				inc(lPos);
				if (lZ >= lZHi) or (lZ <= lZLo) or(lY >= lYHi) or (lY <= lYLo) or (lX >= lXHi) or (lX <= lXLo) then
				  l16Buf[lPos] := 0;
                   end; //for X
           end;
      4: begin
           l32Buf := SingleP(gMRIcroOverlay[kBGOverlayNum].ImgBuffer );
           for lZ := 1 to gBGImg.ScrnDim[3] do
               for lY := 1 to gBGImg.ScrnDim[2] do
                   for lX := 1 to gBGImg.ScrnDim[1] do begin
				inc(lPos);
				if (lZ >= lZHi) or (lZ <= lZLo) or(lY >= lYHi) or (lY <= lYLo) or (lX >= lXHi) or (lX <= lXLo) then
				  l32Buf[lPos] := 0;
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
     //xx Move(gBGImg.VOIUndoVol^,gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVolItems);
     FillChar(gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer^,gBGImg.VOIUndoVolItems,0);
	lXlo := round(LEdit.value);
	lXHi := gBGImg.ScrnDim[1] - round(REdit.value);
	lYlo := round(PEdit.value);
	lYHi := gBGImg.ScrnDim[2] - round(AEdit.value);
	lZLo := round(VEdit.value);
	lZHi := gBGImg.ScrnDim[3] - round(DEdit.value);
	lPos := 0;
	for lZ := 1 to gBGImg.ScrnDim[3] do
		for lY := 1 to gBGImg.ScrnDim[2] do
			for lX := 1 to gBGImg.ScrnDim[1] do begin
				inc(lPos);
				if (lZ >= lZHi) or (lZ <= lZLo) or(lY >= lYHi) or (lY <= lYLo) or (lX >= lXHi) or (lX <= lXLo) then
				  gMRIcroOverlay[kVOIOverlayNum].ScrnBuffer[lPos] := kVOI8bit;
			end; //for X
        //gBGImg.VOIchanged := true;
	ImgForm.RefreshImagesTimer.enabled := true;
end;

var
vFirst: boolean = true;

procedure TCropEdgeForm.FormShow(Sender: TObject);
var
   lV,lD,lA,lP,lL,lR: integer;
begin
     EnsureVOIOpen;
     CreateUndoVol;
     CropEdgeForm.ModalResult := mrCancel;
     lD := 0;
     lV := 0;
     lL := 0;
     lR := 0;
     lA := 0;
     lP := 0;
     if VentralClip (lD,lV,2) then
        if LRClip (lL,lR,2,lD,lV) then
           APClip (lA,lP,2,lD,lV);
     if vFirst then begin
     VEdit.Value := lV;
     DEdit.value := lD;
     LEdit.Value := lL;
     REdit.value := lR;
     AEdit.Value := lA;
     PEdit.value := lP;
     vFirst := false;
     end;
     CropEditChange(nil);
end;

procedure TCropEdgeForm.FormHide(Sender: TObject);
begin
	 //if (CropEdgeForm.ModalResult = mrCancel) then
         UndoVolVOI;
	 if not (CropEdgeForm.ModalResult = mrCancel) then
	 	ApplyCrop2Img
         else
	     ImgForm.RefreshImagesTimer.Enabled := true;
       ImgForm.CloseVOIClick(nil);
end;



procedure TCropEdgeForm.CancelBtnClick(Sender: TObject);
begin
     CropEdgeForm.close;
end;

procedure TCropEdgeForm.ApplyBtnClick(Sender: TObject);
begin
	CropEdgeForm.ModalResult := mrOK;
	CropEdgeForm.close;
end;

procedure TCropEdgeForm.Timer1Timer(Sender: TObject);
begin
     Timer1.Enabled := false;
     ApplyCrop;
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
     lV := VEdit.AsInteger;
     lD := DEdit.AsInteger;
     lL := LEdit.AsInteger;
     lR := REdit.AsInteger;
     lA := AEdit.AsInteger;
     lP := PEdit.AsInteger;
     CropNIfTI(lL,lR,lA,lP,lD,lV);
     CancelBtn.Click;
end;

procedure TCropEdgeForm.SpeedButton1Click(Sender: TObject);
begin
 //GrowNeck ('C:\walker\tpm.nii', -30);
 GrowNeck ('C:\MATLAB\spm8\templates\T1.nii', -30);
end;

end.
