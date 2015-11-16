unit rotation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, nifti_hdr,Buttons, StdCtrls, Mask, RXSpin, nifti_types;

type
  TRotationForm = class(TForm)
    YawEdit: TRxSpinEdit;
    PitchEdit: TRxSpinEdit;
    ROllEdit: TRxSpinEdit;
    LabelX: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    ResliceBtn: TSpeedButton;
    AdjustMatrixBtn: TSpeedButton;
    procedure ResliceBtnClick(Sender: TObject);
    procedure YawPitchRollChange(Sender: TObject);
    procedure RotateNIFTIMatrix (var lHdr: TNIFTIhdr; lYaw,lPitch,lRoll: single);
    procedure AdjustMatrixBtnClick(Sender: TObject);
    procedure GenerateRotation (lReslice: boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RotationForm: TRotationForm;

implementation

{$R *.dfm}
uses
nifti_img_view, nifti_img, nifti_hdr_view, define_types, reslice_img,GraphicsMathLibrary;


procedure ReportMatrix (lM:TMatrix);
const
	kCR = chr (13);
begin
	showmessage(RealToStr(lM.matrix[1,1],6)+','+RealToStr(lM.matrix[1,2],6)+','+RealToStr(lM.matrix[1,3],6)+','+RealToStr(lM.matrix[1,4],6)+kCR+
		RealToStr(lM.matrix[2,1],6)+','+RealToStr(lM.matrix[2,2],6)+','+RealToStr(lM.matrix[2,3],6)+','+RealToStr(lM.matrix[2,4],6)+kCR+
		RealToStr(lM.matrix[3,1],6)+','+RealToStr(lM.matrix[3,2],6)+','+RealToStr(lM.matrix[3,3],6)+','+RealToStr(lM.matrix[3,4],6)+kCR
    +RealToStr(lM.matrix[4,1],6)+','+RealToStr(lM.matrix[4,2],6)+','+RealToStr(lM.matrix[4,3],6)+','+RealToStr(lM.matrix[4,4],6)
	  );
end;   


procedure RotateAroundPivot (var lR: TMatrix; lXoffset,lYoffset,lZOffset: single);
(*http://www.euclideanspace.com/maths/geometry/affine/aroundPoint/index.htm
For three dimensional rotations about x,y we can represent it with the following 4x4 matrix:
r00 	r01 	r02 	x - r00*x - r01*y - r02*z
r10 	r11 	r12 	y - r10*x - r11*y - r12*z
r20 	r21 	r22 	z - r20*x - r21*y - r22*z
0 	0 	0 	1  *)
begin
    lR.matrix[1,4] := lXoffset - lR.matrix[1,1]*lXoffset - lR.matrix[1,2]*lYOffset - lR.matrix[1,3]*lZoffset;
    lR.matrix[2,4] := lYoffset - lR.matrix[2,1]*lXoffset - lR.matrix[2,2]*lYOffset - lR.matrix[2,3]*lZoffset;
    lR.matrix[3,4] := lZoffset - lR.matrix[3,1]*lXoffset - lR.matrix[3,2]*lYOffset - lR.matrix[3,3]*lZoffset;
end;


procedure TRotationForm.RotateNIFTIMatrix (var lHdr: TNIFTIhdr; lYaw,lPitch,lRoll: single);
//reorient image by YPR degrees...
var
  lM, lR:  TMatrix;
begin
  //caption := floattostr(lYaw)+'x'+floattostr(lPitch)+'x'+floattostr(lRoll);
    if (lYaw = 0) and (lPitch = 0) and (lRoll = 0) then
      exit;

    lR := Eye3D;
    RotateYaw(lYaw, lR);
    RotatePitch(lPitch,lR);
    RotateRoll(lRoll, lR);

    lM := Hdr2Mat (lHdr);
    //RotateAroundPivot ( lR,  -lM.matrix[1,4],-lM.matrix[2,4],-lM.matrix[3,4]);
    //RotateAroundPivot ( lR,  lM.matrix[1,4],lM.matrix[2,4],lM.matrix[3,4]);
    //ReportMatrix (lR);
    //order lM := MultiplyMatrices(lM,lR);
    lM := MultiplyMatrices(lR,lM);
    //ReportMatrix(lM);
    Mat2Hdr(lM,lHdr);
end;

(*procedure RotateMatrix (var lHdr: TNIFTIhdr; lYaw,lPitch,lRoll: single);
//reorient image by YPR degrees...
var
  lM, lR:  TMatrix;
begin
    lR := Eye3D;
    lM := Hdr2Mat (lHdr);
    RotateYaw(lYaw, lM);
    RotatePitch(lPitch,lM);
    RotateRoll(lRoll, lM);
    //ReportMatrix (lM);
    Mat2Hdr(lM,lHdr);
end; *)
procedure TRotationForm.GenerateRotation (lReslice: boolean);
var
  lDefaultReorient,lDefaultResliceView: boolean;
  lLayer: integer;
  lY,lP,lR: single;
  lFilename: string;
     lNIFTIhdr: TNIFTIhdr;
begin
  lLayer := ImgForm.ActiveLayer;
	if gMRIcroOverlay[lLayer].ImgBufferItems=0 then begin
		Showmessage('You must load an image [File/Open] before you can save the image.');
		exit;
	end;
  lDefaultResliceView := gBGImg.ResliceOnLoad;
  gBGImg.ResliceOnLoad := lReslice;
  lDefaultReorient := gBGImg.UseReorientHdr;

  lFilename := gMRIcroOverlay[lLayer].HdrFileName;//HdrForm.OpenHdrDlg.Filename;
  lY := YawEdit.Value;
  lP := PitchEdit.value;
  lR := RollEdit.value;
  if not lReslice then begin
    YawEdit.Value := 0;
    PitchEdit.value := 0;
    RollEdit.value := 0;
  end;
  ImgForm.CloseImagesClick(nil);
  ImgForm.OpenAndDisplayImg(lFilename,True);
  lFilename := changefileprefix(lFilename,'r');
  CopyNiftiHdr(gMRIcroOverlay[kBGOverlayNum].NiftiHdr,lNIFTIhdr);

  if not lReslice then begin
    gBGImg.UseReorientHdr := false;
    RotateNIFTIMatrix(lNiftiHdr,lY,lP,lR);
  end;
  SaveAsVOIorNIFTIcore (lFilename, gMRIcroOverlay[kBGOverlayNum].ImgBuffer,gMRIcroOverlay[kBGOverlayNum].ImgBufferItems,gMRIcroOverlay[kBGOverlayNum].ImgBufferBPP,1,lNIFTIhdr) ;
  ImgForm.CloseImagesClick(nil);
  if lReslice then begin
    YawEdit.Value := 0;
    PitchEdit.value := 0;
    RollEdit.value := 0;
  end;
  gBGImg.UseReorientHdr := lDefaultReorient;
  gBGImg.ResliceOnLoad := lDefaultResliceView;
  ImgForm.OpenAndDisplayImg(lFilename,True);
end;

procedure TRotationForm.ResliceBtnClick(Sender: TObject);
begin
  GenerateRotation(true);
end;

procedure TRotationForm.YawPitchRollChange(Sender: TObject);

var
  lLayer: integer;
  lFilename: string;
     lNIFTIhdr: TNIFTIhdr;
begin
    	lLayer := ImgForm.ActiveLayer;
	if gMRIcroOverlay[lLayer].ImgBufferItems=0 then begin
		//Showmessage('You must load an image [File/Open] before you can save the image.');
		exit;
	end;
  (*LoadYaw := YawEdit.value;
  loadPitch := Pitchedit.Value;
  LoadRoll := RollEdit.Value;*)
  lFilename := gMRIcroOverlay[lLayer].HdrFileName;
  //lFilename := HdrForm.OpenHdrDlg.Filename;
  //caption := lFilename;
  ImgForm.OpenAndDisplayImg(lFilename,false);
end;

procedure TRotationForm.AdjustMatrixBtnClick(Sender: TObject);
begin
  GenerateRotation(false);
end;              

end.
