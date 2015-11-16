unit reorient;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin,nifti_hdr,graphicsMathLibrary, ExtCtrls;

type
  TReorientForm = class(TForm)
    ZEdit: TRxSpinEdit;
    YEdit: TRxSpinEdit;
    XEdit: TRxSpinEdit;
    ReorientTimer: TTimer;
    PitchEdit: TRxSpinEdit;
    RollEdit: TRxSpinEdit;
    YawEdit: TRxSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Xlabel: TLabel;
    YLabel: TLabel;
    ZLabel: TLabel;
    procedure ReorientTimerTimer(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyTransform ( var lHdr: TMRIcroHdr);

    { Public declarations }
  end;

var
  ReorientForm: TReorientForm;

implementation

uses nifti_img_view,nifti_img, nifti_hdr_view;

{$R *.DFM}

procedure TReorientForm.ApplyTransform ( var lHdr: TMRIcroHdr);
var
 lM,lRot: TMatrix;
begin
  //exit;
  if (XEdit.value = 0) and (YEdit.value = 0) and (ZEdit.value = 0)
  and (PitchEdit.value = 0) and  (YawEdit.value = 0)  and  (RollEdit.value = 0) then exit;
     lRot := Matrix3D (1,0,0,XEdit.value,
                         0,1,0,YEdit.value,
                         0,0,1,ZEdit.value,
                         0,0,0,1);
     lM := Matrix3D (
  lHdr.NIFTIhdr.srow_x[0],lHdr.NIFTIhdr.srow_x[1],lHdr.NIFTIhdr.srow_x[2],lHdr.NIFTIhdr.srow_x[3],      // 3D "graphics" matrix
  lHdr.NIFTIhdr.srow_y[0],lHdr.NIFTIhdr.srow_y[1],lHdr.NIFTIhdr.srow_y[2],lHdr.NIFTIhdr.srow_y[3],      // 3D "graphics" matrix
  lHdr.NIFTIhdr.srow_z[0],lHdr.NIFTIhdr.srow_z[1],lHdr.NIFTIhdr.srow_z[2],lHdr.NIFTIhdr.srow_z[3],      // 3D "graphics" matrix
						   0,0,0,1);
  if PitchEdit.value <> 0 then RotatePitch(PitchEdit.value{-11.4592},lRot);
  if RollEdit.value <> 0 then RotateRoll(RollEdit.value,lRot);
  if YawEdit.value <> 0 then RotateYaw(YawEdit.value,lRot);
  //lM := lRot;
    lM := MultiplyMatrices(lRot,lM);

(*  WriteNiftiMatrix (lHdr.NiftiHdr,
        lM.matrix[1,1],lM.matrix[1,2],lM.matrix[1,3],lM.matrix[1,4],
        lM.matrix[2,1],lM.matrix[2,2],lM.matrix[1,3],lM.matrix[2,4],
        lM.matrix[3,1],lM.matrix[3,2],lM.matrix[1,3],lM.matrix[3,4]);

  lHdr.Mat := lM;  *)
  //Caption := inttostr(random(888))+floattostr(lM.matrix[1,4]);
  XLabel.caption:= floattostr(lM.matrix[1,1])+'x'+floattostr(lM.matrix[1,2])+'x'+floattostr(lM.matrix[1,3])+'x'+floattostr(lM.matrix[1,4]);
  YLabel.caption:= floattostr(lM.matrix[2,1])+'x'+floattostr(lM.matrix[2,2])+'x'+floattostr(lM.matrix[2,3])+'x'+floattostr(lM.matrix[2,4]);
  ZLabel.caption:= floattostr(lM.matrix[3,1])+'x'+floattostr(lM.matrix[3,2])+'x'+floattostr(lM.matrix[3,3])+'x'+floattostr(lM.matrix[3,4]);
end;
(*procedure DrawLine(lIMage: TImage; lXmm,lYmm,lZmm,lXmm2,lYmm2,lZmm2: integer);
var
   lX,lY,lZ: integer;
begin
     lImage.Canvas.Pen.Color:=gBGImg.XBarClr;
     lImage.Canvas.Pen.Width := gBGImg.XBarThick;
     MMToImgCoord( lX,lY,lZ, lXmm,lYmm,lZmm);
        lImage.Canvas.MoveTo(lX,lY);
     MMToImgCoord( lX,lY,lZ, lXmm2,lYmm2,lZmm2);
     lImage.Canvas.LineTo(lX,lY);
end;*)

procedure TReorientForm.ReorientTimerTimer(Sender: TObject);
begin
     ReorientTimer.enabled := false;
     //ImgForm.CloseOverlayImgClick(nil);

     (*DrawLine(ImgForm.PGImage2,0,-104,0,0,72,0);//horizontal line on Sag
     DrawLine(ImgForm.PGImage2,0,0,78,0,0,-48);//horizontal line on Sag
     DrawLine(ImgForm.PGImage3,0,0,77,0,0,-48);//horizontal line on Sag
     DrawLine(ImgForm.PGImage3,-64,0,0,64,0,0);//horizontal line on Sag
      *)
     ApplyTransform(gMRIcroOverlay[kBGOverlayNum]);
   //MMToImgCoord(var lX,lY,lZ: integer; var lXmm,lYmm,lZmm: single);
end;

(*procedure TReorientForm.ReorientTimerTimer(Sender: TObject);
var
   lFilename: string;
begin
     ReorientTimer.enabled := false;
     ImgForm.CloseOverlayImgClick(nil);
     ReorientTimer.enabled := false;
     lFilename := 'C:\pas\Delphi\niftiview\grey.voi';
     ImgForm.LoadOverlay (lFilename);
end; *)

procedure TReorientForm.EditChange(Sender: TObject);
begin
   ReorientTimer.enabled := true;
end;

end.
