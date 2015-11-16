unit mni;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin, Mask;

type
  TMNIForm = class(TForm)
    XEdit: TRxSpinEdit;
    YEdit: TRxSpinEdit;
    ZEdit: TRxSpinEdit;
    procedure XEditChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MNIForm: TMNIForm;

implementation

uses nifti_img_view,nifti_img,define_types;

{$R *.DFM}

procedure TMNIForm.XEditChange(Sender: TObject);
  var
lXmm,lYmm,lZmm: single;
lX,lY,lZ: integer;
begin
 if not MNIForm.visible then exit;
 if gMRIcroOverlay[kBGOverlayNum].ScrnBufferItems=0 then
		exit;
 lXmm:=XEdit.value;
 lYmm:=YEdit.value;
 lZmm:=ZEdit.value;
 MMToImgCoord(lX,lY,lZ,lXmm,lYmm,lZmm);
 if lX <> ImgForm.XViewEdit.value then ImgForm.XViewEdit.value := lX;
 if lY <> ImgForm.YViewEdit.value then ImgForm.YViewEdit.value := lY;
 if lZ <> ImgForm.ZViewEdit.value then ImgForm.ZViewEdit.value := lZ;
 ImgForm.XViewEditChange(nil);
end;

end.
