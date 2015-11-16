unit mni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Spin;

type

  { TMNIForm }

  TMNIForm = class(TForm)
    XEdit: TSpinEdit;
    YEdit: TSpinEdit;
    ZEdit: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure XEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MNIForm: TMNIForm;

implementation
uses define_types, nifti_img,nifti_img_view;
{ TMNIForm }

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

procedure TMNIForm.FormCreate(Sender: TObject);
begin

end;

initialization
  {$I mni.lrs}

end.

