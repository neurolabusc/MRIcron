unit prefs; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Buttons;

type

  { TPrefForm }

  TPrefForm = class(TForm)
    SingleRowCheck: TCheckBox;
    OrthoCheck: TCheckBox;
    XBarClr: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    ThinPenCheck: TCheckBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ResliceCheck: TCheckBox;
    GroupBox1: TGroupBox;
    MaxDimEdit: TSpinEdit;
    ThreadEdit: TSpinEdit;
    SigDigEdit: TSpinEdit;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure ResliceCheckClick(Sender: TObject);
    procedure XBarClrClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  PrefForm: TPrefForm;

implementation
             uses
    nifti_img_view;
{ TPrefForm }

procedure TPrefForm.CancelBtnClick(Sender: TObject);
begin
       Close;
end;

procedure TPrefForm.FormCreate(Sender: TObject);
begin

end;

procedure TPrefForm.FormShow(Sender: TObject);
begin
        //RGBPlanarCheck.checked := gBGImg.isPlanarRGB;
       ResliceCheck.checked := gBGImg.ResliceOnLoad;
       //OrthoCheck.Visible := not gBGImg.ResliceOnLoad;
       OrthoCheck.checked := gBGImg.OrthoReslice;
     MaxDimEdit.value := gBGImg.MaxDim;
     ThreadEdit.value := gnCPUThreads;
     //DrawCheck.checked := ImgForm.ToolPanel.Visible;
     ThinPenCheck.Checked := gBGImg.ThinPen;
     SigDigEdit.value := gBGImg.SigDig;
          SingleRowCheck.checked := gBGImg.SingleRow;
end;

procedure TPrefForm.OKBtnClick(Sender: TObject);
begin
     //gBGImg.isPlanarRGB := RGBPlanarCheck.checked;
     gBGImg.ResliceOnLoad := ResliceCheck.checked;
          gBGImg.OrthoReslice := OrthoCheck.checked;
     gBGImg.MaxDim := MaxDimEdit.value;
     gnCPUThreads := ThreadEdit.value;
     //ImgForm.ToolPanel.Visible := DrawCheck.checked;
     //ImgForm.DrawMenu.Visible := DrawCheck.checked;
     gBGImg.ThinPen := ThinPenCheck.Checked;
     gBGImg.SigDig := SigDigEdit.value;
     if gBGImg.SingleRow <> SingleRowCheck.Checked then begin
        gBGImg.SingleRow := SingleRowCheck.Checked;
        ImgForm.DefaultControlPanel;
        ImgForm.RefreshImagesTimer.enabled := true;
     end;
     Close;
end;

procedure TPrefForm.ResliceCheckClick(Sender: TObject);
begin
    OrthoCheck.Visible := not ResliceCheck.checked;
end;

procedure TPrefForm.XBarClrClick(Sender: TObject);
begin
  ImgForm.XBarColor;
  PrefForm.BringToFront;
end;

initialization
  {$I prefs.lrs}

end.

