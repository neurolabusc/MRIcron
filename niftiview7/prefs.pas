unit prefs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin, Buttons, Mask;

type
  TPrefForm = class(TForm)
    CancelBtn: TSpeedButton;
    OKBtn: TSpeedButton;
    GroupBox1: TGroupBox;
    ThinPenCheck: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    GroupBox2: TGroupBox;
    ResliceCheck: TCheckBox;
    Label2: TLabel;
    Label1: TLabel;
    LabelX: TLabel;
    MaxDimEdit: TRxSpinEdit;
    ThreadEdit: TRxSpinEdit;
    SigDigEdit: TRxSpinEdit;
    TabletPressureEdit: TRxSpinEdit;
    TabletErasePressureEdit: TRxSpinEdit;
    AutoFillCheck: TCheckBox;
    OrthoCheck: TCheckBox;
    SingleROwCheck: TCheckBox;
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ResliceCheckClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PrefForm: TPrefForm;

implementation
uses
    nifti_img_view;

{$R *.DFM}

procedure TPrefForm.CancelBtnClick(Sender: TObject);
begin
     Close;
end;

procedure TPrefForm.OKBtnClick(Sender: TObject);
begin
     gBGImg.ResliceOnLoad := ResliceCheck.checked;
     gBGImg.OrthoReslice := OrthoCheck.checked;
     gBGImg.MaxDim := MaxDimEdit.asInteger;
     gnCPUThreads{gMaxCPUThreads} := ThreadEdit.asInteger;
     //ImgForm.ToolPanel.Visible := DrawCheck.checked;
     //ImgForm.DrawMenu.Visible := DrawCheck.checked;
     gBGImg.ThinPen := ThinPenCheck.Checked;
     gBGImg.AutoFill := AutoFillCheck.checked;
     gBGImg.SigDig := SigDigEdit.asInteger;
     gBGImg.TabletPressure := TabletPressureEdit.asInteger;
     gBGImg.TabletErasePressure := TabletErasePressureEdit.asInteger;
     ImgForm.SetAutoFill;


     if (gBGImg.SingleRow <> SingleRowCheck.Checked)  then  begin
        gBGImg.SingleRow := SingleRowCheck.Checked;
        //gBGImg.Show2ndSliceViews := Show2ndSliceViewsCheck.checked;
        ImgForm.DefaultControlPanel;
        ImgForm.RefreshImagesTimer.enabled := true;
     end;

          Close;

end;

procedure TPrefForm.FormShow(Sender: TObject);
begin
     ResliceCheck.checked := gBGImg.ResliceOnLoad;
     //OrthoCheck.Visible := not gBGImg.ResliceOnLoad;
     OrthoCheck.checked := gBGImg.OrthoReslice;
     MaxDimEdit.value := gBGImg.MaxDim;
     ThreadEdit.value := gnCPUThreads{gMaxCPUThreads};
     //DrawCheck.checked := ImgForm.ToolPanel.Visible;
     ThinPenCheck.Checked := gBGImg.ThinPen;
     AutoFillCheck.Checked := gBGImg.AutoFill;
     SigDigEdit.value := gBGImg.SigDig;
     SingleRowCheck.checked := gBGImg.SingleRow;
     //Show2ndSliceViewsCheck.Checked := gBGImg.Show2ndSliceViews;
     TabletPressureEdit.value := gBGImg.TabletPressure;
     TabletErasePressureEdit.value := gBGImg.TabletErasePressure;

end;

procedure TPrefForm.ResliceCheckClick(Sender: TObject);
begin
     //OrthoCheck.Visible := not ResliceCheck.checked;
end;

end.
