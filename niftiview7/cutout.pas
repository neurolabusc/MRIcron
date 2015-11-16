unit cutout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin, Buttons,nifti_img_view,nifti_img,define_types;
const
     kMaxFrac = 1000;//e.g. if 100 then cutouts are done by percent, if 1000 then 0.001
type
  TCutoutForm = class(TForm)
    OKBtn: TSpeedButton;
    SpeedButton1: TSpeedButton;
    CutoutBox: TGroupBox;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Xlo: TRxSpinEdit;
    XHi: TRxSpinEdit;
    YLo: TRxSpinEdit;
    YHi: TRxSpinEdit;
    ZLo: TRxSpinEdit;
    ZHi: TRxSpinEdit;
    CutoutBiasDrop: TComboBox;
    cutoutlutdrop: TComboBox;
    DefBtn: TSpeedButton;
    RenderCutoutCheck: TCheckBox;
    PreviewBtn: TSpeedButton;
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PreviewClick(Sender: TObject);
    procedure Prep;
    procedure DefBtnClick(Sender: TObject);
    procedure RenderCutoutCheckClick(Sender: TObject);
    procedure PreviewBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CutoutForm: TCutoutForm;

implementation

uses render,render_composite;
var gInit: boolean;


{$R *.DFM}



procedure TCutoutForm.OKBtnClick(Sender: TObject);
begin
	CutoutForm.close;
end;

procedure TCutoutForm.Prep;
begin
   gInit := true;
   CutoutForm.caption := 'Cutouts: '+inttostr(kMaxFrac)+'= 100%';
   cutoutlutdrop.Items := ImgForm.LUTdrop.items;
   if (gRender.cutoutLUTindex < 1) or (gRender.cutoutLUTindex > cutoutlutdrop.Items.Count) then
      cutoutlutdrop.itemindex := 0
   else
      cutoutlutdrop.itemindex :=  gRender.cutoutLUTindex;
   if gRender.CutoutFrac.Lo[1] < 0 then
           SliceToFrac(gBGImg);
   SortCutout(gRender.CutoutFrac);
   Xlo.maxValue := kMaxFrac;//gBGImg.ScrnDim[1];
   Xhi.maxValue := kMaxFrac;//gBGImg.ScrnDim[1];
   Ylo.maxValue := kMaxFrac;//gBGImg.ScrnDim[2];
   Yhi.maxValue := kMaxFrac;//gBGImg.ScrnDim[2];
   Zlo.maxValue := kMaxFrac;//gBGImg.ScrnDim[3];
   Zhi.maxValue := kMaxFrac;//gBGImg.ScrnDim[3];
   Xlo.Value := gRender.CutoutFrac.Lo[1];
   Xhi.Value := gRender.CutoutFrac.Hi[1];
   Ylo.Value := gRender.CutoutFrac.Lo[2];
   Yhi.Value := gRender.CutoutFrac.Hi[2];
   Zlo.Value := gRender.CutoutFrac.Lo[3];
   Zhi.Value := gRender.CutoutFrac.Hi[3];
   //OverlayClipEdit.value := gRender.OverlayNearClipFrac;
   //BGClipEdit.value := gRender.BGNearClipFrac;
   RenderCutoutCheck.checked := gRender.ShowCutout;
   CutoutBiasDrop.ItemIndex:=( gRender.CutoutBias);
   RenderCutoutCheckClick(nil);
   gInit := false;
end;

procedure TCutoutForm.FormShow(Sender: TObject);
begin
Prep;
end;

procedure ReadCutoutForm;
begin
  with CutoutForm do begin
		gRender.CutoutFrac.Lo[1] := round(Xlo.Value);
		gRender.CutoutFrac.Hi[1] := round(Xhi.Value);
		gRender.CutoutFrac.Lo[2] := round(Ylo.Value);
		gRender.CutoutFrac.Hi[2] := round(Yhi.Value);
		gRender.CutoutFrac.Lo[3] := round(Zlo.Value);
		gRender.CutoutFrac.Hi[3] := round(Zhi.Value);
		SortCutout(gRender.CutoutFrac);
		gRender.ShowCutout := RenderCutoutCheck.checked;
		gRender.CutoutBias := CutoutBiasDrop.ItemIndex;
                gRender.cutoutLUTindex := cutoutlutdrop.itemindex;
  //gRender.OverlayNearClipFrac := round(OverlayClipEdit.value);
  // gRender.BGNearClipFrac := round(BGClipEdit.value);

  end;
end;

(*procedure TCutoutForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
	lCutout : TCutOut;
	lRenderCutoutCheck,lChange : boolean;
	lCutoutBiasDrop,lInc: integer;
begin
 lCutout := gRender.Cutout;
 lRenderCutoutCheck := gRender.ShowCutout;
 lCutoutBiasDrop := gRender.CutoutBias;
 ReadCutoutForm;
 //next: do not render if no changes...
 lChange := false;
 if (gRender.ShowCutout <> lRenderCutoutCheck) or (lCutoutBiasDrop <> CutoutBiasDrop.ItemIndex) then
	lChange := true;
 for lInc := 1 to 3 do
	if (gRender.Cutout.Lo[lInc] <> lCutout.Lo[lInc]) or
		(gRender.Cutout.Hi[lInc] <> lCutout.Hi[lInc]) then
			lChange := true;
 if not lChange then exit;
 //note: exit if no changes
 RenderForm.RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
 RenderForm.RenderRefreshTimer.enabled := true;
end;*)

procedure TCutoutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 ReadCutoutForm;
 //note: exit if no changes
 RenderForm.RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
 RenderForm.RenderRefreshTimer.enabled := true;
end;



procedure TCutoutForm.DefBtnClick(Sender: TObject);
begin
   gInit := true;
   if renderForm.FlipLRcheck.checked then begin
       Xlo.Value := 0;
       Xhi.Value := kMaxFrac shr 1;

   end else begin
       Xlo.Value := kMaxFrac shr 1;
       Xhi.Value := kMaxFrac ;
   end;
   Ylo.Value := kMaxFrac shr 1;
   Yhi.Value := kMaxFrac ;
   Zlo.Value := kMaxFrac shr 1;
   Zhi.Value := kMaxFrac ;
   //OverlayClipEdit.value := 0;
   //BGClipEdit.value := 0;
   RenderCutoutCheck.checked := true;
   CutoutLUTdrop.ItemIndex := 0;
   CutoutBiasDrop.ItemIndex:= 3;
   RenderForm.AzimuthEdit.value := 120;
   RenderForm.ElevationEdit.value := 45;
   gInit := false;
   RenderCutoutCheckClick(nil);//PreviewClick(nil);
end;

procedure TCutoutForm.RenderCutoutCheckClick(Sender: TObject);
begin
      CutoutBox.visible := RenderCutoutCheck.Checked;
     PreviewClick(nil);
end;

procedure TCutoutForm.PreviewClick(Sender: TObject);
begin
        if gInit then
           exit;
	ReadCutoutForm;
        gZoom := 0.5;
	RenderForm.RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
	RenderForm.RenderRefreshTimer.enabled := true;
end;

procedure TCutoutForm.PreviewBtnClick(Sender: TObject);
//preview at normal resolution
begin
        if gInit then
           exit;
	ReadCutoutForm;
	RenderForm.RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
	RenderForm.RenderRefreshTimer.enabled := true;

end;

end.
