unit cutout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin;
const
     kMaxFrac = 1000;//e.g. if 100 then cutouts are done by percent, if 1000 then 0.001

type

  { TCutoutForm }

  TCutoutForm = class(TForm)
    CutoutBiasDrop: TComboBox;
    CutoutLUTDrop: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PreviewBtn: TButton;
    DefBtn: TButton;
    OKBtn: TButton;
    CutoutBox: TGroupBox;
    PreviewBtn1: TButton;
    RenderCutoutCheck: TCheckBox;
    XLo: TSpinEdit;
    XHi: TSpinEdit;
    YLo: TSpinEdit;
    YHi: TSpinEdit;
    ZLo: TSpinEdit;
    ZHi: TSpinEdit;
    procedure Prep;
    procedure DefBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure PreviewBtn1Click(Sender: TObject);
    procedure PreviewClick(Sender: TObject);
    procedure RenderCutoutCheckClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  CutoutForm: TCutoutForm;

implementation
{$DEFINE REND} //if you define "REND" render form will be interactively adjusted
uses render_composite{grender}, nifti_img_view {LUTdrop},define_types {sortcutout}
{$IFDEF REND}, render {$ENDIF}{azimuth,elevation,timer}
;
var gInit: boolean = false;

{ TCutoutForm }
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
{$IFDEF REND}
        gZoom := 0.5;
	RenderForm.RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
	RenderForm.RenderRefreshTimer.enabled := true;
{$ENDIF}
end;

procedure TCutoutForm.DefBtnClick(Sender: TObject);
begin
     gInit := true;
   Ylo.Value := kMaxFrac shr 1;
   Yhi.Value := kMaxFrac ;
   Zlo.Value := kMaxFrac shr 1;
   Zhi.Value := kMaxFrac ;
   //OverlayClipEdit.value := 0;
   //BGClipEdit.value := 0;
   RenderCutoutCheck.checked := true;
   CutoutLUTdrop.ItemIndex := 0;
   CutoutBiasDrop.ItemIndex:= 3;
{$IFDEF REND}
   if renderForm.FlipLRcheck.checked then begin
       Xlo.Value := 0;
       Xhi.Value := kMaxFrac shr 1;

   end else begin
       Xlo.Value := kMaxFrac shr 1;
       Xhi.Value := kMaxFrac ;
   end;
   RenderForm.AzimuthEdit.value := 120;
   RenderForm.ElevationEdit.value := 45;
{$ELSE}
       Xlo.Value := 0;
       Xhi.Value := kMaxFrac shr 1;
{$ENDIF}
   gInit := false;
   RenderCutoutCheckClick(nil);//PreviewClick(nil);

end;

procedure TCutoutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   ReadCutoutForm;
 //note: exit if no changes
{$IFDEF REND}
 RenderForm.RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
 RenderForm.RenderRefreshTimer.enabled := true;
{$ENDIF}
end;

procedure TCutoutForm.FormShow(Sender: TObject);
begin
     Prep;
end;

procedure TCutoutForm.OKBtnClick(Sender: TObject);
begin
  	CutoutForm.close;
end;

procedure TCutoutForm.PreviewBtn1Click(Sender: TObject);
begin
	ReadCutoutForm;
	RenderForm.RenderRefreshTimer.Tag := -1; //force a new rotation matrix to be generated
	RenderForm.RenderRefreshTimer.enabled := true;
end;

initialization
  {$I cutout.lrs}

end.

