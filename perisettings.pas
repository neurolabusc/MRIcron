unit perisettings; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, define_types;

type

  { TPSForm }

  TPSForm = class(TForm)
    BinWidthEdit: TFloatSpinEdit;
    OKBtn: TButton;
    SavePSVolCheck: TCheckBox;
    PctSignalCheck: TCheckBox;
    ModelCheck: TCheckBox;
    RegressCheck: TCheckBox;
    TDCheck: TCheckBox;
    SliceTImeCheck: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PreBinEdit: TSpinEdit;
    PostBinEdit: TSpinEdit;
 procedure FormShow(Sender: TObject);
 function GetPeriSettings(var lPSPlot: TPSPlot): boolean;
  procedure OKBtnClick(Sender: TObject);
  procedure RegressCheckClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  PSForm: TPSForm;

implementation
uses nifti_img_view;

 function TPSForm.GetPeriSettings(var lPSPlot: TPSPlot): boolean;
 begin
      result := false;
      if lPSPlot.TRSec <= 0 then begin
          showmessage('Please specify the TR (in seconds) before creating a peristimulus plot.');
          exit;
      end;
      if BinWidthEdit.value = 0 then
         BinWidthEdit.value := lPSPlot.TRsec;
      PSForm.ShowModal;
      if BinWidthEdit.value = 0 then
         BinWidthEdit.value := lPSPlot.TRsec
      else
          lPSPlot.BinWidthSec :=  BinWidthEdit.Value;
      lPSPlot.nNegBins := PreBinEdit.value;
      lPSPlot.nPosBins := PostBinEdit.value;
      lPSPlot.SliceTime := SliceTimeCheck.checked;
      lPSPlot.SavePSVol := SavePSVolCheck.checked;
      lPSPlot.BaselineCorrect := ModelCheck.checked;
      lPSPlot.PctSignal := PctSignalCheck.checked;
      lPSPlot.RemoveRegressorVariability := RegressCheck.checked;
      lPSPlot.TemporalDeriv := TDcheck.checked;
      lPSPlot.PlotModel := ModelCheck.checked;
      lPSPlot.SPMDefaultsStatsFmriT := gBGImg.SPMDefaultsStatsFmriT;
      lPSPlot.SPMDefaultsStatsFmriT0 := gBGImg.SPMDefaultsStatsFmriT0;
      result := true;
 end;

 procedure TPSForm.FormShow(Sender: TObject);
 begin
      RegressCheckClick(nil);
 end;



 procedure TPSForm.OKBtnClick(Sender: TObject);
 begin

 end;

 procedure TPSForm.RegressCheckClick(Sender: TObject);
 begin
     TDCheck.visible := RegressCheck.checked;
  ModelCheck.Visible := RegressCheck.checked;
 end;

initialization
  {$I perisettings.lrs}

end.

