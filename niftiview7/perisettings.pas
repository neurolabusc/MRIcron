unit perisettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin,define_types;

type
  TPSForm = class(TForm)
    BinWidthEdit: TRxSpinEdit;
    Label3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    SliceTimeCheck: TCheckBox;
    OKBtn: TButton;
    PreBinEdit: TRxSpinEdit;
    PostBinEdit: TRxSpinEdit;
    SavePSVolCheck: TCheckBox;
    BaselineCorrectCheck: TCheckBox;
    PctSignalCheck: TCheckBox;
    RegressCheck: TCheckBox;
    TDCheck: TCheckBox;
    ModelCheck: TCheckBox;
 function GetPeriSettings(var lPSPlot: TPSPlot):boolean;
    procedure RegressCheckClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PSForm: TPSForm;

implementation

{$R *.DFM}
uses NIFti_Img_View;
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
      lPSPlot.nNegBins := PreBinEdit.AsInteger;
      lPSPlot.nPosBins := PostBinEdit.AsInteger;
      lPSPlot.SliceTime := SliceTimeCheck.checked;
      lPSPlot.SavePSVol := SavePSVolCheck.checked;
      lPSPlot.BaselineCorrect := BaselineCorrectCheck.checked;
      lPSPlot.PctSignal := PctSignalCheck.checked;
      lPSPlot.RemoveRegressorVariability := RegressCheck.checked;
      lPSPlot.TemporalDeriv := TDcheck.checked;
      lPSPlot.PlotModel := ModelCheck.checked;
      lPSPlot.SPMDefaultsStatsFmriT := gBGImg.SPMDefaultsStatsFmriT;
      lPSPlot.SPMDefaultsStatsFmriT0 := gBGImg.SPMDefaultsStatsFmriT0;
      result := true;
 end;


procedure TPSForm.RegressCheckClick(Sender: TObject);
begin
  TDCheck.visible := RegressCheck.checked;
  ModelCheck.Visible := RegressCheck.checked;
end;

procedure TPSForm.FormShow(Sender: TObject);
begin
RegressCheckClick(nil);
end;

end.
