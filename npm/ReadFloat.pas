unit ReadFloat;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin;

type
  TReadFloatForm = class(TForm)
    OKBtn: TButton;
    ReadFloatLabel: TLabel;
    ReadFloatEdit: TRxSpinEdit;
     function GetFloat(lStr: string; lMin,lDefault,lMax: double): double;

    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReadFloatForm: TReadFloatForm;

implementation

{$R *.DFM}
 function TReadFloatForm.GetFloat(lStr: string; lMin,lDefault,lMax: double): double;
 begin
	  //result := lDefault;
      ReadFloatLabel.caption := lStr+' ['+floattostr(lMin)+'..'+floattostr(lMax)+']';
	  ReadFloatEdit.MinValue := lMin;
	  ReadFloatEdit.MaxValue := lMax;
	  ReadFloatEdit.Value := lDefault;
	  ReadFloatForm.ShowModal;
	  result :=  ReadFloatEdit.Value;
 end;

procedure TReadFloatForm.OKBtnClick(Sender: TObject);
begin
     ReadFloatForm.ModalResult := mrOK;
end;

end.
