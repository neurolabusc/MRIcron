unit ReadInt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,define_types, RXSpin, Mask;

type
  TReadIntForm = class(TForm)
    ReadIntLabel: TLabel;
    OKBtn: TButton;
    ReadIntEdit: TRxSpinEdit;
    function GetInt(lStr: string; lMin,lDefault,lMax: integer): integer;
    procedure OKBtnClick(Sender: TObject);
	  private
	{ Private declarations }
  public

	{ Public declarations }
  end;

var
  ReadIntForm: TReadIntForm;

implementation

uses nifti_img,nifti_img_view,{license,} MultiSlice, render;

{$R *.DFM}
 function TReadIntForm.GetInt(lStr: string; lMin,lDefault,lMax: integer): integer;
 begin
    ReadIntLabel.caption := lStr+' ['+inttostr(lMin)+'..'+inttostr(lMax)+']';
	  ReadIntEdit.MinValue := lMin;
	  ReadIntEdit.MaxValue := lMax;
	  ReadIntEdit.Value := lDefault;
	  ReadIntForm.ShowModal;
	  result :=  ReadIntEdit.asInteger;
 end;

procedure TReadIntForm.OKBtnClick(Sender: TObject);
begin
	  ReadIntForm.ModalResult := mrOK;
end;


end.
