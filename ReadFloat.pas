unit ReadFloat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin;

type

  { TReadFloatForm }

  TReadFloatForm = class(TForm)
    ReadFloatEdit: TFloatSpinEdit;
    OKBtn: TButton;
    ReadFloatLabel: TLabel;
    procedure OKBtnClick(Sender: TObject);
 function GetFloat(lStr: string; lMin,lDefault,lMax: double): double;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ReadFloatForm: TReadFloatForm;

implementation

{ TReadFloatForm }
 function TReadFloatForm.GetFloat(lStr: string; lMin,lDefault,lMax: double): double;
 begin
	  //result := lDefault;
      ReadFloatLabel.caption := lStr+' ['+floattostr(lMin)+'..'+floattostr(lMax)+']';
	  ReadFloatEdit.MinValue := lMin;
	  ReadFloatEdit.MaxValue := lMax;
	  ReadFloatEdit.Value := lDefault;
	  ReadFloatForm.ShowModal;
	  result :=  ReadFloatEdit.value;
 end;

procedure TReadFloatForm.OKBtnClick(Sender: TObject);
begin
         ReadFloatForm.ModalResult := mrOK;
end;

initialization
  {$I ReadFloat.lrs}

end.

