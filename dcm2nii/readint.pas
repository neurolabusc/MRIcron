unit readint;

interface

uses
{$IFDEF FPC}LResources,Buttons,{$ENDIF}
{$IFNDEF UNIX}  Windows,{$ENDIF}
   Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin,define_types;

type
  TReadIntForm = class(TForm)
    ReadIntEdit: TSpinEdit;
    ReadIntLabel: TLabel;
    OKBtn: TButton;
    function GetInt(lStr: string; lMin,lDefault,lMax: integer): integer;
    procedure OKBtnClick(Sender: TObject);
	  private
	{ Private declarations }
  public

	{ Public declarations }
  end;
const
	gPassname: kStr20='NIH';
var
  ReadIntForm: TReadIntForm;

implementation

{$IFNDEF FPC}{$R *.DFM}   {$ENDIF}

 function TReadIntForm.GetInt(lStr: string; lMin,lDefault,lMax: integer): integer;
 begin
	  //result := lDefault;
      ReadIntLabel.caption := lStr+' ['+inttostr(lMin)+'..'+inttostr(lMax)+']';
	  ReadIntEdit.MinValue := lMin;
	  ReadIntEdit.MaxValue := lMax;
	  ReadIntEdit.Value := lDefault;
	  ReadIntForm.ShowModal;
	  result :=  ReadIntEdit.Value;
 end;

procedure TReadIntForm.OKBtnClick(Sender: TObject);
begin
	  ReadIntForm.ModalResult := mrOK;
end;

{$IFDEF FPC}
initialization
  {$I readint.lrs}
 {$ENDIF}

end.
