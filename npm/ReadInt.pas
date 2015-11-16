unit ReadInt;

interface

uses
 {$IFDEF FPC} LResources,{$ENDIF}
  Buttons{only Lazarus?},SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TReadIntForm = class(TForm)
    ReadIntEdit: TSpinEdit;
    ReadIntLabel: TLabel;
    OKBtn: TButton;
    function GetInt(lStr: string; lMin,lDefault,lMax: integer): integer;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
	  private
	{ Private declarations }
  public

	{ Public declarations }
  end;

var
  ReadIntForm: TReadIntForm;

implementation

                    
  {$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}
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


procedure TReadIntForm.FormCreate(Sender: TObject);
begin
end;

{$IFDEF FPC}
initialization
  {$I ReadInt.lrs}
{$ENDIF}

end.
