unit ReadInt;

interface

uses
 {$IFDEF FPC} LResources,{$ENDIF}
  Buttons{only Lazarus?},SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type

  { TReadIntForm }

  TReadIntForm = class(TForm)
    ReadIntEdit: TSpinEdit;
    ReadIntLabel: TLabel;
    OKBtn: TButton;
    procedure FormShow(Sender: TObject);
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

uses nifti_img_view,{license,} MultiSlice, render;
                    
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
   //ReadIntForm.OKBtn.Focused := true;
     //ReadIntForm.OKBtn.SetFocus;
	  ReadIntForm.ShowModal;
	  result :=  ReadIntEdit.Value;
 end;

 procedure TReadIntForm.FormShow(Sender: TObject);
 begin
    //OKBtn.SetFocus;;
 end;

procedure TReadIntForm.OKBtnClick(Sender: TObject);
begin
	  ReadIntForm.ModalResult := mrOK;
end;


procedure TReadIntForm.FormCreate(Sender: TObject);
//var lCPUid: longint;
begin
         //Jan 2008 39448
	 if Date > (400003) then begin
		showmessage('This software became obsolete on '+datetostr(40000)+'. Please update to the current version.');
		//gBGImg.LicenseID := 1626;
		//ImgForm.Exit1Click(nil);
	 end;
end;

{$IFDEF FPC}
initialization
  {$I ReadInt.lrs}
{$ENDIF}

end.
