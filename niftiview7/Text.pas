unit Text;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls,Define_Types;

type
  TTextForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Save1: TMenuItem;
    Closewindow1: TMenuItem;
    Copy1: TMenuItem;
    Copy2: TMenuItem;
    MemoT: TMemo;
    ClearMenu: TMenuItem;
    procedure Closewindow1Click(Sender: TObject);
    procedure Copy2Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure ClearMenuClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TextForm: TTextForm;

implementation

uses nifti_img_view;

{$R *.DFM}

procedure TTextForm.Closewindow1Click(Sender: TObject);
begin
	TextForm.Close;
end;

procedure TTextForm.Copy2Click(Sender: TObject);
begin
	MemoT.SelectAll;
	MemoT.CopyToClipboard;
end;

procedure TTextForm.Save1Click(Sender: TObject);
var
   lStr: string;
begin
     lStr :=  ImgForm.SaveDialog1.filename;
     ImgForm.SaveDialog1.filename := changefileextX(lstr,'');
   if kTextSep = chr(9) then
	  ImgForm.SaveDialog1.Filter := 'Tab Separated (*.tab)|*.tab|Comma Separated (*.csv)|*.csv|Text (*.txt)|*.txt'
   else
	  ImgForm.SaveDialog1.Filter := 'Comma Separated (*.csv)|*.csv|Tab Separated (*.tab)|*.tab|Text (*.txt)|*.txt';
   if kTextSep = chr(9) then
	  ImgForm.SaveDialog1.DefaultExt := '*.tab'
   else
	  ImgForm.SaveDialog1.DefaultExt := '*.csv';
	 if not ImgForm.SaveDialog1.Execute then exit;
	 MemoT.Lines.SaveToFile(ImgForm.SaveDialog1.Filename);
end;

procedure TTextForm.ClearMenuClick(Sender: TObject);
begin
     MemoT.lines.clear;
end;

end.
