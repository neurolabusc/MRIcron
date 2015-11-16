unit histoform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls,ClipBrd;

type
  THistogramForm = class(TForm)
    HistoPanel: TScrollBox;
    HistoImage: TImage;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Saveasbitmap1: TMenuItem;
    Closewindow1: TMenuItem;
    procedure Copy1Click(Sender: TObject);
    procedure Closewindow1Click(Sender: TObject);
    procedure Saveasbitmap1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HistogramForm: THistogramForm;

implementation
{$R *.DFM}
uses nifti_img;

procedure THistogramForm.Copy1Click(Sender: TObject);
var
  MyFormat : Word;
  AData: THandle;
  APalette : HPalette;   //For later versions of Delphi: APalette : THandle;
begin
	 if (HistoImage.Picture.Graphic = nil) then begin //1420z
        Showmessage('You need to load an image before you can copy it to the clipboard.');
        exit;
	 end;
	 HistoImage.Picture.SaveToClipBoardFormat(MyFormat,AData,APalette);
    ClipBoard.SetAsHandle(MyFormat,AData)
	 //HistoImage.Picture.Bitmap.SaveToClipBoardFormat(MyFormat,AData,APalette);
end;


procedure THistogramForm.Closewindow1Click(Sender: TObject);
begin
	HistogramForm.Close;
end;

procedure THistogramForm.Saveasbitmap1Click(Sender: TObject);
begin
	 SaveImgAsPNGBMP (HistoImage);
end;

end.
