unit histoform;

interface

uses
{$IFNDEF Unix} Windows,{$ENDIF}

  {$IFDEF FPC} LResources,{$ENDIF}
   Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls,ClipBrd;

type

  { THistogramForm }

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
    procedure FormCreate(Sender: TObject);
    procedure Saveasbitmap1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HistogramForm: THistogramForm;

implementation
{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}
uses nifti_img;

procedure THistogramForm.Copy1Click(Sender: TObject);
{$IFDEF FPC}
begin
	 if (HistoImage.Picture.Graphic = nil) then begin //1420z
        Showmessage('You need to load an image before you can copy it to the clipboard.');
        exit;
	 end;
       HistoImage.Picture.Bitmap.SaveToClipboardFormat(2);
end;
{$ELSE}
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
end;
{$ENDIF}


procedure THistogramForm.Closewindow1Click(Sender: TObject);
begin
	HistogramForm.Close;
end;

procedure THistogramForm.FormCreate(Sender: TObject);
begin
          {$IFDEF Darwin}
        {$IFNDEF LCLgtk} //only for Carbon compile
        Copy1.ShortCut := ShortCut(Word('C'), [ssMeta]);
         Saveasbitmap1.ShortCut := ShortCut(Word('S'), [ssMeta]);
         Closewindow1.ShortCut := ShortCut(Word('W'), [ssMeta]);
         {$ENDIF}
        {$ENDIF}
end;

procedure THistogramForm.Saveasbitmap1Click(Sender: TObject);
begin
{$IFNDEF FPC}
	 SaveImgAsPNGBMP (HistoImage);
{$ELSE}
       SaveImgAsPNGBMP (HistoImage);
{$ENDIF}
end;

  {$IFDEF FPC}
initialization
  {$I histoform.lrs}
{$ENDIF}

end.