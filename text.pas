unit text;
{$H+}

interface

uses
{$IFDEF FPC}LResources,{$ENDIF}
{$IFNDEF Unix} Windows,{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls,Define_Types;

type

  { TTextForm }

  TTextForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Save1: TMenuItem;
    Closewindow1: TMenuItem;
    Copy1: TMenuItem;
    Copy2: TMenuItem;
    MemoT: TMemo;
    procedure Closewindow1Click(Sender: TObject);
    procedure Copy2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Save1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TextForm: TTextForm;

implementation

 uses nifti_img_view;
{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

procedure TTextForm.Closewindow1Click(Sender: TObject);
begin
	TextForm.Close;
end;

procedure TTextForm.Copy2Click(Sender: TObject);
begin
        {$IFDEF zxDarwin}
        Showmessage('Copy not yet supported with OSX: use File/Save');
        exit;
        {$ENDIF}
	MemoT.SelectAll;
	MemoT.CopyToClipboard;
end;

procedure TTextForm.FormCreate(Sender: TObject);
begin
                {$IFDEF Darwin}
        {$IFNDEF LCLgtk} //only for Carbon compile
        Copy2.ShortCut := ShortCut(Word('C'), [ssMeta]);
         Save1.ShortCut := ShortCut(Word('S'), [ssMeta]);
         Closewindow1.ShortCut := ShortCut(Word('W'), [ssMeta]);
         {$ENDIF}
        {$ENDIF}
end;

procedure TTextForm.Save1Click(Sender: TObject);
begin
     ImgForm.SaveDialog1.Filename := parsefilename(gMRIcroOverlay[kBGOverlayNum].HdrFilename);
     if kTextSep = chr(9) then
	  ImgForm.SaveDialog1.Filter := 'Tab Separated (*.tab)|*.tab|Comma Separated (*.csv)|*.csv|Text (*.txt)|*.txt'
     else
	  ImgForm.SaveDialog1.Filter := 'Comma Separated (*.csv)|*.csv|Tab Separated (*.tab)|*.tab|Text (*.txt)|*.txt';
     if kTextSep = chr(9) then
          ImgForm.SaveDialog1.DefaultExt := '.tab'
     else
         ImgForm.SaveDialog1.DefaultExt := '.csv';
     if not ImgForm.SaveDialog1.Execute then exit;
     MemoT.Lines.SaveToFile(ImgForm.SaveDialog1.Filename);
end;

{$IFDEF FPC}
initialization
  {$I Text.lrs}
{$ENDIF}


end.