unit about;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,ShellAPI;

type
  TAboutForm = class(TForm)
    Label1: TLabel;
    ThreadLabel: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation
var
  gX: integer = 0;
{$R *.DFM}

procedure TAboutForm.FormCreate(Sender: TObject);
begin
//Image1.Picture.Icon:=Application.Icon;
end;

procedure TAboutForm.Label1Click(Sender: TObject);
begin
   ShellExecute (0, Nil, 'http://www.mricro.com', Nil, Nil, SW_ShowDefault);
end;

end.
