unit about;

interface

uses
{$IFDEF FPC}LResources,{$ELSE} ShellAPI, {$ENDIF}
{$IFNDEF Unix} Windows,{$ENDIF}
   SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, define_types;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    HomepageLabel: TLabel;
    Label1: TLabel;
    ThreadLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure HomePageClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation
{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

procedure TAboutForm.FormCreate(Sender: TObject);
var
  str: string;
begin
{$IFDEF CPU64}
str := '64-bit';
{$ELSE}
str := '32-bit';
{$ENDIF}
{$IFDEF Windows}str := str + ' Windows '; {$ENDIF}
{$IFDEF LINUX}str := str + ' Linux '; {$ENDIF}
{$IFDEF Darwin}str := str + ' OSX '; {$ENDIF}
{$IFDEF LCLQT}str := str + ' (QT) '; {$ENDIF}
{$IFDEF LCLQT5}str := str + ' (QT5) '; {$ENDIF}
{$IFDEF LCLGTK2}str := str + ' (GTK2) '; {$ENDIF}
{$IFDEF LCLGTK3}str := str + ' (GTK3) '; {$ENDIF}
{$IFDEF LCLCocoa}str := str + ' (Cocoa) ';{$ENDIF}
{$IFDEF LCLCarbon}str := str + ' (Carbon) '; {$ENDIF}
{$IFDEF CPUX86_64}
str := str + 'x86-64 ';
{$ENDIF}
{$IFDEF CPUAARCH64}
str := str + 'ARM64 ';
{$ENDIF}
{$IFDEF CPULLVM}str := str + 'LLVM '; {$ENDIF}
      HomepageLabel.caption := 'www.mricro.com :: '+str+kVers ;
end;

procedure TAboutForm.HomePageClick(Sender: TObject);
begin
{$IFDEF FPC}
{$ELSE}
   ShellExecute (0, Nil, 'http://www.mricro.com', Nil, Nil, SW_ShowDefault);
{$ENDIF}
end;

procedure TAboutForm.Panel1Click(Sender: TObject);
begin

end;

procedure TAboutForm.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
//showmessage('x');
end;

{$IFDEF FPC}
initialization
  {$I about.lrs}
{$ENDIF}

end.
