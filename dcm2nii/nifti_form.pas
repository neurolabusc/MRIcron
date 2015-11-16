unit nifti_form;

interface

uses
{$IFDEF FPC}LResources,
{$ELSE}
RXSpin,
{$ENDIF}
{$IFNDEF UNIX} Windows,{$ENDIF}
  Buttons, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls {, Mask};
{$IFNDEF FPC}
type
  TNIfTIform = class(TForm)
    ASLCombo: TComboBox;
    Combo4D: TComboBox;
    Label1: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Combo3D: TComboBox;
    Panel1: TPanel;
    FormulaPanel: TPanel;
    StartEdit: TSpinEdit;
    EndEdit: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    TypeCombo: TComboBox;
    Label4: TLabel;
    ASLPanel: TPanel;
    Label5: TLabel;
    ScaleEdit: TRxSpinEdit;
    PowerEdit: TRxSpinEdit;
    procedure Combo4DChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
{$ELSE}
type
  TNIfTIform = class(TForm)
    ASLCombo: TComboBox;
    Combo4D: TComboBox;
    Label1: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Combo3D: TComboBox;
    Panel1: TPanel;
    FormulaPanel: TPanel;
    StartEdit: TSpinEdit;
    EndEdit: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    TypeCombo: TComboBox;
    Label4: TLabel;
    ASLPanel: TPanel;
    Label5: TLabel;
    ScaleEdit: TFloatSpinEdit;
    PowerEdit: TFloatSpinEdit;
    procedure Combo4DChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private

{$ENDIF}
{Delphi...
    ScaleEdit: TRxSpinEdit;
    PowerEdit: TRxSpinEdit;
Lazarus
    ScaleEdit: TFloatSpinEdit;
    PowerEdit: TFloatSpinEdit;
}
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NIfTIform: TNIfTIform;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

procedure TNIfTIform.Combo4DChange(Sender: TObject);
var
   lS: string;
begin
     lS := '';
     If ((Combo4D.visible) and (Combo4D.ItemIndex = 6))
       or ( (not (Combo4D.visible)) and (Combo3D.ItemIndex = 3)) then begin
        Panel1.visible := true;
        lS := 'Slices';
     end else If (Combo4D.visible) and (Combo4D.ItemIndex = 2) then begin
        Panel1.visible := true;
        lS := 'Volumes'
     end else
         Panel1.visible := false;
     if lS <> '' then begin
         Label2.Caption := lS+' to remove from start';
         Label3.Caption := lS+' to remove from end';

     end;
     If (Combo4D.visible) and (Combo4D.ItemIndex = 4) then
        FormulaPanel.visible := true
     else
         FormulaPanel.visible := false;
     If (Combo4D.visible) and (Combo4D.ItemIndex = 5) then
        ASLPanel.visible := true
     else
         ASLPanel.visible := false;
end;

procedure TNIfTIform.FormCreate(Sender: TObject);
begin
   Combo3D.ItemIndex := 0;
   Combo4D.ItemIndex := 0;
   ASLCombo.ItemIndex := 0;
end;

procedure TNIfTIform.OKBtnClick(Sender: TObject);
begin

end;

{$IFDEF FPC}


initialization
  {$I nifti_form.lrs}
 {$ENDIF}

end.
